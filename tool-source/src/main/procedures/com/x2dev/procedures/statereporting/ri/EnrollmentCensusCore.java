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
package com.x2dev.procedures.statereporting.ri;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.UpdateQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.admin.ReferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import java.util.logging.Level;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 *
 * Rhode Island state report for ECC export.
 *
 * @author X2 Development Corporation
 */
public class EnrollmentCensusCore extends RIStateReportData {
    /**
     * Implementation of StateReportEntity to be used by the ECC export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class EnrollmentCensusCoreEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";
        /*
         * Cached values for retrievers to share.
         */
        EnrollmentCensusCore m_eccData = null;

        List<MembershipAttendance> m_schoolList = new ArrayList<MembershipAttendance>();
        SisStudent m_student = null;

        /**
         * Instantiates a new enrollment census core entity.
         */
           /*
            * Public no argument constructor for dynamic instantiation.
            */
        public EnrollmentCensusCoreEntity() {
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
         * Returns the student's race codes.
         *
         * @return Collection
         */
        public Collection<Race> getPersonRaceCodes() {
            SisStudent student = (SisStudent) getBean();
            Collection<Race> raceBeans = m_eccData.m_raceCodeMap.get(student.getPersonOid());
            return raceBeans;
        }

        /**
         * Returns the student.
         *
         * @return Sis student
         */
        public SisStudent getStudent() {
            return m_student;
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

            m_eccData = (EnrollmentCensusCore) data;
            m_student = (SisStudent) bean;
            SimpleEnrollmentSnapshot snapshot = getSnapshot(m_student, m_eccData.m_reportDate);
            int membCount = 0; // Count of attempts to add membership segments, though some might be
                               // refused. used for recalc attendance.
            List<StudentEnrollmentSpan> spans = getSpans();
            if (snapshot != null && snapshot.getSchool() != null && isStudentActive(spans, snapshot)) {
                String schoolOid = snapshot.getSchool().getOid();
                String enrollCode = snapshot.getEnrollmentStatus();
                SisSchool lastSchool = null;
                String withdrawCode = null;

                PlainDate lastDate = null;
                int lastYog = 0;
                String lastStatus = "";

                StudentEnrollment nextEnrollment = null;
                StudentEnrollmentSpan nextSpan = null;
                // Should indicate what would happen AFTER this status, not what caused this status.
                // Reverse chronological order.
                String lastType = StudentEnrollment.WITHDRAWAL;

                for (StudentEnrollmentSpan span : spans) {
                    String stateExitType = null;
                    String outsidePlacementLocationCode = null;
                    String outsidePlacementClassificationCode = null;
                    String residentCode = m_eccData.getCalculatedResidentCode(span.getFirstActiveEnrollment());
                    List<StudentEnrollment> enrollmentsOfSpan = getCorrectEnrollmentsFormSpan(span);
                    String homeschoolCode = null;
                    nextSpan = getNext(spans, span);
                    PlainDate firstSchoolYearDate = null;
                    PlainDate firstDate = null;
                    School currentSchool = null;
                    School lastCurrentSchool = null;
                    for (StudentEnrollment enrollment : enrollmentsOfSpan) {
                        if (currentSchool == null) {
                            currentSchool = enrollment.getSchool();
                        }
                        if (lastCurrentSchool == null || !lastCurrentSchool.equals(currentSchool)) {
                            lastCurrentSchool = currentSchool;
                            firstSchoolYearDate =
                                    m_eccData.getFirstSchoolYearDate(currentSchool, m_student.getCalendarCode());
                            // First date may be overwritten during iterations, initialize only if
                            // current school has changed and first date is null or before first
                            // school year date.
                            if (firstDate == null
                                    || (firstSchoolYearDate != null && firstDate.before(firstSchoolYearDate))) {
                                firstDate = firstSchoolYearDate;
                            }
                        }
                        nextEnrollment = getNext(enrollmentsOfSpan, enrollment);

                        String preRegCode = getPreRegCode(enrollment.getSchool().getOrganization1());

                        String stateSchoolId = (String) currentSchool.getFieldValueByBeanPath(m_eccData.m_sklIdField);
                        if (StringUtils.isEmpty(stateSchoolId) || preRegCode.equals(enrollment.getStatusCode())) {
                            lastYog = enrollment.getYog();
                            lastSchool = enrollment.getSchool();
                            lastStatus = enrollment.getStatusCode();
                            continue;
                        }

                        if (enrollment.getEnrollmentType().equals(StudentEnrollment.ENTRY)) {
                            firstDate = enrollment.getEnrollmentDate().after(firstSchoolYearDate)
                                    ? enrollment.getEnrollmentDate()
                                    : firstSchoolYearDate;
                            enrollCode = enrollment.getEnrollmentCode();
                            schoolOid = enrollment.getSchoolOid();
                            lastYog = enrollment.getYog();
                            outsidePlacementLocationCode = (String) enrollment
                                    .getFieldValueByBeanPath(m_eccData.m_outsidePlacementLocationCodeBeanPath);
                            outsidePlacementClassificationCode =
                                    (String) enrollment.getFieldValueByBeanPath(m_eccData.m_opClassificationBeanPath);
                            homeschoolCode =
                                    (String) enrollment.getFieldValueByBeanPath(m_eccData.m_fieldEnrHomeschoolCode);
                            if (nextEnrollment == null && nextSpan == null) {
                                lastDate = null;
                                addMembership(schoolOid, enrollCode, withdrawCode, residentCode, firstDate, lastDate,
                                        m_eccData, lastYog, stateExitType, homeschoolCode, outsidePlacementLocationCode,
                                        outsidePlacementClassificationCode);
                                membCount++;
                            }
                        } else if (enrollment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                            // Set up the data for the time prior to the enrollment.
                            schoolOid = enrollment.getSchoolOid();
                            currentSchool = enrollment.getSchool();

                            lastDate = enrollment.getEnrollmentDate();
                            withdrawCode = enrollment.getEnrollmentCode();
                            lastYog = enrollment.getYog();
                            stateExitType = StringUtils
                                    .isEmpty((String) enrollment.getFieldValueByBeanPath(m_eccData.m_stateExitType))
                                            ? stateExitType
                                            : (String) enrollment.getFieldValueByBeanPath(m_eccData.m_stateExitType);
                            outsidePlacementLocationCode = StringUtils.isEmpty((String) enrollment
                                    .getFieldValueByBeanPath(m_eccData.m_outsidePlacementLocationCodeBeanPath))
                                            ? outsidePlacementLocationCode
                                            : (String) enrollment.getFieldValueByBeanPath(
                                                    m_eccData.m_outsidePlacementLocationCodeBeanPath);
                            outsidePlacementClassificationCode = StringUtils.isEmpty(
                                    (String) enrollment.getFieldValueByBeanPath(m_eccData.m_opClassificationBeanPath))
                                            ? outsidePlacementClassificationCode
                                            : (String) enrollment
                                                    .getFieldValueByBeanPath(m_eccData.m_opClassificationBeanPath);
                            homeschoolCode = StringUtils.isEmpty(
                                    (String) enrollment.getFieldValueByBeanPath(m_eccData.m_fieldEnrHomeschoolCode))
                                            ? homeschoolCode
                                            : (String) enrollment
                                                    .getFieldValueByBeanPath(m_eccData.m_fieldEnrHomeschoolCode);

                            if (!lastDate.before(firstSchoolYearDate)) {
                                addMembership(schoolOid, enrollCode, withdrawCode, residentCode, firstDate, lastDate,
                                        m_eccData, lastYog, stateExitType, homeschoolCode, outsidePlacementLocationCode,
                                        outsidePlacementClassificationCode);
                                membCount++;
                            }
                        } else if (enrollment.getEnrollmentType().equals(StudentEnrollment.YOG_CHANGE) ||
                                enrollment.getEnrollmentType().equals(StudentEnrollment.STATUS_CHANGE)) {
                            // if the next enrollment is "S" record continue
                            String nextEnrollType = "";

                            if (nextEnrollment != null) {
                                nextEnrollType = StringUtils.isEmpty(nextEnrollment.getEnrollmentType()) ? ""
                                        : nextEnrollment.getEnrollmentType();
                                if (nextEnrollType.equals(StudentEnrollment.STATUS_CHANGE)) {
                                    lastSchool = enrollment.getSchool();
                                    continue;
                                }
                            }
                            String activeCode = getActiveCode(enrollment.getSchool().getOrganization1());

                            // Checks this when a student was enrolled but with only a status prereg
                            // and
                            // is now currently active
                            if (activeCode.equals(enrollment.getStatusCode()) && preRegCode.equals(lastStatus) &&
                                    lastType.equals(StudentEnrollment.ENTRY)) {
                                firstDate = enrollment.getEnrollmentDate().after(firstSchoolYearDate)
                                        ? enrollment.getEnrollmentDate()
                                        : firstSchoolYearDate;
                                enrollCode = enrollment.getEnrollmentCode();
                            }


                            if (lastType.equals(StudentEnrollment.ENTRY)
                                    || lastType.equals(StudentEnrollment.YOG_CHANGE)
                                    || lastType.equals(StudentEnrollment.STATUS_CHANGE)) {
                                if (!StringUtils.isEmpty(enrollment.getEnrollmentCode())) {
                                    enrollCode = data.lookupReferenceCodeByRefTbl(
                                            m_eccData.m_withdrawalCodeRefTbl,
                                            enrollment.getEnrollmentCode(),
                                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                                    stateExitType = enrollCode;
                                    lastDate = enrollment.getEnrollmentDate();
                                } else if (lastYog != 0 &&
                                        enrollment.getYog() != 0 &&
                                        lastYog > enrollment.getYog()) {
                                    enrollCode = "30"; // promoted to next grade
                                    stateExitType = enrollCode;
                                    lastDate = enrollment.getEnrollmentDate();
                                } else if (lastYog == enrollment.getYog() &&
                                        !lastSchool.equals(enrollment.getSchool())) {
                                    // Proceed to add new membership
                                } else if (lastYog == enrollment.getYog() &&
                                        lastSchool.equals(enrollment.getSchool())) {
                                    if (m_schoolList.size() == 0 &&
                                            activeCode.equals(m_student.getEnrollmentStatus())) {
                                        // Proceed to add new membership
                                        if (nextEnrollType.equals(StudentEnrollment.WITHDRAWAL)) {
                                            continue;
                                        }
                                    } else if (nextEnrollType.equals(StudentEnrollment.WITHDRAWAL)) {
                                        continue;
                                    } else if (nextSpan == null) {
                                        // Proceed to add new membership
                                    } else {
                                        continue;
                                    }
                                } else {
                                    enrollCode = "31"; // demoted to previous grade
                                    stateExitType = enrollCode;
                                    lastDate = enrollment.getEnrollmentDate();
                                }

                                if (nextSpan != null || nextEnrollment != null) {
                                    lastDate = enrollment.getEnrollmentDate();
                                }

                                if (enrollment.getEnrollmentType().equals(StudentEnrollment.YOG_CHANGE)) {
                                    if (lastDate == null) {
                                        lastDate = enrollment.getEnrollmentDate();
                                    }
                                    if (!lastDate.before(firstSchoolYearDate)) {
                                        addMembership(schoolOid, enrollCode, withdrawCode, residentCode, firstDate,
                                                lastDate,
                                                m_eccData, lastYog, stateExitType, homeschoolCode,
                                                outsidePlacementLocationCode,
                                                outsidePlacementClassificationCode);
                                        lastSchool = enrollment.getSchool();
                                    }
                                } else if (nextSpan == null && nextEnrollment == null
                                        && StringUtils.isEmpty(stateExitType)) {
                                    lastDate = null;
                                    addMembership(schoolOid, enrollCode, withdrawCode, residentCode, firstDate,
                                            lastDate,
                                            m_eccData, lastYog, stateExitType, homeschoolCode,
                                            outsidePlacementLocationCode,
                                            outsidePlacementClassificationCode);
                                    lastSchool = enrollment.getSchool();
                                    continue;
                                } else if (!lastDate.before(firstSchoolYearDate)) {
                                    addMembership(schoolOid, enrollCode, withdrawCode, residentCode, firstDate,
                                            lastDate,
                                            m_eccData, lastYog, stateExitType, homeschoolCode,
                                            outsidePlacementLocationCode,
                                            outsidePlacementClassificationCode);
                                }
                            }
                            if (enrollment.getEnrollmentDate().equals(firstSchoolYearDate) && m_schoolList.size() > 0) {
                                firstDate = getNextDate(enrollment.getEnrollmentDate());
                            }

                            if (enrollment.getEnrollmentType().equals(StudentEnrollment.YOG_CHANGE)) {
                                firstDate = enrollment.getEnrollmentDate().after(firstSchoolYearDate)
                                        ? getNextDate(enrollment.getEnrollmentDate())
                                        : firstSchoolYearDate;
                            } else if (!lastType.equals(StudentEnrollment.ENTRY)) {
                                firstDate = enrollment.getEnrollmentDate().after(firstSchoolYearDate)
                                        ? enrollment.getEnrollmentDate()
                                        : firstSchoolYearDate;
                            }

                            schoolOid = enrollment.getSchoolOid();
                            currentSchool = enrollment.getSchool();

                            lastDate = enrollment.getEnrollmentDate();
                            withdrawCode = enrollment.getEnrollmentCode();
                            lastYog = enrollment.getYog();
                            stateExitType = StringUtils
                                    .isEmpty((String) enrollment.getFieldValueByBeanPath(m_eccData.m_stateExitType))
                                            ? stateExitType
                                            : (String) enrollment.getFieldValueByBeanPath(m_eccData.m_stateExitType);
                            outsidePlacementLocationCode = StringUtils.isEmpty((String) enrollment
                                    .getFieldValueByBeanPath(m_eccData.m_outsidePlacementLocationCodeBeanPath))
                                            ? outsidePlacementLocationCode
                                            : (String) enrollment.getFieldValueByBeanPath(
                                                    m_eccData.m_outsidePlacementLocationCodeBeanPath);
                            outsidePlacementClassificationCode = StringUtils.isEmpty(
                                    (String) enrollment.getFieldValueByBeanPath(m_eccData.m_opClassificationBeanPath))
                                            ? outsidePlacementClassificationCode
                                            : (String) enrollment
                                                    .getFieldValueByBeanPath(m_eccData.m_opClassificationBeanPath);
                            homeschoolCode = StringUtils.isEmpty(
                                    (String) enrollment.getFieldValueByBeanPath(m_eccData.m_fieldEnrHomeschoolCode))
                                            ? homeschoolCode
                                            : (String) enrollment
                                                    .getFieldValueByBeanPath(m_eccData.m_fieldEnrHomeschoolCode);

                            if (nextEnrollment == null) {
                                if (enrollment.getEnrollmentType().equals(StudentEnrollment.YOG_CHANGE)) {
                                    lastDate = null;
                                    addMembership(schoolOid, enrollCode, withdrawCode, residentCode, firstDate,
                                            lastDate,
                                            m_eccData, lastYog, stateExitType, homeschoolCode,
                                            outsidePlacementLocationCode,
                                            outsidePlacementClassificationCode);
                                } else if (!lastDate.before(firstSchoolYearDate) ||
                                        (nextSpan == null && m_schoolList.size() == 0 &&
                                                "Active".equals(m_student.getEnrollmentStatus()))) {
                                    lastDate = null;
                                    addMembership(schoolOid, enrollCode, withdrawCode, residentCode, firstDate,
                                            lastDate,
                                            m_eccData, lastYog, stateExitType, homeschoolCode,
                                            outsidePlacementLocationCode,
                                            outsidePlacementClassificationCode);
                                    membCount++;
                                }
                            }
                        } else {
                            enrollCode = enrollment.getEnrollmentCode();
                            schoolOid = enrollment.getSchoolOid();
                        }
                        lastType = enrollment.getEnrollmentType();
                        lastSchool = enrollment.getSchool();
                        lastStatus = enrollment.getStatusCode();
                    }

                }
            }

            // If there are more than one membership period, recalculate attendance
            // by membership period. Only a small percentage of students will need this.
            if (m_schoolList.size() > 0 && membCount > 1) {
                recalcAbsenses();
            }

            setRowCount(m_schoolList.size());
        }

        /**
         * Adds a school and date range to the students list of schools attended in the school year.
         *
         * @param schoolOid String
         * @param enrollCode String
         * @param withdrawCode String
         * @param residentCode String
         * @param firstDate PlainDate
         * @param lastDate PlainDate
         * @param eccData EnrollmentCensusCore
         * @param yog int
         * @param stateExitType String
         * @param outsidePlacementLocationCode Object
         * @param outsidePlacementClassificationCode Object
         */
        private void addMembership(String schoolOid,
                                   String enrollCode,
                                   String withdrawCode,
                                   String residentCode,
                                   PlainDate firstDate,
                                   PlainDate lastDate,
                                   EnrollmentCensusCore eccData,
                                   int yog,
                                   String stateExitType,
                                   String homeschoolCode,
                                   Object outsidePlacementLocationCode,
                                   Object outsidePlacementClassificationCode) {
            SisStudent student = (SisStudent) getBean();

            // check that the school is not archive/inactive.
            SisSchool school = (SisSchool) eccData.getBroker().getBeanByOid(SisSchool.class, schoolOid);

            if ((getData().isSchoolContext() && getData().getSchool().getOid().equals(schoolOid)) ||
                    ((eccData.m_orgFieldStr != null && !StringUtils.isEmpty(eccData.m_orgOid))
                            && (eccData.m_orgOid.equals(school.getFieldValueByBeanPath(eccData.m_orgFieldStr))))
                    ||
                    (!getData().isSchoolContext()
                            && (eccData.m_orgFieldStr == null || StringUtils.isEmpty(eccData.m_orgOid)))) {
                if (!school.getInactiveIndicator()) {
                    if ((!school.getArchiveIndicator() && !school.getInactiveIndicator()) ||
                            !StringUtils.isEmpty(withdrawCode)) {
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
                        MembershipAttendance membership = new MembershipAttendance(schoolOid, enrollCode,
                                withdrawCode, residentCode, firstDate, lastDate, yog, stateExitType, homeschoolCode,
                                outsidePlacementLocationCode,
                                outsidePlacementClassificationCode);

                        Set set = eccData.getCalendarDays(school, student.getCalendarCode(), false);
                        if (set == null) {
                            System.out.println("calendar days is null for student");
                        } else {
                            if (set.size() == 0) {
                                System.out.println("set is empty");
                            }
                        }

                        PlainDate lastDateUse = lastDate;
                        if (lastDateUse == null) {
                            if (m_eccData.m_endOfYearMode) {
                                lastDateUse = m_eccData.m_schoolEndDateInYearMapping.get(membership.getSchoolOid());

                            } else {
                                lastDateUse = m_eccData.m_reportDate;
                            }
                        }

                        int memberDays = m_eccData.getMembershipTotal(
                                student,
                                set,
                                firstDate,
                                lastDateUse,
                                school);
                        // See if there are member days.
                        // If this is the EOY report, include summer withdrawals (which may not have
                        // membership days)
                        // Otherwise, exclude them.
                        // if (memberDays > 0 || !StringUtils.isEmpty(withdrawCode))
                        if (memberDays > 0 || !StringUtils.isEmpty(withdrawCode)
                                || ( /*
                                      * memberDays
                                      * == 0 &&
                                      */ m_eccData.m_startOfYearMode)) {
                            Float absenceCount = eccData.m_absences.get(student.getOid());
                            float absenceDays = 0;
                            if (absenceCount != null) {
                                absenceDays = absenceCount.floatValue();
                            }

                            String smembId = schoolOid + "-" + student.getCalendarCode();
                            Integer schMemb = eccData.m_schoolMembership.get(smembId);

                            membership.setMembership(memberDays);
                            membership.setAbsent(absenceDays);
                            if (schMemb != null) {
                                membership.setSchoolMembership(schMemb.intValue());
                            }

                            String opClassificationCode = (String) outsidePlacementClassificationCode;

                            if (opClassificationCode == null || m_eccData.m_opEvalClassification == null ||
                                    !opClassificationCode
                                            .equalsIgnoreCase(m_eccData.m_opEvalClassification.getCode())) {
                                m_schoolList.add(membership);
                            }
                        }
                    } // end if
                }
            }
        }

        /**
         * Exclude spans.
         *
         * @param spans List<StudentEnrollmentSpan>
         */
        private void excludeSpans(List<StudentEnrollmentSpan> spans) {
            Iterator<StudentEnrollmentSpan> spanIt = spans.iterator();
            label1: while (spanIt.hasNext()) {

                StudentEnrollmentSpan span = spanIt.next();
                Iterator<StudentEnrollment> enrolIt = span.getEnrollments().iterator();

                while (enrolIt.hasNext()) {
                    StudentEnrollment enrollment = enrolIt.next();
                    String excludeEnrIndicator =
                            (String) enrollment.getFieldValueByBeanPath(m_eccData.m_excludeEnrollment);
                    if (!StringUtils.isEmpty(excludeEnrIndicator)
                            && excludeEnrIndicator.equals(BooleanAsStringConverter.TRUE)) {
                        if (enrollment.getEnrollmentType().equals(StudentEnrollment.ENTRY)
                                || enrollment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                            spanIt.remove();
                            continue label1;
                        }
                        enrolIt.remove();

                    }
                }

                if (span.getEnrollments().isEmpty()) {
                    spanIt.remove();
                }

            }


        }

        /**
         * Gets the active code.
         *
         * @param organization Organization
         * @return String
         */
        private String getActiveCode(Organization organization) {
            return PreferenceManager
                    .getPreferenceValue(organization, SystemPreferenceDefinition.STUDENT_ACTIVE_CODE) != null
                            ? PreferenceManager.getPreferenceValue(
                                    organization,
                                    SystemPreferenceDefinition.STUDENT_ACTIVE_CODE)
                            : "Active";
        }

        /**
         * Gets the correct enrollments form span.
         *
         * @param span StudentEnrollmentSpan
         * @return List
         */
        private List<StudentEnrollment> getCorrectEnrollmentsFormSpan(StudentEnrollmentSpan span) {
            List<StudentEnrollment> enrollments = span.getEnrollments();
            Iterator<StudentEnrollment> it = enrollments.iterator();
            while (it.hasNext()) {
                StudentEnrollment enrollment = it.next();
                if (enrollment.getSchool() == null) {
                    it.remove();
                }
            }
            return enrollments;
        }



        /**
         * Gets the next.
         *
         * @param <T> the generic type
         * @param list List<T>
         * @param current T
         * @return t
         */
        private <T> T getNext(List<T> list, T current) {
            T value = null;
            int i = list.indexOf(current);
            if (i >= 0) {
                try {
                    value = list.get(i + 1);
                } catch (@SuppressWarnings("unused") IndexOutOfBoundsException e) {
                    // do nothing - null is correct
                }
            }

            return value;
        }

        /**
         * Gets the next date.
         *
         * @param date PlainDate
         * @return Plain date
         */
        private PlainDate getNextDate(PlainDate date) {
            PlainDate firstDate;
            Calendar c = Calendar.getInstance();
            c.setTime(date);
            c.add(Calendar.DATE, 1);
            firstDate = new PlainDate(c.getTime());
            return firstDate;
        }

        /**
         * Gets the pre reg code.
         *
         * @param organization Organization
         * @return String
         */
        private String getPreRegCode(Organization organization) {
            return PreferenceManager
                    .getPreferenceValue(organization, SystemPreferenceDefinition.STUDENT_PRE_REG) != null
                            ? PreferenceManager.getPreferenceValue(
                                    organization,
                                    SystemPreferenceDefinition.STUDENT_PRE_REG)
                            : "PreReg";
        }

        /**
         * Returns the enrollment snapshot for the student as of the report date.
         *
         * @param student SisStudent
         * @param reportDate PlainDate
         * @return EnrollmentSnapshot
         */

        private SimpleEnrollmentSnapshot getSnapshot(SisStudent student, PlainDate reportDate) {
            SimpleEnrollmentSnapshot snapshot = new SimpleEnrollmentSnapshot(student, reportDate,
                    m_eccData.getStudentHistoryHelper().getStudentEnrollments(student));
            return snapshot;
        }

        /**
         * Gets the spans.
         *
         * @return List
         */
        private List<StudentEnrollmentSpan> getSpans() {
            // Selecting only the enrollment spans that are after school's start date and those that
            // belong
            // to the related school districts only
            List<StudentEnrollmentSpan> spans =
                    m_eccData.getStudentHistoryHelper().getStudentEnrollmentSpans(m_student, false);
            excludeSpans(spans);
            return spans;
        }

        /**
         *
         * Eliminate student with no active spans in the context year.
         *
         * @param spans List<StudentEnrollmentSpan>
         * @param snapshot EnrollmentSnapshot
         * @return true, if is student active
         */
        private boolean isStudentActive(List<StudentEnrollmentSpan> spans, SimpleEnrollmentSnapshot snapshot) {
            Organization organization = m_eccData.getOrganization();
            boolean isEnrolled = StudentManager.isActiveStudent(organization, snapshot.getEnrollmentStatus());
            if (!spans.isEmpty()) {
                // verify there is an active span
                isEnrolled = false;
                for (StudentEnrollmentSpan span : spans) {
                    if (span.getFirstActiveDate() != null && !span.getFirstActiveDate().after(snapshot.getDate()) &&
                            (span.getLastActiveDate() == null || !span.getLastActiveDate()
                                    .before(m_eccData.getCurrentContext().getStartDate()))) {
                        isEnrolled = true;
                    }
                }
            }

            return isEnrolled;

        }

        /**
         * If the student has more than one enrollment (multiple schools/enrollment periods)
         * then we must calculate the attendance for each enrollment period separately.
         * Query for those attendance by date range.
         *
         */
        private void recalcAbsenses() {
            SisStudent student = (SisStudent) getBean();

            for (MembershipAttendance member : m_schoolList) {
                PlainDate lastDate = member.getEndDate();
                if (lastDate == null) {
                    if (m_eccData.m_endOfYearMode) {
                        lastDate = m_eccData.m_schoolEndDateInYearMapping.get(member.getSchoolOid());

                    } else {
                        lastDate = m_eccData.m_reportDate;
                    }
                }

                // Find absences days for the date range provided.
                Criteria criteria = new Criteria();
                criteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, student.getOid());
                criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, member.getBeginDate());
                criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, lastDate);
                criteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

                ReportQueryByCriteria reportQuery = new ReportQueryByCriteria(StudentAttendance.class,
                        new String[] {"SUM(ATT_PORTION_ABSENT)"}, criteria);

                Float absencesCount = Float.valueOf(0);
                ReportQueryIterator iterator = getData().getBroker().getReportQueryIteratorByQuery(reportQuery);
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
                member.setAbsent(absenceDays);
            }
        }

    } // end entity class

    /**
     * The Class MembershipAttendance.
     */
    protected static class MembershipAttendance {
        /*
         * Instance for school, dates, counts and codes.
         */
        float m_absent;
        PlainDate m_beginDate;
        PlainDate m_endDate;
        String m_enrollCode;
        int m_lastYog;
        int m_membership;
        String m_residentCode;
        int m_schoolMembership;
        String m_schoolOid;
        String m_withdrawCode;
        String m_stateExitType;
        String m_homeschoolCode;
        Object m_outsidePlacementLocationCode;
        Object m_outsidePlacementClassificationType;


        /**
         * constructor, set initial attendance and membership counts.
         *
         * @param schoolOid String
         * @param enrollCode String
         * @param withdrawCode String
         * @param residentCode String
         * @param beginDate PlainDate
         * @param endDate PlainDate
         * @param yog int
         * @param stateExitType String
         * @param outsidePlacementLocationCode Object
         * @param outsidePlacementClassificationType Object
         */
        protected MembershipAttendance(String schoolOid, String enrollCode, String withdrawCode, String residentCode,
                PlainDate beginDate, PlainDate endDate, int yog, String stateExitType, String homeschoolCode,
                Object outsidePlacementLocationCode,
                Object outsidePlacementClassificationType) {
            m_schoolOid = schoolOid;
            m_enrollCode = enrollCode;
            m_withdrawCode = withdrawCode;
            m_residentCode = residentCode;
            m_beginDate = beginDate;
            m_endDate = endDate;
            m_lastYog = yog;
            m_stateExitType = stateExitType;
            m_homeschoolCode = homeschoolCode;
            m_outsidePlacementLocationCode = outsidePlacementLocationCode;
            m_outsidePlacementClassificationType = outsidePlacementClassificationType;
        }

        /**
         * Gets the state exit type.
         *
         * @return String
         */
        protected String getStateExitType() {
            return m_stateExitType;
        }

        /**
         * @return the homeschool code, if any
         */
        protected String getHomeschoolCode() {
            return m_homeschoolCode;
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
         * Return calculated resident code.
         *
         * @return String
         */
        protected String getResidentCode() {
            return m_residentCode;
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
         * Returns the withdraw code for the students enrollment segment.
         *
         * @return String
         */
        protected String getWithdrawCode() {
            return m_withdrawCode;
        }

        /**
         * Gets the attendance.
         *
         * @return int
         */
        protected int getAttendance() {
            int absenceDays = 0;
            float absent = getAbsent();
            absenceDays = Float.valueOf(absent).intValue();

            int attendance = getMembership() - absenceDays;
            if (attendance < 0) {
                attendance = 0;
            }
            return attendance;

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

        /**
         * Gets the outside placement location code.
         *
         * @return Object
         */
        protected Object getOutsidePlacementLocationCode() {
            return m_outsidePlacementLocationCode;
        }

        /**
         * Gets the outside placement classification type.
         *
         * @return Object
         */
        protected Object getOutsidePlacementClassificationType() {
            return m_outsidePlacementClassificationType;
        }
    }

    /**
     * The Class SimpleEnrollmentSnapshot.
     */
    public static class SimpleEnrollmentSnapshot {
        private PlainDate m_date = null;
        private String m_enrollmentStatus = null;
        private SisSchool m_school = null;
        private SisStudent m_student = null;
        private int m_yog = 0;

        /**
         * Instantiates a new simple enrollment snapshot.
         *
         * @param student SisStudent
         * @param date PlainDate
         * @param enrollments List<StudentEnrollment>
         */
        public SimpleEnrollmentSnapshot(SisStudent student, PlainDate date,
                List<StudentEnrollment> enrollments) {
            m_student = student;
            m_date = date;

            StudentEnrollment e0 = null;
            StudentEnrollment e1 = null;
            if (enrollments != null) {
                for (StudentEnrollment enrollment : enrollments) {
                    if (enrollment.getEnrollmentDate().after(getDate())) {
                        e1 = enrollment;
                        break;
                    }
                    e0 = enrollment;
                }
            }
            if (e1 == null) {
                m_enrollmentStatus = m_student.getEnrollmentStatus();
                m_school = m_student.getSchool();
                m_yog = m_student.getYog();
            } else {
                if (e1.getEnrollmentType().equals(StudentEnrollment.YOG_CHANGE)) {
                    m_enrollmentStatus = e1.getStatusCode();
                    m_school = e1.getSchool();
                    m_yog = (e0 != null ? e0.getYog() : e1.getYog());
                } else if (e1.getEnrollmentType().equals(StudentEnrollment.STATUS_CHANGE) ||
                        e1.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                    m_enrollmentStatus = (e0 != null ? e0.getStatusCode() : e1.getStatusCode());
                    m_school = e1.getSchool();
                    m_yog = e1.getYog();
                } else if (e1.getEnrollmentType().equals(StudentEnrollment.ENTRY)) {
                    m_enrollmentStatus = (e0 != null ? e0.getStatusCode() : e1.getStatusCode());
                    m_school = (e0 != null ? e0.getSchool() : e1.getSchool());
                    m_yog = (e0 != null ? e0.getYog() : e1.getYog());
                }
            }
        }

        /**
         * Returns the date represented by this snapshot.
         *
         * @return PlainDate
         */
        public PlainDate getDate() {
            return m_date;
        }

        /**
         * Returns the enrollment status for the student on the date represented by this snapshot.
         *
         * @return String
         */
        public String getEnrollmentStatus() {
            return m_enrollmentStatus;
        }

        /**
         * Returns the student represented by this snapshot.
         *
         * @return Student
         */
        public SisStudent getStudent() {
            return m_student;
        }

        /**
         * Returns the school for the student on the date represented by this snapshot.
         *
         * @return School
         */
        public SisSchool getSchool() {
            return m_school;
        }

        /**
         * Returns the year of graduation for the student on the date represented by this snapshot.
         *
         * @return int
         */
        public int getYog() {
            return m_yog;
        }
    }
    /**
     * The Class RetrieveAggregateDaysAttendance.
     */
    protected class RetrieveAggregateDaysAttendance implements FieldRetriever {

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
            MembershipAttendance enrollmentSnapshot = ((EnrollmentCensusCoreEntity) entity).getMembershipAttendance();

            int aggDaysAttendance = enrollmentSnapshot.getAttendance();
            return Integer.valueOf(aggDaysAttendance);
        }
    }

    /**
     * The Class RetrieveAggregateDaysMembership.
     */
    protected class RetrieveAggregateDaysMembership implements FieldRetriever {

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
            MembershipAttendance enrollmentSnapshot = ((EnrollmentCensusCoreEntity) entity).getMembershipAttendance();

            int aggDaysMembership = enrollmentSnapshot.getMembership();
            return Integer.valueOf(aggDaysMembership);
        }
    }

    /**
     * Retrieve the count of school membership days for the student.
     */
    protected class RetrieveAltAsm implements FieldRetriever {
        private static final String CALC_ID = "ECC-ALT-ASM";

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
            SisStudent student = (SisStudent) entity.getBean();
            Object value = null;
            if (!StringUtils.isEmpty(m_doeAltAsm)) {
                value = BooleanAsStringConverter.TRUE.equals(student.getFieldValueByBeanPath(m_doeAltAsm)) ? "Y" : "N";
            }
            return value;
        }
    }
    protected class RetrieveDistrictCode implements FieldRetriever {

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
            Object code = null;
            MembershipAttendance enrollmentSnapshot = ((EnrollmentCensusCoreEntity) entity).getMembershipAttendance();

            // check that the school is not archive/inactive.
            SisSchool school = (SisSchool) getBroker().getBeanByOid(SisSchool.class, enrollmentSnapshot.getSchoolOid());

            if (school != null) {
                String adjustedDistrCode = (String) school.getFieldValueByBeanPath(m_sklFieldAdjDistr);
                code = !StringUtils.isEmpty(adjustedDistrCode)
                        ? adjustedDistrCode
                        : school.getOrganization1().getFieldValueByBeanPath(m_districtIdField);
            }

            return code;
        }
    }

    /**
     * Retrieve the date of enrollment from the student enrollment record.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEnrollmentDate implements FieldRetriever {

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
            MembershipAttendance enrollmentSnapshot = ((EnrollmentCensusCoreEntity) entity).getMembershipAttendance();
            PlainDate enrollmentDate = enrollmentSnapshot.getBeginDate();

            return enrollmentDate;
        }
    }

    /**
     * Retrieve the type of enrollment from the student enrollment record.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEnrollmentType implements FieldRetriever {
        private static final String STATE_OUTSIDE_PLACEMENT_TYPE = "O";
        private static final String STATE_REGULAR_PLACEMENT_TYPE = "R";
        private static final String STATE_HOMESCHOOLING_TYPE = "H";

        private static final String HOME_SCHOOL_STRING = "900";

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
            MembershipAttendance enrollmentSnapshot = ((EnrollmentCensusCoreEntity) entity).getMembershipAttendance();
            String outsidePlacementClassificationType =
                    (String) enrollmentSnapshot.getOutsidePlacementClassificationType();

            String stateEnrollmentType = null;

            SisSchool school = (SisSchool) getBroker().getBeanByOid(SisSchool.class, enrollmentSnapshot.getSchoolOid());

            if ("1".equals(school.getFieldValueByBeanPath(m_outsidePlacementString))) {
                stateEnrollmentType = STATE_OUTSIDE_PLACEMENT_TYPE;
                // need to change to be the student enrollment

                String locationCode = (String) enrollmentSnapshot.getOutsidePlacementLocationCode();
                ReferenceCode refSchoolCode = m_opLocationCodes.get(locationCode);
                if (refSchoolCode != null) {
                    String schoolCodeOut = refSchoolCode.getStateCode();
                    if (!StringUtils.isEmpty(schoolCodeOut) && schoolCodeOut.contains(HOME_SCHOOL_STRING)) {
                        stateEnrollmentType = STATE_HOMESCHOOLING_TYPE;
                    }
                }
            } else {
                stateEnrollmentType = STATE_REGULAR_PLACEMENT_TYPE;
            }

            if (m_opClassificationRefCodes.containsKey(outsidePlacementClassificationType)) {
                ReferenceCode refCode = m_opClassificationRefCodes.get(outsidePlacementClassificationType);
                if (!StringUtils.isEmpty(refCode.getStateCode())) {
                    stateEnrollmentType = refCode.getStateCode();
                }
            }

            // Update the RI enrollment type field based on user input
            if (m_enrollmentTypeBeanPath != null && !StringUtils.isEmpty(stateEnrollmentType)) {
                if (!Integer.valueOf(ENROLLMENT_TYPE_DO_NOT_UPDATE).equals(m_updateStudentEnrollmentType)) {
                    String userCode = lookupUserValue(Student.class, m_enrollmentTypeBeanPath, stateEnrollmentType);

                    Student student = (Student) entity.getBean();
                    student.setFieldValueByBeanPath(m_enrollmentTypeBeanPath, userCode);
                    getBroker().saveBeanForced(student);
                }
            }

            return stateEnrollmentType;
        }
    }

    /**
     * Retrieve the student withdrawal date from the student enrollment record.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveExitDate implements FieldRetriever {

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
            MembershipAttendance enrollmentSnapshot = ((EnrollmentCensusCoreEntity) entity).getMembershipAttendance();

            PlainDate startDate = enrollmentSnapshot.getBeginDate();
            PlainDate exitDate = enrollmentSnapshot.getEndDate();

            PlainDate returnExitDate = null;
            if (exitDate != null && startDate != null) {
                if (!startDate.after(exitDate)) {
                    returnExitDate = exitDate;

                }
            }

            if (m_endOfYearMode && (returnExitDate == null)) {
                returnExitDate = m_schoolEndDateInYearMapping.get(enrollmentSnapshot.getSchoolOid());
            }

            if (exitDate != null && exitDate.after(m_reportDate)) {
                returnExitDate = null;
            }
            // set the exit date on the enrollment snapshot?
            return returnExitDate;
        }
    }

    /**
     * Retrieve the student withdrawal code from the enrollment record.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveExitType implements FieldRetriever {
        private static final String STATE_GRADUATED_CODE = "15";
        private static final String STATE_OTHER_SCHOOL_CODE = "14";
        private static final String STATE_NEXT_GRADE_CODE = "30";
        private static final String STATE_RETAINED_CODE = "31";
        private static final String HIGH_SCHOOL_SENIOR_GRADE_LEVEL = "12";

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
            MembershipAttendance enrollmentSnapshot = ((EnrollmentCensusCoreEntity) entity).getMembershipAttendance();

            PlainDate startDate = enrollmentSnapshot.getBeginDate();
            PlainDate exitDate = enrollmentSnapshot.getEndDate();

            String exitType = null;
            if (exitDate != null && startDate != null) {
                if (!startDate.after(exitDate)) {
                    exitType = enrollmentSnapshot.getStateExitType();
                }
            }

            // end of year mode
            if (m_endOfYearMode && (exitType == null)) {
                // check the next school value for the student
                SisStudent student = ((EnrollmentCensusCoreEntity) entity).getStudent();
                SisSchool nextSchool = student.getNextSchool();
                // nextschool
                if (nextSchool != null) {
                    // student's current school is the same as the next school
                    if (student.getSchool().equals(nextSchool)) {
                        exitType = STATE_NEXT_GRADE_CODE;
                    } else {
                        exitType = STATE_OTHER_SCHOOL_CODE;
                    }
                } else {
                    // graduating seniors are in grade 12 and have an empty next school
                    if (student.getGradeLevel().equals(HIGH_SCHOOL_SENIOR_GRADE_LEVEL)) {
                        exitType = STATE_GRADUATED_CODE;
                    }
                }

                if (m_retainedStudents.contains(student)) {
                    exitType = STATE_RETAINED_CODE;
                }
            }

            if (exitType == null || (exitDate != null && exitDate.after(m_reportDate))) {
                exitType = "";
            }
            return exitType;
        }
    }

    /**
     * Retrieve the student homeschool code from the student enrollment record.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveHomeschoolCode implements FieldRetriever {

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
            MembershipAttendance enrollmentSnapshot = ((EnrollmentCensusCoreEntity) entity).getMembershipAttendance();
            if (enrollmentSnapshot.getHomeschoolCode() == null || enrollmentSnapshot.getHomeschoolCode().matches("")) {
                return "";
            }
            ReferenceCode rcd = m_homeschoolCodes.get(enrollmentSnapshot.getHomeschoolCode());
            return rcd.getStateCode();
        }
    }

    /**
     * Retrieve the grade level of the student.
     * If the YOG at the time of enrollment is different than the student,
     * recalculate the grade level at the time of enrollment.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveGradeLevel implements FieldRetriever {

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
            String gradeLevel = (String) getProperty(entity.getBean(), field.getBeanPath());
            SisStudent student = (SisStudent) entity.getBean();
            MembershipAttendance membAtt = ((EnrollmentCensusCoreEntity) entity).getMembershipAttendance();

            if (membAtt.getLastYog() != student.getYog() && membAtt.getLastYog() != 0) {
                int schoolYear = getCurrentContext().getSchoolYear();
                List<String> gradeLevels =
                        StudentManager.getMatchingGradeLevels(StudentManager.getMaxGradeLevel(getBroker()),
                                membAtt.getLastYog(), schoolYear, m_gradeLevelMap);

                if (m_gradeCodeMap == null) {
                    gradeLevel = gradeLevels.get(0);
                } else if (gradeLevels.contains(gradeLevel)) {
                    // use grade level code
                } else {
                    // gradeLevels list can contain disabled reference codes. Find one that is not
                    // disabled.
                    for (String code : gradeLevels) {
                        ReferenceCode refCode = m_gradeCodeMap.get(code);
                        if (!refCode.getDisabledIndicator()) {
                            gradeLevel = code;
                            break;
                        }
                    }
                }
            }

            return gradeLevel;
        }
    }

    /**
     * Retrieve the special ed. status.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveIepStatus implements FieldRetriever {
        private static final String IEP_SERVICES_ONLY_TYPE = "S";

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
            String returnValue = NO_INDICATOR;

            String enrollmentType = entity.getFieldValue(FIELD_ENROLL_TYPE);
            if (IEP_SERVICES_ONLY_TYPE.equals(enrollmentType)) {
                returnValue = IEP_SERVICES_ONLY_TYPE;
            } else {
                // Lookup student sped status.
                SisStudent student = ((EnrollmentCensusCoreEntity) entity).getStudent();
                String spedStatus = student.getSpedStatusCode();
                PlainDate exitDate = student.getSpedExitDate();
                String stateCode = data.lookupStateValue(SisStudent.class, SisStudent.COL_SPED_STATUS_CODE, spedStatus);

                if (!NO_INDICATOR.equals(stateCode)) {
                    returnValue = stateCode;
                } else {
                    // Lookup exit date. Exit this year indicates active.
                    if (exitDate != null && exitDate.after(m_districtFirstSessionDate)) {
                        returnValue = YES_INDICATOR;
                    }
                }
            }
            return returnValue;
        }
    }

    /**
     * Retrieve the student preferred names from the student record.
     *
     * @author X2 Development Corporation
     */
    protected class RetrievePreferredName implements FieldRetriever {

        /**
         * Gets the field value. If the psn alias is not found use an std alias.
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
            String value = null;
            // try person alias
            Person person = ((Student) entity.getBean()).getPerson();
            DataDictionaryField dictionaryField =
                    getDataDictionary().findDataDictionaryFieldByAlias((String) field.getParameter());
            if (dictionaryField != null) {
                return person.getFieldValueByBeanPath(dictionaryField.getJavaName());
            }

            // try student alias
            String alias = ((String) field.getParameter()).replace("-psn-", "-std-");
            dictionaryField = getDataDictionary().findDataDictionaryFieldByAlias(alias);
            if (dictionaryField != null) {
                return entity.getBean().getFieldValueByBeanPath(dictionaryField.getJavaName());
            }

            return value;
        }
    }

    /**
     * Return a race indicator if a student has a requested race code.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveRaceIndicator implements FieldRetriever {

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
            Collection<Race> personRaceCodes = ((EnrollmentCensusCoreEntity) entity).getPersonRaceCodes();
            String federalRefCodeParam = (String) field.getParameter();

            String raceIndictor = NO_INDICATOR;

            if (personRaceCodes != null) {
                for (Race personRace : personRaceCodes) {
                    String raceCode = personRace.getRaceCode();
                    ReferenceCode refCode = m_raceRefCodes.get(raceCode);
                    if (refCode != null) {

                        if (federalRefCodeParam.equals(refCode.getFederalCode())) {
                            raceIndictor = YES_INDICATOR;
                        }
                    }
                }
            }

            return raceIndictor;
        }
    }

    /**
     * Retrieve the date of enrollment from the student enrollment record.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveResidentCode implements FieldRetriever {

        protected static final String CALC_ID = "RESIDENT-CODE";

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
            MembershipAttendance enrollmentSnapshot = ((EnrollmentCensusCoreEntity) entity).getMembershipAttendance();
            return enrollmentSnapshot.getResidentCode();
        }
    }

    /**
     * The Class RetrieveSchoolCode.
     */
    protected class RetrieveSchoolCode implements FieldRetriever {

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
            MembershipAttendance enrollmentSnapshot = ((EnrollmentCensusCoreEntity) entity).getMembershipAttendance();

            // check that the school is not archive/inactive.
            SisSchool school = (SisSchool) getBroker().getBeanByOid(SisSchool.class, enrollmentSnapshot.getSchoolOid());

            String stateSchoolId = "";
            stateSchoolId = (String) school.getFieldValueByBeanPath(m_sklIdField);

            return stateSchoolId;
        }
    }

    /**
     * The Class RetrieveSchoolCodeOut.
     */
    protected class RetrieveSchoolCodeOut implements FieldRetriever {

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
            MembershipAttendance enrollmentSnapshot = ((EnrollmentCensusCoreEntity) entity).getMembershipAttendance();

            // check that the school is not archive/inactive.
            Object value = null;
            String locationCode = (String) enrollmentSnapshot.getOutsidePlacementLocationCode();
            ReferenceCode refCode = m_opLocationCodes.get(locationCode);
            if (refCode != null) {
                value = refCode.getStateCode();
            }
            return value;
        }
    }

    /**
     * Retrieve the count of school membership days for the student.
     */
    protected class RetrieveSchoolDays implements FieldRetriever {

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
            SisStudent student = (SisStudent) entity.getBean();
            MembershipAttendance membership = ((EnrollmentCensusCoreEntity) entity).getMembershipAttendance();
            EnrollmentCensusCore eccData = (EnrollmentCensusCore) data;
            SisSchool school = (SisSchool) eccData.getBroker().getBeanByOid(SisSchool.class, membership.getSchoolOid());
            int count = 0;

            Set set = eccData.getCalendarDays(school, student.getCalendarCode(), true);
            if (set == null) {
                System.out.println("calendar days is null for student");
            } else {
                if (set.size() == 0) {
                    System.out.println("set is empty");
                }
                count = set.size();
            }

            return Integer.valueOf(count);
        }
    }

    /*
     * Constants
     */
    private static final String ALIAS_DOE_EXCLUDE_ENR = "DOE EXCLUDE ENR";
    private static final String ALIAS_DOE_ALT_ASM = "DOE ALT Assessment";
    private static final String ALIAS_ENR_RES_OVERRIDE = "RI Resident District Override";
    private static final String ALIAS_STD_NON_RES_DISTR_CODE = "Non Resident District Code";
    private static final String ALIAS_ENR_HOMESCHOOL_CODE = "all-enr-HomeSchoolCode";
    public static final String END_OF_YEAR_PARAM = "endOfYear";
    public static final int ENROLLMENT_TYPE_DO_NOT_UPDATE = 0;
    public static final int ENROLLMENT_TYPE_UPDATE_ONLY = 1;
    public static final int ENROLLMENT_TYPE_CLEAR_AND_UPDATE = 2;
    public static final String ORGANIZATION_PARAM = "orgOid";
    public static final String QUERY_BY_PARAM = "queryBy";
    public static final String QUERY_STRING_PARAM = "queryString";
    public static final String REPORT_DATE = "reportDate";
    public static final String RETAINED_SNAPSHOT_NAME_PARAM = "retainedSnapshotName";
    public static final String SASID_STUDENTS_ONLY_PARAM = "sasidStudentsOnly";
    public static final String SORT_PARAM = "sort";
    public static final String START_OF_YEAR_PARAM = "startOfYear";
    public static final String UPDATE_ENROLLMENT_TYPE_PARAM = "updateEnrollmentType";

    protected static final String FIELD_ENROLL_TYPE = "ENROLL_TYPE";
    protected static final String NO_INDICATOR = "N";
    protected static final String YES_INDICATOR = "Y";

    private static final String DEFAULT_CALENDAR_NAME = "Standard";
    private static final String EXCLUDE_ALIAS_STD = "DOE EXCLUDE STD";
    private static final String LUNCH_DESCRIPTION = "Lunch Description";
    private static final String OUTSIDE_PLACEMENT_CLASSIFICATION = "OP Classification";
    private static final String OUTSIDE_PLACEMENT_LOCATION_CODE = "OP Location Code";
    private static final String OUTSIDE_PLACEMENT_SCHOOL_FIELD = "Outside Placement School";
    private static final String STATE_EXIT_TYPE = "RI Exit Type";
    private static final String STUDENT_ENROLLMENT_TYPE_ALIAS = "DOE STD ENR TYPE";

    /**
     * Class members
     */
    protected Map<String, Float> m_absences;
    protected PlainDate m_districtFirstDate;
    protected PlainDate m_districtFirstSessionDate;
    protected String m_doeAltAsm;
    protected boolean m_endOfYearMode;
    protected EnrollmentManager m_enrollmentManager;
    protected String m_enrollmentTypeBeanPath;
    protected String m_excludeStdField;
    protected String m_fieldEnrResCode;
    protected String m_fieldEnrHomeschoolCode;
    protected String m_fieldStdResCode;
    protected Map<String, ReferenceCode> m_gradeCodeMap;
    protected TreeMap<Integer, List<String>> m_gradeLevelMap;
    protected StudentHistoryHelper m_helper;
    protected String m_lunchDescriptionBeanPath;
    protected String m_opClassificationBeanPath;
    protected Map<String, ReferenceCode> m_opClassificationRefCodes;
    protected ReferenceCode m_opEvalClassification;
    protected Map<String, ReferenceCode> m_opLocationCodes;
    protected Map<String, ReferenceCode> m_homeschoolCodes;
    protected String m_excludeEnrollment;
    protected String m_orgFieldStr = null;
    protected String m_orgOid = null;
    protected String m_outsidePlacementLocationCodeBeanPath;
    protected String m_outsidePlacementString;
    protected Map<String, Collection<Race>> m_raceCodeMap;
    protected Map<String, ReferenceCode> m_raceRefCodes;
    protected PlainDate m_reportDate;
    protected Collection<SisStudent> m_retainedStudents;
    protected boolean m_sasidStudentsOnly;
    protected Map<String, PlainDate> m_schoolEndDateInYearMapping = null;
    protected Map<String, Integer> m_schoolMembership;
    protected HashMap m_schoolsToCalendars;
    protected HashMap m_schoolsToCalendarsFull;
    protected boolean m_startOfYearMode;
    protected String m_stateExitType;
    protected Integer m_updateStudentEnrollmentType = Integer.valueOf(0);
    protected String m_withdrawalCodeRefTbl;

    private Map<String, Map<String, PlainDate>> m_firstSchoolYearDate = new HashMap();

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
        initialRaceRefCodes();
        m_reportDate = (PlainDate) getParameter(REPORT_DATE);

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        m_opLocationCodes = getReferenceCodesForAlias(OUTSIDE_PLACEMENT_LOCATION_CODE);
        m_homeschoolCodes = getReferenceCodesForAlias(ALIAS_ENR_HOMESCHOOL_CODE);
        // HERE
        m_opClassificationRefCodes = getReferenceCodesForAlias(OUTSIDE_PLACEMENT_CLASSIFICATION);
        m_opEvalClassification = getEvalReferenceCode();
        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        m_schoolsToCalendars = new HashMap();
        m_schoolsToCalendarsFull = new HashMap();

        SisOrganization district = (SisOrganization) OrganizationManager.getRootOrganization(getBroker());
        DistrictCalendar districtCalender =
                CalendarManager.getDistrictInSessionStartEndDate(district, getCurrentContext(), true, getBroker());
        m_districtFirstSessionDate = districtCalender.getDate();

        m_schoolEndDateInYearMapping = getMapSchoolInSessionStartEndDate(false);

        m_districtFirstDate = getCurrentContext().getStartDate();

        m_endOfYearMode = false;
        // check for running the report for year end
        Boolean endOfYear = (Boolean) getParameter(END_OF_YEAR_PARAM);
        if (endOfYear != null) {
            m_endOfYearMode = endOfYear.booleanValue();
        }

        m_sasidStudentsOnly = true;
        Boolean sasidStudentsOnly = (Boolean) getParameter(SASID_STUDENTS_ONLY_PARAM);
        if (sasidStudentsOnly != null) {
            m_sasidStudentsOnly = sasidStudentsOnly.booleanValue();
        }

        m_startOfYearMode = false;
        // check for running the report for start of year
        Boolean startOfYear = (Boolean) getParameter(START_OF_YEAR_PARAM);
        if (startOfYear != null) {
            m_startOfYearMode = startOfYear.booleanValue();
        }

        if (m_endOfYearMode) {
            String retainedSnapshot = (String) getParameter(RETAINED_SNAPSHOT_NAME_PARAM);
            if (retainedSnapshot == null) {
                addSetupError("required parameter missing",
                        "retained snapshot name is required when running in end of year mode");
            } else {
                Criteria retainedSnapshotCriteria = new X2Criteria();
                addRecordSetCriteria(retainedSnapshotCriteria, retainedSnapshot);

                QueryByCriteria retainedQuery = new QueryByCriteria(SisStudent.class, retainedSnapshotCriteria);

                m_retainedStudents = getBroker().getCollectionByQuery(retainedQuery);
            }
        }

        m_updateStudentEnrollmentType = (Integer) getParameter(UPDATE_ENROLLMENT_TYPE_PARAM);

        // Clear the RI enrollment type field based on user input
        if (Integer.valueOf(ENROLLMENT_TYPE_CLEAR_AND_UPDATE).equals(m_updateStudentEnrollmentType)) {
            clearEnrollmentType();
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

            Integer sort = (Integer) getParameter(SORT_PARAM);
            switch (sort == null ? 0 : sort.intValue()) {
                case 0: // Name
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 1: // YOG
                    studentQuery.addOrderByAscending(SisStudent.COL_YOG);
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 2: // School
                    studentQuery.addOrderByAscending(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
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
            setEntityClass(EnrollmentCensusCoreEntity.class);

            // Load membership, attendance, enrollment and race codes maps.
            loadAbsenceDaysMaps(studentCriteria);

            SubQuery subQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, studentCriteria);
            Criteria raceCriteria = new Criteria();
            raceCriteria.addIn(Race.COL_PERSON_OID, subQuery);

            QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
            m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 100);

            // Build maps of retriever functions
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();

            calcs.put("ECC-ADA", new RetrieveAggregateDaysAttendance());
            calcs.put("ECC-ADM", new RetrieveAggregateDaysMembership());
            calcs.put("ECC-EnrollmentDate", new RetrieveEnrollmentDate());
            calcs.put("ECC-EnrollmentType", new RetrieveEnrollmentType());
            calcs.put("ECC-ExitDate", new RetrieveExitDate());
            calcs.put("ECC-ExitType", new RetrieveExitType());
            calcs.put("ECC-GradeLevel", new RetrieveGradeLevel());
            calcs.put("ECC-HomeschoolCode", new RetrieveHomeschoolCode());
            calcs.put("ECC-IepStatus", new RetrieveIepStatus());
            calcs.put("ECC-PreferredName", new RetrievePreferredName());
            calcs.put("ECC-RaceIndicator", new RetrieveRaceIndicator());
            calcs.put("ECC-DistrictCode", new RetrieveDistrictCode());
            calcs.put("ECC-SchoolCode", new RetrieveSchoolCode());
            calcs.put("ECC-SchoolCodeOut", new RetrieveSchoolCodeOut());
            calcs.put("ECC-SchoolDays", new RetrieveSchoolDays());
            calcs.put(RetrieveAltAsm.CALC_ID, new RetrieveAltAsm());
            calcs.put(RetrieveResidentCode.CALC_ID, new RetrieveResidentCode());
            super.addCalcs(calcs);
        }
    }

    /**
     * Returns the days-in-session for the given school and calendar combination.
     *
     * @param school SisSchool
     * @param calendar String
     * @param full boolean
     * @return Set of PlainDate objects
     */
    protected Set getCalendarDays(SisSchool school, String calendar, boolean full) {
        if (!m_schoolsToCalendars.containsKey(school.getOid())) {
            Schedule schedule = school.getActiveSchedule();
            PlainDate startDate = null;
            PlainDate endDate = null;
            if (schedule != null) {
                startDate = school.getActiveSchedule().getStartDate();
                endDate = school.getActiveSchedule().getEndDate();
            } else {
                AppGlobals.getLog().log(Level.WARNING,
                        " ### school does not have an active schedule:" + school.getName());
                startDate = getCurrentContext().getStartDate();
                endDate = getCurrentContext().getEndDate();
            }
            Map calendarData = m_enrollmentManager.getCalendarLookup(school, startDate, m_reportDate);
            m_schoolsToCalendars.put(school.getOid(), calendarData);
            calendarData = m_enrollmentManager.getCalendarLookup(school, startDate, endDate);
            m_schoolsToCalendarsFull.put(school.getOid(), calendarData);
        }

        // Use either the school calendar map for YTD or the full map for full year count.
        Map<String, Set<PlainDate>> workingCalendarMap = m_schoolsToCalendars;
        if (full) {
            workingCalendarMap = m_schoolsToCalendarsFull;
        }

        Set set = (Set) ((Map) workingCalendarMap.get(school.getOid())).get(calendar);
        if (set == null) {
            set = (Set) ((Map) workingCalendarMap.get(school.getOid())).get(DEFAULT_CALENDAR_NAME);
        }
        if (set == null) {
            AppGlobals.getLog().log(Level.WARNING, " ### school " + school.getName()
                    + " does not have a calendar named " + calendar + " or " + DEFAULT_CALENDAR_NAME);
            if (((Map) workingCalendarMap.get(school.getOid())).size() == 1) {
                Set entrySet = ((Map) workingCalendarMap.get(school.getOid())).entrySet();
                for (Iterator i = entrySet.iterator(); i.hasNext();) {
                    set = (Set) ((Map.Entry) i.next()).getValue();
                }
            }
        }

        return set;
    }

    /**
     * Gets the first school year date.
     *
     * @param school School
     * @param calendarId String
     * @return Plain date
     */
    protected PlainDate getFirstSchoolYearDate(School school, String calendarId) {
        Map<String, PlainDate> calendarMap = m_firstSchoolYearDate.get(school.getOid());
        if (calendarMap == null) {
            calendarMap = new HashMap();
            m_firstSchoolYearDate.put(school.getOid(), calendarMap);
        }
        PlainDate date = calendarMap.get(calendarId);
        if (date == null) {
            List<SchoolCalendar> schoolCalendars = (List<SchoolCalendar>) school.getSchoolCalendars();
            SchoolCalendar studentCalendar = null;

            for (SchoolCalendar schoolCalendar : schoolCalendars) {
                if (schoolCalendar.getCalendarId() != null && schoolCalendar.getCalendarId().equals(calendarId) &&
                        schoolCalendar.getDistrictContext() != null
                        && schoolCalendar.getDistrictContext().equals(getCurrentContext())) {
                    studentCalendar = schoolCalendar;
                    break;
                }
            }

            SchoolCalendarDate firstInSessionDate = null;
            if (studentCalendar != null) {
                firstInSessionDate = CalendarManager.getFirstInSessionDate(studentCalendar, getBroker());
            }

            date = firstInSessionDate != null ? firstInSessionDate.getDate() : m_districtFirstSessionDate;
            calendarMap.put(calendarId, date);
        }
        return date;
    }

    /**
     * Gets the membership total.
     *
     * @param student SisStudent
     * @param sessionDays Set
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @param school SisSchool
     * @return int
     */
    protected int getMembershipTotal(SisStudent student,
                                     Set sessionDays,
                                     PlainDate startDate,
                                     PlainDate endDate,
                                     SisSchool school) {
        int totalMembership = 0;

        if (student != null && sessionDays != null && startDate != null && endDate != null) {
            List<StudentEnrollment> enrollments = getOrderedEnrollment(student, startDate, endDate, school, true);

            Calendar calendar = Calendar.getInstance(Locale.getDefault());
            calendar.setTime(startDate);
            boolean isMemberToday = true;

            while (!calendar.getTime().after(endDate)) {
                boolean isMemberTomorrow = isMemberToday;

                if (!enrollments.isEmpty()) {
                    StudentEnrollment topEnrollmentRecord = enrollments.get(0);
                    while (topEnrollmentRecord.getEnrollmentDate().equals(calendar.getTime())) {
                        if (topEnrollmentRecord.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                            isMemberToday = m_enrollmentManager.getWithdrawalIsMemberDay();
                            isMemberTomorrow = false;
                        } else {
                            isMemberToday = m_enrollmentManager.getEntryIsMemberDay();
                            isMemberTomorrow = true;
                        }

                        enrollments.remove(0);

                        if (enrollments.isEmpty()) {
                            break;
                        }

                        topEnrollmentRecord = enrollments.get(0);
                    }
                }

                if (sessionDays.contains(calendar.getTime())) {
                    if (isMemberToday) {
                        totalMembership++;
                    }
                }

                calendar.add(Calendar.DATE, 1);
                isMemberToday = isMemberTomorrow;
            }
        }

        return totalMembership;
    }

    /**
     * This method returns the StudentHistoryHelper.
     *
     * @return Student history helper
     */
    protected StudentHistoryHelper getStudentHistoryHelper() {
        return m_helper;
    }

    /**
     * Find session date.
     *
     * @param student SisStudent
     * @param school SisSchool
     * @param enrollmentDate PlainDate
     * @param after boolean
     * @return PlainDate
     */
    protected PlainDate findSessionDate(SisStudent student, SisSchool school, PlainDate enrollmentDate, boolean after) {
        PlainDate nearestDate = null;
        Set<PlainDate> insessionDates = getCalendarDays(school, student.getCalendarCode(), true);
        if (insessionDates == null && !"Standard".equals(student.getCalendarCode())) {
            insessionDates = getCalendarDays(school, "Standard", true);
        }
        if (insessionDates != null) {
            for (PlainDate date : insessionDates) {
                if (after && date.after(enrollmentDate)) {
                    if (nearestDate == null || nearestDate.after(date)) {
                        nearestDate = date;
                    }
                } else if (!after && date.before(enrollmentDate)) {
                    if (nearestDate == null || nearestDate.before(date)) {
                        nearestDate = date;
                    }
                }
            }
        }
        if (nearestDate == null) {
            nearestDate = enrollmentDate;
        }
        return nearestDate;
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
     * Clear the RI enrollment type field for the entire organization or the selected school.
     */
    private void clearEnrollmentType() {
        if (m_enrollmentTypeBeanPath != null) {
            X2Criteria clearCriteria = new X2Criteria();

            if (isSchoolContext()) {
                clearCriteria.addEqualTo(Student.COL_SCHOOL_OID,
                        getSchool().getOid());
            } else {
                if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
                    clearCriteria.addEqualTo(Student.REL_SCHOOL + PATH_DELIMITER +
                            m_orgFieldStr,
                            m_orgOid);
                }
            }

            UpdateQuery updateQuery = new UpdateQuery(Student.class, clearCriteria, m_enrollmentTypeBeanPath, null);
            int numberCleared = getBroker().executeUpdateQuery(updateQuery);
            AppGlobals.getLog().info("Cleared " + numberCleared + " RI enrollment type fields");
        }
    }

    /**
     * Calculate resident code.
     *
     * @param enr StudentEnrollment
     * @return String
     */
    private String getCalculatedResidentCode(StudentEnrollment enr) {
        String codeToReturn = null;
        if (enr != null) {
            String enrCode = (String) enr.getFieldValueByBeanPath(m_fieldEnrResCode);
            if (!StringUtils.isEmpty(enrCode)) {
                codeToReturn = lookupStateValue(StudentEnrollment.class, m_fieldEnrResCode, enrCode);
            }
            if (StringUtils.isEmpty(codeToReturn)) {
                SisStudent std = enr.getStudent();
                if (std != null) {
                    String resCode = (String) std.getFieldValueByBeanPath(m_fieldStdResCode);
                    if (!StringUtils.isEmpty(resCode)) {
                        codeToReturn = lookupStateValue(SisStudent.class, m_fieldStdResCode, resCode);
                    }
                }
            }
        }
        return codeToReturn;
    }

    /**
     * Lookup the reference codes for the outplacement classification.
     * Find the "Evaluation" code.
     *
     * @return ReferenceCode
     */
    private ReferenceCode getEvalReferenceCode() {
        ReferenceCode evalRefCode = null;
        Collection<ReferenceCode> opClassificationCodes = m_opClassificationRefCodes.values();
        for (ReferenceCode classificationCode : opClassificationCodes) {
            if (classificationCode.getStateCode() == null) {
                addSetupError("missing state code",
                        "outside placement classification reference table missing state code values");

            } else if (classificationCode.getStateCode().equalsIgnoreCase("Evaluation")) {
                evalRefCode = classificationCode;
                break;
            }
        }

        return evalRefCode;
    }

    /**
     * Gets the map school in session start end date.
     *
     * @param start boolean
     * @return Map
     */
    private Map<String, PlainDate> getMapSchoolInSessionStartEndDate(boolean start) {

        Map<String, PlainDate> schoolEndDateInYearMapping = new HashMap<String, PlainDate>();
        List<String> shoolOids = new ArrayList<String>();
        if (isSchoolContext()) {
            shoolOids.add(getSchool().getOid());
        } else {
            Criteria schoolCriteria = getSchoolCriteria();
            QueryByCriteria byCriteria = new QueryByCriteria(School.class, schoolCriteria);
            QueryIterator iter = getBroker().getIteratorByQuery(byCriteria);
            try {
                while (iter.hasNext()) {
                    School school = (School) iter.next();
                    shoolOids.add(school.getOid());
                }
            } finally {
                iter.close();
            }
        }

        for (String shoolOid : shoolOids) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                    SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
            criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                    SchoolCalendar.COL_SCHOOL_OID, shoolOid);
            criteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);


            QueryByCriteria query = new QueryByCriteria(SchoolCalendarDate.class, criteria);
            if (start) {
                query.addOrderByAscending(SchoolCalendarDate.COL_DATE);
            } else {
                query.addOrderByDescending(SchoolCalendarDate.COL_DATE);
            }

            // The first bean in the query is the last InSession date in the calendar
            SchoolCalendarDate schoolCalendarDate = (SchoolCalendarDate) getBroker().getBeanByQuery(query);
            if (schoolCalendarDate != null) {
                PlainDate shoolEndYearDate = schoolCalendarDate.getDate();
                schoolEndDateInYearMapping.put(shoolOid, shoolEndYearDate);
            }
        }
        return schoolEndDateInYearMapping;
    }

    /**
     * Gets the ordered enrollment.
     *
     * @param student SisStudent
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @param school SisSchool
     * @param ascending boolean
     * @return List
     */
    public List<StudentEnrollment> getOrderedEnrollment(SisStudent student,
                                                        PlainDate startDate,
                                                        PlainDate endDate,
                                                        SisSchool school,
                                                        boolean ascending) {
        List<StudentEnrollment> orderedEnrollments = new LinkedList();

        for (StudentEnrollment enrollment : m_helper.getStudentEnrollments(student)) {
            if (!(enrollment.getEnrollmentType().equals(StudentEnrollment.ENTRY)
                    || enrollment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL))) {
                continue;
            }
            if (startDate != null && enrollment.getEnrollmentDate() != null
                    || enrollment.getEnrollmentDate().before(startDate)) {
                continue;
            }
            if (endDate != null && enrollment.getEnrollmentDate() != null
                    || enrollment.getEnrollmentDate().after(endDate)) {
                continue;
            }
            if (school != null && !enrollment.getSchoolOid().equals(school.getOid())) {
                continue;
            }
            if (ascending) {
                orderedEnrollments.add(0, enrollment);
            } else {
                orderedEnrollments.add(enrollment);
            }
        }
        return orderedEnrollments;
    }

    /**
     * Gets the reference codes for alias.
     *
     * @param alias String
     * @return Map
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
                Criteria criteria = ReferenceManager.getCodesCriteria(refTable.getOid(),
                        null, false, true, false, getBroker().getPersistenceKey());

                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
                refCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 128);
            }
        }

        return refCodes;
    }

    /**
     * Builds a criteria for the students that should be reported (not considering user input
     * parameters).
     *
     * @return Criteria
     */
    private Criteria getReportingCriteria() {
        /*
         * Select students with selected school or organization,
         * or students with enrollment activity (E,W) in the school
         * this year in the selected school or organization.
         */
        // enrollment activity list (E,W).
        List<String> enrollTypes = new ArrayList<String>();
        enrollTypes.add(StudentEnrollment.WITHDRAWAL);
        enrollTypes.add(StudentEnrollment.ENTRY);

        // Get organization Oid and level and field.
        m_orgOid = (String) getParameter(ORGANIZATION_PARAM);
        SisOrganization organization = null;
        OrganizationDefinition def = null;
        if (!StringUtils.isEmpty(m_orgOid)) {
            organization = (SisOrganization) getBroker().getBeanByOid(SisOrganization.class, m_orgOid);
            if (organization != null) {
                def = organization.getOrganizationDefinition();
                m_orgFieldStr = "organization" + Integer.toString(def.getLevel() + 1) + "Oid";
            }
        }

        // With Enrollment records within the active date range and of the type E,W.
        X2Criteria activityCriteria = new X2Criteria();
        activityCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_districtFirstSessionDate);
        activityCriteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, enrollTypes);

        // Look up school or organization level enrollment records.
        if (isSchoolContext()) {
            activityCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else {
            if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
                activityCriteria.addEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER +
                        m_orgFieldStr,
                        m_orgOid);
            }
            activityCriteria.addNotEqualTo(
                    StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            activityCriteria.addNotEqualTo(
                    StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }

        // Check exclude student from state reporting flag if present.
        if (!StringUtils.isEmpty(m_excludeStdField)) {
            activityCriteria.addNotEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER +
                    m_excludeStdField,
                    BooleanAsStringConverter.TRUE);
        }

        // check sasid students only
        if (m_sasidStudentsOnly) {
            activityCriteria.addNotEmpty(StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID,
                    getBroker().getPersistenceKey());
        }

        SubQuery enrQuery = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, activityCriteria);

        // Get the students current primary school, Or in students with enrollment activity.
        X2Criteria enrollCriteria = new X2Criteria();
        enrollCriteria.addIn(X2BaseBean.COL_OID, enrQuery);

        // build the query for students to report.

        X2Criteria activeCriteria = new X2Criteria();
        activeCriteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), Student.COL_ENROLLMENT_STATUS));
        if (isSchoolContext()) {
            activeCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
                activeCriteria.addEqualTo(m_orgFieldStr, m_orgOid);
            }
            activeCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            activeCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }

        // Check exclude student from state reporting flag if present.
        if (!StringUtils.isEmpty(m_excludeStdField)) {
            activeCriteria.addNotEqualTo(m_excludeStdField, BooleanAsStringConverter.TRUE);
        }

        X2Criteria reportingCriteria = new X2Criteria();
        reportingCriteria.addAndCriteria(activeCriteria);
        reportingCriteria.addOrCriteria(enrollCriteria);

        return reportingCriteria;
    }

    /**
     * Gets the school criteria.
     *
     * @return Criteria
     */
    private Criteria getSchoolCriteria() {
        Criteria schoolCriteria = new Criteria();
        if (isSchoolContext()) {
            schoolCriteria.addEqualTo(X2BaseBean.COL_OID,
                    getSchool().getOid());
        } else {
            if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
                schoolCriteria.addEqualTo(m_orgFieldStr, m_orgOid);
            }
        }

        return schoolCriteria;
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
        userCriteria.addAndCriteria(getReportingCriteria());

        if (m_sasidStudentsOnly) {
            userCriteria.addNotEmpty(SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());
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

        AppGlobals.getLog().log(Level.WARNING, "!!! userCriteria: " + userCriteria.toString());

        return userCriteria;
    }

    /**
     * Initialize the Person Race reference codes.
     */
    private void initialRaceRefCodes() {
        // West Warwick is using a custom table for the race codes
        // so looking it up this way for all RI districts
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField raceCodeField =
                dictionary.findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);

        ReferenceTable refTable = raceCodeField.getReferenceTable();
        if (refTable == null) {
            addSetupError("missing reference table",
                    "reference table not found for the race code column on the person race table ");
            m_raceRefCodes = new HashMap<String, ReferenceCode>();
        } else {
            m_raceRefCodes = refTable.getCodeMap(getBroker());
        }
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        DataDictionaryField field = getDataDictionaryField(Student.class, Student.COL_GRADE_LEVEL);
        if (field != null) {
            m_gradeCodeMap = getReferenceCodes(field.getReferenceTableOid());
        }
        m_gradeLevelMap = StudentManager.buildGradeLevelMap(getBroker());

        /*
         * Resolve some support aliases, log an error and stop the export if an alias is not found.
         */
        // school
        m_outsidePlacementString = translateAliasToJavaName(OUTSIDE_PLACEMENT_SCHOOL_FIELD, true);
        // student enrollment
        m_stateExitType = translateAliasToJavaName(STATE_EXIT_TYPE, true);
        m_outsidePlacementLocationCodeBeanPath = translateAliasToJavaName(OUTSIDE_PLACEMENT_LOCATION_CODE, true);
        // student
        m_lunchDescriptionBeanPath = translateAliasToJavaName(LUNCH_DESCRIPTION, true);
        m_excludeEnrollment = translateAliasToJavaName(ALIAS_DOE_EXCLUDE_ENR, true);
        // organization
        // m_districtStateId = translateAliasToJavaName(DOE_DISTRICT_ID, true);
        // person (student)
        // m_legacyRaceCode = translateAliasToJavaName(LEGACY_RACE_CODE, true);
        m_opClassificationBeanPath = translateAliasToJavaName(OUTSIDE_PLACEMENT_CLASSIFICATION, true);
        m_excludeStdField = translateAliasToJavaName(EXCLUDE_ALIAS_STD, false);
        m_enrollmentTypeBeanPath = translateAliasToJavaName(STUDENT_ENROLLMENT_TYPE_ALIAS, false);
        m_withdrawalCodeRefTbl = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);
        m_doeAltAsm = translateAliasToJavaName(ALIAS_DOE_ALT_ASM, false);
        m_fieldEnrResCode = translateAliasToJavaName(ALIAS_ENR_RES_OVERRIDE, true);
        m_fieldEnrHomeschoolCode = translateAliasToJavaName(ALIAS_ENR_HOMESCHOOL_CODE, true);
        m_fieldStdResCode = translateAliasToJavaName(ALIAS_STD_NON_RES_DISTR_CODE, true);
    }

    /**
     * Loads a map by student of absence days for that student.
     * Loads school membership day counts for all schools.
     *
     * @param studentCriteria Criteria
     */
    private void loadAbsenceDaysMaps(Criteria studentCriteria) {
        /*
         * Part I. Absence days from attendance.
         */
        // subQuery - Students.
        SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        // Main report query - students and Absence.
        Criteria criteria = new Criteria();
        criteria.addIn(StudentAttendance.COL_STUDENT_OID, studentsSubQuery);
        criteria.addGreaterOrEqualThanField(StudentAttendance.COL_DATE,
                StudentAttendance.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.REL_ACTIVE_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_START_DATE);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_reportDate);
        criteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

        AppGlobals.getLog().log(Level.WARNING, "!!! attendanceCriteria: " + criteria.toString());

        ReportQueryByCriteria reportQuery = new ReportQueryByCriteria(StudentAttendance.class,
                new String[] {StudentAttendance.COL_STUDENT_OID, "SUM(ATT_PORTION_ABSENT)"}, criteria);
        reportQuery.addGroupBy(StudentAttendance.COL_STUDENT_OID);

        // Build the map of student to absences.
        m_absences = new HashMap<String, Float>();
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(reportQuery);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String studentOid = (String) row[0];
                Float absencesCount = Float.valueOf(row[1].toString());
                m_absences.put(studentOid, absencesCount);
            }
        } finally {
            iterator.close();
        }

        /*
         * Load a map of school membership days by school calendar code.
         */
        // Main query, school, calendar and membership day counts.
        criteria = new Criteria();
        criteria.addEqualTo(
                SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                getCurrentContext().getOid());
        criteria.addLessOrEqualThan(SchoolCalendarDate.COL_DATE, m_reportDate);
        criteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);

        reportQuery = new ReportQueryByCriteria(SchoolCalendarDate.class,
                new String[] {SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                        SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_CALENDAR_ID,
                        "COUNT(*)"},
                criteria);
        reportQuery.addGroupBy(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID);
        reportQuery
                .addGroupBy(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_CALENDAR_ID);

        // Build map of schools, total membership for the year.
        m_schoolMembership = new HashMap<String, Integer>();
        iterator = getBroker().getReportQueryIteratorByQuery(reportQuery);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String schoolOid = (String) row[0];
                String calendarId = (String) row[1];
                Integer membershipCount = Integer.valueOf(row[2].toString());
                m_schoolMembership.put(schoolOid + "-" + calendarId, membershipCount);
            }
        } finally {
            iterator.close();
        }
    }
}
