/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.va;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Virginia state report for Student Record Collection export.
 *
 * 'S' Enrollment Code Enhancement: York needs to be able to report students transfering from
 * district to home based
 * school, and from home based to district, without performing a complete withdrawal/enrollment in
 * Aspen. We accommodate
 * this be adding an enrollment 'S' Record with a special pair of Home Based entry/withdrawal codes.
 * The SRC export will
 * artificially report a withdrawal/entry pair into a new row when an 'S' is found with appropriate
 * codes. The purpose
 * if the 'S' is to mark a transfer from school to home based, using the homebased withdrawal code
 * in the reported exit,
 * and the homebased entry in the reported new entry.
 *
 * Proper usage: Only use 'S' from an active enrolled student to transfer to active homebound
 * enrollment. Only use 'S'
 * to transfer from active homebound to active school based enrollment. If the student is not
 * enrolled/active, use an
 * 'E' with a proper enrollment code to record entry from inactive to homebound active. If the
 * student is withdrawing
 * completely from homebound, use a 'W' and appropriate withdrawal to withdraw the student.
 *
 * @author X2 Development Corporation
 */
public class VAStudentRecordCollection extends StateReportData {
    @SuppressWarnings("unused")
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Implementation of StateReportEntity to be used by the VA Student Record Collection export.
     * This must be a public
     * static inner class with a public no argument constructor so it can be instantiated through
     * reflection.
     *
     * @author X2 Development Corporation
     */
    public static class StudentRecordEntity extends StateReportEntity {
        @SuppressWarnings({"unused", "hiding"})
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";

        Map<String, Object> m_overrideCodeMap = new HashMap<String, Object>();
        Map<String, MembershipAttendance> m_schoolMap = new HashMap<String, MembershipAttendance>();
        List<MembershipAttendance> m_schoolList = new ArrayList<MembershipAttendance>();
        List<EnrollmentSpan> m_enrollmentLists = new ArrayList<EnrollmentSpan>();
        Boolean m_hasDistanceLearningFlag = Boolean.FALSE;
        String m_gradeLevel = null;
        String m_overrideServiceDistrictCode = null;
        String m_overrideServiceSchoolCode = null;
        Integer m_sort = null;


        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StudentRecordEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Method sets m_overrideCodeMap with course data.
         *
         * @param key String
         * @param value String
         */
        public void setOverrideCodeMap(String key, String value) {
            m_overrideCodeMap.put(key, value);
        }

        /**
         * Method returns requested course data.
         *
         * @param key String
         * @return Object
         */
        public Object getOverrideCodeValue(String key) {
            Object value = null;
            if (m_overrideCodeMap.containsKey(key)) {
                value = m_overrideCodeMap.get(key);
            }
            return value;
        }

        /**
         * Gets the status.
         *
         * @param memAtt MembershipAttendance
         * @param data VAStudentRecordCollection
         * @return String
         */
        public String getStatus(MembershipAttendance memAtt, VAStudentRecordCollection data) {
            MembershipAttendance attendance = memAtt;

            String status = "N";
            String entryStatus = null;
            if (attendance.getMembership() > 0) {
                status = "I";
            }

            // Determine if this enrollment span is current as of report date.
            PlainDate beginDate = null;
            PlainDate endDate = null;
            String endStatus = null;
            if (attendance.getEntryEnrollment() != null) {
                beginDate = attendance.getEntryEnrollment().getEnrollmentDate();
                entryStatus = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                        StudentEnrollment.COL_STATUS_CODE, attendance.getEntryEnrollment().getStatusCode(),
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }

            // Find the end date, from the enrollment withdrawal or from report
            // date.
            if (attendance.getExitEnrollment() != null) {
                endDate = attendance.getExitEnrollment().getEnrollmentDate();
                if (StudentEnrollment.STATUS_CHANGE.equals(attendance.getExitEnrollment().getEnrollmentType())) {
                    String hbEntryCode = (String) attendance.getExitEnrollment()
                            .getFieldValueByBeanPath(data.m_fieldHBEntryCode);
                    if (VAStudentRecordCollection.HB_ENTRY_CODES.contains(hbEntryCode)) {
                        endDate = DateUtils.add(endDate, -1);
                    }
                }
                endStatus = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                        StudentEnrollment.COL_STATUS_CODE, attendance.getExitEnrollment().getStatusCode(),
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }

            // If the span is the current span, or the end date is the same as
            // the report date
            // and the end status is ACTIVE, use the entry status from the
            // enrollment record.
            if (((beginDate != null) && !beginDate.after(data.m_reportDate))
                    && ((endDate == null) || endDate.after(data.m_reportDate)
                            || (endDate.equals(data.m_reportDate) && PARAM_ACTIVE.equals(endStatus)))) {
                status = entryStatus;
            }

            if ("TT".equals(m_gradeLevel)) {
                status = "N";
            }

            return status;
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
         * Initialize.
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

            VAStudentRecordCollection srcData = (VAStudentRecordCollection) data;
            SisStudent student = (SisStudent) bean;
            m_gradeLevel = getFieldValue(FIELD_GRADE_LEVEL);
            List<StudentEnrollment> allEnrollments = srcData.m_enrollmentMap.get(student.getOid());

            String enrollCode = null;
            if ((allEnrollments != null) && (allEnrollments.size() > 0)) {
                StudentEnrollment currentEnrollment = allEnrollments.iterator().next();
                enrollCode = currentEnrollment.getStatusCode();
            } else {
                enrollCode = student.getEnrollmentStatus();
            }

            // Is the student within an active span at the current point in
            // enrollment time.
            boolean isOpen = srcData.m_activeCode.equals(enrollCode) || STATUS_SERVICE.equals(enrollCode)
                    || srcData.m_virtualStatusCodes.contains(enrollCode);
            groupEnrollments(allEnrollments, isOpen, student.getSchoolOid());

            // Check the school flag for Exclude from reporting.
            // Generally excluded from the selection, but can creep into spans
            // if the student
            // transfered from an exclude school into an active school during
            // the year.
            // Remove any spans where the entry record is flagged as exclude
            Iterator<EnrollmentSpan> spanIterator = m_enrollmentLists.iterator();
            while (spanIterator.hasNext()) {
                EnrollmentSpan span = spanIterator.next();
                boolean schoolExcluded = true;
                boolean entryExcluded = false;
                SisSchool school = span.getSchool();
                if (school != null) {
                    String excludeFlag = (String) school.getFieldValueByBeanPath(srcData.m_fieldExcludeSchool);
                    schoolExcluded = BooleanAsStringConverter.TRUE.equals(excludeFlag);
                }
                StudentEnrollment entryEnrollment = span.getFirstEntry();
                if (entryEnrollment != null && BooleanAsStringConverter.TRUE
                        .equals(entryEnrollment.getFieldValueByAlias(ALIAS_EXCLUDE_REPORTING_ENR))) {

                    entryExcluded = true;
                }

                if (schoolExcluded || entryExcluded) {
                    spanIterator.remove();
                }
            }

            // If a student has a span within the active school year, remove any
            // summer withdrawal spans.
            if (m_enrollmentLists.size() > 1) {
                // Determine if the student has an active enrollment.
                boolean hasActive = false;
                for (EnrollmentSpan span : m_enrollmentLists) {
                    SisSchool school = span.getSchool();
                    PlainDate startDate = null;
                    if ((school != null)) {
                        startDate = srcData.getFirstSchoolInSessionDate(school, student.getCalendarCode());
                    }
                    if (startDate == null) {
                        startDate = srcData.m_districtStartDate;
                    }
                    if ((span.getFirstWithdrawal() == null)
                            || ((startDate != null)
                                    && !startDate.after(span.getFirstWithdrawal().getEnrollmentDate()))) {
                        hasActive = true;
                    }
                }

                // remove summer exits if there is an active.
                if (hasActive) {
                    spanIterator = m_enrollmentLists.iterator();
                    while (spanIterator.hasNext()) {
                        EnrollmentSpan span = spanIterator.next();
                        SisSchool school = span.getSchool();
                        PlainDate startDate = null;
                        if ((school != null)) {
                            startDate = srcData.getFirstSchoolInSessionDate(school, student.getCalendarCode());
                        }
                        if (startDate == null) {
                            startDate = srcData.m_districtStartDate;
                        }
                        if ((startDate != null) && ((span.getFirstWithdrawal() != null)
                                && startDate.after(span.getFirstWithdrawal().getEnrollmentDate()))) {
                            spanIterator.remove();
                        }
                    }
                }
            }

            // Add spans that overlap this school year.
            for (EnrollmentSpan span : m_enrollmentLists) {
                if (srcData.m_preregOnly) {
                    addMembership(span);
                } else if ((span.getFirstEntry() != null)
                        && !span.getFirstEntry().getEnrollmentDate().after(srcData.m_reportDate)) {
                    if (span.getFirstWithdrawal() == null) {
                        addMembership(span);
                    } else if (!span.getFirstWithdrawal().getEnrollmentDate().before(srcData.m_summerBeginDate)) {
                        if (COLLECTION_SPED.equals(srcData.m_collection)) {
                            /*
                             * For Special Ed (Dec 1) submission, only report students who are
                             * active as of report date.
                             */
                            if (!span.getFirstWithdrawal().getEnrollmentDate().before(srcData.m_reportDate)) {
                                addMembership(span);
                            } else if (isSummerTuitionReimbursment(span.getFirstEntry())) {
                                addMembership(span);
                            }
                        } else {
                            addMembership(span);
                        }
                    }
                }
            }

            m_schoolList.addAll(m_schoolMap.values());
            // Sort by Status if default sort by LASID is selected
            if (((VAStudentRecordCollection) data).m_sort.intValue() == 3) {
                TreeMap<String, Collection<MembershipAttendance>> statusMap =
                        new TreeMap<String, Collection<MembershipAttendance>>();
                for (MembershipAttendance att : m_schoolList) {
                    String status = getStatus(att, ((VAStudentRecordCollection) data));
                    Collection attCollection = statusMap.get(status);
                    if (attCollection == null) {
                        attCollection = new ArrayList<MembershipAttendance>();
                        statusMap.put(status, attCollection);
                    }
                    attCollection.add(att);
                }
            } else {
                // Sort by first enrollment date in the span. The attendance
                // records
                // are unordered because
                // the attendance records can be built from multiple out of
                // order
                // spans.
                boolean swapped = true;
                while (swapped) {
                    swapped = false;
                    for (int pos = 0; pos < (m_schoolList.size() - 1); pos++) {
                        MembershipAttendance att1 = m_schoolList.get(pos);
                        MembershipAttendance att2 = m_schoolList.get(pos + 1);
                        if (att1.getEntryEnrollment().getEnrollmentDate()
                                .after(att2.getEntryEnrollment().getEnrollmentDate())) {
                            m_schoolList.set(pos, att2);
                            m_schoolList.set(pos + 1, att1);
                            swapped = true;
                        }
                    }
                }
            }

            // Determine if student has any course with a Distance Learning Flag
            // which is indicated
            // when a class has a Virtual Course Type.
            // Determine if student has any course which is a "Career" or
            // "Tech Ed" course. If so update
            // Serving Division and Serving School to that of the course.

            if (srcData.m_scheduleMap.containsKey(student.getOid())) {
                Collection<StudentSchedule> schedules = srcData.m_scheduleMap.get(student.getOid());

                if (!schedules.isEmpty()) {
                    for (StudentSchedule schedule : schedules) {
                        if ((schedule.getSection() != null) && (schedule.getSection().getSchoolCourse() != null)
                                && (schedule.getSection().getSchoolCourse().getCourse() != null)) {
                            Course currentCourse = schedule.getSection().getSchoolCourse().getCourse();
                            if (currentCourse.getFieldValueByAlias(ALIAS_VIRTUAL_COURSE_TYPE) != null) {
                                if ((currentCourse.getCredit() != null)
                                        && (currentCourse.getCredit().compareTo(BigDecimal.ZERO) > 0)) {
                                    m_hasDistanceLearningFlag = Boolean.TRUE;
                                }
                            }

                            if (BooleanAsStringConverter.TRUE
                                    .equals(currentCourse.getFieldValueByAlias(ALIAS_CAREER_AND_TECH_ED))
                                    || BooleanAsStringConverter.TRUE
                                            .equals(currentCourse.getFieldValueByAlias(ALIAS_GOVERNORS_SCHOOL))) {
                                if ((schedule.getSection().getPrimaryStaff() != null)
                                        && !StringUtils.isEmpty(schedule.getSection().getPrimaryStaff().getLocalId())) {
                                    String localId = schedule.getSection().getPrimaryStaff().getLocalId();
                                    String servingDistrictCode = localId.substring(0, 3);
                                    setOverrideCodeMap(ALIAS_SERVICE_DISTRICT_CODE, servingDistrictCode);

                                    String serviceSchoolCode = localId.substring(3, localId.length());
                                    setOverrideCodeMap(ALIAS_SERVICE_SCHOOL_CODE, serviceSchoolCode);
                                }
                            }
                        }
                    }
                }
            }
            setRowCount(m_schoolList.size());
            srcData.m_rowCount += m_schoolList.size();
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();

            String name = student.getNameView() + " [LocalID: " + student.getLocalId() + " StateID: "
                    + student.getStateId() + "]";
            return name;
        }

        /**
         * Filter the entity out if it has aggregate present days = 0. If user query mode is
         * snapshot (4), ignore filter
         * criteria.
         *
         * @return StateReportValidationError
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;
            return error;
        }

        /**
         * Add an enrollment span to the schools list.
         *
         * @param span EnrollmentSpan
         */
        private void addMembership(EnrollmentSpan span) {
            VAStudentRecordCollection srcData = (VAStudentRecordCollection) getData();
            SisStudent student = (SisStudent) getBean();
            SisSchool school = span.getSchool();
            if (school != null) {
                // Find the begin date, from the enrollment entry or from school
                // year start.
                PlainDate beginDate = null;
                PlainDate schedDate = null;
                if (span.getFirstEntry() != null) {
                    beginDate = span.getFirstEntry().getEnrollmentDate();
                }
                if (school.getActiveSchedule() != null) {
                    schedDate = school.getActiveSchedule().getStartDate();
                }
                if (schedDate == null) {
                    schedDate = srcData.m_districtStartDate;
                }
                if ((beginDate == null) || beginDate.before(srcData.m_summerBeginDate)) {
                    beginDate = srcData.m_summerBeginDate;
                }

                // Find the end date, from the enrollment withdrawal or from
                // report date.
                PlainDate endDate = null;
                if (span.getFirstWithdrawal() != null) {
                    endDate = span.getFirstWithdrawal().getEnrollmentDate();
                    // If this is a HB status change record, increment the begin
                    // date by one.
                    if (StudentEnrollment.STATUS_CHANGE.equals(span.getFirstWithdrawal().getEnrollmentType())) {
                        String hbEntryCode = (String) span.getFirstWithdrawal()
                                .getFieldValueByBeanPath(srcData.m_fieldHBEntryCode);
                        if (VAStudentRecordCollection.HB_ENTRY_CODES.contains(hbEntryCode)) {
                            endDate = DateUtils.add(endDate, -1);
                        }
                    }
                }
                if ((endDate == null) || endDate.after(srcData.m_reportDate)) {
                    endDate = srcData.m_reportDate;
                }

                // look up serving school. This is used to merge spans together,
                // and determine attendance counts.
                String servingSchool = null;
                if (span.getFirstEntry() != null) {
                    String enrSS = (String) span.getFirstEntry()
                            .getFieldValueByBeanPath(srcData.m_fieldServiceSchoolCode);
                    if (!StringUtils.isEmpty(enrSS)) {
                        servingSchool = srcData.lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                                srcData.m_fieldServiceSchoolCode, enrSS,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    } else {
                        servingSchool = (String) span.getSchool().getFieldValueByBeanPath(srcData.m_fieldSchoolCode);
                    }
                }

                int memberDays = 0;
                float absenceDays = 0;
                int absencesDU = 0;
                float unexAbsentDays = 0;
                // Find membership days and absences in this date range.
                if ((beginDate != null) && (endDate != null) && !beginDate.after(endDate)) {
                    Set<PlainDate> calendarDays = srcData.getCalendarDays(school, student.getCalendarCode());
                    // Students withdrawn with NoShow get no member days
                    String withdrawalStatus = EMPTY_STRING;
                    if (span.getFirstWithdrawal() != null) {
                        withdrawalStatus = srcData.lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                                StudentEnrollment.COL_STATUS_CODE, span.getFirstWithdrawal().getStatusCode(),
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                    if (span.getFirstWithdrawal() == null || !STATUS_NO_SHOW.equals(withdrawalStatus)) {
                        memberDays = srcData.m_enrollmentManager.getMembershipTotal(student, calendarDays, true,
                                beginDate, endDate, school);
                    }
                    List<StudentAttendance> absences = srcData.m_attendance.get(student.getOid());
                    // Do not count attendance for homebound/home based
                    // students. Validation error if they exist.
                    if ("9998".equals(servingSchool) || "9999".equals(servingSchool)) {
                        // Home based, no absences, report any absences as
                        // error.
                        if ((absences != null) && (absences.size() > 0)) {
                            StateReportValidationError error = new StateReportValidationError(this,
                                    srcData.getFieldDefinition(FIELD_DAYS_ABSENT),
                                    "Homebases student has attendance records.",
                                    Integer.toString(absences.size()) + " records found.");
                            this.addRetrievalError(FIELD_DAYS_ABSENT, error);
                        }
                    } else {
                        if (absences != null) {
                            for (StudentAttendance attendance : absences) {
                                if (!attendance.getDate().before(beginDate) && !attendance.getDate().after(endDate)
                                        && (attendance.getPortionAbsent() != null)) {
                                    if (attendance.getAbsentIndicator()) {
                                        absenceDays += attendance.getPortionAbsent().floatValue();
                                        if (!attendance.getExcusedIndicator()) {
                                            unexAbsentDays += attendance.getPortionAbsent().floatValue();
                                        }
                                    }
                                    if (CODE_ATT_DU.equalsIgnoreCase(attendance.getOtherCode())
                                            || CODE_ATT_DU.equalsIgnoreCase(attendance.getOtherCode02())
                                            || CODE_ATT_DU.equalsIgnoreCase(attendance.getReasonCode())) {
                                        absencesDU += 1;
                                    }
                                }
                            }
                        }
                    }
                }

                // See if the student already has an enrollment span in this
                // school.
                MembershipAttendance memb = m_schoolMap.get(servingSchool);
                if (memb == null) {
                    memb = new MembershipAttendance(span.getFirstEntry(), span.getFirstWithdrawal(), span.getSchool());
                    m_schoolMap.put(servingSchool, memb);
                }

                // Accumulate membership and attendance
                memb.setMembership(memb.getMembership() + memberDays);
                memb.setAbsent(memb.getAbsent() + absenceDays);
                memb.setAbsentDU(absencesDU);
                memb.setUnexAbsences(memb.getUnexAbsent() + unexAbsentDays);

                // Replace the entry record if this one is earlier.
                if ((memb.getEntryEnrollment() == null) || ((span.getFirstEntry() != null) && span.getFirstEntry()
                        .getEnrollmentDate().before(memb.getEntryEnrollment().getEnrollmentDate()))) {
                    memb.setEntryEnrollment(span.getFirstEntry());
                }

                // Replace the exit record if this one is later or empty.
                if (span.getFirstWithdrawal() == null) {
                    memb.setExitEnrollment(null);
                } else if ((memb.getExitEnrollment() != null) && span.getFirstWithdrawal().getEnrollmentDate()
                        .after(memb.getExitEnrollment().getEnrollmentDate())) {
                    memb.setExitEnrollment(span.getFirstWithdrawal());
                }
            }
        }

        /**
         * Checks if is summer tuition reimbursment.
         *
         * @param enrollment StudentEnrollment
         * @return true, if is summer tuition reimbursment
         */
        private boolean isSummerTuitionReimbursment(StudentEnrollment enrollment) {
            String tuitionValue = (String) enrollment.getFieldValueByAlias(ALIAS_SPED_TUITION_SUM);
            return !StringUtils.isEmpty(tuitionValue);
        }

        /**
         * Create a list of lists of enrollments. Each sub list is an enrollment span and should
         * encompass one school
         * and a range from first entry to last withdrawal.
         *
         * 'S' Enrollment Code Enhancement: VA SRC needs to be able to report students transfering
         * from district to
         * home-based school, and from home-based to district, without performing a complete
         * withdrawal/enrollment in
         * Aspen. We accommodate this be adding an enrollment 'S' Record with a special pair of Home
         * Based
         * entry/withdrawal codes. The SRC export will artificially report a withdrawal/entry pair
         * into a new row when
         * an 'S' is found with appropriate codes. The purpose if the 'S' is to mark a transfer from
         * school to home
         * based, using the homebased withdrawal code in the reported exit, and the homebased entry
         * in the reported new
         * entry.
         *
         * Proper usage: Only use 'S' from an active enrolled student to transfer to active
         * home-based enrollment. Only
         * use 'S' to transfer from active home-based to active school based enrollment. If the
         * student is not
         * enrolled/active, use an 'E' with a proper enrollment code to record entry from inactive
         * to home-based active.
         * If the student is withdrawing completely from home-based, use a 'W' and proper withdrawal
         * code to withdraw
         * the student.
         *
         * @param enrollments List<StudentEnrollment>
         * @param isOpen boolean
         * @param currentSchoolOid String
         */
        private void groupEnrollments(List<StudentEnrollment> enrollments, boolean isOpen, String currentSchoolOid) {
            VAStudentRecordCollection data = (VAStudentRecordCollection) getData();
            List<StudentEnrollment> currentEnrollments = new ArrayList<StudentEnrollment>();
            boolean containedHb = false;
            boolean presub = COLLECTION_PRESUB.equals(data.m_collection);
            int inactiveCount = 0;
            if (enrollments != null) {
                for (StudentEnrollment enrollment : enrollments) {
                    if (enrollment.getSchoolOid() != null) {
                        // If the school ID doesn't match, close it
                        if (!currentSchoolOid.equals(enrollment.getSchoolOid())) {
                            if (!currentEnrollments.isEmpty()) {
                                if ("Inactive"
                                        .equals(currentEnrollments.get(currentEnrollments.size() - 1).getStatusCode())
                                        && presub) {
                                    m_enrollmentLists.add(0, new EnrollmentSpan(data, currentEnrollments));
                                    inactiveCount++;
                                } else {
                                    m_enrollmentLists.add(inactiveCount, new EnrollmentSpan(data, currentEnrollments));
                                }

                            }
                            currentEnrollments = new ArrayList<StudentEnrollment>();
                            isOpen = true;
                            currentSchoolOid = enrollment.getSchoolOid();
                        }

                        // If its a status change and a HB Entry, close it
                        if (StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType())) {
                            String hbEntryCode = (String) enrollment.getFieldValueByBeanPath(data.m_fieldHBEntryCode);
                            if (VAStudentRecordCollection.HB_ENTRY_CODES.contains(hbEntryCode)) {
                                // This status change will generate a split in
                                // the enrollment
                                // and represent the withdrawal and the entry on
                                // the two sides.
                                // Complete this span here, then continue on to
                                // start the next.
                                containedHb = true;
                                currentEnrollments.add(0, enrollment);
                                if ("Inactive"
                                        .equals(currentEnrollments.get(currentEnrollments.size() - 1).getStatusCode())
                                        && presub) {
                                    m_enrollmentLists.add(0, new EnrollmentSpan(data, currentEnrollments));
                                    inactiveCount++;
                                } else {
                                    m_enrollmentLists.add(inactiveCount, new EnrollmentSpan(data, currentEnrollments));
                                }
                                currentEnrollments = new ArrayList<StudentEnrollment>();
                            }
                        }

                        // if its open, close it
                        if (isOpen) {
                            currentEnrollments.add(0, enrollment);
                            if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
                                isOpen = false;
                            }
                        } else {
                            // otherwise if its a withdrawal and there is more
                            // to do, close it
                            if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                                if (!currentEnrollments.isEmpty()) {
                                    if ("Inactive".equals(
                                            currentEnrollments.get(currentEnrollments.size() - 1).getStatusCode())
                                            && presub) {
                                        m_enrollmentLists.add(0, new EnrollmentSpan(data, currentEnrollments));
                                        inactiveCount++;
                                    } else {
                                        m_enrollmentLists.add(inactiveCount,
                                                new EnrollmentSpan(data, currentEnrollments));
                                    }
                                }
                                currentEnrollments = new ArrayList<StudentEnrollment>();
                                isOpen = true;
                            } else if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
                                isOpen = false;
                            }
                            currentEnrollments.add(0, enrollment);
                        }
                    }
                }

                if (!currentEnrollments.isEmpty() && (!containedHb || (enrollments.size() != 1))) {
                    if ("Inactive".equals(currentEnrollments.get(currentEnrollments.size() - 1).getStatusCode())
                            && presub) {
                        m_enrollmentLists.add(0, new EnrollmentSpan(data, currentEnrollments));
                        inactiveCount++;
                    } else {
                        m_enrollmentLists.add(inactiveCount, new EnrollmentSpan(data, currentEnrollments));
                    }
                }
            }
        }
    }

    /**
     * Represents one enrollment span in one school from first entry to last withdrawal.
     *
     * @author X2 Development Corporation
     */
    protected static class EnrollmentSpan {
        @SuppressWarnings({"unused", "hiding"})
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";

        public static int TYPE_ENTRY = 1;
        public static int TYPE_EXIT = 2;
        public static int TYPE_STATUS = 4;
        public static int TYPE_YOG = 8;

        List<StudentEnrollment> m_enrollments;
        StudentEnrollment m_firstEntry;
        StudentEnrollment m_firstWithdrawal;
        SisSchool m_school;

        /**
         * Constructor: Find other values from the enrollment list.
         *
         * @param data VAStudentRecordCollection
         * @param enrollments List<StudentEnrollment>
         */
        public EnrollmentSpan(VAStudentRecordCollection data, List<StudentEnrollment> enrollments) {
            m_enrollments = enrollments;
            for (StudentEnrollment enrollment : enrollments) {
                if (m_school == null) {
                    m_school = enrollment.getSchool();
                }
                if ((data.m_activeCode.equals(enrollment.getStatusCode())
                        || STATUS_SERVICE.equals(enrollment.getStatusCode())
                        || data.m_virtualStatusCodes.contains(enrollment.getStatusCode())
                        || StringUtils.isEmpty(enrollment.getStatusCode()) || data.m_preregOnly)
                        && (m_firstEntry == null)) {
                    m_firstEntry = enrollment;
                }
                if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())
                        && (m_firstWithdrawal == null)) {
                    m_firstWithdrawal = enrollment;
                }
            }
            /*
             * If we completed the list and have no withdrawal, check if the last thing was a status
             * change for
             * home-bound split. 1. We do not already have a withdrawal of some kind. 2. Must have
             * more than one
             * enrollment record, so the last recored is not also the first record. 3. Make sure the
             * S record has HB
             * ENTRY CODE that is a valid Homebound entry code.
             */
            StudentEnrollment lastEnrollment = enrollments.get(enrollments.size() - 1);
            if ((m_firstWithdrawal == null) && (enrollments.size() > 1)
                    && StudentEnrollment.STATUS_CHANGE.equals(lastEnrollment.getEnrollmentType())) {
                String hbEntryCode = (String) lastEnrollment.getFieldValueByBeanPath(data.m_fieldHBEntryCode);
                if (VAStudentRecordCollection.HB_ENTRY_CODES.contains(hbEntryCode)) {
                    m_firstWithdrawal = lastEnrollment;
                }
            }
        }

        /**
         * Return the school for the enrollment span.
         *
         * @return School
         */
        public SisSchool getSchool() {
            return m_school;
        }

        /**
         * Return the list of enrollments in this span.
         *
         * @return List<StudentEnrollment>
         */
        public List<StudentEnrollment> getEnrollments() {
            return m_enrollments;
        }

        /**
         * Return the first entry record for the enrollment span.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getFirstEntry() {
            return m_firstEntry;
        }

        /**
         * Return the first withdrawal record for the enrollment span.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getFirstWithdrawal() {
            return m_firstWithdrawal;
        }

        /**
         * Return the most recent enrollment record of the specified types that exists on or before
         * the specified date.
         *
         * @param date
         *        as of date to find enrollment records for.
         * @param type
         *        an int as a combined mix of the four types specified in the TYPE_* constants.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getEnrollmentForDate(PlainDate date, int type) {
            StudentEnrollment lastEnrollment = null;
            for (StudentEnrollment enrollment : m_enrollments) {
                if (!enrollment.getEnrollmentDate().after(date)) {
                    if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType()) && ((type & TYPE_ENTRY) > 0)) {
                        lastEnrollment = enrollment;
                    }
                    if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())
                            && ((type & TYPE_EXIT) > 0)) {
                        lastEnrollment = enrollment;
                    }
                    if (StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType())
                            && ((type & TYPE_STATUS) > 0)) {
                        lastEnrollment = enrollment;
                    }
                    if (StudentEnrollment.YOG_CHANGE.equals(enrollment.getEnrollmentType())
                            && ((type & TYPE_YOG) > 0)) {
                        lastEnrollment = enrollment;
                    }
                }
            }
            return lastEnrollment;
        }
    }

    /**
     * An object to track enrollment, membership and attendance in one school. This represents one
     * reporting row for a
     * student.
     *
     * @author X2 Development Corporation
     *
     */
    protected static class MembershipAttendance {
        @SuppressWarnings({"unused", "hiding"})
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";
        /*
         * Instance for school, dates, counts and codes.
         */
        StudentEnrollment m_entryEnrollment;
        StudentEnrollment m_exitEnrollment;
        float m_absent;
        int m_absentDU;
        float m_unexAbsent;
        int m_membership;
        // String m_schoolOid;
        SisSchool m_school;

        /**
         * constructor, set initial attendance and membership counts.
         *
         * @param entryEnrollment StudentEnrollment
         * @param withdrawalEnrollment StudentEnrollment
         * @param school SisSchool
         */
        protected MembershipAttendance(StudentEnrollment entryEnrollment, StudentEnrollment withdrawalEnrollment,
                SisSchool school) {
            m_entryEnrollment = entryEnrollment;
            m_exitEnrollment = withdrawalEnrollment;
            // m_schoolOid = schoolOid;
            m_school = school;
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
         * @return the m_absentDU
         */
        protected int getAbsentDU() {
            return m_absentDU;
        }

        /**
         * Return the student enrollment representing the first entry.
         *
         * @return StudentEnrollment
         */
        protected StudentEnrollment getEntryEnrollment() {
            return m_entryEnrollment;
        }

        /**
         * Return the student enrollment representing the final exit.
         *
         * @return StudentEnrollment
         */
        protected StudentEnrollment getExitEnrollment() {
            return m_exitEnrollment;
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
         * Returns the school for this record.
         *
         * @return String
         */
        protected SisSchool getSchool() {
            return m_school;
        }

        /**
         * Calculate and return the number of days present.
         *
         * @return float
         */
        protected float getAttendance() {
            float attendance = getMembership() - getAbsent();
            if (attendance < 0) {
                attendance = 0;
            }

            return attendance;
        }

        /**
         * Returns the count of unexcused absences.
         *
         * @return float
         */
        protected float getUnexAbsent() {
            return m_unexAbsent;
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
         * @param absentDU
         */
        protected void setAbsentDU(int absentDU) {
            this.m_absentDU = absentDU;
        }

        /**
         * Set the student enrollment record representing the first entry.
         *
         * @param entry void
         */
        protected void setEntryEnrollment(StudentEnrollment entry) {
            m_entryEnrollment = entry;
        }

        /**
         * Set the student enrollment record representing the final exit.
         *
         * @param exit void
         */
        protected void setExitEnrollment(StudentEnrollment exit) {
            m_exitEnrollment = exit;
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
         * Sets the count of unexcused abcences.
         *
         * @param absent void
         */
        protected void setUnexAbsences(float absent) {
            m_unexAbsent = absent;
        }
    }

    /*
     * Report values and aliases.
     */
    private static final String EOL = ExportJavaSource.FORMAT_EOL_WINDOWS; // System.getProperty("line.separator");

    /*
     * Aliases for fields to look up.
     */
    protected static final String ALIAS_ASM_CREDIT_ACC_CODE = "all-asm-CreditAccomodation";
    protected static final String ALIAS_ASM_INTENSIVE_SUPPORT = "all-asm-IntensiveSupportSvs";
    protected static final String ALIAS_CAREER_AND_TECH_ED = "DOE CAREER AND TECH ED";
    protected static final String ALIAS_CTE_CAREER_CLUSTER = "DOE CTE CAREER CLUSTER";
    protected static final String ALIAS_CTE_FINISHER = "DOE CTE FINISHER";
    protected static final String ALIAS_CTE_POPULATION = "DOE CTE POPULATION";
    protected static final String ALIAS_DISTRICT_CODE = "DOE DISTRICT ID";
    protected static final String ALIAS_EXCLUDE_REPORTING_ENR = "DOE EXCLUDE ENR";
    protected static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_GOVERNORS_SCHOOL = "DOE GOVERNORS SCHOOL";
    protected static final String ALIAS_GRADE_LEVEL_OVERRIDE = "DOE GRADE LEVEL OVERRIDE";
    protected static final String ALIAS_HB_ENTRY_CODE = "DOE HB ENTRY CODE";
    protected static final String ALIAS_HB_WITHDRAW_CODE = "DOE HB WITHDRAW CODE";
    protected static final String ALIAS_HOME_DISTRICT_CODE = "DOE DISTRICT HOME";
    protected static final String ALIAS_HOME_SCHOOL_CODE = "DOE SCHOOL HOME";
    protected static final String ALIAS_IMMIGRANT_STATUS = "DOE IMMIGRANT";
    protected static final String ALIAS_MOP_FLAG = "DOE MOP FLAG";
    protected static final String ALIAS_PGM_LIEP = "all-pgm-LanguageInstructionEducationalProgram";
    protected static final String ALIAS_PGM_REGIONAL_LOCAL_CENTER_PERCENT_OF_TIME =
            "all-pgm-RegionalLocalCenterPercentofTime";
    protected static final String ALIAS_PGM_REMOTE_INST_PERCENT_OF_TIME = "all-pgm-RemoteInstructionPercentofTime";
    protected static final String ALIAS_PGM_INTERNET_ACCESS_REMOTE_LEARNING = "all-pgm-InternetAccessforRemoteLearning";
    protected static final String ALIAS_PGM_DEVICE_ACCESS_REMOTE_LEARNING = "all-pgm-DeviceAccessforRemoteLearning";
    protected static final String ALIAS_PGM_PARENTAL_REMOTE_LEARNING_DECISION =
            "all-pgm-ParentalRemoteLearningDecision";
    protected static final String ALIAS_SASID = "DOE SASID";
    protected static final String ALIAS_SCHOOL_CODE = "DOE SCHOOL ID";
    protected static final String ALIAS_SEDF_REPORT_FLAG = "SEDF REPORT FLAG";
    protected static final String ALIAS_SERVICE_DISTRICT_CODE = "DOE DISTRICT SERVE";
    protected static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVE";
    protected static final String ALIAS_SPED_DISABILITY = "DOE SPED DISABILITY";
    protected static final String ALIAS_STD_HOME_SCHOOL_CODE = "DOE STD SCHOOL HOME";
    protected static final String ALIAS_SPED_TUIT_DISABILITY = "DOE SPED TUIT DISABILITY";
    protected static final String ALIAS_SPED_TUITION_S1 = "DOE SPED TUITION S1";
    protected static final String ALIAS_SPED_TUITION_S2 = "DOE SPED TUITION S2";
    protected static final String ALIAS_SPED_TUITION_SUM = "DOE SPED TUITION SUM";
    protected static final String ALIAS_STD_CAREER_CLUSTER = "all-std-CTESRCCareerClusterOveride";
    protected static final String ALIAS_TRADITIONAL_GENDER = "DOE TRADITIONAL GENDER";
    protected static final String ALIAS_TUITION_PAID = "DOE TUITION PAID";
    protected static final String ALIAS_US_END_DATE = "DOE US END DATE";
    protected static final String ALIAS_US_ENTRY_DATE = "DOE US ENTRY DATE";
    protected static final String ALIAS_VIRTUAL_COURSE_TYPE = "DOE VIRTUAL COURSE TYPE";
    protected static final String ALIAS_VIRTUAL_PROGRAM = "DOE VIRTUAL PROGRAM";

    /**
     * STD aliases
     */
    protected static final String ALIAS_STD_ATT_CONF = "all-std-AttendanceConferenceCode";
    protected static final String ALIAS_STD_ATT_PLAN = "all-std-AttendancePlanCode";
    protected static final String ALIAS_STD_COURT_REF = "all-std-CourtReferralComplaintProceedingsCode";
    protected static final String ALIAS_STD_HEAD_START_PROVIDER = "all-std-HeadStartProviderCode";

    /**
     * ENR aliases.
     */
    protected static final String ALIAS_ENR_ATT_CONF = "all-enr-AttendanceConferenceCode";
    protected static final String ALIAS_ENR_ATT_PLAN = "all-enr-AttendancePlanCode";
    protected static final String ALIAS_ENR_COURT_REF = "all-enr-CourtReferralComplaintProceedingsCode";

    /*
     * Constants for each field ID from the export format field definition.
     */
    protected static final String FIELD_RECORD_TYPE = "Record Type";
    protected static final String FIELD_STATE_ID = "State ID";
    protected static final String FIELD_LOCAL_ID = "Local ID";
    protected static final String FIELD_DIVISION_ID = "Division ID";
    protected static final String FIELD_SCHOOL_ID = "School ID";
    protected static final String FIELD_DIVISION_ID_SERVE = "Division ID Serve";
    protected static final String FIELD_SCHOOL_ID_SERVE = "School ID Serve";
    protected static final String FIELD_STATUS = "Status";
    protected static final String FIELD_ENTRY_CODE = "Entry Code";
    protected static final String FIELD_ENTRY_DATE = "Entry Date";
    protected static final String FIELD_EXIT_CODE = "Exit Code";
    protected static final String FIELD_EXIT_DATE = "Exit Date";
    protected static final String FIELD_RETIRED1 = "RETIRED1";
    protected static final String FIELD_GENDER = "Gender";
    protected static final String FIELD_BIRTH_DATE = "Birth date";
    protected static final String FIELD_GRADE_LEVEL = "Grade Level";
    protected static final String FIELD_KG_HALFDAY = "KG HalfDay";
    protected static final String FIELD_PRIMARY_DISABILITY = "Primary Disability";
    protected static final String FIELD_SPED_TIME_PCT = "SPED Time pct";
    protected static final String FIELD_DISADV_STATUS = "Disadv Status";
    protected static final String FIELD_RETIRED2 = "RETIRED2";
    protected static final String FIELD_RETIRED3 = "RETIRED3";
    protected static final String FIELD_COUNTY_BIRTH = "County Birth";
    protected static final String FIELD_HOME_LANGUAGE = "Home Language";
    protected static final String FIELD_IMMIGRANT = "Immigrant";
    protected static final String FIELD_RETIRED4 = "RETIRED4";
    protected static final String FIELD_RETIRED5 = "RETIRED5";
    protected static final String FIELD_RETIRED6 = "RETIRED6";
    protected static final String FIELD_RETIRED7 = "RETIRED7";
    protected static final String FIELD_GIFTED = "Gifted";
    protected static final String FIELD_GIFTED_REFERRAL = "Gifted Referral";
    protected static final String FIELD_TITLE1 = "Title1";
    protected static final String FIELD_GED_PROGRAM = "GED Program";
    protected static final String FIELD_INTL_BACC = "Intl Bacc";
    protected static final String FIELD_GRADUATE_COMP = "Graduate Comp";
    protected static final String FIELD_GRADUATE_PLAN = "Graduate Plan";
    protected static final String FIELD_ADVANCED_PLACEMENT = "Advanced Placement";
    protected static final String FIELD_DUAL_ENROLLMENT = "Dual Enrollment";
    protected static final String FIELD_CTE_FINISHER = "CTE Finisher";
    protected static final String FIELD_CTE_CAREER_CLUSTER = "CTE Career Cluster";
    protected static final String FIELD_CTE_SPECIAL_POPULAT = "CTE Special Populat";
    protected static final String FIELD_W8_REASON = "W8 Reason";
    protected static final String FIELD_RETIRED8 = "RETIRED8";
    protected static final String FIELD_DAYS_PRESENT = "Days Present";
    protected static final String FIELD_DAYS_ABSENT = "Days Absent";
    protected static final String FIELD_RETENTION = "Retention";
    protected static final String FIELD_TRUANCY_CONFERENCE = "Truancy Conference";
    protected static final String FIELD_TUITION_PAID = "Tuition Paid";
    protected static final String FIELD_NON_PUB_FTE = "Non-pub FTE";
    protected static final String FIELD_RETIRED9 = "RETIRED9";
    protected static final String FIELD_KG_READINESS = "KG Readiness";
    protected static final String FIELD_HOMEROOM = "Homeroom";
    protected static final String FIELD_SCHOOL_CHOICE = "School Choice";
    protected static final String FIELD_SUPP_ED = "Supp Ed";
    protected static final String FIELD_RETIRED10 = "RETIRED10";
    protected static final String FIELD_DIPLOMA_SEAL = "Diploma Seal";
    protected static final String FIELD_EARLY_COLLEGE_SCHOL = "Early College Schol";
    protected static final String FIELD_DISTANCE_LEARNING = "Distance Learning";
    protected static final String FIELD_PK_EXP = "PK Exp";
    protected static final String FIELD_PK_FUNDING_CODE = "PK Funding Source";
    protected static final String FIELD_PK_WEEKLY_TIME = "PK Weekly Time";
    protected static final String FIELD_CTE_DUAL_ENROLLMENT = "CTE Dual Enrollment";
    protected static final String FIELD_CTE_ATTAINMENT = "CTE Attainment";
    protected static final String FIELD_CTE_PROGRAM = "CTE Program";
    protected static final String FIELD_ADDRESS_1 = "Address 1";
    protected static final String FIELD_ADDRESS_2 = "Address 2";
    protected static final String FIELD_ZIP_CODE = "Zip Code";
    protected static final String FIELD_PHONE_NUMBER = "Phone Number";
    protected static final String FIELD_UNEXCUSED_ABSENCES = "Unexcused Absences";
    protected static final String FIELD_NIGHT_RESIDENCE = "Night Residence";
    protected static final String FIELD_NEGLECTED_DELINQNT = "Neglected/Delinqnt";
    protected static final String FIELD_FT_VIRTUAL_PROGRAM = "FT Virtual Program";
    protected static final String FIELD_RETIRED11 = "RETIRED11";
    protected static final String FIELD_ETHNIC = "Ethnic";
    protected static final String FIELD_RACE = "Race";
    protected static final String FIELD_RETIRED12 = "RETIRED12";
    protected static final String FIELD_ESL_SERVICES = "EL Code";
    protected static final String FIELD_RETIRED13 = "RETIRED13";
    protected static final String FIELD_IB_CODE = "IB Code";
    protected static final String FIELD_UNACCOMP_HOMELESS = "Unaccomp Homeless";
    protected static final String FIELD_SPED_PLACEMENT = "SPED Placement";
    protected static final String FIELD_SPED_REG_CLASS_PCT = "SPED Reg Class Pct";
    protected static final String FIELD_SPED_PRIM_SVC_PCT = "SPED Prim Svc Pct";
    protected static final String FIELD_SPED_DISABILITY_2 = "SPED Disability 2";
    protected static final String FIELD_SPED_SVC_PCT_2 = "SPED Svc Pct 2";
    protected static final String FIELD_SPED_DIV_2 = "SPED Div 2";
    protected static final String FIELD_SPED_SCHOOL_2 = "SPED School 2";
    protected static final String FIELD_SPED_DISABILITY_3 = "SPED Disability 3";
    protected static final String FIELD_SPED_SVC_PCT_3 = "SPED Svc Pct 3";
    protected static final String FIELD_SPED_DIV_3 = "SPED Div 3";
    protected static final String FIELD_SPED_SCHOOL_3 = "SPED School 3";
    protected static final String FIELD_SPED_TUITION_S1 = "SPED Tuition S1";
    protected static final String FIELD_CAMBRIDGE_PROGRAM = "Cambridge Program";
    protected static final String FIELD_SPED_TUIT_DISABILITY = "SPED Tuit Disability";
    protected static final String FIELD_REGULAR_EC = "Regular EC";
    protected static final String FIELD_SPED_EC = "SPED EC";
    protected static final String FIELD_PARENT_PLACED = "Parent Placed";
    protected static final String FIELD_MOP_FLAG = "MOP Flag";
    protected static final String FIELD_MOP_CLASSES = "MOP Classes";
    protected static final String FIELD_RESIDENT_DIVISION = "Resident Division";
    protected static final String FIELD_SPED_TUITION_S2 = "SPED Tuition S2";
    protected static final String FIELD_SPED_TUITION_SUM = "SPED Tuition Sum";
    protected static final String FIELD_MILITARY_STATUTE = "Military Statute";
    protected static final String FIELD_REPORTING_SCHOOL = "Reporting School";
    protected static final String FIELD_FIRST_NAME = "First Name";
    protected static final String FIELD_MIDDLE_NAME = "Middle Name";
    protected static final String FIELD_LAST_NAME = "Last Name";
    protected static final String FIELD_CRED_ACC = "Credit Accommodation";
    protected static final String FIELD_LIEP = "LIEP";
    protected static final String FIELD_HEAD_START_PROVIDER = "Head Start Provider Code";
    protected static final String FIELD_REGIONAL_LOCAL_CENTER_PERCENT_OF_TIME = "Regional/Local Center Percent of Time";
    protected static final String FIELD_REMOTE_INST_PERCENT_OF_TIME = "Remote Instruction Percent of Time";
    protected static final String FIELD_INTERNET_ACCESS_REMOTE_LEARNING = "Internet Access for Remote Learning";
    protected static final String FIELD_DEVICE_ACCESS_REMOTE_LEARNING = "Device Access for Remote Learning";
    protected static final String FIELD_PARENTAL_REMOTE_LEARNING_DECISION = "Parental Remote Learning Decision";
    protected static final String FIELD_SLIFE_STATUS_FLAG = "SLIFE Status Flag";
    protected static final String FIELD_DATE_OF_ENTRY = "Date of Entry into US Schools";

    /*
     * Constants for reporting parameters.
     */
    protected static final String PARAM_ACTIVE = "A";
    protected static final String PARAM_COLLECTION = "collection";
    protected static final String PARAM_CURRENT_SCHOOL_STATUS = "A";
    protected static final String PARAM_MISSING_SASID = "missingSasid";
    protected static final String PARAM_NON_REQUIRED = "removeNonrequiredFields";
    protected static final String PARAM_PREREG_ONLY = "preregOnly";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_QUERY_BY = "queryBy";
    protected static final String PARAM_QUERY_STRING = "queryString";
    protected static final String PARAM_SORT = "sort";
    protected static final String PARAM_SUMMER_DATE = "summerBeginDate";

    /*
     * Other internal constants
     */
    protected static final String ASD_ID_LAVC = "LAVC";
    protected static final String CODE_ATT_DU = "DU";
    protected static final String DEFAULT_CALENDAR_NAME = "Standard";
    protected static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";
    protected static final String ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED =
            " Assessment Definition is not loaded in the Data Dictionary.";
    protected static final String ERROR_TYPE_WARNING = "Warning";
    protected static final String COLLECTION_FALL = "FALL";
    protected static final String COLLECTION_SPRING = "SPR";
    protected static final String COLLECTION_EOY = "EOY";
    protected static final String COLLECTION_SUMMER = "SUM";
    protected static final String COLLECTION_PRESUB = "PRESUB";
    protected static final String COLLECTION_SPED = "SpecialED";
    protected static final String COLLECTION_SNAPSHOT = "snapshot";
    protected static final String CONTINUATION_CODE_E119 = "E119";
    protected static final String STATUS_NO_SHOW = "N";
    protected static final String STATUS_SERVICE = "Services";
    protected static final String STATUS_VIRTUAL_CLASS = "V";
    protected static final List<String> HB_ENTRY_CODES = Arrays.asList("R216", "R217", "R218", "R219");

    /*
     * Lists of non-required field IDs by collection. The first entry in each sub-list is the
     * collection ID. The rest of
     * the fields in each sub-list are field ID that are not required.
     */
    private static final String[][] NON_REQUIRED_FIELDS = {
            {COLLECTION_FALL, FIELD_GIFTED, FIELD_GIFTED_REFERRAL, FIELD_INTL_BACC, FIELD_ADVANCED_PLACEMENT,
                    FIELD_DUAL_ENROLLMENT, FIELD_CTE_FINISHER, FIELD_CTE_CAREER_CLUSTER, FIELD_CTE_SPECIAL_POPULAT,
                    FIELD_RETENTION, FIELD_TRUANCY_CONFERENCE, FIELD_DIPLOMA_SEAL, FIELD_CTE_DUAL_ENROLLMENT,
                    FIELD_CTE_ATTAINMENT, FIELD_CTE_PROGRAM, FIELD_ADDRESS_2,
                    FIELD_PHONE_NUMBER, FIELD_UNEXCUSED_ABSENCES, FIELD_IB_CODE, FIELD_SPED_PLACEMENT,
                    FIELD_SPED_REG_CLASS_PCT, FIELD_SPED_PRIM_SVC_PCT, FIELD_SPED_DISABILITY_2, FIELD_SPED_SVC_PCT_2,
                    FIELD_SPED_DIV_2, FIELD_SPED_SCHOOL_2, FIELD_SPED_DISABILITY_3, FIELD_SPED_SVC_PCT_3,
                    FIELD_SPED_DIV_3, FIELD_SPED_SCHOOL_3, FIELD_SPED_TUITION_S1, FIELD_CAMBRIDGE_PROGRAM,
                    FIELD_SPED_TUIT_DISABILITY, FIELD_REGULAR_EC, FIELD_SPED_EC, FIELD_PARENT_PLACED,
                    FIELD_SPED_TUITION_S2, FIELD_SPED_TUITION_SUM, FIELD_LIEP,
                    FIELD_REGIONAL_LOCAL_CENTER_PERCENT_OF_TIME},
            {COLLECTION_SPRING, FIELD_CTE_CAREER_CLUSTER, FIELD_SPED_TIME_PCT, FIELD_COUNTY_BIRTH, FIELD_IMMIGRANT,
                    FIELD_GIFTED, FIELD_GIFTED_REFERRAL, FIELD_INTL_BACC, FIELD_ADVANCED_PLACEMENT,
                    FIELD_DUAL_ENROLLMENT, FIELD_CTE_FINISHER, FIELD_RETENTION, FIELD_TRUANCY_CONFERENCE,
                    FIELD_HOMEROOM, FIELD_DIPLOMA_SEAL, FIELD_CTE_DUAL_ENROLLMENT, FIELD_CTE_ATTAINMENT,
                    FIELD_CTE_PROGRAM, FIELD_ADDRESS_1, FIELD_ADDRESS_2, FIELD_ZIP_CODE, FIELD_PHONE_NUMBER,
                    FIELD_UNEXCUSED_ABSENCES, FIELD_IB_CODE, FIELD_SPED_PLACEMENT, FIELD_SPED_REG_CLASS_PCT,
                    FIELD_SPED_PRIM_SVC_PCT, FIELD_SPED_SVC_PCT_2, FIELD_SPED_DIV_2, FIELD_SPED_SCHOOL_2,
                    FIELD_SPED_SVC_PCT_3, FIELD_SPED_DIV_3, FIELD_SPED_SCHOOL_3, FIELD_CAMBRIDGE_PROGRAM,
                    FIELD_REGULAR_EC, FIELD_SPED_EC, FIELD_PARENT_PLACED, FIELD_SPED_TUITION_S2,
                    FIELD_SPED_TUITION_SUM, FIELD_LIEP, FIELD_REGIONAL_LOCAL_CENTER_PERCENT_OF_TIME},
            {COLLECTION_EOY, FIELD_SPED_TIME_PCT, FIELD_COUNTY_BIRTH, FIELD_IMMIGRANT, FIELD_HOMEROOM,
                    FIELD_SPED_PLACEMENT, FIELD_SPED_REG_CLASS_PCT, FIELD_SPED_PRIM_SVC_PCT, FIELD_SPED_SVC_PCT_2,
                    FIELD_SPED_DIV_2, FIELD_SPED_SCHOOL_2, FIELD_SPED_SVC_PCT_3, FIELD_SPED_DIV_3, FIELD_SPED_SCHOOL_3,
                    FIELD_SPED_TUITION_S1, FIELD_REGULAR_EC, FIELD_SPED_EC, FIELD_PARENT_PLACED,
                    FIELD_SPED_TUITION_SUM, FIELD_LIEP},
            {COLLECTION_SUMMER, FIELD_STATUS, FIELD_ENTRY_CODE, FIELD_ENTRY_DATE, FIELD_EXIT_CODE, FIELD_EXIT_DATE,
                    FIELD_KG_HALFDAY, FIELD_SPED_TIME_PCT, FIELD_COUNTY_BIRTH, FIELD_HOME_LANGUAGE, FIELD_IMMIGRANT,
                    FIELD_GIFTED, FIELD_GIFTED_REFERRAL, FIELD_TITLE1, FIELD_ADVANCED_PLACEMENT, FIELD_DUAL_ENROLLMENT,
                    FIELD_W8_REASON, FIELD_DAYS_PRESENT, FIELD_DAYS_ABSENT, FIELD_RETENTION, FIELD_TRUANCY_CONFERENCE,
                    FIELD_NON_PUB_FTE, FIELD_KG_READINESS, FIELD_HOMEROOM, FIELD_SCHOOL_CHOICE, FIELD_SUPP_ED,
                    FIELD_EARLY_COLLEGE_SCHOL, FIELD_DISTANCE_LEARNING, FIELD_PK_EXP, FIELD_PK_WEEKLY_TIME,
                    FIELD_UNEXCUSED_ABSENCES, FIELD_FT_VIRTUAL_PROGRAM, FIELD_IB_CODE, FIELD_SPED_PLACEMENT,
                    FIELD_SPED_REG_CLASS_PCT, FIELD_SPED_PRIM_SVC_PCT, FIELD_SPED_DISABILITY_2, FIELD_SPED_SVC_PCT_2,
                    FIELD_SPED_DIV_2, FIELD_SPED_SCHOOL_2, FIELD_SPED_DISABILITY_3, FIELD_SPED_SVC_PCT_3,
                    FIELD_SPED_DIV_3, FIELD_SPED_SCHOOL_3, FIELD_SPED_TUITION_S1, FIELD_CAMBRIDGE_PROGRAM,
                    FIELD_SPED_TUIT_DISABILITY, FIELD_REGULAR_EC, FIELD_SPED_EC, FIELD_PARENT_PLACED, FIELD_MOP_FLAG,
                    FIELD_MOP_CLASSES, FIELD_RESIDENT_DIVISION, FIELD_SPED_TUITION_S2, FIELD_SPED_TUITION_SUM,
                    FIELD_MILITARY_STATUTE, FIELD_HEAD_START_PROVIDER, FIELD_REGIONAL_LOCAL_CENTER_PERCENT_OF_TIME,
                    FIELD_REMOTE_INST_PERCENT_OF_TIME, FIELD_INTERNET_ACCESS_REMOTE_LEARNING,
                    FIELD_DEVICE_ACCESS_REMOTE_LEARNING, FIELD_PARENTAL_REMOTE_LEARNING_DECISION},
            {COLLECTION_PRESUB, FIELD_KG_HALFDAY, FIELD_SPED_TIME_PCT, FIELD_COUNTY_BIRTH, FIELD_HOME_LANGUAGE,
                    FIELD_IMMIGRANT, FIELD_GIFTED, FIELD_GIFTED_REFERRAL, FIELD_GED_PROGRAM, FIELD_INTL_BACC,
                    FIELD_GRADUATE_COMP, FIELD_GRADUATE_COMP, FIELD_ADVANCED_PLACEMENT, FIELD_DUAL_ENROLLMENT,
                    FIELD_CTE_FINISHER, FIELD_CTE_CAREER_CLUSTER, FIELD_CTE_SPECIAL_POPULAT, FIELD_W8_REASON,
                    FIELD_DAYS_PRESENT, FIELD_DAYS_ABSENT, FIELD_RETENTION, FIELD_TRUANCY_CONFERENCE,
                    FIELD_TUITION_PAID, FIELD_NON_PUB_FTE, FIELD_KG_READINESS, FIELD_HOMEROOM, FIELD_SCHOOL_CHOICE,
                    FIELD_SUPP_ED, FIELD_DIPLOMA_SEAL, FIELD_EARLY_COLLEGE_SCHOL, FIELD_DISTANCE_LEARNING, FIELD_PK_EXP,
                    FIELD_PK_WEEKLY_TIME, FIELD_CTE_DUAL_ENROLLMENT, FIELD_CTE_ATTAINMENT, FIELD_CTE_PROGRAM,
                    FIELD_ADDRESS_1, FIELD_ADDRESS_2, FIELD_ZIP_CODE, FIELD_PHONE_NUMBER, FIELD_UNEXCUSED_ABSENCES,
                    FIELD_NIGHT_RESIDENCE, FIELD_NEGLECTED_DELINQNT, FIELD_FT_VIRTUAL_PROGRAM, FIELD_IB_CODE,
                    FIELD_UNACCOMP_HOMELESS, FIELD_SPED_PLACEMENT, FIELD_SPED_REG_CLASS_PCT, FIELD_SPED_PRIM_SVC_PCT,
                    FIELD_SPED_DISABILITY_2, FIELD_SPED_SVC_PCT_2, FIELD_SPED_DIV_2, FIELD_SPED_SCHOOL_2,
                    FIELD_SPED_DISABILITY_3, FIELD_SPED_SVC_PCT_3, FIELD_SPED_DIV_3, FIELD_SPED_SCHOOL_3,
                    FIELD_SPED_TUITION_S1, FIELD_CAMBRIDGE_PROGRAM, FIELD_SPED_TUIT_DISABILITY, FIELD_REGULAR_EC,
                    FIELD_SPED_EC, FIELD_PARENT_PLACED, FIELD_MOP_FLAG, FIELD_MOP_CLASSES, FIELD_RESIDENT_DIVISION,
                    FIELD_SPED_TUITION_S2, FIELD_SPED_TUITION_SUM, FIELD_MILITARY_STATUTE, FIELD_HEAD_START_PROVIDER,
                    FIELD_REGIONAL_LOCAL_CENTER_PERCENT_OF_TIME, FIELD_REMOTE_INST_PERCENT_OF_TIME,
                    FIELD_INTERNET_ACCESS_REMOTE_LEARNING, FIELD_DEVICE_ACCESS_REMOTE_LEARNING,
                    FIELD_PARENTAL_REMOTE_LEARNING_DECISION},
            {COLLECTION_SPED, FIELD_EXIT_CODE, FIELD_EXIT_DATE, FIELD_KG_HALFDAY, FIELD_SPED_TIME_PCT,
                    FIELD_COUNTY_BIRTH, FIELD_HOME_LANGUAGE, FIELD_IMMIGRANT, FIELD_GIFTED, FIELD_GIFTED_REFERRAL,
                    FIELD_TITLE1, FIELD_GED_PROGRAM, FIELD_INTL_BACC, FIELD_GRADUATE_COMP, FIELD_GRADUATE_PLAN,
                    FIELD_ADVANCED_PLACEMENT, FIELD_DUAL_ENROLLMENT, FIELD_CTE_FINISHER, FIELD_CTE_CAREER_CLUSTER,
                    FIELD_CTE_SPECIAL_POPULAT, FIELD_W8_REASON, FIELD_DAYS_PRESENT, FIELD_DAYS_ABSENT, FIELD_RETENTION,
                    FIELD_TRUANCY_CONFERENCE, FIELD_NON_PUB_FTE, FIELD_KG_READINESS, FIELD_HOMEROOM,
                    FIELD_SCHOOL_CHOICE, FIELD_SUPP_ED, FIELD_DIPLOMA_SEAL, FIELD_EARLY_COLLEGE_SCHOL,
                    FIELD_DISTANCE_LEARNING, FIELD_PK_EXP, FIELD_PK_WEEKLY_TIME, FIELD_CTE_DUAL_ENROLLMENT,
                    FIELD_CTE_ATTAINMENT, FIELD_CTE_PROGRAM, FIELD_ADDRESS_1, FIELD_ADDRESS_2, FIELD_ZIP_CODE,
                    FIELD_PHONE_NUMBER, FIELD_UNEXCUSED_ABSENCES, FIELD_NIGHT_RESIDENCE, FIELD_NEGLECTED_DELINQNT,
                    FIELD_FT_VIRTUAL_PROGRAM, FIELD_IB_CODE, FIELD_UNACCOMP_HOMELESS, FIELD_SPED_TUITION_S1,
                    FIELD_CAMBRIDGE_PROGRAM, FIELD_MOP_FLAG, FIELD_MOP_CLASSES, FIELD_RESIDENT_DIVISION,
                    FIELD_SPED_TUITION_S2, FIELD_MILITARY_STATUTE, FIELD_HEAD_START_PROVIDER,
                    FIELD_REGIONAL_LOCAL_CENTER_PERCENT_OF_TIME, FIELD_REMOTE_INST_PERCENT_OF_TIME,
                    FIELD_INTERNET_ACCESS_REMOTE_LEARNING, FIELD_DEVICE_ACCESS_REMOTE_LEARNING,
                    FIELD_PARENTAL_REMOTE_LEARNING_DECISION},
            {COLLECTION_SNAPSHOT, FIELD_KG_HALFDAY, FIELD_SPED_TIME_PCT, FIELD_DISADV_STATUS, FIELD_COUNTY_BIRTH,
                    FIELD_HOME_LANGUAGE, FIELD_IMMIGRANT, FIELD_GIFTED, FIELD_GIFTED_REFERRAL, FIELD_GED_PROGRAM,
                    FIELD_INTL_BACC, FIELD_GRADUATE_COMP, FIELD_GRADUATE_COMP, FIELD_ADVANCED_PLACEMENT,
                    FIELD_DUAL_ENROLLMENT, FIELD_CTE_FINISHER, FIELD_CTE_CAREER_CLUSTER, FIELD_CTE_SPECIAL_POPULAT,
                    FIELD_DAYS_PRESENT, FIELD_DAYS_ABSENT, FIELD_RETENTION, FIELD_TRUANCY_CONFERENCE,
                    FIELD_TUITION_PAID, FIELD_NON_PUB_FTE, FIELD_KG_READINESS, FIELD_HOMEROOM, FIELD_SCHOOL_CHOICE,
                    FIELD_SUPP_ED, FIELD_DIPLOMA_SEAL, FIELD_EARLY_COLLEGE_SCHOL, FIELD_DISTANCE_LEARNING, FIELD_PK_EXP,
                    FIELD_PK_WEEKLY_TIME, FIELD_CTE_DUAL_ENROLLMENT, FIELD_CTE_ATTAINMENT, FIELD_CTE_PROGRAM,
                    FIELD_ADDRESS_1, FIELD_ADDRESS_2, FIELD_ZIP_CODE, FIELD_PHONE_NUMBER, FIELD_UNEXCUSED_ABSENCES,
                    FIELD_NIGHT_RESIDENCE, FIELD_NEGLECTED_DELINQNT, FIELD_FT_VIRTUAL_PROGRAM, FIELD_IB_CODE,
                    FIELD_UNACCOMP_HOMELESS, FIELD_SPED_PLACEMENT, FIELD_SPED_REG_CLASS_PCT, FIELD_SPED_PRIM_SVC_PCT,
                    FIELD_SPED_DISABILITY_2, FIELD_SPED_SVC_PCT_2, FIELD_SPED_DIV_2, FIELD_SPED_SCHOOL_2,
                    FIELD_SPED_DISABILITY_3, FIELD_SPED_SVC_PCT_3, FIELD_SPED_DIV_3, FIELD_SPED_SCHOOL_3,
                    FIELD_SPED_TUITION_S1, FIELD_CAMBRIDGE_PROGRAM, FIELD_SPED_TUIT_DISABILITY, FIELD_REGULAR_EC,
                    FIELD_SPED_EC, FIELD_PARENT_PLACED, FIELD_MOP_FLAG, FIELD_MOP_CLASSES, FIELD_RESIDENT_DIVISION,
                    FIELD_SPED_TUITION_S2, FIELD_SPED_TUITION_SUM, FIELD_MILITARY_STATUTE, FIELD_HEAD_START_PROVIDER,
                    FIELD_REGIONAL_LOCAL_CENTER_PERCENT_OF_TIME, FIELD_REMOTE_INST_PERCENT_OF_TIME,
                    FIELD_INTERNET_ACCESS_REMOTE_LEARNING, FIELD_DEVICE_ACCESS_REMOTE_LEARNING,
                    FIELD_PARENTAL_REMOTE_LEARNING_DECISION}};

    /*
     * Instance variables.
     */
    protected String m_activeCode;
    private DataDictionary m_asmDictLAVC;
    protected String m_preRegCode;
    protected Map<String, Collection<StudentAssessment>> m_asmLAVCMap =
            new HashMap<String, Collection<StudentAssessment>>();
    protected Map<String, List<StudentAttendance>> m_attendance;
    protected String m_collection;
    protected Iterator m_dataIterator;
    protected EnrollmentManager m_enrollmentManager;
    protected Map<String, List<StudentEnrollment>> m_enrollmentMap;
    protected Map<String, List<Transcript>> m_transcriptMap;
    protected String m_enrollmentCodeRefTableOid;
    protected String m_enrollmentReasonRefTableOid;
    protected String m_fieldAsmCreditAccomodation;
    protected String m_fieldAsmIntensiveSupportSvs;
    protected String m_fieldExcludeSchool;
    protected String m_fieldExcludeStudent;
    protected String m_fieldCteFinisher;
    protected String m_fieldMopFlag;
    protected String m_fieldSasid;
    protected String m_fieldSchoolTuition;
    protected String m_fieldTuitionDisability;
    protected String m_fieldTuitionS1;
    protected String m_fieldTuitionS2;
    protected String m_fieldTuitionSUM;
    protected String m_fieldSpedDisability;
    protected String m_fieldHBEntryCode;
    protected String m_fieldHBWithdrawalCode;
    protected String m_fieldUsEntryDate;
    protected String m_fieldUsEndDate;
    protected String m_fieldImmigrantStatus;
    protected Integer m_sort;
    protected String m_isaepCode;
    protected boolean m_missingSasid;
    protected boolean m_preregOnly;
    protected Map<String, Collection<Race>> m_raceCodeMap;
    protected Map<String, ReferenceCode> m_raceCodes;
    protected boolean m_removeNonrequiredFields;
    protected PlainDate m_reportDate;
    protected int m_rowCount;
    protected Map<String, Collection<StudentSchedule>> m_scheduleMap;
    protected PlainDate m_districtStartDate;
    protected String m_fieldVirtualProgram;
    protected HashMap m_schoolsToCalendars;
    protected List<String> m_serviceStatusCodes;
    protected PlainDate m_summerBeginDate;
    protected Character m_vasrcDelimiterChar;
    protected List<String> m_virtualStatusCodes;
    protected String m_withdrawalCodeRefTableOid;
    protected String m_withdrawalReasonRefTableOid;
    protected ArrayList<String> m_schoolOverride;
    protected Map<String, PlainDate> m_firstDaysOfSchools;
    protected String m_specialPopulationJavaname;

    /*
     * data fields for values, looked up from aliases.
     */
    protected String m_fieldCteCluster;
    protected String m_fieldDistrictCode;
    protected String m_fieldEnrAttPlanCode;
    protected String m_fieldEnrAttConfCode;
    protected String m_fieldEnrCourtRefCode;
    protected String m_fieldGed;
    protected String m_fieldHomeDistrictCode;
    protected String m_fieldHomeSchoolCode;
    protected String m_fieldSchoolCode;
    protected String m_fieldServiceDistrictCode;
    protected String m_fieldServiceSchoolCode;
    protected String m_fieldStdAttPlanCode;
    protected String m_fieldStdAttConfCode;
    protected String m_fieldStdCourtRefCode;
    protected String m_fieldStdCteCluster;
    protected String m_fieldStdHeadStartProviderCode;
    protected String m_fieldStdHomeSchoolCode;
    protected String m_fieldPgmLIEP;
    protected String m_fieldPgmRegionalLocalCenterPercentofTime;
    protected String m_fieldPgmRemoteInstructionPercentofTime;
    protected String m_fieldPgmInternetAccessforRemoteLearning;
    protected String m_fieldPgmDeviceAccessforRemoteLearning;
    protected String m_fieldPgmParentalRemoteLearningDecision;


    /*
     * Translated a sum of race code state codes (1,2,4,8,16) into VA race code representation.
     */
    protected int[] m_raceValueTranslator = new int[] {0, // 0
            5, // 1 W
            3, // 2 B
            14, // 3 WB
            2, // 4 A
            12, // 5 AW
            11, // 6 AB
            20, // 7 AWB
            1, // 8 I
            9, // 9 IW
            8, // 10 IB
            24, // 11 IWB
            7, // 12 IA
            18, // 13 IAW
            17, // 14 IAB
            27, // 15 IAWB
            6, // 16 P
            16, // 17 PW
            15, // 18 PB
            22, // 19 PWB
            13, // 20 PA
            26, // 21 PAW
            21, // 22 PAB
            28, // 23 PAWB
            10, // 24 PI
            25, // 25 PIW
            23, // 26 PIB
            29, // 27 PIWB
            19, // 28 PIA
            30, // 29 PIAW
            31, // 30 PIAB
            32}; // 31
                 // PIAWB

    /**
     * Retrieve LAVC ASM values.
     */
    protected class RetrieveAsmLAVC implements FieldRetriever {

        protected static final String CALC_ID = "ASM-LAVC";
        protected static final String CALC_PARAM_CREDIT_ACC = "CREDIT_ACC";
        protected static final String CALC_PARAM_INTENSIVE_SUPPORT = "INTENSIVE_SUPPORT";
        protected static final String CALC_PARAM_LAVC = "LAVC";
        protected static final String CALC_PARAM_SLAVC = "SLAVC";

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
            VAStudentRecordCollection srcData = (VAStudentRecordCollection) data;
            String value = null;
            if (COLLECTION_EOY.equals(srcData.m_collection)
                    || COLLECTION_SUMMER.equals(srcData.m_collection)) {
                String gradeLevel = entity.getFieldValue(FIELD_GRADE_LEVEL);
                Collection<StudentAssessment> asmLAVCList = srcData.m_asmLAVCMap.get(entity.getBean().getOid());
                if (asmLAVCList != null && !asmLAVCList.isEmpty()) {
                    if (CALC_PARAM_CREDIT_ACC.equals(field.getParameter())) {
                        int valueToReturn = 0;
                        for (StudentAssessment asm : asmLAVCList) {
                            String creditCode =
                                    (String) asm.getFieldValueByBeanPath(srcData.m_fieldAsmCreditAccomodation);
                            if (!StringUtils.isEmpty(creditCode)) {
                                DataDictionary dictionary = getAssessmentDictionary();
                                DataDictionaryField dictField =
                                        dictionary.findDataDictionaryFieldByAlias(ALIAS_ASM_CREDIT_ACC_CODE);
                                if (dictField != null && dictField.getReferenceTableOid() != null) {
                                    String creditCodeState = data.lookupReferenceCodeByRefTbl(
                                            dictField.getReferenceTableOid(), creditCode,
                                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                                    if (!StringUtils.isEmpty(creditCodeState) && creditCodeState.matches("\\d+")) {
                                        valueToReturn += Integer.valueOf(creditCodeState).intValue();
                                    }
                                }
                            }
                        }
                        if (valueToReturn > 0) {
                            value = String.valueOf(valueToReturn);
                        }
                    } else if (CALC_PARAM_LAVC.equals(field.getParameter())) {
                        if ("12".equals(gradeLevel)) {
                            value = String.valueOf(asmLAVCList.size());
                        }
                    } else if (CALC_PARAM_SLAVC.equals(field.getParameter())) {
                        if ("12".equals(gradeLevel)) {
                            if (!StringUtils.isEmpty(entity.getFieldValue(FIELD_PRIMARY_DISABILITY))) {
                                value = String.valueOf(asmLAVCList.size());
                            }
                        }
                    }
                }
            }
            if (CALC_PARAM_INTENSIVE_SUPPORT.equals(field.getParameter())
                    && !COLLECTION_SUMMER.equals(srcData.m_collection)) {
                Collection<StudentAssessment> asmLAVCList = srcData.m_asmLAVCMap.get(entity.getBean().getOid());
                if (asmLAVCList != null && !asmLAVCList.isEmpty()) {
                    for (StudentAssessment asmLAVC : asmLAVCList) {
                        String intSupportCode =
                                (String) asmLAVC.getFieldValueByBeanPath(srcData.m_fieldAsmIntensiveSupportSvs);
                        if (!StringUtils.isEmpty(intSupportCode)) {
                            DataDictionary dictionary = getAssessmentDictionary();
                            DataDictionaryField dictField =
                                    dictionary.findDataDictionaryFieldByAlias(ALIAS_ASM_INTENSIVE_SUPPORT);
                            if (dictField != null && dictField.getReferenceTableOid() != null) {
                                value = data.lookupReferenceCodeByRefTbl(
                                        dictField.getReferenceTableOid(), intSupportCode,
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                                break;
                            }
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retrieve codes and dates from student enrollment.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveAttendance implements FieldRetriever {

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
            String param = (String) field.getParameter();
            StudentRecordEntity srcEntity = (StudentRecordEntity) entity;
            MembershipAttendance attendance = srcEntity.getMembershipAttendance();

            if (param.startsWith("PRESENT")) {
                value = Float.valueOf(attendance.getAttendance());
            } else if (param.startsWith("ABSENT")) {
                value = Float.valueOf(attendance.getAbsent());
            } else if (param.startsWith("UNEXCUSED")) {
                value = Float.valueOf(attendance.getUnexAbsent());
            }

            // If student enrollment status N,
            // then present, absent and unexcused values should all be "0".
            // Same fo students who are "test takers,"
            // indicated by grade level equaling "TT". N status include test
            // takers student.
            String status = entity.getFieldValue(FIELD_STATUS);
            if ("N".equals(status)) {
                value = Float.valueOf(0);
            }

            return value;
        }
    }

    /**
     * Retrieve the CTE Finisher code. A default value of "4" is exported for Pre-submission and
     * EOY
     * collections for students in
     * grades 7, 8, 9, 10, 11, 12 or TT if the student has not been scheduled for a course in
     * the
     * district flagged as CTE since grade 7.
     *
     * @author X2 Development Corporation
     *
     */

    protected class RetrieveCTEFinisher implements FieldRetriever {

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
            X2BaseBean entityBean = entity.getBean();
            String value = (String) data.getProperty(entityBean, m_fieldCteFinisher);
            StudentRecordEntity sre = (StudentRecordEntity) entity;
            Set<String> gradeLevels = new HashSet<String>(Arrays.asList("07", "08", "09", "10", "11", "12", "TT"));
            if ((COLLECTION_PRESUB.equals(m_collection) || COLLECTION_EOY.equals(m_collection))
                    && sre.m_gradeLevel != null && gradeLevels.contains(sre.m_gradeLevel)
                    && StringUtils.isEmpty(value)) {
                boolean foundCteCourse = false;
                List<Transcript> transcripts = m_transcriptMap.get(entityBean.getOid());
                if (transcripts != null && !transcripts.isEmpty()) {
                    for (Transcript transcript : transcripts) {
                        String transcriptGradeLevel = transcript.getGradeLevel();
                        if (!StringUtils.isEmpty(transcriptGradeLevel)
                                && gradeLevels.contains(transcriptGradeLevel)) {
                            SchoolCourse transcriptSchoolCourse = transcript.getSchoolCourse();
                            if (transcriptSchoolCourse != null) {
                                Course transcriptCourse = transcriptSchoolCourse.getCourse();
                                if (transcriptCourse != null) {
                                    Course course = transcriptCourse.getRootCourse();
                                    String isCteCourse =
                                            (String) course.getFieldValueByAlias(ALIAS_CAREER_AND_TECH_ED);
                                    if (BooleanAsStringConverter.TRUE.equals(isCteCourse)) {
                                        foundCteCourse = true;
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
                // If no CTE Course is found on Transcripts, continue looking on current student
                // schedule
                if (!foundCteCourse) {
                    Collection<StudentSchedule> studentSchedules = m_scheduleMap.get(entityBean.getOid());
                    if (studentSchedules != null && !studentSchedules.isEmpty()) {
                        for (StudentSchedule studentSchedule : studentSchedules) {
                            MasterSchedule studentSection = studentSchedule.getSection();
                            if (studentSection != null) {
                                SchoolCourse scheduleSchoolCourse = studentSection.getSchoolCourse();
                                if (scheduleSchoolCourse != null) {
                                    Course scheduleCourse = scheduleSchoolCourse.getCourse();
                                    if (scheduleCourse != null) {
                                        Course course = scheduleCourse.getRootCourse();
                                        String isCteCourse =
                                                (String) course.getFieldValueByAlias(ALIAS_CAREER_AND_TECH_ED);
                                        if (BooleanAsStringConverter.TRUE.equals(isCteCourse)) {
                                            foundCteCourse = true;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                if (!foundCteCourse) {
                    return "4";
                }
            }
            // If student does not meet above scenario, look up state code for the value and
            // return
            // that.
            value = lookupStateValue(SisStudent.class, m_fieldCteFinisher, value);
            return value;
        }
    }

    /**
     * Filter fields to only display students' address if CTE completer flag is set.
     * <p>
     * <ul>
     * <li>1 - Finished CTE requirements for completion
     * <li>5 - Student finished CTE requirements for completion and completion of a CTE dual
     * enrollment course
     * </ul>
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCTEOnly implements FieldRetriever {
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
            VAStudentRecordCollection src = (VAStudentRecordCollection) data;

            String value = (String) data.getProperty(entity.getBean(), field.getBeanPath());
            String cteCompleter = entity.getFieldValue(FIELD_CTE_FINISHER);
            if (!"1".equals(cteCompleter) && !"2".equals(cteCompleter) && !"5".equals(cteCompleter)
                    && !COLLECTION_FALL.equals(src.m_collection) && !FIELD_ADDRESS_1.equals(field.getFieldId())
                    && !FIELD_ZIP_CODE.equals(field.getFieldId())) {
                value = null;
            }
            return value;
        }
    }

    /**
     * Retriever for Attendance Plan, Attendance Conference and Court Referral Codes.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCodes implements FieldRetriever {
        protected static final String CALC_ID = "SRC-CODES";
        protected static final String CALC_PARAM_ATT_CONF = "ATT_CONF";
        protected static final String CALC_PARAM_ATT_PLAN = "ATT_PLAN";
        protected static final String CALC_PARAM_COURT_REF = "COURT_REF";

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
            StudentRecordEntity srcEntity = (StudentRecordEntity) entity;
            MembershipAttendance attendance = srcEntity.getMembershipAttendance();
            SisStudent student = (SisStudent) entity.getBean();
            String param = (String) field.getParameter();
            // Find the entry code, if any.
            StudentEnrollment wEnrollment = attendance.getExitEnrollment();
            String code = null;
            if (CALC_PARAM_ATT_PLAN.equals(param)) {
                code = getCalculatedValue(wEnrollment, student, m_fieldStdAttPlanCode, m_fieldEnrAttPlanCode);
            } else if (CALC_PARAM_ATT_CONF.equals(param)) {
                code = getCalculatedValue(wEnrollment, student, m_fieldStdAttConfCode, m_fieldEnrAttConfCode);
            } else if (CALC_PARAM_COURT_REF.equals(param)) {
                code = getCalculatedValue(wEnrollment, student, m_fieldStdCourtRefCode, m_fieldEnrCourtRefCode);
            }
            return code;
        }

        private String getCalculatedValue(StudentEnrollment wEnrollment,
                                          SisStudent student,
                                          String stdBeanPath,
                                          String enrBeanPath) {
            String value = null;
            if (wEnrollment != null) {
                value = lookupStateValue(StudentEnrollment.class, enrBeanPath,
                        (String) wEnrollment.getFieldValueByBeanPath(enrBeanPath));
            } else {
                value = lookupStateValue(SisStudent.class, stdBeanPath,
                        (String) student.getFieldValueByBeanPath(stdBeanPath));
            }
            return value;
        }
    }

    /**
     * Retrieve a field value, clean any non-digit character. Used for phone numbers to remove
     * parens and dashes.
     *
     * Filter: Only display phone number if CTE completer is set.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveDigits implements FieldRetriever {
        String ILLEGAL_CHARACTERS = "\\D";
        Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_CHARACTERS);

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
            String value = (String) data.getProperty(entity.getBean(), field.getBeanPath());
            String cteCompleter = entity.getFieldValue(FIELD_CTE_FINISHER);
            if (!"1".equals(cteCompleter) && !"2".equals(cteCompleter)) {
                value = null;
            }

            if (!StringUtils.isEmpty(value)) {
                Matcher matcher = m_illegalNameCharacters.matcher(value);
                value = matcher.replaceAll(EMPTY_STRING);
            }
            return value;
        }
    }

    /**
     * Filter out 504 disability codes.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveDisability implements FieldRetriever {

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
            String value = (String) data.getProperty(entity.getBean(), field.getBeanPath());
            if (!StringUtils.isEmpty(value) && "504".equals(value.trim())) {
                value = null;
            }
            return value;
        }
    }

    /**
     * Returns the disadvantaged status indicator. If the initial nighttime residence is not
     * empty,
     * this must be TRUE.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveDisadvantavedStatus implements FieldRetriever {

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
            String value = (String) data.getProperty(entity.getBean(), field.getBeanPath());
            String nighttimeResidence = entity.getFieldValue(FIELD_NIGHT_RESIDENCE);
            if (!StringUtils.isEmpty(nighttimeResidence)) {
                value = BooleanAsStringConverter.TRUE;
            }
            return value;
        }
    }

    /**
     * Retrieve the students home or serving school or district. Apply enrollment overrides for
     * these if necessary.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveDistrictSchool implements FieldRetriever {

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
            String param = (String) field.getParameter();
            StudentRecordEntity srcEntity = (StudentRecordEntity) entity;
            SisStudent student = (SisStudent) entity.getBean();
            String value = null;

            // So we get our original value
            value = getSchoolValueNoOverride(param, srcEntity, student);

            if (m_schoolOverride.contains(value) && ALIAS_SERVICE_SCHOOL_CODE.equals(param)) {
                value = getSchoolValueNoOverride(ALIAS_HOME_SCHOOL_CODE, srcEntity, student);
            }

            // If the home district is 098 and it is not EIMS, then we...
            if ("098".equals(getSchoolValueNoOverride(ALIAS_SERVICE_DISTRICT_CODE, srcEntity, student))
                    && !COLLECTION_PRESUB.equals(m_collection)) {
                // Now, we go in to see if the course is checked as a CTE or
                // Governors school
                if (srcEntity.getOverrideCodeValue(ALIAS_SERVICE_SCHOOL_CODE) != null) {
                    // If it is, then we override the home school with the
                    // serving school original value
                    // look up if there is the school checked override, if so
                    // the serving school becomes the home school
                    if (ALIAS_HOME_SCHOOL_CODE.equals(param)) {
                        value = getSchoolValueNoOverride(ALIAS_SERVICE_SCHOOL_CODE, srcEntity, student);
                    }
                    // the home district with the serving district original
                    // value
                    if (ALIAS_HOME_DISTRICT_CODE.equals(param)) {
                        value = getSchoolValueNoOverride(ALIAS_SERVICE_DISTRICT_CODE, srcEntity, student);
                    }
                    // And the serving school and district get the override
                    // values
                    if (ALIAS_SERVICE_DISTRICT_CODE.equals(param)) {
                        value = getSchoolValue(entity, ALIAS_SERVICE_DISTRICT_CODE, srcEntity, student);
                    }
                    if (ALIAS_SERVICE_SCHOOL_CODE.equals(param)) {
                        if (m_schoolOverride.contains(value)) {
                            value = getSchoolValueNoOverride(ALIAS_HOME_SCHOOL_CODE, srcEntity, student);
                        } else {
                            value = getSchoolValue(entity, ALIAS_SERVICE_SCHOOL_CODE, srcEntity, student);
                        }
                    }
                }
            }
            return value;
        }

        /**
         * Returns the school/district code.
         *
         * @param entity StateReportEntity
         * @param param String
         * @param srcEntity StudentRecordEntity
         * @param student SisStudent
         * @return String
         */
        private String getSchoolValue(StateReportEntity entity,
                                      String param,
                                      StudentRecordEntity srcEntity,
                                      SisStudent student) {
            String value = null;
            if (PARAM_CURRENT_SCHOOL_STATUS.equals(entity.getFieldValue(FIELD_STATUS))
                    && !COLLECTION_PRESUB.equals(m_collection)) {
                value = (String) srcEntity.getOverrideCodeValue(param);
            }
            if (value == null) {
                value = getSchoolValueNoOverride(param, srcEntity, student);
            }
            return value;
        }

        /**
         * Returns the school/district codes from the students' enrollment record.
         *
         * @param param String
         * @param srcEntity StudentRecordEntity
         * @param student SisStudent
         * @return String
         */
        private String getSchoolValueNoOverride(String param, StudentRecordEntity srcEntity, SisStudent student) {
            MembershipAttendance attendance = srcEntity.getMembershipAttendance();
            Map<String, Object> calcValueMap = lookupOverrides(student, attendance.getEntryEnrollment(),
                    attendance.getSchool().getOid());
            return (String) calcValueMap.get(param);
        }
    }

    /**
     * Lookup enrollment codes and dates.
     *
     * @param student SisStudent
     * @param enrollment StudentEnrollment
     * @param schoolOid String
     * @return Map
     */
    protected Map<String, Object> lookupOverrides(SisStudent student,
                                                  StudentEnrollment enrollment,
                                                  String schoolOid) {
        Map<String, Object> calcValueMap = new HashMap<String, Object>();
        SisSchool school = null;
        if (enrollment != null) {
            school = enrollment.getSchool();
        }
        if (school == null) {
            school = (SisSchool) getBroker().getBeanByOid(SisSchool.class, schoolOid);
        }
        if (school != null) {
            String homeDistrictCode = null;
            String serviceDistrictCode = null;
            String homeSchoolCode = null;
            String serviceSchoolCode = null;

            // Get override school codes if they exist.
            if (enrollment != null) {
                homeDistrictCode = (String) enrollment.getFieldValueByBeanPath(m_fieldHomeDistrictCode);
                serviceDistrictCode = (String) enrollment.getFieldValueByBeanPath(m_fieldServiceDistrictCode);
                homeSchoolCode = (String) enrollment.getFieldValueByBeanPath(m_fieldHomeSchoolCode);
                serviceSchoolCode = (String) enrollment.getFieldValueByBeanPath(m_fieldServiceSchoolCode);
            }

            // Apply default value to school and district codes.
            if (!StringUtils.isEmpty(homeDistrictCode)) {
                String stateCode =
                        lookupStateValue(StudentEnrollment.class, m_fieldHomeDistrictCode, homeDistrictCode);
                if (!StringUtils.isEmpty(stateCode)) {
                    homeDistrictCode = stateCode;
                }
            } else {
                homeDistrictCode = (String) getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode);
            }

            if (!StringUtils.isEmpty(serviceDistrictCode)) {
                String stateCode = lookupStateValue(StudentEnrollment.class, m_fieldServiceDistrictCode,
                        serviceDistrictCode);
                if (!StringUtils.isEmpty(stateCode)) {
                    serviceDistrictCode = stateCode;
                }
            } else {
                serviceDistrictCode = (String) getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode);
            }

            // Get intermediate override from student "Home School" field, if
            // present.
            if (!StringUtils.isEmpty(homeSchoolCode)) {
                String stateCode = lookupStateValue(StudentEnrollment.class, m_fieldHomeSchoolCode, homeSchoolCode);
                if (!StringUtils.isEmpty(stateCode)) {
                    homeSchoolCode = stateCode;
                }
            } else {
                if (!StringUtils.isEmpty(m_fieldStdHomeSchoolCode) && StringUtils.isEmpty(homeSchoolCode)) {
                    homeSchoolCode = (String) student.getFieldValueByBeanPath(m_fieldStdHomeSchoolCode);
                    if (!StringUtils.isEmpty(homeSchoolCode)) {
                        homeSchoolCode = lookupReferenceCodeByAlias(ALIAS_STD_HOME_SCHOOL_CODE, homeSchoolCode,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                }
                if (StringUtils.isEmpty(homeSchoolCode) && (enrollment != null)) {
                    homeSchoolCode = (String) enrollment.getSchool().getFieldValueByBeanPath(m_fieldSchoolCode);
                }
                if (StringUtils.isEmpty(homeSchoolCode)) {
                    homeSchoolCode = (String) school.getFieldValueByBeanPath(m_fieldSchoolCode);
                }
            }

            if (!StringUtils.isEmpty(serviceSchoolCode)) {
                String stateCode = lookupStateValue(StudentEnrollment.class, m_fieldServiceSchoolCode,
                        serviceSchoolCode);
                if (!StringUtils.isEmpty(stateCode)) {
                    serviceSchoolCode = stateCode;
                }
            } else if (enrollment != null) {
                serviceSchoolCode = (String) enrollment.getSchool().getFieldValueByBeanPath(m_fieldSchoolCode);
            } else {
                serviceSchoolCode = (String) school.getFieldValueByBeanPath(m_fieldSchoolCode);
            }

            // Check for 7 digit school codes. If so, split them into
            // district(3)/school(4).
            if ((homeSchoolCode != null) && (homeSchoolCode.length() >= 7)) {
                homeDistrictCode = homeSchoolCode.substring(0, 3);
                homeSchoolCode = homeSchoolCode.substring(3);
            }
            if ((serviceSchoolCode != null) && (serviceSchoolCode.length() >= 7)) {
                serviceDistrictCode = serviceSchoolCode.substring(0, 3);
                serviceSchoolCode = serviceSchoolCode.substring(3);
            }

            // Add the final codes to the retrieval map.
            calcValueMap.put(ALIAS_HOME_DISTRICT_CODE, homeDistrictCode);
            calcValueMap.put(ALIAS_SERVICE_DISTRICT_CODE, serviceDistrictCode);
            calcValueMap.put(ALIAS_HOME_SCHOOL_CODE, homeSchoolCode);
            calcValueMap.put(ALIAS_SERVICE_SCHOOL_CODE, serviceSchoolCode);
        }
        return calcValueMap;
    }

    /**
     * Return EL Code.
     */
    protected class RetrieveEnglishLearners extends RetrieveStudentProgram implements FieldValidator {
        private static final String CALC_PARAM_CODE = "Code";
        private static final String CALC_PARAM_SLIFE_STATUS = "SLIFE Status";
        private static final String PROGRAM_CODE = "ESL";
        private static final String PGM_ALIAS_CODE = "all-pgm-ELCode";
        private static final String PGM_ALIAS_SLIFE_STATUS = "all-pgm-SLIFEStatus";
        private static final String STD_ALIAS_CODE = "DOE ESL SERVICE";

        private String pgmELCodeJavaName;
        private String pgmSLIFEStatusJavaName;
        private String stdELCodeJavaName;
        private PlainDate m_latestDOB;

        /**
         * Instantiates a new retrieve english learners.
         *
         * @param studentCriteria Criteria
         */
        protected RetrieveEnglishLearners(Criteria studentCriteria) {
            super(PROGRAM_CODE, studentCriteria);
            pgmELCodeJavaName = translateAliasToJavaName(PGM_ALIAS_CODE, true);
            pgmSLIFEStatusJavaName = translateAliasToJavaName(PGM_ALIAS_SLIFE_STATUS, true);
            stdELCodeJavaName = translateAliasToJavaName(STD_ALIAS_CODE, false);
            Calendar latestDOB = Calendar.getInstance();
            int year = getCurrentContext().getSchoolYear() - 10;
            latestDOB.set(Calendar.YEAR, year);
            latestDOB.set(Calendar.MONTH, Calendar.AUGUST);
            latestDOB.set(Calendar.DAY_OF_MONTH, 1);
            m_latestDOB = new PlainDate(latestDOB.getTime());
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (CALC_PARAM_CODE.equals(field.getParameter())) {
                if (isAnyProgram(entity.getBean().getOid()) && !StringUtils.isEmpty(stdELCodeJavaName)) {
                    String code = (String) entity.getBean().getFieldValueByBeanPath(stdELCodeJavaName);
                    if (!StringUtils.isEmpty(code)) {
                        StateReportValidationError error = new StateReportValidationError(entity, field,
                                "If Student Program Participation exists for English Learners program, the student alias value ["
                                        +
                                        STD_ALIAS_CODE + "] must be empty",
                                "[" + STD_ALIAS_CODE + "]=" + STYLE_BOLD + code + STYLE_END);
                        errors.add(error);
                    }
                }
            } else if (CALC_PARAM_SLIFE_STATUS.equals(field.getParameter())
                    && "Y".equals(entity.getFieldValue(FIELD_SLIFE_STATUS_FLAG))) {
                SisStudent student = (SisStudent) entity.getBean();
                PlainDate dob = student.getPerson().getDob();
                if (dob == null || dob.after(m_latestDOB)) {
                    StateReportValidationError error = new StateReportValidationError(entity, field,
                            "If SLIFE status is Y the student must be at least 8 on August 1, "
                                    + (getCurrentContext().getSchoolYear() - 1) + ".",
                            " Date of birth " + dob + " is after " + m_latestDOB + ".");
                    errors.add(error);
                }
                if (!"1".equals(entity.getFieldValue(FIELD_ESL_SERVICES))
                        && !"2".equals(entity.getFieldValue(FIELD_ESL_SERVICES))) {
                    StateReportValidationError error = new StateReportValidationError(entity, field,
                            "If SLIFE status is Y the EL Code must be '1' or '2'.", "");
                    errors.add(error);
                }
            }
            return errors;
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.va.VAStudentRecordCollection.RetrieveStudentProgram#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            if (CALC_PARAM_CODE.equals(field.getParameter())) {
                StudentProgramParticipation program = matchingProgram(entity.getBean().getOid());
                if (program != null) {
                    String code = (String) program.getFieldValueByBeanPath(pgmELCodeJavaName);
                    value = lookupStateValue(StudentProgramParticipation.class, pgmELCodeJavaName, code);
                } else if (!isAnyProgram(entity.getBean().getOid()) && !StringUtils.isEmpty(stdELCodeJavaName)) {
                    String code = (String) entity.getBean().getFieldValueByBeanPath(stdELCodeJavaName);
                    value = lookupStateValue(SisStudent.class, stdELCodeJavaName, code);
                }
            } else if (CALC_PARAM_SLIFE_STATUS.equals(field.getParameter())) {
                StudentProgramParticipation program = matchingProgram(entity.getBean().getOid());
                if (program != null) {
                    value = data.getPropertyAsJavaType(program, pgmSLIFEStatusJavaName);
                }
            }
            return value;
        }

    }

    /**
     * Retrieve codes and dates from student enrollment.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveEnrollment implements FieldRetriever {

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

            String param = (String) field.getParameter();
            VAStudentRecordCollection srcData = (VAStudentRecordCollection) data;
            StudentRecordEntity srcEntity = (StudentRecordEntity) entity;
            MembershipAttendance attendance = srcEntity.getMembershipAttendance();

            SisStudent student = (SisStudent) entity.getBean();

            String enrollmentCode = null;
            PlainDate enrollmentDate = null;
            String enrollmentECode = null;
            PlainDate enrollmentEDate = null;
            String enrollmentWCode = null;
            String enrollmentWStatusCode = null;
            PlainDate enrollmentWDate = null;
            String codeRefTableOid = null;
            String reasonRefTableOid = null;
            boolean overrideEntry = false;

            StudentEnrollment enrollment = null;
            StudentEnrollment enrollmentE = attendance.getEntryEnrollment();
            StudentEnrollment enrollmentW = attendance.getExitEnrollment();

            // if students are "test takers," indicated by grade level equaling
            // "TT"
            // then enrollments should be empty.
            String gradeLevel = entity.getFieldValue(FIELD_GRADE_LEVEL);
            if ("TT".equals(gradeLevel)) {
                enrollmentE = null;
                enrollmentW = null;
            }

            // Get school start date.
            PlainDate schoolStartDate = null;
            if ((enrollmentE != null) && (enrollmentE.getSchool() != null)) {
                schoolStartDate = getFirstSchoolInSessionDate(enrollmentE.getSchool(), student.getCalendarCode());
            }
            if (schoolStartDate == null) {
                // this case should not be, added just "in case".
                schoolStartDate = srcData.m_districtStartDate;
            }

            // If student enrollment status N,
            // then need to avoid entry enrollment code and date.
            String status = entity.getFieldValue(FIELD_STATUS);
            if ("N".equals(status)) {
                enrollmentE = null;
            }

            if (enrollmentE != null) {
                enrollmentECode = enrollmentE.getEnrollmentCode();
                enrollmentEDate = enrollmentE.getEnrollmentDate();

                // Check if the enrollment data is before start of school.
                // Use start of school date and E119 instead.
                if ((schoolStartDate != null) && schoolStartDate.after(enrollmentEDate)) {
                    if ((enrollmentW == null) || enrollmentW.getEnrollmentDate().after(schoolStartDate)) {
                        enrollmentECode = CONTINUATION_CODE_E119;
                        enrollmentEDate = schoolStartDate;
                        overrideEntry = true;
                    }
                }
            }

            if (enrollmentW != null) {
                enrollmentWCode = enrollmentW.getEnrollmentCode();
                enrollmentWDate = enrollmentW.getEnrollmentDate();

                if ("N".equals(entity.getFieldValue(FIELD_STATUS))) {
                    // set enrollment withdrawal to school start date.
                    if (enrollmentWDate != null && schoolStartDate.after(enrollmentWDate)) {
                        enrollmentWDate = schoolStartDate;
                    }

                }

                enrollmentWStatusCode = enrollmentW.getStatusCode();
                if (enrollmentWStatusCode != null) {
                    enrollmentWStatusCode =
                            lookupStateValue(StudentEnrollment.class, StudentEnrollment.COL_STATUS_CODE,
                                    enrollmentWStatusCode);
                }
            }

            if (param.startsWith("ENTRY_")) {
                enrollment = enrollmentE;
                codeRefTableOid = m_enrollmentCodeRefTableOid;
                reasonRefTableOid = m_enrollmentReasonRefTableOid;
            } else if (param.startsWith("EXIT_")) {
                enrollment = enrollmentW;
                codeRefTableOid = m_withdrawalCodeRefTableOid;
                reasonRefTableOid = m_withdrawalReasonRefTableOid;
            }

            if (enrollment != null) {
                if (StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType())) {
                    String hbEntryCode = (String) enrollment.getFieldValueByBeanPath(m_fieldHBEntryCode);
                    if (VAStudentRecordCollection.HB_ENTRY_CODES.contains(hbEntryCode)) {
                        if (param.startsWith("ENTRY_")) {
                            if (!overrideEntry) {
                                enrollmentECode = hbEntryCode;
                            }
                        } else if (param.startsWith("EXIT_")) {
                            enrollmentWCode = (String) enrollment.getFieldValueByBeanPath(m_fieldHBWithdrawalCode);
                            enrollmentWDate = DateUtils.add(enrollmentWDate, -1);
                        }
                    }
                }

                String reason = lookupReferenceCodeByRefTbl(reasonRefTableOid, enrollment.getReasonCode(),
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                // Avoid exit dates for spans where the exit is after report
                // date.
                if (param.startsWith("EXIT_") && (enrollmentWDate != null) && (m_reportDate.before(enrollmentWDate)
                        || (m_reportDate.equals(enrollmentWDate) && "A".equals(enrollmentWStatusCode)))) {
                    enrollmentWCode = null;
                    enrollmentWDate = null;
                    reason = null;
                }
                // Avoid entry date and code if the row is a summer withdrawal
                // (exit before school start date).
                if (param.startsWith("ENTRY_") && (enrollmentWDate != null) && (schoolStartDate != null)
                        && schoolStartDate.after(enrollmentW.getEnrollmentDate())) {
                    enrollmentECode = null;
                    enrollmentEDate = null;
                }

                if (param.startsWith("ENTRY_")) {
                    enrollmentCode = enrollmentECode;
                    enrollmentDate = enrollmentEDate;
                } else if (param.startsWith("EXIT_")) {
                    enrollmentCode = enrollmentWCode;
                    enrollmentDate = enrollmentWDate;
                }
                if (param.endsWith("_CODE")) {
                    value = lookupReferenceCodeByRefTbl(codeRefTableOid, enrollmentCode,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                } else if (param.endsWith("_DATE")) {
                    value = enrollmentDate;
                } else if (param.endsWith("_REASON")) {
                    value = reason;
                }
            }

            return value;
        }
    }

    /**
     * return foster care flag.
     */
    protected class RetrieveFosterCare extends RetrieveStudentProgram {
        private static final String PROGRAM_CODE = "FosterCare";

        /**
         * Instantiates a new retrieve foster care.
         *
         * @param studentCriteria Criteria
         */
        protected RetrieveFosterCare(Criteria studentCriteria) {
            super(PROGRAM_CODE, studentCriteria);
            setMatchOnReportDate(false);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.va.VAStudentRecordCollection.RetrieveStudentProgram#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            StudentProgramParticipation program = matchingProgram(entity.getBean().getOid());
            if (program != null) {
                value = "Y";
            }
            return value;
        }

    }

    /**
     * Determine if the student entered the school via an Alternative Ed program (E099 or R099).
     * If
     * so, set the GED flag
     * to "2".
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveGED implements FieldRetriever {
        List<String> m_gedCodes = Arrays.asList("E099", "R099");

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
            String value = (String) data.getProperty(entity.getBean(), field.getBeanPath());
            StudentRecordEntity srcEntity = (StudentRecordEntity) entity;
            MembershipAttendance attendance = srcEntity.getMembershipAttendance();

            // Find the entry code, if any.
            StudentEnrollment enrollment = attendance.getEntryEnrollment();
            String entryCode = null;
            if (enrollment != null) {
                entryCode = lookupReferenceCodeByRefTbl(m_enrollmentCodeRefTableOid, enrollment.getEnrollmentCode(),
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }

            // If the entry code is a GED code, return the GED indicator.
            if (m_gedCodes.contains(entryCode)) {
                value = m_isaepCode;
            }
            return value;
        }
    }

    /**
     * Retrieve the gifted referral date. If the date is not empty and is within the current
     * year,
     * return true for
     * gifted referral.
     */
    protected class RetrieveGifted implements FieldRetriever {

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
            PlainDate referralDate = (PlainDate) data.getPropertyAsJavaType(entity.getBean(), field.getBeanPath());
            Boolean value = Boolean.FALSE;

            // Determine if the field should be blanked.
            if ((referralDate != null) && !referralDate.before(getOrganization().getCurrentContext().getStartDate())
                    && !referralDate.after(getOrganization().getCurrentContext().getEndDate())) {
                value = Boolean.TRUE;
            }
            return value;
        }
    }

    /**
     * Used for both Graduate Code and Graduate Plan.
     *
     * Determine if the student is a graduate and only return the specified value is the student
     * is
     * a graduate.
     * Conditions indicating a graduate. 1. If the student has a W730 or W731 withdrawal code.
     * 2. If
     * this is the EOY
     * report, the student is grade 12 and the student is not RETAINED.
     *
     */
    protected class RetrieveGrad implements FieldRetriever {
        List<String> m_gradCodes = Arrays.asList("W730", "W731"); // "W214" ?
        List<String> m_completerCodes19 = Arrays.asList("1", "2", "3", "4", "5", "6", "7", "8", "9");
        List<String> m_completerCodes1013 = Arrays.asList("10", "11", "12", "13");

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
            String value = (String) data.getProperty(entity.getBean(), field.getBeanPath());
            String gradeLevel = entity.getFieldValue(FIELD_GRADE_LEVEL);
            String retained = entity.getFieldValue(FIELD_RETENTION);
            String exitCode = entity.getFieldValue(FIELD_EXIT_CODE);

            // Determine if the field should be blanked.
            boolean isGrad = false;
            if (m_gradCodes.contains(exitCode)) {
                isGrad = true;
            }
            if (COLLECTION_EOY.equals(m_collection) && "12".equals(gradeLevel) && !"Y".equals(retained)
                    && StringUtils.isEmpty(exitCode)) {
                isGrad = true;
            }
            if (!isGrad) {
                value = null;
            }

            /*
             * Add validations for GRADUATE PLAN Required if Graduate/Other Completer Code is 1,
             * 2,
             * 3, 4, 5, 6, 7, 8, or
             * 9 Must be blank if Graduate/Other Completer Code is 10, 11, 12, or 13 Warning if
             * grade is not 11 or 12
             * Must be a valid state assigned code
             */
            if (!StringUtils.isEmpty(field.getFieldId()) && field.getFieldId().equals("Graduate Plan")) {
                SisStudent student = (SisStudent) entity.getBean();
                String completerCode = (String) student.getFieldValueByAlias("DOE GRAD CODE");

                if (StringUtils.isEmpty(value) && !StringUtils.isEmpty(completerCode)
                        && m_completerCodes19.contains(completerCode)) {
                    entity.addRetrievalError(field.getFieldId(),
                            new StateReportValidationError(entity, field, "Missing required value",
                                    "Graduate Plan is required if Graduate/Other Completer Code is 1-9"));
                }

                if (!StringUtils.isEmpty(value) && !StringUtils.isEmpty(completerCode)
                        && m_completerCodes1013.contains(completerCode)) {
                    entity.addRetrievalError(field.getFieldId(), new StateReportValidationError(entity, field,
                            "Must be blank",
                            "Graduate Plan must be blank if Graduate/Other Completer Code is 10-13"));
                }

                if (!StringUtils.isEmpty(value) && !("11".equals(gradeLevel) || "12".equals(gradeLevel))) {
                    entity.addRetrievalError(field.getFieldId(), new StateReportValidationError(entity, field,
                            "Invalid value", "Warning: Graduate Plan present when Grade Level other than 11-12"));
                }
            }

            return value;
        }
    }

    /**
     * Return the grade level which will contain either the original grade level provided or the
     * grade level override.
     */
    protected class RetrieveGradeLevelOverride implements FieldRetriever {

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
            SisStudent student = (SisStudent) entity.getBean();
            String gradeLevel = (String) data.getPropertyAsJavaType(student, field.getBeanPath());

            String gradeLevelOverride = null;
            String gradeLevelOverrideBeanPath = data.translateAliasToJavaName(ALIAS_GRADE_LEVEL_OVERRIDE, false);

            gradeLevelOverride = (String) data.getPropertyAsJavaType(student, gradeLevelOverrideBeanPath);

            if (gradeLevelOverride != null) {
                gradeLevel = gradeLevelOverride;
            }
            return gradeLevel;
        }
    }

    /**
     * return fields from Homeless program.
     */
    protected class RetrieveHomeless extends RetrieveStudentProgram implements FieldValidator {
        private static final String CALC_PARAM_NIGHT_RESIDENCE = "Night Residence";
        private static final String CALC_PARAM_UNACCOMPANIED = "Unaccompanied";
        private static final String PROGRAM_CODE = "Homeless";
        private static final String PGM_ALIAS_NIGHT_RESIDENCE = "all-pgm-InitialPrimaryNightimeResidence";
        private static final String PGM_ALIAS_UNACCOMPANIED = "all-pgm-UnaccompaniedHomelessYouth";
        private static final String STD_ALIAS_NIGHT_RESIDENCE = "DOE NIGHT RESIDENT";
        private static final String STD_ALIAS_UNACCOMPANIED = "DOE UY";

        private String pgmNightResJavaName;
        private String pgmUnaccompanied;
        private String stdNightResJavaName;
        private String stdUnaccompanied;

        /**
         * Instantiates a new retrieve homeless.
         *
         * @param studentCriteria Criteria
         */
        protected RetrieveHomeless(Criteria studentCriteria) {
            super(PROGRAM_CODE, studentCriteria);
            setMatchOnReportDate(false);

            pgmNightResJavaName = translateAliasToJavaName(PGM_ALIAS_NIGHT_RESIDENCE, true);
            pgmUnaccompanied = translateAliasToJavaName(PGM_ALIAS_UNACCOMPANIED, true);
            stdNightResJavaName = translateAliasToJavaName(STD_ALIAS_NIGHT_RESIDENCE, false);
            stdUnaccompanied = translateAliasToJavaName(STD_ALIAS_UNACCOMPANIED, false);
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value2 String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value2) {
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (CALC_PARAM_NIGHT_RESIDENCE.equals(field.getParameter())) {
                if (isAnyProgram(entity.getBean().getOid()) && !StringUtils.isEmpty(stdNightResJavaName)) {
                    String code = (String) entity.getBean().getFieldValueByBeanPath(stdNightResJavaName);
                    if (!StringUtils.isEmpty(code)) {
                        StateReportValidationError error = new StateReportValidationError(entity, field,
                                "If Student Program Participation exists for Homeless program, the student alias value ["
                                        +
                                        STD_ALIAS_NIGHT_RESIDENCE + "] must be empty",
                                "[" + STD_ALIAS_NIGHT_RESIDENCE + "]=" + STYLE_BOLD + code + STYLE_END);
                        errors.add(error);
                    }
                }
            } else if (CALC_PARAM_UNACCOMPANIED.equals(field.getParameter())) {
                if (isAnyProgram(entity.getBean().getOid()) && !StringUtils.isEmpty(stdUnaccompanied)) {
                    String code = (String) entity.getBean().getFieldValueByBeanPath(stdUnaccompanied);
                    if (!StringUtils.isEmpty(code) && !BooleanAsStringConverter.FALSE.equals(code)) {
                        StateReportValidationError error = new StateReportValidationError(entity, field,
                                "If Student Program Participation exists for Homeless program, the student alias value ["
                                        +
                                        STD_ALIAS_UNACCOMPANIED + "] must be empty",
                                "[" + STD_ALIAS_UNACCOMPANIED + "]=" + STYLE_BOLD + code + STYLE_END);
                        errors.add(error);
                    }
                }
            }
            return errors;
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.va.VAStudentRecordCollection.RetrieveStudentProgram#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            if (CALC_PARAM_NIGHT_RESIDENCE.equals(field.getParameter())) {
                StudentProgramParticipation program = matchingProgram(entity.getBean().getOid());
                if (program != null) {
                    String code = (String) program.getFieldValueByBeanPath(pgmNightResJavaName);
                    value = lookupStateValue(StudentProgramParticipation.class, pgmNightResJavaName, code);
                } else if (!isAnyProgram(entity.getBean().getOid()) && !StringUtils.isEmpty(stdNightResJavaName)) {
                    String code = (String) entity.getBean().getFieldValueByBeanPath(stdNightResJavaName);
                    value = lookupStateValue(SisStudent.class, stdNightResJavaName, code);
                }
            } else if (CALC_PARAM_UNACCOMPANIED.equals(field.getParameter())) {
                StudentProgramParticipation program = matchingProgram(entity.getBean().getOid());
                if (program != null) {
                    value = program.getFieldValueByBeanPath(pgmUnaccompanied);
                } else if (!isAnyProgram(entity.getBean().getOid()) && !StringUtils.isEmpty(stdUnaccompanied)) {
                    value = entity.getBean().getFieldValueByBeanPath(stdUnaccompanied);
                }
            }
            return value;
        }

    }

    /**
     * The Class RetrieveIBFlag.
     */
    protected class RetrieveIBFlag implements FieldRetriever {
        private static final String ALIAS_IB_FLAG = "DOE IB PROGRAM";
        private static final String IB_GRAD_COMPLETER_CODE = "6";

        private String m_fieldIBFlag;

        /**
         *
         */
        public RetrieveIBFlag() {
            super();
            m_fieldIBFlag = translateAliasToJavaName(ALIAS_IB_FLAG, true);
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
            Boolean value =
                    IB_GRAD_COMPLETER_CODE.equals(entity.getFieldValue(FIELD_GRADUATE_COMP)) ? Boolean.TRUE : null;
            Object studentValue = data.getPropertyAsJavaType(entity.getBean(), m_fieldIBFlag);
            if (studentValue != null && studentValue instanceof Boolean
                    && ((Boolean) studentValue).booleanValue()) {
                value = Boolean.TRUE;
            }
            return value;
        }

    }

    /**
     * Determines whether the student in an immmigrant or not. A student is an immigrant if DOE
     * IMMIGRANT = Y and the
     * Report Date is >= DOE US ENTRY DATE and <= DOE US END DATE the report date.
     */
    protected class RetrieveImmigrant implements FieldRetriever {

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
            VAStudentRecordCollection vaData = (VAStudentRecordCollection) data;

            Boolean immigrantStatus = null;
            PlainDate usEndDate = null;
            PlainDate usEntryDate = null;

            SisStudent student = (SisStudent) entity.getBean();
            try {
                usEntryDate = (PlainDate) data.getPropertyAsJavaType(student, vaData.m_fieldUsEntryDate);
                usEndDate = (PlainDate) data.getPropertyAsJavaType(student, vaData.m_fieldUsEndDate);
                immigrantStatus = (Boolean) data.getPropertyAsJavaType(student, vaData.m_fieldImmigrantStatus);
            } catch (X2BaseException e) {
                // do nothing. not a date.
            }

            if ((immigrantStatus == Boolean.TRUE) && (usEndDate != null) && !usEndDate.before(m_reportDate)
                    && (usEntryDate != null) && !usEntryDate.after(m_reportDate)) {
                return Boolean.TRUE;
            }

            return Boolean.FALSE;
        }

    }

    /**
     * Retrieve LIEP
     */
    protected class RetrieveLiep extends RetrieveStudentProgram implements FieldValidator {
        private static final String CALC_ID = "SRC-LIEP";
        private static final String PROGRAM_CODE = "ESL";

        /**
         * Instantiates a new retrieve foster care.
         *
         * @param studentCriteria Criteria
         */
        protected RetrieveLiep(Criteria studentCriteria) {
            super(PROGRAM_CODE, studentCriteria);
            setMatchOnReportDate(false);
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            VAStudentRecordCollection src = (VAStudentRecordCollection) data;
            if (src.m_collection != null
                    && (COLLECTION_FALL.equals(src.m_collection) || COLLECTION_EOY.equals(src.m_collection)
                            || COLLECTION_SPRING.equals(src.m_collection))) {
                String elServidceCode = entity.getFieldValue(FIELD_ESL_SERVICES);
                if (!StringUtils.isEmpty(elServidceCode) && "1".equals(elServidceCode)
                        && StringUtils.isEmpty(value)) {
                    StateReportValidationError error = new StateReportValidationError(entity, field,
                            "Valid code is required if EL service code = 1 at any point during this year.",
                            "[" + ALIAS_PGM_LIEP + "]=" + STYLE_BOLD + "is empty" + STYLE_END);
                    errors.add(error);
                }
            }
            return errors;
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.va.VAStudentRecordCollection.RetrieveStudentProgram#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            VAStudentRecordCollection src = (VAStudentRecordCollection) data;
            if (src.m_collection != null
                    && (COLLECTION_FALL.equals(src.m_collection) || COLLECTION_EOY.equals(src.m_collection)
                            || COLLECTION_SPRING.equals(src.m_collection))) {
                StudentProgramParticipation program = matchingProgram(entity.getBean().getOid());
                if (program != null) {
                    String liepCode = (String) program.getFieldValueByBeanPath(m_fieldPgmLIEP);
                    if (!StringUtils.isEmpty(liepCode)) {
                        value = data.lookupStateValue(StudentProgramParticipation.class, m_fieldPgmLIEP, liepCode);
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retrieve the VA specific race code.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveRace implements FieldRetriever {

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

            int totalRace = 0;
            Collection<Race> races = m_raceCodeMap.get(student.getPersonOid());
            if (races != null) {
                for (Race race : races) {
                    ReferenceCode code = m_raceCodes.get(race.getRaceCode());
                    if (code != null) {
                        int codeNum = 0;
                        try {
                            codeNum = Integer.parseInt(code.getStateCode());
                        } catch (NumberFormatException nfe) {
                            // not parsable, ignore.
                        }
                        totalRace += codeNum;
                    }
                }
            }
            if ((totalRace > 0) && (totalRace < 32)) {
                totalRace = m_raceValueTranslator[totalRace];
            } else {
                totalRace = 0;
            }

            return Integer.valueOf(totalRace);
        }
    }

    /**
     * Retrieve sped weekly time. This is required only when following conditions are met:
     * <ul>
     * <li>Student has a disability code that is not 504 (state code of 15)
     * <li>Student is in the grade levels: KA, KP, KG, T1, 01, 02, or 03
     * </ul>
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSpedWeeklyTime implements FieldRetriever {

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
            BigDecimal spedWeeklyTime = null;

            String primaryDisability = entity.getFieldValue(FIELD_PRIMARY_DISABILITY);
            String gradeLevel = entity.getFieldValue(FIELD_GRADE_LEVEL);
            if (!StringUtils.isEmpty(primaryDisability) && !"15".equals(primaryDisability)
                    && ("KA".equals(gradeLevel) || "KP".equals(gradeLevel) || "KG".equals(gradeLevel)
                            || "T1".equals(gradeLevel) || "01".equals(gradeLevel) || "02".equals(gradeLevel)
                            || "03".equals(gradeLevel))) {
                Object value = data.getPropertyAsJavaType(entity.getBean(), field.getBeanPath());
                if (value != null) {
                    spedWeeklyTime = (BigDecimal) value;
                }
            }

            return spedWeeklyTime;
        }
    }

    /**
     * Retrieve the student status from the membership segment. Use enrollment status,
     * enrollment
     * dates and membership
     * days to determine the active status. If the span is not current on enrollment date, use
     * membership days to
     * determine if it is I or N. If the span is the current span, use the enrollment status of
     * the
     * entry enrollment to
     * get the current status (A or V).
     * <p>
     * Status: <br>
     * <ul>
     * <li>A - Active</li>
     * <li>I - Not active but has had membership days in the current year.</li>
     * <li>N - Not active and has not had membership days in the current year.</li>
     * <li>V - MOP student</li>
     * </ul>
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStatus implements FieldRetriever {

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
            StudentRecordEntity srcEntity = (StudentRecordEntity) entity;
            MembershipAttendance attendance = srcEntity.getMembershipAttendance();
            SisStudent student = (SisStudent) srcEntity.getBean();
            String status = "N";
            // use status "V" if "DOE MOP FLAG" is set for student and student enrollment "DOE
            // VIRTUAL PROGRAM" has state code 1
            boolean mopStudent =
                    BooleanAsStringConverter.TRUE.equals(student.getFieldValueByBeanPath(m_fieldMopFlag));
            boolean virtualProgramStudent = false;
            if (mopStudent) {
                if (attendance.getEntryEnrollment() != null) {
                    String virtualProgramValue =
                            (String) attendance.getEntryEnrollment().getFieldValueByAlias(ALIAS_VIRTUAL_PROGRAM);
                    if (!StringUtils.isEmpty(virtualProgramValue)) {
                        String virtualProgramStateCode =
                                lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_fieldVirtualProgram,
                                        virtualProgramValue,
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        if ("1".equals(virtualProgramStateCode)) {
                            virtualProgramStudent = true;
                            status = "V";
                            if (attendance.m_exitEnrollment != null) {
                                String stateWithdrawalCode = lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                                        StudentEnrollment.COL_ENROLLMENT_CODE,
                                        attendance.m_exitEnrollment.getEnrollmentCode(),
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                                if (!StringUtils.isEmpty(stateWithdrawalCode)
                                        && !stateWithdrawalCode.startsWith("W7")) {
                                    status = "I";
                                }
                            }
                        }
                    }
                }
            }
            if (!(mopStudent && virtualProgramStudent)) {
                String entryStatus = null;
                if (attendance.getMembership() > 0) {
                    status = "I";
                }

                // Determine if this enrollment span is current as of report date.
                PlainDate beginDate = null;
                PlainDate endDate = null;
                String endStatus = null;
                if (attendance.getEntryEnrollment() != null) {
                    beginDate = attendance.getEntryEnrollment().getEnrollmentDate();
                    entryStatus =
                            lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                                    StudentEnrollment.COL_STATUS_CODE,
                                    attendance.getEntryEnrollment().getStatusCode(),
                                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }

                // Find the end date, from the enrollment withdrawal or from report
                // date.
                if (attendance.getExitEnrollment() != null) {
                    endDate = attendance.getExitEnrollment().getEnrollmentDate();
                    if (StudentEnrollment.STATUS_CHANGE
                            .equals(attendance.getExitEnrollment().getEnrollmentType())) {
                        String hbEntryCode = (String) attendance.getExitEnrollment()
                                .getFieldValueByBeanPath(m_fieldHBEntryCode);
                        if (VAStudentRecordCollection.HB_ENTRY_CODES.contains(hbEntryCode)) {
                            endDate = DateUtils.add(endDate, -1);
                        }
                    }
                    endStatus =
                            lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                                    StudentEnrollment.COL_STATUS_CODE,
                                    attendance.getExitEnrollment().getStatusCode(),
                                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }

                // If the span is the current span, or the end date is the same as
                // the report date
                // and the end status is ACTIVE, use the entry status from the
                // enrollment record.
                if (((beginDate != null) && !beginDate.after(m_reportDate)) && ((endDate == null)
                        || endDate.after(m_reportDate)
                        || (endDate.equals(m_reportDate) && PARAM_ACTIVE.equals(endStatus)))) {
                    status = entryStatus;
                }

                String gradeLevel = entity.getFieldValue(FIELD_GRADE_LEVEL);
                if ("TT".equals(gradeLevel)) {
                    status = "N";
                }
            }

            return status;
        }
    }

    /**
     * Retrieve the tuition paid from the school.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTuitionPaid implements FieldRetriever {

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
            StudentRecordEntity srcEntity = (StudentRecordEntity) entity;

            String tuitionPaid = null;
            String value = null;
            String param = ALIAS_SERVICE_SCHOOL_CODE;

            tuitionPaid = (String) student.getFieldValueByAlias("DOE STUDENT TUITION");

            if (StringUtils.isEmpty(tuitionPaid)) {
                if (PARAM_CURRENT_SCHOOL_STATUS.equals(entity.getFieldValue(FIELD_STATUS))) {
                    value = (String) srcEntity.getOverrideCodeValue(param);
                }

                if (StringUtils.isEmpty(value)) {
                    MembershipAttendance attendance = srcEntity.getMembershipAttendance();
                    Map<String, Object> calcValueMap = lookupOverrides(student, attendance.getEntryEnrollment(),
                            attendance.getSchool().getOid());
                    value = (String) calcValueMap.get(param);
                } else {
                    if (!isServingAndHomeSame(entity)) {
                        tuitionPaid = "04";
                    }
                }

                if (m_schoolOverride.contains(value)) {
                    if (!isServingAndHomeSame(entity)) {
                        tuitionPaid = "04";
                    }
                }
            }

            if (StringUtils.isEmpty(tuitionPaid)) {
                MembershipAttendance attendance = srcEntity.getMembershipAttendance();

                StudentEnrollment enrollment = attendance.getEntryEnrollment();

                if (enrollment.getSchool() != null) {
                    tuitionPaid = (String) enrollment.getSchool().getFieldValueByBeanPath(m_fieldSchoolTuition);
                }
            }
            return tuitionPaid;
        }

        /**
         * Returns true if division ID and serving division ID are not null and are the same,
         * false
         * otherwise.
         *
         * @param entity StateReportEntity
         * @return boolean
         */
        private boolean isServingAndHomeSame(StateReportEntity entity) {
            boolean same = false;
            String districtHome = entity.getFieldValue(FIELD_DIVISION_ID);
            String districtServe = entity.getFieldValue(FIELD_DIVISION_ID_SERVE);
            if (ObjectUtils.matchStrict(districtHome, districtServe)) {
                same = true;
            }

            return same;
        }
    }

    /**
     * Retrieve the tuition reimbursement and disability for a student.
     *
     * Reimbursement should only be printed for certain schools.
     *
     * The disability should only be shown if the reimbursement for that reporting period is not
     * empty.
     *
     * @author X2 Development Corporation
     */

    protected class RetrieveTuitionReimbursment implements FieldRetriever {

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
            VAStudentRecordCollection srcData = (VAStudentRecordCollection) data;
            StudentRecordEntity srcEntity = (StudentRecordEntity) entity;
            MembershipAttendance attendance = srcEntity.getMembershipAttendance();
            StudentEnrollment enrollment = attendance.getEntryEnrollment();

            String collection = srcData.m_collection;
            Object value = enrollment.getFieldValueByAlias((String) field.getParameter());
            String fieldName = field.getFieldId();
            if (FIELD_SPED_TUIT_DISABILITY.equals(fieldName)) {
                String tuitionValue = null;
                if (COLLECTION_SPED.equals(collection)) {
                    tuitionValue = (String) enrollment.getFieldValueByAlias(ALIAS_SPED_TUITION_SUM);
                } else if (COLLECTION_SPRING.equals(collection)) {
                    tuitionValue = (String) enrollment.getFieldValueByAlias(ALIAS_SPED_TUITION_S1);
                } else if (COLLECTION_EOY.equals(collection)) {
                    tuitionValue = (String) enrollment.getFieldValueByAlias(ALIAS_SPED_TUITION_S2);
                }
                if (StringUtils.isEmpty(tuitionValue)) {
                    value = null;
                } else {
                    value = srcData.lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                            srcData.m_fieldTuitionDisability, (String) value,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            } else
            // FIELD_SPED_TUITION_S*
            {
                String tuitionPaid = (String) enrollment.getFieldValueByAlias(ALIAS_SPED_TUIT_DISABILITY);
                if (StringUtils.isEmpty(tuitionPaid)) {
                    value = null;
                }
            }

            return value;
        }
    }

    /**
     * Fetch remote learning PGM fields.
     */
    protected class RetrievePGMRemoteLearning extends RetrieveStudentProgram {
        private static final String PROGRAM_CODE = "Remote";

        private static final String PARENT_DEC = "PARENT-DEC";
        private static final String LOC_TIME = "LOC-TIME";
        private static final String REM_TIME = "REM-TIME";
        private static final String INT_ACC = "INT-ACC";
        private static final String DEV_ACC = "DEV-ACC";

        private final List<String> VIRTUAL_CODES = Arrays.asList("1", "8");
        private final DateFormat DATE_FORMATTER = new SimpleDateFormat("MM/dd/yyyy");

        /**
         * Instantiates a new PGM remote learning.
         *
         * @param studentCriteria Criteria
         */
        protected RetrievePGMRemoteLearning(Criteria studentCriteria) {
            super(PROGRAM_CODE, studentCriteria);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.va.VAStudentRecordCollection.RetrieveStudentProgram#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            if (VIRTUAL_CODES.contains(entity.getFieldValue(FIELD_FT_VIRTUAL_PROGRAM))) {
                PlainDate entryDate = null;
                try {
                    entryDate = new PlainDate(DATE_FORMATTER.parse(entity.getFieldValue(FIELD_ENTRY_DATE)));
                } catch (ParseException e) {
                    // ignore parse error
                }
                final PlainDate testDate = entryDate; // make final
                StudentProgramParticipation program = null;
                if (entryDate != null) {
                    program = getPrograms(entity.getBean().getOid()).stream()
                            .filter(pgm -> !testDate.before(pgm.getStartDate())
                                    && (pgm.getEndDate() == null || !testDate.after(pgm.getEndDate())))
                            .sorted(new Comparator<StudentProgramParticipation>() {

                                @Override
                                public int compare(StudentProgramParticipation o1, StudentProgramParticipation o2) {
                                    // Decreasing order by start date
                                    return o2.getStartDate().compareTo(o1.getStartDate());
                                }

                            })
                            .findFirst().orElse(null);
                }
                if (program != null) {
                    String param = (String) field.getParameter();
                    if (PARENT_DEC.equals(param)) {
                        String decision =
                                (String) program.getFieldValueByBeanPath(m_fieldPgmParentalRemoteLearningDecision);
                        value = StringUtils.isEqual(BooleanAsStringConverter.TRUE, decision) ? "Y" : "N";
                    } else if (LOC_TIME.equals(param)) {
                        value = program.getFieldValueByBeanPath(m_fieldPgmRegionalLocalCenterPercentofTime);
                    } else if (REM_TIME.equals(param)) {
                        value = program.getFieldValueByBeanPath(m_fieldPgmRemoteInstructionPercentofTime);
                    } else if (INT_ACC.equals(param)) {
                        String access =
                                (String) program.getFieldValueByBeanPath(m_fieldPgmInternetAccessforRemoteLearning);
                        value = lookupStateValue(StudentProgramParticipation.class,
                                m_fieldPgmInternetAccessforRemoteLearning,
                                access);
                    } else if (DEV_ACC.equals(param)) {
                        String access =
                                (String) program.getFieldValueByBeanPath(m_fieldPgmDeviceAccessforRemoteLearning);
                        value = lookupStateValue(StudentProgramParticipation.class,
                                m_fieldPgmDeviceAccessforRemoteLearning,
                                access);
                    }
                }
            }

            return value;
        }
    }

    /**
     * Calculates the population code. 4 for a non-traditional gender role, and the second half
     * is
     * input by the user
     */

    protected class RetrievePopulation implements FieldRetriever {

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
            SisStudent student = (SisStudent) entity.getBean();
            String value = null;

            boolean reportableGrade = false;

            try {
                if (student.getGradeLevel() != null) {
                    reportableGrade = Integer.valueOf(student.getGradeLevel()).intValue() > 6;
                }
            } catch (NumberFormatException e) {
                // Expected for pre-grade 1
            }

            if (reportableGrade) {
                boolean nonTraditional = false;
                for (StudentSchedule schedule : student.getStudentSchedules()) {
                    // If student already found a course considered as non-traditional, the
                    // required
                    // objects are null,
                    // or the schedule is not for the current school year, skip the record
                    if (nonTraditional || schedule.getSection() == null
                            || schedule.getSection().getSchoolCourse() == null
                            || schedule.getSchedule() == null
                            || !schedule.getSchedule().getDistrictContextOid()
                                    .equals(getOrganization().getCurrentContextOid())) {
                        continue;
                    }
                    Course course = schedule.getSection().getSchoolCourse().getCourse();
                    String traditionalGender = EMPTY_STRING;

                    if ((course != null) && (BooleanAsStringConverter.TRUE
                            .equals(course.getFieldValueByAlias(ALIAS_CAREER_AND_TECH_ED))
                            || BooleanAsStringConverter.TRUE
                                    .equals(course.getFieldValueByAlias(ALIAS_SEDF_REPORT_FLAG)))) {
                        traditionalGender = (String) course.getFieldValueByAlias(ALIAS_TRADITIONAL_GENDER);
                        if (!StringUtils.isEmpty(traditionalGender)) {
                            if (student.getPerson() == null || student.getPerson().getGenderCode() == null) {
                                continue;
                            }
                            nonTraditional = student.getPerson().getGenderCode().equals(traditionalGender);
                        }
                    }
                }

                if (nonTraditional) {
                    value = "4";
                }
                // If the student is a taking a course which is considered non-traditional
                // for their gender, append a 4 on the front of the state code.
                // If the state code already contains the 4, the code will end up starting with
                // 44.
                // In these cases, we remove the extra 4.

                if (m_specialPopulationJavaname != null) {
                    String ctePop = (String) student.getFieldValueByBeanPath(m_specialPopulationJavaname);
                    ctePop = lookupReferenceCodeByBeanPath(SisStudent.class, m_specialPopulationJavaname, ctePop,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    if (ctePop == null) {
                        ctePop = EMPTY_STRING;
                    }
                    value = (value != null) ? value + ctePop : ctePop;
                    if (value != null && value.startsWith("44")) {
                        value = value.replaceFirst("44", "4");
                    }

                }
            }
            return value;
        }
    }

    /**
     * Calculates the career cluster value. We now look to the course for this. If a student has
     * multiple take the last
     * one, or the one with the most entries.
     */

    protected class RetrieveNoSpring implements FieldRetriever {

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
            String value = null;
            if (!COLLECTION_SPRING.equals(m_collection)) {
                String param = (String) field.getParameter();
                SisStudent student = (SisStudent) entity.getBean();
                value = (String) student.getFieldValueByAlias(param);
            }
            return value;
        }
    }

    /**
     *
     *
     * Returns the SPED Time percent. If this value is blank, zero, or null, an empty value is
     * returned.
     */

    protected class RetrieveSpedPercent implements FieldRetriever {

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
            String value = EMPTY_STRING;
            String fieldValue = (String) data.getProperty(entity.getBean(), field.getBeanPath());
            if (!StringUtils.isEmpty(fieldValue)) {
                if (Double.parseDouble(fieldValue) != 0) {
                    value = fieldValue;
                }
            }

            return value;
        }
    }

    /**
     * Used to facilitate retrievers based on student program participation.
     */

    protected class RetrieveStudentProgram implements FieldRetriever {
        private boolean m_matchOnReportDate = true;
        private Map<String, List<StudentProgramParticipation>> m_programsMap;

        /**
         * Instantiates a new retrieve student program.
         *
         * @param stateCode String
         * @param studentCriteria Criteria
         */
        protected RetrieveStudentProgram(String stateCode, Criteria studentCriteria) {
            Set<String> programCodes = new HashSet();
            programCodes.add("---dummy---");
            DataDictionaryField field = getDataDictionaryField(StudentProgramParticipation.class,
                    StudentProgramParticipation.COL_PROGRAM_CODE);
            if (field != null && field.getReferenceTableOid() != null) {
                Map<String, ReferenceCode> programCodesMap = getReferenceCodes(field.getReferenceTableOid());
                for (ReferenceCode code : programCodesMap.values()) {
                    if (stateCode.equals(code.getStateCode())) {
                        programCodes.add(code.getCode());
                    }
                }
            }

            SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentsSubQuery);
            criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, programCodes);
            criteria.addNotNull(StudentProgramParticipation.COL_START_DATE);

            QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);

            m_programsMap =
                    getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID,
                            100);
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
            return null;
        }

        /**
         * Gets the programs.
         *
         * @param stdOid String
         * @return List
         */
        protected List<StudentProgramParticipation> getPrograms(String stdOid) {
            List<StudentProgramParticipation> programs = m_programsMap.get(stdOid);
            return programs == null ? Collections.EMPTY_LIST : programs;
        }

        /**
         * Checks if is any program.
         *
         * @param stdOid String
         * @return true, if is any program
         */
        protected boolean isAnyProgram(String stdOid) {
            List<StudentProgramParticipation> programs = m_programsMap.get(stdOid);
            return programs == null || programs.isEmpty() ? false : true;
        }

        /**
         * Matching program.
         *
         * @param stdOid String
         * @return StudentProgramParticipation
         */
        protected StudentProgramParticipation matchingProgram(String stdOid) {
            StudentProgramParticipation program = null;
            List<StudentProgramParticipation> programs = m_programsMap.get(stdOid);
            if (programs != null) {
                PlainDate latestStartDate = null;
                for (StudentProgramParticipation pgm : programs) {
                    if (m_matchOnReportDate) {
                        if (!pgm.getStartDate().after(m_reportDate)
                                && (pgm.getEndDate() == null || !pgm.getEndDate().before(m_reportDate))) {
                            if (latestStartDate == null || latestStartDate.before(pgm.getStartDate())) {
                                program = pgm;
                                latestStartDate = pgm.getStartDate();
                            }
                        }
                    } else {
                        // anytime during year - find record with latest matching start date
                        if (!pgm.getStartDate().after(getCurrentContext().getEndDate()) && (pgm.getEndDate() == null
                                || !pgm.getEndDate().before(getCurrentContext().getStartDate()))) {
                            if (latestStartDate == null || latestStartDate.before(pgm.getStartDate())) {
                                program = pgm;
                                latestStartDate = pgm.getStartDate();
                            }
                        }
                    }
                }
            }
            return program;
        }

        /**
         * Sets the match on report date.
         *
         * @param value void
         */
        protected void setMatchOnReportDate(boolean value) {
            m_matchOnReportDate = value;
        }
    }
    /**
     * Calculates the career cluster value. We now look to the course for this. If a student has
     * multiple take the last
     * one, or the one with the most entries.
     */
    protected class RetrieveVirtualProgram implements FieldRetriever {

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
            StudentRecordEntity srcEntity = (StudentRecordEntity) entity;
            MembershipAttendance attendance = srcEntity.getMembershipAttendance();
            StudentEnrollment enrollment = attendance.getEntryEnrollment();
            String virtualProgramStatus = (String) enrollment.getFieldValueByAlias(ALIAS_VIRTUAL_PROGRAM);
            if (!StringUtils.isEmpty(virtualProgramStatus)) {
                value = lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_fieldVirtualProgram,
                        virtualProgramStatus,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }
            return value;
        }
    }

    /**
     * Calculates the career cluster value. We now look to the course for this. If a student has
     * multiple take the last
     * one, or the one with the most entries.
     */
    protected class RetrieveCareerCluster implements FieldRetriever {

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
            String value = EMPTY_STRING;
            if (COLLECTION_EOY.equals(m_collection) || COLLECTION_SUMMER.equals(m_collection)) {
                SisStudent student = (SisStudent) entity.getBean();

                boolean reportableGrade = false;

                try

                {
                    if (student.getGradeLevel() != null)

                    {
                        reportableGrade = Integer.valueOf(student.getGradeLevel()).intValue() > 6;
                    }
                } catch (NumberFormatException e) {
                    // Expected for pre-grade 1
                }

                if (reportableGrade) {
                    if (student.getFieldValueByBeanPath(m_fieldStdCteCluster) != null) {
                        String careerCluster = (String) student.getFieldValueByBeanPath(m_fieldStdCteCluster);
                        value = data.lookupStateValue(SisStudent.class, m_fieldStdCteCluster, careerCluster);
                    } else {
                        ArrayList<String> clusterCodes = new ArrayList<String>();
                        DistrictSchoolYearContext districtContext = getOrganization().getCurrentContext();
                        Collection<StudentScheduleChange> studentScheduleChanges = null;
                        for (StudentSchedule studentSchedule : student.getStudentSchedules()) {
                            MasterSchedule section = studentSchedule.getSection();
                            if (section != null && section.getSchoolCourse() != null
                                    && section.getSchoolCourse().getCourse() != null) {
                                // Skip courses for schools that are excluded from
                                // reporting
                                boolean isSchoolExcluded = false;
                                SisSchool school = section.getSchoolCourse().getSchool();
                                if (school != null && !StringUtils.isEmpty(m_fieldExcludeSchool)) {
                                    isSchoolExcluded =
                                            "1".equals(school.getFieldValueByBeanPath(m_fieldExcludeSchool));
                                }

                                if (!isSchoolExcluded) {
                                    // Only consider courses for the current school
                                    // year
                                    Schedule schedule = studentSchedule.getSchedule();
                                    DistrictSchoolYearContext courseContext = schedule.getDistrictContext();
                                    if (courseContext.getOid().equals(districtContext.getOid())) {
                                        Course course = studentSchedule.getSection().getSchoolCourse().getCourse();
                                        if (m_fieldCteCluster != null) {
                                            String careerCluster = (String) course
                                                    .getFieldValueByBeanPath(m_fieldCteCluster);
                                            if (!StringUtils.isEmpty(careerCluster)) {
                                                careerCluster =
                                                        data.lookupStateValue(Course.class, m_fieldCteCluster,
                                                                careerCluster);
                                            }
                                            if (!StringUtils.isEmpty(careerCluster)) {
                                                if (studentScheduleChanges == null) {
                                                    studentScheduleChanges = student.getStudentScheduleChanges();
                                                }
                                                if (isDroppedCourse(section, studentScheduleChanges)) {
                                                    // do nothing
                                                } else {
                                                    clusterCodes.add(careerCluster);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        Collections.sort(clusterCodes);
                        ArrayList<String> tempCodes = new ArrayList<String>();
                        tempCodes.addAll(clusterCodes);
                        int codeTotal = 0;
                        for (String code : clusterCodes) {
                            int currentCodeTotal = 0;
                            while (tempCodes.contains(code)) {
                                tempCodes.remove(code);
                                currentCodeTotal++;
                                if (currentCodeTotal >= codeTotal) {
                                    value = code;
                                    codeTotal = currentCodeTotal;
                                }
                            }
                        }
                    }
                }
            }
            return value;
        }

        /**
         * Checks if is dropped course.
         *
         * @param section Course
         * @param studentScheduleChanges Collection<StudentScheduleChange>
         * @return true, if is dropped course
         */
        private boolean isDroppedCourse(MasterSchedule section,
                                        Collection<StudentScheduleChange> studentScheduleChanges) {
            boolean isDropped = false;
            PlainDate latestEnrollDate = null;
            PlainDate latestDropDate = null;

            for (StudentScheduleChange studentScheduleChange : studentScheduleChanges) {
                MasterSchedule masterSchedule = studentScheduleChange.getMasterSchedule();
                if (masterSchedule != null && masterSchedule.getOid().equals(section.getOid()))

                {
                    if ("Drop".equals(studentScheduleChange.getChangeTypeCode())) {
                        latestDropDate = (latestDropDate == null) ? studentScheduleChange.getEffectiveDate()
                                : latestDropDate;
                        latestDropDate = (studentScheduleChange.getEffectiveDate().after(latestDropDate))
                                ? studentScheduleChange.getEffectiveDate()
                                : latestDropDate;
                    } else {
                        latestEnrollDate =
                                (latestEnrollDate == null) ? studentScheduleChange.getEffectiveDate()
                                        : latestEnrollDate;
                        latestEnrollDate =
                                (studentScheduleChange.getEffectiveDate().after(latestEnrollDate))
                                        ? studentScheduleChange.getEffectiveDate()
                                        : latestEnrollDate;
                    }

                }
            }



            if (latestDropDate != null) {
                isDropped = true;
                if (latestEnrollDate != null
                        && (latestEnrollDate.after(latestDropDate) || latestEnrollDate.equals(latestDropDate))) {
                    isDropped = false;
                }
            }
            return isDropped;

        }
    }

    /**
     *
     * The cumulative number STD was enrolled in the school but due to his course schedule, was
     * not expected to attend school.
     *
     */
    protected class RetrieveUnscheduledDays implements FieldRetriever {

        protected static final String CALC_ID = "SRC-UNSCHED-DAYS";

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
            StudentRecordEntity srcEntity = (StudentRecordEntity) entity;
            MembershipAttendance attendance = srcEntity.getMembershipAttendance();
            return !"N".equals(srcEntity.getFieldValue(FIELD_STATUS)) ? String.valueOf(attendance.getAbsentDU())
                    : "0";
        }
    }

    /**
     * Validate CTE Attainment. If the collection is Pre-Submission or EOY and CTE Attainment is
     * "Y", CTE Finisher Code must be "1" or "5".
     */
    protected class ValidateAttainment implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            VAStudentRecordCollection src = (VAStudentRecordCollection) data;
            if (src.m_collection != null
                    && (COLLECTION_PRESUB.equals(src.m_collection) || COLLECTION_EOY.equals(src.m_collection))) {
                if ("Y".equals(value)) {
                    String cteFinisherValue = entity.getFieldValue(FIELD_CTE_FINISHER);
                    if (!"1".equals(cteFinisherValue) || !"5".equals(cteFinisherValue)) {
                        StateReportValidationError error = new StateReportValidationError(entity, field,
                                "If " + field.getFieldId() + " is Y, then CTE Finisher Code must be 1 or 5",
                                FIELD_CTE_FINISHER + "=" + STYLE_BOLD + cteFinisherValue + STYLE_END + ", "
                                        + field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END);
                        errors.add(error);
                    }
                }
            }
            return errors;
        }
    }

    /**
     * Validation for attendance days. Student should not have zero attendance days.
     */
    protected class ValidateAttendanceDays implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            StudentRecordEntity srEntity = (StudentRecordEntity) entity;
            MembershipAttendance ma = srEntity.getMembershipAttendance();
            if (ma.getAttendance() == 0) {
                StateReportValidationError error = new StateReportValidationError(entity, field,
                        "Aggregate present days is 0", EMPTY_STRING);
                errors.add(error);
            }
            return errors;
        }
    }

    /**
     * Validate Career Cluster. If the collection is Pre-Submission or EOY:
     * Must be null for grades PK, JK, KA, KP, KG, 01, 02, 03, 04, 05, and 06.
     */
    protected class ValidateCareerCluster implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            VAStudentRecordCollection src = (VAStudentRecordCollection) data;
            if (src.m_collection != null
                    && (COLLECTION_PRESUB.equals(src.m_collection) || COLLECTION_EOY.equals(src.m_collection))) {
                Set<String> nullGradeLevels = new HashSet<String>(
                        Arrays.asList("PK", "JK", "KA", "KP", "KG", "01", "02", "03", "04", "05", "06"));
                String studentGradeLevel = entity.getFieldValue(FIELD_GRADE_LEVEL);

                if (nullGradeLevels.contains(studentGradeLevel) && !StringUtils.isEmpty(value)) {
                    StateReportValidationError error = new StateReportValidationError(entity, field,
                            field.getFieldId()
                                    + " must be NULL for grades PK, JK, KA, KP, KG, 01, 02, 03, 04, 05, and 06",
                            FIELD_GRADE_LEVEL + "=" + STYLE_BOLD + studentGradeLevel + STYLE_END + ", "
                                    + field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END);
                    errors.add(error);
                }
            }

            return errors;
        }
    }

    /**
     * Validate CTE Finisher. If the collection is Pre-Submission or EOY:
     * Must be null for grades PK, JK, KA, KP, KG, 01, 02, 03, 04, 05, and 06.
     * Required for grades 07, 08, 09, 10, 11, 12, and TT.
     */
    protected class ValidateCteFinisher implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            VAStudentRecordCollection src = (VAStudentRecordCollection) data;
            if (src.m_collection != null
                    && (COLLECTION_PRESUB.equals(src.m_collection) || COLLECTION_EOY.equals(src.m_collection))) {
                Set<String> requiredGradeLevels =
                        new HashSet<String>(Arrays.asList("07", "08", "09", "10", "11", "12", "TT"));
                Set<String> nullGradeLevels = new HashSet<String>(
                        Arrays.asList("PK", "JK", "KA", "KP", "KG", "01", "02", "03", "04", "05", "06"));
                String studentGradeLevel = entity.getFieldValue(FIELD_GRADE_LEVEL);

                if (requiredGradeLevels.contains(studentGradeLevel) && StringUtils.isEmpty(value)) {
                    StateReportValidationError error = new StateReportValidationError(entity, field,
                            field.getFieldId() + " is required for grades 07, 08, 09, 10, 11, 12, and TT",
                            FIELD_GRADE_LEVEL + "=" + STYLE_BOLD + studentGradeLevel + STYLE_END + ", "
                                    + field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END);
                    errors.add(error);
                } else if (nullGradeLevels.contains(studentGradeLevel) && !StringUtils.isEmpty(value)) {
                    StateReportValidationError error = new StateReportValidationError(entity, field,
                            field.getFieldId()
                                    + " must be NULL for grades PK, JK, KA, KP, KG, 01, 02, 03, 04, 05 and 06",
                            FIELD_GRADE_LEVEL + "=" + STYLE_BOLD + studentGradeLevel + STYLE_END + ", "
                                    + field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END);
                    errors.add(error);
                }
            }

            return errors;
        }
    }

    /**
     * Validate division ID.
     */
    protected class ValidateDivId implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String tuitionPaidValue = entity.getFieldValue(FIELD_TUITION_PAID);
            String divIdServeValue = entity.getFieldValue(FIELD_DIVISION_ID_SERVE);
            if (!StringUtils.isEmpty(tuitionPaidValue) && value.equals(divIdServeValue)) {
                errors.add(new StateReportValidationError(entity, field,
                        "If the Tuition Paid field is populated, the Division ID and Division ID Serve must not be the same",
                        FIELD_TUITION_PAID + "=" + STYLE_BOLD + tuitionPaidValue + STYLE_END + ", " +
                                FIELD_DIVISION_ID_SERVE + "=" + STYLE_BOLD + divIdServeValue + STYLE_END + ", " +
                                field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validate disadvantaged is not empty when nighttime residence is not empty.
     */
    protected class ValidateDisadvantaged implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String nightResidence = entity.getFieldValue(FIELD_NIGHT_RESIDENCE);
            if (!"Y".equals(value) && !StringUtils.isEmpty(nightResidence)) {
                StateReportValidationError error = new StateReportValidationError(entity, field,
                        field.getFieldId() + " must be Y when nighttime residence is not blank",
                        FIELD_NIGHT_RESIDENCE + "=" + STYLE_BOLD + nightResidence + STYLE_END + ", "
                                + field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END);
                errors.add(error);
            }
            return errors;
        }
    }

    /**
     * Validate dob.
     */
    protected class ValidateDob implements FieldValidator {
        private Calendar m_calendar = null;
        private SimpleDateFormat m_exportFormat = new SimpleDateFormat("MM/dd/yyyy");

        /**
         * Instantiates a new validate dob.
         */
        public ValidateDob() {
            m_calendar = Calendar.getInstance();
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String gedProgramValue = entity.getFieldValue(FIELD_GED_PROGRAM);
            try {
                if (!StringUtils.isEmpty(gedProgramValue)) {

                    m_calendar.setTime(m_exportFormat.parse(value));
                    m_calendar.add(Calendar.YEAR, 16);
                    if (m_calendar.getTime().after(m_reportDate)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "If GED Program is not empty, student must be at least 16 years old",
                                FIELD_GED_PROGRAM + "=" + STYLE_BOLD + gedProgramValue + STYLE_END + ", " +
                                        field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END + ", " +
                                        "report date = " + STYLE_BOLD + m_exportFormat.format(m_reportDate)
                                        + STYLE_END));
                    }
                }
            } catch (ParseException pe) {
                pe.printStackTrace();
            }


            return errors;
        }
    }

    /**
     * Validate Graduate Comp.
     */
    protected class ValidateGradComp implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String creditAccomodationValue = entity.getFieldValue(FIELD_CRED_ACC);
            if ("Y".equals(creditAccomodationValue) && !"1".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "If the Credit Accommodation is Y, the Graduate Comp must be 1",
                        FIELD_CRED_ACC + "=" + STYLE_BOLD + creditAccomodationValue + STYLE_END + ", " +
                                field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validate Head Start Provider Code. If the student grade level is PK
     * and the PK funding source is 1 the value is required.
     */
    protected class ValidateHeadStart implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            VAStudentRecordCollection src = (VAStudentRecordCollection) data;
            if (src.m_collection != null) {
                String gradeLevel = entity.getFieldValue(FIELD_GRADE_LEVEL);
                String pkFundingCode = entity.getFieldValue(FIELD_PK_FUNDING_CODE);

                if (!StringUtils.isEmpty(gradeLevel) && gradeLevel.contains("PK") && !StringUtils.isEmpty(pkFundingCode)
                        && StringUtils.isEqual(pkFundingCode, "1") && StringUtils.isEmpty(value)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "This field is required.",
                            "Field " + STYLE_BOLD + field.getFieldId() + STYLE_END
                                    + " is empty and is required when student grade level = PK and DOE PK FUNDING = 1."));
                }
            }
            return errors;
        }
    }


    /**
     * A field value is required if the student has a disability.
     */
    protected class ValidateRequireDisability implements FieldValidator {
        private final String DISABILITY_504 = "15";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String disabilityCode = entity.getFieldValue(FIELD_PRIMARY_DISABILITY);
            if (StringUtils.isEmpty(value)
                    && !(StringUtils.isEmpty(disabilityCode) || DISABILITY_504.equals(disabilityCode))) {
                StateReportValidationError error = new StateReportValidationError(entity, field,
                        field.getFieldId() + " is required when a student has a primary disability",
                        "Primary Disability=" + STYLE_BOLD + disabilityCode + STYLE_END + ", " + field.getFieldId()
                                + "=" + STYLE_BOLD + value + STYLE_END);
                errors.add(error);
            }
            return errors;
        }
    }

    /**
     * Validate CTE Special Population. If the collection is Pre-Submission or EOY:
     * Must be null for grades PK, JK, KA, KP, KG, 01, 02, 03, 04, 05, and 06.
     */
    protected class ValidateSpecialPopulation implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            VAStudentRecordCollection src = (VAStudentRecordCollection) data;
            if (src.m_collection != null
                    && (COLLECTION_PRESUB.equals(src.m_collection) || COLLECTION_EOY.equals(src.m_collection))) {
                Set<String> nullGradeLevels = new HashSet<String>(
                        Arrays.asList("PK", "JK", "KA", "KP", "KG", "01", "02", "03", "04", "05", "06"));
                String studentGradeLevel = entity.getFieldValue(FIELD_GRADE_LEVEL);

                if (nullGradeLevels.contains(studentGradeLevel) && !StringUtils.isEmpty(value)) {
                    StateReportValidationError error = new StateReportValidationError(entity, field,
                            field.getFieldId()
                                    + " must be NULL for grades PK, JK, KA, KP, KG, 01, 02, 03, 04, 05, and 06",
                            FIELD_GRADE_LEVEL + "=" + STYLE_BOLD + studentGradeLevel + STYLE_END + ", "
                                    + field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END);
                    errors.add(error);
                }
            }

            return errors;
        }
    }

    /**
     * General validations for the entity:
     *
     * Checking if the enrollment period of one school overlaps the enrollment period for
     * another
     * school
     *
     * If CTE Finisher Code is 1 or 5 and Graduate Code is 1-9, one of the following must be
     * satisfied:
     * 1. Address line 1 and Zip Code are populated
     * 2. Phone Number is populated
     */
    protected class ValidateEntity implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            StudentRecordEntity sreEntity = (StudentRecordEntity) entity;
            List validationErrors = new ArrayList<StateReportValidationError>();
            if (sreEntity.m_schoolList.size() > 0) {
                MembershipAttendance currentSchool = sreEntity.m_schoolList.get(sreEntity.getCurrentRow());
                PlainDate currentStartDate = null;
                if (currentSchool.getEntryEnrollment() != null) {
                    currentStartDate = currentSchool.getEntryEnrollment().getEnrollmentDate();
                }

                PlainDate currentEndDate = null;
                if (currentSchool.getExitEnrollment() != null) {
                    currentEndDate = currentSchool.getExitEnrollment().getEnrollmentDate();
                }
                // Skipping those schools that have passed before.
                for (int i = sreEntity.getCurrentRow() + 1; i < sreEntity.m_schoolList.size(); i++) {
                    MembershipAttendance checkingSchool = sreEntity.m_schoolList.get(i);
                    if (currentSchool.equals(checkingSchool)) {
                        continue;
                    }
                    boolean isOverlaps = false;

                    PlainDate checkingStartDate = null;
                    if (checkingSchool.getEntryEnrollment() != null) {
                        checkingStartDate = checkingSchool.getEntryEnrollment().getEnrollmentDate();
                    }
                    PlainDate checkingEndDate = null;
                    if (checkingSchool.getExitEnrollment() != null) {
                        checkingEndDate = checkingSchool.getExitEnrollment().getEnrollmentDate();
                    }
                    // Here we assume that currentStartDate and
                    // checkingStartDate always not null, because
                    // entity filter out spans without entry enrollment.
                    if ((currentEndDate != null) && (checkingEndDate != null)) {
                        if ((currentStartDate.before(checkingStartDate)
                                && !currentEndDate.before(checkingStartDate))
                                || (checkingStartDate.before(currentStartDate)
                                        && !checkingEndDate.before(currentStartDate))) {
                            isOverlaps = true;
                        }
                    } else if ((currentEndDate == null) && (checkingEndDate != null)) {
                        if (!checkingEndDate.before(currentStartDate)) {
                            isOverlaps = true;
                        }
                    } else if ((currentEndDate != null) && (checkingEndDate == null)) {
                        if (!currentEndDate.before(checkingStartDate)) {
                            isOverlaps = true;
                        }
                    } else {
                        isOverlaps = true;
                    }
                    if (isOverlaps) {
                        String schoolName = currentSchool.getSchool().getName();
                        String overlapSchoolName = checkingSchool.getSchool().getName();
                        StringBuffer message = new StringBuffer();
                        message.append("School ");
                        message.append(schoolName);
                        message.append(" overlaps with ");
                        message.append(overlapSchoolName);
                        message.append(".");
                        validationErrors.add(new StateReportValidationError(sreEntity.getEntityName(), "Entity",
                                "Enrollments overlaps", message.toString()));
                    }
                }
            }

            String cteFinisherCode = entity.getFieldValue(FIELD_CTE_FINISHER);
            String graduateCode = entity.getFieldValue(FIELD_GRADUATE_COMP);

            if (!StringUtils.isEmpty(cteFinisherCode) && !StringUtils.isEmpty(graduateCode)) {
                Set<String> finisherCodesToValidate = new HashSet<String>(Arrays.asList("1", "5"));
                Set<String> graduateCodesToValidate =
                        new HashSet<String>(Arrays.asList("1", "2", "3", "4", "5", "6", "7", "8", "9"));
                if (finisherCodesToValidate.contains(cteFinisherCode)
                        && graduateCodesToValidate.contains(graduateCode)) {
                    String address1 = entity.getFieldValue(FIELD_ADDRESS_1);
                    String zipCode = entity.getFieldValue(FIELD_ZIP_CODE);
                    String phoneNumber = entity.getFieldValue(FIELD_PHONE_NUMBER);
                    boolean demographicDataExists = false;

                    if (!StringUtils.isEmpty(phoneNumber)
                            || (!StringUtils.isEmpty(zipCode) && !StringUtils.isEmpty(address1))) {
                        demographicDataExists = true;
                    }
                    if (!demographicDataExists) {
                        StringBuffer message = new StringBuffer();
                        message.append("CTE Finisher Code: ");
                        message.append(cteFinisherCode);
                        message.append(", Graduate Code: ");
                        message.append(graduateCode);
                        message.append(", Phone:");
                        message.append(phoneNumber);
                        message.append(", Address1: ");
                        message.append(address1);
                        message.append(", Zip:");
                        message.append(zipCode);
                        validationErrors.add(new StateReportValidationError(sreEntity.getEntityName(), "Entity",
                                "If CTE Finisher Code is 1 or 5 and Graduate Code is 1-9, either a phone number or an address and zip code are required",
                                message.toString()));
                    }
                }

            }


            return validationErrors;
        }
    }

    /**
     * Returns heading text to include at the top of the export file. For Student Record
     * Collection,
     * this will include
     * the heading and the A record.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        // get values used in the heading.
        String divId = (String) getOrganization().getFieldValueByAlias(ALIAS_DISTRICT_CODE);
        String email = getUser().getPerson().getEmail01();
        if (StringUtils.isEmpty(email)) {
            email = "{email}";
        }

        // Header row
        StringBuilder header = new StringBuilder(70);
        java.util.Date currDate = new Date();
        SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");
        SimpleDateFormat timeFormat = new SimpleDateFormat("hh:mm:ss");
        int year = getOrganization().getCurrentContext().getSchoolYear() - 1;

        // Heading row.

        header.append("SenderID=").append(divId).append(EOL);
        header.append("CreateDate=").append(dateFormat.format(currDate)).append(EOL);
        header.append("CreateTime=").append(timeFormat.format(currDate)).append(EOL);
        header.append("EMAIL=").append(email).append(EOL);
        header.append("~~").append(EOL);
        header.append("DATATYPE=");
        if (!COLLECTION_SPED.equals(m_collection)) {
            header.append("STUDENT_");
        }
        header.append(m_collection).append(EOL);
        header.append("~").append(EOL);

        // A record.
        header.append("A");
        if (COLLECTION_FALL.equals(m_collection)) {
            header.append("1");
        } else if (COLLECTION_SPRING.equals(m_collection)) {
            header.append("2");
        } else if (COLLECTION_EOY.equals(m_collection)) {
            header.append("3");
        } else if (COLLECTION_SUMMER.equals(m_collection)) {
            header.append("4");
        } else if (COLLECTION_PRESUB.equals(m_collection)) {
            header.append("9");
        } else if (COLLECTION_SPED.equals(m_collection)) {
            header.append("8");
        }
        header.append(Integer.toString(year));
        header.append(divId);
        header.append(EOL);

        return header.toString();
    }

    /**
     * Override the delimiter character. It may have been altered in the initialization method.
     *
     * @return Character
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getValueDelimiter()
     */
    @Override
    public Character getValueDelimiter() {
        return m_vasrcDelimiterChar;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        /*
         * Define delimiter char. The char can be a tab. The user may enter a tab as "\t" in the
         * Export Format
         * Definition. Internally, only the "\" will come through, so assume it represents a tab
         * and
         * change the
         * delimiter char to a tab.
         */
        m_vasrcDelimiterChar = super.getValueDelimiter();
        if ((m_vasrcDelimiterChar != null) && (m_vasrcDelimiterChar.charValue() == '\\')) {
            m_vasrcDelimiterChar = Character.valueOf('\t');
        }

        m_activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        m_preRegCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.STUDENT_PREREG_CODE);
        m_enrollmentCodeRefTableOid = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_ENTRY_CODES);
        m_withdrawalCodeRefTableOid = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);
        m_enrollmentReasonRefTableOid = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_ENTRY_REASONS);
        m_withdrawalReasonRefTableOid = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_REASONS);

        m_collection = (String) getParameter(PARAM_COLLECTION);
        m_removeNonrequiredFields = (getParameter(PARAM_NON_REQUIRED) != null
                ? ((Boolean) getParameter(PARAM_NON_REQUIRED)).booleanValue()
                : false);
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        m_summerBeginDate = (PlainDate) getParameter(PARAM_SUMMER_DATE);
        m_missingSasid = getParameter(PARAM_MISSING_SASID) == null ? false
                : ((Boolean) getParameter(PARAM_MISSING_SASID)).booleanValue();
        m_preregOnly = getParameter(PARAM_PREREG_ONLY) == null ? false
                : ((Boolean) getParameter(PARAM_PREREG_ONLY)).booleanValue();

        // Initialize to 1 row for the A record in the heading.
        m_rowCount = 1;

        initializeFields();

        if (getSetupErrors().size() == 0) {

            // Remove non-required fields.
            // For each non-required field, blank the bean path, default value and
            // calculation.
            int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
            if (m_removeNonrequiredFields) {
                for (String fieldName : getNonrequiredFieldNames(m_collection, queryBy)) {
                    FieldDefinition fieldDef = getFieldDefinition(fieldName);
                    if (fieldDef != null) {
                        fieldDef.setBeanPath(StateReportData.LABEL_PREFIX_CHAR + fieldDef.getFieldId());
                        fieldDef.setRetriever(null);
                        // fieldDef.setDefaultValue(null);
                        // If we have a reference table lookup, this will cause this
                        // code to blow up. Lets scrub the field to keep it safe
                        if (fieldDef.getMappedLookup() != 0) {
                            List<FieldDefinition> fieldDefinitions = getFieldDefinitions();

                            FieldDefinition newFieldDef =
                                    new FieldDefinition(fieldDef.getFieldId(), fieldDef.getBeanPath(),
                                            fieldDef.getDefaultValue(), 0, // No Lookup

                                            fieldDef.getMinLength(), fieldDef.getMaxLength(), fieldDef.getPattern(),
                                            fieldDef.getFormatter(), fieldDef.getRetriever(), fieldDef.getValidator(),
                                            fieldDef.getParameter());

                            int fieldLocation = fieldDefinitions.indexOf(fieldDef);
                            fieldDefinitions.remove(fieldLocation);
                            fieldDefinitions.add(fieldLocation, newFieldDef);
                            setFieldDefinitions(fieldDefinitions);
                        }
                    }
                }
            }

            /*
             * Build the criteria/query for the staff to include in this export based on user input.
             */
            Criteria studentCriteria = getStudentCriteria();
            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
            m_sort = (Integer) getParameter(PARAM_SORT);
            switch (m_sort != null ? m_sort.intValue() : 0) {
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
                    studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
                    break;
            }

            // Special sort order for prereg
            if (COLLECTION_PRESUB.equals(m_collection) && ((m_sort != null) && (m_sort.intValue() != 3))) {
                studentQuery.addOrderBy(Student.COL_LOCAL_ID, true);
            }

            // Set the query to be used for student selection.
            setQuery(studentQuery);

            setEntityClass(StudentRecordEntity.class);

            loadEnrollments(studentCriteria);
            loadAttendance(studentCriteria);
            loadRaceCodes(studentCriteria);
            loadStudentSchedules();
            loadTranscipts(studentCriteria);
            loadLAVCAsms(studentCriteria);
            loadSchoolOverrides();

            // Add any retrievers or validators.
            RetrieveHomeless homelessRetriever = new RetrieveHomeless(studentCriteria);
            RetrieveEnglishLearners elRetriver = new RetrieveEnglishLearners(studentCriteria);
            RetrieveLiep liepRetriver = new RetrieveLiep(studentCriteria);

            Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("SRC-ATTENDANCE", new RetrieveAttendance());
            calcs.put("SRC-CTE-ONLY", new RetrieveCTEOnly());
            calcs.put("SRC-DIGITS", new RetrieveDigits());
            calcs.put("SRC-DISABILITY", new RetrieveDisability());
            calcs.put("SRC-DISADV", new RetrieveDisadvantavedStatus());
            calcs.put("SRC-DISTRICT-SCHOOL", new RetrieveDistrictSchool());
            calcs.put("SRC-ENROL", new RetrieveEnrollment());
            calcs.put("SRC-GED", new RetrieveGED());
            calcs.put("SRC-GIFTED-DATE", new RetrieveGifted());
            calcs.put("SRC-GRAD", new RetrieveGrad());
            calcs.put("SRC-GRADE-LEVEL", new RetrieveGradeLevelOverride());
            calcs.put("SRC-IB-FLAG", new RetrieveIBFlag());
            calcs.put("SRC-IMMIGRANT", new RetrieveImmigrant());
            calcs.put("SRC-RACE", new RetrieveRace());
            calcs.put("SRC-SPED-WK-TIME", new RetrieveSpedWeeklyTime());
            calcs.put("SRC-STATUS", new RetrieveStatus());
            calcs.put("SRC-TUITION-PAID", new RetrieveTuitionPaid());
            calcs.put("SRC-TUITION-REIMB", new RetrieveTuitionReimbursment());
            calcs.put("SRC-POP", new RetrievePopulation());
            calcs.put("SRC-CC", new RetrieveCareerCluster());
            calcs.put("SRC-VIRTUAL", new RetrieveVirtualProgram());
            calcs.put("SRC-SPED-PCT", new RetrieveSpedPercent());
            calcs.put("NOSPRING", new RetrieveNoSpring());
            calcs.put("CTE-FINISHER", new RetrieveCTEFinisher());
            calcs.put("SRC-FOSTER", new RetrieveFosterCare(studentCriteria));
            calcs.put("SRC-PGM-REMOTE", new RetrievePGMRemoteLearning(studentCriteria));
            calcs.put("SRC-HOMELESS", homelessRetriever);
            calcs.put("SRC-EL", elRetriver);
            calcs.put(RetrieveUnscheduledDays.CALC_ID, new RetrieveUnscheduledDays());
            calcs.put(RetrieveCodes.CALC_ID, new RetrieveCodes());
            calcs.put(RetrieveLiep.CALC_ID, liepRetriver);
            calcs.put(RetrieveAsmLAVC.CALC_ID, new RetrieveAsmLAVC());

            Map<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put("SRC-ATTENDANCE", new ValidateAttendanceDays());
            validators.put("SRC-DISADV", new ValidateDisadvantaged());
            validators.put("SRC-REQUIRES-DISAB", new ValidateRequireDisability());
            validators.put("SRC-ENTITY", new ValidateEntity());
            validators.put("CTE-FINISHER", new ValidateCteFinisher());
            validators.put("CTE-CAREER-CLUSTER", new ValidateCareerCluster());
            validators.put("CTE-SPECIAL-POP", new ValidateSpecialPopulation());
            validators.put("CTE-ATTAINMENT", new ValidateAttainment());
            validators.put("SRC-HOMELESS", homelessRetriever);
            validators.put("SRC-EL", elRetriver);
            validators.put("SRC-GRAD-COMP", new ValidateGradComp());
            validators.put("SRC-DOB", new ValidateDob());
            validators.put("SRC-DIV-ID", new ValidateDivId());
            validators.put("SRC-HEAD-START", new ValidateHeadStart());
            validators.put(RetrieveLiep.CALC_ID, liepRetriver);
            super.addCalcs(calcs);
            super.addValidators(validators);
        }

    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#close()
     */
    @Override
    public void close() {
        // nothing needed for collection iterator
    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#next()
     */
    @Override
    public StateReportEntity next() throws X2BaseException {
        StateReportEntity entity = null;

        // See if the current entity can iterate more. If not, abandon it.
        if (m_currentEntity != null) {
            if (m_currentEntity.getCurrentRow() + 1 < m_currentEntity.getRowCount()) {
                m_currentEntity.setCurrentRow(m_currentEntity.getCurrentRow() + 1);
                entity = m_currentEntity;
            } else {
                m_currentEntity = null;
            }
        }

        /*
         * If multiple queries are used, and the current query/iterator is exhausted,
         * open the next available query/iterator.
         */
        boolean iteratorHasNext = false;
        if (m_dataIterator != null) {
            iteratorHasNext = m_dataIterator.hasNext();
        }
        /*
         * If the last entity was exhausted, get another from the iterator.
         * Entities may generate zero rows, if so skip them and get the
         * next from the iterator until it is exhausted too.
         */
        while (entity == null && getEntityClass() != null && m_dataIterator != null && iteratorHasNext) {
            X2BaseBean bean = (X2BaseBean) m_dataIterator.next();
            if (bean != null) {
                try {
                    entity = (StateReportEntity) getEntityClass().getDeclaredConstructor().newInstance();
                } catch (ClassCastException | InstantiationException | IllegalAccessException | IllegalArgumentException
                        | InvocationTargetException | NoSuchMethodException | SecurityException e) {
                    throw new X2BaseException(e);
                }

                if (entity != null) {
                    entity.intitialize(this, bean);
                    if (entity.getRowCount() > 0) {
                        entity.setCurrentRow(0);
                        m_currentEntity = entity;
                    } else {
                        entity = null;
                    }
                }
            }

            iteratorHasNext = m_dataIterator.hasNext();
        }

        // Get the correct fields set for this entity/iteration based on the entity requested
        // definition Id.
        if (entity != null) {
            String fieldKey = entity.getCurrentFormatDefinitionId();
            m_currentFieldDefinitions = m_loadedFieldDefinitions.get(fieldKey);
            if (m_currentFieldDefinitions == null) {
                m_currentFieldDefinitions = new ArrayList<FieldDefinition>();
            }
        }

        return entity;

    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#open()
     */
    @Override
    public boolean open() {
        QueryByCriteria query = getQuery();
        if (query == null) {
            return false;
        }
        m_dataIterator = getBroker().getCollectionByQuery(query).iterator();
        return (m_dataIterator != null);
    }

    /**
     * Returns the days-in-session for the given school and calendar combination.
     *
     * @param school SisSchool
     * @param calendar String
     * @return Set of PlainDate objects
     */
    protected Set getCalendarDays(SisSchool school, String calendar) {
        if (!m_schoolsToCalendars.containsKey(school.getOid())) {
            PlainDate startDate = null;
            Schedule schedule = school.getActiveSchedule();
            if (schedule != null) {
                startDate = school.getActiveSchedule().getStartDate();
            } else {
                startDate = m_districtStartDate;
            }
            Map calendarData = m_enrollmentManager.getCalendarLookup(school, startDate, m_reportDate);
            m_schoolsToCalendars.put(school.getOid(), calendarData);
        }

        Set set = (Set) ((Map) m_schoolsToCalendars.get(school.getOid())).get(calendar);
        if (set == null) {
            set = (Set) ((Map) m_schoolsToCalendars.get(school.getOid())).get(DEFAULT_CALENDAR_NAME);
            if (set == null) {
                Set entrySet = ((Map) m_schoolsToCalendars.get(school.getOid())).entrySet();
                set = (Set) ((Map.Entry) entrySet.iterator().next()).getValue();
            }
        }
        return set;
    }

    /**
     * Gets the first school in session date.
     *
     * @param school SisSchool
     * @param calendarId String
     * @return Plain date
     */
    protected PlainDate getFirstSchoolInSessionDate(SisSchool school, String calendarId) {
        calendarId = StringUtils.isEmpty(calendarId) ? DEFAULT_CALENDAR_NAME : calendarId;
        String calendarKey = school.getOid() + calendarId;
        if (!m_firstDaysOfSchools.containsKey(calendarKey)) {
            SchoolCalendar actualCalendar = null;
            while (actualCalendar == null) {
                for (SchoolCalendar calendar : school.getSchoolCalendars(getBroker())) {
                    if (calendarId.equals(calendar.getCalendarId())
                            && calendar.getDistrictContextOid().equals(getOrganization().getCurrentContextOid())) {
                        actualCalendar = calendar;
                        break;
                    }
                }
                // If student calendarId not standard, in first try to find
                // calendar for student calendarId
                if (!DEFAULT_CALENDAR_NAME.equals(calendarId)) {
                    calendarId = DEFAULT_CALENDAR_NAME;
                    calendarKey = school.getOid() + calendarId;
                } else {
                    break;
                }
            }
            PlainDate firstInSessionDate = null;
            if (actualCalendar != null) {
                SisSchoolCalendarDate firstDay = CalendarManager.getFirstInSessionDate(actualCalendar, getBroker());
                firstInSessionDate = firstDay.getDate();
            } else {
                firstInSessionDate = m_districtStartDate;
            }
            m_firstDaysOfSchools.put(calendarKey, firstInSessionDate);
        }
        return m_firstDaysOfSchools.get(calendarKey);
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + "." + RecordSet.COL_NAME, recordSetName);

        criteria.addIn(X2BaseBean.COL_OID,
                new SubQuery(RecordSetKey.class, RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Gets the assessment dictionary.
     *
     * @return Data dictionary
     */
    protected DataDictionary getAssessmentDictionary() {
        if (m_asmDictLAVC == null) {
            X2Criteria assessmentDefinitonCriteria = new X2Criteria();
            assessmentDefinitonCriteria.addEqualTo(AssessmentDefinition.COL_ID, ASD_ID_LAVC);

            QueryByCriteria assessmentDefinitonQuery =
                    new QueryByCriteria(AssessmentDefinition.class, assessmentDefinitonCriteria);

            AssessmentDefinition assessmentDefinition =
                    (AssessmentDefinition) getBroker().getBeanByQuery(assessmentDefinitonQuery);
            if (assessmentDefinition == null) {
                addSetupError(ERROR_TYPE_WARNING,
                        ASD_ID_LAVC + ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED);
            } else {
                m_asmDictLAVC =
                        DataDictionary.getDistrictDictionary(assessmentDefinition, getBroker().getPersistenceKey());
            }
        }
        return m_asmDictLAVC;
    }

    /**
     * Get Field Java Name according given alias.
     *
     * @param alias String
     * @param dataDictionary DataDictionary
     * @param isRequired boolean
     * @return String
     */
    private String getAsmJavaName(String alias, DataDictionary dataDictionary, boolean isRequired) {
        String javaName = null;
        DataDictionaryField dictField = dataDictionary.findDataDictionaryFieldByAlias(alias);
        if (dictField != null && dataDictionary.containsAlias(alias)) {
            javaName = dictField.getJavaName();
        } else if (isRequired) {
            String aliasMsg =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(ERROR_ALIAS_LOOKUP);
            addSetupError(aliasMsg, alias);
        }
        return javaName;
    }

    /**
     * For the collection specified, return a list of field definitions that are not required
     * for
     * the collection.
     *
     * @param collection String
     * @param queryBy int
     * @return String[]
     */
    private String[] getNonrequiredFieldNames(String collection, int queryBy) {
        String[] fields = null;
        if (queryBy == 4) {
            for (String[] arrayFields : NON_REQUIRED_FIELDS) {
                if (arrayFields[0].equals(COLLECTION_SNAPSHOT)) {
                    fields = arrayFields;
                    break;
                }
            }
        } else {
            for (String[] arrayFields : NON_REQUIRED_FIELDS) {
                if (arrayFields[0].equals(collection)) {
                    fields = arrayFields;
                    break;
                }
            }
        }
        return fields;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        /*
         * First build the criteria based on the user's input
         */
        X2Criteria userCriteria = new X2Criteria();

        if (m_preregOnly) {
            userCriteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, m_preRegCode);
            // Check school context.
            if (isSchoolContext()) {
                userCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
            } else {
                userCriteria.addNotEqualTo(
                        SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                        Boolean.TRUE);
                userCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                        Boolean.TRUE);
                userCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + m_fieldExcludeSchool,
                        BooleanAsStringConverter.TRUE);
            }
        } else if (queryBy == 4) {
            // For Snapshot criteria, display all students in the snapshot
            // regardless of other statuses.
            // This can pick up students in archive or any past enrollment
            // status.
            // For historic students, much data could be out of date.
        } else {
            /*
             * Find active students or students with enrollment activity within the date range:
             * summer begin to report
             * date.
             */
            X2Criteria enrollmentCriteria = new X2Criteria();
            enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_summerBeginDate);
            if (isSchoolContext()) {
                enrollmentCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
            } else {
                enrollmentCriteria.addNotEqualTo(
                        StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                        Boolean.TRUE);
                enrollmentCriteria.addNotEqualTo(
                        StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                        Boolean.TRUE);
                enrollmentCriteria.addNotEqualTo(
                        StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + m_fieldExcludeSchool,
                        BooleanAsStringConverter.TRUE);
            }

            SubQuery enrollmentQuery = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID,
                    enrollmentCriteria);
            X2Criteria activeCriteria1 = new X2Criteria();
            X2Criteria activeCriteria2 = new X2Criteria();
            X2Criteria activeCriteria3 = new X2Criteria();

            // Collect enrollment statuses to include
            List<String> statuses = new LinkedList<String>();
            statuses.add(m_activeCode);
            statuses.add(STATUS_SERVICE);
            if (m_virtualStatusCodes.size() > 0) {
                statuses.addAll(m_virtualStatusCodes);
            }
            activeCriteria2.addIn(SisStudent.COL_ENROLLMENT_STATUS, statuses);

            // Check school context.
            if (isSchoolContext()) {
                activeCriteria2.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
            } else {
                activeCriteria2.addNotEqualTo(
                        SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                        Boolean.TRUE);
                activeCriteria2.addNotEqualTo(
                        SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                        Boolean.TRUE);
                activeCriteria2.addNotEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + m_fieldExcludeSchool,
                        BooleanAsStringConverter.TRUE);
            }

            activeCriteria3.addIn(X2BaseBean.COL_OID, enrollmentQuery);

            activeCriteria1.addOrCriteria(activeCriteria2);
            activeCriteria1.addOrCriteria(activeCriteria3);

            userCriteria.addAndCriteria(activeCriteria1);

            // Check the exclude student flag.
            userCriteria.addNotEqualTo(m_fieldExcludeStudent, BooleanAsStringConverter.TRUE);
        }

        // Optionally, report only students who are missing a SASID.
        if (m_missingSasid) {
            userCriteria.addEmpty(m_fieldSasid, getBroker().getPersistenceKey());
        }

        // For December 1/SPED report, include only students with SPED Placement
        // code.
        if (COLLECTION_SPED.equals(m_collection)) {
            userCriteria.addNotEmpty(m_fieldSpedDisability, getBroker().getPersistenceKey());
            /*
             * Restrict to Special Education Status ( STD_SPED_STATUS) = Active.
             * If not Active, check STD_SPED_EXIT_DATE, compare to Report date and if the exit
             * date
             * is
             * after the Report date, include student in export.
             */
            X2Criteria spedStatusCriteria = new X2Criteria();
            String spedActiveCode =
                    PreferenceManager.getPreferenceValue(getOrganization(),
                            SisPreferenceConstants.SPED_ACTIVE_CODE);
            spedStatusCriteria.addEqualTo(SisStudent.COL_SPED_STATUS_CODE, spedActiveCode);
            X2Criteria orCriteria = new X2Criteria();
            orCriteria.addGreaterThan(SisStudent.COL_SPED_EXIT_DATE, m_reportDate);
            spedStatusCriteria.addOrCriteria(orCriteria);
            userCriteria.addAndCriteria(spedStatusCriteria);
        }

        // Check user selection criteria.
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

        return userCriteria;
    }

    /**
     * Lookup alias fields.
     */
    private void initializeFields() {
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_CODE, true);
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_CODE, true);
        m_fieldHomeDistrictCode = translateAliasToJavaName(ALIAS_HOME_DISTRICT_CODE, true);
        m_fieldServiceDistrictCode = translateAliasToJavaName(ALIAS_SERVICE_DISTRICT_CODE, true);
        m_fieldHomeSchoolCode = translateAliasToJavaName(ALIAS_HOME_SCHOOL_CODE, true);
        m_fieldServiceSchoolCode = translateAliasToJavaName(ALIAS_SERVICE_SCHOOL_CODE, true);
        m_fieldSasid = translateAliasToJavaName(ALIAS_SASID, true);
        m_fieldSchoolTuition = translateAliasToJavaName(ALIAS_TUITION_PAID, true);
        m_fieldSpedDisability = translateAliasToJavaName(ALIAS_SPED_DISABILITY, true);
        m_fieldHBEntryCode = translateAliasToJavaName(ALIAS_HB_ENTRY_CODE, true);
        m_fieldHBWithdrawalCode = translateAliasToJavaName(ALIAS_HB_WITHDRAW_CODE, true);
        m_fieldStdHomeSchoolCode = translateAliasToJavaName(ALIAS_STD_HOME_SCHOOL_CODE, false);
        m_fieldExcludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
        m_fieldExcludeStudent = translateAliasToJavaName(ALIAS_EXCLUDE_STD, true);
        m_fieldCteFinisher = translateAliasToJavaName(ALIAS_CTE_FINISHER, true);
        m_fieldMopFlag = translateAliasToJavaName(ALIAS_MOP_FLAG, true);
        m_fieldVirtualProgram = translateAliasToJavaName(ALIAS_VIRTUAL_PROGRAM, true);
        m_fieldStdAttPlanCode = translateAliasToJavaName(ALIAS_STD_ATT_PLAN, true);
        m_fieldStdAttConfCode = translateAliasToJavaName(ALIAS_STD_ATT_CONF, true);
        m_fieldStdCourtRefCode = translateAliasToJavaName(ALIAS_STD_COURT_REF, true);
        m_fieldStdHeadStartProviderCode = translateAliasToJavaName(ALIAS_STD_HEAD_START_PROVIDER, true);
        m_fieldEnrAttPlanCode = translateAliasToJavaName(ALIAS_ENR_ATT_PLAN, true);
        m_fieldEnrAttConfCode = translateAliasToJavaName(ALIAS_ENR_ATT_CONF, true);
        m_fieldEnrCourtRefCode = translateAliasToJavaName(ALIAS_ENR_COURT_REF, true);
        m_fieldUsEntryDate = translateAliasToJavaName(ALIAS_US_ENTRY_DATE, true);
        m_fieldUsEndDate = translateAliasToJavaName(ALIAS_US_END_DATE, true);
        m_fieldImmigrantStatus = translateAliasToJavaName(ALIAS_IMMIGRANT_STATUS, true);
        m_fieldCteCluster = translateAliasToJavaName(ALIAS_CTE_CAREER_CLUSTER, true);
        m_fieldStdCteCluster = translateAliasToJavaName(ALIAS_STD_CAREER_CLUSTER, true);
        m_fieldTuitionDisability = translateAliasToJavaName(ALIAS_SPED_TUIT_DISABILITY, true);
        m_fieldTuitionS1 = translateAliasToJavaName(ALIAS_SPED_TUITION_S1, true);
        m_fieldTuitionS2 = translateAliasToJavaName(ALIAS_SPED_TUITION_S2, true);
        m_fieldTuitionSUM = translateAliasToJavaName(ALIAS_SPED_TUITION_SUM, true);
        m_fieldPgmLIEP = translateAliasToJavaName(ALIAS_PGM_LIEP, true);
        m_fieldPgmRegionalLocalCenterPercentofTime =
                translateAliasToJavaName(ALIAS_PGM_REGIONAL_LOCAL_CENTER_PERCENT_OF_TIME, true);
        m_fieldPgmRemoteInstructionPercentofTime =
                translateAliasToJavaName(ALIAS_PGM_REMOTE_INST_PERCENT_OF_TIME, true);
        m_fieldPgmInternetAccessforRemoteLearning =
                translateAliasToJavaName(ALIAS_PGM_INTERNET_ACCESS_REMOTE_LEARNING, true);
        m_fieldPgmDeviceAccessforRemoteLearning =
                translateAliasToJavaName(ALIAS_PGM_DEVICE_ACCESS_REMOTE_LEARNING, true);
        m_fieldPgmParentalRemoteLearningDecision =
                translateAliasToJavaName(ALIAS_PGM_PARENTAL_REMOTE_LEARNING_DECISION, true);
        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        m_schoolsToCalendars = new HashMap();
        m_firstDaysOfSchools = new HashMap<String, PlainDate>();

        m_activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        m_enrollmentCodeRefTableOid = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_ENTRY_CODES);
        m_withdrawalCodeRefTableOid = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);

        // Find enrollment status codes that represent "V" virtual class status
        m_virtualStatusCodes = new ArrayList<String>();
        m_serviceStatusCodes = new ArrayList<String>();
        DataDictionaryField enrollmentStatusfield = getDataDictionaryField(SisStudent.class,
                SisStudent.COL_ENROLLMENT_STATUS);
        Map<String, ReferenceCode> entryMap = getReferenceCodes(enrollmentStatusfield.getReferenceTableOid());
        for (ReferenceCode code : entryMap.values()) {
            if (STATUS_VIRTUAL_CLASS.equals(code.getStateCode())) {
                m_virtualStatusCodes.add(code.getCode());
            }
            if (STATUS_SERVICE.equals(code.getCode())) {
                m_serviceStatusCodes.add(code.getCode());
            }
        }

        // Find the isaep code from GED reference table. It has a state code of
        // 2.
        m_fieldGed = translateAliasToJavaName("DOE GED", true);
        DataDictionaryField gedDDField = getDataDictionaryField(Student.class, m_fieldGed);
        if ((gedDDField != null) && (gedDDField.getReferenceTableOid() != null)) {
            Map<String, ReferenceCode> codes = getReferenceCodes(gedDDField.getReferenceTableOid());
            for (ReferenceCode code : codes.values()) {
                if ("2".equals(code.getStateCode())) {
                    m_isaepCode = code.getCode();
                    break;
                }
            }
        }

        // Find a general "Start of School" date to use for schools that do
        // not have their own Schedule object to define start of school.
        DistrictCalendar districtFirstDay = CalendarManager.getDistrictInSessionStartEndDate(getOrganization(),
                getOrganization().getCurrentContext(), true, getBroker());
        m_districtStartDate = districtFirstDay.getDate();
        m_specialPopulationJavaname = translateAliasToJavaName(ALIAS_CTE_POPULATION, false);
    }

    /**
     * Loads a map of absence attendance records by student.
     *
     * @param studentCriteria Criteria
     */
    private void loadAttendance(Criteria studentCriteria) {
        // subQuery - Students.
        SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
        // Select all attendance absence records for each student in the
        // reporting date range.
        // Group by student.
        Criteria criteria = new Criteria();
        criteria.addIn(StudentAttendance.COL_STUDENT_OID, studentsSubQuery);
        criteria.addGreaterOrEqualThanField(StudentAttendance.COL_DATE,
                StudentAttendance.REL_SCHOOL + PATH_DELIMITER + SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER
                        + SchoolScheduleContext.REL_ACTIVE_SCHEDULE + PATH_DELIMITER + Schedule.COL_START_DATE);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_reportDate);
        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria);
        query.addOrderBy(StudentAttendance.COL_STUDENT_OID, true);
        query.addOrderBy(StudentAttendance.COL_DATE, true);
        m_attendance = getBroker().getGroupedCollectionByQuery(query, StudentAttendance.COL_STUDENT_OID, 100);
    }

    /**
     * Load a map of student enrollments for the students in the export.
     *
     * @param studentCriteria Criteria
     */
    private void loadEnrollments(Criteria studentCriteria) {
        SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        // Main report query - students and Absence.
        // Temporarily remove YOG records until rollover issues are past.
        List<String> enrollmentTypes = Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.WITHDRAWAL,
                StudentEnrollment.STATUS_CHANGE);
        Criteria criteria = new Criteria();
        criteria.addIn(StudentEnrollment.COL_STUDENT_OID, studentsSubQuery);
        criteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, enrollmentTypes);
        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
        query.addOrderBy(StudentEnrollment.COL_STUDENT_OID, true);
        query.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, false);
        query.addOrderBy(StudentEnrollment.COL_TIMESTAMP, false);
        m_enrollmentMap = getBroker().getGroupedCollectionByQuery(query, StudentEnrollment.COL_STUDENT_OID, 100);
    }

    /**
     * Loads a map of LAVC Assessments.
     *
     * @param studentCriteria Criteria
     */
    private void loadLAVCAsms(Criteria studentCriteria) {
        // subQuery - Students.
        SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
        X2Criteria asdCriteria = new X2Criteria();
        asdCriteria.addEqualTo(AssessmentDefinition.COL_ID, ASD_ID_LAVC);
        AssessmentDefinition asdLAVC = (AssessmentDefinition) getBroker()
                .getBeanByQuery(new QueryByCriteria(AssessmentDefinition.class, asdCriteria));
        if (asdLAVC != null) {
            DataDictionary dataDictionary =
                    DataDictionary.getDistrictDictionary(asdLAVC, getBroker().getPersistenceKey());
            m_fieldAsmCreditAccomodation = getAsmJavaName(ALIAS_ASM_CREDIT_ACC_CODE, dataDictionary, true);
            m_fieldAsmIntensiveSupportSvs = getAsmJavaName(ALIAS_ASM_INTENSIVE_SUPPORT, dataDictionary, true);
            Criteria criteria = new Criteria();
            criteria.addEqualTo(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID, asdLAVC.getOid());
            criteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE, getCurrentContext().getStartDate());
            criteria.addLessOrEqualThan(StudentAssessment.COL_DATE, getCurrentContext().getEndDate());
            criteria.addIn(StudentAttendance.COL_STUDENT_OID, studentsSubQuery);
            QueryByCriteria query = new QueryByCriteria(StudentAssessment.class, criteria);
            query.addOrderBy(StudentAssessment.COL_STUDENT_OID, true);
            query.addOrderBy(StudentAssessment.COL_DATE, false);
            m_asmLAVCMap = getBroker().getGroupedCollectionByQuery(query, StudentAssessment.COL_STUDENT_OID, 100);
        }
    }

    /**
     * Load maps for race codes reference codes and person race for students.
     *
     * @param studentCriteria Criteria
     */
    private void loadRaceCodes(Criteria studentCriteria) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);

        // Get race code reference codes for use in the race retriever.
        Criteria raceCriteria = new Criteria();
        raceCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, raceCriteria);
        m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 5);

        // Load the race codes for all students included in the export.
        SubQuery subQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, studentCriteria);
        raceCriteria = new Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID, subQuery);
        QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
        m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 100);
    }

    /**
     * Loads a list of school codes where the SRC alias override is set on the school.
     */
    private void loadSchoolOverrides() {
        m_schoolOverride = new ArrayList<String>();

        QueryByCriteria query = new QueryByCriteria(SisSchool.class);
        QueryIterator schools = getBroker().getIteratorByQuery(query);
        while (schools.hasNext()) {
            SisSchool school = (SisSchool) schools.next();
            if (BooleanAsStringConverter.TRUE
                    .equalsIgnoreCase((String) school.getFieldValueByAlias("DOE SRC OVERRIDE"))) {
                String key = (String) school.getFieldValueByAlias(ALIAS_SCHOOL_CODE);
                m_schoolOverride.add(key);
            }
        }
    }

    /**
     * Load a map of sets of student schedule records by student oid for the students in the
     * export.
     */
    private void loadStudentSchedules() {
        X2Criteria scheduleCriteria = new X2Criteria();

        // Master type Class
        scheduleCriteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE
                + PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);

        // From active Schedule
        scheduleCriteria.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL
                + PATH_DELIMITER + SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER
                + SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, StudentSchedule.COL_SCHEDULE_OID);

        // section term started before report date.
        scheduleCriteria.addLessOrEqualThan(
                StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER
                        + ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER + ScheduleTermDate.COL_START_DATE,
                m_reportDate);

        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, scheduleCriteria);
        // make sort order deterministic
        query.addOrderByAscending(StudentSchedule.COL_STUDENT_OID);
        query.addOrderByAscending(X2BaseBean.COL_OID);
        m_scheduleMap = getBroker().getGroupedCollectionByQuery(query, StudentSchedule.COL_STUDENT_OID, 500);
    }

    /**
     * Load a map of student transcripts for the students in the export.
     *
     * @param studentCriteria Criteria
     */
    private void loadTranscipts(Criteria studentCriteria) {
        SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        Criteria criteria = new Criteria();
        criteria.addIn(Transcript.COL_STUDENT_OID, studentsSubQuery);
        QueryByCriteria query = new QueryByCriteria(Transcript.class, criteria);
        query.addOrderBy(Transcript.COL_STUDENT_OID, true);

        m_transcriptMap = getBroker().getGroupedCollectionByQuery(query, Transcript.COL_STUDENT_OID, 100);
    }
}
