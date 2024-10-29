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
import com.follett.fsc.core.framework.persistence.CollectionCriteriaHelper;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.framework.persistence.adjusters.JoinAdjuster.JoinType;
import com.follett.fsc.core.k12.beans.DistrictCalendar;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.*;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanDefinition.JoinAdjusterPattern;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldValidator;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportValidationError;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
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
public class VAStudentRecordCollectionRevised extends ToolsSharedContainer.StateReportData {
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
    public static class StudentRecordEntity extends ToolsSharedContainer.StateReportEntity {
        @SuppressWarnings({"unused", "hiding"})
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";

        List<EnrollmentSpan> m_enrollmentLists = new ArrayList<EnrollmentSpan>();
        String m_gradeLevel = null;
        Boolean m_hasDistanceLearningFlag = Boolean.FALSE;
        Map<String, Object> m_overrideCodeMap = new HashMap<String, Object>();
        String m_overrideServiceDistrictCode = null;
        String m_overrideServiceSchoolCode = null;
        Map<String, MembershipAttendance> m_schoolMap = new HashMap<String, MembershipAttendance>();
        List<MembershipAttendance> m_schoolList = new ArrayList<MembershipAttendance>();
        Integer m_sort = null;


        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StudentRecordEntity() {
            // public no argument constructor for dynamic instantiation.
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
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            ToolStudent student = (ToolStudent) getBean();

            String name = student.getNameView() + " [LocalID: " + student.getLocalId() + " StateID: "
                    + student.getStateId() + "]";
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
        public String getStatus(MembershipAttendance memAtt, VAStudentRecordCollectionRevised data) {
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
                entryStatus = attendance.getEntryEnrollment().getValueReferenceState(ToolEnrollment.FIELD_STATUS_CODE);
            }

            // Find the end date, from the enrollment withdrawal or from report
            // date.
            if (attendance.getExitEnrollment() != null) {
                endDate = attendance.getExitEnrollment().getEnrollmentDate();
                if (StudentEnrollment.STATUS_CHANGE.equals(attendance.getExitEnrollment().getEnrollmentType())) {
                    String hbEntryCode = attendance.getExitEnrollment().getHbEntryCode();
                    if (VAStudentRecordCollectionRevised.HB_ENTRY_CODES.contains(hbEntryCode)) {
                        endDate = DateUtils.add(endDate, -1);
                    }
                }
                endStatus = attendance.getExitEnrollment().getValueReferenceState(ToolEnrollment.FIELD_STATUS_CODE);
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
        public void intitialize(ToolsSharedContainer.StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            VAStudentRecordCollectionRevised srcData = (VAStudentRecordCollectionRevised) data;
            VAToolStudent student = (VAToolStudent) bean;
            m_gradeLevel = getFieldValue(FIELD_GRADE_LEVEL);

            List<VAToolEnrollment> allEnrollments = student.getVAEnrollments(data.getBroker());
            String enrollCode = allEnrollments.stream().findFirst()
                    .map(ToolBean.ToolEnrollment::getStatusCode)
                    .orElse(student.getEnrollmentStatus());


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
                VAToolSchool school = span.getSchool();
                VAToolEnrollment entryEnrollment = span.getFirstEntry();
                boolean schoolExcluded = school == null || school.isExcluded();
                boolean entryExcluded = entryEnrollment != null && entryEnrollment.excludeReporting();

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
                    ToolSchool school = span.getSchool();
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
                        ToolSchool school = span.getSchool();
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
            if (((VAStudentRecordCollectionRevised) data).m_sort.intValue() == 3) {
                TreeMap<String, Collection<MembershipAttendance>> statusMap =
                        new TreeMap<String, Collection<MembershipAttendance>>();
                for (MembershipAttendance att : m_schoolList) {
                    String status = getStatus(att, ((VAStudentRecordCollectionRevised) data));
                    if (!StringUtils.isEmpty(status)) {
                        Collection attCollection = statusMap.get(status);
                        if (attCollection == null) {
                            attCollection = new ArrayList<MembershipAttendance>();
                            statusMap.put(status, attCollection);
                        }
                        attCollection.add(att);
                    }
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

            Collection<VAToolStudentSchedule> schedules = student.getStudentSchedules(data.getBroker())
                    .stream()
                    .map(ssc -> (VAToolStudentSchedule) ssc)
                    .filter(ssc -> ssc.isCurrent(srcData.getBroker(), srcData.m_reportDate))
                    .sorted(new Comparator<ToolStudentSchedule>() {

                        @Override
                        public int compare(ToolStudentSchedule ssc1, ToolStudentSchedule ssc2) {
                            return ssc1.getOid().toUpperCase().compareTo(ssc2.getOid().toUpperCase());
                        }
                    })
                    .collect(Collectors.toList());
            if (!schedules.isEmpty()) {
                for (ToolStudentSchedule schedule : schedules) {
                    VAToolSection section = (VAToolSection) schedule.getSection(data.getBroker());
                    if (section != null) {
                        if (section.getVirtualCourseType() != null) {
                            VAToolCourse course = (VAToolCourse) section.getCourse(data.getBroker());
                            if (course != null && (course.getCourseCredit() != null)
                                    && (course.getCourseCredit().compareTo(BigDecimal.ZERO) > 0)) {
                                m_hasDistanceLearningFlag = Boolean.TRUE;
                            }
                        }

                        if (section.getCareerAndTechEd() || section.getGovenorsSchool()) {
                            ToolStaff primaryStaff = section.getPrimaryStaff(data.getBroker());
                            if ((primaryStaff != null) && !StringUtils.isEmpty(primaryStaff.getLocalId())) {
                                String localId = primaryStaff.getLocalId();
                                String servingDistrictCode = localId.substring(0, 3);
                                setOverrideCodeMap(VAToolEnrollment.ALIAS_SERVICE_DISTRICT_CODE,
                                        servingDistrictCode);

                                String serviceSchoolCode = localId.substring(3, localId.length());
                                setOverrideCodeMap(VAToolEnrollment.ALIAS_SERVICE_SCHOOL_CODE, serviceSchoolCode);
                            }
                        }
                    }
                }
            }
            setRowCount(m_schoolList.size());
            srcData.m_rowCount += m_schoolList.size();
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
         * Add an enrollment span to the schools list.
         *
         * @param span EnrollmentSpan
         */
        private void addMembership(EnrollmentSpan span) {
            VAStudentRecordCollectionRevised srcData = (VAStudentRecordCollectionRevised) getData();
            ToolStudent student = (ToolStudent) getBean();
            VAToolSchool school = span.getSchool();
            if (school != null) {
                // Find the begin date, from the enrollment entry or from school
                // year start.
                PlainDate beginDate = null;
                PlainDate schedDate = null;
                if (span.getFirstEntry() != null) {
                    beginDate = span.getFirstEntry().getEnrollmentDate();
                }
                ToolBean.ToolSchedule activeSchedule = school.getActiveSchedule(srcData.m_broker);
                if (activeSchedule != null) {
                    schedDate = activeSchedule.getStartDate();
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
                        String hbEntryCode = span.getFirstWithdrawal().getHbEntryCode();
                        if (VAStudentRecordCollectionRevised.HB_ENTRY_CODES.contains(hbEntryCode)) {
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
                    String enrSS = span.getFirstEntry().getServiceSchoolCode();
                    if (!StringUtils.isEmpty(enrSS)) {
                        servingSchool =
                                srcData.getDictionaryExtractor()
                                        .lookupStateValue(VAToolEnrollment.FIELD_SERVICE_SCHOOL_CODE, enrSS);
                    } else {
                        servingSchool = span.getSchool().getCode();
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
                        withdrawalStatus =
                                span.getFirstWithdrawal().getValueReferenceState(ToolEnrollment.FIELD_STATUS_CODE);
                    }
                    if (span.getFirstWithdrawal() == null || !STATUS_NO_SHOW.equals(withdrawalStatus)) {
                        memberDays = student.getMembershipTotal(srcData.getBroker(), calendarDays, true,
                                Range.of(beginDate, endDate), school);
                    }
                    List<ToolStudentAttendance> absences = student.getAttendance(srcData.getBroker());
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
                            for (ToolStudentAttendance attendance : absences) {
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
        private void groupEnrollments(List<VAToolEnrollment> enrollments,
                                      boolean isOpen,
                                      String currentSchoolOid) {
            VAStudentRecordCollectionRevised data = (VAStudentRecordCollectionRevised) getData();
            List<VAToolEnrollment> currentEnrollments = new ArrayList<VAToolEnrollment>();
            boolean containedHb = false;
            boolean presub = COLLECTION_PRESUB.equals(data.m_collection);
            int inactiveCount = 0;
            if (enrollments != null) {
                for (VAToolEnrollment enrollment : enrollments) {
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
                            currentEnrollments = new ArrayList<VAToolEnrollment>();
                            isOpen = true;
                            currentSchoolOid = enrollment.getSchoolOid();
                        }

                        // If its a status change and a HB Entry, close it
                        if (StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType())) {
                            String hbEntryCode = enrollment.getHbEntryCode();
                            if (VAStudentRecordCollectionRevised.HB_ENTRY_CODES.contains(hbEntryCode)) {
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
                                currentEnrollments = new ArrayList<VAToolEnrollment>();
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
                                currentEnrollments = new ArrayList<VAToolEnrollment>();
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

        /**
         * Checks if is summer tuition reimbursment.
         *
         * @param enrollment StudentEnrollment
         * @return true, if is summer tuition reimbursment
         */
        private boolean isSummerTuitionReimbursment(VAToolEnrollment enrollment) {
            String tuitionValue = enrollment.getSpedTuitionSum();
            return !StringUtils.isEmpty(tuitionValue);
        }
    }

    /**
     * The Class VAStudentAssessment.
     */
    public static class VAStudentAssessment extends ToolBean {
        private static final String ALIAS_ASM_CREDIT_ACC_CODE = "all-asm-CreditAccomodation";
        private static final String ALIAS_ASM_INTENSIVE_SUPPORT = "all-asm-IntensiveSupportSvs";

        private static final String ASD_ID_LAVC = "LAVC";

        public static final ToolBeanColumn FIELD_ASM_CREDIT_ACC_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT, ALIAS_ASM_CREDIT_ACC_CODE, ASD_ID_LAVC);
        public static final ToolBeanColumn FIELD_ASM_INTENSIVE_SUPPORT =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT, ALIAS_ASM_INTENSIVE_SUPPORT, ASD_ID_LAVC);
        public static final ToolBeanColumn FIELD_ASSESSMENT_ID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT.assessmentDefinition().id());
        public static final ToolBeanColumn FIELD_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT.date(), false);
        public static final ToolBeanColumn FIELD_STUDENT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT.studentOid());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION =
                ToolBean.FULL_DEFINITION
                        .expand(FIELD_ASSESSMENT_ID,
                                FIELD_DATE,
                                FIELD_ASM_CREDIT_ACC_CODE,
                                FIELD_ASM_INTENSIVE_SUPPORT,
                                FIELD_STUDENT_OID)
                        .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                            @Override
                            public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                                criteria.addEqualTo(FIELD_ASSESSMENT_ID.resolve(getDictionaryExtractor()), ASD_ID_LAVC);
                                return criteria;
                            }
                        });

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT_ASSESSMENT.getBeanType();
        }

        /**
         * Instantiates a new onsis student assessment.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public VAStudentAssessment(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the credit accomodation.
         *
         * @return String
         */
        public String getCreditAccomodation() {
            return getValueReferenceState(FIELD_ASM_CREDIT_ACC_CODE);
        }

        /**
         * Gets the date.
         *
         * @return Plain date
         */
        public PlainDate getDate() {
            return getValueDate(FIELD_DATE);
        }

        /**
         * Gets the intensive support svs.
         *
         * @return String
         */
        public String getIntensiveSupportSvs() {
            return getValueReferenceState(FIELD_ASM_INTENSIVE_SUPPORT);
        }

        /**
         * Gets the student oid.
         *
         * @return String
         */
        public String getStudentOid() {
            return getValueString(FIELD_STUDENT_OID);
        }
    }

    public static class VAToolConductAction extends ToolConductAction {
        private static final String ALIAS_RESTRAINT = "all-act-Restraint";
        private static final String ALIAS_SECLUSION = "all-act-Seclusion";

        public static final ToolBeanColumn FIELD_RESTRAINT =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_ACTION,
                        new ToolBeanColumn.AliasDefinition(ALIAS_RESTRAINT, null, false));

        public static final ToolBeanColumn FIELD_SECLUSION =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_ACTION,
                        new ToolBeanColumn.AliasDefinition(ALIAS_SECLUSION, null, false));

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolConductAction.FULL_DEFINITION
                .expand(FIELD_RESTRAINT,
                        FIELD_SECLUSION);

        public VAToolConductAction(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        public boolean isRestraint() {
            return getValueLogical(FIELD_RESTRAINT);
        }

        public boolean isSeclusion() {
            return getValueLogical(FIELD_SECLUSION);
        }
    }

    /**
     * The Class VAToolCourse.
     */
    public static class VAToolCourse extends ToolCourse {
        public static final ToolBeanColumn FIELD_CAREER_AND_TECH_ED =
                new ToolBeanColumn(SisBeanPaths.COURSE,
                        new ToolBeanColumn.AliasDefinition(ALIAS_CAREER_AND_TECH_ED, null, false));

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolCourse.FULL_DEFINITION
                .expand(FIELD_CAREER_AND_TECH_ED);

        /**
         * Instantiates a new VA tool course.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public VAToolCourse(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the career and tech ed.
         *
         * @return boolean
         */
        public boolean getCareerAndTechEd() {
            return getValueLogical(FIELD_CAREER_AND_TECH_ED);
        }

    }

    public static class VAToolStudentSchedule extends ToolStudentSchedule {
        public static final ToolBeanColumn FIELD_SCHEDULE_SKX_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE.schedule().activeSchoolScheduleContexts().oid());
        public static final ToolBeanColumn FIELD_SCHOOL_ACTIVE_SKX_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE.schedule().school().activeSchoolSchedOid());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStudentSchedule.FULL_DEFINITION
                .expand(FIELD_SCHEDULE_SKX_OID,
                        FIELD_SCHOOL_ACTIVE_SKX_OID);

        /**
         * @param columns
         * @param data
         */
        public VAToolStudentSchedule(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        public boolean isCurrent(X2Broker broker, PlainDate reportDate) {
            if (getValueString(FIELD_SCHEDULE_SKX_OID).equals(getValueString(FIELD_SCHOOL_ACTIVE_SKX_OID))) {
                PlainDate startDate = getSection(broker).getStartDate(broker, Range.of(null, null));
                return startDate != null && !startDate.after(reportDate);
            }
            return false;
        }

    }


    /**
     * The Class VAToolEnrollment.
     */
    public static class VAToolEnrollment extends ToolBean.ToolEnrollment {
        private static final String ALIAS_ENR_ATT_CONF = "all-enr-AttendanceConferenceCode";
        private static final String ALIAS_ENR_ATT_PLAN = "all-enr-AttendancePlanCode";
        private static final String ALIAS_ENR_COURT_REF = "all-enr-CourtReferralComplaintProceedingsCode";
        private static final String ALIAS_EXCLUDE_REPORTING_ENR = "DOE EXCLUDE ENR";
        private static final String ALIAS_HB_ENTRY_CODE = "DOE HB ENTRY CODE";
        private static final String ALIAS_HB_WITHDRAW_CODE = "DOE HB WITHDRAW CODE";
        private static final String ALIAS_HOME_DISTRICT_CODE = "DOE DISTRICT HOME";
        private static final String ALIAS_HOME_SCHOOL_CODE = "DOE SCHOOL HOME";
        private static final String ALIAS_SERVICE_DISTRICT_CODE = "DOE DISTRICT SERVE";
        private static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVE";
        private static final String ALIAS_SPED_TUITION_DISABILITY = "DOE SPED TUIT DISABILITY";
        private static final String ALIAS_SPED_TUITION_S1 = "DOE SPED TUITION S1";
        private static final String ALIAS_SPED_TUITION_S2 = "DOE SPED TUITION S2";
        private static final String ALIAS_SPED_TUITION_SUM = "DOE SPED TUITION SUM";
        private static final String ALIAS_VIRTUAL_PROGRAM = "DOE VIRTUAL PROGRAM";

        public static final ToolBeanColumn FIELD_ENR_ATT_CONF =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_ENR_ATT_CONF);
        public static final ToolBeanColumn FIELD_ENR_ATT_PLAN =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_ENR_ATT_PLAN);
        public static final ToolBeanColumn FIELD_ENR_COURT_REF =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_ENR_COURT_REF);
        public static final ToolBeanColumn FIELD_EXCLUDE_REPORTING_ENR =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_EXCLUDE_REPORTING_ENR);
        public static final ToolBeanColumn FIELD_HB_ENTRY_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_HB_ENTRY_CODE);
        public static final ToolBeanColumn FIELD_HB_WITHDRAW_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_HB_WITHDRAW_CODE);
        public static final ToolBeanColumn FIELD_HOME_DISTRICT_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_HOME_DISTRICT_CODE);
        public static final ToolBeanColumn FIELD_HOME_SCHOOL_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_HOME_SCHOOL_CODE);
        public static final ToolBeanColumn FIELD_SERVICE_DISTRICT_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_SERVICE_DISTRICT_CODE);
        public static final ToolBeanColumn FIELD_SERVICE_SCHOOL_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_SERVICE_SCHOOL_CODE);
        public static final ToolBeanColumn FIELD_SPED_TUITION_DISABILITY =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_SPED_TUITION_DISABILITY);
        public static final ToolBeanColumn FIELD_SPED_TUITION_S1 =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_SPED_TUITION_S1);
        public static final ToolBeanColumn FIELD_SPED_TUITION_S2 =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_SPED_TUITION_S2);
        public static final ToolBeanColumn FIELD_SPED_TUITION_SUM =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_SPED_TUITION_SUM);
        public static final ToolBeanColumn FIELD_VIRTUAL_PROGRAM =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_VIRTUAL_PROGRAM);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolEnrollment.FULL_DEFINITION
                .expand(FIELD_HOME_DISTRICT_CODE,
                        FIELD_EXCLUDE_REPORTING_ENR,
                        FIELD_SERVICE_DISTRICT_CODE,
                        FIELD_SERVICE_SCHOOL_CODE,
                        FIELD_HOME_SCHOOL_CODE,
                        FIELD_SPED_TUITION_DISABILITY,
                        FIELD_SPED_TUITION_SUM,
                        FIELD_SPED_TUITION_S1,
                        FIELD_SPED_TUITION_S2,
                        FIELD_VIRTUAL_PROGRAM,
                        FIELD_ENR_ATT_CONF,
                        FIELD_HB_ENTRY_CODE,
                        FIELD_ENR_ATT_PLAN,
                        FIELD_ENR_COURT_REF,
                        FIELD_HB_WITHDRAW_CODE);

        /**
         * Instantiates a new VA tool enrollment.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public VAToolEnrollment(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Exclude reporting.
         *
         * @return true, if successful
         */
        public boolean excludeReporting() {
            return getValueLogical(FIELD_EXCLUDE_REPORTING_ENR);
        }

        /**
         * Gets the att conf.
         *
         * @return String
         */
        public String getAttConf() {
            return getValueString(FIELD_ENR_ATT_CONF);
        }

        /**
         * Gets the att plan.
         *
         * @return String
         */
        public String getAttPlan() {
            return getValueString(FIELD_ENR_ATT_PLAN);
        }

        /**
         * Gets the court ref.
         *
         * @return String
         */
        public String getCourtRef() {
            return getValueString(FIELD_ENR_COURT_REF);
        }

        /**
         * Gets the hb entry code.
         *
         * @return String
         */
        public String getHbEntryCode() {
            return getValueString(FIELD_HB_ENTRY_CODE);
        }

        /**
         * Gets the hb withdrawal code.
         *
         * @return String
         */
        public String getHbWithdrawalCode() {
            return getValueString(FIELD_HB_WITHDRAW_CODE);
        }

        /**
         * Gets the home district code.
         *
         * @return String
         */
        public String getHomeDistrictCode() {
            return getValueString(FIELD_HOME_DISTRICT_CODE);
        }

        /**
         * Gets the home school code.
         *
         * @return String
         */
        public String getHomeSchoolCode() {
            return getValueString(FIELD_HOME_SCHOOL_CODE);
        }

        /**
         * Gets the service district code.
         *
         * @return String
         */
        public String getServiceDistrictCode() {
            return getValueString(FIELD_SERVICE_DISTRICT_CODE);
        }

        /**
         * Gets the service school code.
         *
         * @return String
         */
        public String getServiceSchoolCode() {
            return getValueString(FIELD_SERVICE_SCHOOL_CODE);
        }

        /**
         * Gets the sped tuition disability.
         *
         * @return String
         */
        public String getSpedTuitionDisability() {
            return getValueString(FIELD_SPED_TUITION_DISABILITY);
        }

        /**
         * Gets the sped tuition S 1.
         *
         * @return String
         */
        public String getSpedTuitionS1() {
            return getValueString(FIELD_SPED_TUITION_S1);
        }

        /**
         * Gets the sped tuition S 2.
         *
         * @return String
         */
        public String getSpedTuitionS2() {
            return getValueString(FIELD_SPED_TUITION_S2);
        }

        /**
         * Gets the sped tuition sum.
         *
         * @return String
         */
        public String getSpedTuitionSum() {
            return getValueString(FIELD_SPED_TUITION_SUM);
        }

        /**
         * Gets the virtual program.
         *
         * @return String
         */
        public String getVirtualProgram() {
            return getValueString(FIELD_VIRTUAL_PROGRAM);
        }
    }

    /**
     * The Class VAToolSchool.
     */
    public static class VAToolSchool extends ToolSchool {
        private static final String ALIAS_CODE = "DOE SCHOOL ID";
        private static final String ALIAS_DOE_SRC_OVERRIDE = "DOE SRC OVERRIDE";
        private static final String ALIAS_EXCLUDE = "DOE EXCLUDE SKL";
        private static final String ALIAS_TUITION_PAID = "DOE TUITION PAID";

        public static final ToolBeanColumn FIELD_CODE = new ToolBeanColumn(SisBeanPaths.SCHOOL, ALIAS_CODE);
        public static final ToolBeanColumn FIELD_DOE_SRC_OVERRIDE =
                new ToolBeanColumn(SisBeanPaths.SCHOOL, ALIAS_DOE_SRC_OVERRIDE);
        public static final ToolBeanColumn FIELD_EXCLUDE = new ToolBeanColumn(SisBeanPaths.SCHOOL, ALIAS_EXCLUDE);
        public static final ToolBeanColumn FIELD_TUITION_PAID =
                new ToolBeanColumn(SisBeanPaths.SCHOOL, ALIAS_TUITION_PAID);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolSchool.FULL_DEFINITION
                .expand(FIELD_CODE,
                        FIELD_DOE_SRC_OVERRIDE,
                        FIELD_EXCLUDE,
                        FIELD_TUITION_PAID);

        /**
         * Instantiates a new VA tool school.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public VAToolSchool(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the code.
         *
         * @return String
         */
        public String getCode() {
            return getValueString(FIELD_CODE);
        }

        /**
         * Gets the doe src override indicator.
         *
         * @return boolean
         */
        public boolean getDoeSrcOverrideIndicator() {
            return getValueLogical(FIELD_DOE_SRC_OVERRIDE);
        }

        /**
         * Gets the tuition paid.
         *
         * @return String
         */
        public String getTuitionPaid() {
            return getValueString(FIELD_TUITION_PAID);
        }

        /**
         * Checks if is excluded.
         *
         * @return true, if is excluded
         */
        public boolean isExcluded() {
            return getValueLogical(FIELD_EXCLUDE);
        }
    }

    /**
     * The Class VAToolStudentProgramParticipation.
     */
    public static class VAToolStudentProgramParticipation extends ToolStudentProgramParticipation {

        private static final String ALIAS_PGM_CODE = "all-pgm-ELCode";

        private static final String ALIAS_PGM_DEVICE_ACCESS_REMOTE_LEARNING = "all-pgm-DeviceAccessforRemoteLearning";
        private static final String ALIAS_PGM_INTERNET_ACCESS_REMOTE_LEARNING =
                "all-pgm-InternetAccessforRemoteLearning";
        private static final String ALIAS_PGM_LIEP = "all-pgm-LanguageInstructionEducationalProgram";
        private static final String ALIAS_PGM_NIGHT_RESIDENCE = "all-pgm-InitialPrimaryNightimeResidence";
        private static final String ALIAS_PGM_PARENTAL_REMOTE_LEARNING_DECISION =
                "all-pgm-ParentalRemoteLearningDecision";
        private static final String ALIAS_PGM_REGIONAL_LOCAL_CENTER_PERCENT_OF_TIME =
                "all-pgm-RegionalLocalCenterPercentofTime";
        private static final String ALIAS_PGM_REMOTE_INST_PERCENT_OF_TIME = "all-pgm-RemoteInstructionPercentofTime";
        private static final String ALIAS_PGM_SLIFE_STATUS = "all-pgm-SLIFEStatus";
        private static final String ALIAS_PGM_UNACCOMPANIED = "all-pgm-UnaccompaniedHomelessYouth";

        private static final ToolBeanColumn FIELD_PGM_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION, ALIAS_PGM_CODE);
        private static final ToolBeanColumn FIELD_PGM_DEVICE_ACCESS_REMOTE_LEARNING =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION, ALIAS_PGM_DEVICE_ACCESS_REMOTE_LEARNING);
        private static final ToolBeanColumn FIELD_PGM_INTERNET_ACCESS_REMOTE_LEARNING =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION,
                        ALIAS_PGM_INTERNET_ACCESS_REMOTE_LEARNING);
        private static final ToolBeanColumn FIELD_PGM_LIEP =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION, ALIAS_PGM_LIEP);
        private static final ToolBeanColumn FIELD_PGM_NIGHT_RESIDENCE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION, ALIAS_PGM_NIGHT_RESIDENCE);
        private static final ToolBeanColumn FIELD_PGM_PARENTAL_REMOTE_LEARNING_DECISION =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION,
                        ALIAS_PGM_PARENTAL_REMOTE_LEARNING_DECISION);
        private static final ToolBeanColumn FIELD_PGM_REGIONAL_LOCAL_CENTER_PERCENT_OF_TIME =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION,
                        ALIAS_PGM_REGIONAL_LOCAL_CENTER_PERCENT_OF_TIME);
        private static final ToolBeanColumn FIELD_PGM_REMOTE_INST_PERCENT_OF_TIME =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION, ALIAS_PGM_REMOTE_INST_PERCENT_OF_TIME);
        private static final ToolBeanColumn FIELD_PGM_SLIFE_STATUS =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION, ALIAS_PGM_SLIFE_STATUS);
        private static final ToolBeanColumn FIELD_PGM_UNACCOMPANIED =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION, ALIAS_PGM_UNACCOMPANIED);
        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStudentProgramParticipation.FULL_DEFINITION
                .expand(FIELD_PGM_CODE,
                        FIELD_PGM_DEVICE_ACCESS_REMOTE_LEARNING,
                        FIELD_PGM_INTERNET_ACCESS_REMOTE_LEARNING,
                        FIELD_PGM_LIEP,
                        FIELD_PGM_NIGHT_RESIDENCE,
                        FIELD_PGM_PARENTAL_REMOTE_LEARNING_DECISION,
                        FIELD_PGM_REGIONAL_LOCAL_CENTER_PERCENT_OF_TIME,
                        FIELD_PGM_REMOTE_INST_PERCENT_OF_TIME,
                        FIELD_PGM_SLIFE_STATUS,
                        FIELD_PGM_UNACCOMPANIED);

        /**
         * Instantiates a new VA tool student program participation.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public VAToolStudentProgramParticipation(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the device accessfor remote learning.
         *
         * @return String
         */
        public String getDeviceAccessforRemoteLearning() {
            return getValueReferenceState(FIELD_PGM_DEVICE_ACCESS_REMOTE_LEARNING);
        }

        /**
         * Gets the internet accessfor remote learning.
         *
         * @return String
         */
        public String getInternetAccessforRemoteLearning() {
            return getValueReferenceState(FIELD_PGM_INTERNET_ACCESS_REMOTE_LEARNING);
        }

        /**
         * Gets the liep.
         *
         * @return String
         */
        public String getLiep() {
            return getValueReferenceState(FIELD_PGM_LIEP);
        }

        /**
         * Gets the night residence.
         *
         * @return String
         */
        public String getNightResidence() {
            return getValueReferenceState(FIELD_PGM_NIGHT_RESIDENCE);
        }

        /**
         * Gets the parental remote learning decision.
         *
         * @return boolean
         */
        public boolean getParentalRemoteLearningDecision() {
            return getValueLogical(FIELD_PGM_PARENTAL_REMOTE_LEARNING_DECISION);
        }

        /**
         * Gets the regional local center percentof time.
         *
         * @return String
         */
        public String getRegionalLocalCenterPercentofTime() {
            return getValueString(FIELD_PGM_REGIONAL_LOCAL_CENTER_PERCENT_OF_TIME);
        }

        /**
         * Gets the remote instruction percentof time.
         *
         * @return String
         */
        public String getRemoteInstructionPercentofTime() {
            return getValueString(FIELD_PGM_REMOTE_INST_PERCENT_OF_TIME);
        }

        /**
         * Gets the slife status.
         *
         * @return String
         */
        public String getSlifeStatus() {
            return getValueString(FIELD_PGM_SLIFE_STATUS);
        }

        /**
         * Gets the state code.
         *
         * @return String
         */
        public String getStateCode() {
            return getValueReferenceState(FIELD_PGM_CODE);
        }

        /**
         * Checks if is unaccompanied.
         *
         * @return true, if is unaccompanied
         */
        public boolean isUnaccompanied() {
            return getValueLogical(FIELD_PGM_UNACCOMPANIED);
        }
    }

    /**
     * The Class VAToolStudent.
     */
    public static class VAToolStudent extends ToolStudent {

        private static final String ALIAS_AP_CODE = "DOE AP CODE";
        private static final String ALIAS_ASSOCIATES = "DOE ASSOCIATES";
        private static final String ALIAS_ATT_CONF = "all-std-AttendanceConferenceCode";
        private static final String ALIAS_ATT_PLAN = "all-std-AttendancePlanCode";
        private static final String ALIAS_BIRTH_COUNTRY = "DOE BIRTH COUNTRY";
        private static final String ALIAS_CAMBRIDGE_PROG = "DOE CAMBRIDGE PROG";
        private static final String ALIAS_CAREER_CLUSTER = "all-std-CTESRCCareerClusterOveride";
        private static final String ALIAS_COURT_REF = "all-std-CourtReferralComplaintProceedingsCode";
        private static final String ALIAS_CTE_ATTAINMENT = "DOE CTE ATTAINMENT";
        private static final String ALIAS_CTE_POPULATION = "DOE CTE POPULATION";
        private static final String ALIAS_CTE_FINISHER = "DOE CTE FINISHER";
        private static final String ALIAS_CTE_PATHWAY = "DOE CTE PATHWAY";
        private static final String ALIAS_DIPLOMA = "DOE DIPLOMA";
        private static final String ALIAS_DISADV_STATUS = "DOE DISADV STATUS";
        private static final String ALIAS_EARLY_SCHOLARSHIP = "DOE EARLY SCHOLARSHIP";
        private static final String ALIAS_EC_SPED = "DOE EC SPED";
        private static final String ALIAS_EC_TIME = "DOE EC TIME";
        private static final String ALIAS_ESL_CODE = "DOE ESL SERVICE";
        private static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
        private static final String ALIAS_GED = "DOE GED";
        private static final String ALIAS_GIFTED = "DOE GIFTED";
        private static final String ALIAS_GIFTED_REFERRAL = "DOE GIFTED REFERRAL";
        private static final String ALIAS_GRAD_CODE = "DOE GRAD CODE";
        private static final String ALIAS_GRAD_PLAN = "DOE GRAD PLAN";
        private static final String ALIAS_GRADE_LEVEL_OVERRIDE = "DOE GRADE LEVEL OVERRIDE";
        private static final String ALIAS_HEAD_START_PROVIDER = "all-std-HeadStartProviderCode";
        private static final String ALIAS_IB_CODE = "DOE IB CODE";
        private static final String ALIAS_IB_FLAG = "DOE IB PROGRAM";
        private static final String ALIAS_IMMIGRANT_STATUS = "DOE IMMIGRANT";
        private static final String ALIAS_K_READINESS = "DOE K READINESS";
        private static final String ALIAS_MILITARY_COMPACT = "DOE MILITARY COMPACT";
        private static final String ALIAS_MILITARY_CONNECTED = "DOE MILITARY CONNECTED";
        private static final String ALIAS_MOP_FLAG = "DOE MOP FLAG";
        private static final String ALIAS_NIGHT_RESIDENCE = "DOE NIGHT RESIDENT";
        private static final String ALIAS_NONPUB_FTE = "DOE NONPUB FTE";
        private static final String ALIAS_PARENT_PLACED = "DOE PARENT PLACED";
        private static final String ALIAS_PK_EXPERIENCE = "DOE PK EXPERIENCE";
        private static final String ALIAS_PK_FUNDING = "DOE PK FUNDING";
        private static final String ALIAS_PK_WEEKLY_TIME = "DOE PK WEEKLY TIME";
        private static final String ALIAS_RESIDENT_DIV = "DOE RESIDENT DIV";
        private static final String ALIAS_SASID = "DOE SASID";
        private static final String ALIAS_SPED_DISABILITY = "DOE SPED DISABILITY";
        private static final String ALIAS_SPED_DIV_2 = "DOE SPED DIV 2";
        private static final String ALIAS_SPED_DIV_3 = "DOE SPED DIV 3";
        private static final String ALIAS_SPED_PLACEMENT = "DOE SPED PLACEMENT";
        private static final String ALIAS_SPED_REG_PCT = "DOE SPED REG PCT";
        private static final String ALIAS_SPED_SKL_2 = "DOE SPED SKL 2";
        private static final String ALIAS_SPED_SKL_3 = "DOE SPED SKL 3";
        private static final String ALIAS_SPED_SVC_PCT = "DOE SPED SVC PCT";
        private static final String ALIAS_SPED_SVC_PCT_2 = "DOE SPED SVC PCT 2";
        private static final String ALIAS_SPED_SVC_PCT_3 = "DOE SPED SVC PCT 3";
        private static final String ALIAS_SPED_TIME_PCT = "DOE SPED TIME PCT";
        private static final String ALIAS_STD_HOME_SCHOOL_CODE = "DOE STD SCHOOL HOME";
        private static final String ALIAS_TITLE_I = "DOE TITLE I";
        private static final String ALIAS_TUITION = "DOE STUDENT TUITION";
        private static final String ALIAS_UNACCOMPANIED = "DOE UY";
        private static final String ALIAS_US_END_DATE = "DOE US END DATE";
        private static final String ALIAS_US_ENTRY_DATE = "DOE US ENTRY DATE";
        private static final String ALIAS_SPED_DISABILITY_2 = "DOE SPED DISABILITY 2";
        private static final String ALIAS_SPED_DISABILITY_3 = "DOE SPED DISABILITY 3";
        private static final String CHILD_KEY_STUDENT_ASSESSMENT = "studentAssessments";
        private static final String CHILD_KEY_STUDENT_PROGRAMS = "studentPrograms";

        public static final ToolBeanColumn FIELD_AP_CODE = new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_AP_CODE);
        public static final ToolBeanColumn FIELD_ASSOCIATES =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_ASSOCIATES);
        public static final ToolBeanColumn FIELD_ATT_CONF =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_ATT_CONF);
        public static final ToolBeanColumn FIELD_ATT_PLAN =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_ATT_PLAN);
        public static final ToolBeanColumn FIELD_BIRTH_COUNTRY =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_BIRTH_COUNTRY);
        public static final ToolBeanColumn FIELD_CAREER_CLUSTER =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_CAREER_CLUSTER);
        public static final ToolBeanColumn FIELD_CAMBRIDGE_PROG =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_CAMBRIDGE_PROG);
        public static final ToolBeanColumn FIELD_COURT_REF =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_COURT_REF);
        public static final ToolBeanColumn FIELD_CTE_ATTAINMENT =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_CTE_ATTAINMENT);
        public static final ToolBeanColumn FIELD_CTE_FINISHER =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_CTE_FINISHER);
        public static final ToolBeanColumn FIELD_CTE_PATHWAY =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_CTE_PATHWAY);
        public static final ToolBeanColumn FIELD_CTE_POPULATION =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_CTE_POPULATION);
        public static final ToolBeanColumn FIELD_DIPLOMA = new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_DIPLOMA);
        public static final ToolBeanColumn FIELD_DISADV_STATUS =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_DISADV_STATUS);
        public static final ToolBeanColumn FIELD_EARLY_SCHOLARSHIP =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_EARLY_SCHOLARSHIP);
        public static final ToolBeanColumn FIELD_EC_TIME = new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_EC_TIME);
        public static final ToolBeanColumn FIELD_EC_SPED = new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_EC_SPED);
        public static final ToolBeanColumn FIELD_ESL_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_ESL_CODE);
        public static final ToolBeanColumn FIELD_EXCLUDE =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_EXCLUDE_STD);
        public static final ToolBeanColumn FIELD_GED =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_GED);
        public static final ToolBeanColumn FIELD_GENDER_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().genderCode());
        public static final ToolBeanColumn FIELD_GRAD_CODE = new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_GRAD_CODE);
        public static final ToolBeanColumn FIELD_GRAD_PLAN =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_GRAD_PLAN);
        public static final ToolBeanColumn FIELD_GRADE_LEVEL_OVERRIDE =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_GRADE_LEVEL_OVERRIDE);
        public static final ToolBeanColumn FIELD_GIFTED = new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_GIFTED);
        public static final ToolBeanColumn FIELD_GIFTED_REFERRAL =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_GIFTED_REFERRAL);
        public static final ToolBeanColumn FIELD_HISPANIC_LATINO_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().hispanicLatinoIndicator());
        public static final ToolBeanColumn FIELD_HOME_LANGUAGE_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT.homeLanguageCode());
        public static final ToolBeanColumn FIELD_HOME_SCHOOL_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_HOME_SCHOOL_CODE);
        public static final ToolBeanColumn FIELD_IB_CODE = new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_IB_CODE);
        public static final ToolBeanColumn FIELD_IB_FLAG = new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_IB_FLAG);
        public static final ToolBeanColumn FIELD_IMMIGRANT_STATUS =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_IMMIGRANT_STATUS);
        public static final ToolBeanColumn FIELD_K_READINESS =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_K_READINESS);
        public static final ToolBeanColumn FIELD_MILITARY_COMPACT =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_MILITARY_COMPACT);
        public static final ToolBeanColumn FIELD_MILITARY_CONNECTED =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_MILITARY_CONNECTED);
        public static final ToolBeanColumn FIELD_NONPUB_FTE =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_NONPUB_FTE);
        public static final ToolBeanColumn FIELD_PARENT_PLACED =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_PARENT_PLACED);
        public static final ToolBeanColumn FIELD_PERSON_PHONE_01 =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().phone01());
        public static final ToolBeanColumn FIELD_PHYSICAL_ADDRESS_LINE_01 =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().physicalAddress().addressLine01());
        public static final ToolBeanColumn FIELD_PHYSICAL_ADDRESS_LINE_02 =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().physicalAddress().addressLine02());
        public static final ToolBeanColumn FIELD_PHYSICAL_ADDRESS_POSTAL_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().physicalAddress().postalCode());
        public static final ToolBeanColumn FIELD_PK_EXPERIENCE =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_PK_EXPERIENCE);
        public static final ToolBeanColumn FIELD_PK_FUNDING =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_PK_FUNDING);
        public static final ToolBeanColumn FIELD_PK_WEEKLY_TIME =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_PK_WEEKLY_TIME);
        public static final ToolBeanColumn FIELD_RESIDENT_DIV =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_RESIDENT_DIV);
        public static final ToolBeanColumn FIELD_SASID =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_SASID);
        public static final ToolBeanColumn FIELD_SCHOOL_NAME =
                new ToolBeanColumn(SisBeanPaths.STUDENT.school().name());
        public static final ToolBeanColumn FIELD_SPED_DISABILITY =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_SPED_DISABILITY);
        public static final ToolBeanColumn FIELD_SPED_DISABILITY_2 =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_SPED_DISABILITY_2);
        public static final ToolBeanColumn FIELD_SPED_DISABILITY_3 =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_SPED_DISABILITY_3);
        public static final ToolBeanColumn FIELD_SPED_DIV_2 =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_SPED_DIV_2);
        public static final ToolBeanColumn FIELD_SPED_DIV_3 =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_SPED_DIV_3);
        public static final ToolBeanColumn FIELD_SPED_PLACEMENT =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_SPED_PLACEMENT);
        public static final ToolBeanColumn FIELD_SPED_REG_PCT =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_SPED_REG_PCT);
        public static final ToolBeanColumn FIELD_SPED_SKL_2 =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_SPED_SKL_2);
        public static final ToolBeanColumn FIELD_SPED_SKL_3 =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_SPED_SKL_3);
        public static final ToolBeanColumn FIELD_SPED_SVC_PCT =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_SPED_SVC_PCT);
        public static final ToolBeanColumn FIELD_SPED_SVC_PCT_2 =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_SPED_SVC_PCT_2);
        public static final ToolBeanColumn FIELD_SPED_SVC_PCT_3 =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_SPED_SVC_PCT_3);
        public static final ToolBeanColumn FIELD_SPED_TIME_PCT =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_SPED_TIME_PCT);
        public static final ToolBeanColumn FIELD_STD_HEAD_START_PROVIDER =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_HEAD_START_PROVIDER);
        public static final ToolBeanColumn FIELD_STD_MOP_FLAG =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_MOP_FLAG);
        public static final ToolBeanColumn FIELD_STD_NIGHT_RESIDENCE =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_NIGHT_RESIDENCE);
        public static final ToolBeanColumn FIELD_STUDENT_TUITION =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_TUITION);
        public static final ToolBeanColumn FIELD_TITLE_I =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_TITLE_I);
        public static final ToolBeanColumn FIELD_UNACCOMPANIED =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_UNACCOMPANIED);
        public static final ToolBeanColumn FIELD_US_END_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_US_END_DATE);
        public static final ToolBeanColumn FIELD_US_ENTRY_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_US_ENTRY_DATE);
        public static ToolBeanRelationship CHILD_STUDENT_ASSESSMENTS =
                new ToolBeanRelationship(VAToolStudent.class,
                        VAStudentAssessment.class,
                        CHILD_KEY_STUDENT_ASSESSMENT,
                        SisBeanPaths.STUDENT_ASSESSMENT.studentOid().toString(),
                        SisBeanPaths.STUDENT.studentAssessments().getRelationshipType());
        public static ToolBeanRelationship CHILD_STUDENT_PROGRAMS =
                new ToolBeanRelationship(VAToolStudent.class,
                        VAToolStudentProgramParticipation.class,
                        CHILD_KEY_STUDENT_PROGRAMS,
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid().toString(),
                        SisBeanPaths.STUDENT.programParticipation().getRelationshipType());
        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStudent.FULL_DEFINITION
                .expand(FIELD_AP_CODE,
                        FIELD_ASSOCIATES,
                        FIELD_ATT_CONF,
                        FIELD_ATT_PLAN,
                        FIELD_BIRTH_COUNTRY,
                        FIELD_CAMBRIDGE_PROG,
                        FIELD_CAREER_CLUSTER,
                        FIELD_COURT_REF,
                        FIELD_CTE_ATTAINMENT,
                        FIELD_CTE_FINISHER,
                        FIELD_CTE_PATHWAY,
                        FIELD_CTE_POPULATION,
                        FIELD_DIPLOMA,
                        FIELD_DISADV_STATUS,
                        FIELD_EARLY_SCHOLARSHIP,
                        FIELD_EC_SPED,
                        FIELD_EC_TIME,
                        FIELD_ESL_CODE,
                        FIELD_EXCLUDE,
                        FIELD_GED,
                        FIELD_GENDER_CODE,
                        FIELD_GIFTED_REFERRAL,
                        FIELD_GIFTED,
                        FIELD_GRAD_CODE,
                        FIELD_GRAD_PLAN,
                        FIELD_GRADE_LEVEL_OVERRIDE,
                        FIELD_HISPANIC_LATINO_INDICATOR,
                        FIELD_HOME_LANGUAGE_CODE,
                        FIELD_HOME_SCHOOL_CODE,
                        FIELD_IB_CODE,
                        FIELD_IB_FLAG,
                        FIELD_IMMIGRANT_STATUS,
                        FIELD_K_READINESS,
                        FIELD_MILITARY_COMPACT,
                        FIELD_MILITARY_CONNECTED,
                        FIELD_NONPUB_FTE,
                        FIELD_PARENT_PLACED,
                        FIELD_PERSON_PHONE_01,
                        FIELD_PHYSICAL_ADDRESS_LINE_01,
                        FIELD_PHYSICAL_ADDRESS_LINE_02,
                        FIELD_PHYSICAL_ADDRESS_POSTAL_CODE,
                        FIELD_PK_EXPERIENCE,
                        FIELD_PK_FUNDING,
                        FIELD_PK_WEEKLY_TIME,
                        FIELD_RESIDENT_DIV,
                        FIELD_SASID,
                        FIELD_SCHOOL_NAME,
                        FIELD_SPED_DISABILITY,
                        FIELD_SPED_DISABILITY_2,
                        FIELD_SPED_DISABILITY_3,
                        FIELD_SPED_DIV_2,
                        FIELD_SPED_DIV_3,
                        FIELD_SPED_PLACEMENT,
                        FIELD_SPED_REG_PCT,
                        FIELD_SPED_SKL_2,
                        FIELD_SPED_SKL_3,
                        FIELD_SPED_SVC_PCT,
                        FIELD_SPED_SVC_PCT_2,
                        FIELD_SPED_SVC_PCT_3,
                        FIELD_SPED_TIME_PCT,
                        FIELD_STD_HEAD_START_PROVIDER,
                        FIELD_STD_MOP_FLAG,
                        FIELD_STD_NIGHT_RESIDENCE,
                        FIELD_STUDENT_TUITION,
                        FIELD_TITLE_I,
                        FIELD_UNACCOMPANIED,
                        FIELD_US_END_DATE,
                        FIELD_US_ENTRY_DATE)
                .expandJoinAdjusters(
                        new JoinAdjusterPattern(JoinType.LEFT_OUTER, SisBeanPaths.PERSON_ADDRESS.getDatabaseName()))
                .expandRelationships(
                        CHILD_STUDENT_ASSESSMENTS,
                        CHILD_STUDENT_PROGRAMS);

        /**
         * Instantiates a new VA tool student.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public VAToolStudent(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the attendance conference code.
         *
         * @return String
         */
        public String getAttendanceConferenceCode() {
            return getValueString(FIELD_ATT_CONF);
        }

        /**
         * Gets the attendance plan code.
         *
         * @return String
         */
        public String getAttendancePlanCode() {
            return getValueString(FIELD_ATT_PLAN);
        }

        /**
         * Gets the career cluster.
         *
         * @return String
         */
        public String getCareerCluster() {
            return getValueString(FIELD_CAREER_CLUSTER);
        }

        /**
         * Gets the court referral complaint proceedings code.
         *
         * @return String
         */
        public String getCourtReferralComplaintProceedingsCode() {
            return getValueString(FIELD_COURT_REF);
        }

        /**
         * Gets the cte population.
         *
         * @return String
         */
        public String getCtePopulation() {
            return getValueString(FIELD_CTE_POPULATION);
        }

        /**
         * Gets the cte finisher.
         *
         * @return String
         */
        public String getCteFinisher() {
            return getValueReferenceState(FIELD_CTE_FINISHER);
        }

        /**
         * Gets the disadvantage status.
         *
         * @return String
         */
        public String getDisadvantageStatus() {
            return getValueString(FIELD_DISADV_STATUS);
        }

        /**
         * Gets the esl code.
         *
         * @return String
         */
        public String getEslCode() {
            return getValueReferenceState(FIELD_ESL_CODE);
        }

        /**
         * Gets the grade level state override.
         *
         * @return String
         */
        public String getGradeLevelStateOverride() {
            return getValueReferenceState(FIELD_GRADE_LEVEL_OVERRIDE);
        }

        /**
         * Gets the ged.
         *
         * @return String
         */
        public String getGed() {
            return getValueString(FIELD_GED);
        }

        /**
         * Gets the gifted referral.
         *
         * @return Plain date
         */
        public PlainDate getGiftedReferral() {
            return getValueDate(FIELD_GIFTED_REFERRAL);
        }

        /**
         * Gets the grad plan.
         *
         * @return String
         */
        public String getGradPlan() {
            return getValueReferenceState(FIELD_GRAD_PLAN);
        }

        /**
         * Gets the grad code.
         *
         * @return String
         */
        public String getGradCode() {
            return getValueReferenceState(FIELD_GRAD_CODE);
        }

        /**
         * Gets the head start provider code.
         *
         * @return String
         */
        public String getHeadStartProviderCode() {
            return getValueString(FIELD_STD_HEAD_START_PROVIDER);
        }

        /**
         * Gets the home school code.
         *
         * @return String
         */
        public String getHomeSchoolCode() {
            return getValueString(FIELD_HOME_SCHOOL_CODE);
        }

        /**
         * Gets the ib flag.
         *
         * @return boolean
         */
        public boolean getIbFlag() {
            return getValueLogical(FIELD_IB_FLAG);
        }

        /**
         * Gets the night residence.
         *
         * @return String
         */
        public String getNightResidence() {
            return getValueReferenceState(FIELD_STD_NIGHT_RESIDENCE);
        }

        /**
         * Checks if is unaccompanied.
         *
         * @return true, if is unaccompanied
         */
        public boolean isUnaccompanied() {
            return getValueLogical(FIELD_UNACCOMPANIED);
        }

        /**
         * Gets the gender code.
         *
         * @return String
         */
        public String getGenderCode() {
            return this.getValueReferenceState(FIELD_GENDER_CODE);
        }

        /**
         * Gets the hispanic latino indicator.
         *
         * @return boolean
         */
        public boolean getHispanicLatinoIndicator() {
            return getValueLogical(FIELD_HISPANIC_LATINO_INDICATOR);
        }

        /**
         * Gets the home language code.
         *
         * @return String
         */
        public String getHomeLanguageCode() {
            return this.getValueReferenceState(FIELD_HOME_LANGUAGE_CODE);
        }

        /**
         * Gets the person phone 01.
         *
         * @return String
         */
        public String getPersonPhone01() {
            return getValueString(FIELD_PERSON_PHONE_01);
        }

        /**
         * Gets the sasid.
         *
         * @return String
         */
        public String getSasid() {
            return getValueString(FIELD_SASID);
        }

        /**
         * Gets the school name.
         *
         * @return String
         */
        public String getSchoolName() {
            return getValueString(FIELD_SCHOOL_NAME);
        }

        /**
         * Gets the sped disability.
         *
         * @return String
         */
        public String getSpedDisability() {
            return getValueString(FIELD_SPED_DISABILITY);
        }

        /**
         * Gets the sped disability 2.
         *
         * @return String
         */
        public String getSpedDisability2() {
            return getValueString(FIELD_SPED_DISABILITY_2);
        }

        /**
         * Gets the sped disability 3.
         *
         * @return String
         */
        public String getSpedDisability3() {
            return getValueString(FIELD_SPED_DISABILITY_3);
        }

        /**
         * Gets the sped pct.
         *
         * @return Double
         */
        public Double getSpedPct() {
            Object rawValue = getValue(FIELD_SPED_TIME_PCT);
            if (rawValue == null) {
                return null;
            }
            return (Double) PredefinedConverter.DOUBLE.convertedValue(rawValue);
        }

        /**
         * Gets the student assessments.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<VAStudentAssessment> getStudentAssessments(X2Broker broker) {
            return (List<VAStudentAssessment>) getChildren(broker, CHILD_STUDENT_ASSESSMENTS);
        }

        /**
         * Gets the student programs.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<VAToolStudentProgramParticipation> getStudentPrograms(X2Broker broker) {
            return (List<VAToolStudentProgramParticipation>) getChildren(broker, CHILD_STUDENT_PROGRAMS);
        }

        /**
         * Gets the student tuition.
         *
         * @return String
         */
        public String getStudentTuition() {
            return getValueString(FIELD_STUDENT_TUITION);
        }

        /**
         * Gets the us end date.
         *
         * @return Plain date
         */
        public PlainDate getUsEndDate() {
            return getValueDate(FIELD_US_END_DATE);
        }

        /**
         * Gets the us entry date.
         *
         * @return Plain date
         */
        public PlainDate getUsEntryDate() {
            return getValueDate(FIELD_US_ENTRY_DATE);
        }

        /**
         * Gets the VA enrollments.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<VAToolEnrollment> getVAEnrollments(X2Broker broker) {
            return super.getEnrollments(broker).stream()
                    .map(it -> (VAToolEnrollment) it)
                    .collect(Collectors.toList());
        }

        /**
         * Checks for immigrant status.
         *
         * @return Boolean
         */
        public Boolean hasImmigrantStatus() {
            return getValueLogical(FIELD_IMMIGRANT_STATUS);
        }

        /**
         * Checks for mop flag.
         *
         * @return true, if successful
         */
        public boolean hasMopFlag() {
            return getValueLogical(FIELD_STD_MOP_FLAG);
        }

        /**
         * Checks if is excluded.
         *
         * @return Boolean
         */
        public Boolean isExcluded() {
            return getValueLogical(FIELD_EXCLUDE);
        }
    }

    /**
     * The Class VAToolSection.
     */
    public static class VAToolSection extends ToolSection {

        public static final ToolBeanColumn FIELD_CAREER_AND_TECH_ED =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_CAREER_AND_TECH_ED, null, false));

        @SuppressWarnings("hiding")
        public static final ToolBeanColumn FIELD_CTE_CAREER_CLUSTER =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_CTE_CAREER_CLUSTER, null, false));

        public static final ToolBeanColumn FIELD_GOVERNORS_SCHOOL =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_GOVERNORS_SCHOOL, null, false));

        public static final ToolBeanColumn FIELD_SEDF_REPORT_FLAG =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_SEDF_REPORT_FLAG, null, false));

        public static final ToolBeanColumn FIELD_TRADITIONAL_GENDER =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_TRADITIONAL_GENDER, null, false));

        public static final ToolBeanColumn FIELD_VIRTUAL_COURSE_TYPE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_VIRTUAL_COURSE_TYPE, null, false));

        /**
         * Instantiates a new VA tool section.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public VAToolSection(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolSection.FULL_DEFINITION
                .expand(FIELD_CAREER_AND_TECH_ED,
                        FIELD_GOVERNORS_SCHOOL,
                        FIELD_SEDF_REPORT_FLAG,
                        FIELD_TRADITIONAL_GENDER,
                        FIELD_CTE_CAREER_CLUSTER,
                        FIELD_VIRTUAL_COURSE_TYPE);

        /**
         * Gets the career and tech ed.
         *
         * @return boolean
         */
        public boolean getCareerAndTechEd() {
            return getValueLogical(FIELD_CAREER_AND_TECH_ED);
        }

        /**
         * Gets the cte career cluster.
         *
         * @return String
         */
        public String getCteCareerCluster() {
            return getValueReferenceState(FIELD_CTE_CAREER_CLUSTER);
        }

        /**
         * Gets the govenors school.
         *
         * @return boolean
         */
        public boolean getGovenorsSchool() {
            return getValueLogical(FIELD_GOVERNORS_SCHOOL);
        }

        /**
         * Gets the sedf report flag.
         *
         * @return boolean
         */
        public boolean getSedfReportFlag() {
            return getValueLogical(FIELD_SEDF_REPORT_FLAG);
        }

        /**
         * Gets the traditional gender.
         *
         * @return String
         */
        public String getTraditionalGender() {
            return getValueString(FIELD_TRADITIONAL_GENDER);
        }

        /**
         * Gets the virtual course type.
         *
         * @return String
         */
        public String getVirtualCourseType() {
            return getValueReferenceState(FIELD_VIRTUAL_COURSE_TYPE);
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

        List<VAToolEnrollment> m_enrollments;
        VAToolEnrollment m_firstEntry;
        VAToolEnrollment m_firstWithdrawal;
        VAToolSchool m_school;

        /**
         * Constructor: Find other values from the enrollment list.
         *
         * @param data VAStudentRecordCollection
         * @param enrollments List<StudentEnrollment>
         */
        public EnrollmentSpan(VAStudentRecordCollectionRevised data, List<VAToolEnrollment> enrollments) {
            m_enrollments = enrollments;
            for (VAToolEnrollment enrollment : enrollments) {
                if (m_school == null) {
                    m_school = (VAToolSchool) enrollment.getSchool(data.getBroker());
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
            VAToolEnrollment lastEnrollment = enrollments.get(enrollments.size() - 1);
            if ((m_firstWithdrawal == null) && (enrollments.size() > 1)
                    && StudentEnrollment.STATUS_CHANGE.equals(lastEnrollment.getEnrollmentType())) {
                String hbEntryCode = lastEnrollment.getHbEntryCode();
                if (VAStudentRecordCollectionRevised.HB_ENTRY_CODES.contains(hbEntryCode)) {
                    m_firstWithdrawal = lastEnrollment;
                }
            }
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
        public VAToolEnrollment getEnrollmentForDate(PlainDate date, int type) {
            VAToolEnrollment lastEnrollment = null;
            for (VAToolEnrollment enrollment : m_enrollments) {
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

        /**
         * Return the list of enrollments in this span.
         *
         * @return List<StudentEnrollment>
         */
        public List<VAToolEnrollment> getEnrollments() {
            return m_enrollments;
        }

        /**
         * Return the first entry record for the enrollment span.
         *
         * @return StudentEnrollment
         */
        public VAToolEnrollment getFirstEntry() {
            return m_firstEntry;
        }

        /**
         * Return the first withdrawal record for the enrollment span.
         *
         * @return StudentEnrollment
         */
        public VAToolEnrollment getFirstWithdrawal() {
            return m_firstWithdrawal;
        }

        /**
         * Return the school for the enrollment span.
         *
         * @return School
         */
        public VAToolSchool getSchool() {
            return m_school;
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
        float m_absent;
        int m_absentDU;
        VAToolEnrollment m_entryEnrollment;
        VAToolEnrollment m_exitEnrollment;
        int m_membership;
        ToolSchool m_school;
        float m_unexAbsent;

        /**
         * constructor, set initial attendance and membership counts.
         *
         * @param entryEnrollment StudentEnrollment
         * @param withdrawalEnrollment StudentEnrollment
         * @param school SisSchool
         */
        protected MembershipAttendance(VAToolEnrollment entryEnrollment, VAToolEnrollment withdrawalEnrollment,
                ToolSchool school) {
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
         * Set the accumulated absence count.
         *
         * @param absent void
         */
        protected void setAbsent(float absent) {
            m_absent = absent;
        }

        /**
         * Gets the absent DU.
         *
         * @return the m_absentDU
         */
        protected int getAbsentDU() {
            return m_absentDU;
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
         * Return the student enrollment representing the first entry.
         *
         * @return StudentEnrollment
         */
        protected VAToolEnrollment getEntryEnrollment() {
            return m_entryEnrollment;
        }

        /**
         * Return the student enrollment representing the final exit.
         *
         * @return StudentEnrollment
         */
        protected VAToolEnrollment getExitEnrollment() {
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
        protected ToolSchool getSchool() {
            return m_school;
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
         * Sets the absent DU.
         *
         * @param absentDU void
         */
        protected void setAbsentDU(int absentDU) {
            this.m_absentDU = absentDU;
        }

        /**
         * Set the student enrollment record representing the first entry.
         *
         * @param entry void
         */
        protected void setEntryEnrollment(VAToolEnrollment entry) {
            m_entryEnrollment = entry;
        }

        /**
         * Set the student enrollment record representing the final exit.
         *
         * @param exit void
         */
        protected void setExitEnrollment(VAToolEnrollment exit) {
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
    protected static final String ALIAS_CAREER_AND_TECH_ED = "DOE CAREER AND TECH ED";
    protected static final String ALIAS_CTE_CAREER_CLUSTER = "DOE CTE CAREER CLUSTER";
    protected static final String ALIAS_DISTRICT_CODE = "DOE DISTRICT ID";
    protected static final String ALIAS_GOVERNORS_SCHOOL = "DOE GOVERNORS SCHOOL";
    protected static final String ALIAS_SEDF_REPORT_FLAG = "SEDF REPORT FLAG";
    protected static final String ALIAS_TRADITIONAL_GENDER = "DOE TRADITIONAL GENDER";
    protected static final String ALIAS_VIRTUAL_COURSE_TYPE = "DOE VIRTUAL COURSE TYPE";

    /*
     * Constants for each field ID from the export format field definition.
     */
    protected static final String FIELD_ADDRESS_1 = "Address 1";
    protected static final String FIELD_ADDRESS_2 = "Address 2";
    protected static final String FIELD_ADVANCED_PLACEMENT = "Advanced Placement";
    protected static final String FIELD_BIRTH_DATE = "Birth date";
    protected static final String FIELD_CAMBRIDGE_PROGRAM = "Cambridge Program";
    protected static final String FIELD_COUNTY_BIRTH = "County Birth";
    protected static final String FIELD_CRED_ACC = "Credit Accommodation";
    protected static final String FIELD_CTE_ATTAINMENT = "CTE Attainment";
    protected static final String FIELD_CTE_CAREER_CLUSTER = "CTE Career Cluster";
    protected static final String FIELD_CTE_DUAL_ENROLLMENT = "CTE Dual Enrollment";
    protected static final String FIELD_CTE_FINISHER = "CTE Finisher";
    protected static final String FIELD_CTE_PROGRAM = "CTE Program";
    protected static final String FIELD_CTE_SPECIAL_POPULAT = "CTE Special Populat";
    protected static final String FIELD_DATE_OF_ENTRY = "Date of Entry into US Schools";
    protected static final String FIELD_DAYS_ABSENT = "Days Absent";
    protected static final String FIELD_DAYS_PRESENT = "Days Present";
    protected static final String FIELD_DEVICE_ACCESS_REMOTE_LEARNING = "Device Access for Remote Learning";
    protected static final String FIELD_DIPLOMA_SEAL = "Diploma Seal";
    protected static final String FIELD_DISADV_STATUS = "Disadv Status";
    protected static final String FIELD_DISTANCE_LEARNING = "Distance Learning";
    protected static final String FIELD_DIVISION_ID = "Division ID";
    protected static final String FIELD_DIVISION_ID_SERVE = "Division ID Serve";
    protected static final String FIELD_DUAL_ENROLLMENT = "Dual Enrollment";
    protected static final String FIELD_EARLY_COLLEGE_SCHOL = "Early College Schol";
    protected static final String FIELD_ENTRY_CODE = "Entry Code";
    protected static final String FIELD_ENTRY_DATE = "Entry Date";
    protected static final String FIELD_ESL_SERVICES = "EL Code";
    protected static final String FIELD_ETHNIC = "Ethnic";
    protected static final String FIELD_EXIT_CODE = "Exit Code";
    protected static final String FIELD_EXIT_DATE = "Exit Date";
    protected static final String FIELD_FIRST_NAME = "First Name";
    protected static final String FIELD_FT_VIRTUAL_PROGRAM = "FT Virtual Program";
    protected static final String FIELD_GED_PROGRAM = "GED Program";
    protected static final String FIELD_GENDER = "Gender";
    protected static final String FIELD_GIFTED = "Gifted";
    protected static final String FIELD_GIFTED_REFERRAL = "Gifted Referral";
    protected static final String FIELD_GRADE_LEVEL = "Grade Level";
    protected static final String FIELD_GRADUATE_COMP = "Graduate Comp";
    protected static final String FIELD_GRADUATE_PLAN = "Graduate Plan";
    protected static final String FIELD_HEAD_START_PROVIDER = "Head Start Provider Code";
    protected static final String FIELD_HOME_LANGUAGE = "Home Language";
    protected static final String FIELD_HOMEROOM = "Homeroom";
    protected static final String FIELD_IB_CODE = "IB Code";
    protected static final String FIELD_IMMIGRANT = "Immigrant";
    protected static final String FIELD_INTERNET_ACCESS_REMOTE_LEARNING = "Internet Access for Remote Learning";
    protected static final String FIELD_INTL_BACC = "Intl Bacc";
    protected static final String FIELD_KG_HALFDAY = "KG HalfDay";
    protected static final String FIELD_KG_READINESS = "KG Readiness";
    protected static final String FIELD_LAST_NAME = "Last Name";
    protected static final String FIELD_LIEP = "LIEP";
    protected static final String FIELD_LOCAL_ID = "Local ID";
    protected static final String FIELD_MIDDLE_NAME = "Middle Name";
    protected static final String FIELD_MILITARY_STATUTE = "Military Statute";
    protected static final String FIELD_MOP_CLASSES = "MOP Classes";
    protected static final String FIELD_MOP_FLAG = "MOP Flag";
    protected static final String FIELD_NEGLECTED_DELINQNT = "Neglected/Delinqnt";
    protected static final String FIELD_NIGHT_RESIDENCE = "Night Residence";
    protected static final String FIELD_NON_PUB_FTE = "Non-pub FTE";
    protected static final String FIELD_PARENT_PLACED = "Parent Placed";
    protected static final String FIELD_PARENTAL_REMOTE_LEARNING_DECISION = "Parental Remote Learning Decision";
    protected static final String FIELD_PHONE_NUMBER = "Phone Number";
    protected static final String FIELD_PK_EXP = "PK Exp";
    protected static final String FIELD_PK_FUNDING_CODE = "PK Funding Source";
    protected static final String FIELD_PK_WEEKLY_TIME = "PK Weekly Time";
    protected static final String FIELD_PRIMARY_DISABILITY = "Primary Disability";
    protected static final String FIELD_RACE = "Race";
    protected static final String FIELD_RECORD_TYPE = "Record Type";
    protected static final String FIELD_REGIONAL_LOCAL_CENTER_PERCENT_OF_TIME = "Regional/Local Center Percent of Time";
    protected static final String FIELD_REGULAR_EC = "Regular EC";
    protected static final String FIELD_REMOTE_INST_PERCENT_OF_TIME = "Remote Instruction Percent of Time";
    protected static final String FIELD_REPORTING_SCHOOL = "Reporting School";
    protected static final String FIELD_RESIDENT_DIVISION = "Resident Division";
    protected static final String FIELD_RESTRAINT = "Restraint";
    protected static final String FIELD_RETENTION = "Retention";
    protected static final String FIELD_RETIRED1 = "RETIRED1";
    protected static final String FIELD_RETIRED10 = "RETIRED10";
    protected static final String FIELD_RETIRED11 = "RETIRED11";
    protected static final String FIELD_RETIRED12 = "RETIRED12";
    protected static final String FIELD_RETIRED13 = "RETIRED13";
    protected static final String FIELD_RETIRED2 = "RETIRED2";
    protected static final String FIELD_RETIRED3 = "RETIRED3";
    protected static final String FIELD_RETIRED4 = "RETIRED4";
    protected static final String FIELD_RETIRED5 = "RETIRED5";
    protected static final String FIELD_RETIRED6 = "RETIRED6";
    protected static final String FIELD_RETIRED7 = "RETIRED7";
    protected static final String FIELD_RETIRED8 = "RETIRED8";
    protected static final String FIELD_RETIRED9 = "RETIRED9";
    protected static final String FIELD_SCHOOL_CHOICE = "School Choice";
    protected static final String FIELD_SCHOOL_ID = "School ID";
    protected static final String FIELD_SCHOOL_ID_SERVE = "School ID Serve";
    protected static final String FIELD_SECLUSION = "Seclusion";
    protected static final String FIELD_SLIFE_STATUS_FLAG = "SLIFE Status Flag";
    protected static final String FIELD_SPED_DISABILITY_2 = "SPED Disability 2";
    protected static final String FIELD_SPED_DISABILITY_3 = "SPED Disability 3";
    protected static final String FIELD_SPED_DIV_2 = "SPED Div 2";
    protected static final String FIELD_SPED_DIV_3 = "SPED Div 3";
    protected static final String FIELD_SPED_EC = "SPED EC";
    protected static final String FIELD_SPED_PLACEMENT = "SPED Placement";
    protected static final String FIELD_SPED_PRIM_SVC_PCT = "SPED Prim Svc Pct";
    protected static final String FIELD_SPED_REG_CLASS_PCT = "SPED Reg Class Pct";
    protected static final String FIELD_SPED_SCHOOL_2 = "SPED School 2";
    protected static final String FIELD_SPED_SCHOOL_3 = "SPED School 3";
    protected static final String FIELD_SPED_SVC_PCT_2 = "SPED Svc Pct 2";
    protected static final String FIELD_SPED_SVC_PCT_3 = "SPED Svc Pct 3";
    protected static final String FIELD_SPED_TIME_PCT = "SPED Time pct";
    protected static final String FIELD_SPED_TUIT_DISABILITY = "SPED Tuit Disability";
    protected static final String FIELD_SPED_TUITION_S1 = "SPED Tuition S1";
    protected static final String FIELD_SPED_TUITION_S2 = "SPED Tuition S2";
    protected static final String FIELD_SPED_TUITION_SUM = "SPED Tuition Sum";
    protected static final String FIELD_STATE_ID = "State ID";
    protected static final String FIELD_STATUS = "Status";
    protected static final String FIELD_SUPP_ED = "Supp Ed";
    protected static final String FIELD_TITLE1 = "Title1";
    protected static final String FIELD_TRUANCY_CONFERENCE = "Truancy Conference";
    protected static final String FIELD_TUITION_PAID = "Tuition Paid";
    protected static final String FIELD_UNACCOMP_HOMELESS = "Unaccomp Homeless";
    protected static final String FIELD_UNEXCUSED_ABSENCES = "Unexcused Absences";
    protected static final String FIELD_W8_REASON = "W8 Reason";
    protected static final String FIELD_ZIP_CODE = "Zip Code";

    protected static final int MAX_SAFE_PARAMETERS = 1800;

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
    protected static final String CODE_ATT_DU = "DU";
    protected static final String COLLECTION_FALL = "FALL";
    protected static final String COLLECTION_SPRING = "SPR";
    protected static final String COLLECTION_EOY = "EOY";
    protected static final String COLLECTION_SUMMER = "SUM";
    protected static final String COLLECTION_PRESUB = "PRESUB";
    protected static final String COLLECTION_SPED = "SpecialED";
    protected static final String COLLECTION_SNAPSHOT = "snapshot";
    protected static final String CONTINUATION_CODE_E119 = "E119";
    protected static final String DEFAULT_CALENDAR_NAME = "Standard";
    protected static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";
    protected static final String ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED =
            " Assessment Definition is not loaded in the Data Dictionary.";
    protected static final String ERROR_TYPE_WARNING = "Warning";
    protected static final List<String> HB_ENTRY_CODES = Arrays.asList("R216", "R217", "R218", "R219");
    protected static final String STATUS_NO_SHOW = "N";
    protected static final String STATUS_SERVICE = "Services";
    protected static final String STATUS_VIRTUAL_CLASS = "V";

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
                    FIELD_REGIONAL_LOCAL_CENTER_PERCENT_OF_TIME, FIELD_RESTRAINT, FIELD_SECLUSION},
            {COLLECTION_SPRING, FIELD_CTE_CAREER_CLUSTER, FIELD_SPED_TIME_PCT, FIELD_COUNTY_BIRTH, FIELD_IMMIGRANT,
                    FIELD_GIFTED, FIELD_GIFTED_REFERRAL, FIELD_INTL_BACC, FIELD_ADVANCED_PLACEMENT,
                    FIELD_DUAL_ENROLLMENT, FIELD_CTE_FINISHER, FIELD_RETENTION, FIELD_TRUANCY_CONFERENCE,
                    FIELD_HOMEROOM, FIELD_DIPLOMA_SEAL, FIELD_CTE_DUAL_ENROLLMENT, FIELD_CTE_ATTAINMENT,
                    FIELD_CTE_PROGRAM, FIELD_ADDRESS_1, FIELD_ADDRESS_2, FIELD_ZIP_CODE, FIELD_PHONE_NUMBER,
                    FIELD_UNEXCUSED_ABSENCES, FIELD_IB_CODE, FIELD_SPED_PLACEMENT, FIELD_SPED_REG_CLASS_PCT,
                    FIELD_SPED_PRIM_SVC_PCT, FIELD_SPED_SVC_PCT_2, FIELD_SPED_DIV_2, FIELD_SPED_SCHOOL_2,
                    FIELD_SPED_SVC_PCT_3, FIELD_SPED_DIV_3, FIELD_SPED_SCHOOL_3, FIELD_CAMBRIDGE_PROGRAM,
                    FIELD_REGULAR_EC, FIELD_SPED_EC, FIELD_PARENT_PLACED, FIELD_SPED_TUITION_S2,
                    FIELD_SPED_TUITION_SUM, FIELD_LIEP, FIELD_REGIONAL_LOCAL_CENTER_PERCENT_OF_TIME, FIELD_RESTRAINT,
                    FIELD_SECLUSION},
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
                    FIELD_DEVICE_ACCESS_REMOTE_LEARNING, FIELD_PARENTAL_REMOTE_LEARNING_DECISION, FIELD_RESTRAINT,
                    FIELD_SECLUSION},
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
                    FIELD_PARENTAL_REMOTE_LEARNING_DECISION, FIELD_RESTRAINT, FIELD_SECLUSION},
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
                    FIELD_PARENTAL_REMOTE_LEARNING_DECISION, FIELD_RESTRAINT, FIELD_SECLUSION},
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
                    FIELD_PARENTAL_REMOTE_LEARNING_DECISION, FIELD_RESTRAINT, FIELD_SECLUSION}};

    /*
     * Instance variables.
     */
    protected String m_activeCode;
    protected String m_collection;
    protected PlainDate m_districtStartDate;
    protected String m_enrollmentCodeRefTableOid;
    protected String m_enrollmentReasonRefTableOid;
    protected Map<String, PlainDate> m_firstDaysOfSchools;
    protected String m_isaepCode;
    protected boolean m_missingSasid;
    protected String m_preRegCode;
    protected boolean m_preregOnly;
    protected Map<String, ReferenceCode> m_raceCodes;
    protected boolean m_removeNonrequiredFields;
    protected PlainDate m_reportDate;
    protected int m_rowCount;
    protected List<String> m_schoolOverride;
    protected Map<String, Map<String, Set<PlainDate>>> m_schoolsToCalendars;
    protected List<String> m_serviceStatusCodes;
    protected Integer m_sort;
    protected PlainDate m_summerBeginDate;
    protected Character m_vasrcDelimiterChar;
    protected List<String> m_virtualStatusCodes;
    protected String m_withdrawalCodeRefTableOid;
    protected String m_withdrawalReasonRefTableOid;

    /*
     * data fields for values, looked up from aliases.
     */
    protected String m_fieldDistrictCode;

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

    protected class RetrieveConductAction implements FieldRetriever {

        protected static final String CALC_ID = "ACT";
        protected static final String CALC_PARAM_SECLUSION = "SECLUSION";
        protected static final String CALC_PARAM_RESTRAINT = "RESTRAINT";

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
            VAToolStudent std = (VAToolStudent) entity.getBean();
            Collection<ToolConductAction> conductActions = std.getConductActions(m_broker);
            Predicate<VAToolConductAction> propertyPredicate = CALC_PARAM_RESTRAINT.equals(field.getParameter())
                    ? VAToolConductAction::isRestraint
                    : VAToolConductAction::isSeclusion;
            return conductActions.stream().map(act -> (VAToolConductAction) act).filter(propertyPredicate).count();
        }
    }

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
            VAStudentRecordCollectionRevised srcData = (VAStudentRecordCollectionRevised) data;
            String value = null;
            if (COLLECTION_EOY.equals(srcData.m_collection)
                    || COLLECTION_SUMMER.equals(srcData.m_collection)) {
                String gradeLevel = entity.getFieldValue(FIELD_GRADE_LEVEL);
                VAToolStudent std = (VAToolStudent) entity.getBean();
                Collection<VAStudentAssessment> asmLAVCList = std.getStudentAssessments(m_broker);
                if (asmLAVCList != null && !asmLAVCList.isEmpty()) {
                    if (CALC_PARAM_CREDIT_ACC.equals(field.getParameter())) {
                        int valueToReturn = 0;
                        for (VAStudentAssessment asm : asmLAVCList) {
                            String creditCodeState = asm.getCreditAccomodation();
                            if (!StringUtils.isEmpty(creditCodeState) && creditCodeState.matches("\\d+")) {
                                valueToReturn += Integer.valueOf(creditCodeState).intValue();
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
                VAToolStudent std = (VAToolStudent) entity.getBean();
                Collection<VAStudentAssessment> asmLAVCList = std.getStudentAssessments(m_broker);
                if (asmLAVCList != null && !asmLAVCList.isEmpty()) {
                    for (VAStudentAssessment asmLAVC : asmLAVCList) {
                        value = asmLAVC.getIntensiveSupportSvs();
                        if (!StringUtils.isEmpty(value)) {
                            break;
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
            VAToolStudent std = (VAToolStudent) entity.getBean();
            String value = std.getCteFinisher();
            StudentRecordEntity sre = (StudentRecordEntity) entity;
            Set<String> gradeLevels = new HashSet<String>(Arrays.asList("07", "08", "09", "10", "11", "12", "TT"));
            if ((COLLECTION_PRESUB.equals(m_collection) || COLLECTION_EOY.equals(m_collection))
                    && sre.m_gradeLevel != null && gradeLevels.contains(sre.m_gradeLevel)
                    && StringUtils.isEmpty(value)) {
                boolean foundCteCourse = false;
                List<ToolTranscript> transcripts = std.getStudentTranscripts(data.getBroker());
                if (transcripts != null && !transcripts.isEmpty()) {
                    for (ToolTranscript transcript : transcripts) {
                        String transcriptGradeLevel = transcript.getGradeLevel();
                        if (!StringUtils.isEmpty(transcriptGradeLevel)
                                && gradeLevels.contains(transcriptGradeLevel)) {
                            VAToolCourse transcriptCourse = (VAToolCourse) transcript.getCourse(data.getBroker());
                            if (transcriptCourse != null) {
                                VAToolCourse course = (VAToolCourse) transcriptCourse.getRootCourse(data.getBroker());
                                if (course.getCareerAndTechEd()) {
                                    foundCteCourse = true;
                                    break;
                                }
                            }
                        }
                    }
                }
                // If no CTE Course is found on Transcripts, continue looking on current student
                // schedule
                if (!foundCteCourse) {
                    Range<Date> infiniteRange = Range.of(null, null);
                    Collection<ToolStudentSchedule> studentSchedules = std.getStudentSchedules(data.getBroker())
                            .stream()
                            .map(ssc -> (VAToolStudentSchedule) ssc)
                            .filter(ssc -> ssc.isCurrent(getBroker(), m_reportDate))
                            .sorted(new Comparator<ToolStudentSchedule>() {

                                @Override
                                public int compare(ToolStudentSchedule ssc1, ToolStudentSchedule ssc2) {
                                    return ssc1.getOid().toUpperCase().compareTo(ssc2.getOid().toUpperCase());
                                }
                            })
                            .collect(Collectors.toList());
                    if (studentSchedules != null && !studentSchedules.isEmpty()) {
                        for (ToolStudentSchedule studentSchedule : studentSchedules) {
                            VAToolSection studentSection = (VAToolSection) studentSchedule.getSection(data.getBroker());
                            if (studentSection != null) {
                                VAToolCourse scheduleCourse = (VAToolCourse) studentSection.getCourse(data.getBroker());
                                if (scheduleCourse != null) {
                                    if (scheduleCourse.getCareerAndTechEd()) {
                                        foundCteCourse = true;
                                        break;
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
            VAStudentRecordCollectionRevised src = (VAStudentRecordCollectionRevised) data;

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
            VAToolStudent student = (VAToolStudent) entity.getBean();
            String param = (String) field.getParameter();
            // Find the entry code, if any.
            VAToolEnrollment wEnrollment = attendance.getExitEnrollment();
            String code = null;
            if (CALC_PARAM_ATT_PLAN.equals(param)) {
                code = getCalculatedValue(wEnrollment, VAToolEnrollment.FIELD_ENR_ATT_PLAN,
                        VAToolEnrollment::getAttPlan,
                        VAToolStudent.FIELD_ATT_PLAN, student.getAttendancePlanCode());
            } else if (CALC_PARAM_ATT_CONF.equals(param)) {
                code = getCalculatedValue(wEnrollment, VAToolEnrollment.FIELD_ENR_ATT_CONF,
                        VAToolEnrollment::getAttConf, VAToolStudent.FIELD_ATT_CONF,
                        student.getAttendanceConferenceCode());
            } else if (CALC_PARAM_COURT_REF.equals(param)) {
                code = getCalculatedValue(wEnrollment, VAToolEnrollment.FIELD_ENR_COURT_REF,
                        VAToolEnrollment::getCourtRef, VAToolStudent.FIELD_COURT_REF,
                        student.getCourtReferralComplaintProceedingsCode());
            }
            return code;
        }

        /**
         * Gets the calculated value.
         *
         * @param wEnrollment VAToolEnrollment
         * @param enrColumn ToolBeanColumn
         * @param enrCodeSupplier Function<VAToolEnrollment,String>
         * @param stdColumn ToolBeanColumn
         * @param stdCode String
         * @return String
         * @throws X2BaseException exception
         */
        private String getCalculatedValue(VAToolEnrollment wEnrollment,
                                          ToolBean.ToolBeanColumn enrColumn,
                                          Function<VAToolEnrollment, String> enrCodeSupplier,
                                          ToolBean.ToolBeanColumn stdColumn,
                                          String stdCode)
                throws X2BaseException {
            String value = null;
            if (wEnrollment != null) {
                value = ToolBean.getDictionaryExtractor().lookupStateValue(enrColumn,
                        enrCodeSupplier.apply(wEnrollment));
            } else {
                value = ToolBean.getDictionaryExtractor().lookupStateValue(stdColumn, stdCode);
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
            VAToolStudent student = (VAToolStudent) entity.getBean();
            String value = null;
            if (field.getFieldId().contains("2")) {
                value = student.getSpedDisability2();
            } else if (field.getFieldId().contains("3")) {
                value = student.getSpedDisability3();
            }
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
            String nighttimeResidence = entity.getFieldValue(FIELD_NIGHT_RESIDENCE);
            if (!StringUtils.isEmpty(nighttimeResidence)) {
                return BooleanAsStringConverter.TRUE;
            }
            VAToolStudent student = (VAToolStudent) entity.getBean();
            return student.getDisadvantageStatus();

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
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            StudentRecordEntity srcEntity = (StudentRecordEntity) entity;
            ToolStudent student = (ToolStudent) entity.getBean();
            String value = null;

            // So we get our original value
            value = getSchoolValueNoOverride(param, srcEntity, student);

            if (m_schoolOverride.contains(value)
                    && VAToolEnrollment.FIELD_SERVICE_SCHOOL_CODE.getAlias().equals(param)) {
                value = getSchoolValueNoOverride(VAToolEnrollment.ALIAS_HOME_SCHOOL_CODE, srcEntity, student);
            }

            // If the home district is 098 and it is not EIMS, then we...
            if ("098".equals(getSchoolValueNoOverride(VAToolEnrollment.ALIAS_SERVICE_DISTRICT_CODE, srcEntity, student))
                    && !COLLECTION_PRESUB.equals(m_collection)) {
                // Now, we go in to see if the course is checked as a CTE or
                // Governors school
                if (srcEntity.getOverrideCodeValue(VAToolEnrollment.ALIAS_SERVICE_SCHOOL_CODE) != null) {
                    // If it is, then we override the home school with the
                    // serving school original value
                    // look up if there is the school checked override, if so
                    // the serving school becomes the home school
                    if (VAToolEnrollment.ALIAS_HOME_SCHOOL_CODE.equals(param)) {
                        value = getSchoolValueNoOverride(VAToolEnrollment.ALIAS_SERVICE_SCHOOL_CODE, srcEntity,
                                student);
                    }
                    // the home district with the serving district original
                    // value
                    if (VAToolEnrollment.ALIAS_HOME_DISTRICT_CODE.equals(param)) {
                        value = getSchoolValueNoOverride(VAToolEnrollment.ALIAS_SERVICE_DISTRICT_CODE, srcEntity,
                                student);
                    }
                    // And the serving school and district get the override
                    // values
                    if (VAToolEnrollment.ALIAS_SERVICE_DISTRICT_CODE.equals(param)) {
                        value = getSchoolValue(entity, VAToolEnrollment.ALIAS_SERVICE_DISTRICT_CODE, srcEntity,
                                student);
                    }
                    if (VAToolEnrollment.ALIAS_SERVICE_SCHOOL_CODE.equals(param)) {
                        if (m_schoolOverride.contains(value)) {
                            value = getSchoolValueNoOverride(VAToolStudent.ALIAS_STD_HOME_SCHOOL_CODE, srcEntity,
                                    student);
                        } else {
                            value = getSchoolValue(entity, VAToolEnrollment.ALIAS_SERVICE_SCHOOL_CODE, srcEntity,
                                    student);
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
         * @param student ToolStudent
         * @return String
         * @throws X2BaseException exception
         */
        private String getSchoolValue(StateReportEntity entity,
                                      String param,
                                      StudentRecordEntity srcEntity,
                                      ToolStudent student)
                throws X2BaseException {
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
         * @param student ToolStudent
         * @return String
         * @throws X2BaseException exception
         */
        private String getSchoolValueNoOverride(String param, StudentRecordEntity srcEntity, ToolStudent student)
                throws X2BaseException {
            MembershipAttendance attendance = srcEntity.getMembershipAttendance();
            Map<String, Object> calcValueMap = lookupOverrides(student, attendance.getEntryEnrollment(),
                    attendance.getSchool().getOid());
            return (String) calcValueMap.get(param);
        }
    }

    /**
     * Return EL Code.
     */
    protected class RetrieveEnglishLearners extends RetrieveStudentProgram implements FieldValidator {
        private static final String CALC_PARAM_CODE = "Code";
        private static final String CALC_PARAM_SLIFE_STATUS = "SLIFE Status";
        private static final String PROGRAM_CODE = "ESL";

        private PlainDate m_latestDOB;

        /**
         * Instantiates a new retrieve english learners.
         *
         * @param data StateReportData
         */
        protected RetrieveEnglishLearners(StateReportData data) {
            super(data, PROGRAM_CODE);
        }

        /**
         * Inits the.
         *
         * @param data StateReportData
         */
        private void init(StateReportData data) {
            if (m_latestDOB == null) {
                Calendar latestDOB = Calendar.getInstance();
                int year = data.getCurrentContext().getSchoolYear() - 10;
                latestDOB.set(Calendar.YEAR, year);
                latestDOB.set(Calendar.MONTH, Calendar.AUGUST);
                latestDOB.set(Calendar.DAY_OF_MONTH, 1);
                m_latestDOB = new PlainDate(latestDOB.getTime());
            }
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
            init(data);
            VAToolStudent student = (VAToolStudent) entity.getBean();
            if (CALC_PARAM_CODE.equals(field.getParameter())) {
                if (isAnyProgram(student)) {
                    String code = student.getEslCode();
                    if (!StringUtils.isEmpty(code)) {
                        StateReportValidationError error = new StateReportValidationError(entity, field,
                                "If Student Program Participation exists for English Learners program, the student alias value ["
                                        + VAToolStudent.ALIAS_ESL_CODE + "] must be empty",
                                "[" + VAToolStudent.ALIAS_ESL_CODE + "]=" + STYLE_BOLD + code + STYLE_END);
                        errors.add(error);
                    }
                }
            } else if (CALC_PARAM_SLIFE_STATUS.equals(field.getParameter())
                    && "Y".equals(entity.getFieldValue(FIELD_SLIFE_STATUS_FLAG))) {
                PlainDate dob = student.getDob();
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
         * @see com.x2dev.procedures.statereporting.va.VAStudentRecordCollectionRevised.RetrieveStudentProgram#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            init(data);
            VAToolStudent student = (VAToolStudent) entity.getBean();
            if (CALC_PARAM_CODE.equals(field.getParameter())) {
                VAToolStudentProgramParticipation program = matchingProgram(student);
                if (program != null) {
                    value = program.getStateCode();
                } else if (!isAnyProgram(student)) {
                    value = student.getEslCode();
                }
            } else if (CALC_PARAM_SLIFE_STATUS.equals(field.getParameter())) {
                VAToolStudentProgramParticipation program = matchingProgram(student);
                if (program != null) {
                    value = program.getSlifeStatus();
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
            VAStudentRecordCollectionRevised srcData = (VAStudentRecordCollectionRevised) data;
            StudentRecordEntity srcEntity = (StudentRecordEntity) entity;
            MembershipAttendance attendance = srcEntity.getMembershipAttendance();

            ToolStudent student = (ToolStudent) entity.getBean();

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

            VAToolEnrollment enrollment = null;
            VAToolEnrollment enrollmentE = attendance.getEntryEnrollment();
            VAToolEnrollment enrollmentW = attendance.getExitEnrollment();

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
            if (enrollmentE != null) {
                ToolBean.ToolSchool schoolE = enrollmentE.getSchool(data.getBroker());
                if (schoolE != null) {
                    schoolStartDate = getFirstSchoolInSessionDate(schoolE, student.getCalendarCode());
                }
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
                    enrollmentWStatusCode = data.getDictionaryExtractor()
                            .lookupStateValue(ToolEnrollment.FIELD_STATUS_CODE,
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
                    String hbEntryCode = enrollment.getHbEntryCode();
                    if (VAStudentRecordCollectionRevised.HB_ENTRY_CODES.contains(hbEntryCode)) {
                        if (param.startsWith("ENTRY_")) {
                            if (!overrideEntry) {
                                enrollmentECode = hbEntryCode;
                            }
                        } else if (param.startsWith("EXIT_")) {
                            enrollmentWCode = enrollment.getHbWithdrawalCode();
                            enrollmentWDate = DateUtils.add(enrollmentWDate, -1);
                        }
                    }
                }

                String reason = data.getDictionaryExtractor().lookupReferenceCodeByRefTbl(reasonRefTableOid,
                        enrollment.getReasonCode(), ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

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
                    value = data.getDictionaryExtractor().lookupReferenceCodeByRefTbl(codeRefTableOid, enrollmentCode,
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
         * @param data StateReportData
         */
        protected RetrieveFosterCare(StateReportData data) {
            super(data, PROGRAM_CODE);
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
         * @see com.x2dev.procedures.statereporting.va.VAStudentRecordCollectionRevised.RetrieveStudentProgram#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return matchingProgram((VAToolStudent) entity.getBean()) != null ? "Y" : null;
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
            VAToolStudent student = (VAToolStudent) entity.getBean();
            String value = student.getGed();
            StudentRecordEntity srcEntity = (StudentRecordEntity) entity;
            MembershipAttendance attendance = srcEntity.getMembershipAttendance();

            // Find the entry code, if any.
            VAToolEnrollment enrollment = attendance.getEntryEnrollment();
            String entryCode = null;
            if (enrollment != null) {
                entryCode = data.getDictionaryExtractor().lookupReferenceCodeByRefTbl(m_enrollmentCodeRefTableOid,
                        enrollment.getEnrollmentCode(), ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
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
            VAToolStudent student = (VAToolStudent) entity.getBean();
            PlainDate referralDate = student.getGiftedReferral();

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
            VAToolStudent student = (VAToolStudent) entity.getBean();
            String value = field.getFieldId().equals("Graduate Plan") ? student.getGradPlan() : student.getGradCode();
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
                String completerCode = student.getGradCode();

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
            VAToolStudent student = (VAToolStudent) entity.getBean();
            String gradeLevelOverride = student.getGradeLevelStateOverride();
            return gradeLevelOverride != null ? gradeLevelOverride : student.getGradeLevelState();
        }
    }

    /**
     * return fields from Homeless program.
     */
    protected class RetrieveHomeless extends RetrieveStudentProgram implements FieldValidator {
        private static final String CALC_PARAM_NIGHT_RESIDENCE = "Night Residence";
        private static final String CALC_PARAM_UNACCOMPANIED = "Unaccompanied";
        private static final String PROGRAM_CODE = "Homeless";

        /**
         * Instantiates a new retrieve homeless.
         *
         * @param data StateReportData
         */
        protected RetrieveHomeless(StateReportData data) {
            super(data, PROGRAM_CODE);
            setMatchOnReportDate(false);
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

            VAToolStudent student = (VAToolStudent) entity.getBean();
            if (CALC_PARAM_NIGHT_RESIDENCE.equals(field.getParameter())) {
                if (isAnyProgram(student)) {
                    String code = student.getNightResidence();
                    if (!StringUtils.isEmpty(code)) {
                        StateReportValidationError error = new StateReportValidationError(entity, field,
                                "If Student Program Participation exists for Homeless program, the student alias value ["
                                        +
                                        VAToolStudent.ALIAS_NIGHT_RESIDENCE + "] must be empty",
                                "[" + VAToolStudent.ALIAS_NIGHT_RESIDENCE + "]=" + STYLE_BOLD + code + STYLE_END);
                        errors.add(error);
                    }
                }
            } else if (CALC_PARAM_UNACCOMPANIED.equals(field.getParameter())) {
                if (isAnyProgram(student)) {
                    boolean isUnAccompanied = student.isUnaccompanied();
                    if (isUnAccompanied) {
                        StateReportValidationError error = new StateReportValidationError(entity, field,
                                "If Student Program Participation exists for Homeless program, the student alias value ["
                                        +
                                        VAToolStudent.ALIAS_UNACCOMPANIED + "] must be empty",
                                "[" + VAToolStudent.ALIAS_UNACCOMPANIED + "]=" + STYLE_BOLD + "true" + STYLE_END);
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
         * @see com.x2dev.procedures.statereporting.va.VAStudentRecordCollectionRevised.RetrieveStudentProgram#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            VAToolStudent student = (VAToolStudent) entity.getBean();
            if (CALC_PARAM_NIGHT_RESIDENCE.equals(field.getParameter())) {
                VAToolStudentProgramParticipation program = matchingProgram(student);
                if (program != null) {
                    value = program.getNightResidence();
                } else if (!isAnyProgram(student)) {
                    value = student.getNightResidence();
                }
            } else if (CALC_PARAM_UNACCOMPANIED.equals(field.getParameter())) {
                VAToolStudentProgramParticipation program = matchingProgram(student);
                if (program != null) {
                    value = program.isUnaccompanied();
                } else if (!isAnyProgram(student)) {
                    value = student.isUnaccompanied();
                }
            }
            return value;
        }

    }

    /**
     * The Class RetrieveIBFlag.
     */
    protected class RetrieveIBFlag implements FieldRetriever {
        private static final String IB_GRAD_COMPLETER_CODE = "6";

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
            VAToolStudent student = (VAToolStudent) entity.getBean();
            Boolean value =
                    IB_GRAD_COMPLETER_CODE.equals(entity.getFieldValue(FIELD_GRADUATE_COMP)) ? Boolean.TRUE : null;
            if (student.getIbFlag()) {
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
            Boolean immigrantStatus = null;
            PlainDate usEndDate = null;
            PlainDate usEntryDate = null;

            VAToolStudent student = (VAToolStudent) entity.getBean();
            usEntryDate = student.getUsEntryDate();
            usEndDate = student.getUsEndDate();
            immigrantStatus = student.hasImmigrantStatus();

            if ((immigrantStatus == Boolean.TRUE) && (usEndDate != null) && !usEndDate.before(m_reportDate)
                    && (usEntryDate != null) && !usEntryDate.after(m_reportDate)) {
                return Boolean.TRUE;
            }
            return Boolean.FALSE;
        }

    }

    /**
     * Retrieve LIEP.
     */
    protected class RetrieveLiep extends RetrieveStudentProgram implements FieldValidator {
        private static final String CALC_ID = "SRC-LIEP";
        private static final String PROGRAM_CODE = "ESL";

        /**
         * Instantiates a new retrieve foster care.
         *
         * @param data StateReportData
         */
        protected RetrieveLiep(StateReportData data) {
            super(data, PROGRAM_CODE);
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
            VAStudentRecordCollectionRevised src = (VAStudentRecordCollectionRevised) data;
            if (src.m_collection != null
                    && (COLLECTION_FALL.equals(src.m_collection) || COLLECTION_EOY.equals(src.m_collection)
                            || COLLECTION_SPRING.equals(src.m_collection))) {
                String elServidceCode = entity.getFieldValue(FIELD_ESL_SERVICES);
                if (!StringUtils.isEmpty(elServidceCode) && "1".equals(elServidceCode)
                        && StringUtils.isEmpty(value)) {
                    StateReportValidationError error = new StateReportValidationError(entity, field,
                            "Valid code is required if EL service code = 1 at any point during this year.",
                            "[" + VAToolStudentProgramParticipation.ALIAS_PGM_LIEP + "]=" + STYLE_BOLD + "is empty"
                                    + STYLE_END);
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
         * @see com.x2dev.procedures.statereporting.va.VAStudentRecordCollectionRevised.RetrieveStudentProgram#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            VAStudentRecordCollectionRevised src = (VAStudentRecordCollectionRevised) data;
            if (src.m_collection != null
                    && (COLLECTION_FALL.equals(src.m_collection) || COLLECTION_EOY.equals(src.m_collection)
                            || COLLECTION_SPRING.equals(src.m_collection))) {
                VAToolStudent student = (VAToolStudent) entity.getBean();
                VAToolStudentProgramParticipation program = matchingProgram(student);
                if (program != null) {
                    value = program.getLiep();
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
            VAToolStudent student = (VAToolStudent) entity.getBean();

            int totalRace = 0;
            Collection<ToolRace> races = student.getPersonRaces(data.getBroker());
            if (races != null) {
                for (ToolRace race : races) {
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
                Object value =
                        data.getDictionaryExtractor().getPropertyAsJavaType(entity.getBean(), field.getBeanPath());
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
            VAToolStudent student = (VAToolStudent) srcEntity.getBean();
            String status = "N";
            // use status "V" if "DOE MOP FLAG" is set for student and student enrollment "DOE
            // VIRTUAL PROGRAM" has state code 1
            boolean mopStudent = student.hasMopFlag();
            boolean virtualProgramStudent = false;
            if (mopStudent) {
                if (attendance.getEntryEnrollment() != null) {
                    String virtualProgramValue = attendance.getEntryEnrollment().getVirtualProgram();
                    if (!StringUtils.isEmpty(virtualProgramValue)) {
                        String virtualProgramStateCode = attendance.getEntryEnrollment()
                                .getValueReferenceState(VAToolEnrollment.FIELD_VIRTUAL_PROGRAM);
                        if ("1".equals(virtualProgramStateCode)) {
                            virtualProgramStudent = true;
                            status = "V";
                            if (attendance.m_exitEnrollment != null) {
                                String stateWithdrawalCode = attendance.m_exitEnrollment
                                        .getValueReferenceState(ToolEnrollment.FIELD_ENROLLMENT_CODE);
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
                            attendance.getEntryEnrollment().getValueReferenceState(ToolEnrollment.FIELD_STATUS_CODE);
                }

                // Find the end date, from the enrollment withdrawal or from report
                // date.
                if (attendance.getExitEnrollment() != null) {
                    endDate = attendance.getExitEnrollment().getEnrollmentDate();
                    if (StudentEnrollment.STATUS_CHANGE
                            .equals(attendance.getExitEnrollment().getEnrollmentType())) {
                        String hbEntryCode = attendance.getExitEnrollment().getHbEntryCode();
                        if (VAStudentRecordCollectionRevised.HB_ENTRY_CODES.contains(hbEntryCode)) {
                            endDate = DateUtils.add(endDate, -1);
                        }
                    }
                    endStatus = attendance.getExitEnrollment().getValueReferenceState(ToolEnrollment.FIELD_STATUS_CODE);
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
            VAToolStudent student = (VAToolStudent) entity.getBean();
            StudentRecordEntity srcEntity = (StudentRecordEntity) entity;

            String tuitionPaid = null;
            String value = null;
            String param = VAToolEnrollment.ALIAS_SERVICE_SCHOOL_CODE;

            tuitionPaid = student.getStudentTuition();

            if (StringUtils.isEmpty(tuitionPaid)) {
                if (PARAM_CURRENT_SCHOOL_STATUS.equals(entity.getFieldValue(FIELD_STATUS))) {
                    value = (String) srcEntity.getOverrideCodeValue(param);
                }

                if (StringUtils.isEmpty(value)) {
                    MembershipAttendance attendance = srcEntity.getMembershipAttendance();
                    try {
                        Map<String, Object> calcValueMap = lookupOverrides(student, attendance.getEntryEnrollment(),
                                attendance.getSchool().getOid());
                        value = (String) calcValueMap.get(param);
                    } catch (X2BaseException e) {
                        // ignore exception
                    }
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

                VAToolEnrollment enrollment = attendance.getEntryEnrollment();
                VAToolSchool school = (VAToolSchool) enrollment.getSchool(m_broker);
                if (school != null) {
                    tuitionPaid = school.getTuitionPaid();
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
            VAStudentRecordCollectionRevised srcData = (VAStudentRecordCollectionRevised) data;
            StudentRecordEntity srcEntity = (StudentRecordEntity) entity;
            MembershipAttendance attendance = srcEntity.getMembershipAttendance();
            VAToolEnrollment enrollment = attendance.getEntryEnrollment();

            String collection = srcData.m_collection;
            String value = null;
            if (VAToolEnrollment.FIELD_SPED_TUITION_DISABILITY.getAlias().equals(field.getParameter())) {
                value = enrollment.getSpedTuitionDisability();
            } else if (VAToolEnrollment.FIELD_SPED_TUITION_S1.getAlias().equals(field.getParameter())) {
                value = enrollment.getSpedTuitionS1();
            } else if (VAToolEnrollment.FIELD_SPED_TUITION_S2.getAlias().equals(field.getParameter())) {
                value = enrollment.getSpedTuitionS2();
            } else if (VAToolEnrollment.FIELD_SPED_TUITION_SUM.getAlias().equals(field.getParameter())) {
                value = enrollment.getSpedTuitionSum();
            }
            String fieldName = field.getFieldId();
            if (FIELD_SPED_TUIT_DISABILITY.equals(fieldName)) {
                String tuitionValue = null;
                if (COLLECTION_SPED.equals(collection)) {
                    tuitionValue = enrollment.getSpedTuitionSum();
                } else if (COLLECTION_SPRING.equals(collection)) {
                    tuitionValue = enrollment.getSpedTuitionS1();
                } else if (COLLECTION_EOY.equals(collection)) {
                    tuitionValue = enrollment.getSpedTuitionS2();
                }
                if (StringUtils.isEmpty(tuitionValue)) {
                    value = null;
                } else {
                    value = enrollment.getValueReferenceState(VAToolEnrollment.FIELD_SPED_TUITION_DISABILITY);
                }
            } else
            // FIELD_SPED_TUITION_S*
            {
                String tuitionPaid = enrollment.getSpedTuitionDisability();
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
         * @param data StateReportData
         */
        protected RetrievePGMRemoteLearning(StateReportData data) {
            super(data, PROGRAM_CODE);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.va.VAStudentRecordCollectionRevised.RetrieveStudentProgram#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
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
                VAToolStudent student = (VAToolStudent) entity.getBean();
                VAToolStudentProgramParticipation program = null;
                if (entryDate != null) {
                    program = getPrograms(student).stream()
                            .filter(pgm -> !testDate.before(pgm.getStartDate())
                                    && (pgm.getEndDate() == null || !testDate.after(pgm.getEndDate())))
                            .sorted(new Comparator<VAToolStudentProgramParticipation>() {

                                @Override
                                public int compare(VAToolStudentProgramParticipation o1,
                                                   VAToolStudentProgramParticipation o2) {
                                    // Decreasing order by start date
                                    return o2.getStartDate().compareTo(o1.getStartDate());
                                }

                            })
                            .findFirst().orElse(null);
                }
                if (program != null) {
                    String param = (String) field.getParameter();
                    if (PARENT_DEC.equals(param)) {
                        value = program.getParentalRemoteLearningDecision() ? "Y" : "N";
                    } else if (LOC_TIME.equals(param)) {
                        value = program.getRegionalLocalCenterPercentofTime();
                    } else if (REM_TIME.equals(param)) {
                        value = program.getRemoteInstructionPercentofTime();
                    } else if (INT_ACC.equals(param)) {
                        value = program.getInternetAccessforRemoteLearning();
                    } else if (DEV_ACC.equals(param)) {
                        value = program.getDeviceAccessforRemoteLearning();
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
            VAToolStudent student = (VAToolStudent) entity.getBean();
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
                for (ToolBean.ToolStudentSchedule studentSchedule : student.getStudentSchedules(data.getBroker())) {
                    // If student already found a course considered as non-traditional, the
                    // required
                    // objects are null,
                    // or the schedule is not for the current school year, skip the record
                    if (nonTraditional || studentSchedule.getSectionOid() == null) {
                        continue;
                    }
                    VAToolSection section  = (VAToolSection) studentSchedule.getSection(m_broker);
                    if (section == null) {
                        continue;
                    }
                    ToolBean.ToolSchedule schedule = section.getSchedule(m_broker);
                    if (StringUtils.isEmpty(section.getCourseView())
                            || schedule == null
                            || !schedule.getDistrictContextOid()
                                    .equals(getOrganization().getCurrentContextOid())) {
                        continue;
                    }
                    String traditionalGender = EMPTY_STRING;

                    if (section.getCareerAndTechEd() || section.getSedfReportFlag()) {
                        traditionalGender = section.getTraditionalGender();
                        if (!StringUtils.isEmpty(traditionalGender)) {
                            if (student.getGenderCode() == null) {
                                continue;
                            }
                            nonTraditional = student.getGenderCode().equals(traditionalGender);
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

                String ctePop = student.getCtePopulation();
                ctePop = data.getDictionaryExtractor().lookupStateValue(VAToolStudent.FIELD_CTE_POPULATION, ctePop);
                if (ctePop == null) {
                    ctePop = EMPTY_STRING;
                }
                value = (value != null) ? value + ctePop : ctePop;
                if (value != null && value.startsWith("44")) {
                    value = value.replaceFirst("44", "4");
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
            /*
             * TODO: can't find NOSPRING calculation id in export-format-EXPDATA-VA-SRC.xml, so that
             * impossible to
             * figure out enumeration of possible "param" values for
             * student.getFieldValueByAlias(param) call
             *
             * if (!COLLECTION_SPRING.equals(m_collection)) {
             * String param = (String) field.getParameter();
             * VAToolStudent student = (VAToolStudent) entity.getBean();
             * value = (String) student.getFieldValueByAlias(param);
             * }
             */
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
            VAToolStudent student = (VAToolStudent) entity.getBean();
            Double value = null;
            if (field.getBeanPath().charAt(0) != StateReportData.LABEL_PREFIX_CHAR) {
                value = student.getSpedPct();
            }
            return value != null && value.doubleValue() > 0 ? value : EMPTY_STRING;
        }
    }


    /**
     * Used to facilitate retrievers based on student program participation.
     */

    protected class RetrieveStudentProgram implements FieldRetriever {
        private boolean m_matchOnReportDate = true;
        private Filterable<VAToolStudentProgramParticipation> m_filterable;

        /**
         * Instantiates a new retrieve student program.
         *
         * @param data StateReportData
         * @param stateCode String
         */
        protected RetrieveStudentProgram(StateReportData data, String stateCode) {
            Set<String> programCodes = new HashSet();
            programCodes.add("---dummy---");
            DataDictionaryField field =
                    ToolStudentProgramParticipation.FIELD_PROGRAM_CODE.getField(data.getDictionaryExtractor());
            if (field != null && field.getReferenceTableOid() != null) {
                Map<String, ReferenceCode> programCodesMap =
                        data.getDictionaryExtractor().getReferenceCodes(field.getReferenceTableOid());
                for (ReferenceCode code : programCodesMap.values()) {
                    if (stateCode.equals(code.getStateCode())) {
                        programCodes.add(code.getCode());
                    }
                }
            }
            CollectionCriteriaHelper helper = null;
            try {
                Set<String> studentOids = ToolBean.getCachedToolBeanOids(VAToolStudent.class);

                X2Criteria criteria = new X2Criteria();
                X2Criteria idCriteria = new X2Criteria();
                if (studentOids.size() > MAX_SAFE_PARAMETERS) {
                    helper = new CollectionCriteriaHelper(studentOids, data.getBroker());
                    helper.applyToCriteria(ToolStudentProgramParticipation.FIELD_STUDENT_OID.resolve(null), idCriteria);
                } else {
                    idCriteria.addIn(ToolStudentProgramParticipation.FIELD_STUDENT_OID.resolve(null), studentOids);
                }
                criteria.addAndCriteria(idCriteria);
                criteria.addIn(ToolStudentProgramParticipation.FIELD_PROGRAM_CODE.resolve(null), programCodes);
                criteria.addNotNull(ToolStudentProgramParticipation.FIELD_START_DATE.resolve(null));
                m_filterable = FilterableFactory.create(data.getBroker(), getDictionaryExtractor(),
                        VAToolStudentProgramParticipation.class, criteria, null);
            } finally {
                if (helper != null) {
                    helper.cleanup();
                }
            }

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
         * @param std VAToolStudent
         * @return List
         */
        protected List<VAToolStudentProgramParticipation> getPrograms(VAToolStudent std) {
            List<VAToolStudentProgramParticipation> programs =
                    m_filterable.getGroup(VAToolStudentProgramParticipation.FIELD_STUDENT_OID, std.getOid());
            return programs == null ? Collections.EMPTY_LIST : programs;
        }

        /**
         * Checks if is any program.
         *
         * @param std VAToolStudent
         * @return true, if is any program
         */
        protected boolean isAnyProgram(VAToolStudent std) {
            return !getPrograms(std).isEmpty();
        }

        /**
         * Matching program.
         *
         * @param std VAToolStudent
         * @return StudentProgramParticipation
         */
        protected VAToolStudentProgramParticipation matchingProgram(VAToolStudent std) {
            VAToolStudentProgramParticipation program = null;
            List<VAToolStudentProgramParticipation> programs = getPrograms(std);
            if (programs != null && !programs.isEmpty()) {
                PlainDate latestStartDate = null;
                for (VAToolStudentProgramParticipation pgm : programs) {
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
                        if ((pgm.getStartDate() == null || !pgm.getStartDate().after(getCurrentContext().getEndDate()))
                                && (pgm.getEndDate() == null
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
            VAToolEnrollment enrollment = attendance.getEntryEnrollment();
            String virtualProgramStatus = enrollment.getVirtualProgram();
            if (!StringUtils.isEmpty(virtualProgramStatus)) {
                value = enrollment.getValueReferenceState(VAToolEnrollment.FIELD_VIRTUAL_PROGRAM);
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
                VAToolStudent student = (VAToolStudent) entity.getBean();

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
                    String careerCluster = student.getCareerCluster();
                    if (careerCluster != null) {
                        value = data.getDictionaryExtractor().lookupStateValue(VAToolStudent.FIELD_CAREER_CLUSTER,
                                careerCluster);
                    } else {
                        ArrayList<String> clusterCodes = new ArrayList<String>();
                        DistrictSchoolYearContext districtContext = getOrganization().getCurrentContext();
                        Collection<ToolBean.ToolStudentScheduleChange> studentScheduleChanges = null;
                        for (ToolBean.ToolStudentSchedule studentSchedule : student.getStudentSchedules(m_broker)) {
                            VAToolSection section = (VAToolSection) studentSchedule.getSection(m_broker);
                            if (section != null && !StringUtils.isEmpty(section.getCourseView())) {
                                // Skip courses for schools that are excluded from
                                // reporting
                                ToolBean.ToolSchedule schedule = section.getSchedule(m_broker);
                                boolean isSchoolExcluded = false;
                                VAToolSchool school = (VAToolSchool) schedule.getSchool(m_broker);
                                if (school != null) {
                                    isSchoolExcluded = school.isExcluded();
                                }

                                if (!isSchoolExcluded) {
                                    // Only consider courses for the current school
                                    // year
                                    ToolBean.ToolDistrictContext courseContext = schedule.getDistrictContext(m_broker);
                                    if (courseContext.getOid().equals(districtContext.getOid())) {
                                        careerCluster = section.getCteCareerCluster();
                                        if (!StringUtils.isEmpty(careerCluster)) {
                                            if (studentScheduleChanges == null) {
                                                studentScheduleChanges =
                                                        student.getStudentScheduleChanges(m_broker);
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
         * @param section VAToolSection
         * @param studentScheduleChanges Collection<StudentScheduleChange>
         * @return true, if is dropped course
         */
        private boolean isDroppedCourse(VAToolSection section,
                                        Collection<ToolBean.ToolStudentScheduleChange> studentScheduleChanges) {
            if (StringUtils.isEmpty(section.getCourseView())) {
                return false;
            }
            boolean isDropped = false;
            PlainDate latestEnrollDate = null;
            PlainDate latestDropDate = null;

            for (ToolBean.ToolStudentScheduleChange studentScheduleChange : studentScheduleChanges) {
                VAToolSection studentScheduleSection = (VAToolSection) studentScheduleChange.getSection(m_broker);
                if (section.getOid().equals(studentScheduleChange.getSectionOid())) {
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

            VAStudentRecordCollectionRevised src = (VAStudentRecordCollectionRevised) data;
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

            VAStudentRecordCollectionRevised src = (VAStudentRecordCollectionRevised) data;
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

            VAStudentRecordCollectionRevised src = (VAStudentRecordCollectionRevised) data;
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

            VAStudentRecordCollectionRevised src = (VAStudentRecordCollectionRevised) data;
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

            VAStudentRecordCollectionRevised src = (VAStudentRecordCollectionRevised) data;
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

        ToolBean.setDictionaryExtractor(getDictionaryExtractor());
        ToolBean.registerClass(VAToolSchool.class);
        ToolBean.registerClass(VAToolStudent.class);
        ToolBean.registerClass(VAToolStudentSchedule.class);
        ToolBean.registerClass(VAToolSection.class);
        ToolBean.registerClass(VAToolCourse.class);
        ToolBean.registerClass(VAToolConductAction.class);
        ToolBean.registerClass(VAToolEnrollment.class);
        ToolBean.registerClass(VAStudentAssessment.class);
        ToolBean.registerClass(ToolStudentProgramParticipation.class);

        initializeFields();

        if (getSetupErrors().size() == 0) {

            m_sort = (Integer) getParameter(PARAM_SORT);
            /*
             * Build the criteria/query for the staff to include in this export based on user input.
             */
            /*
             * X2Criteria studentCriteria = getStudentCriteria();
             * QueryByCriteria studentQuery = new QueryByCriteria(VAToolStudent.class,
             * studentCriteria);
             * switch (m_sort != null ? m_sort.intValue() : 0) {
             * case 0: // Name
             * studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
             * break;
             *
             * case 1: // YOG
             * studentQuery.addOrderByAscending(SisStudent.COL_YOG);
             * studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
             * break;
             *
             * case 2: // School
             * studentQuery.addOrderByAscending(SisStudent.REL_SCHOOL + PATH_DELIMITER +
             * SisSchool.COL_NAME);
             * studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
             * break;
             *
             * case 3: // LASID
             * studentQuery.addOrderByAscending(SisStudent.COL_LOCAL_ID);
             * break;
             *
             * case 4: // SASID
             * studentQuery.addOrderByAscending(SisStudent.COL_STATE_ID);
             * break;
             *
             * default:
             * studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
             * break;
             * }
             *
             * // Special sort order for prereg
             * if (COLLECTION_PRESUB.equals(m_collection) && ((m_sort != null) && (m_sort.intValue()
             * != 3))) {
             * studentQuery.addOrderBy(Student.COL_LOCAL_ID, true);
             * }
             *
             * // Set the query to be used for student selection.
             * setQuery(studentQuery);
             */
            setEntityClass(StudentRecordEntity.class);

            Filterable<VAToolStudent> filterable = getStudentFilterable();
            setFilterable(filterable);

            loadEnrollments();
            loadAttendance();
            loadSchoolsAndOverrides();


            loadAddresses();
            loadAssessments();
            loadConductActions();
            loadRaceCodes();
            loadStudentSchedules();
            loadTranscipts();

            // Add any retrievers or validators.
            RetrieveHomeless homelessRetriever = new RetrieveHomeless(this);
            RetrieveEnglishLearners elRetriver = new RetrieveEnglishLearners(this);
            RetrieveLiep liepRetriver = new RetrieveLiep(this);

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
            calcs.put("SRC-FOSTER", new RetrieveFosterCare(this));
            calcs.put("SRC-PGM-REMOTE", new RetrievePGMRemoteLearning(this));
            calcs.put("SRC-HOMELESS", homelessRetriever);
            calcs.put("SRC-EL", elRetriver);
            calcs.put(RetrieveUnscheduledDays.CALC_ID, new RetrieveUnscheduledDays());
            calcs.put(RetrieveCodes.CALC_ID, new RetrieveCodes());
            calcs.put(RetrieveLiep.CALC_ID, liepRetriver);
            calcs.put(RetrieveAsmLAVC.CALC_ID, new RetrieveAsmLAVC());
            calcs.put(RetrieveConductAction.CALC_ID, new RetrieveConductAction());

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
     * Gets the filterable.
     *
     * @return the filterable
     */
    public Filterable<VAToolStudent> getStudentFilterable() {
        X2Criteria criteria = getStudentCriteria();

        // Apply criteria from tool input.
        applyInputCriteria(criteria, false, null);

        List<ToolBean.ToolBeanColumn> columns = new ArrayList<>();
        switch (m_sort != null ? m_sort.intValue() : 0) {
            case 0: // Name
                columns.add(ToolStudent.FIELD_NAME_VIEW);
                break;

            case 1: // YOG
                columns.add(ToolStudent.FIELD_YOG);
                columns.add(ToolStudent.FIELD_NAME_VIEW);
                break;

            case 2: // School
                columns.add(VAToolStudent.FIELD_SCHOOL_NAME);
                columns.add(ToolStudent.FIELD_NAME_VIEW);
                break;

            case 3: // LASID
                columns.add(ToolStudent.FIELD_LOCAL_ID);
                break;

            case 4: // SASID
                columns.add(ToolStudent.FIELD_STATE_ID);
                break;

            default:
                columns.add(ToolStudent.FIELD_LOCAL_ID);
                break;
        }

        // Special sort order for prereg
        if (COLLECTION_PRESUB.equals(m_collection) && ((m_sort != null) && (m_sort.intValue() != 3))) {
            columns.add(ToolStudent.FIELD_LOCAL_ID);
        }

        return FilterableFactory.create(getBroker(), VAToolStudent.class, criteria, columns);
    }

    /**
     * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData#open()
     */
    @Override
    public boolean open() {
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

        return super.open();
    }

    /**
     * Returns the days-in-session for the given school and calendar combination.
     *
     * @param school SisSchool
     * @param calendar String
     * @return Set of PlainDate objects
     */
    protected Set getCalendarDays(VAToolSchool school, String calendar) {
        Map<String, Set<PlainDate>> calendarData = m_schoolsToCalendars.get(school.getOid());
        if (calendarData == null) {
            calendarData = school.getCalendarsByContext(m_broker).stream()
                    .collect(Collectors.toMap(ToolSchoolCalendar::getCalendarId,
                            cas -> cas.getDatesInSchedule(m_broker).stream()
                                    .filter(date -> !date.after(m_reportDate))
                                    .collect(Collectors.toSet())));
            m_schoolsToCalendars.put(school.getOid(), calendarData);
        }

        Set<PlainDate> set = calendarData.get(calendar);
        if (set == null) {
            set = calendarData.get(DEFAULT_CALENDAR_NAME);
            if (set == null) {
                if (!calendarData.values().isEmpty()) {
                    set = calendarData.values().iterator().next();
                }
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
    protected PlainDate getFirstSchoolInSessionDate(ToolSchool school, String calendarId) {
        calendarId = StringUtils.isEmpty(calendarId) ? DEFAULT_CALENDAR_NAME : calendarId;
        String calendarKey = school.getOid() + calendarId;
        if (!m_firstDaysOfSchools.containsKey(calendarKey)) {
            ToolBean.ToolSchoolCalendar actualCalendar = null;
            while (actualCalendar == null) {
                for (ToolBean.ToolSchoolCalendar calendar : school.getCalendars(getBroker()).extract()) {
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
                firstInSessionDate = actualCalendar.findFirstInSessionDate(m_broker, null, true);
            } else {
                firstInSessionDate = m_districtStartDate;
            }
            m_firstDaysOfSchools.put(calendarKey, firstInSessionDate);
        }
        return m_firstDaysOfSchools.get(calendarKey);
    }

    /**
     * Lookup enrollment codes and dates.
     *
     * @param student ToolStudent
     * @param enrollment StudentEnrollment
     * @param schoolOid String
     * @return Map
     * @throws X2BaseException exception
     */
    protected Map<String, Object> lookupOverrides(ToolStudent student,
                                                  VAToolEnrollment enrollment,
                                                  String schoolOid)
            throws X2BaseException {
        Map<String, Object> calcValueMap = new HashMap<String, Object>();
        VAToolSchool school = null;
        if (enrollment != null) {
            school = (VAToolSchool) enrollment.getSchool(getBroker());
        }
        if (school == null) {
            school = ToolBean.getBeanByOid(getBroker(), VAToolSchool.class, schoolOid);
        }
        if (school != null) {
            String homeDistrictCode = null;
            String serviceDistrictCode = null;
            String homeSchoolCode = null;
            String serviceSchoolCode = null;
            DictionaryExtractor extractor = ToolBean.getDictionaryExtractor();
            // Get override school codes if they exist.
            if (enrollment != null) {
                homeDistrictCode = enrollment.getHomeDistrictCode();
                serviceDistrictCode = enrollment.getServiceDistrictCode();
                homeSchoolCode = enrollment.getHomeSchoolCode();
                serviceSchoolCode = enrollment.getServiceSchoolCode();
            }

            // Apply default value to school and district codes.
            if (!StringUtils.isEmpty(homeDistrictCode)) {
                String stateCode =
                        extractor.lookupStateValue(VAToolEnrollment.FIELD_HOME_DISTRICT_CODE, homeDistrictCode);
                if (!StringUtils.isEmpty(stateCode)) {
                    homeDistrictCode = stateCode;
                }
            } else {
                homeDistrictCode = (String) getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode);
            }

            if (!StringUtils.isEmpty(serviceDistrictCode)) {
                String stateCode =
                        extractor.lookupStateValue(VAToolEnrollment.FIELD_SERVICE_DISTRICT_CODE, serviceDistrictCode);
                if (!StringUtils.isEmpty(stateCode)) {
                    serviceDistrictCode = stateCode;
                }
            } else {
                serviceDistrictCode = (String) getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode);
            }

            // Get intermediate override from student "Home School" field, if
            // present.
            if (!StringUtils.isEmpty(homeSchoolCode)) {
                String stateCode = extractor.lookupStateValue(VAToolEnrollment.FIELD_HOME_SCHOOL_CODE, homeSchoolCode);
                if (!StringUtils.isEmpty(stateCode)) {
                    homeSchoolCode = stateCode;
                }
            } else {
                VAToolStudent vaStudent = (VAToolStudent) student;
                homeSchoolCode = vaStudent.getHomeSchoolCode();
                if (!StringUtils.isEmpty(homeSchoolCode)) {
                    homeSchoolCode = extractor.lookupStateValue(VAToolStudent.FIELD_HOME_SCHOOL_CODE,
                            homeSchoolCode);
                }
                if (StringUtils.isEmpty(homeSchoolCode) && (enrollment != null)) {
                    homeSchoolCode = ((VAToolSchool) enrollment.getSchool(getBroker())).getCode();
                }
                if (StringUtils.isEmpty(homeSchoolCode)) {
                    homeSchoolCode = vaStudent.getHomeSchoolCode();
                }
            }

            if (!StringUtils.isEmpty(serviceSchoolCode)) {
                String stateCode =
                        extractor.lookupStateValue(VAToolEnrollment.FIELD_SERVICE_SCHOOL_CODE, serviceSchoolCode);
                if (!StringUtils.isEmpty(stateCode)) {
                    serviceSchoolCode = stateCode;
                }
            } else if (enrollment != null) {
                serviceSchoolCode = ((VAToolSchool) enrollment.getSchool(getBroker())).getCode();
            } else {
                serviceSchoolCode = school.getCode();
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
            calcValueMap.put(VAToolEnrollment.ALIAS_HOME_DISTRICT_CODE, homeDistrictCode);
            calcValueMap.put(VAToolEnrollment.ALIAS_SERVICE_DISTRICT_CODE, serviceDistrictCode);
            calcValueMap.put(VAToolEnrollment.ALIAS_HOME_SCHOOL_CODE, homeSchoolCode);
            calcValueMap.put(VAToolEnrollment.ALIAS_SERVICE_SCHOOL_CODE, serviceSchoolCode);
        }
        return calcValueMap;
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
    private X2Criteria getStudentCriteria() {
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        /*
         * First build the criteria based on the user's input
         */
        X2Criteria userCriteria = new X2Criteria();

        if (m_preregOnly) {
            userCriteria.addEqualTo(SisBeanPaths.STUDENT.enrollmentStatus().getPath(), m_preRegCode);
            // Check school context.
            if (isSchoolContext()) {
                userCriteria.addEqualTo(SisBeanPaths.STUDENT.schoolOid().getPath(), getSchool().getOid());
            } else {
                userCriteria.addNotEqualTo(SisBeanPaths.STUDENT.school().getPath()
                        + PATH_DELIMITER + SisBeanPaths.SCHOOL.inactiveIndicator().getPath(),
                        Boolean.TRUE);
                userCriteria.addNotEqualTo(SisBeanPaths.STUDENT.school().getPath() + PATH_DELIMITER +
                        SisBeanPaths.SCHOOL.archiveIndicator().getPath(),
                        Boolean.TRUE);
                userCriteria.addNotEqualTo(
                        SisBeanPaths.STUDENT.school().getPath() + PATH_DELIMITER
                                + VAToolSchool.FIELD_EXCLUDE.resolve(null),
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
            enrollmentCriteria.addGreaterOrEqualThan(SisBeanPaths.STUDENT_ENROLLMENT.enrollmentDate().getPath(),
                    m_summerBeginDate);
            if (isSchoolContext()) {
                enrollmentCriteria.addEqualTo(SisBeanPaths.STUDENT_ENROLLMENT.schoolOid().getPath(),
                        getSchool().getOid());
            } else {
                enrollmentCriteria.addNotEqualTo(SisBeanPaths.STUDENT_ENROLLMENT.school().getPath()
                        + PATH_DELIMITER + SisBeanPaths.SCHOOL.inactiveIndicator().getPath(),
                        Boolean.TRUE);
                enrollmentCriteria.addNotEqualTo(
                        SisBeanPaths.STUDENT_ENROLLMENT.school().getPath() + PATH_DELIMITER
                                + SisBeanPaths.SCHOOL.archiveIndicator().getPath(),
                        Boolean.TRUE);
                enrollmentCriteria.addNotEqualTo(
                        SisBeanPaths.STUDENT_ENROLLMENT.school().getPath() + PATH_DELIMITER
                                + VAToolSchool.FIELD_EXCLUDE.resolve(null),
                        BooleanAsStringConverter.TRUE);
            }

            SubQuery enrollmentQuery = new SubQuery(SisBeanPaths.STUDENT_ENROLLMENT.getBeanType(),
                    SisBeanPaths.STUDENT_ENROLLMENT.studentOid().getPath(), enrollmentCriteria);
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
            activeCriteria2.addIn(SisBeanPaths.STUDENT.enrollmentStatus().getPath(), statuses);

            // Check school context.
            if (isSchoolContext()) {
                activeCriteria2.addEqualTo(SisBeanPaths.STUDENT.school().getPath(), getSchool().getOid());
            } else {
                activeCriteria2.addNotEqualTo(
                        SisBeanPaths.STUDENT.school().getPath() + PATH_DELIMITER
                                + SisBeanPaths.SCHOOL.inactiveIndicator().getPath(),
                        Boolean.TRUE);
                activeCriteria2.addNotEqualTo(
                        SisBeanPaths.STUDENT.school().getPath() + PATH_DELIMITER
                                + SisBeanPaths.SCHOOL.archiveIndicator().getPath(),
                        Boolean.TRUE);
                activeCriteria2.addNotEqualTo(
                        SisBeanPaths.STUDENT.school().getPath() + PATH_DELIMITER
                                + VAToolSchool.FIELD_EXCLUDE.resolve(null),
                        BooleanAsStringConverter.TRUE);
            }

            activeCriteria3.addIn(X2BaseBean.COL_OID, enrollmentQuery);

            activeCriteria1.addOrCriteria(activeCriteria2);
            activeCriteria1.addOrCriteria(activeCriteria3);

            userCriteria.addAndCriteria(activeCriteria1);

            // Check the exclude student flag.
            userCriteria.addNotEqualTo(VAToolStudent.FIELD_EXCLUDE.resolve(null), BooleanAsStringConverter.TRUE);
        }

        // Optionally, report only students who are missing a SASID.
        if (m_missingSasid) {
            userCriteria.addEmpty(VAToolStudent.FIELD_SASID.resolve(null), getBroker().getPersistenceKey());
        }

        // For December 1/SPED report, include only students with SPED Placement
        // code.
        if (COLLECTION_SPED.equals(m_collection)) {
            userCriteria.addNotEmpty(VAToolStudent.FIELD_SPED_DISABILITY.resolve(null),
                    getBroker().getPersistenceKey());
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
            spedStatusCriteria.addEqualTo(SisBeanPaths.STUDENT.spedStatusCode().getPath(), spedActiveCode);
            X2Criteria orCriteria = new X2Criteria();
            orCriteria.addGreaterThan(SisBeanPaths.STUDENT.spedExitDate().getPath(), m_reportDate);
            spedStatusCriteria.addOrCriteria(orCriteria);
            userCriteria.addAndCriteria(spedStatusCriteria);
        }

        // Check user selection criteria.
        switch (queryBy) {
            case 1: // YOG
                userCriteria.addEqualTo(SisBeanPaths.STUDENT.yog().getPath(), queryString);
                break;

            case 2: // LASID
                userCriteria.addEqualTo(SisBeanPaths.STUDENT.localId().getPath(), queryString);
                break;

            case 3: // SASID
                userCriteria.addEqualTo(SisBeanPaths.STUDENT.stateId().getPath(), queryString);
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
        DataDictionaryField enrollmentStatusfield =
                ToolStudent.FIELD_ENROLLMENT_STATUS.getField(getDictionaryExtractor());;
        Map<String, ReferenceCode> entryMap =
                getDictionaryExtractor().getReferenceCodes(enrollmentStatusfield.getReferenceTableOid());
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
        String fieldGed = translateAliasToJavaName("DOE GED", true);
        DataDictionaryField gedDDField = getDictionaryExtractor().getDataDictionaryField(ToolStudent.class, fieldGed);
        if ((gedDDField != null) && (gedDDField.getReferenceTableOid() != null)) {
            Map<String, ReferenceCode> codes =
                    getDictionaryExtractor().getReferenceCodes(gedDDField.getReferenceTableOid());
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
    }

    /**
     * Load addresses.
     */
    private void loadAddresses() {
        List<String> addressOids = ToolBean.getCachedToolBeans(VAToolStudent.class).stream()
                .map(std -> std.getPhysicalAddressOid())
                .collect(Collectors.toList());
        ToolBean.loadByOid(getBroker(), getDictionaryExtractor(), ToolAddress.class, addressOids);
    }

    /**
     * Load attendance.
     */
    private void loadAssessments() {
        // Preload assessments
        X2Criteria criteria = new X2Criteria();
        criteria.addGreaterOrEqualThan(VAStudentAssessment.FIELD_DATE.resolve(getDictionaryExtractor()),
                getCurrentContext().getStartDate());
        criteria.addLessOrEqualThan(VAStudentAssessment.FIELD_DATE.resolve(getDictionaryExtractor()),
                getCurrentContext().getEndDate());
        ToolBean.addAndCriteria(getBroker(), VAStudentAssessment.class, criteria);
        ToolBean.preload(getBroker(), getDictionaryExtractor(), Arrays.asList(VAStudentAssessment.FIELD_DATE),
                VAToolStudent.CHILD_STUDENT_ASSESSMENTS);
    }

    /**
     * Load conduct actions.
     */
    private void loadConductActions() {
        // Preload conduct actions
        X2Criteria criteria = new X2Criteria();
        criteria.addGreaterOrEqualThan(ToolConductAction.FIELD_ACTION_START_DATE.resolve(getDictionaryExtractor()),
                getCurrentContext().getStartDate());
        criteria.addLessOrEqualThan(ToolConductAction.FIELD_ACTION_START_DATE.resolve(getDictionaryExtractor()),
                getCurrentContext().getEndDate());

        X2Criteria propsCriteria = new X2Criteria();
        propsCriteria.addEqualTo(VAToolConductAction.FIELD_SECLUSION.resolve(getDictionaryExtractor()),
                Boolean.TRUE);
        propsCriteria.addOrEqualTo(VAToolConductAction.FIELD_RESTRAINT.resolve(getDictionaryExtractor()),
                Boolean.TRUE);

        criteria.addAndCriteria(propsCriteria);

        ToolBean.addAndCriteria(getBroker(), VAToolConductAction.class, criteria);
        ToolBean.preload(getBroker(), getDictionaryExtractor(),
                Arrays.asList(ToolConductAction.FIELD_ACTION_START_DATE),
                ToolStudent.CHILD_STUDENT_CONDUCT_ACTIONS);
    }

    /**
     * Load attendance.
     */
    private void loadAttendance() {
        // Preload attendance
        X2Criteria attendanceCriteria = new X2Criteria();
        attendanceCriteria.addGreaterOrEqualThanField(ToolStudentAttendance.FIELD_DATE.resolve(null),
                SisBeanPaths.STUDENT_ATTENDANCE.school().activeSchoolSched().activeSchedule().startDate().getPath());
        attendanceCriteria.addLessOrEqualThan(ToolStudentAttendance.FIELD_DATE.resolve(null), m_reportDate);
        ToolBean.addAndCriteria(getBroker(), ToolStudentAttendance.class, attendanceCriteria);
        ToolBean.preload(getBroker(), getDictionaryExtractor(), null, ToolStudent.CHILD_STUDENT_ATTENDANCE);
    }

    /**
     * Load enrollments.
     */
    private void loadEnrollments() {
        // Preload enrollments
        List<String> enrollmentTypes = Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.WITHDRAWAL,
                StudentEnrollment.STATUS_CHANGE);
        X2Criteria enrollmentCriteria = new X2Criteria();

        enrollmentCriteria.addIn(SisBeanPaths.STUDENT_ENROLLMENT.enrollmentType().getPath(), enrollmentTypes);
        ToolBean.addAndCriteria(getBroker(), VAToolEnrollment.class, enrollmentCriteria);
        ToolBean.preload(getBroker(), getDictionaryExtractor(),
                Arrays.asList(ToolEnrollment.FIELD_DATE_DESC, ToolEnrollment.FIELD_TIMESTAMP_DESC),
                ToolStudent.CHILD_STUDENT_ENROLLMENTS);
    }

    /**
     * Load maps for race codes reference codes and person race for students.
     */
    private void loadRaceCodes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);

        // Get race code reference codes for use in the race retriever.
        Criteria raceCriteria = new Criteria();
        raceCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, raceCriteria);
        m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 5);

        ToolBean.preload(getBroker(), getDictionaryExtractor(), null, ToolStudent.CHILD_PERSON_RACES);
    }

    /**
     * Load schools and overrides.
     */
    private void loadSchoolsAndOverrides() {
        // loadSchoolOverrides
        ToolBean.load(getBroker(), getDictionaryExtractor(), VAToolSchool.class);
        m_schoolOverride = ToolBean.getCachedToolBeans(VAToolSchool.class).stream()
                .filter(VAToolSchool::getDoeSrcOverrideIndicator)
                .map(VAToolSchool::getCode)
                .collect(Collectors.toList());
    }

    /**
     * Load a map of sets of student schedule records by student oid for the students in the
     * export.
     */
    private void loadStudentSchedules() {
        X2Criteria scheduleCriteria = new X2Criteria();
        String cskPrefix = SisBeanPaths.STUDENT_SCHEDULE.section().schoolCourse().getPath()
                + ModelProperty.PATH_DELIMITER;

        // Master type Class
        scheduleCriteria.addEqualTo(cskPrefix + ToolSection.FIELD_CSK_MASTER_TYPE.resolve(getDictionaryExtractor()),
                SchoolCourse.MASTER_TYPE_CLASS);

        // Join active Schedule
        scheduleCriteria.addEqualToField(
                SisBeanPaths.STUDENT_SCHEDULE.schedule().school().schoolScheduleContexts().activeScheduleOid()
                        .getPath(),
                ToolStudentSchedule.FIELD_SCHEDULE_OID.resolve(getDictionaryExtractor()));

        ToolBean.addAndCriteria(getBroker(), VAToolStudentSchedule.class, scheduleCriteria);
        ToolBean.preload(getBroker(), getDictionaryExtractor(), null, ToolStudentSchedule.PARENT_STUDENT);

        List<String> sectionOids = ToolBean.getCachedToolBeans(VAToolStudentSchedule.class).stream()
                .map(ssc -> ssc.getSectionOid())
                .collect(Collectors.toList());
        ToolBean.loadByOid(getBroker(), getDictionaryExtractor(), VAToolSection.class, sectionOids);

        List<String> courseOids = ToolBean.getCachedToolBeans(VAToolSection.class).stream()
                .map(mst -> mst.getCourseOid())
                .collect(Collectors.toList());
        ToolBean.loadByOid(getBroker(), getDictionaryExtractor(), VAToolCourse.class, courseOids);
    }

    /**
     * Load a map of student transcripts for the students in the export.
     */
    private void loadTranscipts() {
        ToolBean.preload(getBroker(), getDictionaryExtractor(), null, ToolTranscript.PARENT_STUDENT);
    }

    /**
     * Translate alias to java name.
     *
     * @param alias String
     * @param required boolean
     * @return String
     */
    private String translateAliasToJavaName(String alias, boolean required) {
        return m_dictionaryExtractor.translateAliasToJavaName(alias, required);
    }

}
