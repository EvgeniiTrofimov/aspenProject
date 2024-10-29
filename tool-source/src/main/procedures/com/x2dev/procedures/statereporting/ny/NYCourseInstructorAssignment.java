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
package com.x2dev.procedures.statereporting.ny;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * New York state procedure for Staff Student Course export.
 *
 * @author X2 Development Corporation
 */

public class NYCourseInstructorAssignment extends StateReportData {
    // static Logger m_logger = AppGlobals.getLog();

    /**
     * Entity class for Staff Student Course export.
     *
     * @author X2 Development Corporation
     */

    public static class StaffStudentCourseEntity extends StateReportEntity {
        /**
         * Local variables for reporting information.
         */
        private NYCourseInstructorAssignment m_data;
        private List<StaffCourseRecord> m_staffCourseRecords;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StaffStudentCourseEntity() {
            // no argument constructor
        }

        public final double DEFAULT_LINK_VALUE = 1.00;
        public final String STRING_NO = "N";

        /**
         * This method returns the current staff student course record.
         *
         * @return StaffCourseRecord
         */
        public StaffCourseRecord getStaffCourseRecord() {
            return m_staffCourseRecords.get(getCurrentRow());
        }

        /**
         * Gets the staff course records.
         *
         * @return the m_staffStudentCourseRecords
         */
        public List<StaffCourseRecord> getStaffCourseRecords() {
            return m_staffCourseRecords;
        }

        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_data = (NYCourseInstructorAssignment) data;

            ScheduleTeacher mtc = (ScheduleTeacher) bean;

            m_staffCourseRecords = new ArrayList<StaffCourseRecord>();

            MasterSchedule masterSchedule = mtc.getSection();
            String sectionOid = masterSchedule.getOid();

            if (masterSchedule != null && !filterCourse(masterSchedule)) {
                if (!m_data.m_termCodeMap.containsKey(masterSchedule.getOid())) {
                    populateStartEndDateAndTermCodeForMasterSchedule(masterSchedule);
                }

                String termCode = m_data.m_termCodeMap.get(sectionOid);

                if (!m_data.m_masterScheduleSchoolMap.containsKey(sectionOid)) {
                    populateMasterScheduleMaps(masterSchedule);
                }

                // Course info
                Course course = m_data.m_masterScheduleCourseMap.get(sectionOid);

                String courseNumber = "";
                if (course != null) {
                    courseNumber = course.getNumber();
                }

                // Section School info
                SisSchool school = m_data.m_masterScheduleSchoolMap.get(sectionOid);

                String schoolLocationCode = "";
                if (school != null) {
                    // FIELD 7: Course Location Code
                    schoolLocationCode = (String) school.getFieldValueByBeanPath(m_data.m_schoolLocationCodeField);
                }

                // FIELD 10: Course Code
                String sectionCode = calcSectionCode(masterSchedule);

                Collection<TeacherSpan> spans = getTeacherSpans(masterSchedule);

                if (spans != null && !spans.isEmpty()) {
                    for (TeacherSpan span : spans) {
                        PlainDate relStartDate = span.getSpanInterval().getStartDate();
                        PlainDate relEndDate = span.getSpanInterval().getEndDate();

                        // Skip the assessment record if the student started
                        // after the assessment of note.
                        if (m_data.m_assessmentAliasDate != null) {
                            if (!m_data.m_reportDate.after(relStartDate)) {
                                continue;
                            }
                        }

                        if (!m_data.m_today.after(relStartDate)) {
                            continue;
                        }

                        StaffCourseRecord staffStudentCrsRecord = m_data.new StaffCourseRecord();
                        staffStudentCrsRecord.setSectionOid(masterSchedule.getOid());
                        // FIELD 1: Staff District Code
                        // Set in the export format.

                        // FIELD 2: Setting Staff ID
                        if (span.getStaff() != null) {
                            staffStudentCrsRecord
                                    .setStaffId((String) span.m_staff.getFieldValueByBeanPath(
                                            m_data.m_fieldTeachId));
                        }
                        String primEnlInd = BooleanAsStringConverter.TRUE.equals(
                                span.getSchTeacher().getFieldValueByBeanPath(m_data.m_fieldMtcPrimEnl))
                                        ? CODE_YES
                                        : CODE_NO;
                        staffStudentCrsRecord.setPrimEnlInd(primEnlInd);

                        String primInstrInd = null;
                        if (BooleanAsStringConverter.TRUE.equals(
                                span.getSchTeacher().getFieldValueByBeanPath(m_data.m_fieldMtcPrimInd))
                                || span.getSchTeacher().getPrimaryTeacherIndicator()) {
                            primInstrInd = CODE_YES;
                        } else {
                            primInstrInd = CODE_NO;
                        }
                        staffStudentCrsRecord.setPrimInstrInd(primInstrInd);

                        String primSpedInd = BooleanAsStringConverter.TRUE.equals(
                                span.getSchTeacher().getFieldValueByBeanPath(m_data.m_fieldMtcSpedInd))
                                        ? CODE_YES
                                        : CODE_NO;
                        staffStudentCrsRecord.setPrimSpedInd(primSpedInd);

                        staffStudentCrsRecord.setCourseLocationCode(schoolLocationCode);

                        // FIELD 8: Course Code
                        staffStudentCrsRecord.setCourseCode(courseNumber);

                        // FIELD 10: Course Code
                        staffStudentCrsRecord.setSectionCode(sectionCode);

                        // FIELD 12: Setting relationship start date.
                        // If the teacher's schedule doesn't have a start
                        // date, then compare the student's start date
                        // with the term's start date. whichever is later is
                        // the relationship start date.
                        staffStudentCrsRecord.setRelationshipStartDate(relStartDate);

                        // FIELD 13: Setting relationship end date.
                        // If the teacher's schedule doesn't have an end
                        // date, then compare the student's end date
                        // with the term's end date. whichever is earlier is
                        // the relationship end date.
                        staffStudentCrsRecord.setRelationshipExitDate(relEndDate);

                        // FIELD 19: Term Code
                        staffStudentCrsRecord.setTermCode(termCode);

                        // Add Student Course Record
                        if (!m_staffCourseRecords.contains(staffStudentCrsRecord)
                                && !m_data.m_allStaffCourseRecords.contains(staffStudentCrsRecord)) {
                            m_staffCourseRecords.add(staffStudentCrsRecord);
                            m_data.m_allStaffCourseRecords.add(staffStudentCrsRecord);
                        }
                    }
                }
            }
            setRowCount(m_staffCourseRecords.size());
        }

        /**
         * Find the next in-session date.
         *
         * @param inSessionDates Collection<PlainDate>
         * @param testDate PlainDate
         * @return PlainDate
         */
        protected PlainDate nextInsessionDate(Collection<PlainDate> inSessionDates, PlainDate testDate) {
            PlainDate result = null;
            for (PlainDate date : inSessionDates) {
                if (date.after(testDate)) {
                    result = date;
                    break;
                }
            }
            return result;
        }

        /**
         * Calculate Linkage Duration Adjustment.
         *
         * @param teacherSchedule ScheduleTeacher
         * @return double
         */
        private double calcLinkageDurationAdjustment(ScheduleTeacher teacherSchedule) {
            double linkageDurationAdjustment = DEFAULT_LINK_VALUE;

            String linkageString = (String) teacherSchedule.getFieldValueByAlias(ALIAS_LINKAGE_ADJUSTMENT);
            Double linkageValue = null;
            if (linkageString != null) {
                if (StringUtils.isNumeric(linkageString)) {
                    linkageValue = Double.valueOf(linkageString);
                }
            }

            if (linkageValue != null && linkageValue.doubleValue() == 0) {
                linkageDurationAdjustment = linkageValue.doubleValue();
            }

            return linkageDurationAdjustment;
        }

        /**
         * Calculate Section Code.
         *
         * @param masterSchedule MasterSchedule
         * @return String
         */
        private String calcSectionCode(MasterSchedule masterSchedule) {
            String sectionCode = masterSchedule.getSectionNumber();

            if (m_data.m_sectionCodeOverrideField != null) {
                String overrideCode =
                        (String) masterSchedule.getFieldValueByBeanPath(m_data.m_sectionCodeOverrideField);
                if (!StringUtils.isEmpty(overrideCode)) {
                    sectionCode = overrideCode;
                }
            }

            return sectionCode;
        }

        /**
         * Removes adjacent teacher spans with similar content.
         *
         * @param inSessionDates List<PlainDate>
         * @param tsList List<TeacherSpan>
         */
        private void coalesceTeacherSpans(List<PlainDate> inSessionDates, List<TeacherSpan> tsList) {
            Collections.sort(tsList);
            if (tsList.size() > 1) {
                Iterator<TeacherSpan> spans = tsList.iterator();
                TeacherSpan previous = spans.next();
                PlainDate previousEndDate =
                        inSessionDates.contains(previous.getSpanInterval().getEndDate()) ? previous.getSpanInterval()
                                .getEndDate()
                                : previousInsessionDate(inSessionDates, previous.getSpanInterval()
                                        .getEndDate());
                while (spans.hasNext()) {
                    TeacherSpan span = spans.next();
                    if (((span.getStaff() == null && previous.getStaff() == null) || (span.getStaff() != null
                            && previous.getStaff() != null && span.getStaff().equals(previous.getStaff())))
                            && calcLinkageDurationAdjustment(span.getSchTeacher()) == calcLinkageDurationAdjustment(
                                    previous
                                            .getSchTeacher())) {
                        PlainDate previousDate =
                                previousInsessionDate(inSessionDates, span.getSpanInterval().getStartDate());
                        if (previousDate != null && !previousDate.after(previousEndDate)) {
                            previous.getSpanInterval().setEndDate(span.getSpanInterval().getEndDate());
                            spans.remove();
                        } else {
                            previous = span;
                        }
                    } else {
                        previous = span;
                    }
                    previousEndDate =
                            inSessionDates.contains(previous.getSpanInterval().getEndDate()) ? previous
                                    .getSpanInterval().getEndDate()
                                    : previousInsessionDate(inSessionDates, previous
                                            .getSpanInterval().getEndDate());
                }
            }
        }

        /**
         * Checks for courses to be filtered then caches the results.
         *
         * @param section MasterSchedule
         * @return boolean
         */
        private boolean filterCourse(MasterSchedule section) {
            boolean filter = false;

            Course course = null;

            if (section != null) {
                if (!m_data.m_courseFilter.containsKey(section.getOid())) {
                    SchoolCourse schoolCourse = section.getSchoolCourse();
                    if (schoolCourse != null) {
                        course = schoolCourse.getCourse();
                    }

                    if (course != null) {
                        String stateCourseCode = (String) course.getFieldValueByBeanPath(m_data.m_stateCourseCodeField);

                        if (m_data.m_filterCourse == 1 && !StringUtils.isEmpty(m_data.m_courseNumberFilter)) {
                            if (m_data.m_courseNumberFilter.contains(COMMA)) {
                                filter = true;
                                String[] stateCodes = m_data.m_courseNumberFilter.split(COMMA);

                                for (String stateCode : stateCodes) {
                                    if (!StringUtils.isEmpty(stateCode) && stateCode.trim().equals(stateCourseCode)) {
                                        filter = false;
                                    }
                                }
                            } else if (!m_data.m_courseNumberFilter.equals(stateCourseCode)) {
                                filter = true;
                            }
                        }

                        if (!BooleanAsStringConverter.TRUE.equals(course
                                .getFieldValueByBeanPath(m_data.m_courseActiveIndicatorField))) {
                            filter = true;
                        }

                        if (StringUtils.isEmpty(stateCourseCode)) {
                            filter = true;
                        }

                        if (m_data.m_assessmentAliasCriteria != null) {
                            filter =
                                    filter
                                            || !BooleanAsStringConverter.TRUE.equals(course
                                                    .getFieldValueByBeanPath(m_data.m_assessmentAliasCriteria));
                        }
                    } else {
                        filter = true;
                    }

                    m_data.m_courseFilter.put(section.getOid(), Boolean.valueOf(filter));
                } else {
                    filter = m_data.m_courseFilter.get(section.getOid()).booleanValue();
                }
            }

            return filter;
        }

        /**
         * Populates in session dates for section.
         *
         * @param mst MasterSchedule
         * @return List
         */
        private List<PlainDate> getInSessionDatesForSection(MasterSchedule mst) {
            List<PlainDate> inSessionDates = null;

            if (m_data.m_inSessionDatesMap.containsKey(mst.getOid())) {
                inSessionDates = m_data.m_inSessionDatesMap.get(mst.getOid());
            } else {
                String calendarId = m_data.getMostCommonCalendar(mst);

                inSessionDates = new ArrayList<PlainDate>();

                String sklOid = m_data.m_masterScheduleSchoolMap.get(mst.getOid()).getOid();

                if (!StringUtils.isEmpty(sklOid)) {
                    Collection<SisSchoolCalendarDate> calDate = m_data.getSchoolCalendarDatesLookup(sklOid, calendarId);
                    for (SisSchoolCalendarDate sklCalDate : calDate) {
                        inSessionDates.add(sklCalDate.getDate());
                    }
                    Collections.sort(inSessionDates);
                }
                m_data.m_inSessionDatesMap.put(mst.getOid(), inSessionDates);
            }

            return inSessionDates;
        }

        /**
         * Populates Teacher Span information keyed on master schedule Oid.
         *
         * @param section MasterSchedule
         * @return Collection
         */
        private Collection<TeacherSpan> getTeacherSpans(MasterSchedule section) {
            List<TeacherSpan> tsList = null;
            String sectionOid = section.getOid();
            if (m_data.m_teacherSpanBySection.containsKey(sectionOid)) {
                tsList = m_data.m_teacherSpanBySection.get(sectionOid);
            } else {
                Collection<ScheduleTeacher> teacherSchedules = m_data.getTeacherSections(section);
                tsList = new ArrayList<NYCourseInstructorAssignment.TeacherSpan>();
                m_data.m_teacherSpanBySection.put(sectionOid, tsList);

                List<PlainDate> inSessionDates = getInSessionDatesForSection(section);
                ArrayList<TeacherSpan> tsListForMtc = null;
                for (ScheduleTeacher teacherSchedule : teacherSchedules) {
                    tsListForMtc = new ArrayList<TeacherSpan>();
                    boolean isCorrectRole = CODE_TEACHER_OF_RECORD.equals(teacherSchedule.getRole());
                    if (!isCorrectRole) {
                        continue;
                    }

                    ScheduleTerm term = m_data.getScheduleTerm(teacherSchedule.getScheduleTermOid());
                    SisStaff staff = teacherSchedule.getStaff();

                    String teacherStartStr =
                            (String) teacherSchedule.getFieldValueByBeanPath(m_data.m_teacherStartDateField);
                    PlainDate teacherStartDate = DateUtils.getDate(teacherStartStr);
                    String teacherExitStr =
                            (String) teacherSchedule.getFieldValueByBeanPath(m_data.m_teacherEndDateField);
                    PlainDate teacherExitDate = DateUtils.getDate(teacherExitStr);

                    if (term != null) {
                        Collection<ScheduleTermDate> termDates = m_data.getScheduleTermDates(term);

                        for (ScheduleTermDate trmDate : termDates) {
                            TeacherSpan tsToAdd = m_data.new TeacherSpan();
                            SpanInterval interval = m_data.new SpanInterval();

                            PlainDate trmStartDate = trmDate.getStartDate();
                            PlainDate trmEndDate = trmDate.getEndDate();

                            if (teacherStartDate != null && teacherStartDate.after(trmStartDate)) {
                                interval.setStartDate(teacherStartDate);
                            } else {
                                interval.setStartDate(trmStartDate);
                            }

                            if (teacherExitDate != null && teacherExitDate.before(trmEndDate)) {
                                interval.setEndDate(teacherExitDate);
                            } else {
                                interval.setEndDate(trmEndDate);
                            }

                            tsToAdd.setStaff(staff);
                            tsToAdd.setSpanInterval(interval);
                            tsToAdd.setSchTeacher(teacherSchedule);

                            if (!tsListForMtc.contains(tsToAdd)) {
                                tsListForMtc.add(tsToAdd);
                            }
                        }
                    } else if (teacherStartDate != null && teacherExitDate != null) {
                        TeacherSpan tsToAdd = m_data.new TeacherSpan();
                        SpanInterval interval = m_data.new SpanInterval();

                        interval.setStartDate(teacherStartDate);
                        interval.setEndDate(teacherExitDate);

                        tsToAdd.setStaff(staff);
                        tsToAdd.setSpanInterval(interval);
                        tsToAdd.setSchTeacher(teacherSchedule);

                        if (!tsListForMtc.contains(tsToAdd)) {
                            tsListForMtc.add(tsToAdd);
                        }
                    }

                    Collection<StaffAttendance> sfas = m_data.getStaffAttendance(staff);
                    Collection<PlainDate> stfAttDates = new ArrayList<PlainDate>();
                    Map<PlainDate, StaffAttendance> attMap = new HashMap<PlainDate, StaffAttendance>();

                    if (sfas != null && m_data.m_absenceInterval != null
                            && sfas.size() >= m_data.m_absenceInterval.intValue()) {
                        for (StaffAttendance sfa : sfas) {
                            stfAttDates.add(sfa.getDate());
                            attMap.put(sfa.getDate(), sfa);
                        }

                        if (inSessionDates != null && !stfAttDates.isEmpty()) {
                            int absIntervalCount = 0;
                            LinkedList<PlainDate> intervalDates = new LinkedList<PlainDate>();
                            Iterator<PlainDate> daysIterator = inSessionDates.iterator();
                            while (daysIterator.hasNext()) {
                                PlainDate date = daysIterator.next();
                                if (stfAttDates.contains(date)) {
                                    absIntervalCount += 1;
                                    intervalDates.add(date);
                                }
                                if (!stfAttDates.contains(date) || (!daysIterator.hasNext())) {
                                    if (absIntervalCount >= m_data.m_absenceInterval.intValue()) {
                                        Map<SisStaff, LinkedList<PlainDate>> subsMap =
                                                getSubsForInterval(attMap, intervalDates);
                                        if (!subsMap.isEmpty()) {
                                            for (Entry<SisStaff, LinkedList<PlainDate>> subEntry : subsMap.entrySet()) {
                                                SpanInterval newInterval = m_data.new SpanInterval();
                                                newInterval.setStartDate(subEntry.getValue().getFirst());
                                                newInterval.setEndDate(subEntry.getValue().getLast());
                                                SisStaff newStaff = subEntry.getKey();

                                                List<TeacherSpan> tsLoop = new ArrayList<TeacherSpan>(tsListForMtc);
                                                for (TeacherSpan span : tsLoop) {
                                                    List<SpanInterval> splits =
                                                            splitIntervals(inSessionDates, span.getSpanInterval(),
                                                                    newInterval);
                                                    if (splits.isEmpty()) {
                                                        // change entire interval to
                                                        // substitute
                                                        span.setSpanInterval(newInterval);
                                                        span.setStaff(newStaff);
                                                    } else {
                                                        span.setSpanInterval(splits.get(0));
                                                        TeacherSpan newSpan =
                                                                m_data.new TeacherSpan(newStaff, newInterval,
                                                                        teacherSchedule);
                                                        if (!tsListForMtc.contains(newSpan)) {
                                                            tsListForMtc.add(newSpan);
                                                        }
                                                        if (splits.size() > 1) {
                                                            newSpan =
                                                                    m_data.new TeacherSpan(span.getStaff(),
                                                                            splits.get(1),
                                                                            span.getSchTeacher());
                                                            if (!tsListForMtc.contains(newSpan)) {
                                                                tsListForMtc.add(newSpan);
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }

                                    absIntervalCount = 0;
                                    intervalDates = new LinkedList<PlainDate>();
                                }
                            }
                        }
                    }

                    tsList.addAll(tsListForMtc);
                }
                coalesceTeacherSpans(inSessionDates, tsList);
            }
            return tsList;
        }

        /**
         * Find the staff member most commonly used during a period of absence.
         *
         * @param attMap Map<PlainDate,StaffAttendance>
         * @param intervalDates LinkedList<PlainDate>
         * @return Map
         */
        private Map<SisStaff, LinkedList<PlainDate>> getSubsForInterval(Map<PlainDate, StaffAttendance> attMap,
                                                                        LinkedList<PlainDate> intervalDates) {
            Map<SisStaff, LinkedList<PlainDate>> staffDates = new HashMap();
            for (PlainDate date : intervalDates) {
                List<StaffAttendanceSub> subs = (List<StaffAttendanceSub>) attMap.get(date).getStaffAttendanceSub();
                if (subs != null) {
                    for (StaffAttendanceSub sub : subs) {
                        SisStaff subStaff = sub.getSubstitute();
                        if (subStaff != null) {
                            if (staffDates.containsKey(subStaff)) {
                                LinkedList<PlainDate> dates = staffDates.get(subStaff);
                                dates.add(date);
                                Collections.sort(dates);
                            } else {
                                LinkedList<PlainDate> dates = new LinkedList<PlainDate>();
                                dates.add(date);
                                staffDates.put(subStaff, dates);
                            }
                        }
                    }
                }
            }

            return staffDates;
        }

        /**
         * Caching everything associated with this master schedule to reduce the number of database
         * calls.
         *
         * @param masterSchedule MasterSchedule
         */
        private void populateMasterScheduleMaps(MasterSchedule masterSchedule) {
            SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
            m_data.m_masterScheduleSchoolCourseMap.put(masterSchedule.getOid(), schoolCourse);
            m_data.m_masterScheduleSchoolMap.put(masterSchedule.getOid(), schoolCourse.getSchool());
            m_data.m_masterScheduleCourseMap.put(masterSchedule.getOid(), schoolCourse.getCourse());
        }

        /**
         * Caches the term code and beginning and end date for this master schedule.
         *
         * @param masterSchedule MasterSchedule
         */
        private void populateStartEndDateAndTermCodeForMasterSchedule(MasterSchedule masterSchedule) {
            ScheduleTerm term = m_data.getScheduleTerm(masterSchedule.getScheduleTermOid());
            PlainDate termStartDate = null;
            PlainDate termEndDate = null;

            if (term != null) {
                Collection<ScheduleTermDate> termDates = m_data.getScheduleTermDates(term);

                for (ScheduleTermDate schedTermDate : termDates) {
                    if (termStartDate == null || termStartDate.after(schedTermDate.getStartDate())) {
                        m_data.m_termStartDateMap.put(masterSchedule.getOid(), schedTermDate.getStartDate());
                    }
                    if (termEndDate == null || termEndDate.before(schedTermDate.getEndDate())) {
                        m_data.m_termEndDateMap.put(masterSchedule.getOid(), schedTermDate.getEndDate());
                    }
                }

                String termCode = term.getCode();
                String stateTermCode =
                        m_data.lookupReferenceCodeByBeanPath(ScheduleTerm.class, ScheduleTerm.COL_CODE, termCode,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                if (stateTermCode == null) {
                    stateTermCode = termCode;
                }

                m_data.m_termCodeMap.put(masterSchedule.getOid(), stateTermCode);
            }
        }

        /**
         * Find the previous in-session date.
         *
         * @param inSessionDates List<PlainDate>
         * @param testDate PlainDate
         * @return PlainDate
         */
        private PlainDate previousInsessionDate(List<PlainDate> inSessionDates, PlainDate testDate) {
            PlainDate result = null;
            if (!inSessionDates.isEmpty()) {
                result = inSessionDates.get(0);
                for (PlainDate date : inSessionDates) {
                    if (!date.before(testDate)) {
                        break;
                    }
                    result = date;
                }
            }
            return result;
        }

        /**
         * Return new span intervals for teacher .
         *
         * @param inSessionDates List<PlainDate>
         * @param sched SpanInterval
         * @param absent SpanInterval
         * @return List
         */
        private List<SpanInterval> splitIntervals(List<PlainDate> inSessionDates,
                                                  SpanInterval sched,
                                                  SpanInterval absent) {
            List<SpanInterval> splits = new LinkedList();
            PlainDate startDate;
            PlainDate endDate;

            // add split before absent interval
            if (sched.getStartDate().before(absent.getStartDate())) {
                startDate = sched.getStartDate();
                if (!absent.getStartDate().after(sched.getEndDate())) {
                    endDate = previousInsessionDate(inSessionDates, absent.getStartDate());
                } else {
                    endDate = sched.getEndDate();
                }
                splits.add(m_data.new SpanInterval(startDate, endDate));
            }
            // add split after absent interval
            if (sched.getEndDate().after(absent.getEndDate())) {
                endDate = sched.getEndDate();
                if (sched.getStartDate().after(absent.getEndDate())) {
                    startDate = sched.getStartDate();
                } else {
                    startDate = nextInsessionDate(inSessionDates, absent.getEndDate());
                }
                splits.add(m_data.new SpanInterval(startDate, endDate));
            }
            return splits;
        }
    }

    /**
     * This class returns the Staff Student Course Records.
     */
    protected class RetrieveStaffStudentCourseDetails implements FieldRetriever {

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
            NYCourseInstructorAssignment nyData = (NYCourseInstructorAssignment) data;
            String param = (String) field.getParameter();
            StaffCourseRecord stfStdCrsRec = ((StaffStudentCourseEntity) entity).getStaffCourseRecord();

            if (CALC_PARAM_STAFF_ID.equals(param)) {
                value = stfStdCrsRec.getStaffId();
            } else if (CALC_PARAM_CRS_LOC_CODE.equals(param)) {
                value = stfStdCrsRec.getCourseLocationCode();
            } else if (CALC_PARAM_CRS_CODE.equals(param)) {
                value = stfStdCrsRec.getCourseCode();
            } else if (CALC_PARAM_SECTION_CODE.equals(param)) {
                value = stfStdCrsRec.getSectionCode();
            } else if (CALC_PARAM_REL_START_DATE.equals(param)) {
                value = stfStdCrsRec.getRelationshipStartDate();
            } else if (CALC_PARAM_REL_EXIT_DATE.equals(param)) {
                Date relExitDate = stfStdCrsRec.getRelationshipExitDate();
                // Limit the relationship EndDate for assessment record to the
                // time of the assessment.
                if (relExitDate != null && nyData.m_assessmentAliasDate != null) {
                    relExitDate = nyData.m_reportDate.before(relExitDate) ? nyData.m_reportDate : relExitDate;
                }
                value = relExitDate != null && m_today.after(relExitDate) ? relExitDate : null;
            } else if (CALC_PARAM_TERM_CODE.equals(param)) {
                value = stfStdCrsRec.getTermCode();
            } else if (CALC_PARAM_SFP_POS_PRIM.equals(param)) {
                value = stfStdCrsRec.getPrimInstrInd();
            } else if (CALC_PARAM_PRIM_ENL.equals(param)) {
                value = stfStdCrsRec.getPrimEnlInd();
            } else if (CALC_PARAM_SFP_SPED_IND.equals(param)) {
                value = stfStdCrsRec.getPrimSpedInd();
            } else if (CALC_PARAM_DELIVERY_CODE.equals(param)) {
                String mstOid = stfStdCrsRec.getSectionOid();
                MasterSchedule mst = nyData.getMstByOid(mstOid);
                String code = null;
                if (mst != null) {
                    code = (String) mst.getFieldValueByBeanPath(nyData.m_fieldDeliveryCodeMst);
                    if (!StringUtils.isEmpty(code)) {
                        code = lookupStateValue(MasterSchedule.class, m_fieldDeliveryCodeMst, code);
                    } else {
                        SchoolCourse csk = mst.getSchoolCourse();
                        if (csk != null) {
                            code = (String) csk.getFieldValueByBeanPath(nyData.m_fieldDeliveryCodeCsk);
                            if (!StringUtils.isEmpty(code)) {
                                code = lookupStateValue(SchoolCourse.class, m_fieldDeliveryCodeCsk, code);
                            } else {
                                Course crs = csk.getCourse();
                                if (crs != null) {
                                    code = (String) crs.getFieldValueByBeanPath(nyData.m_fieldDeliveryCodeCrs);
                                    if (!StringUtils.isEmpty(code)) {
                                        code = lookupStateValue(Course.class, m_fieldDeliveryCodeCrs, code);
                                    }
                                }
                            }
                        }
                    }
                }
                value = code;
            } else if (CALC_PARAM_PRIM_CRS_LANG.equals(param)) {
                String mstOid = stfStdCrsRec.getSectionOid();
                MasterSchedule mst = nyData.getMstByOid(mstOid);
                value = "ENG";
                if (mst != null) {
                    SchoolCourse csk = mst.getSchoolCourse();
                    if (csk != null) {
                        Course crs = csk.getCourse();
                        if (crs != null) {
                            String code = (String) crs.getFieldValueByBeanPath(m_fieldCrsPrimCrsLang);
                            if (!StringUtils.isEmpty(code)) {
                                String stateCode = nyData.lookupStateValue(Course.class, m_fieldCrsPrimCrsLang, code);
                                if (!StringUtils.isEmpty(stateCode)) {
                                    value = stateCode;
                                }
                            }
                        }
                    }
                    if (value == null || mst.getFieldValueByBeanPath(m_fieldMstPrimCrsLang) != null) {
                        value = nyData.lookupStateValue(MasterSchedule.class, m_fieldMstPrimCrsLang,
                                (String) mst.getFieldValueByBeanPath(m_fieldMstPrimCrsLang));
                    }
                }
            }
            return value;
        }
    }

    /**
     * These validations will conditionally report records where there contain dates assigned to
     * more than
     * one teacher and create validation errors when there is no teacher assigned to in-session
     * dates.
     */
    protected class ValidateAssignments implements FieldValidator {

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

            StaffStudentCourseEntity nyEntity = (StaffStudentCourseEntity) entity;
            NYCourseInstructorAssignment nyData = (NYCourseInstructorAssignment) data;

            String stdOid = nyEntity.getBean().getOid();
            List<StaffCourseRecord> records = nyEntity.getStaffCourseRecords();
            StaffCourseRecord currentRecord = nyEntity.getStaffCourseRecord();
            String sectionOid = currentRecord.getSectionOid();
            Collection errors = new ArrayList<StateReportValidationError>();

            if (((Boolean) getParameter(PARAM_ASSIGN_OVERLAP)).booleanValue()) {

                Date relStartDate = currentRecord.getRelationshipStartDate();
                Date relExitDate = currentRecord.getRelationshipExitDate();
                // Limit the relationship EndDate for assessment record to the
                // time of the assessment.
                if (relExitDate != null && nyData.m_assessmentAliasDate != null) {
                    relExitDate = nyData.m_reportDate.before(relExitDate) ? nyData.m_reportDate : relExitDate;
                }

                for (StaffCourseRecord record : records) {
                    Date recordStartDate = record.getRelationshipStartDate();

                    if (!StringUtils.isEmpty(record.getStaffId()) && !StringUtils.isEmpty(currentRecord.getStaffId())
                            && !currentRecord.getStaffId().equals(record.getStaffId())) {
                        if (recordStartDate.equals(relStartDate) || recordStartDate.after(relStartDate)
                                && recordStartDate.before(relExitDate)) {
                            StateReportValidationError error =
                                    new StateReportValidationError(entity, field, "Assigment Overlap",
                                            currentRecord.toString() + "\n OVERLAPS \n" + record.toString());
                            errors.add(error);

                        }
                    }
                }

            }

            if (((Boolean) getParameter(PARAM_ASSIGN_GAP)).booleanValue()) {

                if (StringUtils.isEmpty(nyData.m_tempStdOid) || !nyData.m_tempStdOid.equals(stdOid)
                        || !nyData.m_tempSectionOid.equals(sectionOid)) {
                    m_tempStdOid = stdOid;
                    m_tempSectionOid = sectionOid;
                    m_checkInSessionGap = true;
                }

                if (m_checkInSessionGap) {
                    List<PlainDate> inSessionDates = m_inSessionDatesMap.get(currentRecord.getSectionOid());

                    Collection<PlainDate> notUsedInSessionDates = new ArrayList<PlainDate>();

                    for (PlainDate inSessionDate : inSessionDates) {

                        if (isNoAssignmentForDate(records, inSessionDate)) {
                            notUsedInSessionDates.add(inSessionDate);
                        }

                    }

                    boolean isNewInterval = false;
                    Map<SpanInterval, StaffCourseRecord> gapMap =
                            new HashMap<NYCourseInstructorAssignment.SpanInterval, NYCourseInstructorAssignment.StaffCourseRecord>();

                    PlainDate spanStart = null;
                    Iterator<PlainDate> iter = notUsedInSessionDates.iterator();

                    while (iter.hasNext()) {
                        PlainDate inSessionDate = iter.next();

                        if (spanStart == null || isNewInterval) {
                            spanStart = inSessionDate;
                        }

                        PlainDate nextAfterInSessionDate = nyEntity.nextInsessionDate(inSessionDates, inSessionDate);

                        if (!notUsedInSessionDates.contains(nextAfterInSessionDate)) {
                            SpanInterval newSpanInterval = new SpanInterval(spanStart, inSessionDate);
                            gapMap.put(newSpanInterval, getAdjacentRecordByDate(records, spanStart));
                            isNewInterval = true;
                            if (iter.hasNext()) {
                                iter.next();
                            }
                        }
                    }

                    if (!gapMap.isEmpty()) {
                        for (Entry<SpanInterval, StaffCourseRecord> entry : gapMap.entrySet()) {

                            String msgRecord =
                                    entry.getValue() != null ? entry.getValue().toString() : "Record was not found.";
                            StateReportValidationError error =
                                    new StateReportValidationError(entity, field, "Assigment Gap: Start Date: "
                                            + entry.getKey().getStartDate() + ", " + "End Date: "
                                            + entry.getKey().getEndDate(), "Adjacent " + msgRecord);
                            errors.add(error);

                        }

                    }

                    m_checkInSessionGap = false;
                }

                if (StringUtils.isEmpty(currentRecord.getStaffId())) {
                    Date gapDate = currentRecord.getRelationshipStartDate();
                    StaffCourseRecord adjRecord = getAdjacentRecordByDate(records, gapDate);

                    StateReportValidationError error =
                            new StateReportValidationError(entity, field, "Assigment Gap: Start Date: "
                                    + currentRecord.getRelationshipStartDate() + ", " + "End Date: "
                                    + currentRecord.getRelationshipExitDate(),
                                    "Adjacent "
                                            + (adjRecord != null ? adjRecord.toString() : "Record was not found."));
                    errors.add(error);
                }
            }

            return errors;
        }

        /**
         * Return adjacent StaffCourseRecord for the date from gap interval.
         *
         * @param records Collection<StaffCourseRecord>
         * @param gapDate Date
         * @return Staff course record
         */
        private StaffCourseRecord getAdjacentRecordByDate(Collection<StaffCourseRecord> records, Date gapDate) {
            Date candidateDate = null;
            StaffCourseRecord candidateRecord = null;
            for (StaffCourseRecord sscr : records) {
                Date tempDate = sscr.getRelationshipExitDate();

                if (candidateDate == null && gapDate.after(tempDate)) {
                    candidateDate = tempDate;
                    candidateRecord = sscr;
                }

                if (candidateDate != null && candidateDate.before(tempDate) && gapDate.after(tempDate)) {
                    candidateDate = tempDate;
                    candidateRecord = sscr;
                }
            }

            return candidateRecord;
        }

        /**
         * Check if there is assignment for the given in session date.
         *
         * @param records Collection<StaffCourseRecord>
         * @param inSessionDate Date
         * @return true, if is no assignment for date
         */
        private boolean isNoAssignmentForDate(Collection<StaffCourseRecord> records, Date inSessionDate) {
            boolean isNoAssignmentForDate = true;

            for (StaffCourseRecord record : records) {
                Date relStartDate = record.getRelationshipStartDate();
                Date relExitDate = record.getRelationshipExitDate();

                if (inSessionDate.equals(relStartDate) || inSessionDate.equals(relExitDate)
                        || inSessionDate.after(relStartDate) && inSessionDate.before(relExitDate)) {
                    isNoAssignmentForDate = false;
                    break;
                }
            }

            return isNoAssignmentForDate;
        }

    }

    /**
     *
     * Helper class to store interval of Teacher Span.
     *
     */
    private class SpanInterval implements Comparable<SpanInterval> {

        private PlainDate m_startDate;
        private PlainDate m_endDate;

        /**
         * Instantiates a new span interval.
         */
        public SpanInterval() {

        }

        /**
         * Instantiates a new span interval.
         *
         * @param startDate PlainDate
         * @param endDate PlainDate
         */
        public SpanInterval(PlainDate startDate, PlainDate endDate) {
            m_startDate = startDate;
            m_endDate = endDate;
        }

        /**
         * Implementing interface's method.
         *
         * @param o SpanInterval
         * @return int
         */
        @Override
        public int compareTo(SpanInterval o) {
            int result = m_startDate.compareTo(o.getStartDate());

            if (result == 0) {
                if (m_endDate != null) {
                    result = m_endDate.compareTo(o.getEndDate());
                }
            }

            return result;
        }

        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {

            return compareTo((SpanInterval) obj) == 0 ? true : false;
        }

        /**
         * hashCode
         * Provide a comparable hash code for HashMap compatibility.
         *
         * @return int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return Objects.hash(m_startDate, m_endDate);
        }

        /**
         * Gets the end date.
         *
         * @return the m_endDate
         */
        public PlainDate getEndDate() {
            return m_endDate;
        }

        /**
         * Gets the start date.
         *
         * @return the m_startDate
         */
        public PlainDate getStartDate() {
            return m_startDate;
        }

        /**
         * Sets the end date.
         *
         * @param endDate void
         */
        public void setEndDate(PlainDate endDate) {
            this.m_endDate = endDate;
        }

        /**
         * Sets the start date.
         *
         * @param startDate void
         */
        public void setStartDate(PlainDate startDate) {
            this.m_startDate = startDate;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "SpanInterval [m_startDate=" + m_startDate + ", m_endDate=" + m_endDate + "]";
        }
    }

    /**
     * Inner data object for saving all of the student information during the pre-processing of the
     * export.
     */
    private class StaffCourseRecord implements Comparable<StaffCourseRecord> {
        private String staffId;
        private String courseLocationCode;
        private String courseCode;
        private String primEnlInd;
        private String primInstrInd;
        private String primSpedInd;
        private String sectionCode;
        private String sectionOid;
        private Date relationshipStartDate;
        private Date relationshipExitDate;
        private String termCode = "";


        /**
         * Instantiates a new staff course record.
         */
        public StaffCourseRecord() {

        }

        /**
         * Implementing interface's method.
         *
         * @param o StaffCourseRecord
         * @return int
         */
        @Override
        public int compareTo(StaffCourseRecord o) {
            int result = courseCode.compareTo(o.getCourseCode());
            if (result == 0) {
                result = courseLocationCode.compareTo(o.getCourseLocationCode());
            }
            if (result == 0) {
                if (staffId != null && o.getStaffId() != null) {
                    result = staffId.compareTo(o.getStaffId());
                }
            }
            if (result == 0) {
                result = sectionCode.compareTo(o.getSectionCode());
            }
            if (result == 0) {
                if (relationshipStartDate != null) {
                    result = relationshipStartDate.compareTo(o.getRelationshipStartDate());
                }
            }
            if (result == 0) {
                if (relationshipExitDate != null) {
                    result = relationshipExitDate.compareTo(o.getRelationshipExitDate());
                }
            }
            if (result == 0) {
                result = termCode.compareTo(o.getTermCode());
            }

            return result;
        }

        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            return compareTo((StaffCourseRecord) obj) == 0 ? true : false;
        }

        /**
         * Hash code.
         *
         * @return int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return staffId.hashCode() + courseLocationCode.hashCode() + courseCode.hashCode() + sectionCode.hashCode()
                    + termCode.hashCode();
        }

        /**
         * Gets the staff id.
         *
         * @return String
         */
        public String getStaffId() {
            return staffId;
        }

        /**
         * Sets the staff id.
         *
         * @param staffId void
         */
        public void setStaffId(String staffId) {
            this.staffId = staffId;
        }

        /**
         * Gets the course location code.
         *
         * @return String
         */
        public String getCourseLocationCode() {
            return courseLocationCode;
        }

        /**
         * Sets the course location code.
         *
         * @param courseLocationCode void
         */
        public void setCourseLocationCode(String courseLocationCode) {
            this.courseLocationCode = courseLocationCode;
        }

        /**
         * Gets the course code.
         *
         * @return String
         */
        public String getCourseCode() {
            return courseCode;
        }

        /**
         * Sets the course code.
         *
         * @param courseCode void
         */
        public void setCourseCode(String courseCode) {
            this.courseCode = courseCode;
        }

        /**
         * Gets the section code.
         *
         * @return String
         */
        public String getSectionCode() {
            return sectionCode;
        }

        /**
         * Gets the section oid.
         *
         * @return String
         */
        public String getSectionOid() {
            return sectionOid;
        }

        /**
         * Sets the section code.
         *
         * @param sectionCode void
         */
        public void setSectionCode(String sectionCode) {
            this.sectionCode = sectionCode;
        }

        /**
         * Sets the section oid.
         *
         * @param sectionOid void
         */
        public void setSectionOid(String sectionOid) {
            this.sectionOid = sectionOid;
        }

        /**
         * Gets the relationship start date.
         *
         * @return Date
         */
        public Date getRelationshipStartDate() {
            return relationshipStartDate;
        }

        /**
         * Sets the relationship start date.
         *
         * @param relationshipStartDate void
         */
        public void setRelationshipStartDate(Date relationshipStartDate) {
            this.relationshipStartDate = relationshipStartDate;
        }

        /**
         * Gets the relationship exit date.
         *
         * @return Date
         */
        public Date getRelationshipExitDate() {
            return relationshipExitDate;
        }

        /**
         * Sets the relationship exit date.
         *
         * @param relationshipExitDate void
         */
        public void setRelationshipExitDate(Date relationshipExitDate) {
            this.relationshipExitDate = relationshipExitDate;
        }

        /**
         * Gets the term code.
         *
         * @return String
         */
        public String getTermCode() {
            return termCode;
        }

        /**
         * Sets the term code.
         *
         * @param termCode void
         */
        public void setTermCode(String termCode) {
            if (termCode != null) {
                this.termCode = termCode;
            }
        }

        /**
         * Gets the prim enl ind.
         *
         * @return the primEnlInd
         */
        public String getPrimEnlInd() {
            return primEnlInd;
        }

        /**
         * Sets the prim enl ind.
         *
         * @param primEnl void
         */
        public void setPrimEnlInd(String primEnl) {
            this.primEnlInd = primEnl;
        }


        /**
         * Gets the prim instr ind.
         *
         * @return the primInstrInd
         */
        public String getPrimInstrInd() {
            return primInstrInd;
        }

        /**
         * Sets the prim instr ind.
         *
         * @param primInstrInd void
         */
        public void setPrimInstrInd(String primInstrInd) {
            this.primInstrInd = primInstrInd;
        }

        /**
         * Gets the prim sped ind.
         *
         * @return the primSpedInd
         */
        public String getPrimSpedInd() {
            return primSpedInd;
        }

        /**
         * Sets the prim sped ind.
         *
         * @param primSpedInd void
         */
        public void setPrimSpedInd(String primSpedInd) {
            this.primSpedInd = primSpedInd;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "Record: staffId=" + staffId + ", crsCode=" + courseCode + ", sectionCode=" + sectionCode
                    + ", relStartDate=" + relationshipStartDate + ", relEndDate=" + relationshipExitDate + ".";
        }

    }

    /**
     * Helper class to store information about teacher staff and interval.
     *
     */
    private class TeacherSpan implements Comparable<TeacherSpan> {

        private SisStaff m_staff;
        private SpanInterval m_interval;
        private ScheduleTeacher m_schTeacher;

        /**
         * Instantiates a new teacher span.
         *
         * @param staff SisStaff
         * @param interval SpanInterval
         * @param schTeacher ScheduleTeacher
         */
        public TeacherSpan(SisStaff staff, SpanInterval interval, ScheduleTeacher schTeacher) {
            m_staff = staff;
            m_interval = interval;
            m_schTeacher = schTeacher;
        }

        /**
         * Instantiates a new teacher span.
         */
        public TeacherSpan() {

        }

        /**
         * Implementing interface's method.
         *
         * @param o TeacherSpan
         * @return int
         */
        @Override
        public int compareTo(TeacherSpan o) {
            int result;
            if (m_staff == null ^ o.getStaff() == null) {
                result = (m_staff == null) ? -1 : 1;
            } else if (m_staff == null && o.getStaff() == null) {
                result = 0;
            } else {
                result = m_staff.getOid().compareTo(o.getStaff().getOid());
            }
            if (result == 0) {
                result = m_interval.compareTo(o.getSpanInterval());
            }

            return result;
        }

        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            return compareTo((TeacherSpan) obj) == 0 ? true : false;
        }

        /**
         * Gets the sch teacher.
         *
         * @return the m_schTeacher
         */
        public ScheduleTeacher getSchTeacher() {
            return m_schTeacher;
        }

        /**
         * Gets the span interval.
         *
         * @return the m_startDate
         */
        public SpanInterval getSpanInterval() {
            return m_interval;
        }

        /**
         * Gets the staff.
         *
         * @return the m_staff
         */
        public SisStaff getStaff() {
            return m_staff;
        }

        /**
         * Sets the sch teacher.
         *
         * @param schTeacher void
         */
        public void setSchTeacher(ScheduleTeacher schTeacher) {
            this.m_schTeacher = schTeacher;
        }

        /**
         * Sets the span interval.
         *
         * @param interval void
         */
        public void setSpanInterval(SpanInterval interval) {
            this.m_interval = interval;
        }

        /**
         * Sets the staff.
         *
         * @param staff void
         */
        public void setStaff(SisStaff staff) {
            this.m_staff = staff;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "TeacherSpan [staff=" + (m_staff == null ? "null" : m_staff.getNameView()) + ", m_interval="
                    + m_interval + "]";
        }
    }

    /**
     * Input Definition Parameters
     */
    private static final String PARAM_ABSENCE_INTERVAL = "absenceInterval";
    private static final String PARAM_ASSESSMENT_ALIAS = "assessmentAlias";
    private static final String PARAM_ASSIGN_GAP = "assignGap";
    private static final String PARAM_ASSIGN_OVERLAP = "assignOverlap";
    private static final String PARAM_FILTER_COURSES = "filterCourses";
    private static final String PARAM_FILTER_COURSES_CODE = "filterCoursesCode";
    private static final String PARAM_REMOVE_HEADER = "removeHeader";
    private static final String PARAM_REPORT_DATE = "reportDate";

    /**
     * Retriever Parameters
     */
    private static final String CALC_ID_STAFF_STUDENT_COURSE = "STF-STD-CRS";

    private static final String CALC_PARAM_CRS_CODE = "CRS-CODE";
    private static final String CALC_PARAM_CRS_LOC_CODE = "CRS-LOC-CODE";
    private static final String CALC_PARAM_DELIVERY_CODE = "DELIVERY-CODE";
    private static final String CALC_PARAM_PRIM_CRS_LANG = "PRIM_CRS_LANG";
    private static final String CALC_PARAM_PRIM_ENL = "PRIM_ENL";
    private static final String CALC_PARAM_REL_EXIT_DATE = "REL-EXIT-DATE";
    private static final String CALC_PARAM_REL_START_DATE = "REL-START-DATE";
    private static final String CALC_PARAM_SFP_POS_PRIM = "SFP-POS-PRIM";
    private static final String CALC_PARAM_SFP_SPED_IND = "SFP-SPED-IND";
    private static final String CALC_PARAM_SECTION_CODE = "SECTION-CODE";
    private static final String CALC_PARAM_STAFF_ID = "STAFF-ID";
    private static final String CALC_PARAM_TERM_CODE = "TERM-CODE";

    /**
     * Aliases
     */
    // ORGANIZATION
    private static final String ALIAS_ASSESSMENT_DATE_PREFIX = "NY ASSESS DATE ";
    private static final String ALIAS_ASSESSMENT_TYPE_PREFIX = "NY ASSESSMENT ";

    // SCHOOL
    private static final String ALIAS_LOCATION_CODE = "LOCATION CODE";
    private static final String ALIAS_SKL_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";

    // COURSE
    private static final String ALIAS_ACTIVE_INDICATOR = "DOE ACTIVE";
    private static final String ALIAS_CRS_DELIVERY_CODE = "all-crs-PrimaryInstructionDeliveryMethod";
    private static final String ALIAS_CRS_PRIM_CRS_LANG = "all-crs-PrimaryCourseLanguage";
    private static final String ALIAS_CSK_DELIVERY_CODE = "all-csk-PrimaryInstructionDeliveryMethodOverride";
    private static final String ALIAS_STATE_COURSE_CODE = "DOE STATE COURSE";

    // MASTER_SCHEDULE
    private static final String ALIAS_EXCLUDE_FROM_EVALUATION_INDICATOR = "DOE EXCLUDE EVALUATION";
    private static final String ALIAS_MST_DELIVERY_CODE = "all-mst-PrimaryInstructionDeliveryMethodOveride";
    private static final String ALIAS_MST_PRIM_CRS_LANG = "all-mst-PrimaryCourseLanguageOverride";
    private static final String ALIAS_PLATOON_CODE = "DOE SECTION OVERRIDE";

    // TEACHER_SCHEDULE
    private static final String ALIAS_LINKAGE_ADJUSTMENT = "DOE LINK DUR ADJUST";
    private static final String ALIAS_MTC_EXCLUDE = "all-mtc-ExcludeFromReporting";
    private static final String ALIAS_MTC_PRIM_ENL = "all-mtc-PrimaryENLInstructorIndicator";
    private static final String ALIAS_TEACHER_START_DATE = "DOE TEACHER START";
    private static final String ALIAS_TEACHER_END_DATE = "DOE TEACHER END";

    // STAFF
    private static final String ALIAS_TEACH_ID = "DOE TEACH ID";
    private static final String ALIAS_EXCLUDE_STAFF = "DOE EXCLUDE STF";

    // SCHEDULE TEACHER
    private static final String ALIAS_MTC_PRIM_IND = "all-mtc-PrimaryInstructorIndicator";
    private static final String ALIAS_MTC_SPED_IND = "all-mtc-PrimarySpecialEdInstructor";
    private static final String CODE_NO = "N";
    private static final String CODE_TEACHER_OF_RECORD = "Teacher of Record";
    private static final String CODE_YES = "Y";

    private static final String VAL_ASSIGNMENTS = "VAL-ASSIGNMENTS";

    /**
     * Primitive members
     */
    protected boolean m_checkInSessionGap = false;
    private boolean m_removeHeaderIndicator;
    protected int m_filterCourse;

    /**
     * Reference types members
     */
    protected Integer m_absenceInterval;
    protected String m_aliasSuffix;
    protected String m_assessmentAliasCriteria;
    protected String m_assessmentAliasDate;
    protected String m_courseActiveIndicatorField;
    protected String m_courseNumberFilter;
    protected String m_currentContextOid;
    protected String m_fieldCrsPrimCrsLang;
    protected String m_fieldDeliveryCodeCrs;
    protected String m_fieldDeliveryCodeCsk;
    protected String m_fieldDeliveryCodeMst;
    protected String m_fieldMstPrimCrsLang;
    protected String m_fieldMtcExclude;
    protected String m_fieldMtcPrimInd;
    protected String m_fieldMtcPrimEnl;
    protected String m_fieldMtcSpedInd;
    protected String m_fieldSklExcludeSchool;
    protected String m_fieldStfExclInd;
    protected String m_fieldTeachId;
    protected PlainDate m_reportDate;
    protected String m_schoolLocationCodeField;
    protected ScheduleManager m_scheduleMgr;
    protected String m_sectionCodeOverrideField;
    protected String m_sectionExcludeFromEvalIndField;
    protected String m_stateCourseCodeField;
    protected String m_teacherEndDateField;
    protected String m_teacherStartDateField;
    protected String m_tempStdOid;
    protected String m_tempSectionOid;
    protected PlainDate m_today;

    /**
     * Collections
     */
    private Collection<SchoolCalendar> m_schoolCalendars;

    /**
     * Maps
     */
    protected List<StaffCourseRecord> m_allStaffCourseRecords =
            new ArrayList<NYCourseInstructorAssignment.StaffCourseRecord>();
    protected Map<String, Boolean> m_courseFilter = new HashMap<String, Boolean>();
    protected Map<String, List<PlainDate>> m_inSessionDatesMap = new HashMap<String, List<PlainDate>>();
    protected Map<String, Course> m_masterScheduleCourseMap = new HashMap<String, Course>();
    protected Map<String, SchoolCourse> m_masterScheduleSchoolCourseMap = new HashMap<String, SchoolCourse>();
    protected Map<String, SisSchool> m_masterScheduleSchoolMap = new HashMap<String, SisSchool>();
    protected Map<String, Collection<SisSchoolCalendarDate>> m_schoolCalendarDatesMap;
    protected Map<String, List<TeacherSpan>> m_teacherSpanBySection = new HashMap<String, List<TeacherSpan>>();
    protected Map<String, String> m_termCodeMap = new HashMap<String, String>();
    protected Map<String, PlainDate> m_termEndDateMap = new HashMap<String, PlainDate>();
    protected Map<String, PlainDate> m_termStartDateMap = new HashMap<String, PlainDate>();

    private static final String COMMA = ",";

    private Map<String, String> m_mostCommonCalendarSection = new HashMap();
    private Map<String, MasterSchedule> m_mstsByOid = new HashMap();
    private Map<String, ScheduleTerm> m_scheduleTerms = new HashMap();
    private Map<String, Collection<ScheduleTermDate>> m_scheduleTermDates = new HashMap();
    private Map<String, Collection<StaffAttendance>> m_staffAttendances = new HashMap();
    private Map<String, Collection<SisSchoolCalendarDate>> m_schoolCalendarDates = new HashMap();
    private Map<String, Collection<ScheduleTeacher>> m_teacherSections = new HashMap();

    /**
     * Allows user to determine if the export needs to have a header at runtime.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        if (m_removeHeaderIndicator) {
            return null;
        }

        return super.getHeading();
    }

    /**
     * basic initialize method .
     *
     * @throws X2BaseException exception
     */
    @Override
    public void initialize() throws X2BaseException {
        initializeFields();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Load lookup tables
             */
            loadSchoolCalendars();

            // MAPS
            loadScheduleTerms();
            loadStaffAttendancesBySTFOid();
            loadScheduleTermDatesByTRMOid();
            loadMostCommonCalendarSection();

            // Set the query to be used for MTC selection.
            setQuery(getStaffScheduleQuery());
            setEntityClass(StaffStudentCourseEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALC_ID_STAFF_STUDENT_COURSE, new RetrieveStaffStudentCourseDetails());
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldRetriever>();
            validators.put(VAL_ASSIGNMENTS, new ValidateAssignments());
            super.addValidators(validators);
        }
    }

    /**
     * basic initialize fields.
     *
     * @throws X2BaseException exception
     */
    public void initializeFields() throws X2BaseException {
        m_today = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));

        // System Parameters
        m_currentContextOid = ((SisOrganization) getOrganization()).getCurrentContextOid();

        // Load Parameters
        m_absenceInterval = (Integer) getParameter(PARAM_ABSENCE_INTERVAL);

        if (m_absenceInterval == null) {
            m_absenceInterval = Integer.valueOf(10);
        }

        m_filterCourse = 0;
        if (getParameter(PARAM_FILTER_COURSES) != null) {
            String filterCourse = (String) getParameter(PARAM_FILTER_COURSES);
            if (StringUtils.isNumeric(filterCourse)) {
                m_filterCourse = Integer.valueOf(filterCourse).intValue();
            }
        }
        m_courseNumberFilter = (String) getParameter(PARAM_FILTER_COURSES_CODE);
        m_removeHeaderIndicator = false;
        if (getParameter(PARAM_REMOVE_HEADER) != null) {
            m_removeHeaderIndicator = ((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue();
        }
        m_aliasSuffix = (String) getParameter(PARAM_ASSESSMENT_ALIAS);

        // Use the input def to translate the right alias, only using the end of
        // the alias so I can reuse it
        Organization organization = getOrganization();
        if (m_aliasSuffix != null) {
            m_assessmentAliasCriteria = translateAliasToJavaName(ALIAS_ASSESSMENT_TYPE_PREFIX + m_aliasSuffix, true);
            m_assessmentAliasDate = translateAliasToJavaName(ALIAS_ASSESSMENT_DATE_PREFIX + m_aliasSuffix, true);
            String assessmentDateStr = (String) organization.getFieldValueByBeanPath(m_assessmentAliasDate);
            if (assessmentDateStr != null) {
                m_reportDate = DateUtils.getDate(assessmentDateStr);
            }
        }

        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
            if (getParameter(PARAM_REPORT_DATE) != null) {
                m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
            }
        }

        // Load Alias database field Names
        m_scheduleMgr = new ScheduleManager(getBroker());
        m_courseActiveIndicatorField = translateAliasToJavaName(ALIAS_ACTIVE_INDICATOR, true);
        m_schoolLocationCodeField = translateAliasToJavaName(ALIAS_LOCATION_CODE, true);
        m_sectionCodeOverrideField = translateAliasToJavaName(ALIAS_PLATOON_CODE, false);
        m_sectionExcludeFromEvalIndField = translateAliasToJavaName(ALIAS_EXCLUDE_FROM_EVALUATION_INDICATOR, true);
        m_stateCourseCodeField = translateAliasToJavaName(ALIAS_STATE_COURSE_CODE, true);
        m_teacherEndDateField = translateAliasToJavaName(ALIAS_TEACHER_END_DATE, true);
        m_teacherStartDateField = translateAliasToJavaName(ALIAS_TEACHER_START_DATE, true);
        m_fieldMtcPrimInd = translateAliasToJavaName(ALIAS_MTC_PRIM_IND, true);
        m_fieldMtcPrimEnl = translateAliasToJavaName(ALIAS_MTC_PRIM_ENL, true);
        m_fieldMtcSpedInd = translateAliasToJavaName(ALIAS_MTC_SPED_IND, true);
        m_fieldStfExclInd = translateAliasToJavaName(ALIAS_EXCLUDE_STAFF, true);
        m_fieldTeachId = translateAliasToJavaName(ALIAS_TEACH_ID, true);
        m_fieldDeliveryCodeCrs = translateAliasToJavaName(ALIAS_CRS_DELIVERY_CODE, true);
        m_fieldDeliveryCodeCsk = translateAliasToJavaName(ALIAS_CSK_DELIVERY_CODE, true);
        m_fieldDeliveryCodeMst = translateAliasToJavaName(ALIAS_MST_DELIVERY_CODE, true);
        m_fieldMstPrimCrsLang = translateAliasToJavaName(ALIAS_MST_PRIM_CRS_LANG, true);
        m_fieldCrsPrimCrsLang = translateAliasToJavaName(ALIAS_CRS_PRIM_CRS_LANG, true);
        m_fieldMtcExclude = translateAliasToJavaName(ALIAS_MTC_EXCLUDE, false);
        m_fieldSklExcludeSchool = translateAliasToJavaName(ALIAS_SKL_EXCLUDE_SCHOOL, true);
    }

    /**
     * Get the list of SisSchoolCalendarDate's for a specific schoolOid and calendarId.
     *
     * @param schoolOid String
     * @param calendarId String
     * @return Collection<SisSchoolCalendarDate>
     */
    Collection<SisSchoolCalendarDate> getSchoolCalendarDatesLookup(String schoolOid, String calendarId) {
        Collection<SisSchoolCalendarDate> dates = null;
        String key = schoolOid + calendarId;
        if (m_schoolCalendarDates.containsKey(key)) {
            dates = m_schoolCalendarDates.get(key);
        } else {
            dates = new HashSet();
            String SchoolCalendarOid = null;

            for (SchoolCalendar schoolCalendar : m_schoolCalendars) {
                if (schoolCalendar.getSchoolOid().equals(schoolOid)
                        && m_currentContextOid.equals(schoolCalendar.getDistrictContextOid())
                        && schoolCalendar.getCalendarId().equals(calendarId)) {
                    SchoolCalendarOid = schoolCalendar.getOid();
                }
            }

            if (SchoolCalendarOid != null) {
                Collection<SisSchoolCalendarDate> schoolCalendarDates = m_schoolCalendarDatesMap.get(SchoolCalendarOid);
                dates.addAll(schoolCalendarDates);
            }
            m_schoolCalendarDates.put(key, dates);
        }

        return dates;
    }

    /**
     * Cache most common calendar for sections .
     *
     * @param mst MasterSchedule
     * @return String
     */
    protected String getMostCommonCalendar(MasterSchedule mst) {
        String calendarId = null;
        if (m_mostCommonCalendarSection.containsKey(mst.getOid())) {
            calendarId = m_mostCommonCalendarSection.get(mst.getOid());
        } else {
            MasterSchedule[] msts = new MasterSchedule[] {mst};

            calendarId = m_scheduleMgr.getMostCommonCalendar(mst.getSchedule(), Arrays.asList(msts));
            m_mostCommonCalendarSection.put(mst.getOid(), calendarId);
        }
        return calendarId;
    }

    /**
     * Cache master schedule.
     *
     * @param mstOid String
     * @return Master schedule
     */
    protected MasterSchedule getMstByOid(String mstOid) {
        MasterSchedule mst = null;
        if (m_mstsByOid.containsKey(mstOid)) {
            mst = m_mstsByOid.get(mstOid);
        } else {

            mst = (MasterSchedule) getBroker().getBeanByOid(MasterSchedule.class, mstOid);
            m_mstsByOid.put(mstOid, mst);
        }
        return mst;
    }

    /**
     * Cache schedule terms for ScheduleTeacher.
     *
     * @param trmOid String
     * @return Schedule term
     */
    protected ScheduleTerm getScheduleTerm(String trmOid) {
        ScheduleTerm term = null;
        if (m_scheduleTerms.containsKey(trmOid)) {
            term = m_scheduleTerms.get(trmOid);
        } else {
            term = (ScheduleTerm) getBroker().getBeanByOid(ScheduleTerm.class, trmOid);
            m_scheduleTerms.put(trmOid, term);
        }
        return term;
    }

    /**
     * Cache schedule term dates.
     *
     * @param term ScheduleTerm
     * @return Collection
     */
    protected Collection<ScheduleTermDate> getScheduleTermDates(ScheduleTerm term) {
        Collection<ScheduleTermDate> termDates = null;
        if (m_scheduleTermDates.containsKey(term.getOid())) {
            termDates = m_scheduleTermDates.get(term.getOid());
        } else {
            termDates = term.getScheduleTermDates();
            m_scheduleTermDates.put(term.getOid(), termDates);
        }
        return termDates;
    }

    /**
     * Cache staff attendance.
     *
     * @param staff SisStaff
     * @return Collection
     */
    protected Collection<StaffAttendance> getStaffAttendance(SisStaff staff) {
        return m_staffAttendances.get(staff.getOid());
    }

    /**
     * Cache teacher sections.
     *
     * @param masterSchedule MasterSchedule
     * @return Collection
     */
    protected Collection<ScheduleTeacher> getTeacherSections(MasterSchedule masterSchedule) {
        Collection<ScheduleTeacher> teacherSchedules = null;
        if (m_teacherSections.containsKey(masterSchedule.getOid())) {
            teacherSchedules = m_teacherSections.get(masterSchedule.getOid());
        } else {
            teacherSchedules = masterSchedule.getTeacherSections();
            m_teacherSections.put(masterSchedule.getOid(), teacherSchedules);
        }
        return teacherSchedules;
    }

    /**
     * Build a query of staff schedules for the current year.
     *
     * @return QueryByCriteria
     */
    private QueryByCriteria getStaffScheduleQuery() {
        X2Criteria criteria = new X2Criteria();

        // Active schedule classes.
        criteria.addEqualToField(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + Section.REL_SCHEDULE + PATH_DELIMITER
                + Schedule.REL_SCHOOL + PATH_DELIMITER + SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER
                + SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                ScheduleTeacher.REL_SECTION + PATH_DELIMITER
                        + Section.COL_SCHEDULE_OID);

        // "Class" type classes.
        criteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + Section.REL_SCHOOL_COURSE + PATH_DELIMITER
                + SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);

        // Exclude flags.
        criteria.addNotEqualTo(ScheduleTeacher.REL_STAFF + PATH_DELIMITER + m_fieldStfExclInd,
                BooleanAsStringConverter.TRUE);
        criteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + m_sectionExcludeFromEvalIndField,
                BooleanAsStringConverter.TRUE);
        if (!StringUtils.isEmpty(m_fieldMtcExclude)) {
            criteria.addNotEqualTo(m_fieldMtcExclude, BooleanAsStringConverter.TRUE);
        }
        // no sections without students

        // no sections without students. This will allow sections with staff but no students to be
        // part of the extract to account for a withdrawn student.
        // criteria.addGreaterThan(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
        // MasterSchedule.COL_ENROLLMENT_TOTAL, Integer.valueOf(0));

        // criteria from user input definition
        applyInputCriteria(criteria, false, ScheduleTeacher.REL_STAFF);

        // criteria for school selection.
        if (isSchoolContext()) {
            criteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHEDULE
                    + PATH_DELIMITER + Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHEDULE
                    + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);

            criteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHEDULE
                    + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }
        criteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHEDULE
                + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER + m_fieldSklExcludeSchool,
                Boolean.TRUE);

        // create query
        QueryByCriteria query = new QueryByCriteria(ScheduleTeacher.class, criteria);
        query.addOrderBy(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + MasterSchedule.COL_COURSE_VIEW, true);

        m_teacherSections = getBroker().getGroupedCollectionByQuery(query, ScheduleTeacher.COL_SECTION_OID, 1024);

        return query;
    }

    /**
     * Load the most common calendars for the schedules selected that have students assigned.
     */

    private void loadMostCommonCalendarSection() {
        X2Criteria sscCriteria = new X2Criteria();

        sscCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_DISTRICT_CONTEXT_OID,
                m_currentContextOid);

        // From active Schedule
        sscCriteria.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL
                + PATH_DELIMITER + SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER
                + SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, StudentSchedule.COL_SCHEDULE_OID);

        // check school or organization selection.
        if (isSchoolContext()) {
            sscCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID, getSchool()
                    .getOid());
        } else {
            sscCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL
                    + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);

            sscCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL
                    + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        sscCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL
                + PATH_DELIMITER + m_fieldSklExcludeSchool, Boolean.TRUE);
        String[] columns =
                new String[] {StudentSchedule.COL_SECTION_OID,
                        StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_CALENDAR_CODE, "count(*)"};

        ReportQueryByCriteria query = new ReportQueryByCriteria(StudentSchedule.class, columns, sscCriteria);
        query.addGroupBy(columns[0]);
        query.addGroupBy(columns[1]);
        query.addOrderByDescending(columns[2]);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String sectionOid = (String) row[0];
                String calendarCode = (String) row[1];

                String key = sectionOid;

                if (!m_mostCommonCalendarSection.containsKey(key)) {
                    m_mostCommonCalendarSection.put(key, calendarCode);
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Load ScheduleTerms map keyed on MST Oid.
     */
    private void loadScheduleTerms() {
        X2Criteria scheduleTermCriteria = new X2Criteria();

        scheduleTermCriteria.addEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_DISTRICT_CONTEXT_OID,
                m_currentContextOid);

        // From active Schedule
        scheduleTermCriteria.addEqualToField(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL
                + PATH_DELIMITER + SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER
                + SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, ScheduleTerm.COL_SCHEDULE_OID);

        // check school or organization selection.
        if (isSchoolContext()) {
            scheduleTermCriteria.addEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else {
            scheduleTermCriteria.addNotEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL
                    + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);

            scheduleTermCriteria.addNotEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL
                    + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        scheduleTermCriteria.addNotEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL
                + PATH_DELIMITER + m_fieldSklExcludeSchool, Boolean.TRUE);
        QueryByCriteria query = new QueryByCriteria(ScheduleTerm.class, scheduleTermCriteria);

        m_scheduleTerms = getBroker().getMapByQuery(query, X2BaseBean.COL_OID, 256);

    }

    /**
     * Load ScheduleTermDates map keyed on TRM Oid.
     */
    private void loadScheduleTermDatesByTRMOid() {

        X2Criteria datesCriteria = new X2Criteria();

        // From active Schedule
        datesCriteria.addEqualToField(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.REL_SCHEDULE
                + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER + SisSchool.REL_ACTIVE_SCHOOL_SCHED
                + PATH_DELIMITER + SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, StudentSchedule.COL_SCHEDULE_OID);

        datesCriteria.addLessOrEqualThan(ScheduleTermDate.COL_START_DATE, m_reportDate);

        // check school or organization selection.
        if (isSchoolContext()) {
            datesCriteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.REL_SCHEDULE
                    + PATH_DELIMITER + Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            datesCriteria.addNotEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.REL_SCHEDULE
                    + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);

            datesCriteria.addNotEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.REL_SCHEDULE
                    + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }
        datesCriteria.addNotEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.REL_SCHEDULE
                + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER + m_fieldSklExcludeSchool,
                Boolean.TRUE);
        QueryByCriteria query = new QueryByCriteria(ScheduleTermDate.class, datesCriteria);

        m_scheduleTermDates =
                getBroker().getGroupedCollectionByQuery(query, ScheduleTermDate.COL_SCHEDULE_TERM_OID, 1024);
    }

    /**
     * Load SchoolCalendar and SchoolCalendarDate tables.
     */
    private void loadSchoolCalendars() {
        X2Criteria schoolCalendarCriteria = new X2Criteria();
        schoolCalendarCriteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        schoolCalendarCriteria.addNotEqualTo(
                SchoolCalendar.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_fieldSklExcludeSchool, Boolean.TRUE);
        QueryByCriteria schoolCalendarQuery = new QueryByCriteria(SchoolCalendar.class, schoolCalendarCriteria);
        m_schoolCalendars = getBroker().getCollectionByQuery(schoolCalendarQuery);

        X2Criteria schoolCalendarDateCriteria = new X2Criteria();
        schoolCalendarDateCriteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER
                + SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        schoolCalendarDateCriteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.valueOf(true));
        schoolCalendarDateCriteria.addNotEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER
                + SchoolCalendar.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_fieldSklExcludeSchool, Boolean.TRUE);

        QueryByCriteria schoolCalendarDateQuery =
                new QueryByCriteria(SchoolCalendarDate.class, schoolCalendarDateCriteria);

        m_schoolCalendarDatesMap =
                getBroker().getGroupedCollectionByQuery(schoolCalendarDateQuery,
                        SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID, 100);
    }

    /**
     * Load map of SFA keyed on STF oid.
     */
    private void loadStaffAttendancesBySTFOid() {
        X2Criteria sfaCriteria = new X2Criteria();

        if (isSchoolContext()) {
            sfaCriteria.addEqualTo(StaffAttendance.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            sfaCriteria.addNotEqualTo(StaffAttendance.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);

            sfaCriteria.addNotEqualTo(StaffAttendance.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
        }
        sfaCriteria.addNotEqualTo(StaffAttendance.REL_SCHOOL + PATH_DELIMITER + m_fieldSklExcludeSchool, Boolean.TRUE);
        QueryByCriteria query = new QueryByCriteria(StaffAttendance.class, sfaCriteria);
        m_staffAttendances = getBroker().getGroupedCollectionByQuery(query, StaffAttendance.COL_STAFF_OID, 1024);
    }
}
