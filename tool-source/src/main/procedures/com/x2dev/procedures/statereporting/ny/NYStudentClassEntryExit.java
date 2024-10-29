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
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New York state procedure for Student Class Entry Exit export.
 *
 * @author X2 Development Corporation
 */

public class NYStudentClassEntryExit extends StateReportData {
    // static Logger m_logger = AppGlobals.getLog();

    /**
     * Entity class for Student Class Entry Exit export.
     *
     * @author X2 Development Corporation
     */

    public static class StudentClassEntryExitEntity extends StateReportEntity {
        /**
         * Local variables for reporting information.
         */
        private NYStudentClassEntryExit m_data;

        private List<StudentCourseRecord> m_studentCourseRecords;
        private List<StudentScheduleSpan> m_studentScheduleSpans;
        private ArrayList<PlainDate> m_scheduleDropDates;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StudentClassEntryExitEntity() {
            // no argument constructor
        }

        /**
         * This method returns the current staff student course record.
         *
         * @return StaffStudentCourseRecord
         */
        public StudentCourseRecord getStudentCourseRecord() {
            return m_studentCourseRecords.get(getCurrentRow());
        }

        /**
         * Gets the student course records.
         *
         * @return the m_staffStudentCourseRecords
         */
        public List<StudentCourseRecord> getStudentCourseRecords() {
            return m_studentCourseRecords;
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
            m_data = (NYStudentClassEntryExit) data;

            SisStudent student = (SisStudent) bean;

            m_studentCourseRecords = new ArrayList<StudentCourseRecord>();
            m_studentScheduleSpans = m_data.m_helper.getStudentScheduleSpans(student);

            populateDropDates();

            for (StudentScheduleSpan studentScheduleSpan : m_studentScheduleSpans) {
                PlainDate spanStartDate = studentScheduleSpan.getEntryDate();
                PlainDate spanExitDate = studentScheduleSpan.getExitDate();

                // If the class begin is outside the span, use the Student Enrollment date as the
                // Class begin date and show validation error message.
                StudentEnrollment entry = m_data.m_helper.getEnrollmentForDate(student.getOid(), spanStartDate,
                        StudentEnrollment.ENTRY);
                StudentEnrollment withdrawal = m_data.m_helper.getEnrollmentForDate(student.getOid(), spanStartDate,
                        StudentEnrollment.WITHDRAWAL);
                if (entry != null && withdrawal != null
                        && withdrawal.getEnrollmentDate().after(entry.getEnrollmentDate())) {
                    entry = m_data.m_helper.getEnrollmentForDate(student.getOid(), spanExitDate,
                            StudentEnrollment.ENTRY);
                    if (entry != null && entry.getEnrollmentDate().after(spanStartDate)) {
                        addValidationError(VALIDATION_ERROR_TYPE_CLASS_ENTRY_DATE_OUT,
                                "School: " + studentScheduleSpan.getSection().getSchoolCourse().getSchool().getName()
                                        + "\n" +
                                        "School Entry Date: " + entry.getEnrollmentDate() + "\n" +
                                        "Class: " + studentScheduleSpan.getSection().getCourseView() + "\n" +
                                        "Class Entry Date: " + spanStartDate);
                        spanStartDate = entry.getEnrollmentDate();
                    }
                }

                // First filter if the district start date, and the span entry
                // date are null
                if (m_data.m_schoolYearStartDate != null && spanStartDate != null
                // Then filter if the span date is before the district start
                // date
                        && spanStartDate.before(m_data.m_schoolYearStartDate) ||
                // Or filter if spanStartDate is after report date (any record with a Future ENTRY
                // date should not be in the output file.)
                        spanStartDate.after(m_data.m_reportDate) ||
                        // Or filter if the course is set to be filtered or doesn't
                        // meet criteria
                        filterCourse(studentScheduleSpan) ||
                        // Or filter if the student left the district
                        (spanStartDate != null && spanStartDate.equals(spanExitDate))) {
                    continue;
                }

                MasterSchedule masterSchedule = studentScheduleSpan.getSection();

                String sectionOid = masterSchedule.getOid();

                spanExitDate = calcStudentExitDateFromSpanOrEnrollment(studentScheduleSpan, student);

                if (masterSchedule != null) {
                    if (!m_data.m_termCodeMap.containsKey(masterSchedule.getOid())) {
                        populateStartEndDateAndTermCodeForMasterSchedule(masterSchedule);
                    }

                    String termCode = m_data.m_termCodeMap.get(sectionOid);

                    if (!m_data.m_masterScheduleSchoolMap.containsKey(sectionOid)) {
                        populateMasterScheduleMaps(masterSchedule);
                    }

                    // Course info
                    SchoolCourse schoolCourse = m_data.m_masterScheduleSchoolCourseMap.get(sectionOid);
                    Course course = m_data.m_masterScheduleCourseMap.get(sectionOid);

                    String courseNumber = "";
                    Boolean dualEnrollment = Boolean.FALSE;
                    if (course != null) {
                        courseNumber = course.getNumber();
                        dualEnrollment =
                                BooleanAsStringConverter.TRUE
                                        .equals(course.getFieldValueByBeanPath(m_data.m_dualEnrollmentField))
                                                ? Boolean.TRUE
                                                : Boolean.FALSE;

                        if (dualEnrollment.booleanValue()) {
                            StudentSchedule stdSchedule = studentScheduleSpan.getSchedule();
                            if (stdSchedule != null && BooleanAsStringConverter.TRUE
                                    .equals(stdSchedule.getFieldValueByBeanPath(m_data.m_dualOptOutField))) {
                                dualEnrollment = Boolean.FALSE;
                            }
                        }
                    }

                    // Section School info
                    SisSchool school = m_data.m_masterScheduleSchoolMap.get(sectionOid);

                    if (schoolCourse != null && school != null) {
                        spanStartDate = calcAdjustedEnrollmentDate(school, student, spanStartDate);
                    }
                    if (spanExitDate != null) {
                        if (spanStartDate.after(spanExitDate)) {
                            addValidationError(VALIDATION_ERROR_TYPE_CLASS_START_DATE_AFTER_END_DATE,
                                    "Class: " + studentScheduleSpan.getSection().getCourseView() + "\n" +
                                            "Class Entry Date: " + spanStartDate + "\n" +
                                            "Class Exit Date:" + spanExitDate);
                        }
                    }

                    if (!includeByDaysLength(spanStartDate, spanExitDate)) {
                        continue;
                    }

                    String schoolLocationCode = "";
                    if (school != null) {
                        schoolLocationCode = (String) school.getFieldValueByBeanPath(m_data.m_schoolLocationCodeField);
                    }

                    String sectionCode = calcSectionCode(masterSchedule);
                    if (!spanStartDate.after(m_data.m_reportDate)) {
                        StudentCourseRecord staffStudentCrsRecord = m_data.new StudentCourseRecord();

                        staffStudentCrsRecord.setStudentId(student.getLocalId());
                        staffStudentCrsRecord.setCourseLocationCode(schoolLocationCode);
                        staffStudentCrsRecord.setCourseCode(courseNumber);
                        staffStudentCrsRecord.setSectionCode(sectionCode);
                        staffStudentCrsRecord.setTermCode(termCode);
                        staffStudentCrsRecord.setClassEntryDate(spanStartDate);
                        staffStudentCrsRecord.setClassExitDate(spanExitDate);
                        staffStudentCrsRecord.setDualEnr(dualEnrollment);

                        // Add Student Course Record
                        m_studentCourseRecords.add(staffStudentCrsRecord);
                    }
                }
            }

            setRowCount(m_studentCourseRecords.size());
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
         * Adds the validation error.
         *
         * @param errorType String
         * @param errorMessage String
         */
        private void addValidationError(String errorType, String errorMessage) {
            List<StateReportValidationError> errors = m_data.m_validationErrors.get(errorType);
            if (errors == null) {
                errors = new ArrayList<>();
                m_data.m_validationErrors.put(errorType, errors);
            }
            errors.add(new StateReportValidationError(this.toString(), "", errorType, errorMessage));
        }

        /**
         * Aspen enters entry and withdrawals on the same day, because of counting times in course,
         * any adds within
         * the middle of a semester go to the next day active day within their calendar. Unless
         * students first day in school is that date
         *
         * @param school SisSchool
         * @param student SisStudent
         * @param spanStartDate PlainDate
         * @return PlainDate
         */
        private PlainDate calcAdjustedEnrollmentDate(SisSchool school, SisStudent student, PlainDate spanStartDate) {
            PlainDate adjustedDate = null;
            PlainDate calendarStartDate = null;
            StudentEnrollment entry = m_data.m_helper.getEnrollmentForDate(student.getOid(), spanStartDate,
                    StudentEnrollment.ENTRY);

            if ((m_scheduleDropDates.contains(spanStartDate)) &&
                    (entry == null || !spanStartDate.equals(entry.getEnrollmentDate()))) {
                if (school != null && spanStartDate != null) {
                    Collection<SchoolCalendar> schoolCalendars = m_data.getSchoolCalendars(school);
                    SchoolCalendar activeSchoolCalendar = m_data.getActiveSchoolCalendar(schoolCalendars,
                            student.getCalendarCode());
                    if (activeSchoolCalendar != null) {
                        for (SchoolCalendarDate date : m_data.m_schoolCalendarDatesMap
                                .get(activeSchoolCalendar.getOid())) {
                            if (date.getInSessionIndicator()) {
                                if (date.getDate().after(spanStartDate) &&
                                        (adjustedDate == null || adjustedDate.after(date.getDate()))) {
                                    adjustedDate = date.getDate();
                                }

                                if (calendarStartDate == null || calendarStartDate.after(date.getDate())) {
                                    calendarStartDate = date.getDate();
                                }
                            }
                        }
                    }
                }
            }

            if (adjustedDate == null || (calendarStartDate != null && calendarStartDate.equals(spanStartDate))) {
                adjustedDate = spanStartDate;
            }

            return adjustedDate;
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
         * compare the student schedule span and the enrollment to see which date is appropriate.
         * Also looks at if the student left and came back
         *
         * @param studentScheduleSpan StudentScheduleSpan
         * @param student SisStudent
         * @return PlainDate
         */
        private PlainDate calcStudentExitDateFromSpanOrEnrollment(StudentScheduleSpan studentScheduleSpan,
                                                                  SisStudent student) {
            PlainDate exitDate = studentScheduleSpan.getExitDate();
            if (exitDate != null) {
                StudentEnrollment withdrawal = m_data.m_helper.getEnrollmentForDate(student.getOid(), exitDate,
                        StudentEnrollment.WITHDRAWAL);
                if (withdrawal != null) {
                    MasterSchedule masterSchedule = studentScheduleSpan.getSection();
                    SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                    String sectionSchoolOid = schoolCourse.getSchoolOid();

                    if (withdrawal.getSchoolOid().equals(sectionSchoolOid)) {
                        StudentEnrollment entry = m_data.m_helper.getEnrollmentForDate(student.getOid(), exitDate,
                                StudentEnrollment.ENTRY);
                        if (entry == null || entry.getEnrollmentDate().before(withdrawal.getEnrollmentDate())
                                || !entry.getSchoolOid().equals(sectionSchoolOid)) {

                            if (withdrawal.getEnrollmentDate().before(exitDate)) {
                                addValidationError(VALIDATION_ERROR_TYPE_CLASS_EXIT_DATE_OUT,
                                        "School: "
                                                + studentScheduleSpan.getSection().getSchoolCourse().getSchool()
                                                        .getName()
                                                + "\n" +
                                                "School Exit Date: " + withdrawal.getEnrollmentDate() + "\n" +
                                                "Class: " + studentScheduleSpan.getSection().getCourseView() + "\n" +
                                                "Class Exit Date: " + exitDate);
                            }

                            exitDate = withdrawal.getEnrollmentDate();

                            if (exitDate.before(studentScheduleSpan.getEntryDate())) {
                                addValidationError(VALIDATION_ERROR_TYPE_CLASS_ENTRY_DATE_AFTER_SCHOOL_EXIT,
                                        "School: "
                                                + studentScheduleSpan.getSection().getSchoolCourse().getSchool()
                                                        .getName()
                                                + "\n" +
                                                "School Exit Date: " + withdrawal.getEnrollmentDate() + "\n" +
                                                "Class: " + studentScheduleSpan.getSection().getCourseView() + "\n" +
                                                "Class Entry Date: " + studentScheduleSpan.getEntryDate());
                            }
                        }
                    }
                }
            }

            return exitDate;
        }

        /**
         * Checks for courses to be filtered then caches the results.
         *
         * @param studentScheduleSpan StudentScheduleSpan
         * @return boolean
         */
        private boolean filterCourse(StudentScheduleSpan studentScheduleSpan) {
            boolean filter = false;

            Course course = null;
            MasterSchedule section = studentScheduleSpan.getSection();

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

                        if (!BooleanAsStringConverter.TRUE
                                .equals(course.getFieldValueByBeanPath(m_data.m_courseActiveIndicatorField))) {
                            filter = true;
                        }

                        if (StringUtils.isEmpty(stateCourseCode)) {
                            filter = true;
                        }
                    } else {
                        filter = true;
                    }

                    m_data.m_courseFilter.put(section.getOid(), Boolean.valueOf(filter));
                } else {
                    filter = m_data.m_courseFilter.get(section.getOid()).booleanValue();
                }
            }

            if (studentScheduleSpan.getEntryDate() != null &&
                    studentScheduleSpan.getEntryDate().equals(studentScheduleSpan.getExitDate())) {
                filter = true;
            }

            return filter;
        }

        /**
         * Include by dates length.
         *
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @return true, if successful
         */
        private boolean includeByDaysLength(PlainDate startDate, PlainDate endDate) {
            boolean include = true;
            if (endDate != null) {
                Calendar calendar = Calendar.getInstance();
                calendar.setTime(startDate);
                calendar.add(Calendar.DAY_OF_MONTH, m_data.m_minMembDaysLength.intValue());
                include = !calendar.getTime().after(endDate);
            }
            return include;
        }

        /**
         * Populates a map of the students drop dates. This is so that we can check if a student
         * left a class on a particular date.
         */
        private void populateDropDates() {
            m_scheduleDropDates = new ArrayList<PlainDate>();

            for (StudentScheduleSpan span : m_studentScheduleSpans) {
                if (span.getExitChange() != null) {
                    m_scheduleDropDates.add(span.getExitDate());
                }
            }
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
                String stateTermCode = m_data.lookupReferenceCodeByBeanPath(ScheduleTerm.class,
                        ScheduleTerm.COL_CODE,
                        termCode,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                if (stateTermCode == null) {
                    stateTermCode = termCode;
                }

                m_data.m_termCodeMap.put(masterSchedule.getOid(), stateTermCode);
            }
        }
    }

    /**
     * This class returns the Staff Student Course Records.
     */
    protected class RetrieveStudentCourseDetails implements FieldRetriever {

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
            StudentCourseRecord stdCrsRec = ((StudentClassEntryExitEntity) entity).getStudentCourseRecord();
            PlainDate reportDate = ((NYStudentClassEntryExit) data).m_reportDate;

            if (CALC_PARAM_STUDENT_ID.equals(param)) {
                value = stdCrsRec.getStudentId();
            } else if (CALC_PARAM_CRS_LOC_CODE.equals(param)) {
                value = stdCrsRec.getCourseLocationCode();
            } else if (CALC_PARAM_CRS_CODE.equals(param)) {
                value = stdCrsRec.getCourseCode();
            } else if (CALC_PARAM_SECTION_CODE.equals(param)) {
                value = stdCrsRec.getSectionCode();
            } else if (CALC_PARAM_CLASS_ENTRY_DATE.equals(param)) {
                value = stdCrsRec.getClassEntryDate();
            } else if (CALC_PARAM_CLASS_EXIT_DATE.equals(param)) {
                PlainDate exitDate = (PlainDate) stdCrsRec.getClassExitDate();
                if (exitDate != null) {
                    value = reportDate.after(exitDate) ? exitDate : null;
                }
            } else if (CALC_PARAM_TERM_CODE.equals(param)) {
                value = stdCrsRec.getTermCode();
            } else if (CALC_PARAM_DUAL_ENROLLMENT.equals(param)) {
                value = stdCrsRec.getDualEnr();
            }

            return value;
        }
    }

    /**
     * Inner data object for saving all of the student information during the pre-processing of the
     * export.
     */
    private class StudentCourseRecord {
        private Date classEntryDate;
        private Date classExitDate;
        private String courseLocationCode;
        private String courseCode;
        private Boolean dualEnrollment;
        private String sectionCode;
        private String studentId;
        private String termCode;

        /**
         * Instantiates a new student course record.
         */
        public StudentCourseRecord() {

        }

        /**
         * Gets the dual enr.
         *
         * @return Boolean
         */
        public Boolean getDualEnr() {
            return this.dualEnrollment;
        }

        /**
         * Sets the dual enr.
         *
         * @param dualEnr void
         */
        public void setDualEnr(Boolean dualEnr) {
            this.dualEnrollment = dualEnr;
        }

        /**
         * Gets the student id.
         *
         * @return String
         */
        public String getStudentId() {
            return studentId;
        }

        /**
         * Sets the student id.
         *
         * @param studentId void
         */
        public void setStudentId(String studentId) {
            this.studentId = studentId;
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
         * Sets the section code.
         *
         * @param sectionCode void
         */
        public void setSectionCode(String sectionCode) {
            this.sectionCode = sectionCode;
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
            this.termCode = termCode;
        }

        /**
         * Gets the class entry date.
         *
         * @return the classEntryDate
         */
        public Date getClassEntryDate() {
            return classEntryDate;
        }

        /**
         * Sets the class entry date.
         *
         * @param classEntryDate the classEntryDate to set
         */
        public void setClassEntryDate(Date classEntryDate) {
            this.classEntryDate = classEntryDate;
        }

        /**
         * Gets the class exit date.
         *
         * @return the classExitDate
         */
        public Date getClassExitDate() {
            return classExitDate;
        }

        /**
         * Sets the class exit date.
         *
         * @param classExitDate the classExitDate to set
         */
        public void setClassExitDate(Date classExitDate) {
            this.classExitDate = classExitDate;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "Record: crsCode=" + courseCode +
                    ", sectionCode=" + sectionCode +
                    ", classEntryDate=" + classEntryDate +
                    ", classExitDate=" + classExitDate + ".";
        }

    }

    /**
     * Input Definition Parameters
     */
    protected static final String PARAM_EXCLUDE_REPORT_DATE = "excludeReportDate";
    protected static final String PARAM_FILTER_COURSES = "filterCourses";
    protected static final String PARAM_FILTER_COURSES_CODE = "filterCoursesCode";
    protected static final String PARAM_MIN_MEMBERSHIP_DAYS_LENGTH = "minMembDaysLength";
    protected static final String PARAM_REMOVE_HEADER = "removeHeader";
    protected static final String PARAM_REPORT_DATE = "reportDate";

    /**
     * Retriever Parameters
     */
    protected static final String CALC_ID_STUDENT_COURSE = "STD-CRS";

    protected static final String CALC_PARAM_CRS_CODE = "CRS-CODE";
    protected static final String CALC_PARAM_CRS_LOC_CODE = "CRS-LOC-CODE";
    protected static final String CALC_PARAM_DUAL_ENROLLMENT = "DUAL-ENR";
    protected static final String CALC_PARAM_SECTION_CODE = "SECTION-CODE";
    protected static final String CALC_PARAM_STUDENT_ID = "STUDENT-ID";
    protected static final String CALC_PARAM_CLASS_ENTRY_DATE = "CRS-ENTRY-DATE";
    protected static final String CALC_PARAM_CLASS_EXIT_DATE = "CRS-EXIT-DATE";
    protected static final String CALC_PARAM_TERM_CODE = "TERM-CODE";

    /**
     * Aliases
     */
    // SCHOOL
    protected static final String ALIAS_LOCATION_CODE = "LOCATION CODE";
    protected static final String ALIAS_SKL_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";

    // COURSE
    protected static final String ALIAS_ACTIVE_INDICATOR = "DOE ACTIVE";
    protected static final String ALIAS_CRS_DUAL_ENROLLMENT = "all-crs-DualConcurrentCredit";
    protected static final String ALIAS_SSC_DUAL_OPT_OUT = "all-ssc-DualCreditOptOut";
    protected static final String ALIAS_STATE_COURSE_CODE = "DOE STATE COURSE";

    // MASTER_SCHEDULE
    protected static final String ALIAS_PLATOON_CODE = "DOE SECTION OVERRIDE";


    protected static final String VALIDATION_ERROR_TYPE_CLASS_ENTRY_DATE_AFTER_SCHOOL_EXIT =
            "Class entry date is after school exit date";
    protected static final String VALIDATION_ERROR_TYPE_CLASS_ENTRY_DATE_OUT =
            "Class entry date is before school entry date";
    protected static final String VALIDATION_ERROR_TYPE_CLASS_EXIT_DATE_OUT =
            "Class exit date is after school exit date";
    protected static final String VALIDATION_ERROR_TYPE_CLASS_START_DATE_AFTER_END_DATE =
            "Class entry date is after class exit date";

    /**
     * Primitive members
     */
    protected boolean m_excludeReportDate;
    protected boolean m_removeHeaderIndicator;
    protected int m_filterCourse;

    /**
     * Reference types members
     */
    protected String m_courseActiveIndicatorField;
    protected String m_courseNumberFilter;
    protected String m_currentContextOid;
    protected String m_dualEnrollmentField;
    protected String m_dualOptOutField;
    protected String m_fieldSklExcludeSchool;
    protected StudentHistoryHelper m_helper;
    protected Integer m_minMembDaysLength;
    protected PlainDate m_reportDate;
    protected String m_schoolLocationCodeField;
    protected PlainDate m_schoolYearStartDate;
    protected String m_sectionCodeOverrideField;
    protected String m_stateCourseCodeField;

    /**
     * Collections
     */
    protected Collection<MasterTerm> m_masterTerms = new ArrayList<MasterTerm>();
    protected Map<String, Collection<MasterScheduleMatrix>> m_scheduleMasterMatrices = new HashMap();
    protected Collection<SchoolCalendar> m_schoolCalendars;

    /**
     * Maps
     */
    protected Map<String, Boolean> m_courseFilter = new HashMap<String, Boolean>();
    protected Map<String, Course> m_masterScheduleCourseMap = new HashMap<String, Course>();
    protected Map<String, SchoolCourse> m_masterScheduleSchoolCourseMap = new HashMap<String, SchoolCourse>();
    protected Map<String, SisSchool> m_masterScheduleSchoolMap = new HashMap<String, SisSchool>();
    protected Map<String, Collection<SisSchoolCalendarDate>> m_schoolCalendarDatesMap;
    protected Map<String, String> m_termCodeMap = new HashMap<String, String>();
    protected Map<String, PlainDate> m_termEndDateMap = new HashMap<String, PlainDate>();
    protected Map<String, PlainDate> m_termStartDateMap = new HashMap<String, PlainDate>();

    private static final String COMMA = ",";

    private Map<String, ScheduleTerm> m_scheduleTerms = new HashMap();
    private Map<String, Collection<ScheduleTermDate>> m_scheduleTermDates = new HashMap();
    private Map<String, Collection<SchoolCalendar>> m_schoolCalendarsMap = new HashMap();
    private Map<String, Collection<ScheduleTeacher>> m_teacherSections = new HashMap();

    private Map<String, List<StateReportValidationError>> m_validationErrors = new HashMap<>();

    private List<String> m_validationErrorTypes = null;

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
             * Build helper object.
             */
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SCHEDULE_SPANS, Boolean.TRUE);

            X2Criteria studentCriteria = m_helper.getStudentCriteria();
            studentCriteria.addNotEqualTo(
                    SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_fieldSklExcludeSchool,
                    Boolean.TRUE);

            m_validationErrorTypes = Arrays.asList(
                    VALIDATION_ERROR_TYPE_CLASS_ENTRY_DATE_AFTER_SCHOOL_EXIT,
                    VALIDATION_ERROR_TYPE_CLASS_ENTRY_DATE_OUT,
                    VALIDATION_ERROR_TYPE_CLASS_START_DATE_AFTER_END_DATE,
                    VALIDATION_ERROR_TYPE_CLASS_EXIT_DATE_OUT);

            /*
             * Load lookup tables
             */
            loadSchoolCalendars();

            loadSchoolCalendarsBySKLOid();
            loadScheduleTerms();
            loadScheduleTermDatesByTRMOid();

            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(true));
            setEntityClass(StudentClassEntryExitEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALC_ID_STUDENT_COURSE, new RetrieveStudentCourseDetails());
            super.addCalcs(calcs);
        }
    }

    /**
     * basic initialize fields.
     *
     * @throws X2BaseException exception
     */
    public void initializeFields() throws X2BaseException {
        // System Parameters
        m_schoolYearStartDate = getOrganization().getCurrentContext().getStartDate();
        m_currentContextOid = ((SisOrganization) getOrganization()).getCurrentContextOid();

        m_filterCourse = 0;
        if (getParameter(PARAM_FILTER_COURSES) != null) {
            String filterCourse = (String) getParameter(PARAM_FILTER_COURSES);
            if (StringUtils.isNumeric(filterCourse)) {
                m_filterCourse = Integer.valueOf(filterCourse).intValue();
            }
        }
        m_courseNumberFilter = (String) getParameter(PARAM_FILTER_COURSES_CODE);
        m_excludeReportDate = true;
        if (getParameter(PARAM_EXCLUDE_REPORT_DATE) != null) {
            m_excludeReportDate = ((Boolean) getParameter(PARAM_EXCLUDE_REPORT_DATE)).booleanValue();
        }
        m_removeHeaderIndicator = false;
        if (getParameter(PARAM_REMOVE_HEADER) != null) {
            m_removeHeaderIndicator = ((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue();
        }
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);

        m_minMembDaysLength = (Integer) getParameter(PARAM_MIN_MEMBERSHIP_DAYS_LENGTH);
        if (m_minMembDaysLength == null || m_minMembDaysLength.intValue() < 0) {
            m_minMembDaysLength = Integer.valueOf(1);
        }

        // Load Alias database field Names
        m_courseActiveIndicatorField = translateAliasToJavaName(ALIAS_ACTIVE_INDICATOR, true);
        m_schoolLocationCodeField = translateAliasToJavaName(ALIAS_LOCATION_CODE, true);
        m_sectionCodeOverrideField = translateAliasToJavaName(ALIAS_PLATOON_CODE, false);
        m_stateCourseCodeField = translateAliasToJavaName(ALIAS_STATE_COURSE_CODE, true);
        m_dualEnrollmentField = translateAliasToJavaName(ALIAS_CRS_DUAL_ENROLLMENT, true);
        m_dualOptOutField = translateAliasToJavaName(ALIAS_SSC_DUAL_OPT_OUT, true);
        m_fieldSklExcludeSchool = translateAliasToJavaName(ALIAS_SKL_EXCLUDE_SCHOOL, true);
    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#postProcess()
     */
    @Override
    public Collection<StateReportValidationError> postProcess() {
        List<StateReportValidationError> errors = new ArrayList<>();
        if (m_validationErrors.size() > 0) {
            for (List<StateReportValidationError> errorsOfType : m_validationErrors.values()) {
                errors.addAll(errorsOfType);
            }
        }
        Collections.sort(errors, new Comparator<StateReportValidationError>() {
            @Override
            public int compare(StateReportValidationError o1, StateReportValidationError o2) {
                return o1.getEntityName().compareTo(o2.getEntityName());
            }
        });

        for (String valErrorType : m_validationErrorTypes) {
            Collection errorsOfType = m_validationErrors.get(valErrorType);
            if (errorsOfType != null && errorsOfType.size() > 0) {
                StateReportValidationError totalRow =
                        new StateReportValidationError("Total:", "", valErrorType, String.valueOf(errorsOfType.size()));
                errors.add(totalRow);
            }
        }
        return errors;
    }

    /**
     * Get active school calendar based on the student's calendar code and the current organization
     * context.
     *
     * @param schoolCalendars Collection<SchoolCalendar>
     * @param studentCalendarCode String
     * @return SchoolCalendar
     */
    protected SchoolCalendar getActiveSchoolCalendar(Collection<SchoolCalendar> schoolCalendars,
                                                     String studentCalendarCode) {
        SchoolCalendar currentSchoolCalendar = null;

        /*
         * Find the school calendar which is equal to the current context.
         * Once you have the school calendar, get the list of all in session dates
         */
        for (SchoolCalendar schoolCalendar : schoolCalendars) {
            if (m_currentContextOid != null && m_currentContextOid.equals(schoolCalendar.getDistrictContextOid())
                    && studentCalendarCode != null && studentCalendarCode.equals(schoolCalendar.getCalendarId())) {
                currentSchoolCalendar = schoolCalendar;
                break;
            }
        }

        return currentSchoolCalendar;
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
     * Get school calendars for a school.
     *
     * @param school SisSchool
     * @return Collection<SchoolCalendar>
     */
    protected Collection<SchoolCalendar> getSchoolCalendars(SisSchool school) {
        Collection<SchoolCalendar> calendars = null;
        if (m_schoolCalendarsMap.containsKey(school.getOid())) {
            calendars = m_schoolCalendarsMap.get(school.getOid());
        } else {
            calendars = new ArrayList<SchoolCalendar>();

            for (SchoolCalendar SchoolCalendar : m_schoolCalendars) {
                if (SchoolCalendar.getSchoolOid().equals(school.getOid())) {
                    calendars.add(SchoolCalendar);
                }
            }
            m_schoolCalendarsMap.put(school.getOid(), calendars);
        }

        return calendars;
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
     * Load ScheduleTerms map keyed on MST Oid.
     */
    private void loadScheduleTerms() {
        X2Criteria scheduleTermCriteria = new X2Criteria();

        scheduleTermCriteria.addEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_DISTRICT_CONTEXT_OID,
                m_currentContextOid);

        // From active Schedule
        scheduleTermCriteria.addEqualToField(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                ScheduleTerm.COL_SCHEDULE_OID);

        // check school or organization selection.
        if (isSchoolContext()) {
            scheduleTermCriteria.addEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            scheduleTermCriteria.addNotEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);

            scheduleTermCriteria.addNotEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        scheduleTermCriteria.addNotEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER + m_fieldSklExcludeSchool, Boolean.TRUE);
        QueryByCriteria query = new QueryByCriteria(ScheduleTerm.class, scheduleTermCriteria);
        m_scheduleTerms = getBroker().getMapByQuery(query, X2BaseBean.COL_OID, 256);
    }

    /**
     * Load ScheduleTermDates map keyed on TRM Oid.
     */
    private void loadScheduleTermDatesByTRMOid() {

        X2Criteria datesCriteria = new X2Criteria();

        // From active Schedule
        datesCriteria.addEqualToField(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentSchedule.COL_SCHEDULE_OID);

        datesCriteria.addLessOrEqualThan(ScheduleTermDate.COL_START_DATE, m_reportDate);

        // check school or organization selection.
        if (isSchoolContext()) {
            datesCriteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            datesCriteria.addNotEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);

            datesCriteria.addNotEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        datesCriteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER + m_fieldSklExcludeSchool, Boolean.TRUE);
        QueryByCriteria query = new QueryByCriteria(ScheduleTermDate.class, datesCriteria);

        m_scheduleTermDates =
                getBroker().getGroupedCollectionByQuery(query, ScheduleTermDate.COL_SCHEDULE_TERM_OID, 1024);
    }

    /**
     * Load SchoolCalendar and SchoolCalendarDate tables.
     */
    private void loadSchoolCalendars() {
        X2Criteria schoolCalendarCriteria = new X2Criteria();

        QueryByCriteria schoolCalendarQuery = new QueryByCriteria(SchoolCalendar.class, schoolCalendarCriteria);

        m_schoolCalendars = getBroker().getCollectionByQuery(schoolCalendarQuery);

        X2Criteria schoolCalendarDateCriteria = new X2Criteria();
        schoolCalendarDateCriteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.valueOf(true));

        QueryByCriteria schoolCalendarDateQuery =
                new QueryByCriteria(SchoolCalendarDate.class, schoolCalendarDateCriteria);

        m_schoolCalendarDatesMap = getBroker().getGroupedCollectionByQuery(schoolCalendarDateQuery,
                SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID, 100);
    }

    /**
     * Load SchoolCalendars map keyed on SKL Oid.
     */
    private void loadSchoolCalendarsBySKLOid() {
        X2Criteria schoolCalendarCriteria = new X2Criteria();

        schoolCalendarCriteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_currentContextOid);

        // check school or organization selection.
        if (isSchoolContext()) {
            schoolCalendarCriteria.addEqualTo(SchoolCalendar.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            schoolCalendarCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);

            schoolCalendarCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        schoolCalendarCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                m_fieldSklExcludeSchool, Boolean.TRUE);
        QueryByCriteria query = new QueryByCriteria(SchoolCalendar.class, schoolCalendarCriteria);
        m_schoolCalendarsMap = getBroker().getGroupedCollectionByQuery(query, SchoolCalendar.COL_SCHOOL_OID, 1024);
    }
}
