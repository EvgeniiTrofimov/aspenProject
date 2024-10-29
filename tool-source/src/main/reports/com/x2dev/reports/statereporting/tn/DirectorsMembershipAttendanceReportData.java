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

package com.x2dev.reports.statereporting.tn;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictCalendar;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.procedures.statereporting.tn.TNClassSectionHelper;
import com.x2dev.procedures.statereporting.tn.TNClassSectionScheduleData;
import com.x2dev.procedures.statereporting.tn.TNClassSectionScheduleData.TNClassSectionScheduleEntity;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentEnrollmentSpan;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentMultiYearHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentScheduleSpan;
import com.x2dev.procedures.statereporting.tn.TNStateReportData;
import com.x2dev.procedures.statereporting.tn.TNStateReportData.Pair;
import com.x2dev.reports.statereporting.tn.TNReportingPeriodHelper.NetEnrolledInfo;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentContextAttributes;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.RoundingMode;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Query;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.springframework.util.CollectionUtils;

/**
 * Data source for the "Director's Annual Membership/Attendance" report.
 *
 * @author X2 Development Corporation
 */
public class DirectorsMembershipAttendanceReportData extends ReportJavaSourceNet {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * The Class EnrollmentStatistics.
     */
    class EnrollmentStatistics extends TNStateReportData {
        protected static final String PARAM_END_DATE = "endDate";
        protected static final String PARAM_START_DATE = "startDate";
        private static final String PARAM_INCLUDE_SECONDARY = "includeSecondary";
        private static final String REASON_CODE_ADULT_HS = "Adult HS";

        protected Map<String, ReferenceCode> m_referenceGradeCodeMap;
        protected TNEnrollmentHelper m_tnEnrHelper;
        protected TNStudentMultiYearHelper m_tnMultiYearHelper;

        private DateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
        private int m_maxGradeLevel;
        private TreeMap m_sortedGradeLevels;
        private TNStudentHistoryHelper m_tnStudentHelper;
        private Set<String> m_adultHSReasonCodes;

        /**
         * Close the student history helper.
         *
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#close()
         */
        @Override
        public void close() {
            super.close();
            if (m_tnStudentHelper != null) {
                m_tnStudentHelper.close();
            }
        }

        /**
         * Return calendar days.
         *
         * @param school SisSchool
         * @param calendar String
         * @return Set<PlainDate>
         */
        public Set<PlainDate> getCalendarDays(SisSchool school, String calendar) {
            return m_tnStudentHelper.getCalendarDays(school, calendar);
        }

        /**
         * Calculate grade code from StudentEnrollmentSpan.
         *
         * @param student SisStudent
         * @param span TNStudentEnrollmentSpan
         * @return ReferenceCode
         */
        public ReferenceCode getGradeLevel(SisStudent student, TNStudentEnrollmentSpan span) {
            ReferenceCode gradeCode = null;

            int yog = 0;
            // use student's attributes first (but only for current context)
            StudentContextAttributes attributes =
                    (StudentContextAttributes) m_tnEnrHelper.getStudentMultiYearHelper()
                            .getContextAttributes(student.getOid());
            if (attributes != null && attributes.getContextOid().equals(getCurrentContext().getOid())) {
                yog = ((Integer) m_tnEnrHelper.getStudentValueByBeanPath(student, SisStudent.COL_YOG)).intValue();
            }

            // then, if there are no attributes for current context or the yog of it == 0, use
            // span's yog
            if (yog == 0) {
                yog = span.getYog();
            }

            // and only then, if yog still is 0, use yog of student
            if (yog == 0) {
                yog = student.getYog();
            }

            List<String> matchingGradeLevels = StudentManager.getMatchingGradeLevels(m_maxGradeLevel,
                    yog,
                    getCurrentContext().getSchoolYear(),
                    m_sortedGradeLevels);
            for (String matchingGradeLevel : matchingGradeLevels) {
                gradeCode = m_referenceGradeCodeMap.get(matchingGradeLevel);
                if (gradeCode != null) {
                    break;
                }
            }


            return gradeCode;
        }

        /**
         * Return the current student criteria.
         *
         * @return X2Criteria
         */
        public X2Criteria getStudentCriteria() {
            X2Criteria crit = m_tnStudentHelper.getStudentCriteria();
            return crit;
        }

        /**
         * Returns a list of student enrollment spans that represent all of the students enrollment
         * activity and segments.
         *
         * @param student Student
         * @param limit boolean
         * @return List<StudentEnrollmentSpan>
         */
        public List<TNStudentEnrollmentSpan> getStudentEnrollmentSpans(Student student, boolean limit) {
            List<TNStudentEnrollmentSpan> spans = m_tnStudentHelper.getTNStudentEnrollmentSpans(student, limit);

            // remove adult education spans
            Iterator<TNStudentEnrollmentSpan> iterator = spans.iterator();
            while (iterator.hasNext()) {
                TNStudentEnrollmentSpan span = iterator.next();
                if (isAdultHSSpan(span)) {
                    iterator.remove();
                }
            }
            return spans;
        }

        /**
         * Return the current student query.
         *
         * @param distinct boolean
         * @return Query
         */
        public Query getStudentQuery(boolean distinct) {
            return m_tnStudentHelper.getStudentQuery(distinct);
        }

        /**
         * Return the list of student schedule spans.
         *
         * @param student SisStudent
         * @return List<StudentScheduleSpan>
         */
        public List<TNStudentScheduleSpan> getTNStudentScheduleSpans(SisStudent student) {
            return m_tnStudentHelper.getTNStudentScheduleSpans(student);
        }


        /**
         * Return student attendances.
         *
         * @param studentOid String
         * @return List<StudentAttendance>
         */
        public List<StudentAttendance> getStudentAttendances(String studentOid) {
            return m_tnStudentHelper.getStudentAttendances(studentOid);
        }

        /**
         * Gets the student standard day.
         *
         * @param student SisStudent
         * @param span TNStudentEnrollmentSpan
         * @return int
         */
        public int getStudentStandardDay(SisStudent student, TNStudentEnrollmentSpan span) {
            int stdStandardDay = 0;
            ReferenceCode refCode = getGradeLevel(student, span);
            if (refCode != null) {
                String standardDayString = m_tnMultiYearHelper.getHistoryValueByAlias(refCode,
                        ALIAS_HISTORY_STUDENT_STANDARD_DAY, ALIAS_STUDENT_STANDARD_DAY);
                if (!StringUtils.isEmpty(standardDayString)) {
                    try {
                        stdStandardDay = Integer.valueOf(standardDayString).intValue();
                    } catch (NumberFormatException nfe) {
                        stdStandardDay = 0; // Set the value back to 0
                    }
                }
            }
            return stdStandardDay;
        }

        /**
         * Get formatted date.
         *
         * @param date PlainDate
         * @return String
         */
        public String getFormattedDate(PlainDate date) {
            return m_dateFormat.format(date);
        }

        /**
         * Get Start Date.
         *
         * @return String
         */
        public String getStartDate() {
            return m_dateFormat
                    .format(m_tnStudentHelper.getSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE));
        }

        /**
         * Get End Date.
         *
         * @return String
         */
        public String getEndDate() {
            return m_dateFormat.format(m_tnStudentHelper.getSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE));
        }

        /**
         * Initialize the export.
         * Set up the student history helper.
         *
         * @throws X2BaseException exception
         */
        @Override
        public void initialize() throws X2BaseException {
            super.initialize();
            // Set to include the student schedule history when getting the student schedule span
            m_tnEnrHelper = new TNEnrollmentHelper(this);
            m_tnMultiYearHelper = m_tnEnrHelper.getStudentMultiYearHelper();
            m_tnStudentHelper = m_tnEnrHelper.getStudentHistoryHelper();
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SCHEDULE_SPANS, Boolean.TRUE);
            m_tnStudentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_EXCLUDE_FUTURE_SCHEDULES,
                    Boolean.TRUE);

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_schoolStartDate);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_periodEndDate);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_SECONDARY,
                    getParameter(PARAM_INCLUDE_SECONDARY));

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_ATTENDANCE, Boolean.FALSE);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.TRUE);

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_MATERIALIZED_STUDENT_SELECTION,
                    Boolean.TRUE);

            m_sortedGradeLevels = StudentManager.buildGradeLevelMap(getBroker());
            m_maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());
            loadGradeCodes();
            loadAdultHSReasonCodes();
        }

        /**
         * Materialize the contained history helper.
         *
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#materializeStudentCriteria()
         */
        public void materializeStudentCriteria() {
            m_tnStudentHelper.materializeStudentCriteria();
        }

        /**
         * Checks if is adult HS span.
         *
         * @param span TNStudentEnrollmentSpan
         * @return true, if is adult HS span
         */
        private boolean isAdultHSSpan(TNStudentEnrollmentSpan span) {
            boolean result = false;
            if (span.getFirstActiveEnrollment() != null
                    && m_adultHSReasonCodes.contains(span.getFirstActiveEnrollment().getReasonCode())) {
                result = true;
            }
            return result;
        }

        /**
         * Load adult HS reason codes.
         */
        private void loadAdultHSReasonCodes() {
            m_adultHSReasonCodes = new HashSet();
            ModelProperty prop =
                    new ModelProperty(StudentEnrollment.class, StudentEnrollment.COL_REASON_CODE,
                            getBroker().getPersistenceKey());
            DataDictionaryField field = m_dictionary.findDataDictionaryField(prop.getFieldId());
            ReferenceTable referenceTable = field.getReferenceTable();
            if (referenceTable != null) {
                for (ReferenceCode rcd : referenceTable.getReferenceCodes()) {
                    if (!rcd.getDisabledIndicator() && REASON_CODE_ADULT_HS.equals(rcd.getStateCode())) {
                        m_adultHSReasonCodes.add(rcd.getCode());
                    }
                }
            }
        }

        /**
         * Load grade codes.
         */
        private void loadGradeCodes() {
            ModelProperty prop =
                    new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL, getBroker().getPersistenceKey());
            DataDictionaryField field = m_dictionary.findDataDictionaryField(prop.getFieldId());
            ReferenceTable referenceTable = field.getReferenceTable();
            m_referenceGradeCodeMap = referenceTable.getCodeMap();
        }
    }

    /**
     * The Class RowCount.
     */
    class RowCount {
        private double m_averageDailyAttendance = 0;
        private double m_averageDailyMembership = 0;
        private int m_endOfPeriodMembershipStudents = 0;
        private int m_netEnrolledStudentsFemale = 0;
        private int m_netEnrolledStudentsMale = 0;

        /**
         * Gets the average daily attendance.
         *
         * @return double
         */
        protected double getAverageDailyAttendance() {
            return m_averageDailyAttendance;
        }

        /**
         * Gets the average daily membership.
         *
         * @return double
         */
        protected double getAverageDailyMembership() {
            return m_averageDailyMembership;
        }

        /**
         * Gets the end of period membership students.
         *
         * @return int
         */
        protected int getEndOfPeriodMembershipStudents() {
            return m_endOfPeriodMembershipStudents;
        }

        /**
         * Gets the net enrolled students female.
         *
         * @return int
         */
        protected int getNetEnrolledStudentsFemale() {
            return m_netEnrolledStudentsFemale;
        }

        /**
         * Gets the net enrolled students male.
         *
         * @return int
         */
        protected int getNetEnrolledStudentsMale() {
            return m_netEnrolledStudentsMale;
        }

        /**
         * Increment average daily attendance.
         *
         * @param d double
         */
        protected void incrementAverageDailyAttendance(double d) {
            m_averageDailyAttendance += d;
        }

        /**
         * Increment average daily membership.
         *
         * @param d double
         */
        protected void incrementAverageDailyMembership(double d) {
            m_averageDailyMembership += d;
        }

        /**
         * Increment enrolled students female.
         */
        protected void incrementEnrolledStudentsFemale() {
            m_netEnrolledStudentsFemale++;
        }

        /**
         * Increment enrolled students male.
         */
        protected void incrementEnrolledStudentsMale() {
            m_netEnrolledStudentsMale++;
        }

        /**
         * Increment period membership student.
         */
        protected void incrementPeriodMembershipStudent() {
            m_endOfPeriodMembershipStudents++;
        }
    }

    /**
     * The Class StudentMembershipFlags.
     */
    protected class StudentMembershipFlags {
        private boolean m_endOfPeriodMembershipNGradeLevelStudent = false;
        private boolean m_endOfPeriodMembershipStudent = false;
        private boolean m_netEnrolledNGradeLevelStudent = false;
        private boolean m_netEnrolledStudent = false;

        /**
         * Checks if is end of period membership N grade level student.
         *
         * @return true, if is end of period membership N grade level student
         */
        protected boolean isEndOfPeriodMembershipNGradeLevelStudent() {
            return m_endOfPeriodMembershipNGradeLevelStudent;
        }

        /**
         * Checks if is end of period membership student.
         *
         * @return true, if is end of period membership student
         */
        protected boolean isEndOfPeriodMembershipStudent() {
            return m_endOfPeriodMembershipStudent;
        }

        /**
         * Checks if is net enrolled N grade level student.
         *
         * @return true, if is net enrolled N grade level student
         */
        protected boolean isNetEnrolledNGradeLevelStudent() {
            return m_netEnrolledNGradeLevelStudent;
        }

        /**
         * Checks if is net enrolled student.
         *
         * @return true, if is net enrolled student
         */
        protected boolean isNetEnrolledStudent() {
            return m_netEnrolledStudent;
        }

        /**
         * Sets the end of period.
         *
         * @param m_periodEndDate PlainDate
         * @param nGradeLevelsPeriodCollections List<Pair<PlainDate,PlainDate>>
         */
        protected void setEndOfPeriod(PlainDate m_periodEndDate,
                                      List<Pair<PlainDate, PlainDate>> nGradeLevelsPeriodCollections) {
            if (nGradeLevelsPeriodCollections != null && TNReportingPeriodHelper
                    .isStudentNGradeLevelOnGivenDate(m_periodEndDate, null, nGradeLevelsPeriodCollections)) {
                m_endOfPeriodMembershipNGradeLevelStudent = true;
            } else {
                m_endOfPeriodMembershipStudent = true;
            }
        }

        /**
         * When setting the enrolled flag, logic must be applied to determine if the student is N
         * Grade level for
         * the reporting period. Since this logic changes frequently, both dates of the interval are
         * passed as
         * arguments, even though only the end date is used in the current method.
         *
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @param nGradeLevelsPeriodCollections List<Pair<PlainDate,PlainDate>>
         */
        protected void setEnrolled(PlainDate startDate,
                                   PlainDate endDate,
                                   List<Pair<PlainDate, PlainDate>> nGradeLevelsPeriodCollections) {
            if ((m_isAnnual && nGradeLevelsPeriodCollections != null && TNReportingPeriodHelper
                    .isStudentNGradeLevelDuringDateRange(startDate, endDate, nGradeLevelsPeriodCollections)) ||
                    (!m_isAnnual && nGradeLevelsPeriodCollections != null && TNReportingPeriodHelper
                            .isStudentNGradeLevelOnGivenDate(endDate, null, nGradeLevelsPeriodCollections))) {
                m_netEnrolledNGradeLevelStudent = true;
            } else {
                m_netEnrolledStudent = true;
            }
        }
    }

    /**
     * The Class StudentSpanCount.
     */
    class StudentSpanCount {
        private int m_absenceMinutes = 0;
        private int m_absenceNGradelevelMinutes = 0;
        private int m_absenceMinutesEarlyGraduated = 0;
        private int m_absenceMinutesIEA = 0;
        private int m_membershipMinutes = 0;
        private int m_membershipMinutesEarlyGraduated = 0;
        private int m_membershipMinutesIEA = 0;
        private int m_membershipNGradelevelMinutes = 0;
        private int m_inSessionDays;
        private int m_inSessionDaysEarlyGraduated;
        private int m_inSessionDaysIEA;
        private int m_inSessionDaysNGradelevel;

        /**
         * Instantiates a new student span count.
         *
         * @param absenceMinutes int
         * @param absenceNGradelevelMinutes int
         * @param absenceMinutesEarlyGraduated int
         * @param absenceMinutesIEA int
         * @param membershipMinutes int
         * @param membershipNGradelevelMinutes int
         * @param membershipMinutesEarlyGraduated int
         * @param membershipMinutesIEA int
         * @param inSessionDays int
         * @param inSessionDaysNGradelevel int
         * @param inSessionDaysEarlyGraduated int
         * @param inSessionDaysIEA int
         */
        protected StudentSpanCount(int absenceMinutes,
                int absenceNGradelevelMinutes,
                int absenceMinutesEarlyGraduated,
                int absenceMinutesIEA,
                int membershipMinutes,
                int membershipNGradelevelMinutes,
                int membershipMinutesEarlyGraduated,
                int membershipMinutesIEA,
                int inSessionDays,
                int inSessionDaysNGradelevel,
                int inSessionDaysEarlyGraduated,
                int inSessionDaysIEA) {
            m_absenceMinutes = absenceMinutes;
            m_absenceNGradelevelMinutes = absenceNGradelevelMinutes;
            m_absenceMinutesEarlyGraduated = absenceMinutesEarlyGraduated;
            m_absenceMinutesIEA = absenceMinutesIEA;
            m_membershipMinutes = membershipMinutes;
            m_membershipMinutesEarlyGraduated = membershipMinutesEarlyGraduated;
            m_membershipMinutesIEA = membershipMinutesIEA;
            m_membershipNGradelevelMinutes = membershipNGradelevelMinutes;
            m_inSessionDays = inSessionDays;
            m_inSessionDaysNGradelevel = inSessionDaysNGradelevel;
            m_inSessionDaysEarlyGraduated = inSessionDaysEarlyGraduated;
            m_inSessionDaysIEA = inSessionDaysIEA;
        }

        /**
         * Gets the absence minutes.
         *
         * @return int
         */
        protected int getAbsenceMinutes() {
            return m_absenceMinutes;
        }

        /**
         * Gets the absence minutes Early.
         *
         * @return int
         */
        protected int getAbsenceMinutesEarly() {
            return m_absenceMinutesEarlyGraduated;
        }

        /**
         * Gets the absence minutes IEA.
         *
         * @return int
         */
        protected int getAbsenceMinutesIEA() {
            return m_absenceMinutesIEA;
        }

        /**
         * Gets the absence minutes N Grade.
         *
         * @return int
         */
        protected int getAbsenceMinutesNGradelevel() {
            return m_absenceNGradelevelMinutes;
        }

        /**
         * Gets the in session days.
         *
         * @return the m_inSessionDays
         */
        protected int getInSessionDays() {
            return m_inSessionDays;
        }

        /**
         * Gets the in session days early graduated.
         *
         * @return the m_inSessionDaysEarlyGraduated
         */
        protected int getInSessionDaysEarlyGraduated() {
            return m_inSessionDaysEarlyGraduated;
        }

        /**
         * Gets the in session days IEA.
         *
         * @return the m_inSessionDaysIEA
         */
        protected int getInSessionDaysIEA() {
            return m_inSessionDaysIEA;
        }

        /**
         * Gets the in session days N gradelevel.
         *
         * @return the m_inSessionDaysNGradelevel
         */
        protected int getInSessionDaysNGradelevel() {
            return m_inSessionDaysNGradelevel;
        }

        /**
         * Gets the membership minutes.
         *
         * @return int
         */
        protected int getMembershipMinutes() {
            return m_membershipMinutes;
        }

        /**
         * Gets the membership minutes early graduated.
         *
         * @return int
         */
        protected int getMembershipMinutesEarlyGraduated() {
            return m_membershipMinutesEarlyGraduated;
        }

        /**
         * Gets the membership minutes for IEA.
         *
         * @return int
         */
        protected int getMembershipMinutesIEA() {
            return m_membershipMinutesIEA;
        }

        /**
         * Gets the membership N gradelevel minutes.
         *
         * @return int
         */
        protected int getMembershipMinutesNGradelevel() {
            return m_membershipNGradelevelMinutes;
        }
    }

    private static final String ALIAS_DAY_EVENT_TYPE_2 = "DOE DAY EVENT TYPE 2";
    private static final String ALIAS_DAY_EVENT_TYPE_3 = "DOE DAY EVENT TYPE 3";
    private static final String ALIAS_EIS_STATE_ID = "DOE EIS STATE ID";
    private static final String ALIAS_HISTORY_STUDENT_STANDARD_DAY = "all-rcd-StdStandardDayHistory";
    private static final String ALIAS_INSTR_SERVICE_TYPE = "DOE INSTR SERVICE TYPE";
    private static final String ALIAS_SKL_SP_STOCKPILE = "DOE SCHOOL SP STOCKPILE";
    private static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL STATE ID";
    private static final String ALIAS_STUDENT_STANDARD_DAY = "DOE STUDENT STANDARD DAY";
    private static final String ANNUAL_PERIOD_NAME = "Annual";

    private static final List<String> CODES_ENROLLMENT_ADDITIONAL_STUDENT = new ArrayList<String>();

    private static final String CODE_ENROLL_TRANSFERRED = "TR";
    private static final String CODE_ENROLL_UNDER_ACHIEVING = "TC";
    private static final String CODE_MAKEUP_DAY = "MD";
    private static final String CODE_OUT_OF_STATE_ENROLLMENT = "E1";
    private static final String CODE_REGULAR_ENROLLMENT = "E";
    private static final String CODE_TRANSFER_SCHOOL_UNDER_ARCHIEVING = "EC";

    private static final String DETAIL_REPORT_FIELD_ABS_MINUTES = "AbsenceMinutes";
    private static final String DETAIL_REPORT_FIELD_ABS_MINUTES_EARLY = "AbsenceMinutesEarlyGraduates";
    private static final String DETAIL_REPORT_FIELD_ABS_MINUTES_IEA = "AbsenceMinutesIEA";
    private static final String DETAIL_REPORT_FIELD_ABS_N_MINUTES = "AbsenceMinutesNGradeLevel";
    private static final String DETAIL_REPORT_FIELD_ADA = "stdAda";
    private static final String DETAIL_REPORT_FIELD_ADA_CALCULATED = "stdAdaCalculated";
    private static final String DETAIL_REPORT_FIELD_ADA_EARLY = "stdAdaEarly";
    private static final String DETAIL_REPORT_FIELD_ADA_IEA = "stdAdaIEA";
    private static final String DETAIL_REPORT_FIELD_ADA_N = "stdAdaGradeN";
    private static final String DETAIL_REPORT_FIELD_ADM = "stdAdm";
    private static final String DETAIL_REPORT_FIELD_ADM_N = "stdAdmGradeN";
    private static final String DETAIL_REPORT_FIELD_ADM_EARLY = "ADMEarly";
    private static final String DETAIL_REPORT_FIELD_ADM_CALCULATED = "ADMCalculated";
    private static final String DETAIL_REPORT_FIELD_GENDER = "Gender";
    private static final String DETAIL_REPORT_FIELD_GRADE_LEVEL = "GradeLevel";
    private static final String DETAIL_REPORT_FIELD_ADM_IEA = "ADMIea";
    private static final String DETAIL_REPORT_FIELD_IN_SESSION_DAYS = "InSessionDays";
    private static final String DETAIL_REPORT_FIELD_IN_SESSION_DAYS_EARLY = "InSessionDaysEarlyGraduates";
    private static final String DETAIL_REPORT_FIELD_IN_SESSION_DAYS_IEA = "InSessionDaysIEA";
    private static final String DETAIL_REPORT_FIELD_IN_SESSION_N_DAYS = "InSessionDaysNGradeLevel";
    private static final String DETAIL_REPORT_FIELD_IS_END_PERIOD = "IsEndOfPeriod";
    private static final String DETAIL_REPORT_FIELD_IS_N_END_PERIOD = "IsNGradeLevelEndOfPeriod";
    private static final String DETAIL_REPORT_FIELD_IS_N_ENROLLED = "IsNGradeLevelEnrolled";
    private static final String DETAIL_REPORT_FIELD_IS_ENROLLED = "IsEnrolled";
    private static final String DETAIL_REPORT_FIELD_LOCAL_ID = "LocalID";
    private static final String DETAIL_REPORT_FIELD_MEM_MINUTES = "MembershipMinutes";
    private static final String DETAIL_REPORT_FIELD_MEM_MINUTES_EARLY = "MembershipMinutesEarlyGraduation";
    private static final String DETAIL_REPORT_FIELD_MEM_MINUTES_IEA = "MembershipMinutesIEA";
    private static final String DETAIL_REPORT_FIELD_MEM_N_MINUTES = "MembershipMinutesNGradeLevel";
    private static final String DETAIL_REPORT_FIELD_NUM_PR_SPANS = "NumProcessedSpans";
    private static final String DETAIL_REPORT_FIELD_NUM_SPANS = "NumSpans";
    private static final String DETAIL_REPORT_FIELD_SCHOOL = "School";
    private static final String DETAIL_REPORT_FIELD_SCHOOL_NAME = "SchoolName";
    private static final String DETAIL_REPORT_FIELD_STATE_ID = "StateID";
    private static final String DETAIL_REPORT_FIELD_STD_NAME = "StudentName";

    private static final int GRADE_FILTER_MIN_LEVEL = 0;
    private static final int GRADE_FILTER_MAX_LEVEL = 12;

    private static final String GRADE_LEVEL_LETTER_KINDERGARTEN = "K";
    private static final String GRADE_LEVEL_LETTER_NONGRADED = "N";
    private static final String GRADE_LEVEL_LETTER_TOTAL_K_12 = "K-12";
    private static final String GRADE_LEVEL_LETTER_TOTAL_K_12_N = "TOTAL\nK-12,N";

    private static final int GRADE_LEVEL_NUMERIC_KINDERGARTEN = 0;
    private static final int GRADE_LEVEL_NUMERIC_TOTAL_K_12 = 100;
    private static final int GRADE_LEVEL_NUMERIC_NONGRADED = 200;
    private static final int GRADE_LEVEL_NUMERIC_TOTAL_K_12_N = 300;

    private static final String INPUT_INCL_ALL_GRADES = "includeAllGrades";
    private static final String INPUT_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_INCL_EARLY_GRADUATES = "includeEarlyGraduates";
    private static final String INPUT_INCL_IEA = "includeIEA";
    private static final String INPUT_INCL_NONGRADED = "includeNonGraded";
    private static final String INPUT_SCHOOLS = "schoolOids";
    private static final String INPUT_PARAM_GRADES = "grades";
    private static final String INPUT_PARAM_IS_ANNUAL = "isAnnual";
    private static final String INPUT_PARAM_DISTRICT_SUMMARY = "includeDistrictSummary";
    private static final String INPUT_PARAM_SUMMARY_ONLY = "summaryOnly";
    private static final String INPUT_REPORT_PERIOD = "reportPeriod";
    private static final String INPUT_REPORT_ID_CSV = "subreportIdCSVVersion";
    private static final String INPUT_REPORT_ID_PDF = "subreportIdPDFVersion";
    private static final String INPUT_REPORT_ID_SUM_CSV = "subreportIdSummaryCSVVersion";

    private static final String INSTR_SERVICE_TYPE_PRIMARY_SCHOOL_CODE = "P";

    private static final String INITIALIZE_KEY = "label.state.report.initialize";

    private static final String PROCEDURE_ID_CLASS_SECTION_SCHEDULE = "EXPDATA-TN-MSTS";

    private static final String REPORT_PARAMETER_DAYS_IN_SESSION = "daysInSession";
    private static final String REPORT_PARAMETER_USER = "user";
    private static final String REPORT_PARAMETER_MONTH = "month";

    private static final String REPORT_FIELD_AVERAGE_DAILY_ATTENDANCE = "averageDailyAttendance";
    private static final String REPORT_FIELD_AVERAGE_DAILY_MEMBERSHIP = "averageDailyMembership";
    private static final String REPORT_FIELD_END_DATE = "endDate";
    private static final String REPORT_FIELD_END_OF_PERIOD_MEMBERSHIP = "endOfPeriodMembership";
    private static final String REPORT_FIELD_ERROR_MESSAGE = "errorMessage";
    private static final String REPORT_FIELD_GRADE_CODE = "gradeCode";
    private static final String REPORT_FIELD_GRADE_LEVEL = "gradeLevel";
    private static final String REPORT_FIELD_NET_ENROLLMENT_TO_DATE_FEMALE = "netEnrollmentToDateFemale";
    private static final String REPORT_FIELD_NET_ENROLLMENT_TO_DATE_MALE = "netEnrollmentToDateMale";
    private static final String REPORT_FIELD_NET_ENROLLMENT_TO_DATE_TOTAL = "netEnrollmentToDateTotal";
    private static final String REPORT_FIELD_ORG_NAME = "orgName";
    private static final String REPORT_FIELD_SCHOOL = "school";
    private static final String REPORT_FIELD_START_DATE = "startDate";
    private static final String REPORT_FIELD_SCHOOL_SP_STOCKPILE = "stockpile";
    private static final String REPORT_FIELD_TOTAL_MAKEUP_DAYS = "totalMakeUpDays";

    private static final String STAFF_SCHOOL_CODE = "9999";

    private static final String STATE_CODE_GENDER_MALE = "M";
    private static final String STATE_CODE_GENDER_FEMALE = "F";

    protected PlainDate m_calendarStartDate = null;
    protected DataDictionary m_dictionary;
    protected boolean m_isAnnual;
    protected PlainDate m_periodEndDate = null;
    protected PlainDate m_periodStartDate = null;
    protected Map<String, ReferenceCode> m_referenceEnrollmentCodeMap;
    protected Map<String, ReferenceCode> m_referenceGenderCodeMap;
    protected PlainDate m_schoolStartDate = null;

    private DistrictSchoolYearContext m_context;

    private EnrollmentStatistics m_data;
    private DecimalFormat m_decimalFormat = new DecimalFormat("#0.0000");
    private List<Integer> m_grades = new ArrayList<>();
    private Boolean m_includeAllGrades;
    private Boolean m_includeDistrictSummary;
    private Boolean m_includeEarlyGraduates;
    private Boolean m_includeIEA;
    private Boolean m_includeNonGraded;
    private Boolean m_includedSomeGrade;
    private TNReportingPeriodHelper m_periodHelper = null;
    private ReferenceCode m_reportPeriod;
    private TNClassSectionScheduleData m_scheduleData;

    private Boolean m_summaryOnly;
    private boolean m_useDetail;

    static {
        CODES_ENROLLMENT_ADDITIONAL_STUDENT.add(CODE_REGULAR_ENROLLMENT);
        CODES_ENROLLMENT_ADDITIONAL_STUDENT.add(CODE_OUT_OF_STATE_ENROLLMENT);
        CODES_ENROLLMENT_ADDITIONAL_STUDENT.add(CODE_TRANSFER_SCHOOL_UNDER_ARCHIEVING);
    }

    /**
     * Cleanup.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#cleanup()
     */
    @Override
    protected void cleanup() {
        super.cleanup();
        if (m_data != null) {
            m_data.close();
        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();


        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_decimalFormat.setRoundingMode(RoundingMode.DOWN); // because we want to truncate, not
                                                            // rounding up

        loadGenderCodes();
        loadEnrollmentCodes();
        m_context = getCurrentContext();

        // grade output control inputs
        m_includeAllGrades = Boolean.TRUE.equals(getParameter(INPUT_INCL_ALL_GRADES));
        m_includeNonGraded = Boolean.TRUE.equals(getParameter(INPUT_INCL_NONGRADED));

        m_grades = getSelectedGradesInputParam();
        if (m_grades.isEmpty()) {
            m_includedSomeGrade = Boolean.FALSE;
            m_grades = IntStream.rangeClosed(GRADE_FILTER_MIN_LEVEL, GRADE_FILTER_MAX_LEVEL).boxed()
                    .collect(Collectors.toList());
        } else {
            m_includedSomeGrade = Boolean.TRUE;
        }

        m_includeDistrictSummary = (Boolean) getParameter(INPUT_PARAM_DISTRICT_SUMMARY);
        if (m_includeDistrictSummary == null) {
            m_includeDistrictSummary = Boolean.FALSE;
        }
        m_includeEarlyGraduates = (Boolean) getParameter(INPUT_INCL_EARLY_GRADUATES);
        m_includeIEA = (Boolean) getParameter(INPUT_INCL_IEA);
        m_summaryOnly = (Boolean) getParameter(INPUT_PARAM_SUMMARY_ONLY);
        if (m_summaryOnly == null) {
            m_summaryOnly = Boolean.FALSE;
        } else if (m_summaryOnly.booleanValue()) {
            m_includeDistrictSummary = Boolean.TRUE;
        }

        Boolean isAnnual = (Boolean) getParameter(INPUT_PARAM_IS_ANNUAL);
        m_isAnnual = isAnnual == null ? false : isAnnual.booleanValue();

        String reportPeriodOid = (String) getParameter(INPUT_REPORT_PERIOD);
        if (StringUtils.isEmpty(reportPeriodOid)) {
            m_periodHelper = new TNReportingPeriodHelper(getOrganization(), m_context, null, getBroker());
        } else {
            m_reportPeriod = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, reportPeriodOid);
            addParameter(REPORT_PARAMETER_MONTH, m_reportPeriod.getCode());
            m_periodHelper = new TNReportingPeriodHelper(getOrganization(), m_context, m_reportPeriod, getBroker());
        }

        m_periodStartDate = m_context.getStartDate();
        m_schoolStartDate = m_context.getStartDate();
        m_periodEndDate = m_context.getEndDate();

        addParameter(REPORT_PARAMETER_USER, getUser());
        addParameter(INPUT_INCL_EARLY_GRADUATES, m_includeEarlyGraduates);
        addParameter(INPUT_INCL_IEA, m_includeIEA);

        initReportsFormat();
    }

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        HashMap<String, Integer> daysInSessionByOrg = new HashMap<String, Integer>();

        ReportDataGrid pdfGrid = new ReportDataGrid();
        ReportDataGrid districtGrid = new ReportDataGrid();
        ReportDataGrid detailGrid = new ReportDataGrid();

        Collection<SisSchool> schools = null;
        Object objIsAllSchools = getParameter(INPUT_ALL_SCHOOLS);
        boolean isAllSchools = objIsAllSchools == null ? false : ((Boolean) objIsAllSchools).booleanValue();
        if (isAllSchools) {
            X2Criteria schoolCriteria = new X2Criteria();

            schoolCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            schoolCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

            DataDictionaryField aliasSklStateIDField =
                    m_dictionary.findDataDictionaryFieldByAlias(ALIAS_SKL_STATE_ID);
            schoolCriteria.addNotEmpty(aliasSklStateIDField.getJavaName(), getBroker().getPersistenceKey());
            schoolCriteria.addNotEqualTo(aliasSklStateIDField.getJavaName(), STAFF_SCHOOL_CODE);

            QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
            schoolQuery.addOrderByAscending(SisSchool.COL_NAME);
            schools = getBroker().getCollectionByQuery(schoolQuery);
        } else {
            schools = new LinkedList();
            Object objSchools = getParameter(INPUT_SCHOOLS);
            String schoolOids = objSchools == null ? "" : (String) objSchools;
            if (!StringUtils.isEmpty(schoolOids)) {
                List<String> oids = Arrays.asList(schoolOids.split(","));
                X2Criteria schoolCriteria = new X2Criteria();

                schoolCriteria.addIn(X2BaseBean.COL_OID, oids);

                QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
                schoolQuery.addOrderByAscending(SisSchool.COL_NAME);
                schools = getBroker().getCollectionByQuery(schoolQuery);
            }
        }

        if (schools.isEmpty()) {
            appendSchoolGridWithMessage(null, pdfGrid, detailGrid, "No schools are selected");
        }

        Collection<StateReportValidationError> initErrors = new ArrayList<StateReportValidationError>();
        m_data = new EnrollmentStatistics();
        m_data.setBroker(getBroker());
        m_data.setOrganization(getOrganization());
        m_data.setPrivilegeSet(getPrivilegeSet());
        m_data.setSchoolContext(false);
        m_data.setParameters(getParameters());
        m_data.setUser(getUser());
        m_data.initializeExport();
        initErrors.addAll(m_data.getSetupErrors());

        /*
         * Adjust student criteria here so that criteria is already set when initializing
         * TNClassSectionScheduleData
         */
        if (!isAllSchools) {
            TNReportingPeriodHelper.adjustStudentCriteria(m_data.getStudentCriteria(), schools, getOrganization(),
                    getCurrentContext());
            m_data.materializeStudentCriteria();
        }

        addParameter(TNClassSectionHelper.INPUT_PARAM_ENROLLMENT_HELPER, m_data.m_tnEnrHelper);
        addParameter(TNClassSectionHelper.INPUT_PARAM_BYPASS_DUP_SECT_TEST, Boolean.TRUE);
        addParameter(TNClassSectionHelper.INPUT_PARAM_LOAD_STUDENT_SECTIONS_ONLY, Boolean.TRUE);

        m_scheduleData = new TNClassSectionScheduleData();
        if (m_scheduleData != null && initErrors.size() == 0) {
            try {
                // Initialize the report data object.
                m_scheduleData.setBroker(getBroker());
                m_scheduleData.setOrganization(getOrganization());
                m_scheduleData.setPrivilegeSet(getPrivilegeSet());
                m_scheduleData.setSchoolContext(false);
                m_scheduleData.setParameters(getParameters());
                m_scheduleData.setUser(getUser());
                initErrors.addAll(m_scheduleData.loadDefinitions(PROCEDURE_ID_CLASS_SECTION_SCHEDULE, getBroker()));
                m_scheduleData.initializeExport();
            } catch (X2BaseException x2be) {
                String init_msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                        .getMessage(INITIALIZE_KEY);
                initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));
            }

            initErrors.addAll(m_scheduleData.getSetupErrors());

            if (initErrors.size() > 0) {
                StringBuilder reportMessage = new StringBuilder(StringUtils.EMPTY);
                for (StateReportValidationError error : initErrors) {
                    AppGlobals.getLog().log(Level.SEVERE, error.getErrorMessage(), error);
                    reportMessage.append(error.getErrorMessage() + "\n");
                }
                appendSchoolGridWithMessage(schools.iterator().next(), pdfGrid, detailGrid, reportMessage.toString());
            }
        }
        if (initErrors.isEmpty()) {
            Map<String, TreeMap<Integer, Map<SisStudent, List<TNStudentEnrollmentSpan>>>> mapSpans = initSortedSpans();
            m_periodHelper.setStudentCriteria(m_data.getStudentCriteria());
            for (SisSchool school : schools) {
                int makeUpDays = 0;
                int stockpile = 0;
                Collection daysInSession = null;


                m_periodStartDate = m_periodHelper.getDateBegin(school.getOid());
                m_schoolStartDate = m_periodHelper.getYearBeginDate();
                m_calendarStartDate = m_periodHelper.getCalendarBeginDate();
                m_periodEndDate = m_periodHelper.getDateEnd(school.getOid());
                daysInSession = m_periodHelper.getDaysInSession(school.getOid());
                if (m_periodStartDate == null || m_periodEndDate == null || daysInSession.isEmpty()) {
                    String code = m_periodHelper.getMonth() == null ? ANNUAL_PERIOD_NAME
                            : m_periodHelper.getMonth().getCode();
                    // this is used to avoid NUllPointerException and to print an error message in a
                    // report
                    m_periodStartDate = m_context.getStartDate();
                    m_periodEndDate = m_context.getEndDate();
                    String message = null;
                    if (school.getArchiveIndicator() == true || school.getInactiveIndicator() == true) {
                        message = "Report for school " + school.getName() +
                                " cannot be generated. The school is archive or inactive. Please set the school as "
                                + "active and not as archive and rerun the report.";
                    } else {
                        message = "Report for school " + school.getName() +
                                " cannot be generated. Report period with code: " +
                                code + " is not defined. Please define a school "
                                + "calendar and rerun the report.";
                    }
                    AppGlobals.getLog().log(Level.WARNING, message);
                    appendSchoolGridWithMessage(school, pdfGrid, detailGrid, message);
                    continue;
                }
                if (m_periodHelper.getMonth() == null) {
                    /*
                     * At this point, this should be for 'TN_DIR_MEM_ANNUAL' report (not
                     * TN_DIR_MEM_MONTH),
                     * and we want to check the stockpile and totalMakeUpDays on school calendar.
                     */
                    String mostCommonCalendar = getMostCommonCalendar(school);
                    for (SchoolCalendar calendar : school.getSchoolCalendars()) {
                        if (calendar.getDistrictContextOid().equals(m_context.getOid()) &&
                                calendar.getCalendarId().equals(mostCommonCalendar)) {
                            if (calendar.getFieldValueByAlias(ALIAS_SKL_SP_STOCKPILE) != null) {
                                try {
                                    stockpile = Integer
                                            .parseInt(calendar.getFieldValueByAlias(ALIAS_SKL_SP_STOCKPILE).toString());
                                } catch (NumberFormatException nfe) {
                                    AppGlobals.getLog().log(Level.SEVERE, nfe.getMessage(), nfe);
                                    stockpile = 0;
                                    continue;
                                }
                            }

                            // Reset the number of makeUpDays, then count the number of make up days
                            // in this calendar
                            makeUpDays = 0;
                            Collection<SchoolCalendarDate> calDates = calendar.getSchoolCalendarDates();
                            for (SchoolCalendarDate date : calDates) {
                                String eventType1 = date.getScheduleDayType();
                                String eventType2 = date.getFieldValueByAlias(ALIAS_DAY_EVENT_TYPE_2) != null
                                        ? date.getFieldValueByAlias(ALIAS_DAY_EVENT_TYPE_2).toString()
                                        : "";
                                String eventType3 = date.getFieldValueByAlias(ALIAS_DAY_EVENT_TYPE_3) != null
                                        ? date.getFieldValueByAlias(ALIAS_DAY_EVENT_TYPE_3).toString()
                                        : "";

                                if ((!StringUtils.isEmpty(eventType1) && eventType1.startsWith(CODE_MAKEUP_DAY)) ||
                                        (!StringUtils.isEmpty(eventType2) && eventType2.startsWith(CODE_MAKEUP_DAY)) ||
                                        (!StringUtils.isEmpty(eventType3) && eventType3.startsWith(CODE_MAKEUP_DAY))) {
                                    makeUpDays++;
                                }
                            }
                        }
                    }
                }

                TreeMap<Integer, Map<SisStudent, List<TNStudentEnrollmentSpan>>> sortedSpans =
                        mapSpans.get(school.getOid());
                if (sortedSpans == null) {
                    sortedSpans = getDefaultSortedSpans();
                }
                m_scheduleData.setSchool(school);

                daysInSessionByOrg.put(school.getName(), Integer.valueOf(daysInSession.size()));
                ReportDataGrid schoolGrid = new ReportDataGrid();

                appendDataGrid(school, daysInSession, sortedSpans, stockpile, makeUpDays, schoolGrid, detailGrid, null);
                countTotals(schoolGrid);
                pdfGrid.append(schoolGrid);

                if (m_includeDistrictSummary.booleanValue()) {
                    // add the data from school to district grids
                    districtGrid = appendDistrictSummaryDataGrid(schoolGrid, districtGrid);
                }
            }
        }

        if (m_summaryOnly.booleanValue()) {
            pdfGrid = new ReportDataGrid();
        }
        if (m_includeDistrictSummary.booleanValue()) {
            daysInSessionByOrg.put(getOrganization().getName(), Integer.valueOf(getDistrictDaysInSession()));
            pdfGrid.append(districtGrid);
        }

        addParameter(REPORT_PARAMETER_DAYS_IN_SESSION, daysInSessionByOrg);

        detailGrid.beforeTop();
        pdfGrid.beforeTop();

        return m_useDetail ? detailGrid : pdfGrid;
    }

    /**
     * Returns the number of in-session days for the most common calendar for the school.
     *
     * @param school SisSchool
     * @return int total days
     */
    protected int getTotalCalendarDays(SisSchool school) {
        String calendarId = null;
        if (school.getActiveSchedule() != null) {
            ScheduleManager mgr = new ScheduleManager(getBroker());
            calendarId = mgr.getMostCommonCalendar(school.getActiveSchedule(), null);
        }

        Collection<SchoolCalendar> schoolCalendars = school.getSchoolCalendars();

        int totalDays = 0;
        if (calendarId != null) {
            for (SchoolCalendar schoolCalendar : schoolCalendars) {
                if (calendarId.equals(schoolCalendar.getCalendarId())) {
                    int daysInSession = schoolCalendar.getDaysInSession();
                    if (daysInSession > totalDays) {
                        totalDays = daysInSession;
                    }
                }
            }
        }

        if (totalDays == 0) {
            for (SchoolCalendar schoolCalendar : schoolCalendars) {
                int daysInSession = schoolCalendar.getDaysInSession();
                if (daysInSession > totalDays) {
                    totalDays = daysInSession;
                }
            }
        }

        return totalDays;
    }

    /**
     * Add data for the school to grid.
     *
     * @param school SisSchool
     * @param daysInSession Collection<PlainDate>
     * @param sortedSpans TreeMap<Integer,Map<SisStudent,List<TNStudentEnrollmentSpan>>>
     * @param stockpile int
     * @param totalMakeUpDays int
     * @param grid ReportDataGrid
     * @param detailGrid ReportDataGrid
     * @param emptyReportErrorMessage String
     */
    private void appendDataGrid(SisSchool school,
                                Collection<PlainDate> daysInSession,
                                TreeMap<Integer, Map<SisStudent, List<TNStudentEnrollmentSpan>>> sortedSpans,
                                int stockpile,
                                int totalMakeUpDays,
                                ReportDataGrid grid,
                                ReportDataGrid detailGrid,
                                String emptyReportErrorMessage) {
        RowCount accumulatorNGradeLevel = new RowCount();

        for (Entry<Integer, Map<SisStudent, List<TNStudentEnrollmentSpan>>> entrySortedSpans : sortedSpans.entrySet()) {
            Integer gradeLevel = entrySortedSpans.getKey();
            Map<SisStudent, List<TNStudentEnrollmentSpan>> gradeLevelStudentSpansMap = entrySortedSpans.getValue();

            Collection<PlainDate> daysInSessionForStudent = null;
            RowCount accumulator = new RowCount();

            for (Entry<SisStudent, List<TNStudentEnrollmentSpan>> entryGradeLevelStudentSpans : gradeLevelStudentSpansMap
                    .entrySet()) {
                SisStudent student = entryGradeLevelStudentSpans.getKey();

                StudentMembershipFlags membershipFlags = new StudentMembershipFlags();
                List<StudentAttendance> absences = m_data.getStudentAttendances(student.getOid());
                int absenceMinutes = 0;
                int absenceNGradelevelMinutes = 0;
                int absenceMinutesEarlyGraduated = 0;
                int absenceMinutesIEA = 0;
                int membershipMinutes = 0;
                int membershipMinutesEarlyGraduated = 0;
                int membershipMinutesIEA = 0;
                int membershipNGradelevelMinutes = 0;
                int inSessionDays = 0;
                int inSessionDaysEarlyGraduated = 0;
                int inSessionDaysIEA = 0;
                int inSessionDaysNGradelevel = 0;

                // Convert the students absence to student absence dates
                List<PlainDate> absenceDates = new ArrayList<PlainDate>();
                if (!CollectionUtils.isEmpty(absences)) {
                    for (StudentAttendance absence : absences) {
                        absenceDates.add(absence.getDate());
                    }
                }

                String gender = "";
                if (student.getPerson() != null) {
                    ReferenceCode genderCode = m_referenceGenderCodeMap.get(student.getPerson().getGenderCode());
                    if (genderCode != null) {
                        gender = genderCode.getStateCode();
                    }
                }

                /*
                 * Check for student ineligibility status (I20 or out of state student). If the
                 * student is ineligible,
                 * then exclude the student from End of Month/Year count, ADM, and ADA calculations.
                 *
                 * This will also do the calculation for the 'Expelled' student as well. During the
                 * expulsion (from "S"
                 * enrollment with 'ExpBeg' until "S" enrollment with 'ExpEnd'), that student will
                 * have "Inactive" status.
                 */
                boolean isStudentIneligible = m_periodHelper.hasStudentIneligibilityFundingStatus(student,
                        m_periodStartDate, m_periodEndDate);

                List<Pair<PlainDate, PlainDate>> nGradeLevelsPeriodCollections =
                        m_periodHelper.getStudentNGradeLevelRange(student, m_periodStartDate, m_periodEndDate);
                List<Pair<PlainDate, PlainDate>> ieaPeriodCollections =
                        m_periodHelper.getStudentIEAPgmsRange(student, m_periodStartDate, m_periodEndDate,
                                nGradeLevelsPeriodCollections);

                /*
                 * Go through all the enrollment spans to determine if the student is net enrolled,
                 * is enrolled at the end of the month/period/year, and to calculate the ADM and ADA
                 */
                TNStudentEnrollmentSpan firstSpan = null;
                TNStudentEnrollmentSpan nextSpan = null;

                List<TNStudentEnrollmentSpan> spans = entryGradeLevelStudentSpans.getValue();

                int numProcessedSpans = 0;

                int stdStandardDay = m_data.getStudentStandardDay(student, spans.iterator().next());
                if (stdStandardDay == 0) {
                    String message =
                            "The value for standard day cannot be determined for Student " + student.getNameView();
                    AppGlobals.getLog().log(Level.SEVERE, message);
                    emptyReportErrorMessage = message;
                }

                for (TNStudentEnrollmentSpan span : spans) {
                    ++numProcessedSpans;
                    if (firstSpan != null && nextSpan != null) // The previous 2 spans are E-S and
                                                               // S-W
                    {
                        // The check for the "End of Month Membership"
                        if (!firstSpan.getFirstActiveEnrollment().getEnrollmentDate().after(m_periodEndDate) &&
                                !nextSpan.getLastActiveDate().before(m_periodEndDate)) {
                            membershipFlags.setEndOfPeriod(m_periodEndDate, nGradeLevelsPeriodCollections);
                        }

                        // Reset the span or add the current span as the first span
                        firstSpan = span.getFirstActiveEnrollment() != null
                                && StudentEnrollment.ENTRY.equals(span.getFirstActiveEnrollment().getEnrollmentType())
                                        ? span
                                        : null;
                        nextSpan = null;
                    } else {
                        // First span behaves as entry span even if it is YOG or STATUS
                        if (span.getFirstActiveEnrollment() != null
                                && (numProcessedSpans == 1 || StudentEnrollment.ENTRY
                                        .equals(span.getFirstActiveEnrollment().getEnrollmentType()))) {
                            firstSpan = span;

                            if (firstSpan.getFirstInactiveEnrollment() != null && StudentEnrollment.WITHDRAWAL
                                    .equals(firstSpan.getFirstInactiveEnrollment().getEnrollmentType())) // E-W
                                                                                                         // case
                            {
                                // The check for the "End of Month Membership" column
                                if (!firstSpan.getFirstActiveEnrollment().getEnrollmentDate().after(m_periodEndDate) &&
                                        !firstSpan.getLastActiveDate().before(m_periodEndDate)) {
                                    membershipFlags.setEndOfPeriod(m_periodEndDate, nGradeLevelsPeriodCollections);
                                }

                                // Reset the first span again (after we check if the student is net
                                // enrolled above)
                                firstSpan = null;
                            } else // E-S case
                            {
                                firstSpan = span;
                            }
                        } else if (span.getFirstInactiveEnrollment() != null && StudentEnrollment.WITHDRAWAL
                                .equals(span.getFirstInactiveEnrollment().getEnrollmentType())) // S-W
                                                                                                // case
                        {
                            nextSpan = span;
                        }
                    }

                    if (!isStudentIneligible) {
                        /*
                         * Check if the student is enrolled and withdrawn on the same day.
                         * If yes, then we don't want to count this student.
                         *
                         * NOTE: right now, it should be only on the E-W case. I think a student
                         * cannot be enrolled and withdrawn on the same day with E-S-S-W case.
                         */
                        if (!(span.getFirstActiveEnrollment() != null
                                && StudentEnrollment.ENTRY.equals(span.getFirstActiveEnrollment().getEnrollmentType())
                                &&
                                span.getFirstInactiveEnrollment() != null && StudentEnrollment.WITHDRAWAL
                                        .equals(span.getFirstInactiveEnrollment().getEnrollmentType())
                                &&
                                span.getFirstActiveEnrollment().getEnrollmentDate()
                                        .equals(span.getFirstInactiveEnrollment().getEnrollmentDate()))) {
                            if (daysInSessionForStudent == null) {
                                // Get the in-session calendar days for student
                                Collection<PlainDate> calendarDays =
                                        m_data.getCalendarDays(school, student.getCalendarCode());
                                if (calendarDays != null) {
                                    daysInSessionForStudent = getDaysInSession(calendarDays);
                                } else {
                                    daysInSessionForStudent = daysInSession;
                                }
                            }

                            StudentSpanCount count = getStudentCountsForSpan(span, student, daysInSession,
                                    daysInSessionForStudent, nGradeLevelsPeriodCollections, ieaPeriodCollections,
                                    stdStandardDay);
                            membershipMinutes += count.getMembershipMinutes();
                            membershipMinutesEarlyGraduated += count.getMembershipMinutesEarlyGraduated();
                            membershipMinutesIEA += count.getMembershipMinutesIEA();
                            absenceMinutes += count.getAbsenceMinutes();
                            absenceMinutesIEA += count.getAbsenceMinutesIEA();
                            absenceMinutesEarlyGraduated += count.getAbsenceMinutesEarly();
                            membershipNGradelevelMinutes += count.getMembershipMinutesNGradelevel();
                            absenceNGradelevelMinutes += count.getAbsenceMinutesNGradelevel();
                            inSessionDays += count.getInSessionDays();
                            inSessionDaysEarlyGraduated += count.getInSessionDaysEarlyGraduated();
                            inSessionDaysIEA += count.getInSessionDaysIEA();
                            inSessionDaysNGradelevel += count.getInSessionDaysNGradelevel();
                        }
                    }
                }

                // Check the last span
                if (firstSpan != null) {
                    // The check for the "End of Month Membership" column
                    StudentEnrollment withdrawalEnrollment = null;
                    PlainDate withdrawalDate = null;
                    if (nextSpan != null) // It's E-S and [S-W or S-S]
                    {
                        withdrawalEnrollment = nextSpan.getFirstInactiveEnrollment();
                        withdrawalDate = nextSpan.getLastActiveDate();
                    } else // It's E-S or E-W
                    {
                        withdrawalEnrollment = firstSpan.getFirstInactiveEnrollment();
                        withdrawalDate = firstSpan.getLastActiveDate();
                    }

                    // If there's no 'nextSpan', make sure the firstSpan is not E-W with withdrawal
                    // date before the period end date
                    // OR enrollment date after the period end date

                    /*
                     * if first active enrollment date on or before period end date
                     * and (withdrawal enrollment is null
                     * or withdrawal enrollment is anything except 'W"
                     * or withdrawal date on or after period end date)
                     */
                    if (!firstSpan.getFirstActiveEnrollment().getEnrollmentDate().after(m_periodEndDate) &&
                            (withdrawalEnrollment == null ||
                                    !StudentEnrollment.WITHDRAWAL.equals(withdrawalEnrollment.getEnrollmentType()) ||
                                    !withdrawalDate.before(m_periodEndDate))) {
                        membershipFlags.setEndOfPeriod(m_periodEndDate, nGradeLevelsPeriodCollections);
                    }

                    // Reset the firstSpan and nextSpan
                    firstSpan = null;
                    nextSpan = null;
                }

                // Use full set of enrollment spans here
                List<TNStudentEnrollmentSpan> totalSpans = m_data.getStudentEnrollmentSpans(student, false);
                NetEnrolledInfo netEnrInfo =
                        m_periodHelper.getStudentNetEnrolled(student, totalSpans, m_schoolStartDate);

                // Set period for net enrolled here
                if (netEnrInfo != null
                        && !CODE_ENROLL_TRANSFERRED.equals(calculateEnrollmentCode(netEnrInfo.getEntryEnrollment()))
                        && school.getOid().equals(netEnrInfo.getSchool().getOid()) && m_periodHelper
                                .isEnrolled(totalSpans, school.getOid(), m_calendarStartDate, m_periodEndDate)) {
                    membershipFlags.setEnrolled(m_calendarStartDate, m_periodEndDate,
                            m_periodHelper.getStudentNGradeLevelRange(student, m_calendarStartDate, m_periodEndDate));
                }

                if (membershipFlags.isNetEnrolledNGradeLevelStudent() && !m_includeNonGraded) {
                    continue;
                }
                if (!membershipFlags.isNetEnrolledNGradeLevelStudent() && !m_includeAllGrades && !m_includedSomeGrade) {
                    continue;
                }

                detailGrid.append();
                detailGrid.set(DETAIL_REPORT_FIELD_SCHOOL, school);
                detailGrid.set(DETAIL_REPORT_FIELD_SCHOOL_NAME, school.getName());
                detailGrid.set(DETAIL_REPORT_FIELD_GRADE_LEVEL, gradeLevel);
                detailGrid.set(DETAIL_REPORT_FIELD_STD_NAME, student.getNameView());
                detailGrid.set(DETAIL_REPORT_FIELD_STATE_ID, student.getFieldValueByAlias(ALIAS_EIS_STATE_ID));
                detailGrid.set(DETAIL_REPORT_FIELD_LOCAL_ID, student.getLocalId());
                detailGrid.set(DETAIL_REPORT_FIELD_GENDER, gender);
                detailGrid.set(DETAIL_REPORT_FIELD_NUM_SPANS, Integer.valueOf(spans.size()));
                detailGrid.set(DETAIL_REPORT_FIELD_NUM_PR_SPANS, Integer.valueOf(numProcessedSpans));

                detailGrid.set(DETAIL_REPORT_FIELD_IS_ENROLLED,
                        Boolean.valueOf(membershipFlags.isNetEnrolledStudent()));
                detailGrid.set(DETAIL_REPORT_FIELD_IS_N_ENROLLED,
                        Boolean.valueOf(membershipFlags.isNetEnrolledNGradeLevelStudent()));
                detailGrid.set(DETAIL_REPORT_FIELD_IS_END_PERIOD,
                        Boolean.valueOf(membershipFlags.isEndOfPeriodMembershipStudent()));
                detailGrid.set(DETAIL_REPORT_FIELD_IS_N_END_PERIOD,
                        Boolean.valueOf(membershipFlags.isEndOfPeriodMembershipNGradeLevelStudent()));

                detailGrid.set(DETAIL_REPORT_FIELD_IN_SESSION_DAYS, Integer.valueOf(inSessionDays));
                detailGrid.set(DETAIL_REPORT_FIELD_IN_SESSION_N_DAYS, Integer.valueOf(inSessionDaysNGradelevel));
                detailGrid.set(DETAIL_REPORT_FIELD_IN_SESSION_DAYS_EARLY, Integer.valueOf(inSessionDaysEarlyGraduated));
                detailGrid.set(DETAIL_REPORT_FIELD_IN_SESSION_DAYS_IEA, Integer.valueOf(inSessionDaysIEA));
                detailGrid.set(DETAIL_REPORT_FIELD_MEM_MINUTES, Integer.valueOf(membershipMinutes));
                detailGrid.set(DETAIL_REPORT_FIELD_MEM_N_MINUTES, Integer.valueOf(membershipNGradelevelMinutes));
                detailGrid.set(DETAIL_REPORT_FIELD_MEM_MINUTES_EARLY, Integer.valueOf(membershipMinutesEarlyGraduated));
                detailGrid.set(DETAIL_REPORT_FIELD_MEM_MINUTES_IEA, Integer.valueOf(membershipMinutesIEA));
                detailGrid.set(DETAIL_REPORT_FIELD_ABS_MINUTES, Integer.valueOf(absenceMinutes));
                detailGrid.set(DETAIL_REPORT_FIELD_ABS_N_MINUTES, Integer.valueOf(absenceNGradelevelMinutes));
                detailGrid.set(DETAIL_REPORT_FIELD_ABS_MINUTES_IEA, Integer.valueOf(absenceMinutesIEA));
                detailGrid.set(DETAIL_REPORT_FIELD_ABS_MINUTES_EARLY, Integer.valueOf(absenceMinutesEarlyGraduated));
                updateAccumulators(membershipFlags, accumulator, accumulatorNGradeLevel, gender);

                // If the student doesn't have time scheduled for the given range of dates, then no
                // need to calculate the ADM and ADA
                if (daysInSessionForStudent != null
                        && ((membershipMinutes != 0 || membershipMinutesEarlyGraduated != 0)
                                || membershipNGradelevelMinutes != 0)
                        || membershipMinutesIEA != 0) {

                    double stdAdm = ((double) membershipMinutes) / (stdStandardDay * daysInSessionForStudent.size());
                    double stdAdmGradeN =
                            ((double) membershipNGradelevelMinutes) / (stdStandardDay * daysInSessionForStudent.size());
                    double stdAdmEarlyGraduates = ((double) membershipMinutesEarlyGraduated)
                            / (stdStandardDay * daysInSessionForStudent.size());
                    double stdAdmIEA =
                            ((double) membershipMinutesIEA) / (stdStandardDay * daysInSessionForStudent.size());
                    double admCalculated = stdAdm +
                            (m_includeEarlyGraduates.booleanValue() ? stdAdmEarlyGraduates : 0) +
                            (m_includeIEA.booleanValue() ? stdAdmIEA : 0);

                    double stdAda =
                            ((double) membershipMinutes - absenceMinutes)
                                    / (stdStandardDay * daysInSessionForStudent.size());
                    double stdAdaGradeN =
                            ((double) membershipNGradelevelMinutes - absenceNGradelevelMinutes)
                                    / (stdStandardDay * daysInSessionForStudent.size());
                    double stdAdaEarly =
                            ((double) membershipMinutesEarlyGraduated - absenceMinutesEarlyGraduated)
                                    / (stdStandardDay * daysInSessionForStudent.size());
                    double stdAdaIEA =
                            ((double) membershipMinutesIEA - absenceMinutesIEA)
                                    / (stdStandardDay * daysInSessionForStudent.size());
                    double adaCalculated = stdAda +
                            (m_includeEarlyGraduates.booleanValue() ? stdAdaEarly : 0) +
                            (m_includeIEA.booleanValue() ? stdAdaIEA : 0);

                    // Increment NGradeLevel
                    accumulatorNGradeLevel.incrementAverageDailyMembership(stdAdmGradeN);
                    accumulatorNGradeLevel.incrementAverageDailyAttendance(stdAdaGradeN);

                    // Increment standard
                    accumulator.incrementAverageDailyMembership(admCalculated);
                    accumulator.incrementAverageDailyAttendance(adaCalculated);

                    // Set Grid
                    detailGrid.set(DETAIL_REPORT_FIELD_ADM_N, Double.valueOf(stdAdmGradeN));
                    detailGrid.set(DETAIL_REPORT_FIELD_ADA_N, Double.valueOf(stdAdaGradeN));
                    detailGrid.set(DETAIL_REPORT_FIELD_ADM_EARLY, Double.valueOf(stdAdmEarlyGraduates));
                    detailGrid.set(DETAIL_REPORT_FIELD_ADM, Double.valueOf(stdAdm));
                    detailGrid.set(DETAIL_REPORT_FIELD_ADM_IEA, Double.valueOf(stdAdmIEA));
                    detailGrid.set(DETAIL_REPORT_FIELD_ADM_CALCULATED, Double.valueOf(admCalculated));
                    detailGrid.set(DETAIL_REPORT_FIELD_ADA, Double.valueOf(stdAda));
                    detailGrid.set(DETAIL_REPORT_FIELD_ADA_EARLY, Double.valueOf(stdAdaEarly));
                    detailGrid.set(DETAIL_REPORT_FIELD_ADA_IEA, Double.valueOf(stdAdaIEA));
                    detailGrid.set(DETAIL_REPORT_FIELD_ADA_CALCULATED, Double.valueOf(adaCalculated));
                } else {
                    detailGrid.set(DETAIL_REPORT_FIELD_ADM_N, Double.valueOf(0.0));
                    detailGrid.set(DETAIL_REPORT_FIELD_ADA_N, Double.valueOf(0.0));
                    detailGrid.set(DETAIL_REPORT_FIELD_ADA_EARLY, Double.valueOf(0.0));
                    detailGrid.set(DETAIL_REPORT_FIELD_ADA_IEA, Double.valueOf(0.0));
                    detailGrid.set(DETAIL_REPORT_FIELD_ADM, Double.valueOf(0.0));
                    detailGrid.set(DETAIL_REPORT_FIELD_ADA, Double.valueOf(0.0));
                    detailGrid.set(DETAIL_REPORT_FIELD_ADM_EARLY, Double.valueOf(0.0));
                    detailGrid.set(DETAIL_REPORT_FIELD_ADM_IEA, Double.valueOf(0.0));
                    detailGrid.set(DETAIL_REPORT_FIELD_ADA_CALCULATED, Double.valueOf(0.0));
                    detailGrid.set(DETAIL_REPORT_FIELD_ADM_CALCULATED, Double.valueOf(0.0));
                }
            }
            if (m_includeAllGrades || m_includedSomeGrade || gradeLevel.intValue() == GRADE_LEVEL_NUMERIC_NONGRADED) {
                grid.append();
                grid.set(REPORT_FIELD_SCHOOL, school);
                grid.set(REPORT_FIELD_TOTAL_MAKEUP_DAYS, Integer.toString(totalMakeUpDays));
                grid.set(REPORT_FIELD_SCHOOL_SP_STOCKPILE, Integer.valueOf(stockpile));
                grid.set(REPORT_FIELD_ERROR_MESSAGE, emptyReportErrorMessage);

                // For N Grade level
                if (gradeLevel.intValue() == GRADE_LEVEL_NUMERIC_NONGRADED) {
                    grid.set(REPORT_FIELD_GRADE_LEVEL, Integer.valueOf(GRADE_LEVEL_NUMERIC_NONGRADED));
                    grid.set(REPORT_FIELD_GRADE_CODE, getGradeCode(Integer.valueOf(GRADE_LEVEL_NUMERIC_NONGRADED)));
                    grid.set(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_MALE,
                            Integer.valueOf(accumulatorNGradeLevel.getNetEnrolledStudentsMale()));
                    grid.set(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_FEMALE,
                            Integer.valueOf(accumulatorNGradeLevel.getNetEnrolledStudentsFemale()));
                    grid.set(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_TOTAL,
                            Integer.valueOf(accumulatorNGradeLevel.getNetEnrolledStudentsMale()
                                    + accumulatorNGradeLevel.getNetEnrolledStudentsFemale()));
                    grid.set(REPORT_FIELD_END_OF_PERIOD_MEMBERSHIP,
                            Integer.valueOf(accumulatorNGradeLevel.getEndOfPeriodMembershipStudents()));
                    grid.set(REPORT_FIELD_AVERAGE_DAILY_ATTENDANCE,
                            m_decimalFormat.format(accumulatorNGradeLevel.getAverageDailyAttendance()).toString());
                    grid.set(REPORT_FIELD_AVERAGE_DAILY_MEMBERSHIP,
                            m_decimalFormat.format(accumulatorNGradeLevel.getAverageDailyMembership()).toString());
                } else {
                    grid.set(REPORT_FIELD_GRADE_LEVEL, gradeLevel);
                    grid.set(REPORT_FIELD_GRADE_CODE, getGradeCode(gradeLevel));
                    Integer netEnrollmentToDateMale = Integer.valueOf(accumulator.getNetEnrolledStudentsMale());
                    Integer netEnrollmentToDateFemale = Integer.valueOf(accumulator.getNetEnrolledStudentsFemale());
                    Integer netEnrollmentToDateTotal = Integer.valueOf(netEnrollmentToDateMale.intValue() +
                            netEnrollmentToDateFemale.intValue());
                    grid.set(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_MALE, netEnrollmentToDateMale);
                    grid.set(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_FEMALE, netEnrollmentToDateFemale);
                    grid.set(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_TOTAL, netEnrollmentToDateTotal);
                    grid.set(REPORT_FIELD_END_OF_PERIOD_MEMBERSHIP,
                            Integer.valueOf(accumulator.getEndOfPeriodMembershipStudents()));
                    grid.set(REPORT_FIELD_AVERAGE_DAILY_ATTENDANCE,
                            m_decimalFormat.format(accumulator.getAverageDailyAttendance()));
                    grid.set(REPORT_FIELD_AVERAGE_DAILY_MEMBERSHIP,
                            m_decimalFormat.format(accumulator.getAverageDailyMembership()));
                }
                grid.set(REPORT_FIELD_ORG_NAME, school == null ? "No School" : school.getName());
                if (m_data != null) {
                    grid.set(REPORT_FIELD_START_DATE, m_data.getFormattedDate(m_periodStartDate));
                    grid.set(REPORT_FIELD_END_DATE, m_data.getFormattedDate(m_periodEndDate));
                }
            }
        }
    }

    /**
     * Add data for the schoolGrid to districtGrid
     *
     * NOTE: the rows and columns of schoolGrid and districtGrid should be match.
     *
     * @param schoolGrid ReportDataGrid
     * @param districtGrid return ReportDataGrid updatedDistrictGrid
     * @return ReportDataGrid
     */
    private ReportDataGrid appendDistrictSummaryDataGrid(ReportDataGrid schoolGrid, ReportDataGrid districtGrid) {
        // Copy the districtGrid and clear it, so it can be reused
        ReportDataGrid updatedDistrictGrid = new ReportDataGrid();

        if (!districtGrid.isEmpty()) {
            districtGrid.beforeTop();
        }
        schoolGrid.beforeTop();
        while (schoolGrid.next()) {
            Integer gradeLevel = (Integer) schoolGrid.get(REPORT_FIELD_GRADE_LEVEL);
            int totalMakeUpDays = 0;
            int stockpile = 0;
            int netEnrollmentToDateMale = 0;
            int netEnrollmentToDateFemale = 0;
            int netEnrollmentToDateTotal = 0;
            int endOfPeriodMembershipStudents = 0;
            double averageDailyAttendance = 0;
            double averageDailyMembership = 0;

            // add the data from the same row of district grid if the data is available
            if (!districtGrid.isEmpty()) {
                districtGrid.next();

                totalMakeUpDays = Integer.parseInt(districtGrid.get(REPORT_FIELD_TOTAL_MAKEUP_DAYS).toString());
                stockpile = Integer.parseInt(districtGrid.get(REPORT_FIELD_SCHOOL_SP_STOCKPILE).toString());
                netEnrollmentToDateMale =
                        Integer.parseInt(districtGrid.get(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_MALE).toString());
                netEnrollmentToDateFemale =
                        Integer.parseInt(districtGrid.get(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_FEMALE).toString());
                netEnrollmentToDateTotal =
                        Integer.parseInt(districtGrid.get(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_TOTAL).toString());
                endOfPeriodMembershipStudents =
                        Integer.parseInt(districtGrid.get(REPORT_FIELD_END_OF_PERIOD_MEMBERSHIP).toString());
                averageDailyAttendance =
                        Double.parseDouble(districtGrid.get(REPORT_FIELD_AVERAGE_DAILY_ATTENDANCE).toString());
                averageDailyMembership =
                        Double.parseDouble(districtGrid.get(REPORT_FIELD_AVERAGE_DAILY_MEMBERSHIP).toString());
            }

            // add the data row from the school grid
            totalMakeUpDays += Integer.parseInt(schoolGrid.get(REPORT_FIELD_TOTAL_MAKEUP_DAYS).toString());
            stockpile += Integer.parseInt(schoolGrid.get(REPORT_FIELD_SCHOOL_SP_STOCKPILE).toString());
            netEnrollmentToDateMale +=
                    Integer.parseInt(schoolGrid.get(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_MALE).toString());
            netEnrollmentToDateFemale +=
                    Integer.parseInt(schoolGrid.get(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_FEMALE).toString());
            netEnrollmentToDateTotal +=
                    Integer.parseInt(schoolGrid.get(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_TOTAL).toString());
            endOfPeriodMembershipStudents +=
                    Integer.parseInt(schoolGrid.get(REPORT_FIELD_END_OF_PERIOD_MEMBERSHIP).toString());
            averageDailyAttendance +=
                    Double.parseDouble(schoolGrid.get(REPORT_FIELD_AVERAGE_DAILY_ATTENDANCE).toString());
            averageDailyMembership +=
                    Double.parseDouble(schoolGrid.get(REPORT_FIELD_AVERAGE_DAILY_MEMBERSHIP).toString());

            updatedDistrictGrid.append();
            updatedDistrictGrid.set(REPORT_FIELD_TOTAL_MAKEUP_DAYS, Integer.toString(totalMakeUpDays));
            updatedDistrictGrid.set(REPORT_FIELD_SCHOOL_SP_STOCKPILE, Integer.valueOf(stockpile));
            updatedDistrictGrid.set(REPORT_FIELD_GRADE_LEVEL, gradeLevel);
            updatedDistrictGrid.set(REPORT_FIELD_GRADE_CODE, getGradeCode(gradeLevel));
            updatedDistrictGrid.set(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_MALE, Integer.valueOf(netEnrollmentToDateMale));
            updatedDistrictGrid.set(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_FEMALE,
                    Integer.valueOf(netEnrollmentToDateFemale));
            updatedDistrictGrid.set(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_TOTAL,
                    Integer.valueOf(netEnrollmentToDateTotal));
            updatedDistrictGrid.set(REPORT_FIELD_END_OF_PERIOD_MEMBERSHIP,
                    Integer.valueOf(endOfPeriodMembershipStudents));

            if (gradeLevel.intValue() == GRADE_LEVEL_NUMERIC_TOTAL_K_12_N
                    || gradeLevel.intValue() == GRADE_LEVEL_NUMERIC_TOTAL_K_12) {
                updatedDistrictGrid.set(REPORT_FIELD_AVERAGE_DAILY_ATTENDANCE,
                        m_decimalFormat.format(averageDailyAttendance).toString());
                updatedDistrictGrid.set(REPORT_FIELD_AVERAGE_DAILY_MEMBERSHIP,
                        m_decimalFormat.format(averageDailyMembership).toString());
            } else {
                updatedDistrictGrid.set(REPORT_FIELD_AVERAGE_DAILY_ATTENDANCE,
                        m_decimalFormat.format(averageDailyAttendance).toString());
                updatedDistrictGrid.set(REPORT_FIELD_AVERAGE_DAILY_MEMBERSHIP,
                        m_decimalFormat.format(averageDailyMembership).toString());
            }
            updatedDistrictGrid.set(REPORT_FIELD_ORG_NAME, getOrganization().getName());
            updatedDistrictGrid.set(REPORT_FIELD_START_DATE, m_data.getFormattedDate(m_periodStartDate));
            updatedDistrictGrid.set(REPORT_FIELD_END_DATE, m_data.getFormattedDate(m_periodEndDate));
        }

        return updatedDistrictGrid;
    }

    /**
     * Fills school grid by zeros and shows error message in report for this school.
     *
     * @param school SisSchool
     * @param grid ReportDataGrid
     * @param detailGrid ReportDataGrid
     * @param message String
     */
    private void appendSchoolGridWithMessage(SisSchool school,
                                             ReportDataGrid grid,
                                             ReportDataGrid detailGrid,
                                             String message) {
        ReportDataGrid schoolGrid = new ReportDataGrid();
        appendDataGrid(school, null, getDefaultSortedSpans(), 0, 0, schoolGrid, detailGrid, message);
        grid.append(schoolGrid);
    }

    /**
     * Count totals for classes K-12 and for all classes.
     *
     * @param grid ReportDataGrid
     */
    private void countTotals(ReportDataGrid grid) {
        int netEnrollmentToDateMale = 0;
        int netEnrollmentToDateFemale = 0;
        int netEnrollmentToDateTotal = 0;
        int endOfPeriodMembershipStudents = 0;
        double averageDailyMembership = 0;
        double averageDailyAttendance = 0;

        // For N Grade level
        int netEnrollmentNGradeLevelMale = 0;
        int netEnrollmentNGradeLevelFemale = 0;
        int netEnrollmentNGradeLevelTotal = 0;
        int endOfPeriodMembershipNGradeLevelStudents = 0;
        double averageNGradeLevelDailyMembership = 0;
        double averageNGradeLevelDailyAttendance = 0;

        grid.beforeTop();
        while (grid.next()) {
            Integer gradeLevel = (Integer) grid.get(REPORT_FIELD_GRADE_LEVEL);
            if (gradeLevel.intValue() == GRADE_LEVEL_NUMERIC_TOTAL_K_12) {
                grid.set(REPORT_FIELD_GRADE_CODE, GRADE_LEVEL_LETTER_TOTAL_K_12);
                grid.set(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_MALE, Integer.valueOf(netEnrollmentToDateMale));
                grid.set(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_FEMALE, Integer.valueOf(netEnrollmentToDateFemale));
                grid.set(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_TOTAL, Integer.valueOf(netEnrollmentToDateTotal));
                grid.set(REPORT_FIELD_END_OF_PERIOD_MEMBERSHIP, Integer.valueOf(endOfPeriodMembershipStudents));
                grid.set(REPORT_FIELD_AVERAGE_DAILY_ATTENDANCE,
                        m_decimalFormat.format(averageDailyAttendance).toString());
                grid.set(REPORT_FIELD_AVERAGE_DAILY_MEMBERSHIP,
                        m_decimalFormat.format(averageDailyMembership).toString());
            } else if (gradeLevel.intValue() == GRADE_LEVEL_NUMERIC_TOTAL_K_12_N) {
                grid.set(REPORT_FIELD_GRADE_CODE, GRADE_LEVEL_LETTER_TOTAL_K_12_N);
                grid.set(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_MALE,
                        Integer.valueOf(netEnrollmentToDateMale + netEnrollmentNGradeLevelMale));
                grid.set(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_FEMALE,
                        Integer.valueOf(netEnrollmentToDateFemale + netEnrollmentNGradeLevelFemale));
                grid.set(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_TOTAL,
                        Integer.valueOf(netEnrollmentToDateTotal + netEnrollmentNGradeLevelTotal));
                grid.set(REPORT_FIELD_END_OF_PERIOD_MEMBERSHIP,
                        Integer.valueOf(endOfPeriodMembershipStudents + endOfPeriodMembershipNGradeLevelStudents));
                grid.set(REPORT_FIELD_AVERAGE_DAILY_ATTENDANCE,
                        m_decimalFormat.format(averageDailyAttendance + averageNGradeLevelDailyAttendance).toString());
                grid.set(REPORT_FIELD_AVERAGE_DAILY_MEMBERSHIP,
                        m_decimalFormat.format(averageDailyMembership + averageNGradeLevelDailyMembership).toString());
            } else if (gradeLevel.intValue() == GRADE_LEVEL_NUMERIC_NONGRADED) {
                netEnrollmentNGradeLevelMale +=
                        ((Integer) grid.get(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_MALE)).intValue();
                netEnrollmentNGradeLevelFemale +=
                        ((Integer) grid.get(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_FEMALE)).intValue();
                netEnrollmentNGradeLevelTotal +=
                        ((Integer) grid.get(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_TOTAL)).intValue();
                endOfPeriodMembershipNGradeLevelStudents +=
                        ((Integer) grid.get(REPORT_FIELD_END_OF_PERIOD_MEMBERSHIP)).intValue();
                averageNGradeLevelDailyAttendance +=
                        (Double.parseDouble((String) grid.get(REPORT_FIELD_AVERAGE_DAILY_ATTENDANCE)));
                averageNGradeLevelDailyMembership +=
                        (Double.parseDouble((String) grid.get(REPORT_FIELD_AVERAGE_DAILY_MEMBERSHIP)));
            } else {
                netEnrollmentToDateMale += ((Integer) grid.get(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_MALE)).intValue();
                netEnrollmentToDateFemale +=
                        ((Integer) grid.get(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_FEMALE)).intValue();
                netEnrollmentToDateTotal += ((Integer) grid.get(REPORT_FIELD_NET_ENROLLMENT_TO_DATE_TOTAL)).intValue();
                endOfPeriodMembershipStudents += ((Integer) grid.get(REPORT_FIELD_END_OF_PERIOD_MEMBERSHIP)).intValue();
                averageDailyAttendance +=
                        (Double.parseDouble((String) grid.get(REPORT_FIELD_AVERAGE_DAILY_ATTENDANCE)));
                averageDailyMembership +=
                        (Double.parseDouble((String) grid.get(REPORT_FIELD_AVERAGE_DAILY_MEMBERSHIP)));
            }
        }
    }

    /**
     * Calculates enrollment code for the given enrollment.
     *
     * @param enrollment StudentEnrollment
     * @return String
     */
    private String calculateEnrollmentCode(StudentEnrollment enrollment) {
        String stateEnrollmentCode =
                m_data.lookupStateValue(StudentEnrollment.class, StudentEnrollment.COL_ENROLLMENT_CODE,
                        enrollment.getEnrollmentCode());
        if (enrollment.getEnrollmentDate().before(m_context.getStartDate())) {
            if (CODE_OUT_OF_STATE_ENROLLMENT.equals(stateEnrollmentCode)
                    || CODE_ENROLL_TRANSFERRED.equals(stateEnrollmentCode)) {
                stateEnrollmentCode = CODE_REGULAR_ENROLLMENT;
            } else if (CODE_ENROLL_UNDER_ACHIEVING.equals(stateEnrollmentCode)) {
                stateEnrollmentCode = CODE_TRANSFER_SCHOOL_UNDER_ARCHIEVING;
            }
        }
        if (StringUtils.isEmpty(stateEnrollmentCode)
                && StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType())) {
            stateEnrollmentCode = CODE_ENROLL_TRANSFERRED;
        }
        return stateEnrollmentCode;
    }

    /**
     * Returns in-session days for the reporting period.
     *
     * @param calendarDays Collection<PlainDate>
     * @return Set<PlainDate> days in session
     */
    private Set<PlainDate> getDaysInSession(Collection<PlainDate> calendarDays) {
        Set<PlainDate> daysInSession = new HashSet<PlainDate>();
        for (PlainDate calendarDay : calendarDays) {
            if (!calendarDay.before(m_periodStartDate) && !calendarDay.after(m_periodEndDate)) {
                daysInSession.add(calendarDay);
            }
        }
        return daysInSession;
    }

    /**
     * Create default map.
     *
     * @return TreeMap<Integer, Map<SisStudent, List<StudentEnrollmentSpan>>>
     */
    private TreeMap<Integer, Map<SisStudent, List<TNStudentEnrollmentSpan>>> getDefaultSortedSpans() {
        TreeMap<Integer, Map<SisStudent, List<TNStudentEnrollmentSpan>>> sortedSpans =
                new TreeMap<Integer, Map<SisStudent, List<TNStudentEnrollmentSpan>>>();

        m_grades.stream().forEach(grade -> {
            Map<SisStudent, List<TNStudentEnrollmentSpan>> map = new HashMap();
            sortedSpans.put(Integer.valueOf(grade), map);
        });

        Map<SisStudent, List<TNStudentEnrollmentSpan>> gradeLevelStudentSpansMap = new HashMap();
        sortedSpans.put(Integer.valueOf(GRADE_LEVEL_NUMERIC_TOTAL_K_12), gradeLevelStudentSpansMap);
        gradeLevelStudentSpansMap = new HashMap<SisStudent, List<TNStudentEnrollmentSpan>>();
        if (m_includeNonGraded) {
            sortedSpans.put(Integer.valueOf(GRADE_LEVEL_NUMERIC_NONGRADED), gradeLevelStudentSpansMap);
        }
        gradeLevelStudentSpansMap = new HashMap<SisStudent, List<TNStudentEnrollmentSpan>>();
        sortedSpans.put(Integer.valueOf(GRADE_LEVEL_NUMERIC_TOTAL_K_12_N), gradeLevelStudentSpansMap);

        return sortedSpans;
    }

    /**
     * Calculate number of days in session for the District.
     *
     * @return int
     */
    private int getDistrictDaysInSession() {
        int daysInsession = 0;
        X2Criteria criteria = new X2Criteria();

        criteria.addEqualTo(DistrictCalendar.COL_DISTRICT_CONTEXT_OID, m_context.getOid());
        if (m_reportPeriod != null) {
            criteria.addEqualTo(DistrictCalendar.COL_CYCLE, m_reportPeriod.getCode());
        }
        criteria.addEqualTo(DistrictCalendar.COL_IN_SESSION_INDICATOR, BooleanAsStringConverter.TRUE);

        QueryByCriteria query = new QueryByCriteria(DistrictCalendar.class, criteria);

        Collection days = getBroker().getCollectionByQuery(query);

        if (days != null) {
            daysInsession += days.size();
        }

        return daysInsession;
    }

    /**
     * Returns dates for early gradutes.
     *
     * @param span TNStudentEnrollmentSpan
     * @return Collection
     */
    private Collection<PlainDate> getEarlyGraduatedInSesionDates(TNStudentEnrollmentSpan span) {
        Collection<PlainDate> inSessionForEarly = new ArrayList<>();
        StudentEnrollment studentEnrollment = span.getEnrollmentForDate(m_periodEndDate, "W");
        if (studentEnrollment != null) {
            PlainDate wDate = studentEnrollment.getEnrollmentDate();
            PlainDate ctxEndDate = m_periodHelper.getYearEndDate();
            if (wDate != null && wDate.before(ctxEndDate)) {
                Collection<PlainDate> inSessionDates =
                        m_periodHelper.getDaysInSession(studentEnrollment.getSchoolOid());
                if (inSessionDates != null && !inSessionDates.isEmpty()) {
                    for (PlainDate date : inSessionDates) {
                        if (DateUtils.isBetween(date, wDate, ctxEndDate)) {
                            inSessionForEarly.add(date);
                        }
                    }
                }
            }
        }
        return inSessionForEarly;
    }

    /**
     * Get grade level code.
     *
     * @param gradeLevel Integer
     * @return String
     */
    private String getGradeCode(Integer gradeLevel) {
        if (gradeLevel.intValue() == GRADE_LEVEL_NUMERIC_KINDERGARTEN) {
            return GRADE_LEVEL_LETTER_KINDERGARTEN;
        } else if (gradeLevel.intValue() == GRADE_LEVEL_NUMERIC_NONGRADED) {
            return GRADE_LEVEL_LETTER_NONGRADED;
        } else if (gradeLevel.intValue() == GRADE_LEVEL_NUMERIC_TOTAL_K_12) {
            return GRADE_LEVEL_LETTER_TOTAL_K_12;
        } else if (gradeLevel.intValue() == GRADE_LEVEL_NUMERIC_TOTAL_K_12_N) {
            return GRADE_LEVEL_LETTER_TOTAL_K_12_N;
        }
        return gradeLevel.toString();
    }

    /**
     * Get a list of the grade codes selected.
     *
     * @return List
     */
    private List<Integer> getSelectedGradesInputParam() {
        List<Integer> grades = new ArrayList<>();
        String gradesInputString = (String) getParameter(INPUT_PARAM_GRADES);
        if (!StringUtils.isEmpty(gradesInputString)) {
            String[] rcdOids = gradesInputString.split(",");
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(X2BaseBean.COL_OID, Arrays.asList(rcdOids));
            BeanQuery gradesQuery = new BeanQuery(ReferenceCode.class, criteria);
            Map<String, ReferenceCode> gradesMap = getBroker().getMapByQuery(gradesQuery, ReferenceCode.COL_CODE, 5);
            if (gradesMap != null) {
                grades.addAll(gradesMap.values().stream().map(this::prepareGradeLevel).collect(Collectors.toList()));
            }
        }
        return grades;
    }

    /**
     * Get the most common calendar of the given school.
     *
     * @param school SisSchool
     * @return String
     */
    private String getMostCommonCalendar(SisSchool school) {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(m_data.m_tnMultiYearHelper.getActiveStudentCriteria());
        criteria.addEqualTo(m_data.m_tnMultiYearHelper.getSchoolOidField(), school.getOid());

        SubQuery query = new SubQuery(SisStudent.class, m_data.m_tnMultiYearHelper.getCalendarCodeField(), criteria);
        query.addGroupBy(m_data.m_tnMultiYearHelper.getCalendarCodeField());
        query.addOrderByDescending("count(*)");

        return (String) getBroker().getSubQueryValueByQuery(query);
    }

    /**
     * Gets the student counts for span.
     *
     * @param span TNStudentEnrollmentSpan
     * @param student SisStudent
     * @param daysInSession Collection<PlainDate>
     * @param daysInSessionForStudent Collection<PlainDate>
     * @param nGradeLevelsPeriodCollections List<Pair<PlainDate,PlainDate>>
     * @param ieaPeriodCollections List<Pair<PlainDate,PlainDate>>
     * @param stdStandardDay int
     * @return Student span count
     */
    private StudentSpanCount getStudentCountsForSpan(TNStudentEnrollmentSpan span,
                                                     SisStudent student,
                                                     Collection<PlainDate> daysInSession,
                                                     Collection<PlainDate> daysInSessionForStudent,
                                                     List<Pair<PlainDate, PlainDate>> nGradeLevelsPeriodCollections,
                                                     List<Pair<PlainDate, PlainDate>> ieaPeriodCollections,
                                                     int stdStandardDay) {
        int absenceMinutes = 0;
        int absenceNGradelevelMinutes = 0;
        int absenceMinutesEarlyGraduated = 0;
        int absenceMinutesIEA = 0;
        int membershipMinutes = 0;
        int membershipMinutesEarlyGraduated = 0;
        int membershipMinutesIEA = 0;
        int membershipNGradelevelMinutes = 0;
        int inSessionDaysEarlyGraduated = 0;
        int inSessionDays = 0;
        int inSessionDaysIEA = 0;
        int inSessionDaysNGradelevel = 0;
        PlainDate spanFirstActiveDate = span.getFirstActiveDate();
        PlainDate spanLastActiveDate = span.getFirstInactiveEnrollment() != null ? span.getLastActiveDate() : null;

        if (spanFirstActiveDate == null
                || (spanFirstActiveDate != null && spanFirstActiveDate.before(m_periodStartDate))) {
            spanFirstActiveDate = m_periodStartDate;
        }
        if (spanLastActiveDate == null || (spanLastActiveDate != null && spanLastActiveDate.after(m_periodEndDate))) {
            spanLastActiveDate = m_periodEndDate;
        }

        try {
            List<TNStudentScheduleSpan> scheduleSpans = m_data.getTNStudentScheduleSpans(student);

            TNClassSectionScheduleEntity sectionHelper = new TNClassSectionScheduleEntity(m_scheduleData, scheduleSpans,
                    daysInSession, spanFirstActiveDate, spanLastActiveDate);
            for (PlainDate day : daysInSession) {
                if (!day.before(spanFirstActiveDate) && !day.after(spanLastActiveDate)) {
                    ++inSessionDays;
                }
            }

            // Useful Debugging block
            //
            // for (TNStudentScheduleSpan tmpSpan : scheduleSpans) {
            // List<TNStudentScheduleSpan> tmpSpans = new ArrayList(1);
            // tmpSpans.add(tmpSpan);
            // TNClassSectionScheduleEntity tmpHelper = new
            // TNClassSectionScheduleEntity(m_scheduleData, tmpSpans,
            // daysInSession, spanFirstActiveDate, spanLastActiveDate);
            // int index = 0;
            // DateAsStringConverter dateConverter = (DateAsStringConverter) ConverterFactory
            // .getConverterForClass(PlainDate.class.getName(),
            // Locale.getDefault(), true);
            // PlainDate compareDate = (PlainDate) dateConverter.parseSystemString("2016-11-03");
            // System.out.println(tmpSpan.toString() + " " + (++index) + " " +
            // compareDate.toString() + " " +
            // tmpHelper.getScheduledMinutes(compareDate, stdStandardDay));
            // }
            // int index = 0;
            // TreeSet<PlainDate> dates = new TreeSet(daysInSession);
            // for (PlainDate date : dates) {
            // int tmpMinutes = sectionHelper.getScheduledMinutes(date, stdStandardDay);
            // if (tmpMinutes > 0) {
            // System.out.println("Total " + (++index) + " " + date.toString() + " " + tmpMinutes);
            // }
            // }

            if (daysInSessionForStudent != null) {
                List<StudentAttendance> absences = m_data.getStudentAttendances(student.getOid());
                List<PlainDate> absenceDates = new ArrayList<PlainDate>();
                if (!CollectionUtils.isEmpty(absences)) {
                    for (StudentAttendance absence : absences) {
                        absenceDates.add(absence.getDate());
                    }
                }
                // Get the total membership minutes
                membershipMinutes += sectionHelper.getTotalMinutes(stdStandardDay);
                if (isEarlyGraduated(span)) {
                    Collection<PlainDate> datesForEarly = getEarlyGraduatedInSesionDates(span);
                    inSessionDaysEarlyGraduated += datesForEarly.size();
                    membershipMinutesEarlyGraduated += datesForEarly.size() * stdStandardDay;
                }
                // Get the absence minutes
                if (!spanFirstActiveDate.after(spanLastActiveDate) && !CollectionUtils.isEmpty(absenceDates)) {
                    for (PlainDate absenceDate : absenceDates) {
                        if (DateUtils.isBetween(absenceDate, spanFirstActiveDate, spanLastActiveDate)) {
                            absenceMinutes += sectionHelper.getScheduledMinutes(absenceDate, stdStandardDay);
                        }
                    }
                }

                // Count the N grade level program participation within this span, and subtract from
                // the numbers we calculated above
                if (nGradeLevelsPeriodCollections != null) {
                    for (Pair<PlainDate, PlainDate> startEndDates : nGradeLevelsPeriodCollections) {
                        PlainDate programStartDate = startEndDates.getLeft();
                        PlainDate programEndDate = startEndDates.getRight();

                        // Re-adjust the date, then calculate the membership minutes
                        if (TNReportingPeriodHelper.doDatesOverlap(spanFirstActiveDate, spanLastActiveDate,
                                programStartDate, programEndDate)) {
                            PlainDate tempStartDate = spanFirstActiveDate;
                            PlainDate tempEndDate = spanLastActiveDate;

                            // Check which start date to use. NOTE: participation start date cannot
                            // be null.
                            if (tempStartDate.before(programStartDate)) {
                                tempStartDate = programStartDate;
                            }

                            // Check which end date to use.
                            if (programEndDate != null && tempEndDate.after(programEndDate)) {
                                tempEndDate = programEndDate;
                            }

                            // Count the membership minutes for N Grade Level
                            for (PlainDate curInSessionDate : daysInSessionForStudent) {
                                if (DateUtils.isBetween(curInSessionDate, tempStartDate, tempEndDate)) {
                                    inSessionDaysNGradelevel += 1;
                                    inSessionDays -= 1;
                                    int minutes = sectionHelper.getScheduledMinutes(curInSessionDate, stdStandardDay);
                                    membershipNGradelevelMinutes += minutes;
                                    membershipMinutes -= minutes;
                                }
                            }
                            // Check/count for the N Grade Level absence minutes
                            if (!tempStartDate.after(tempEndDate) && !CollectionUtils.isEmpty(absences)) {
                                for (PlainDate absenceDate : absenceDates) {
                                    if (DateUtils.isBetween(absenceDate, tempStartDate, tempEndDate)) {
                                        int minutes = sectionHelper.getScheduledMinutes(absenceDate, stdStandardDay);
                                        absenceNGradelevelMinutes += minutes;
                                        absenceMinutes -= minutes;
                                    }
                                }
                            }
                        }
                    }
                }

                // Count the N grade level program participation within this span, and subtract from
                // the numbers we calculated above
                if (ieaPeriodCollections != null) {
                    for (Pair<PlainDate, PlainDate> startEndDates : ieaPeriodCollections) {
                        PlainDate programStartDate = startEndDates.getLeft();
                        PlainDate programEndDate = startEndDates.getRight();
                        // Re-adjust the date, then calculate the membership minutes
                        if (TNReportingPeriodHelper.doDatesOverlap(spanFirstActiveDate, spanLastActiveDate,
                                programStartDate, programEndDate)) {
                            PlainDate tempStartDate = spanFirstActiveDate;
                            PlainDate tempEndDate = spanLastActiveDate;
                            // Check which start date to use. NOTE: participation start date cannot
                            // be null.
                            if (tempStartDate.before(programStartDate)) {
                                tempStartDate = programStartDate;
                            }
                            // Check which end date to use.
                            if (programEndDate != null && tempEndDate.after(programEndDate)) {
                                tempEndDate = programEndDate;
                            }
                            // Count the membership minutes for IEA
                            for (PlainDate curInSessionDate : daysInSessionForStudent) {
                                if (DateUtils.isBetween(curInSessionDate, tempStartDate, tempEndDate)) {
                                    int minutes = sectionHelper.getScheduledMinutes(curInSessionDate, stdStandardDay);
                                    membershipMinutesIEA += minutes;
                                    membershipMinutes -= minutes;
                                    inSessionDaysIEA += 1;
                                    inSessionDays -= 1;
                                }
                            }
                            // Check/count for the IEA absence minutes
                            if (!tempStartDate.after(tempEndDate) && !CollectionUtils.isEmpty(absences)) {
                                for (PlainDate absenceDate : absenceDates) {
                                    if (DateUtils.isBetween(absenceDate, tempStartDate, tempEndDate)) {
                                        absenceMinutesIEA +=
                                                sectionHelper.getScheduledMinutes(absenceDate, stdStandardDay);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        } catch (X2BaseException x2Exception) {
            absenceMinutes = 0;
            absenceNGradelevelMinutes = 0;
            absenceMinutesEarlyGraduated = 0;
            absenceMinutesIEA = 0;
            membershipMinutes = 0;
            membershipNGradelevelMinutes = 0;
            membershipMinutesEarlyGraduated = 0;
            membershipMinutesIEA = 0;
            inSessionDays = 0;
            inSessionDaysNGradelevel = 0;
            inSessionDaysEarlyGraduated = 0;
            inSessionDaysIEA = 0;
            AppGlobals.getLog().log(Level.SEVERE, x2Exception.getMessage(), x2Exception);
        }
        return new StudentSpanCount(absenceMinutes,
                absenceNGradelevelMinutes,
                absenceMinutesEarlyGraduated,
                absenceMinutesIEA,
                membershipMinutes,
                membershipNGradelevelMinutes,
                membershipMinutesEarlyGraduated,
                membershipMinutesIEA,
                inSessionDays,
                inSessionDaysNGradelevel,
                inSessionDaysEarlyGraduated,
                inSessionDaysIEA);
    }

    /**
     * Initialize report formats.
     */
    private void initReportsFormat() {
        String formatPDF = (String) getParameter(INPUT_REPORT_ID_PDF);
        String formatCSVDetail = (String) getParameter(INPUT_REPORT_ID_CSV);
        String formatCSVSum = (String) getParameter(INPUT_REPORT_ID_SUM_CSV);
        ToolJob job = this.getJob();
        m_useDetail = false;
        switch (job.getInput().getFormat()) {
            case ToolInput.CSV_FORMAT:
                if (m_includeDistrictSummary) {
                    this.setFormatId(formatCSVSum);
                } else {
                    m_useDetail = true;
                    this.setFormatId(formatCSVDetail);
                }
                break;
            case ToolInput.HTML_FORMAT:
                this.setFormatId(formatPDF);
                break;
            case ToolInput.PDF_FORMAT:
                this.setFormatId(formatPDF);
                break;
            case ToolInput.XLS_FORMAT:
                this.setFormatId(formatPDF);
                break;
        }
    }

    /**
     * Inits the sorted spans.
     *
     * @return Map
     */
    private Map<String, TreeMap<Integer, Map<SisStudent, List<TNStudentEnrollmentSpan>>>> initSortedSpans() {
        Map<String, TreeMap<Integer, Map<SisStudent, List<TNStudentEnrollmentSpan>>>> mapSpans = new HashMap();
        BeanQuery studentQuery = new BeanQuery(SisStudent.class, m_data.getStudentCriteria());
        studentQuery.setDistinct(true);
        QueryIterator students = getBroker().getIteratorByQuery(studentQuery);
        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();

                List<TNStudentEnrollmentSpan> spans = m_data.getStudentEnrollmentSpans(student, true);

                // Find Student Grade Level - use last grade level from last span that intersects
                // with period.
                Integer gradeLevelLatest = null;

                boolean earlyGraduated = false;
                for (TNStudentEnrollmentSpan span : spans) {
                    earlyGraduated = isEarlyGraduated(span);
                    if (earlyGraduated) {
                        break;
                    }
                }

                for (TNStudentEnrollmentSpan span : spans) {
                    if (span.getFirstActiveEnrollment() != null && span.getFirstActiveEnrollment().getSchool() != null
                            && (spanIntersectsWithPeriod(span) || earlyGraduated)) {
                        // Only count students with the DOE INSTR SERVICE TYPE = P, or when it's
                        // null (P should be the default value when it's null)
                        String instrServiceType =
                                span.getFirstActiveEnrollment().getFieldValueByAlias(ALIAS_INSTR_SERVICE_TYPE) != null
                                        ? span.getFirstActiveEnrollment().getFieldValueByAlias(ALIAS_INSTR_SERVICE_TYPE)
                                                .toString()
                                        : null;
                        if (instrServiceType == null
                                || INSTR_SERVICE_TYPE_PRIMARY_SCHOOL_CODE.equals(instrServiceType)) {
                            ReferenceCode gradeCode = m_data.getGradeLevel(student, span);
                            Integer gradeLevel = prepareGradeLevel(gradeCode);
                            if (gradeLevel != null) {
                                gradeLevelLatest = gradeLevel;
                            }
                        }
                    }
                }
                if (gradeLevelLatest != null && m_grades.contains(gradeLevelLatest)) {
                    for (TNStudentEnrollmentSpan span : spans) {
                        if (span.getFirstActiveEnrollment() != null
                                && span.getFirstActiveEnrollment().getSchool() != null) {
                            // Only count students with the DOE INSTR SERVICE TYPE = P, or when it's
                            // null (P should be the default value when it's null)
                            String instrServiceType = span.getFirstActiveEnrollment()
                                    .getFieldValueByAlias(ALIAS_INSTR_SERVICE_TYPE) != null
                                            ? span.getFirstActiveEnrollment()
                                                    .getFieldValueByAlias(ALIAS_INSTR_SERVICE_TYPE).toString()
                                            : null;
                            if (instrServiceType == null
                                    || INSTR_SERVICE_TYPE_PRIMARY_SCHOOL_CODE.equals(instrServiceType)) {
                                String schoolOid = span.getFirstActiveEnrollment().getSchool().getOid();
                                TreeMap<Integer, Map<SisStudent, List<TNStudentEnrollmentSpan>>> sortedSpans =
                                        mapSpans.get(schoolOid);
                                if (sortedSpans == null) {
                                    sortedSpans = getDefaultSortedSpans();
                                    mapSpans.put(schoolOid, sortedSpans);
                                }
                                Map<SisStudent, List<TNStudentEnrollmentSpan>> gradeLevelStudentSpansMap =
                                        sortedSpans.get(gradeLevelLatest);
                                if (gradeLevelStudentSpansMap == null) {
                                    gradeLevelStudentSpansMap =
                                            new HashMap<SisStudent, List<TNStudentEnrollmentSpan>>();
                                    sortedSpans.put(gradeLevelLatest, gradeLevelStudentSpansMap);
                                }
                                List<TNStudentEnrollmentSpan> gradeLevelStudentSpans =
                                        gradeLevelStudentSpansMap.get(student);
                                if (gradeLevelStudentSpans == null) {
                                    gradeLevelStudentSpans = new ArrayList<TNStudentEnrollmentSpan>();
                                    gradeLevelStudentSpansMap.put(student, gradeLevelStudentSpans);
                                }
                                gradeLevelStudentSpans.add(span);
                            }
                        }
                    }
                }
            }
        } finally {
            if (students != null) {
                students.close();
            }
        }
        return mapSpans;
    }

    /**
     * Returns true if student is early graduated, otherwise false.
     *
     * @param span TNStudentEnrollmentSpan
     * @return true, if is early graduated
     */
    private boolean isEarlyGraduated(TNStudentEnrollmentSpan span) {
        boolean isEarlyGraduate = false;
        StudentEnrollment studentEnrollment = span.getEnrollmentForDate(m_periodEndDate, "W");
        if (studentEnrollment != null) {
            String enrCode = studentEnrollment.getEnrollmentCode();
            ReferenceCode refCode = m_referenceEnrollmentCodeMap.get(enrCode);
            if (refCode != null && "12".equals(refCode.getStateCode())) {
                isEarlyGraduate = true;
            }
        }
        return isEarlyGraduate;
    }

    /**
     * Load enrollment codes.
     */
    private void loadEnrollmentCodes() {
        ModelProperty prop = new ModelProperty(StudentEnrollment.class, StudentEnrollment.COL_ENROLLMENT_CODE,
                getBroker().getPersistenceKey());
        DataDictionaryField field = m_dictionary.findDataDictionaryField(prop.getFieldId());
        ReferenceTable referenceTable = field.getReferenceTable();
        m_referenceEnrollmentCodeMap = referenceTable.getCodeMap();
    }

    /**
     * Load gender codes.
     */
    private void loadGenderCodes() {
        ModelProperty prop =
                new ModelProperty(SisPerson.class, SisPerson.COL_GENDER_CODE, getBroker().getPersistenceKey());
        DataDictionaryField field = m_dictionary.findDataDictionaryField(prop.getFieldId());
        ReferenceTable referenceTable = field.getReferenceTable();
        m_referenceGenderCodeMap = referenceTable.getCodeMap();
    }

    /**
     * Check and prepare grade level.
     *
     * @param gradeCode ReferenceCode
     * @return Integer
     */
    private Integer prepareGradeLevel(ReferenceCode gradeCode) {
        if (gradeCode == null) {
            return null;
        }

        String gradeLevel = gradeCode.getStateCode();
        if (gradeLevel == null || gradeLevel.equals("")) {
            return null;
        }

        int grade = -1000;

        try {
            grade = Integer.parseInt(gradeLevel);
        } catch (NumberFormatException nfe) {
            // gradeLevel contains letters
            gradeLevel = gradeCode.getFieldA005();

            try {
                grade = Integer.parseInt(gradeLevel);
            } catch (NumberFormatException nfex) {
                // gradeLevel contains letters
                grade = -1000;
            }
        }

        Integer preparedGradeLevel = null;
        if (grade > -1000) {
            preparedGradeLevel = Integer.valueOf((grade));
        }

        return preparedGradeLevel;
    }

    /**
     * Returns true if span intersects with period, otherwise returns false.
     *
     * @param span TNStudentEnrollmentSpan
     * @return true, if successful
     */
    private boolean spanIntersectsWithPeriod(TNStudentEnrollmentSpan span) {
        boolean spanIntersects = false;

        PlainDate periodStartDate = m_periodHelper.getYearBeginDate();
        PlainDate periodEndDate = m_periodHelper.getDateEnd(span.getSchool().getOid());

        if (periodStartDate != null && periodEndDate != null) {
            if (!span.getFirstActiveEnrollment().getEnrollmentDate().after(periodEndDate) &&
                    (span.getFirstInactiveEnrollment() == null
                            || !span.getFirstInactiveEnrollment().getEnrollmentDate().before(periodStartDate))) {
                spanIntersects = true;
            }
        }

        return spanIntersects;
    }

    /**
     * Update accumulators.
     *
     * @param membershipFlags StudentMembershipFlags
     * @param accumulator RowCount
     * @param accumulatorNGradeLevel RowCount
     * @param gender String
     */
    private void updateAccumulators(StudentMembershipFlags membershipFlags,
                                    RowCount accumulator,
                                    RowCount accumulatorNGradeLevel,
                                    String gender) {
        if (STATE_CODE_GENDER_MALE.equals(gender)) {
            if (membershipFlags.isNetEnrolledNGradeLevelStudent()) {
                accumulatorNGradeLevel.incrementEnrolledStudentsMale();
                if (m_isAnnual) {
                    // N Grade Level students count twice
                    accumulator.incrementEnrolledStudentsMale();
                }
            } else if (membershipFlags.isNetEnrolledStudent()) {
                accumulator.incrementEnrolledStudentsMale();
            }
        } else if (STATE_CODE_GENDER_FEMALE.equals(gender)) {
            if (membershipFlags.isNetEnrolledNGradeLevelStudent()) {
                accumulatorNGradeLevel.incrementEnrolledStudentsFemale();
                if (m_isAnnual) {
                    // N Grade Level students count twice
                    accumulator.incrementEnrolledStudentsFemale();
                }
            } else if (membershipFlags.isNetEnrolledStudent()) {
                accumulator.incrementEnrolledStudentsFemale();
            }
        }

        if (membershipFlags.isEndOfPeriodMembershipNGradeLevelStudent()) {
            accumulatorNGradeLevel.incrementPeriodMembershipStudent();
        } else if (membershipFlags.isEndOfPeriodMembershipStudent()) {
            accumulator.incrementPeriodMembershipStudent();
        }
    }
}
