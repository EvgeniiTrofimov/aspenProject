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
package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.fl.FLStateReportData.SurveyPeriod;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import org.apache.ojb.broker.query.ReportQueryByCriteria;
import jersey.repackaged.com.google.common.collect.ImmutableSet;

/**
 * The Class FLScheduleHelper.
 */
public class FLScheduleHelper {

    /**
     * The Class MasterScheduleInfo.
     */
    public class MasterScheduleInfo {
        private static final int INCONSISTENT_PERIOD_NUMBER = 88;
        private static final String INCONSISTENT_PERIOD_NUMBER_CODE = "88";
        private static final int NUM_FTE_DATES = 11;

        // The number of days the section is scheduled on each day of the week
        private Boolean m_blendedLearningIndicator;
        private String m_classroomIdentificationNo;
        private String m_courseNumber;
        private String m_cteCompletionPointCode;
        private String m_cteProgramNumber;
        private Integer m_daysInTerm;
        private int[] m_daysOfWeekScheduled = null;
        private String m_dualEnrollmentCrsLocation;
        private String m_dualEnrollmentIndicator;
        private String m_ellInstructionalModel;
        private String m_facilityType;
        private ImmutableSet<PlainDate> m_fteDates;
        private Integer m_hrsInSurveyPeriod;
        private Integer m_minutesPerWeek;
        private String m_periodNumber;
        private String m_readingInterventionIndicator;
        private String m_schoolNumber;
        private MasterSchedule m_section;
        private String m_sectionNumber;
        private Boolean m_titleIIIFundedIndicator;
        private String m_virtualCourseLocation;
        private String m_virtualInstructionProvider;
        private String m_wdisDldIndicator;
        private String m_wdisHomelessAdultProgram;

        /**
         * Instantiates a new master schedule info.
         *
         * @param section MasterSchedule
         */
        public MasterScheduleInfo(MasterSchedule section) {
            m_section = section;
        }

        /**
         * Gets the homeless adult program indicator.
         *
         * @return String
         * @throws X2BaseException exception
         */
        public String getHomelessAdultProgramIndicator() throws X2BaseException {
            if (m_wdisHomelessAdultProgram == null) {
                DataDictionaryField field =
                        getFLData().translateAliasToDictionaryField(ALIAS_WDIS_HOMELESS_ADULT_PROGRAM_MST, false);
                if (field != null) {
                    m_wdisHomelessAdultProgram = (String) getFLData().getFieldValue(m_section, field);
                }
                if (StringUtils.isEmpty(m_wdisHomelessAdultProgram)) {
                    SchoolCourse sklCourse = m_section.getSchoolCourse();
                    if (sklCourse != null) {
                        Course course = sklCourse.getCourse();
                        if (course != null) {
                            field = getFLData()
                                    .translateAliasToDictionaryField(ALIAS_WDIS_HOMELESS_ADULT_PROGRAM_CRS, false);
                            if (field != null) {
                                m_wdisHomelessAdultProgram = (String) getFLData().getFieldValue(course, field);
                            }
                        }
                    }
                }
            }
            return m_wdisHomelessAdultProgram;
        }

        /**
         * Gets the blended learning indicator.
         *
         * @return Boolean
         * @throws X2BaseException exception
         */
        public Boolean getBlendedLearningIndicator() throws X2BaseException {
            if (m_blendedLearningIndicator == null) {
                boolean blendedLearningIndicator = false;

                DataDictionaryField field =
                        getFLData().translateAliasToDictionaryField(ALIAS_BLENDED_LEARNING_MST, false);
                if (field != null) {
                    Boolean fieldValue = (Boolean) getFLData().getFieldValue(m_section, field);
                    if (fieldValue != null) {
                        blendedLearningIndicator |= fieldValue.booleanValue();
                    }
                }

                if (!blendedLearningIndicator) {
                    SchoolCourse schoolCourse = m_section.getSchoolCourse();
                    if (schoolCourse != null) {
                        field = getFLData().translateAliasToDictionaryField(ALIAS_BLENDED_LEARNING_CSK, false);
                        if (field != null) {
                            Boolean fieldValue = (Boolean) getFLData().getFieldValue(schoolCourse, field);
                            if (fieldValue != null) {
                                blendedLearningIndicator |= fieldValue.booleanValue();
                            }
                        }
                        if (!blendedLearningIndicator) {
                            Course course = schoolCourse.getCourse();
                            if (course != null) {
                                field = getFLData().translateAliasToDictionaryField(ALIAS_BLENDED_LEARNING_CRS, false);
                                if (field != null) {
                                    Boolean fieldValue = (Boolean) getFLData().getFieldValue(course, field);
                                    if (fieldValue != null) {
                                        blendedLearningIndicator |= fieldValue.booleanValue();
                                    }
                                }
                            }
                        }
                    }
                }
                m_blendedLearningIndicator = Boolean.valueOf(blendedLearningIndicator);
            }
            return m_blendedLearningIndicator;
        }

        /**
         * Gets the classroom identification no.
         *
         * @return String
         * @throws X2BaseException exception
         */
        public String getClassroomIdentificationNo() throws X2BaseException {
            if (m_classroomIdentificationNo == null) {
                m_classroomIdentificationNo = "";
                if (m_section != null) {
                    SchoolRoom room = m_section.getPrimaryRoom();
                    if (room != null) {
                        DataDictionaryField field =
                                getFLData().translateAliasToDictionaryField(ALIAS_CLASSROOM_ID, false);
                        if (field != null) {
                            m_classroomIdentificationNo = (String) getFLData().getFieldValue(room, field);
                        }
                    }
                }
            }
            return m_classroomIdentificationNo;
        }

        /**
         * Gets the course number.
         *
         * @return String
         * @throws X2BaseException exception
         */
        public String getCourseNumber() throws X2BaseException {
            if (m_courseNumber == null) {
                SchoolCourse sklCourse = m_section.getSchoolCourse();
                if (sklCourse != null) {
                    Course course = sklCourse.getCourse();
                    if (course != null) {
                        DataDictionaryField field =
                                getFLData().translateAliasToDictionaryField(ALIAS_COURSE_NUMBER, false);
                        if (field != null) {
                            m_courseNumber = (String) getFLData().getFieldValue(course, field);
                        }
                    }
                }
                if (StringUtils.isEmpty(m_courseNumber)) {
                    m_courseNumber = "";
                }
            }
            return m_courseNumber;
        }

        /**
         * Gets the cte completion point code.
         *
         * @return String
         * @throws X2BaseException exception
         */
        public String getCteCompletionPointCode() throws X2BaseException {
            if (m_cteCompletionPointCode == null) {
                SchoolCourse sklCourse = m_section.getSchoolCourse();
                if (sklCourse != null) {
                    Course course = sklCourse.getCourse();
                    if (course != null) {
                        DataDictionaryField field =
                                getFLData().translateAliasToDictionaryField(ALIAS_CTE_COMPLETION_POINT_CODE, false);
                        if (field != null) {
                            m_cteCompletionPointCode = (String) getFLData().getFieldValue(course, field);
                        }
                    }
                }
                if (StringUtils.isEmpty(m_cteCompletionPointCode)) {
                    m_cteCompletionPointCode = "";
                }
            }
            return m_cteCompletionPointCode;
        }

        /**
         * Gets the cte program number.
         *
         * @return String
         * @throws X2BaseException exception
         */
        public String getCteProgramNumber() throws X2BaseException {
            if (m_cteProgramNumber == null) {
                SchoolCourse sklCourse = m_section.getSchoolCourse();
                if (sklCourse != null) {
                    Course course = sklCourse.getCourse();
                    if (course != null) {
                        DataDictionaryField field =
                                getFLData().translateAliasToDictionaryField(ALIAS_CTE_PROGRAM_CODE_CRS, false);
                        if (field != null) {
                            m_cteProgramNumber = (String) getFLData().getFieldValue(course, field);
                        }
                    }
                }
                if (StringUtils.isEmpty(m_cteProgramNumber)) {
                    m_cteProgramNumber = "";
                }
            }
            return m_cteProgramNumber;
        }

        /**
         * Gets the days per week.
         *
         * @return Integer
         * @throws X2BaseException exception
         */
        public Integer getDaysPerWeek() throws X2BaseException {
            initDaysOfWeekScheduled();
            int numDays = 0;
            if (!isVirtualCourse()) {
                for (int i = 0; i < m_daysOfWeekScheduled.length; ++i) {
                    if (m_daysOfWeekScheduled[i] > 0) {
                        ++numDays;
                    }
                }
            }
            return Integer.valueOf(numDays);
        }

        /**
         * Gets the days in term.
         *
         * @return Integer
         */
        public Integer getDaysInTerm() {
            if (m_daysInTerm == null) {
                int daysInTerm = 0;
                for (SisSchoolCalendarDate date : getSectionCalendarDates(m_section, getFLData().getSurveyPeriod())) {
                    Collection<SchedulePeriod> periods = getScheduleMatrixHelper().getSectionPeriods(m_section, date);
                    if (periods != null && !periods.isEmpty()) {
                        ++daysInTerm;
                    }
                }
                m_daysInTerm = Integer.valueOf(daysInTerm);
            }
            return m_daysInTerm;
        }

        /**
         * Gets the DLD indicator.
         *
         * @return String
         * @throws X2BaseException exception
         */
        public String getDLDIndicator() throws X2BaseException {
            if (m_wdisDldIndicator == null) {
                DataDictionaryField field =
                        getFLData().translateAliasToDictionaryField(ALIAS_WDIS_DLD_LEARNING_INDICATOR, false);
                if (field != null) {
                    m_wdisDldIndicator = (String) getFLData().getFieldValue(m_section, field);
                }
            }
            return m_wdisDldIndicator;
        }

        /**
         * Gets the dual enrollment crs location.
         *
         * @return String
         * @throws X2BaseException exception
         */
        public String getDualEnrollmentCrsLocation() throws X2BaseException {
            if (m_dualEnrollmentCrsLocation == null) {
                m_dualEnrollmentCrsLocation = "";
                DataDictionaryField field =
                        getFLData().translateAliasToDictionaryField(ALIAS_DUAL_ENROLLMENT_CRS_LOC_MST, false);
                if (field != null) {
                    m_dualEnrollmentCrsLocation = (String) getFLData().getFieldValue(m_section, field);
                }
            }
            return m_dualEnrollmentCrsLocation;
        }

        /**
         * Gets the dual enrollment indicator.
         *
         * @return String
         * @throws X2BaseException exception
         */
        public String getDualEnrollmentIndicator() throws X2BaseException {
            if (m_dualEnrollmentIndicator == null) {
                m_dualEnrollmentIndicator = "";
                DataDictionaryField field =
                        getFLData().translateAliasToDictionaryField(ALIAS_DUAL_ENROLLMENT_MST, false);
                if (field != null) {
                    m_dualEnrollmentIndicator = (String) getFLData().getFieldValue(m_section, field);
                }
            }
            return m_dualEnrollmentIndicator;
        }

        /**
         * Gets the ell instructional model.
         *
         * @return String
         * @throws X2BaseException exception
         */
        public String getEllInstructionalModel() throws X2BaseException {
            if (m_ellInstructionalModel == null) {
                m_ellInstructionalModel = "";
                DataDictionaryField field = getFLData().translateAliasToDictionaryField(ALIAS_ELL_MODEL_MST, false);
                if (field != null) {
                    m_ellInstructionalModel = (String) getFLData().getFieldValue(m_section, field);
                }
            }
            return m_ellInstructionalModel;
        }

        /**
         * Gets the facility type.
         *
         * @return String
         * @throws X2BaseException exception
         */
        public String getFacilityType() throws X2BaseException {
            if (m_facilityType == null) {
                m_facilityType = "";
                if (m_section != null) {
                    SchoolRoom room = m_section.getPrimaryRoom();
                    if (room != null) {
                        DataDictionaryField field =
                                getFLData().translateAliasToDictionaryField(ALIAS_FACILITY_TYPE_RMS, false);
                        if (field != null) {
                            m_facilityType = (String) getFLData().getFieldValue(room, field);
                        }
                    }
                    if (StringUtils.isEmpty(m_facilityType)) {
                        Schedule schedule = m_section.getSchedule();
                        if (schedule != null) {
                            SisSchool school = schedule.getSchool();
                            if (school != null) {
                                DataDictionaryField field =
                                        getFLData().translateAliasToDictionaryField(ALIAS_FACILITY_TYPE_SKL, false);
                                if (field != null) {
                                    m_facilityType = (String) getFLData().getFieldValue(school, field);
                                }
                            }
                        }
                    }
                }
            }
            return m_facilityType;
        }

        /**
         * Gets the fte dates.
         *
         * @return Immutable set
         */
        public ImmutableSet<PlainDate> getFteDates() {
            if (m_fteDates == null) {
                PlainDate lastDate = getFLData().getSurveyPeriod().getEndDate();
                TreeSet<PlainDate> dates = new TreeSet();
                TreeSet<SisSchoolCalendarDate> calendarDates = getSectionCalendarDates(m_section,
                        getFLData().getCurrentContext().getStartDate(), getFLData().getCurrentContext().getEndDate());
                Iterator<SisSchoolCalendarDate> iterator = calendarDates.descendingIterator();
                while (iterator.hasNext() && dates.size() < NUM_FTE_DATES) {
                    SisSchoolCalendarDate csd = iterator.next();
                    if (!lastDate.before(csd.getDate())) {
                        dates.add(csd.getDate());
                    }
                }
                m_fteDates = ImmutableSet.copyOf(dates);
            }
            return m_fteDates;
        }

        /**
         * This method returns the number of classroom meeting minutes
         * for the first week of the survey period or the complet survey period if an entire week is
         * not included.
         *
         * @return Integer
         * @throws X2BaseException exception
         */
        public Integer getMinutesPerWeek() throws X2BaseException {
            if (m_minutesPerWeek == null) {
                PlainDate lastDate = null;
                int totalMinutes = 0;
                if (!isVirtualCourse()) {
                    for (SisSchoolCalendarDate date : getSectionCalendarDates(m_section)) {
                        if (lastDate == null) {
                            Calendar calendar = Calendar.getInstance();
                            calendar.setTime(date.getDate());
                            calendar.add(Calendar.DATE, 6);
                            lastDate = new PlainDate(calendar.getTime());
                        }
                        if (!date.getDate().after(lastDate)) {
                            Collection<SchedulePeriod> periods =
                                    getScheduleMatrixHelper().getSectionPeriods(m_section, date);
                            if (periods != null && !periods.isEmpty()) {
                                ScheduleBell bell = date.getBellSchedule();
                                if (bell == null) {
                                    throw new X2RuntimeException(new UnsupportedOperationException(
                                            "No bell schedule found for "
                                                    + date.getSchoolCalendar().getSchool().getName()
                                                    + "-" +
                                                    date.getSchoolCalendar().getCalendarId() + " for date "
                                                    + date.getDate()));
                                }
                                for (SchedulePeriod period : periods) {
                                    totalMinutes += getPeriodTime(bell, period);
                                }
                            }
                        }
                    }
                }
                m_minutesPerWeek = Integer.valueOf(totalMinutes);
            }
            return m_minutesPerWeek;
        }

        /**
         * Gets the NCLB title III funded indicator.
         *
         * @return Boolean
         * @throws X2BaseException exception
         */
        public Boolean getNCLBTitleIIIFundedIndicator() throws X2BaseException {
            if (m_titleIIIFundedIndicator == null) {
                boolean titleIIIFundedIndicator = false;

                DataDictionaryField field =
                        getFLData().translateAliasToDictionaryField(ALIAS_TITLE_III_FUNDED_MST, false);
                if (field != null) {
                    Boolean fieldValue = (Boolean) getFLData().getFieldValue(m_section, field);
                    if (fieldValue != null) {
                        titleIIIFundedIndicator |= fieldValue.booleanValue();
                    }
                }

                if (!titleIIIFundedIndicator) {
                    SchoolCourse schoolCourse = m_section.getSchoolCourse();
                    if (schoolCourse != null) {
                        field = getFLData().translateAliasToDictionaryField(ALIAS_TITLE_III_FUNDED_CSK, false);
                        if (field != null) {
                            Boolean fieldValue = (Boolean) getFLData().getFieldValue(schoolCourse, field);
                            if (fieldValue != null) {
                                titleIIIFundedIndicator |= fieldValue.booleanValue();
                            }
                        }
                    }
                }

                if (!titleIIIFundedIndicator) {
                    Schedule schedule = m_section.getSchedule();
                    if (schedule != null) {
                        SisSchool school = schedule.getSchool();
                        if (school != null) {
                            field = getFLData().translateAliasToDictionaryField(ALIAS_TITLE_III_FUNDED_SKL, false);
                            if (field != null) {
                                Boolean fieldValue = (Boolean) getFLData().getFieldValue(school, field);
                                if (fieldValue != null) {
                                    titleIIIFundedIndicator |= fieldValue.booleanValue();
                                }
                            }
                        }
                    }
                }

                m_titleIIIFundedIndicator = Boolean.valueOf(titleIIIFundedIndicator);
            }
            return m_titleIIIFundedIndicator;
        }

        /**
         * Gets the period number.
         *
         * @return String
         */
        public String getPeriodNumber() {
            if (m_periodNumber == null) {
                m_periodNumber = "";
                boolean consistentPeriodNumber = true;
                for (ScheduleTermDate tmd : getScheduleTermDates(m_section.getScheduleTerm().getOid())) {
                    for (ScheduleDay day : getScheduleDays(m_section.getSchedule())) {
                        Set<SchedulePeriod> periods = getScheduleMatrixHelper().getSectionPeriods(m_section, tmd, day);
                        if (periods != null && !periods.isEmpty()) {
                            String periodNumber = getPeriodNumber(periods);
                            if (StringUtils.isEmpty(m_periodNumber)) {
                                m_periodNumber = periodNumber;
                            } else {
                                int icmp = periodNumber.compareTo(m_periodNumber);

                                if (icmp < 0) {
                                    consistentPeriodNumber = false;
                                    m_periodNumber = periodNumber;
                                } else if (icmp > 0) {
                                    consistentPeriodNumber = false;
                                }
                            }
                        }
                    }
                }
                if (!consistentPeriodNumber) {
                    m_periodNumber = m_periodNumber.substring(0, 2) + INCONSISTENT_PERIOD_NUMBER_CODE;
                }
            }
            return m_periodNumber;
        }

        /**
         * Gets the reading intervention indicator.
         *
         * @return String
         * @throws X2BaseException exception
         */
        public String getReadingInterventionIndicator() throws X2BaseException {
            if (m_readingInterventionIndicator == null) {
                m_readingInterventionIndicator = "N";
                DataDictionaryField field =
                        getFLData().translateAliasToDictionaryField(ALIAS_READING_INTERVENTION_MST, false);
                if (field != null) {
                    Boolean value = (Boolean) getFLData().getFieldValue(m_section, field);
                    if (value != null && value.booleanValue()) {
                        m_readingInterventionIndicator = "Y";
                    }
                }
            }
            return m_readingInterventionIndicator;
        }

        /**
         * Gets section hours in date range.
         *
         * @param startDate
         * @param endDate
         *
         * @return Integer
         */
        public Integer getSectionHoursInDateRange(PlainDate startDate, PlainDate endDate) {
            int minutesInDateRange = 0;
            for (SisSchoolCalendarDate date : getSectionCalendarDates(m_section, startDate, endDate)) {
                ScheduleBell bell = date.getBellSchedule();
                Collection<SchedulePeriod> periods = getScheduleMatrixHelper().getSectionPeriods(m_section, date);
                if (periods != null) {
                    for (SchedulePeriod period : periods) {
                        minutesInDateRange += getPeriodTime(bell, period);
                    }
                }
            }
            Integer hrsInDateRange = Integer.valueOf(minutesInDateRange / 60);
            return hrsInDateRange;
        }

        /**
         * Gets section hours in date range.
         *
         * @param date
         *
         * @return Integer
         */
        public int getSectionNumOfPeriodsOnDate(PlainDate date) {
            int numOfPeriods = 0;
            for (SisSchoolCalendarDate calendarDate : getSectionCalendarDates(m_section, date, date)) {
                Collection<SchedulePeriod> periods =
                        getScheduleMatrixHelper().getSectionPeriods(m_section, calendarDate);
                if (periods != null) {
                    numOfPeriods = periods.size();
                }
            }
            return numOfPeriods;
        }

        /**
         * Gets section hours in survey period.
         *
         * @return Integer
         */
        public Integer getSectionHoursInSurveyPeriod() {
            if (m_hrsInSurveyPeriod == null) {
                m_hrsInSurveyPeriod = getSectionHoursInDateRange(m_startDate, m_endDate);
            }
            return m_hrsInSurveyPeriod;
        }

        /**
         * Gets the school number.
         *
         * @return String
         * @throws X2BaseException exception
         */
        public String getSchoolNumber() throws X2BaseException {
            if (m_schoolNumber == null) {
                Schedule schedule = m_section.getSchedule();
                if (schedule != null) {
                    SisSchool school = schedule.getSchool();
                    if (school != null) {
                        DataDictionaryField field =
                                getFLData().translateAliasToDictionaryField(ALIAS_SCHOOL_NUMBER, false);
                        if (field != null) {
                            m_schoolNumber = (String) getFLData().getFieldValue(school, field);
                        }
                    }
                }
                if (StringUtils.isEmpty(m_schoolNumber)) {
                    m_schoolNumber = "";
                }
            }
            return m_schoolNumber;
        }

        /**
         * Gets the scheduled on day of week.
         *
         * @param dayOfWeek int
         * @return Boolean
         * @throws X2BaseException exception
         */
        public Boolean getScheduledOnDayOfWeek(int dayOfWeek) throws X2BaseException {
            initDaysOfWeekScheduled();
            return !isVirtualCourse() && m_daysOfWeekScheduled[dayOfWeek] > 0 ? Boolean.TRUE : Boolean.FALSE;
        }

        /**
         * Gets the section.
         *
         * @return Master schedule
         */
        public MasterSchedule getSection() {
            return m_section;
        }

        /**
         * Gets the section number.
         *
         * @return String
         * @throws X2BaseException exception
         */
        public String getSectionNumber() throws X2BaseException {
            if (m_sectionNumber == null) {
                if (m_section != null) {
                    DataDictionaryField field =
                            getFLData().translateAliasToDictionaryField(ALIAS_SECTION_NUMBER, false);
                    if (field != null) {
                        m_sectionNumber = (String) getFLData().getFieldValue(m_section, field);
                    }
                }
                if (StringUtils.isEmpty(m_sectionNumber)) {
                    m_sectionNumber = "";
                }
            }
            return m_sectionNumber;
        }

        /**
         * Gets the virtual course location.
         *
         * @return String
         * @throws X2BaseException exception
         */
        public String getVirtualCourseLocation() throws X2BaseException {
            if (m_virtualCourseLocation == null) {
                m_virtualCourseLocation = "Z";
                DataDictionaryField field =
                        getFLData().translateAliasToDictionaryField(ALIAS_VIRTUAL_COURSE_LOCATION, false);
                if (field != null) {
                    String value = (String) getFLData().getFieldValue(m_section, field);
                    if (!StringUtils.isEmpty(value)) {
                        m_virtualCourseLocation = value;
                    }
                }
            }
            return m_virtualCourseLocation;
        }

        /**
         * Gets the virtual instruction provider.
         *
         * @return String
         * @throws X2BaseException exception
         */
        public String getVirtualInstructionProvider() throws X2BaseException {
            if (m_virtualInstructionProvider == null) {
                m_virtualInstructionProvider = "ZZZ";
                DataDictionaryField field =
                        getFLData().translateAliasToDictionaryField(ALIAS_VIRTUAL_INSTRUCTION_PROVIDER, false);
                if (field != null) {
                    String value = (String) getFLData().getFieldValue(m_section, field);
                    if (!StringUtils.isEmpty(value)) {
                        m_virtualInstructionProvider = value;
                    }
                }
            }
            return m_virtualInstructionProvider;
        }

        /**
         * Checks if is virtual course.
         *
         * @return true, if is virtual course
         * @throws X2BaseException exception
         */
        public boolean isVirtualCourse() throws X2BaseException {
            return !"ZZZ".equals(getVirtualInstructionProvider());
        }

        /**
         * Gets the period number.
         *
         * @param periodSet Set<SchedulePeriod>
         * @return String
         */
        private String getPeriodNumber(Set<SchedulePeriod> periodSet) {
            int begin = -1, end = -1;
            Collection<SchedulePeriod> periods = periodSet;
            if (periods.size() > 1) {
                List<SchedulePeriod> periodList = new ArrayList(periodSet);
                Collections.sort(periodList, new Comparator<SchedulePeriod>() {
                    @Override
                    public int compare(SchedulePeriod o1, SchedulePeriod o2) {
                        return o1.getNumber() - o2.getNumber();
                    }
                });

                Iterator<SchedulePeriod> iterator = periodList.iterator();
                begin = iterator.next().getNumber();
                int previous = begin;
                while (iterator.hasNext()) {
                    int next = iterator.next().getNumber();
                    if (previous + 1 == next) {
                        end = next;
                        previous = next;
                    } else {
                        end = INCONSISTENT_PERIOD_NUMBER;
                    }
                }
            } else if (!periods.isEmpty()) {
                begin = end = periods.iterator().next().getNumber();
            }

            String periodNumber = null;
            if (begin >= 0 && begin < 100 && end >= 0 && end < 100) {
                periodNumber = String.format("%02d%02d", Integer.valueOf(begin), Integer.valueOf(end));
            }
            return periodNumber;
        }

        /**
         * Inits the days of week scheduled.
         */
        private void initDaysOfWeekScheduled() {
            if (m_daysOfWeekScheduled == null) {
                Calendar cal = Calendar.getInstance();
                m_daysOfWeekScheduled = new int[Calendar.SATURDAY + 1];
                for (SisSchoolCalendarDate date : getSectionCalendarDates(m_section)) {
                    Collection<SchedulePeriod> periods = getScheduleMatrixHelper().getSectionPeriods(m_section, date);
                    if (periods != null && !periods.isEmpty()) {
                        cal.setTime(date.getDate());
                        ++m_daysOfWeekScheduled[cal.get(Calendar.DAY_OF_WEEK)];
                    }
                }
            }
        }
    }

    /**
     * The Class ScheduleMatrixHelper.
     */
    public class ScheduleMatrixHelper {
        private Set<String> m_initializedSchedules = new HashSet();
        private Map<String, Set<SchedulePeriod>> m_sectionTermDayPeriods = new HashMap();
        private Map<String, Collection<ScheduleTerm>> m_sectionTermsMap = new HashMap();

        /**
         * Get the schedule periods for a particular section on a particular day.
         *
         * @param section MasterSchedule
         * @param csd SisSchoolCalendarDate
         * @return Collection<SchedulePeriod>
         */
        public Set<SchedulePeriod> getSectionPeriods(MasterSchedule section, SisSchoolCalendarDate csd) {
            initializeSection(section);

            Set<SchedulePeriod> periods = null;
            Collection<ScheduleTerm> terms = m_sectionTermsMap.get(section.getOid());
            if (terms != null) {
                for (ScheduleTerm trm : terms) {
                    for (ScheduleTermDate tmd : getScheduleTermDates(trm.getOid())) {
                        if (!csd.getDate().before(tmd.getStartDate()) && !csd.getDate().after(tmd.getEndDate())) {
                            ScheduleDay day = getScheduleDay(section.getSchedule(), csd.getScheduleDayNumber());
                            if (day == null) {
                                throw new X2RuntimeException(new UnsupportedOperationException(
                                        "No schedule day found for " + csd.getSchoolCalendar().getSchool().getName()
                                                + "-" +
                                                csd.getSchoolCalendar().getCalendarId() + " for date "
                                                + csd.getDate()));
                            }
                            periods = getSectionPeriods(section, tmd, day);
                            break;
                        }
                    }
                    if (periods != null) {
                        break;
                    }
                }
            }
            return periods;
        }

        /**
         * Gets the section periods.
         *
         * @param section MasterSchedule
         * @param tmd ScheduleTermDate
         * @param day ScheduleDay
         * @return Sets the
         */
        public Set<SchedulePeriod> getSectionPeriods(MasterSchedule section, ScheduleTermDate tmd, ScheduleDay day) {
            initializeSection(section);

            String key = section.getOid() + tmd.getScheduleTermOid() + day.getOid();
            Set<SchedulePeriod> periods = m_sectionTermDayPeriods.get(key);

            return periods;
        }

        /**
         * Initialize section.
         *
         * @param section MasterSchedule
         */
        private void initializeSection(MasterSchedule section) {
            if (!m_initializedSchedules.contains(section.getSchedule().getOid())) {
                m_initializedSchedules.add(section.getSchedule().getOid());

                // Key is mst, trm, day oids
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(
                        MasterTerm.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER + MasterSchedule.COL_SCHEDULE_OID,
                        section.getSchedule().getOid());

                String[] columns = new String[] {MasterTerm.COL_MASTER_SCHEDULE_OID,
                        MasterTerm.COL_SCHEDULE_TERM_OID,
                        MasterTerm.REL_MASTER_MATRICES + ModelProperty.PATH_DELIMITER +
                                MasterScheduleMatrix.REL_SCHEDULE_MATRIX + ModelProperty.PATH_DELIMITER +
                                ScheduleMatrix.COL_SCHEDULE_DAY_OID,
                        MasterTerm.REL_MASTER_MATRICES + ModelProperty.PATH_DELIMITER +
                                MasterScheduleMatrix.REL_SCHEDULE_MATRIX + ModelProperty.PATH_DELIMITER +
                                ScheduleMatrix.COL_SCHEDULE_PERIOD_OID};

                ReportQueryByCriteria query = new ReportQueryByCriteria(MasterTerm.class, columns, criteria);
                query.addOrderByAscending(MasterTerm.COL_MASTER_SCHEDULE_OID);
                query.addOrderByAscending(MasterTerm.COL_SCHEDULE_TERM_OID);
                query.addOrderByAscending(MasterTerm.REL_MASTER_MATRICES + ModelProperty.PATH_DELIMITER +
                        MasterScheduleMatrix.REL_SCHEDULE_MATRIX + ModelProperty.PATH_DELIMITER +
                        ScheduleMatrix.COL_SCHEDULE_DAY_OID);

                ReportQueryIterator iterator = getFLData().getBroker().getReportQueryIteratorByQuery(query);

                try {
                    while (iterator.hasNext()) {
                        Object[] row = (Object[]) iterator.next();
                        String mstOid = (String) row[0];
                        String trmOid = (String) row[1];
                        String dayOid = (String) row[2];
                        String perOid = (String) row[3];
                        String key = mstOid + trmOid + dayOid;

                        Collection<ScheduleTerm> terms = m_sectionTermsMap.get(mstOid);
                        if (terms == null) {
                            terms = new HashSet();
                            m_sectionTermsMap.put(mstOid, terms);
                        }
                        ScheduleTerm term = getScheduleTerm(trmOid);
                        if (term != null) {
                            terms.add(term);
                        }
                        Set<SchedulePeriod> periods = m_sectionTermDayPeriods.get(key);
                        if (periods == null) {
                            periods = new HashSet<SchedulePeriod>();
                            m_sectionTermDayPeriods.put(key, periods);
                        }
                        periods.add(getSchedulePeriod(perOid));
                    }
                } finally {
                    iterator.close();
                }
            }
        }

    }

    protected static final String ALIAS_BLENDED_LEARNING_CRS = "all-crs-BlendedLearningIndicator";
    protected static final String ALIAS_BLENDED_LEARNING_CSK = "all-csk-BlendedLearningIndicator";
    protected static final String ALIAS_BLENDED_LEARNING_MST = "all-mst-BlendedLearningIndicator";
    protected static final String ALIAS_CLASSROOM_ID = "all-rms-ClassroomIdentificationNo";
    protected static final String ALIAS_COURSE_NUMBER = "all-crs-StateId";
    protected static final String ALIAS_CTE_COMPLETION_POINT_CODE = "all-crs-CTECompletionPointCode";
    protected static final String ALIAS_CTE_EXCEPTIONAL_STUDENT_COURSE_SETTING =
            "all-crs-CTEExceptionalStudentCourseSetting";
    protected static final String ALIAS_CTE_PROGRAM_CODE_CRS = "all-crs-CTEProgramCode";
    protected static final String ALIAS_DUAL_ENROLLMENT_MST = "all-mst-DualEnrollmentIndicator";
    protected static final String ALIAS_DUAL_ENROLLMENT_CRS_LOC_MST = "all-mst-DualEnrollmentCrsLoc";
    protected static final String ALIAS_ELL_MODEL_MST = "all-mst-ELLInstructionalModel";
    protected static final String ALIAS_FACILITY_TYPE_RMS = "all-rms-FacilityType";
    protected static final String ALIAS_FACILITY_TYPE_SKL = "all-skl-FacilityType";
    protected static final String ALIAS_READING_INTERVENTION_MST = "all-mst-ReadingIntervention";
    protected static final String ALIAS_SCHOOL_NUMBER = "all-skl-StateId";
    protected static final String ALIAS_SECTION_NUMBER = "all-mst-StateId";
    protected static final String ALIAS_TITLE_III_FUNDED_CSK = "all-csk-NCLBTitleIIIFunded";
    protected static final String ALIAS_TITLE_III_FUNDED_MST = "all-mst-NCLBTitleIIIFunded";
    protected static final String ALIAS_TITLE_III_FUNDED_SKL = "all-skl-NCLBTitleIIIFunded";
    protected static final String ALIAS_VIRTUAL_COURSE_LOCATION = "all-mst-VirtualCourseLocation";
    protected static final String ALIAS_VIRTUAL_INSTRUCTION_PROVIDER = "all-mst-VirtualInstructionProvider";
    protected static final String ALIAS_WDIS_DLD_LEARNING_INDICATOR = "all-mst-WdisDLDIndicator";
    protected static final String ALIAS_WDIS_HOMELESS_ADULT_PROGRAM_CRS = "all-crs-WdisHomelessIndicator";
    protected static final String ALIAS_WDIS_HOMELESS_ADULT_PROGRAM_MST = "all-mst-WdisHomelessIndicator";

    private Map<String, Collection<ScheduleBellPeriod>> m_bellPeriods = new HashMap();
    private FLStateReportData m_data;
    private PlainDate m_endDate;
    private Set<String> m_loadedScheduleOids = new HashSet();
    private Map<String, List<ScheduleDay>> m_scheduleDays = new HashMap();
    private ScheduleMatrixHelper m_scheduleMatrixHelper;
    private ScheduleManager m_scheduleMgr;
    private Map<String, SchedulePeriod> m_schedulePeriods = new HashMap();
    private Map<String, Collection<ScheduleTermDate>> m_scheduleTermDates = new HashMap();
    private Map<String, ScheduleTerm> m_scheduleTerms = new HashMap();
    private Map<String, Map<String, TreeSet<SisSchoolCalendarDate>>> m_sectionDates = new HashMap();
    private Map<String, MasterScheduleInfo> m_sectionInfoMap = new HashMap();
    private Map<String, MasterSchedule> m_sectionMap;
    private Map<String, String> m_sectionToCalendarCode = new HashMap();
    private PlainDate m_startDate;
    private Map<KeyValuePair<String, String>, Collection<ScheduleTermDate>> m_stateTermCodeDates = new HashMap();

    /**
     * Instantiates a new FL schedule helper.
     *
     * @param data FLStateReportData
     * @param startDate PlainDate
     * @param endDate PlainDate
     */
    public FLScheduleHelper(FLStateReportData data, PlainDate startDate, PlainDate endDate) {
        m_startDate = startDate;
        m_endDate = endDate;
        m_data = data;

        initialize();
    }

    /**
     * Gets the master schedule.
     *
     * @param oid String
     * @return Master schedule
     */
    public MasterSchedule getMasterSchedule(String oid) {
        MasterSchedule section = m_sectionMap.get(oid);
        if (section == null && m_data.isSchoolContext()) {
            // attempt to load other schedules for secondary schools if running with school context
            MasterSchedule bean = (MasterSchedule) m_data.getBroker().getBeanByOid(MasterSchedule.class, oid);
            if (bean != null) {
                String scheduleOid = bean.getScheduleOid();
                if (!StringUtils.isEmpty(scheduleOid) && !m_loadedScheduleOids.contains(scheduleOid)) {
                    m_loadedScheduleOids.add(scheduleOid);

                    X2Criteria sectionCriteria = getSectionCriteria();
                    sectionCriteria.addEqualTo(MasterSchedule.COL_SCHEDULE_OID, scheduleOid);
                    BeanQuery query = new BeanQuery(MasterSchedule.class, sectionCriteria);
                    Map<String, MasterSchedule> sectionMap =
                            m_data.getBroker().getMapByQuery(query, X2BaseBean.COL_OID, 1024);
                    m_sectionMap.putAll(sectionMap);
                }
                section = m_sectionMap.get(oid);
            }
        }
        return section;
    }

    /**
     * Gets the master schedule info.
     *
     * @param oid String
     * @return Master schedule info
     */
    public MasterScheduleInfo getMasterScheduleInfo(String oid) {
        MasterScheduleInfo sectionInfo = m_sectionInfoMap.get(oid);
        if (sectionInfo == null) {
            MasterSchedule section = getMasterSchedule(oid);
            sectionInfo = new MasterScheduleInfo(section);
            m_sectionInfoMap.put(oid, sectionInfo);
        }
        return sectionInfo;
    }

    /**
     * return sorted list of schedule days for the schedule.
     *
     * @param sch Schedule
     * @return List
     */
    public List<ScheduleDay> getScheduleDays(Schedule sch) {
        List<ScheduleDay> items = m_scheduleDays.get(sch.getOid());
        if (items == null) {
            Collection<ScheduleDay> days = sch.getScheduleDays();
            items = new ArrayList(days.size());
            items.addAll(days);
            Collections.sort(items, new Comparator<ScheduleDay>() {

                @Override
                public int compare(ScheduleDay o1, ScheduleDay o2) {
                    return o1.getNumber() - o2.getNumber();
                }
            });
            m_scheduleDays.put(sch.getOid(), items);
        }
        return items;
    }

    /**
     * Get the schedule term dates.
     *
     * @param oid String
     * @return Collection<ScheduleTermDate>
     */
    public Collection<ScheduleTermDate> getScheduleTermDates(String oid) {
        Collection<ScheduleTermDate> dates = null;
        if (m_scheduleTermDates.containsKey(oid)) {
            dates = m_scheduleTermDates.get(oid);
        } else {
            ScheduleTerm term = getScheduleTerm(oid);
            dates = term.getScheduleTermDates();
            m_scheduleTermDates.put(oid, dates);
        }
        return dates;
    }

    /**
     * Gets the section criteria.
     *
     * @return X 2 criteria
     */
    X2Criteria getSectionCriteria() {
        X2Criteria sectionCriteria = new X2Criteria();
        sectionCriteria.addEqualTo(MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + ModelProperty.PATH_DELIMITER +
                SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, m_data.getCurrentContext().getOid());

        // Add schedule term criteria based on date range specified
        if (m_startDate != null) {
            sectionCriteria.addGreaterOrEqualThan(MasterSchedule.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE_TERM_DATES + ModelProperty.PATH_DELIMITER +
                    ScheduleTermDate.COL_END_DATE, m_startDate);
        }
        if (m_endDate != null) {
            sectionCriteria.addLessOrEqualThan(MasterSchedule.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE_TERM_DATES + ModelProperty.PATH_DELIMITER +
                    ScheduleTermDate.COL_START_DATE, m_endDate);
        }
        return sectionCriteria;
    }

    /**
     * Gets the FL data.
     *
     * @return FL state report data
     */
    protected FLStateReportData getFLData() {
        return m_data;
    }

    /**
     * Calculate the duration in minutes for a particular period.
     *
     * @param bell ScheduleBell
     * @param per SchedulePeriod
     * @return int
     */
    protected int getPeriodTime(ScheduleBell bell, SchedulePeriod per) {
        if (bell != null && per != null) {
            Collection<ScheduleBellPeriod> bellPeriods = m_bellPeriods.get(bell.getOid());
            if (bellPeriods == null) {
                bellPeriods = bell.getScheduleBellPeriods();
                m_bellPeriods.put(bell.getOid(), bellPeriods);
            }
            if (bellPeriods != null) {
                for (ScheduleBellPeriod bpe : bellPeriods) {
                    if (bpe.getSchedulePeriodOid() != null && bpe.getId().equals(per.getId()) &&
                            bpe.getStartTime() != null && bpe.getEndTime() != null) {
                        return (int) (bpe.getEndTime().getTimeInMinutes() - bpe.getStartTime().getTimeInMinutes());
                    }
                }
            }
        }
        return 0;
    }

    /**
     * Get the ScheduleDay for a schedule and schedule day number.
     *
     * @param sch Schedule
     * @param dayNumber int
     * @return String
     */
    protected ScheduleDay getScheduleDay(Schedule sch, int dayNumber) {
        List<ScheduleDay> days = getScheduleDays(sch);
        for (ScheduleDay day : days) {
            if (day.getNumber() == dayNumber) {
                return day;
            }
        }
        return null;
    }

    /**
     * Gets the schedule matrix helper.
     *
     * @return Schedule matrix helper
     */
    protected ScheduleMatrixHelper getScheduleMatrixHelper() {
        if (m_scheduleMatrixHelper == null) {
            m_scheduleMatrixHelper = new ScheduleMatrixHelper();
        }
        return m_scheduleMatrixHelper;
    }

    /**
     * Get the SchedulePeriod for a period oid.
     *
     * @param perOid String
     * @return SchedulePeriod
     */
    protected SchedulePeriod getSchedulePeriod(String perOid) {
        SchedulePeriod period = m_schedulePeriods.get(perOid);
        if (!m_schedulePeriods.containsKey(perOid)) {
            period = (SchedulePeriod) m_data.getBroker().getBeanByOid(SchedulePeriod.class, perOid);
            m_schedulePeriods.put(perOid, period);
        }
        return period;
    }

    /**
     * Get the schedule term.
     *
     * @param oid String
     * @return Schedule term
     */
    protected ScheduleTerm getScheduleTerm(String oid) {
        ScheduleTerm term = null;
        if (m_scheduleTerms.containsKey(oid)) {
            term = m_scheduleTerms.get(oid);
        } else {
            term = (ScheduleTerm) m_data.getBroker().getBeanByOid(ScheduleTerm.class, oid);
            m_scheduleTerms.put(oid, term);
        }
        return term;
    }

    /**
     * Retrieve the most common calendar code for a section.
     *
     * @param section MasterSchedule
     * @return String
     */
    protected String getSectionCalendarCode(MasterSchedule section) {
        String calendarCode;
        if (m_sectionToCalendarCode.containsKey(section.getOid())) {
            calendarCode = m_sectionToCalendarCode.get(section.getOid());
        } else {
            if (m_scheduleMgr == null) {
                m_scheduleMgr = new ScheduleManager(m_data.getBroker());
            }
            calendarCode = m_scheduleMgr.getMostCommonCalendar(section.getSchedule(), null);
        }
        return calendarCode;
    }

    /**
     * Get a collection of in session calendar dates for a particular section based on the schedule
     * term.
     * This collection may include dates when this individual section does not meet if the section
     * is not scheduled for all schedule days.
     * The dates are returned in ascending order by csdDate.
     *
     * @param section MasterSchedule
     * @return Collection<SisSchoolCalendarDate>
     */
    protected TreeSet<SisSchoolCalendarDate> getSectionCalendarDates(MasterSchedule section) {
        return getSectionCalendarDates(section, m_startDate, m_endDate);
    }

    /**
     * Gets the section calendar dates.
     *
     * @param section MasterSchedule
     * @param surveyPeriod SurveyPeriod
     * @return Tree set
     */
    protected TreeSet<SisSchoolCalendarDate> getSectionCalendarDates(MasterSchedule section,
                                                                     SurveyPeriod surveyPeriod) {
        TreeSet<SisSchoolCalendarDate> values = null;
        String stateTermCode = surveyPeriod.getFLTermCode();
        if (!StringUtils.isEmpty(stateTermCode)) {
            Collection<ScheduleTermDate> termDates = lookupStateTermCodeDates(section.getSchedule(), stateTermCode);
            if (termDates.isEmpty()) {
                return new TreeSet();
            } else if (termDates.size() == 1) {
                ScheduleTermDate termDate = termDates.iterator().next();
                values = getSectionCalendarDates(section, termDate.getStartDate(), termDate.getEndDate());
            } else {
                values = new TreeSet<SisSchoolCalendarDate>(
                        getFLData().getSchoolCalendarHelper().getSchoolCalendarDatesComparator());
                for (ScheduleTermDate termDate : termDates) {
                    values.addAll(getSectionCalendarDates(section, termDate.getStartDate(), termDate.getEndDate()));
                }
            }
        }
        return values;
    }

    /**
     * Gets the section calendar dates.
     *
     * @param section MasterSchedule
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return Tree set
     */
    protected TreeSet<SisSchoolCalendarDate> getSectionCalendarDates(MasterSchedule section,
                                                                     PlainDate startDate,
                                                                     PlainDate endDate) {
        String calendarCode = getSectionCalendarCode(section);

        String key = section.getSchedule().getDistrictContextOid() + section.getSchedule().getSchoolOid() + calendarCode
                + startDate + endDate;
        Map<String, TreeSet<SisSchoolCalendarDate>> termDates = m_sectionDates.get(key);
        if (termDates == null) {
            termDates = new HashMap<String, TreeSet<SisSchoolCalendarDate>>();
            m_sectionDates.put(key, termDates);

            X2Criteria criteriaDates = new X2Criteria();
            criteriaDates.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                    SchoolCalendar.COL_DISTRICT_CONTEXT_OID, section.getSchedule().getDistrictContextOid());
            criteriaDates.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                    SchoolCalendar.COL_SCHOOL_OID, section.getSchedule().getSchoolOid());
            criteriaDates.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                    SchoolCalendar.COL_CALENDAR_ID, calendarCode);
            criteriaDates.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);

            if (startDate != null) {
                criteriaDates.addGreaterOrEqualThan(SisSchoolCalendarDate.COL_DATE, startDate);
            }
            if (endDate != null) {
                criteriaDates.addLessOrEqualThan(SisSchoolCalendarDate.COL_DATE, endDate);
            }
            BeanQuery queryDates = new BeanQuery(SisSchoolCalendarDate.class, criteriaDates);
            queryDates.addOrderBy(SisSchoolCalendarDate.COL_DATE, true);

            Collection<SisSchoolCalendarDate> dates = m_data.getBroker().getCollectionByQuery(queryDates);

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER +
                    ScheduleTerm.COL_SCHEDULE_OID, section.getScheduleOid());

            BeanQuery query = new BeanQuery(ScheduleTermDate.class, criteria);
            Map<String, Collection<ScheduleTermDate>> terms = m_data.getBroker().getGroupedCollectionByQuery(query,
                    ScheduleTermDate.COL_SCHEDULE_TERM_OID,
                    32);

            for (String termOid : terms.keySet()) {
                TreeSet<SisSchoolCalendarDate> csdSet = new TreeSet<SisSchoolCalendarDate>(
                        getFLData().getSchoolCalendarHelper().getSchoolCalendarDatesComparator());
                for (ScheduleTermDate tmd : terms.get(termOid)) {
                    for (SisSchoolCalendarDate csd : dates) {

                        if (tmd.getStartDate() != null && tmd.getEndDate() != null &&
                                !csd.getDate().before(tmd.getStartDate()) && !csd.getDate().after(tmd.getEndDate())) {
                            csdSet.add(csd);
                        }
                    }
                }
                termDates.put(termOid, csdSet);
            }
        }
        return termDates.get(section.getScheduleTermOid());
    }

    /**
     * Need to load all current sections to allow reporting of secondary schools.
     */
    private void initialize() {
        X2Criteria sectionCriteria = getSectionCriteria();

        if (m_data.isSchoolContext()) {
            sectionCriteria.addEqualTo(MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + ModelProperty.PATH_DELIMITER +
                    SchoolScheduleContext.COL_SCHOOL_OID, m_data.getSchool().getOid());
        }

        BeanQuery query = new BeanQuery(MasterSchedule.class, sectionCriteria);
        m_sectionMap = m_data.getBroker().getMapByQuery(query, X2BaseBean.COL_OID, 1024);
    }

    /**
     * Lookup state term code dates.
     *
     * @param schedule Schedule
     * @param stateTermCode String
     * @return Collection
     */
    private Collection<ScheduleTermDate> lookupStateTermCodeDates(Schedule schedule, String stateTermCode) {
        KeyValuePair<String, String> key = new KeyValuePair(schedule.getOid(), stateTermCode);
        Collection<ScheduleTermDate> values = m_stateTermCodeDates.get(key);
        if (values == null) {
            for (ScheduleTerm term : schedule.getScheduleTerms()) {
                String stateCode =
                        getFLData().lookupStateValue(ScheduleTerm.class, ScheduleTerm.COL_CODE, term.getCode());
                if (stateTermCode.equals(stateCode)) {
                    values = term.getScheduleTermDates();
                    break;
                }
            }
            if (values == null) {
                values = new LinkedList();
            }
            m_stateTermCodeDates.put(key, values);
        }
        return values;
    }

}

