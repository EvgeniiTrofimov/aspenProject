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

package com.x2dev.procedures.statereporting.tn;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.procedures.statereporting.tn.TNClassSectionScheduleData.TNClassSectionScheduleEntity.ValuesHelper;
import com.x2dev.procedures.statereporting.tn.TNClassSectionScheduleData.TNClassSectionScheduleEntity.ValuesKey;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentScheduleSpan;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Class for Class Section Schedule export
 *
 * NOTE: Most of the codes to calculate the student schedule minutes are copied to
 * VocationalClassReportData.java. Any change there or here might need to be
 * applied to both files.
 *
 */
public class TNClassSectionScheduleData extends TNStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Entity class for Class Section Schedule export.
     *
     */
    public static class TNClassSectionScheduleEntity extends TNStateReportEntity {

        /**
         * Helper class to keep calculated values on entity.
         *
         */
        class ValuesHelper {
            private int[] m_countDays = new int[7];
            private Map<PlainDate, int[]> m_dateScheduleMinutesMap;
            private int[] m_idPeriod = new int[7];
            private List<PlainDate> m_listMissedDates;
            private int[] m_minPeriod = new int[7];
            private int[] m_totalMinutes = new int[7];

            /**
             * Original Constructor.
             */
            public ValuesHelper() {
                m_dateScheduleMinutesMap = new HashMap<PlainDate, int[]>();
                m_listMissedDates = new LinkedList();
            }

            /**
             * Instantiates a new values helper.
             *
             * @param daysInSession Collection<PlainDate>
             */
            public ValuesHelper(Collection<PlainDate> daysInSession) {
                this();
                m_dateScheduleMinutesMap = new HashMap<PlainDate, int[]>();

                for (PlainDate tempDate : daysInSession) {
                    m_dateScheduleMinutesMap.put(tempDate, new int[12]);
                }
            }

            /**
             * Adds the minutes.
             *
             * @param index int
             * @param time int
             */
            public void addMinutes(int index, int time) {
                m_totalMinutes[index] += time;
            }

            /**
             * Adds the missed date.
             *
             * @param date PlainDate
             */
            public void addMissedDate(PlainDate date) {
                m_listMissedDates.add(date);
            }

            /**
             * Adds the total minutes.
             *
             * @param dateTotalMinutes Map<PlainDate,Integer>
             * @return Map
             */
            public Map<PlainDate, Integer> addTotalMinutes(Map<PlainDate, Integer> dateTotalMinutes) {
                if (dateTotalMinutes == null) {
                    dateTotalMinutes = new HashMap();
                }
                for (Entry<PlainDate, int[]> entry : m_dateScheduleMinutesMap.entrySet()) {
                    int dateMinutes = 0;
                    int[] minutesIndex = entry.getValue();
                    if (minutesIndex != null) {
                        for (int i = 0; i < minutesIndex.length; i++) {
                            dateMinutes = dateMinutes + minutesIndex[i];
                        }
                    }
                    if (dateMinutes > 0) {
                        Integer value = dateTotalMinutes.get(entry.getKey());
                        if (value == null) {
                            value = Integer.valueOf(dateMinutes);
                        } else {
                            value = Integer.valueOf(value.intValue() + dateMinutes);
                        }
                        dateTotalMinutes.put(entry.getKey(), value);
                    }
                }
                return dateTotalMinutes;
            }

            /**
             * Gets the count days.
             *
             * @param index int
             * @return int
             */
            public int getCountDays(int index) {
                return m_countDays[index];
            }

            /**
             * Gets the count days as string.
             *
             * @return String
             */
            public String getCountDaysAsString() {
                return intArrayToString(m_countDays);
            }

            /**
             * Gets the min period.
             *
             * @param index int
             * @return int
             */
            public int getMinPeriod(int index) {
                return m_minPeriod[index];
            }

            /**
             * Gets the min period as string.
             *
             * @return String
             */
            public String getMinPeriodAsString() {
                return intArrayToString(m_minPeriod);
            }

            /**
             * Gets the minutes.
             *
             * @param index int
             * @return int
             */
            public int getMinutes(int index) {
                return m_totalMinutes[index];
            }

            /**
             * Gets the minutes as string.
             *
             * @return String
             */
            public String getMinutesAsString() {
                return intArrayToString(m_totalMinutes);
            }

            /**
             * Gets the missed dates as string.
             *
             * @return String
             */
            public String getMissedDatesAsString() {
                return m_listMissedDates.toString();
            }

            /**
             * Gets the period id.
             *
             * @param index int
             * @return int
             */
            public int getPeriodId(int index) {
                return m_idPeriod[index];
            }

            /**
             * Gets the total minutes.
             *
             * @param date PlainDate
             * @return int
             */
            public int getTotalMinutes(PlainDate date) {
                int totalMinutes = 0;
                int[] minutesIndex = m_dateScheduleMinutesMap.get(date);
                if (minutesIndex != null) {
                    for (int i = 0; i < minutesIndex.length; i++) {
                        totalMinutes = totalMinutes + minutesIndex[i];
                    }
                }
                return totalMinutes;
            }

            /**
             * Increment daily time.
             *
             * @param date PlainDate
             * @param index int
             * @param time int
             */
            public void incrementDailyTime(PlainDate date, int index, int time) {
                if (m_dateScheduleMinutesMap != null) {
                    m_dateScheduleMinutesMap.get(date)[index] += time;
                }
            }

            /**
             * Increment count days.
             *
             * @param index int
             */
            public void incrementCountDays(int index) {
                m_countDays[index]++;
            }

            /**
             * Sets the min period.
             *
             * @param index int
             * @param number int
             */
            public void setMinPeriod(int index, int number) {
                m_minPeriod[index] = number;
            }

            /**
             * Sets the period id.
             *
             * @param index int
             * @param id int
             */
            public void setPeriodId(int index, int id) {
                m_idPeriod[index] = id;
            }

            /**
             * Convert the Array of int to String.
             *
             * @param arr int[]
             * @return String
             */
            private String intArrayToString(int[] arr) {
                StringBuilder output = new StringBuilder();
                output.append("[");
                int i = 0;
                for (i = 0; i < arr.length - 1; ++i) {
                    output.append(arr[i]);
                    output.append(",");
                }
                output.append(arr[i]);
                output.append("]");
                return output.toString();
            }
        }

        /**
         * Helper class to keep key for the map of calculated values.
         *
         */
        class ValuesKey implements Comparable<ValuesKey> {
            private String m_schoolOid;
            private String m_courseView;

            /**
             * Instantiates a new values key.
             *
             * @param mstOid String
             * @param courseView String
             */
            public ValuesKey(String mstOid, String courseView) {
                m_schoolOid = mstOid;
                m_courseView = courseView;
            }

            /**
             * Gets the school oid.
             *
             * @return the m_sectionOid
             */
            public String getSchoolOid() {
                return m_schoolOid;
            }

            /**
             * Gets the course view.
             *
             * @return the m_courseView
             */
            public String getCourseView() {
                return m_courseView;
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

                return compareTo((ValuesKey) obj) == 0 ? true : false;
            }

            /**
             * Hash code.
             *
             * @return int
             * @see java.lang.Object#hashCode()
             */
            @Override
            public int hashCode() {
                return m_schoolOid.hashCode() + m_courseView.hashCode();
            }

            /**
             * Compare to.
             *
             * @param o ValuesKey
             * @return int
             * @see java.lang.Comparable#compareTo(java.lang.Object)
             */
            @Override
            public int compareTo(ValuesKey o) {
                int result = m_schoolOid.compareTo(o.getSchoolOid());
                if (result == 0) {
                    result = m_courseView.compareTo(o.getCourseView());
                }
                return result;
            }
        }

        /**
         * Entity instance variables.
         */
        TNClassSectionScheduleData m_exportData;
        boolean m_isOriginalExport;
        Map<ValuesKey, ValuesHelper> m_valuesMap;

        private List<MasterSchedule> m_sections;

        /**
         * Original Constructor.
         */
        public TNClassSectionScheduleEntity() {
            m_isOriginalExport = true;
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Custom constructor to calculate number of minutes in student schedule (for ADM and ADA
         * calculation in director membership reports).
         *
         * @param data StateReportData
         * @param scheduleSpans List<TNStudentScheduleSpan>
         * @param daysInSession Collection<PlainDate>
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @throws X2BaseException exception
         */
        public TNClassSectionScheduleEntity(StateReportData data, List<TNStudentScheduleSpan> scheduleSpans,
                Collection<PlainDate> daysInSession, PlainDate startDate, PlainDate endDate)
                throws X2BaseException {
            m_exportData = (TNClassSectionScheduleData) data;
            m_isOriginalExport = false;

            m_valuesMap =
                    new TreeMap<ValuesKey, TNClassSectionScheduleData.TNClassSectionScheduleEntity.ValuesHelper>();

            // Initiate the date-timeScheduled map first

            for (TNStudentScheduleSpan studentScheduleSpan : scheduleSpans) {
                // Set the assigned start and exit date
                PlainDate assignedStartDate = startDate;
                PlainDate assignedEndDate = endDate;

                // Ignore school in this selection - count sections at secondary school
                if (studentScheduleSpan.getSection() != null &&
                        studentScheduleSpan.getSection().getScheduleTerm() != null &&
                        m_exportData.getScheduleTermDates(studentScheduleSpan.getSection().getScheduleTermOid()) != null
                        &&
                        studentScheduleSpan.getSection().getSchedule() != null) {
                    // First filter if the start date, and the span entry date
                    // are null
                    if (assignedStartDate != null &&
                            studentScheduleSpan.getEntryDate() != null &&
                            // Then filter if the span date is within the given
                            // start and end date
                            (!assignedStartDate.after(studentScheduleSpan.getExitDate())
                                    || !assignedEndDate.before(studentScheduleSpan.getEntryDate()))
                            &&
                            // Or filter if the student hasn't left the district
                            (studentScheduleSpan.getExitDate() != null
                                    && !studentScheduleSpan.getEntryDate().equals(studentScheduleSpan.getExitDate()))) {
                        // Get the master schedule, date converter, and override
                        // field change date
                        MasterSchedule mstSched = studentScheduleSpan.getSection();

                        // Check the term start and end dates
                        PlainDate termStart = null;
                        PlainDate termEnd = null;
                        Collection<ScheduleTermDate> termDates =
                                m_exportData.getScheduleTermDates(mstSched.getScheduleTermOid());
                        for (ScheduleTermDate termDate : termDates) {
                            if (termStart == null || termStart.after(termDate.getStartDate())) {
                                termStart = termDate.getStartDate();
                            }
                            if (termEnd == null || termEnd.before(termDate.getEndDate())) {
                                termEnd = termDate.getEndDate();
                            }
                        }

                        // Check the override start and end date. Use these
                        // dates if they are not null. Otherwise, use the
                        // 'assignedDates'
                        PlainDate overrideStartDate =
                                m_exportData.getOverrideDate(studentScheduleSpan.getEntryChange());

                        if (overrideStartDate != null &&
                                (assignedStartDate == null || !assignedStartDate.after(overrideStartDate))) {
                            assignedStartDate = overrideStartDate; // If the
                                                                   // overrideStartDate
                                                                   // is not
                                                                   // null, then
                                                                   // use that
                                                                   // date
                        } else if (assignedStartDate.before(studentScheduleSpan.getEntryDate())) {
                            assignedStartDate = studentScheduleSpan.getEntryDate();
                        }

                        PlainDate overrideEndDate = m_exportData.getOverrideDate(studentScheduleSpan.getExitChange());

                        if (overrideEndDate != null
                                && (assignedEndDate == null || assignedEndDate.after(overrideEndDate))) {
                            assignedEndDate = overrideEndDate; // If the
                                                               // overrideEndDate
                                                               // is not null,
                                                               // then use that
                                                               // date
                        } else if (!assignedEndDate.before(studentScheduleSpan.getExitDate())) {
                            assignedEndDate = studentScheduleSpan.getExitDate();
                            if (studentScheduleSpan.getExitChange() != null &&
                                    termEnd != null && assignedEndDate.before(termEnd) &&
                                    termStart != null && assignedEndDate.after(termStart)) {
                                Calendar cal = Calendar.getInstance();
                                cal.setTime(assignedEndDate);
                                cal.add(Calendar.DATE, -1);
                                assignedEndDate = new PlainDate(cal.getTime());
                            }
                        }

                        // Exclude the 'lunch' courses
                        try {
                            String excludeCourseIndicator = StringUtils.unNullify((String) mstSched.getSchoolCourse()
                                    .getCourse().getFieldValueByAlias(ALIAS_EXCLUDE_CRS));
                            ValuesKey key =
                                    new ValuesKey(mstSched.getSchoolCourse().getSchoolOid(), mstSched.getCourseView());
                            if (!BooleanAsStringConverter.TRUE.equals(excludeCourseIndicator)) {
                                intitialize(data, mstSched);
                                Collection<SisSchoolCalendarDate> sectionDates = m_exportData.getSectionDates(mstSched);

                                for (SisSchoolCalendarDate curSectionDate : sectionDates) {
                                    PlainDate curDate = curSectionDate.getDate();
                                    if (daysInSession.contains(curDate) && !curDate.before(assignedStartDate) &&
                                            !curDate.after(assignedEndDate)) {
                                        ValuesHelper valuesHelper = m_valuesMap.get(key);
                                        if (valuesHelper == null) {
                                            valuesHelper = new ValuesHelper(daysInSession);

                                            m_valuesMap.put(key, valuesHelper);
                                        }
                                        accumulateValuesHelper(mstSched, curSectionDate, valuesHelper);

                                    }
                                }
                            }
                        } catch (NullPointerException npe) {
                            // This code below is for debugging in TN live
                            // environment to find out which record causing the
                            // issue / error.
                            String message = "ERROR ON THIS RECORD: ";
                            if (mstSched != null) {
                                message = message + " Master Schedule '" + mstSched.getCourseView() + " - " +
                                        mstSched.getDescription() + "' ";
                                if (mstSched.getSchoolCourse() != null) {
                                    message = message + ", " + mstSched.getSchoolCourse().getNumber() + " - " +
                                            mstSched.getSchoolCourse().getDescription() + "' ";

                                    if (mstSched.getSchoolCourse().getCourse() != null) {
                                        message = message + ", " + mstSched.getSchoolCourse().getCourse().getNumber() +
                                                " - " + mstSched.getSchoolCourse().getCourse().getDescription()
                                                + "' has bad field " + ALIAS_EXCLUDE_CRS;
                                    } else {
                                        message = message + " has no course";
                                    }
                                } else {
                                    message = message + " has no school course";
                                }
                            }

                            AppGlobals.getLog().log(Level.SEVERE, message, npe);
                            throw npe;
                        }
                    }
                }
            }
        }

        /**
         * Filter all Durations and Periods can't be ZERO or EMPTY.
         *
         * @return StateReportValidationError
         */
        @Override
        public StateReportValidationError filterEntity() {

            StateReportValidationError error = super.filterEntity();

            if (error == null) {
                FieldDefinition field = getData().getFieldDefinition(FIELD_MONDAY_DURATION);

                String valueMonDur = getFieldValue(FIELD_MONDAY_DURATION);
                String valueMonPer = getFieldValue(FIELD_MONDAY_PERIOD);
                String valueTueDur = getFieldValue(FIELD_TUESDAY_DURATION);
                String valueTuePer = getFieldValue(FIELD_TUESDAY_PERIOD);
                String valueWedDur = getFieldValue(FIELD_WEDNESDAY_DURATION);
                String valueWedPer = getFieldValue(FIELD_WEDNESDAY_PERIOD);
                String valueThuDur = getFieldValue(FIELD_THURSDAY_DURATION);
                String valueThuPer = getFieldValue(FIELD_THURSDAY_PERIOD);
                String valueFriDur = getFieldValue(FIELD_FRIDAY_DURATION);
                String valueFriPer = getFieldValue(FIELD_FRIDAY_PERIOD);
                String valueSatDur = getFieldValue(FIELD_SATURDAY_DURATION);
                String valueSatPer = getFieldValue(FIELD_SATURDAY_PERIOD);

                if ((StringUtils.isEmpty(valueMonDur) || ZERO_STRING.equals(valueMonDur)) &&
                        (StringUtils.isEmpty(valueMonPer) || ZERO_STRING.equals(valueMonPer)) &&
                        (StringUtils.isEmpty(valueTueDur) || ZERO_STRING.equals(valueTueDur)) &&
                        (StringUtils.isEmpty(valueTuePer) || ZERO_STRING.equals(valueTuePer)) &&
                        (StringUtils.isEmpty(valueWedDur) || ZERO_STRING.equals(valueWedDur)) &&
                        (StringUtils.isEmpty(valueWedPer) || ZERO_STRING.equals(valueWedPer)) &&
                        (StringUtils.isEmpty(valueThuDur) || ZERO_STRING.equals(valueThuDur)) &&
                        (StringUtils.isEmpty(valueThuPer) || ZERO_STRING.equals(valueThuPer)) &&
                        (StringUtils.isEmpty(valueFriDur) || ZERO_STRING.equals(valueFriDur)) &&
                        (StringUtils.isEmpty(valueFriPer) || ZERO_STRING.equals(valueFriPer)) &&
                        (StringUtils.isEmpty(valueSatDur) || ZERO_STRING.equals(valueSatDur)) &&
                        (StringUtils.isEmpty(valueSatPer) || ZERO_STRING.equals(valueSatPer))) {
                    error = new StateReportValidationError(this, field,
                            "All 'Duration' fields and all 'Period' fields are blank for a given entity",
                            "Entity was not reported.");
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
            SisSchool school = (SisSchool) getBean();
            MasterSchedule section = getSection();
            String name = school.getName() + " - " + section.getCourseView();
            return name;
        }

        /**
         * Get the scheduled minutes on the given date with maximum of standard day minutes
         *
         * Note: Only works for the Director Memberships report that using the custom constructor.
         * Otherwise, returns 0 (zero).
         *
         * @param date PlainDate
         * @param stdStandardDay int
         * @return scheduledMinutes
         */
        public int getScheduledMinutes(PlainDate date, int stdStandardDay) {
            return getScheduledMinutes(date, stdStandardDay, true);
        }

        /**
         * Get the scheduled minutes on the given date
         *
         * Note: Only works for the Director Memberships report that using the custom constructor.
         * Otherwise, returns 0 (zero).
         *
         * @param date PlainDate
         * @param stdStandardDay int
         * @param limit boolean
         * @return scheduledMinutes
         */
        public int getScheduledMinutes(PlainDate date, int stdStandardDay, boolean limit) {
            int totalMinutes = 0;
            if (m_valuesMap != null) {
                for (ValuesHelper vh : m_valuesMap.values()) {
                    totalMinutes += vh.getTotalMinutes(date);
                }
            }

            return (limit && stdStandardDay < totalMinutes) ? stdStandardDay : totalMinutes;
        }

        /**
         * Get the total minutes for all periods limited to standard day per day.
         *
         * @param stdStandardDay int
         * @return int
         */
        public int getTotalMinutes(int stdStandardDay) {
            return getTotalMinutes(stdStandardDay, true);
        }

        /**
         * Get the total minutes for all periods.
         *
         * @param stdStandardDay int
         * @param limit - limit to standard day minutes per day
         * @return int
         */
        public int getTotalMinutes(int stdStandardDay, boolean limit) {
            int totalMinutes = 0;
            if (m_valuesMap != null) {
                Map<PlainDate, Integer> dateTotalMinutes = null;
                for (ValuesHelper vh : m_valuesMap.values()) {
                    dateTotalMinutes = vh.addTotalMinutes(dateTotalMinutes);
                }
                if (dateTotalMinutes != null) {
                    for (Entry<PlainDate, Integer> entry : dateTotalMinutes.entrySet()) {
                        int minutes = entry.getValue().intValue();
                        totalMinutes += (limit && stdStandardDay < minutes) ? stdStandardDay : minutes;
                    }
                }
            }

            return totalMinutes;
        }

        /**
         * Gets the total minutes including additional standard day minutes for any day in dates
         * included that is after the latest scheduled day. The maximum for any day is the standard
         * day.
         *
         * @param datesIncluded Collection<PlainDate>
         * @param stdStandardDay int
         * @param startDate
         * @return int
         */
        public int getTotalMinutes(Collection<PlainDate> datesIncluded, int stdStandardDay, PlainDate startDate) {
            int totalMinutes = 0;
            PlainDate maxDate = startDate;

            if (m_valuesMap != null) {
                Map<PlainDate, Integer> dateTotalMinutes = null;
                for (ValuesHelper vh : m_valuesMap.values()) {
                    dateTotalMinutes = vh.addTotalMinutes(dateTotalMinutes);
                }
                if (dateTotalMinutes != null) {
                    for (Entry<PlainDate, Integer> entry : dateTotalMinutes.entrySet()) {
                        int minutes = entry.getValue().intValue();
                        totalMinutes += (stdStandardDay < minutes) ? stdStandardDay : minutes;
                        if ((maxDate == null || maxDate.before(entry.getKey()))) {
                            maxDate = entry.getKey();
                        }
                    }
                }
                for (PlainDate date : datesIncluded) {
                    if (maxDate == null || date.after(maxDate)) {
                        totalMinutes += stdStandardDay;
                    }
                }
            }

            return totalMinutes;
        }

        /**
         * Gets the section dates.
         *
         * @param section MasterSchedule
         * @return Collection
         */
        public Collection<SisSchoolCalendarDate> getSectionDates(MasterSchedule section) {
            return m_exportData.getSectionDates(section);
        }

        /**
         * Gets the section periods.
         *
         * @param section MasterSchedule
         * @param csd SisSchoolCalendarDate
         * @return Collection
         */
        public Collection<SchedulePeriod> getSectionPeriods(MasterSchedule section, SisSchoolCalendarDate csd) {
            return m_exportData.getSectionPeriods(section, csd);
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            /*
             * If it's not the original 'export', then don't need to do the below
             */
            if (m_isOriginalExport) {
                m_exportData = (TNClassSectionScheduleData) data;

                Collection<String> schoolOids = m_exportData.m_classSectionHelper.getSchoolOidsCourse();

                if (schoolOids == null || schoolOids.isEmpty() || schoolOids.contains(bean.getOid())) {
                    m_sections = new ArrayList<MasterSchedule>();
                    Set<MasterSchedule> sections =
                            m_exportData.m_classSectionHelper.getReportableSectionForSchool(bean.getOid());
                    m_sections.addAll(sections);

                    int sectionsCount = m_sections.size();

                    if (sectionsCount > 0) {
                        populateValuesMap();
                    }
                    setRowCount(sectionsCount);

                    m_exportData.addEntityRowsCount(getRowCount());
                } else {
                    setRowCount(0);
                }
            }
        }

        /**
         * Returns new instance of ValuesKey for the map.
         *
         * @param sklOid String
         * @param courseView String
         * @return Values key
         */
        public ValuesKey getValueKey(String sklOid, String courseView) {
            return new ValuesKey(sklOid, courseView);
        }

        /**
         * Returns MST from the current row.
         *
         * @return Master schedule
         */
        protected MasterSchedule getSection() {
            return m_sections.get(getCurrentRow());
        }

        /**
         * Create report values based on schedule days .
         *
         * @param section MasterSchedule
         * @param valuesHelper ValuesHelper
         */
        private void accumulateScheduleValues(MasterSchedule section, ValuesHelper valuesHelper) {
            if (section.getSchedule() != null) {
                int index = 1;
                ScheduleBell bell = m_exportData.getSectionBellSchedule(section);
                if (bell != null) {
                    for (int i = 0; i <= 4; ++i) {
                        for (ScheduleDay day : m_exportData.getScheduleDays(section.getSchedule())) {
                            Collection<SchedulePeriod> periods = m_exportData.getSectionPeriods(section, day);
                            valuesHelper.incrementCountDays(index);
                            if (periods != null && periods.size() > 0) {
                                for (SchedulePeriod period : periods) {
                                    int time = m_exportData.getPeriodTime(bell, period);
                                    valuesHelper.addMinutes(index, time);

                                    if (valuesHelper.getMinPeriod(index) == 0 ||
                                            valuesHelper.getMinPeriod(index) > period.getNumber()) {
                                        valuesHelper.setMinPeriod(index, period.getNumber());
                                        valuesHelper.setPeriodId(index, 0);
                                        try {
                                            valuesHelper.setPeriodId(index, Integer.parseInt(period.getId()));
                                        } catch (NumberFormatException e) {
                                            // Use default zero if conversion
                                            // fails
                                        }
                                    }
                                }
                            }
                            // set next day Monday - Friday
                            index = index == 5 ? 1 : index + 1;
                        }
                    }
                }
            }
        }

        /**
         * Accumulate total time, number of sessions and minimum period for each section
         *
         * Maintain this method in case we revert to actual in-session times to calculate .
         *
         * @param section MasterSchedule
         * @param valuesHelper ValuesHelper
         */
        @SuppressWarnings("unused")
        private void accumulateValues(MasterSchedule section, ValuesHelper valuesHelper) {
            accumulateValues(section, null, null, valuesHelper);
        }

        /**
         * Accumulate total time, number of sessions and minimum period for each section for the
         * given span date.
         *
         * @param section MasterSchedule
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @param valuesHelper ValuesHelper
         */
        private void accumulateValues(MasterSchedule section,
                                      PlainDate startDate,
                                      PlainDate endDate,
                                      ValuesHelper valuesHelper) {
            Collection<SisSchoolCalendarDate> dates = m_exportData.getSectionDates(section);
            if (dates != null) {
                for (SisSchoolCalendarDate date : dates) {
                    if ((startDate == null || !startDate.after(date.getDate())) &&
                            (endDate == null || !endDate.before(date.getDate()))) {
                        accumulateValuesHelper(section, date, valuesHelper);
                    }
                }
            }
        }

        /**
         * Helper method for accumulateValues method.
         *
         * @param section MasterSchedule
         * @param date SisSchoolCalendarDate
         * @param valuesHelper ValuesHelper
         */
        private void accumulateValuesHelper(MasterSchedule section,
                                            SisSchoolCalendarDate date,
                                            ValuesHelper valuesHelper) {
            if (date.getInSessionIndicator()) {
                ScheduleBell bell = date.getBellSchedule();
                if (bell != null) {
                    int index = m_exportData.getDateIndex(date.getDate());
                    valuesHelper.incrementCountDays(index);
                    Collection<SchedulePeriod> periods = m_exportData.getSectionPeriods(section, date);
                    if (periods != null && periods.size() > 0) {
                        for (SchedulePeriod period : periods) {
                            int time = m_exportData.getPeriodTime(bell, period);
                            // Useful debugging block
                            //
                            // {
                            // DateAsStringConverter dateConverter = (DateAsStringConverter)
                            // ConverterFactory
                            // .getConverterForClass(PlainDate.class.getName(),
                            // Locale.getDefault(), true);
                            // PlainDate compareDate = (PlainDate)
                            // dateConverter.parseSystemString("2016-11-03");
                            // if (compareDate.equals(date.getDate())) {
                            // System.out.println(section.getCourseView() + " " + date.getDate() +
                            // " " + period.getId() + " " + time);
                            // }
                            // }
                            boolean isNullFiedDuration = false;
                            if (!m_isOriginalExport && time == 1 && isPullout(section)) {
                                isNullFiedDuration = true;
                            }

                            if (m_isOriginalExport || !isNullFiedDuration) {
                                valuesHelper.addMinutes(index, time);
                                valuesHelper.incrementDailyTime(date.getDate(), index, time);

                                if (valuesHelper.getMinPeriod(index) == 0 ||
                                        valuesHelper.getMinPeriod(index) > period.getNumber()) {
                                    valuesHelper.setMinPeriod(index, period.getNumber());
                                    valuesHelper.setPeriodId(index, 0);
                                    try {
                                        valuesHelper.setPeriodId(index, Integer.parseInt(period.getId()));
                                    } catch (NumberFormatException e) {
                                        // Use default zero if conversion fails
                                    }
                                }
                            }
                        }
                    } else {
                        valuesHelper.addMissedDate(date.getDate());
                    }
                }
            }
        }

        /**
         * Populates map with necessary values for retrievers.
         */
        private void populateValuesMap() {
            m_valuesMap =
                    new HashMap<ValuesKey, TNClassSectionScheduleData.TNClassSectionScheduleEntity.ValuesHelper>();
            for (MasterSchedule section : m_sections) {
                ValuesKey key = new ValuesKey(section.getSchoolCourse().getSchoolOid(), section.getCourseView());
                ValuesHelper valuesHelper = m_valuesMap.get(key);

                if (valuesHelper == null) {
                    valuesHelper = new ValuesHelper();
                    m_valuesMap.put(key, valuesHelper);
                } else {
                    valuesHelper = m_valuesMap.get(key);
                }
                accumulateScheduleValues(section, valuesHelper);
            }
        }

        /**
         * Check to see if class is pullout.
         *
         * @param section MasterSchedule
         * @return true, if is pullout
         */
        boolean isPullout(MasterSchedule section) {
            boolean value = false;
            if (section.getFieldValueByAlias(ALIAS_CLASS_TYPE_OVERRIDE) != null) {
                String classType = (String) section.getFieldValueByAlias(ALIAS_CLASS_TYPE_OVERRIDE);
                value = PULLOUT_CLASS_TYPE.equals(classType);
            }
            if (value == false && section.getSchoolCourse() != null && section.getSchoolCourse().getCourse() != null) {
                String classType =
                        (String) section.getSchoolCourse().getCourse().getFieldValueByAlias(ALIAS_COURSE_CLASS_TYPE);
                value = PULLOUT_CLASS_TYPE.equals(classType);
            }
            return value;
        }
    }

    /**
     * Field retriever for Class begin and end dates.
     */
    protected class RetrieveBeanPath implements FieldRetriever {
        static final String ALIAS_EXPRESSION = ".*\\[(.*)\\].*";
        Pattern m_aliasPattern;

        /**
         * Instantiates a new retrieve bean path.
         */
        public RetrieveBeanPath() {
            m_aliasPattern = Pattern.compile(ALIAS_EXPRESSION);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param reportEntity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity reportEntity, FieldDefinition field)
                throws X2BaseException {
            MasterSchedule section = ((TNClassSectionScheduleEntity) reportEntity).getSection();
            String beanPath = (String) field.getParameter();
            Matcher matcher = m_aliasPattern.matcher(beanPath);
            if (matcher.find()) {
                String alias = matcher.group(1);
                String path = data.translateAliasToJavaName(alias, false);
                if (!StringUtils.isEmpty(path)) {
                    beanPath = beanPath.replace("[" + alias + "]", path);
                }
            }
            return getProperty(section, beanPath);
        }
    }

    /**
     * Field retriever for Class begin and end dates.
     */
    protected class RetrieveBellPeriods implements FieldRetriever {
        static final int INDEX_FRI = 5;
        static final int INDEX_MON = 1;
        static final int INDEX_SAT = 6;
        static final int INDEX_SUN = 0;
        static final int INDEX_THU = 4;
        static final int INDEX_TUE = 2;
        static final int INDEX_WED = 3;

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param reportEntity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity reportEntity, FieldDefinition field) {
            TNClassSectionScheduleEntity entity = (TNClassSectionScheduleEntity) reportEntity;

            MasterSchedule mst = entity.getSection();
            ValuesKey key = entity.getValueKey(mst.getSchoolCourse().getSchoolOid(), mst.getCourseView());
            ValuesHelper valuesHelper = null;
            Object value = null;

            if (entity != null && (valuesHelper = entity.m_valuesMap.get(key)) != null) {

                String param = (String) field.getParameter();

                if (param.equalsIgnoreCase(CALC_PARAM_MONDAYDURATION)) {
                    value = Integer.valueOf(
                            valuesHelper.getCountDays(INDEX_MON) > 0 ? valuesHelper.getMinutes(INDEX_MON) /
                                    valuesHelper.getCountDays(INDEX_MON) : 0);
                } else if (param.equalsIgnoreCase(CALC_PARAM_MONDAYPERIOD)) {
                    value = Integer.valueOf(valuesHelper.getPeriodId(INDEX_MON));
                } else if (param.equalsIgnoreCase(CALC_PARAM_TUESDAYDURATION)) {
                    value = Integer.valueOf(
                            valuesHelper.getCountDays(INDEX_TUE) > 0 ? valuesHelper.getMinutes(INDEX_TUE) /
                                    valuesHelper.getCountDays(INDEX_TUE) : 0);
                } else if (param.equalsIgnoreCase(CALC_PARAM_TUESDAYPERIOD)) {
                    value = Integer.valueOf(valuesHelper.getPeriodId(INDEX_TUE));
                } else if (param.equalsIgnoreCase(CALC_PARAM_WEDNESDAYDURATION)) {
                    value = Integer.valueOf(
                            valuesHelper.getCountDays(INDEX_WED) > 0 ? valuesHelper.getMinutes(INDEX_WED) /
                                    valuesHelper.getCountDays(INDEX_WED) : 0);
                } else if (param.equalsIgnoreCase(CALC_PARAM_WEDNESDAYPERIOD)) {
                    value = Integer.valueOf(valuesHelper.getPeriodId(INDEX_WED));
                } else if (param.equalsIgnoreCase(CALC_PARAM_THURSDAYDURATION)) {
                    value = Integer.valueOf(
                            valuesHelper.getCountDays(INDEX_THU) > 0 ? valuesHelper.getMinutes(INDEX_THU) /
                                    valuesHelper.getCountDays(INDEX_THU) : 0);
                } else if (param.equalsIgnoreCase(CALC_PARAM_THURSDAYPERIOD)) {
                    value = Integer.valueOf(valuesHelper.getPeriodId(INDEX_THU));
                } else if (param.equalsIgnoreCase(CALC_PARAM_FRIDAYDURATION)) {
                    value = Integer.valueOf(
                            valuesHelper.getCountDays(INDEX_FRI) > 0 ? valuesHelper.getMinutes(INDEX_FRI) /
                                    valuesHelper.getCountDays(INDEX_FRI) : 0);
                } else if (param.equalsIgnoreCase(CALC_PARAM_FRIDAYPERIOD)) {
                    value = Integer.valueOf(valuesHelper.getPeriodId(INDEX_FRI));
                } else if (param.equalsIgnoreCase(CALC_PARAM_SATURDAYDURATION)) {
                    value = Integer.valueOf(
                            valuesHelper.getCountDays(INDEX_SAT) > 0 ? valuesHelper.getMinutes(INDEX_SAT) /
                                    valuesHelper.getCountDays(INDEX_SAT) : 0);
                } else if (param.equalsIgnoreCase(CALC_PARAM_SATURDAYPERIOD)) {
                    value = Integer.valueOf(valuesHelper.getPeriodId(INDEX_SAT));
                } else if (param.equalsIgnoreCase(CALC_PARAM_SUNDAYDURATION)) {
                    value = Integer.valueOf(
                            valuesHelper.getCountDays(INDEX_SUN) > 0 ? valuesHelper.getMinutes(INDEX_SUN) /
                                    valuesHelper.getCountDays(INDEX_SUN) : 0);
                } else if (param.equalsIgnoreCase(CALC_PARAM_SUNDAYPERIOD)) {
                    value = Integer.valueOf(valuesHelper.getPeriodId(INDEX_SUN));
                }

            }
            return value;
        }

    }

    /**
     * Field retriever for debugging the calculation
     * This field retriever returns the total days, total minutes and periods arrays.
     */
    protected class RetrieveDebug implements FieldRetriever {
        protected static final String TN_CALC_INSTPGM_ID = "DEBUG-CALC-ID";

        private static final String CALC_PARAM_TOTAL_MINUTES = "TOTAL-MINUTES";
        private static final String CALC_PARAM_COUNT_DAYS = "COUNT-DAYS";
        private static final String CALC_PARAM_PERIODS = "PERIODS";
        private static final String CALC_PARAM_MISSED_DATES = "MISSED-DATES";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param reportEntity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity reportEntity, FieldDefinition field) {
            TNClassSectionScheduleEntity seEntity = (TNClassSectionScheduleEntity) reportEntity;

            String param = (String) field.getParameter();

            MasterSchedule mst = seEntity.getSection();
            ValuesKey key = seEntity.getValueKey(mst.getSchoolCourse().getSchoolOid(), mst.getCourseView());
            ValuesHelper valuesHelper = null;
            Object value = null;

            if (seEntity != null && (valuesHelper = seEntity.m_valuesMap.get(key)) != null) {

                if (param.equalsIgnoreCase(CALC_PARAM_TOTAL_MINUTES)) {
                    value = "Total Minutes: " + valuesHelper.getMinutesAsString() + "   ";
                } else if (param.equalsIgnoreCase(CALC_PARAM_COUNT_DAYS)) {
                    value = "Count Days: " + valuesHelper.getCountDaysAsString() + "   ";
                } else if (param.equalsIgnoreCase(CALC_PARAM_PERIODS)) {
                    value = "Periods: " + valuesHelper.getMinPeriodAsString() + "   ";
                } else if (param.equalsIgnoreCase(CALC_PARAM_MISSED_DATES)) {
                    value = "Missed Dates: " + valuesHelper.getMissedDatesAsString() + "   ";
                }
            }

            return value;
        }

    }

    /**
     * Field retriever for instructional program and school year.
     */
    protected class RetrieveDefault implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param reportEntity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity reportEntity, FieldDefinition field) {
            TNClassSectionScheduleEntity seEntity = (TNClassSectionScheduleEntity) reportEntity;
            TNClassSectionScheduleData seData = (TNClassSectionScheduleData) data;

            String param = (String) field.getParameter();
            Object value = null;

            MasterSchedule section = seEntity.getSection();

            if (param.equalsIgnoreCase(CALC_PARAM_SCHOOLYEAR)) {
                value = seData.m_schoolYear;
            } else if (param.equalsIgnoreCase(CALC_PARAM_LOCALCNUM)) {
                value = section.getCourseView().replace("-", "");
            }

            return value;
        }
    }

    /**
     * Field retriever for Instructional program field.
     * Can be used only with SisStudent beans.
     */
    protected class RetrieveInstProgram implements FieldRetriever {
        public static final String TN_CALC_INSTPGM_ID = "TN_INSTRPGM_MST";
        private static final String INSTPGM_EMPTY_ERROR = "Instructional Program is empty.";

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
            TNClassSectionScheduleData tnData = (TNClassSectionScheduleData) data;

            MasterSchedule section = ((TNClassSectionScheduleEntity) entity).getSection();
            SisSchool school = (SisSchool) entity.getBean();
            String sectionOid = section.getOid();

            String instPgm = (tnData.m_classSectionHelper.m_instProgMap.containsKey(sectionOid))
                    ? tnData.m_classSectionHelper.m_instProgMap.get(sectionOid)
                    : tnData.m_classSectionHelper.m_defaultInstProgMap.get(school.getOid());
            if (!StringUtils.isEmpty(instPgm)) {
                return instPgm;
            }
            String calendarCode = tnData.m_classSectionHelper.m_sectionToCalendarCode.get(section.getOid());
            if (calendarCode == null) {
                calendarCode = EMPTY_STRING;
            }
            String message = "Value not specified. Set to " + field.getDefaultValue() + " by default. Context:" +
                    tnData.getCurrentContext().getContextId() + ". School:" + school.getName() +
                    ". Calendar code:" + calendarCode;
            StateReportValidationError error =
                    new StateReportValidationError(entity, field, INSTPGM_EMPTY_ERROR, message);
            entity.addRetrievalError(field.getFieldId(), error);
            return null;
        }
    }

    /**
     * Validator to determine issue, for Bridgeport Elementary loading all zeros for TN 031 Class
     * Minutes.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateDurations implements FieldValidator {
        Pattern m_regex = Pattern.compile("(.*)DURATION", Pattern.CASE_INSENSITIVE);

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
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String param = (String) field.getParameter();
            Matcher matcher = m_regex.matcher(param);
            if (matcher.matches()) {
                String periodField = matcher.replaceAll("$1 PERIOD");
                String durationField = matcher.replaceAll("$1 DURATION");
                int period = 0;
                int duration = 0;

                try {
                    period = Integer.parseInt(entity.getFieldValue(periodField));
                    duration = Integer.parseInt(entity.getFieldValue(durationField));
                } catch (NumberFormatException e) {
                    period = 0;
                    duration = 0;
                }
                if ((period > 0 && duration == 0) || period == 0 && duration > 0) {
                    String error = "Invalid Value";
                    String message =
                            periodField + " and " + durationField + " must either both be zero or neither be zero. " +
                                    "Values are " + periodField + "=" + period + ", " + durationField + "=" + duration;
                    errors.add(new StateReportValidationError(entity, field, error, message));
                }
            }

            return errors;
        }
    }

    /**
     * Constants: Aliases, Fields, IDs, Parameters
     */
    protected static final String ALIAS_CLASS_TYPE_OVERRIDE = "DOE CLASS TYPE OVERRIDE";
    protected static final String ALIAS_COURSE_CLASS_TYPE = "DOE CLASS TYPE";
    protected static final String ALIAS_EXCLUDE_CRS = "DOE EXCLUDE CRS";

    protected static final String CALC_PARAM_FRIDAYDURATION = "FRIDAYDURATION";
    protected static final String CALC_PARAM_FRIDAYPERIOD = "FRIDAYPERIOD";
    protected static final String CALC_PARAM_LOCALCNUM = "LOCALCNUM";
    protected static final String CALC_PARAM_MONDAYDURATION = "MONDAYDURATION";
    protected static final String CALC_PARAM_MONDAYPERIOD = "MONDAYPERIOD";
    protected static final String CALC_PARAM_SATURDAYDURATION = "SATURDAYDURATION";
    protected static final String CALC_PARAM_SATURDAYPERIOD = "SATURDAYPERIOD";
    protected static final String CALC_PARAM_SCHOOLYEAR = "SCHOOLYEAR";
    protected static final String CALC_PARAM_SUNDAYDURATION = "SUNDAYDURATION";
    protected static final String CALC_PARAM_SUNDAYPERIOD = "SUNDAYPERIOD";
    protected static final String CALC_PARAM_THURSDAYDURATION = "THURSDAYDURATION";
    protected static final String CALC_PARAM_THURSDAYPERIOD = "THURSDAYPERIOD";
    protected static final String CALC_PARAM_TUESDAYDURATION = "TUESDAYDURATION";
    protected static final String CALC_PARAM_TUESDAYPERIOD = "TUESDAYPERIOD";
    protected static final String CALC_PARAM_WEDNESDAYDURATION = "WEDNESDAYDURATION";
    protected static final String CALC_PARAM_WEDNESDAYPERIOD = "WEDNESDAYPERIOD";

    /**
     * Export's Fields
     */
    protected static final String FIELD_FRIDAY_DURATION = "FRIDAY DURATION";
    protected static final String FIELD_FRIDAY_PERIOD = "FRIDAY PERIOD";
    protected static final String FIELD_MONDAY_DURATION = "MONDAY DURATION";
    protected static final String FIELD_MONDAY_PERIOD = "MONDAY PERIOD";
    protected static final String FIELD_SATURDAY_DURATION = "SATURDAY DURATION";
    protected static final String FIELD_SATURDAY_PERIOD = "SATURDAY PERIOD";
    protected static final String FIELD_THURSDAY_DURATION = "THURSDAY DURATION";
    protected static final String FIELD_THURSDAY_PERIOD = "THURSDAY PERIOD";
    protected static final String FIELD_TUESDAY_DURATION = "TUESDAY DURATION";
    protected static final String FIELD_TUESDAY_PERIOD = "TUESDAY PERIOD";
    protected static final String FIELD_WEDNESDAY_DURATION = "WEDNESDAY DURATION";
    protected static final String FIELD_WEDNESDAY_PERIOD = "WEDNESDAY PERIOD";

    /**
     * Other constants
     */
    protected static final String ZERO_STRING = "0";

    private static final String CALC_ID_BEAN_PATH = "MST_BEAN_PATH";
    private static final String CALC_ID_BELLPERIOD = "MSTS_CALC_BELLPERIOD";
    private static final String CALC_ID_DEFAULT = "MSTS_CALC_DEFAULT";

    private static final String PULLOUT_CLASS_TYPE = "P";

    private static final String VAL_ID_DURATION_PERIOD = "DURATION_PERIOD";

    /**
     * Instance variables.
     */
    protected Map<String, Collection<ScheduleBellPeriod>> m_bellPeriods =
            new HashMap<String, Collection<ScheduleBellPeriod>>();
    protected TNClassSectionHelper m_classSectionHelper;
    protected SimpleDateFormat m_dateIndexFormat = new SimpleDateFormat("u");
    protected String m_fieldExcludeCrs;
    protected Map<String, List<ScheduleDay>> m_scheduleDays = new HashMap<String, List<ScheduleDay>>();
    protected Map<String, SchedulePeriod> m_schedulePeriods = new HashMap<String, SchedulePeriod>();
    protected Map<String, Collection<ScheduleTermDate>> m_scheduleTermDates =
            new HashMap<String, Collection<ScheduleTermDate>>();
    protected Map<String, ScheduleTerm> m_scheduleTerms = new HashMap<String, ScheduleTerm>();
    protected String m_schoolYear;
    protected Map<String, Map<String, Collection<SisSchoolCalendarDate>>> m_sectionDates =
            new HashMap<String, Map<String, Collection<SisSchoolCalendarDate>>>();
    protected Map<String, ScheduleBell> m_setionScheduleBell = new HashMap<String, ScheduleBell>();
    protected Map<String, Set<SchedulePeriod>> m_sectionTermDayPeriods = new HashMap<String, Set<SchedulePeriod>>();
    Map<String, Collection<ScheduleTerm>> m_sectionTermsMap = new HashMap<String, Collection<ScheduleTerm>>();

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query to load.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();
        initializeFields();
        School school = null;
        if (this.isSchoolContext()) {
            school = getSchool();
        }
        m_classSectionHelper = new TNClassSectionHelper(this, school, false,
                (Collection<String>) getParameter("studentOids"), getBypassValue());
        if (getSetupErrors().size() != 0) {
            return;
        }
        getCalendarsForContextOid(m_contextOid);
        X2Criteria sklCriteria = new X2Criteria();
        sklCriteria.addNotEmpty(m_fieldStateSchoolId, getBroker().getPersistenceKey());
        if (isSchoolContext()) {
            sklCriteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        }
        QueryByCriteria sklQuery = new QueryByCriteria(SisSchool.class, sklCriteria);
        applyInputSort(sklQuery, null);
        setQuery(sklQuery);
        setEntityClass(TNClassSectionScheduleEntity.class);
        m_classSectionHelper.loadDefaultInstProgMap();
        m_classSectionHelper.loadInstProgMap();
        loadSectionTermDayPeriods();
        initFieldRetrievers();
    }

    /**
     * Calculate the duration in minutes for a particular period.
     *
     * @param bell ScheduleBell
     * @param per SchedulePeriod
     * @return int
     */
    protected int getPeriodTime(ScheduleBell bell, SchedulePeriod per) {
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
        return 0;
    }

    /**
     * Get the day of week index for a date.
     *
     * @param date PlainDate
     * @return int
     */
    protected int getDateIndex(PlainDate date) {
        String index = m_dateIndexFormat.format(date);
        return Integer.parseInt(index);
    }

    /**
     * return sorted list of schedule days for the schedule.
     *
     * @param sch Schedule
     * @return List
     */
    protected List<ScheduleDay> getScheduleDays(Schedule sch) {
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
            term = (ScheduleTerm) getBroker().getBeanByOid(ScheduleTerm.class, oid);
            m_scheduleTerms.put(oid, term);
        }
        return term;
    }

    /**
     * Get the schedule term dates.
     *
     * @param oid String
     * @return Collection<ScheduleTermDate>
     */
    protected Collection<ScheduleTermDate> getScheduleTermDates(String oid) {
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
     * Get the most common bell schedule for the section.
     *
     * @param section MasterSchedule
     * @return Schedule bell
     */
    protected ScheduleBell getSectionBellSchedule(MasterSchedule section) {
        ScheduleBell bell = null;
        if (m_setionScheduleBell.containsKey(section.getOid())) {
            bell = m_setionScheduleBell.get(section.getOid());
        } else {
            Map<String, Integer> counts = new HashMap();
            Collection<SisSchoolCalendarDate> dates = getSectionDates(section);
            if (dates != null) {
                for (SisSchoolCalendarDate date : dates) {
                    String bellOid = date.getBellScheduleOid();
                    Integer currentCount = counts.get(bellOid);
                    currentCount = currentCount == null ? Integer.valueOf(1) : Integer.valueOf(currentCount.intValue() + 1);
                    counts.put(bellOid, currentCount);
                }
                String oid = null;
                int count = 0;
                for (Entry<String, Integer> entry : counts.entrySet()) {
                    if (entry.getValue().intValue() > count) {
                        count = entry.getValue().intValue();
                        oid = entry.getKey();
                    }
                }
                bell = oid == null ? null : (ScheduleBell) getBroker().getBeanByOid(ScheduleBell.class, oid);
            }
            m_setionScheduleBell.put(section.getOid(), bell);
        }
        return bell;
    }

    /**
     * Get a collection of in session calendar dates for a particular section.
     *
     * @param section MasterSchedule
     * @return Collection<SisSchoolCalendarDate>
     */
    protected Collection<SisSchoolCalendarDate> getSectionDates(MasterSchedule section) {
        String calendarCode = m_classSectionHelper.getSectionCalendarCode(section);

        String key =
                section.getSchedule().getDistrictContextOid() + section.getSchedule().getSchoolOid() + calendarCode;
        Map<String, Collection<SisSchoolCalendarDate>> termDates = m_sectionDates.get(key);
        if (termDates == null) {
            termDates = new HashMap<String, Collection<SisSchoolCalendarDate>>();
            m_sectionDates.put(key, termDates);

            X2Criteria criteriaDates = new X2Criteria();
            criteriaDates.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                    SchoolCalendar.COL_DISTRICT_CONTEXT_OID, section.getSchedule().getDistrictContextOid());
            criteriaDates.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                    SchoolCalendar.COL_SCHOOL_OID, section.getSchedule().getSchoolOid());
            criteriaDates.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                    SchoolCalendar.COL_CALENDAR_ID, calendarCode);
            criteriaDates.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);

            QueryByCriteria queryDates = new QueryByCriteria(SisSchoolCalendarDate.class, criteriaDates);
            queryDates.addOrderBy(SisSchoolCalendarDate.COL_DATE, true);

            Collection<SisSchoolCalendarDate> dates = getBroker().getCollectionByQuery(queryDates);

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.COL_SCHEDULE_OID, section.getScheduleOid());

            QueryByCriteria query = new QueryByCriteria(ScheduleTermDate.class, criteria);
            Map<String, Collection<ScheduleTermDate>> terms = getBroker().getGroupedCollectionByQuery(query,
                    ScheduleTermDate.COL_SCHEDULE_TERM_OID,
                    32);

            for (String termOid : terms.keySet()) {
                Set<SisSchoolCalendarDate> csdSet = new HashSet<SisSchoolCalendarDate>();
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
     * Get the schedule periods for a particular section on a particular day.
     *
     * @param section MasterSchedule
     * @param csd SisSchoolCalendarDate
     * @return Collection<SchedulePeriod>
     */
    protected Collection<SchedulePeriod> getSectionPeriods(MasterSchedule section, SisSchoolCalendarDate csd) {
        Collection<SchedulePeriod> periods = null;
        Collection<ScheduleTerm> terms = m_sectionTermsMap.get(section.getOid());
        if (terms != null) {
            for (ScheduleTerm trm : terms) {
                for (ScheduleTermDate tmd : getScheduleTermDates(trm.getOid())) {
                    if (!csd.getDate().before(tmd.getStartDate()) && !csd.getDate().after(tmd.getEndDate())) {
                        String key = section.getOid() + tmd.getScheduleTermOid() +
                                getScheduleDayOid(section.getSchedule(), csd.getScheduleDayNumber());
                        periods = m_sectionTermDayPeriods.get(key);
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
     * Get the schedule periods for a particular section on a particular schedule day.
     *
     * @param section MasterSchedule
     * @param day ScheduleDay
     * @return Collection<SchedulePeriod>
     */
    protected Collection<SchedulePeriod> getSectionPeriods(MasterSchedule section, ScheduleDay day) {
        Collection<SchedulePeriod> periods = null;
        Collection<ScheduleTerm> terms = m_sectionTermsMap.get(section.getOid());
        if (terms != null) {
            for (ScheduleTerm trm : terms) {
                String key = section.getOid() + trm.getOid() + day.getOid();
                periods = m_sectionTermDayPeriods.get(key);
                if (periods != null) {
                    break;
                }
            }
        }
        return periods;
    }

    /**
     * get the parameter setting for bypass value. Default to false.
     *
     * @return boolean
     */
    private boolean getBypassValue() {
        boolean value = false;
        if (getParameter(PARAM_BYPASS_DUP_SECT_TEST) != null) {
            value = ((Boolean) getParameter(PARAM_BYPASS_DUP_SECT_TEST)).booleanValue();
        }
        return value;
    }

    /**
     * Method for implementing business rule for schoolYear.
     *
     * @return string representation of school year
     */
    private String getCurentSchoolYear() {
        return Integer.toString(getCurrentContext().getSchoolYear() - 1);
    }

    /**
     * Get the ScheduleDay oid for a schedule and schedule day number.
     *
     * @param sch Schedule
     * @param dayNumber int
     * @return String
     */
    private String getScheduleDayOid(Schedule sch, int dayNumber) {
        List<ScheduleDay> days = getScheduleDays(sch);
        for (ScheduleDay day : days) {
            if (day.getNumber() == dayNumber) {
                return day.getOid();
            }
        }
        return "";
    }

    /**
     * Get the SchedulePeriod for a period oid.
     *
     * @param perOid String
     * @return SchedulePeriod
     */
    private SchedulePeriod getSchedulePeriod(String perOid) {
        SchedulePeriod period = m_schedulePeriods.get(perOid);
        if (!m_schedulePeriods.containsKey(perOid)) {
            period = (SchedulePeriod) getBroker().getBeanByOid(SchedulePeriod.class, perOid);
            m_schedulePeriods.put(perOid, period);
        }
        return period;
    }

    /**
     * Register custom field Retrievers.
     */
    private void initFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(CALC_ID_BEAN_PATH, new RetrieveBeanPath());
        calcs.put(CALC_ID_DEFAULT, new RetrieveDefault());
        calcs.put(CALC_ID_BELLPERIOD, new RetrieveBellPeriods());
        calcs.put(RetrieveInstProgram.TN_CALC_INSTPGM_ID, new RetrieveInstProgram());
        calcs.put(RetrieveDebug.TN_CALC_INSTPGM_ID, new RetrieveDebug());
        super.addCalcs(calcs);

        HashMap validators = new HashMap<String, FieldValidator>();
        validators.put(VAL_ID_DURATION_PERIOD, new ValidateDurations());
        super.addValidators(validators);
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_schoolYear = getCurentSchoolYear();
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        m_fieldExcludeCrs = translateAliasToJavaName(ALIAS_EXCLUDE_CRS, true);
    }

    /**
     * Build maps of Section per day periods.
     */
    private void loadSectionTermDayPeriods() {
        for (Collection<String> list : m_classSectionHelper.getReportableSectionsOidLists()) {
            // Key is mst, trm, day oids
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(MasterTerm.COL_MASTER_SCHEDULE_OID, list);

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

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);

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
                        terms = new LinkedList();
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
