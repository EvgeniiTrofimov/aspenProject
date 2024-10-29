/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.tn;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Data source of Data sources for the "TN 200 Day Accountability" report and sub reports.
 *
 * @author X2 Development Corporation
 */

public class TN200DayAccountabilityReportData extends ReportJavaSourceNet {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 

    /**
     * This class provides counts of all types of the days in the current context.
     */
    class DistrictCalendarDaysHelper {

        /**
         * The Class DistrictCalendarEntity.
         */
        class DistrictCalendarEntity {
            Map<PlainDate, String> m_abbreviatedDays = new HashMap<PlainDate, String>();
            Collection<PlainDate> m_christmasDates = new ArrayList<PlainDate>();
            Map<PlainDate, String> m_discretionaryDaysLength = new HashMap<PlainDate, String>();
            Map<PlainDate, String> m_discretionaryDaysType = new HashMap<PlainDate, String>();
            Map<PlainDate, String> m_inServiceDays = new HashMap<PlainDate, String>();
            Map<PlainDate, String> m_inServiceOptDays = new HashMap<PlainDate, String>();
            int m_makeUpExtendedCount = 0;
            Map<PlainDate, String> m_parentConference = new HashMap<PlainDate, String>();
            Collection<PlainDate> m_springBreakDates = new ArrayList<PlainDate>();
            int m_stockpileInclementCount = 0;
            int m_stockpileProfessionCount = 0;
            int m_studentsDaysCount = 0;
            int m_teacherVacationCount = 0;
            PlainDate m_endDate;
            PlainDate m_startDate;

        }

        private Map<String, DistrictCalendarEntity> m_districtCalendarDataMap =
                new HashMap<String, DistrictCalendarEntity>();
        private Collection<SchoolCalendar> m_districtCalendars = new ArrayList<SchoolCalendar>();

        /**
         * Gets the district calendars.
         *
         * @return Collection
         */
        public Collection<SchoolCalendar> getDistrictCalendars() {
            return m_districtCalendars;
        }

        /**
         * Return last inSession day for given districtContext.
         *
         * @param districtCalendarOid String
         * @return the m_endDate
         */
        public PlainDate getEndDate(String districtCalendarOid) {
            return m_districtCalendarDataMap.get(districtCalendarOid).m_endDate;
        }

        /**
         * Return first inSession day for given districtContext.
         *
         * @param districtCalendarOid String
         * @return the m_startDate
         */
        public PlainDate getStartDate(String districtCalendarOid) {
            return m_districtCalendarDataMap.get(districtCalendarOid).m_startDate;
        }

        /**
         * Instantiates a new district calendar days helper.
         *
         * @param context DistrictSchoolYearContext
         * @param broker X2Broker
         */
        public DistrictCalendarDaysHelper(DistrictSchoolYearContext context, X2Broker broker) {
            initialize(context, broker);
        }

        /**
         * Gets the abbreviated days.
         *
         * @param districtCalendarOid String
         * @return the m_abbreviatedDays
         */
        public Map<PlainDate, String> getAbbreviatedDays(String districtCalendarOid) {
            return m_districtCalendarDataMap.get(districtCalendarOid).m_abbreviatedDays;
        }

        /**
         * Gets the christmas dates.
         *
         * @param districtCalendarOid String
         * @return the m_christmasDates
         */
        public Collection<PlainDate> getChristmasDates(String districtCalendarOid) {
            return m_districtCalendarDataMap.get(districtCalendarOid).m_christmasDates;
        }

        /**
         * Gets the conference.
         *
         * @param districtCalendarOid String
         * @return the m_tpConference
         */
        public Map<PlainDate, String> getConference(String districtCalendarOid) {
            return m_districtCalendarDataMap.get(districtCalendarOid).m_parentConference;
        }

        /**
         * Gets the discretionary days length.
         *
         * @param districtCalendarOid String
         * @return the m_discretionaryDays
         */
        public Map<PlainDate, String> getDiscretionaryDaysLength(String districtCalendarOid) {
            return m_districtCalendarDataMap.get(districtCalendarOid).m_discretionaryDaysLength;
        }

        /**
         * Gets the discretionary days type.
         *
         * @param districtCalendarOid String
         * @return the m_discretionaryDays
         */
        public Map<PlainDate, String> getDiscretionaryDaysType(String districtCalendarOid) {
            return m_districtCalendarDataMap.get(districtCalendarOid).m_discretionaryDaysType;
        }

        /**
         * Gets the in service days.
         *
         * @param districtCalendarOid String
         * @return the m_inServiceDays
         */
        public Map<PlainDate, String> getInServiceDays(String districtCalendarOid) {
            return m_districtCalendarDataMap.get(districtCalendarOid).m_inServiceDays;
        }

        /**
         * Gets the in service opt days.
         *
         * @param districtCalendarOid String
         * @return the m_inServiceOptDays
         */
        public Map<PlainDate, String> getInServiceOptDays(String districtCalendarOid) {
            return m_districtCalendarDataMap.get(districtCalendarOid).m_inServiceOptDays;
        }

        /**
         * Gets the make up extended count.
         *
         * @param districtCalendarOid String
         * @return the m_makeUpExtendedCount
         */
        public int getMakeUpExtendedCount(String districtCalendarOid) {
            return m_districtCalendarDataMap.get(districtCalendarOid).m_makeUpExtendedCount;
        }

        /**
         * Gets the spring break dates.
         *
         * @param districtCalendarOid String
         * @return the m_springBreakDates
         */
        public Collection<PlainDate> getSpringBreakDates(String districtCalendarOid) {
            return m_districtCalendarDataMap.get(districtCalendarOid).m_springBreakDates;
        }

        /**
         * Gets the spring holidays days count.
         *
         * @param districtCalendarOid String
         * @return Integer
         */
        public Integer getSpringHolidaysDaysCount(String districtCalendarOid) {
            return Integer.valueOf(m_districtCalendarDataMap.get(districtCalendarOid).m_springBreakDates.size());
        }

        /**
         * Gets the stockpile inclement count.
         *
         * @param districtCalendarOid String
         * @return the m_stockpileInclementCount
         */
        public int getStockpileInclementCount(String districtCalendarOid) {
            return m_districtCalendarDataMap.get(districtCalendarOid).m_stockpileInclementCount;
        }

        /**
         * Gets the stockpile profession count.
         *
         * @param districtCalendarOid String
         * @return the m_stockpileProfessionCount
         */
        public int getStockpileProfessionCount(String districtCalendarOid) {
            return m_districtCalendarDataMap.get(districtCalendarOid).m_stockpileProfessionCount;
        }

        /**
         * Gets the students days count.
         *
         * @param districtCalendarOid String
         * @return the m_studentsDaysCount
         */
        public int getStudentsDaysCount(String districtCalendarOid) {
            return m_districtCalendarDataMap.get(districtCalendarOid).m_studentsDaysCount;
        }

        /**
         * Gets the teacher vacation count.
         *
         * @param districtCalendarOid String
         * @return the m_teacherVacationCount
         */
        public int getTeacherVacationCount(String districtCalendarOid) {
            return m_districtCalendarDataMap.get(districtCalendarOid).m_teacherVacationCount;
        }

        /**
         * Checks if is district stockpiling.
         *
         * @param districtCalendarOid String
         * @return Boolean
         */
        public Boolean isDistrictStockpiling(String districtCalendarOid) {
            return Boolean.valueOf(m_districtCalendarDataMap.get(districtCalendarOid).m_stockpileInclementCount != 0 ||
                    m_districtCalendarDataMap.get(districtCalendarOid).m_stockpileProfessionCount != 0 ||
                    m_districtCalendarDataMap.get(districtCalendarOid).m_makeUpExtendedCount != 0);
        }

        /**
         * Gets the spring holiday start date.
         *
         * @param districtCalendarOid String
         * @return Plain date
         */
        public PlainDate getSpringHolidayStartDate(String districtCalendarOid) {
            if (!m_districtCalendarDataMap.get(districtCalendarOid).m_springBreakDates.iterator().hasNext()) {
                return null;
            }
            PlainDate startDate =
                    m_districtCalendarDataMap.get(districtCalendarOid).m_springBreakDates.iterator().next();
            for (PlainDate date : m_districtCalendarDataMap.get(districtCalendarOid).m_springBreakDates) {
                if (date.before(startDate)) {
                    startDate = date;
                }
            }
            return startDate;
        }

        /**
         * Gets the christmas holiday start date.
         *
         * @param districtCalendarOid String
         * @return Plain date
         */
        public PlainDate getChristmasHolidayStartDate(String districtCalendarOid) {
            if (!m_districtCalendarDataMap.get(districtCalendarOid).m_christmasDates.iterator().hasNext()) {
                return null;
            }
            PlainDate startDate = m_districtCalendarDataMap.get(districtCalendarOid).m_christmasDates.iterator().next();
            for (PlainDate date : m_districtCalendarDataMap.get(districtCalendarOid).m_christmasDates) {
                if (date.before(startDate)) {
                    startDate = date;
                }
            }
            return startDate;
        }

        /**
         * Gets the christmas holidays days count.
         *
         * @param districtCalendarOid String
         * @return Integer
         */
        public Integer getChristmasHolidaysDaysCount(String districtCalendarOid) {
            return Integer.valueOf(m_districtCalendarDataMap.get(districtCalendarOid).m_christmasDates.size());
        }

        /**
         * By default return FULL_DAY constants.
         *
         * @param eventCode String
         * @return String
         */
        private String determineEventLength(String eventCode) {
            if (!StringUtils.isEmpty(eventCode) && eventCode.contains(CODE_SEPARATOR)) {
                String dayLengthCode = eventCode.substring(eventCode.indexOf(CODE_SEPARATOR));
                if (!StringUtils.isEmpty(dayLengthCode)) {
                    if (DAY_FULL_INDICATOR.equals(dayLengthCode)) {
                        return FULL_DAY;
                    } else if (DAY_HALF_INDICATOR.equals(dayLengthCode)) {
                        return HALF_DAY;
                    } else if (DAY_ONE_THIRD_INDICATOR.equals(dayLengthCode)) {
                        return ONE_THIRD_DAY;
                    }
                }
            }
            return FULL_DAY;
        }

        /**
         * This method collect all possible events of the given day.
         *
         * @param eventCodes List<String>
         * @return Collection of events
         */
        private Collection<String> getAllDayEvents(List<String> eventCodes) {
            Collection<String> events = new ArrayList<String>();
            for (String eventCode : eventCodes) {
                if (!StringUtils.isEmpty(eventCode)) {
                    if (m_eventTypes.get(eventCode) != null &&
                            !StringUtils.isEmpty(m_eventTypes.get(eventCode).getStateCode())) {
                        events.add(m_eventTypes.get(eventCode).getStateCode());
                    } else {
                        events.add(eventCode);
                    }
                }
            }
            return events;
        }

        /**
         * This method provide all counts.
         *
         * @param context DistrictSchoolYearContext
         * @param broker X2Broker
         */
        private void initialize(DistrictSchoolYearContext context, X2Broker broker) {
            Collection<SchoolCalendar> disCalendars = loadDistrictCalendars(context, broker);
            for (SchoolCalendar calendar : disCalendars) {
                String oid = calendar.getOid();
                m_districtCalendars.add(calendar);
                DistrictCalendarEntity entity = new DistrictCalendarEntity();
                m_districtCalendarDataMap.put(oid, entity);
                String stockpile = (String) calendar.getFieldValueByBeanPath(m_fieldStockpile);
                if (stockpile != null && !stockpile.isEmpty()) {
                    BigDecimal bd = new BigDecimal(stockpile);
                    entity.m_stockpileInclementCount = bd.intValue();
                }

                for (SchoolCalendarDate schoolCalendarDate : calendar.getSchoolCalendarDates(broker)) {
                    PlainDate eventDate = schoolCalendarDate.getDate();

                    if (schoolCalendarDate.getInSessionIndicator() &&
                            (entity.m_startDate == null
                                    || (entity.m_startDate != null && entity.m_startDate.after(eventDate)))) {
                        entity.m_startDate = eventDate;
                    }

                    if (schoolCalendarDate.getInSessionIndicator() &&
                            (entity.m_endDate == null
                                    || (entity.m_endDate != null && entity.m_endDate.before(eventDate)))) {
                        entity.m_endDate = eventDate;
                    }

                    String eventCode1 = schoolCalendarDate.getScheduleDayType();
                    String eventCode2 = (String) schoolCalendarDate.getFieldValueByBeanPath(m_fieldEvent2);
                    String eventCode3 = (String) schoolCalendarDate.getFieldValueByBeanPath(m_fieldEvent3);

                    Collection<String> events = getAllDayEvents(Arrays.asList(eventCode1, eventCode2, eventCode3));

                    if (schoolCalendarDate.getInSessionIndicator()) {
                        entity.m_studentsDaysCount++;
                    }

                    for (String eventCode : events) {
                        if (m_teacherVacationCodes.contains(eventCode)) {
                            entity.m_teacherVacationCount++;
                        }
                        if (m_makeupDayCodes.contains(eventCode)) {
                            entity.m_makeUpExtendedCount++;
                        }
                        if (m_stockpileProfessionCodes.contains(eventCode)) {
                            entity.m_stockpileProfessionCount++;
                        }
                        if (m_christmasHolidayCodes.contains(eventCode)) {
                            entity.m_christmasDates.add(eventDate);
                        }
                        if (m_springHolidayCodes.contains(eventCode)) {
                            entity.m_springBreakDates.add(eventDate);
                        }

                        String dayLength = determineEventLength(eventCode);
                        if (m_serviceDayCodes.contains(eventCode)) {
                            entity.m_inServiceDays.put(eventDate, dayLength);
                        } else if (m_serviceDayOptCodes.contains(eventCode)) {
                            entity.m_inServiceOptDays.put(eventDate, dayLength);
                        } else if (m_abbreviatedDaysCodes.contains(eventCode)) {
                            entity.m_abbreviatedDays.put(eventDate, dayLength);
                        } else if (m_parentConferenceCodes.contains(eventCode)) {
                            entity.m_parentConference.put(eventDate, dayLength);
                        } else if (m_discretionaryOCodes.contains(eventCode)) {
                            entity.m_discretionaryDaysLength.put(eventDate, dayLength);
                            entity.m_discretionaryDaysType.put(eventDate, ACTIVITY_OTHER);
                        } else if (m_discretionaryACodes.contains(eventCode)) {
                            entity.m_discretionaryDaysLength.put(eventDate, dayLength);
                            entity.m_discretionaryDaysType.put(eventDate, ACTIVITY_ADMINISTRATIVE);
                        } else if (m_discretionaryICodes.contains(eventCode)) {
                            entity.m_discretionaryDaysLength.put(eventDate, dayLength);
                            entity.m_discretionaryDaysType.put(eventDate, ACTIVITY_SERVICE);
                        } else if (m_discretionaryVCodes.contains(eventCode)) {
                            entity.m_discretionaryDaysLength.put(eventDate, dayLength);
                            entity.m_discretionaryDaysType.put(eventDate, ACTIVITY_VACATION);
                        }
                    }
                }
            }
        }

        /**
         * Loads school calendars marked as district calendars.
         *
         * @param context DistrictSchoolYearContext
         * @param broker X2Broker
         * @return collection of district calendars
         */
        private Collection<SchoolCalendar> loadDistrictCalendars(DistrictSchoolYearContext context, X2Broker broker) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_SCHOOL_ID,
                    PARAM_DISTRICT_CALENDAR);
            criteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, context.getOid());
            BeanQuery query = new BeanQuery(SchoolCalendar.class, criteria);
            return broker.getCollectionByQuery(query);
        }
    }

    // Activity codes
    private static final String ACTIVITY_ADMINISTRATIVE = "A";
    private static final String ACTIVITY_OTHER = "O";
    private static final String ACTIVITY_SERVICE = "I";
    private static final String ACTIVITY_VACATION = "V";

    // Aliases
    private static final String ALIAS_DAY_EVENT2 = "DOE DAY EVENT TYPE 2";
    private static final String ALIAS_DAY_EVENT3 = "DOE DAY EVENT TYPE 3";
    private static final String ALIAS_DISTRICT_SI_STOCKPILE = "DOE DISTRICT SI STOCKPILE";

    private static final String ALIAS_SCHOOL_STATE_ID = "DOE SCHOOL STATE ID";

    private static final String CODE_SEPARATOR = "-";

    private static final String DAY_FULL_INDICATOR = "-100";
    private static final String DAY_HALF_INDICATOR = "-050";
    private static final String DAY_ONE_THIRD_INDICATOR = "-033";

    // Fields
    private static final String FIELD_ABBREVIATED_DAY = "abbreviatedDate";
    private static final String FIELD_ABBREVIATED_TIME = "abbreviatedTime";
    private static final String FIELD_CHRISTMAS_DATE = "christmasDate";
    private static final String FIELD_CHRISTMAS_DAYS = "christmasCount";
    private static final String FIELD_CONFERENCE_DATE = "conferenceDate";
    private static final String FIELD_CONFERENCE_TIME = "conferenceTime";
    private static final String FIELD_DISCRETIONARY_ACTIVITY = "discretionaryActivity";
    private static final String FIELD_DISCRETIONARY_DATE = "discretionaryDate";
    private static final String FIELD_DISCRETIONARY_DAY = "discretionaryDays";
    private static final String FIELD_DISCRETIONARY_TIME = "discretionaryTime";
    private static final String FIELD_DISTRICT_CALENDAR = "districtCalendar";
    private static final String FIELD_END_DATE = "closingDate";
    private static final String FIELD_IS_STOCKPILING = "isStockpiling";
    private static final String FIELD_MAKEUP_COUNT = "makeUpCount";
    private static final String FIELD_PARENT_CONFERENCE = "parentConfCount";
    private static final String FIELD_SERVICE_DATE = "serviceDate";
    private static final String FIELD_SERVICE_DAY = "serviceDays";
    private static final String FIELD_SERVICE_DAY_OPT = "serviceOptDays";
    private static final String FIELD_SERVICE_TIME = "serviceTime";
    private static final String FIELD_SPRING_DATE = "springDate";
    private static final String FIELD_SPRING_DAYS = "springCount";
    private static final String FIELD_START_DATE = "openDate";
    private static final String FIELD_STOCKPILE_PROF = "stockpileProfCount";
    private static final String FIELD_STOCKPILE_WEATHER = "stockpileWeatherCount";
    private static final String FIELD_STUDENTS_DAY = "studentDays";
    private static final String FIELD_TEACHER_VACATION = "teacherVacation";

    // Breaking days constants
    private static final String FULL_DAY = "01.00";
    private static final String HALF_DAY = "00.50";
    private static final String ONE_THIRD_DAY = "00.33";

    // Input Parameters
    private static final String PARAM_ABBREVIATED_DAYS = "abbreviatedDay";
    private static final String PARAM_CHRISTMAS_HOLIDAY = "christmasHoliday";
    private static final String PARAM_DISCRETIONARY_A = "discretionaryA";
    private static final String PARAM_DISCRETIONARY_I = "discretionaryI";
    private static final String PARAM_DISCRETIONARY_O = "discretionaryO";
    private static final String PARAM_DISCRETIONARY_V = "discretionaryV";
    private static final String PARAM_MAKEUP_DAY = "makeupDay";
    private static final String PARAM_PARENT_CONFERENCE = "parentConference";
    private static final String PARAM_SERVICE_DAY = "serviceDay";
    private static final String PARAM_SERVICE_DAY_OPT = "serviceDayOpt";
    private static final String PARAM_SPRING_HOLIDAY = "springHoliday";
    private static final String PARAM_STOCKPILE_PROFESSION = "stockpileProfession";
    private static final String PARAM_TEACHER_VACATION = "teacherVacation";

    // Report Parameters
    private static final String PARAM_DISTRICT = "district";
    private static final String PARAM_SCHOOL_YEAR = "schoolYear";
    private static final String PARAM_STOCKPILE_SCHOOLS = "stockpileSchools";

    // Calendar Id of District Calendars
    private static final String PARAM_DISTRICT_CALENDAR = "dist";

    protected List<String> m_abbreviatedDaysCodes = new ArrayList<String>();
    protected List<String> m_christmasHolidayCodes = new ArrayList<String>();
    protected List<String> m_discretionaryACodes = new ArrayList<String>();
    protected List<String> m_discretionaryICodes = new ArrayList<String>();
    protected List<String> m_discretionaryOCodes = new ArrayList<String>();
    protected List<String> m_discretionaryVCodes = new ArrayList<String>();
    protected Map<String, ReferenceCode> m_eventTypes;
    protected String m_fieldEvent1;
    protected String m_fieldEvent2;
    protected String m_fieldEvent3;
    protected String m_fieldStateId;
    protected String m_fieldStockpile;
    protected List<String> m_makeupDayCodes = new ArrayList<String>();
    protected List<String> m_parentConferenceCodes = new ArrayList<String>();
    protected List<String> m_serviceDayCodes = new ArrayList<String>();
    protected List<String> m_serviceDayOptCodes = new ArrayList<String>();
    protected List<String> m_springHolidayCodes = new ArrayList<String>();
    protected List<String> m_stockpileInclementCodes = new ArrayList<String>();
    protected List<String> m_stockpileProfessionCodes = new ArrayList<String>();
    protected List<String> m_teacherVacationCodes = new ArrayList<String>();

    private DistrictCalendarDaysHelper m_data;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {

        ReportDataGrid grid = new ReportDataGrid();

        Collection<SchoolCalendar> districtCalendars = m_data.getDistrictCalendars();
        for (SchoolCalendar districtCalendar : districtCalendars) {
            String oid = districtCalendar.getOid();
            grid.append();
            grid.set(FIELD_DISTRICT_CALENDAR, districtCalendar.getCalendarId());
            grid.set(FIELD_STUDENTS_DAY, Integer.valueOf(m_data.getStudentsDaysCount(oid)));
            grid.set(FIELD_TEACHER_VACATION, Integer.valueOf(m_data.getTeacherVacationCount(oid)));

            grid.set(FIELD_SERVICE_DAY, calcDaysLengthSum(m_data.getInServiceDays(oid).values()));
            grid.set(FIELD_SERVICE_DAY_OPT, calcDaysLengthSum(m_data.getInServiceOptDays(oid).values()));
            grid.set(FIELD_PARENT_CONFERENCE, calcDaysLengthSum(m_data.getConference(oid).values()));
            grid.set(FIELD_DISCRETIONARY_DAY, calcDaysLengthSum(m_data.getDiscretionaryDaysLength(oid).values()));

            grid.set(FIELD_CHRISTMAS_DAYS, m_data.getChristmasHolidaysDaysCount(oid));
            grid.set(FIELD_CHRISTMAS_DATE, m_data.getChristmasHolidayStartDate(oid));
            grid.set(FIELD_SPRING_DAYS, m_data.getSpringHolidaysDaysCount(oid));
            grid.set(FIELD_SPRING_DATE, m_data.getSpringHolidayStartDate(oid));

            Map<PlainDate, String> abbreviatedMap = m_data.getAbbreviatedDays(oid);
            grid.set(FIELD_ABBREVIATED_DAY, getSortedDays(abbreviatedMap.keySet()));
            grid.set(FIELD_ABBREVIATED_TIME, abbreviatedMap);

            Map<PlainDate, String> conferenceMap = m_data.getConference(oid);
            grid.set(FIELD_CONFERENCE_DATE, getSortedDays(conferenceMap.keySet()));
            grid.set(FIELD_CONFERENCE_TIME, conferenceMap);

            Map<PlainDate, String> inServiceMap = m_data.getInServiceDays(oid);
            grid.set(FIELD_SERVICE_DATE, getSortedDays(inServiceMap.keySet()));
            grid.set(FIELD_SERVICE_TIME, inServiceMap);

            Map<PlainDate, String> discretionaryDaysLength = m_data.getDiscretionaryDaysLength(oid);
            Map<PlainDate, String> discretionaryDaysType = m_data.getDiscretionaryDaysType(oid);
            grid.set(FIELD_DISCRETIONARY_DATE, getSortedDays(discretionaryDaysLength.keySet()));
            grid.set(FIELD_DISCRETIONARY_TIME, discretionaryDaysLength);
            grid.set(FIELD_DISCRETIONARY_ACTIVITY, discretionaryDaysType);

            grid.set(FIELD_MAKEUP_COUNT, Integer.valueOf(m_data.getMakeUpExtendedCount(oid)));
            grid.set(FIELD_STOCKPILE_PROF, Integer.valueOf(m_data.getStockpileProfessionCount(oid)));
            grid.set(FIELD_STOCKPILE_WEATHER, Integer.valueOf(m_data.getStockpileInclementCount(oid)));

            grid.set(FIELD_IS_STOCKPILING, m_data.isDistrictStockpiling(oid));
            grid.set(FIELD_START_DATE, m_data.getStartDate(oid));
            grid.set(FIELD_END_DATE, m_data.getEndDate(oid));
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * This method is provided as a convenient way for subclasses to initialize member variables
     * before the <code>run()</code> method is called. The default implementation does nothing.
     */
    @Override
    protected void initialize() {
        initDictionaryValues();
        initEventCodes();

        m_data = new DistrictCalendarDaysHelper(getOrganization().getCurrentContext(), getBroker());
        addParameter(PARAM_SCHOOL_YEAR, getOrganization().getCurrentContext().getContextId());
        addParameter(PARAM_DISTRICT, getOrganization());
        addParameter(PARAM_STOCKPILE_SCHOOLS, getSchoolsStockpiling());
    }

    /**
     * Returns names of active schools with valid state id.
     *
     * @return Collection
     */
    private Collection<String> getSchoolsStockpiling() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SisSchool.COL_INACTIVE_INDICATOR, BooleanAsStringConverter.FALSE);
        criteria.addEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, BooleanAsStringConverter.FALSE);
        criteria.addNotEmpty(m_fieldStateId, getBroker().getPersistenceKey());
        SubQuery schoolNames = new SubQuery(SisSchool.class, SisSchool.COL_NAME, criteria, true);

        return getBroker().getSubQueryCollectionByQuery(schoolNames);
    }

    /**
     * Calc days length sum.
     *
     * @param days is a list containing durations of the events in string form
     * @return sum of event durations
     */
    private Double calcDaysLengthSum(Collection<String> days) {
        int summ = 0;
        for (String dayLength : days) {
            if (ONE_THIRD_DAY.equals(dayLength)) {
                summ += 2;
            } else if (HALF_DAY.equals(dayLength)) {
                summ += 3;
            } else {
                summ += 6;
            }
        }

        return Double.valueOf(summ / 6.);
    }

    /**
     * Gets the sorted days.
     *
     * @param dates Set<PlainDate>
     * @return List
     */
    private List getSortedDays(Set<PlainDate> dates) {
        ArrayList sortedDates = new ArrayList(dates);
        Collections.sort(sortedDates);
        return sortedDates;
    }

    /**
     * Inits the dictionary values.
     */
    private void initDictionaryValues() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_fieldEvent2 = dictionary.findDataDictionaryFieldByAlias(ALIAS_DAY_EVENT2).getJavaName();
        m_fieldEvent3 = dictionary.findDataDictionaryFieldByAlias(ALIAS_DAY_EVENT3).getJavaName();
        m_fieldStockpile = dictionary.findDataDictionaryFieldByAlias(ALIAS_DISTRICT_SI_STOCKPILE).getJavaName();
        m_fieldStateId = dictionary.findDataDictionaryFieldByAlias(ALIAS_SCHOOL_STATE_ID).getJavaName();

        DataDictionaryField eventType = dictionary.findDataDictionaryField(SchoolCalendarDate.class.getName(),
                SchoolCalendarDate.COL_SCHEDULE_DAY_TYPE);
        if (eventType != null && eventType.hasReferenceTable()) {
            m_eventTypes = eventType.getReferenceTable().getCodeMap(getBroker());
        }
    }

    /**
     * Inits the event codes.
     */
    private void initEventCodes() {
        char delimiter = ',';
        m_abbreviatedDaysCodes
                .addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_ABBREVIATED_DAYS),
                        delimiter, true));
        m_teacherVacationCodes
                .addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_TEACHER_VACATION),
                        delimiter, true));
        m_parentConferenceCodes
                .addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_PARENT_CONFERENCE),
                        delimiter, true));
        m_christmasHolidayCodes
                .addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_CHRISTMAS_HOLIDAY),
                        delimiter, true));
        m_springHolidayCodes
                .addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_SPRING_HOLIDAY),
                        delimiter, true));
        m_makeupDayCodes.addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_MAKEUP_DAY),
                delimiter, true));
        m_serviceDayCodes.addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_SERVICE_DAY),
                delimiter, true));
        m_serviceDayOptCodes
                .addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_SERVICE_DAY_OPT),
                        delimiter, true));
        m_discretionaryOCodes
                .addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_DISCRETIONARY_O),
                        delimiter, true));
        m_discretionaryACodes
                .addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_DISCRETIONARY_A),
                        delimiter, true));
        m_discretionaryICodes
                .addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_DISCRETIONARY_I),
                        delimiter, true));
        m_discretionaryVCodes
                .addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_DISCRETIONARY_V),
                        delimiter, true));
        m_stockpileProfessionCodes
                .addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_STOCKPILE_PROFESSION),
                        delimiter, true));
    }
}
