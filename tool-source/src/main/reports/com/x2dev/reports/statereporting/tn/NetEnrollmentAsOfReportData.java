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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentEnrollmentSpan;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.procedures.statereporting.tn.TNStateReportData;
import com.x2dev.reports.statereporting.tn.TNReportingPeriodHelper.NetEnrolledInfo;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentContextAttributes;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.logging.Level;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.Query;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Net Enrollment As Of" report.
 *
 * @author X2 Development Corporation
 */
public class NetEnrollmentAsOfReportData extends ReportJavaSourceNet {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Inner subclass to provide access to opportunities of StudentHistoryHelper.
     */
    class EnrollmentStatistics extends TNStateReportData {
        protected static final String PARAM_REPORT_PERIOD = "reportPeriod";

        private int m_maxGradeLevel;
        private Map<String, ReferenceCode> m_referenceGradeCodeMap;
        private TreeMap m_sortedGradeLevels;
        private TNEnrollmentHelper m_tnEnrHelper;
        private TNStudentHistoryHelper m_tnStudentHelper;

        /**
         * Calculate grade code considering student's attributes, span and yog of student.
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
                yog = getYogOnEndDate(span);
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
         * @return Criteria
         */
        public Criteria getStudentCriteria() {
            return m_tnStudentHelper.getStudentCriteria();
        }

        /**
         * Return the most recent enrollment record for the student of the specified types that
         * exists on or before the specified date.
         *
         * @param student SisStudent
         * @param endDate PlainDate
         * @param enrollmentType String
         * @return StudentEnrollment
         */
        public StudentEnrollment getStudentEnrollmentForDate(SisStudent student,
                                                             PlainDate endDate,
                                                             String enrollmentType) {
            return m_tnStudentHelper.getEnrollmentForDate(student.getOid(), endDate, enrollmentType);
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
            return m_tnStudentHelper.getTNStudentEnrollmentSpans(student, limit);
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
         * Return Race collection for the student.
         *
         * @param student Student
         * @return Collection<Race>
         */
        public Collection<Race> getStudentRace(Student student) {
            return m_tnStudentHelper.getPersonRaceMap().get(student.getPersonOid());
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

            m_tnEnrHelper = new TNEnrollmentHelper(this);
            m_tnStudentHelper = m_tnEnrHelper.getStudentHistoryHelper();

            m_tnStudentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.FALSE);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.FALSE);

            loadGradeCodes();
            m_maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());

            m_sortedGradeLevels = StudentManager.buildGradeLevelMap(getBroker());
        }


        /**
         * Returns yog based on student enrollment span and last date of report period.
         *
         * @param span TNStudentEnrollmentSpan
         * @return int
         */
        private int getYogOnEndDate(TNStudentEnrollmentSpan span) {
            int yog;
            StudentEnrollment enrollment = span.getEnrollmentForDate(m_endDate, StudentEnrollment.YOG_CHANGE);
            if (enrollment != null) {
                yog = enrollment.getYog();
            } else {
                yog = span.getFirstActiveEnrollment().getYog();
            }
            return yog;
        }

        /**
         * Load grade codes.
         */
        private void loadGradeCodes() {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            ModelProperty prop =
                    new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL, getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
            ReferenceTable referenceTable = field.getReferenceTable();
            m_referenceGradeCodeMap = referenceTable.getCodeMap();
        }
    }

    /**
     * The Class CalendarPeriodHelper.
     */
    protected class CalendarPeriodHelper {
        public static final String KEY_DATE_BEGIN = "DATE_BEGIN";
        public static final String KEY_DATE_END = "DATE_END";
        private static final int REPORT_PERIOD_LENGTH = 20;

        private Map<String, Map<String, PlainDate>> m_calendarPeriodMap = null;
        private PlainDate m_yearBeginDate = null;
        private PlainDate m_yearEndDate = null;

        /**
         * Instantiates a new calendar period helper.
         *
         * @param calendarDates Collection<SchoolCalendarDate>
         */
        public CalendarPeriodHelper(Collection<SchoolCalendarDate> calendarDates) {
            m_calendarPeriodMap = new HashMap<String, Map<String, PlainDate>>();

            if (calendarDates != null && !calendarDates.isEmpty()) {
                // periodCounter should start from 1 as
                // ReportPeriod codes start from 01.
                int periodCounter = 1;
                int dayCounter = 1;
                boolean isYearStart = true;
                PlainDate startDate = null;
                PlainDate endDate = null;

                for (SchoolCalendarDate calendarDate : calendarDates) {
                    if (dayCounter == 1) {
                        startDate = calendarDate.getDate();
                        if (isYearStart) {
                            m_yearBeginDate = startDate;
                            isYearStart = false;
                        }
                    } else if ((dayCounter == REPORT_PERIOD_LENGTH) ||
                            ((periodCounter == 9) &&
                                    calendarDate.equals(new ArrayList<SchoolCalendarDate>(calendarDates)
                                            .get(calendarDates.size() - 1)))) {
                        endDate = calendarDate.getDate();
                        m_yearEndDate = endDate;

                        Map<String, PlainDate> datesMap = new HashMap<String, PlainDate>();
                        datesMap.put(KEY_DATE_BEGIN, startDate);
                        datesMap.put(KEY_DATE_END, endDate);

                        m_calendarPeriodMap.put(leftPad(periodCounter, 2), datesMap);

                        dayCounter = 0;
                        periodCounter++;
                    }
                    dayCounter++;
                }
            }
        }

        /**
         * Get Calendar Period Map.
         *
         * @return Map<String, Map<String, PlainDate>>
         */
        public Map<String, Map<String, PlainDate>> getCalendarPeriodMap() {
            return m_calendarPeriodMap;
        }

        /**
         * Get Month Begin Date.
         *
         * @param monthCode String
         * @return PlainDate
         */
        public PlainDate getMonthBeginDate(String monthCode) {
            PlainDate beginDate = null;

            if (getCalendarPeriodMap().containsKey(monthCode)) {
                beginDate = getCalendarPeriodMap().get(monthCode).get(KEY_DATE_BEGIN);
            }

            return beginDate;
        }

        /**
         * Get Month End Date.
         *
         * @param monthCode String
         * @return PlainDate
         */
        public PlainDate getMonthEndDate(String monthCode) {
            PlainDate endDate = null;

            if (getCalendarPeriodMap().containsKey(monthCode)) {
                endDate = getCalendarPeriodMap().get(monthCode).get(KEY_DATE_END);
            }

            return endDate;
        }

        /**
         * Get Year Begin Date.
         *
         * @return PlainDate
         */
        public PlainDate getYearBeginDate() {
            return m_yearBeginDate;
        }

        /**
         * Get Year End Date.
         *
         * @return PlainDate
         */
        public PlainDate getYearEndDate() {
            return m_yearEndDate;
        }

        /**
         * Check if period map is the same.
         *
         * @param calendarPeriod CalendarPeriodHelper
         * @return boolean
         */
        public boolean isPeriodMapSame(CalendarPeriodHelper calendarPeriod) {
            boolean isPeriodMapSame = true;

            if (getYearBeginDate().equals(calendarPeriod.getYearBeginDate()) &&
                    getYearEndDate().equals(calendarPeriod.getYearEndDate())) {
                for (String period : getCalendarPeriodMap().keySet()) {
                    if (calendarPeriod.getCalendarPeriodMap().containsKey(period)) {
                        if (!calendarPeriod.getCalendarPeriodMap().get(period).get(KEY_DATE_BEGIN)
                                .equals(getCalendarPeriodMap().get(period).get(KEY_DATE_BEGIN))
                                || !calendarPeriod.getCalendarPeriodMap().get(period).get(KEY_DATE_END)
                                        .equals(getCalendarPeriodMap().get(period).get(KEY_DATE_END))) {
                            isPeriodMapSame = false;
                            break;
                        }
                    } else {
                        isPeriodMapSame = false;
                        break;
                    }

                }
            } else {
                isPeriodMapSame = false;
            }

            return isPeriodMapSame;
        }

        /**
         * Pad zeros to the left of the string to match month codes.
         *
         * @param n number of places
         * @param padding number of zeros to pad
         * @return formatted String
         */
        private String leftPad(int n, int padding) {
            return String.format("%0" + padding + "d", Integer.valueOf(n));
        }
    }

    /**
     * The Class PeriodHelper.
     */
    protected class PeriodHelper {
        /**
         * Fields
         */
        private ReferenceCode m_month;
        private DistrictSchoolYearContext m_currentContext;
        private X2Broker m_broker;
        private HashMap<String, Map<String, CalendarPeriodHelper>> m_schoolCalendarPeriodMap;

        /**
         * Public constructor.
         *
         * @param organization Organization
         * @param currentContext Current context
         * @param month Selected month
         * @param broker X2Broker
         */
        public PeriodHelper(Organization organization, DistrictSchoolYearContext currentContext, ReferenceCode month,
                X2Broker broker) {
            this.m_broker = broker;
            this.m_currentContext = currentContext;
            this.m_month = month;
            initializeSclCalPeriodDatesMap();

        }

        /**
         * Get the calendar oids.
         *
         * @param schoolOid String
         * @return Collection<String>
         */
        public Collection<String> getCalendarOids(String schoolOid) {
            return m_schoolCalendarPeriodMap.get(schoolOid).keySet();
        }

        /**
         * Get the date begin.
         *
         * @param schoolOid String
         * @param calendarOid String
         * @return PlainDate the Begin Date of the reporting period
         */
        public PlainDate getDateBegin(String schoolOid, String calendarOid) {
            PlainDate startDate = null;

            if (m_schoolCalendarPeriodMap != null &&
                    m_schoolCalendarPeriodMap.containsKey(schoolOid) &&
                    m_schoolCalendarPeriodMap.get(schoolOid).containsKey(calendarOid)) {
                startDate =
                        m_schoolCalendarPeriodMap.get(schoolOid).get(calendarOid).getMonthBeginDate(m_month.getCode());
            }

            return startDate;
        }

        /**
         * Get the date end.
         *
         * @param schoolOid String
         * @param calendarOid String
         * @return PlainDate the End Date of the reporting period
         */
        public PlainDate getDateEnd(String schoolOid, String calendarOid) {
            PlainDate endDate = null;

            if (m_schoolCalendarPeriodMap != null &&
                    m_schoolCalendarPeriodMap.containsKey(schoolOid) &&
                    m_schoolCalendarPeriodMap.get(schoolOid).containsKey(calendarOid)) {
                endDate = m_schoolCalendarPeriodMap.get(schoolOid).get(calendarOid).getMonthEndDate(m_month.getCode());
            }

            return endDate;
        }

        /**
         * Get the month.
         *
         * @return ReferenceCode
         */
        public ReferenceCode getMonth() {
            return m_month;
        }

        /**
         * Get the yearBegin.
         *
         * @param schoolOid String
         * @param calendarOid String
         * @return PlainDate the Begin Date of the reporting period
         */
        public PlainDate getYearBegin(String schoolOid, String calendarOid) {
            PlainDate startDate = null;

            if (m_schoolCalendarPeriodMap != null &&
                    m_schoolCalendarPeriodMap.containsKey(schoolOid) &&
                    m_schoolCalendarPeriodMap.get(schoolOid).containsKey(calendarOid)) {
                startDate = m_schoolCalendarPeriodMap.get(schoolOid).get(calendarOid).getYearBeginDate();
            }

            return startDate;
        }

        /**
         * Get the yearEnd.
         *
         * @param schoolOid String
         * @param calendarOid String
         * @return PlainDate the End Date of the reporting period
         */
        public PlainDate getYearEnd(String schoolOid, String calendarOid) {
            PlainDate endDate = null;

            if (m_schoolCalendarPeriodMap != null &&
                    m_schoolCalendarPeriodMap.containsKey(schoolOid) &&
                    m_schoolCalendarPeriodMap.get(schoolOid).containsKey(calendarOid)) {
                endDate = m_schoolCalendarPeriodMap.get(schoolOid).get(calendarOid).getYearEndDate();
            }

            return endDate;
        }


        /**
         * Set the month.
         *
         * @param month the m_month to set
         */
        public void setMonth(ReferenceCode month) {
            this.m_month = month;
        }

        /**
         * Build collection of SchoolCalendars.
         *
         * @return Map<String, SchoolCalendar>
         */
        private Map<String, SchoolCalendar> getCalendars() {
            X2Criteria casCriteria = new X2Criteria();

            casCriteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_currentContext.getOid());
            // Filter to eliminate unused schools.
            casCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            casCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);

            QueryByCriteria query = new QueryByCriteria(SchoolCalendar.class, casCriteria);

            Map<String, SchoolCalendar> calendars =
                    m_broker.getGroupedCollectionByQuery(query, X2BaseBean.COL_OID, 100);

            return calendars;
        }

        /**
         * The method which build map of maps of school calendar dates keyed on school oid and
         * calendar oid.
         *
         * @return Map<String, Map<String, Collection<SchoolCalendarDate>>>
         */
        private Map<String, Map<String, Collection<SchoolCalendarDate>>> getSCDMap() {
            X2Criteria csdCriteria = new X2Criteria();
            csdCriteria.addIn(SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID, getCalendars().keySet());
            csdCriteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);

            QueryByCriteria csdQuery = new QueryByCriteria(SchoolCalendarDate.class, csdCriteria);
            csdQuery.addOrderBy(SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID, true);
            csdQuery.addOrderBy(SchoolCalendarDate.COL_DATE, true);

            return m_broker.getGroupedCollectionByQuery(csdQuery, new String[] {
                    SchoolCalendarDate.REL_SCHOOL_CALENDAR
                            + ModelProperty.PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                    SchoolCalendarDate.REL_SCHOOL_CALENDAR
                            + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID},
                    new int[] {16, 4});
        }

        /**
         * Initialize school dates map keyed on school oid .<br>
         * Map contains map keyed on calendar oid, which contains map keyed on reporting
         * period(month's number) which
         * then contains map keyed on BEGIN or END date key with values of dates.<br>
         * The purpose then is to get dates by school and month's number.
         */
        private void initializeSclCalPeriodDatesMap() {
            m_schoolCalendarPeriodMap = new HashMap<String, Map<String, CalendarPeriodHelper>>();

            Map<String, Map<String, Collection<SchoolCalendarDate>>> schoolCalendarDateMap = getSCDMap();

            for (String schoolOid : schoolCalendarDateMap.keySet()) {
                for (String calendarOid : schoolCalendarDateMap.get(schoolOid).keySet()) {
                    CalendarPeriodHelper calendarPeriod =
                            new CalendarPeriodHelper(schoolCalendarDateMap.get(schoolOid).get(calendarOid));

                    if (!isCalendarSame(schoolOid, calendarPeriod)) {
                        Map<String, CalendarPeriodHelper> calendarPeriodMap =
                                new HashMap<String, CalendarPeriodHelper>();
                        calendarPeriodMap.put(calendarOid, calendarPeriod);
                        m_schoolCalendarPeriodMap.put(schoolOid, calendarPeriodMap);
                    }
                }
            }
        }

        /**
         * Check if the calendar same.
         *
         * @param schoolOid String
         * @param calendarPeriod CalendarPeriodHelper
         * @return boolean
         */
        private boolean isCalendarSame(String schoolOid, CalendarPeriodHelper calendarPeriod) {
            boolean isCalendarSame = false;
            if (m_schoolCalendarPeriodMap.get(schoolOid) != null) {
                for (CalendarPeriodHelper schoolCalendarPeriod : m_schoolCalendarPeriodMap.get(schoolOid).values()) {
                    if (schoolCalendarPeriod.isPeriodMapSame(calendarPeriod)) {
                        isCalendarSame = true;
                        break;
                    }
                }
            }

            return isCalendarSame;
        }
    }

    private static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL STATE ID";

    private static final String EMPTY_STRING = "";

    private static final String ENROLLMENT_TYPES = "EW";

    private static final int GRADE_FILTER_MIN_LEVEL = 0;
    private static final int GRADE_FILTER_MAX_LEVEL = 12;

    private static final int GRADE_LEVEL_NUMERIC_TOTAL_K_12 = 100;


    public static final String INPUT_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_REPORT_PERIOD = "reportPeriod";
    private static final String INPUT_REPORT_DATE = "reportDate";
    private static final String INPUT_REPORT_MODE = "reportMode";
    private static final String INPUT_SCHOOLS = "schoolOids";

    private static final String REPORT_FIELD_CALENDAR_NUMBER = "calendarNumber";
    private static final String REPORT_FIELD_DATE_END = "dateEnd";
    private static final String REPORT_FIELD_DATE_START = "dateStart";
    private static final String REPORT_FIELD_GRADE_TOTAL = "gradeTotal";
    private static final String REPORT_FIELD_GRADE_K = "gradeK";
    private static final String REPORT_FIELD_SCHOOL = "school";
    private static final String REPORT_FIELD_ORG_NAME = "orgName";
    private static final String REPORT_FIELD_RACE = "race";
    private static final String REPORT_FIELD_REPORT_TYPE = "reportType";

    private static final String REPORT_PARAMETER_CONTEXT = "context";
    private static final String REPORT_PARAMETER_END_DATE = "dateEnd";
    private static final String REPORT_PARAMETER_REPORT_PERIOD = "period";
    private static final String REPORT_PARAMETER_SCL_CAL_COUNTER = "sclCalCounter";
    private static final String REPORT_PARAMETER_USER = "user";

    private static final String RACE_CODE_ASIAN = "A";
    private static final String RACE_CODE_BLACK = "B";
    private static final String RACE_CODE_INDIAN = "I";
    private static final String RACE_CODE_PACIFIC = "P";
    private static final String RACE_CODE_WHITE = "W";

    private static final String RACE_TOTAL_DESCRIPTION = "Total";
    private static final String RACE_TYPE_2_OR_MORE_DESCRIPTION = "2 or More";
    private static final String RACE_TYPE_HISPANIC_ALL_DESCRIPTION = "Hispanic-All Races";
    private static final String RACE_TYPE_NON_HISPANIC_ASIAN_DESCRIPTION = "Non-Hisp. Asian";
    private static final String RACE_TYPE_NON_HISPANIC_BLACK_DESCRIPTION = "Non-Hisp. Black";
    private static final String RACE_TYPE_NON_HISPANIC_INDIAN_DESCRIPTION = "Non-Hisp. Indian";
    private static final String RACE_TYPE_NON_HISPANIC_PACIFIC_DESCRIPTION = "Non-Hisp. Pacific";
    private static final String RACE_TYPE_NON_HISPANIC_WHITE_DESCRIPTION = "Non-Hisp. White";

    private static final String STAFF_SCHOOL_CODE = "9999";

    private static final String STATE_CODE_GENDER_MALE = "M";
    private static final String STATE_CODE_GENDER_FEMALE = "F";
    private static final String STATE_CODE_TOTAL = "T";

    protected PlainDate m_endDate = null;

    private DistrictSchoolYearContext m_context;
    private EnrollmentStatistics m_data;
    private Map<String, Map<String, Map<Integer, Integer>>> m_districtMembershipData = null;
    private boolean m_isAllSchools;
    private PeriodHelper m_periodHelper = null;
    private Map<String, ReferenceCode> m_referenceGenderCodeMap;
    private Map<String, ReferenceCode> m_referenceRaceCodeMap;
    private PlainDate m_reportDate = null;
    private Integer m_reportMode = null;
    private Collection<SisSchool> m_schools = null;
    protected PlainDate m_startDate = null;
    private TNReportingPeriodHelper m_tnPeriodHelper = null;


    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        // Map to count numbers of calendars with different days for current school.
        Map<String, Map<String, Integer>> schCalCounter = new HashMap<String, Map<String, Integer>>();

        for (SisSchool school : m_schools) {
            Collection<String> calendarOids = m_periodHelper.getCalendarOids(school.getOid());
            for (String calOid : calendarOids) {
                if (m_reportMode.intValue() == 1) {
                    m_startDate = m_periodHelper.getYearBegin(school.getOid(), calOid);
                    m_endDate = m_periodHelper.getDateEnd(school.getOid(), calOid);
                    if (m_startDate == null || m_endDate == null) {
                        String message =
                                "For school " + school.getName() + " is not defined report period with code: " +
                                        m_periodHelper.getMonth().getCode();
                        AppGlobals.getLog().log(Level.WARNING, message);
                        continue;
                    }
                } else if (m_reportMode.intValue() == 2) {
                    m_startDate = m_periodHelper.getYearBegin(school.getOid(), calOid);
                    if (m_startDate == null) {
                        continue;
                    }
                } else {
                    m_startDate = m_periodHelper.getYearBegin(school.getOid(), calOid);
                    m_endDate = m_periodHelper.getYearEnd(school.getOid(), calOid);
                    if (m_startDate == null || m_endDate == null) {
                        continue;
                    }
                }

                m_data = new EnrollmentStatistics();
                m_data.setBroker(getBroker());
                m_data.setOrganization(getOrganization());
                m_data.setPrivilegeSet(getPrivilegeSet());
                m_data.setSchoolContext(true);
                m_data.setSchool(school);
                m_data.setParameters(getParameters());
                m_data.setUser(getUser());
                m_data.initializeExport();

                QueryIterator students = getBroker().getIteratorByQuery(m_data.getStudentQuery(false));
                Map<String, Map<String, Map<Integer, Integer>>> schoolMembershipData;
                try {
                    schoolMembershipData = populateMembershipData(students);
                    calculateTotalDataSet(schoolMembershipData);
                    calculateColumnTotals(schoolMembershipData);
                } finally {
                    if (students != null) {
                        students.close();
                    }
                }

                ReportDataGrid schoolGrid = new ReportDataGrid();
                String orgName = school.getName();

                SchoolCalendar calendar = (SchoolCalendar) getBroker().getBeanByOid(SchoolCalendar.class, calOid);


                populateGrid(schoolGrid, orgName, schoolMembershipData, school, calendar);

                grid.append(schoolGrid);

                // increment calendar count for current school
                if (schCalCounter.get(school.getOid()) != null) {
                    Map<String, Integer> calOidCount = schCalCounter.get(school.getOid());
                    calOidCount.put(calOid, Integer.valueOf(calOidCount.size() + 1));
                } else {
                    Map<String, Integer> countMap = new HashMap<String, Integer>();
                    countMap.put(calOid, Integer.valueOf(1));
                    schCalCounter.put(school.getOid(), countMap);
                }
            }
        }
        // Add District summary if All active schools selected.
        if (m_isAllSchools) {
            ReportDataGrid schoolGrid = new ReportDataGrid();

            calculateTotalDataSet(m_districtMembershipData);
            calculateColumnTotals(m_districtMembershipData);
            populateGrid(schoolGrid, null, m_districtMembershipData, null, null);

            grid.append(schoolGrid);
        }

        addParameter(REPORT_PARAMETER_SCL_CAL_COUNTER, schCalCounter);
        grid.beforeTop();
        return grid;
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
        m_schools = getSchools();
        loadGenderCodes();
        loadRaceCodes();

        if (m_isAllSchools) {
            m_districtMembershipData = getDefaultMembership();
        }

        m_reportMode = getParameter(INPUT_REPORT_MODE) != null ? (Integer) getParameter(INPUT_REPORT_MODE)
                : Integer.valueOf(0);

        m_context = getCurrentContext();
        m_startDate = m_context.getStartDate();
        m_endDate = m_context.getEndDate();

        // Create default mockup object to initialize Period Helper
        ReferenceCode mockup = X2BaseBean.newInstance(ReferenceCode.class, getUser().getPersistenceKey());
        mockup.setCode("01");
        m_periodHelper = new PeriodHelper(getOrganization(), m_context, mockup, getBroker());
        m_tnPeriodHelper = new TNReportingPeriodHelper(getOrganization(), m_context, mockup, getBroker());

        // Annual Mode - used by default.
        switch (m_reportMode.intValue()) {
            // Report Date mode
            case 2:
                m_reportDate = (PlainDate) getParameter(INPUT_REPORT_DATE);
                if (m_reportDate == null || (m_startDate != null && m_reportDate.before(m_startDate))) {
                    String errorMessage =
                            "Report can not be generated. Selected Report Date: " + m_reportDate.toString() +
                                    " is before context start date: " + m_startDate.toString();
                    AppGlobals.getLog().log(Level.SEVERE, errorMessage);
                    throw new IllegalArgumentException(errorMessage);
                } else if (m_reportDate != null && m_endDate != null && !m_reportDate.before(m_endDate)) {
                    String warningMessage = "Selected Report Date: " + m_reportDate.toString() +
                            " is after context end date: " + m_endDate.toString() +
                            ". Report Date set context end date.";
                    AppGlobals.getLog().log(Level.WARNING, warningMessage);
                } else {
                    m_endDate = m_reportDate;
                }
                m_periodHelper = new PeriodHelper(getOrganization(), m_context, null, getBroker());
                break;
            // Report Period Mode
            case 1:
                ReferenceCode reportPeriod = null;
                String reportPeriodOid = (String) getParameter(INPUT_REPORT_PERIOD);
                if (!StringUtils.isEmpty(reportPeriodOid)) {
                    reportPeriod = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, reportPeriodOid);
                } else {
                    String errorMessage = "Report period must be specified";
                    AppGlobals.getLog().log(Level.SEVERE, errorMessage);
                    throw new IllegalArgumentException(errorMessage);
                }
                m_periodHelper = new PeriodHelper(getOrganization(), m_context, reportPeriod, getBroker());
                addParameter(REPORT_PARAMETER_REPORT_PERIOD, reportPeriod.getCode());
                break;
        }

        addParameter(REPORT_PARAMETER_CONTEXT, m_context);
        addParameter(REPORT_PARAMETER_END_DATE, m_endDate);
        addParameter(REPORT_PARAMETER_USER, getUser());
    }

    /**
     * Helper method to add the membership data.
     *
     * @param membershipData Map<String,Map<String,Map<Integer,Integer>>>
     * @param student SisStudent
     * @param span TNStudentEnrollmentSpan
     * @param dataWasAdded Boolean
     * @return Boolean
     */
    private Boolean addMembershipData(Map<String, Map<String, Map<Integer, Integer>>> membershipData,
                                      SisStudent student,
                                      TNStudentEnrollmentSpan span,
                                      Boolean dataWasAdded) {
        ReferenceCode gradeCode = m_data.getGradeLevel(student, span);
        Integer gradeLevel = prepareGradeLevel(gradeCode);

        String gender = getPersonGender(student.getPerson());

        String raceStateCode = getRaceTypeDescription(student);
        if (gradeLevel != null && gender != null && raceStateCode != null && !gender.isEmpty() &&
                !raceStateCode.isEmpty() && !dataWasAdded.booleanValue()) {
            Integer currentMembershipLevel = membershipData.get(gender).get(raceStateCode).get(gradeLevel);
            membershipData.get(gender).get(raceStateCode).put(gradeLevel,
                    Integer.valueOf(currentMembershipLevel.intValue() + 1));
            dataWasAdded = Boolean.TRUE;

            // Add data for District summary.
            if (m_isAllSchools) {
                Integer districtCurrentMembershipLevel =
                        m_districtMembershipData.get(gender).get(raceStateCode).get(gradeLevel);
                m_districtMembershipData.get(gender).get(raceStateCode).put(gradeLevel,
                        Integer.valueOf(districtCurrentMembershipLevel.intValue() + 1));
            }

        }
        return dataWasAdded;
    }

    /**
     * Calculate total values for columns in data structure.
     *
     * @param membershipData Map<String,Map<String,Map<Integer,Integer>>>
     */
    private void calculateColumnTotals(Map<String, Map<String, Map<Integer, Integer>>> membershipData) {
        for (String genderCodes : new String[] {STATE_CODE_GENDER_MALE, STATE_CODE_GENDER_FEMALE, STATE_CODE_TOTAL}) {
            for (Integer gradeCode : membershipData.get(genderCodes).get(RACE_TYPE_HISPANIC_ALL_DESCRIPTION).keySet()) {
                int totalValueForColumn = 0;
                for (String raceCodeState : membershipData.get(genderCodes).keySet()) {
                    totalValueForColumn += membershipData.get(genderCodes).get(raceCodeState).get(gradeCode).intValue();
                }
                membershipData.get(genderCodes).get(RACE_TOTAL_DESCRIPTION).put(gradeCode,
                        Integer.valueOf(totalValueForColumn));
            }
        }
    }

    /**
     * Calculate values in data set which represent Total table in report.
     *
     * @param membershipData Map<String,Map<String,Map<Integer,Integer>>>
     */
    private void calculateTotalDataSet(Map<String, Map<String, Map<Integer, Integer>>> membershipData) {
        for (String genderCodes : new String[] {STATE_CODE_GENDER_MALE, STATE_CODE_GENDER_FEMALE}) {
            for (String raceCodeState : membershipData.get(genderCodes).keySet()) {
                int totalByRaceAndGender = 0;
                for (Integer gradeCode : membershipData.get(genderCodes).get(raceCodeState).keySet()) {
                    int currentMembershipValue =
                            membershipData.get(STATE_CODE_TOTAL).get(raceCodeState).get(gradeCode).intValue();
                    int genderMembershipValue =
                            membershipData.get(genderCodes).get(raceCodeState).get(gradeCode).intValue();
                    if (gradeCode.intValue() != GRADE_LEVEL_NUMERIC_TOTAL_K_12) {
                        totalByRaceAndGender += genderMembershipValue;
                    } else {
                        genderMembershipValue = totalByRaceAndGender;
                    }
                    membershipData.get(STATE_CODE_TOTAL).get(raceCodeState).put(gradeCode,
                            Integer.valueOf(currentMembershipValue +
                                    genderMembershipValue));
                }
                membershipData.get(genderCodes).get(raceCodeState).put(Integer.valueOf(GRADE_LEVEL_NUMERIC_TOTAL_K_12),
                        Integer.valueOf(totalByRaceAndGender));
            }
        }
    }

    /**
     * Populate grade data structure with default values.
     *
     * @return Map<Integer, Integer>
     */
    private Map<Integer, Integer> getDefaultGrades() {
        Map<Integer, Integer> gradeMap = new LinkedHashMap<Integer, Integer>();
        for (int i = GRADE_FILTER_MIN_LEVEL; i <= GRADE_FILTER_MAX_LEVEL; i++) {
            gradeMap.put(Integer.valueOf(i), Integer.valueOf(0));
        }

        gradeMap.put(Integer.valueOf(GRADE_LEVEL_NUMERIC_TOTAL_K_12), Integer.valueOf(0));

        return gradeMap;
    }

    /**
     * Populate data structure with default predefined values.
     *
     * @return Map<String, Map<String, Map<Integer, Integer>>>
     */
    private Map<String, Map<String, Map<Integer, Integer>>> getDefaultMembership() {
        Map<String, Map<String, Map<Integer, Integer>>> defaultMembershipTable =
                new LinkedHashMap<String, Map<String, Map<Integer, Integer>>>();
        defaultMembershipTable.put(STATE_CODE_GENDER_MALE, new LinkedHashMap<String, Map<Integer, Integer>>());
        defaultMembershipTable.put(STATE_CODE_GENDER_FEMALE, new LinkedHashMap<String, Map<Integer, Integer>>());
        defaultMembershipTable.put(STATE_CODE_TOTAL, new LinkedHashMap<String, Map<Integer, Integer>>());

        // populate default races
        for (String genderCode : defaultMembershipTable.keySet()) {
            defaultMembershipTable.get(genderCode).put(RACE_TYPE_HISPANIC_ALL_DESCRIPTION, getDefaultGrades());
            defaultMembershipTable.get(genderCode).put(RACE_TYPE_NON_HISPANIC_WHITE_DESCRIPTION, getDefaultGrades());
            defaultMembershipTable.get(genderCode).put(RACE_TYPE_NON_HISPANIC_BLACK_DESCRIPTION, getDefaultGrades());
            defaultMembershipTable.get(genderCode).put(RACE_TYPE_NON_HISPANIC_ASIAN_DESCRIPTION, getDefaultGrades());
            defaultMembershipTable.get(genderCode).put(RACE_TYPE_NON_HISPANIC_INDIAN_DESCRIPTION, getDefaultGrades());
            defaultMembershipTable.get(genderCode).put(RACE_TYPE_NON_HISPANIC_PACIFIC_DESCRIPTION, getDefaultGrades());
            defaultMembershipTable.get(genderCode).put(RACE_TYPE_2_OR_MORE_DESCRIPTION, getDefaultGrades());
            defaultMembershipTable.get(genderCode).put(RACE_TOTAL_DESCRIPTION, getDefaultGrades());
        }

        return defaultMembershipTable;
    }

    /**
     * Return person gender state code.
     *
     * @param person SisPerson
     * @return String
     */
    private String getPersonGender(SisPerson person) {
        String gender = EMPTY_STRING;
        ReferenceCode genderCode = m_referenceGenderCodeMap.get(person.getGenderCode());
        if (genderCode != null) {
            gender = genderCode.getStateCode();
        }
        return gender;
    }

    /**
     * Return race state code by race code representation.
     *
     * @param raceCode String
     * @return String
     */
    private String getRaceStateCode(String raceCode) {
        String race = EMPTY_STRING;
        ReferenceCode referenceRaceCode = m_referenceRaceCodeMap.get(raceCode);
        if (referenceRaceCode != null) {
            race = referenceRaceCode.getStateCode();
        }
        return race;
    }

    /**
     * Get Race type description based on hispanic/latino indicator and race code.
     *
     * @param student SisStudent
     * @return String
     */
    private String getRaceTypeDescription(SisStudent student) {
        String raceTypeDescription = EMPTY_STRING;

        Collection<Race> races = m_data.getStudentRace(student);
        boolean isHispanic = student.getPerson().getHispanicLatinoIndicator();

        if (isHispanic) {
            raceTypeDescription = RACE_TYPE_HISPANIC_ALL_DESCRIPTION;
        } else {
            if (races != null && races.size() > 1) {
                raceTypeDescription = RACE_TYPE_2_OR_MORE_DESCRIPTION;
            } else if (races != null && races.size() == 1) {
                String localRaceCode = races.iterator().next().getRaceCode();
                String stateRaceCode = getRaceStateCode(localRaceCode);
                if (stateRaceCode.equalsIgnoreCase(RACE_CODE_BLACK)) {
                    raceTypeDescription = RACE_TYPE_NON_HISPANIC_BLACK_DESCRIPTION;
                } else if (stateRaceCode.equalsIgnoreCase(RACE_CODE_WHITE)) {
                    raceTypeDescription = RACE_TYPE_NON_HISPANIC_WHITE_DESCRIPTION;
                } else if (stateRaceCode.equalsIgnoreCase(RACE_CODE_ASIAN)) {
                    raceTypeDescription = RACE_TYPE_NON_HISPANIC_ASIAN_DESCRIPTION;
                } else if (stateRaceCode.equalsIgnoreCase(RACE_CODE_INDIAN)) {
                    raceTypeDescription = RACE_TYPE_NON_HISPANIC_INDIAN_DESCRIPTION;
                } else if (stateRaceCode.equalsIgnoreCase(RACE_CODE_PACIFIC)) {
                    raceTypeDescription = RACE_TYPE_NON_HISPANIC_PACIFIC_DESCRIPTION;
                }
            }
        }

        return raceTypeDescription;
    }

    /**
     * Return collection of schools.
     *
     * @return Collection<SisSchool> collection of schools
     */
    private Collection<SisSchool> getSchools() {
        Collection<SisSchool> schools = null;
        Object objIsAllSchools = getParameter(INPUT_ALL_SCHOOLS);
        m_isAllSchools = objIsAllSchools == null ? false : ((Boolean) objIsAllSchools).booleanValue();
        if (m_isAllSchools) {
            X2Criteria schoolCriteria = new X2Criteria();

            schoolCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            schoolCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

            DataDictionaryField aliasSklStateIDField = DataDictionary.getDistrictDictionary(
                    getUser().getPersistenceKey()).findDataDictionaryFieldByAlias(ALIAS_SKL_STATE_ID);
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

        return schools;
    }

    /**
     * Load gender codes.
     */
    private void loadGenderCodes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty prop =
                new ModelProperty(SisPerson.class, SisPerson.COL_GENDER_CODE, getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
        ReferenceTable referenceTable = field.getReferenceTable();
        m_referenceGenderCodeMap = referenceTable.getCodeMap();
    }

    /**
     * Load race codes.
     */
    private void loadRaceCodes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty prop = new ModelProperty(Race.class, Race.COL_RACE_CODE, getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
        ReferenceTable referenceTable = field.getReferenceTable();
        m_referenceRaceCodeMap = referenceTable.getCodeMap();
    }

    /**
     * Populate the grid.
     *
     * @param schoolGrid ReportDataGrid
     * @param orgName String
     * @param membershipData Map<String,Map<String,Map<Integer,Integer>>>
     * @param school SisSchool
     * @param calendar SchoolCalendar
     */
    private void populateGrid(ReportDataGrid schoolGrid,
                              String orgName,
                              Map<String, Map<String, Map<Integer, Integer>>> membershipData,
                              SisSchool school,
                              SchoolCalendar calendar) {
        PlainDate dateStart = m_startDate;
        PlainDate dateEnd = m_endDate;

        for (String tableType : membershipData.keySet()) {
            Map<String, Map<Integer, Integer>> raceDataSet = membershipData.get(tableType);

            for (String race : raceDataSet.keySet()) {
                schoolGrid.append();
                schoolGrid.set(REPORT_FIELD_REPORT_TYPE, tableType);
                schoolGrid.set(REPORT_FIELD_SCHOOL, school);
                schoolGrid.set(REPORT_FIELD_ORG_NAME, orgName);
                schoolGrid.set(REPORT_FIELD_RACE, race);
                schoolGrid.set(REPORT_FIELD_DATE_START, dateStart);
                schoolGrid.set(REPORT_FIELD_DATE_END, dateEnd);
                schoolGrid.set(REPORT_FIELD_CALENDAR_NUMBER, calendar);

                Map<Integer, Integer> gradeDataSet = raceDataSet.get(race);

                for (Integer gradeCode : gradeDataSet.keySet()) {
                    switch (gradeCode.intValue()) {
                        case GRADE_FILTER_MIN_LEVEL:
                            schoolGrid.set(REPORT_FIELD_GRADE_K, gradeDataSet.get(gradeCode));
                            break;
                        case GRADE_LEVEL_NUMERIC_TOTAL_K_12:
                            schoolGrid.set(REPORT_FIELD_GRADE_TOTAL, gradeDataSet.get(gradeCode));
                            break;
                        default:
                            schoolGrid.set("grade" + gradeCode.toString(), gradeDataSet.get(gradeCode));
                            break;
                    }
                }
            }
        }
    }

    /**
     * Fill in student membership data based on information about gender/race/grade/enrollment code
     * level.
     *
     * @param students QueryIterator
     * @return Map<String, Map<String, Map<Integer, Integer>>>
     */
    private Map<String, Map<String, Map<Integer, Integer>>> populateMembershipData(QueryIterator students) {
        Map<String, Map<String, Map<Integer, Integer>>> membershipData = getDefaultMembership();

        while (students.hasNext()) {
            Boolean dataWasAdded = Boolean.FALSE;
            SisStudent student = (SisStudent) students.next();
            SisSchool school = (SisSchool) m_data.getSchool();
            StudentEnrollment latestEnrollment =
                    m_data.getStudentEnrollmentForDate(student, m_endDate, ENROLLMENT_TYPES);

            if (latestEnrollment == null) {
                continue;
            }

            /*
             * Return all student enrollment spans, even from previous years.
             *
             * NOTE: It shouldn't break on YOG or Status Change. We only want to get the E and W
             * enrollment spans.
             */
            List<TNStudentEnrollmentSpan> spans = m_data.getStudentEnrollmentSpans(student, false);

            NetEnrolledInfo netEnrInfo = m_tnPeriodHelper.getStudentNetEnrolled(student, spans, m_startDate);
            if (netEnrInfo != null && school.getOid().equals(netEnrInfo.getSchool().getOid())) {
                dataWasAdded = addMembershipData(membershipData, student, netEnrInfo.getEnrSpan(), dataWasAdded);
            }
        }

        return membershipData;
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
        if (grade >= GRADE_FILTER_MIN_LEVEL && grade <= GRADE_FILTER_MAX_LEVEL) {
            preparedGradeLevel = Integer.valueOf((grade));
        }

        return preparedGradeLevel;
    }
}
