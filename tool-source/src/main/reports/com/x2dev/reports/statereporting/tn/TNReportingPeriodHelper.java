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

package com.x2dev.reports.statereporting.tn;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentEnrollmentSpan;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentMultiYearHelper;
import com.x2dev.procedures.statereporting.tn.TNStateReportData.Pair;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Helper class than helps calculate reporting period dates range.
 *
 * @author X2 Development Corporation
 */
public class TNReportingPeriodHelper {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Helper class than stores net enrolled data for the student.
     *
     * @author X2 Development Corporation
     */
    class NetEnrolledInfo {
        private PlainDate m_enrDate;
        private StudentEnrollment m_entryEnr;
        private SisSchool m_school;
        private TNStudentEnrollmentSpan m_tnSpan;

        /**
         * Public constructor.
         *
         * @param enrDate PlainDate
         * @param school SisSchool
         * @param tnSpan TNStudentEnrollmentSpan
         */
        public NetEnrolledInfo(PlainDate enrDate, SisSchool school, TNStudentEnrollmentSpan tnSpan) {
            super();
            this.m_enrDate = enrDate;
            this.m_school = school;
            this.m_tnSpan = tnSpan;
        }

        /**
         * Getter for enrollment code
         *
         * @return
         */
        public StudentEnrollment getEntryEnrollment() {
            return m_entryEnr;
        }

        /**
         * Gets the enr date.
         *
         * @return the m_enrDate
         */
        public PlainDate getEnrDate() {
            return m_enrDate;
        }

        /**
         * Gets the school.
         *
         * @return the m_school
         */
        public SisSchool getSchool() {
            return m_school;
        }

        /**
         * Gets the enr span.
         *
         * @return the m_tnSpan
         */
        public TNStudentEnrollmentSpan getEnrSpan() {
            return m_tnSpan;
        }

        /**
         * Setter for enrollment code
         *
         * @param entryEnrolllment
         */
        public void setEntryEnrollment(StudentEnrollment entryEnrolllment) {
            this.m_entryEnr = entryEnrolllment;
        }
    }

    public static final String KEY_DATE_BEGIN = "DATE_BEGIN";

    public static final String KEY_DATE_END = "DATE_END";

    public static final String KEY_PERIOD_DATE_TOTAL = "total";

    /**
     * Constants: Aliases, Fields, IDs, Parameters
     */
    private static final int REPORT_PERIOD_LENGTH = 20;

    private static final String ALIAS_CALENDAR_EVENT_TYPE_2 = "DOE DAY EVENT TYPE 2";
    private static final String ALIAS_CALENDAR_EVENT_TYPE_3 = "DOE DAY EVENT TYPE 3";
    private static final String ALIAS_IEP_TYPE = "EasyIEP Type";
    private static final String ALIAS_INSTR_SERVICE_TYPE = "DOE INSTR SERVICE TYPE";
    private static final String ALIAS_STUDENT_INELIGIBILITY_STATUS = "DOE STUDENT INELIGIBILITY";

    private static final String ATTENDANCE_START_EVENT_CODE = "AS";
    private static final String CALENDAR_START_EVENT_CODE = "CS";

    private static final String DISTRICT_SCHOOL_CODE = "dist";

    private static final String IEP_TYPE_PRIMARY = "P";

    private static final String INSTR_SERVICE_TYPE_SECONDARY_SCHOOL_CODE = "S";

    private static final String NET_ENROLLMENT_E = "E";
    private static final String NET_ENROLLMENT_E1 = "E1";
    private static final String NET_ENROLLMENT_EC = "EC";
    private static final String NET_ENROLLMENT_TC = "TC";
    private static final String NET_ENROLLMENT_TR = "TR";

    private static final int N_GRADE_LEVEL_MAX = 9;
    private static final int N_GRADE_LEVEL_MIN = 7;

    private static final String PERIOD_CODE_FALL_BEGIN = "01";
    private static final String PERIOD_CODE_FALL_END = "04";
    private static final String PERIOD_CODE_SPRING_BEGIN = "05";
    private static final String PERIOD_CODE_SPRING_END = "09";

    private static final ArrayList<String> PRE_KINDERGARTEN_GRADE_LEVEL_CODES =
            new ArrayList<String>(Arrays.asList("PK", "P3", "P4"));

    private static final String STATE_CODE_FUNDING_INELIGIBILITY_EXCLUDED_OUT_OF_STATE = "1";
    private static final String STATE_CODE_FUNDING_INELIGIBILITY_EXCLUDED_I20_STUDENT = "2";

    private static final String YEAR_CODE = "YEAR";

    /**
     * Adjust student criteria.
     *
     * @param criteria X2Criteria
     * @param schools Collection<SisSchool>
     * @param organization Organization
     * @param currentContext DistrictSchoolYearContext
     */
    public static void adjustStudentCriteria(X2Criteria criteria,
                                             Collection<SisSchool> schools,
                                             Organization organization,
                                             DistrictSchoolYearContext currentContext) {
        Collection<String> oids = new ArrayList(schools.size());
        for (SisSchool school : schools) {
            oids.add(school.getOid());
        }

        TNStudentMultiYearHelper multiYearHelper = new TNStudentMultiYearHelper(organization, currentContext, null);

        X2Criteria enrollmentCriteria = new X2Criteria();
        enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, currentContext.getStartDate());
        enrollmentCriteria.addIn(StudentEnrollment.COL_SCHOOL_OID, oids);

        SubQuery enrollmentSubQuery =
                new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, enrollmentCriteria);
        X2Criteria enrCriteria = new X2Criteria();
        enrCriteria.addIn(X2BaseBean.COL_OID, enrollmentSubQuery);

        X2Criteria activeCriteria = new X2Criteria();
        activeCriteria.addAndCriteria(multiYearHelper.getActiveStudentCriteria());
        activeCriteria.addIn(multiYearHelper.getSchoolOidField(), oids);

        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addOrCriteria(enrCriteria);
        orCriteria.addOrCriteria(activeCriteria);

        criteria.addAndCriteria(orCriteria);
    }

    /**
     * Check whether the two given range of dates (start and end dates) overlap or not.
     *
     * @param startDate1 PlainDate
     * @param endDate1 PlainDate
     * @param startDate2 PlainDate
     * @param endDate2 PlainDate
     * @return boolean check if dates overlap
     */
    public static boolean doDatesOverlap(PlainDate startDate1,
                                         PlainDate endDate1,
                                         PlainDate startDate2,
                                         PlainDate endDate2) {
        if ((endDate1 != null && endDate2 != null
                && (DateUtils.isBetween(startDate2, startDate1, endDate1)
                        || DateUtils.isBetween(endDate2, startDate1, endDate1)
                        || DateUtils.isBetween(startDate1, startDate2, endDate2)
                        || DateUtils.isBetween(endDate1, startDate2, endDate2)))
                ||
                (endDate1 != null && endDate2 == null && startDate2 != null && !startDate2.after(endDate1)) ||
                (endDate1 == null && endDate2 != null && startDate1 != null && !startDate1.after(endDate2)) ||
                (endDate1 == null && endDate2 == null)) {
            return true;
        }
        return false;
    }

    /**
     * Check whether the given date is within the start and end dates. The start date cannot be
     * null.
     * If the end date is null, then check if the given date is after the start date.
     *
     * @param dateToCheck PlainDate
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return boolean check if the given date is between the given range of dates (start and end
     *         dates)
     */
    public static boolean isDateWithinRange(PlainDate dateToCheck, PlainDate startDate, PlainDate endDate) {
        if (dateToCheck != null && startDate != null) // if the start date or the date to check is
                                                      // null, return false
        {
            if ((endDate != null && DateUtils.isBetween(dateToCheck, startDate, endDate)) ||
                    (endDate == null && !dateToCheck.before(startDate))) {
                return true;
            }
        }
        return false;
    }

    /**
     * Check if the given date is within the given N grade level period date range.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @param nGradeLevelStartEndDates List<Pair<PlainDate,PlainDate>>
     * @return boolean
     */
    public static boolean isStudentNGradeLevelDuringDateRange(PlainDate startDate,
                                                              PlainDate endDate,
                                                              List<Pair<PlainDate, PlainDate>> nGradeLevelStartEndDates) {
        for (Pair<PlainDate, PlainDate> startEndDates : nGradeLevelStartEndDates) {
            PlainDate start = startEndDates.getLeft();
            PlainDate end = startEndDates.getRight();

            if (TNReportingPeriodHelper.doDatesOverlap(startDate, endDate, start, end)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Check if the given date is within the given N grade level period date range.
     *
     * @param dateToCheck PlainDate
     * @param enrollmentDate PlainDate
     * @param nGradeLevelStartEndDates List<Pair<PlainDate,PlainDate>>
     * @return boolean
     */
    public static boolean isStudentNGradeLevelOnGivenDate(PlainDate dateToCheck,
                                                          PlainDate enrollmentDate,
                                                          List<Pair<PlainDate, PlainDate>> nGradeLevelStartEndDates) {
        boolean isEnrollmentDateEmpty = (enrollmentDate == null);
        for (Pair<PlainDate, PlainDate> startEndDates : nGradeLevelStartEndDates) {
            PlainDate startDate = startEndDates.getLeft();
            PlainDate endDate = startEndDates.getRight();

            if ((!isEnrollmentDateEmpty && enrollmentDate.equals(startDate))
                    || TNReportingPeriodHelper.isDateWithinRange(dateToCheck, startDate, endDate)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Get the district start / school year date (the first date with 'CS' (Calendar Start) event
     * code).
     *
     * @param context DistrictSchoolYearContext
     * @param broker X2Broker
     * @return PlainDate
     */
    public static PlainDate getDistrictCalendarYearStart(DistrictSchoolYearContext context, X2Broker broker) {
        return getDistrictFirstDateByType(context, broker, CALENDAR_START_EVENT_CODE);
    }

    /**
     * Get the district start / school year date (the first date with 'AS' (Attendance Start) event
     * code).
     *
     * @param currentContext DistrictSchoolYearContext
     * @param broker X2Broker
     * @return PlainDate
     */
    public static PlainDate getDistrictYearStart(DistrictSchoolYearContext currentContext, X2Broker broker) {
        return getDistrictFirstDateByType(currentContext, broker, ATTENDANCE_START_EVENT_CODE);
    }

    /**
     * Get the date of the first occurrence of a particular event type in the calendar.
     *
     * @param currentContext DistrictSchoolYearContext
     * @param broker X2Broker
     * @param eventType String
     * @return Plain date
     */
    private static PlainDate getDistrictFirstDateByType(DistrictSchoolYearContext currentContext,
                                                        X2Broker broker,
                                                        String eventType) {
        PlainDate startDate = null;

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(broker.getPersistenceKey());
        DataDictionaryField aliasEventType2 = dictionary.findDataDictionaryFieldByAlias(ALIAS_CALENDAR_EVENT_TYPE_2);
        DataDictionaryField aliasEventType3 = dictionary.findDataDictionaryFieldByAlias(ALIAS_CALENDAR_EVENT_TYPE_3);

        X2Criteria eventTypeCriteria = new X2Criteria();
        eventTypeCriteria.addEqualTo(SchoolCalendarDate.COL_SCHEDULE_DAY_TYPE, eventType);
        if (aliasEventType2 != null) {
            eventTypeCriteria.addOrEqualTo(aliasEventType2.getJavaName(), eventType);
        }
        if (aliasEventType3 != null) {
            eventTypeCriteria.addOrEqualTo(aliasEventType3.getJavaName(), eventType);
        }

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(
                SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                currentContext.getOid());
        criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.REL_SCHOOL
                + PATH_DELIMITER + SisSchool.COL_SCHOOL_ID, DISTRICT_SCHOOL_CODE);
        criteria.addAndCriteria(eventTypeCriteria);

        QueryByCriteria yearStartQuery = new QueryByCriteria(SchoolCalendarDate.class, criteria);
        yearStartQuery.addOrderByAscending(SchoolCalendarDate.COL_DATE);

        QueryIterator startDates = broker.getIteratorByQuery(yearStartQuery);
        try {
            if (startDates.hasNext()) {
                SchoolCalendarDate date = (SchoolCalendarDate) startDates.next();
                startDate = date.getDate();
            }
        } finally {
            if (startDates != null) {
                startDates.close();
            }
        }

        return startDate;
    }

    /**
     * Pad zeros to the left of the string to match month codes.
     *
     * @param n number of places
     * @param padding number of zeros to pad
     * @return formatted String
     */
    private static String leftPad(int n, int padding) {
        return String.format("%0" + padding + "d", Integer.valueOf(n));
    }

    /**
     * Fields
     */
    private X2Broker m_broker;
    private PlainDate m_calendarBeginDate;
    private DistrictSchoolYearContext m_currentContext;
    private Set<String> m_enrolledCodes;
    private ReferenceCode m_month;
    private TNStudentMultiYearHelper m_multiYearHelper;
    private Organization m_organization;
    private String m_periodBeginCode;
    private String m_periodEndCode;
    private Collection<String> m_programCodes;
    private Collection<String> m_programCodesIEA;
    private Map<String, ReferenceCode> m_programCodeMap;
    private Map<String, ReferenceCode> m_programCodeMapIEA;
    private Map<String, Map<String, Map<String, PlainDate>>> m_sclDatesMap;
    private Map<String, Map<String, Set<PlainDate>>> m_sclDatesSet;
    private Map<String, NetEnrolledInfo> m_stdNetEnrollMap;
    private Criteria m_studentCriteria;
    private Map<String, Collection<StudentProgramParticipation>> m_studentIEAProgramMap;
    private Map<String, Collection<StudentProgramParticipation>> m_studentIneligibilityProgramMap;
    private Map<String, Collection<StudentProgramParticipation>> m_studentNGradeLevelProgramMap;
    private Set<String> m_transferredCodes;
    private PlainDate m_yearBeginDate;
    private PlainDate m_yearEndDate;

    /**
     * Public constructor.
     *
     * @param organization Organization
     * @param currentContext Current context
     * @param month Selected month
     * @param broker X2Broker
     */
    public TNReportingPeriodHelper(Organization organization, DistrictSchoolYearContext currentContext,
            ReferenceCode month,
            X2Broker broker) {
        m_broker = broker;
        m_currentContext = currentContext;
        m_month = month;
        m_multiYearHelper = new TNStudentMultiYearHelper(organization, currentContext, broker);
        m_organization = organization;

        /*
         * Get the year begin from the district calendar, the date with 'AS' code (Attendance Start)
         *
         * The default values are the start date of the district
         */
        PlainDate yearStart = getDistrictYearStart(m_currentContext, broker);
        if (yearStart != null) {
            m_yearBeginDate = yearStart;
        } else {
            m_yearBeginDate = m_currentContext.getStartDate();
        }

        /*
         * Get the year begin from the district calendar, the date with 'CS' code (Calendar Start)
         *
         * The default values are the start date of the district
         */
        yearStart = getDistrictCalendarYearStart(m_currentContext, broker);
        if (yearStart != null) {
            m_calendarBeginDate = yearStart;
        } else {
            m_calendarBeginDate = m_currentContext.getStartDate();
        }

        // Put the end district date for now.
        m_yearEndDate = m_currentContext.getEndDate();
        initializeSclDatesMap();
        initializeEnrollmentCodes();
    }

    /**
     * Get the calendar start date of the district school year.
     *
     * @return PlainDate yearBeginDate
     */
    public PlainDate getCalendarBeginDate() {
        return m_calendarBeginDate;
    }

    /**
     * Get Collection of days in session.
     *
     * @param sclOid String
     * @return Collection<PlainDate> the collection of dates for the reporting period
     */
    public Collection<PlainDate> getDaysInSession(String sclOid) {
        if (m_sclDatesSet != null && m_sclDatesSet.containsKey(sclOid)) {
            String code = m_month == null ? YEAR_CODE : m_month.getCode();
            if (m_sclDatesSet.get(sclOid).containsKey(code)) {
                return m_sclDatesSet.get(sclOid).get(code);
            }
        }
        return new HashSet<PlainDate>();
    }

    /**
     * Get Collection of days in session.
     *
     * @param sclOid String
     * @return Collection<PlainDate> the collection of dates for the reporting period
     */
    public Collection<PlainDate> getDaysInSessionYear(String sclOid) {
        if (m_sclDatesSet != null && m_sclDatesSet.containsKey(sclOid)) {
            if (m_sclDatesSet.get(sclOid).containsKey(YEAR_CODE)) {
                return m_sclDatesSet.get(sclOid).get(YEAR_CODE);
            }
        }
        return new HashSet<PlainDate>();
    }

    /**
     * Get the Date Begin.
     *
     * @param sclOid String
     * @return PlainDate the Begin Date of the reporting period
     */
    public PlainDate getDateBegin(String sclOid) {
        PlainDate startDate = null;
        if (m_sclDatesMap != null && m_sclDatesMap.containsKey(sclOid)) {
            String code = m_month == null ? YEAR_CODE : m_month.getCode();
            if (m_sclDatesMap.get(sclOid).containsKey(code)) {
                startDate = m_sclDatesMap.get(sclOid).get(code).get(KEY_DATE_BEGIN);
            }
        }
        return startDate;
    }

    /**
     * Get the Date End.
     *
     * @param sclOid String
     * @return PlainDate the End Date of the reporting period
     */
    public PlainDate getDateEnd(String sclOid) {
        PlainDate endDate = null;
        if (m_sclDatesMap != null && m_sclDatesMap.containsKey(sclOid)) {
            String code = m_month == null ? YEAR_CODE : m_month.getCode();
            if (m_sclDatesMap.get(sclOid).containsKey(code)) {
                endDate = m_sclDatesMap.get(sclOid).get(code).get(KEY_DATE_END);
            }
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
     * Gets the scl dates map.
     *
     * @return the m_sclDatesMap
     */
    public Map<String, Map<String, Map<String, PlainDate>>> getSclDatesMap() {
        return m_sclDatesMap;
    }

    /**
     * Returns map of semester begin dates for each school from the input collection (key is school
     * oid).
     * Also returns the earliest date with key "total"
     *
     * @param schools Collection<School>
     * @return Map
     */
    public Map<String, PlainDate> getSemesterBeginDates(Collection<School> schools) {
        m_month = X2BaseBean.newInstance(ReferenceCode.class, m_broker.getPersistenceKey());
        m_month.setCode(m_periodBeginCode);
        Map<String, PlainDate> semesterBeginDates = new HashMap<String, PlainDate>();
        PlainDate totalBeginDate = null;
        for (School school : schools) {
            PlainDate beginDate = getDateBegin(school.getOid());
            if (beginDate != null) {
                semesterBeginDates.put(school.getOid(), beginDate);
                if (totalBeginDate == null || totalBeginDate.after(beginDate)) {
                    totalBeginDate = beginDate;
                }
            }
        }
        semesterBeginDates.put(KEY_PERIOD_DATE_TOTAL, totalBeginDate);
        return semesterBeginDates;
    }

    /**
     * Returns map of semester end dates for each school from the input collection (key is school
     * oid).
     * Also returns the latest date with key "total"
     *
     * @param schools Collection<School>
     * @return Map
     */
    public Map<String, PlainDate> getSemesterEndDates(Collection<School> schools) {
        m_month = X2BaseBean.newInstance(ReferenceCode.class, m_broker.getPersistenceKey());
        m_month.setCode(m_periodEndCode);
        Map<String, PlainDate> semesterEndDates = new HashMap<String, PlainDate>();
        PlainDate totalEndDate = null;
        for (School school : schools) {
            PlainDate endDate = getDateBegin(school.getOid());
            if (endDate != null) {
                semesterEndDates.put(school.getOid(), endDate);
                if (totalEndDate == null || totalEndDate.before(endDate)) {
                    totalEndDate = endDate;
                }
            }
        }
        semesterEndDates.put(KEY_PERIOD_DATE_TOTAL, totalEndDate);
        return semesterEndDates;
    }

    /**
     * Return the list of pair of start and end dates of IEA program participations.
     * It will check for overlap dates, and returns only different period of dates.
     *
     * @param student SisStudent
     * @param reportStartDate PlainDate
     * @param reportEndDate PlainDate
     * @param resultsNGrade
     * @return List<Pair<PlainDate, PlainDate>> list of pair of dates (start and end dates) of
     *         student program participation N Grade levels
     */
    public List<Pair<PlainDate, PlainDate>> getStudentIEAPgmsRange(SisStudent student,
                                                                   PlainDate reportStartDate,
                                                                   PlainDate reportEndDate,
                                                                   List<Pair<PlainDate, PlainDate>> resultsNGrade) {
        List<Pair<PlainDate, PlainDate>> tempResults = new ArrayList<Pair<PlainDate, PlainDate>>();
        List<Pair<PlainDate, PlainDate>> results = new ArrayList<Pair<PlainDate, PlainDate>>();
        Collection<StudentProgramParticipation> participations = getIEAPrograms(student);
        for (StudentProgramParticipation participation : participations) {
            // If this studentProgramParticipation is categorized as N Grade Level, then add the
            // start and end date list to it
            if (doDatesOverlap(reportStartDate, reportEndDate, participation.getStartDate(),
                    participation.getEndDate())) {
                boolean datesOverlap = false;
                // If the list is not empty, check for any dates overlaps
                if (!tempResults.isEmpty()) {
                    PlainDate newStartDate = participation.getStartDate();
                    PlainDate newEndDate = participation.getEndDate();
                    for (int i = 0; i < tempResults.size(); i++) {
                        PlainDate oldStartDate = tempResults.get(i).getLeft();
                        PlainDate oldEndDate = tempResults.get(i).getRight();

                        // Check if the dates overlap
                        if (doDatesOverlap(oldStartDate, oldEndDate, newStartDate, newEndDate)) {
                            PlainDate tempStartDate;
                            PlainDate tempEndDate;
                            // Check which start date to use. NOTE: Start date cannot be null.
                            if (newStartDate.before(oldStartDate)) {
                                tempStartDate = newStartDate;
                            } else {
                                tempStartDate = oldStartDate;
                            }
                            // Check which end date to use. If one of them is null, then use the
                            // null. Otherwise, use the later one
                            if (oldEndDate != null && newEndDate != null) {
                                if (newEndDate.after(oldEndDate)) {
                                    tempEndDate = newEndDate;
                                } else {
                                    tempEndDate = oldEndDate;
                                }
                            } else {
                                tempEndDate = null;
                            }

                            // Set the new start and end date to the list
                            tempResults.set(i, Pair.of(tempStartDate, tempEndDate));

                            datesOverlap = true;
                            break;
                        }
                    }
                }

                // If the list is empty OR the dates are not overlapping, then just add it
                if (tempResults.isEmpty() || !datesOverlap) {
                    tempResults.add(Pair.of(participation.getStartDate(), participation.getEndDate()));
                }
            }
        }
        for (Pair<PlainDate, PlainDate> resultIEA : tempResults) {
            Pair<PlainDate, PlainDate> ieaPairToAdd1 = null;
            Pair<PlainDate, PlainDate> ieaPairToAdd2 = null;
            if (resultsNGrade != null && !resultsNGrade.isEmpty()) {
                boolean overlap = false;
                for (Pair<PlainDate, PlainDate> resultNGrade : resultsNGrade) {
                    if (doDatesOverlap(resultIEA.getLeft(), resultIEA.getRight(), resultNGrade.getLeft(),
                            resultNGrade.getRight())) {
                        overlap = true;
                        if (resultIEA.getLeft().before(resultNGrade.getLeft())) {
                            if (resultIEA.getRight() == null || resultNGrade.getRight() == null) {
                                ieaPairToAdd1 = Pair.of(resultIEA.getLeft(), resultNGrade.getLeft());
                            } else if (resultNGrade.getRight() != null
                                    && resultIEA.getRight().after(resultNGrade.getRight())) {
                                ieaPairToAdd1 = Pair.of(resultIEA.getLeft(), DateUtils.add(resultNGrade.getLeft(), -1));
                                ieaPairToAdd2 =
                                        Pair.of(DateUtils.add(resultNGrade.getRight(), 1), resultIEA.getRight());
                            }
                        } else {
                            if (resultIEA.getRight() == null && resultNGrade.getRight() != null) {
                                ieaPairToAdd1 =
                                        Pair.of(DateUtils.add(resultNGrade.getRight(), 1),
                                                m_currentContext.getEndDate());
                            } else if (resultIEA.getRight() != null && resultNGrade.getRight() != null
                                    && resultIEA.getRight().after(resultNGrade.getRight())) {
                                ieaPairToAdd1 =
                                        Pair.of(DateUtils.add(resultNGrade.getRight(), 1), resultIEA.getRight());
                            }
                        }
                        break;
                    }
                }
                if (!overlap) {
                    ieaPairToAdd1 = Pair.of(resultIEA.getLeft(),
                            resultIEA.getRight() != null ? resultIEA.getRight() : getYearEndDate());
                }
            } else {
                ieaPairToAdd1 = Pair.of(resultIEA.getLeft(),
                        resultIEA.getRight() != null ? resultIEA.getRight() : getYearEndDate());
            }

            if (ieaPairToAdd1 != null) {
                results.add(ieaPairToAdd1);
            }
            if (ieaPairToAdd2 != null) {
                results.add(ieaPairToAdd2);
            }
        }
        return results;
    }

    /**
     * Return the list of pair of start and end dates of N Grade Level program participations.
     * It will check for overlap dates, and returns only different period of dates.
     *
     * @param student SisStudent
     * @param reportStartDate PlainDate
     * @param reportEndDate PlainDate
     * @return List<Pair<PlainDate, PlainDate>> list of pair of dates (start and end dates) of
     *         student program participation N Grade levels
     */
    public List<Pair<PlainDate, PlainDate>> getStudentNGradeLevelRange(SisStudent student,
                                                                       PlainDate reportStartDate,
                                                                       PlainDate reportEndDate) {
        List<Pair<PlainDate, PlainDate>> results = new ArrayList<Pair<PlainDate, PlainDate>>();

        Collection<StudentProgramParticipation> programParticipations = getNGradeLevelPrograms(student);
        for (StudentProgramParticipation participation : programParticipations) {
            // If this studentProgramParticipation is categorized as N Grade Level, then add the
            // start and end date list to it
            if (isStudentNGradeLevelOverlaps(participation, reportStartDate, reportEndDate)) {
                boolean datesOverlap = false;

                // If the list is not empty, check for any dates overlaps
                if (!results.isEmpty()) {
                    PlainDate newStartDate = participation.getStartDate();
                    PlainDate newEndDate = participation.getEndDate();

                    for (int i = 0; i < results.size(); i++) {
                        PlainDate oldStartDate = results.get(i).getLeft();
                        PlainDate oldEndDate = results.get(i).getRight();

                        // Check if the dates overlap
                        if (doDatesOverlap(oldStartDate, oldEndDate, newStartDate, newEndDate)) {
                            PlainDate tempStartDate;
                            PlainDate tempEndDate;

                            // Check which start date to use. NOTE: Start date cannot be null.
                            if (newStartDate.before(oldStartDate)) {
                                tempStartDate = newStartDate;
                            } else {
                                tempStartDate = oldStartDate;
                            }

                            // Check which end date to use. If one of them is null, then use the
                            // null. Otherwise, use the later one
                            if (oldEndDate != null && newEndDate != null) {
                                if (newEndDate.after(oldEndDate)) {
                                    tempEndDate = newEndDate;
                                } else {
                                    tempEndDate = oldEndDate;
                                }
                            } else {
                                tempEndDate = null;
                            }

                            // Set the new start and end date to the list
                            results.set(i, Pair.of(tempStartDate, tempEndDate));

                            datesOverlap = true;
                            break;
                        }
                    }
                }

                // If the list is empty OR the dates are not overlapping, then just add it
                if (results.isEmpty() || !datesOverlap) {
                    results.add(Pair.of(participation.getStartDate(), participation.getEndDate()));
                }
            }
        }
        return results;
    }

    /**
     * Gets the student net enrolled.
     *
     * @param student SisStudent
     * @param spans TNStudentEnrollmentSpan
     * @param schoolStartDate PlainDate
     * @return Net enrolled info
     */
    public NetEnrolledInfo getStudentNetEnrolled(SisStudent student,
                                                 Collection<TNStudentEnrollmentSpan> spans,
                                                 PlainDate schoolStartDate) {
        NetEnrolledInfo netEnrInfo = null;

        if (m_stdNetEnrollMap == null) {
            m_stdNetEnrollMap = new HashMap<String, TNReportingPeriodHelper.NetEnrolledInfo>();
        }

        if (student != null && schoolStartDate != null) {
            TNStudentEnrollmentSpan operatedSpan = null;
            PlainDate operatedDate = null;
            String stdOid = student.getOid();

            if (!m_stdNetEnrollMap.keySet().contains(stdOid)) {
                for (TNStudentEnrollmentSpan span : spans) {
                    StudentEnrollment entryEnrollment = getEntryEnrollment(span);
                    if (entryEnrollment != null) {
                        PlainDate spanDate = span.getFirstActiveEnrollment().getEnrollmentDate();

                        if (spanDate != null) {
                            if (spanDate.before(schoolStartDate)) {
                                // get latest span before school start date
                                operatedSpan = span;
                                operatedDate = schoolStartDate;
                            } else {
                                // Use any span or any earlier span
                                if (operatedDate == null || spanDate.before(operatedDate)) {
                                    operatedSpan = span;
                                    operatedDate = spanDate;
                                }
                            }
                        }
                    }

                    if (span.getFirstInactiveEnrollment() != null &&
                            StudentEnrollment.WITHDRAWAL.equals(span.getFirstInactiveEnrollment().getEnrollmentType())
                            &&
                            operatedDate != null && span.getLastActiveDate().before(operatedDate) &&
                            span.getFirstInactiveEnrollment().getEnrollmentDate().before(operatedDate)) {
                        operatedDate = null;
                        operatedSpan = null;
                    }
                }

                StudentEnrollment entryEnr = null;
                if (operatedSpan != null && (entryEnr = getEntryEnrollment(operatedSpan)) != null) {
                    if (isStudentEnrolledOnGivenDate(entryEnr, operatedSpan.getSchool(), operatedDate)) {
                        netEnrInfo = new NetEnrolledInfo(operatedDate, operatedSpan.getSchool(), operatedSpan);
                        netEnrInfo.setEntryEnrollment(entryEnr);
                    }
                }
            } else {
                return m_stdNetEnrollMap.get(stdOid);
            }

            m_stdNetEnrollMap.put(stdOid, netEnrInfo);
        }

        return netEnrInfo;
    }

    /**
     * Getter for set of transferred codes.
     *
     * @return
     */
    public Set<String> getTransferredCodes() {
        return m_transferredCodes;
    }

    /**
     * Get the attendance start date of the district school year.
     *
     * @return PlainDate yearBeginDate
     */
    public PlainDate getYearBeginDate() {
        return m_yearBeginDate;
    }

    /**
     * Get the in-session end date of the calendar school year.
     *
     * @return PlainDate yearEndDate
     */
    public PlainDate getYearEndDate() {
        return m_yearEndDate;
    }

    /**
     * Check if student has 'ineligibility funding status' in his/her student program participation.
     * NOTE: we don't need to check the start and end date of the program participation since
     * if the student has it, it means that student will always have it.
     *
     * @param student SisStudent
     * @param reportBeginDate PlainDate
     * @param reportEndDate PlainDate
     * @return boolean isStudentIneligible
     */
    public boolean hasStudentIneligibilityFundingStatus(SisStudent student,
                                                        PlainDate reportBeginDate,
                                                        PlainDate reportEndDate) {
        boolean isStudentIneligible = false;

        Collection<StudentProgramParticipation> programParticipations = getIneligibiityPrograms(student);
        for (StudentProgramParticipation participation : programParticipations) {
            if (doDatesOverlap(reportBeginDate, reportEndDate, participation.getStartDate(),
                    participation.getEndDate())) {
                isStudentIneligible = true;
            }
        }

        return isStudentIneligible;
    }

    /**
     * Checks if is enrolled.
     *
     * @param spans List<TNStudentEnrollmentSpan>
     * @param schoolOid String
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return true, if is enrolled
     */
    public boolean isEnrolled(List<TNStudentEnrollmentSpan> spans,
                              String schoolOid,
                              PlainDate startDate,
                              PlainDate endDate) {
        boolean value = false;

        for (TNStudentEnrollmentSpan span : spans) {
            if (span.getSchool() != null && span.getSchool().getOid().equals(schoolOid) &&
                    doDatesOverlap(startDate, endDate, span.getFirstActiveDate(), span.getLastActiveDate())) {
                value = true;
                break;
            }
        }
        return value;
    }

    /**
     * Checks if is program N grade level.
     *
     * @param participation StudentProgramParticipation
     * @return true, if is program N grade level
     */
    public boolean isProgramNGradeLevel(StudentProgramParticipation participation) {
        boolean result = false;
        if (participation == null) {
            return result;
        }
        String iepType = (String) participation.getFieldValueByAlias(ALIAS_IEP_TYPE);
        if (StringUtils.isEmpty(iepType) || IEP_TYPE_PRIMARY.equals(iepType)) {
            String programCode = participation.getProgramCode();
            ReferenceCode code = getNGradeLevelProgramCodeMap().get(programCode);
            try {
                String stateCode = code.getStateCode();
                if (stateCode != null && stateCode.length() > 2) {
                    stateCode = stateCode.substring(0, 2);
                }
                int option = Integer.parseInt(stateCode);
                result = option >= N_GRADE_LEVEL_MIN && option <= N_GRADE_LEVEL_MAX;
            } catch (Exception e) {
                // false if number conversion or exception
            }
        }
        return result;
    }

    /**
     * Return true if enrollment code for student in 'E' 'EC' 'E1' in current year and 'TR' 'TC' in
     * second year or any following year
     * otherwise return false.
     *
     * @param studentEnrollment the student enrollment
     * @param school SisSchool
     * @param startDate given start date
     * @return boolean
     */
    public boolean isStudentEnrolledOnGivenDate(StudentEnrollment studentEnrollment,
                                                SisSchool school,
                                                PlainDate startDate) {
        if (studentEnrollment != null && studentEnrollment.getSchoolOid() != null
                && studentEnrollment.getSchoolOid().equals(school.getOid())) {
            String enrollmentCode = studentEnrollment.getEnrollmentCode();
            String instrServiceType = studentEnrollment.getFieldValueByAlias(ALIAS_INSTR_SERVICE_TYPE) != null
                    ? studentEnrollment.getFieldValueByAlias(ALIAS_INSTR_SERVICE_TYPE).toString()
                    : null;

            // Only count students with the DOE INSTR SERVICE TYPE = P, or when it's null (P should
            // be the default value when it's null)
            if (enrollmentCode != null && !INSTR_SERVICE_TYPE_SECONDARY_SCHOOL_CODE.equals(instrServiceType)) {
                if (m_enrolledCodes.contains(enrollmentCode)) {
                    return true;
                }
                if (startDate != null && !studentEnrollment.getEnrollmentDate().after(startDate)) {
                    if (m_transferredCodes.contains(enrollmentCode)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /**
     * Check if student is categorized as N Grade Level. Pre-Kindergarten levels are not counted as
     * N Grade Level.
     *
     * A student in N Grade Level identified by:
     * - PGM_PROGRAM_CODE = 'o07', 'o08', or 'o09'
     * - PGM_START_DATE not null
     * - PGM_END_DATE null or PGM_END_DATE within the report date window.
     *
     * The student has to fulfill all those criteria.
     *
     * @param student SisStudent
     * @param reportEndDate PlainDate
     * @param stdGradeLevel String
     * @return boolean isStudentNGradeLevel
     */
    public boolean isStudentNGradeLevel(SisStudent student, PlainDate reportEndDate, String stdGradeLevel) {
        if (!PRE_KINDERGARTEN_GRADE_LEVEL_CODES.contains(stdGradeLevel)) {
            Collection<StudentProgramParticipation> programParticipations = getNGradeLevelPrograms(student);
            for (StudentProgramParticipation participation : programParticipations) {
                if (isStudentNGradeLevel(participation, reportEndDate)) {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * Check if studentProgramParticipation is categorized as N Grade Level.
     *
     * A student in N Grade Level identified by:
     * - PGM_PROGRAM_CODE = 'o07', 'o08', or 'o09'
     * - PGM_START_DATE not null
     * - PGM_END_DATE null or PGM_END_DATE after or on the report end date.
     *
     * The student has to fulfill all those criteria.
     *
     * @param participation StudentProgramParticipation
     * @param reportEndDate PlainDate
     * @return boolean isStudentNGradeLevel
     */
    public boolean isStudentNGradeLevel(StudentProgramParticipation participation, PlainDate reportEndDate) {
        if (isProgramNGradeLevel(participation)) {
            // PGM_START_DATE not null, and PGM_END_DATE null or PGM_END_DATE after or on report end
            // date.
            if (participation.getStartDate() != null &&
                    (participation.getEndDate() == null || reportEndDate == null
                            || !participation.getEndDate().before(reportEndDate))) {
                return true;
            }
        }

        return false;
    }

    /**
     * Check if studentProgramParticipation counts for member days as N Grade Level.
     *
     * A student in N Grade Level identified by:
     * - PGM_PROGRAM_CODE = 'o07', 'o08', or 'o09'
     *
     * In addition, the participation record must overlap with the report period
     *
     * @param participation StudentProgramParticipation
     * @param reportBeginDate PlainDate
     * @param reportEndDate PlainDate
     * @return boolean isStudentNGradeLevel
     */
    public boolean isStudentNGradeLevelOverlaps(StudentProgramParticipation participation,
                                                PlainDate reportBeginDate,
                                                PlainDate reportEndDate) {
        if (isProgramNGradeLevel(participation)) {
            // PGM_START_DATE not null, and PGM_END_DATE null or PGM_END_DATE after or on report end
            // date.
            if (doDatesOverlap(reportBeginDate, reportEndDate, participation.getStartDate(),
                    participation.getEndDate())) {
                return true;
            }
        }

        return false;
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
     * Sets the semester.
     *
     * @param semesterChoice 0:fall, 1:spring
     */
    public void setSemester(Integer semesterChoice) {
        m_periodBeginCode = (semesterChoice.intValue() == 0) ? PERIOD_CODE_FALL_BEGIN : PERIOD_CODE_SPRING_BEGIN;
        m_periodEndCode = (semesterChoice.intValue() == 0) ? PERIOD_CODE_FALL_END : PERIOD_CODE_SPRING_END;
    }

    /**
     * Set the criteria used to generate program maps in the helper.
     * The existing program maps are cleared when this method is called.
     *
     * @param studentCriteria void
     */
    public void setStudentCriteria(Criteria studentCriteria) {
        m_studentCriteria = studentCriteria;
        m_studentNGradeLevelProgramMap = null;
    }

    public boolean isIEAStudent(SisStudent student) {
        Collection<StudentProgramParticipation> ieaPgms = null;
        if (m_studentIEAProgramMap == null) {
            ieaPgms = getIEAPrograms(student);
        }
        return ieaPgms != null && ieaPgms.size() > 0;
    }

    /**
     * The method which build map of all school calendar dates keyed on school oid.
     *
     * @return Map<String, Collection<SchoolCalendarDate>>
     */
    private Map<String, Collection<SchoolCalendarDate>> getCSDMap() {
        X2Criteria csdCriteria = new X2Criteria();
        csdCriteria.addIn(SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID, getMostCommonCalendars());

        QueryByCriteria csdQuery = new QueryByCriteria(SchoolCalendarDate.class, csdCriteria);
        csdQuery.addOrderBy(SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID, true);
        csdQuery.addOrderBy(SchoolCalendarDate.COL_DATE, true);

        return m_broker.getGroupedCollectionByQuery(csdQuery,
                SchoolCalendarDate.REL_SCHOOL_CALENDAR
                        + ModelProperty.PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                2056);
    }

    /**
     * Find the entry enrollment record for the span.
     *
     * @param span TNStudentEnrollmentSpan
     * @return Student enrollment
     */
    private StudentEnrollment getEntryEnrollment(TNStudentEnrollmentSpan span) {
        StudentEnrollment enrollment = null;
        // Find active enrollment
        for (StudentEnrollment item : span.getEnrollments()) {
            if (StudentEnrollment.ENTRY.equals(item.getEnrollmentType())
                    && StudentManager.isActiveStudent(m_organization, item.getStatusCode())) {
                enrollment = item;
                break;
            }
        }

        if (enrollment == null) {
            // Find any enrollment
            for (StudentEnrollment item : span.getEnrollments()) {
                if (StudentEnrollment.ENTRY.equals(item.getEnrollmentType())) {
                    enrollment = item;
                    break;
                }
            }
        }
        return enrollment;
    }

    /**
     * Return a collection of IEA programs for this student for this year.
     *
     * @param student SisStudent
     * @return Collection
     */
    private Collection<StudentProgramParticipation> getIEAPrograms(SisStudent student) {
        Collection<StudentProgramParticipation> programs = null;
        if (m_studentIEAProgramMap == null
                || (m_studentCriteria == null && !m_studentIEAProgramMap.containsKey(student.getOid()))) {
            X2Criteria criteria = getProgramDatesCriteria();
            criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, getIEAProgramCodes());
            m_studentIEAProgramMap = populateProgramMap(criteria, student, m_studentIEAProgramMap);
        }
        programs = m_studentIEAProgramMap.get(student.getOid());
        return programs == null ? new ArrayList<StudentProgramParticipation>() : programs;
    }

    /**
     * Return map of IEA program codes.
     *
     * @return Map
     */
    private Map<String, ReferenceCode> getIEAProgramCodeMap() {
        if (m_programCodeMapIEA == null) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(StudentProgramParticipation.class.getName(),
                    StudentProgramParticipation.COL_PROGRAM_CODE);
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_CATEGORY, "Student Classification");
            criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, "8");
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
            m_programCodeMapIEA = m_broker.getMapByQuery(new QueryByCriteria(ReferenceCode.class, criteria),
                    ReferenceCode.COL_CODE, 64);
        }
        return m_programCodeMapIEA;
    }

    /**
     * Return collection of IEA program codes.
     *
     * @return Collection
     */
    private Collection<String> getIEAProgramCodes() {
        if (m_programCodesIEA == null) {
            m_programCodesIEA = new HashSet<String>();
            for (ReferenceCode value : getIEAProgramCodeMap().values()) {
                m_programCodesIEA.add(value.getCode());
            }
        }
        return m_programCodesIEA;
    }

    /**
     * Return the ineligibility funding programs for the student.
     *
     * @param student SisStudent
     * @return Collection
     */
    private Collection<StudentProgramParticipation> getIneligibiityPrograms(SisStudent student) {
        Collection<StudentProgramParticipation> programs = null;

        if (m_studentIneligibilityProgramMap == null
                || (m_studentCriteria == null && !m_studentIneligibilityProgramMap.containsKey(student.getOid()))) {
            X2Criteria criteria = getProgramDatesCriteria();

            DataDictionary dictionary = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_STUDENT_INELIGIBILITY_STATUS);
            X2Criteria criteriaReference = new X2Criteria();
            criteriaReference.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
            criteriaReference.addIn(ReferenceCode.COL_STATE_CODE,
                    Arrays.asList(STATE_CODE_FUNDING_INELIGIBILITY_EXCLUDED_OUT_OF_STATE,
                            STATE_CODE_FUNDING_INELIGIBILITY_EXCLUDED_I20_STUDENT));

            criteria.addIn(field.getJavaName(),
                    new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteriaReference));

            m_studentIneligibilityProgramMap = populateProgramMap(criteria, student, m_studentIneligibilityProgramMap);
        }
        programs = m_studentIneligibilityProgramMap.get(student.getOid());
        return programs == null ? new ArrayList<StudentProgramParticipation>() : programs;
    }

    /**
     * Build the list of most Common Calendars for schools.
     *
     * @return Collection of SchoolCalendars oids
     */
    private Collection getMostCommonCalendars() {
        Map<String, Map<String, Collection<SchoolCalendar>>> mapSchoolCalendars = getSchoolCalendars();

        HashMap<String, String> schoolCalendars = new HashMap();

        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(m_multiYearHelper.getActiveStudentCriteria());
        criteria.addAndCriteria(m_multiYearHelper.getNotEmptyCalendarCriteria());

        String[] columns = new String[] {m_multiYearHelper.getSchoolOidField(),
                m_multiYearHelper.getCalendarCodeField(), "count(*)"};

        ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class, columns, criteria);
        query.addGroupBy(m_multiYearHelper.getSchoolOidField());
        query.addGroupBy(m_multiYearHelper.getCalendarCodeField());
        query.addOrderByDescending("count(*)");

        ReportQueryIterator iterator = m_broker.getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String schoolOid = (String) row[0];
                String calendarCode = (String) row[1];

                if (!schoolCalendars.containsKey(schoolOid)) {
                    Map<String, Collection<SchoolCalendar>> mapCalendars = mapSchoolCalendars.get(schoolOid);
                    if (mapCalendars != null && mapCalendars.containsKey(calendarCode)) {
                        SchoolCalendar schoolCalendar = mapCalendars.get(calendarCode).iterator().next();
                        schoolCalendars.put(schoolOid, schoolCalendar.getOid());
                    }
                }
            }
        } finally {
            iterator.close();
        }

        // Add schools without students - any calendar will do
        for (String oid : mapSchoolCalendars.keySet()) {
            if (!schoolCalendars.containsKey(oid)) {
                Map<String, Collection<SchoolCalendar>> mapCalendars = mapSchoolCalendars.get(oid);
                SchoolCalendar schoolCalendar = mapCalendars.values().iterator().next().iterator().next();
                schoolCalendars.put(oid, schoolCalendar.getOid());
            }
        }

        return schoolCalendars.values();
    }

    /**
     * Return map of NGradeLevel program codes.
     *
     * @return Map
     */
    private Map<String, ReferenceCode> getNGradeLevelProgramCodeMap() {
        if (m_programCodeMap == null) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(StudentProgramParticipation.class.getName(),
                    StudentProgramParticipation.COL_PROGRAM_CODE);

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_CATEGORY, "Options");
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());

            m_programCodeMap = m_broker.getMapByQuery(new QueryByCriteria(ReferenceCode.class, criteria),
                    ReferenceCode.COL_CODE, 64);
        }

        return m_programCodeMap;
    }

    /**
     * Return collection of NGradeLevel program codes.
     *
     * @return Collection
     */
    private Collection<String> getNGradeLevelProgramCodes() {
        if (m_programCodes == null) {
            m_programCodes = new HashSet<String>();
            for (ReferenceCode value : getNGradeLevelProgramCodeMap().values()) {
                m_programCodes.add(value.getCode());
            }
        }

        return m_programCodes;
    }

    /**
     * Return a collection of NGradeLevel programs for this student for this year.
     *
     * @param student SisStudent
     * @return Collection
     */
    private Collection<StudentProgramParticipation> getNGradeLevelPrograms(SisStudent student) {
        Collection<StudentProgramParticipation> programs = null;

        if (m_studentNGradeLevelProgramMap == null
                || (m_studentCriteria == null && !m_studentNGradeLevelProgramMap.containsKey(student.getOid()))) {
            X2Criteria criteria = getProgramDatesCriteria();

            criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, getNGradeLevelProgramCodes());

            m_studentNGradeLevelProgramMap = populateProgramMap(criteria, student, m_studentNGradeLevelProgramMap);
        }
        programs = m_studentNGradeLevelProgramMap.get(student.getOid());
        return programs == null ? new ArrayList<StudentProgramParticipation>() : programs;
    }

    /**
     * Gets the program dates criteria.
     *
     * @return a criteria with date ranges for this school year
     */
    private X2Criteria getProgramDatesCriteria() {
        X2Criteria criteria = new X2Criteria();

        criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_currentContext.getEndDate());
        X2Criteria endDate1Criteria = new X2Criteria();
        endDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE, m_broker.getPersistenceKey());
        X2Criteria endDate2Criteria = new X2Criteria();
        endDate2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE,
                m_currentContext.getStartDate());
        endDate1Criteria.addOrCriteria(endDate2Criteria);
        criteria.addAndCriteria(endDate1Criteria);

        return criteria;
    }

    /**
     * Build map of maps of SchoolCalendars keyed on school oid and school calendar id.
     *
     * @return Map<String, Map<String, Collection<SchoolCalendar>>>
     */
    private Map<String, Map<String, Collection<SchoolCalendar>>> getSchoolCalendars() {
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

        return m_broker.getGroupedCollectionByQuery(query, new String[] {SchoolCalendar.COL_SCHOOL_OID,
                SchoolCalendar.COL_CALENDAR_ID}, new int[] {100, 5});
    }

    /**
     * Initialize enrollment codes.
     */
    private void initializeEnrollmentCodes() {
        m_enrolledCodes = new HashSet();
        m_transferredCodes = new HashSet();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(StudentEnrollment.class.getName(),
                StudentEnrollment.COL_ENROLLMENT_CODE);
        for (Entry<String, ReferenceCode> entry : field.getReferenceTable().getCodeMap().entrySet()) {
            String stateCode = entry.getValue().getStateCode();
            if (NET_ENROLLMENT_E.equals(stateCode) || NET_ENROLLMENT_EC.equals(stateCode)
                    || NET_ENROLLMENT_E1.equals(stateCode)) {
                m_enrolledCodes.add(entry.getKey());
            }
            if (NET_ENROLLMENT_TR.equals(stateCode) || NET_ENROLLMENT_TC.equals(stateCode)) {
                m_transferredCodes.add(entry.getKey());
            }
        }
    }

    /**
     * Initialize school dates map keyed on school oid.<br>
     * Map contains map keyed on reporting period(month's number) which then contains map keyed
     * on BEGIN or END date key with values of dates.<br>
     * The purpose then is to get dates by school and month's number.<br>
     * Also initialize school dates set keyed on school oid and report period number.
     */
    private void initializeSclDatesMap() {
        m_sclDatesMap = new HashMap<String, Map<String, Map<String, PlainDate>>>();
        m_sclDatesSet = new HashMap<String, Map<String, Set<PlainDate>>>();

        Map<String, Collection<SchoolCalendarDate>> csdMap = getCSDMap();

        for (String sclOid : csdMap.keySet()) {
            Map<String, Map<String, PlainDate>> commonMap = new HashMap<String, Map<String, PlainDate>>();
            Map<String, Set<PlainDate>> commonDatesSet = new HashMap<String, Set<PlainDate>>();
            Collection<SchoolCalendarDate> csdCollection = csdMap.get(sclOid);
            if (csdCollection != null) {
                // periodCounter should start from 1 as
                // ReportPeriod codes start from 01.
                int periodCounter = 1;
                int dayCounter = 1;
                PlainDate periodStartDate = null;
                PlainDate yearStartDate = null;
                PlainDate lastDate = null;
                Set<PlainDate> datesSet = new HashSet<PlainDate>();
                Set<PlainDate> yearDatesSet = new HashSet<PlainDate>();
                Iterator csdIterator = csdCollection.iterator();
                while (csdIterator.hasNext()) {
                    SchoolCalendarDate csd = (SchoolCalendarDate) csdIterator.next();
                    if (!isValidYearDate(csd)) {
                        continue;
                    }
                    if (yearStartDate == null) {
                        yearStartDate = csd.getDate();
                    }
                    if (!isValidPeriodDate(csd)) {
                        continue;
                    }
                    if (dayCounter == 1) {
                        periodStartDate = csd.getDate();
                    } else if (dayCounter == REPORT_PERIOD_LENGTH || !csdIterator.hasNext()) {
                        Map<String, PlainDate> datesMap = new HashMap<String, PlainDate>();
                        datesMap.put(KEY_DATE_BEGIN, periodStartDate);
                        datesMap.put(KEY_DATE_END, csd.getDate());
                        commonMap.put(leftPad(periodCounter, 2), datesMap);
                        lastDate = csd.getDate();
                        m_yearEndDate = csd.getDate();

                        datesSet.add(csd.getDate());
                        yearDatesSet.add(csd.getDate());
                        commonDatesSet.put(leftPad(periodCounter, 2), datesSet);
                        datesSet = new HashSet<PlainDate>();

                        periodCounter++;
                        dayCounter = 1;
                        continue;
                    }
                    lastDate = csd.getDate();
                    datesSet.add(csd.getDate());
                    yearDatesSet.add(csd.getDate());
                    dayCounter++;
                }
                if (dayCounter > 1) {
                    Map<String, PlainDate> datesMap = new HashMap<String, PlainDate>();
                    datesMap.put(KEY_DATE_BEGIN, periodStartDate);
                    datesMap.put(KEY_DATE_END, lastDate);
                    commonMap.put(leftPad(periodCounter, 2), datesMap);
                    commonDatesSet.put(leftPad(periodCounter, 2), datesSet);

                    m_yearEndDate = lastDate;
                }

                Map<String, PlainDate> datesMap = new HashMap<String, PlainDate>();
                datesMap.put(KEY_DATE_BEGIN, yearStartDate);
                datesMap.put(KEY_DATE_END, lastDate);
                commonMap.put(YEAR_CODE, datesMap);
                commonDatesSet.put(YEAR_CODE, yearDatesSet);
            }
            m_sclDatesSet.put(sclOid, commonDatesSet);
            m_sclDatesMap.put(sclOid, commonMap);
        }
    }

    /**
     * A date is valid for the period if it is in-session.
     *
     * @param csd SchoolCalendarDate
     * @return true, if is valid period date
     */
    private boolean isValidPeriodDate(SchoolCalendarDate csd) {
        return csd.getInSessionIndicator();
    }

    /**
     * Checks if is valid year date.
     *
     * @param csd SchoolCalendarDate
     * @return true, if is valid year date
     */
    private boolean isValidYearDate(SchoolCalendarDate csd) {
        return csd.getInSessionIndicator() ||
                CALENDAR_START_EVENT_CODE.equals(csd.getScheduleDayType()) ||
                CALENDAR_START_EVENT_CODE.equals(csd.getFieldValueByAlias(ALIAS_CALENDAR_EVENT_TYPE_2)) ||
                CALENDAR_START_EVENT_CODE.equals(csd.getFieldValueByAlias(ALIAS_CALENDAR_EVENT_TYPE_3));
    }

    /**
     * Populate program map.
     *
     * @param criteria X2Criteria
     * @param student SisStudent
     * @param map Map<String,Collection<StudentProgramParticipation>>
     * @return Map
     */
    private Map<String, Collection<StudentProgramParticipation>> populateProgramMap(X2Criteria criteria,
                                                                                    SisStudent student,
                                                                                    Map<String, Collection<StudentProgramParticipation>> map) {
        if (m_studentCriteria == null) {
            if (map == null) {
                map = new HashMap();
            }
            criteria.addEqualTo(StudentProgramParticipation.COL_STUDENT_OID, student.getOid());
            Collection<StudentProgramParticipation> programs =
                    m_broker.getCollectionByQuery(new QueryByCriteria(StudentProgramParticipation.class, criteria));
            map.put(student.getOid(), programs);
        } else {
            criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID,
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentCriteria));
            map = m_broker.getGroupedCollectionByQuery(new QueryByCriteria(StudentProgramParticipation.class, criteria),
                    StudentProgramParticipation.COL_STUDENT_OID, 1024);
        }
        return map;
    }
}
