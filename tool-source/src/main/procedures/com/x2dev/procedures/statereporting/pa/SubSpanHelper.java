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
package com.x2dev.procedures.statereporting.pa;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.*;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class SubSpanHelper.
 */
public class SubSpanHelper {

    /**
     * Enum for member day options.
     *
     * @author Follett Software Company
     */
    public enum MemberDay {
        CURRENT, NEXT_IN_SESSION, PREIOUS_IN_SESSION
    }

    /**
     * The interface used to determine how an enrollment record should be processed.
     *
     * @author Follett Software Company
     * @param <W> the generic type
     * @param <T> the generic type
     */
    public interface SplitRule<W extends SpanWorker, T extends Set<StudentEnrollment>> {

        /**
         * return custom active statues.
         *
         * @return String
         */
        String getEnrollmentType();

        /**
         * Does current enrollment start span period.
         *
         * @param currentEnrollment StudentEnrollment
         * @param subSpan SubSpan<W>
         * @param nextEnrs T
         * @return true, if is first in period
         */
        boolean isFirstInPeriod(StudentEnrollment currentEnrollment, SubSpan<W> subSpan, T nextEnrs);

        /**
         * Does current enrollment end span period.
         *
         * @param currentEnrollment StudentEnrollment
         * @param subSpan SubSpan<W>
         * @param nextEnrs T
         * @return true, if is last in period
         */
        boolean isLastInPeriod(StudentEnrollment currentEnrollment, SubSpan<W> subSpan, T nextEnrs);

        /**
         * validate current Enrollment.
         *
         * @param currentEnrollment StudentEnrollment
         * @return true, if is skip record
         */
        boolean isSkipRecord(StudentEnrollment currentEnrollment);

        /**
         * return MemberDay type for first day in span.
         *
         * @return Member day
         */
        MemberDay getFirstDayMemberType();

        /**
         * return MemberDay type for last day in span.
         *
         * @return Member day
         */
        MemberDay getLastDayMemberType();

        /**
         * if enrollment has empty status, should we count it like active.
         *
         * @return true, if successful
         */
        boolean useEmptyStatusLikeActive();

        /**
         * return active statues.
         *
         * @return List
         */
        List<String> getActiveStatuses();

        /**
         * should we use only active enrollment.
         *
         * @return true, if successful
         */
        boolean useOnlyActiveEnrollment();
    }

    /**
     * THe interface used to generate spans from the list of student enrollment records.
     *
     * @author Follett Software Company
     * @param <W> the generic type
     * @param <S> the generic type
     */
    public interface SplitSpan<W extends SpanWorker, S extends SubSpan> {

        /**
         * split enrollments by SplitRules
         * if deleteSkipedRecord is true - return only span which got success validation.
         *
         * @param enrollments List<StudentEnrollment>
         * @param deleteSkipedRecord boolean
         * @return List
         * @see SubSpan#isSkipRecord()
         */
        List<SubSpan<W>> splitSpan(List<StudentEnrollment> enrollments, boolean deleteSkipedRecord);

        /**
         * set SplitRule.
         *
         * @param rule void
         */
        void setSplitRule(SplitRule rule);

        /**
         * adding List<SplitRule> to existing.
         *
         * @param rules void
         */
        void setSplitRules(List<SplitRule> rules);

        /**
         * return Organization.
         *
         * @return Organization
         */
        Organization getOrganization();

        /**
         * return new Worker <br>
         * .
         *
         * @return w
         */
        W getNewWorker();

        /**
         * return new SubSpan, must linked it with previous subspan .
         *
         * @param previousSpan S
         * @return S
         */
        S createSubSpan(S previousSpan);
    }

    /**
     * The interface for the class that provides attributes for the span.
     *
     * @author Follett Software Company
     * @param <S> the generic type
     */
    public interface SpanWorker<S extends SubSpan> {

        /**
         * Sets the sub span.
         *
         * @param subSpan void
         */
        void setSubSpan(S subSpan);
    }

    /**
     * This interface is used to create the span worker and span.
     *
     * @author Follett Software Company
     * @param <W> the generic type
     * @param <S> the generic type
     */
    public interface WorkerSubSpanFabric<W extends SpanWorker, S extends SubSpan> {

        /**
         * Creates the worker.
         *
         * @return W
         */
        W createWorker();

        /**
         * Creates the sub span.
         *
         * @param previousSpan S
         * @return S
         */
        S createSubSpan(S previousSpan);
    }
    /**
     * Contains the parameters used for the SubSpanHelper
     * The parameters provide date range and calendar selection filters for the spans.
     *
     * @author Follett Software Company
     */
    public class PAChildCommonData {
        private static final String PGM_CODE_IEP = "IEP";

        private X2Broker m_brokerPACD;
        private String m_calendarIdPACD;
        private Map<String, Map<String, Pair<SchoolCalendar, List<SchoolCalendarDate>>>> m_casSCMap = null;
        private PlainDate m_endDatePACD;
        private Map<String, List<StudentProgramParticipation>> m_iepMap = null;
        private StudentHistoryHelper m_helperPACD = null;
        private Boolean m_isAllCalendarsPACD;
        private Organization m_organizationPACD;
        private Map<String, Map<String, Pair<SchoolCalendar, List<SchoolCalendarDate>>>> m_periodCasSCMap = null;
        private Collection<School> m_schoolsPACD;
        private TreeMap m_sortedGradeLevels;
        private PlainDate m_startDatePACD;

        /**
         * Instantiates a new PA child common data.
         *
         * @param helper StudentHistoryHelper
         * @param schools Collection<School>
         * @param isAllCalendars Boolean
         * @param calendarId String
         * @param broker X2Broker
         * @param organization Organization
         */
        public PAChildCommonData(StudentHistoryHelper helper, Collection<School> schools, Boolean isAllCalendars,
                String calendarId, X2Broker broker, Organization organization) {
            m_schoolsPACD = schools;
            m_helperPACD = helper;
            m_startDatePACD = (PlainDate) m_helperPACD.getSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE);
            m_endDatePACD = (PlainDate) m_helperPACD.getSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE);
            m_isAllCalendarsPACD = isAllCalendars;
            m_calendarIdPACD = calendarId;
            m_brokerPACD = broker;
            m_organizationPACD = organization;
        }

        /**
         * return X2Broker.
         *
         * @return X 2 broker
         */
        public X2Broker getBroker() {
            return m_brokerPACD;
        }

        /**
         * Return calendarId which user chose in input for limited data .
         *
         * @return String
         */
        public String getCalendarId() {
            return m_calendarIdPACD;
        }

        /**
         * Return a Map<sklOid, Map<casCalendarId, Pair<SchoolCalendar, List<SchoolCalendarDate>>
         * for the dates in the context year.
         *
         * @return Map
         */
        public Map<String, Map<String, Pair<SchoolCalendar, List<SchoolCalendarDate>>>> getCasMap() {
            if (m_casSCMap == null) {
                m_casSCMap = initSclCalendarsMap(false);
            }
            return m_casSCMap;
        }

        /**
         * return StudentHistoryHelper for common use.
         *
         * @return Student history helper
         */
        public StudentHistoryHelper getHelper() {
            return m_helperPACD;
        }

        /**
         * Gets the iep programs.
         *
         * @param studentOid String
         * @return List
         */
        public List<StudentProgramParticipation> getIepPrograms(String studentOid) {
            if (m_iepMap == null) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, PGM_CODE_IEP);
                m_helperPACD.getStudentSelectionQuery(StudentProgramParticipation.class, criteria,
                        StudentProgramParticipation.COL_STUDENT_OID);
                BeanQuery query = new BeanQuery(StudentProgramParticipation.class, criteria);
                m_iepMap = m_brokerPACD.getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID,
                        1024);
            }
            return m_iepMap.get(studentOid);
        }

        /**
         * Return boolean is use all calendars, which user chose in input.
         *
         * @return Boolean
         */
        public Boolean getIsAllCalendars() {
            return m_isAllCalendarsPACD;
        }

        /**
         * return report end date, this field is used for limited data by end report date .
         *
         * @return Plain date
         */
        public PlainDate getLimitEndDate() {
            return m_endDatePACD;
        }

        /**
         * return report start date, this field is used for limited data by start report date.
         *
         * @return Plain date
         */
        public PlainDate getLimitStartDate() {
            return m_startDatePACD;
        }

        /**
         * return Organization.
         *
         * @return Organization
         */
        public Organization getOrganization() {
            return m_organizationPACD;
        }

        /**
         * Return a Map<sklOid, Map<casCalendarId, Pair<SchoolCalendar, List<SchoolCalendarDate>>
         * for the dates in the period
         * data limited by getLimitEndDate() and getLimitEndDate().
         *
         * @return Map
         */
        public Map<String, Map<String, Pair<SchoolCalendar, List<SchoolCalendarDate>>>> getPeriodCasMap() {
            if (m_periodCasSCMap == null) {
                m_periodCasSCMap = initSclCalendarsMap(true);
            }
            return m_periodCasSCMap;
        }

        /**
         * return selected schools from input .
         *
         * @return Collection
         */
        public Collection<School> getSchools() {
            return m_schoolsPACD == null ? new ArrayList<School>() : m_schoolsPACD;
        }

        /**
         * return sorted Grade Levels.
         *
         * @return Tree map
         * @see com.follett.fsc.core.k12.business.StudentManager#buildGradeLevelMap(X2Broker)
         */
        public TreeMap getSortedGradeLevels() {
            if (m_sortedGradeLevels == null) {
                m_sortedGradeLevels = StudentManager.buildGradeLevelMap(m_brokerPACD);
            }
            return m_sortedGradeLevels;
        }

        /**
         * Translates an alias into a Java bean path name. An initialization error will be logged
         * if the alias does not exist.
         *
         * @param alias String
         * @return String
         */
        public String translateAliasToJavaName(String alias) {
            String javaName = null;

            DataDictionaryField field = getDataDictionary(null).findDataDictionaryFieldByAlias(alias);
            if (field != null) {
                javaName = field.getJavaName();
            }

            return javaName;
        }

        /**
         * Return a Map<sklOid, Map<casCalendarId, Pair<SchoolCalendar, List<SchoolCalendarDate>>
         * for the dates range
         * if limitReportPeriod is true - limited by report start and end dates, if false - limited
         * by school year
         * context dates.
         *
         * @param limitReportPeriod boolean
         * @return Map
         */
        private Map<String, Map<String, Pair<SchoolCalendar, List<SchoolCalendarDate>>>> initSclCalendarsMap(boolean limitReportPeriod) {
            List<String> sklOids = new ArrayList<String>();
            if (m_schoolsPACD != null) {
                for (School skl : m_schoolsPACD) {
                    sklOids.add(skl.getOid());
                }
            }
            X2Criteria casCriteria = new X2Criteria();
            casCriteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_organizationPACD.getCurrentContextOid());


            if (!m_isAllCalendarsPACD.booleanValue()) {
                casCriteria.addEqualTo(SchoolCalendar.COL_CALENDAR_ID, m_calendarIdPACD);
            }

            if (!sklOids.isEmpty()) {
                casCriteria.addIn(SchoolCalendar.COL_SCHOOL_OID, sklOids);
            } else {
                casCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.COL_INACTIVE_INDICATOR,
                        Boolean.TRUE);
                casCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.COL_ARCHIVE_INDICATOR,
                        Boolean.TRUE);
            }

            SubQuery casSubQuery = new SubQuery(SchoolCalendar.class, X2BaseBean.COL_OID, casCriteria);


            X2Criteria csdCriteria = new X2Criteria();
            csdCriteria.addIn(SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID, casSubQuery);
            if (limitReportPeriod) {
                csdCriteria.addLessOrEqualThan(SchoolCalendarDate.COL_DATE, getLimitEndDate());
                csdCriteria.addGreaterOrEqualThan(SchoolCalendarDate.COL_DATE, getLimitStartDate());
            } else {
                DistrictSchoolYearContext context = getOrganization().getCurrentContext();

                csdCriteria.addLessOrEqualThan(SchoolCalendarDate.COL_DATE, context.getEndDate());
                csdCriteria.addGreaterOrEqualThan(SchoolCalendarDate.COL_DATE, context.getStartDate());
            }
            csdCriteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
            QueryByCriteria csdQuery = new QueryByCriteria(SchoolCalendarDate.class, csdCriteria);

            csdQuery.addOrderBy(SchoolCalendarDate.COL_DATE, true);

            String sklOidField = SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER
                    + SchoolCalendar.REL_SCHOOL + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID;
            String sklCasIdField = SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER
                    + SchoolCalendar.COL_CALENDAR_ID;


            String[] columns = new String[] {sklOidField, sklCasIdField};
            int[] sizes = new int[] {1024, 1024};

            Map<String, Map<String, List<SchoolCalendarDate>>> map =
                    new HashMap<String, Map<String, List<SchoolCalendarDate>>>();
            Map<String, Map<String, Pair<SchoolCalendar, List<SchoolCalendarDate>>>> casMap =
                    new HashMap<String, Map<String, Pair<SchoolCalendar, List<SchoolCalendarDate>>>>();
            map.putAll(getBroker().getGroupedCollectionByQuery(csdQuery, columns, sizes));

            for (String sklOidKey : map.keySet()) {
                Map<String, List<SchoolCalendarDate>> sklCasIdMap = map.get(sklOidKey);
                Map<String, Pair<SchoolCalendar, List<SchoolCalendarDate>>> casIdPairMap =
                        new HashMap<String, Pair<SchoolCalendar, List<SchoolCalendarDate>>>();
                casMap.put(sklOidKey, casIdPairMap);

                for (String sklCasIdKey : sklCasIdMap.keySet()) {
                    List<SchoolCalendarDate> dates = sklCasIdMap.get(sklCasIdKey);
                    SchoolCalendar calendar = null;
                    if (dates != null && !dates.isEmpty()) {
                        calendar = dates.get(0).getSchoolCalendar();
                    }
                    Pair<SchoolCalendar, List<SchoolCalendarDate>> pair =
                            Pair.of(calendar, dates);
                    casIdPairMap.put(sklCasIdKey, pair);
                }
            }


            return casMap;

        }
    }

    /**
     * The class that generates attribute information from the data in the span
     * Specific for PA state, used DOE STD CALENDAR like info for previous calendar Id.
     *
     * @author Follett Software Company
     */
    public class PAChildWorker implements SpanWorker {
        public static final String ALIAS_ENR_STD_CAL = "DOE STD CALENDAR";
        public static final String ALIAS_ENR_STD_CAL_PERCENT = "DOE CALENDAR PERCENT";
        public static final String ALIAS_SKL_CAL_PERCENT = "all-cas-PercentTimeEnrolled";

        private static final String DEFAULT_CALENDAR_ID = "Standard";
        private static final String IEP_EXIT = "E";
        private static final String IEP_NO = "N";
        private static final String IEP_YES = "Y";

        private final DecimalFormat m_percentFormat = new DecimalFormat("0.###");

        private SchoolCalendar m_calendar = null;
        private PAChildCommonData m_commonData;
        private PlainDate m_firstInSessionDateCSY = null;
        private PlainDate m_firstInSessionDateRD = null;
        private PlainDate m_firstReportDate = null;
        private StudentHistoryHelper m_helper = null;
        private List<PlainDate> m_inSessionDatesCSY = null;
        private List<PlainDate> m_inSessionDatesRD = null;
        private boolean m_isInitLastInSessionDateCSY = false;
        private boolean m_isInitLastInSessionDateRD = false;
        private boolean m_isInitSchoolCalendar = false;
        private PlainDate m_lastInSessionDateCSY = null;
        private PlainDate m_lastInSessionDateRD = null;
        private PlainDate m_lastReportDate = null;
        private SubSpan m_subSpan = null;

        /**
         * try to find near session date. If after is true - try find next in session date
         * If after is false - try find previous in session date
         * if not found - return original enrollmentDate param
         *
         * @param insessionDates List<PlainDate>
         * @param enrollmentDate PlainDate
         * @param after boolean
         * @return PlainDate
         */
        public PlainDate findSessionDate(List<PlainDate> insessionDates, PlainDate enrollmentDate, boolean after) {

            PlainDate nearestDate = null;
            for (PlainDate date : insessionDates) {
                if (after && date.after(enrollmentDate)) {
                    if (nearestDate == null || nearestDate.after(date)) {
                        nearestDate = date;
                    }
                } else if (!after && date.before(enrollmentDate)) {
                    if (nearestDate == null || nearestDate.before(date)) {
                        nearestDate = date;
                    }
                }
            }
            if (nearestDate == null) {
                nearestDate = enrollmentDate;
            }
            return nearestDate;

        }

        /**
         * return absent days in period limited by report dates.
         *
         * @return double
         */
        public double getAbsentDays() {
            StudentEnrollment stdEnrollment = m_subSpan.getFirstInSpanEnr();
            Student student = stdEnrollment.getStudent();
            List<StudentAttendance> stdAttendances = m_helper.getStudentAttendances(student.getOid());
            stdAttendances = stdAttendances == null ? new ArrayList<StudentAttendance>() : stdAttendances;
            double count = 0;
            PlainDate firstDate = getFstInSessionDateLimitByRD();
            PlainDate lastDate = getLstInSessionDateLimitedByRD();
            for (StudentAttendance attendance : stdAttendances) {
                PlainDate date = attendance.getDate();
                if (firstDate != null && !date.before(firstDate) && (lastDate == null || !date.after(lastDate))) {
                    BigDecimal portialAtt = attendance.getPortionAbsent();
                    if (portialAtt != null) {
                        count = count + portialAtt.doubleValue();
                    }
                }
            }

            return count;
        }

        /**
         * return common data.
         *
         * @return PA child common data
         */
        public PAChildCommonData getCommonData() {
            return m_commonData;
        }

        /**
         * return SchoolCalendar by casId and School<br>
         * casId use @see {@link #getCalendarId()}<br>
         * School use @see {@link #getSchool()}.
         *
         * @return School calendar
         */
        public SchoolCalendar getCalendar() {
            if (!m_isInitSchoolCalendar && m_calendar == null) {
                m_isInitSchoolCalendar = true;
                String casId = getCalendarId();
                m_calendar = getCalendarByID(casId);
            }

            return m_calendar;
        }

        /**
         * return SchoolCalendar by casId and School<br>
         * School use @see {@link #getSchool()}.
         *
         * @param casId String
         * @return School calendar
         */
        public SchoolCalendar getCalendarByID(String casId) {
            Map<String, Pair<SchoolCalendar, List<SchoolCalendarDate>>> casOidsMap =
                    m_commonData.getPeriodCasMap().get(getSchool().getOid());
            SchoolCalendar calendar = null;
            if (casOidsMap != null) {
                Pair<SchoolCalendar, List<SchoolCalendarDate>> pair = casOidsMap.get(casId);
                if (pair != null) {
                    calendar = pair.getLeft();
                }
            }
            return calendar;
        }

        /**
         * return calendar Id which was used in last in session date inside this span
         * Generally split rule configuration can not split all cases. In this case span period can
         * contain two or more
         * calendar Id. Example enrollments E S Y W. Split rule configure only for E and W. But
         * inside S was changed
         * calendar Id. Method return more latest calendar Id (between S and W). Or if it is empty -
         * calendar form Student.
         * Method working correctly only in case if each enrollment contain calendar info for
         * previous Enrollment.
         *
         * @return String
         */
        public String getCalendarId() {
            String casId = null;
            StudentEnrollment lastSpanEnr = m_subSpan.getLastInSpanEnr();
            if (lastSpanEnr != null) {
                casId = (String) lastSpanEnr.getFieldValueByAlias(ALIAS_ENR_STD_CAL);
            }
            if (StringUtils.isEmpty(casId)) {
                casId = getStudent().getCalendarCode();
            }
            return casId;
        }

        /**
         * get first in session date limit by current school year.
         *
         * @return Plain date
         */
        public PlainDate getFstInSessionDateLimitByCSY() {
            if (m_firstInSessionDateCSY == null) {
                StudentEnrollment stdEnrollment = m_subSpan.getFirstInSpanEnr();
                PlainDate enrollmentDate = stdEnrollment.getEnrollmentDate();
                List<PlainDate> insessionDates = getInsessionCalendarDatesLimitByCSY();
                PlainDate currentSYstartDate = getCommonData().getOrganization().getCurrentContext().getStartDate();
                if (m_subSpan.getFirstDateMemberType().equals(MemberDay.CURRENT)) {
                    m_firstInSessionDateCSY = enrollmentDate;
                } else if (m_subSpan.getFirstDateMemberType().equals(MemberDay.NEXT_IN_SESSION)) {

                    m_firstInSessionDateCSY = findSessionDate(insessionDates, enrollmentDate, true);
                } else if (m_subSpan.getFirstDateMemberType().equals(MemberDay.PREIOUS_IN_SESSION)) {
                    m_firstInSessionDateCSY = findSessionDate(insessionDates, enrollmentDate, false);
                }

                if (currentSYstartDate != null && currentSYstartDate.after(m_firstInSessionDateCSY)) {
                    if (insessionDates.contains(currentSYstartDate)) {
                        m_firstInSessionDateCSY = currentSYstartDate;
                    } else {
                        m_firstInSessionDateCSY = findSessionDate(insessionDates, currentSYstartDate, true);
                    }

                }

                if (!insessionDates.contains(m_firstInSessionDateCSY)) {
                    m_firstInSessionDateCSY = findSessionDate(insessionDates, m_firstInSessionDateCSY, true);
                }
            }
            return m_firstInSessionDateCSY;
        }

        /**
         * get span first in session date limited by report date.
         *
         * @return Plain date
         */
        public PlainDate getFstInSessionDateLimitByRD() {
            if (m_firstInSessionDateRD == null) {
                StudentEnrollment stdEnrollment = m_subSpan.getFirstInSpanEnr();
                PlainDate enrollmentDate = stdEnrollment.getEnrollmentDate();
                List<PlainDate> insessionDates = getInsessionCalendarDatesLimitByRD();
                if (m_subSpan.getFirstDateMemberType().equals(MemberDay.CURRENT)) {
                    m_firstInSessionDateRD = enrollmentDate;
                } else if (m_subSpan.getFirstDateMemberType().equals(MemberDay.NEXT_IN_SESSION)) {

                    m_firstInSessionDateRD = findSessionDate(insessionDates, enrollmentDate, true);
                } else if (m_subSpan.getFirstDateMemberType().equals(MemberDay.PREIOUS_IN_SESSION)) {
                    m_firstInSessionDateRD = findSessionDate(insessionDates, enrollmentDate, false);
                }

                if (m_firstReportDate != null && m_firstReportDate.after(m_firstInSessionDateRD)) {
                    if (insessionDates.contains(m_firstReportDate)) {
                        m_firstInSessionDateRD = m_firstReportDate;
                    } else {
                        m_firstInSessionDateRD = findSessionDate(insessionDates, m_firstReportDate, true);
                    }

                }

                if (!insessionDates.contains(m_firstInSessionDateRD)) {
                    m_firstInSessionDateRD = findSessionDate(insessionDates, m_firstInSessionDateRD, true);
                }

            }
            return m_firstInSessionDateRD;
        }

        /**
         * get first in session date and last in session dates from school calendar limited by
         * report dates .
         *
         * @return Pair
         */
        public Pair<PlainDate, PlainDate> getFstLstCalendarDatesLimitedByRD() {
            List<PlainDate> insessionDates = getInsessionCalendarDatesLimitByRD();
            Collections.sort(insessionDates);
            Pair<PlainDate, PlainDate> flCasDates = null;
            if (!insessionDates.isEmpty()) {
                PlainDate first = insessionDates.get(0);
                PlainDate last = insessionDates.get(insessionDates.size() - 1);
                flCasDates = Pair.of(first, last);
            }
            if (flCasDates == null) {
                flCasDates = Pair.of(getCommonData().getLimitStartDate(),
                        getCommonData().getLimitEndDate());
            }
            return flCasDates;

        }

        /**
         * return grade Level for span.
         * Grade level info get from first enrollment. If split rule not use all cases. And inside
         * span we can have Y and S
         * records, grade level will give form first period.
         * Example: enrollments E S Y W, Split rule use only E and W. Inside Y was changed YOG.
         * But info will get from period E to Y.
         *
         * @return String
         */
        public String getGradeLevel() {
            int yog = getFirstEnr().getStudent().getYog();
            TreeMap sortedGradeLevels = getCommonData().getSortedGradeLevels();
            List<String> matchingGradeLevels =
                    StudentManager.getMatchingGradeLevels(StudentManager.getMaxGradeLevel(getCommonData().getBroker()),
                            yog,
                            getCommonData().getOrganization().getCurrentContext().getSchoolYear(),
                            sortedGradeLevels);
            String gradeLevel =
                    matchingGradeLevels == null || matchingGradeLevels.size() == 0 ? "" : matchingGradeLevels.get(0);

            gradeLevel = lookupReferenceCodeByBeanPath(SisStudent.class, SisStudent.COL_GRADE_LEVEL, gradeLevel,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            return gradeLevel;
        }

        /**
         * get in session calendar dates limited by school year context dates.
         *
         * @return List
         * @see {@link #getCalendarId()}
         *      if calendar dates does not exist for calendar ID - used default calendar<br>
         *      if doesn't exist for default calendar - used any calendar
         */
        public List<PlainDate> getInsessionCalendarDatesLimitByCSY() {
            StudentEnrollment stdEnr = getFirstEnr();
            String casId = getCalendarId();
            if (m_inSessionDatesCSY == null) {

                SisSchool school = stdEnr.getSchool();


                m_inSessionDatesCSY = getInsessionCalDatesLimitByCSY(school.getOid(), casId);

                if (m_inSessionDatesCSY.isEmpty()
                        && !DEFAULT_CALENDAR_ID.equals(stdEnr.getStudent().getCalendarCode())) {
                    m_inSessionDatesCSY = getInsessionCalDatesLimitByCSY(school.getOid(), DEFAULT_CALENDAR_ID);
                }
                if (m_inSessionDatesCSY.isEmpty()) {
                    m_inSessionDatesCSY =
                            getInsessionCalDatesLimitByCSY(school.getOid(), StudentHistoryHelper.CALENDAR_ANY);
                }
            }
            if (m_inSessionDatesCSY == null) {
                m_inSessionDatesCSY = new ArrayList<PlainDate>();
            }

            return m_inSessionDatesCSY;
        }

        /**
         * get in session calendar dates limited by school year context dates.
         *
         * @param schoolOid String
         * @param calendarId String
         * @return List
         */
        public List<PlainDate> getInsessionCalDatesLimitByCSY(String schoolOid, String calendarId) {
            List<PlainDate> dates = new ArrayList<PlainDate>();
            Pair<SchoolCalendar, List<SchoolCalendarDate>> pair = null;
            Map<String, Pair<SchoolCalendar, List<SchoolCalendarDate>>> map =
                    getCommonData().getCasMap().get(schoolOid);
            if (map != null) {
                if (calendarId.equals(StudentHistoryHelper.CALENDAR_ANY)) {
                    String anyKey = map.keySet().size() > 0 ? map.keySet().iterator().next() : null;
                    if (!StringUtils.isEmpty(anyKey)) {
                        pair = map.get(anyKey);
                    }
                } else {
                    pair = map.get(calendarId);
                }
            }

            if (pair != null) {
                List<SchoolCalendarDate> calDates = pair.getRight();
                if (calDates != null) {
                    for (SchoolCalendarDate calDate : calDates) {
                        dates.add(calDate.getDate());
                    }
                }
            }
            return dates;

        }

        /**
         * get in session calendar dates limited by report dates.
         *
         * @return List
         */
        public List<PlainDate> getInsessionCalendarDatesLimitByRD() {

            StudentEnrollment stdEnr = getFirstEnr();
            String casId = getCalendarId();
            if (m_inSessionDatesRD == null) {

                SisSchool school = stdEnr.getSchool();
                // if I have map whit review it to use something is not in helper

                m_inSessionDatesRD = getInsessionCalendarDatesLimitByRD(school.getOid(), casId);

                if (m_inSessionDatesRD.isEmpty()
                        && !DEFAULT_CALENDAR_ID.equals(stdEnr.getStudent().getCalendarCode())) {
                    m_inSessionDatesRD = getInsessionCalendarDatesLimitByRD(school.getOid(), DEFAULT_CALENDAR_ID);
                }
                if (m_inSessionDatesRD.isEmpty()) {
                    m_inSessionDatesRD =
                            getInsessionCalendarDatesLimitByRD(school.getOid(), StudentHistoryHelper.CALENDAR_ANY);
                }
            }
            if (m_inSessionDatesRD == null) {
                m_inSessionDatesRD = new ArrayList<PlainDate>();
            }

            return m_inSessionDatesRD;
        }

        /**
         * get in session calendar dates limit by report date.
         *
         * @param schoolOid String
         * @param calendarId String
         * @return List
         */
        public List<PlainDate> getInsessionCalendarDatesLimitByRD(String schoolOid, String calendarId) {
            List<PlainDate> dates = new ArrayList<PlainDate>();
            Pair<SchoolCalendar, List<SchoolCalendarDate>> pair = null;
            Map<String, Pair<SchoolCalendar, List<SchoolCalendarDate>>> map =
                    getCommonData().getPeriodCasMap().get(schoolOid);
            if (map != null) {
                if (calendarId.equals(StudentHistoryHelper.CALENDAR_ANY)) {
                    String anyKey = map.keySet().size() > 0 ? map.keySet().iterator().next() : null;
                    if (!StringUtils.isEmpty(anyKey)) {
                        pair = map.get(anyKey);
                    }
                } else {
                    pair = map.get(calendarId);
                }
            }

            if (pair != null) {
                List<SchoolCalendarDate> calDates = pair.getRight();
                if (calDates != null) {
                    for (SchoolCalendarDate calDate : calDates) {
                        dates.add(calDate.getDate());
                    }
                }
            }
            return dates;

        }

        /**
         * get in session calendar dates limited by school year context dates.
         *
         * @return Plain date
         */
        public PlainDate getLstInSessionDateLimitedByCSY() {

            if (!m_isInitLastInSessionDateCSY && m_lastInSessionDateCSY == null) {
                m_isInitLastInSessionDateCSY = true;
                List<PlainDate> insessionDates = getInsessionCalendarDatesLimitByCSY();
                StudentEnrollment lastStdEnr = m_subSpan.getLastInSpanEnr();
                PlainDate currentSYendDate = getCommonData().getOrganization().getCurrentContext().getEndDate();
                if (lastStdEnr != null) {
                    PlainDate enrollmentDate = lastStdEnr.getEnrollmentDate();
                    if (m_subSpan.getLastDateMemberType().equals(MemberDay.CURRENT)) {
                        m_lastInSessionDateCSY = enrollmentDate;
                    } else if (m_subSpan.getLastDateMemberType().equals(MemberDay.NEXT_IN_SESSION)) {

                        m_lastInSessionDateCSY = findSessionDate(insessionDates, enrollmentDate, true);
                    } else if (m_subSpan.getLastDateMemberType().equals(MemberDay.PREIOUS_IN_SESSION)) {
                        m_lastInSessionDateCSY = findSessionDate(insessionDates, enrollmentDate, false);
                    }
                }

                if ((currentSYendDate != null && m_lastInSessionDateCSY == null) ||
                        (currentSYendDate != null && m_lastInSessionDateCSY != null
                                && currentSYendDate.before(m_lastInSessionDateCSY))) {
                    if (insessionDates.contains(currentSYendDate)) {
                        m_lastInSessionDateCSY = currentSYendDate;
                    } else {
                        m_lastInSessionDateCSY = findSessionDate(insessionDates, currentSYendDate, false);
                    }
                }

                if (!insessionDates.contains(m_lastInSessionDateCSY)) {
                    m_lastInSessionDateCSY = findSessionDate(insessionDates, m_lastInSessionDateCSY, false);
                }
            }
            return m_lastInSessionDateCSY;
        }

        /**
         * get span last in session date limited by report date.
         *
         * @return Plain date
         */
        public PlainDate getLstInSessionDateLimitedByRD() {
            if (!m_isInitLastInSessionDateRD && m_lastInSessionDateRD == null) {
                m_isInitLastInSessionDateRD = true;
                List<PlainDate> insessionDates = getInsessionCalendarDatesLimitByRD();
                StudentEnrollment lastStdEnr = m_subSpan.getLastInSpanEnr();
                if (lastStdEnr != null) {
                    PlainDate enrollmentDate = lastStdEnr.getEnrollmentDate();
                    if (m_subSpan.getLastDateMemberType().equals(MemberDay.CURRENT)) {
                        m_lastInSessionDateRD = enrollmentDate;
                    } else if (m_subSpan.getLastDateMemberType().equals(MemberDay.NEXT_IN_SESSION)) {

                        m_lastInSessionDateRD = findSessionDate(insessionDates, enrollmentDate, true);
                    } else if (m_subSpan.getLastDateMemberType().equals(MemberDay.PREIOUS_IN_SESSION)) {
                        m_lastInSessionDateRD = findSessionDate(insessionDates, enrollmentDate, false);
                    }
                }

                if ((m_lastReportDate != null && m_lastInSessionDateRD == null) ||
                        (m_lastReportDate != null && m_lastInSessionDateRD != null
                                && m_lastReportDate.before(m_lastInSessionDateRD))) {
                    if (insessionDates.contains(m_lastReportDate)) {
                        m_lastInSessionDateRD = m_lastReportDate;
                    } else {
                        m_lastInSessionDateRD = findSessionDate(insessionDates, m_lastReportDate, false);
                    }
                }

                if (!insessionDates.contains(m_lastInSessionDateRD)) {
                    m_lastInSessionDateRD = findSessionDate(insessionDates, m_lastInSessionDateRD, false);
                }
            }
            return m_lastInSessionDateRD;
        }

        /**
         * return member ship days limited by report dates.
         *
         * @return List
         */
        public List<PlainDate> getMemberShipDaysInPeriod() {
            List<PlainDate> membershipDays = new ArrayList<PlainDate>();
            List<PlainDate> insessionDates = getInsessionCalendarDatesLimitByRD();

            // Most common calendar is used if there is an issue with student's casId,
            // so insessionDates can't be null.
            PlainDate firstDate = getFstInSessionDateLimitByRD();
            PlainDate lastDate = getLstInSessionDateLimitedByRD();
            if (insessionDates != null) {
                for (PlainDate date : insessionDates) {
                    if (firstDate != null && !date.before(firstDate) && (lastDate == null || !date.after(lastDate))) {
                        membershipDays.add(date);
                    }
                }
            }
            return membershipDays;
        }

        /**
         * return percentage of time enrolled for calendar.
         *
         * @return String
         */
        public String getPercentageForCalendar() {
            String percentage = null;
            StudentEnrollment firstSpanEnr = m_subSpan.getFirstInSpanEnr();

            // To determine the "Percentage of Time Enrolled for Calendar" on the report,
            // first examine the current StudentEnrollment.[DOE CALENDAR PERCENT].
            // If this value is populated and non-zero, it is used.
            percentage = (String) firstSpanEnr.getFieldValueByAlias(ALIAS_ENR_STD_CAL_PERCENT);
            double percentageDouble = 0;
            if (percentage != null) {
                try {
                    percentageDouble = Double.parseDouble(percentage);
                } catch (NumberFormatException e) {
                    // leave 0 for exception
                }
            }

            // If the value is not populated, check the value of
            // SchoolCalendar.[all-cas-PercentTimeEnrolled]
            // for the span's calendar.
            if (percentageDouble == 0) {
                // If it is populated and non-zero, the value is used.
                SchoolCalendar schoolCalendar = getCalendar();
                if (schoolCalendar != null) {
                    percentage = (String) schoolCalendar.getFieldValueByAlias(ALIAS_SKL_CAL_PERCENT);
                    if (percentage != null) {
                        try {
                            percentageDouble = Double.parseDouble(percentage);
                        } catch (NumberFormatException e) {
                            // leave 0 for exception
                        }
                    }
                }
                // Otherwise, the value is 100 percent.
                if (percentageDouble == 0) {
                    percentageDouble = 100;
                }
            }
            String percentageString = m_percentFormat.format(percentageDouble);

            return percentageString;
        }

        /**
         * return state resident status when span was started
         * If split rule not use all cases. And inside span we can have Y and S
         * records, resident status will give form first period.
         * Example: enrollments E S Y W, Split rule use only E and W. Inside S was changed resident
         * status.
         * But info will get from period E to S.
         *
         * @return String
         */
        public String getResidentStatus() {
            String residenceStatus = (String) getFirstEnr().getFieldValueByAlias("DOE RESIDENCE STATUS");
            String fieldResStatus = getCommonData().translateAliasToJavaName("DOE RESIDENCE STATUS");
            residenceStatus = lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                    fieldResStatus,
                    residenceStatus,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            return residenceStatus;
        }

        /**
         * return school when span was started
         * If split rule not use all cases. And inside span we can have Y and S
         * records, school will give form first period.
         * Example: enrollments E S Y W, Split rule use only E and W. Inside S was changed school.
         * But info will get from period E to S.
         *
         * @return Sis school
         */
        public SisSchool getSchool() {
            return m_subSpan.getFirstInSpanEnr().getSchool();
        }

        /**
         * Gets the special ed indicator.
         *
         * @return String
         */
        public String getSpecialEdIndicator() {
            String value = IEP_NO;

            Collection<StudentProgramParticipation> programs = m_commonData.getIepPrograms(getStudent().getOid());

            if (programs != null && !programs.isEmpty()) {
                for (StudentProgramParticipation program : programs) {
                    // If report date falls between start and end dates
                    if (!getFstInSessionDateLimitByCSY().before(program.getStartDate()) &&
                            (program.getEndDate() == null
                                    || !getFstInSessionDateLimitByCSY().after(program.getEndDate()))) {
                        value = IEP_YES;
                    } else if (program.getStartDate().before(getFstInSessionDateLimitByCSY()) && value != IEP_YES) {
                        PlainDate spedExitDate = program.getEndDate();
                        // Exited IEP end date is < 2 years
                        if (spedExitDate != null) {
                            Calendar exitDate = Calendar.getInstance();
                            exitDate.setTime(spedExitDate);
                            exitDate.add(Calendar.YEAR, 2);
                            spedExitDate = new PlainDate(exitDate.getTime());
                            if (spedExitDate.after(getFstInSessionDateLimitByCSY())) {
                                value = IEP_EXIT;
                            }
                        }
                    }
                }
            }
            return value;
        }

        /**
         * return student.
         *
         * @return Sis student
         */
        public SisStudent getStudent() {
            return getFirstEnr().getStudent();
        }

        /**
         * set PAChildCommonData.
         *
         * @param data void
         */
        public void setCommonData(PAChildCommonData data) {
            m_commonData = data;
            m_helper = data.getHelper();
            m_firstReportDate = data.getLimitStartDate();
            m_lastReportDate = data.getLimitEndDate();
        }

        /**
         * set SubSpan.
         *
         * @param subSpan void
         */
        @Override
        public void setSubSpan(SubSpan subSpan) {
            m_subSpan = subSpan;
        }

        /**
         * get first Enrollment from sub span.
         *
         * @return Student enrollment
         */
        private StudentEnrollment getFirstEnr() {
            return m_subSpan.getFirstInSpanEnr();
        }
    }

    /**
     * Class for Withdrawal and Enrollment rules.
     *
     * @author Follett Software Company
     */
    public class PAEWRuleImpl implements SplitRule<SpanWorker, Set<StudentEnrollment>> {

        private List<String> m_customActiveStatuses;
        private String m_enrollmentType;
        private MemberDay m_firstDayMemberType;
        private boolean m_isFirstInPeriod = false;
        private boolean m_isLastInPeriod = false;
        private MemberDay m_lastDayMemberType;
        private boolean m_useEmptyStatusLikeActive;
        private boolean m_useOnlyActiveEnrollment;

        /**
         * Instantiates a new PAEW rule impl.
         *
         * @param enrollmentType String
         * @param isFirstInPeriod boolean
         * @param isLastInPeriod boolean
         * @param firstDayMemberType MemberDay
         * @param lastDayMemberType MemberDay
         * @param useOnlyActiveEnrollment boolean
         * @param customActiveStatuses List<String>
         * @param useEmptyStatusLikeActive boolean
         */
        public PAEWRuleImpl(String enrollmentType,
                boolean isFirstInPeriod,
                boolean isLastInPeriod,
                MemberDay firstDayMemberType,
                MemberDay lastDayMemberType,
                boolean useOnlyActiveEnrollment,
                List<String> customActiveStatuses,
                boolean useEmptyStatusLikeActive) {
            m_enrollmentType = enrollmentType;
            m_isFirstInPeriod = isFirstInPeriod;
            m_isLastInPeriod = isLastInPeriod;
            m_firstDayMemberType = firstDayMemberType;
            m_lastDayMemberType = lastDayMemberType;
            m_useOnlyActiveEnrollment = useOnlyActiveEnrollment;
            m_customActiveStatuses = customActiveStatuses;
            m_useEmptyStatusLikeActive = useEmptyStatusLikeActive;
        }

        /**
         * return custom active statues. If empty - return default active statuses
         *
         * @return List
         * @see com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#getActiveStatuses()
         */
        @Override
        public List<String> getActiveStatuses() {
            List<String> activeStatus = null;
            if (m_customActiveStatuses == null) {
                m_customActiveStatuses = new ArrayList<String>();
            }
            if (m_customActiveStatuses.isEmpty()) {
                activeStatus = new ArrayList<String>(
                        StudentManager.getActiveStudentCodeList(getOrganizationFromHelper(), null));
            } else {
                activeStatus = m_customActiveStatuses;
            }

            return activeStatus;
        }

        /**
         * Gets the enrollment type.
         *
         * @return String
         * @see com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#getEnrollmentType()
         */
        @Override
        public String getEnrollmentType() {
            return m_enrollmentType;
        }

        /**
         * Gets the first day member type.
         *
         * @return Member day
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#getFirstDayMemberType()
         */
        @Override
        public MemberDay getFirstDayMemberType() {
            return m_firstDayMemberType;
        }

        /**
         * Gets the last day member type.
         *
         * @return Member day
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#getLastDayMemberType()
         */
        @Override
        public MemberDay getLastDayMemberType() {
            return m_lastDayMemberType;
        }

        /**
         * Checks if is first in period.
         *
         * @param currentEnrollment StudentEnrollment
         * @param subSpan SubSpan<SpanWorker>
         * @param nextEnrs Set<StudentEnrollment>
         * @return true, if is first in period
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#isFirstInPeriod(com.x2dev.
         *      sis.model.beans.StudentEnrollment,
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SubSpan, java.util.Set)
         */
        @Override
        public boolean isFirstInPeriod(StudentEnrollment currentEnrollment,
                                       SubSpan<SpanWorker> subSpan,
                                       Set<StudentEnrollment> nextEnrs) {
            boolean isFirstInPeriod = false;
            if (currentEnrollment.getEnrollmentType().equals(getEnrollmentType())) {
                isFirstInPeriod = true;
            }
            return isFirstInPeriod && m_isFirstInPeriod;
        }

        /**
         * Checks if is last in period.
         *
         * @param currentEnrollment StudentEnrollment
         * @param subSpan SubSpan<SpanWorker>
         * @param nextEnrs Set<StudentEnrollment>
         * @return true, if is last in period
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#isLastInPeriod(com.x2dev.
         *      sis.model.beans.StudentEnrollment,
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SubSpan, java.util.Set)
         */
        @Override
        public boolean isLastInPeriod(StudentEnrollment currentEnrollment,
                                      SubSpan<SpanWorker> subSpan,
                                      Set<StudentEnrollment> nextEnrs) {
            boolean isLastInPeriod = false;
            if (currentEnrollment.getEnrollmentType().equals(getEnrollmentType())) {
                isLastInPeriod = true;
            }
            return isLastInPeriod && m_isLastInPeriod;
        }

        /**
         * Checks if is skip record.
         *
         * @param currentEnrollment StudentEnrollment
         * @return true, if is skip record
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#isSkipRecord(com.x2dev.sis
         *      .model.beans.StudentEnrollment)
         */
        @Override
        public boolean isSkipRecord(StudentEnrollment currentEnrollment) {
            return false;
        }


        /**
         * Use empty status like active.
         *
         * @return true, if successful
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#useEmptyStatusLikeActive()
         */
        @Override
        public boolean useEmptyStatusLikeActive() {
            return m_useEmptyStatusLikeActive;
        }


        /**
         * Use only active enrollment.
         *
         * @return true, if successful
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#useOnlyActiveEnrollment()
         */
        @Override
        public boolean useOnlyActiveEnrollment() {
            return m_useOnlyActiveEnrollment;
        }

    }

    /**
     * class provided Rules for S records.
     *
     * @author Follett Software Company
     */
    public class PASRuleImpl implements SplitRule<SpanWorker, Set<StudentEnrollment>> {
        private List<String> m_customActiveStatuses;
        private String m_enrollmentType = StudentEnrollment.STATUS_CHANGE;
        private boolean m_useEmptyStatusLikeActive;
        private boolean m_useOnlyActiveEnrollment;

        /**
         * Instantiates a new PAS rule impl.
         *
         * @param useOnlyActiveEnrollment boolean
         * @param customActiveStatuses List<String>
         * @param useEmptyStatusLikeActive boolean
         */
        public PASRuleImpl(boolean useOnlyActiveEnrollment,
                List<String> customActiveStatuses,
                boolean useEmptyStatusLikeActive) {
            m_useOnlyActiveEnrollment = useOnlyActiveEnrollment;
            m_customActiveStatuses = customActiveStatuses;
            m_useEmptyStatusLikeActive = useEmptyStatusLikeActive;
        }

        /**
         * Gets the active statuses.
         *
         * @return List
         * @see com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#getActiveStatuses()
         */
        @Override
        public List<String> getActiveStatuses() {
            List<String> activeStatus = null;
            if (m_customActiveStatuses == null) {
                m_customActiveStatuses = new ArrayList<String>();
            }
            if (m_customActiveStatuses.isEmpty()) {
                activeStatus = new ArrayList<String>(
                        StudentManager.getActiveStudentCodeList(getOrganizationFromHelper(), null));
            } else {
                activeStatus = m_customActiveStatuses;
            }

            return activeStatus;
        }

        /**
         * return STATUS_CHANGE type.
         *
         * @return String
         * @see com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#getEnrollmentType()
         */
        @Override
        public String getEnrollmentType() {
            return m_enrollmentType;
        }

        /**
         * Gets the first day member type.
         *
         * @return Member day
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#getFirstDayMemberType()
         */
        @Override
        public MemberDay getFirstDayMemberType() {
            return MemberDay.CURRENT;
        }

        /**
         * Gets the last day member type.
         *
         * @return Member day
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#getLastDayMemberType()
         */
        @Override
        public MemberDay getLastDayMemberType() {
            return MemberDay.PREIOUS_IN_SESSION;
        }

        /**
         * Checks if is first in period.
         *
         * @param currentEnrollment StudentEnrollment
         * @param subSpan SubSpan<SpanWorker>
         * @param nextEnrs Set<StudentEnrollment>
         * @return true, if is first in period
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#isFirstInPeriod(com.x2dev.
         *      sis.model.beans.StudentEnrollment,
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SubSpan, java.util.Set)
         */
        @Override
        public boolean isFirstInPeriod(StudentEnrollment currentEnrollment,
                                       SubSpan<SpanWorker> subSpan,
                                       Set<StudentEnrollment> nextEnrs) {
            boolean isFirstInPeriod = false;
            if (currentEnrollment.getEnrollmentType().equals(getEnrollmentType())) {
                SubSpan previousSpan = subSpan.getPreviousSpan();
                if (previousSpan == null || hasChanges(currentEnrollment, subSpan, nextEnrs)) {
                    isFirstInPeriod = true;
                }
            }
            return isFirstInPeriod;
        }

        /**
         * Checks if is last in period.
         *
         * @param currentEnrollment StudentEnrollment
         * @param subSpan SubSpan<SpanWorker>
         * @param nextEnrs Set<StudentEnrollment>
         * @return true, if is last in period
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#isLastInPeriod(com.x2dev.
         *      sis.model.beans.StudentEnrollment,
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SubSpan, java.util.Set)
         */
        @Override
        public boolean isLastInPeriod(StudentEnrollment currentEnrollment,
                                      SubSpan<SpanWorker> subSpan,
                                      Set<StudentEnrollment> nextEnrs) {
            boolean isLastInPeriod = false;
            if (currentEnrollment.getEnrollmentType().equals(getEnrollmentType())) {
                if (hasChanges(currentEnrollment, subSpan, nextEnrs)) {
                    isLastInPeriod = true;
                }
            }
            return isLastInPeriod;
        }

        /**
         * Checks if is skip record.
         *
         * @param currentEnrollment StudentEnrollment
         * @return true, if is skip record
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#isSkipRecord(com.x2dev.sis
         *      .model.beans.StudentEnrollment)
         */
        @Override
        public boolean isSkipRecord(StudentEnrollment currentEnrollment) {
            return false;
        }

        /**
         * Use empty status like active.
         *
         * @return true, if successful
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#useEmptyStatusLikeActive()
         */
        @Override
        public boolean useEmptyStatusLikeActive() {

            return m_useEmptyStatusLikeActive;
        }

        /**
         * Use only active enrollment.
         *
         * @return true, if successful
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#useOnlyActiveEnrollment()
         */
        @Override
        public boolean useOnlyActiveEnrollment() {
            return m_useOnlyActiveEnrollment;
        }

        /**
         * return true if current enrollment is different from previous or next enrollments.
         *
         * @param currentEnrollment StudentEnrollment
         * @param subSpan SubSpan<SpanWorker>
         * @param nextEnrs Set<StudentEnrollment>
         * @return true, if successful
         * @see {@link #isStatusChanged(StudentEnrollment, SubSpan)}
         * @see {@link #isCalendarChanged(StudentEnrollment, Set)}
         */
        private boolean hasChanges(StudentEnrollment currentEnrollment,
                                   SubSpan<SpanWorker> subSpan,
                                   Set<StudentEnrollment> nextEnrs) {
            boolean hasChanges = isStatusChanged(currentEnrollment, subSpan);
            if (!hasChanges) {
                hasChanges = isCalendarChanged(currentEnrollment, nextEnrs);
            }
            return hasChanges;
        }

        /**
         * is current enrollment has another calendar then previous<br>
         * warning: current enrollment contain information about previous calendar .
         *
         * @param currentEnrollment StudentEnrollment
         * @param nextEnrs Set<StudentEnrollment>
         * @return true, if is calendar changed
         */
        private boolean isCalendarChanged(StudentEnrollment currentEnrollment, Set<StudentEnrollment> nextEnrs) {
            boolean calendarChanged = false;
            String previousCalendar = (String) currentEnrollment.getFieldValueByAlias(PAChildWorker.ALIAS_ENR_STD_CAL);
            previousCalendar = previousCalendar == null ? "" : previousCalendar;
            String currentCalendar = null;
            for (StudentEnrollment enrollment : nextEnrs) {
                currentCalendar = (String) enrollment.getFieldValueByAlias(PAChildWorker.ALIAS_ENR_STD_CAL);
                if (!StringUtils.isEmpty(currentCalendar)) {
                    break;
                }
            }
            if (StringUtils.isEmpty(currentCalendar)) {
                currentCalendar = currentEnrollment.getStudent().getCalendarCode();
            }
            currentCalendar = currentCalendar == null ? "" : currentCalendar;
            if (!currentCalendar.equals(previousCalendar)) {
                calendarChanged = true;
            }
            return calendarChanged;
        }

        /**
         * is current enrollment has another residence status then previous<br>
         * .
         *
         * @param currentEnrollment StudentEnrollment
         * @param subSpan SubSpan<SpanWorker>
         * @return true, if is status changed
         */
        private boolean isStatusChanged(StudentEnrollment currentEnrollment, SubSpan<SpanWorker> subSpan) {
            SubSpan<SpanWorker> spanWithPreviusEnr = subSpan.getPreviousSpan();
            boolean statusChanges = false;
            String currentStatus = (String) currentEnrollment.getFieldValueByAlias("DOE RESIDENCE STATUS");
            currentStatus = currentStatus == null ? "" : currentStatus;
            String previousStatus = null;
            if (spanWithPreviusEnr != null) {
                previousStatus =
                        (String) spanWithPreviusEnr.getFirstInSpanEnr().getFieldValueByAlias("DOE RESIDENCE STATUS");
                previousStatus = previousStatus == null ? "" : previousStatus;
                if (!currentStatus.equals(previousStatus)) {
                    statusChanges = true;
                }
            }

            return statusChanges;
        }
    }

    /**
     * class provided Rules for Y records.
     *
     * @author Follett Software Company
     */
    public class PAYRuleImpl implements SplitRule<SpanWorker, Set<StudentEnrollment>> {
        private List<String> m_customActiveStatuses;
        private String m_enrollmentType = StudentEnrollment.YOG_CHANGE;
        private boolean m_useEmptyStatusLikeActive;
        private boolean m_useOnlyActiveEnrollment;

        /**
         * Instantiates a new PAY rule impl.
         *
         * @param useOnlyActiveEnrollment boolean
         * @param customActiveStatuses List<String>
         * @param useEmptyStatusLikeActive boolean
         */
        PAYRuleImpl(boolean useOnlyActiveEnrollment,
                List<String> customActiveStatuses,
                boolean useEmptyStatusLikeActive) {
            m_useOnlyActiveEnrollment = useOnlyActiveEnrollment;
            m_customActiveStatuses = customActiveStatuses;
            m_useEmptyStatusLikeActive = useEmptyStatusLikeActive;
        }

        /**
         * Gets the active statuses.
         *
         * @return List
         * @see com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#getActiveStatuses()
         */
        @Override
        public List<String> getActiveStatuses() {
            List<String> activeStatus = null;
            if (m_customActiveStatuses == null) {
                m_customActiveStatuses = new ArrayList<String>();
            }
            if (m_customActiveStatuses.isEmpty()) {
                activeStatus = new ArrayList<String>(
                        StudentManager.getActiveStudentCodeList(getOrganizationFromHelper(), null));
            } else {
                activeStatus = m_customActiveStatuses;
            }

            return activeStatus;
        }

        /**
         * return YOG_CHANGE type .
         *
         * @return String
         * @see com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#getEnrollmentType()
         */
        @Override
        public String getEnrollmentType() {
            return m_enrollmentType;
        }

        /**
         * Gets the first day member type.
         *
         * @return Member day
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#getFirstDayMemberType()
         */
        @Override
        public MemberDay getFirstDayMemberType() {

            return MemberDay.CURRENT;
        }

        /**
         * Gets the last day member type.
         *
         * @return Member day
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#getLastDayMemberType()
         */
        @Override
        public MemberDay getLastDayMemberType() {
            return MemberDay.PREIOUS_IN_SESSION;
        }

        /**
         * Checks if is first in period.
         *
         * @param currentEnrollment StudentEnrollment
         * @param subSpan SubSpan<SpanWorker>
         * @param nextEnrs Set<StudentEnrollment>
         * @return true, if is first in period
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#isFirstInPeriod(com.x2dev.
         *      sis.model.beans.StudentEnrollment,
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SubSpan, java.util.Set)
         */
        @Override
        public boolean isFirstInPeriod(StudentEnrollment currentEnrollment,
                                       SubSpan<SpanWorker> subSpan,
                                       Set<StudentEnrollment> nextEnrs) {
            boolean isFirstInPeriod = false;
            if (currentEnrollment.getEnrollmentType().equals(getEnrollmentType())) {
                SubSpan previousSpan = subSpan.getPreviousSpan();
                if (previousSpan == null || hasChanges(currentEnrollment, previousSpan)) {
                    isFirstInPeriod = true;
                }
            }
            return isFirstInPeriod;

        }

        /**
         * Checks if is last in period.
         *
         * @param currentEnrollment StudentEnrollment
         * @param subSpan SubSpan<SpanWorker>
         * @param nextEnrs Set<StudentEnrollment>
         * @return true, if is last in period
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#isLastInPeriod(com.x2dev.
         *      sis.model.beans.StudentEnrollment,
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SubSpan, java.util.Set)
         */
        @Override
        public boolean isLastInPeriod(StudentEnrollment currentEnrollment,
                                      SubSpan<SpanWorker> subSpan,
                                      Set<StudentEnrollment> nextEnrs) {
            boolean isLastInPeriod = false;
            if (currentEnrollment.getEnrollmentType().equals(getEnrollmentType())) {
                if (hasChanges(currentEnrollment, subSpan)) {
                    isLastInPeriod = true;
                }
            }
            return isLastInPeriod;
        }

        /**
         * Checks if is skip record.
         *
         * @param currentEnrollment StudentEnrollment
         * @return true, if is skip record
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#isSkipRecord(com.x2dev.sis
         *      .model.beans.StudentEnrollment)
         */
        @Override
        public boolean isSkipRecord(StudentEnrollment currentEnrollment) {
            return false;
        }

        /**
         * Use empty status like active.
         *
         * @return true, if successful
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#useEmptyStatusLikeActive()
         */
        @Override
        public boolean useEmptyStatusLikeActive() {

            return m_useEmptyStatusLikeActive;
        }

        /**
         * Use only active enrollment.
         *
         * @return true, if successful
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitRule#useOnlyActiveEnrollment()
         */
        @Override
        public boolean useOnlyActiveEnrollment() {
            return m_useOnlyActiveEnrollment;
        }

        /**
         * return true if current enrollment is different from previous or next enrollments
         * method check Yog between current and previous enrollent.
         *
         * @param currentEnrollment StudentEnrollment
         * @param spanWithPreviusEnr SubSpan
         * @return true, if successful
         */
        private boolean hasChanges(StudentEnrollment currentEnrollment, SubSpan spanWithPreviusEnr) {
            boolean hasChanges = false;
            int currentYog = currentEnrollment.getYog();
            int previousYog = spanWithPreviusEnr.getFirstInSpanEnr().getYog();
            if (currentYog != previousYog) {
                hasChanges = true;
            }
            return hasChanges;
        }
    }

    /**
     * class provide default behavior for split span
     * class sorted inbox student enrollment by date and Timestamp
     * class split enrollment by inbox rules.
     *
     * @author Follett Software Company
     * @param <W> the generic type
     * @param <S> the generic type
     */
    public class SplitSpanDefaultBehavior<W extends SpanWorker, S extends SubSpan> implements SplitSpan<W, S> {
        private Organization m_organization;
        private List<SplitRule> m_rules = new ArrayList<SplitRule>();
        private WorkerSubSpanFabric<W, S> m_wFabric = null;

        private final Comparator<StudentEnrollment> m_comarator = new Comparator<StudentEnrollment>() {

            @Override
            public int compare(StudentEnrollment o1, StudentEnrollment o2) {
                int result = o1.getEnrollmentDate().compareTo(o2.getEnrollmentDate());
                if (result == 0) {
                    result = Long.valueOf(o1.getTimestamp()).compareTo(Long.valueOf(o2.getTimestamp()));
                }
                return result; // return the results
            }
        };

        /**
         * Instantiates a new split span default behavior.
         *
         * @param wFabric WorkerSubSpanFabric<W,S>
         */
        public SplitSpanDefaultBehavior(WorkerSubSpanFabric<W, S> wFabric) {
            m_organization = getOrganizationFromHelper();
            m_wFabric = wFabric;
        }

        /**
         * using inboxed WorkerSubSpanFabric.
         *
         * @param previousSpan S
         * @return S
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitSpan#createSubSpan(com.x2dev.
         *      procedures.statereporting.pa.SubSpanHelper.SubSpan)
         */
        @Override
        public S createSubSpan(S previousSpan) {
            return m_wFabric.createSubSpan(previousSpan);

        }

        /**
         * using inboxed WorkerSubSpanFabric.
         *
         * @return w
         * @see com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitSpan#getNewWorker()
         */
        @Override
        public W getNewWorker() {
            return m_wFabric.createWorker();
        }

        /**
         * Gets the organization.
         *
         * @return Organization
         * @see com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitSpan#getOrganization()
         */
        @Override
        public Organization getOrganization() {
            return m_organization;
        }

        /**
         * Sets the split rule.
         *
         * @param rule void
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitSpan#setSplitRule(com.x2dev.
         *      procedures.statereporting.pa.SubSpanHelper.SplitRule)
         */
        @Override
        public void setSplitRule(SplitRule rule) {
            m_rules.add(rule);

        }

        /**
         * Sets the split rules.
         *
         * @param rules void
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitSpan#setSplitRules(java.util.
         *      List)
         */
        @Override
        public void setSplitRules(List<SplitRule> rules) {
            m_rules.addAll(rules);

        }

        /**
         * Split span.
         *
         * @param enrollments List<StudentEnrollment>
         * @param deleteSkipedRecord boolean
         * @return List
         * @see
         *      com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitSpan#splitSpan(java.util.List,
         *      boolean)
         */
        @Override
        public List<SubSpan<W>> splitSpan(List<StudentEnrollment> enrollments, boolean deleteSkipedRecord) {

            Set<StudentEnrollment> enrollmentsSet = new TreeSet<StudentEnrollment>(m_comarator);
            Set<StudentEnrollment> enrollmentsSetNext = new TreeSet<StudentEnrollment>(m_comarator);

            enrollmentsSet.addAll(enrollments);
            enrollmentsSetNext.addAll(enrollmentsSet);
            List<SubSpan<W>> subSpans = new ArrayList<SubSpan<W>>();
            S subSpan = createSubSpan(null);
            for (StudentEnrollment currentEnr : enrollmentsSet) {
                enrollmentsSetNext.remove(currentEnr);
                if (validateEnrollment(currentEnr)) {
                    SplitRule rule = getRule(currentEnr);
                    if (rule != null) {
                        StudentEnrollment firstEnrollment = subSpan.getFirstInSpanEnr();

                        if (firstEnrollment == null) {
                            if (rule.isFirstInPeriod(currentEnr, subSpan, enrollmentsSetNext)) {
                                subSpan.setFirstInSpanEnr(currentEnr);
                                subSpans.add(subSpan);
                                // need to start put enrollments form first in period
                                subSpan.setFirstDateMemberType(rule.getFirstDayMemberType());
                                subSpan.addEnrollment(currentEnr);
                            }
                        } else {
                            // need to put all next enrollments after first in period
                            subSpan.addEnrollment(currentEnr);
                            if (rule.isLastInPeriod(currentEnr, subSpan, enrollmentsSetNext)) {
                                subSpan.setLastInSpanEnr(currentEnr);
                                subSpan.setLastDateMemberType(rule.getLastDayMemberType());
                                subSpan = createSubSpan(subSpan);
                                if (rule.isFirstInPeriod(currentEnr, subSpan, enrollmentsSetNext)) {
                                    subSpan.setFirstInSpanEnr(currentEnr);
                                    // need to start put enrollments form first in period
                                    subSpan.addEnrollment(currentEnr);
                                    subSpan.setFirstDateMemberType(rule.getFirstDayMemberType());
                                    subSpans.add(subSpan);
                                }
                            }
                        }
                    }
                }
            }

            if (deleteSkipedRecord) {
                Iterator<SubSpan<W>> iterator = subSpans.iterator();
                while (iterator.hasNext()) {
                    SubSpan<W> currentSubSpan = iterator.next();
                    if (currentSubSpan.isSkipRecord()) {
                        iterator.remove();
                        currentSubSpan.removeLinks();
                    }
                }
            }

            return subSpans;
        }

        /**
         * return appropriate rule for currentEnr
         * used enrollment type for matches.
         *
         * @param currentEnr StudentEnrollment
         * @return Split rule
         */
        private SplitRule getRule(StudentEnrollment currentEnr) {
            SplitRule returnRule = null;
            for (SplitRule rule : m_rules) {
                if (rule.getEnrollmentType().equals(currentEnr.getEnrollmentType())) {
                    returnRule = rule;
                    break;
                }
            }
            return returnRule;
        }

        /**
         * validate Enrollment
         * used rules from appropriate SplitRule .
         *
         * @param currentEnrollment StudentEnrollment
         * @return true, if successful
         */
        private boolean validateEnrollment(StudentEnrollment currentEnrollment) {
            boolean valid = true;
            SplitRule rule = getRule(currentEnrollment);
            if (rule != null && rule.useOnlyActiveEnrollment()) {
                String statusCode = currentEnrollment.getStatusCode();

                if ((!StringUtils.isEmpty(statusCode) && !rule.getActiveStatuses().contains(statusCode))
                        || (StringUtils.isEmpty(statusCode) && !rule.useEmptyStatusLikeActive())) {
                    valid = false;
                }

            }

            return valid;
        }


    }

    /**
     * container contain information about Student Enrollments inside period<br>
     * has information about first in period enrollment<br>
     * last in period enrollment<br>
     * list Student enrollments belong to this period<br>
     * SubSpan linked between another SubSpan and has information about<br>
     * previous, next subSpan<br>
     * subSpan size and current position<br>
     * SubSpan can contain SpanWorker for empowerment.
     *
     * @author Follett Software Company
     * @param <T> the generic type
     */
    public abstract class SubSpan<T extends SpanWorker> {
        private int m_currentPosition = 0;
        private MemberDay m_firstDayMemberType = null;
        private StudentEnrollment m_firstInSpanEnr = null;
        private MemberDay m_lastDayMemberType = null;
        private StudentEnrollment m_lastInSpanEnr = null;
        private List<StudentEnrollment> m_listStudentEnr = new ArrayList<StudentEnrollment>();
        private SubSpan m_nextSpan = null;
        private SubSpan m_previousSpan = null;
        private int m_totalSpans = 0;
        private T m_worker = null;

        /**
         * Instantiates a new sub span.
         *
         * @param previousSpan SubSpan
         * @param worker T
         */
        public SubSpan(SubSpan previousSpan, T worker) {
            m_previousSpan = previousSpan;

            if (m_previousSpan == null) {
                m_currentPosition = 1;
            } else {
                m_previousSpan.setNextSpan(this);
                m_currentPosition = m_previousSpan.getCurrentNumberOfSpan() + 1;
            }
            setTotalSpans(m_currentPosition);
            m_worker = worker;
            m_worker.setSubSpan(this);
        }

        /**
         * method for put enrollment into List<StudentEnrollment>
         * belong to period .
         *
         * @param enrollment StudentEnrollment
         */
        public void addEnrollment(StudentEnrollment enrollment) {
            getListStuentEnr().add(enrollment);
        }

        /**
         * return current position relatively to linked between another Subspan.
         *
         * @return int
         */
        public int getCurrentNumberOfSpan() {
            return m_currentPosition;
        }

        /**
         * return DayMember Type for first in preiod date.
         *
         * @return Member day
         */
        public MemberDay getFirstDateMemberType() {
            return m_firstDayMemberType;
        }


        /**
         * return first in period enrollment.
         *
         * @return Student enrollment
         */
        public StudentEnrollment getFirstInSpanEnr() {
            return m_firstInSpanEnr;
        }


        /**
         * return DayMember Type for last in period date.
         *
         * @return Member day
         */
        public MemberDay getLastDateMemberType() {
            return m_lastDayMemberType;
        }

        /**
         * return last in period enrollment.
         *
         * @return Student enrollment
         */
        public StudentEnrollment getLastInSpanEnr() {
            return m_lastInSpanEnr;
        }

        /**
         * return list StudentEnrollment belong to this period.
         *
         * @return List
         */
        public List<StudentEnrollment> getListStuentEnr() {
            return m_listStudentEnr;
        }

        /**
         * return next linked SubSpan.
         *
         * @return Sub span
         */
        public SubSpan getNextSpan() {
            return m_nextSpan;
        }

        /**
         * return previous lnked SubSpan.
         *
         * @return Sub span
         */
        public SubSpan getPreviousSpan() {
            return m_previousSpan;
        }

        /**
         * return concatenated EnrollmentType from first and last enrollments.
         *
         * @return String
         */
        public String getSpanType() {
            StringBuilder spanType = new StringBuilder();
            if (getFirstInSpanEnr() != null) {
                spanType.append(getFirstInSpanEnr().getEnrollmentType());
                if (getLastInSpanEnr() != null) {
                    spanType.append(getLastInSpanEnr().getEnrollmentType());
                } else {
                    spanType.append("N");
                }
            }
            return spanType.toString();
        }

        /**
         * return total counted spans.
         *
         * @return int
         */
        public int getTotalSpans() {
            return m_totalSpans;
        }

        /**
         * return Worker.
         *
         * @return t
         */
        public T getWorker() {
            return m_worker;
        }

        /**
         * validate does this span needed.
         *
         * @return true, if is skip record
         */
        public abstract boolean isSkipRecord();


        /**
         * remove current span form linked, now it is not recalculate position !!!<br>
         * should be fixed!!!.
         */
        public void removeLinks() {
            SubSpan previous = getPreviousSpan();

            SubSpan next = getNextSpan();
            if (next != null) {
                next.setPreviousSpan(previous);
            }
            if (previous != null) {
                previous.setNextSpan(next);
                // we need to start recalculate form near previous
                previous.recalculatePosition();
            } else if (next != null) {
                next.recalculatePosition();
            }
        }

        /**
         * return current position in SubSpan linked .
         *
         * @param numberSubSpan void
         */
        public void setCurrentNumberOfSpan(int numberSubSpan) {
            m_currentPosition = numberSubSpan;
        }

        /**
         * set MemberType for first in period date.
         *
         * @param firstDayMemberType void
         */
        public void setFirstDateMemberType(MemberDay firstDayMemberType) {
            m_firstDayMemberType = firstDayMemberType;
        }

        /**
         * set first in period enrollment.
         *
         * @param enr void
         */
        public void setFirstInSpanEnr(StudentEnrollment enr) {
            m_firstInSpanEnr = enr;
        }

        /**
         * set MemberType for last in period date.
         *
         * @param lastDayMemberType void
         */
        public void setLastDateMemberType(MemberDay lastDayMemberType) {
            m_lastDayMemberType = lastDayMemberType;
        }

        /**
         * set last in period enrollment.
         *
         * @param enr void
         */
        public void setLastInSpanEnr(StudentEnrollment enr) {
            m_lastInSpanEnr = enr;
        }

        /**
         * set next span<br>
         * warning it is not recalculate position.
         *
         * @param nextSpan void
         */
        public void setNextSpan(SubSpan nextSpan) {
            m_nextSpan = nextSpan;
        }

        /**
         * Gets the first sub span.
         *
         * @return Sub span
         */
        private SubSpan getFirstSubSpan() {
            SubSpan firsSpan = this;
            SubSpan tempFirstSan = getPreviousSpan();
            while (tempFirstSan != null) {
                if (tempFirstSan != null) {
                    firsSpan = tempFirstSan;
                }
                tempFirstSan = tempFirstSan.getPreviousSpan();
            }
            return firsSpan;
        }

        /**
         * Recalculate position.
         */
        private void recalculatePosition() {
            SubSpan firstSpan = getFirstSubSpan();
            SubSpan nextSpan = firstSpan.getNextSpan();
            int totalCount = getTotalCountSubSpan(firstSpan);
            firstSpan.setTotalSpans(totalCount);
            int currentPosition = 1;
            firstSpan.setCurrentNumberOfSpan(currentPosition);
            while (nextSpan != null) {
                currentPosition++;
                nextSpan.setCurrentNumberOfSpan(currentPosition);
                nextSpan.setTotalSpans(totalCount);
                nextSpan = nextSpan.getNextSpan();
            }


        }

        /**
         * Gets the total count sub span.
         *
         * @param firstSpan SubSpan
         * @return int
         */
        private int getTotalCountSubSpan(SubSpan firstSpan) {
            SubSpan nextSpan = firstSpan.getNextSpan();
            int totalCount = 1;
            while (nextSpan != null) {
                totalCount++;
                nextSpan = nextSpan.getNextSpan();
            }
            return totalCount;
        }

        /**
         * set previous span<br>
         * warning it is not recalculate position.
         *
         * @param subSpan void
         */
        private void setPreviousSpan(SubSpan subSpan) {
            m_previousSpan = subSpan;
        }

        /**
         * set total count linked subSpan.
         *
         * @param total void
         */
        private void setTotalSpans(int total) {
            m_totalSpans = total;
            if (m_previousSpan != null) {
                m_previousSpan.setTotalSpans(total);
            }
        }
    }

    /**
     * SubSpan for PAChildAccountingSummaryData.
     *
     * @author Follett Software Company
     */
    public class SubSpanPAChild extends SubSpan<PAChildWorker> {

        /**
         * Instantiates a new sub span PA child.
         *
         * @param previousSpan SubSpan
         * @param worker PAChildWorker
         */
        SubSpanPAChild(SubSpan previousSpan, PAChildWorker worker) {
            super(previousSpan, worker);

        }

        /**
         * skip Record if calendarId is empty, if user select school/s for limited and span contain
         * another school,
         * if span doesn't belong to report period, if user select calendar for limited and span
         * contain another calendar.
         *
         * @return true, if is skip record
         * @see com.x2dev.procedures.statereporting.pa.SubSpanHelper.SubSpan#isSkipRecord()
         */
        @Override
        public boolean isSkipRecord() {
            return isCalendarIsEmpty() || !isReportSchool() || !isCrossPeriod() || !isReportCalendar();
        }

        /**
         * Checks if is calendar is empty.
         *
         * @return true, if is calendar is empty
         */
        private boolean isCalendarIsEmpty() {
            boolean isEmpty = false;
            if (StringUtils.isEmpty(getWorker().getCalendarId())) {
                isEmpty = true;
            }
            return isEmpty;
        }

        /**
         * Checks if is cross period.
         *
         * @return true, if is cross period
         */
        private boolean isCrossPeriod() {
            boolean isCrossPeriod = false;
            PAChildCommonData data = getWorker().getCommonData();
            PlainDate startDate = data.getLimitStartDate();
            PlainDate endDate = data.getLimitEndDate();
            PlainDate enrStartDate = getFirstInSpanEnr().getEnrollmentDate();
            PlainDate enrEndDate = getLastInSpanEnr() == null ? null : getLastInSpanEnr().getEnrollmentDate();
            if (startDate == null && endDate == null) {
                isCrossPeriod = true;
            }
            if ((endDate == null || (!enrStartDate.after(endDate)))
                    &&
                    (enrEndDate == null || (!startDate.after(enrEndDate)))) {
                isCrossPeriod = true;
            }
            return isCrossPeriod;
        }

        /**
         * Checks if is report calendar.
         *
         * @return true, if is report calendar
         */
        private boolean isReportCalendar() {
            boolean isReportCalendar = false;
            Boolean isAllCalendars = getWorker().getCommonData().getIsAllCalendars();
            String calendarIdFromInput = getWorker().getCommonData().getCalendarId();
            String calendarIdFromSpan = getWorker().getCalendarId();
            if (!StringUtils.isEmpty(calendarIdFromSpan)) {
                if (isAllCalendars.booleanValue() ||
                        (calendarIdFromInput != null && calendarIdFromSpan.equals(calendarIdFromInput))) {
                    isReportCalendar = true;
                }
            }
            return isReportCalendar;
        }

        /**
         * Checks if is report school.
         *
         * @return true, if is report school
         */
        private boolean isReportSchool() {
            return getWorker().getCommonData().getSchools().contains(getWorker().getSchool());
        }

    }

    /**
     * class for PAStudentCalendarFact.
     *
     * @author Follett Software Company
     */
    public class SubSpanPACF extends SubSpan<PAChildWorker> {

        /**
         * Instantiates a new sub span PACF.
         *
         * @param previousSpan SubSpan
         * @param worker PAChildWorker
         */
        SubSpanPACF(SubSpan previousSpan, PAChildWorker worker) {
            super(previousSpan, worker);

        }

        /**
         * skip Record if calendarId is empty, if user select school/s for limited and span contain
         * another school,
         * if span doesn't belong to report period.
         *
         * @return true, if is skip record
         * @see com.x2dev.procedures.statereporting.pa.SubSpanHelper.SubSpan#isSkipRecord()
         */
        @Override
        public boolean isSkipRecord() {
            return isCalendarIsEmpty() || !isReportSchool() || !isCrossPeriod();
        }

        /**
         * Checks if is calendar is empty.
         *
         * @return true, if is calendar is empty
         */
        private boolean isCalendarIsEmpty() {
            boolean isEmpty = false;
            if (StringUtils.isEmpty(getWorker().getCalendarId())) {
                isEmpty = true;
            }
            return isEmpty;
        }

        /**
         * Checks if is cross period.
         *
         * @return true, if is cross period
         */
        private boolean isCrossPeriod() {
            boolean isCrossPeriod = false;
            PAChildCommonData data = getWorker().getCommonData();
            PlainDate startDate = data.getLimitStartDate();
            PlainDate endDate = data.getLimitEndDate();
            PlainDate enrStartDate = getFirstInSpanEnr().getEnrollmentDate();
            PlainDate enrEndDate = getLastInSpanEnr() == null ? null : getLastInSpanEnr().getEnrollmentDate();
            if (startDate == null && endDate == null) {
                isCrossPeriod = true;
            }
            if ((endDate == null || (!enrStartDate.after(endDate)))
                    &&
                    (enrEndDate == null || (startDate != null && !startDate.after(enrEndDate)))) {
                isCrossPeriod = true;
            }
            return isCrossPeriod;
        }

        /**
         * Checks if is report school.
         *
         * @return true, if is report school
         */
        private boolean isReportSchool() {
            Collection<School> schools = getWorker().getCommonData().getSchools();

            return schools.isEmpty() || schools.contains((getWorker().getSchool()));
        }

    }

    public static final String SPLIT_RULES_PA_CHILD = "pachildRules";

    private X2Broker m_brokerSSH = null;
    private DataDictionary m_dictionary;
    private Organization m_organizationSSH = null;
    private boolean m_preferenceMemberOnEntry;
    private boolean m_preferenceMemberOnWithdrawal;
    private Map<String, Map<String, ReferenceCode>> m_refTableMap = null;
    private StudentHistoryHelper m_studentHelperSSH = null;

    /**
     * Instantiates a new sub span helper.
     *
     * @param studentHelper StudentHistoryHelper
     * @param organization Organization
     * @param borker X2Broker
     */
    public SubSpanHelper(StudentHistoryHelper studentHelper, Organization organization, X2Broker borker) {
        m_brokerSSH = borker;
        m_organizationSSH = organization;
        m_studentHelperSSH = studentHelper;
    }

    /**
     * Returns a local instance of a district data dictionary.
     *
     * @param extendedDictionary ExtendedDictionaryAttributes
     * @return DataDictionary.
     */
    public DataDictionary getDataDictionary(ExtendedDictionaryAttributes extendedDictionary) {
        if (m_dictionary == null) {
            if (extendedDictionary != null) {
                m_dictionary =
                        DataDictionary.getDistrictDictionary(extendedDictionary, m_brokerSSH.getPersistenceKey());
            } else {
                m_dictionary = DataDictionary.getDistrictDictionary(m_brokerSSH.getPersistenceKey());
            }



        }

        return m_dictionary;

    }

    /**
     * Lookup and return a DataDictionaryField based on a root bean and bean path.
     * This allows multi-hop paths in the bean path.
     *
     *
     * @param beanClass Class
     * @param path String
     * @return DataDictionaryField
     */
    public DataDictionaryField getDataDictionaryField(Class beanClass, String path) {
        ModelProperty prop = new ModelProperty(beanClass, path, m_brokerSSH.getPersistenceKey());
        DataDictionaryField dictionaryField = getDataDictionary(null).findDataDictionaryField(prop.getFieldId());

        return dictionaryField;
    }

    /**
     * return new PAChildCommonData.
     *
     * @param schools Collection<School>
     * @param isAllCalendars Boolean
     * @param calendarId String
     * @return PA child common data
     */
    public PAChildCommonData getPAChildCommonData(Collection<School> schools,
                                                  Boolean isAllCalendars,
                                                  String calendarId) {
        return new PAChildCommonData(m_studentHelperSSH, schools, isAllCalendars, calendarId, m_brokerSSH,
                m_organizationSSH);
    }

    /**
     * return new PAChildWorker.
     *
     * @return PA child worker
     */
    public PAChildWorker getPaChildWorker() {
        return new PAChildWorker();
    }

    /**
     * Gets the organization from helper.
     *
     * @return Organization
     */
    public Organization getOrganizationFromHelper() {
        return m_organizationSSH;
    }

    /**
     * Lookup a map of reference codes for a reference table oid.
     * Cache the results for later use.
     *
     * @param referenceTableOid String
     * @return Map<String, ReferenceCode>
     */
    public Map<String, ReferenceCode> getReferenceCodes(String referenceTableOid) {
        Map<String, ReferenceCode> codeMap = null;
        if (m_refTableMap == null) {
            m_refTableMap = new HashMap<String, Map<String, ReferenceCode>>();
        }

        if (m_refTableMap.containsKey(referenceTableOid)) {
            codeMap = m_refTableMap.get(referenceTableOid);
        } else {
            codeMap = new HashMap<String, ReferenceCode>();
            ReferenceTable refTable =
                    (ReferenceTable) m_brokerSSH.getBeanByOid(ReferenceTable.class, referenceTableOid);
            if (refTable != null) {
                codeMap = refTable.getCodeMap(m_brokerSSH);
            }
            m_refTableMap.put(referenceTableOid, codeMap);
        }

        return codeMap;
    }

    /**
     * get rules by bunchName
     * bunchName it is Static string which started by SPLIT_RULES_.
     *
     * @param bunchName String
     * @return List
     */
    public List<SplitRule> getRules(String bunchName) {
        List<SplitRule> rules = null;
        if (bunchName.equals(SPLIT_RULES_PA_CHILD)) {
            rules = getRulesForPAChild();
        }
        if (rules == null) {
            rules = new ArrayList<SplitRule>();
        }
        return rules;
    }

    /**
     * return new SplitSpanDefaultBehavior.
     *
     * @param fabric WorkerSubSpanFabric
     * @param rules List<SplitRule>
     * @return Split span default behavior
     */
    public SplitSpanDefaultBehavior getSplitSPanDefaultBehavior(WorkerSubSpanFabric fabric, List<SplitRule> rules) {
        SplitSpanDefaultBehavior defaultSplit = new SplitSpanDefaultBehavior<PAChildWorker, SubSpanPACF>(fabric);
        defaultSplit.setSplitRules(rules);
        return defaultSplit;
    }

    /**
     * return new SubSpanPACF.
     *
     * @param previousSpan SubSpan
     * @param worker PAChildWorker
     * @return Sub span PACF
     */
    public SubSpanPACF getSubSpanPACF(SubSpan previousSpan, PAChildWorker worker) {
        return new SubSpanPACF(previousSpan, worker);
    }

    /**
     * return new SubSpanPAChild.
     *
     * @param previousSpan SubSpan
     * @param worker PAChildWorker
     * @return Sub span PA child
     */
    public SubSpanPAChild getSubSpanPAChild(SubSpan previousSpan, PAChildWorker worker) {
        return new SubSpanPAChild(previousSpan, worker);
    }

    /**
     * Returns the state lookup code for field value.
     * Look up based on bean path.
     *
     * @param beanClass - data dictionary table class to lookup in
     * @param beanPath - data dictionary field path from the passed table to lookup in
     * @param value - the value to lookup and translate in the lookup table.
     * @param referenceMap - reference map type (ExportFormatField.ReferenceMapTypeCode.*.ordinal())
     *        of the lookup.
     *
     * @return String - state code for input value.
     */
    public String lookupReferenceCodeByBeanPath(Class beanClass, String beanPath, String value, int referenceMap) {
        String stateValue = null;
        DataDictionaryField dictionaryField = getDataDictionaryField(beanClass, beanPath);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            stateValue = lookupReferenceCodeByRefTbl(dictionaryField.getReferenceTableOid(), value, referenceMap);
        }

        return stateValue;
    }



    /**
     * Returns the lookup code value for field value.
     * Look up based on the reference table.
     *
     * @param referenceTableOid String
     * @param value - the value to lookup and translate in the lookup table.
     * @param referenceMap - the reference map type
     *        (ExportFormatField.ReferenceMapTypeCode.*.ordinal()) of the lookup.
     * @return String - reference code lookup value for input value.
     */
    public String lookupReferenceCodeByRefTbl(String referenceTableOid, String value, int referenceMap) {
        String returnValue = null;
        Map<String, ReferenceCode> refCodes = getReferenceCodes(referenceTableOid);
        ReferenceCode code = refCodes.get(value);
        if (code != null) {
            if (referenceMap == ExportFormatField.ReferenceMapTypeCode.STATE.ordinal()) {
                returnValue = code.getStateCode();
            } else if (referenceMap == ExportFormatField.ReferenceMapTypeCode.FEDERAL.ordinal()) {
                returnValue = code.getFederalCode();
            } else if (referenceMap == ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal()) {
                returnValue = code.getLocalCode();
            } else if (referenceMap == ExportFormatField.ReferenceMapTypeCode.SYSTEM.ordinal()) {
                returnValue = code.getSystemCode();
            }
        }

        return returnValue;
    }


    /**
     * return list of rule for PAChild.
     *
     * @return List
     */
    private List<SplitRule> getRulesForPAChild() {
        m_preferenceMemberOnEntry = Boolean.valueOf(PreferenceManager.getPreferenceValue(m_organizationSSH,
                SisPreferenceConstants.STUDENT_MEMBER_ON_ENTRY_DATE)).booleanValue();
        m_preferenceMemberOnWithdrawal = Boolean.valueOf(PreferenceManager.getPreferenceValue(m_organizationSSH,
                SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE)).booleanValue();
        List<SplitRule> rules = new ArrayList<SplitRule>();
        MemberDay eDayType = m_preferenceMemberOnEntry ? MemberDay.CURRENT : MemberDay.NEXT_IN_SESSION;
        MemberDay wDayType = m_preferenceMemberOnWithdrawal ? MemberDay.CURRENT : MemberDay.PREIOUS_IN_SESSION;
        SplitRule eRule = new PAEWRuleImpl(StudentEnrollment.ENTRY, true, false, eDayType, null, true, null, true);
        SplitRule wRule =
                new PAEWRuleImpl(StudentEnrollment.WITHDRAWAL, false, true, null, wDayType, false, null, true);
        SplitRule sRule = new PASRuleImpl(true, null, true);
        SplitRule yRule = new PAYRuleImpl(true, null, true);
        rules.add(eRule);
        rules.add(wRule);
        rules.add(sRule);
        rules.add(yRule);
        return rules;
    }



}
