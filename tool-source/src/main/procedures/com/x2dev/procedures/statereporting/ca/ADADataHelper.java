/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.ca;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictCalendar;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.lang.reflect.Method;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Level;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Helper class that helps to collect all necessary data based on state report entity to run all
 * dependable reports.
 *
 * @author X2 Development Corporation
 *
 */
public class ADADataHelper extends Object {

    /**
     * The Enum ADA_ENROLLMENT_TYPE.
     */
    public enum ADA_ENROLLMENT_TYPE {
        FIRST, LAST, NONE;

        private static final String VALUE_FIRST = "First";
        private static final String VALUE_LAST = "Last";

        /**
         * Parses the.
         *
         * @param value String
         * @return ADA_ENROLLMENT_TYPE
         */
        static ADA_ENROLLMENT_TYPE parse(String value) {
            ADA_ENROLLMENT_TYPE retValue = NONE;
            if (VALUE_FIRST.endsWith(value)) {
                retValue = FIRST;
            } else if (VALUE_LAST.endsWith(value)) {
                retValue = LAST;
            }
            return retValue;
        }
    }

    /**
     * The Class ADADetailItem.
     */
    public class ADADetailItem {
        // in export format order
        private StudentAttendance m_attendance;
        private SisStudent m_student;
        private SisSchool m_school;
        private String m_fundingCategory;
        private PlainDate m_attendanceDate;
        private String m_attendancePeriod;
        private String m_gradeLevel;
        private String m_attendanceCode;
        private boolean m_apportionment;
        private boolean m_absent;
        private boolean m_excused;
        private boolean m_scheduled;
        private boolean m_suspended;
        private boolean m_truant;
        private boolean m_enrolled;
        private List<StudentEnrollmentSpan> m_stdSpans;
        private ADA_ENROLLMENT_TYPE m_enrollmentType;

        /**
         * Instantiates a new ADA detail item.
         *
         * @param student SisStudent
         * @param school SisSchool
         * @param attendance
         * @param fundingCategory String
         * @param stdSpans List<StudentEnrollmentSpan>
         * @param attendanceDate PlainDate
         * @param attendancePeriod String
         * @param gradeLevel String
         * @param attendanceCode String
         * @param apportionment boolean
         * @param absent boolean
         * @param excused boolean
         * @param scheduled boolean
         * @param suspended boolean
         * @param truant boolean
         * @param enrollmentType ADA_ENROLLMENT_TYPE
         * @param enrolled boolean
         */
        public ADADetailItem(SisStudent student, SisSchool school, StudentAttendance attendance, String fundingCategory,
                List<StudentEnrollmentSpan> stdSpans, PlainDate attendanceDate,
                String attendancePeriod, String gradeLevel, String attendanceCode, boolean apportionment,
                boolean absent, boolean excused,
                boolean scheduled, boolean suspended, boolean truant, ADA_ENROLLMENT_TYPE enrollmentType,
                boolean enrolled) {
            m_student = student;
            m_school = school;
            m_attendance = attendance;
            m_fundingCategory = fundingCategory;
            m_attendanceDate = attendanceDate;
            m_attendancePeriod = attendancePeriod;
            m_gradeLevel = gradeLevel;
            m_attendanceCode = attendanceCode;
            m_apportionment = apportionment;
            m_absent = absent;
            m_excused = excused;
            m_scheduled = scheduled;
            m_suspended = suspended;
            m_truant = truant;
            m_stdSpans = stdSpans;
            m_enrollmentType = enrollmentType;
            m_enrolled = enrolled;
        }

        /**
         * Gets the attendance.
         *
         * @return Sis school
         */
        public StudentAttendance getAttendance() {
            return m_attendance;
        }

        /**
         * Gets the school.
         *
         * @return Sis school
         */
        public SisSchool getSchool() {
            return m_school;
        }

        /**
         * Gets the attendance code.
         *
         * @return String
         */
        public String getAttendanceCode() {
            return m_attendanceCode;
        }

        /**
         * Gets the attendance date.
         *
         * @return Plain date
         */
        public PlainDate getAttendanceDate() {
            return m_attendanceDate;
        }

        /**
         * Gets the attendance period.
         *
         * @return String
         */
        public String getAttendancePeriod() {
            return m_attendancePeriod;
        }

        /**
         * Gets the enrollment type.
         *
         * @return ada enrollment type
         */
        public ADA_ENROLLMENT_TYPE getEnrollmentType() {
            return m_enrollmentType;
        }

        /**
         * Gets the funding category.
         *
         * @return String
         */
        public String getFundingCategory() {
            return m_fundingCategory;
        }

        /**
         * Gets the grade level.
         *
         * @return String
         */
        public String getGradeLevel() {
            return m_gradeLevel;
        }

        /**
         * Gets the std spans.
         *
         * @return List
         */
        public List<StudentEnrollmentSpan> getStdSpans() {
            return m_stdSpans;
        }

        /**
         * Gets the student.
         *
         * @return Sis student
         */
        public SisStudent getStudent() {
            return m_student;
        }

        /**
         * Checks if is absent.
         *
         * @return true, if is absent
         */
        public boolean isAbsent() {
            return m_absent;
        }

        /**
         * Checks if is apportionment.
         *
         * @return true, if is apportionment
         */
        public boolean isApportionment() {
            return m_apportionment;
        }

        /**
         * Checks if is enrolled.
         *
         * @return true, if is enrolled
         */
        public boolean isEnrolled() {
            return m_enrolled;
        }

        /**
         * Checks if is excused.
         *
         * @return true, if is excused
         */
        public boolean isExcused() {
            return m_excused;
        }

        /**
         * Checks if is scheduled.
         *
         * @return true, if is scheduled
         */
        public boolean isScheduled() {
            return m_scheduled;
        }

        /**
         * Checks if is suspended.
         *
         * @return true, if is suspended
         */
        public boolean isSuspended() {
            return m_suspended;
        }

        /**
         * Checks if is truant.
         *
         * @return true, if is truant
         */
        public boolean isTruant() {
            return m_truant;
        }
    }

    /**
     * Helper class that helps to populate data to iterate through.
     *
     * @author X2 Development Corporation
     *
     */
    public class ADADetailIterator {
        private ADADataHelper m_helper;
        private StateReportData m_stateReportData;
        private ADADetailItem m_nextItem = null;
        private boolean fOpen = false;

        /**
         * Instantiates a new ADA detail iterator.
         *
         * @param helper ADADataHelper
         * @param stateReportData StateReportData
         * @throws X2BaseException exception
         */
        public ADADetailIterator(ADADataHelper helper, StateReportData stateReportData) throws X2BaseException {
            m_helper = helper;
            m_stateReportData = stateReportData;
            if ((m_stateReportData != null) && m_stateReportData.open()) {
                fOpen = true;
                m_nextItem = readNext();
            }
        }

        /**
         * Closes m_stateReportData if not closed.
         */
        public void close() {
            if (fOpen) {
                m_stateReportData.close();
            }
        }

        /**
         * True if next element is not null.
         *
         * @return true, if successful
         */
        public boolean hasNext() {
            return m_nextItem == null ? false : true;
        }

        /**
         * Next.
         *
         * @return ADADetailItem
         */
        public ADADetailItem next() {
            ADADetailItem item = m_nextItem;
            m_nextItem = readNext();
            return item;
        }

        /**
         * Removes the.
         */
        public void remove() {
            throw new UnsupportedOperationException();
        }

        /**
         * Read next.
         *
         * @return ADADetailItem
         */
        private ADADetailItem readNext() {
            ADADetailItem nextItem = null;
            try {

                StateReportEntity entity = null;
                if ((entity = m_stateReportData.next()) != null) {
                    entity.preProcess();
                    nextItem = m_helper.processEntity(entity);
                    entity.postProcess();
                }
            } catch (X2BaseException e) {
                throw new NoSuchElementException(e.toString());
            }
            return nextItem;
        }
    }

    // These constants are in field order
    private static final String ENTITY_FIELD_SCHOOL = "School";
    private static final String ENTITY_FIELD_FUND = "Fund";
    private static final String ENTITY_FIELD_ATTENDANCE_DATE = "Attendance Date";
    private static final String ENTITY_FIELD_ATTENDANCE_PERIOD = "Attendance Period";
    private static final String ENTITY_FIELD_ATTENDANCE_CODE = "Attendance Code";
    private static final String ENTITY_FIELD_GRADE_LEVEL = "Grade Level";
    private static final String ENTITY_FIELD_APPORTIONMENT = "Apportionment";
    private static final String ENTITY_FIELD_ABSENT = "Absent";
    private static final String ENTITY_FIELD_EXCUSED = "Excused";
    private static final String ENTITY_FIELD_SCHEDULED = "Scheduled";
    private static final String ENTITY_FIELD_SUSPENDED = "Suspended";
    private static final String ENTITY_FIELD_TRUANT = "Truant";
    private static final String ENTITY_FIELD_ENROLLMENT_TYPE = "Enrollment Type";
    private static final String ENTITY_FIELD_ENROLLED = "Enrolled";

    public static final String INPUT_PARAM_START_CYCLE = "cycle";
    public static final String INPUT_PARAM_END_CYCLE = "endCycle";
    public static final String INPUT_PARAM_START_DATE = "startDate";

    public static final String KEY_DATE_START = "DATE_BEGIN";
    public static final String KEY_DATE_END = "DATE_END";

    private static final ThreadLocal<DateFormat> SYSTEM_STRING_DATE_FORMAT = new ThreadLocal<DateFormat>() {
        @Override
        protected DateFormat initialValue() {
            return new SimpleDateFormat("yyyy-MM-dd");
        }
    };

    // ef - stands for Entity Field
    int m_efPosSchool = -1;
    int m_efPosFund = -1;
    int m_efPosAttendanceDate = -1;
    int m_efPosAttendancePeriod = -1;
    int m_efPosAttendanceCode = -1;
    int m_efPosGradeLevel = -1;
    int m_efPosApportionment = -1;
    int m_efPosAbsent = -1;
    int m_efPosExcused = -1;
    int m_efPosScheduled = -1;
    int m_efPosSuspended = -1;
    int m_efPosTruant = -1;
    int m_efPosEnrollmentType = -1;
    int m_efEnrolled = -1;

    private X2Broker m_broker;
    private Map<String, Collection<SchoolCalendarDate>> m_csdMap = null;
    private Map<String, Map<String, PlainDate>> m_cycEdgeDatesMap = null;
    private Map<String, Map<PlainDate, DistrictCalendar>> m_cyclesDaysMap = null;
    private Collection<String> m_cycles = null;
    private PlainDate[] m_days = null;
    private Map<String, PlainDate> m_inSessionDateMap = null;
    private Organization m_organization;
    private StateReportData m_reportData;

    /**
     * Public constructor.
     *
     * @param broker X2Broker
     * @param organization Organization
     */
    public ADADataHelper(X2Broker broker, Organization organization) {
        m_broker = broker;
        m_organization = organization;
    }

    /**
     * Gets the cycles.
     *
     * @return Collection
     */
    public Collection<String> getCycles() {
        return m_cycles;
    }

    /**
     * Returns the StateReportData object used by this helper instance.
     *
     * @return StateReportData
     */
    public StateReportData getData() {
        return m_reportData;
    }

    /**
     * Return days for the running report
     *
     * @return m_days Array
     */
    public PlainDate[] getDays() {
        return m_days;
    }

    /**
     * Get the first in-session date for a calendar.
     *
     * @param schoolOid String
     * @param calendarId String
     * @return Plain date
     */
    public PlainDate getFirstInSessionDate(String schoolOid, String calendarId) {
        PlainDate value = null;
        String key = schoolOid + StringUtils.unNullify(calendarId);
        if (m_inSessionDateMap == null) {
            m_inSessionDateMap = new HashMap();
        }
        if (m_inSessionDateMap.containsKey(key)) {
            value = m_inSessionDateMap.get(key);
        } else {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                    SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_organization.getCurrentContextOid());
            criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                    SchoolCalendar.COL_SCHOOL_OID, schoolOid);
            criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                    SchoolCalendar.COL_CALENDAR_ID, calendarId);
            criteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
            QueryByCriteria query = new QueryByCriteria(SchoolCalendarDate.class, criteria);
            query.addOrderByAscending(SchoolCalendarDate.COL_DATE);

            QueryIterator days = m_broker.getIteratorByQuery(query);
            try {
                if (days.hasNext()) {
                    SchoolCalendarDate date = (SchoolCalendarDate) days.next();
                    value = date.getDate();
                }
            } finally {
                if (days != null) {
                    days.close();
                }
            }

            if (value == null) {
                // Try without calendar id
                criteria = new X2Criteria();
                criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                        SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_organization.getCurrentContextOid());
                criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                        SchoolCalendar.COL_SCHOOL_OID, schoolOid);
                criteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
                query = new QueryByCriteria(SchoolCalendarDate.class, criteria);
                query.addOrderByAscending(SchoolCalendarDate.COL_DATE);

                days = m_broker.getIteratorByQuery(query);
                try {
                    if (days.hasNext()) {
                        SchoolCalendarDate date = (SchoolCalendarDate) days.next();
                        value = date.getDate();
                    }
                } finally {
                    if (days != null) {
                        days.close();
                    }
                }
            }
            m_inSessionDateMap.put(key, value);
        }
        return value;
    }

    /**
     * Get in-session days for period.
     *
     * @param cycle String
     * @param schoolOid String
     * @return Collection
     */
    public Collection<SchoolCalendarDate> getInSessionDaysForCycle(String cycle, String schoolOid) {
        Collection<SchoolCalendarDate> dates = m_csdMap.get(schoolOid);
        Collection<SchoolCalendarDate> inSessionDays = new ArrayList<SchoolCalendarDate>();

        PlainDate startDate = m_cycEdgeDatesMap.get(cycle).get(KEY_DATE_START);
        PlainDate endDate = m_cycEdgeDatesMap.get(cycle).get(KEY_DATE_END);
        if (dates != null) {
            for (SchoolCalendarDate day : dates) {
                if (!day.getDate().before(startDate) && !day.getDate().after(endDate) && isCycleDay(day.getDate())) {
                    inSessionDays.add(day);
                }
            }
        }

        return inSessionDays;
    }

    /**
     * Initialize all necessary data .
     *
     * @param privilegeSet PrivilegeSet
     * @param isSchoolContext boolean
     * @param school School
     * @param parameters Map
     * @param user User
     * @param procedureId String
     * @throws X2BaseException exception
     */
    public void initialize(PrivilegeSet privilegeSet,
                           boolean isSchoolContext,
                           School school,
                           Map parameters,
                           User user,
                           String procedureId)
            throws X2BaseException {
        List<StateReportValidationError> initErrors = new ArrayList<StateReportValidationError>();
        m_reportData = StateReportData.getReportDataFromProcedure(procedureId, m_broker, initErrors);
        m_csdMap = getCSDMap();
        if ((m_reportData != null) && (initErrors.size() == 0)) {
            try {
                // Initialize the report data object.
                m_reportData.setBroker(m_broker);
                m_reportData.setOrganization(m_organization);
                m_reportData.setPrivilegeSet(privilegeSet);
                m_reportData.setSchoolContext(isSchoolContext);
                m_reportData.setSchool(school);
                m_reportData.setParameters(parameters);
                m_reportData.setUser(user);
                m_reportData.initializeExport();
            } catch (X2BaseException x2be) {
                String init_msg = "Failure initializing data structure !!!";
                initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));
            }

            initErrors.addAll(m_reportData.getSetupErrors());
        }
        if (initErrors.size() > 0) {
            StringBuilder str = new StringBuilder();
            for (StateReportValidationError error : initErrors) {
                str.append(error.getErrorMessage());
                str.append("\n");
            }
            throw new X2BaseException(new Exception(str.toString()));
        }
    }

    /**
     * Returns true is passed day is cycle day, otherwise false.
     *
     * @param date PlainDate
     * @return true, if is cycle day
     */
    public boolean isCycleDay(PlainDate date) {
        boolean isCycleDay = false;
        if (m_cyclesDaysMap != null) {
            for (Entry<String, Map<PlainDate, DistrictCalendar>> entry : m_cyclesDaysMap.entrySet()) {
                if (entry.getValue().keySet().contains(date)) {
                    isCycleDay = true;
                }
            }
        }
        return isCycleDay;
    }

    /**
     * Create ADADetailIterator based on report data.
     *
     * @return ADADetailIterator
     * @throws X2BaseException exception
     */
    public ADADetailIterator iterator() throws X2BaseException {
        ADADetailIterator iter = null;
        if (initializeEntityFieldPositions()) {
            iter = new ADADetailIterator(this, m_reportData);
        }
        return iter;
    }

    /**
     * Parses the string to plain date.
     *
     * @param stringDate String
     * @return PlainDate
     */
    public PlainDate parseStringToPlainDate(String stringDate) {
        PlainDate date = null;

        try {
            Date utilDate = null;
            if (stringDate != null) {
                utilDate = SYSTEM_STRING_DATE_FORMAT.get().parse(stringDate);
            }
            if (utilDate != null) {
                date = new PlainDate(utilDate);
            }
        } catch (ParseException e) {
            // Do nothing, we will return null.
        }
        return date;
    }

    /**
     * Method determine in what way to set attendance period.</br>
     * It helps to set periods for the different input parameters and report's types.
     *
     * @param inputDate can be null
     * @param cycleFrom can be null
     * @param cycleTo can be null
     * @param cycles can be null
     * @param isADASummary only if you set cycles for ADA Summary
     */
    public void setCycle(PlainDate inputDate,
                         String cycleFrom,
                         String cycleTo,
                         Collection<String> cycles,
                         boolean isADASummary) {
        if (isADASummary) {
            setCycleDaysForADASummary(cycleFrom, cycleTo, null);
        } else if (cycles != null) {
            setCycleDaysForADASummary(null, null, cycles);
        } else if (cycleFrom != null) {
            setCycleDays(cycleFrom, cycleTo, null);
        }

        else if (inputDate != null) {
            setMondays(inputDate);
        } else {
            setMondays(new PlainDate());
        }
    }

    /**
     * Initialize ADADetailItem with the data from the state report entity.
     *
     * @param entity StateReportEntity
     * @return ADADetailItem
     */
    ADADetailItem processEntity(StateReportEntity entity) {
        SisStudent student = (SisStudent) entity.getBean();
        String schoolOid = entity.getFieldValue(m_efPosSchool);

        // List<StudentEnrollmentSpan> stdSpans =
        // adaDetailEntity.getCombination().getStdEnrlSpans();
        // Must call using reflection since separate jars are used for report and data source
        List<StudentEnrollmentSpan> stdSpans = null;
        StudentAttendance attendance = null;
        try {
            Method getCombination = entity.getClass().getMethod("getCombination", (Class<?>[]) null);
            Object combination = getCombination.invoke(entity, (Object[]) null);
            Method getStdEnrlSpans = combination.getClass().getMethod("getStdEnrlSpans", (Class<?>[]) null);
            stdSpans = (List<StudentEnrollmentSpan>) getStdEnrlSpans.invoke(combination, (Object[]) null);
        } catch (Exception e) {
            AppGlobals.getLog().log(Level.SEVERE, "Schedule spans cannot be accessed: " + e.toString());
        }

        try {
            Method getStdAtt = entity.getClass().getMethod("getStdAtt", (Class<?>[]) null);
            attendance = (StudentAttendance) getStdAtt.invoke(entity, (Object[]) null);
        } catch (Exception e) {
            AppGlobals.getLog().log(Level.SEVERE, "Attendance cannot be accessed: " + e.toString());
        }

        SisSchool school = (SisSchool) m_broker.getBeanByOid(SisSchool.class, schoolOid);
        String fundingCategory = entity.getFieldValue(m_efPosFund);
        PlainDate attendanceDate = parseStringToPlainDate(entity.getFieldValue(m_efPosAttendanceDate));
        String attendancePeriod = entity.getFieldValue(m_efPosAttendancePeriod);
        String gradeLevel = entity.getFieldValue(m_efPosGradeLevel);
        String attendanceCode = entity.getFieldValue(m_efPosAttendanceCode);
        boolean apportionment = BooleanAsStringConverter.TRUE.equals(entity.getFieldValue(m_efPosApportionment));
        boolean absent = BooleanAsStringConverter.TRUE.equals(entity.getFieldValue(m_efPosAbsent));
        boolean excused = BooleanAsStringConverter.TRUE.equals(entity.getFieldValue(m_efPosExcused));
        boolean scheduled = BooleanAsStringConverter.TRUE.equals(entity.getFieldValue(m_efPosScheduled));
        boolean suspended = BooleanAsStringConverter.TRUE.equals(entity.getFieldValue(m_efPosSuspended));
        boolean truant = BooleanAsStringConverter.TRUE.equals(entity.getFieldValue(m_efPosTruant));
        ADA_ENROLLMENT_TYPE enrollmentType = ADA_ENROLLMENT_TYPE.parse(entity.getFieldValue(m_efPosEnrollmentType));
        boolean enrolled = BooleanAsStringConverter.TRUE.equals(entity.getFieldValue(m_efEnrolled));

        return new ADADetailItem(student, school, attendance, fundingCategory, stdSpans, attendanceDate,
                attendancePeriod, gradeLevel, attendanceCode, apportionment, absent, excused,
                scheduled, suspended, truant, enrollmentType, enrolled);
    }

    /**
     * Fill dates for each cycle.
     *
     * @param cycles Collection<String>
     * @param contextOid String
     * @return PlainDate[]
     */
    private PlainDate[] addDaysForCycles(Collection<String> cycles, String contextOid) {
        ArrayList<PlainDate> cyclesDays = new ArrayList<PlainDate>();

        X2Criteria criteria = new X2Criteria();

        // get all dates for current context
        criteria.addEqualTo(DistrictCalendar.COL_DISTRICT_CONTEXT_OID, contextOid);

        QueryByCriteria query = new QueryByCriteria(DistrictCalendar.class, criteria);

        query.addOrderBy(DistrictCalendar.COL_DATE, true);

        ArrayList<DistrictCalendar> calendarDays = new ArrayList(m_broker.getCollectionByQuery(query));

        for (String cycle : cycles) {
            Calendar cal = Calendar.getInstance();

            Iterator<DistrictCalendar> iterator = calendarDays.iterator();

            ArrayList<PlainDate> cycleDays = new ArrayList<PlainDate>();

            // should be 4 Mondays for each cycle for correct work of reports
            ArrayList<PlainDate> mondays = new ArrayList<PlainDate>();


            while (iterator.hasNext()) {
                DistrictCalendar calendar = iterator.next();

                // if current cycle matched with DistrictCalendar date, get index of first Monday of
                // cycle
                if (cycle.equals(calendar.getCycle())) {
                    cal.setTime(calendar.getDate());

                    // rewind Calendar to Monday
                    while (cal.get(Calendar.DAY_OF_WEEK) != Calendar.MONDAY) {
                        cal.add(Calendar.DAY_OF_WEEK, -1);
                    }

                    // and add each Monday in collection of Mondays of current cycle
                    mondays.add(new PlainDate(cal.getTime()));

                    // rewind Calendar to Friday
                    for (int i = 0; i < 5; i++) {
                        cal.add(Calendar.DAY_OF_WEEK, 1);
                    }

                    // and rewind iterator to Friday
                    while (iterator.hasNext() && calendar.getDate().before(cal.getTime())) {
                        calendar = iterator.next();
                    }
                }
            }
            // If number of weeks of cycle = 3, then find and insert lost week for correct report
            // work.
            // Free weeks can are before, after or within Mondays.
            if (mondays.size() == 3) {
                // if length of days between Mondays of cycle greater than 1 week, then insert week
                // between them
                Calendar cal1 = (Calendar.getInstance());
                cal1.setTime(mondays.get(0));
                Calendar cal2 = (Calendar.getInstance());
                cal2.setTime(mondays.get(1));
                Calendar cal3 = (Calendar.getInstance());
                cal3.setTime(mondays.get(2));

                cal1.add(Calendar.WEEK_OF_YEAR, 1);

                cal3.add(Calendar.WEEK_OF_YEAR, -1);

                if (!cal2.getTime().equals(cal1.getTime())) {
                    mondays.add(1, new PlainDate(cal1.getTime()));
                } else if (!cal2.getTime().equals(cal3.getTime())) {
                    mondays.add(2, new PlainDate(cal3.getTime()));
                }
            }
            // add dates of work week for this cycle
            for (PlainDate monday : mondays) {
                cal.setTime(monday);
                for (int i = 0; i < 5; i++) {
                    cycleDays.add(new PlainDate(cal.getTime()));
                    cal.add(Calendar.DAY_OF_WEEK, 1);
                }
            }
            cyclesDays.addAll(cycleDays);
        }

        return Arrays.copyOf(cyclesDays.toArray(), cyclesDays.toArray().length, PlainDate[].class);
    }

    /**
     * The method which build map of all school calendar dates keyed on school oid.
     *
     * @return Map<String, Collection<SchoolCalendarDate>>
     */
    private Map<String, Collection<SchoolCalendarDate>> getCSDMap() {
        X2Criteria csdCriteria = new X2Criteria();
        csdCriteria.addIn(SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID, getMostCommonCalendars());
        csdCriteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, BooleanAsStringConverter.TRUE);

        QueryByCriteria csdQuery = new QueryByCriteria(SchoolCalendarDate.class, csdCriteria);
        csdQuery.addOrderBy(SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID, true);
        csdQuery.addOrderBy(SchoolCalendarDate.COL_DATE, true);

        return m_broker.getGroupedCollectionByQuery(csdQuery,
                SchoolCalendarDate.REL_SCHOOL_CALENDAR
                        + ModelProperty.PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                2056);
    }

    /**
     * Build the list of most Common Calendars for schools.
     *
     * @return Collection of SchoolCalendars oids
     */
    private Collection getMostCommonCalendars() {
        Map<String, Map<String, Collection<SchoolCalendar>>> mapSchoolCalendars = getSchoolCalendars();

        HashMap<String, String> schoolCalendars = new HashMap();

        String activeStatus = PreferenceManager.getPreferenceValue(m_organization,
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, activeStatus);

        String[] columns = new String[] {SisStudent.COL_SCHOOL_OID, SisStudent.COL_CALENDAR_CODE, "count(*)"};

        ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class, columns, criteria);
        query.addGroupBy(SisStudent.COL_SCHOOL_OID);
        query.addGroupBy(SisStudent.COL_CALENDAR_CODE);
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
     * Build map of maps of SchoolCalendars keyed on school oid and school calendar id.
     *
     * @return Map<String, Map<String, Collection<SchoolCalendar>>>
     */
    private Map<String, Map<String, Collection<SchoolCalendar>>> getSchoolCalendars() {
        X2Criteria casCriteria = new X2Criteria();

        casCriteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_organization.getCurrentContextOid());
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
     * Initialize fields' positions of the entity.
     *
     * @return true, if successful
     */
    private boolean initializeEntityFieldPositions() {
        for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
            FieldDefinition field = m_reportData.getFieldDefinition(pos);
            String fieldName = field.getFieldId();

            if (ENTITY_FIELD_SCHOOL.equals(fieldName)) {
                m_efPosSchool = pos;
            } else if (ENTITY_FIELD_FUND.equals(fieldName)) {
                m_efPosFund = pos;
            } else if (ENTITY_FIELD_ATTENDANCE_DATE.equals(fieldName)) {
                m_efPosAttendanceDate = pos;
            } else if (ENTITY_FIELD_ATTENDANCE_PERIOD.equals(fieldName)) {
                m_efPosAttendancePeriod = pos;
            } else if (ENTITY_FIELD_ATTENDANCE_CODE.equals(fieldName)) {
                m_efPosAttendanceCode = pos;
            } else if (ENTITY_FIELD_GRADE_LEVEL.equals(fieldName)) {
                m_efPosGradeLevel = pos;
            } else if (ENTITY_FIELD_APPORTIONMENT.equals(fieldName)) {
                m_efPosApportionment = pos;
            } else if (ENTITY_FIELD_ABSENT.equals(fieldName)) {
                m_efPosAbsent = pos;
            } else if (ENTITY_FIELD_EXCUSED.equals(fieldName)) {
                m_efPosExcused = pos;
            } else if (ENTITY_FIELD_SCHEDULED.equals(fieldName)) {
                m_efPosScheduled = pos;
            } else if (ENTITY_FIELD_SUSPENDED.equals(fieldName)) {
                m_efPosSuspended = pos;
            } else if (ENTITY_FIELD_TRUANT.equals(fieldName)) {
                m_efPosTruant = pos;
            } else if (ENTITY_FIELD_ENROLLMENT_TYPE.equals(fieldName)) {
                m_efPosEnrollmentType = pos;
            } else if (ENTITY_FIELD_ENROLLED.equals(fieldName)) {
                m_efEnrolled = pos;
            }
        }

        if ((m_efPosSchool < 0) ||
                (m_efPosFund < 0) ||
                (m_efPosAttendanceDate < 0) ||
                (m_efPosAttendancePeriod < 0) ||
                (m_efPosAttendanceCode < 0) ||
                (m_efPosGradeLevel < 0) ||
                (m_efPosApportionment < 0) ||
                (m_efPosAbsent < 0) ||
                (m_efPosExcused < 0) ||
                (m_efPosScheduled < 0) ||
                (m_efPosSuspended < 0) ||
                (m_efPosTruant < 0) ||
                (m_efPosEnrollmentType < 0)) {
            return false;
        }
        return true;
    }

    /**
     * Calculate days of the given attendance period.
     *
     * @param startCycle String
     * @param endCycle String
     * @param inputCycles Collection<String>
     */
    private void setCycleDays(String startCycle, String endCycle, Collection<String> inputCycles) {
        m_cycles = new TreeSet<String>();

        if (startCycle != null && endCycle != null) {
            int start = Integer.parseInt(startCycle);
            int end = Integer.parseInt(endCycle);
            for (int i = start; i <= end; i++) {
                String cycleToAdd = String.valueOf(i);
                if (cycleToAdd.length() < 2) {
                    cycleToAdd = "0".concat(cycleToAdd);
                }
                m_cycles.add(cycleToAdd);
            }
        }

        if (inputCycles != null && !inputCycles.isEmpty()) {
            m_cycles.addAll(inputCycles);
        }

        m_days = new PlainDate[20 * m_cycles.size()];

        // add days for each cycle
        m_days = addDaysForCycles(m_cycles, m_organization.getCurrentContextOid());
    }

    /**
     * Calculate days of the given attendance period.
     *
     * @param startCycle String
     * @param endCycle String
     * @param inputCycles Collection<String>
     */
    private void setCycleDaysForADASummary(String startCycle, String endCycle, Collection<String> inputCycles) {
        m_cycles = new ArrayList<String>();
        if (startCycle != null && endCycle != null) {
            m_cycles = new ArrayList<String>();
            int start = Integer.parseInt(startCycle);
            int end = Integer.parseInt(endCycle);
            for (int i = start; i <= end; i++) {
                String cycleToAdd = String.valueOf(i);
                if (cycleToAdd.length() < 2) {
                    cycleToAdd = "0".concat(cycleToAdd);
                }
                m_cycles.add(cycleToAdd);
            }
        } else if (inputCycles != null && !inputCycles.isEmpty()) {
            m_cycles.addAll(inputCycles);
        }

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(DistrictCalendar.COL_DISTRICT_CONTEXT_OID, m_organization.getCurrentContextOid());

        criteria.addIn(DistrictCalendar.COL_CYCLE, m_cycles);

        QueryByCriteria query = new QueryByCriteria(DistrictCalendar.class, criteria);

        query.addOrderByAscending(DistrictCalendar.COL_DATE);

        String[] columns = {DistrictCalendar.COL_CYCLE, DistrictCalendar.COL_DATE};
        int[] sizes = {1024, 1024};
        m_cyclesDaysMap = m_broker.getGroupedCollectionByQuery(query, columns, sizes);

        Set<PlainDate> daysTemp = new TreeSet<PlainDate>();

        m_cycEdgeDatesMap = new HashMap<String, Map<String, PlainDate>>();

        for (Entry<String, Map<PlainDate, DistrictCalendar>> entry : m_cyclesDaysMap.entrySet()) {
            ArrayList<PlainDate> cycleDates = new ArrayList<PlainDate>(entry.getValue().keySet());

            Collections.sort(cycleDates);

            Set<PlainDate> mondays = new HashSet();
            for (int i = 0; i < cycleDates.size(); i++) {
                Calendar calendar = Calendar.getInstance();
                calendar.setTime(cycleDates.get(i));
                calendar.set(Calendar.DAY_OF_WEEK, Calendar.MONDAY);
                mondays.add(new PlainDate(calendar.getTime()));
            }

            // Must be 20 days, adjust if needed.
            ArrayList<PlainDate> adjustedCycleDates = new ArrayList<PlainDate>(20);
            for (int i = 0; i < cycleDates.size(); i++) {
                Calendar calendar = Calendar.getInstance();
                calendar.setTime(cycleDates.get(i));

                // Skip Saturdays and Sundays
                if (calendar.get(Calendar.DAY_OF_WEEK) > Calendar.FRIDAY) {
                    continue;
                }
                // If current cycle date is Mondays, try to fill week using cycle dates
                if (calendar.get(Calendar.DAY_OF_WEEK) == Calendar.MONDAY) {
                    ArrayList<PlainDate> weekDates = new ArrayList<PlainDate>(5);

                    weekDates.add(cycleDates.get(i));

                    Calendar nextDateCal = Calendar.getInstance();
                    nextDateCal.setTime(cycleDates.get(i));
                    nextDateCal.add(Calendar.DAY_OF_WEEK, 1);

                    int k = 0;
                    for (k = i + 1; k < cycleDates.size() && weekDates.size() < 5; k++) {
                        PlainDate curDate = cycleDates.get(k);

                        if (curDate.equals(nextDateCal.getTime())
                                || (mondays.size() > 4 && splitWeekOk(curDate, nextDateCal.getTime()))) {
                            weekDates.add(curDate);
                            nextDateCal.setTime(curDate);
                            nextDateCal.add(Calendar.DAY_OF_WEEK, 1);
                        }
                        // If there are no next date in cycle dates, just add next date instead
                        else {
                            weekDates.add(new PlainDate(nextDateCal.getTime()));
                            nextDateCal.add(Calendar.DAY_OF_WEEK, 1);

                            // Try again for current cycle date
                            --k;
                        }
                    }

                    // Rewind to the last added cycle date
                    i = k - 1; // i will be incremented to k at loop end
                    adjustedCycleDates.addAll(weekDates);
                }
                // If current cycle date is not Monday, fill previous dates of this week with
                // previous dates and than add this date. If previous dates are already filled, just
                // add this date.
                else if (calendar.get(Calendar.DAY_OF_WEEK) <= Calendar.FRIDAY) {
                    calendar.set(Calendar.DAY_OF_WEEK, Calendar.MONDAY);
                    // Adjust dates if it is still not adjusted
                    if (!adjustedCycleDates.contains(new PlainDate(calendar.getTime()))) {
                        while (!calendar.getTime().equals(cycleDates.get(i))) {
                            adjustedCycleDates.add(new PlainDate(calendar.getTime()));
                            calendar.add(Calendar.DAY_OF_WEEK, 1);
                        }
                    }
                    adjustedCycleDates.add(cycleDates.get(i));
                }
            }

            // If still less than 20 days add missing
            if (adjustedCycleDates.size() < 20) {
                while (adjustedCycleDates.size() != 20) {
                    PlainDate lastDate = adjustedCycleDates.get(adjustedCycleDates.size() - 1);
                    Calendar nextDate = Calendar.getInstance();
                    nextDate.setTime(lastDate);
                    nextDate.add(Calendar.DAY_OF_WEEK, 1);

                    if (nextDate.get(Calendar.DAY_OF_WEEK) > Calendar.FRIDAY) {
                        nextDate.add(Calendar.WEEK_OF_YEAR, 1);
                        nextDate.set(Calendar.DAY_OF_WEEK, Calendar.MONDAY);
                    }
                    adjustedCycleDates.add(new PlainDate(nextDate.getTime()));
                }
            }
            daysTemp.addAll(adjustedCycleDates);

            Map<String, PlainDate> cycleDatesMap = new HashMap<String, PlainDate>();
            int FIRST_DATE_OF_CYCLE = 0;
            cycleDatesMap.put(KEY_DATE_START, adjustedCycleDates.get(FIRST_DATE_OF_CYCLE));
            int LAST_DATE_OF_CYCLE = 19;
            cycleDatesMap.put(KEY_DATE_END, adjustedCycleDates.get(LAST_DATE_OF_CYCLE));

            m_cycEdgeDatesMap.put(entry.getKey(), cycleDatesMap);
        }

        m_days = daysTemp.toArray(new PlainDate[daysTemp.size()]);

    }

    /**
     * Calculate only Mondays.
     *
     * @param inputDate void
     */
    private void setMondays(PlainDate inputDate) {
        m_days = new PlainDate[20];
        Calendar cal = Calendar.getInstance();
        cal.setTime(inputDate);
        for (int i = 0; i < m_days.length / 5; ++i) {
            while (cal.get(Calendar.DAY_OF_WEEK) != Calendar.MONDAY) {
                cal.add(Calendar.DAY_OF_WEEK, 1);
            }
            m_days[i * 5] = new PlainDate(cal.getTime());
            cal.add(Calendar.DAY_OF_WEEK, 1);
            m_days[i * 5 + 1] = new PlainDate(cal.getTime());
            cal.add(Calendar.DAY_OF_WEEK, 1);
            m_days[i * 5 + 2] = new PlainDate(cal.getTime());
            cal.add(Calendar.DAY_OF_WEEK, 1);
            m_days[i * 5 + 3] = new PlainDate(cal.getTime());
            cal.add(Calendar.DAY_OF_WEEK, 1);
            m_days[i * 5 + 4] = new PlainDate(cal.getTime());
            cal.add(Calendar.DAY_OF_WEEK, 1);
        }
    }

    /**
     * @param curDate
     * @param time
     * @param object
     * @param plainDate
     * @return
     */
    private boolean splitWeekOk(PlainDate cycleDate, Date nextDate) {
        Calendar cycleDateCal = Calendar.getInstance();
        cycleDateCal.setTime(cycleDate);

        Calendar nextDateCal = Calendar.getInstance();
        nextDateCal.setTime(nextDate);

        return nextDateCal.get(Calendar.DAY_OF_WEEK) <= cycleDateCal.get(Calendar.DAY_OF_WEEK);
    }

}
