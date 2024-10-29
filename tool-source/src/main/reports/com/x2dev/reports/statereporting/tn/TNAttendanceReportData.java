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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SecondaryStudentDataSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.procedures.statereporting.common.CriteriaHelper;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolEnrollment;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchool;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchoolCalendar;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchoolCalendarDate;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentAttendance;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.tn.TnToolBean;
import com.x2dev.procedures.statereporting.tn.TnToolBean.TnEnrollment;
import com.x2dev.procedures.statereporting.tn.TnToolBean.TnSchool;
import com.x2dev.procedures.statereporting.tn.TnToolBean.TnStudent;
import com.x2dev.procedures.statereporting.tn.TnToolBean.TnStudentAttendance;
import com.x2dev.procedures.statereporting.tn.TnToolBean.TnStudentEnrollmentSpan;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.stream.Collectors;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Data source for the TN Student Detail report.
 *
 * @author X2 Development Corporation
 */
public class TNAttendanceReportData extends SecondaryStudentDataSource {

    public static final String PREFERENCE_CURRENT_CONTEXT = "currentContext";

    private static final String COLUMN_ERROR_MESSAGE = "error";
    private static final String COLUMN_SCHOOL_OID = "schoolOid";
    private static final String COLUMN_SCHOOL_NAME = "schoolName";
    private static final String COLUMN_STUDENT_EIS = "studentEISStateId";
    private static final String COLUMN_STUDENT_GRADE = "studentGrade";
    private static final String COLUMN_STUDENT_LOCAL_ID = "studentLocalId";
    private static final String COLUMN_STUDENT_NAME = "studentName";
    private static final String COLUMN_STUDENT_OID = "studentOid";
    private static final String COLUMN_PERIOD_ABSENT = "A";
    private static final String COLUMN_PERIOD_ADA = "periodAda";
    private static final String COLUMN_PERIOD_DAYS_IN_SESSION = "daysInSession";
    private static final String COLUMN_PERIOD_DISMISSED = "D";
    private static final String COLUMN_PERIOD_ENROLLED = "periodEnrolled";
    private static final String COLUMN_PERIOD_EXCUSED = "E";
    private static final String COLUMN_PERIOD_EXCUSED_NO = "UE";
    private static final String COLUMN_PERIOD_OSS = "OSS";
    private static final String COLUMN_PERIOD_PRESENT = "periodPresent";
    private static final String COLUMN_PERIOD_TARDY = "T";
    private static final int GRID_INITIAL_COLUMN_SIZE = 35;
    private static final int GRID_INITIAL_ROW_SIZE = 1500;
    private static final String INPUT_ABS_THRESHOLD = "absenceThreshold";
    private static final String INPUT_ABS_THRESHOLD_OPTION = "thresholdOption";
    private static final String INPUT_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_DATE_END = "dateRangeEnd";
    private static final String INPUT_DATE_START = "dateRangeStart";
    private static final String INPUT_PARAM_THRU_CURRENT_DATE = "thruCurrentDate";
    private static final String INPUT_REPORT_ID_CSV = "subreportIdCSVVersion";
    private static final String INPUT_REPORT_ID_PDF = "subreportIdPDFVersion";
    private static final String INPUT_REPORT_MODE = "reportMode";
    private static final String INPUT_SCHOOLS = "schoolOids";
    private static final String INPUT_SORT_BY = "sortReport";
    private static final String PARAM_CURRENT_CONTEXT = "currentContext";
    private static final String PARAM_END_DATE = "endDate";
    private static final String PARAM_REPORT_ABS_THRESHOLD = "absenceThreshold";
    private static final String PARAM_REPORT_ABS_THRESHOLD_OPTION = "absenceThresholdOption";
    private static final String PARAM_REPORT_PERIOD = "reportPeriod";
    private static final String PARAM_START_DATE = "startDate";
    private static final String SORT_GRADE = "grade";
    private static final String SORT_NAME = "nameView";
    private static final String STAFF_SCHOOL_CODE = "9999";

    private static final long serialVersionUID = 1L;

    private Integer m_absenceThreshold;
    private int m_absenceThresholdOption;
    private DistrictSchoolYearContext m_context = null;
    private Map<String, LinkedList<SchoolCalendarDate>> m_csdMapBySkl;
    private PlainDate m_dateEnd;
    private PlainDate m_dateStart;
    private DictionaryExtractor m_dictExtractor;
    private ReportDataGrid m_grid;
    private Map<String, KeyValuePair<PlainDate, PlainDate>> m_periodDatesMap = new HashMap<>();
    private TNReportingPeriodHelper m_periodHelper = null;
    private Integer m_reportMode;
    private Collection<TnSchool> m_schools;

    /**
     * Constructs a ReportDataGrid containing reporting period register data.
     *
     * @return JRDataSource
     * @throws ParseException exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws ParseException {
        m_grid = new ReportDataGrid();
        X2Broker broker = getBroker();
        broker.beginSession();
        try {
            ToolBean.registerClass(TnEnrollment.class);
            ToolBean.registerClass(TnSchool.class);
            ToolBean.registerClass(TnStudent.class);
            ToolBean.registerClass(TnStudentAttendance.class);
            ToolBean.setPreference(TnToolBean.PREFERENCE_ACTIVE_STUDENT_CODES,
                    StudentManager.getActiveStudentCodeList(getOrganization()));
            ToolBean.setPreference(TnToolBean.PREFERENCE_CURRENT_CONTEXT, getCurrentContext());
            ToolBean.setPreference(TnToolBean.PREFERENCE_SPAN_BREAK_ON_STATUS, Boolean.TRUE);
            ToolBean.setPreference(TnToolBean.PREFERENCE_SPAN_BREAK_ON_YOG, Boolean.TRUE);
            ToolBean.setPreference(ToolBean.PREFERENCE_HISTORICAL_CUTOFF_DATE, getCurrentContext().getStartDate());
            /*
             * TODO: Normally we will create a dictionary extractor and associate it with the tool
             * beans. It can then be used for the dictionary extractor wherever needed and will be
             * used automatically when needed in the tool beans
             */
            ToolBean.setDictionaryExtractor(new DictionaryExtractor(getBroker()));

            Filterable<TnSchool> schools =
                    FilterableFactory.create(broker, getDictionaryExtractor(), TnSchool.class, getSchoolCriteria(),
                            Arrays.asList(ToolSchool.FIELD_NAME, ToolBean.FIELD_OID));

            X2Criteria casCriteria = new X2Criteria();
            casCriteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
            ToolBean.addAndCriteria(broker, ToolSchoolCalendar.class, casCriteria);
            ToolBean.preload(getBroker(), null,
                    Arrays.asList(ToolSchoolCalendar.FIELD_CALENDAR_ID),
                    ToolSchool.CHILD_CALENDARS);

            Filterable<ToolSchoolCalendar> schoolCommonCalendars = getMostCommonCalendars();

            m_schools = schools.extract();

            initHelpers();

            if (m_schools == null || m_schools.isEmpty()) {
                m_grid.append();
                m_grid.set(COLUMN_ERROR_MESSAGE, "No schools are selected");
                m_grid.beforeTop();
                return m_grid;
            }
            if (m_dateStart != null && m_dateEnd != null) {
                StringBuilder error = new StringBuilder();
                boolean errorReport = false;
                if (m_dateEnd.before(m_dateStart)) {
                    errorReport = true;
                    error.append(
                            "Report can not be generated. Selected Start Date: " + m_dateStart.toString() +
                            " is after: " + m_dateEnd.toString());
                }
                if (m_dateStart.before(getCurrentContext().getStartDate())
                        || m_dateStart.after(getCurrentContext().getEndDate())
                        || m_dateEnd.before(getCurrentContext().getStartDate())
                        || m_dateEnd.after(getCurrentContext().getEndDate())) {
                    errorReport = true;
                    error.append(
                            "Incorrect dates for report mode \"Date Range\". Selected Start Date: "
                                    + m_dateStart.toString()
                                    + " End Date: " + m_dateEnd.toString() + " should be from the current school year");
                }
                if (errorReport) {
                    m_grid.append();
                    m_grid.set(COLUMN_ERROR_MESSAGE,
                            error.toString());
                    m_grid.beforeTop();
                    return m_grid;
                }
            }


            for (TnSchool school : m_schools) {
                KeyValuePair<PlainDate, PlainDate> periodDates = m_periodDatesMap.get(school.getOid());
                if (periodDates == null || periodDates.getKey() == null || periodDates.getValue() == null) {
                    String message = "Report for school " + school.getName() +
                            " cannot be generated. Please define dates for the selected period "
                            + "and rerun the report.";
                    m_grid.append();
                    m_grid.set(COLUMN_ERROR_MESSAGE, message);
                    m_grid.beforeTop();
                    return m_grid;
                }
                populateGrid(school, schoolCommonCalendars);
                addParameter(PARAM_START_DATE, periodDates.getKey());
                addParameter(PARAM_END_DATE, periodDates.getValue());
                addParameter(PARAM_CURRENT_CONTEXT, m_context);
            }
        } finally {
            broker.endSession();
        }
        if (getParameter(INPUT_SORT_BY) != null) {
            switch ((String) getParameter(INPUT_SORT_BY)) {
                case SORT_NAME:
                    m_grid.sort(COLUMN_STUDENT_NAME, true);
                    break;
                case SORT_GRADE:
                    m_grid.sort(COLUMN_STUDENT_GRADE, true);
                    break;
                default:
                    break;
            }
        }
        m_grid.sort(COLUMN_SCHOOL_NAME, true);
        m_grid.beforeTop();
        return m_grid;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        initReportsFormat();
        m_context = getCurrentContext();
        m_grid = new ReportDataGrid(GRID_INITIAL_ROW_SIZE, GRID_INITIAL_COLUMN_SIZE);
        m_reportMode = getParameter(INPUT_REPORT_MODE) != null ? (Integer) getParameter(INPUT_REPORT_MODE)
                : Integer.valueOf(0);
        if (((Boolean) getParameter(INPUT_PARAM_THRU_CURRENT_DATE)).booleanValue()) {
            m_dateStart = getCurrentContext().getStartDate();
            m_dateEnd = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
            addParameter(PARAM_REPORT_PERIOD, "Thru Current Date");
        } else {
            switch (m_reportMode.intValue()) {
                // Report Date Range
                case 2:
                    m_dateStart = (PlainDate) getParameter(INPUT_DATE_START);
                    m_dateEnd = (PlainDate) getParameter(INPUT_DATE_END);
                    addParameter(PARAM_REPORT_PERIOD, "Date Range");
                    break;
                    // Report Period Mode
                case 1:
                    ReferenceCode reportPeriod = null;
                    String reportPeriodOid = (String) getParameter(PARAM_REPORT_PERIOD);
                    if (!StringUtils.isEmpty(reportPeriodOid)) {
                        reportPeriod = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, reportPeriodOid);
                    } else {
                        String errorMessage = "Report period must be specified";
                        AppGlobals.getLog().log(Level.SEVERE, errorMessage);
                        throw new IllegalArgumentException(errorMessage);
                    }
                    m_periodHelper =
                            new TNReportingPeriodHelper(getOrganization(), m_context, reportPeriod, getBroker());
                    addParameter(PARAM_REPORT_PERIOD, reportPeriod.getCode());
                    break;
                case 0:
                    m_dateStart = getCurrentContext().getStartDate();
                    m_dateEnd = getCurrentContext().getEndDate();
                    addParameter(PARAM_REPORT_PERIOD, "Annual");
                    break;
            }
        }
        if (getParameter(INPUT_ABS_THRESHOLD) != null) {
            m_absenceThreshold = ((Integer) getParameter(INPUT_ABS_THRESHOLD));
        }
        m_absenceThresholdOption = ((Integer) getParameter(INPUT_ABS_THRESHOLD_OPTION)).intValue();
        addParameter(PARAM_REPORT_ABS_THRESHOLD_OPTION, Integer.valueOf(m_absenceThresholdOption));
        addParameter(PARAM_REPORT_ABS_THRESHOLD, m_absenceThreshold);
    }

    /**
     * Returns student absence map.
     *
     * @param school
     *
     * @param student TnStudent
     * @param studentMembership Collection<PlainDate>
     * @return Map
     */
    private Map<String, Integer> getAbsenceMap(TnSchool school,
                                               TnStudent student,
                                               Collection<PlainDate> studentMembership) {
        Map<String, Integer> attendanceMap = new HashMap<>();
        List<TnStudentAttendance> studentAttendances = student.getAttendance(getBroker())
                .stream().map(att -> (TnStudentAttendance)att).collect(Collectors.toList());
        Collection<TnStudentEnrollmentSpan> spans = student.getTnStudentEnrollmentSpans(getBroker());
        int absCount = 0;
        int tardyCount = 0;
        int dismCount = 0;
        int excusedCount = 0;
        int unexcusedCount = 0;
        int ossCount = 0;
        if (studentAttendances != null) {
            for (TnStudentAttendance attendance : studentAttendances) {
                if (school.getOid().equals(getSchoolForAttendance(attendance, spans).getOid())) {
                    PlainDate attendanceDate = attendance.getDate();
                    boolean isMember = studentMembership.contains(attendanceDate);
                    if (attendanceDate != null && isMember) {
                        if (attendance.getAbsentIndicator()) {
                            absCount += 1;
                            if (attendance.getExcusedIndicator()) {
                                excusedCount += 1;
                            } else {
                                unexcusedCount += 1;
                            }
                            if ("OSS".equals(attendance.getOtherCode())
                                    || "OSS".equals(attendance.getOtherCode02())) {
                                ossCount += 1;
                            }
                        }
                        tardyCount += attendance.getTardyIndicator() ? 1 : 0;
                        dismCount += attendance.getDismissedIndicator() ? 1 : 0;
                    }
                }
            }
            if (0 == m_absenceThresholdOption || m_absenceThreshold == null
                    || absCount > m_absenceThreshold.intValue()) {
                attendanceMap.put(COLUMN_PERIOD_ABSENT, absCount > 0 ? Integer.valueOf(absCount) : null);
                attendanceMap.put(COLUMN_PERIOD_TARDY, tardyCount > 0 ? Integer.valueOf(tardyCount) : null);
                attendanceMap.put(COLUMN_PERIOD_DISMISSED, dismCount > 0 ? Integer.valueOf(dismCount) : null);
                attendanceMap.put(COLUMN_PERIOD_EXCUSED, excusedCount > 0 ? Integer.valueOf(excusedCount) : null);
                attendanceMap.put(COLUMN_PERIOD_EXCUSED_NO,
                        unexcusedCount > 0 ? Integer.valueOf(unexcusedCount) : null);
                attendanceMap.put(COLUMN_PERIOD_OSS, ossCount > 0 ? Integer.valueOf(ossCount) : null);
            }
        }
        return attendanceMap;

    }

    private DictionaryExtractor getDictionaryExtractor() {
        if (m_dictExtractor == null) {
            m_dictExtractor = new DictionaryExtractor(getBroker());
        }
        return m_dictExtractor;
    }

    /**
     * Build the list of most Common Calendars for schools.
     *
     * @param sklOids Collection
     * @return Collection of SchoolCalendars oids
     */
    private Filterable<ToolSchoolCalendar> getMostCommonCalendars() {
        HashMap<String, ToolSchoolCalendar> schoolCalendars = new HashMap();

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(SisStudent.COL_ENROLLMENT_STATUS, StudentManager.getActiveStudentCodeList(getOrganization()));
        criteria.addIn(SisStudent.COL_SCHOOL_OID, ToolBean.getCachedToolBeanOids(TnSchool.class));

        String[] columns = new String[] {SisStudent.COL_SCHOOL_OID, SisStudent.COL_CALENDAR_CODE, "count(*)"};

        ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class, columns, criteria);
        query.addGroupBy(SisStudent.COL_SCHOOL_OID);
        query.addGroupBy(SisStudent.COL_CALENDAR_CODE);
        query.addOrderByDescending("count(*)");

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String schoolOid = (String) row[0];
                String calendarCode = (String) row[1];

                if (!schoolCalendars.containsKey(schoolOid)) {
                    TnSchool school = ToolBean.getBeanByOid(TnSchool.class, schoolOid, false);
                    ToolSchoolCalendar calendar =
                            school.getCalendarByCode(getBroker(), getCurrentContext().getOid(), calendarCode);
                    if (calendar != null) {
                        schoolCalendars.put(schoolOid, calendar);
                    }
                }
            }
        } finally {
            iterator.close();
        }

        // Add schools without students - any calendar will do
        for (TnSchool school : ToolBean.getCachedToolBeans(TnSchool.class)) {
            if (!schoolCalendars.containsKey(school.getOid())) {
                Collection<ToolSchoolCalendar> calendars = school.getCalendars(getBroker()).extract();
                if (!calendars.isEmpty()) {
                    schoolCalendars.put(school.getOid(), calendars.iterator().next());
                }
            }
        }

        return FilterableFactory.createFilterableToolBeans(schoolCalendars.values());
    }

    private X2Criteria getSchoolCriteria() {
        Object objIsAllSchools = getParameter(INPUT_ALL_SCHOOLS);
        boolean isAllSchools =
                objIsAllSchools == null ? false : ((Boolean) objIsAllSchools).booleanValue() && !isSchoolContext();
        X2Criteria schoolCriteria = new X2Criteria();
        if (isAllSchools) {
            schoolCriteria.addNotEqualTo(ToolSchool.FIELD_INACTIVE_INDICATOR.resolve(null), Boolean.TRUE);
            schoolCriteria.addNotEqualTo(ToolSchool.FIELD_ARCHIVE_INDICATOR.resolve(null), Boolean.TRUE);
            String fieldSklStateIdPath = TnSchool.FIELD_SKL_STATE_ID.resolve(getDictionaryExtractor());
            schoolCriteria.addNotEmpty(fieldSklStateIdPath, getBroker().getPersistenceKey());
            schoolCriteria.addNotEqualTo(fieldSklStateIdPath, STAFF_SCHOOL_CODE);
        } else if (!isSchoolContext()) {
            Object objSchools = getParameter(INPUT_SCHOOLS);
            String schoolOids = objSchools == null ? "" : (String) objSchools;
            if (!StringUtils.isEmpty(schoolOids)) {
                List<String> oids = Arrays.asList(schoolOids.split(","));
                schoolCriteria.addIn(ToolBean.FIELD_OID.resolve(null), oids);
            }
        } else {
            schoolCriteria.addEqualTo(ToolBean.FIELD_OID.resolve(null), getSchool().getOid());
        }
        return schoolCriteria;
    }

    /**
     * Look at enrollment spans to capture the primary school and always report that school as
     * Attendance School for the given attendance.
     *
     * @param att
     * @return
     */
    private TnSchool getSchoolForAttendance(TnStudentAttendance att, Collection<TnStudentEnrollmentSpan> spans) {
        TnSchool skl = null;
        if (spans != null) {
            for (TnStudentEnrollmentSpan span : spans) {
                if (isBetween(span.getFirstActiveDate(), span.getLastActiveDate(), att.getDate())) {
                    // Make sure we return the last matching span so that unclosed previous spans
                    // are ignored
                    skl = span.getSchool();
                }
            }
        }
        if (skl == null) {
            skl = (TnSchool) att.getSchool(getBroker());
        }
        return skl;
    }

    /**
     * Return student attendance criteria.
     *
     * @param studentOids Collection<String>
     * @return X2Criteria
     */
    private X2Criteria getStudentAttendanceCriteria(TnSchool school) {
        X2Criteria studentAttendanceCriteria = new X2Criteria();
        studentAttendanceCriteria.addGreaterOrEqualThan(ToolStudentAttendance.FIELD_DATE.resolve(null),
                m_dateStart);
        studentAttendanceCriteria.addLessOrEqualThan(ToolStudentAttendance.FIELD_DATE.resolve(null), m_dateEnd);
        studentAttendanceCriteria.addEqualTo(ToolStudentAttendance.FIELD_SCHOOL_OID.resolve(null), school.getOid());
        return studentAttendanceCriteria;
    }

    /**
     * TODO: This is the portion of the code where we find the students on the report
     *
     * @param school
     * @return List<TnStudent>
     */
    private List<TnStudent> getStudents(TnSchool school, List<ToolSchoolCalendarDate> csdList) {
        /*
         * TODO: Clear any cached students, enrollments, attendance before loading this school
         */

        ToolBean.clearAllCachedToolBeans(TnEnrollment.class);
        ToolBean.clearAllCachedToolBeans(TnStudentAttendance.class);
        ToolBean.clearAllCachedToolBeans(TnStudent.class);

        ToolBean.resetCriteria(getBroker(), TnEnrollment.class);
        ToolBean.resetCriteria(getBroker(), TnStudentAttendance.class);
        ToolBean.resetCriteria(getBroker(), TnStudent.class);

        EnrollmentSpanCriteria spanCriteria = new EnrollmentSpanCriteria()
                .setSchoolOids(Arrays.asList(school.getOid()))
                .setIncludeSecondarySpans(false)
                .setExcludeStudent(TnStudent.FIELD_EXCLUDE_FROM_REPORTING);
        X2Criteria candidateCriteria = CriteriaHelper.getStudentCandidateCriteria(spanCriteria, getBroker());
        // load students using filterable
        FilterableFactory.create(getBroker(), getDictionaryExtractor(), TnStudent.class, candidateCriteria,
                null);

        ToolBean.preload(getBroker(), getDictionaryExtractor(),
                Arrays.asList(ToolEnrollment.FIELD_DATE_DESC, ToolEnrollment.FIELD_TIMESTAMP_DESC),
                ToolStudent.CHILD_STUDENT_ENROLLMENTS);

        List<TnStudent> students = ToolBean.getCachedToolBeans(TnStudent.class).stream()
                .filter(student -> {
                    return student.getTnStudentEnrollmentSpans(getBroker()).stream()
                            .filter(span -> {
                                if (span.getSchool() == null || !span.getSchool().getOid().equals(school.getOid())) {
                                    return false;
                                } else if ((span.getLastActiveDate() != null
                                        && span.getLastActiveDate()
                                        .before(m_periodDatesMap.get(span.getSchool().getOid()).getKey()))
                                        ||
                                        (span.getFirstActiveDate() != null
                                        && span.getFirstActiveDate().after(
                                                m_periodDatesMap.get(span.getSchool().getOid()).getValue()))) {
                                    return false;
                                } else if (span != null && span.getFirstActiveEnrollment() != null
                                        && (StudentEnrollment.ENTRY
                                                .equals(span.getFirstActiveEnrollment().getEnrollmentType())
                                                || StudentEnrollment.YOG_CHANGE
                                                .equals(span.getFirstActiveEnrollment().getEnrollmentType()))
                                        &&
                                        span.getFirstInactiveEnrollment() != null && StudentEnrollment.WITHDRAWAL
                                        .equals(span.getFirstInactiveEnrollment().getEnrollmentType())
                                        &&
                                        (span.getFirstActiveEnrollment().getEnrollmentDate()
                                                .equals(span.getFirstInactiveEnrollment().getEnrollmentDate())
                                                || (m_periodHelper == null
                                                && csdList != null
                                                && !csdList.isEmpty()
                                                && span.getFirstInactiveEnrollment().getEnrollmentDate()
                                                .equals(csdList.get(0).getDate())))) {
                                    // Remove if first and last day are the same
                                    return false;
                                }
                                return true;
                            })
                            .anyMatch(span -> true);
                })
                .collect(Collectors.toList());

        // create list of student oids to filter cache before additional preloads
        Set<String> studentsList =
                students.stream().map(TnStudent::getOid).collect(Collectors.toSet());
        ToolBean.filterCachedToolBeans(TnStudent.class,
                student -> studentsList.contains(student.getOid()));

        ToolBean.addAndCriteria(getBroker(), TnStudentAttendance.class,
                getStudentAttendanceCriteria(school));

        ToolBean.preload(getBroker(), getDictionaryExtractor(), null, ToolStudent.CHILD_STUDENT_ATTENDANCE);

        return students;
    }

    /**
     * Initialize helper objects. Should be called when
     * m_schoolPeriodStartDate/m_schoolPeriodEndDate initialized
     */
    private void initHelpers() {

        if (m_schools != null && !m_schools.isEmpty()) {
            Collection<String> sklOids = new ArrayList<>();
            for (TnSchool school : m_schools) {
                sklOids.add(school.getOid());
                PlainDate schoolPeriodStartDate =
                        m_periodHelper == null ? m_dateStart : m_periodHelper.getDateBegin(school.getOid());
                PlainDate schoolPeriodEndDate =
                        m_periodHelper == null ? m_dateEnd : m_periodHelper.getDateEnd(school.getOid());
                KeyValuePair<PlainDate, PlainDate> pairToPut = null;
                if (schoolPeriodStartDate != null && schoolPeriodEndDate != null) {
                    pairToPut = new KeyValuePair<PlainDate, PlainDate>(schoolPeriodStartDate, schoolPeriodEndDate);
                }
                m_periodDatesMap.put(school.getOid(), pairToPut);
            }
        }
    }

    /**
     * Initialize report formats.
     */
    private void initReportsFormat() {
        String formatPDF = (String) getParameter(INPUT_REPORT_ID_PDF);
        String formatCSV = (String) getParameter(INPUT_REPORT_ID_CSV);
        ToolJob job = this.getJob();
        switch (job.getInput().getFormat()) {
            case ToolInput.CSV_FORMAT:
                this.setFormatId(formatCSV);
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
     * Checks if is between.
     *
     * @param startPeriod PlainDate
     * @param endPeriod PlainDate
     * @param findingDate PlainDate
     * @return true, if is between
     */
    private boolean isBetween(PlainDate startPeriod, PlainDate endPeriod, PlainDate findingDate) {

        boolean isBetween = false;
        if (!findingDate.before(startPeriod) && (endPeriod == null || !findingDate.after(endPeriod))) {
            isBetween = true;
        }
        return isBetween;
    }

    /**
     * Populates the grid.
     *
     * @param school SisSchool
     * @param schoolCommonCalendars
     */
    private void populateGrid(TnSchool school, Filterable<ToolSchoolCalendar> schoolCommonCalendars) {
        List<ToolSchoolCalendarDate> csdList = null;
        Collection<PlainDate> inSessionDays = Collections.EMPTY_LIST;
        if (m_periodHelper != null) {
            inSessionDays = m_periodHelper.getDaysInSession(school.getOid());
        } else {
            ToolSchoolCalendar calendar =
                    schoolCommonCalendars.extractFirst(ToolSchoolCalendar.FIELD_SCHOOL_OID, school.getOid());
            if (calendar != null) {
                TreeSet<ToolSchoolCalendarDate> dates =
                        calendar.getCalendarDatesForRange(getBroker(), Range.of(m_dateStart, m_dateEnd));
                csdList = dates != null
                        ? dates.stream().filter(csd -> csd.getInSessionIndicator()).collect(Collectors.toList())
                                : Collections.emptyList();
                inSessionDays = csdList.stream().map(ToolSchoolCalendarDate::getDate).collect(Collectors.toList());
            }
        }
        for (TnStudent student : getStudents(school, csdList)) {
            String studentOid = student.getOid();
            Set<PlainDate> membership = student.getMembershipDates(getBroker(), school, csdList, inSessionDays);
            Map<String, Integer> absences = getAbsenceMap(school, student, membership);
            if (!m_grid.locate(COLUMN_STUDENT_OID, studentOid) && !absences.isEmpty()) {
                m_grid.append();
                m_grid.set(COLUMN_SCHOOL_OID, school.getOid());
                m_grid.set(COLUMN_SCHOOL_NAME, school.getName());
                m_grid.set(COLUMN_STUDENT_NAME, student.getNameView());
                m_grid.set(COLUMN_STUDENT_GRADE, student.getGradeLevel());
                m_grid.set(COLUMN_STUDENT_EIS, student.getEISStateId());
                m_grid.set(COLUMN_STUDENT_LOCAL_ID, student.getLocalId());
                int daysInSession = m_periodHelper != null ? m_periodHelper.getDaysInSession(school.getOid()).size()
                        : m_csdMapBySkl != null ? m_csdMapBySkl.get(school.getOid()).size() : 0;
                m_grid.set(COLUMN_PERIOD_DAYS_IN_SESSION, Integer.valueOf(daysInSession));
                int enrCount = membership.size();
                m_grid.set(COLUMN_PERIOD_ENROLLED, Integer.valueOf(enrCount));
                int presentCount = enrCount;
                if (absences != null) {
                    for (Entry<String, Integer> entry : absences.entrySet()) {
                        m_grid.set(entry.getKey(), entry.getValue());
                        if (COLUMN_PERIOD_ABSENT.equals(entry.getKey())) {
                            if (entry.getValue() != null) {
                                presentCount -= entry.getValue().intValue();
                            }
                        }
                    }
                }
                m_grid.set(COLUMN_PERIOD_PRESENT, Integer.valueOf(presentCount));
                Double percentAbsence = Double.valueOf(100d - (double) presentCount / enrCount * 100);
                if (1 == m_absenceThresholdOption || m_absenceThreshold == null
                        || percentAbsence.compareTo(Double.valueOf((m_absenceThreshold.toString()))) > 0) {
                    m_grid.set(COLUMN_PERIOD_ADA, Double.valueOf((double) presentCount / enrCount * 100));
                } else {
                    m_grid.deleteRow();
                }
            }
        }
    }
}
