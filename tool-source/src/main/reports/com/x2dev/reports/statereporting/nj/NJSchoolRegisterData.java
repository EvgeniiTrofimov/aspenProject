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
package com.x2dev.reports.statereporting.nj;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.KeyValueTrio;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.*;
import java.util.Map.Entry;
import org.apache.commons.lang3.StringUtils;

/**
 * The Class NJSchoolRegisterData.
 *
 * This class implements the non-standard membership rules used in NJ. Attendance must always be
 * queried from the beginning of the year to get the correct max day count for college visits.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class NJSchoolRegisterData extends ReportJavaSourceNet {

    /**
     * The Class NJSchoolRegisterHelper.
     */
    private static class NJSchoolRegisterHelper extends StudentHistoryHelper {
        private static final String CODE_NON_MEMBER = "NONMEM";
        private static final String CODE_NON_MEMBER_COLLEGE = "CLGNONMEM";
        private static final int INDEX_ATTENDANCE = 0;
        private static final int INDEX_NON_MEMBER = 1;
        private static final int INDEX_NON_MEMBER_COLLEGE = 2;
        private static final int MAX_NON_MEMBER_COLLEGE_COUNT = 3;

        private Map<String, List<StudentAttendance>[]> m_studentAttendanceMap;

        /**
         * Instantiates a new NJ school register helper.
         *
         * @param data StateReportData
         */
        public NJSchoolRegisterHelper(StateReportData data) {
            super(data);
        }

        /**
         * Gets the non member student attendances.
         *
         * @param studentOid String
         * @return List
         */
        public List<StudentAttendance> getNonMemberStudentAttendances(String studentOid) {
            if (m_studentAttendanceMap == null) {
                initStudentAttendance();
            }

            // Return the requests students attendance records.
            List<StudentAttendance> value = null;
            List<StudentAttendance>[] lists = m_studentAttendanceMap.get(studentOid);
            if (lists != null) {
                if (!lists[INDEX_NON_MEMBER].isEmpty()) {
                    if (!lists[INDEX_NON_MEMBER_COLLEGE].isEmpty()) {
                        value = new ArrayList(lists[INDEX_NON_MEMBER].size() + lists[INDEX_NON_MEMBER_COLLEGE].size());
                        value.addAll(lists[INDEX_NON_MEMBER]);
                        value.addAll(lists[INDEX_NON_MEMBER_COLLEGE]);
                    } else {
                        value = lists[INDEX_NON_MEMBER];
                    }
                } else if (!lists[INDEX_NON_MEMBER_COLLEGE].isEmpty()) {
                    value = lists[INDEX_NON_MEMBER_COLLEGE];
                }
            }
            return value;
        }

        /**
         * Gets the student attendances.
         *
         * @param studentOid String
         * @return List
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#getStudentAttendances(java.lang.String)
         */
        @Override
        public List<StudentAttendance> getStudentAttendances(String studentOid) {
            if (m_studentAttendanceMap == null) {
                initStudentAttendance();
            }

            // Return the requests students attendance records.
            List<StudentAttendance>[] lists = m_studentAttendanceMap.get(studentOid);
            return lists == null ? null : lists[INDEX_ATTENDANCE];
        }

        /**
         * Inits the student attendance.
         */
        private void initStudentAttendance() {
            PlainDate beginDate = getData().getCurrentContext().getStartDate();
            PlainDate endDate = getData().getCurrentContext().getEndDate();

            X2Criteria studentAttendanceCriteria = new X2Criteria();
            studentAttendanceCriteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);
            studentAttendanceCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, beginDate);
            studentAttendanceCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, endDate);

            // Apply school criteria.
            if (getData().isSchoolContext()) {
                studentAttendanceCriteria.addEqualTo(StudentAttendance.REL_SCHOOL,
                        getData().getSchool().getOid());
            }

            BeanQuery studentAttendanceQuery;
            studentAttendanceQuery = new BeanQuery(StudentAttendance.class, studentAttendanceCriteria);

            studentAttendanceQuery.addOrderBy(StudentAttendance.COL_STUDENT_OID, true);
            studentAttendanceQuery.addOrderBy(StudentAttendance.COL_DATE, true);

            m_studentAttendanceMap = new HashMap();
            QueryIterator iterator = getData().getBroker().getIteratorByQuery(studentAttendanceQuery);
            try {
                while (iterator.hasNext()) {
                    StudentAttendance bean = (StudentAttendance) iterator.next();
                    List<StudentAttendance>[] lists = m_studentAttendanceMap.get(bean.getStudentOid());
                    if (lists == null) {
                        lists = new List[] {new LinkedList(), new LinkedList(), new LinkedList()};
                        m_studentAttendanceMap.put(bean.getStudentOid(), lists);
                    }
                    String reasonCode = getData().lookupStateValue(StudentAttendance.class,
                            StudentAttendance.COL_REASON_CODE, bean.getReasonCode());
                    if (!StringUtils.isEmpty(reasonCode) && reasonCode.contains(CODE_NON_MEMBER_COLLEGE)) {
                        if (lists[INDEX_NON_MEMBER_COLLEGE].size() < MAX_NON_MEMBER_COLLEGE_COUNT) {
                            lists[INDEX_NON_MEMBER_COLLEGE].add(bean);
                        } else {
                            lists[INDEX_ATTENDANCE].add(bean);
                        }
                    } else if (!StringUtils.isEmpty(reasonCode) && reasonCode.contains(CODE_NON_MEMBER)) {
                        lists[INDEX_NON_MEMBER].add(bean);
                    } else {
                        lists[INDEX_ATTENDANCE].add(bean);
                    }
                }
            } finally {
                iterator.close();
            }
        }
    }
    /**
     * The Class SubSpan.
     */
    private class SubSpan implements Comparable<SubSpan> {
        private SisStudent m_student;
        private PlainDate m_spanBeginDate;
        private PlainDate m_spanEndDate;
        private StudentEnrollmentSpan m_span;

        /**
         * Instantiates a new sub span.
         *
         * @param student SisStudent
         * @param beginDate PlainDate
         * @param endDate PlainDate
         * @param span StudentEnrollmentSpan
         */
        protected SubSpan(SisStudent student, PlainDate beginDate, PlainDate endDate, StudentEnrollmentSpan span) {
            super();
            m_student = student;
            m_spanBeginDate = beginDate;
            m_spanEndDate = endDate;
            m_span = span;
        }

        /**
         * Compare to.
         *
         * @param other SubSpan
         * @return int
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(SubSpan other) {
            return this.m_student.getNameView().compareTo(other.m_student.getNameView());
        }

        /**
         * Gets the absent excused days.
         *
         * @return Big decimal
         */
        public BigDecimal getAbsentExcusedDays() {
            BigDecimal days = BigDecimal.ZERO;
            List<StudentAttendance> list = m_helper.getStudentAttendances(m_student.getOid());
            if (list != null && !list.isEmpty()) {
                for (StudentAttendance att : list) {
                    if (!att.getDate().before(m_spanBeginDate) && !att.getDate().after(m_spanEndDate)
                            && att.getAbsentIndicator() && att.getExcusedIndicator()) {
                        days = days.add(att.getPortionAbsent());
                    }
                }
            }
            return days;
        }

        /**
         * Gets the absent unexcused days.
         *
         * @return Big decimal
         */
        public BigDecimal getAbsentUnexcusedDays() {
            BigDecimal days = BigDecimal.ZERO;
            List<StudentAttendance> list = m_helper.getStudentAttendances(m_student.getOid());
            if (list != null && !list.isEmpty()) {
                for (StudentAttendance att : list) {
                    if (!att.getDate().before(m_spanBeginDate) && !att.getDate().after(m_spanEndDate)
                            && att.getAbsentIndicator() && !att.getExcusedIndicator()) {
                        days = days.add(att.getPortionAbsent());
                    }
                }
            }
            return days;
        }

        /**
         * Gets the calendar.
         *
         * @return String
         */
        public String getCalendar() {
            return StringUtils.isEmpty(getStudent().getCalendarCode()) ? "Missing" : getStudent().getCalendarCode();
        }

        /**
         * Gets the enrollment code.
         *
         * @return String
         */
        public String getEnrollmentCode() {
            String enrollmentCode = null;
            if (getWithdrawalDate() != null) {
                enrollmentCode = m_span.getFirstInactiveEnrollment().getEnrollmentCode();
            } else if (getEnrollmentDate() != null) {
                enrollmentCode = m_span.getFirstActiveEnrollment().getEnrollmentCode();
                if (StringUtils.isEmpty(enrollmentCode)
                        && "S".equals(m_span.getFirstActiveEnrollment().getEnrollmentType())) {
                    Collection<StudentEnrollment> enrs = m_span.getEnrollments();
                    PlainDate enrDate = m_span.getFirstActiveEnrollment().getEnrollmentDate();
                    for (StudentEnrollment enr : enrs) {
                        if ("E".equals(enr.getEnrollmentType()) &&
                                !enrDate.before(enr.getEnrollmentDate())) {
                            enrollmentCode = enr.getEnrollmentCode();
                        }
                    }
                }
            }
            return enrollmentCode;
        }

        /**
         * Gets the enrollment date.
         *
         * @return Plain date
         */
        public PlainDate getEnrollmentDate() {
            return m_span.getFirstActiveDate().before(m_spanBeginDate) ? null : m_span.getFirstActiveDate();
        }

        /**
         * Gets the enrollment days.
         *
         * @return int
         */
        public BigDecimal getEnrollmentDays() {
            BigDecimal numDays = BigDecimal.ZERO;

            Set<PlainDate> insessionDates = m_helper.getCalendarDays(m_span.getSchool(), getCalendar());
            if (insessionDates == null) {
                insessionDates = m_helper.getCalendarDays(m_span.getSchool(), StudentHistoryHelper.CALENDAR_ANY);
            }

            List<StudentAttendance> list = m_helper.getNonMemberStudentAttendances(getStudent().getOid());

            if (insessionDates != null) {
                for (PlainDate date : insessionDates) {
                    if (!date.before(m_spanBeginDate) && !date.after(m_spanEndDate)) {
                        numDays = numDays.add(BigDecimal.ONE);
                        if (list != null) {
                            for (StudentAttendance item : list) {
                                if (date.equals(item.getDate())) {
                                    numDays = numDays.subtract(item.getPortionAbsent());
                                    break;
                                }
                            }
                        }
                    }
                }
            }
            return numDays;
        }

        /**
         * Gets the present days.
         *
         * @return Big decimal
         */
        public BigDecimal getPresentDays() {
            BigDecimal daysPresent = getEnrollmentDays();
            daysPresent = daysPresent.subtract(getAbsentExcusedDays());
            daysPresent = daysPresent.subtract(getAbsentUnexcusedDays());
            return daysPresent;
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
         * Gets the withdrawal date.
         *
         * @return Plain date
         */
        public PlainDate getWithdrawalDate() {
            return (m_span.getLastActiveDate() != null && !m_span.getLastActiveDate().after(m_spanEndDate))
                    ? m_span.getLastActiveDate() : null;
        }
    }


    /**
     * The Class SubSpanAccumulator.
     */
    private class SubSpanAccumulator {
        ReferenceCode m_code;
        Map<String, KeyValueTrio<BigDecimal, BigDecimal, List<SubSpan>>> m_mapCalendarValues = new HashMap();

        /**
         * Instantiates a new sub span accumulator.
         *
         * @param code ReferenceCode
         */
        public SubSpanAccumulator(ReferenceCode code) {
            m_code = code;
        }

        /**
         * Adds the span.
         *
         * @param span SubSpan
         */
        public void addSpan(SubSpan span) {
            String calendarId = span.getCalendar();
            KeyValueTrio<BigDecimal, BigDecimal, List<SubSpan>> trio = m_mapCalendarValues.get(calendarId);
            if (trio == null) {
                List<SubSpan> list = new LinkedList();
                list.add(span);
                trio = new KeyValueTrio(span.getEnrollmentDays(), span.getPresentDays(), list);
            } else {
                trio.getValue2().add(span);
                trio = new KeyValueTrio(trio.getKey().add(span.getEnrollmentDays()),
                        trio.getValue1().add(span.getPresentDays()), trio.getValue2());
            }
            m_mapCalendarValues.put(calendarId, trio);
        }

        /**
         * Gets the calendars.
         *
         * @return List
         */
        public List<String> getCalendars() {
            List calendars = new ArrayList(m_mapCalendarValues.keySet().size());
            calendars.addAll(m_mapCalendarValues.keySet());
            if (!calendars.isEmpty()) {
                Collections.sort(calendars);
            }
            return calendars;
        }

        /**
         * Gets the days enrolled.
         *
         * @param calendarId String
         * @return Integer
         */
        public BigDecimal getDaysEnrolled(String calendarId) {
            BigDecimal value = BigDecimal.ZERO;
            KeyValueTrio<BigDecimal, BigDecimal, List<SubSpan>> trio = m_mapCalendarValues.get(calendarId);
            if (trio != null) {
                value = trio.getKey();
            }
            return value;
        }

        /**
         * Gets the days present.
         *
         * @param calendarId String
         * @return Big decimal
         */
        public BigDecimal getDaysPresent(String calendarId) {
            BigDecimal value = BigDecimal.ZERO;
            KeyValueTrio<BigDecimal, BigDecimal, List<SubSpan>> trio = m_mapCalendarValues.get(calendarId);
            if (trio != null) {
                value = trio.getValue();
            }
            return value;
        }

        /**
         * Gets the reference code.
         *
         * @return Reference code
         */
        public ReferenceCode getReferenceCode() {
            return m_code;
        }

        /**
         * Gets the spans.
         *
         * @param calendarId String
         * @return List
         */
        public List<SubSpan> getSpans(String calendarId) {
            List<SubSpan> value = null;
            KeyValueTrio<BigDecimal, BigDecimal, List<SubSpan>> trio = m_mapCalendarValues.get(calendarId);
            if (trio != null) {
                value = trio.getValue2();
                Collections.sort(value);
            } else {
                value = new LinkedList();
            }
            return value;
        }
    }

    private static final String ALIAS_PROGRAM_TYPE = "DOE PROG TYPE CODE";

    private static final String DETAIL_FIELD_STUDENT = "student";
    private static final String DETAIL_FIELD_PROGRAM_TYPE_REF_CODE = "programTypeRefCode";
    private static final String DETAIL_FIELD_ENROLLMENT_DATE = "enrollmentDate";
    private static final String DETAIL_FIELD_WITHDRAWAL_DATE = "withdrawalDate";
    private static final String DETAIL_FIELD_ENROLLMENT_CODE = "enrollmentCode";
    private static final String DETAIL_FIELD_DAYS_ENROLLED = "enrolledDays";
    private static final String DETAIL_FIELD_DAYS_PRESENT = "presentDays";

    private static final String DETAIL_FIELD_DAYS_EXCUSED = "excusedAbsentDays";
    private static final String DETAIL_FIELD_DAYS_NOT_EXCUSED = "notExcusedAbsentDays";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_PROGRAM_TYPE_REF_CODE = "programTypeRefCode";
    private static final String FIELD_CALENDAR = "calendar";
    private static final String FIELD_DAYS_OPEN = "daysOpen";

    private static final String FIELD_DAYS_ENROLLED = "daysEnrolled";
    private static final String FIELD_DAYS_PRESENT = "daysPresent";

    private static final String INPUT_PARAM_END_DATE = "endDate";
    private static final String INPUT_PARAM_START_DATE = "startDate";
    private static final String INPUT_PARAM_INCLUDE_DETAILS = "includeDetails";
    private static final String INPUT_PARAM_INCLUDE_EXCLUDE_STUDENTS = "includeExcludeStudents";

    private static final String OUTPUT_PARAM_DETAIL_DATA_MAP = "detailDataMap";
    private static final String OUTPUT_PARAM_FORMAT = "format";

    private static final String SUB_REPORT_ID = "NJ-SCH-REGISTER_SUB1";

    private Map<String, ReferenceCode> m_codeMap = null;
    private PlainDate m_endDate;
    private DataDictionaryField m_fieldProgramType;
    private NJSchoolRegisterHelper m_helper;
    private boolean m_includeDetails;
    private Map<String, Map<String, Integer>> m_mapDaysOpen = new HashMap();
    private Map<String, Map<String, ReportDataGrid>> m_mapDetailGrid = new HashMap();
    private Map<String, Map<String, List<SubSpan>>> m_spanMap = new HashMap();
    private PlainDate m_startDate;

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        m_startDate = (PlainDate) getParameter(INPUT_PARAM_START_DATE);
        m_endDate = (PlainDate) getParameter(INPUT_PARAM_END_DATE);
        m_includeDetails = (getParameter(INPUT_PARAM_INCLUDE_DETAILS) != null
                && getParameter(INPUT_PARAM_INCLUDE_DETAILS) instanceof Boolean
                && ((Boolean) getParameter(INPUT_PARAM_INCLUDE_DETAILS)).booleanValue());
        if (m_includeDetails) {
            addParameter(OUTPUT_PARAM_FORMAT, getSubReportFormat());
            addParameter(OUTPUT_PARAM_DETAIL_DATA_MAP, m_mapDetailGrid);
        }

        StateReportData data = initStateReportData();
        m_fieldProgramType = data.getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_PROGRAM_TYPE);
        initCodesMap(data);

        m_helper = new NJSchoolRegisterHelper(data);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_ATTENDANCE, Boolean.TRUE);
        boolean applyExclude = (getParameter(INPUT_PARAM_INCLUDE_EXCLUDE_STUDENTS) != null
                && getParameter(INPUT_PARAM_INCLUDE_EXCLUDE_STUDENTS) instanceof Boolean
                && !((Boolean) getParameter(INPUT_PARAM_INCLUDE_EXCLUDE_STUDENTS)).booleanValue());
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.valueOf(applyExclude));
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);

        QueryIterator students = getBroker().getIteratorByQuery(m_helper.getStudentQuery(false));
        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();
                processStudent(student);
            }
        } finally {
            students.close();
        }

        ReportDataGrid grid = generateGrid();

        return grid;
    }

    /**
     * Generate grid.
     *
     * @return ReportDataGrid
     */
    private ReportDataGrid generateGrid() {
        ReportDataGrid grid = new ReportDataGrid();
        List<ReferenceCode> listCodes = sortedCodeList();
        for (SisSchool school : getSchools()) {
            Map<String, List<SubSpanAccumulator>> mapAccumulatorByCalendar = new TreeMap();
            Map<String, List<SubSpan>> map = m_spanMap.get(school.getOid());
            if (map != null && !map.isEmpty()) {
                for (ReferenceCode code : listCodes) {
                    List<SubSpan> spans = map.get(code.getCode());
                    if (spans != null && !spans.isEmpty()) {
                        processReferenceCode(mapAccumulatorByCalendar, code, spans);
                    }
                }
                List<SubSpan> spans = map.get("");
                if (spans != null && !spans.isEmpty()) {
                    processReferenceCode(mapAccumulatorByCalendar, null, spans);
                }
            }
            for (Entry<String, List<SubSpanAccumulator>> entry : mapAccumulatorByCalendar.entrySet()) {
                ReportDataGrid detailGrid = null;
                if (m_includeDetails) {
                    detailGrid = new ReportDataGrid();
                }
                String calendarCode = mapAccumulatorByCalendar.size() == 1 ? null : entry.getKey();
                for (SubSpanAccumulator accumulator : entry.getValue()) {
                    grid.append();
                    grid.set(FIELD_SCHOOL, school);
                    grid.set(FIELD_PROGRAM_TYPE_REF_CODE, accumulator.getReferenceCode());
                    grid.set(FIELD_CALENDAR, calendarCode);
                    grid.set(FIELD_DAYS_OPEN, getDaysOpen(school, entry.getKey()));
                    grid.set(FIELD_DAYS_ENROLLED, accumulator.getDaysEnrolled(entry.getKey()));
                    grid.set(FIELD_DAYS_PRESENT, accumulator.getDaysPresent(entry.getKey()));
                    if (m_includeDetails) {
                        populateDetailGrid(detailGrid, accumulator.getReferenceCode(), calendarCode,
                                accumulator.getSpans(entry.getKey()));
                    }
                }
                if (detailGrid != null && !detailGrid.isEmpty()) {
                    detailGrid.beforeTop();
                    Map<String, ReportDataGrid> detailMap = m_mapDetailGrid.get(school.getOid());
                    if (detailMap == null) {
                        detailMap = new HashMap();
                        m_mapDetailGrid.put(school.getOid(), detailMap);
                    }
                    detailMap.put(calendarCode, detailGrid);
                }
            }
        }
        grid.beforeTop();
        return grid;
    }

    /**
     * Gets the days open.
     *
     * @param school SisSchool
     * @param calendar String
     * @return Integer
     */
    private Integer getDaysOpen(SisSchool school, String calendar) {
        Map<String, Integer> daysOpen = m_mapDaysOpen.get(school.getOid());
        if (daysOpen == null) {
            daysOpen = new HashMap();
            m_mapDaysOpen.put(school.getOid(), daysOpen);
        }
        Integer days = daysOpen.get(calendar);
        if (days == null) {
            Set<PlainDate> insessionDates = m_helper.getCalendarDays(school, calendar);
            if (insessionDates == null) {
                insessionDates = m_helper.getCalendarDays(school, StudentHistoryHelper.CALENDAR_ANY);
            }
            if (insessionDates == null) {
                days = Integer.valueOf(0);
            } else {
                int numDays = 0;
                for (PlainDate date : insessionDates) {
                    if (!date.before(m_startDate) && !date.after(m_endDate)) {
                        ++numDays;
                    }
                }
                days = Integer.valueOf(numDays);
            }
            daysOpen.put(calendar, days);
        }
        return days;
    }

    /**
     * Gets the program type.
     *
     * @param student SisStudent
     * @return String
     */
    private String getProgramType(SisStudent student) {
        String type = "";
        if (m_fieldProgramType != null) {
            String value = (String) student.getFieldValueByBeanPath(m_fieldProgramType.getJavaName());
            if (!StringUtils.isEmpty(value) && m_codeMap.containsKey(value)) {
                type = value;
            }
        }
        return type;
    }

    /**
     * Gets the schools.
     *
     * @return List
     */
    private List<SisSchool> getSchools() {
        List<SisSchool> schools = new LinkedList();
        if (this.isSchoolContext()) {
            schools.add((SisSchool) this.getSchool());
        } else {
            X2Criteria criteria = new X2Criteria();
            criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            BeanQuery query = new BeanQuery(SisSchool.class, criteria);
            query.addOrderBy(SisSchool.COL_NAME, true);
            schools.addAll(getBroker().getCollectionByQuery(query));
        }
        return schools;
    }

    /**
     * Gets the sub report format.
     *
     * @return byte[]
     */
    private byte[] getSubReportFormat() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getRootOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, SUB_REPORT_ID);
        BeanQuery query = new BeanQuery(Report.class, criteria);

        Report report = (Report) getBroker().getBeanByQuery(query);
        return report.getCompiledFormat();
    }

    /**
     * Inits the codes map.
     *
     * @param data StateReportData
     */
    private void initCodesMap(StateReportData data) {
        if (m_fieldProgramType != null && m_fieldProgramType.getDataTable().getDataClass().equals(SisStudent.class)
                && !StringUtils.isEmpty(m_fieldProgramType.getReferenceTableOid())) {
            m_codeMap = data.getReferenceCodes(m_fieldProgramType.getReferenceTableOid());
        } else {
            m_codeMap = new HashMap();
        }
    }

    /**
     * Inits the state report data.
     *
     * @return StateReportData
     * @throws X2BaseException exception
     */
    private StateReportData initStateReportData() throws X2BaseException {
        StateReportData data = new StateReportData();
        data.setBroker(getBroker());
        data.setOrganization(getOrganization());
        data.setPrivilegeSet(getPrivilegeSet());
        data.setSchoolContext(isSchoolContext());
        data.setSchool(getSchool());
        data.setParameters(getParameters());
        data.setUser(getUser());
        data.initializeExport();
        return data;
    }

    /**
     * Populate detail grid.
     *
     * @param grid ReportDataGrid
     * @param code ReferenceCode
     * @param string String
     * @param spans List<SubSpan>
     */
    private void populateDetailGrid(ReportDataGrid grid, ReferenceCode code, String string, List<SubSpan> spans) {
        for (SubSpan span : spans) {
            grid.append();
            grid.set(DETAIL_FIELD_PROGRAM_TYPE_REF_CODE, code);
            grid.set(DETAIL_FIELD_STUDENT, span.getStudent());
            grid.set(DETAIL_FIELD_ENROLLMENT_DATE, span.getEnrollmentDate());
            grid.set(DETAIL_FIELD_WITHDRAWAL_DATE, span.getWithdrawalDate());
            grid.set(DETAIL_FIELD_ENROLLMENT_CODE, span.getEnrollmentCode());
            grid.set(DETAIL_FIELD_DAYS_ENROLLED, span.getEnrollmentDays());
            grid.set(DETAIL_FIELD_DAYS_PRESENT, span.getPresentDays());
            grid.set(DETAIL_FIELD_DAYS_EXCUSED, span.getAbsentExcusedDays());
            grid.set(DETAIL_FIELD_DAYS_NOT_EXCUSED, span.getAbsentUnexcusedDays());
        }
    }

    /**
     * Process reference code.
     *
     * @param mapAccumulatorByCalendar Map<String,List<SubSpanAccumulator>>
     * @param code ReferenceCode
     * @param spans List<SubSpan>
     */
    private void processReferenceCode(Map<String, List<SubSpanAccumulator>> mapAccumulatorByCalendar,
                                      ReferenceCode code,
                                      List<SubSpan> spans) {
        SubSpanAccumulator accumulator = new SubSpanAccumulator(code);
        for (SubSpan span : spans) {
            accumulator.addSpan(span);
        }
        List<String> calendars = accumulator.getCalendars();
        for (String calendar : calendars) {
            List<SubSpanAccumulator> accumulators = mapAccumulatorByCalendar.get(calendar);
            if (accumulators == null) {
                accumulators = new LinkedList();
                mapAccumulatorByCalendar.put(calendar, accumulators);
            }
            accumulators.add(accumulator);
        }
    }

    /**
     * Process student.
     *
     * @param student SisStudent
     */
    private void processStudent(SisStudent student) {
        for (StudentEnrollmentSpan span : m_helper.getStudentEnrollmentSpans(student, true)) {
            if (span.getSchool() != null) {
                PlainDate startDate =
                        (span.getFirstActiveDate().after(m_startDate)) ? span.getFirstActiveDate() : m_startDate;
                PlainDate endDate = (span.getLastActiveDate() == null || span.getLastActiveDate().after(m_endDate))
                        ? m_endDate : span.getLastActiveDate();
                if (startDate.before(endDate)) {
                    SubSpan subSpan = new SubSpan(student, startDate, endDate, span);
                    String programType = getProgramType(student);
                    processSubSpan(span.getSchool(), programType, subSpan);
                }
            }
        }
    }

    /**
     * Process sub span.
     *
     * @param school SisSchool
     * @param programType String
     * @param subSpan SubSpan
     */
    private void processSubSpan(SisSchool school, String programType, SubSpan subSpan) {
        Map<String, List<SubSpan>> map = m_spanMap.get(school.getOid());
        if (map == null) {
            map = new HashMap();
            m_spanMap.put(school.getOid(), map);
        }
        List<SubSpan> spans = map.get(programType);
        if (spans == null) {
            spans = new LinkedList();
            map.put(programType, spans);
        }
        spans.add(subSpan);
    }

    /**
     * Sorted code list.
     *
     * @return List
     */
    private List<ReferenceCode> sortedCodeList() {
        List<ReferenceCode> codes = new ArrayList(m_codeMap.size());
        codes.addAll(m_codeMap.values());
        Collections.sort(codes, new Comparator<ReferenceCode>() {
            @Override
            public int compare(ReferenceCode ref1, ReferenceCode ref2) {
                int value = ref1.getSequenceNumber() - ref2.getSequenceNumber();
                if (value == 0) {
                    value = (ref1.getDescription() == null ? "" : ref1.getDescription())
                            .compareTo((ref2.getDescription() == null ? "" : ref2.getDescription()));
                }
                return value;
            }
        });
        return codes;
    }

}
