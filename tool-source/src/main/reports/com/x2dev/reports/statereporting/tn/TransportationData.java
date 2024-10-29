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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper.Strategy;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentEnrollmentSpan;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentMultiYearHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentScheduleSpan;
import com.x2dev.procedures.statereporting.tn.TNStateReportData;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisSchoolCalendarDate;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentTransportation;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Data source of Data sources for the "TN Transportation Report" report and sub reports.
 *
 * @author X2 Development Corporation
 */
public class TransportationData extends ReportJavaSourceNet {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * The Class EnrollmentStatistics.
     */
    class EnrollmentStatistics extends TNStateReportData {
        protected static final String PARAM_END_DATE = "endDate";
        protected static final String PARAM_START_DATE = "startDate";

        private DateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
        private TNStudentMultiYearHelper m_studentMultiYearHelper;
        private TNEnrollmentHelper m_tnEnrHelper;
        private TNStudentHistoryHelper m_helper;

        /**
         * Adjust criteria based on passed strategy, bean path and arguments considering selected
         * context.
         * 1) If context is not overridden, add usual criteria.
         * 2) If context is overridden, create and add criteria with adjusted beanPath.
         * In this case we need add relation to context attributes with correct bean path of the
         * column.
         * 3) If context is overridden and field value is stored in blob, iterate through loaded
         * attributes to determine
         * attributes with blob containing needed field and value in the field. Then add criteria
         * filtering by students'
         * oids.
         *
         * @param criteria X2Criteria
         * @param strategy Strategy
         * @param beanPath String
         * @param args Object[]
         */
        public void adjustCriteria(X2Criteria criteria, Strategy strategy, String beanPath, Object... args) {
            m_studentMultiYearHelper.adjustCriteria(criteria, strategy, beanPath, args);
        }

        /**
         * Return calendar days.
         *
         * @param school SisSchool
         * @param calendar String
         * @return Set<PlainDate>
         */
        public Set<PlainDate> getCalendarDays(SisSchool school, String calendar) {
            return m_helper.getCalendarDays(school, calendar);
        }

        /**
         * Return student helper.
         *
         * @return Set<PlainDate>
         */
        public TNStudentHistoryHelper getStdHelper() {
            return m_helper;
        }

        /**
         * Return student's calendar ID.
         *
         * @param student SisStudent
         * @return X2Criteria
         */
        public String getStudentCalendarId(SisStudent student) {
            return getStdCalendarId(student);
        }

        /**
         * Return the current student criteria.
         *
         * @return X2Criteria
         */
        public X2Criteria getStudentCriteria() {
            return m_helper.getStudentCriteria();
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
            return m_helper.getTNStudentEnrollmentSpans(student, limit);
        }


        /**
         * Return the list of student schedule spans.
         *
         * @param student SisStudent
         * @return List<StudentScheduleSpan>
         */
        public List<TNStudentScheduleSpan> getTNStudentScheduleSpans(SisStudent student) {
            return m_helper.getTNStudentScheduleSpans(student);
        }


        /**
         * Return student attendances.
         *
         * @param studentOid String
         * @return List<StudentAttendance>
         */
        public List<StudentAttendance> getStudentAttendances(String studentOid) {
            return m_helper.getStudentAttendances(studentOid);
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
            return m_dateFormat.format(m_helper.getSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE));
        }

        /**
         * Get End Date.
         *
         * @return String
         */
        public String getEndDate() {
            return m_dateFormat.format(m_helper.getSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE));
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
            m_helper = m_tnEnrHelper.getStudentHistoryHelper();
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_yearStartDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_SECONDARY, Boolean.TRUE);
            m_studentMultiYearHelper = m_tnEnrHelper.getStudentMultiYearHelper();
        }
    }

    private static final String ALIAS_AM_BUS = "DOE AM BUS";
    private static final String ALIAS_ATTENDANCE_REPORTED = "DOE ATTENDANCE REPORTED";
    private static final String ALIAS_BUS_SERVICE_BEGIN_DATE = "all-rcd-BusServiceBeginDate";
    private static final String ALIAS_BUS_SERVICE_END_DATE = "all-rcd-BusServiceEndDate";
    private static final String ALIAS_EIS_STATE_ID = "DOE EIS STATE ID";
    private static final String ALIAS_EST_MILES = "DOE EST MILES TRANSPORTED";
    private static final String ALIAS_NUMERIC_GRADE = "NumericGradeLevel";
    private static final String ALIAS_PM_BUS = "DOE PM BUS";
    private static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL STATE ID";
    private static final String ALIAS_SPECIALL_EQUIPPED = "DOE SPECIALLY EQUIPPED";


    private static final String CONST_ATTENDANCE_AM = "attendanceAM";
    private static final String CONST_ATTENDANCE_PM = "attendancePM";
    private static final String CONST_ENROLLED_DAYS = "enrolledDays";
    private static final String CONST_ENROLLMENT_MILES_AM = "milesAM";
    private static final String CONST_ENROLLMENT_MILES_PM = "milesPM";
    private static final String CONST_ENROLLMENT_PER_AM = "periodAM";
    private static final String CONST_ENROLLMENT_PER_PM = "periodPM";
    private static final String CONST_ENROLLMENT_YTD_AM = "ytdAM";
    private static final String CONST_ENROLLMENT_YTD_PM = "ytdPM";
    private static final double CONST_MILES_LIMIT = 1.5;
    private static final Double CONST_ZERO_DOUBLE = Double.valueOf(0);

    private static final String DATE_FORMAT_SERVICE = "yyyy-MM-dd";

    private static final String DETAIL_REPORT_FIELD_ATT_AM = "AttendanceAM";
    private static final String DETAIL_REPORT_FIELD_ATT_PM = "AttendancePM";
    private static final String DETAIL_REPORT_FIELD_BUS_NUM = "BusNumber";
    private static final String DETAIL_REPORT_FIELD_CAL_DAYS = "CalendarDays";
    private static final String DETAIL_REPORT_FIELD_ENR_DAYS = "EnrolledDays";
    private static final String DETAIL_REPORT_FIELD_IS_BUS_SPEC = "IsBusSpecial";
    private static final String DETAIL_REPORT_FIELD_LOCAL_ID = "LocalID";
    private static final String DETAIL_REPORT_FIELD_MILES_TRANS_AM = "MilesTransportedAM";
    private static final String DETAIL_REPORT_FIELD_MILES_TRANS_PM = "MilesTransportedPM";
    private static final String DETAIL_REPORT_FIELD_PER_ENR_AM = "PeriodEnrollmentAM";
    private static final String DETAIL_REPORT_FIELD_PER_ENR_PM = "PeriodEnrollmentPM";
    private static final String DETAIL_REPORT_FIELD_SCHOOL_NAME = "SchoolName";
    private static final String DETAIL_REPORT_FIELD_STATE_ID = "StateID";
    private static final String DETAIL_REPORT_FIELD_STD_NAME = "StudentName";
    private static final String DETAIL_REPORT_FIELD_YTD_ENR_AM = "YTDEnrollmentAM";
    private static final String DETAIL_REPORT_FIELD_YTD_ENR_PM = "YTDEnrollmentPM";


    private static final String EXTENDED_DICTIONARY_ID_BUS = "REF-BUS";
    private static final String EXTENDED_DICTIONARY_ID_GRADE_NUMERIC = "REF-GRADE-LEVELS";

    private static final String FIELD_BUS_NUMBER = "busNumber";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_SCHOOL_NAME = "schoolName";

    private static final String INPUT_DELIMITER = "delimiter";
    private static final String INPUT_INCLUDE_BUS_NOT_REPORTED = "inclBussesNotReported";
    private static final String INPUT_K12_GRADES = "k12GradesNumeric";
    private static final String INPUT_PARAM_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_PARAM_BUS_NUMBERS = "busNumbers";
    private static final String INPUT_PARAM_SCHOOLS = "schoolOids";
    private static final String INPUT_PARAMETER_PERIOD = "reportPeriod";
    private static final String INPUT_PRINT_TOTALS = "printTotals";
    private static final String INPUT_REPORT_DATE = "reportDate";
    private static final String INPUT_REPORT_ID_CSV = "subreportIdCSVVersion";
    private static final String INPUT_REPORT_ID_PDF = "subreportIdPDFVersion";
    private static final String INPUT_REPORT_MODE = "reportMode";

    private static final String PARAM_CONTEXT = "context";
    private static final String PARAM_DISTIRCT = "district";
    private static final String PARAM_PRINT_TOTALS = "printTotals";
    private static final String PERIOD_ANNUAL = "Annual";
    private static final String PERIOD_ANNUAL_AS_OF = "Annual as of ";

    private static final String REF_TBL_OID = "rtbAttLockCycl";

    private static final String REPORT_PARAMETER_PERIOD = "period";
    private static final String REPORT_PARAMETER_PERIOD_DAYS = "periodDays";


    private static final String ZERO = "0";

    private String m_attendanceReported;
    private String m_busAMJavaName;
    private Map<String, ReferenceCode> m_busCodes = new HashMap<String, ReferenceCode>();
    private Map<String, ReferenceCode> m_buses = new HashMap<String, ReferenceCode>();
    private String m_busPMJavaName;
    private Calendar m_calendar;
    private DistrictSchoolYearContext m_context;
    private EnrollmentStatistics m_data;
    private Map<String, Set<PlainDate>> m_days = new HashMap<String, Set<PlainDate>>();
    private DataDictionary m_dictionary;
    private Set<PlainDate> m_emptySet = new HashSet();
    private String m_estMilesJavaName;
    private Map<String, PlainDate> m_firstDatesForSchools;
    private SimpleDateFormat m_format = new SimpleDateFormat(DATE_FORMAT_SERVICE);
    private Boolean m_includeNotReported;
    private String m_isBusSpeciallyEquipped;
    private Collection<String> m_k12grades;
    private ReferenceCode m_month = null;
    private Collection<ReferenceCode> m_monthCodes;
    private TNReportingPeriodHelper m_periodHelper;
    private Boolean m_printTotals;
    private PlainDate m_reportDate;
    private Integer m_reportMode = null;
    private ScheduleManager m_scheduleMgr;
    private Collection<SisSchool> m_schools = null;
    private boolean m_useDetail;
    PlainDate m_yearStartDate;

    PlainDate m_endDate;
    PlainDate m_startDate;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid();
        ReportDataGrid detailGrid = new ReportDataGrid();
        if (m_data.getSetupErrors().isEmpty()) {
            iterateTransportations(grid, detailGrid);
        } else {
            for (StateReportValidationError error : m_data.getSetupErrors()) {
                AppGlobals.getLog().log(Level.SEVERE, error.getErrorMessage());
            }
        }
        if (grid.rowCount() == 0) {
            grid.append();
            grid.set(FIELD_BUS_NUMBER, ZERO);
            grid.set(CONST_ENROLLMENT_YTD_AM, CONST_ZERO_DOUBLE);
            grid.set(CONST_ENROLLMENT_YTD_PM, CONST_ZERO_DOUBLE);
            grid.set(CONST_ENROLLMENT_PER_AM, CONST_ZERO_DOUBLE);
            grid.set(CONST_ENROLLMENT_PER_PM, CONST_ZERO_DOUBLE);
            grid.set(CONST_ENROLLMENT_MILES_AM, CONST_ZERO_DOUBLE);
            grid.set(CONST_ENROLLMENT_MILES_PM, CONST_ZERO_DOUBLE);
            grid.set(CONST_ATTENDANCE_AM, CONST_ZERO_DOUBLE);
            grid.set(CONST_ATTENDANCE_PM, CONST_ZERO_DOUBLE);
            grid.set(FIELD_SCHOOL, getSchool());
            addParameter(REPORT_PARAMETER_PERIOD_DAYS, Double.valueOf(1));

        }

        if (m_useDetail) {
            detailGrid.sort(Arrays.asList(new String[] {DETAIL_REPORT_FIELD_SCHOOL_NAME, DETAIL_REPORT_FIELD_STD_NAME}),
                    false);
            detailGrid.beforeTop();
            return detailGrid;
        }
        grid.sort(FIELD_SCHOOL_NAME, false);
        grid.beforeTop();
        return grid;
    }


    /**
     * Returns school if school collection contains 1 school.
     *
     * @return School
     */
    @Override
    protected School getSchool() {
        SisSchool school = null;
        if (m_schools.size() == 1) {
            ArrayList<SisSchool> schools = new ArrayList<SisSchool>(m_schools);
            school = schools.get(0);
        }
        return school;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_calendar = Calendar.getInstance();

        initReportsFormat();
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_schools = getSchools();

        m_context = getCurrentContext();

        m_reportMode = getParameter(INPUT_REPORT_MODE) != null ? (Integer) getParameter(INPUT_REPORT_MODE)
                : Integer.valueOf(0);
        String month;
        switch (m_reportMode.intValue()) {
            // Report Annual as of mode
            case 2:
                m_reportDate = (PlainDate) getParameter(INPUT_REPORT_DATE);
                if (m_reportDate == null ||
                        (m_startDate != null && m_reportDate.before(m_startDate))) {
                    String errorMessage =
                            "Report can not be generated. Selected Report Date: " + m_reportDate.toString() +
                                    " is before context start date: " + m_startDate.toString();
                    AppGlobals.getLog().log(Level.SEVERE, errorMessage);
                    throw new IllegalArgumentException(errorMessage);
                } else if (m_endDate != null && m_reportDate != null && !m_reportDate.before(m_endDate)) {
                    String warningMessage = "Selected Report Date: " + m_reportDate.toString() +
                            " is after context end date: " + m_endDate.toString() +
                            ". Report Date set context end date.";
                    AppGlobals.getLog().log(Level.WARNING, warningMessage);
                } else {
                    m_endDate = m_reportDate;
                }
                m_periodHelper = new TNReportingPeriodHelper(getOrganization(), m_context, null, getBroker());
                addParameter(REPORT_PARAMETER_PERIOD, PERIOD_ANNUAL_AS_OF + m_endDate);
                break;
            // Report Period Mode
            case 1:
                month = (String) getParameter(INPUT_PARAMETER_PERIOD);
                if (!StringUtils.isEmpty(month)) {
                    m_month = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, month);
                } else {
                    String errorMessage = "Report period must be specified";
                    AppGlobals.getLog().log(Level.SEVERE, errorMessage);
                    throw new IllegalArgumentException(errorMessage);
                }
                m_periodHelper = new TNReportingPeriodHelper(getOrganization(), m_context, m_month, getBroker());
                addParameter(REPORT_PARAMETER_PERIOD, m_month.getCode());
                break;
            case 0:
                addParameter(REPORT_PARAMETER_PERIOD, PERIOD_ANNUAL);
                Criteria criteria = new Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, REF_TBL_OID);
                criteria.addNotEqualTo(ReferenceCode.COL_CODE, "00");
                criteria.addNotEqualTo(ReferenceCode.COL_CODE, "99");
                criteria.addEqualTo(ReferenceCode.COL_DISABLED_INDICATOR, BooleanAsStringConverter.FALSE);
                QueryByCriteria byCriteria = new QueryByCriteria(ReferenceCode.class, criteria);
                m_monthCodes = getBroker().getCollectionByQuery(byCriteria);
                m_periodHelper = new TNReportingPeriodHelper(getOrganization(), m_context, m_month, getBroker());
                break;
        }
        if (m_startDate == null) {
            m_startDate = m_context.getStartDate();
        }

        if (m_endDate == null) {
            m_endDate = m_context.getEndDate();
        }

        m_printTotals = (Boolean) getParameter(INPUT_PRINT_TOTALS);

        if (m_printTotals == null || isSchoolContext()) {
            m_printTotals = Boolean.FALSE;
        }
        if (getParameter(INPUT_INCLUDE_BUS_NOT_REPORTED) != null) {
            m_includeNotReported = (Boolean) getParameter(INPUT_INCLUDE_BUS_NOT_REPORTED);
        }
        m_yearStartDate = getCurrentContext().getStartDate();

        m_data = new EnrollmentStatistics();
        m_data.setBroker(getBroker());
        m_data.setOrganization(getOrganization());
        m_data.setPrivilegeSet(getPrivilegeSet());
        m_data.setSchoolContext(isSchoolContext());
        m_data.setSchool(getSchool());
        m_data.setParameters(getParameters());
        m_data.setUser(getUser());
        m_data.initializeExport();

        m_busAMJavaName = m_data.translateAliasToJavaName(ALIAS_AM_BUS, true);
        m_busPMJavaName = m_data.translateAliasToJavaName(ALIAS_PM_BUS, true);
        m_estMilesJavaName = m_data.translateAliasToJavaName(ALIAS_EST_MILES, true);
        m_attendanceReported = m_data.translateAliasToJavaName(ALIAS_ATTENDANCE_REPORTED, true);

        initGrades();
        initBuses();

        addParameter(PARAM_CONTEXT, getCurrentContext());
        addParameter(PARAM_DISTIRCT, getOrganization().getRootOrganization());
        addParameter(PARAM_PRINT_TOTALS, getParameter(INPUT_PRINT_TOTALS));

    }

    /**
     * Returns true if schools collections contains only 1 school.
     *
     * @return true, if is school context
     */
    @Override
    protected boolean isSchoolContext() {
        return m_schools.size() == 1 ? true : false;
    }


    /**
     * Returns true if dates of bus service overlap with dates of school year, otherwise false.
     *
     * @param code ReferenceCode
     * @return boolean
     */
    private boolean datesOverlapWithYear(ReferenceCode code) {
        boolean datesOverlapWithYear = false;

        String startDateDB = (String) code.getFieldValueByAlias(ALIAS_BUS_SERVICE_BEGIN_DATE);
        String endDateDB = (String) code.getFieldValueByAlias(ALIAS_BUS_SERVICE_END_DATE);
        Date serviceStartDate = null;
        Date serviceEndDate = null;
        try {
            if (!StringUtils.isEmpty(startDateDB)) {
                serviceStartDate = m_format.parse(startDateDB);
            }
            if (!StringUtils.isEmpty(endDateDB)) {
                serviceEndDate = m_format.parse(endDateDB);
            }
        } catch (ParseException e) {
            e.printStackTrace();
        }

        PlainDate contextStartDate = getCurrentContext().getStartDate();
        PlainDate contextEndDate = getCurrentContext().getEndDate();

        if (rangesOverlap(serviceStartDate, serviceEndDate, contextStartDate, contextEndDate)) {
            datesOverlapWithYear = true;
        }

        return datesOverlapWithYear;
    }

    /**
     * Define m_startDate and m_endDate as first day of selected month and last day of month for the
     * given school.
     *
     * @param school SisSchool
     */
    private void determineStartEndDates(SisSchool school) {
        boolean error = false;
        String errorMessage = "";

        if (m_periodHelper != null && m_reportMode.intValue() == 1) {
            Collection<PlainDate> daysInPeriods = m_periodHelper.getDaysInSession(school.getOid());
            if (daysInPeriods != null && !CollectionUtils.isEmpty(daysInPeriods)) {
                m_startDate = m_periodHelper.getDateBegin(school.getOid());
                m_endDate = m_periodHelper.getDateEnd(school.getOid());
                addParameter(REPORT_PARAMETER_PERIOD_DAYS, Double.valueOf(daysInPeriods.size()));
            } else {
                errorMessage =
                        "Report can not be generated for school: " + school == null ? "" : school.getName() + ". ";
                error = true;
            }
        } else if (m_periodHelper != null && m_reportMode.intValue() == 2) {
            Collection<PlainDate> daysInPeriods = m_periodHelper.getDaysInSession(school.getOid());
            List<PlainDate> days = new ArrayList<PlainDate>();

            if (daysInPeriods != null && !CollectionUtils.isEmpty(daysInPeriods)) {
                m_startDate = m_periodHelper.getDateBegin(school.getOid());

                for (PlainDate date : daysInPeriods) {
                    if (!date.before(m_startDate) && !date.after(m_endDate)) {
                        days.add(date);
                    }
                }

                addParameter(REPORT_PARAMETER_PERIOD_DAYS, Double.valueOf(days.size()));
            } else {
                errorMessage =
                        "Report can not be generated for school: " + school == null ? "" : school.getName() + ". ";
                error = true;
            }
        } else if (m_periodHelper != null && m_reportMode.intValue() == 0) {
            Collection<PlainDate> daysInPeriods = new HashSet<PlainDate>();
            for (ReferenceCode month : m_monthCodes) {
                m_periodHelper.setMonth(month);
                daysInPeriods.addAll(m_periodHelper.getDaysInSession(school.getOid()));
                m_startDate = m_periodHelper.getDateBegin(school.getOid());
                m_endDate = m_periodHelper.getDateEnd(school.getOid());
            }
            if (daysInPeriods != null && !CollectionUtils.isEmpty(daysInPeriods)) {
                addParameter(REPORT_PARAMETER_PERIOD_DAYS, Double.valueOf(daysInPeriods.size()));
            } else {
                errorMessage =
                        "Report can not be generated for school: " + school == null ? "" : school.getName() + ". ";
                error = true;
            }
        } else {
            error = true;
        }

        if (error) {
            errorMessage = "Report can not be generated. Found zero days for period: "
                    + (m_month == null ? PERIOD_ANNUAL + "." : m_month.getCode()) + ". ";
            AppGlobals.getLog().log(Level.SEVERE, errorMessage);
            throw new IllegalArgumentException(errorMessage);
        }
    }

    /**
     * Extract data into grid from countsTable.
     *
     * @param grid ReportDataGrid
     * @param countsTable Map<String,Map>
     * @param school SisSchool
     */
    private void expandToGrid(ReportDataGrid grid, Map<String, Map> countsTable, SisSchool school) {
        SortedSet<String> keys = new TreeSet<String>();
        keys.addAll(countsTable.keySet());
        for (String key : keys) {
            String stateBusNumber = m_busCodes.containsKey(key) ? m_busCodes.get(key).getStateCode() : null;
            grid.append(countsTable.get(key));
            grid.set(FIELD_BUS_NUMBER, stateBusNumber != null ? stateBusNumber : "null" + " (" + key + ")");
            grid.set(FIELD_SCHOOL, school);
            grid.set(FIELD_SCHOOL_NAME, school == null ? null : school.getName());
        }
    }

    /**
     * Get absences for a student.
     *
     * @param studentOid String
     * @param schoolOid String
     * @return Sets the
     */
    private Set<PlainDate> getAbsences(String studentOid, String schoolOid) {
        Set<PlainDate> absences = new HashSet();

        List<StudentAttendance> attendances = m_data.getStdHelper().getStudentAttendances(studentOid);
        if (attendances == null) {
            absences = m_emptySet;
        } else {
            for (StudentAttendance attendance : attendances) {
                String status = (String) attendance.getFieldValueByBeanPath(m_attendanceReported);
                String reportedStatus = m_data.lookupStateValue(attendance.getClass(), m_attendanceReported, status);

                if (attendance.getSchoolOid().equals(schoolOid)
                        && ("A".equals(reportedStatus) || "U".equals(reportedStatus))) {
                    absences.add(attendance.getDate());
                }
            }
        }
        return absences;
    }

    /**
     * Group studentTransportations by busNumberJavaName field value.
     *
     * @param studentTransportations List<StudentTransportation>
     * @param busNumberJavaName String
     * @return Map
     */
    private Map<String, Set<StudentTransportation>> getCollectionGroupedBy(List<StudentTransportation> studentTransportations,
                                                                           String busNumberJavaName) {
        Map<String, Set<StudentTransportation>> groupedTransportations = new HashMap();
        for (StudentTransportation trs : studentTransportations) {
            String busNumber = (String) trs.getFieldValueByBeanPath(busNumberJavaName);
            if (StringUtils.isEmpty(busNumber) || !m_buses.keySet().contains(busNumber)) {
                continue;
            }

            Set transportations = groupedTransportations.get(busNumber);
            if (null == transportations) {
                transportations = new HashSet();
                groupedTransportations.put(busNumber, transportations);
            }
            transportations.add(trs);
        }
        return groupedTransportations;
    }

    /**
     * Get year to date calendar days.
     *
     * @param school SisSchool
     * @param calendarCode String
     * @return Sets the
     */
    private Set<PlainDate> getDaysYTD(SisSchool school, String calendarCode) {
        String key = school.getOid() + calendarCode;
        if (!m_days.containsKey(key)) {
            Set<PlainDate> calendarDays = m_data.getStdHelper().getCalendarDays(school, calendarCode);
            Set<PlainDate> resultDays = new HashSet();
            if (calendarDays == null || calendarDays.isEmpty()) {
                calendarDays = m_data.getStdHelper().getCalendarDays(school, "Standard");
            }
            if (calendarDays != null) {
                for (PlainDate day : calendarDays) {
                    if (isDayBetween(day, m_startDate, m_endDate)) {
                        resultDays.add(day);
                    }
                }
            }
            m_days.put(key, resultDays);
        }

        return m_days.get(key);
    }

    /**
     * Returns a set of days-in-session for the given school and calendar ID combination.
     *
     * @param school SisSchool
     * @param calendar String
     * @return Set of PlainDate objects
     */
    private PlainDate getFirstCalendarDay(SisSchool school, String calendar) {
        PlainDate firstDate = null;
        Criteria criteria = new Criteria();
        criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_SCHOOL_OID,
                school.getOid());
        criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                getCurrentContext().getOid());
        criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_CALENDAR_ID, calendar);
        criteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);

        QueryByCriteria calendarQuery = new QueryByCriteria(SisSchoolCalendarDate.class, criteria);
        calendarQuery.addOrderByAscending(SisSchoolCalendarDate.COL_DATE);
        QueryIterator calendars = null;
        try {
            calendars = getBroker().getIteratorByQuery(calendarQuery);
            if (calendars.hasNext()) {
                SisSchoolCalendarDate calendarDate = (SisSchoolCalendarDate) calendars.next();
                firstDate = calendarDate.getDate();
            }
        } finally {
            if (calendars != null) {
                calendars.close();
            }
        }
        return firstDate;
    }

    /**
     * Determine the enrollment date that should be used as the start date for
     * this student. The default case is the later of the first in-session date for the school
     * and the span first active date.
     *
     * @param student SisStudent
     * @param span TNStudentEnrollmentSpan
     * @return PlainDate
     */
    private PlainDate getFirstInSessionDay(SisStudent student, TNStudentEnrollmentSpan span) {
        String calendarCode = m_data.getStdCalendarId(student);
        String key = calendarCode + span.getSchool().getOid();
        if (m_firstDatesForSchools == null) {
            m_firstDatesForSchools = new HashMap<String, PlainDate>();
        }
        PlainDate firstInSessionDate = null;
        if (m_firstDatesForSchools.containsKey(key)) {
            firstInSessionDate = m_firstDatesForSchools.get(key);
        } else {
            firstInSessionDate = getFirstCalendarDay(span.getSchool(), calendarCode);
            if (firstInSessionDate == null) {
                // try to use most common calendar
                if (span.getSchool().getActiveSchedule() != null) {
                    if (m_scheduleMgr == null) {
                        m_scheduleMgr = new ScheduleManager(getBroker());
                    }
                    calendarCode = m_scheduleMgr.getMostCommonCalendar(span.getSchool().getActiveSchedule(), null);
                    firstInSessionDate = getFirstCalendarDay(span.getSchool(), calendarCode);
                }
            }
            if (firstInSessionDate == null) {
                firstInSessionDate = m_context.getStartDate();
                AppGlobals.getLog().log(Level.WARNING, "First In-Session date not found for school " +
                        span.getSchool().getName() + " and calendar code " + calendarCode);
            }
            m_firstDatesForSchools.put(key, firstInSessionDate);
        }
        return firstInSessionDate;
    }


    /**
     * Gets the new count.
     *
     * @return Hash map
     */
    private HashMap<String, Number> getNewCount() {
        HashMap<String, Number> busCounts = new HashMap<String, Number>();
        busCounts.put(CONST_ENROLLED_DAYS, CONST_ZERO_DOUBLE);

        busCounts.put(CONST_ENROLLMENT_YTD_AM, CONST_ZERO_DOUBLE);
        busCounts.put(CONST_ENROLLMENT_YTD_PM, CONST_ZERO_DOUBLE);

        busCounts.put(CONST_ENROLLMENT_PER_AM, CONST_ZERO_DOUBLE);
        busCounts.put(CONST_ENROLLMENT_PER_PM, CONST_ZERO_DOUBLE);

        busCounts.put(CONST_ENROLLMENT_MILES_AM, CONST_ZERO_DOUBLE);
        busCounts.put(CONST_ENROLLMENT_MILES_PM, CONST_ZERO_DOUBLE);

        busCounts.put(CONST_ATTENDANCE_AM, CONST_ZERO_DOUBLE);
        busCounts.put(CONST_ATTENDANCE_PM, CONST_ZERO_DOUBLE);

        return busCounts;
    }

    /**
     * Return collection of schools.
     *
     * @return Collection<SisSchool> collection of schools
     */
    private Collection<SisSchool> getSchools() {
        Collection<SisSchool> schools = null;
        Object objIsAllSchools = getParameter(INPUT_PARAM_ALL_SCHOOLS);
        boolean isAllSchools = objIsAllSchools == null ? false : ((Boolean) objIsAllSchools).booleanValue();
        if (isAllSchools) {
            X2Criteria schoolCriteria = new X2Criteria();

            schoolCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            schoolCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

            DataDictionaryField aliasSklStateIDField =
                    m_dictionary.findDataDictionaryFieldByAlias(ALIAS_SKL_STATE_ID);
            schoolCriteria.addNotEmpty(aliasSklStateIDField.getJavaName(), getBroker().getPersistenceKey());

            QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
            schoolQuery.addOrderByAscending(SisSchool.COL_NAME);
            schools = getBroker().getCollectionByQuery(schoolQuery);
        } else {
            schools = new LinkedList();
            Object objSchools = getParameter(INPUT_PARAM_SCHOOLS);
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
     * Get statistic map from school table by bus number.
     *
     * @param countsTable Map<String,Map>
     * @param busNumber String
     * @return Map
     */
    private Map getStatisticMap(Map<String, Map> countsTable, String busNumber) {
        Map busCounts = countsTable.get(busNumber);
        if (busCounts == null) {
            busCounts = getNewCount();
            countsTable.put(busNumber, busCounts);
        }
        return busCounts;
    }

    /**
     * Create query for student.
     *
     * @return Report query by criteria
     */
    private ReportQueryByCriteria getTransportationQuery() {
        X2Criteria transportationCriteria = new X2Criteria();
        List<String> schoolOids = new ArrayList<String>();
        for (SisSchool school : m_schools) {
            schoolOids.add(school.getOid());
        }
        transportationCriteria.addIn(StudentTransportation.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                X2BaseBean.COL_OID, schoolOids);
        // Case 1 strStartDate not null, strEndDate can be null
        X2Criteria dateCriteria = new X2Criteria();
        dateCriteria.addLessOrEqualThan(StudentTransportation.COL_START_DATE, m_endDate);

        X2Criteria orEndDateCriteria = new X2Criteria();
        orEndDateCriteria.addEmpty(StudentTransportation.COL_END_DATE, getBroker().getPersistenceKey());

        X2Criteria endDateCriteria = new X2Criteria();
        endDateCriteria.addGreaterOrEqualThan(StudentTransportation.COL_END_DATE, m_yearStartDate);
        endDateCriteria.addOrCriteria(orEndDateCriteria);

        dateCriteria.addAndCriteria(endDateCriteria);

        // Case 2 strStartDate null, strEndDate not null
        X2Criteria startDateNullCriteria = new X2Criteria();
        startDateNullCriteria.addEmpty(StudentTransportation.COL_START_DATE, getBroker().getPersistenceKey());
        startDateNullCriteria.addGreaterOrEqualThan(StudentTransportation.COL_END_DATE, m_yearStartDate);

        dateCriteria.addOrCriteria(startDateNullCriteria);

        // Case then both strStartDate and strEndDate null not considered
        transportationCriteria.addAndCriteria(dateCriteria);

        DataDictionaryField amBusField = m_data.getDataDictionaryField(StudentTransportation.class, m_busAMJavaName);
        DataDictionaryField pmBusField = m_data.getDataDictionaryField(StudentTransportation.class, m_busPMJavaName);
        X2Criteria busCodesCriteria = new X2Criteria();
        Collection<String> refTablesOIds = new ArrayList<String>();
        if (amBusField.hasReferenceTable()) {
            refTablesOIds.add(amBusField.getReferenceTableOid());
        }
        if (pmBusField.hasReferenceTable() && !refTablesOIds.contains(pmBusField.getReferenceTableOid())) {
            refTablesOIds.add(pmBusField.getReferenceTableOid());
        }
        busCodesCriteria.addIn(ReferenceCode.COL_REFERENCE_TABLE_OID, refTablesOIds);
        if (!m_includeNotReported.booleanValue()) {
            busCodesCriteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
        } else {
            busCodesCriteria.addNotEmpty(ReferenceCode.COL_CODE, getBroker().getPersistenceKey());
        }
        String busCodes = (String) getParameter(INPUT_PARAM_BUS_NUMBERS);
        if (!StringUtils.isEmpty(busCodes)) {
            busCodesCriteria.addIn(X2BaseBean.COL_OID, Arrays.asList((busCodes.split(","))));
        }
        QueryByCriteria codesQuery = new QueryByCriteria(ReferenceCode.class, busCodesCriteria);
        m_busCodes = getBroker().getMapByQuery(codesQuery, ReferenceCode.COL_CODE, 256);
        Collection<String> availableCodes = m_busCodes.keySet();
        // load only transportations with bus
        X2Criteria busTransCriteria = new X2Criteria();
        busTransCriteria.addNotEmpty(m_busAMJavaName, getBroker().getPersistenceKey());
        busTransCriteria.addIn(m_busAMJavaName, availableCodes);
        X2Criteria orBusTransCriteria = new X2Criteria();
        orBusTransCriteria.addNotEmpty(m_busPMJavaName, getBroker().getPersistenceKey());
        orBusTransCriteria.addIn(m_busPMJavaName, availableCodes);
        busTransCriteria.addOrCriteria(orBusTransCriteria);
        transportationCriteria.addAndCriteria(busTransCriteria);

        // student grade must be k12.
        X2Criteria studentCriteria = m_data.getStudentCriteria();
        m_data.adjustCriteria(studentCriteria, Strategy.IN, Student.COL_GRADE_LEVEL, m_k12grades);

        SubQuery studentQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        transportationCriteria.addIn(StudentTransportation.COL_STUDENT_OID,
                studentQuery);

        return new ReportQueryByCriteria(StudentTransportation.class, transportationCriteria);
    }

    /**
     * Increment counts for bus if necessary.
     *
     * @param transportations Set<StudentTransportation>
     * @param days Set<PlainDate>
     * @param spans List<TNStudentEnrollmentSpan>
     * @param absences Set<PlainDate>
     * @param isMilesMoreThen Boolean
     * @param isAmBus true if AM bus, false if PM bus
     * @param isBusSpecial true if the bus is special equipped bus
     * @return HashMap
     */
    private HashMap<String, Number> incBusCounts(SisStudent student,
                                                 Set<StudentTransportation> transportations,
                                                 Set<PlainDate> days,
                                                 List<TNStudentEnrollmentSpan> spans,
                                                 Set<PlainDate> absences,
                                                 Boolean isMilesMoreThen,
                                                 boolean isAmBus,
                                                 boolean isBusSpecial,
                                                 String sklOid) {
        HashMap<String, Number> busCounts = getNewCount();
        String enrollmentYtd = isAmBus ? CONST_ENROLLMENT_YTD_AM : CONST_ENROLLMENT_YTD_PM;
        String enrollmentPer = isAmBus ? CONST_ENROLLMENT_PER_AM : CONST_ENROLLMENT_PER_PM;
        String enrollmentMiles = isAmBus ? CONST_ENROLLMENT_MILES_AM : CONST_ENROLLMENT_MILES_PM;
        String attendance = isAmBus ? CONST_ATTENDANCE_AM : CONST_ATTENDANCE_PM;
        int daysInAttendance = 0;
        int periodDays = 0;
        boolean isATTEnrolled = false;
        boolean isYTDEnrolled = false;
        boolean isWithdrewFirstInSession = isWithdrewFirstInSessionDay(student, spans);
        boolean isYTDTransported = false;
        for (PlainDate day : days) {
            boolean isEnrolledDay =
                    isDayInSpans(day, spans, sklOid) && !isWithdrewFirstInSessionDay(student, spans, day);
            if (isEnrolledDay) {
                incValue(busCounts, CONST_ENROLLED_DAYS, 0.5);
            }

            boolean isTransportedDay = isEnrolledDay && isDayInTransportations(day, transportations);
            if (isTransportedDay) {
                periodDays++;
            }

            boolean inAttendance = isTransportedDay && isEnrolledDay && !absences.contains(day);
            if (inAttendance) {
                daysInAttendance++;
            }
        }
        for (StudentTransportation str : transportations) {
            if (!str.getStartDate().after(m_endDate)) {
                isYTDTransported = true;
                break;
            }
        }
        String schoolOid = transportations.isEmpty() ? "" : transportations.iterator().next().getSchoolOid();
        for (TNStudentEnrollmentSpan span : spans) {
            if (!span.getFirstActiveDate().after(m_endDate) && schoolOid.equals(span.getSchool().getOid())) {
                isATTEnrolled = true;
                break;
            }
        }
        for (TNStudentEnrollmentSpan span : spans) {
            if (!span.getFirstActiveDate().after(m_endDate) && schoolOid.equals(span.getSchool().getOid())
                    && (span.getLastActiveDate() == null || !span.getLastActiveDate().before(m_endDate))) {
                isYTDEnrolled = true;
                break;
            }
        }

        if (isYTDEnrolled && isYTDTransported && !isWithdrewFirstInSessionDay(student, spans, null, sklOid)) {
            incValue(busCounts, enrollmentYtd);
        }
        if (periodDays != 0 && !isWithdrewFirstInSession) {
            incValue(busCounts, enrollmentPer);
            if (isMilesMoreThen.booleanValue() || isBusSpecial) {
                incValue(busCounts, enrollmentMiles);
            }
        }
        if (isATTEnrolled && periodDays != 0 && daysInAttendance != 0
                && (isMilesMoreThen.booleanValue() || isBusSpecial)) {
            incValue(busCounts, attendance, 0.5 * daysInAttendance);
        }
        return busCounts;
    }

    /**
     * Checks if is withdrew first in session day.
     *
     * @param student SisStudent
     * @param spans List<TNStudentEnrollmentSpan>
     * @return true, if is withdrew first in session day
     */
    private boolean isWithdrewFirstInSessionDay(SisStudent student,
                                                List<TNStudentEnrollmentSpan> spans) {
        return spans.stream().allMatch(span -> {
            if (span.getFirstInactiveEnrollment() != null
                    && span.getFirstInactiveEnrollment().getEnrollmentDate() != null) {
                String calendarCode = m_data.getStdCalendarId(student);
                if (!StringUtils.isEmpty(calendarCode)) {
                    PlainDate firstCalDate = getFirstInSessionDay(student, span);
                    if (firstCalDate != null
                            && firstCalDate.equals(span.getFirstInactiveEnrollment().getEnrollmentDate())) {
                        return true;
                    }
                }
            }
            return false;
        });
    }

    /**
     * Checks if is withdrew first in session day.
     *
     * @param student SisStudent
     * @param spans List<TNStudentEnrollmentSpan>
     * @param day PlainDate
     * @return true, if is withdrew first in session day
     */
    private boolean isWithdrewFirstInSessionDay(SisStudent student,
                                                List<TNStudentEnrollmentSpan> spans,
                                                PlainDate day) {
        for (TNStudentEnrollmentSpan span : spans) {
            if (span.getFirstInactiveEnrollment() != null
                    && span.getFirstInactiveEnrollment().getEnrollmentDate() != null) {
                String calendarCode = m_data.getStdCalendarId(student);
                if (!StringUtils.isEmpty(calendarCode)) {
                    PlainDate firstCalDate = getFirstInSessionDay(student, span);
                    if (firstCalDate != null
                            && firstCalDate.equals(span.getFirstInactiveEnrollment().getEnrollmentDate())
                            && (day == null || day.equals(span.getFirstInactiveEnrollment().getEnrollmentDate()))) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /**
     * Checks if is withdrew first in session day.
     *
     * @param student SisStudent
     * @param spans List<TNStudentEnrollmentSpan>
     * @param day PlainDate
     * @param sklOid String
     * @return true, if is withdrew first in session day
     */
    private boolean isWithdrewFirstInSessionDay(SisStudent student,
                                                List<TNStudentEnrollmentSpan> spans,
                                                PlainDate day,
                                                String sklOid) {
        for (TNStudentEnrollmentSpan span : spans) {
            if (span.getSchool().getOid().equals(sklOid) && span.getFirstInactiveEnrollment() != null
                    && span.getFirstInactiveEnrollment().getEnrollmentDate() != null) {
                String calendarCode = m_data.getStdCalendarId(student);
                if (!StringUtils.isEmpty(calendarCode)) {
                    PlainDate firstCalDate = getFirstInSessionDay(student, span);
                    if (firstCalDate != null
                            && firstCalDate.equals(span.getFirstInactiveEnrollment().getEnrollmentDate())
                            && (day == null || day.equals(span.getFirstInactiveEnrollment().getEnrollmentDate()))) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /**
     * Increment counts for the detail grid.
     *
     * @param busCounts HashMap<String,Number>
     * @param busCountsFromMap HashMap<String,Number>
     */
    private void incBusCountsFromMap(HashMap<String, Number> busCounts, HashMap<String, Number> busCountsFromMap) {
        for (String key : busCounts.keySet()) {
            Number value = busCountsFromMap.get(key);
            double result = value != null ? value.doubleValue() + busCounts.get(key).doubleValue()
                    : CONST_ZERO_DOUBLE.doubleValue() + busCounts.get(key).doubleValue();
            busCountsFromMap.put(key, Double.valueOf(result));
        }
    }

    /**
     * Inc org counts.
     *
     * @param orgTable Map<String,Map>
     * @param busCounts Map<String,Number>
     * @param busNumber String
     */
    private void incOrgCounts(Map<String, Map> orgTable, Map<String, Number> busCounts, String busNumber) {
        Map<String, Number> busOrgCounts = getStatisticMap(orgTable, busNumber);
        for (String column : busCounts.keySet()) {
            Number orgValue = busOrgCounts.get(column);
            Number busValue = busCounts.get(column);

            if (orgValue instanceof Integer) {
                busOrgCounts.put(column, Integer.valueOf(orgValue.intValue() + busValue.intValue()));
            } else {
                busOrgCounts.put(column, Double.valueOf(orgValue.doubleValue() + busValue.doubleValue()));
            }
        }
    }

    /**
     * Increase the value by 0.5 for AM or PM.
     *
     * @param busCounts Map<String,Number>
     * @param enrollmentYtd String
     */
    private void incValue(Map<String, Number> busCounts, String enrollmentYtd) {
        Double value = (Double) busCounts.get(enrollmentYtd);
        double result = value == null ? 0.5 : 0.5 + value.doubleValue();
        busCounts.put(enrollmentYtd, Double.valueOf(result));
    }

    /**
     * Increase the value by incValue.
     *
     * @param busCounts Map<String,Number>
     * @param attendance String
     * @param incValue double
     */
    private void incValue(Map<String, Number> busCounts, String attendance, double incValue) {
        Double value = (Double) busCounts.get(attendance);
        double result = value == null ? incValue : incValue + value.doubleValue();
        busCounts.put(attendance, Double.valueOf(result));
    }

    /**
     * Inits the buses.
     */
    private void initBuses() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, EXTENDED_DICTIONARY_ID_BUS);
        ExtendedDataDictionary extendedDictionary = (ExtendedDataDictionary) getBroker()
                .getBeanByQuery(new QueryByCriteria(ExtendedDataDictionary.class, criteria));

        List<ReferenceCode> busCodes = (List<ReferenceCode>) extendedDictionary.getReferenceCodes(getBroker());
        for (ReferenceCode busCode : busCodes) {
            boolean includeBus = m_includeNotReported.booleanValue()
                    || (!m_includeNotReported.booleanValue() && !StringUtils.isEmpty(busCode.getStateCode()));
            if (includeBus && datesOverlapWithYear(busCode)) {
                m_buses.put(busCode.getCode(), busCode);
            }
        }
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extendedDictionary, getBroker().getPersistenceKey());
        DataDictionaryField isBusSpeciallyEquippedField =
                dictionary.findDataDictionaryFieldByAlias(ALIAS_SPECIALL_EQUIPPED);
        m_isBusSpeciallyEquipped = isBusSpeciallyEquippedField != null ? isBusSpeciallyEquippedField.getJavaName() : "";
    }

    /**
     * Prepare list of grades that need to include in report.
     * List is based on grade numeric levels defined in input.
     */
    private void initGrades() {
        m_k12grades = new ArrayList<String>();
        char delimiter = ((String) getParameter(INPUT_DELIMITER)).charAt(0);
        List gradesNumeric =
                StringUtils.convertDelimitedStringToList((String) getParameter(INPUT_K12_GRADES), delimiter,
                        true);

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, EXTENDED_DICTIONARY_ID_GRADE_NUMERIC);
        ExtendedDataDictionary extendedDictionary = (ExtendedDataDictionary) getBroker()
                .getBeanByQuery(new QueryByCriteria(ExtendedDataDictionary.class, criteria));

        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extendedDictionary, getBroker().getPersistenceKey());
        DataDictionaryField gradeLevelField = dictionary.findDataDictionaryField(Student.class.getName(),
                Student.COL_GRADE_LEVEL);

        DataDictionaryField numericGradeLevelField = dictionary.findDataDictionaryFieldByAlias(ALIAS_NUMERIC_GRADE);
        if (gradeLevelField.hasReferenceTable()) {
            for (ReferenceCode code : gradeLevelField.getReferenceTable().getReferenceCodes(getBroker())) {
                String numericLevel = (String) code.getFieldValueByBeanPath(numericGradeLevelField.getJavaName());
                if (!StringUtils.isEmpty(numericLevel) && gradesNumeric.contains(numericLevel)) {
                    m_k12grades.add(code.getCode());
                }
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
        m_useDetail = false;
        switch (job.getInput().getFormat()) {
            case ToolInput.CSV_FORMAT:
                m_useDetail = true;
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
     * Check special field on bus reference code.
     *
     * @param busNumber String
     * @return true, if is bus special
     */
    private boolean isBusSpecial(String busNumber) {
        boolean result = false;
        ReferenceCode bus = m_buses.get(busNumber);
        if (bus != null) {
            try {
                result = BooleanAsStringConverter.TRUE.equals(bus.getFieldValueByBeanPath(m_isBusSpeciallyEquipped));
            } catch (Exception ex) {
                result = false;
            }
        }

        return result;
    }

    /**
     * Check if the day is between the start and end.
     *
     * @param day PlainDate
     * @param start PlainDate
     * @param end PlainDate
     * @return true, if is day between
     */
    private boolean isDayBetween(PlainDate day, PlainDate start, PlainDate end) {
        boolean bool = false;
        if ((start != null && !day.before(start) && (end == null || !day.after(end)))
                || (start == null && end != null && !day.after(end))) {
            bool = true;
        }
        return bool;
    }

    /**
     * Check if the day includes in spans.
     *
     * @param day PlainDate
     * @param spans List<TNStudentEnrollmentSpan>
     * @return true, if is day in spans
     */
    private boolean isDayInSpans(PlainDate day,
                                 List<TNStudentEnrollmentSpan> spans,
                                 String sklOid) {
        boolean bool = false;
        for (TNStudentEnrollmentSpan span : spans) {
            PlainDate startSpanDate = span.getFirstActiveDate();
            PlainDate endSpanDate = span.getLastActiveDate();
            if (span.getSchool().getOid().equals(sklOid) && isDayBetween(day, startSpanDate, endSpanDate)) {
                bool = true;
                break;
            }
        }
        return bool;
    }

    /**
     * Check if the day is between the start and the end of transportations.
     *
     * @param day PlainDate
     * @param transportations Collection<StudentTransportation>
     * @return true, if is day in transportations
     */
    private boolean isDayInTransportations(PlainDate day, Collection<StudentTransportation> transportations) {
        boolean bool = false;
        for (StudentTransportation transportation : transportations) {
            PlainDate startDate = transportation.getStartDate();
            PlainDate endDate = transportation.getEndDate();

            if (isDayBetween(day, startDate, endDate)) {
                bool = true;
                break;
            }
        }
        return bool;
    }

    /**
     * Check if transportation has miles more or equal than CONST_MILE_LIMIT.
     *
     * @param transportations Set<StudentTransportation>
     * @return Boolean
     */
    private Boolean isMilesMoreThen(Set<StudentTransportation> transportations) {
        Boolean bool = Boolean.FALSE;
        for (StudentTransportation transportation : transportations) {
            String value = (String) transportation.getFieldValueByBeanPath(m_estMilesJavaName);
            if (StringUtils.isNumeric(value) && Double.parseDouble(value) >= CONST_MILES_LIMIT) {
                bool = Boolean.TRUE;
                break;
            }
        }
        return bool;
    }


    /**
     * Iterate founded studentTransportation.
     *
     * @param grid ReportDataGrid
     * @param detailGrid ReportDataGrid
     */
    private void iterateTransportations(ReportDataGrid grid, ReportDataGrid detailGrid) {
        Map<String, Map<String, List<StudentTransportation>>> transportationsBySchool = null;
        String[] columnKeys = new String[] {StudentTransportation.COL_SCHOOL_OID,
                StudentTransportation.COL_STUDENT_OID};
        transportationsBySchool = getBroker().getGroupedCollectionByQuery(getTransportationQuery(),
                columnKeys, new int[] {10, 200});
        if (m_schools.size() == 0) {
            transportationsBySchool = new HashMap<String, Map<String, List<StudentTransportation>>>();
        }
        Map districtTable = new HashMap<String, Map>();
        Collection<String> availableCodes = m_busCodes.keySet();
        for (Map.Entry<String, Map<String, List<StudentTransportation>>> itemBySchool : transportationsBySchool
                .entrySet()) {
            String sklOid = itemBySchool.getKey();
            Map<String, List<StudentTransportation>> transportationsByStudent = itemBySchool.getValue();
            SisSchool school = (SisSchool) getBroker().getBeanByOid(SisSchool.class, sklOid);
            Map schoolTable = new HashMap<String, Map>();
            determineStartEndDates(school);

            for (Map.Entry<String, List<StudentTransportation>> itemByStudent : transportationsByStudent.entrySet()) {
                String stdOid = itemByStudent.getKey();

                SisStudent student = (SisStudent) getBroker().getBeanByOid(SisStudent.class, stdOid);

                String calendarId = m_data.getStudentCalendarId(student);
                Set<PlainDate> daysYTD = getDaysYTD(school, calendarId);

                List<TNStudentEnrollmentSpan> spans = m_data.getStudentEnrollmentSpans(student, true);
                if (m_month != null) {
                    boolean skipRecord = true;
                    for (TNStudentEnrollmentSpan span : spans) {
                        if (span.getLastActiveDate() == null || !span.getLastActiveDate().before(m_endDate)) {
                            skipRecord = false;
                        }
                    }
                    if (skipRecord) {
                        continue;
                    }
                }
                Set<PlainDate> absences = getAbsences(stdOid, sklOid);

                HashMap<String, HashMap<String, Number>> busMap = new HashMap<String, HashMap<String, Number>>();
                HashMap<String, Number> busCountsfromMap = null;

                makeStrsNotOverlapped(itemByStudent.getValue());

                // AM bus
                Map<String, Set<StudentTransportation>> transportationsByBusAM =
                        getCollectionGroupedBy(itemByStudent.getValue(),
                                m_busAMJavaName);

                for (Map.Entry<String, Set<StudentTransportation>> entry : transportationsByBusAM.entrySet()) {
                    String busNumber = entry.getKey();
                    if (availableCodes.contains(busNumber)) {
                        boolean isBusSpecial = isBusSpecial(busNumber);
                        Set<StudentTransportation> transportations = entry.getValue();

                        Boolean isMilesMoreThen = isMilesMoreThen(transportations);
                        HashMap<String, Number> busCounts =
                                incBusCounts(student, transportations, daysYTD, spans, absences,
                                        isMilesMoreThen, true, isBusSpecial, sklOid);

                        if (busMap.containsKey(busNumber)) {
                            busCountsfromMap = busMap.get(busNumber);
                        } else {
                            busCountsfromMap = new HashMap<String, Number>();
                            busMap.put(busNumber, busCountsfromMap);
                        }

                        incBusCountsFromMap(busCounts, busCountsfromMap);

                        if (detailGrid == null) {
                            detailGrid = new ReportDataGrid();
                        }

                        incOrgCounts(schoolTable, busCounts, busNumber);

                        if (m_printTotals.booleanValue()) {
                            incOrgCounts(districtTable, busCounts, busNumber);
                        }
                    }
                }

                // PM bus
                Map<String, Set<StudentTransportation>> transportationsByBusPM =
                        getCollectionGroupedBy(itemByStudent.getValue(),
                                m_busPMJavaName);
                for (Map.Entry<String, Set<StudentTransportation>> entry : transportationsByBusPM.entrySet()) {
                    String busNumber = entry.getKey();
                    if (availableCodes.contains(busNumber)) {
                        boolean isBusSpecial = isBusSpecial(busNumber);
                        Set<StudentTransportation> transportations = entry.getValue();

                        Boolean isMilesMoreThen = isMilesMoreThen(transportations);
                        HashMap<String, Number> busCounts =
                                incBusCounts(student, transportations, daysYTD, spans, absences,
                                        isMilesMoreThen, false, isBusSpecial, sklOid);

                        if (busMap.containsKey(busNumber)) {
                            busCountsfromMap = busMap.get(busNumber);
                        } else {
                            busCountsfromMap = new HashMap<String, Number>();
                            busMap.put(busNumber, busCountsfromMap);
                        }

                        incBusCountsFromMap(busCounts, busCountsfromMap);

                        if (detailGrid == null) {
                            detailGrid = new ReportDataGrid();
                        }

                        incOrgCounts(schoolTable, busCounts, busNumber);

                        if (m_printTotals.booleanValue()) {
                            incOrgCounts(districtTable, busCounts, busNumber);
                        }
                    }
                }

                populateDetailGrid(detailGrid, student, school, busMap, daysYTD.size());
            }
            expandToGrid(grid, schoolTable, school);
        }
        if (m_printTotals.booleanValue()) {
            expandToGrid(grid, districtTable, null);
        }
    }

    /**
     * Make strs not overlapped.
     *
     * @param strList List<StudentTransportation>
     */
    private void makeStrsNotOverlapped(List<StudentTransportation> strList) {
        Collections.sort(strList, new Comparator<StudentTransportation>() {
            @Override
            public int compare(StudentTransportation o1, StudentTransportation o2) {
                return o1.getStartDate().compareTo(o2.getStartDate());
            }
        });

        StudentTransportation prevStr = null;

        Iterator<StudentTransportation> iterator = strList.iterator();

        while (iterator.hasNext()) {
            StudentTransportation curStr = iterator.next();

            boolean isFirstStr = prevStr == null;

            if (!isFirstStr) {
                boolean endedDuringPrev = (prevStr.getEndDate() == null ||
                        (curStr.getEndDate() != null && !curStr.getEndDate().after(prevStr.getEndDate())));

                boolean partiallyOverlapped = prevStr.getEndDate() != null &&
                        !prevStr.getEndDate().before(curStr.getStartDate());

                if (endedDuringPrev) {
                    iterator.remove();
                    continue;
                } else if (partiallyOverlapped) {
                    m_calendar.setTime(prevStr.getEndDate());
                    m_calendar.add(Calendar.DATE, 1);
                    curStr.setStartDate(new PlainDate(m_calendar.getTime()));
                }
            }

            prevStr = curStr;
        }
    }

    /**
     * Method populates grid with values needed for CSV version of report.
     *
     * @param detailGrid ReportDataGrid
     * @param student SisStudent
     * @param school SisSchool
     * @param busMap HashMap<String,HashMap<String,Number>>
     * @param calDays int
     */
    private void populateDetailGrid(ReportDataGrid detailGrid,
                                    SisStudent student,
                                    SisSchool school,
                                    HashMap<String, HashMap<String, Number>> busMap,
                                    int calDays) {
        for (String busNumber : busMap.keySet()) {
            detailGrid.append();
            detailGrid.set(DETAIL_REPORT_FIELD_SCHOOL_NAME, school != null ? school.getName() : null);
            detailGrid.set(DETAIL_REPORT_FIELD_STD_NAME, student != null ? student.getNameView() : null);
            detailGrid.set(DETAIL_REPORT_FIELD_STATE_ID,
                    student != null ? student.getFieldValueByAlias(ALIAS_EIS_STATE_ID) : null);
            detailGrid.set(DETAIL_REPORT_FIELD_LOCAL_ID, student != null ? student.getLocalId() : null);
            // In some of the client sites, the code and state code are not the same. The report
            // needs to be based on the state code.
            String stateBusNumber = m_busCodes.containsKey(busNumber) ? m_busCodes.get(busNumber).getStateCode() : null;
            detailGrid.set(DETAIL_REPORT_FIELD_BUS_NUM,
                    !StringUtils.isEmpty(stateBusNumber) ? stateBusNumber : "null" + " (" + busNumber + ")");

            boolean isBusSpecial = isBusSpecial(busNumber);
            detailGrid.set(DETAIL_REPORT_FIELD_IS_BUS_SPEC, isBusSpecial ? "Y" : "N");
            detailGrid.set(DETAIL_REPORT_FIELD_CAL_DAYS, Integer.valueOf(calDays));

            HashMap<String, Number> busCounts = busMap.get(busNumber);

            detailGrid.set(DETAIL_REPORT_FIELD_YTD_ENR_AM, busCounts.get(CONST_ENROLLMENT_YTD_AM));
            detailGrid.set(DETAIL_REPORT_FIELD_YTD_ENR_PM, busCounts.get(CONST_ENROLLMENT_YTD_PM));
            detailGrid.set(DETAIL_REPORT_FIELD_PER_ENR_AM, busCounts.get(CONST_ENROLLMENT_PER_AM));
            detailGrid.set(DETAIL_REPORT_FIELD_PER_ENR_PM, busCounts.get(CONST_ENROLLMENT_PER_PM));
            detailGrid.set(DETAIL_REPORT_FIELD_ATT_AM, busCounts.get(CONST_ATTENDANCE_AM));
            detailGrid.set(DETAIL_REPORT_FIELD_ATT_PM, busCounts.get(CONST_ATTENDANCE_PM));
            detailGrid.set(DETAIL_REPORT_FIELD_ENR_DAYS, busCounts.get(CONST_ENROLLED_DAYS));

            BigDecimal milesAM = new BigDecimal(((Double) busCounts.get(CONST_ENROLLMENT_MILES_AM)).doubleValue());
            BigDecimal milesPM = new BigDecimal(((Double) busCounts.get(CONST_ENROLLMENT_MILES_PM)).doubleValue());

            detailGrid.set(DETAIL_REPORT_FIELD_MILES_TRANS_AM, milesAM);
            detailGrid.set(DETAIL_REPORT_FIELD_MILES_TRANS_PM, milesPM);

        }
    }


    /**
     * Return true if left date pair overlaps with right date pair.
     * Null is considering as unlimited past/future.
     *
     * @param leftStartDate Date
     * @param leftEndDate Date
     * @param rightStartDate Date
     * @param rightEndDate Date
     * @return boolean
     */
    private boolean rangesOverlap(Date leftStartDate,
                                  Date leftEndDate,
                                  Date rightStartDate,
                                  Date rightEndDate) {
        boolean rangesOverlap = false;

        boolean correctDates = ((leftStartDate == null || leftEndDate == null) || !leftStartDate.after(leftEndDate)) &&
                ((rightStartDate == null || rightEndDate == null) || !rightStartDate.after(rightEndDate));

        // Both ranges have unlimited past
        if (correctDates &&
                ((leftStartDate == null && rightStartDate == null) ||
                // both ranges have unlimited future
                        (leftEndDate == null && rightEndDate == null) ||
                        // left range has unlimited past and future
                        (leftStartDate == null && leftEndDate == null) ||
                        // right range has unlimited past and future
                        (rightStartDate == null && rightEndDate == null) ||

                        // to this point if range has unlimited past (null), it has limited future
                        // and second range has limited past (not nulls),
                        // and if range has unlimited future (null), it has limited past and second
                        // range has limited future (not nulls), so
                        // we can check if ranges are overlap
                        (leftEndDate == null && !leftStartDate.after(rightEndDate)) ||
                        (rightEndDate == null && !rightStartDate.after(leftEndDate)) ||
                        (leftStartDate == null && !leftEndDate.before(rightStartDate)) ||
                        (rightStartDate == null && !rightEndDate.before(leftStartDate)) ||

                        (leftStartDate != null && leftEndDate != null &&
                                rightStartDate != null && rightEndDate != null &&
                                !leftStartDate.after(rightEndDate) && !leftEndDate.before(rightStartDate)))) {
            rangesOverlap = true;
        }

        return rangesOverlap;
    }

}
