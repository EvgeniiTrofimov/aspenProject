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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.procedures.statereporting.tn.TNClassSectionHelper;
import com.x2dev.procedures.statereporting.tn.TNClassSectionScheduleData;
import com.x2dev.procedures.statereporting.tn.TNClassSectionScheduleData.TNClassSectionScheduleEntity;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentEnrollmentSpan;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentScheduleSpan;
import com.x2dev.procedures.statereporting.tn.TNStateReportData;
import com.x2dev.procedures.statereporting.tn.TNStateReportData.Pair;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.util.*;
import java.util.logging.Level;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.Query;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data sources for the "Vocational Class" reports:
 * TN Annual Vocational Class Full Time Equivalent Average Daily Membership Report
 * TN Monthly Vocational Class Full Time Equivalent Average Daily Attendance Report
 * TN Monthly Vocational Class Full Time Equivalent Average Daily Membership Report
 *
 * VoC (VOC) stands for Vocational Class
 * FTEADM stands for Full time Equivalent Average Daily Membership
 * FTEADA stands for Full time Equivalent Average Daily Attendance
 *
 * NOTE: most of the code to calculate the student schedule minutes are copied from
 * TNClassSectionScheduleData.java. Any change there or here might need to be
 * applied to both files.
 *
 * @author X2 Development Corporation
 */
public class VocationalClassReportData extends ReportJavaSourceNet {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * The Enum REPORT_TYPE.
     */
    enum REPORT_TYPE {
        MEMBERSHIP, ATTENDANCE
    }

    /**
     * The Class EnrollmentStatistics.
     */
    class EnrollmentStatistics extends TNStateReportData {
        protected TNEnrollmentHelper m_tnEnrHelper;

        private TNStudentHistoryHelper m_tnStudentHelper;

        /**
         * Close the student history helper.
         *
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#close()
         */
        @Override
        public void close() {
            super.close();
            if (m_tnStudentHelper != null) {
                m_tnStudentHelper.close();
            }
        }

        /**
         * Return the list of student schedule spans.
         *
         * @param student SisStudent
         * @return List<StudentScheduleSpan>
         */
        public List<TNStudentScheduleSpan> getTNStudentScheduleSpans(SisStudent student) {
            List<TNStudentScheduleSpan> schedulesSpans = m_tnStudentHelper.getTNStudentScheduleSpans(student);
            Iterator iterator = schedulesSpans.iterator();
            while (iterator.hasNext()) {
                TNStudentScheduleSpan scheduleSpan = (TNStudentScheduleSpan) iterator.next();
                MasterSchedule section = scheduleSpan.getSection();
                if (section.getSchoolCourse() != null && section.getSchoolCourse().getCourse() != null) {
                    String classType = (String) section.getSchoolCourse().getCourse()
                            .getFieldValueByAlias(ALIAS_COURSE_CLASS_TYPE);
                    if (PULLOUT_CLASS_TYPE.equals(classType)) {
                        iterator.remove();
                    }
                }
            }

            return schedulesSpans;
        }

        /**
         * Return calendar days.
         *
         * @param school SisSchool
         * @param calendar String
         * @return Set<PlainDate>
         */
        public Set<PlainDate> getCalendarDays(SisSchool school, String calendar) {
            return m_tnStudentHelper.getCalendarDays(school, calendar);
        }

        /**
         * Returns grade level reference code that had student for date range.
         *
         * @param student SisStudent
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @return Reference code
         */
        public ReferenceCode getGradeLevelByDates(SisStudent student, PlainDate startDate, PlainDate endDate) {
            return m_tnStudentHelper.getGradeLevelByDates(student, startDate, endDate);
        }

        /**
         * Get student schedule change end date.
         *
         * @return Plain date
         */
        public PlainDate getSpanEndDate() {
            return m_spanEndDate;
        }

        /**
         * Get student schedule change start date.
         *
         * @return Plain date
         */
        public PlainDate getSpanStartDate() {
            return m_spanStartDate;
        }

        /**
         * Return student attendances.
         *
         * @param studentOid String
         * @return List<StudentAttendance>
         */
        public List<StudentAttendance> getStudentAttendances(String studentOid) {
            return m_tnStudentHelper.getStudentAttendances(studentOid);
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
         * Gets the student oids.
         *
         * @return Collection
         */
        public Collection<String> getStudentOids() {
            return getBroker().getCollectionByQuery(getStudentQuery(false));
        }

        /**
         * Returns a list of student enrollment spans that represent all of the students enrollment
         * activity and segments.
         *
         * @param student Student
         * @param limit boolean
         * @return List<StudentEnrollmentSpan>
         */
        public List<TNStudentEnrollmentSpan> getTNStudentEnrollmentSpans(Student student, boolean limit) {
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

            m_tnStudentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_context.getStartDate());
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_context.getEndDate());

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_ATTENDANCE, Boolean.FALSE);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SCHEDULE_SPANS, Boolean.TRUE);

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.TRUE);

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_MATERIALIZED_STUDENT_SELECTION,
                    Boolean.TRUE);


            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_COURSE_VOC);
            X2Criteria scheduleCriteria = m_tnStudentHelper.getStudentScheduleCriteria();
            scheduleCriteria.addNotEmpty(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER + field.getJavaName(), getBroker().getPersistenceKey());

            X2Criteria changeCriteria = m_tnStudentHelper.getStudentScheduleChangeCriteria();
            changeCriteria.addNotEmpty(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER + field.getJavaName(), getBroker().getPersistenceKey());
        }
    }

    /**
     * Aliases
     */
    private static final String ALIAS_COURSE_CLASS_TYPE = "DOE CLASS TYPE";
    private static final String ALIAS_COURSE_SDE_CODE = "DOE SDE COURSE CODE";
    private static final String ALIAS_COURSE_SERVICE_DISTRICT_ID = "DOE CRS SERVICE DISTRICT ID";
    private static final String ALIAS_COURSE_VOC = "DOE VOC CLASSIFICATION";
    private static final String ALIAS_EIS_STATE_ID = "DOE EIS STATE ID";
    private static final String ALIAS_INSTR_PGM = "DOE INSTRUCTIONAL PROGRAM";
    private static final String ALIAS_INSTR_SERVICE_TYPE = "DOE INSTR SERVICE TYPE";
    private static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL STATE ID";
    private static final String ALIAS_STUDENT_HISTORY_STANDARD_DAY = "all-rcd-StdStandardDayHistory";
    private static final String ALIAS_STUDENT_STANDARD_DAY = "DOE STUDENT STANDARD DAY";

    /**
     * Detail report fields
     */
    private static final String DETAIL_REPORT_FIELD_END_DAY = "EndDay";
    private static final String DETAIL_REPORT_FIELD_FTEADE = "FTEADE";
    private static final String DETAIL_REPORT_FIELD_FTEAD = "FTEAD";
    private static final String DETAIL_REPORT_FIELD_GRADE = "Grade";
    private static final String DETAIL_REPORT_FIELD_INS_SERVICE_TYPE = "InsServiceType";
    private static final String DETAIL_REPORT_FIELD_LOCAL_ID = "LocalID";
    private static final String DETAIL_REPORT_FIELD_SECTION_NAME = "SectionName";
    private static final String DETAIL_REPORT_FIELD_SCHOOL = "School";
    private static final String DETAIL_REPORT_FIELD_SCHOOL_NAME = "SchoolName";
    private static final String DETAIL_REPORT_FIELD_STANDARD_DAY_MINS = "StandardDayMinutes";
    private static final String DETAIL_REPORT_FIELD_STD_NAME = "StudentName";
    private static final String DETAIL_REPORT_FIELD_START_DAY = "StartDay";
    private static final String DETAIL_REPORT_FIELD_STATE_ID = "StateID";
    private static final String DETAIL_REPORT_FIELD_STD_TYPE = "RecordType";
    private static final String DETAIL_REPORT_FIELD_STUDENT_MINS = "StudentMinutes";
    private static final String DETAIL_REPORT_FIELD_VOC_CODE = "VocCode";

    /**
     * Input parameters
     */
    private static final String INPUT_PARAM_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_PARAM_DISTRICT_SUMMARY = "includeDistrictSummary";
    private static final String INPUT_PARAM_INCL_EARLY_GRADUATES = "includeEarlyGraduates";
    private static final String INPUT_PARAM_INCL_IEA = "includeIEA";
    private static final String INPUT_PARAM_REPORT_PERIOD = "reportPeriod";
    private static final String INPUT_PARAM_REPORT_TYPE = "reportType";
    private static final String INPUT_PARAM_SCHOOLS = "schoolOids";
    private static final String INPUT_PARAM_SUBREPORT_ID_CSV = "subreportIdCSVVersion";
    private static final String INPUT_PARAM_SUBREPORT_ID_PDF = "subreportIdPDFVersion";
    private static final String INPUT_PARAM_SUMMARY_ONLY = "summaryOnly";

    /**
     * Other constants
     */
    private static final String CODE_TYPE_INSTR_SERVICE_PRIMARY_SCHOOL = "P";
    private static final String CODE_TYPE_INSTR_SERVICE_PARTIAL_SERVICE = "S";
    private static final int COLUMN_FIRST_INDEX = 1;
    private static final int COLUMN_LAST_INDEX = 7;
    private static final int COLUMN_NOT_DEFINED = -1;
    private static final String INITIALIZE_KEY = "label.state.report.initialize";
    private static final String KEY_DELIMITER = ":";
    private static final String PULLOUT_CLASS_TYPE = "P";
    private static final String PROCEDURE_ID_CLASS_SECTION_SCHEDULE = "EXPDATA-TN-MSTS";

    private static final String RECORD_TYPE_REGULAR = "Regular";
    private static final String RECORD_TYPE_EARLY = "Early";
    private static final String RECORD_TYPE_IEA = "IEA";
    private static final String REPORT_FIELD_GRADE_COLUMN_PREFIX = "column_";
    private static final String REPORT_FIELD_ORG_NAME = "orgName";
    private static final String REPORT_FIELD_SCHOOL = "school";
    private static final String REPORT_FIELD_VOC_CODE = "vocCode";
    private static final String REPORT_FIELD_YEAR = "year";
    private static final String REPORT_FIELD_VOC_CLASSIFICATION = "voc_classification";
    private static final String REPORT_PARAMETER_MONTH = "month";
    private static final String REPORT_PARAMETER_USER = "user";
    private static final String REPORT_TYPE_ATTENDANCE = "attendance";
    private static final String REPORT_TYPE_MEMBERSHIP = "membership";
    private static final String STAFF_SCHOOL_CODE = "9999";
    private static final String VOC_CLASSIFICATION_TOTAL = "Total VOC";

    /**
     * Class members
     */
    private Map<String, Collection<ScheduleBellPeriod>> m_bellPeriods =
            new HashMap<String, Collection<ScheduleBellPeriod>>();
    private PlainDate m_endDate = null;
    private HashMap<String, String> m_calendarOids;
    private DistrictSchoolYearContext m_context;
    private EnrollmentStatistics m_data;
    private Collection<PlainDate> m_daysInSession;
    private Collection<PlainDate> m_daysInSessionYear;
    private DecimalFormat m_decimalFormat = new DecimalFormat("#0.0000");
    private DataDictionary m_dictionary;
    private Boolean m_includeDistrictSummary;
    private Boolean m_includeEarlyGraduates;
    private Boolean m_includeIEA;
    private boolean m_isAllSchools;
    private Collection<String> m_lunchStateCodes = Arrays.asList("09304", "09604");
    private TNMultiYearHelper m_multiYearHelper;
    private TNReportingPeriodHelper m_periodHelper = null;
    private Map<String, ReferenceCode> m_referenceEnrollmentCodeMap;
    private Map<String, ReferenceCode> m_referenceVOCCodeMap;
    private REPORT_TYPE m_reportType = REPORT_TYPE.MEMBERSHIP;
    private TNClassSectionScheduleData m_scheduleData;
    private ScheduleManager m_scheduleManager;
    private PlainDate m_startDate = null;
    private Boolean m_summaryOnly;
    private boolean m_useDetail;
    private Map<String, String> m_validVocCodes;

    /**
     * Cleanup.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#cleanup()
     */
    @Override
    protected void cleanup() {
        super.cleanup();
        if (m_data != null) {
            m_data.close();
        }
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
        m_multiYearHelper =
                new TNEnrollmentHelper.TNStudentMultiYearHelper(getOrganization(), getCurrentContext(), getBroker());
        initReportsFormat();
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_decimalFormat.setRoundingMode(RoundingMode.DOWN);
        m_referenceVOCCodeMap = loadRefCodeMapByAlias(ALIAS_COURSE_VOC);
        m_validVocCodes = loadVocClassificationCodes();
        m_context = getCurrentContext();
        m_scheduleManager = new ScheduleManager(getBroker());
        m_includeDistrictSummary = (Boolean) getParameter(INPUT_PARAM_DISTRICT_SUMMARY);
        if (m_includeDistrictSummary == null || isSchoolContext()) {
            m_includeDistrictSummary = Boolean.FALSE;
        }
        m_summaryOnly = (Boolean) getParameter(INPUT_PARAM_SUMMARY_ONLY);
        if (m_summaryOnly == null || isSchoolContext()) {
            m_summaryOnly = Boolean.FALSE;
        }
        String reportPeriodOid = (String) getParameter(INPUT_PARAM_REPORT_PERIOD);
        if (!StringUtils.isEmpty(reportPeriodOid)) {
            ReferenceCode reportPeriod = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, reportPeriodOid);
            m_periodHelper = new TNReportingPeriodHelper(getOrganization(), m_context, reportPeriod, getBroker());
            addParameter(REPORT_PARAMETER_MONTH, reportPeriod.getCode());
        } else {
            m_periodHelper = new TNReportingPeriodHelper(getOrganization(), m_context, null, getBroker());
        }
        m_startDate = m_context.getStartDate();
        m_endDate = m_context.getEndDate();
        if (getParameter(INPUT_PARAM_REPORT_TYPE) != null) {
            String reportType = (String) getParameter(INPUT_PARAM_REPORT_TYPE);
            if (reportType.equalsIgnoreCase(REPORT_TYPE_MEMBERSHIP)) {
                m_reportType = REPORT_TYPE.MEMBERSHIP;
            }
            if (reportType.equalsIgnoreCase(REPORT_TYPE_ATTENDANCE)) {
                m_reportType = REPORT_TYPE.ATTENDANCE;
            }
        }
        loadEnrollmentCodes();
        m_includeEarlyGraduates = (Boolean) getParameter(INPUT_PARAM_INCL_EARLY_GRADUATES);
        m_includeIEA = (Boolean) getParameter(INPUT_PARAM_INCL_IEA);
        addParameter(INPUT_PARAM_INCL_EARLY_GRADUATES, m_includeEarlyGraduates);
        addParameter(INPUT_PARAM_INCL_IEA, m_includeIEA);
        addParameter(REPORT_PARAMETER_USER, getUser());
        addParameter(REPORT_FIELD_YEAR, Integer.toString(m_context.getSchoolYear()));
    }

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid pdfGrid = new ReportDataGrid();
        ReportDataGrid detailGrid = new ReportDataGrid();
        Map<String, Map<Integer, Double>> districtDataSource = prepareDefaultDataSource();

        Collection<SisSchool> schools = getSchools();

        for (SisSchool school : schools) {
            Collection<StateReportValidationError> initErrors = new ArrayList<StateReportValidationError>();

            m_data = new EnrollmentStatistics();
            m_data.setBroker(getBroker());
            m_data.setOrganization(getOrganization());
            m_data.setPrivilegeSet(getPrivilegeSet());
            m_data.setSchoolContext(true);
            m_data.setSchool(school);
            m_data.setParameters(getParameters());
            m_data.setUser(getUser());
            m_data.initializeExport();

            Collection<String> studentOids = m_data.getStudentOids();

            addParameter(TNClassSectionHelper.INPUT_PARAM_ENROLLMENT_HELPER, m_data.m_tnEnrHelper);
            addParameter(TNClassSectionHelper.INPUT_PARAM_BYPASS_DUP_SECT_TEST, Boolean.TRUE);
            addParameter(TNClassSectionHelper.INPUT_PARAM_LOAD_STUDENT_SECTIONS_ONLY, Boolean.TRUE);

            // Init Schedule Data
            m_scheduleData = new TNClassSectionScheduleData();
            if (m_scheduleData != null && initErrors.size() == 0) {
                try {
                    // Initialize the report data object.
                    Map parameters = getParameters();
                    parameters.put("studentOids", studentOids);
                    m_scheduleData.setBroker(getBroker());
                    m_scheduleData.setOrganization(getOrganization());
                    m_scheduleData.setPrivilegeSet(getPrivilegeSet());
                    m_scheduleData.setSchoolContext(isSchoolContext());
                    m_scheduleData.setSchool(school);
                    m_scheduleData.setParameters(parameters);
                    m_scheduleData.setUser(getUser());
                    m_scheduleData.loadDefinitions(PROCEDURE_ID_CLASS_SECTION_SCHEDULE, getBroker());
                    m_scheduleData.initializeExport();
                } catch (X2BaseException x2be) {
                    String init_msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                            .getMessage(INITIALIZE_KEY);
                    initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));
                }
                initErrors.addAll(m_scheduleData.getSetupErrors());
            }

            m_startDate = m_periodHelper.getDateBegin(school.getOid());
            m_endDate = m_periodHelper.getDateEnd(school.getOid());
            if (m_startDate == null || m_endDate == null) {
                String period = m_periodHelper.getMonth() != null ? m_periodHelper.getMonth().getCode() : "YEAR";

                String message =
                        "For school " + school.getName() + " is not defined report period with code: " + period;

                AppGlobals.getLog().log(Level.WARNING, message);
                continue;
            }

            // Load all the data that needed
            loadCalendarsForContextOid(getCurrentContext().getOid());

            ReportDataGrid schoolGrid = new ReportDataGrid();

            Map<String, Map<Integer, Double>> schoolDataSource = prepareDefaultDataSource();

            populateDataSource(school, schoolDataSource, detailGrid);
            calculateTotals(schoolDataSource);

            populateGrid(schoolGrid, schoolDataSource, school, school.getName());
            pdfGrid.append(schoolGrid);

            if (m_includeDistrictSummary.booleanValue()) {
                calculateDistrictTotals(districtDataSource, schoolDataSource);
            }
        }
        if (m_summaryOnly.booleanValue()) {
            pdfGrid = new ReportDataGrid();
        }
        if (m_includeDistrictSummary.booleanValue()) {
            ReportDataGrid districtGrid = new ReportDataGrid();
            populateGrid(districtGrid, districtDataSource, null, getOrganization().getName());
            pdfGrid.append(districtGrid);
        }

        pdfGrid.beforeTop();
        detailGrid.beforeTop();

        return m_useDetail ? detailGrid : pdfGrid;

    }

    /**
     * Summarizes the values obtained for schools.
     *
     * @param districtDataSource Map<String,Map<Integer,Double>>
     * @param schoolDataSource Map<String,Map<Integer,Double>>
     */
    private void calculateDistrictTotals(Map<String, Map<Integer, Double>> districtDataSource,
                                         Map<String, Map<Integer, Double>> schoolDataSource) {
        for (String vocStateCode : schoolDataSource.keySet()) {
            Map<Integer, Double> schoolGridRow = schoolDataSource.get(vocStateCode);
            Map<Integer, Double> districtGridRow = districtDataSource.get(vocStateCode);
            for (Integer columnNo : schoolGridRow.keySet()) {
                double newValue =
                        schoolGridRow.get(columnNo).doubleValue() + districtGridRow.get(columnNo).doubleValue();
                districtGridRow.put(columnNo, Double.valueOf(newValue));
            }
        }

    }

    /**
     * Calculate Total values for VOC data source.
     *
     * @param dataSource Map<String,Map<Integer,Double>>
     */
    private void calculateTotals(Map<String, Map<Integer, Double>> dataSource) {
        for (String vocCode : dataSource.keySet()) {
            if (!vocCode.equalsIgnoreCase(VOC_CLASSIFICATION_TOTAL)) {
                Map<Integer, Double> dataSourceRow = dataSource.get(vocCode);
                for (int index = COLUMN_FIRST_INDEX; index <= COLUMN_LAST_INDEX; index++) {
                    double vocValue = dataSourceRow.get(Integer.valueOf(index)).doubleValue();
                    double totalVocValue =
                            dataSource.get(VOC_CLASSIFICATION_TOTAL).get(Integer.valueOf(index)).doubleValue();
                    dataSource.get(VOC_CLASSIFICATION_TOTAL).put(Integer.valueOf(index),
                            Double.valueOf(vocValue + totalVocValue));
                }
            }
        }
    }

    /**
     * Calculate the duration in minutes for a particular period.
     *
     * @param bell ScheduleBell
     * @param per SchedulePeriod
     * @return int
     */
    private int getBellPeriodMinutes(ScheduleBell bell, SchedulePeriod per) {
        Collection<ScheduleBellPeriod> bellPeriods = m_bellPeriods.get(bell.getOid());
        if (bellPeriods == null) {
            bellPeriods = bell.getScheduleBellPeriods();
            m_bellPeriods.put(bell.getOid(), bellPeriods);
        }
        if (bellPeriods != null && per != null) {
            for (ScheduleBellPeriod bpe : bellPeriods) {
                if (bpe != null && per.getOid().equals(bpe.getSchedulePeriodOid())) {
                    return (int) (bpe.getEndTime().getTimeInMinutes() - bpe.getStartTime().getTimeInMinutes());
                }
            }
        }
        return 0;
    }

    /**
     * Returns in-session days for the reporting period.
     *
     * @param calendarDays Collection<PlainDate>
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return Set<PlainDate> days in session
     */
    private Set<PlainDate> getDaysInSession(Collection<PlainDate> calendarDays,
                                            PlainDate startDate,
                                            PlainDate endDate) {
        Set<PlainDate> daysInSession = new HashSet<PlainDate>();
        if (calendarDays != null && !calendarDays.isEmpty()) {
            for (PlainDate calendarDay : calendarDays) {
                if (!calendarDay.before(startDate) && !calendarDay.after(endDate)) {
                    daysInSession.add(calendarDay);
                }
            }
        }
        return daysInSession;
    }

    /**
     * Encapsulate logic for selecting column number in report grid for column 1 through 4.
     * Only calculates the student with enrollment type P
     *
     * @param student SisStudent
     * @param instrServiceType String
     * @param gradeReferenceCode ReferenceCode
     * @return int
     */
    private int getGridColumnNo(SisStudent student, String instrServiceType, ReferenceCode gradeReferenceCode) {
        if (CODE_TYPE_INSTR_SERVICE_PARTIAL_SERVICE.equals(instrServiceType)
                && m_periodHelper.isStudentNGradeLevel(student, m_endDate, gradeReferenceCode.getCode())) {
            return 1;
        }
        if (gradeReferenceCode != null) {
            String gradeStateCode = gradeReferenceCode.getStateCode();
            if (gradeStateCode.equalsIgnoreCase("07") || gradeStateCode.equalsIgnoreCase("08")) {
                return 2;
            } else if (gradeStateCode.equalsIgnoreCase("09")) {
                return 3;
            } else if (gradeStateCode.equalsIgnoreCase("10")
                    || gradeStateCode.equalsIgnoreCase("11")
                    || gradeStateCode.equalsIgnoreCase("12")) {
                return 4;
            }
        }

        return COLUMN_NOT_DEFINED;
    }

    /**
     * Return collection of schools.
     *
     * @return Collection<SisSchool> collection of schools
     */
    private Collection<SisSchool> getSchools() {
        Collection<SisSchool> schools = null;
        Object objIsAllSchools = getParameter(INPUT_PARAM_ALL_SCHOOLS);
        m_isAllSchools = objIsAllSchools == null ? false : ((Boolean) objIsAllSchools).booleanValue();
        if (m_isAllSchools) {
            X2Criteria schoolCriteria = new X2Criteria();

            schoolCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            schoolCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

            DataDictionaryField aliasSklStateIDField =
                    m_dictionary.findDataDictionaryFieldByAlias(ALIAS_SKL_STATE_ID);
            schoolCriteria.addNotEmpty(aliasSklStateIDField.getJavaName(), getBroker().getPersistenceKey());
            schoolCriteria.addNotEqualTo(aliasSklStateIDField.getJavaName(), STAFF_SCHOOL_CODE);

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
                schoolQuery.addOrderByAscending(SisSchool.COL_SCHOOL_LEVEL_CODE);
                schoolQuery.addOrderByAscending(SisSchool.COL_NAME);
                schools = getBroker().getCollectionByQuery(schoolQuery);
            }
        }

        return schools;
    }

    /**
     * Get set of sections by period and school.
     *
     * @param school SisSchool
     * @return Collection<MasterSchedule>
     */
    private Collection<MasterSchedule> getSections(SisSchool school) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_COURSE_VOC);
        X2Criteria sectionsCriteria = new X2Criteria();
        sectionsCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER + Course.COL_DISTRICT_CONTEXT_OID,
                getCurrentContext().getOid());
        sectionsCriteria.addNotEmpty(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER + field.getJavaName(), getBroker().getPersistenceKey());
        sectionsCriteria.addEqualTo(MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        sectionsCriteria.addEqualTo(MasterSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID,
                school.getOid());

        QueryByCriteria sectionsQuery = new QueryByCriteria(MasterSchedule.class, sectionsCriteria);
        return getBroker().getCollectionByQuery(sectionsQuery);
    }

    /**
     * Get meeting dates for sections.
     *
     * @param sections Collection<MasterSchedule>
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return Map<MasterSchedule, Collection<PlainDate>>
     */
    private Map<MasterSchedule, Collection<PlainDate>> getSectionsMeetingDates(Collection<MasterSchedule> sections,
                                                                               PlainDate startDate,
                                                                               PlainDate endDate) {
        Map<MasterSchedule, Collection<PlainDate>> sectionsMeetingDates =
                new HashMap<MasterSchedule, Collection<PlainDate>>();

        for (MasterSchedule section : sections) {
            Collection<PlainDate> meetingDates = m_scheduleManager.getSectionMeetingDates(section);
            Iterator<PlainDate> iterator = meetingDates.iterator();
            while (iterator.hasNext()) {
                PlainDate date = iterator.next();
                if (date.before(startDate) || date.after(endDate)) {
                    iterator.remove();
                }
            }
            sectionsMeetingDates.put(section, meetingDates);
        }

        return sectionsMeetingDates;
    }

    /**
     * Calculate student FTEADE (full time equivalent daily enrollment).
     *
     * @param studentTimeScheduleMinutesTotal - minutes for studentMembershipDays or
     *        studentAttendanceDays
     * @param studentStandardDayMinutes - student standard day (from DOE STUDENT STANDARD DAY)
     * @param meetingDays - meeting days
     * @return double
     */
    private double getStudentFTEADE(int studentTimeScheduleMinutesTotal,
                                    int studentStandardDayMinutes,
                                    int meetingDays) {
        double result = 0;

        // ADM = SUM [DAYS-SCHEDULED * TIME-SCHEDULED / STD-DAY] / REPORT-PERIOD-DAYS
        if (meetingDays != 0) {
            result = ((double) studentTimeScheduleMinutesTotal) / (studentStandardDayMinutes * meetingDays);
        }

        if (result > 1) {
            result = 1.0d;
        }
        return result;
    }

    /**
     * Calculate student FTEADA (for TN_VOC_MONTH_ADA) or student FTEADM (for TN_VOC_MONTH_ADM and
     * TN_VOC_ANNUAL)
     * for column 6 on Vocational reports. It should always be 0 UNLESS the 'DOE CRS SERVICE
     * DISTRICT ID' alias
     * does not match the current district code.
     *
     *
     * The logic:
     *
     * If course alias 'DOE CRS_SERVICE DISTRICT ID = DISTRICT ID' and
     * Enrollment alias DOE INSTR SERVICE TYPE = 'P'
     * !
     * ! Report generated in the primary school for course taught in home district
     * !
     * Column 5 = calculated ADA/ADM
     * Column 6 = 0
     * Column 7 = Column 5
     * !
     * elseIf course alias 'DOE CRS SERVICE DISTRICT ID' != District ID and
     * Enrollment alias 'DOE INSTR SERVICE TYPE' = 'P' then
     * !
     * ! Report generated in primary school for course taught in a different district
     * ! Primary school for student but Course is being taught in another school district
     * ! this school does not get credit for the Voc attendance
     * ! NOTE 2: The FTEADM which you send to vocational program operated by someone else is
     * subtracted.
     * !
     * Column 5 = calculated ADM/ADA
     * Column 6 = calculated ADM/ADA * -1
     * Column 7 = 0
     * elseIf course alias 'DOE CRS SERVICE DISTRICT ID' = District ID and
     * Enrollment alias 'DOE INSTR SERVICE TYPE' = 'S' then
     * !
     * ! Report generated in secondary school for course taught away from home district
     * ! Not primary school for student, but student is taking course at this school
     * ! The secondary school get ADM credit
     * ! NOTE 2: The FTEADM received from another district in a center operated by you is added
     * !
     * Column 5 = 0
     * Column 6 = Calculated ADA/ADM
     * Column 7 = Column 5 (0) + Column 6
     * END
     *
     *
     *
     * NOTE: This logic is not a possibility:
     *
     * If course alias �DOE CRS_SERVICE DISTRICT ID != DISTRICT ID and
     * Enrollment alias DOE INSTR SERVICE TYPE = �S�
     *
     * Since: ENR IS_TYPE = S which means student not at primary school
     * DOE CRS SERVICE DISTRICT which is where the course is being taught.
     * If IS_TYPE = �S� then student not at primary district so service district can�t not
     * match secondary enrollment district
     *
     * @param course Course
     * @param student SisStudent
     * @param studentFTEADE double
     * @return double
     */
    private double getStudentFTEAverageDaily(Course course, SisStudent student, double studentFTEADE) {
        boolean isZero = true;
        boolean isPositive = true;
        double studentFTEAverageDaily = 0;
        String districtId = OrganizationManager.getParentOrganization(student).getId();

        /*
         * If the course is null OR the 'DOE CRS SERVICE DISTRICT ID' field in course is null,
         * then just assume 'DOE CRS SERVICE DISTRICT ID' field is null. In this case, it will
         * return 0.
         */
        if (course != null) {
            String courseDistrictID = (String) course.getFieldValueByAlias(ALIAS_COURSE_SERVICE_DISTRICT_ID);
            List<TNStudentEnrollmentSpan> enrollmentSpans = m_data.getTNStudentEnrollmentSpans(student, false);
            for (TNStudentEnrollmentSpan span : enrollmentSpans) {
                if (isEarlyGraduated(span)) {
                    if (!m_includeEarlyGraduates.booleanValue()) {
                        continue;
                    }
                }

                PlainDate spanFirstActiveDate = span.getFirstActiveDate();
                PlainDate spanLastActiveDate = span.getLastActiveDate();

                /*
                 * Check if the enrollment start date is before the enrollment last active date
                 */
                if ((spanFirstActiveDate == null || spanLastActiveDate == null ||
                        (spanFirstActiveDate != null && spanLastActiveDate != null
                                && spanFirstActiveDate.before(spanLastActiveDate)))
                        &&
                        /*
                         * Only counts the membership and attendance days of the span that overlap
                         * with the current period range
                         */
                        (span.getFirstActiveDate() != null
                                && (span.getLastActiveDate() == null || !span.getLastActiveDate().before(m_startDate)))
                        &&
                        /*
                         * Check if the student is enrolled and withdrawn on the same day.
                         * If yes, then we don't want to count this student.
                         *
                         * NOTE: right now, it should be only on the E-W case. I think a student
                         * cannot be enrolled and withdrawn on the same day with E-S-S-W case.
                         */
                        (!(span.getFirstActiveEnrollment() != null
                                && StudentEnrollment.ENTRY.equals(span.getFirstActiveEnrollment().getEnrollmentType())
                                &&
                                span.getFirstInactiveEnrollment() != null && StudentEnrollment.WITHDRAWAL
                                        .equals(span.getFirstInactiveEnrollment().getEnrollmentType())
                                &&
                                span.getFirstActiveEnrollment().getEnrollmentDate()
                                        .equals(span.getFirstInactiveEnrollment().getEnrollmentDate())))) {
                    String instrServiceType =
                            span.getFirstActiveEnrollment().getFieldValueByAlias(ALIAS_INSTR_SERVICE_TYPE) != null
                                    ? span.getFirstActiveEnrollment().getFieldValueByAlias(ALIAS_INSTR_SERVICE_TYPE)
                                            .toString()
                                    : null;

                    // When DOE INSTR SERVICE TYPE = P or when it's null (P should be the default
                    // value when it's null)
                    if (instrServiceType == null
                            || CODE_TYPE_INSTR_SERVICE_PRIMARY_SCHOOL.equals(instrServiceType.toString())) {
                        // It should always be 0 UNLESS the �DOE CRS SERVICE DISTRICT ID� alias
                        // does not match the current district code
                        if (districtId != null && !StringUtils.isEmpty(courseDistrictID)
                                && !districtId.equals(courseDistrictID)) {
                            isZero = false;
                            isPositive = false;
                        } else {
                            isZero = true;
                        }
                    } else if (instrServiceType != null
                            && CODE_TYPE_INSTR_SERVICE_PARTIAL_SERVICE.equals(instrServiceType.toString())) {
                        if (districtId != null && !StringUtils.isEmpty(courseDistrictID)
                                && !districtId.equals(courseDistrictID.toString())) {
                            // NOTE: this is not a possibility, but return 0 anyway (default)
                            isZero = true;
                        } else {
                            isZero = false;
                            isPositive = true;
                        }
                    }
                }
            }
        }

        if (!isZero) {
            if (isPositive) {
                studentFTEAverageDaily = studentFTEADE;
            } else {
                studentFTEAverageDaily = -1 * studentFTEADE;
            }
        }


        if (studentFTEAverageDaily > 1) {
            studentFTEAverageDaily = 1.0d;
        }
        return studentFTEAverageDaily;
    }

    /**
     * Get the student time schedule minutes within the given date range.
     *
     * @param studentOid String
     * @param studentScheduleSpan TNStudentScheduleSpan
     * @param daysInSession Collection<PlainDate>
     * @return studentTimeScheduleMinutes
     * @throws X2BaseException exception
     */
    private int getStudentTimeScheduledMinutes(String studentOid,
                                               TNStudentScheduleSpan studentScheduleSpan,
                                               NavigableSet<PlainDate> daysInSession)
            throws X2BaseException {
        int studentTimeScheduledMinutes = 0;

        if (daysInSession != null && !daysInSession.isEmpty()) {
            PlainDate startDate = daysInSession.iterator().next();
            PlainDate endDate = daysInSession.descendingIterator().next();

            List<TNStudentScheduleSpan> scheduleSpans = new ArrayList();
            scheduleSpans.add(studentScheduleSpan);
            TNClassSectionScheduleEntity sectionHelper =
                    new TNClassSectionScheduleEntity(m_scheduleData, scheduleSpans, daysInSession, startDate, endDate);

            if (studentScheduleSpan.getSection() != null &&
                    studentScheduleSpan.getSection().getSchedule() != null &&
                    studentScheduleSpan.getSection().getSchedule().getSchool() != null &&
                    studentScheduleSpan.getSection().getSchedule().getSchoolOid().equals(m_data.getSchool().getOid())) {
                // First filter if the start date, and the span entry date are null
                // Get the master schedule
                MasterSchedule mstSched = studentScheduleSpan.getSection();

                // Exclude the 'lunch' courses. If the stateCode is null, we assume it's not lunch
                // course
                SchoolCourse schoolCourse = mstSched != null ? mstSched.getSchoolCourse() : null;
                Course course = schoolCourse != null ? schoolCourse.getCourse() : null;
                Object stateCourse = course != null ? course.getFieldValueByAlias(ALIAS_COURSE_SDE_CODE) : null;
                String stateCourseCode = stateCourse != null ? stateCourse.toString() : "";
                if (!m_lunchStateCodes.contains(stateCourseCode.toString())) {
                    Collection<SisSchoolCalendarDate> sectionDates = sectionHelper.getSectionDates(mstSched);
                    if (sectionDates != null) {
                        for (SisSchoolCalendarDate curSectionDate : sectionDates) {
                            PlainDate curDate = curSectionDate.getDate();

                            if (daysInSession.contains(curDate)) {
                                // Check if it's ADA and if the student is absent for the given date
                                if (REPORT_TYPE.ATTENDANCE != m_reportType || !isStudentAbsent(studentOid, curDate)) {
                                    if (curSectionDate.getInSessionIndicator()) {
                                        ScheduleBell bell = curSectionDate.getBellSchedule();
                                        if (bell != null) {
                                            Collection<SchedulePeriod> periods =
                                                    sectionHelper.getSectionPeriods(mstSched, curSectionDate);
                                            if (periods != null && periods.size() > 0) {
                                                for (SchedulePeriod period : periods) {
                                                    int time = getBellPeriodMinutes(bell, period);
                                                    if (time == 1
                                                            && PULLOUT_CLASS_TYPE.equals(mstSched.getSchoolCourse()
                                                                    .getCourse()
                                                                    .getFieldValueByAlias(ALIAS_COURSE_CLASS_TYPE)
                                                                    .toString())) {
                                                        time = 0;
                                                    }
                                                    studentTimeScheduledMinutes += time;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        return studentTimeScheduledMinutes;
    }

    /**
     * Get VOC state code for section.
     *
     * @param masterSchedule MasterSchedule
     * @return String
     */
    private String getVocStateCode(MasterSchedule masterSchedule) {
        String vocStateCode = "";

        Course course = masterSchedule.getSchoolCourse() != null ? masterSchedule.getSchoolCourse().getCourse() : null;
        if (course != null && m_referenceVOCCodeMap != null) {
            String vocCode = (String) course.getFieldValueByAlias(ALIAS_COURSE_VOC);
            if (m_referenceVOCCodeMap != null && m_referenceVOCCodeMap.containsKey(vocCode)) {
                ReferenceCode vocReferenceCode = m_referenceVOCCodeMap.get(vocCode);
                vocStateCode = vocReferenceCode.getStateCode();
            }
        }

        return vocStateCode;
    }

    /**
     * Initialize report formats.
     */
    private void initReportsFormat() {
        String formatPDF = (String) getParameter(INPUT_PARAM_SUBREPORT_ID_PDF);
        String formatCSV = (String) getParameter(INPUT_PARAM_SUBREPORT_ID_CSV);
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
     * Returns true if student is early graduated, otherwise false.
     *
     * @param span TNStudentEnrollmentSpan
     * @return true, if is early graduated
     */
    private boolean isEarlyGraduated(TNStudentEnrollmentSpan span) {
        boolean isEarlyGraduate = false;
        StudentEnrollment studentEnrollment = span.getEnrollmentForDate(m_endDate, "W");
        if (studentEnrollment != null) {
            String enrCode = studentEnrollment.getEnrollmentCode();
            ReferenceCode refCode = m_referenceEnrollmentCodeMap.get(enrCode);
            if (refCode != null && "12".equals(refCode.getStateCode())) {
                isEarlyGraduate = true;
            }
        }
        return isEarlyGraduate;
    }

    /**
     * Return if the student is absent for the given attendance date.
     *
     * @param studentOid String
     * @param attendanceDate PlainDate
     * @return boolean
     */
    private boolean isStudentAbsent(String studentOid, PlainDate attendanceDate) {
        List<StudentAttendance> attendances = m_data.getStudentAttendances(studentOid);
        if (!CollectionUtils.isEmpty(attendances)) {
            for (StudentAttendance attendance : attendances) {
                if (attendance.getDate().equals(attendanceDate)) {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * Method for getting calendars for all schools for given context.
     *
     * @param contextOid String
     */
    private void loadCalendarsForContextOid(String contextOid) {
        m_calendarOids = new HashMap<String, String>();
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, contextOid);

        QueryByCriteria query = new QueryByCriteria(SchoolCalendar.class, criteria);
        QueryIterator iter = getBroker().getIteratorByQuery(query);

        try {
            while (iter.hasNext()) {
                SchoolCalendar calendar = (SchoolCalendar) iter.next();

                String calendarCode = calendar.getCalendarId();
                String schoolOid = calendar.getSchoolOid();
                String key = makeCalendarLookupKey(contextOid, schoolOid, calendarCode);

                if (!m_calendarOids.containsKey(key)) {
                    String value = (String) calendar.getFieldValueByAlias(ALIAS_INSTR_PGM);
                    String instrPgm = m_data.lookupReferenceCodeByAlias(ALIAS_INSTR_PGM, value,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    m_calendarOids.put(key, instrPgm);
                }
            }
        } finally {
            iter.close();
        }

        // Throw an error / warning message
        if (m_calendarOids.isEmpty()) {
            String msg = "No Data: School calendar is not defined for any school in the current contextOid ";
            AppGlobals.getLog().log(Level.SEVERE, msg);
        }
    }

    /**
     * Load enrollment codes.
     */
    private void loadEnrollmentCodes() {
        ModelProperty prop = new ModelProperty(StudentEnrollment.class, StudentEnrollment.COL_ENROLLMENT_CODE,
                getBroker().getPersistenceKey());
        DataDictionaryField field = m_dictionary.findDataDictionaryField(prop.getFieldId());
        ReferenceTable referenceTable = field.getReferenceTable();
        m_referenceEnrollmentCodeMap = referenceTable.getCodeMap();
    }

    /**
     * Load reference code map by alias.
     *
     * @param alias String
     * @return Map<String, ReferenceCode>
     */
    private Map<String, ReferenceCode> loadRefCodeMapByAlias(String alias) {
        Map<String, ReferenceCode> refCodeMap = null;
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);

        if (field != null) {
            ReferenceTable referenceTable = field.getReferenceTable();
            refCodeMap = referenceTable.getCodeMap();
        }
        return refCodeMap;
    }

    /**
     * Load the vocational classification codes.
     *
     * @return Map<String, String>
     */
    private Map<String, String> loadVocClassificationCodes() {
        Map<String, String> valid_voc_codes = new LinkedHashMap<String, String>();

        // If the reference VOC Code Map is null, then try to load it again. It shouldn't be null at
        // this point, but just to make sure.
        if (m_referenceVOCCodeMap == null) {
            m_referenceVOCCodeMap = loadRefCodeMapByAlias(ALIAS_COURSE_VOC);
        }

        // If the reference VOC Code Map is not null, then get the valid VOC codes from it (code
        // that's not disabled)
        if (m_referenceVOCCodeMap != null) {
            for (String key : m_referenceVOCCodeMap.keySet()) {
                ReferenceCode refCode = m_referenceVOCCodeMap.get(key);
                if (!refCode.getDisabledIndicator()) {
                    valid_voc_codes.put(key, refCode.getDescription());
                }
            }
        }

        // Add the special code for the total values
        valid_voc_codes.put(VOC_CLASSIFICATION_TOTAL, "TOTAL Vocational By Classification");

        return Collections.unmodifiableMap(valid_voc_codes);
    }

    /**
     * NOTE: makeCalendarLookupKey method from TNStateReportData.java file
     *
     * @param orgOid String
     * @param sklOid String
     * @param calendarCode String
     * @return String
     */
    private static String makeCalendarLookupKey(String orgOid, String sklOid, String calendarCode) {
        return orgOid + KEY_DELIMITER + sklOid + KEY_DELIMITER + calendarCode;
    }

    /**
     * Populate data source with appropriate data.
     *
     * @param school SisSchool
     * @param dataSource Map<String,Map<Integer,Double>>
     * @param detailGrid ReportDataGrid
     * @throws X2BaseException exception
     */
    private void populateDataSource(SisSchool school,
                                    Map<String, Map<Integer, Double>> dataSource,
                                    ReportDataGrid detailGrid)
            throws X2BaseException {
        // Use all sections to support early withdrawl graduates
        Collection<MasterSchedule> sections = getSections(school);
        Map<MasterSchedule, Collection<PlainDate>> sectionsMeetingDates = getSectionsMeetingDates(sections, m_startDate,
                m_endDate);
        if (!sectionsMeetingDates.keySet().isEmpty()) {
            m_periodHelper.setStudentCriteria(m_data.getStudentCriteria());

            QueryIterator students = null;
            Query query = m_data.getStudentQuery(false);
            try {
                students = getBroker().getIteratorByQuery(query);
            } catch (Exception e) {
                StringBuilder output = new StringBuilder();
                output.append("School: " + school.getName() + "\n");
                output.append(query.toString());
                output.append("\n");
                output.append(getBroker().getSelectSql(query));
                output.append("\n");
                StringWriter sw = new StringWriter();
                PrintWriter pw = new PrintWriter(sw);
                e.printStackTrace(pw);
                output.append(sw.toString());

                throw new IllegalStateException(output.toString());
            }
            try {
                while (students.hasNext()) {
                    SisStudent student = (SisStudent) students.next();

                    m_daysInSession = m_periodHelper.getDaysInSession(school.getOid());
                    m_daysInSessionYear = m_periodHelper.getDaysInSessionYear(school.getOid());

                    populateDataSourceWithMeetingDays(student, school, sectionsMeetingDates, dataSource, detailGrid);
                }
            } finally {
                if (students != null) {
                    students.close();
                }
            }
        }
    }

    /**
     * Calculate appropriate FTEADE value and update data source.
     *
     * @param student SisStudent
     * @param school SisSchool
     * @param sectionsMeetingDates Map<MasterSchedule,Collection<PlainDate>>
     * @param dataSource Map<String,Map<Integer,Double>>
     * @param detailGrid ReportDataGrid
     * @throws X2BaseException exception
     */
    private void populateDataSourceWithMeetingDays(SisStudent student,
                                                   SisSchool school,
                                                   Map<MasterSchedule, Collection<PlainDate>> sectionsMeetingDates,
                                                   Map<String, Map<Integer, Double>> dataSource,
                                                   ReportDataGrid detailGrid)
            throws X2BaseException {
        /*
         * Check for student ineligibility status (I20 or out of state student). If the student is
         * ineligible,
         * then exclude the student from ADM, and ADA calculations.
         */

        boolean isStudentIneligible =
                m_periodHelper.hasStudentIneligibilityFundingStatus(student, m_startDate, m_endDate);
        ReferenceCode gradeLevelCode = m_data.getGradeLevelByDates(student, m_startDate, m_endDate);
        String gradeLevel = gradeLevelCode.getCode();
        int stdStandardDayMinutes = 0;
        try {
            String standardDayString =
                    m_multiYearHelper.getHistoryValueByAlias(gradeLevelCode,
                            ALIAS_STUDENT_HISTORY_STANDARD_DAY, ALIAS_STUDENT_STANDARD_DAY);
            stdStandardDayMinutes = Integer.valueOf(standardDayString).intValue();
        } catch (NumberFormatException nfe) {
            stdStandardDayMinutes = 0; // Set the value
                                       // back to 0
        }
        if (stdStandardDayMinutes == 0) {
            throw new IllegalStateException("Standard Day for grade " + gradeLevel +
                    "cannot be 0 or null.");
        }

        if (!isStudentIneligible && stdStandardDayMinutes > 0) {
            NavigableSet<PlainDate> inSessionDatesForIea = null;
            List<Pair<PlainDate, PlainDate>> ieaPeriodCollections = null;
            boolean isIeaStd = m_periodHelper.isIEAStudent(student);
            if (isIeaStd) {
                inSessionDatesForIea = new TreeSet<>();
                List<Pair<PlainDate, PlainDate>> nGradeLevelsPeriodCollections =
                        m_periodHelper.getStudentNGradeLevelRange(student, m_startDate, m_endDate);
                ieaPeriodCollections =
                        m_periodHelper.getStudentIEAPgmsRange(student, m_startDate, m_endDate,
                                nGradeLevelsPeriodCollections);
                for (Pair<PlainDate, PlainDate> iea : ieaPeriodCollections) {
                    inSessionDatesForIea
                            .addAll(getDaysInSession(m_daysInSession, iea.getLeft(),
                                    iea.getRight() != null ? iea.getRight() : m_endDate));
                }
            }

            List<TNStudentScheduleSpan> scheduleSpans = m_data.getTNStudentScheduleSpans(student);
            List<TNStudentEnrollmentSpan> enrollmentSpans = m_data.getTNStudentEnrollmentSpans(student, true);


            for (TNStudentEnrollmentSpan span : enrollmentSpans) {
                /*
                 * Check if the student is enrolled and withdrawn on the same day.
                 * If yes, then we don't want to count this student.
                 *
                 * NOTE: right now, it should be only on the E-W case. I think a student
                 * cannot be enrolled and withdrawn on the same day with E-S-S-W case.
                 */
                if ((span.getFirstActiveEnrollment() != null &&
                        StudentEnrollment.ENTRY.equals(span.getFirstActiveEnrollment().getEnrollmentType()) &&
                        span.getFirstInactiveEnrollment() != null &&
                        StudentEnrollment.WITHDRAWAL
                                .equals(span.getFirstInactiveEnrollment().getEnrollmentType())
                        &&
                        span.getFirstActiveEnrollment().getEnrollmentDate()
                                .equals(span.getFirstInactiveEnrollment().getEnrollmentDate()))) {
                    continue;
                }

                boolean isEarlyGraduatedSpan = isEarlyGraduated(span);
                NavigableSet<PlainDate> spanDates = new TreeSet();
                if (span.getFirstActiveDate() != null) {
                    spanDates.addAll(getDaysInSession(m_daysInSession, span.getFirstActiveDate(),
                            span.getLastActiveDate() != null ? span.getLastActiveDate() : m_endDate));
                }

                NavigableSet<PlainDate> datesForEarly = null;
                if (isEarlyGraduatedSpan && span.getLastActiveDate() != null) {
                    datesForEarly = new TreeSet();
                    datesForEarly.addAll(getDaysInSession(m_daysInSession, span.getLastActiveDate(), m_endDate));
                    datesForEarly.remove(span.getLastActiveDate());
                }

                /*
                 * Check if the enrollment start date is before the enrollment last active date
                 */
                if (!spanDates.isEmpty() || (datesForEarly != null && !datesForEarly.isEmpty())) {
                    for (TNStudentScheduleSpan scheduleSpan : scheduleSpans) {
                        MasterSchedule studentMasterSchedule = scheduleSpan.getSection();

                        if (!sectionsMeetingDates.containsKey(studentMasterSchedule) && !isEarlyGraduatedSpan) {
                            continue;
                        }

                        String vocStateCode = getVocStateCode(studentMasterSchedule);
                        if (StringUtils.isEmpty(vocStateCode)) {
                            continue;
                        }

                        // Set instructional service type
                        String instrServiceType =
                                (String) span.getFirstActiveEnrollment().getFieldValueByAlias(ALIAS_INSTR_SERVICE_TYPE);
                        if (StringUtils.isEmpty(instrServiceType)) {
                            instrServiceType = CODE_TYPE_INSTR_SERVICE_PRIMARY_SCHOOL;
                        }

                        NavigableSet<PlainDate> scheduleDates = new TreeSet();
                        scheduleDates.addAll(getDaysInSession(m_daysInSession, scheduleSpan.getEntryDate(),
                                scheduleSpan.getExitDate()));
                        scheduleDates.retainAll(spanDates);

                        // Check for early graduate
                        if (isEarlyGraduatedSpan &&
                                scheduleSpan.getExitDate() != null &&
                                !scheduleSpan.getExitDate().before(span.getLastActiveDate()) &&
                                datesForEarly.size() > 0) {
                            NavigableSet<PlainDate> scheduleDatesYear = new TreeSet();
                            scheduleDatesYear.addAll(getDaysInSession(m_daysInSessionYear, scheduleSpan.getEntryDate(),
                                    scheduleSpan.getExitDate()));
                            double studentMinutesDouble =
                                    getStudentTimeScheduledMinutes(student.getOid(), scheduleSpan, scheduleDatesYear);
                            studentMinutesDouble /= scheduleDatesYear.size();
                            studentMinutesDouble *= datesForEarly.size();
                            int studentMinutes = (int) studentMinutesDouble;
                            populateDataSourceWithSection(dataSource, detailGrid, datesForEarly, studentMinutes,
                                    m_daysInSession.size(), RECORD_TYPE_EARLY, school, student, gradeLevel,
                                    gradeLevelCode, scheduleSpan, stdStandardDayMinutes, instrServiceType,
                                    vocStateCode);
                        }

                        // split days between IEA and regular
                        NavigableSet<PlainDate> scheduleIeaDates = null;
                        if (inSessionDatesForIea != null && inSessionDatesForIea.size() > 0) {
                            scheduleIeaDates = new TreeSet();
                            scheduleIeaDates.addAll(scheduleDates);
                            scheduleIeaDates.retainAll(inSessionDatesForIea);
                            scheduleDates.removeAll(scheduleIeaDates);
                        }
                        if (scheduleDates.size() > 0) {
                            int studentMinutes =
                                    getStudentTimeScheduledMinutes(student.getOid(), scheduleSpan, scheduleDates);
                            populateDataSourceWithSection(dataSource, detailGrid, scheduleDates, studentMinutes,
                                    m_daysInSession.size(), RECORD_TYPE_REGULAR, school, student, gradeLevel,
                                    gradeLevelCode, scheduleSpan, stdStandardDayMinutes, instrServiceType,
                                    vocStateCode);
                        }
                        if (scheduleIeaDates != null && scheduleIeaDates.size() > 0) {
                            int studentMinutes =
                                    getStudentTimeScheduledMinutes(student.getOid(), scheduleSpan, scheduleIeaDates);
                            populateDataSourceWithSection(dataSource, detailGrid, scheduleIeaDates, studentMinutes,
                                    m_daysInSession.size(), RECORD_TYPE_IEA, school, student, gradeLevel,
                                    gradeLevelCode, scheduleSpan, stdStandardDayMinutes, instrServiceType,
                                    vocStateCode);
                        }
                    }
                }
            }

        }
    }

    /**
     * Populate data source with section.
     *
     * @param dataSource Map<String,Map<Integer,Double>>
     * @param detailGrid ReportDataGrid
     * @param daysInSession NavigableSet<PlainDate>
     * @param studentMinutes int
     * @param totalDays int
     * @param recordType String
     * @param school SisSchool
     * @param student SisStudent
     * @param gradeLevel String
     * @param gradeLevelCode ReferenceCode
     * @param scheduleSpan TNStudentScheduleSpan
     * @param stdStandardDayMinutes int
     * @param instrServiceType String
     * @param vocStateCode String
     * @throws X2BaseException exception
     */
    private void populateDataSourceWithSection(Map<String, Map<Integer, Double>> dataSource,
                                               ReportDataGrid detailGrid,
                                               NavigableSet<PlainDate> daysInSession,
                                               int studentMinutes,
                                               int totalDays,
                                               String recordType,
                                               SisSchool school,
                                               SisStudent student,
                                               String gradeLevel,
                                               ReferenceCode gradeLevelCode,
                                               TNStudentScheduleSpan scheduleSpan,
                                               int stdStandardDayMinutes,
                                               String instrServiceType,
                                               String vocStateCode)
            throws X2BaseException {
        MasterSchedule studentMasterSchedule = scheduleSpan.getSection();
        // detailGrid.set(column, value);
        // If the vocational classification code is empty
        // for this student schedule, we don't need to count
        // it
        // The standard day minutes. We can get it from
        // the 'DOE STUDENT STANDARD DAY' field in Grade
        // Level reference table.
        double studentFTEADE = getStudentFTEADE(studentMinutes, stdStandardDayMinutes, totalDays);
        int gridColumnNo = -1;
        /*
         * For column 1 through 4, only count students with the DOE
         * INSTR SERVICE TYPE = P,
         * or when it's null (P should be default value when it's null).
         */
        if (instrServiceType == null ||
                CODE_TYPE_INSTR_SERVICE_PRIMARY_SCHOOL.equals(instrServiceType) ||
                CODE_TYPE_INSTR_SERVICE_PARTIAL_SERVICE.equals(instrServiceType)) {
            gridColumnNo = getGridColumnNo(student, instrServiceType, gradeLevelCode);
        }

        updateDataSource(dataSource, vocStateCode, gridColumnNo, studentFTEADE, recordType);

        Course course = studentMasterSchedule.getSchoolCourse() != null
                ? studentMasterSchedule.getSchoolCourse().getCourse()
                : null;
        double studentFTEAD = getStudentFTEAverageDaily(course, student, studentFTEADE);

        /*
         * Column 5 Gross district FTEADM
         *
         * NOTE:
         * If column 5 and 6 are equals, then we need to set column 5 to
         * 0.
         * It's to cover this logic (column 6 will equal with column 5
         * in this case):
         *
         * If
         * course alias �DOE CRS SERVICE DISTRICT ID� = District ID
         * and
         * enrollment alias �DOE INSTR SERVICE TYPE� = �S� then
         * Then
         * Column 5 = 0
         * Column 6 = Calculated ADA/ADM
         * Column 7 = Column 5 (0) + Column 6
         *
         */
        double column5Value = 0;
        if (studentFTEADE != 0 && studentFTEADE == studentFTEAD) {
            updateDataSource(dataSource, vocStateCode, 5, 0, recordType);
        } else {
            updateDataSource(dataSource, vocStateCode, 5, studentFTEADE, recordType);
            column5Value = studentFTEADE;
        }

        // Column 6
        updateDataSource(dataSource, vocStateCode, 6, studentFTEAD, recordType);
        double column6Value = studentFTEAD;

        // Column 7 is the sum of Column 5 and 6
        double netFTEADM = column5Value + column6Value;
        updateDataSource(dataSource, vocStateCode, 7, netFTEADM, recordType);

        detailGrid.append();
        detailGrid.set(DETAIL_REPORT_FIELD_SCHOOL, school);
        detailGrid.set(DETAIL_REPORT_FIELD_SCHOOL_NAME, school.getName());
        detailGrid.set(DETAIL_REPORT_FIELD_STD_NAME, student.getNameView());
        detailGrid.set(DETAIL_REPORT_FIELD_STATE_ID,
                student.getFieldValueByAlias(ALIAS_EIS_STATE_ID));
        detailGrid.set(DETAIL_REPORT_FIELD_LOCAL_ID, student.getLocalId());

        detailGrid.set(DETAIL_REPORT_FIELD_GRADE, gradeLevel);
        detailGrid.set(DETAIL_REPORT_FIELD_SECTION_NAME,
                studentMasterSchedule.getDescription());
        detailGrid.set(DETAIL_REPORT_FIELD_END_DAY, daysInSession.descendingIterator().next());
        detailGrid.set(DETAIL_REPORT_FIELD_FTEADE, Double.valueOf(studentFTEADE));
        detailGrid.set(DETAIL_REPORT_FIELD_FTEAD, Double.valueOf(studentFTEAD));
        detailGrid.set(DETAIL_REPORT_FIELD_INS_SERVICE_TYPE, instrServiceType);
        detailGrid.set(DETAIL_REPORT_FIELD_STANDARD_DAY_MINS,
                Integer.valueOf(stdStandardDayMinutes));
        detailGrid.set(DETAIL_REPORT_FIELD_START_DAY, daysInSession.iterator().next());
        detailGrid.set(DETAIL_REPORT_FIELD_STUDENT_MINS, Integer.valueOf(studentMinutes));
        detailGrid.set(DETAIL_REPORT_FIELD_VOC_CODE, vocStateCode);
        detailGrid.set(DETAIL_REPORT_FIELD_STD_TYPE, recordType);

    }

    /**
     * Populate grid method.
     *
     * @param schoolGrid ReportDataGrid
     * @param dataSource Map<String,Map<Integer,Double>>
     * @param school SisSchool
     * @param orgName String
     */
    private void populateGrid(ReportDataGrid schoolGrid,
                              Map<String, Map<Integer, Double>> dataSource,
                              SisSchool school,
                              String orgName) {
        for (String vocStateCode : dataSource.keySet()) {

            schoolGrid.append();
            schoolGrid.set(REPORT_FIELD_SCHOOL, school);
            schoolGrid.set(REPORT_FIELD_ORG_NAME, orgName);
            schoolGrid.set(REPORT_FIELD_VOC_CODE, vocStateCode);
            schoolGrid.set(REPORT_FIELD_VOC_CLASSIFICATION, m_validVocCodes.get(vocStateCode));
            Map<Integer, Double> defaultGridRow = dataSource.get(vocStateCode);
            for (Integer columnNo : defaultGridRow.keySet()) {
                schoolGrid.set(REPORT_FIELD_GRADE_COLUMN_PREFIX + columnNo,
                        m_decimalFormat.format(defaultGridRow.get(columnNo)).toString());
            }
        }
    }

    /**
     * Prepare data source with default values (used as data-source for report).
     *
     * @return Map<String, Map<Integer, Double>>
     */
    private Map<String, Map<Integer, Double>> prepareDefaultDataSource() {
        Map<String, Map<Integer, Double>> defaultGridStructure = new LinkedHashMap<String, Map<Integer, Double>>();
        for (String vocStateCode : m_validVocCodes.keySet()) {
            Map<Integer, Double> defaultGridRow = new LinkedHashMap<Integer, Double>();
            for (int index = COLUMN_FIRST_INDEX; index <= COLUMN_LAST_INDEX; index++) {
                defaultGridRow.put(Integer.valueOf(index), Double.valueOf(0));
            }
            defaultGridStructure.put(vocStateCode, defaultGridRow);
        }

        return defaultGridStructure;
    }

    /**
     * Update datasource (sum with appropriate existing value).
     *
     * @param dataSource Map<String,Map<Integer,Double>>
     * @param vocStateCode String
     * @param gridColumnNo int
     * @param studentFTEADE double
     * @param recordType String
     */
    private void updateDataSource(Map<String, Map<Integer, Double>> dataSource,
                                  String vocStateCode,
                                  int gridColumnNo,
                                  double studentFTEADE,
                                  String recordType) {
        if (RECORD_TYPE_REGULAR.equals(recordType) ||
                (RECORD_TYPE_EARLY.equals(recordType) && m_includeEarlyGraduates.booleanValue()) ||
                (RECORD_TYPE_IEA.equals(recordType) && m_includeIEA.booleanValue())) {
            Map<Integer, Double> vocMeetingDates = dataSource.get(vocStateCode);
            if (vocMeetingDates != null && vocMeetingDates.containsKey(Integer.valueOf(gridColumnNo))) {
                double enrolledMeetingDaysCount = vocMeetingDates.get(Integer.valueOf(gridColumnNo)).doubleValue();
                enrolledMeetingDaysCount += studentFTEADE;
                vocMeetingDates.put(Integer.valueOf(gridColumnNo), Double.valueOf(enrolledMeetingDaysCount));
            }
        }
    }
}
