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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentEnrollmentSpan;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.procedures.statereporting.tn.TNStateReportData;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.util.*;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.Query;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Director's Annual Special Education" report.
 *
 * @author X2 Development Corporation
 */
public class DirectorsSpecialEducationOptionsReportData extends ReportJavaSourceNet {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Helper class for various student information delivery.
     */
    class EnrollmentStatistics extends TNStateReportData {
        protected TNStudentHistoryHelper m_helper;
        protected TNEnrollmentHelper m_tnEnrHelper;

        /**
         * Return calendar days.
         *
         * @param school SisSchool
         * @param calendar String
         * @return Sets the
         */
        public Set<PlainDate> getCalendarDays(SisSchool school, String calendar) {
            Set<PlainDate> dates = m_helper.getCalendarDays(school, calendar);
            if (dates == null) {
                dates = m_helper.getCalendarDays(school, StudentHistoryHelper.CALENDAR_ANY);
            }
            if (dates == null) {
                dates = new HashSet();
            }
            return dates;
        }

        /**
         * Returns in-session days for the reporting period.
         *
         * @param school SisSchool
         * @param calendar String
         * @return days in session
         */
        public Set<PlainDate> getDaysInSession(SisSchool school, String calendar) {
            Set<PlainDate> calendarDays = getCalendarDays(school, calendar);
            Set<PlainDate> daysInSession = null;
            if (calendarDays != null) {
                daysInSession = new HashSet<PlainDate>();

                for (PlainDate calendarDay : calendarDays) {
                    if (!calendarDay.before(m_startDate) && !calendarDay.after(m_endDate)) {
                        daysInSession.add(calendarDay);
                    }
                }
            }
            return daysInSession;
        }

        /**
         * Return the current student criteria.
         *
         * @return Criteria
         */
        public Criteria getStudentCriteria() {
            X2Criteria criteria = m_helper.getStudentCriteria();
            return criteria;
        }

        /**
         * Returns a list of student enrollment spans that represent all of the students enrollment
         * activity and segments.
         *
         * @param student Student
         * @param limit boolean
         * @return List
         */
        public List<TNStudentEnrollmentSpan> getTNStudentEnrollmentSpans(Student student, boolean limit) {
            return m_helper.getTNStudentEnrollmentSpans(student, limit);
        }

        /**
         * Return the current student query.
         *
         * @param distinct boolean
         * @return Query
         */
        public Query getStudentQuery(boolean distinct) {

            return m_helper.getStudentQuery(distinct);
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
            m_helper = m_tnEnrHelper.getStudentHistoryHelper();
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);

            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_ATTENDANCE, Boolean.FALSE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);

            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.TRUE);
        }
    }

    private static final String ALIAS_EIS_STATE_ID = "DOE EIS STATE ID";
    private static final String ALIAS_IEP_TYPE = "EasyIEP Type";
    private static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL STATE ID";

    private static final String DATASRC_PRIMARY_PROGRAM_KEY = "primary";
    private static final String DATASRC_SECONDARY_PROGRAM_KEY = "secondary";
    private static final String DATASRC_TOTAL_PROGRAM_KEY = "total";

    private static final String IEP_TYPE_PRIMARY = "P";
    private static final String IEP_TYPE_SECONDARY = "S";

    private static final String INPUT_INCL_EARLY_GRADUATES = "includeEarlyGraduates";
    private static final String INPUT_INCL_IEA = "includeIEA";
    private static final String INPUT_PARAMETER_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_PARAMETER_MONTH = "month";
    private static final String INPUT_PARAMETER_DISTRICT_SUMMARY = "includeDistrictSummary";
    private static final String INPUT_PARAMETER_SCHOOLS = "schoolOids";
    private static final String INPUT_PARAMETER_SUMMARY_ONLY = "summaryOnly";

    private static final String INPUT_REPORT_ID_CSV = "subreportIdCSVVersion";
    private static final String INPUT_REPORT_ID_PDF = "subreportIdPDFVersion";

    private static final int OPTIONS_END_VALUE = 10;
    private static final int OPTIONS_GRAND_TOTAL_INDEX = 100;
    private static final int OPTIONS_START_VALUE = 1;

    private static final String REPORT_FIELD_END_DATE = "endDate";
    private static final String REPORT_FIELD_ORG_NAME = "orgName";
    private static final String REPORT_FIELD_OPTION_BEGIN_DATE = "optBeginDate";
    private static final String REPORT_FIELD_OPTION_END_DATE = "optEndDate";
    private static final String REPORT_FIELD_OPTION_NAME = "optionName";
    private static final String REPORT_FIELD_OPTIONS_TOTAL = "optionsTotal";
    private static final String REPORT_FIELD_PRIMARY_OPTION = "primaryOption";
    private static final String REPORT_FIELD_PRIMARY_OPTION_ADM = "primaryOptionADM";
    private static final String REPORT_FIELD_PRIMARY_SEC_FLAG = "primarySecondary";
    private static final String REPORT_FIELD_SECONDARY_OPTION = "secondaryOption";
    private static final String REPORT_FIELD_SECONDARY_OPTION_ADM = "secondaryOptionADM";
    private static final String REPORT_FIELD_SCHOOL = "school";
    private static final String REPORT_FIELD_START_DATE = "startDate";
    private static final String REPORT_FIELD_STUDENT_NAME = "studentName";
    private static final String REPORT_FIELD_STUDENT_GENDER = "gender";
    private static final String REPORT_FIELD_STUDENT_GRADE_LEVEL = "gradeLevel";
    private static final String REPORT_FIELD_STUDENT_LASID = "LASID";
    private static final String REPORT_FIELD_STUDENT_SASID = "SASID";
    private static final String REPORT_FIELD_TOTAL_ADM = "totalADM";
    private static final String REPORT_FIELD_TYPE_STD = "stdType";

    private static final String REPORT_PARAMETER_BEGINNING_PERIOD = "begginingPeriod";
    private static final String REPORT_PARAMETER_ENDING_PERIOD = "endingPeriod";
    private static final String REPORT_PARAMETER_MONTH = "month";
    private static final String REPORT_PARAMETER_USER = "user";

    private static final String REPORT_OPTIONS_COLUMN_DEFAULT_TEXT = "OPTION ";
    private static final String REPORT_OPTIONS_GRAND_TOTAL_TEXT = "Grand Total";

    private static final String PGM_CATEGORY_STD_CLASS = "Student Classification";
    private static final String PGM_CODE_STD_CLASS = "8";
    private static final String STD_TYPE_REGULAR = "Regular";
    private static final String STD_TYPE_EARLY = "Early";
    private static final String STD_TYPE_IEA = "IEA";
    private static final String VALID_PROGRAM_CATEGORY = "Options";
    private static final String[] VALID_PROGRAM_CODES = {"1", "2", "3", "4", "5", "6", "7", "8", "9", "10"};

    protected PlainDate m_endDate = null;
    protected PlainDate m_startDate = null;

    private DistrictSchoolYearContext m_context;
    private EnrollmentStatistics m_data;
    private DataDictionary m_dictionary;
    private DecimalFormat m_decimalFormat;
    protected Map<String, ReferenceCode> m_referenceEnrollmentCodeMap;
    protected Map<String, Collection<StudentProgramParticipation>> m_ieaStdPgms = new HashMap<>();
    private Boolean m_includeDistrictSummary;
    private Boolean m_includeEarlyGraduates;
    private Boolean m_includeIEA;
    private ReferenceCode m_month = null;
    private TNReportingPeriodHelper m_periodHelper = null;
    private Map<String, ReferenceCode> m_refCodeMap;
    private Map<String, Collection<StudentProgramParticipation>> m_studentProgramParticipationMap;
    private Boolean m_summaryOnly;
    private boolean m_useDetail;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        addParameter(REPORT_PARAMETER_USER, getUser());
        addParameter(REPORT_PARAMETER_BEGINNING_PERIOD, m_startDate);
        addParameter(REPORT_PARAMETER_ENDING_PERIOD, m_endDate);
        if (m_month != null) {
            addParameter(REPORT_PARAMETER_MONTH, m_month.getCode());
        }

        ReportDataGrid pdfGrid = new ReportDataGrid();
        ReportDataGrid detailGrid = new ReportDataGrid();

        Collection<SisSchool> schools = getSchools();


        Map<Integer, Map<String, Double>> studentDistrictOptionsMembership = getDefaultOptionsMembership();
        for (SisSchool school : schools) {
            m_data = new EnrollmentStatistics();
            m_data.setBroker(getBroker());
            m_data.setOrganization(getOrganization());
            m_data.setPrivilegeSet(getPrivilegeSet());
            m_data.setSchoolContext(true);
            m_data.setSchool(school);
            m_data.setParameters(getParameters());
            m_data.setUser(getUser());
            m_data.initializeExport();

            if (m_studentProgramParticipationMap != null) {
                m_data.getStudentCriteria().addIn(X2BaseBean.COL_OID, m_studentProgramParticipationMap.keySet());
            }

            QueryIterator students = getBroker().getIteratorByQuery(m_data.getStudentQuery(false));
            Map<Integer, Map<String, Double>> studentOptionsMembership = null;
            try {
                studentOptionsMembership = handleStudentProgramMembership(school, students, detailGrid);
                calculateTotals(studentOptionsMembership);
                if (m_includeDistrictSummary.booleanValue()) {
                    calculateDistrictTotals(studentDistrictOptionsMembership, studentOptionsMembership);
                }
            } finally {
                if (students != null) {
                    students.close();
                }
            }
            ReportDataGrid schoolGrid = new ReportDataGrid();
            String schoolName = school.getName();
            populateGrid(schoolGrid, studentOptionsMembership, school, schoolName);

            pdfGrid.append(schoolGrid);
        }
        if (m_summaryOnly.booleanValue()) {
            pdfGrid = new ReportDataGrid();
        }
        if (m_includeDistrictSummary.booleanValue()) {
            ReportDataGrid districtGrid = new ReportDataGrid();
            populateGrid(districtGrid, studentDistrictOptionsMembership, null, getOrganization().getName());
            pdfGrid.append(districtGrid);
        }
        detailGrid.beforeTop();
        detailGrid.sort(Arrays.asList(REPORT_FIELD_SCHOOL, REPORT_FIELD_STUDENT_NAME, REPORT_FIELD_STUDENT_GRADE_LEVEL),
                Arrays.asList(Boolean.TRUE, Boolean.TRUE, Boolean.TRUE), true);
        pdfGrid.beforeTop();
        return m_useDetail ? detailGrid : pdfGrid;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        initReportsFormat();

        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_decimalFormat = new DecimalFormat("#0.0000");
        m_decimalFormat.setRoundingMode(RoundingMode.DOWN);

        m_includeDistrictSummary = (Boolean) getParameter(INPUT_PARAMETER_DISTRICT_SUMMARY);

        if (m_includeDistrictSummary == null || isSchoolContext()) {
            m_includeDistrictSummary = Boolean.FALSE;
        }
        m_summaryOnly = (Boolean) getParameter(INPUT_PARAMETER_SUMMARY_ONLY);
        if (m_summaryOnly == null || isSchoolContext()) {
            m_summaryOnly = Boolean.FALSE;
        }

        m_context = getCurrentContext();

        String month = (String) getParameter(INPUT_PARAMETER_MONTH);
        if (!StringUtils.isEmpty(month)) {
            m_month = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, month);
        }
        if (m_startDate == null) {
            m_startDate = m_context.getStartDate();
        }

        if (m_endDate == null) {
            m_endDate = m_context.getEndDate();
        }
        m_includeEarlyGraduates = (Boolean) getParameter(INPUT_INCL_EARLY_GRADUATES);
        m_includeIEA = (Boolean) getParameter(INPUT_INCL_IEA);
        addParameter(INPUT_INCL_EARLY_GRADUATES, m_includeEarlyGraduates);
        addParameter(INPUT_INCL_IEA, m_includeIEA);
        loadStudentProgramParticipationMap();
        loadEnrollmentCodes();
        if (m_month != null) {
            m_periodHelper = new TNReportingPeriodHelper(getOrganization(), m_context, m_month, getBroker());
        }

    }

    /**
     * Calculate district totals.
     *
     * @param studentDistrictOptionsMembership Map<Integer,Map<String,Double>>
     * @param studentOptionsMembership Map<Integer,Map<String,Double>>
     */
    private void calculateDistrictTotals(Map<Integer, Map<String, Double>> studentDistrictOptionsMembership,
                                         Map<Integer, Map<String, Double>> studentOptionsMembership) {
        for (Integer optionNo : studentOptionsMembership.keySet()) {
            double newPrimaryValue =
                    studentOptionsMembership.get(optionNo).get(DATASRC_PRIMARY_PROGRAM_KEY).doubleValue() +
                            studentDistrictOptionsMembership.get(optionNo).get(DATASRC_PRIMARY_PROGRAM_KEY)
                                    .doubleValue();
            studentDistrictOptionsMembership.get(optionNo).put(DATASRC_PRIMARY_PROGRAM_KEY,
                    Double.valueOf(newPrimaryValue));

            double newSecondaryValue =
                    studentOptionsMembership.get(optionNo).get(DATASRC_SECONDARY_PROGRAM_KEY).doubleValue() +
                            studentDistrictOptionsMembership.get(optionNo).get(DATASRC_SECONDARY_PROGRAM_KEY)
                                    .doubleValue();
            studentDistrictOptionsMembership.get(optionNo).put(DATASRC_SECONDARY_PROGRAM_KEY,
                    Double.valueOf(newSecondaryValue));

            double newTotalValue = studentOptionsMembership.get(optionNo).get(DATASRC_TOTAL_PROGRAM_KEY).doubleValue() +
                    studentDistrictOptionsMembership.get(optionNo).get(DATASRC_TOTAL_PROGRAM_KEY).doubleValue();
            studentDistrictOptionsMembership.get(optionNo).put(DATASRC_TOTAL_PROGRAM_KEY,
                    Double.valueOf(newTotalValue));
        }

    }

    /**
     * Calculate total values for option membership data structure.
     *
     * @param studentOptionsMembership Map<Integer,Map<String,Double>>
     */
    private void calculateTotals(Map<Integer, Map<String, Double>> studentOptionsMembership) {
        double primaryProgramTotal = 0;
        double secondaryProgramTotal = 0;
        for (Integer optionNo : studentOptionsMembership.keySet()) {
            double primaryProgram =
                    studentOptionsMembership.get(optionNo).get(DATASRC_PRIMARY_PROGRAM_KEY).doubleValue();
            double secnodaryPorgram =
                    studentOptionsMembership.get(optionNo).get(DATASRC_SECONDARY_PROGRAM_KEY).doubleValue();
            primaryProgramTotal += primaryProgram;
            secondaryProgramTotal += secnodaryPorgram;
            studentOptionsMembership.get(optionNo).put(DATASRC_TOTAL_PROGRAM_KEY,
                    Double.valueOf(primaryProgram + secnodaryPorgram));
        }
        studentOptionsMembership.get(Integer.valueOf(OPTIONS_GRAND_TOTAL_INDEX)).put(DATASRC_PRIMARY_PROGRAM_KEY,
                Double.valueOf(primaryProgramTotal));
        studentOptionsMembership.get(Integer.valueOf(OPTIONS_GRAND_TOTAL_INDEX)).put(DATASRC_SECONDARY_PROGRAM_KEY,
                Double.valueOf(secondaryProgramTotal));
        studentOptionsMembership.get(Integer.valueOf(OPTIONS_GRAND_TOTAL_INDEX)).put(DATASRC_TOTAL_PROGRAM_KEY,
                Double.valueOf(primaryProgramTotal +
                        secondaryProgramTotal));
    }

    /**
     * Initialize options membership data structure with initial values.
     *
     * @return Map
     */
    private Map<Integer, Map<String, Double>> getDefaultOptionsMembership() {
        Map<Integer, Map<String, Double>> defaultOptionsMembership = new LinkedHashMap<Integer, Map<String, Double>>();

        for (int i = OPTIONS_START_VALUE; i <= OPTIONS_END_VALUE; i++) {
            Integer index = Integer.valueOf(i);
            defaultOptionsMembership.put(index, getDefaultOptionValues());
        }
        defaultOptionsMembership.put(Integer.valueOf(OPTIONS_GRAND_TOTAL_INDEX), getDefaultOptionValues());

        return defaultOptionsMembership;
    }

    /**
     * Return default option values (0 by default).
     *
     * @return Map
     */
    private Map<String, Double> getDefaultOptionValues() {
        Map<String, Double> defaultOptionValues = new HashMap<String, Double>();
        defaultOptionValues.put(DATASRC_PRIMARY_PROGRAM_KEY, Double.valueOf(0));
        defaultOptionValues.put(DATASRC_SECONDARY_PROGRAM_KEY, Double.valueOf(0));
        defaultOptionValues.put(DATASRC_TOTAL_PROGRAM_KEY, Double.valueOf(0));

        return defaultOptionValues;
    }

    /**
     * Return in-session days from start date to end date.
     *
     * @param calendarDays Set<PlainDate>
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return Set<PlainDate>
     */
    private NavigableSet<PlainDate> getCalendarDays(Set<PlainDate> calendarDays,
                                                    PlainDate startDate,
                                                    PlainDate endDate) {
        NavigableSet<PlainDate> dates = new TreeSet();
        for (PlainDate day : calendarDays) {
            if ((startDate == null || !day.before(startDate))
                    && (endDate == null || !day.after(endDate))) {
                dates.add(day);
            }
        }
        return dates;
    }

    /**
     * Return collection of schools.
     *
     * @return Collection<SisSchool> collection of schools
     */
    private Collection<SisSchool> getSchools() {
        Collection<SisSchool> schools = null;
        Object objIsAllSchools = getParameter(INPUT_PARAMETER_ALL_SCHOOLS);
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
            Object objSchools = getParameter(INPUT_PARAMETER_SCHOOLS);
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

    private void handleStudentMembershipType(SisSchool school,
                                             SisStudent student,
                                             String stdType,
                                             Map<Integer, Map<String, Double>> defaultOptionsMembership,
                                             Set<PlainDate> daysInSession,
                                             StudentProgramParticipation program,
                                             NavigableSet<PlainDate> memberDays,
                                             ReportDataGrid detailGrid,
                                             boolean included) {
        double averageMembershipDays = 0;
        if (daysInSession != null) {
            averageMembershipDays = (double) memberDays.size() / daysInSession.size();
        }

        if (averageMembershipDays > 0.0) {
            // put appropriate value in report
            ReferenceCode rcd = m_refCodeMap.get(program.getProgramCode());
            String stateCode = rcd == null ? null : rcd.getStateCode();
            String iepType = (String) program.getFieldValueByAlias(ALIAS_IEP_TYPE);
            Integer optionNo = Integer.valueOf(0);
            if (stateCode != null) {
                try {
                    optionNo = Integer.valueOf(Integer.parseInt(stateCode));
                } catch (NumberFormatException e) {
                    // leave default at 0
                }
            }

            if (optionNo.intValue() >= OPTIONS_START_VALUE && optionNo.intValue() <= OPTIONS_END_VALUE) {
                Double totalADM = null;
                detailGrid.append();
                detailGrid.set(REPORT_FIELD_TYPE_STD, stdType);
                if (IEP_TYPE_PRIMARY.equals(iepType)) {
                    if (included) {
                        double optionValue = defaultOptionsMembership.get(optionNo)
                                .get(DATASRC_PRIMARY_PROGRAM_KEY).doubleValue();
                        defaultOptionsMembership.get(optionNo).put(DATASRC_PRIMARY_PROGRAM_KEY,
                                totalADM = Double.valueOf(optionValue + averageMembershipDays));
                    }

                    detailGrid.set(REPORT_FIELD_PRIMARY_OPTION_ADM,
                            Double.valueOf(averageMembershipDays));

                } else if (IEP_TYPE_SECONDARY.equals(iepType)) {
                    if (included) {
                        double optionValue = defaultOptionsMembership.get(optionNo)
                                .get(DATASRC_SECONDARY_PROGRAM_KEY).doubleValue();
                        defaultOptionsMembership.get(optionNo).put(DATASRC_SECONDARY_PROGRAM_KEY,
                                totalADM = Double.valueOf(optionValue + averageMembershipDays));
                    }

                    detailGrid.set(REPORT_FIELD_SECONDARY_OPTION_ADM,
                            Double.valueOf(averageMembershipDays));
                }

                detailGrid.set(REPORT_FIELD_SCHOOL, school.getName());
                detailGrid.set(REPORT_FIELD_STUDENT_NAME, student.getNameView());
                detailGrid.set(REPORT_FIELD_STUDENT_SASID,
                        student.getFieldValueByAlias(ALIAS_EIS_STATE_ID));
                detailGrid.set(REPORT_FIELD_STUDENT_LASID, student.getLocalId());

                PlainDate startDate = m_month != null ? m_periodHelper.getDateBegin(school.getOid())
                        : m_startDate;
                PlainDate endDate =
                        m_month != null ? m_periodHelper.getDateEnd(school.getOid()) : m_endDate;

                detailGrid.set(REPORT_FIELD_STUDENT_GRADE_LEVEL, m_data.m_helper
                        .getGradeLevelByDates(student, startDate, endDate).getCode());
                detailGrid.set(REPORT_FIELD_OPTION_NAME,
                        REPORT_OPTIONS_COLUMN_DEFAULT_TEXT + optionNo);
                detailGrid.set(REPORT_FIELD_PRIMARY_SEC_FLAG, iepType);

                detailGrid.set(REPORT_FIELD_OPTION_BEGIN_DATE, memberDays.iterator().next());
                detailGrid.set(REPORT_FIELD_OPTION_END_DATE, memberDays.descendingIterator().next());
                detailGrid.set(REPORT_FIELD_TOTAL_ADM, totalADM);
                detailGrid.set(REPORT_FIELD_STUDENT_GENDER, student.getPerson().getGenderCode());
            }
        }
    }

    /**
     * Return calculated program membership statistic AND prepare detail grid.
     *
     * @param school SisSchool
     * @param students QueryIterator
     * @param detailGrid ReportDataGrid
     * @return Map
     */
    private Map<Integer, Map<String, Double>> handleStudentProgramMembership(SisSchool school,
                                                                             QueryIterator students,
                                                                             ReportDataGrid detailGrid) {
        if (m_month != null) {
            m_startDate = m_periodHelper.getDateBegin(school.getOid());
            m_endDate = m_periodHelper.getDateEnd(school.getOid());
            m_periodHelper = new TNReportingPeriodHelper(getOrganization(), m_context, m_month, getBroker());
        }
        Map<Integer, Map<String, Double>> defaultOptionsMembership = getDefaultOptionsMembership();
        while (students.hasNext()) {
            SisStudent student = (SisStudent) students.next();
            Set<PlainDate> daysInSession = m_data.getDaysInSession(school,
                    (String) m_data.m_tnEnrHelper.getStudentValueByBeanPath(student, SisStudent.COL_CALENDAR_CODE));
            ArrayList<PlainDate> sortedDays = new ArrayList<>();
            sortedDays.addAll(daysInSession);
            Collections.sort(sortedDays);
            Set<PlainDate> ieaDays = new HashSet();
            if (m_ieaStdPgms.get(student.getOid()) != null && !m_ieaStdPgms.get(student.getOid()).isEmpty()) {
                for (StudentProgramParticipation program : m_ieaStdPgms.get(student.getOid())) {
                    PlainDate programStartDate = (program.getStartDate() != null) ? (program.getStartDate())
                            : (m_startDate);
                    PlainDate programEndDate = (program.getEndDate() != null) ? (program.getEndDate())
                            : (m_endDate);
                    ieaDays.addAll(getCalendarDays(daysInSession, programStartDate, programEndDate));
                }
            }

            List<TNStudentEnrollmentSpan> spans = m_data.getTNStudentEnrollmentSpans(student, true);
            for (TNStudentEnrollmentSpan span : spans) {
                boolean withdrawFirstInstDay = false;
                PlainDate participationStartDate =
                        (span.getFirstActiveDate() != null) ? (span.getFirstActiveDate())
                                : (m_startDate);
                PlainDate participationEndDate = (span.getLastActiveDate() != null) ? (span.getLastActiveDate())
                        : (m_endDate);
                boolean isEarly = isEarlyGraduated(span);
                if (isEarly && m_includeEarlyGraduates && span.getLastActiveDate() != null) {
                    int year = m_context.getSchoolYear();
                    Calendar cal = Calendar.getInstance();
                    cal.set(year, Calendar.JANUARY, 1);
                    Date firstYearDay = cal.getTime();
                    PlainDate firstInstrDate = null;
                    if (m_month == null) {
                        for (PlainDate date : sortedDays) {
                            if (!date.before(firstYearDay)) {
                                firstInstrDate = date;
                                break;
                            }
                        }
                    } else {
                        Set<PlainDate> daysInSessionForMonth =
                                m_data.getCalendarDays(school, SisStudent.COL_CALENDAR_CODE);
                        ArrayList<PlainDate> sortedDaysForMonth = new ArrayList<>();
                        sortedDaysForMonth.addAll(daysInSessionForMonth);
                        Collections.sort(sortedDaysForMonth);
                        for (PlainDate date : sortedDaysForMonth) {
                            if (!date.before(firstYearDay)) {
                                firstInstrDate = date;
                                break;
                            }
                        }
                    }
                    if (span.getFirstInactiveEnrollment().getEnrollmentDate().equals(firstInstrDate)) {
                        withdrawFirstInstDay = true;
                    }
                }
                Set<PlainDate> spanDays = getCalendarDays(daysInSession, participationStartDate, participationEndDate);
                if (!spanDays.isEmpty() || isEarly) {
                    if (m_studentProgramParticipationMap.get(student.getOid()) != null
                            && school.equals(span.getSchool())) {
                        for (StudentProgramParticipation program : m_studentProgramParticipationMap
                                .get(student.getOid())) {
                            PlainDate programStartDate = (program.getStartDate() != null) ? (program.getStartDate())
                                    : (m_startDate);
                            PlainDate programEndDate = (program.getEndDate() != null) ? (program.getEndDate())
                                    : (m_endDate);
                            NavigableSet<PlainDate> pgmDaysForEarly = null;
                            if (withdrawFirstInstDay && span.getLastActiveDate() != null
                                    && !programEndDate.before(span.getLastActiveDate())) {
                                pgmDaysForEarly =
                                        getCalendarDays(daysInSession, programStartDate, m_endDate);
                            }
                            NavigableSet<PlainDate> pgmDays =
                                    getCalendarDays(daysInSession, programStartDate, programEndDate);

                            // process early graduates
                            if (isEarly && span.getLastActiveDate() != null && pgmDaysForEarly != null) {
                                NavigableSet<PlainDate> earlyDays =
                                        getCalendarDays(daysInSession, span.getLastActiveDate(), m_endDate);
                                earlyDays.remove(span.getLastActiveDate());
                                earlyDays.retainAll(pgmDaysForEarly);
                                if (!earlyDays.isEmpty()) {
                                    handleStudentMembershipType(school, student, STD_TYPE_EARLY,
                                            defaultOptionsMembership,
                                            daysInSession, program, earlyDays, detailGrid,
                                            m_includeEarlyGraduates.booleanValue());
                                }
                            }
                            // limit pgmDays to span after processing early graduates
                            pgmDays.retainAll(spanDays);
                            // separate IEA days
                            NavigableSet<PlainDate> pgmIeaDays = null;
                            if (!ieaDays.isEmpty()) {
                                pgmIeaDays = new TreeSet();
                                pgmIeaDays.addAll(pgmDays);
                                pgmIeaDays.retainAll(ieaDays);
                                pgmDays.removeAll(ieaDays);
                            }

                            // process regular days
                            if (!pgmDays.isEmpty()) {
                                handleStudentMembershipType(school, student, STD_TYPE_REGULAR, defaultOptionsMembership,
                                        daysInSession, program, pgmDays, detailGrid, true);
                            }

                            // process IEA days
                            if (pgmIeaDays != null && !pgmIeaDays.isEmpty()) {
                                handleStudentMembershipType(school, student, STD_TYPE_IEA, defaultOptionsMembership,
                                        daysInSession, program, pgmIeaDays, detailGrid, m_includeIEA.booleanValue());
                            }
                        }
                    }
                }
            }
        }
        return defaultOptionsMembership;
    }

    /**
     * Preparing criteria for selecting reference codes.
     *
     * @param validProgramCodes Collection<String>
     * @return X 2 criteria
     */
    private X2Criteria getStudentProgramParticipationCriteria(Collection<String> validProgramCodes) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty prop = new ModelProperty(StudentProgramParticipation.class,
                StudentProgramParticipation.COL_PROGRAM_CODE, getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());

        X2Criteria categoryCrit = new X2Criteria();
        categoryCrit.addEqualTo(ReferenceCode.COL_CATEGORY, VALID_PROGRAM_CATEGORY);
        categoryCrit.addIn(ReferenceCode.COL_STATE_CODE, validProgramCodes);
        if (m_includeIEA.booleanValue()) {
            X2Criteria categoryOrCrit = new X2Criteria();
            categoryOrCrit.addEqualTo(ReferenceCode.COL_CATEGORY, PGM_CATEGORY_STD_CLASS);
            categoryOrCrit.addEqualTo(ReferenceCode.COL_STATE_CODE, PGM_CODE_STD_CLASS);
            categoryCrit.addOrCriteria(categoryOrCrit);
        }
        criteria.addAndCriteria(categoryCrit);
        return criteria;
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
     * Returns true if student is early graduated, otherwise false.
     *
     * @param span TNStudentEnrollmentSpan
     * @return
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
     * Load map of IEA PGMS keyed on STD oid.
     */
    private void loadIEAStdPgmsMap() {
        ModelProperty prop =
                new ModelProperty(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE,
                        getBroker().getPersistenceKey());
        DataDictionaryField field = m_dictionary.findDataDictionaryField(prop.getFieldId());
        if (field.hasReferenceTable()) {
            String referenceTableOid = field.getReferenceTableOid();
            X2Criteria rcdCriteria = new X2Criteria();
            rcdCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            rcdCriteria.addEqualTo(ReferenceCode.COL_STATE_CODE, PGM_CODE_STD_CLASS);
            rcdCriteria.addEqualTo(ReferenceCode.COL_CATEGORY, PGM_CATEGORY_STD_CLASS);
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, rcdCriteria);
            if (getBroker().getCount(query) > 0) {
                Collection<String> ieaRefCodes =
                        getBroker().getGroupedCollectionByQuery(query, ReferenceCode.COL_CODE, 512).keySet();

                X2Criteria criteria = new X2Criteria();
                criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_endDate);
                X2Criteria endDate1Criteria = new X2Criteria();
                endDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
                X2Criteria endDate2Criteria = new X2Criteria();
                endDate2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_startDate);
                endDate1Criteria.addOrCriteria(endDate2Criteria);
                criteria.addAndCriteria(endDate1Criteria);
                if (!ieaRefCodes.isEmpty()) {
                    criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, ieaRefCodes);
                } else {
                    criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, "___dummy___");
                }
                QueryByCriteria pgmQuery = new QueryByCriteria(StudentProgramParticipation.class, criteria);
                if (getBroker().getCount(pgmQuery) > 0) {
                    m_ieaStdPgms.putAll(getBroker().getGroupedCollectionByQuery(pgmQuery,
                            StudentProgramParticipation.COL_STUDENT_OID, 1024));
                }
            }
        }
    }

    /**
     * Load student program participation map.
     */
    private void loadStudentProgramParticipationMap() {
        X2Criteria refCodeCriteria = getStudentProgramParticipationCriteria(Arrays.asList(VALID_PROGRAM_CODES));
        m_refCodeMap = getBroker().getMapByQuery(new QueryByCriteria(ReferenceCode.class, refCodeCriteria),
                ReferenceCode.COL_CODE, 100);
        SubQuery programStateRefCodeSubQuery =
                new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, refCodeCriteria);

        // valid program criteria
        X2Criteria studentProgramCriteria = new X2Criteria();
        studentProgramCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, programStateRefCodeSubQuery);

        // Start date criteria
        X2Criteria studentProgramEmptyStartDateCriteria = new X2Criteria();
        studentProgramEmptyStartDateCriteria.addIsNull(StudentProgramParticipation.COL_START_DATE);

        X2Criteria studentProgramStartDateCriteria = new X2Criteria();
        studentProgramStartDateCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_endDate);
        studentProgramStartDateCriteria.addOrCriteria(studentProgramEmptyStartDateCriteria);

        // End date criteria
        X2Criteria studentProgramEmptyEndDateCriteria = new X2Criteria();
        studentProgramEmptyEndDateCriteria.addIsNull(StudentProgramParticipation.COL_END_DATE);

        X2Criteria studentProgramEndDateCriteria = new X2Criteria();
        studentProgramEndDateCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_startDate);
        studentProgramEndDateCriteria.addOrCriteria(studentProgramEmptyEndDateCriteria);

        studentProgramCriteria.addAndCriteria(studentProgramStartDateCriteria);
        studentProgramCriteria.addAndCriteria(studentProgramEndDateCriteria);

        QueryByCriteria studentProgrammParticipationQuery =
                new QueryByCriteria(StudentProgramParticipation.class, studentProgramCriteria);

        studentProgrammParticipationQuery.addOrderByAscending(StudentProgramParticipation.COL_STUDENT_OID);
        studentProgrammParticipationQuery.addOrderByAscending(StudentProgramParticipation.COL_PROGRAM_CODE);

        m_studentProgramParticipationMap =
                getBroker().getGroupedCollectionByQuery(studentProgrammParticipationQuery,
                        StudentProgramParticipation.COL_STUDENT_OID,
                        500);
        loadIEAStdPgmsMap();
    }

    /**
     * Populate grid.
     *
     * @param grid ReportDataGrid
     * @param studentOptionsMembership Map<Integer,Map<String,Double>>
     * @param school SisSchool
     * @param orgName String
     */
    private void populateGrid(ReportDataGrid grid,
                              Map<Integer, Map<String, Double>> studentOptionsMembership,
                              SisSchool school,
                              String orgName) {

        PlainDate startDate = null;
        PlainDate endDate = null;
        if (school != null && m_periodHelper != null) {
            startDate = m_periodHelper.getDateBegin(school.getOid());
            endDate = m_periodHelper.getDateEnd(school.getOid());
        }
        for (Integer optionNo : studentOptionsMembership.keySet()) {
            grid.append();
            grid.set(REPORT_FIELD_SCHOOL, school);
            grid.set(REPORT_FIELD_START_DATE, startDate);
            grid.set(REPORT_FIELD_END_DATE, endDate);
            grid.set(REPORT_FIELD_ORG_NAME, orgName);
            if (optionNo.intValue() == OPTIONS_GRAND_TOTAL_INDEX) {
                grid.set(REPORT_FIELD_OPTION_NAME, REPORT_OPTIONS_GRAND_TOTAL_TEXT);
            } else {
                grid.set(REPORT_FIELD_OPTION_NAME, REPORT_OPTIONS_COLUMN_DEFAULT_TEXT + optionNo);
            }
            grid.set(REPORT_FIELD_PRIMARY_OPTION,
                    m_decimalFormat.format(studentOptionsMembership.get(optionNo).get(DATASRC_PRIMARY_PROGRAM_KEY)));
            grid.set(REPORT_FIELD_SECONDARY_OPTION,
                    m_decimalFormat.format(studentOptionsMembership.get(optionNo).get(DATASRC_SECONDARY_PROGRAM_KEY)));
            grid.set(REPORT_FIELD_OPTIONS_TOTAL,
                    m_decimalFormat.format(studentOptionsMembership.get(optionNo).get(DATASRC_TOTAL_PROGRAM_KEY)));
        }
    }
}
