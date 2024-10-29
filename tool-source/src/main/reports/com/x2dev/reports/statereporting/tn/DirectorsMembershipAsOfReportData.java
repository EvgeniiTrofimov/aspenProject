/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentEnrollmentSpan;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.procedures.statereporting.tn.TNStateReportData;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Query;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Director's Membership As Of" report.
 *
 * @author X2 Development Corporation
 */
public class DirectorsMembershipAsOfReportData extends ReportJavaSourceNet {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 

    /**
     * The Class EnrollmentStatistics.
     */
    class EnrollmentStatistics extends TNStateReportData {
        protected static final String PARAM_REPORT_PERIOD = "reportPeriod";

        private TNEnrollmentHelper m_tnEnrHelper;
        private TNStudentHistoryHelper m_helper;

        /**
         * Returns grade level reference code that had student for date range.
         *
         * @param student SisStudent
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @return Reference code
         */
        public ReferenceCode getGradeLevelByDates(SisStudent student, PlainDate startDate, PlainDate endDate) {
            return m_helper.getGradeLevelByDates(student, startDate, endDate);
        }

        /**
         * Return the current student criteria.
         *
         * @return X 2 criteria
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
         * Return Race collection for the student.
         *
         * @param student Student
         * @return Collection
         */
        public Collection<Race> getStudentRace(Student student) {
            return m_helper.getPersonRaceMap().get(student.getPersonOid());
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
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.FALSE);
        }
    }

    private static final String ALIAS_SKL_EIS_STATE_ID = "DOE EIS STATE ID";
    private static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL STATE ID";
    private static final String ALIAS_DOE_INSTR_SERVICE_TYPE = "DOE INSTR SERVICE TYPE";
    private static final String ALIAS_DAY_EVENT2 = "DOE DAY EVENT TYPE 2";
    private static final String ALIAS_DAY_EVENT3 = "DOE DAY EVENT TYPE 3";

    private static final String ATTENDANCE_START_DATE = "AS";
    private static final String CONTEXT_START_DATE = "CS";

    private static final int GRADE_LEVEL_NUMERIC_TOTAL_K_12 = 100;

    private static final int GRADE_FILTER_MIN_LEVEL = 0;
    private static final int GRADE_FILTER_MAX_LEVEL = 12;

    private static final String INPUT_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_REPORT_PERIOD = "reportPeriod";
    private static final String INPUT_REPORT_DATE = "reportDate";
    private static final String INPUT_REPORT_ID_CSV = "subreportIdCSVVersion";
    private static final String INPUT_REPORT_ID_PDF = "subreportIdPDFVersion";
    private static final String INPUT_REPORT_MODE = "reportMode";
    private static final String INPUT_SCHOOLS = "schoolOids";
    private static final String INPUT_SUMMARY_ONLY = "summaryOnly";
    private static final String INPUT_DISTRICT_SUMMARY = "includeDistrictSummary";

    private static final String REPORT_PARAMETER_USER = "user";
    private static final String REPORT_PARAMETER_PERIOD = "period";

    private static final String REPORT_FIELD_CURRENT_MEMB_LEVEL = "currentMembLevel";
    private static final String REPORT_FIELD_DATE_START = "dateStart";
    private static final String REPORT_FIELD_DATE_END = "dateEnd";
    private static final String REPORT_FIELD_END_DATE = "endDate";
    private static final String REPORT_FIELD_GENDER = "gender";
    private static final String REPORT_FIELD_GRADE_K = "gradeK";
    private static final String REPORT_FIELD_GRADE_LEVEL = "gradeLevel";
    private static final String REPORT_FIELD_GRADE_TOTAL = "gradeTotal";
    private static final String REPORT_FIELD_LASID = "LASID";
    private static final String REPORT_FIELD_ORG_NAME = "orgName";
    private static final String REPORT_FIELD_RACE = "race";
    private static final String REPORT_FIELD_RACE_CODE = "raceCode";
    private static final String REPORT_FIELD_REPORT_TYPE = "reportType";
    private static final String REPORT_FIELD_SASID = "SASID";
    private static final String REPORT_FIELD_SCHOOL = "school";
    private static final String REPORT_FIELD_SCHOOL_NAME = "schoolName";
    private static final String REPORT_FIELD_START_DATE = "startDate";
    private static final String REPORT_FIELD_STUDENT_NAME = "studentName";

    private static final String RACE_TYPE_HISPANIC_ALL_DESCRIPTION = "Hispanic-All Races";
    private static final String RACE_TYPE_NON_HISPANIC_WHITE_DESCRIPTION = "Non-Hisp. White";
    private static final String RACE_TYPE_NON_HISPANIC_BLACK_DESCRIPTION = "Non-Hisp. Black";
    private static final String RACE_TYPE_NON_HISPANIC_ASIAN_DESCRIPTION = "Non-Hisp. Asian";
    private static final String RACE_TYPE_NON_HISPANIC_INDIAN_DESCRIPTION = "Non-Hisp. Indian";
    private static final String RACE_TYPE_NON_HISPANIC_PACIFIC_DESCRIPTION = "Non-Hisp. Pacific";
    private static final String RACE_TYPE_2_OR_MORE_DESCRIPTION = "2 or More";
    private static final String RACE_TOTAL_DESCRIPTION = "Total";

    private static final String RACE_CODE_BLACK = "B";
    private static final String RACE_CODE_WHITE = "W";
    private static final String RACE_CODE_ASIAN = "A";
    private static final String RACE_CODE_INDIAN = "I";
    private static final String RACE_CODE_PACIFIC = "P";

    private static final String SERVICE_TYPE_SECONDARY = "S";

    private static final String STAFF_SCHOOL_CODE = "9999";

    private static final String STATE_CODE_GENDER_MALE = "M";
    private static final String STATE_CODE_GENDER_FEMALE = "F";
    private static final String STATE_CODE_TOTAL = "T";

    protected PlainDate m_startDate = null;
    protected PlainDate m_endDate = null;

    private Map<String, PlainDate> m_attendanceDaysMap;
    private DistrictSchoolYearContext m_context;
    private EnrollmentStatistics m_data;
    private DataDictionary m_dictionary;
    private Map<String, Map<String, Map<Integer, Integer>>> m_districtMembershipData;
    private Set<String> m_districtStudentReported;
    private String m_fieldEvent2;
    private String m_fieldEvent3;
    private String m_fieldInstrServiceType;
    private Boolean m_includeDistrictSummary;
    private TNReportingPeriodHelper m_periodHelper;
    private Map<String, ReferenceCode> m_referenceGenderCodeMap;
    private Map<String, ReferenceCode> m_referenceRaceCodeMap;
    private PlainDate m_reportDate;
    private Integer m_reportMode;
    private Boolean m_summaryOnly;
    private boolean m_useDetail;

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        initReportsFormat();
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        initDictionaryValues();
        loadGenderCodes();
        loadRaceCodes();

        m_reportMode = getParameter(INPUT_REPORT_MODE) != null ? (Integer) getParameter(INPUT_REPORT_MODE)
                : Integer.valueOf(0);

        m_context = getCurrentContext();
        m_startDate = getCalendarStartDate(m_context);

        PlainDate lastDate = null;

        Collection<DistrictCalendar> calendars = m_context.getCalendars();
        for (DistrictCalendar calendar : calendars) {
            if (calendar.getInSessionIndicator()) {
                if (lastDate == null || lastDate.before(calendar.getDate())) {
                    lastDate = calendar.getDate();
                }
            }
        }
        m_endDate = lastDate;

        // Annual Mode - used by default.
        switch (m_reportMode.intValue()) {
            // Report Date mode
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
                m_periodHelper = new TNReportingPeriodHelper(getOrganization(), m_context, reportPeriod, getBroker());
                addParameter(REPORT_PARAMETER_PERIOD, reportPeriod.getCode());
                break;
        }

        m_includeDistrictSummary = (Boolean) getParameter(INPUT_DISTRICT_SUMMARY);

        if (m_includeDistrictSummary == null || isSchoolContext()) {
            m_includeDistrictSummary = Boolean.FALSE;
        }

        m_summaryOnly = (Boolean) getParameter(INPUT_SUMMARY_ONLY);
        if (m_summaryOnly == null || isSchoolContext()) {
            m_summaryOnly = Boolean.FALSE;
        }


        addParameter(REPORT_PARAMETER_USER, getUser());
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid detailGrid = new ReportDataGrid();
        ReportDataGrid pdfGrid = new ReportDataGrid();

        Collection<SisSchool> schools = getSchools();

        ReportDataGrid districtGrid = new ReportDataGrid();
        if (m_includeDistrictSummary.booleanValue()) {
            m_districtMembershipData = getDefaultMembership();
            m_districtStudentReported = new HashSet();
        }
        for (SisSchool school : schools) {
            if (m_reportMode.intValue() == 1 && m_periodHelper != null) {
                m_startDate = m_periodHelper.getDateBegin(school.getOid());
                m_endDate = m_periodHelper.getDateEnd(school.getOid());
                if (m_startDate == null || m_endDate == null) {
                    String message = "For school " + school.getName() + " is not defined report period with code: " +
                            m_periodHelper.getMonth().getCode();
                    AppGlobals.getLog().log(Level.WARNING, message);
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
            Map<String, Map<String, Map<Integer, Integer>>> membershipData;
            try {
                membershipData = handleMembershipData(school, students, detailGrid);
                calculateTotalDataSet(membershipData);
                calculateColumnTotals(membershipData);
            } finally {
                if (students != null) {
                    students.close();
                }
            }

            ReportDataGrid schoolGrid = new ReportDataGrid();

            populateGrid(schoolGrid, membershipData, school);
            pdfGrid.append(schoolGrid);

        }
        if (m_summaryOnly.booleanValue()) {
            pdfGrid = new ReportDataGrid();
        }
        if (m_includeDistrictSummary.booleanValue()) {
            calculateTotalDataSet(m_districtMembershipData);
            calculateColumnTotals(m_districtMembershipData);
            populateGrid(districtGrid, m_districtMembershipData, null);
            pdfGrid.append(districtGrid);
        }
        pdfGrid.beforeTop();
        detailGrid.beforeTop();
        detailGrid.sort(Arrays.asList(REPORT_FIELD_SCHOOL_NAME, REPORT_FIELD_STUDENT_NAME, REPORT_FIELD_GRADE_LEVEL),
                Arrays.asList(Boolean.TRUE, Boolean.TRUE, Boolean.TRUE), true);
        return m_useDetail ? detailGrid : pdfGrid;
    }

    /**
     * Helper method to add the membership data.
     *
     * @param membershipData Map<String,Map<String,Map<Integer,Integer>>>
     * @param student SisStudent
     * @param span TNStudentEnrollmentSpan
     * @param studentsReported Set<String>
     * @param gradeCode ReferenceCode
     * @param detailGrid ReportDataGrid
     */
    private void addMembershipData(Map<String, Map<String, Map<Integer, Integer>>> membershipData,
                                   SisStudent student,
                                   TNStudentEnrollmentSpan span,
                                   Set<String> studentsReported,
                                   ReferenceCode gradeCode,
                                   ReportDataGrid detailGrid) {
        Integer gradeLevel = prepareGradeLevel(gradeCode);

        String gender = getPersonGender(student.getPerson());

        String raceStateCode = getRaceTypeDescription(student);

        String serviceType = (String) span.getFirstActiveEnrollment().getFieldValueByBeanPath(m_fieldInstrServiceType);
        serviceType = m_data.lookupStateValue(span.getFirstActiveEnrollment().getClass(), m_fieldInstrServiceType,
                serviceType);

        if (gradeLevel != null && gender != null && raceStateCode != null && !gender.isEmpty()
                && !raceStateCode.isEmpty() && !SERVICE_TYPE_SECONDARY.equals(serviceType)) {
            if (!studentsReported.contains(student.getOid())) {
                studentsReported.add(student.getOid());
                Integer currentMembershipLevel = membershipData.get(gender).get(raceStateCode).get(gradeLevel);
                membershipData.get(gender).get(raceStateCode).put(gradeLevel,
                        Integer.valueOf((currentMembershipLevel.intValue() + 1)));
                detailGrid.append();
                detailGrid.set(REPORT_FIELD_SCHOOL_NAME, span.getSchool().getName());
                detailGrid.set(REPORT_FIELD_STUDENT_NAME, student.getNameView());
                detailGrid.set(REPORT_FIELD_LASID, student.getLocalId());
                detailGrid.set(REPORT_FIELD_SASID, student.getFieldValueByAlias(ALIAS_SKL_EIS_STATE_ID));
                detailGrid.set(REPORT_FIELD_GRADE_LEVEL, gradeLevel);
                detailGrid.set(REPORT_FIELD_GENDER, gender);
                detailGrid.set(REPORT_FIELD_RACE_CODE, raceStateCode);
                detailGrid.set(REPORT_FIELD_CURRENT_MEMB_LEVEL,
                        membershipData.get(gender).get(raceStateCode).get(gradeLevel));

                PlainDate attendanceStartDate = getAttendanceStartDate(m_data.getSchool().getOid());
                PlainDate spanStartDate = span.getFirstActiveDate();

                PlainDate startDate = attendanceStartDate.after(spanStartDate) ? attendanceStartDate : spanStartDate;

                detailGrid.set(REPORT_FIELD_START_DATE, startDate);
                detailGrid.set(REPORT_FIELD_END_DATE, span.getLastActiveDate());
            } else {
                System.out.println("Skipped");
            }
            if (m_includeDistrictSummary.booleanValue() && !m_districtStudentReported.contains(student.getOid())) {
                m_districtStudentReported.add(student.getOid());
                Integer currentMembershipLevel =
                        m_districtMembershipData.get(gender).get(raceStateCode).get(gradeLevel);
                m_districtMembershipData.get(gender).get(raceStateCode).put(gradeLevel,
                        Integer.valueOf((currentMembershipLevel.intValue() + 1)));
            }
        }
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
                        Integer.valueOf((totalValueForColumn)));
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
                            Integer.valueOf((currentMembershipValue + genderMembershipValue)));
                }
                membershipData.get(genderCodes).get(raceCodeState).put(Integer.valueOf(GRADE_LEVEL_NUMERIC_TOTAL_K_12),
                        Integer.valueOf((totalByRaceAndGender)));
            }
        }
    }

    /**
     * Loading the attendance start day (day with "AS" event).
     *
     * @param schoolOid String
     * @return Plain date
     */
    private PlainDate getAttendanceStartDate(String schoolOid) {
        if (m_attendanceDaysMap == null) {
            m_attendanceDaysMap = new HashMap<String, PlainDate>();
        }

        PlainDate attendanceStartDate = m_attendanceDaysMap.get(schoolOid);

        if (attendanceStartDate == null) {
            X2Criteria calendarDateCriteria = new X2Criteria();
            calendarDateCriteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + "." +
                    SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_context.getOid());
            calendarDateCriteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                    SchoolCalendar.COL_SCHOOL_OID, schoolOid);

            X2Criteria orCriteria = new X2Criteria();
            X2Criteria orCriteria2 = new X2Criteria();
            X2Criteria orCriteria3 = new X2Criteria();
            orCriteria.addEqualTo(SchoolCalendarDate.COL_SCHEDULE_DAY_TYPE, ATTENDANCE_START_DATE);
            orCriteria2.addEqualTo(m_fieldEvent2, ATTENDANCE_START_DATE);
            orCriteria3.addEqualTo(m_fieldEvent3, ATTENDANCE_START_DATE);
            orCriteria.addOrCriteria(orCriteria2);
            orCriteria.addOrCriteria(orCriteria3);
            calendarDateCriteria.addAndCriteria(orCriteria);
            QueryByCriteria calendarDateQuery = new QueryByCriteria(SchoolCalendarDate.class, calendarDateCriteria);
            QueryIterator iter = getBroker().getIteratorByQuery(calendarDateQuery);

            PlainDate startDate = null;
            try {
                while (iter.hasNext()) {
                    SchoolCalendarDate item = (SchoolCalendarDate) iter.next();
                    PlainDate date = item.getDate();
                    if (date != null && (startDate == null || date.after(startDate))) {
                        startDate = date;
                    }
                }
            } finally {
                iter.close();
            }
            if (startDate == null) {
                startDate = m_context.getStartDate();
            }

            attendanceStartDate = startDate;
            m_attendanceDaysMap.put(schoolOid, attendanceStartDate);
        }


        return attendanceStartDate;
    }

    /**
     * Loading the start day of context (day with "CS" event).
     *
     * @param context DistrictSchoolYearContext
     * @return Plain date
     */
    private PlainDate getCalendarStartDate(DistrictSchoolYearContext context) {
        X2Criteria calendarDateCriteria = new X2Criteria();
        calendarDateCriteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + "." +
                SchoolCalendar.COL_DISTRICT_CONTEXT_OID, context.getOid());

        X2Criteria orCriteria = new X2Criteria();
        X2Criteria orCriteria2 = new X2Criteria();
        X2Criteria orCriteria3 = new X2Criteria();
        orCriteria.addEqualTo(SchoolCalendarDate.COL_SCHEDULE_DAY_TYPE, CONTEXT_START_DATE);
        orCriteria2.addEqualTo(m_fieldEvent2, CONTEXT_START_DATE);
        orCriteria3.addEqualTo(m_fieldEvent3, CONTEXT_START_DATE);
        orCriteria.addOrCriteria(orCriteria2);
        orCriteria.addOrCriteria(orCriteria3);
        calendarDateCriteria.addAndCriteria(orCriteria);
        QueryByCriteria calendarDateQuery = new QueryByCriteria(SchoolCalendarDate.class, calendarDateCriteria);
        QueryIterator iter = getBroker().getIteratorByQuery(calendarDateQuery);

        PlainDate startDate = null;
        try {
            while (iter.hasNext()) {
                SchoolCalendarDate item = (SchoolCalendarDate) iter.next();
                PlainDate date = item.getDate();
                if (date != null && (startDate == null || date.after(startDate))) {
                    startDate = date;
                }
            }
        } finally {
            iter.close();
        }

        if (startDate == null) {
            return context.getStartDate();
        }
        return startDate;
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
        String gender = "";
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
        String race = "";
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
        String raceTypeDescription = "";

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
        boolean isAllSchools = objIsAllSchools == null ? false : ((Boolean) objIsAllSchools).booleanValue();
        if (isAllSchools) {
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
     * Loads field names for aliases.
     */
    private void initDictionaryValues() {
        DataDictionary dd = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_fieldInstrServiceType = dd.findDataDictionaryFieldByAlias(ALIAS_DOE_INSTR_SERVICE_TYPE).getJavaName();
        m_fieldEvent2 = dd.findDataDictionaryFieldByAlias(ALIAS_DAY_EVENT2).getJavaName();
        m_fieldEvent3 = dd.findDataDictionaryFieldByAlias(ALIAS_DAY_EVENT3).getJavaName();
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
     * Transform membershipData in grid.
     *
     * @param grid ReportDataGrid
     * @param membershipData Map<String,Map<String,Map<Integer,Integer>>>
     * @param school SisSchool
     */
    private void populateGrid(ReportDataGrid grid,
                              Map<String, Map<String, Map<Integer, Integer>>> membershipData,
                              SisSchool school) {

        PlainDate dateStart = m_startDate;
        PlainDate dateEnd = m_endDate;
        for (String tableType : membershipData.keySet()) {
            Map<String, Map<Integer, Integer>> raceDataSet = membershipData.get(tableType);

            for (String race : raceDataSet.keySet()) {
                grid.append();
                grid.set(REPORT_FIELD_REPORT_TYPE, tableType);
                grid.set(REPORT_FIELD_DATE_START, dateStart);
                grid.set(REPORT_FIELD_DATE_END, dateEnd);
                if (school != null) {
                    grid.set(REPORT_FIELD_SCHOOL, school);
                    grid.set(REPORT_FIELD_ORG_NAME, school.getName());
                } else {
                    grid.set(REPORT_FIELD_ORG_NAME, getOrganization().getName());
                }
                grid.set(REPORT_FIELD_RACE, race);

                Map<Integer, Integer> gradeDataSet = raceDataSet.get(race);

                for (Integer gradeCode : gradeDataSet.keySet()) {
                    switch (gradeCode.intValue()) {
                        case GRADE_FILTER_MIN_LEVEL:
                            grid.set(REPORT_FIELD_GRADE_K, gradeDataSet.get(gradeCode));
                            break;
                        case GRADE_LEVEL_NUMERIC_TOTAL_K_12:
                            grid.set(REPORT_FIELD_GRADE_TOTAL, gradeDataSet.get(gradeCode));
                            break;
                        default:
                            grid.set("grade" + gradeCode.toString(), gradeDataSet.get(gradeCode));
                            break;
                    }
                }
            }
        }
    }

    /**
     * Fill in student membership data based on information about gender/race/grade level and
     * populate detail report.
     *
     * @param school SisSchool
     * @param students QueryIterator
     * @param detailGrid ReportDataGrid
     * @return Map<String, Map<String, Map<Integer, Integer>>>
     */
    private Map<String, Map<String, Map<Integer, Integer>>> handleMembershipData(SisSchool school,
                                                                                 QueryIterator students,
                                                                                 ReportDataGrid detailGrid) {
        Map<String, Map<String, Map<Integer, Integer>>> membershipData = getDefaultMembership();

        while (students.hasNext()) {
            SisStudent student = (SisStudent) students.next();
            Set<String> studentsReported = new HashSet();
            List<TNStudentEnrollmentSpan> spans = m_data.getTNStudentEnrollmentSpans(student, false);

            TNStudentEnrollmentSpan firstSpan = null;
            TNStudentEnrollmentSpan nextSpan = null;

            ReferenceCode gradeCode = m_data.getGradeLevelByDates(student, m_startDate, m_endDate);

            for (TNStudentEnrollmentSpan span : spans) {
                if (firstSpan != null && nextSpan != null) // The previous 2 spans are E-S and S-W
                {
                    // The check for the "End of Month Membership"
                    if (!firstSpan.getFirstActiveEnrollment().getEnrollmentDate().after(m_endDate) &&
                            !nextSpan.getLastActiveDate().before(m_endDate) &&
                            school.equals(firstSpan.getSchool())) {
                        addMembershipData(membershipData, student, firstSpan, studentsReported, gradeCode, detailGrid);
                    }

                    // Reset the span or add the current span as the first span
                    firstSpan = span.getFirstActiveEnrollment() != null
                            && StudentEnrollment.ENTRY.equals(span.getFirstActiveEnrollment().getEnrollmentType())
                                    ? span : null;
                    nextSpan = null;
                } else {
                    if (span.getFirstActiveEnrollment() != null
                            && StudentEnrollment.ENTRY.equals(span.getFirstActiveEnrollment().getEnrollmentType())) {
                        firstSpan = span;

                        if (firstSpan.getFirstInactiveEnrollment() != null && StudentEnrollment.WITHDRAWAL
                                .equals(firstSpan.getFirstInactiveEnrollment().getEnrollmentType())) // E-W
                                                                                                     // case
                        {
                            // The check for the "End of Month Membership" column
                            if (!firstSpan.getFirstActiveEnrollment().getEnrollmentDate().after(m_endDate) &&
                                    !firstSpan.getLastActiveDate().before(m_endDate) &&
                                    school.equals(firstSpan.getSchool())) {
                                addMembershipData(membershipData, student, firstSpan, studentsReported, gradeCode,
                                        detailGrid);
                            }

                            // Reset the first span again (after we check if the student is net
                            // enrolled above)
                            firstSpan = null;
                        } else // E-S case
                        {
                            firstSpan = span;
                        }
                    } else if (span.getFirstInactiveEnrollment() != null && StudentEnrollment.WITHDRAWAL
                            .equals(span.getFirstInactiveEnrollment().getEnrollmentType())) // S-W
                                                                                            // case
                    {
                        nextSpan = span;
                    }
                }
            }

            // Check the last span
            if (firstSpan != null) {
                // The check for the "End of Month Membership" column
                StudentEnrollment withdrawalEnrollment = null;
                PlainDate withdrawalDate = null;
                if (nextSpan != null) // It's E-S and [S-W or S-S]
                {
                    withdrawalEnrollment = nextSpan.getFirstInactiveEnrollment();
                    withdrawalDate = nextSpan.getLastActiveDate();
                } else // It's E-S or E-W
                {
                    withdrawalEnrollment = firstSpan.getFirstInactiveEnrollment();
                    withdrawalDate = firstSpan.getLastActiveDate();
                }

                // If there's no 'nextSpan', make sure the firstSpan is not E-W with withdrawal date
                // before the period end date
                // OR enrollment date after the period end date
                if (!firstSpan.getFirstActiveEnrollment().getEnrollmentDate().after(m_endDate) &&
                        school.equals(firstSpan.getSchool()) &&
                        (withdrawalEnrollment == null ||
                                !(StudentEnrollment.WITHDRAWAL.equals(withdrawalEnrollment.getEnrollmentType()) &&
                                        !withdrawalDate.after(m_endDate)))) {
                    addMembershipData(membershipData, student, firstSpan, studentsReported, gradeCode, detailGrid);
                }
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
