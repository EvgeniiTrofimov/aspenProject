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
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper.Strategy;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentMultiYearHelper;
import com.x2dev.procedures.statereporting.tn.TNStateReportData;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.Query;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Director's Annual Special Education" report.
 *
 * @author X2 Development Corporation
 */

public class PromotionsAndRetentionsReportData extends ReportJavaSourceNet {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 

    /**
     * Helper class for various student information delivery.
     */
    class EnrollmentStatistics extends TNStateReportData {
        protected TNStudentHistoryHelper m_helper;
        protected TNStudentMultiYearHelper m_multiYearHelper;

        private TNEnrollmentHelper m_enrHelper;


        /**
         * Return calendar days.
         *
         * @param school SisSchool
         * @param calendar String
         * @return Sets the
         */
        public Set<PlainDate> getCalendarDays(SisSchool school, String calendar) {
            return m_helper.getCalendarDays(school, calendar);
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
        public List<StudentEnrollmentSpan> getStudentEnrollmentSpans(Student student, boolean limit) {
            return m_helper.getStudentEnrollmentSpans(student, limit);
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

            m_enrHelper = new TNEnrollmentHelper(this);

            m_helper = m_enrHelper.getStudentHistoryHelper();
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);

            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_ATTENDANCE, Boolean.FALSE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);

            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.TRUE);

            m_multiYearHelper = m_enrHelper.getStudentMultiYearHelper();
        }
    }

    /**
     * Helper class for Student Program Code representation.
     * Required state code value.
     */
    class ProgramCode {
        private String m_programCode;

        /**
         * Instantiates a new program code.
         *
         * @param programCode String
         */
        public ProgramCode(String programCode) {
            m_programCode = programCode;
        }

        /**
         * return primary option.
         *
         * @return Integer
         */
        public Integer getPrimaryOption() {
            Integer primaryOption = null;
            if (hasPrimaryOption()) {
                primaryOption = Integer.valueOf(Integer.parseInt(m_programCode.substring(0, 2)));
            }
            return primaryOption;
        }

        /**
         * Gets the program code.
         *
         * @return String
         */
        public String getProgramCode() {
            return m_programCode;
        }

        /**
         * Gets the secondary option.
         *
         * @return second option
         */
        public Integer getSecondaryOption() {
            Integer secondaryOption = null;
            if (hasSecondaryOption()) {
                secondaryOption = Integer.valueOf(Integer.parseInt(m_programCode.substring(2, 3)));
            }
            return secondaryOption;
        }

        /**
         * Checks for primary option.
         *
         * @return true, if program code has primary program
         *         (represented as first and second characters as integer)
         */
        public boolean hasPrimaryOption() {
            return m_programCode.length() >= 2
                    && Character.isDigit(m_programCode.charAt(0))
                    && Character.isDigit(m_programCode.charAt(1));
        }

        /**
         * Checks for secondary option.
         *
         * @return true, if program code has secondary program
         *         (represented as third character as integer)
         */
        public boolean hasSecondaryOption() {
            return m_programCode.length() >= 3
                    && Character.isDigit(m_programCode.charAt(2));
        }
    }

    private static final String ALIAS_CALENDAR_EVENT_TYPE_2 = "DOE DAY EVENT TYPE 2";
    private static final String ALIAS_CALENDAR_EVENT_TYPE_3 = "DOE DAY EVENT TYPE 3";
    private static final String ALIAS_EASY_IEP_TYPE = "EasyIEP Type";
    private static final String ALIAS_SERVICE_ACTION = "DOE END SERVICE ACTION";
    private static final String ALIAS_SERVICE_ACTION_DATE = "DOE END SERVICE ACTION DATE";
    private static final String ALIAS_SCHOOL_STATE_ID = "DOE SCHOOL STATE ID";

    private static final String EASY_IEP_TYPE_STATE_CODE = "P";

    private static final String EVENT_TYPE_CALENDAR_END_DATE = "CE";

    private static final int GRADE_FILTER_MAX_LEVEL = 12;
    private static final int GRADE_FILTER_MIN_LEVEL = 0;

    private static final String GRADE_LEVEL_LETTER_KINDERGARTEN = "K";
    private static final String GRADE_LEVEL_LETTER_NONGRADED = "N";
    private static final int GRADE_LEVEL_NUMERIC_KINDERGARTEN = 0;
    private static final int GRADE_LEVEL_NUMERIC_NONGRADED = 200;
    private static final int GRADE_LEVEL_NUMERIC_TOTAL = 100;
    private static final String GRADE_LEVEL_SPED = "SP";
    private static final String GRADE_LEVEL_TOTAL = "TOTAL";

    private static final String INPUT_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_INCLUDE_DISTR_SUMMARY = "includeDistrictSummary";
    private static final String INPUT_INCLUDE_GRADES = "includeGrades";
    private static final String INPUT_REPORT_MODE = "reportMode";
    private static final int INPUT_REPORT_MODE_YEAR = 0;
    private static final int INPUT_REPORT_MODE_SUMMER = 1;
    private static final String INPUT_SCHOOLS = "schoolOids";
    private static final String INPUT_SPECIAL_EDUCATION_REPORT = "specialEducationReport";
    private static final String INPUT_SUMMARY_ONLY = "summaryOnly";

    private static final String KEY_PROMOTION = "promotion";
    private static final String KEY_RETENTION = "retention";

    private static final String PROMOTION_CODE = "P";

    private static final String REPORT_FIELD_GRADE = "grade";
    private static final String REPORT_FIELD_ORG_NAME = "orgName";
    private static final String REPORT_FIELD_PROMOTION = "promotion";
    private static final String REPORT_FIELD_RETENTION = "retention";
    private static final String REPORT_FIELD_SCHOOL = "school";

    private static final String REPORT_PARAMETER_END_DATE = "dateThrough";
    private static final String REPORT_PARAMETER_IS_SPED_REPORT = "isSpedReport";
    private static final String REPORT_PARAMETER_ORGANIZATION = "organization";
    private static final String REPORT_PARAMETER_REPORT_DATE = "reportDate";
    private static final String REPORT_PARAMETER_START_DATE = "startDate";
    private static final String REPORT_PARAMETER_USER = "user";

    private static final String RETENTION_CODE = "R";

    private static final String SCHOOL_DISTRICT_CALENDAR = "dist";

    private static final String SPECIAL_EDUCATION_CATEGORY_OPTIONS = "Options";

    protected PlainDate m_endDate = null;
    protected PlainDate m_startDate = null;

    private DistrictSchoolYearContext m_context;
    private EnrollmentStatistics m_data;
    private DataDictionary m_dictionary;
    private String m_fieldServiceAction;
    private String m_fieldServiceActionDate;
    private Map<SisSchool, PlainDate> m_firstDaysOfSchools;
    private List<Integer> m_gradeLevels;
    private boolean m_includeDistrSummary;
    private List<String> m_includeGrades;
    private boolean m_isSpecialReport = false;
    private TNReportingPeriodHelper m_periodHelper = null;
    private Map<String, ReferenceCode> m_referenceEndServiceCodeMap;
    private int m_reportMode;
    private Map<String, PlainDate> m_schoolCalendarEndDate;
    private Collection<SisSchool> m_schools;
    private Map<String, Map<String, List<StudentProgramParticipation>>> m_schoolStudentPrograms;
    private boolean m_summaryOnly;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        Map<Integer, HashMap<String, Integer>> districtCounter = getDefaultCounter();
        Integer one = Integer.valueOf(1);

        DateAsStringConverter dateConverter =
                (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                        Locale.getDefault(), true);
        for (SisSchool school : m_schools) {
            PlainDate schoolStartDate = m_firstDaysOfSchools.get(school);

            m_data = new EnrollmentStatistics();
            m_data.setBroker(getBroker());
            m_data.setOrganization(getOrganization());
            m_data.setPrivilegeSet(getPrivilegeSet());
            m_data.setCurrentContext(getCurrentContext());
            m_data.setSchoolContext(true);
            m_data.setSchool(school);
            m_data.setParameters(getParameters());
            m_data.setUser(getUser());
            m_data.initializeExport();

            Map<Integer, HashMap<String, Integer>> schoolCounter = getDefaultCounter();

            // Limit query only for students with grade K, 1-12, SP;
            // can be changed from input
            X2Criteria studentCriteria = (X2Criteria) m_data.getStudentCriteria();
            m_data.m_multiYearHelper.adjustCriteria(studentCriteria, Strategy.IN, Student.COL_GRADE_LEVEL,
                    m_includeGrades);
            m_data.m_multiYearHelper.adjustCriteria(studentCriteria, Strategy.EQUAL_TO, Student.COL_SCHOOL_OID,
                    school.getOid());

            if (m_schoolStudentPrograms.containsKey(school.getOid())) {
                if (m_isSpecialReport) {
                    studentCriteria.addIn(X2BaseBean.COL_OID, m_schoolStudentPrograms.get(school.getOid()).keySet());
                } else {
                    studentCriteria.addNotIn(X2BaseBean.COL_OID, m_schoolStudentPrograms.get(school.getOid()).keySet());
                }
            }
            QueryIterator students = getBroker().getIteratorByQuery(m_data.getStudentQuery(false));
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();
                boolean studentInCurrentSchool = m_data.m_multiYearHelper
                        .getFieldValueByBeanPath(student, SisStudent.COL_SCHOOL_OID).equals(school.getOid());
                if (!studentInCurrentSchool) {
                    continue;
                }
                String serviceActionCode =
                        (String) m_data.m_multiYearHelper.getFieldValueByBeanPath(student, m_fieldServiceAction);
                ReferenceCode endServiceRefCode = m_referenceEndServiceCodeMap.get(serviceActionCode);

                String serviceActionDateString =
                        (String) m_data.m_multiYearHelper.getFieldValueByBeanPath(student, m_fieldServiceActionDate);
                PlainDate serviceActionDate = null;
                if (!StringUtils.isEmpty(serviceActionDateString)) {
                    serviceActionDate = (PlainDate) dateConverter.parseSystemString(serviceActionDateString);
                }

                ReferenceCode gradeLevelRefCode = m_data.m_helper.getGradeLevelByDates(student, m_startDate, m_endDate);
                Integer gradeLevel = prepareGradeLevel(gradeLevelRefCode);
                if (gradeLevel != null && endServiceRefCode != null && serviceActionDate != null
                        && !serviceActionDate.before(m_startDate) &&
                        ((m_reportMode == INPUT_REPORT_MODE_YEAR && !serviceActionDate.after(m_endDate)) ||
                                (m_reportMode == INPUT_REPORT_MODE_SUMMER
                                        && serviceActionDate.before(schoolStartDate)))) {
                    if (m_isSpecialReport) {
                        if (RETENTION_CODE.equals(endServiceRefCode.getStateCode())) {
                            // check program dates
                            if (m_periodHelper.isStudentNGradeLevel(student, getCalendarEndDate(school.getOid()),
                                    gradeLevelRefCode.getCode())) {
                                addValue(schoolCounter, Integer.valueOf(GRADE_LEVEL_NUMERIC_NONGRADED), KEY_RETENTION,
                                        one);
                            } else {
                                addValue(schoolCounter, gradeLevel, KEY_RETENTION, one);
                            }
                        } else if (PROMOTION_CODE.equals(endServiceRefCode.getStateCode())) {
                            if (m_periodHelper.isStudentNGradeLevel(student, getCalendarEndDate(school.getOid()),
                                    gradeLevelRefCode.getCode())) {
                                addValue(schoolCounter, Integer.valueOf(GRADE_LEVEL_NUMERIC_NONGRADED), KEY_PROMOTION,
                                        one);
                            } else {
                                addValue(schoolCounter, gradeLevel, KEY_PROMOTION, one);
                            }
                        }
                    } else {
                        if (!m_periodHelper.isStudentNGradeLevel(student, getCalendarEndDate(school.getOid()),
                                gradeLevelRefCode.getCode())) {
                            if (RETENTION_CODE.equals(endServiceRefCode.getStateCode())) {
                                addValue(schoolCounter, gradeLevel, KEY_RETENTION, one);
                            } else if (PROMOTION_CODE.equals(endServiceRefCode.getStateCode())) {
                                addValue(schoolCounter, gradeLevel, KEY_PROMOTION, one);
                            }
                        }
                    }
                }
            }
            countTotals(schoolCounter);
            calculateDistrictCount(districtCounter, schoolCounter);
            ReportDataGrid schoolGrid = new ReportDataGrid();
            populateGrid(schoolGrid, schoolCounter, school.getName(), school);
            grid.append(schoolGrid);

        }
        if (m_summaryOnly) {
            grid = new ReportDataGrid();
        }
        if (m_includeDistrSummary) {
            ReportDataGrid districtGrid = new ReportDataGrid();
            populateGrid(districtGrid, districtCounter, getOrganization().getName(), null);
            grid.append(districtGrid);
        }
        grid.beforeTop();
        return grid;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_data = new EnrollmentStatistics();
        m_data.setBroker(getBroker());
        m_data.setOrganization(getOrganization());
        m_data.setPrivilegeSet(getPrivilegeSet());
        m_data.setParameters(getParameters());
        m_data.setUser(getUser());
        m_data.initializeExport();
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_context = getCurrentContext();
        m_startDate = m_context.getStartDate();
        m_endDate = m_context.getEndDate();
        // Create default mockup object to initialize Period Helper
        ReferenceCode mockup = X2BaseBean.newInstance(ReferenceCode.class, getBroker().getPersistenceKey());
        mockup.setCode("01");
        m_periodHelper = new TNReportingPeriodHelper(getOrganization(), m_context, mockup, getBroker());

        String includeGrades = (String) getParameter(INPUT_INCLUDE_GRADES);
        m_includeGrades = StringUtils.convertDelimitedStringToList(includeGrades, ',', true);

        if (getParameter(INPUT_SPECIAL_EDUCATION_REPORT) != null) {
            m_isSpecialReport = ((Boolean) getParameters().get(INPUT_SPECIAL_EDUCATION_REPORT)).booleanValue();
        }
        if (getParameter(INPUT_INCLUDE_DISTR_SUMMARY) != null) {
            m_includeDistrSummary = ((Boolean) getParameters().get(INPUT_INCLUDE_DISTR_SUMMARY)).booleanValue();
        }
        if (getParameter(INPUT_SUMMARY_ONLY) != null) {
            m_summaryOnly = ((Boolean) getParameters().get(INPUT_SUMMARY_ONLY)).booleanValue();
        }
        if (getParameter(INPUT_REPORT_MODE) != null) {
            m_reportMode = ((Integer) getParameters().get(INPUT_REPORT_MODE)).intValue();
        }

        m_fieldServiceAction = translateAliasToJavaName(ALIAS_SERVICE_ACTION);
        m_fieldServiceActionDate = translateAliasToJavaName(ALIAS_SERVICE_ACTION_DATE);

        loadSchools();
        loadFirstDaysOfSchool();
        initGradeLevels();
        initSchoolCalendatEndDates();
        loadEndServiceCodes();
        loadStudentPrograms();

        addParameter(REPORT_PARAMETER_USER, getUser());
        addParameter(REPORT_PARAMETER_ORGANIZATION, getOrganization());
        addParameter(REPORT_PARAMETER_REPORT_DATE, new PlainDate());
        addParameter(REPORT_PARAMETER_START_DATE, m_startDate);
        addParameter(REPORT_PARAMETER_END_DATE, m_endDate);
        addParameter(REPORT_PARAMETER_IS_SPED_REPORT, Boolean.valueOf(m_isSpecialReport));
    }

    /**
     * Adds the value.
     *
     * @param counter Map<Integer,HashMap<String,Integer>>
     * @param gradeLevel Integer
     * @param countType String
     * @param value Integer
     */
    private void addValue(Map<Integer, HashMap<String, Integer>> counter,
                          Integer gradeLevel,
                          String countType,
                          Integer value) {
        int prevValue = counter.get(gradeLevel).get(countType).intValue();
        counter.get(gradeLevel).put(countType, Integer.valueOf(prevValue + value.intValue()));
    }

    /**
     * Calculate district count.
     *
     * @param districtCounter Map<Integer,HashMap<String,Integer>>
     * @param schoolCounter Map<Integer,HashMap<String,Integer>>
     */
    private void calculateDistrictCount(Map<Integer, HashMap<String, Integer>> districtCounter,
                                        Map<Integer, HashMap<String, Integer>> schoolCounter) {
        for (Integer gradeLevel : m_gradeLevels) {
            addValue(districtCounter, gradeLevel, KEY_PROMOTION, schoolCounter.get(gradeLevel).get(KEY_PROMOTION));
            addValue(districtCounter, gradeLevel, KEY_RETENTION, schoolCounter.get(gradeLevel).get(KEY_RETENTION));
        }
    }

    /**
     * Count totals.
     *
     * @param schoolCounter Map<Integer,HashMap<String,Integer>>
     */
    private void countTotals(Map<Integer, HashMap<String, Integer>> schoolCounter) {
        int totalPromotions = 0;
        int totalRetentions = 0;
        for (Integer gradeLevel : m_gradeLevels) {
            if (gradeLevel.intValue() != GRADE_LEVEL_NUMERIC_NONGRADED) {
                totalPromotions += schoolCounter.get(gradeLevel).get(KEY_PROMOTION).intValue();
                totalRetentions += schoolCounter.get(gradeLevel).get(KEY_RETENTION).intValue();
            }
        }
        Integer totalGrade = Integer.valueOf(GRADE_LEVEL_NUMERIC_TOTAL);
        schoolCounter.get(totalGrade).put(KEY_PROMOTION, Integer.valueOf(totalPromotions));
        schoolCounter.get(totalGrade).put(KEY_RETENTION, Integer.valueOf(totalRetentions));
    }

    /**
     * Gets the calendar end date.
     *
     * @param schoolOid String
     * @return Plain date
     */
    private PlainDate getCalendarEndDate(String schoolOid) {
        return m_schoolCalendarEndDate.get(schoolOid);
    }

    /**
     * Gets the default counter.
     *
     * @return Map
     */
    private Map<Integer, HashMap<String, Integer>> getDefaultCounter() {
        Map counter = new TreeMap<Integer, HashMap<String, Integer>>();
        for (Integer gradeLevel : m_gradeLevels) {
            Map inner = new HashMap<String, Integer>();
            inner.put(KEY_PROMOTION, Integer.valueOf(0));
            inner.put(KEY_RETENTION, Integer.valueOf(0));
            counter.put(gradeLevel, inner);
        }
        return counter;
    }

    /**
     * Get grade level code.
     *
     * @param gradeLevel Integer
     * @return String
     */
    private String getGradeCode(Integer gradeLevel) {
        if (gradeLevel.intValue() == GRADE_LEVEL_NUMERIC_KINDERGARTEN) {
            return GRADE_LEVEL_LETTER_KINDERGARTEN;
        } else if (gradeLevel.intValue() == GRADE_LEVEL_NUMERIC_NONGRADED) {
            return GRADE_LEVEL_LETTER_NONGRADED;
        } else if (gradeLevel.intValue() == GRADE_LEVEL_NUMERIC_TOTAL) {
            return GRADE_LEVEL_TOTAL;
        }
        return gradeLevel.toString();
    }

    /**
     * Load student programs.
     */
    private void loadStudentPrograms() {
        if (m_schoolStudentPrograms == null) {
            X2Criteria programCriteria = new X2Criteria();

            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_EASY_IEP_TYPE);

            X2Criteria spedProgramCriteria = new X2Criteria();

            X2Criteria programTypeCriteria = new X2Criteria();
            programTypeCriteria.addEqualTo(ReferenceCode.COL_STATE_CODE, EASY_IEP_TYPE_STATE_CODE);
            SubQuery typeSubQuery = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, programTypeCriteria);
            spedProgramCriteria.addIn(field.getJavaName(), typeSubQuery);

            X2Criteria programCategoryCriteria = new X2Criteria();
            programCategoryCriteria.addEqualTo(ReferenceCode.COL_CATEGORY, SPECIAL_EDUCATION_CATEGORY_OPTIONS);
            SubQuery categorySubQuery =
                    new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, programCategoryCriteria);
            spedProgramCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, categorySubQuery);

            // TODO: This subquery is not needed. The spedProgramCriteria should be added to the
            // programCriteria

            SubQuery spedSubQuery =
                    new SubQuery(StudentProgramParticipation.class, X2BaseBean.COL_OID, spedProgramCriteria);
            programCriteria.addIn(X2BaseBean.COL_OID, spedSubQuery);

            programCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_endDate);

            X2Criteria programEndDateCriteria = new X2Criteria();
            X2Criteria programNullEndDateCriteria = new X2Criteria();
            programNullEndDateCriteria.addIsNull(StudentProgramParticipation.COL_END_DATE);
            programEndDateCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_startDate);
            programEndDateCriteria.addOrCriteria(programNullEndDateCriteria);

            programCriteria.addAndCriteria(programEndDateCriteria);

            QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, programCriteria);
            Map<String, List<StudentProgramParticipation>> studentPrograms =
                    getBroker().getGroupedCollectionByQuery(query,
                            new String[] {
                                    StudentProgramParticipation.COL_STUDENT_OID},
                            new int[] {100, 200});

            X2Criteria studentCriteria = new X2Criteria();
            studentCriteria.addIn(X2BaseBean.COL_OID, studentPrograms.keySet());
            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
            Collection<SisStudent> students = getBroker().getCollectionByQuery(studentQuery);

            m_schoolStudentPrograms = new HashMap<String, Map<String, List<StudentProgramParticipation>>>();
            for (SisStudent student : students) {
                String schoolOid =
                        (String) m_data.m_multiYearHelper.getFieldValueByBeanPath(student, SisStudent.COL_SCHOOL_OID);
                Map<String, List<StudentProgramParticipation>> currentStudentPrograms =
                        m_schoolStudentPrograms.get(schoolOid);
                if (currentStudentPrograms == null) {
                    currentStudentPrograms = new HashMap<String, List<StudentProgramParticipation>>();
                    m_schoolStudentPrograms.put(schoolOid, currentStudentPrograms);
                }
                currentStudentPrograms.put(student.getOid(), studentPrograms.get(student.getOid()));
            }
        }
    }

    /**
     * Initialize numeric grade levels.
     */
    private void initGradeLevels() {
        // Ordering is required
        m_gradeLevels = new ArrayList<Integer>();
        // Zero stands for K grade;
        for (int i = 0; i <= 12; i++) {
            m_gradeLevels.add(Integer.valueOf(i));
        }
        // Total numeric grade
        m_gradeLevels.add(Integer.valueOf(GRADE_LEVEL_NUMERIC_TOTAL));

        if (m_isSpecialReport) {
            // Grade N
            m_gradeLevels.add(Integer.valueOf(GRADE_LEVEL_NUMERIC_NONGRADED));
        }
    }

    /**
     * Initialize school calendar end dates map (the last dates with 'CE' (Calendar End date) event
     * code).
     */
    private void initSchoolCalendatEndDates() {
        m_schoolCalendarEndDate = new HashMap<String, PlainDate>();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField aliasEventType2 = dictionary.findDataDictionaryFieldByAlias(ALIAS_CALENDAR_EVENT_TYPE_2);
        DataDictionaryField aliasEventType3 = dictionary.findDataDictionaryFieldByAlias(ALIAS_CALENDAR_EVENT_TYPE_3);

        X2Criteria eventTypeCriteria = new X2Criteria();
        eventTypeCriteria.addEqualTo(SchoolCalendarDate.COL_SCHEDULE_DAY_TYPE, EVENT_TYPE_CALENDAR_END_DATE);
        if (aliasEventType2 != null) {
            eventTypeCriteria.addOrEqualTo(aliasEventType2.getJavaName(), EVENT_TYPE_CALENDAR_END_DATE);
        }
        if (aliasEventType3 != null) {
            eventTypeCriteria.addOrEqualTo(aliasEventType3.getJavaName(), EVENT_TYPE_CALENDAR_END_DATE);
        }

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(
                SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                getCurrentContext().getOid());
        criteria.addAndCriteria(eventTypeCriteria);

        QueryByCriteria calendarEndDatesQuery = new QueryByCriteria(SchoolCalendarDate.class, criteria);
        calendarEndDatesQuery.addOrderByAscending(SchoolCalendarDate.COL_DATE);

        QueryIterator endDates = getBroker().getIteratorByQuery(calendarEndDatesQuery);
        try {
            while (endDates.hasNext()) {
                SchoolCalendarDate date = (SchoolCalendarDate) endDates.next();
                m_schoolCalendarEndDate.put(date.getSchoolCalendar().getSchoolOid(), date.getDate());
            }
        } finally {
            if (endDates != null) {
                endDates.close();
            }
        }
    }

    /**
     * Load grade codes.
     */
    private void loadEndServiceCodes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SERVICE_ACTION);
        ReferenceTable referenceTable = field.getReferenceTable();
        m_referenceEndServiceCodeMap = referenceTable.getCodeMap();
    }

    /**
     * Load first days of school.
     */
    private void loadFirstDaysOfSchool() {
        m_firstDaysOfSchools = new HashMap<SisSchool, PlainDate>();
        for (SisSchool school : m_schools) {
            PlainDate startDate = m_periodHelper.getDateBegin(school.getOid());
            if (startDate != null) {
                m_firstDaysOfSchools.put(school, startDate);
            }
        }
    }

    /**
     * Load schools.
     *
     * @return Collection
     */
    private Collection<SisSchool> loadSchools() {
        m_schools = new ArrayList<SisSchool>();

        Object objIsAllSchools = getParameter(INPUT_ALL_SCHOOLS);
        boolean isAllSchools = objIsAllSchools == null ? false : ((Boolean) objIsAllSchools).booleanValue();
        if (isAllSchools) {
            X2Criteria schoolCriteria = new X2Criteria();

            schoolCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            schoolCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            schoolCriteria.addNotEqualTo(SisSchool.COL_SCHOOL_ID, SCHOOL_DISTRICT_CALENDAR);

            DataDictionaryField aliasSklStateIDField =
                    m_dictionary.findDataDictionaryFieldByAlias(ALIAS_SCHOOL_STATE_ID);
            schoolCriteria.addNotEmpty(aliasSklStateIDField.getJavaName(), getBroker().getPersistenceKey());

            QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
            schoolQuery.addOrderByAscending(SisSchool.COL_NAME);
            m_schools.addAll(getBroker().getCollectionByQuery(schoolQuery));
        } else {
            Object objSchools = getParameter(INPUT_SCHOOLS);
            String schoolOids = objSchools == null ? "" : (String) objSchools;
            if (!StringUtils.isEmpty(schoolOids)) {
                List<String> oids = Arrays.asList(schoolOids.split(","));
                X2Criteria schoolCriteria = new X2Criteria();

                schoolCriteria.addIn(X2BaseBean.COL_OID, oids);

                QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
                schoolQuery.addOrderByAscending(SisSchool.COL_NAME);
                m_schools.addAll(getBroker().getCollectionByQuery(schoolQuery));
            }
        }
        return m_schools;
    }

    /**
     * Populate grid.
     *
     * @param schoolGrid ReportDataGrid
     * @param schoolCounter Map<Integer,HashMap<String,Integer>>
     * @param orgName String
     * @param school SisSchool
     */
    private void populateGrid(ReportDataGrid schoolGrid,
                              Map<Integer, HashMap<String, Integer>> schoolCounter,
                              String orgName,
                              SisSchool school) {
        for (Integer gradeLevel : m_gradeLevels) {
            schoolGrid.append();
            schoolGrid.set(REPORT_FIELD_SCHOOL, school);
            schoolGrid.set(REPORT_FIELD_ORG_NAME, orgName);
            schoolGrid.set(REPORT_FIELD_GRADE, getGradeCode(gradeLevel));

            Integer promotionNum = schoolCounter.get(gradeLevel).get(KEY_PROMOTION);
            Integer retentionNum = schoolCounter.get(gradeLevel).get(KEY_RETENTION);

            schoolGrid.set(REPORT_FIELD_PROMOTION, promotionNum);
            schoolGrid.set(REPORT_FIELD_RETENTION, retentionNum);
        }
        schoolGrid.beforeTop();
    }

    /**
     * Check and prepare grade level.
     *
     * @param gradeCode ReferenceCode
     * @return Integer
     */
    private Integer prepareGradeLevel(ReferenceCode gradeCode) {
        Integer preparedGradeLevel = null;
        if (gradeCode != null) {
            if (GRADE_LEVEL_SPED.equals(gradeCode.getCode())) {
                return Integer.valueOf(GRADE_LEVEL_NUMERIC_NONGRADED);
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

            if (grade >= GRADE_FILTER_MIN_LEVEL && grade <= GRADE_FILTER_MAX_LEVEL) {
                preparedGradeLevel = Integer.valueOf((grade));
            }
        }
        return preparedGradeLevel;

    }

    /**
     * Translates an alias into a Java bean path name.
     *
     * @param alias String
     * @return String
     */
    private String translateAliasToJavaName(String alias) {
        String javaName = null;

        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
        }

        return javaName;
    }
}
