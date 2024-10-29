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
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentScheduleSpan;
import com.x2dev.procedures.statereporting.tn.TNStateReportData;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import net.sf.jasperreports3.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class PreliminarySchoolReportData.
 */
public class PreliminarySchoolReportData extends ReportJavaSourceNet {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 

    /**
     * The Class EnrollmentStatistics.
     */
    class EnrollmentStatistics extends TNStateReportData {
        protected static final String PARAM_END_DATE = "endDate";
        protected static final String PARAM_START_DATE = "startDate";
        private static final String PARAM_INCLUDE_SECONDARY = "includeSecondary";

        private DateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
        private TNEnrollmentHelper m_tnEnrHelper;
        private TNStudentHistoryHelper m_tnStudentHelper;

        /**
         * Returns school if collections contain one school, otherwise returns null.
         *
         * @return School
         */
        @Override
        public School getSchool() {
            School school = null;
            if (m_schools.size() == 1) {
                school = m_schools.iterator().next();
            }
            return school;
        }

        /**
         * Return the list of student schedule spans.
         *
         * @param student SisStudent
         * @return List<StudentScheduleSpan>
         */
        public List<TNStudentScheduleSpan> getTNStudentScheduleSpans(SisStudent student) {
            return m_tnStudentHelper.getTNStudentScheduleSpans(student);
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
            m_tnStudentHelper = m_tnEnrHelper.getStudentHistoryHelper();
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SCHEDULE_SPANS, Boolean.TRUE);
            m_tnStudentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            PlainDate startDate = isSchoolContext() ? m_startDate.get(getSchool().getOid()) : m_startDateTotal;
            PlainDate endDate = isSchoolContext() ? m_endDate.get(getSchool().getOid()) : m_endDateTotal;
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, startDate);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, endDate);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_SECONDARY,
                    getParameter(PARAM_INCLUDE_SECONDARY));

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_ATTENDANCE, Boolean.FALSE);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.TRUE);

        }

        /**
         * Returns true if school collection size == 1.
         *
         * @return true, if is school context
         */
        @Override
        public boolean isSchoolContext() {
            return m_schools.size() == 1 ? true : false;
        }
    }


    // Aliases
    private static final String ALIAS_COURSE_SDE_CODE = "DOE SDE COURSE CODE";
    private static final String ALIAS_COURSE_TYPE = "DOE CLASS TYPE";
    private static final String ALIAS_COURSE_EXCLUDE = "DOE EXCLUDE CRS";
    private static final String ALIAS_COURSE_VOC = "DOE VOC CLASSIFICATION";
    private static final String ALIAS_COURSE_EXCLUDE_PRELIMINARY = "DOE EXCLUDE PRELIMINARY";
    private static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL STATE ID";

    private static final String COURSE_CODE_PULLOUT = "P";
    private static final String COURSE_CODE_DEPARTMENTALIZED = "C";
    private static final String DECIMAL_FORMAT = "0.00";
    private static final String DELIMITER = ", ";
    private static final String FD_CODE = "FD";

    // DataGrid columns
    private static final String FIELD_CLASS_NUMBER = "classNumber";
    private static final String FIELD_CLASS_SIZE = "classSize";
    private static final String FIELD_COURSE_SDE_CODE = "courseSDECode";
    private static final String FIELD_EIA_CLASS_SIZE = "eiaClassSize";
    private static final String FIELD_GRADE_LEVELS = "classGradeLevels";
    private static final String FIELD_GROUP_CODE = "gradeGroupCode";
    private static final String FIELD_IS_MULTIAGE = "isMultiAge";
    private static final String FIELD_IS_VOC = "isVoc";
    private static final String FIELD_IS_SPED = "isSped";
    private static final String FIELD_SCHOOL_NAME = "schoolName";
    private static final String FIELD_SCHOOL_NUMBER = "schoolNumber";
    private static final String FIELD_SCHOOL_SEMESTER = "semester";

    private static final String GRADE_GROUP_CODE_K12 = "K12";
    private static final String GRADE_GROUP_CODE_K6 = "K6";
    private static final String GRADE_GROUP_CODE_K3 = "K3";
    private static final String GRADE_GROUP_CODE_PK = "PK";
    private static final String GRADE_GROUP_VOC = "VOC";

    private static final String INPUT_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_SCHOOLS = "schoolOids";

    // Input params and reportDesign params
    private static final String PARAM_CHAR_DELIMITER = "charDelimiter";
    private static final String PARAM_CODE_CLASS_SUF = "_EIAMaxClassSize";
    private static final String PARAM_CODE_GRADE_SUF = "_EIAGradeAVG";
    private static final String PARAM_CONTEXT = "context";
    private static final String PARAM_CONTEXT_OID = "schoolYearOid";
    private static final String PARAM_DISTRICT = "district";
    private static final String PARAM_GROUP_CODES = "groupCodes";
    private static final String PARAM_GROUP_K3_LEVELS = "groupK3Levels";
    private static final String PARAM_GROUP_K6_LEVELS = "groupK6Levels";
    private static final String PARAM_GROUP_K12_LEVELS = "groupK12Levels";
    private static final String PARAM_GROUP_PK_LEVELS = "groupPKLevels";
    private static final String PARAM_MAX_CLASS_SIZE_MAP = "maxClassSizeMap";
    private static final String PARAM_REPORT_DATE = "reportDate";
    private static final String PARAM_SEMESTER = "semester";

    private static final String SEMESTER_FALL = "Fall";
    private static final String SEMESTER_SPRING = "Spring";

    private static final String SPECIAL_EDUCATION_CATEGORY_OPTIONS = "Options";
    private static final String SPECIAL_EDUCATION_OPTION_07 = "07";
    private static final String SPECIAL_EDUCATION_OPTION_08 = "08";
    private static final String SPECIAL_EDUCATION_OPTION_09 = "09";

    // fields
    protected Map<String, PlainDate> m_endDate;
    protected PlainDate m_endDateTotal;
    protected Map<String, PlainDate> m_startDate;
    protected PlainDate m_startDateTotal;

    private Map<String, int[]> m_calculateMaxClassSize = new HashMap<String, int[]>();
    private final HashMap<String, Map<String, Set<PlainDate>>> m_calendarDatas =
            new HashMap<String, Map<String, Set<PlainDate>>>();
    private DistrictSchoolYearContext m_context;
    private EnrollmentStatistics m_data;
    private DecimalFormat m_decimalFormat;
    private final HashMap<String, Integer> m_eiaClassSizes = new HashMap<String, Integer>();
    private EnrollmentManager m_enrollmentManager;
    private String m_fieldCrsType;
    private String m_fieldExclCrs;
    private String m_fieldExclCrsPrel;
    private final HashSet<String> m_groupPKLevels = new HashSet<String>();
    private final HashSet<String> m_groupK3Levels = new HashSet<String>();
    private final HashSet<String> m_groupK6Levels = new HashSet<String>();
    private final HashSet<String> m_groupK12Levels = new HashSet<String>();
    private boolean m_isAllSchools;
    private Map<String, String> m_maxClassSize = new HashMap<String, String>();
    public Collection<School> m_schools;
    private List<String> m_schOids;
    private Integer m_semesterChoice;
    private HashSet<String> m_spedStdOids;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        String semester = m_semesterChoice.intValue() == 0 ? SEMESTER_FALL : SEMESTER_SPRING;
        processPeriod(grid, semester);

        List sortColumns = new ArrayList();
        sortColumns.add(FIELD_SCHOOL_NAME);
        sortColumns.add(FIELD_SCHOOL_SEMESTER);
        sortColumns.add(FIELD_IS_MULTIAGE);
        sortColumns.add(FIELD_COURSE_SDE_CODE);
        sortColumns.add(FIELD_CLASS_NUMBER);
        grid.sort(sortColumns, true);
        grid.beforeTop();
        return grid;
    }

    /**
     * Returns school if collections contain one school, otherwise returns null.
     *
     * @return School
     */
    @Override
    protected School getSchool() {
        School school = null;
        if (m_schools.size() == 1) {
            school = m_schools.iterator().next();
        }
        return school;
    }

    /**
     * Get InputParams, load jobCodes and JobCategories.
     * Defines yearContext
     *
     * @throws X2BaseException exception
     */
    @Override
    protected void initialize() throws X2BaseException {

        String districtOid = (String) getParameter(PARAM_CONTEXT_OID);
        m_context = (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class,
                districtOid);
        m_semesterChoice = (Integer) getParameter(PARAM_SEMESTER);

        m_schools = getSchools();
        m_schOids = new ArrayList<String>();
        for (School school : m_schools) {
            m_schOids.add(school.getOid());
        }
        initDates();
        initFields();
        initCodes();
        initDataHelper();
        loadSpedStudents();
        m_decimalFormat = new DecimalFormat(DECIMAL_FORMAT);

        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());

        addParameter(PARAM_REPORT_DATE, new PlainDate(new Date()));
        addParameter(PARAM_DISTRICT, getOrganization());
        addParameter(PARAM_CONTEXT, m_context);
    }

    /**
     * Returns true if school collection size == 1.
     *
     * @return true, if is school context
     */
    @Override
    protected boolean isSchoolContext() {
        return m_schools.size() == 1 ? true : false;
    }

    /**
     * Uses student schedule spans to determine the dates when a student is enrolled in a particular
     * class.
     * From these, calculate the number of students in the class on each in-session day
     * and then average this number for the count for that section for the report period.
     *
     * @param section MasterSchedule
     * @param students HashSet<SisStudent>
     * @param school SisSchool
     * @return Average class size, or if dates can't be count students.size();
     * @throws X2BaseException exception
     */
    private Integer countAverageClassSize(MasterSchedule section, HashSet<SisStudent> students, SisSchool school)
            throws X2BaseException {
        PlainDate startDate = m_startDate.get(school.getOid());
        PlainDate endDate = m_endDate.get(school.getOid());

        Set<PlainDate> calendarDates = null;
        // make local cash of dates to prevent getting the same info from db
        String key = school.getOid() + startDate.toString() + endDate.toString();
        Map<String, Set<PlainDate>> calendarData = m_calendarDatas.get(key);
        if (calendarData == null) {
            calendarData = m_enrollmentManager.getCalendarLookup(school, startDate, endDate);
            m_calendarDatas.put(key, calendarData);
        }
        if (!calendarData.isEmpty() && !students.isEmpty()) {
            int countStudents = 0;
            HashMap<PlainDate, Integer> stdPerDate = new HashMap<PlainDate, Integer>();
            for (SisStudent student : students) {

                calendarDates = calendarData.get(student.getCalendarCode());
                List<TNStudentScheduleSpan> spans = m_data.getTNStudentScheduleSpans(student);

                if ((calendarDates == null) || calendarDates.isEmpty()) {
                    // case then student drop out from average count
                    // we add this students after average count
                    countStudents++;
                    continue;
                }
                for (TNStudentScheduleSpan span : spans) {
                    if (((span.getSection() != null) && span.getSection().equals(section))) {
                        PlainDate entryDate = span.getEntryDate();
                        PlainDate exitDate = span.getExitDate();
                        for (PlainDate date : calendarDates) {
                            if (!date.before(entryDate) && !date.after(exitDate)) {
                                Integer count = stdPerDate.get(date);
                                if (count == null) {
                                    count = Integer.valueOf(1);
                                    stdPerDate.put(date, count);
                                    continue;
                                }
                                stdPerDate.put(date, Integer.valueOf(count.intValue() + 1));
                            }
                        }
                    }
                }
            }
            int stdAverage = 0;
            for (Integer dateValue : stdPerDate.values()) {
                stdAverage += dateValue.intValue();
            }
            if (!stdPerDate.isEmpty()) {
                return Integer.valueOf((stdAverage / stdPerDate.size()) + countStudents);
            }
        }
        return Integer.valueOf(students.size());
    }

    /**
     * Load StaffPositions List grouped in Map by schoolOid, by staffOid respectively.
     * Need for excluding Federally Funded staffs
     *
     * @return Map&lt;String, Map&lt;String, List&gt;&gt;
     */
    private HashSet<String> getExcludedStaffs() {
        X2Criteria positionsCriteria = new X2Criteria();

        X2Criteria startDateCriteria = new X2Criteria();
        startDateCriteria.addLessOrEqualThan(StaffPosition.COL_START_DATE, m_endDateTotal);

        X2Criteria endDateCriteria = new X2Criteria();
        endDateCriteria.addGreaterOrEqualThan(StaffPosition.COL_END_DATE, m_startDateTotal);

        X2Criteria nullEndDate = new X2Criteria();
        nullEndDate.addEmpty(StaffPosition.COL_END_DATE, getBroker().getPersistenceKey());
        endDateCriteria.addOrCriteria(nullEndDate);

        positionsCriteria.addAndCriteria(startDateCriteria);
        positionsCriteria.addAndCriteria(endDateCriteria);
        String activeStatus = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
        positionsCriteria.addEqualTo(StaffPosition.REL_STAFF + PATH_DELIMITER + Staff.COL_STATUS, activeStatus);
        // FD stands for Federally Funded
        positionsCriteria.addEqualTo(StaffPosition.COL_JOB_CODE, FD_CODE);

        positionsCriteria.addIn(StaffPosition.COL_SCHOOL_OID, m_schOids);

        SubQuery subQuery = new SubQuery(StaffPosition.class, X2BaseBean.COL_OID, positionsCriteria);

        return new HashSet<String>(getBroker().getSubQueryCollectionByQuery(subQuery));
    }

    /**
     * If multi-age class then the largest gradeLevel
     * will defines grade group.
     *
     * @param course SchoolCourse
     * @param grades Collection<String>
     * @param isVoc Boolean
     * @return String
     */
    private String getGroupCode(SchoolCourse course, Collection<String> grades, Boolean isVoc) {
        String groupCode = null;
        if (isVoc.booleanValue()) {
            groupCode = GRADE_GROUP_VOC;
        } else {
            Set<String> groupCodes = new TreeSet<String>(new Comparator<String>() {
                private static final String _100 = "100";
                Pattern pattern = Pattern.compile("\\d+");

                @Override
                public int compare(String a, String b) {
                    a = digitsFrom(a);
                    b = digitsFrom(b);
                    if (a.isEmpty()) {
                        a = _100;
                    }
                    if (b.isEmpty()) {
                        b = _100;
                    }
                    return Integer.valueOf(a).compareTo(Integer.valueOf(b));
                }

                private String digitsFrom(String a) {
                    StringBuilder output = new StringBuilder();
                    Matcher match = pattern.matcher(a);
                    while (match.find()) {
                        output.append(match.group());
                    }
                    return output.toString();
                }
            });

            for (String gradeLevel : grades) {
                if (m_groupK12Levels.contains(gradeLevel)) {
                    groupCode = GRADE_GROUP_CODE_K12;
                } else if (m_groupK6Levels.contains(gradeLevel)) {
                    groupCode = GRADE_GROUP_CODE_K6;
                } else if (m_groupK3Levels.contains(gradeLevel)) {
                    groupCode = GRADE_GROUP_CODE_K3;
                } else if (m_groupPKLevels.contains(gradeLevel)) {
                    groupCode = GRADE_GROUP_CODE_PK;
                }
                groupCodes.add(groupCode);

            }
            String[] listGroupCodes = new String[groupCodes.size()];
            groupCodes.toArray(listGroupCodes);
            groupCode = listGroupCodes[0];
        }

        return groupCode;
    }

    /**
     * Load StudentSchedule's list grouped in map's by masterScheduleOid respectively.
     *
     * @return Map
     */
    private Map<String, List> getSchedulesForPeriod() {
        X2Criteria termCriteria = new X2Criteria();
        termCriteria.addLessOrEqualThan(ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                ScheduleTermDate.COL_START_DATE, m_startDateTotal);
        termCriteria.addGreaterOrEqualThan(ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                ScheduleTermDate.COL_END_DATE, m_endDateTotal);

        termCriteria.addIn(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID,
                m_schOids);

        QueryByCriteria termQuery = new QueryByCriteria(ScheduleTerm.class, termCriteria, true);
        QueryIterator termIterator = getBroker().getIteratorByQuery(termQuery);

        Collection<String> scheduleTerms = new HashSet<String>();
        try {
            while (termIterator.hasNext()) {
                ScheduleTerm term = (ScheduleTerm) termIterator.next();
                String schoolOid = term.getSchedule().getSchoolOid();
                PlainDate startDate = m_startDate.get(schoolOid);
                PlainDate endDate = m_endDate.get(schoolOid);

                Collection<ScheduleTermDate> dates = term.getScheduleTermDates(getBroker());
                boolean found = false;

                for (ScheduleTermDate date : dates) {
                    if (!found && !startDate.before(date.getStartDate()) && !endDate.after(date.getEndDate())) {
                        found = true;
                        scheduleTerms.add(term.getOid());
                    }
                }
            }
        } finally {
            termIterator.close();
        }

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StudentSchedule.REL_SECTION + PATH_DELIMITER + Section.COL_SCHEDULE_TERM_OID, scheduleTerms);


        criteria.addIn(StudentSchedule.REL_STUDENT + PATH_DELIMITER + Student.REL_SCHOOL +
                PATH_DELIMITER + X2BaseBean.COL_OID,
                m_schOids);

        // Exclude flags
        String pathToCourse =
                StudentSchedule.REL_SECTION + PATH_DELIMITER + Section.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.REL_COURSE;
        if (m_fieldCrsType != null) {
            criteria.addNotEqualTo(pathToCourse + PATH_DELIMITER + m_fieldCrsType, COURSE_CODE_PULLOUT);
            criteria.addNotEqualTo(pathToCourse + PATH_DELIMITER + m_fieldCrsType, COURSE_CODE_DEPARTMENTALIZED);
        }

        if (m_fieldExclCrs != null) {
            criteria.addNotEqualTo(pathToCourse + PATH_DELIMITER + m_fieldExclCrs, BooleanAsStringConverter.TRUE);
        }

        if (m_fieldExclCrsPrel != null) {
            criteria.addNotEqualTo(pathToCourse + PATH_DELIMITER + m_fieldExclCrsPrel, BooleanAsStringConverter.TRUE);
        }

        // Exclude sections with Federally Funded staff
        HashSet<String> excludedStaffs = getExcludedStaffs();
        if (!excludedStaffs.isEmpty()) {
            criteria.addNotIn(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    Section.COL_PRIMARY_STAFF_OID, excludedStaffs);
        }

        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, criteria);

        return getBroker().getGroupedCollectionByQuery(query, StudentSchedule.COL_SECTION_OID, 100);
    }

    /**
     * Return collection of schools.
     *
     * @return Collection<SisSchool> collection of schools
     */
    private Collection<School> getSchools() {
        Collection<School> schools = null;
        Object objIsAllSchools = getParameter(INPUT_ALL_SCHOOLS);
        m_isAllSchools = objIsAllSchools == null ? false : ((Boolean) objIsAllSchools).booleanValue();
        if (m_isAllSchools) {
            X2Criteria schoolCriteria = new X2Criteria();

            schoolCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            schoolCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

            DataDictionaryField aliasSklStateIDField =
                    DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                            .findDataDictionaryFieldByAlias(ALIAS_SKL_STATE_ID);
            schoolCriteria.addNotEmpty(aliasSklStateIDField.getJavaName(), getBroker().getPersistenceKey());

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
     * Initialize default values from inputParams.
     */
    private void initCodes() {
        String delimiter = ((String) getParameter(PARAM_CHAR_DELIMITER));
        String[] groupCodes = ((String) getParameter(PARAM_GROUP_CODES)).split(delimiter);
        for (String groupCode : groupCodes) {
            String gradeLevelAvg = (String) getParameter(groupCode.concat(PARAM_CODE_GRADE_SUF));
            String classSizeMax = (String) getParameter(groupCode.concat(PARAM_CODE_CLASS_SUF));
            addParameter(groupCode.concat(PARAM_CODE_GRADE_SUF), Integer.valueOf(gradeLevelAvg));
            addParameter(groupCode.concat(PARAM_CODE_CLASS_SUF), Integer.valueOf(classSizeMax));

            m_eiaClassSizes.put(groupCode, Integer.valueOf(classSizeMax));
        }
        m_groupPKLevels.addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_GROUP_PK_LEVELS),
                delimiter.charAt(0), true));
        m_groupK3Levels.addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_GROUP_K3_LEVELS),
                delimiter.charAt(0), true));
        m_groupK6Levels.addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_GROUP_K6_LEVELS),
                delimiter.charAt(0), true));
        m_groupK12Levels.addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_GROUP_K12_LEVELS),
                delimiter.charAt(0), true));
    }

    /**
     * Sets semester start and end dates using TNReportingPeriodHelper.
     */
    private void initDates() {
        TNReportingPeriodHelper periodHelper = new TNReportingPeriodHelper(getOrganization(), m_context,
                null, getBroker());
        periodHelper.setSemester(m_semesterChoice);
        m_startDate = periodHelper.getSemesterBeginDates(m_schools);
        m_endDate = periodHelper.getSemesterEndDates(m_schools);
        m_startDateTotal = m_startDate.get(TNReportingPeriodHelper.KEY_PERIOD_DATE_TOTAL);
        m_endDateTotal = m_endDate.get(TNReportingPeriodHelper.KEY_PERIOD_DATE_TOTAL);
    }

    /**
     * Inits the fields.
     */
    private void initFields() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        DataDictionaryField exclCrs = dictionary.findDataDictionaryFieldByAlias(ALIAS_COURSE_EXCLUDE);
        if (exclCrs != null) {
            m_fieldExclCrs = exclCrs.getJavaName();
        }

        DataDictionaryField exclCrsPrel = dictionary.findDataDictionaryFieldByAlias(ALIAS_COURSE_EXCLUDE_PRELIMINARY);
        if (exclCrsPrel != null) {
            m_fieldExclCrsPrel = exclCrsPrel.getJavaName();
        }

        DataDictionaryField crsType = dictionary.findDataDictionaryFieldByAlias(ALIAS_COURSE_TYPE);
        if (crsType != null) {
            m_fieldCrsType = crsType.getJavaName();
        }

    }

    /**
     * Inits the data helper.
     *
     * @throws X2BaseException exception
     */
    private void initDataHelper() throws X2BaseException {
        m_data = new EnrollmentStatistics();
        m_data.setBroker(getBroker());
        m_data.setOrganization(getOrganization());
        m_data.setPrivilegeSet(getPrivilegeSet());
        m_data.setSchoolContext(isSchoolContext());
        m_data.setSchool(getSchool());
        m_data.setParameters(getParameters());
        m_data.setUser(getUser());
        m_data.initializeExport();
    }

    /**
     * Need to determine different classes, because if two or more
     * sections have same staff and students, it is one class
     * and should be counted once. Exception for departmentalized
     * classes.
     *
     * @param section MasterSchedule
     * @param allClasses Map<String,ArrayList<HashSet<SisStudent>>>
     * @param students HashSet<SisStudent>
     * @return true, if is section included
     */
    private boolean isSectionIncluded(MasterSchedule section,
                                      Map<String, ArrayList<HashSet<SisStudent>>> allClasses,
                                      HashSet<SisStudent> students) {

        ArrayList<HashSet<SisStudent>> staffClasses = null;

        String staffOid = section.getPrimaryStaffOid();
        boolean isStaffAttached = !StringUtils.isEmpty(staffOid);
        boolean isSameClass = false;

        if (isStaffAttached) {
            staffClasses = allClasses.get(staffOid);
            if (staffClasses == null) {
                staffClasses = new ArrayList<HashSet<SisStudent>>();
                allClasses.put(staffOid, staffClasses);
            }

            if (staffClasses.contains(students)) {
                isSameClass = true;
            } else {
                staffClasses.add(students);
            }
        }

        String departmentCode = section.getSchoolCourse().getCourse().getDepartmentCode();
        boolean isDepartClass = !StringUtils.isEmpty(departmentCode);
        // if staff not attached we assume that this is different classes
        return isDepartClass || !isStaffAttached || (isStaffAttached && !isSameClass);
    }

    /**
     * Load sped students.
     */
    private void loadSpedStudents() {
        X2Criteria programCodeCriteria = new X2Criteria();
        programCodeCriteria.addEqualTo(ReferenceCode.COL_CATEGORY, SPECIAL_EDUCATION_CATEGORY_OPTIONS);

        X2Criteria orCode07 = new X2Criteria();
        orCode07.addBeginsWith(ReferenceCode.COL_STATE_CODE, SPECIAL_EDUCATION_OPTION_07);
        X2Criteria orCode08 = new X2Criteria();
        orCode08.addBeginsWith(ReferenceCode.COL_STATE_CODE, SPECIAL_EDUCATION_OPTION_08);
        X2Criteria orCode09 = new X2Criteria();
        orCode09.addBeginsWith(ReferenceCode.COL_STATE_CODE, SPECIAL_EDUCATION_OPTION_09);
        orCode07.addOrCriteria(orCode08);
        orCode07.addOrCriteria(orCode09);

        programCodeCriteria.addAndCriteria(orCode07);

        SubQuery spedSubQuery = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, programCodeCriteria);
        X2Criteria programCriteria = new X2Criteria();
        programCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, spedSubQuery);
        programCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_endDate);

        X2Criteria programEndDateCriteria = new X2Criteria();
        X2Criteria programNullEndDateCriteria = new X2Criteria();
        programNullEndDateCriteria.addIsNull(StudentProgramParticipation.COL_END_DATE);
        programEndDateCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_startDate);
        programEndDateCriteria.addOrCriteria(programNullEndDateCriteria);

        programCriteria.addAndCriteria(programEndDateCriteria);

        SubQuery subQuery = new SubQuery(StudentProgramParticipation.class, StudentProgramParticipation.COL_STUDENT_OID,
                programCriteria, true);
        m_spedStdOids = new HashSet<String>(getBroker().getSubQueryCollectionByQuery(subQuery));
    }

    /**
     * Process Period defines by Semester Type(Fall or Spring), start and end dates.
     *
     * @param grid ReportDataGrid
     * @param semester String
     * @throws X2BaseException exception
     */
    private void processPeriod(ReportDataGrid grid, String semester) throws X2BaseException {
        Map<String, List> schedules = getSchedulesForPeriod();

        if (m_schools.size() == 0) {
            schedules = new HashMap();
        }

        Map<String, ArrayList<HashSet<SisStudent>>> allClasses = new HashMap<String, ArrayList<HashSet<SisStudent>>>();

        for (Entry sectionEntry : schedules.entrySet()) {
            MasterSchedule section = (MasterSchedule) getBroker().getBeanByOid(MasterSchedule.class,
                    (String) sectionEntry.getKey());

            List<StudentSchedule> stdSchedules = (List<StudentSchedule>) sectionEntry.getValue();
            Collection<String> grades = new TreeSet<String>();
            HashSet<SisStudent> students = new HashSet<SisStudent>();
            for (StudentSchedule stdSchedule : stdSchedules) {
                SisStudent student = stdSchedule.getStudent();
                grades.add(student.getGradeLevel());
                students.add(student);
            }

            if (isSectionIncluded(section, allClasses, students)) {
                SchoolCourse schoolCourse = section.getSchoolCourse();
                String sdeCode = (String) schoolCourse.getCourse().getFieldValueByAlias(ALIAS_COURSE_SDE_CODE);
                SisSchool school = section.getSchoolCourse().getSchool();

                grid.append();
                grid.set(FIELD_SCHOOL_NAME, school.getName());
                grid.set(FIELD_SCHOOL_NUMBER, school.getSchoolId());
                grid.set(FIELD_SCHOOL_SEMESTER, semester);
                Integer classSize = countAverageClassSize(section, students, school);

                Boolean isMultiAge = Boolean.valueOf(grades.size() > 1 ? true : false);
                grid.set(FIELD_IS_MULTIAGE, isMultiAge);

                String crsVocType = (String) schoolCourse.getCourse().getFieldValueByAlias(ALIAS_COURSE_VOC);
                Boolean isVoc = !StringUtils.isEmpty(crsVocType) ? Boolean.TRUE : Boolean.FALSE;
                grid.set(FIELD_IS_VOC, isVoc);

                // if all students in class are sped students,
                // then whole class is sped.

                HashSet<String> studentsOids = new HashSet<String>();
                for (SisStudent student : students) {
                    studentsOids.add(student.getOid());
                }
                Boolean isSped = m_spedStdOids.containsAll(studentsOids) ? Boolean.TRUE : Boolean.FALSE;
                grid.set(FIELD_IS_SPED, isSped);
                String groupCode = getGroupCode(schoolCourse, grades, isVoc);
                if (groupCode.equals(GRADE_GROUP_CODE_PK)) {
                    grid.set(FIELD_GROUP_CODE, GRADE_GROUP_CODE_PK);
                }
                if (!isSped.booleanValue() && !groupCode.equals(GRADE_GROUP_CODE_PK)) {
                    grid.set(FIELD_CLASS_NUMBER, schoolCourse.getNumber() + "-" + section.getSectionNumber());
                    grid.set(FIELD_GRADE_LEVELS, StringUtils.convertCollectionToDelimitedString(grades, DELIMITER));// grid.set(FIELD_GRADE_LEVELS,
                                                                                                                    // getGradeLevel(grades));

                    grid.set(FIELD_COURSE_SDE_CODE, sdeCode);
                    grid.set(FIELD_CLASS_SIZE, classSize);
                    grid.set(FIELD_GROUP_CODE, groupCode);
                    grid.set(FIELD_EIA_CLASS_SIZE, m_eiaClassSizes.get(groupCode));
                    String key = school.getSchoolId() + semester + groupCode;//
                    if (m_calculateMaxClassSize.containsKey(key)) {
                        int[] value = m_calculateMaxClassSize.get(key);
                        value[0] += classSize.intValue();
                        value[1] += 1;
                    } else {
                        Integer defaultMaxClassSize = (Integer) getParameter(groupCode.concat(PARAM_CODE_CLASS_SUF));//
                        int[] value = {classSize.intValue(), 1, defaultMaxClassSize.intValue()};
                        m_calculateMaxClassSize.put(key, value);
                    }
                }
            }
        }
        for (String key : m_calculateMaxClassSize.keySet()) {
            int[] value = m_calculateMaxClassSize.get(key);
            double minValue = value[0] / (double) value[1];
            minValue = value[2] < minValue ? value[2] : minValue;
            m_maxClassSize.put(key, m_decimalFormat.format(minValue));
        }
        addParameter(PARAM_MAX_CLASS_SIZE_MAP, m_maxClassSize);
    }
}
