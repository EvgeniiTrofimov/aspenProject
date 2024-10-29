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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.procedures.statereporting.tn.TNStateReportData;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Report class for DropoutReport.
 * Count amount of students who dropped out from school per race view per gender per grade level and
 * during the period
 * There are three types of report: for male, female and total.
 * School and period are input parameters for report.
 *
 * @author X2 Development Corporation
 */
public class DropoutReports1 extends ReportJavaSourceNet {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 

    /**
     * The Class SpanHelper.
     */
    class SpanHelper extends TNStateReportData {
        private TNEnrollmentHelper m_tnEnrHelper;
        private TNStudentHistoryHelper m_tnStudentHelper;
        private Map<String, ReferenceCode> m_studentGradeLvlMap = new HashMap<String, ReferenceCode>();

        /**
         * Return student's grade level.
         *
         * @param student SisStudent
         * @return ReferenceCode
         */
        public ReferenceCode getStudentGradeLvl(SisStudent student) {
            ReferenceCode gradeLvl = m_studentGradeLvlMap.get(student.getOid());
            if (gradeLvl == null) {
                // check that dates is correct
                gradeLvl = m_tnStudentHelper.getGradeLevelByDates(student, m_startDate, m_endDate);
                m_studentGradeLvlMap.put(student.getOid(), gradeLvl);
            }

            return gradeLvl;
        }


        /**
         * Checks if is context override.
         *
         * @return true, if is context override
         */
        public boolean isContextOverride() {
            return m_tnEnrHelper.getStudentMultiYearHelper().isContextOverride();
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

            m_tnStudentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_ATTENDANCE, Boolean.FALSE);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SCHEDULE_SPANS, Boolean.TRUE);

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.TRUE);
        }
    }

    private static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL STATE ID";

    private static final String ENROLLMENT_CODE = "01";

    private static final String GENDER_CODE_FEMALE = "Female";
    private static final String GENDER_CODE_MALE = "Male";
    private static final String GENDER_CODE_TOTAL = "Total";

    private static final String PARAM_ALL_SCHOOLS = "allSchools";
    private static final String PARAM_DISTRICT_SUMMARY = "includeDistrictSummary";
    private static final String PARAM_SCHOOLS = "schoolOids";
    private static final String PARAM_SUMMARY_ONLY = "summaryOnly";

    private static final String RACE_NON_HISP_2_OR_MORE = "Non-Hisp. 2 or More";
    private static final String RACE_HISPANIC_ALL_RACES = "Hispanic-All Races";
    private static final String RACE_NON_HISP_ASIAN = "Non-Hisp. Asian";
    private static final String RACE_NON_HISP_BLACK = "Non-Hisp. Black";
    private static final String RACE_NON_HISP_INDIAN = "Non-Hisp. Indian";
    private static final String RACE_NON_HISP_PACIFIC = "Non-Hisp. Pacific";
    private static final String RACE_NON_HISP_WHITE = "Non-Hisp. White";

    private static final String REPORT_PARAM_DATE_END = "dateEnd";
    private static final String REPORT_PARAM_DATE_START = "dateStart";
    private static final String REPORT_FIELD_GENDER = "gender";
    private static final String REPORT_FIELD_ORG_NAME = "orgName";
    private static final String REPORT_FIELD_RACE = "race";
    private static final String REPORT_FIELD_SCHOOL = "school";

    private static final String REPORT_PARAM_ORGANIZATION = "organization";
    private static final String REPORT_PARAM_USER = "user";

    private static final Map<String, String> s_genderCodes;
    private static final Map<String, String> s_races;


    private SpanHelper m_data;
    private DataDictionary m_dictionary;
    private Map<String, Map<String, Map<String, Integer>>> m_districtCounters;
    protected PlainDate m_endDate;
    private static final ArrayList<String> m_grades = new ArrayList(Arrays.asList("K", "01", "02",
            "03", "04", "05", "06",
            "07", "08", "09", "10",
            "11", "12"));
    private Boolean m_includeDistrictSummary;
    private Map<String, SisSchool> m_schoolNameMap;
    private List<SisSchool> m_schools;
    private Map<String, Map<String, Map<String, Map<String, Integer>>>> m_schoolsCounters;
    protected PlainDate m_startDate;
    private Boolean m_summaryOnly;
    static {
        s_genderCodes = new LinkedHashMap<String, String>();
        s_genderCodes.put("M", GENDER_CODE_MALE);
        s_genderCodes.put("F", GENDER_CODE_FEMALE);
        s_genderCodes.put("Total", GENDER_CODE_TOTAL);

        s_races = new LinkedHashMap<String, String>();
        s_races.put("HAR", RACE_HISPANIC_ALL_RACES);
        s_races.put("W", RACE_NON_HISP_WHITE);
        s_races.put("B", RACE_NON_HISP_BLACK);
        s_races.put("A", RACE_NON_HISP_ASIAN);
        s_races.put("I", RACE_NON_HISP_INDIAN);
        s_races.put("P", RACE_NON_HISP_PACIFIC);
        s_races.put("NHAR", RACE_NON_HISP_2_OR_MORE);
    }


    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        QueryByCriteria query = getEnrollmentQuery();
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        ReportDataGrid grid = new ReportDataGrid(10000, 20);

        // This collection need to generate reports for schools where students
        // was not dropout.
        Collection<SisSchool> schools = getSchools();
        if (schools.size() == 0) {
            return grid;
        }

        try {
            SisSchool school = null;
            while (iterator.hasNext()) {
                StudentEnrollment enrollment = (StudentEnrollment) iterator.next();
                if (validateEnrollment(enrollment)) {
                    if (school == null ||
                            !school.getOid().equals(enrollment.getSchoolOid())) {
                        school = enrollment.getSchool();


                    }
                    SisStudent student = enrollment.getStudent();

                    Map<String, Map<String, Map<String, Integer>>> schoolCounters = getSchoolCounters(school.getName());
                    m_schoolNameMap.put(school.getName(), school);
                    ReferenceCode gradeLevel = m_data.getStudentGradeLvl(student);
                    fillMap(student, schoolCounters.get(s_genderCodes.get(student.getPerson().getGenderCode())),
                            gradeLevel.getCode());
                }
            }
        } finally {
            if (iterator != null) {
                iterator.close();
            }
        }
        // add schools in the map
        for (SisSchool curSchool : schools) {
            m_schoolNameMap.put(curSchool.getName(), curSchool);
        }
        countTotalMap();

        if (!m_summaryOnly.booleanValue()) {
            for (SisSchool curSchool : schools) {
                Map<String, Map<String, Map<String, Integer>>> schoolCounters =
                        m_schoolsCounters.get(curSchool.getName());
                if (schoolCounters == null) {
                    schoolCounters = getSchoolCounters(curSchool.getName());
                }
                fillDataGrid(grid, curSchool.getName(), schoolCounters);
            }
        }

        if (m_includeDistrictSummary.booleanValue()) {
            populateWithZero(m_districtCounters);
            countDistrictMap();

            ReportDataGrid districtGrid = new ReportDataGrid();
            fillDataGrid(districtGrid, getOrganization().getName(), m_districtCounters);
            districtGrid.beforeTop();
            grid.append(districtGrid);
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        initializeData();
        m_endDate = m_data.getCurrentContext().getEndDate();
        m_startDate = m_data.getCurrentContext().getStartDate();

        if (m_endDate.before(m_startDate)) {
            throw new IllegalArgumentException("end date of the period must be equal or bigger than start date");
        }

        m_includeDistrictSummary = (Boolean) getParameter(PARAM_DISTRICT_SUMMARY);

        if (m_includeDistrictSummary == null || isSchoolContext()) {
            m_includeDistrictSummary = Boolean.FALSE;
        }

        m_summaryOnly = (Boolean) getParameter(PARAM_SUMMARY_ONLY);
        if (m_summaryOnly == null || isSchoolContext()) {
            m_summaryOnly = Boolean.FALSE;
        }

        m_schoolNameMap = new HashMap<String, SisSchool>();
        m_schoolsCounters = new TreeMap<String, Map<String, Map<String, Map<String, Integer>>>>();
        m_districtCounters = new TreeMap<String, Map<String, Map<String, Integer>>>();

        addParameter(REPORT_PARAM_DATE_START, m_startDate.toString());
        addParameter(REPORT_PARAM_DATE_END, m_endDate.toString());
        addParameter(REPORT_PARAM_ORGANIZATION, getOrganization());
        addParameter(REPORT_PARAM_USER, getUser());
    }

    /**
     * Calculate district counters.
     */
    private void countDistrictMap() {
        for (String name : m_schoolsCounters.keySet()) {
            for (String gender : s_genderCodes.values()) {
                for (String race : s_races.keySet()) {
                    for (String grade : m_grades) {
                        Integer value = Integer.valueOf(
                                m_schoolsCounters.get(name).get(gender).get(s_races.get(race)).get(grade).intValue() +
                                        m_districtCounters.get(gender).get(s_races.get(race)).get(grade).intValue());
                        m_districtCounters.get(gender).get(s_races.get(race)).put(grade, value);
                    }
                }
            }
        }

    }

    /**
     * counts student's amount from two maps (male and female), then puts values into the map
     * contains total amount.
     */
    private void countTotalMap() {
        for (String name : m_schoolsCounters.keySet()) {
            for (String race : s_races.keySet()) {
                for (String grade : m_grades) {
                    Integer counter = Integer.valueOf(m_schoolsCounters.get(name).get(GENDER_CODE_FEMALE)
                            .get(s_races.get(race)).get(grade).intValue() +
                            m_schoolsCounters.get(name).get(GENDER_CODE_MALE).get(s_races.get(race)).get(grade)
                                    .intValue());
                    m_schoolsCounters.get(name).get(GENDER_CODE_TOTAL).get(s_races.get(race)).put(grade, counter);
                }
            }
        }
    }

    /**
     * fills ReportDataGrid with values from the school map.
     *
     * @param dataGrid ReportDataGrid
     * @param orgName String
     * @param counters Map<String,Map<String,Map<String,Integer>>>
     */
    private void fillDataGrid(ReportDataGrid dataGrid,
                              String orgName,
                              Map<String, Map<String, Map<String, Integer>>> counters) {
        for (String gender : s_genderCodes.keySet()) {
            gender = s_genderCodes.get(gender);
            for (String race : s_races.keySet()) {
                dataGrid.append();
                dataGrid.set(REPORT_FIELD_RACE, s_races.get(race));
                dataGrid.set(REPORT_FIELD_SCHOOL, m_schoolNameMap.get(orgName));
                dataGrid.set(REPORT_FIELD_ORG_NAME, orgName);

                for (String grade : m_grades) {
                    dataGrid.set(grade, counters.get(gender).get(s_races.get(race)).get(grade));
                    dataGrid.set(REPORT_FIELD_GENDER, gender);
                }
            }
        }
    }

    /**
     * finds the suitable map according student's data, then count student's amount and put the
     * value in the map.
     *
     * @param student SisStudent
     * @param racesMap Map<String,Map<String,Integer>>
     * @param gradeLevel String
     */
    private void fillMap(SisStudent student, Map<String, Map<String, Integer>> racesMap, String gradeLevel) {
        String raceView = student.getPerson().getRaceView();

        Map<String, Integer> branch = getBranch(raceView, student.getPerson().getHispanicLatinoIndicator(), racesMap);
        branch.put(gradeLevel, Integer.valueOf(branch.get(gradeLevel).intValue() + 1));
    }

    /**
     * finds map by race view then returns it.
     *
     * @param raceView String
     * @param isHispanic boolean
     * @param racesMap Map<String,Map<String,Integer>>
     * @return map
     */
    private Map<String, Integer> getBranch(String raceView,
                                           boolean isHispanic,
                                           Map<String, Map<String, Integer>> racesMap) {
        if (!isHispanic) {
            return (s_races.keySet().contains(raceView)) ? racesMap.get(s_races.get(raceView))
                    : racesMap.get(RACE_NON_HISP_2_OR_MORE);
        }
        return racesMap.get(RACE_HISPANIC_ALL_RACES);
    }

    /**
     * Return studentEnrollment query with enrollment type "W", enrollment code "01" and
     * students with grade from K to 12.
     *
     * @return Query by criteria
     */
    private QueryByCriteria getEnrollmentQuery() {


        Criteria studentEnrollmentCriteria = new Criteria();
        studentEnrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_startDate);
        studentEnrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_endDate);
        studentEnrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
        studentEnrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_CODE, ENROLLMENT_CODE);

        List<String> schoolOids = new LinkedList<>();
        for (SisSchool school : getSchools()) {
            schoolOids.add(school.getOid());
        }
        studentEnrollmentCriteria.addIn(StudentEnrollment.COL_SCHOOL_OID, schoolOids);
        // for override context case see validateEnrollment method
        if (!m_data.isContextOverride()) {
            Criteria studentCriteria = new Criteria();
            studentCriteria.addIn(SisStudent.COL_GRADE_LEVEL, m_grades);
            SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
            studentEnrollmentCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, subQuery);
        }
        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, studentEnrollmentCriteria);
        query.addOrderBy(StudentEnrollment.COL_SCHOOL_OID, true);
        return query;
    }

    /**
     * creates tree for each school according to races, genders and grades. then puts it into school
     * map.
     *
     * @param schoolName String
     * @return Map
     */
    private Map<String, Map<String, Map<String, Integer>>> getSchoolCounters(String schoolName) {
        if (!m_schoolsCounters.containsKey(schoolName)) {
            m_schoolsCounters.put(schoolName, new LinkedHashMap());
            Map<String, Map<String, Map<String, Integer>>> schoolCounter = m_schoolsCounters.get(schoolName);
            populateWithZero(schoolCounter);
        }
        return m_schoolsCounters.get(schoolName);
    }

    /**
     * Return collection of schools.
     *
     * @return Collection<SisSchool> collection of schools
     */
    private Collection<SisSchool> getSchools() {
        if (m_schools == null) {
            Object objIsAllSchools = getParameter(PARAM_ALL_SCHOOLS);
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
                m_schools = new LinkedList(getBroker().getCollectionByQuery(schoolQuery));
            } else {
                Object objSchools = getParameter(PARAM_SCHOOLS);
                String schoolOids = objSchools == null ? "" : (String) objSchools;
                if (!StringUtils.isEmpty(schoolOids)) {
                    List<String> oids = Arrays.asList(schoolOids.split(","));
                    X2Criteria schoolCriteria = new X2Criteria();

                    schoolCriteria.addIn(X2BaseBean.COL_OID, oids);

                    QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
                    schoolQuery.addOrderByAscending(SisSchool.COL_NAME);
                    m_schools = new LinkedList(getBroker().getCollectionByQuery(schoolQuery));
                }
            }
        }
        if (m_schools == null) {
            m_schools = new LinkedList();
        }

        return m_schools;
    }

    /**
     * initialize m_data.
     *
     * @throws X2BaseException exception
     */
    private void initializeData() throws X2BaseException {
        m_data = new SpanHelper();
        m_data.setBroker(getBroker());
        m_data.setOrganization(getOrganization());
        m_data.setPrivilegeSet(getPrivilegeSet());
        if (getSchools().size() == 1) {
            m_data.setSchoolContext(true);
            m_data.setSchool(getSchools().iterator().next());
        } else {
            m_data.setSchoolContext(false);
        }

        m_data.setParameters(getParameters());
        m_data.setUser(getUser());
        m_data.initializeExport();
    }

    /**
     * Populate with zero.
     *
     * @param schoolCounter Map<String,Map<String,Map<String,Integer>>>
     */
    private void populateWithZero(Map<String, Map<String, Map<String, Integer>>> schoolCounter) {
        for (String gender : s_genderCodes.keySet()) {
            gender = s_genderCodes.get(gender);
            schoolCounter.put(gender, new LinkedHashMap<String, Map<String, Integer>>());
            for (String raceKey : s_races.keySet()) {
                schoolCounter.get(gender).put(s_races.get(raceKey), new HashMap<String, Integer>());
                for (String grade : m_grades) {
                    schoolCounter.get(gender).get(s_races.get(raceKey)).put(grade, Integer.valueOf(0));
                }
            }
        }
    }

    /**
     * make additional check - does it appropriate records<br>
     * it's require because we haven't simple way to determine grade level for previous than current
     * context<br>
     * information about grade level can be located on student context record or on enrollments
     * records (span)<br>
     * In case of enrollment records more easy way to find grade level - it is use span. We can use
     * span in java, not in criteria.
     * these method need for both cases: override context true and false
     *
     * @param enrollment StudentEnrollment
     * @return true, if successful
     */
    private boolean validateEnrollment(StudentEnrollment enrollment) {
        boolean isValid = false;
        SisStudent student = enrollment.getStudent();
        ReferenceCode code = m_data.getStudentGradeLvl(student);
        String gradeLevel = code.getCode();
        if (m_grades.contains(gradeLevel)) {
            isValid = true;
        }

        return isValid;
    }
}
