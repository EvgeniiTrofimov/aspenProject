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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import net.sf.jasperreports3.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Report class for TN Preliminary Staff Report.
 * Count unique StaffPositions per school per Semester.
 * Defines two Semesters: Fall and Spring.
 * StaffPosition groups by its category.
 * Category defines in inputParams.
 * 
 * @author X2 Development Corporation
 */
public class PreliminaryStaffReportData extends ReportJavaSourceNet {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 
    // Aliases
    private static final String ALIAS_COURSE_CODE = "DOE SDE COURSE CODE";
    private static final String ALIAS_EMPLOY_STATUS = "DOE EMPLOY STATUS";
    private static final String ALIAS_EXCLUDE_STUDENT = "DOE EXCLUDE STD";
    private static final String ALIAS_LICENSE_NUMBER = "DOE LICENSE NUMBER";
    private static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL STATE ID";

    // Employ status
    private static final String EMPLOY_STATUS_F = "F";
    private static final String EMPLOY_STATUS_P = "P";

    // DataGrid columns
    private static final String FIELD_CLASS_NUMBER = "classNumber";
    private static final String FIELD_COURSE_SDE_CODE = "courseSDECode";
    private static final String FIELD_DO_NOT_SHOW = "doNotShow";
    private static final String FIELD_IS_FULL_TIME = "isFullTime";
    private static final String FIELD_IS_PART_TIME = "isPartTime";
    private static final String FIELD_NUMBER_OF_STUDENT = "numberOfStudent";
    private static final String FIELD_SCHOOL_ID = "schoolId";
    private static final String FIELD_SCHOOL_NAME = "schoolName";
    private static final String PARAM_SCHOOL_TERM = "schoolTerm";
    private static final String FIELD_STAFF_JOB_CAT = "staffJobCategorie";
    private static final String FIELD_STAFF_LICENSE = "staffLicense";
    private static final String FIELD_STAFF_NAME = "staffName";
    private static final String FIELD_STAFF_SSN = "staffSSN";

    // Input params and erportDesign params
    private static final String PARAM_ALL_SCHOOLS = "allSchools";
    private static final String PARAM_CHAR_DELIMITER = "charDelimiter";
    private static final String PARAM_CONTEXT = "contex";
    private static final String PARAM_CONTEXT_OID = "schoolYearOid";
    private static final String PARAM_DISTRICT = "district";
    private static final String PARAM_EXCLUDE_INACTIVE = "inactiveStaffExclude";
    private static final String PARAM_JOB_CODE_TEMPLATE = "staffJobCode";
    private static final String PARAM_SCHOOLS = "schoolOids";
    private static final String PARAM_REPORT_DATE = "reportDate";
    private static final String PARAM_STAFF_CATEGORIES = "staffCatCode";
    private static final String PARAM_SEMESTER = "semester";

    private static final String SEMESTER_FALL = "Fall";
    private static final String SEMESTER_SPRING = "Spring";

    private static final String STATUS_ACTIVE = "Active";

    // fields
    private final HashMap<String, HashSet> m_categories = new HashMap<String, HashSet>();
    private DistrictSchoolYearContext m_context;
    private DataDictionary m_dictionary;
    private Map<String, PlainDate> m_endDate;
    private Collection<String> m_schoolOids;
    private Collection<School> m_schools;
    private Integer m_semesterChoice;
    private Map<String, PlainDate> m_startDate;
    private PlainDate m_startDateTotal;
    private PlainDate m_endDateTotal;

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

        processPeriod(grid, m_startDate, m_endDate);

        List sortColumns = new ArrayList();
        sortColumns.add(FIELD_SCHOOL_NAME);
        sortColumns.add(PARAM_SCHOOL_TERM);
        sortColumns.add(FIELD_STAFF_NAME);
        sortColumns.add(FIELD_COURSE_SDE_CODE);
        grid.sort(sortColumns, true);
        // if there are no recipients, use an empty grid to print a blank report
        if (grid.rowCount() == 0) {
            grid.append();
        }
        grid.beforeTop();
        return grid;
    }

    /**
     * Get InputParams, load jobCodes and JobCategories.
     * Defines yearContext
     */
    @Override
    protected void initialize() {
        String districtOid = (String) getParameter(PARAM_CONTEXT_OID);
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_context = (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class,
                districtOid);
        m_semesterChoice = (Integer) getParameter(PARAM_SEMESTER);
        m_schools = getSchools();
        m_schoolOids = new ArrayList<String>();
        for (School school : m_schools) {
            m_schoolOids.add(school.getOid());
        }
        initDates();

        addParameter(PARAM_REPORT_DATE, new PlainDate());
        addParameter(PARAM_DISTRICT, getOrganization());
        addParameter(PARAM_CONTEXT, m_context);
        addParameter(PARAM_SCHOOL_TERM, m_semesterChoice.intValue() == 0 ? SEMESTER_FALL : SEMESTER_SPRING);

        // loading job codes from input param
        String delimiter = ((String) getParameter(PARAM_CHAR_DELIMITER));
        String[] categories = ((String) getParameter(PARAM_STAFF_CATEGORIES)).split(delimiter);
        for (String categorie : categories) {
            String jobCodeSecuence = ((String) getParameter(PARAM_JOB_CODE_TEMPLATE + categorie));
            if ((jobCodeSecuence != null) && !jobCodeSecuence.isEmpty()) {
                HashSet codes =
                        new HashSet(StringUtils.convertDelimitedStringToList(jobCodeSecuence, delimiter.charAt(0),
                                true));
                m_categories.put(categorie, codes);
            }
        }
    }

    /**
     * Count unique students per course.
     *
     * @param list List
     * @return int
     */
    private int countStudent(List list) {
        HashSet<String> students = new HashSet<String>();
        for (StudentSchedule schedule : ((List<StudentSchedule>) list)) {
            if (!students.contains(schedule.getStudentOid())) {
                students.add(schedule.getStudentOid());
            }
        }

        return students.size();
    }

    /**
     * Simple method filling in grid data repeated per each course.
     *
     * @param grid DataGrid
     * @param school SisSchool
     * @param staff SisStaff
     * @param position StaffPosition
     * @param doNotShow Boolean
     */
    private void fillGridWithBasicInfo(DataGrid grid,
                                       SisSchool school,
                                       SisStaff staff,
                                       StaffPosition position,
                                       Boolean doNotShow) {
        grid.append();
        grid.set(FIELD_STAFF_NAME, staff.getNameView());
        grid.set(FIELD_STAFF_JOB_CAT, getJobCategory(position.getJobCode()));
        grid.set(FIELD_IS_FULL_TIME,
                Boolean.valueOf(EMPLOY_STATUS_F.equals(staff.getFieldValueByAlias(ALIAS_EMPLOY_STATUS))
                        ? true : false));
        grid.set(FIELD_IS_PART_TIME,
                Boolean.valueOf(EMPLOY_STATUS_P.equals(staff.getFieldValueByAlias(ALIAS_EMPLOY_STATUS))
                        ? true : false));
        grid.set(FIELD_STAFF_LICENSE, staff.getFieldValueByAlias(ALIAS_LICENSE_NUMBER));
        grid.set(FIELD_STAFF_SSN, staff.getPerson().getPersonId());
        grid.set(FIELD_SCHOOL_NAME, school.getName());
        grid.set(FIELD_SCHOOL_ID, school.getSchoolId());
        grid.set(FIELD_DO_NOT_SHOW, doNotShow);
    }

    /**
     * Find conformity of jobCode and jobCategory.
     * Connection of jobCode and JobCategory came from inputParams
     * and loaded by initialize().
     *
     * @param jobCode String
     * @return jobCategory or null
     */
    private String getJobCategory(String jobCode) {
        for (Entry entry : m_categories.entrySet()) {
            if (((HashSet) entry.getValue()).contains(jobCode)) {
                return (String) entry.getKey();
            }
        }
        return null;
    }

    /**
     * Load StudentSchedule's list grouped in map's by school, by staff, by schoolCourse
     * respectively.
     *
     * @return Map&lt;String, Map&lt;String, Map&ltString, List&ltStudentSchedules&gt;&gt;&gt;&gt;
     */
    private Map<String, Map<String, Map<String, List<StudentSchedule>>>> getSchedulesForPeriod() {
        X2Criteria criteria = new X2Criteria();
        X2Criteria termCriteria = new X2Criteria();
        termCriteria.addLessOrEqualThan(ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                ScheduleTermDate.COL_START_DATE, m_endDateTotal);
        termCriteria.addGreaterOrEqualThan(ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                ScheduleTermDate.COL_END_DATE, m_startDateTotal);

        termCriteria.addIn(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID, m_schoolOids);

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

        criteria.addIn(StudentSchedule.REL_SECTION + PATH_DELIMITER + Section.COL_SCHEDULE_TERM_OID, scheduleTerms);

        criteria.addNotEmpty(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                Section.COL_PRIMARY_STAFF_OID, getBroker().getPersistenceKey());

        // Student Table STD_FIELDA_006, alias �DOE EXCLUDE STD� � if true do not report this
        // student
        DataDictionaryField stdExcludeField =
                DataDictionary.getDistrictDictionary(getUser().getPersistenceKey())
                        .findDataDictionaryFieldByAlias(ALIAS_EXCLUDE_STUDENT);
        criteria.addNotEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER + stdExcludeField.getJavaName(),
                Boolean.TRUE);

        criteria.addIn(StudentSchedule.REL_STUDENT + PATH_DELIMITER + Student.REL_SCHOOL +
                PATH_DELIMITER + X2BaseBean.COL_OID, m_schoolOids);

        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, criteria);
        String[] groupColumns =
                new String[] {StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE +
                        PATH_DELIMITER + SchoolCourse.COL_SCHOOL_OID,
                        StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.COL_PRIMARY_STAFF_OID,
                        StudentSchedule.REL_SECTION + PATH_DELIMITER + X2BaseBean.COL_OID};

        return getBroker().getGroupedCollectionByQuery(query, groupColumns, new int[] {10, 25, 25});
    }

    /**
     * Return collection of schools.
     *
     * @return Collection<SisSchool> collection of schools
     */
    private Collection<School> getSchools() {
        Collection<School> schools = null;
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
            schools = getBroker().getCollectionByQuery(schoolQuery);
        } else {
            schools = new LinkedList();
            Object objSchools = getParameter(PARAM_SCHOOLS);
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
     * Load StaffPositions List grouped in Map by schoolOid, by staffOid respectively.
     * Each list will contain only actual StaffPosition for jobCategory.
     * Can be situation when List will have two or more StaffPosition for one staff in one school.
     *
     * @return Map&lt;String, Map&lt;String, List&gt;&gt;
     */
    private Map<String, Map<String, List<StaffPosition>>> getStaffPositions() {
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

        if (((Boolean) getParameter(PARAM_EXCLUDE_INACTIVE)).booleanValue()) {
            positionsCriteria.addEqualTo(StaffPosition.REL_STAFF + PATH_DELIMITER + Staff.COL_STATUS, STATUS_ACTIVE);
        }

        positionsCriteria.addIn(StaffPosition.COL_SCHOOL_OID, m_schoolOids);

        QueryByCriteria positionsQuery = new QueryByCriteria(StaffPosition.class, positionsCriteria);

        String[] groupColumns = new String[] {
                StaffPosition.COL_SCHOOL_OID, StaffPosition.COL_STAFF_OID};

        Map<String, Map<String, List<StaffPosition>>> positionsBySchool =
                getBroker().getGroupedCollectionByQuery(positionsQuery,
                        groupColumns,
                        new int[] {10, 10});
        // also removes positions that are not included in the time-frame for their school
        removeOldPositions(positionsBySchool);

        // add selected school that not contained staff positions to print zeros
        X2Criteria schoolCriteria = new X2Criteria();

        schoolCriteria.addIn(X2BaseBean.COL_OID, m_schoolOids);

        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
        QueryIterator schoolIterator = getBroker().getIteratorByQuery(schoolQuery);
        try {
            while (schoolIterator.hasNext()) {
                SisSchool school = (SisSchool) schoolIterator.next();
                if (!positionsBySchool.keySet().contains(school.getOid())) {
                    positionsBySchool.put(school.getOid(), null);
                }
            }
        } finally {
            schoolIterator.close();
        }
        // There is no selected schools.
        if (m_schools.size() == 0) {
            positionsBySchool = new HashMap();
        }
        return positionsBySchool;
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
     * Process Period defines by Semester Type(Fall or Spring), start and end dates.
     *
     * @param grid ReportDataGrid
     * @param startDate Map<String,PlainDate>
     * @param endDate Map<String,PlainDate>
     */
    private void processPeriod(ReportDataGrid grid, Map<String, PlainDate> startDate, Map<String, PlainDate> endDate) {
        Map<String, Map<String, List<StaffPosition>>> positionsBySchool = new HashMap();
        if (m_schoolOids.size() != 0) {
            positionsBySchool = getStaffPositions();
        }

        Map<String, Map<String, Map<String, List<StudentSchedule>>>> schedules = getSchedulesForPeriod();
        for (Entry schoolEntry : positionsBySchool.entrySet()) {
            SisSchool school = (SisSchool) getBroker().getBeanByOid(SisSchool.class, (String) schoolEntry.getKey());
            int currentRowBefore = grid.rowCount();
            if (((Map<String, List>) schoolEntry.getValue()) != null) {
                for (Entry staffEntry : ((Map<String, List>) schoolEntry.getValue()).entrySet()) {
                    SisStaff staff = (SisStaff) getBroker().getBeanByOid(SisStaff.class, (String) staffEntry.getKey());
                    List<StaffPosition> positionsByStaff = (List<StaffPosition>) staffEntry.getValue();
                    Boolean isCoursesProcessed = Boolean.FALSE;
                    for (StaffPosition position : positionsByStaff) {
                        if (!isCoursesProcessed.booleanValue()) {
                            Map<String, List<StudentSchedule>> stdSchoolCourses = null;
                            if ((schedules.get(schoolEntry.getKey())) != null) {
                                stdSchoolCourses = schedules.get(schoolEntry.getKey()).get(staff.getOid());
                            }
                            if (stdSchoolCourses != null) {
                                for (Entry sectionEntry : stdSchoolCourses.entrySet()) {
                                    MasterSchedule section =
                                            (MasterSchedule) getBroker().getBeanByOid(MasterSchedule.class,
                                                    (String) sectionEntry.getKey());
                                    fillGridWithBasicInfo(grid, school, staff, position, isCoursesProcessed);
                                    grid.set(FIELD_COURSE_SDE_CODE, section.getSchoolCourse().getCourse()
                                            .getFieldValueByAlias(ALIAS_COURSE_CODE));
                                    grid.set(FIELD_CLASS_NUMBER, section.getCourseView());
                                    int countStudent = countStudent(((List) sectionEntry.getValue()));
                                    grid.set(FIELD_NUMBER_OF_STUDENT, String.valueOf(countStudent));
                                }
                            } else {
                                fillGridWithBasicInfo(grid, school, staff, position, isCoursesProcessed);
                            }
                            isCoursesProcessed = Boolean.TRUE;
                        } else {
                            fillGridWithBasicInfo(grid, school, staff, position, isCoursesProcessed);
                        }
                    }
                }
            }
            int currentRowAfter = grid.rowCount();
            // if rows weren't added, then add report for school with zeros
            if (currentRowBefore == currentRowAfter) {
                grid.append();
                grid.set(FIELD_SCHOOL_NAME, school.getName());
                grid.set(FIELD_SCHOOL_ID, school.getSchoolId());
            }
        }
    }

    /**
     * Need to determine actual staff position's for current school.
     * Position with highest startDate will recognized like actual.
     * positionList should contain only one staffPosition for one
     * jobCategory
     *
     * @param positionsBySchool Map<String,Map<String,List<StaffPosition>>>
     */
    private void removeOldPositions(Map<String, Map<String, List<StaffPosition>>> positionsBySchool) {
        for (Entry<String, Map<String, List<StaffPosition>>> entry : positionsBySchool.entrySet()) {
            Map<String, List<StaffPosition>> positionsByStaff = entry.getValue();
            String schoolOid = entry.getKey();
            PlainDate startDate = (m_startDate.containsKey(schoolOid)) ? m_startDate.get(schoolOid) : m_startDateTotal;
            // PlainDate endDate = (m_endDate.containsKey(schoolOid)) ? m_endDate.get(schoolOid) :
            // m_endDateTotal;

            for (List<StaffPosition> positionsList : positionsByStaff.values()) {
                HashMap<String, StaffPosition> uniquePositions = new HashMap<String, StaffPosition>();
                for (StaffPosition currentStaffPosition : positionsList) {
                    if (!currentStaffPosition.getStartDate().before(startDate)) {

                        String positionCategory = getJobCategory(currentStaffPosition.getJobCode());
                        StaffPosition uniqueStaffPosition = uniquePositions.get(positionCategory);
                        if (uniqueStaffPosition == null) {
                            uniquePositions.put(positionCategory, currentStaffPosition);
                        } else if ((currentStaffPosition.getStartDate() != null) &&
                                (uniqueStaffPosition.getStartDate() != null) &&
                                currentStaffPosition.getStartDate().after(uniqueStaffPosition.getStartDate())) {
                            uniquePositions.put(positionCategory, currentStaffPosition);
                        }
                    }
                }
                positionsList.clear();
                positionsList.addAll(uniquePositions.values());
            }
        }
    }
}
