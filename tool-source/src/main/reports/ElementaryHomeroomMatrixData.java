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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.beans.Schedule.ScheduleMode;
import com.x2dev.sis.model.business.ElementaryScheduleManager;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.model.business.schedule.ScheduleStructureManager;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.TimeUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Elementary Homeroom Schedule Matrix" report.
 *
 * @author X2 Development Corporation
 */
public class ElementaryHomeroomMatrixData extends ReportJavaSourceNet {
    /**
     * Name for the "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    public static final String STUDENT_SCHEDULE_GROUP_ID = "groupIds";

    // Grid fields
    private static final String FIELD_PERIOD_NUMBER = "period";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_HOMEROOM = "homeroom";
    private static final String FIELD_PLATOON = "platoon";

    // Report parameters
    private static final String DAY_ID_LOOKUP_PARAM = "dayIdLookup";
    private static final String HOMEROOM_MAP_PARAM = "homeroomMap";
    private static final String PERIOD_ID_LOOKUP_PARAM = "periodIdLookup";

    // The initial column size
    private static final int INITIAL_COLUMN_SIZE = 15;

    // The initial row size if the report is run for a single student
    private static final int INITIAL_ROW_SIZE_SINGLE = 15;

    private static final long serialVersionUID = 1L;

    private int m_initialRowSize;
    private ScheduleReportHelper m_reportHelper;
    private ScheduleManager m_scheduleManager;
    private Map<String, StudentScheduleAttributes> m_studentsWithPlatoonMap;
    private Map<String, Collection<Section>> m_sectionsBySectionTypeMap;
    private Map<String, Boolean> m_subGroupIndicatorByCourseOidMap;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        m_scheduleManager = new ScheduleManager(getBroker());

        /*
         * The list of term oids
         */
        X2Criteria termCriteria = new X2Criteria();
        termCriteria.addEqualTo(ScheduleTerm.COL_SCHEDULE_OID, m_reportHelper.getTimeScheduleOid());

        SubQuery termQuery = new SubQuery(ScheduleTerm.class, X2BaseBean.COL_OID, termCriteria);
        Collection<String> termOids = getBroker().getSubQueryCollectionByQuery(termQuery);

        /*
         * Retrieve all sections by section type except lunch
         */
        X2Criteria sectionCriteria = new X2Criteria();
        sectionCriteria.addEqualTo(Section.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());
        sectionCriteria.addEqualTo(Section.COL_ELEMENTARY_INDICATOR, Boolean.TRUE);
        sectionCriteria.addNotEqualTo(Section.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_MASTER_TYPE,
                SchoolCourse.MASTER_TYPE_LUNCH);
        sectionCriteria.addNotEmpty(Section.COL_SECTION_TYPE, getBroker().getPersistenceKey());

        QueryByCriteria sectionQuery = new QueryByCriteria(m_reportHelper.getSectionClass(), sectionCriteria);
        m_sectionsBySectionTypeMap =
                getBroker().getGroupedCollectionByQuery(sectionQuery, Section.COL_SECTION_TYPE, 1000);

        ModelBroker modelBroker = new ModelBroker(getPrivilegeSet());

        /*
         * The list course that should include subgroup
         */
        m_subGroupIndicatorByCourseOidMap =
                ElementaryScheduleManager.getSubGroupIndicatorByCourseOid(m_reportHelper.getSchedule(), modelBroker);


        /*
         * Build the list of platoon by students map
         */
        X2Criteria studentAttributeCriteria = new X2Criteria();
        studentAttributeCriteria.addEqualTo(StudentScheduleAttributes.COL_SCHEDULE_OID,
                m_reportHelper.getStudentScheduleOid());
        studentAttributeCriteria.addNotEmpty(StudentScheduleAttributes.COL_SCHEDULE_PLATOON_CODE,
                getBroker().getPersistenceKey());

        QueryByCriteria studentAttributeQuery =
                new QueryByCriteria(StudentScheduleAttributes.class, studentAttributeCriteria);
        m_studentsWithPlatoonMap =
                getBroker().getMapByQuery(studentAttributeQuery, StudentScheduleAttributes.COL_STUDENT_OID, 1000);

        boolean useNextHomeroom = !m_reportHelper.getSchedule().getDistrictContextOid()
                .equals(m_reportHelper.getSchedule().getSchool().getCurrentContextOid());

        /*
         * Build the criteria based on user input
         */
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentSection.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());

        String selectedGroupOids = (String) getParameter(STUDENT_SCHEDULE_GROUP_ID);
        if (!StringUtils.isEmpty(selectedGroupOids)) {

            X2Criteria studentGroupCriteria = new X2Criteria();
            studentGroupCriteria.addEqualTo(StudentScheduleGroup.COL_DISTRICT_CONTEXT_OID,
                    m_reportHelper.getSchedule().getDistrictContextOid());
            studentGroupCriteria.addEqualTo(StudentScheduleGroup.COL_SCHOOL_OID,
                    m_reportHelper.getSchedule().getSchoolOid());
            studentGroupCriteria.addIn(X2BaseBean.COL_OID,
                    StringUtils.convertDelimitedStringToList(selectedGroupOids, ","));

            SubQuery homeroomSubQuery =
                    new SubQuery(StudentScheduleGroup.class, StudentScheduleGroup.COL_ID, studentGroupCriteria);

            if (useNextHomeroom) {
                criteria.addEqualTo(StudentSection.REL_STUDENT + PATH_DELIMITER + Student.COL_NEXT_SCHOOL_OID,
                        m_reportHelper.getSchedule().getSchoolOid());
                criteria.addIn(StudentSection.REL_STUDENT + PATH_DELIMITER + Student.COL_NEXT_HOMEROOM,
                        getBroker().getSubQueryCollectionByQuery(homeroomSubQuery));
            } else {
                criteria.addEqualTo(StudentSection.REL_STUDENT + PATH_DELIMITER + Student.COL_SCHOOL_OID,
                        m_reportHelper.getSchedule().getSchoolOid());
                criteria.addIn(StudentSection.REL_STUDENT + PATH_DELIMITER + Student.COL_HOMEROOM,
                        getBroker().getSubQueryCollectionByQuery(homeroomSubQuery));
            }
        }
        m_initialRowSize = INITIAL_ROW_SIZE_SINGLE;

        QueryByCriteria query = new QueryByCriteria(m_reportHelper.getStudentSectionClass(), criteria);

        /*
         * The student sort order always has to end with the student OID so that schedules are
         * grouped properly.
         */
        if (useNextHomeroom) {
            query.addOrderByAscending(StudentSection.REL_STUDENT + PATH_DELIMITER +
                    Student.COL_NEXT_HOMEROOM);
        } else {
            query.addOrderByAscending(StudentSection.REL_STUDENT + PATH_DELIMITER +
                    Student.COL_HOMEROOM);
        }
        // query.addOrderByAscending(StudentSection.REL_SECTION + "." + Section.COL_PLATOON_CODE);
        query.addOrderByAscending(StudentSection.COL_STUDENT_OID);
        query.addOrderByDescending(StudentSection.REL_SECTION + PATH_DELIMITER + Section.COL_TERM_VIEW);

        /*
         * Execute the query and build the matrices
         */
        ReportDataGrid grid = new ReportDataGrid(10);
        Iterator scheduleIterator = getBroker().getIteratorByQuery(query);

        try {
            grid = buildMatrices(scheduleIterator, termOids, useNextHomeroom);
        } finally {
            if (scheduleIterator instanceof QueryIterator) {
                ((QueryIterator) scheduleIterator).close();
            }
        }

        /*
         * Set the report parameters
         */
        if (m_reportHelper.getSchedule() != null) {
            HashMap periodIdLookup = updatePeriodIdMap(grid);
            addParameter(PERIOD_ID_LOOKUP_PARAM, periodIdLookup);

            HashMap dayIdLookup = new HashMap(15);
            Iterator days = m_reportHelper.getSchedule().getScheduleDays(true).iterator();
            while (days.hasNext()) {
                ScheduleDay day = (ScheduleDay) days.next();
                dayIdLookup.put(Integer.valueOf(day.getNumber()), day.getName());
            }
            addParameter(DAY_ID_LOOKUP_PARAM, dayIdLookup);
        }

        addParameter(HOMEROOM_MAP_PARAM,
                ReportUtils.buildHomeroomToStaffMap(getBroker(), getOrganization(), getSchool()));

        grid.sort(FIELD_HOMEROOM, true);
        grid.sort(FIELD_PLATOON, true);

        grid.beforeTop();

        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_reportHelper = new ScheduleReportHelper(userData);
    }

    /**
     * Builds a ReportDataGrid object representing one ore more student schedule matrices. The
     * matrices are generated from the passed Iterator over StudentSchedule beans which must be
     * in student order.
     * <p>
     * The returned ReportDataGrid contains a row per student, per period. A column per day exists
     * as well as a column for the student bean.
     *
     * @param scheduleIterator Iterator
     * @param termOids Collection<String>
     * @param useNextHomeroom
     * @return ReportDataGrid
     */
    private ReportDataGrid buildMatrices(Iterator scheduleIterator,
                                         Collection<String> termOids,
                                         boolean useNextHomeroom) {
        ReportDataGrid grid = new ReportDataGrid(m_initialRowSize, INITIAL_COLUMN_SIZE);

        Collection<String> homeroomPlatoonList = new HashSet<String>();
        Map<String, Collection<Section>> sectionsIncludedByHomeroomPlatoon = new HashMap<String, Collection<Section>>();
        Map<String, Boolean> subGroupIndicatorByCourseOid =
                ElementaryScheduleManager.getSubGroupIndicatorByCourseOid(m_reportHelper.getSchedule(), getBroker());

        String homeroom = "";
        String platoonCode = "";
        String groupCode = "";
        String lastHomeroom = "";
        String lastPlatoonCode = "";
        String lastGroupCode = "";
        StudentScheduleGroup studentScheduleGroup = null;

        if (m_reportHelper.getSchedule() != null) {
            SisStudent lastStudent = null;
            Map periodRowMap = null;
            while (scheduleIterator.hasNext()) {
                StudentSection studentSchedule = (StudentSection) scheduleIterator.next();
                Section section = studentSchedule.getSection();
                SisStudent student = studentSchedule.getStudent();

                homeroom = useNextHomeroom ? student.getNextHomeroom().trim() : student.getHomeroom().trim();
                StudentScheduleAttributes studentScheduleAttribute = m_studentsWithPlatoonMap.get(student.getOid());
                platoonCode =
                        studentScheduleAttribute == null ? " "
                                : studentScheduleAttribute.getSchedulePlatoonCode().trim();

                /*
                 * Including the student if
                 * 1. It is the first section
                 * 2. It is the section for the same last student
                 * 3. It is a new student with different homeroom or platoon.
                 */
                groupCode = homeroom + "-" + platoonCode;
                boolean includeSection = lastStudent == null ||
                        lastStudent.equals(student) ||
                        (!lastStudent.equals(student) && !homeroomPlatoonList.contains(groupCode));

                if (includeSection) {

                    if (lastStudent == null || !lastStudent.equals(student)) {
                        if (lastStudent != null) {

                            // logToolMessage(Level.INFO, "last student: " +
                            // lastStudent.getNameView() + " " + lastStudent.getLocalId() +
                            // " current student: " + student.getNameView() + " " +
                            // student.getLocalId(), false);
                            studentScheduleGroup =
                                    getStudentGroupForSection(lastHomeroom, lastPlatoonCode, section.getSchedule());

                            addAdditionalSection(lastHomeroom, lastPlatoonCode, lastGroupCode,
                                    studentScheduleGroup, grid, periodRowMap,
                                    termOids, sectionsIncludedByHomeroomPlatoon, subGroupIndicatorByCourseOid);
                            updatePeriodRowDisplay(grid, lastStudent, periodRowMap);
                        }

                        periodRowMap = initializeGridForStudent(grid, student, homeroom, platoonCode);
                        homeroomPlatoonList.add(groupCode);
                    }

                    lastStudent = student;
                    lastHomeroom = homeroom;
                    lastPlatoonCode = platoonCode;
                    lastGroupCode = groupCode;

                    if (section != null) {

                        populateMatrix(grid, section, studentSchedule, periodRowMap, termOids);

                        Collection<Section> sectionsIncluded = sectionsIncludedByHomeroomPlatoon.get(groupCode);
                        if (sectionsIncluded == null) {
                            sectionsIncluded = new ArrayList<Section>();
                        }
                        sectionsIncluded.add(section);
                        sectionsIncludedByHomeroomPlatoon.put(groupCode, sectionsIncluded);
                    }
                }
            }

            /*
             * Updates the PeriodRow display status for the last student
             */
            // logToolMessage(Level.INFO, "last stuent only: " + lastStudent.getNameView(), false);
            studentScheduleGroup =
                    getStudentGroupForSection(homeroom, platoonCode, m_reportHelper.getSchedule());
            addAdditionalSection(homeroom, platoonCode, groupCode, studentScheduleGroup, grid, periodRowMap, termOids,
                    sectionsIncludedByHomeroomPlatoon, subGroupIndicatorByCourseOid);
            updatePeriodRowDisplay(grid, lastStudent, periodRowMap);
        }

        return grid;
    }

    private StudentScheduleGroup getStudentGroupForSection(String homeroom,
                                                           String platoonCode,
                                                           Schedule schedule) {
        X2Criteria groupCriteria = new X2Criteria();
        groupCriteria.addEqualTo(StudentScheduleGroup.COL_DISTRICT_CONTEXT_OID, schedule.getDistrictContextOid());
        groupCriteria.addEqualTo(StudentScheduleGroup.COL_SCHOOL_OID, schedule.getSchoolOid());

        if (StringUtils.isEmpty(platoonCode)) {
            groupCriteria.addEqualTo(StudentScheduleGroup.COL_ID, homeroom);
        } else {
            groupCriteria.addEqualTo(
                    StudentScheduleGroup.REL_PARENT_STUDENT_SCHEDULE_GROUP + "." + StudentScheduleGroup.COL_ID,
                    homeroom);
            groupCriteria.addEqualTo(StudentScheduleGroup.COL_ID, platoonCode);
        }

        QueryByCriteria groupQuery = new QueryByCriteria(StudentScheduleGroup.class, groupCriteria);
        return (StudentScheduleGroup) getBroker().getBeanByQuery(groupQuery);
    }


    private void addAdditionalSection(String homeroom,
                                      String platoonCode,
                                      String groupCode,
                                      StudentScheduleGroup studentScheduleGroup,
                                      DataGrid grid,
                                      Map<String, Integer> periodRowMap,
                                      Collection<String> termOidsInRange,
                                      Map<String, Collection<Section>> sectionsIncludedByHomeroomPlatoon,
                                      Map<String, Boolean> subGroupIndicatorByCourseOid) {

        Collection<Section> sectionsForHomeroom = m_sectionsBySectionTypeMap.get(homeroom);
        Collection<Section> sectionsIncluded = sectionsIncludedByHomeroomPlatoon.get(groupCode);

        if (sectionsForHomeroom != null && studentScheduleGroup != null) {

            for (Section sectionToAdd : sectionsForHomeroom) {
                if (sectionsIncluded != null && !sectionsIncluded.contains(sectionToAdd)) {

                    boolean add = true;
                    if (StringUtils.isEmpty(sectionToAdd.getPlatoonCode())) {
                        boolean inCludeSubGroup =
                                ElementaryScheduleManager.isIncludeInSubGroup(subGroupIndicatorByCourseOid,
                                        studentScheduleGroup,
                                        sectionToAdd.getSchoolCourseOid());
                        // logToolMessage(Level.INFO, "Homeroom: " + homeroom + " " +
                        // "platoon code: " + platoonCode, false);
                        // logToolMessage(Level.INFO, "Group: " + studentScheduleGroup.getId() + " "
                        // +
                        // "Section: " + sectionToAdd.getDescription() + " " + (inCludeSubGroup ?
                        // "add" : "no"),
                        // false);
                        if (inCludeSubGroup
                                || sectionToAdd.getSchoolCourse().getMasterType()
                                        .equals(SchoolCourse.MASTER_TYPE_RECESS)) {
                            add = true;
                        } else {
                            add = false;
                        }
                    } else {
                        add = sectionToAdd.getPlatoonCode().equals(platoonCode.trim());
                    }

                    if (add) {

                        sectionsIncluded.add(sectionToAdd);
                        sectionsIncludedByHomeroomPlatoon.put(groupCode, sectionsIncluded);

                        populateMatrix(grid, sectionToAdd, null, periodRowMap, termOidsInRange);
                    }
                }
            }
        }
    }

    /**
     * Updates the period row status and period ID
     *
     * @param grid
     * @param student
     * @param periodRowMap
     */
    private void updatePeriodRowDisplay(ReportDataGrid grid, SisStudent student, Map<String, Integer> periodRowMap) {
        Collection<String> periodIdsToDelete = new ArrayList<String>();
        Collection<String> periodIdsToUpdate = new ArrayList<String>();

        /*
         * Update the grid for the previous student in the elementary mode
         */
        if (m_reportHelper.getSchedule().getScheduleMode() == ScheduleMode.Elementary.ordinal() ||
                m_reportHelper.getSchedule().getScheduleMode() == ScheduleMode.K8.ordinal()) {
            Collection<String> previousCellDetails = new ArrayList<String>();
            String previousPeriodNumber = "";

            int periods = m_reportHelper.getSchedule().getElementaryPeriods();
            for (int i = 1; i <= periods; i++) {
                Integer rowNumber = periodRowMap == null ? null : periodRowMap.get(String.valueOf(i));
                if (rowNumber != null) {
                    grid.gotoRow(rowNumber.intValue());

                    Collection<String> currentCellDetails = new HashSet<String>();
                    for (int day = 1; day <= m_reportHelper.getSchedule().getElementaryDays(); day++) {
                        if (grid.get(String.valueOf(day)) != null) {
                            currentCellDetails.add((String) grid.get(String.valueOf(day)) + "@" + day);
                        } else {
                            currentCellDetails.add("xxx"); // for empty cell - add non-empty note.
                        }
                    }

                    if (previousCellDetails.size() == currentCellDetails.size()) {
                        previousCellDetails.removeAll(currentCellDetails);
                        if (previousCellDetails.isEmpty() && i != 1) {
                            /*
                             * First, combine the period row that contains the same value as
                             * previous row
                             * Second, Remove the period row that is the same as previous row
                             */
                            String newPeriodNumber = previousPeriodNumber + "-" + String.valueOf(i);
                            periodIdsToDelete.add(String.valueOf(i));

                            previousPeriodNumber = newPeriodNumber;
                        } else {
                            // Reset the previous period number
                            if (!StringUtils.isEmpty(previousPeriodNumber)) {
                                periodIdsToUpdate.add(previousPeriodNumber);
                            }
                            previousPeriodNumber = String.valueOf(i);
                        }

                    } else {
                        if (!StringUtils.isEmpty(previousPeriodNumber)) {
                            periodIdsToUpdate.add(previousPeriodNumber);
                        }
                        previousPeriodNumber = String.valueOf(i);
                    }
                    previousCellDetails.clear();
                    previousCellDetails.addAll(currentCellDetails);
                }
            }

            // Add the last set of period number to be updated
            periodIdsToUpdate.add(previousPeriodNumber);
        }

        /*
         * Update the period ID for the rows that should be combined
         */
        for (String periodIdToUpdate : periodIdsToUpdate) {
            int indexOfSeparator = periodIdToUpdate.indexOf("-");
            if (periodIdToUpdate.indexOf("-") != -1) {
                String periodId = periodIdToUpdate.substring(0, indexOfSeparator);
                Integer rowNumber = periodRowMap.get(periodId);

                if (rowNumber != null) {
                    grid.gotoRow(rowNumber.intValue());
                    grid.set(FIELD_PERIOD_NUMBER, periodIdToUpdate);
                }
            }
        }

        /*
         * Delete all the rows that mark to delete in the end.
         */
        for (String periodIdToDelete : periodIdsToDelete) {
            grid.beforeTop();

            for (int row = 0; row < grid.rowCount(); row++) {
                grid.gotoRow(row);

                if (periodIdToDelete.equals(grid.get(FIELD_PERIOD_NUMBER)) &&
                        student.equals(grid.get(FIELD_STUDENT))) {
                    grid.deleteRow(row);
                    break;
                }
            }
        }
    }


    /**
     * Prepares the passed DataGrid to be populated with the passed student's schedule. A row is
     * appended for each period in the schedule, and the student and period number fields are set.
     * <p>
     * A map is returned that resolves a period number to a row number in the grid. This map can be
     * used to look up a row number when populating the matrix cells with section data.
     *
     * @param grid DataGrid
     * @param student SisStudent
     * @param homeroom String
     * @param platoonCode String
     * @return Map
     */
    private Map initializeGridForStudent(DataGrid grid, SisStudent student, String homeroom, String platoonCode) {
        HashMap periodRowMap = new HashMap(15);

        Collection<SchedulePeriod> periods = m_reportHelper.getSchedule().getSchedulePeriods(true);
        String noon = "12:00:00 PM";
        PlainTime noonTime = TimeUtils.getTime(noon);
        for (SchedulePeriod period : periods) {
            grid.append();
            grid.set(FIELD_PERIOD_NUMBER, String.valueOf(period.getNumber()));
            grid.set(FIELD_STUDENT, student);
            grid.set(FIELD_HOMEROOM, homeroom);
            grid.set(FIELD_PLATOON, platoonCode);

            periodRowMap.put(String.valueOf(period.getNumber()), Integer.valueOf(grid.currentRowNumber()));

            /*
             * For the period that does not meet on the day, set it as dismissal/planning
             */
            for (ScheduleDay day : m_reportHelper.getSchedule().getScheduleDays(true)) {
                if (period.getStartTime().after(day.getEndTime()) || period.getStartTime().equals(day.getEndTime()) ||
                        period.getEndTime().before(day.getStartTime())
                        || period.getEndTime().equals(day.getStartTime())) {

                    grid.set(String.valueOf(day.getNumber()),
                            period.getEndTime().before(noonTime) ? "Late Start" : "Early Dismissal");

                    String cellDepartmentKey = String.valueOf(day.getNumber()) + "D";
                    grid.set(cellDepartmentKey, "");

                }
            }
        }

        return periodRowMap;
    }

    /**
     * Populates the passed DataGrid with student schedule matrix data for the passed section and
     * student schedule.
     *
     * @param grid DataGrid
     * @param section can be either a MasterSchedule or BuildMasterSchedule object
     * @param studentSchedule can be either a StudentSchedule or BuildStudentSchedule object
     * @param periodRowMap Map
     * @param termOidsInRange Collection<String>
     */
    private void populateMatrix(DataGrid grid,
                                Section section,
                                StudentSection studentSchedule,
                                Map<String, Integer> periodRowMap,
                                Collection<String> termOidsInRange) {
        String sectionDescription = !StringUtils.isEmpty(section.getSchoolCourse().getShortDescription())
                ? section.getSchoolCourse().getShortDescription()
                : section.getDescription();
        String sectionStaffView = section.getStaffView();
        String embeddedClass = section.getClass().equals(BuildMasterSchedule.class)
                ? ((BuildMasterSchedule) section).getFieldB002()
                : ((MasterSchedule) section).getFieldB002();

        Map scheduledMap =
                m_scheduleManager.getScheduledMap(studentSchedule, section, m_reportHelper.getSectionClass());
        Iterator termIterator = scheduledMap.keySet().iterator();
        while (termIterator.hasNext()) {
            ScheduleTerm term = (ScheduleTerm) termIterator.next();

            Iterator dayPeriodIterator = ((Collection) scheduledMap.get(term)).iterator();
            while (dayPeriodIterator.hasNext()) {
                KeyValuePair dayPeriod = (KeyValuePair) dayPeriodIterator.next();
                ScheduleDay day = (ScheduleDay) dayPeriod.getKey();
                SchedulePeriod period = (SchedulePeriod) dayPeriod.getValue();

                StringBuilder matrixDetail = new StringBuilder(25);

                if (sectionDescription != null) {
                    matrixDetail.append(sectionDescription);
                }
                if (!StringUtils.isEmpty(embeddedClass)) {
                    matrixDetail.append("\r\n");
                    matrixDetail.append(embeddedClass);
                }
                if (!StringUtils.isEmpty(sectionStaffView)) {
                    matrixDetail.append("\n");
                    matrixDetail.append(sectionStaffView);
                }
                Integer rowNumber = periodRowMap.get(String.valueOf(period.getNumber()));
                if (rowNumber != null) {
                    grid.gotoRow(rowNumber.intValue());

                    // Populate the cell
                    String cellKey = Integer.toString(day.getNumber());

                    String currentContents = (String) grid.get(cellKey);
                    if (currentContents != null) {
                        matrixDetail.append("\r\n");
                        matrixDetail.append(currentContents);
                    }

                    grid.set(cellKey, matrixDetail.toString());
                    String cellDepartmentKey = cellKey + "D";
                    grid.set(cellDepartmentKey, section.getSchoolCourse().getDepartmentCode());
                }
            }
        }
    }

    /**
     * Updates the period ID look-up map
     *
     * @param grid
     *
     * @return HashMap
     */
    private HashMap updatePeriodIdMap(ReportDataGrid grid) {
        HashMap periodIdLookup = new HashMap(15);
        Collection<SchedulePeriod> periods = m_reportHelper.getSchedule().getSchedulePeriods(getBroker(), true);
        for (SchedulePeriod period : periods) {
            if (period.getElementaryIndicator()) {
                periodIdLookup.put(String.valueOf(period.getNumber()), period.getTimeLabel());
            } else {
                periodIdLookup.put(String.valueOf(period.getNumber()), period.getId());
            }
        }

        Converter timeConverter = ConverterFactory.getConverterForClass(Converter.TIME_CONVERTER);
        grid.beforeTop();

        for (int row = 0; row < grid.rowCount(); row++) {
            grid.gotoRow(row);

            String periodNumber = (String) grid.get(FIELD_PERIOD_NUMBER);

            if (periodNumber.indexOf("-") != -1 &&
                    !periodIdLookup.containsKey(periodNumber)) {

                int firstIndexOfSeparator = periodNumber.indexOf("-");
                String startPeriod = periodNumber.substring(0, firstIndexOfSeparator);

                int lastIndexOfSeparator = periodNumber.lastIndexOf("-");
                String endPeriod = periodNumber.substring(lastIndexOfSeparator + 1);

                String periodLable = "";
                for (SchedulePeriod period : periods) {
                    if (period.getNumber() == Integer.valueOf(startPeriod).intValue()) {
                        periodLable = timeConverter.javaToString(period.getStartTime());
                        break;
                    }
                }

                for (SchedulePeriod period : periods) {
                    if (period.getNumber() == Integer.valueOf(endPeriod).intValue()) {
                        periodLable += ScheduleStructureManager.SCHEDULE_DISPLAY_CONNECT_SYMBOL
                                + timeConverter.javaToString(period.getEndTime());
                        break;
                    }
                }

                periodIdLookup.put(periodNumber, periodLable);
            }
        }
        return periodIdLookup;
    }
}
