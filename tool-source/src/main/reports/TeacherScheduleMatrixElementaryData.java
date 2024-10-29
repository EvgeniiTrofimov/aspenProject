/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2004 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sched.utils.ScheduleMap;
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
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Prepares the data for the "Elementary Teacher Schedule Matrix" report.
 *
 * @author X2 Development Corporation
 */
public class TeacherScheduleMatrixElementaryData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    // Grid field name constants
    private static final String FIELD_PERIOD_NUMBER = "period";
    private static final String FIELD_STAFF = "staff";
    private static final String FIELD_GRADE_LEVEL = "gradeLevel";
    private static final String FIELD_HOMEROOM = "homeroom";
    private static final String FIELD_PROGRAM = "program";

    // The initial column size
    private static final int INITIAL_COLUMN_SIZE = 15;

    // The initial row size if the report is run for multiple students
    private static final int INITIAL_ROW_SIZE_MULTIPLE = 1000;

    // The initial row size if the report is run for a single student
    private static final int INITIAL_ROW_SIZE_SINGLE = 15;

    // Report parameter name constants
    private static final String PARAMETER_DAY_ID_LOOKUP = "dayIdLookup";
    private static final String PARAMETER_PERIOD_ID_LOOKUP = "periodIdLookup";

    // Term display brackets
    // private static final String TERM_BRACKET_OPEN = "[";

    /**
     * Name for the "selection" report parameter. The value is an String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "teacher sort" report parameter. The value is an String.
     */
    public static final String TEACHER_SORT_PARAM = "teacherSort";

    private int m_initialRowSize;
    private ScheduleReportHelper m_reportHelper;
    private ScheduleManager m_scheduleManager;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(10, 15);

        if (m_reportHelper.getSchedule() != null) {
            m_scheduleManager = new ScheduleManager(getBroker());

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(TeacherSection.REL_SECTION + "." + Section.COL_SCHEDULE_OID,
                    m_reportHelper.getScheduleOid());
            if (m_reportHelper.getSchedule().getScheduleMode() == Schedule.ScheduleMode.K8.ordinal()) {
                criteria.addEqualTo(TeacherSection.REL_SECTION + "." + Section.COL_ELEMENTARY_INDICATOR,
                        Boolean.TRUE);

            }

            if (!StringUtils.isEmpty(m_reportHelper.getStaffOid())) {
                /*
                 * Add the teacher criteria.
                 */
                criteria.addEqualTo(TeacherSection.COL_STAFF_OID, m_reportHelper.getStaffOid());

                m_initialRowSize = INITIAL_ROW_SIZE_SINGLE;
                /*
                 * Set the school associated with the staff
                 */
                addParameter(SCHOOLNAME_KEY, getSchool().getName());
                addParameter(SCHOOL_KEY, getSchool());
            } else {
                String queryBy = (String) getParameter(QUERY_BY_PARAM);
                String queryString = (String) getParameter(QUERY_STRING_PARAM);

                if (queryBy.equals("localId")) {
                    criteria.addIn(TeacherSection.REL_STAFF + "." + Staff.COL_LOCAL_ID,
                            StringUtils.convertDelimitedStringToList(queryString, ",", true));
                } else {
                    addUserCriteria(criteria, queryBy, queryString, SisStaff.class, TeacherSection.COL_STAFF_OID);
                }

                m_initialRowSize = INITIAL_ROW_SIZE_MULTIPLE;
            }

            QueryByCriteria query = new QueryByCriteria(m_reportHelper.getTeacherSectionClass(), criteria);

            /*
             * Build the sort based on user input.
             */
            String sortBy = (String) getParameter(TEACHER_SORT_PARAM);
            applyUserSort(query, sortBy);

            /*
             * The teacher sort order always has to end with the staff OID so that schedules are
             * grouped properly.
             */
            query.addOrderByAscending(TeacherSection.COL_STAFF_OID);

            /*
             * Execute the query and build the matrices
             */
            QueryIterator scheduleIterator = null;
            try {
                scheduleIterator = getBroker().getIteratorByQuery(query);
                grid = buildMatrices(scheduleIterator);
            } finally {
                if (scheduleIterator != null) {
                    scheduleIterator.close();
                }
            }

            /*
             * Set the report parameters
             */
            if (m_reportHelper.getSchedule() != null) {
                HashMap periodIdLookup = updatePeriodIdMap(grid);
                addParameter(PARAMETER_PERIOD_ID_LOOKUP, periodIdLookup);

                HashMap dayIdLookup = new HashMap(15);
                Iterator days = m_reportHelper.getSchedule().getScheduleDays(getBroker(), true).iterator();
                while (days.hasNext()) {
                    ScheduleDay day = (ScheduleDay) days.next();
                    dayIdLookup.put(Integer.valueOf(day.getNumber()), day.getName());
                }
                addParameter(PARAMETER_DAY_ID_LOOKUP, dayIdLookup);
            }

            grid.beforeTop();
        }

        return grid;
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

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_reportHelper = new ScheduleReportHelper(userData);
    }

    /**
     * Builds a ReportDataGrid object representing one ore more teacher schedule matrices. The
     * matrices are generated from the passed Iterator over ScheduleTeacher beans which must be
     * in staff order.
     * <p>
     * The returned ReportDataGrid contains a row per staff, per period. A column per day exists
     * as well as a column for the staff bean.
     *
     * @param scheduleIterator
     *
     * @return ReportDataGrid
     */
    private ReportDataGrid buildMatrices(QueryIterator scheduleIterator) {
        ReportDataGrid grid = new ReportDataGrid(m_initialRowSize, INITIAL_COLUMN_SIZE);

        if (m_reportHelper.getSchedule() != null) {
            SisStaff lastStaff = null;
            Map periodRowMap = null;
            String teacherHomeroom = "";
            while (scheduleIterator.hasNext()) {
                TeacherSection scheduleTeacher = (TeacherSection) scheduleIterator.next();
                Section section = scheduleTeacher.getSection();
                SisStaff staff = scheduleTeacher.getStaff();

                if (lastStaff == null || !lastStaff.equals(staff)) {
                    if (lastStaff != null) {
                        updatePeriodRowDisplay(grid, lastStaff, periodRowMap);
                    }

                    /*
                     * Initialize the grid for the new staff
                     */
                    periodRowMap = initializeGridForStaff(grid, staff);
                    teacherHomeroom = (String) grid.get(FIELD_HOMEROOM);
                }
                lastStaff = staff;

                if (section != null) {
                    populateMatrix(grid, section, periodRowMap, scheduleTeacher, teacherHomeroom);
                }
            }

            /*
             * Updates the PeriodRow display status for the last teacher
             */
            updatePeriodRowDisplay(grid, lastStaff, periodRowMap);
        }

        return grid;
    }

    /**
     * Updates the period row status and period ID
     *
     * @param grid
     * @param teacher
     * @param periodRowMap
     */
    private void updatePeriodRowDisplay(ReportDataGrid grid, SisStaff teacher, Map<String, Integer> periodRowMap) {
        Collection<String> periodIdsToDelete = new HashSet<String>();
        Collection<String> periodIdsToUpdate = new HashSet<String>();

        /*
         * Update the grid for the previous staff in the elementary mode
         */
        if (m_reportHelper.getSchedule().getScheduleMode() == ScheduleMode.Elementary.ordinal() ||
                m_reportHelper.getSchedule().getScheduleMode() == Schedule.ScheduleMode.K8.ordinal()) {
            Collection<String> previousCellDetails = new ArrayList<String>();
            String previousPeriodNumber = "";

            int periods = m_reportHelper.getSchedule().getElementaryPeriods();
            for (int i = 1; i <= periods; i++) {
                Integer rowNumber = periodRowMap.get(String.valueOf(i));
                if (rowNumber != null) {
                    grid.gotoRow(rowNumber.intValue());

                    Collection<String> currentCellDetails = new HashSet<String>();
                    for (int day = 1; day <= m_reportHelper.getSchedule().getElementaryDays(); day++) {
                        if (grid.get(String.valueOf(day)) != null) {
                            currentCellDetails.add((String) grid.get(String.valueOf(day)) + "@" + day);
                        } else {
                            currentCellDetails.add("xxx");
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
                        teacher.equals(grid.get(FIELD_STAFF))) {
                    grid.deleteRow(row);
                    break;
                }
            }
        }
    }

    /**
     * Prepares the passed DataGrid to be populated with the passed teacher's schedule. A row is
     * appended for each period in the schedule, and the staff and period number fields are set.
     * <p>
     * A map is returned that resolves a period number to a row number in the grid. This map can be
     * used to look up a row number when populating the matrix cells with section data.
     *
     * @param grid
     * @param staff
     *
     * @return Map
     */
    private Map initializeGridForStaff(DataGrid grid, SisStaff staff) {
        HashMap periodRowMap = new HashMap(15);

        /*
         * Determine the number of periods in the schedule and initialize an adequate number
         * rows for the matrix
         */
        String noon = "12:00:00 PM";
        PlainTime noonTime = TimeUtils.getTime(noon);

        Object[] staffInfo = collectionStaffInfo(staff);
        Collection<SchedulePeriod> periods = m_reportHelper.getSchedule().getSchedulePeriods(true);
        for (SchedulePeriod period : periods) {
            grid.append();
            grid.set(FIELD_PERIOD_NUMBER, String.valueOf(period.getNumber()));
            grid.set(FIELD_STAFF, staff);
            grid.set(FIELD_GRADE_LEVEL, staffInfo[1]);
            grid.set(FIELD_PROGRAM, staffInfo[2]);
            grid.set(FIELD_HOMEROOM, staffInfo[3]);

            periodRowMap.put(String.valueOf(period.getNumber()), Integer.valueOf(grid.currentRowNumber()));

            /*
             * For the period that does not meet on the day, set it as dismissal/planning
             */
            for (ScheduleDay day : m_reportHelper.getSchedule().getScheduleDays(true)) {
                if (period.getStartTime().after(day.getEndTime()) || period.getStartTime().equals(day.getEndTime()) ||
                        period.getEndTime().before(day.getStartTime())
                        || period.getEndTime().equals(day.getStartTime())) {

                    grid.set(String.valueOf(day.getNumber()),
                            period.getEndTime().before(noonTime) ? "Late Start/Planning" : "Dismissal/Planning");

                    String cellDepartmentKey = String.valueOf(day.getNumber()) + "D";
                    grid.set(cellDepartmentKey, "");

                }
            }
        }

        /*
         * Populate the matrix with the lunch section directly
         */
        if (staffInfo[0] != null) {
            populateMatrix(grid, (Section) staffInfo[0], periodRowMap, null, "");
        }

        return periodRowMap;
    }

    /**
     * Sets the grade level and program
     *
     * @param staff
     */
    private Object[] collectionStaffInfo(SisStaff staff) {
        /*
         * Identify the sections with max# section type and platoon code
         */
        X2Criteria sectionCriteria = new X2Criteria();
        sectionCriteria.addEqualTo(Section.COL_SCHEDULE_OID, m_reportHelper.getSchedule().getOid());
        sectionCriteria.addEqualTo(Section.COL_PRIMARY_STAFF_OID, staff.getOid());
        sectionCriteria.addContains(Section.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_DESCRIPTION,
                "Elementary Homeroom");

        String[] attributes = new String[] {Section.COL_SECTION_TYPE, Section.COL_PLATOON_CODE, "count(*)"};

        ReportQueryByCriteria sectionQuery = new ReportQueryByCriteria(m_reportHelper.getSectionClass(),
                attributes, sectionCriteria);

        sectionQuery.addGroupBy(Section.COL_SECTION_TYPE);
        sectionQuery.addGroupBy(Section.COL_PLATOON_CODE);
        sectionQuery.addOrderByDescending("count(*)");

        String sectionType = "";
        String platoonCode = "";
        ReportQueryIterator results = getBroker().getReportQueryIteratorByQuery(sectionQuery);
        try {
            while (results.hasNext()) {
                Object[] row = (Object[]) results.next();

                sectionType = (String) row[0];
                platoonCode = (String) row[1];
            }
        } finally {
            results.close();
        }

        /*
         * Identify the student schedule group for the staff
         */
        X2Criteria groupCriteria = new X2Criteria();
        groupCriteria.addEqualTo(StudentScheduleGroup.COL_DISTRICT_CONTEXT_OID,
                m_reportHelper.getSchedule().getDistrictContextOid());
        groupCriteria.addEqualTo(StudentScheduleGroup.COL_SCHOOL_OID, m_reportHelper.getSchedule().getSchoolOid());

        if (StringUtils.isEmpty(platoonCode)) {
            groupCriteria.addEqualTo(StudentScheduleGroup.COL_ID, sectionType);
        } else {
            groupCriteria.addEqualTo(
                    StudentScheduleGroup.REL_PARENT_STUDENT_SCHEDULE_GROUP + "." + StudentScheduleGroup.COL_ID,
                    sectionType);
            groupCriteria.addEqualTo(StudentScheduleGroup.COL_ID, platoonCode);
        }

        QueryByCriteria groupQuery = new QueryByCriteria(StudentScheduleGroup.class, groupCriteria);
        StudentScheduleGroup group = (StudentScheduleGroup) getBroker().getBeanByQuery(groupQuery);

        Section lunchSection = null;
        if (group != null) {
            X2Criteria studentScheduleCriteria = new X2Criteria();
            studentScheduleCriteria.addEqualTo(StudentSection.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());
            studentScheduleCriteria.addEqualTo(
                    StudentSection.REL_SECTION + "." + Section.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_MASTER_TYPE,
                    SchoolCourse.MASTER_TYPE_LUNCH);

            Collection<String> studentsInGroup = ElementaryScheduleManager
                    .getGroupMembersForGroup(m_reportHelper.getSchedule(), group, true, getBroker());
            studentScheduleCriteria.addIn(StudentSection.COL_STUDENT_OID, studentsInGroup);

            QueryByCriteria studentScheduleQuery =
                    new QueryByCriteria(m_reportHelper.getStudentSectionClass(), studentScheduleCriteria);
            StudentSection studentSection = (StudentSection) getBroker().getBeanByQuery(studentScheduleQuery);
            if (studentSection != null) {
                lunchSection = studentSection.getSection();
            }
        }

        return new Object[] {lunchSection,
                group != null && group.getProgram() != null ? group.getProgram().getGradeLevel() : "",
                group != null && group.getProgram() != null ? group.getProgram().getDescription() : "",
                group != null && group.getParentStudentScheduleGroup() != null
                        ? group.getParentStudentScheduleGroup().getId() + " / " + group.getId()
                        : (group != null ? group.getId() : "")};
    }

    /**
     * Populates the passed DataGrid with teacher schedule matrix data for the passed section.
     *
     * @param grid
     * @param section can be either a MasterSchedule or BuildMasterSchedule object
     * @param periodRowMap
     * @param teacherSection
     * @param teacherHomeroom
     */
    private void populateMatrix(DataGrid grid,
                                Section section,
                                Map<String, Integer> periodRowMap,
                                TeacherSection teacherSection,
                                String teacherHomeroom) {
        // String sectionCourseView = section.getCourseView();
        String sectionDescription = !StringUtils.isEmpty(section.getSchoolCourse().getShortDescription())
                ? section.getSchoolCourse().getShortDescription() : section.getDescription();

        String embeddedClass = section.getClass().equals(BuildMasterSchedule.class)
                ? ((BuildMasterSchedule) section).getFieldB002() : ((MasterSchedule) section).getFieldB002();

        String sectionType = section.getClass().equals(BuildMasterSchedule.class)
                ? ((BuildMasterSchedule) section).getSectionType() : ((MasterSchedule) section).getSectionType();

        if (section != null &&
                section.getSchedule() != null) {
            ScheduleMap teacherMap = teacherSection == null ? section.getScheduleMap(getBroker())
                    : teacherSection.getSection().getScheduleMap(getBroker());
            if (teacherMap != null) {
                ScheduleMap scheduleMapForTeacher = teacherMap.expand(section.getSchedule().getTerms());
                HashMap scheduledMap = getScheduledMap(section);
                Iterator termIterator = scheduledMap.keySet().iterator();
                while (termIterator.hasNext()) {
                    ScheduleTerm term = (ScheduleTerm) termIterator.next();

                    Iterator dayPeriodIterator = ((Collection) scheduledMap.get(term)).iterator();
                    while (dayPeriodIterator.hasNext()) {
                        KeyValuePair dayPeriod = (KeyValuePair) dayPeriodIterator.next();
                        ScheduleDay day = (ScheduleDay) dayPeriod.getKey();
                        SchedulePeriod period = (SchedulePeriod) dayPeriod.getValue();

                        StringBuilder matrixDetail = new StringBuilder(25);

                        if (scheduleMapForTeacher != null && !scheduleMapForTeacher.isEmpty()) {
                            if (sectionDescription != null) {
                                matrixDetail.append(sectionDescription);
                            }
                            if (!StringUtils.isEmpty(embeddedClass)) {
                                matrixDetail.append("\r\n");
                                matrixDetail.append(embeddedClass);
                            }
                            if (!StringUtils.isEmpty(sectionType) &&
                            // !StringUtils.isEqual(sectionType, teacherHomeroom) &&
                                    !section.getSchoolCourse().getMasterType().equals(SchoolCourse.MASTER_TYPE_LUNCH)) {
                                matrixDetail.append("\r\n");
                                matrixDetail.append(sectionType);
                            }
                            matrixDetail.append("\r\n");
                        }

                        Integer rowNumber = periodRowMap.get(String.valueOf(period.getNumber()));
                        if (rowNumber != null) {
                            grid.gotoRow(rowNumber.intValue());

                            // Populate the cell
                            String cellKey = Integer.toString(day.getNumber());

                            String currentContents = (String) grid.get(cellKey);
                            if (currentContents != null && !matrixDetail.toString().contains(currentContents)) {
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
        }
    }

    /**
     * Returns the scheduled time slot map for the passed section.
     *
     * @param section
     *
     * @return HashMap key is ScheduleTerm object,
     *         Value is a list KeyValuePair object with ScheduleDay as Key, SchedulePeriod as value.
     */
    private HashMap getScheduledMap(Section section) {
        HashMap map = new HashMap();

        if (section.getClass().equals(MasterSchedule.class)) {
            MasterSchedule masterSection = (MasterSchedule) section;
            Collection masterTerms = masterSection.getMasterTerms();

            Iterator masterTermIterator = masterTerms.iterator();
            while (masterTermIterator.hasNext()) {
                MasterTerm masterTerm = (MasterTerm) masterTermIterator.next();
                ScheduleTerm term = masterTerm.getScheduleTerm();

                Iterator masterMatrixIterator = masterTerm.getMasterMatrices().iterator();
                while (masterMatrixIterator.hasNext()) {
                    MasterScheduleMatrix masterMatrix = (MasterScheduleMatrix) masterMatrixIterator.next();
                    ScheduleMatrix scheduleMatrix = masterMatrix.getScheduleMatrix();
                    KeyValuePair dayPeriodPair =
                            new KeyValuePair(scheduleMatrix.getScheduleDay(), scheduleMatrix.getSchedulePeriod());

                    Collection dayPeriods = (Collection) map.get(term);
                    if (dayPeriods == null) {
                        dayPeriods = new ArrayList();
                    }
                    dayPeriods.add(dayPeriodPair);
                    map.put(term, dayPeriods);
                }
            }
        } else if (section.getClass().equals(BuildMasterSchedule.class)) {
            BuildMasterSchedule buildSection = (BuildMasterSchedule) section;
            if (buildSection.getTermMap() != null && buildSection.getScheduleMatrix() != null) {
                ScheduleMap scheduleMap = new ScheduleMap(buildSection.getTermMap(), buildSection.getScheduleMatrix());
                if (map != null) {
                    Iterator termIterator =
                            m_scheduleManager.getContainedTermsForBuildMaster(buildSection).iterator();
                    while (termIterator.hasNext()) {
                        ScheduleTerm term = (ScheduleTerm) termIterator.next();

                        Iterator dayIterator = m_reportHelper.getSchedule().getScheduleDays(true).iterator();
                        while (dayIterator.hasNext()) {
                            ScheduleDay day = (ScheduleDay) dayIterator.next();

                            if (scheduleMap.isScheduledForDay(day.getNumber())) {
                                Iterator periodIterator =
                                        m_reportHelper.getSchedule().getSchedulePeriods(true).iterator();
                                while (periodIterator.hasNext()) {
                                    SchedulePeriod period = (SchedulePeriod) periodIterator.next();

                                    if (scheduleMap.isScheduled(term.getBaseTermMap(), day.getNumber(),
                                            period.getNumber())) {
                                        KeyValuePair dayPeriodPair = new KeyValuePair(day, period);

                                        Collection dayPeriods = (Collection) map.get(term);
                                        if (dayPeriods == null) {
                                            dayPeriods = new ArrayList();
                                        }
                                        dayPeriods.add(dayPeriodPair);
                                        map.put(term, dayPeriods);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        return map;
    }
}
