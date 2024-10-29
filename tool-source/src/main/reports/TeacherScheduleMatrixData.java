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

import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sched.utils.ScheduleMap;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.TreeSet;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Teacher Schedule Matrix" report.
 *
 * @author X2 Development Corporation
 */
public class TeacherScheduleMatrixData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    // Grid field name constants
    private static final String FIELD_PERIOD_NUMBER = "period";
    private static final String FIELD_STAFF = "staff";

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
    private static final String TERM_BRACKET_CLOSE = "] ";
    private static final String TERM_BRACKET_OPEN = "[";

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
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(10, 15);

        if (m_reportHelper.getSchedule() != null) {
            m_scheduleManager = new ScheduleManager(getBroker());

            Criteria criteria = new Criteria();
            criteria.addEqualTo(TeacherSection.REL_SECTION + "." + Section.COL_SCHEDULE_OID,
                    m_reportHelper.getScheduleOid());

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

                addUserCriteria(criteria, queryBy, queryString, SisStaff.class, TeacherSection.COL_STAFF_OID);

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
                HashMap periodIdLookup = new HashMap(15);
                Iterator periods = m_reportHelper.getSchedule().getSchedulePeriods(getBroker()).iterator();
                while (periods.hasNext()) {
                    SchedulePeriod period = (SchedulePeriod) periods.next();
                    periodIdLookup.put(Integer.valueOf(period.getNumber()), period.getId());
                }
                addParameter(PARAMETER_PERIOD_ID_LOOKUP, periodIdLookup);

                HashMap dayIdLookup = new HashMap(15);
                Iterator days = m_reportHelper.getSchedule().getScheduleDays(getBroker()).iterator();
                while (days.hasNext()) {
                    ScheduleDay day = (ScheduleDay) days.next();
                    dayIdLookup.put(Integer.valueOf(day.getNumber()), day.getId());
                }
                addParameter(PARAMETER_DAY_ID_LOOKUP, dayIdLookup);
            }

            grid.beforeTop();
        }

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
     * Builds a ReportDataGrid object representing one ore more teacher schedule matrices. The
     * matrices are generated from the passed Iterator over ScheduleTeacher beans which must be
     * in staff order.
     * <p>
     * The returned ReportDataGrid contains a row per staff, per period. A column per day exists
     * as well as a column for the staff bean.
     *
     * @param scheduleIterator QueryIterator
     * @return ReportDataGrid
     */
    private ReportDataGrid buildMatrices(QueryIterator scheduleIterator) {
        ReportDataGrid grid = new ReportDataGrid(m_initialRowSize, INITIAL_COLUMN_SIZE);

        if (m_reportHelper.getSchedule() != null) {
            SisStaff lastStaff = null;
            Map periodRowMap = null;
            while (scheduleIterator.hasNext()) {
                TeacherSection scheduleTeacher = (TeacherSection) scheduleIterator.next();
                Section section = scheduleTeacher.getSection();
                SisStaff staff = scheduleTeacher.getStaff();

                if (lastStaff == null || !lastStaff.equals(staff)) {
                    periodRowMap = initializeGridForStaff(grid, staff);
                }
                lastStaff = staff;

                if (section != null) {
                    populateMatrix(grid, section, periodRowMap, scheduleTeacher);
                }
            }
        }

        return grid;
    }

    /**
     * Prepares the passed DataGrid to be populated with the passed teacher's schedule. A row is
     * appended for each period in the schedule, and the staff and period number fields are set.
     * <p>
     * A map is returned that resolves a period number to a row number in the grid. This map can be
     * used to look up a row number when populating the matrix cells with section data.
     *
     * @param grid DataGrid
     * @param staff SisStaff
     * @return Map
     */
    private Map initializeGridForStaff(DataGrid grid, SisStaff staff) {
        HashMap periodRowMap = new HashMap(15);

        /*
         * Determine the number of periods in the schedule and initialize an adequate number
         * rows for the matrix
         */
        int periods = m_reportHelper.getSchedule().getPeriods();
        for (int i = 1; i <= periods; i++) {
            grid.append();
            grid.set(FIELD_PERIOD_NUMBER, Integer.valueOf(i));
            grid.set(FIELD_STAFF, staff);

            periodRowMap.put(Integer.valueOf(i), Integer.valueOf(grid.currentRowNumber()));
        }

        return periodRowMap;
    }

    /**
     * Populates the passed DataGrid with teacher schedule matrix data for the passed section.
     *
     * @param grid DataGrid
     * @param section can be either a MasterSchedule or BuildMasterSchedule object
     * @param periodRowMap Map
     * @param teacherSection TeacherSection
     */
    private void populateMatrix(DataGrid grid, Section section, Map periodRowMap, TeacherSection teacherSection) {
        String sectionCourseView = section.getCourseView();
        String sectionDescription = section.getDescription();
        String sectionRoomView = section.getRoomView();
        String sectionTeamCode = section.getTeamCode();


        if (teacherSection != null &&
                section != null &&
                section.getSchedule() != null) {
            ScheduleMap teacherMap = teacherSection.getScheduleMap(getBroker());
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
                            matrixDetail.append(TERM_BRACKET_OPEN);
                            matrixDetail.append(teacherSection.getScheduleTerm().getCode());
                            matrixDetail.append(TERM_BRACKET_CLOSE);

                            if (sectionCourseView != null) {
                                matrixDetail.append(sectionCourseView);
                                matrixDetail.append(" ");
                            }
                            if (sectionDescription != null) {
                                matrixDetail.append(sectionDescription);
                            }

                            if (sectionRoomView != null) {
                                if (section.getSplitRoomIndicator()) {
                                    /*
                                     * If there are more than one room for the section, retrieve the
                                     * right room for the cell.
                                     */
                                    SchoolRoom room = m_scheduleManager.getRoomScheduled(section, term, day, period);
                                    if (room != null) {
                                        sectionRoomView = room.getRoomNumber();
                                    }
                                }
                                matrixDetail.append(" - ");
                                matrixDetail.append(sectionRoomView);
                            }

                            if (sectionTeamCode != null) {
                                matrixDetail.append("\n");
                                matrixDetail.append(sectionTeamCode);
                            }
                            matrixDetail.append("\r\n");
                        }

                        Integer rowNumber = (Integer) periodRowMap.get(Integer.valueOf(period.getNumber()));
                        if (rowNumber != null) {
                            grid.gotoRow(rowNumber.intValue());

                            // Populate the cell
                            String cellKey = Integer.toString(day.getNumber());

                            String currentContents = (String) grid.get(cellKey);
                            if (currentContents != null) {
                                /*
                                 * There are multiple courses in the cell. Use a TreeSet to maintain
                                 * a
                                 * term-code order within the cell.
                                 */
                                TreeSet sortSet = new TreeSet();
                                StringTokenizer tokenizer =
                                        new StringTokenizer(currentContents, TERM_BRACKET_OPEN);
                                while (tokenizer.hasMoreTokens()) {
                                    sortSet.add(TERM_BRACKET_OPEN + tokenizer.nextToken());
                                }
                                sortSet.add(matrixDetail.toString());

                                matrixDetail = new StringBuilder(75);
                                Iterator sortSetIterator = sortSet.iterator();
                                while (sortSetIterator.hasNext()) {
                                    matrixDetail.append(sortSetIterator.next());
                                }
                            }

                            grid.set(cellKey, matrixDetail.toString());
                        }
                    }
                }
            }
        }
    }

    /**
     * Returns the scheduled time slot map for the passed section.
     *
     * @param section Section
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

                        Iterator dayIterator = m_reportHelper.getSchedule().getScheduleDays().iterator();
                        while (dayIterator.hasNext()) {
                            ScheduleDay day = (ScheduleDay) dayIterator.next();

                            if (scheduleMap.isScheduledForDay(day.getNumber())) {
                                Iterator periodIterator = m_reportHelper.getSchedule().getSchedulePeriods().iterator();
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
