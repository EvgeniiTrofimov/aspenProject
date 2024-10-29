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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.schedule.ScheduleInterfaceManager;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.model.business.schedule.future.StudentScheduleChangeReportHelper;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Student Schedule Matrix" report.
 *
 * @author X2 Development Corporation
 */
public class StudentScheduleMatrixData extends ReportJavaSourceNet {
    /**
     * Name for the "effective date" report parameter. The value is a PlainDate.
     */
    public static final String EFFECTIVE_DATE_PARAM = "effectiveDate";

    /**
     * Name for the "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "student sort" report parameter. The value is a String.
     */
    public static final String STUDENT_SORT_PARAM = "studentSort";

    // Grid fields
    private static final String FIELD_PERIOD_NUMBER = "period";
    private static final String FIELD_STUDENT = "student";

    // Report parameters
    private static final String DAY_ID_LOOKUP_PARAM = "dayIdLookup";
    private static final String HOMEROOM_MAP_PARAM = "homeroomMap";
    private static final String PERIOD_ID_LOOKUP_PARAM = "periodIdLookup";

    // Term display brackets
    private static final String TERM_BRACKET_CLOSE = "] ";
    private static final String TERM_BRACKET_OPEN = "[";

    // The initial column size
    private static final int INITIAL_COLUMN_SIZE = 15;

    // The initial row size if the report is run for multiple students
    private static final int INITIAL_ROW_SIZE_MULTIPLE = 5000;

    // The initial row size if the report is run for a single student
    private static final int INITIAL_ROW_SIZE_SINGLE = 15;

    private static final long serialVersionUID = 1L;

    private int m_initialRowSize;
    private ScheduleReportHelper m_reportHelper;
    private ScheduleManager m_scheduleManager;
    private ScheduleInterfaceManager m_interfaceManager;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        m_scheduleManager = new ScheduleManager(getBroker());
        m_interfaceManager = new ScheduleInterfaceManager(getBroker());

        /*
         * The list of term oids
         */
        X2Criteria termCriteria = new X2Criteria();
        termCriteria.addEqualTo(ScheduleTerm.COL_SCHEDULE_OID, m_reportHelper.getTimeScheduleOid());

        SubQuery termQuery = new SubQuery(ScheduleTerm.class, X2BaseBean.COL_OID, termCriteria);
        Collection<String> termOids = getBroker().getSubQueryCollectionByQuery(termQuery);

        /*
         * Build the criteria based on user input
         */
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentSection.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());

        if (!StringUtils.isEmpty(m_reportHelper.getStudentOid())) {
            criteria.addEqualTo(StudentSection.COL_STUDENT_OID, m_reportHelper.getStudentOid());
            m_initialRowSize = INITIAL_ROW_SIZE_SINGLE;
        } else {
            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);

            addUserCriteria(criteria, queryBy, queryString, SisStudent.class, StudentSection.COL_STUDENT_OID);

            m_initialRowSize = INITIAL_ROW_SIZE_MULTIPLE;
        }

        QueryByCriteria query = new QueryByCriteria(m_reportHelper.getStudentSectionClass(), criteria);

        /*
         * Build the sort based on user input.
         */
        String sort = (String) getParameter(STUDENT_SORT_PARAM);
        applyUserSort(query, sort);

        /*
         * The student sort order always has to end with the student OID so that schedules are
         * grouped properly.
         */
        query.addOrderByAscending(StudentSection.COL_STUDENT_OID);
        query.addOrderByDescending(StudentSection.REL_SECTION + PATH_DELIMITER + Section.COL_TERM_VIEW);

        /*
         * Execute the query and build the matrices
         */
        ReportDataGrid grid = new ReportDataGrid(10);
        Iterator scheduleIterator = null;
        PlainDate effectiveDate = (PlainDate) getParameter(EFFECTIVE_DATE_PARAM);

        /*
         * If we have an effective date we need to merge pending StudentScheduleChange records that
         * would be effective as of the date
         * selected with StudentSchedule records.
         */
        if (effectiveDate != null && isSchoolContext()) {
            scheduleIterator = mergeStudentScheduleWithPendingStudentSchedules(effectiveDate, query, null).iterator();
        } else {
            scheduleIterator = getBroker().getIteratorByQuery(query);
        }

        try {
            grid = buildMatrices(scheduleIterator, termOids);
        } finally {
            if (scheduleIterator instanceof QueryIterator) {
                ((QueryIterator) scheduleIterator).close();
            }
        }

        /*
         * Set the report parameters
         */
        if (m_reportHelper.getSchedule() != null) {
            HashMap periodIdLookup = new HashMap(15);
            Iterator periods = m_reportHelper.getSchedule().getSchedulePeriods().iterator();
            while (periods.hasNext()) {
                SchedulePeriod period = (SchedulePeriod) periods.next();
                periodIdLookup.put(Integer.valueOf(period.getNumber()), period.getId());
            }
            addParameter(PERIOD_ID_LOOKUP_PARAM, periodIdLookup);

            HashMap dayIdLookup = new HashMap(15);
            Iterator days = m_reportHelper.getSchedule().getScheduleDays().iterator();
            while (days.hasNext()) {
                ScheduleDay day = (ScheduleDay) days.next();
                dayIdLookup.put(Integer.valueOf(day.getNumber()), day.getId());
            }
            addParameter(DAY_ID_LOOKUP_PARAM, dayIdLookup);
        }

        addParameter(HOMEROOM_MAP_PARAM,
                ReportUtils.buildHomeroomToStaffMap(getBroker(), getOrganization(), getSchool()));

        grid.beforeTop();
        return grid;
    }

    /**
     * Builds a Collection of StudentSchedule records merged with pending StudentScheduleChange
     * records as of the specified effectiveDate.
     *
     * @param effectiveDate PlainDate
     * @param query QueryByCriteria
     * @param scheduleSort String
     * @return Collection<StudentSchedule>
     */
    protected Collection<StudentSchedule> mergeStudentScheduleWithPendingStudentSchedules(PlainDate effectiveDate,
                                                                                          QueryByCriteria query,
                                                                                          String scheduleSort) {
        Schedule schedule = (Schedule) getBroker().getBeanByOid(Schedule.class, m_reportHelper.getScheduleOid());

        // Ensure effective date is now or later and within the bounds of the schedules start and
        // end dates.
        effectiveDate = StudentScheduleChangeReportHelper.validateEffectiveDate(effectiveDate, schedule, getTimeZone());

        // Calling this method in the SSCRP rather than the broker is important for maintaining the
        // order of the hashmap for displaying results in the report.
        Map<String, Collection<StudentSchedule>> studentSchedules =
                StudentScheduleChangeReportHelper.getGroupedCollectionByQuery(query,
                        new String[] {StudentSection.COL_STUDENT_OID},
                        new int[] {256},
                        getBroker());

        StudentScheduleChangeReportHelper sscHelper =
                new StudentScheduleChangeReportHelper((X2Criteria) query.getCriteria(),
                        effectiveDate,
                        m_reportHelper.getScheduleOid(),
                        null, // school oid
                        getBroker());

        Map<String, Map<StudentSchedule, StudentScheduleChange>> pendingStudentSchedules =
                sscHelper.getPendingStudentSchedules();

        return sscHelper.mergeStudentScheduleRecords(studentSchedules, pendingStudentSchedules,
                getUserSortFields(scheduleSort));
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
     * @return ReportDataGrid
     */
    private ReportDataGrid buildMatrices(Iterator scheduleIterator, Collection<String> termOids) {
        ReportDataGrid grid = new ReportDataGrid(m_initialRowSize, INITIAL_COLUMN_SIZE);

        if (m_reportHelper.getSchedule() != null) {
            SisStudent lastStudent = null;
            Map periodRowMap = null;
            while (scheduleIterator.hasNext()) {
                StudentSection studentSchedule = (StudentSection) scheduleIterator.next();
                Section section = studentSchedule.getSection();
                SisStudent student = studentSchedule.getStudent();

                if (lastStudent == null || !lastStudent.equals(student)) {
                    periodRowMap = initializeGridForStudent(grid, student);
                }

                lastStudent = student;

                if (section != null) {
                    populateMatrix(grid, section, studentSchedule, periodRowMap, termOids);
                }
            }
        }

        return grid;
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
     * @return Map
     */
    private Map initializeGridForStudent(DataGrid grid, SisStudent student) {
        HashMap periodRowMap = new HashMap(15);

        /*
         * Determine the number of periods in the schedule and initialize an adequate number
         * rows for the matrix
         */
        int periods = m_reportHelper.getSchedule().getPeriods();
        for (int i = 1; i <= periods; i++) {
            grid.append();
            grid.set(FIELD_PERIOD_NUMBER, Integer.valueOf(i));
            grid.set(FIELD_STUDENT, student);

            periodRowMap.put(Integer.valueOf(i), Integer.valueOf(grid.currentRowNumber()));
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
                                Map periodRowMap,
                                Collection<String> termOidsInRange) {
        String sectionDescription = section.getDescription();
        String sectionRoomView = section.getRoomView();
        String sectionStaffView = section.getStaffView();

        Map scheduledMap = m_scheduleManager.getScheduledMap(studentSchedule, m_reportHelper.getSectionClass());
        Iterator termIterator = scheduledMap.keySet().iterator();
        while (termIterator.hasNext()) {
            ScheduleTerm term = (ScheduleTerm) termIterator.next();

            Iterator dayPeriodIterator = ((Collection) scheduledMap.get(term)).iterator();
            while (dayPeriodIterator.hasNext()) {
                KeyValuePair dayPeriod = (KeyValuePair) dayPeriodIterator.next();
                ScheduleDay day = (ScheduleDay) dayPeriod.getKey();
                SchedulePeriod period = (SchedulePeriod) dayPeriod.getValue();

                StringBuilder matrixDetail = new StringBuilder(25);

                if (termOidsInRange != null && termOidsInRange.size() > 1) {
                    matrixDetail.append(TERM_BRACKET_OPEN);
                    matrixDetail.append(term.getCode());
                    matrixDetail.append(TERM_BRACKET_CLOSE);
                } else {
                    matrixDetail.append(term.getCode());
                }

                if (sectionDescription != null) {
                    matrixDetail.append(sectionDescription);
                }
                matrixDetail.append("\n");

                if (!StringUtils.isEmpty(sectionStaffView)) {
                    if (section.getSplitTeacherIndicator()) {
                        sectionStaffView = m_interfaceManager.createStaffViewForSection(studentSchedule.getSection(),
                                termOidsInRange, term, day, period, true, null).getKey();
                    }
                    if (!StringUtils.isEmpty(sectionStaffView)) {
                        matrixDetail.append(sectionStaffView);
                    }
                }

                if (!StringUtils.isEmpty(sectionRoomView)) {
                    if (section.getSplitRoomIndicator()) {
                        /*
                         * If there are more than one room for the section, retrieve the right room
                         * for the cell.
                         */
                        SchoolRoom room =
                                m_scheduleManager.getRoomScheduled(studentSchedule.getSection(), term, day, period);
                        if (room != null) {
                            sectionRoomView = room.getRoomNumber();
                        }
                    }

                    if (!StringUtils.isEmpty(sectionRoomView)) {
                        matrixDetail.append(" - ");
                        matrixDetail.append(sectionRoomView);
                    }
                }

                Integer rowNumber = (Integer) periodRowMap.get(Integer.valueOf(period.getNumber()));
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
                }
            }
        }
    }
}
