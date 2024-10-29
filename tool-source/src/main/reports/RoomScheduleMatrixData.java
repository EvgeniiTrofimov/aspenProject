/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2013 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sched.utils.ScheduleMap;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.schedule.ScheduleInterfaceManager;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Room Schedule Matrix" report.
 *
 * @author X2 Development Corporation
 */
public class RoomScheduleMatrixData extends ReportJavaSourceNet {
    /**
     * Name for the enumerated "selection" report parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "schedule term" report parameter. The value is an Integer.
     */
    public static final String SCHEDULE_TERM_OID_PARAM = "scheduleTermOid";

    /**
     * Name for the enumerated "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    // Grid field name constants
    private static final String FIELD_PERIOD_NUMBER = "period";
    private static final String FIELD_ROOM = "room";

    // The initial column size
    private static final int INITIAL_COLUMN_SIZE = 15;

    // The initial row size
    private static final int INITIAL_ROW_SIZE = 1000;

    // Report parameter name constants
    private static final String DAY_ID_LOOKUP_PARAM = "dayIdLookup";
    private static final String PERIOD_ID_LOOKUP_PARAM = "periodIdLookup";

    private ScheduleReportHelper m_reportHelper;
    private Criteria m_roomCriteria;
    private Map m_roomToSectionMap;
    private ScheduleManager m_scheduleManager;
    private Map m_roomNumberToRoom;
    private ScheduleInterfaceManager m_interfaceManager;
    private Map m_roomNumberToPeriodRowMap; // Map of room numbers keyed to period row maps
    private Collection<Section> m_multipleRooms; // Collection of courses with multiple rooms
    private Collection<String> m_termOids;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.reporting.ReportDataSource#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(4);

        m_scheduleManager = new ScheduleManager(getBroker());
        m_roomNumberToPeriodRowMap = new HashMap();
        m_multipleRooms = new ArrayList();
        m_roomNumberToRoom = new HashMap();
        m_interfaceManager = new ScheduleInterfaceManager(getBroker());

        buildRoomCriteria();
        loadSectionData();

        QueryByCriteria query = new QueryByCriteria(SchoolRoom.class, m_roomCriteria);

        int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
        switch (sort) {
            case 0: // Number
                query.addOrderByAscending(SchoolRoom.COL_ROOM_NUMBER);
                break;

            case 1: // Department
                query.addOrderByAscending(SchoolRoom.COL_DEPARTMENT_CODE);
                query.addOrderByAscending(SchoolRoom.COL_ROOM_NUMBER);
                break;

            case 2: // Building
                query.addOrderByAscending(SchoolRoom.COL_BUILDING_CODE);
                query.addOrderByAscending(SchoolRoom.COL_ROOM_NUMBER);
                break;

            default:
                break;
        }

        /*
         * Execute the query and build the matrices
         */
        QueryIterator roomIterator = null;
        try {
            roomIterator = getBroker().getIteratorByQuery(query);
            grid = buildMatrices(roomIterator);
        } finally {
            if (roomIterator != null) {
                roomIterator.close();
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
            addParameter(PERIOD_ID_LOOKUP_PARAM, periodIdLookup);

            HashMap dayIdLookup = new HashMap(15);
            Iterator days = m_reportHelper.getSchedule().getScheduleDays(getBroker()).iterator();
            while (days.hasNext()) {
                ScheduleDay day = (ScheduleDay) days.next();
                dayIdLookup.put(Integer.valueOf(day.getNumber()), day.getId());
            }
            addParameter(DAY_ID_LOOKUP_PARAM, dayIdLookup);
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.ToolJavaSource#saveState(com.x2dev.sis.web.UserDataContainer)
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
     * @param roomIterator QueryIterator
     * @return ReportDataGrid
     */
    private ReportDataGrid buildMatrices(QueryIterator roomIterator) {
        ReportDataGrid grid = new ReportDataGrid(INITIAL_ROW_SIZE, INITIAL_COLUMN_SIZE);
        CharSequence spaceSequence = new StringBuilder(",");

        if (m_reportHelper.getSchedule() != null) {
            Map periodRowMap = null;
            while (roomIterator.hasNext()) {
                SchoolRoom room = (SchoolRoom) roomIterator.next();
                m_roomNumberToRoom.put(room.getRoomNumber(), room);
                String oid = room.getOid();

                periodRowMap = initializeGridForRoom(grid, room);
                m_roomNumberToPeriodRowMap.put(room.getRoomNumber(), periodRowMap);

                if (m_roomToSectionMap.containsKey(oid)) {
                    Collection teacherSections = (Collection) m_roomToSectionMap.get(oid);

                    Iterator sectionIterator = teacherSections.iterator();
                    while (sectionIterator.hasNext()) {
                        Section section = (Section) sectionIterator.next();
                        if (section != null) {
                            populateMatrix(grid, section, periodRowMap, room);

                            if (section.getRoomView().contains(spaceSequence)) {
                                m_multipleRooms.add(section);
                            }
                        }
                    }
                }
            }

            if (!m_multipleRooms.isEmpty()) {
                Iterator multipleRoomsIterator = m_multipleRooms.iterator();
                while (multipleRoomsIterator.hasNext()) {
                    Section section = (Section) multipleRoomsIterator.next();
                    String roomView = section.getRoomView();

                    for (String roomNumber : StringUtils.convertDelimitedStringToList(roomView, ',')) {
                        if (m_roomNumberToRoom.containsKey(roomNumber) &&
                                !roomNumber.equals(section.getPrimaryRoom().getRoomNumber())) {
                            SchoolRoom multipleRoom = (SchoolRoom) m_roomNumberToRoom.get(roomNumber);

                            Map multiplePeriodRowMap = (Map) m_roomNumberToPeriodRowMap.get(roomNumber);
                            populateMatrix(grid, section, multiplePeriodRowMap, multipleRoom);
                        }
                    }
                }
            }
        }

        return grid;
    }

    /**
     * Builds the criteria for the SchoolRoom.
     * 
     * @return ReportDataGrid
     */
    private void buildRoomCriteria() {
        m_roomCriteria = new X2Criteria();

        int queryBy = 0;
        if (getParameter(QUERY_BY_PARAM) != null) {
            queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        }
        if (queryBy == 0) {
            m_roomCriteria = getCurrentCriteria();
        }

        else {
            m_roomCriteria.addEqualTo(SchoolRoom.COL_SCHOOL_OID, ((SisSchool) getSchool()).getOid());

            switch (queryBy) {
                case 2: // Department
                    m_roomCriteria.addEqualTo(SchoolRoom.COL_DEPARTMENT_CODE, getParameter(QUERY_STRING_PARAM));
                    break;

                case 3: // Building
                    m_roomCriteria.addEqualTo(SchoolRoom.COL_BUILDING_CODE, getParameter(QUERY_STRING_PARAM));
                    break;

                default: // All
                    break;
            }
        }
    }

    /**
     * Find masters that apply to the room that the room is not primary for based on the sectionOids
     * passed in.
     *
     * @param roomOid String
     * @param sectionOids Collection<String>
     * @return Collection<MasterSchedule>
     */
    private Collection<Section> findSplitRoomSections(String roomOid, Collection<String> sectionOids) {
        Collection<Section> otherSections = new ArrayList<Section>();
        X2Criteria sectionMultiRoom = new X2Criteria();

        if (m_reportHelper.getSectionClass().equals(MasterSchedule.class)) {
            // Find MMX records that are for this room but are not part of sections that are
            // previously processed.
            sectionMultiRoom.addEqualTo(MasterScheduleMatrix.COL_SCHOOL_ROOM_OID, roomOid);
            sectionMultiRoom.addEqualTo(MasterScheduleMatrix.REL_MASTER_TERM + ModelProperty.PATH_DELIMITER +
                    MasterTerm.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    MasterSchedule.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());
            if (!CollectionUtils.isEmpty(sectionOids)) {
                sectionMultiRoom.addNotIn(MasterScheduleMatrix.REL_MASTER_TERM + ModelProperty.PATH_DELIMITER +
                        MasterTerm.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                        X2BaseBean.COL_OID, sectionOids);
            }

            if (!m_termOids.isEmpty()) {
                sectionMultiRoom.addIn(MasterScheduleMatrix.REL_MASTER_TERM + ModelProperty.PATH_DELIMITER +
                        MasterTerm.COL_SCHEDULE_TERM_OID, m_termOids);
            }

            SubQuery sectionMultiRoomQuery = new SubQuery(MasterScheduleMatrix.class,
                    MasterScheduleMatrix.REL_MASTER_TERM + ModelProperty.PATH_DELIMITER +
                            MasterTerm.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                            X2BaseBean.COL_OID,
                    sectionMultiRoom);

            /*
             * Grab the collection of section oids to then grab the collection of beans.
             */
            Collection<String> multiRoomSections = getBroker().getSubQueryCollectionByQuery(sectionMultiRoomQuery);

            if (!CollectionUtils.isEmpty(multiRoomSections)) {
                Criteria sectionCriteria = new Criteria();
                sectionCriteria.addIn(X2BaseBean.COL_OID, multiRoomSections);

                QueryByCriteria query = new QueryByCriteria(MasterSchedule.class, sectionCriteria);

                otherSections = getBroker().getCollectionByQuery(query);
            }
        } else {
            /*
             * Find all BLR records that apply to this room but are not part of the sections that
             * the rooms are primary.
             */
            sectionMultiRoom.addEqualTo(
                    BuildMasterSchedule.REL_BUILD_ROOMS + "." + BuildStudentRoom.COL_SCHOOL_ROOM_OID, roomOid);
            if (!CollectionUtils.isEmpty(sectionOids)) {
                sectionMultiRoom.addNotIn(X2BaseBean.COL_OID, sectionOids);
            }
            sectionMultiRoom.addEqualTo(BuildMasterSchedule.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());

            if (!m_termOids.isEmpty()) {
                sectionMultiRoom.addIn(BuildMasterSchedule.COL_SCHEDULE_TERM_OID, m_termOids);
            }

            QueryByCriteria query = new QueryByCriteria(BuildMasterSchedule.class, sectionMultiRoom);

            otherSections = getBroker().getCollectionByQuery(query);
        }
        return otherSections;
    }


    /**
     * Returns the scheduled time slot map for the passed section.
     *
     * @param section Section
     * @param room SchoolRoom
     * @return HashMap key is ScheduleTerm object,
     *         Value is a list KeyValuePair object with ScheduleDay as Key, SchedulePeriod as value.
     */
    private HashMap getScheduledMap(Section section, SchoolRoom room) {
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

                    if (room.equals(masterMatrix.getSchoolRoom())) {
                        Collection dayPeriods = (Collection) map.get(term);
                        if (dayPeriods == null) {
                            dayPeriods = new ArrayList();
                        }
                        dayPeriods.add(dayPeriodPair);
                        map.put(term, dayPeriods);
                    }
                }
            }
        } else if (section.getClass().equals(BuildMasterSchedule.class)) {
            ScheduleMap scheduleMap = null;

            BuildMasterSchedule buildSection = (BuildMasterSchedule) section;
            if (buildSection.getTermMap() != null && buildSection.getScheduleMatrix() != null) {
                if (!m_scheduleManager.hasMultipleRooms(BuildMasterSchedule.class, buildSection)) {
                    scheduleMap = new ScheduleMap(buildSection.getTermMap(), buildSection.getScheduleMatrix());
                } else {
                    Iterator buildRoomIterator = buildSection.getBuildRooms().iterator();
                    while (buildRoomIterator.hasNext()) {
                        BuildStudentRoom buildRoom = (BuildStudentRoom) buildRoomIterator.next();

                        if (buildRoom.getSchoolRoom().equals(room)) {
                            scheduleMap = new ScheduleMap(buildRoom.getTermMap(), buildRoom.getScheduleMatrix());
                            break;
                        }
                    }
                }

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

    /**
     * Prepares the passed DataGrid to be populated with the passed school room. A row is
     * appended for each period in the schedule, and the room and period number fields are set.
     * <p>
     * A map is returned that resolves a period number to a row number in the grid. This map can be
     * used to look up a row number when populating the matrix cells with section data.
     *
     * @param grid DataGrid
     * @param room SchoolRoom
     * @return Map
     */
    private Map initializeGridForRoom(DataGrid grid, SchoolRoom room) {
        HashMap periodRowMap = new HashMap(15);

        /*
         * Determine the number of periods in the schedule and initialize an adequate number
         * rows for the matrix
         */
        int periods = m_reportHelper.getSchedule().getPeriods();
        for (int i = 1; i <= periods; i++) {
            grid.append();
            grid.set(FIELD_PERIOD_NUMBER, Integer.valueOf(i));
            grid.set(FIELD_ROOM, room);

            periodRowMap.put(Integer.valueOf(i), Integer.valueOf(grid.currentRowNumber()));
        }

        return periodRowMap;
    }

    /**
     * Loads a map of room numbers keyed to a collection of teacher sections.
     * Loads a map of room numbers keyed to a primary staff bean.
     */
    private void loadSectionData() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Section.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());

        SubQuery subQuery = new SubQuery(SchoolRoom.class, X2BaseBean.COL_OID, m_roomCriteria);
        criteria.addIn(Section.COL_PRIMARY_ROOM_OID, subQuery);

        String scheduleTermOid = (String) getParameter(SCHEDULE_TERM_OID_PARAM);
        if (!StringUtils.isEmpty(scheduleTermOid)) {
            m_termOids = new ArrayList<String>();

            ScheduleTerm term = (ScheduleTerm) getBroker().getBeanByOid(ScheduleTerm.class, scheduleTermOid);

            m_termOids.addAll(m_scheduleManager.getCoveredTermOids(term));
            m_termOids.addAll(m_scheduleManager.getCoveringTermOids(term));

            if (!m_termOids.isEmpty()) {
                criteria.addIn(Section.COL_SCHEDULE_TERM_OID, m_termOids);
            }
        }

        QueryByCriteria query = new QueryByCriteria(m_reportHelper.getSectionClass(), criteria);
        query.addOrderByDescending(Section.REL_SCHEDULE_TERM + "." + ScheduleTerm.COL_CODE);

        m_roomToSectionMap = getBroker().getGroupedCollectionByQuery(query, Section.COL_PRIMARY_ROOM_OID, 500);

        /*
         * Find additional sections for the room for which the room is not primary.
         */
        for (String roomOid : (Collection<String>) m_roomToSectionMap.keySet()) {
            Collection<String> existingSectionOids = new ArrayList<String>();
            Collection<Section> existingSections = (Collection<Section>) m_roomToSectionMap.get(roomOid);
            CollectionUtils.loadPropertyCollection(existingSections, X2BaseBean.COL_OID, existingSectionOids);
            existingSections.addAll(findSplitRoomSections(roomOid, existingSectionOids));
            m_roomToSectionMap.put(roomOid, existingSections);
        }
    }

    /**
     * Populates the passed DataGrid with teacher schedule matrix data for the passed section.
     *
     * @param grid DataGrid
     * @param section can be either a MasterSchedule or BuildMasterSchedule object
     * @param periodRowMap Map
     * @param room SchoolRoom
     */
    private void populateMatrix(DataGrid grid, Section section, Map periodRowMap, SchoolRoom room) {
        String sectionCourseView = section.getCourseView();
        String sectionDescription = section.getDescription();
        ScheduleTerm term = section.getScheduleTerm();
        SisStaff staff = (section.getPrimaryStaff());

        HashMap scheduledMap = getScheduledMap(section, room);
        if (section.getPrimaryRoom() != null && scheduledMap != null && scheduledMap.containsKey(term)) {
            Iterator dayPeriodIterator = ((Collection) scheduledMap.get(term)).iterator();
            while (dayPeriodIterator.hasNext()) {
                KeyValuePair dayPeriod = (KeyValuePair) dayPeriodIterator.next();
                ScheduleDay day = (ScheduleDay) dayPeriod.getKey();
                SchedulePeriod period = (SchedulePeriod) dayPeriod.getValue();

                StringBuilder matrixDetail = new StringBuilder(25);

                if (sectionCourseView != null) {
                    matrixDetail.append(sectionCourseView);
                }

                if (sectionDescription != null) {
                    matrixDetail.append("\n");
                    matrixDetail.append(sectionDescription);
                }

                if (staff != null) {
                    String sectionStaffView = staff.getNameView();
                    if (section.getSplitTeacherIndicator()) {
                        sectionStaffView = m_interfaceManager
                                .createStaffViewForSection(section, m_termOids, term, day, period, true, null).getKey();
                    }
                    matrixDetail.append("\n");
                    matrixDetail.append(sectionStaffView);
                }

                if (term != null) {
                    matrixDetail.append("\n");
                    matrixDetail.append(term.getCode());
                }

                Integer rowNumber = (Integer) periodRowMap.get(Integer.valueOf(period.getNumber()));
                if (rowNumber != null) {
                    grid.gotoRow(rowNumber.intValue());

                    // Populate the cell
                    String cellKey = Integer.toString(day.getNumber());

                    String currentContents = (String) grid.get(cellKey);
                    if (currentContents != null &&
                            !currentContents.contains(matrixDetail.toString())) {
                        matrixDetail.append("\r\n\n");
                        matrixDetail.append(currentContents);
                    }

                    grid.set(cellKey, matrixDetail.toString());
                }
            }
        }
    }
}
