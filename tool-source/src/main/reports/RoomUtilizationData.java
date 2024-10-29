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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sched.utils.ScheduleMap;
import com.x2dev.sis.model.beans.BuildMasterSchedule;
import com.x2dev.sis.model.beans.BuildStudentRoom;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.MasterScheduleMatrix;
import com.x2dev.sis.model.beans.MasterTerm;
import com.x2dev.sis.model.beans.SchedulePeriod;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.SchoolRoom;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.business.schedule.ScheduleStructureManager;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.CollectionUtils;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares data for the "Room Utilization" report.
 *
 * @author X2 Development Corporation
 */
public class RoomUtilizationData extends ReportJavaSourceNet {
    private static final long serialVersionUID = 1L;

    /**
     * Name for the overbooked only parameter. The value is a boolean.
     */
    public static final String PARAM_OVER_BOOKED_ONLY = "overBookedOnly";

    /**
     * Name for the enumerated "selection" report parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "QueryBy input" report parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    // Grid fields
    private static final String FIELD_ROOM = "room";

    // Report parameter name constants
    private static final String PARAMETER_PERIOD_ID_LOOKUP = "periodIdLookup";
    private static final String PARAMETER_SCHOOL_YEAR_CONTEXT = "schoolYear";

    private ScheduleReportHelper m_reportHelper;
    private Map<String, Section> m_roomToSectionMap;
    private ScheduleStructureManager m_structureManager;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        m_structureManager = new ScheduleStructureManager(getBroker());
        Boolean overbookedOnly = (Boolean) getParameter(PARAM_OVER_BOOKED_ONLY);

        loadSectionData();
        ReportDataGrid grid = null;


        X2Criteria roomCriteria = new X2Criteria();
        roomCriteria.addEqualTo(SchoolRoom.COL_SCHOOL_OID, getSchool().getOid());

        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        addUserCriteria(roomCriteria, queryBy, queryString, null, null);

        QueryByCriteria query = new QueryByCriteria(SchoolRoom.class, roomCriteria);

        String sortBy = (String) getParameter(SORT_PARAM);
        applyUserSort(query, sortBy);

        QueryIterator roomIterator = getBroker().getIteratorByQuery(query);

        try {
            grid = new ReportDataGrid(1000, 15);
            StringBuilder roomDisplay = null;

            while (roomIterator.hasNext()) {
                SchoolRoom room = (SchoolRoom) roomIterator.next();

                LinkedList scheduleMaps = new LinkedList();

                Collection<Section> sectionsForRoom = (Collection) m_roomToSectionMap.get(room.getOid());
                Collection<String> seenSectionOids = new ArrayList<String>();

                /*
                 * Find any additional sections that this room is used by.
                 */
                if (CollectionUtils.isEmpty(sectionsForRoom)) {
                    sectionsForRoom = new ArrayList<Section>();
                    sectionsForRoom.addAll(findSplitRoomSections(room, seenSectionOids));
                } else {
                    CollectionUtils.loadPropertyCollection(sectionsForRoom, X2BaseBean.COL_OID, seenSectionOids);
                    sectionsForRoom.addAll(findSplitRoomSections(room, seenSectionOids));
                }

                grid.append();
                grid.set(FIELD_ROOM, room);

                if (sectionsForRoom != null && !sectionsForRoom.isEmpty()) {
                    for (Section master : sectionsForRoom) {
                        Collection<String> scheduledSlots =
                                m_structureManager.getScheduledSlotsForSection(master, true);
                        ScheduleMap scheduleMap = null;
                        String display = "";

                        if (master instanceof MasterSchedule &&
                                master.getSplitRoomIndicator()) {
                            /*
                             * If the section is split then remove any time slots that are not
                             * covered by this room.
                             */
                            Collection<String> notScheduledSlots =
                                    removeScheduledSlotsNotForRoom((MasterSchedule) master, room);
                            scheduledSlots.removeAll(notScheduledSlots);

                            scheduleMap = m_structureManager.convertTimeSlotsToScheduleMap(master.getSchedule(),
                                    scheduledSlots, new HashSet(), new ArrayList(0));
                        } else if (master instanceof BuildMasterSchedule &&
                                master.getSplitRoomIndicator()) {
                            /*
                             * If the section is split then grab the schedule map for this room and
                             * make those
                             * the schedule time slots.
                             */
                            Collection<BuildStudentRoom> buildRooms =
                                    ((BuildMasterSchedule) master).getBuildRooms(getBroker());
                            if (!CollectionUtils.isEmpty(buildRooms)) {
                                for (BuildStudentRoom buildRoom : buildRooms) {
                                    if (buildRoom.getSchoolRoomOid().equals(room.getOid())) {
                                        scheduleMap = buildRoom.getScheduleMap();
                                        display = buildRoom.getScheduleDisplay();
                                        break;
                                    }
                                }
                            }
                        } else {
                            scheduleMap = getScheduleMap(master);
                        }

                        if (scheduleMap != null) {

                            if (master.getSplitRoomIndicator()) {
                                if (master instanceof MasterSchedule) {
                                    String oidString = m_structureManager
                                            .convertScheduleMapToOidStrings(master.getSchedule(), scheduleMap, false);
                                    display = m_structureManager.convertOidStringsToScheduleDisplay(oidString,
                                            master.getSchedule().scheduleExpressionPeriodFirst(), false, false);
                                }
                            } else {
                                display = master.getScheduleDisplay();
                            }

                            for (int i = 1; i <= m_reportHelper.getSchedule().getPeriods(); i++) {
                                String periodNumber = String.valueOf(i);
                                if (scheduleMap.isScheduledForPeriod(i)) {
                                    roomDisplay = new StringBuilder(64);
                                    roomDisplay.append(master.getCourseView() + "\n");
                                    roomDisplay.append(master.getDescription() + "\n");
                                    if (master.getStaffView() != null) {
                                        roomDisplay.append(master.getStaffView() + "\n");
                                    }
                                    roomDisplay.append("Ttl: " + master.getEnrollmentTotal() + " ");

                                    if (scheduleMap.getBaseTerms() != scheduleMap.getCoveredTerms()) {
                                        roomDisplay.append("[" + master.getTermView() + "] ");
                                    }

                                    // TODO: The range of days only works if the schedule expression
                                    // is period-first.
                                    roomDisplay.append(display + "\n\n");

                                    StringBuilder preExists = (StringBuilder) grid.get(periodNumber);
                                    if (preExists == null) {
                                        grid.set(periodNumber, roomDisplay);
                                    } else {
                                        grid.set(periodNumber, preExists.append(roomDisplay));
                                    }
                                }
                            }

                            scheduleMaps.add(scheduleMap);
                        }
                    }
                }

                if (overbookedOnly.booleanValue() && !isOverBooked(scheduleMaps)) {
                    grid.deleteRow();
                }

                scheduleMaps.clear();
            }
        } finally {
            if (roomIterator != null) {
                roomIterator.close();
            }
        }

        addParameter(PARAMETER_PERIOD_ID_LOOKUP, getPeriodIdMap());
        addParameter(PARAMETER_SCHOOL_YEAR_CONTEXT,
                Integer.valueOf(m_reportHelper.getSchedule().getDistrictContext().getSchoolYear()));

        grid.beforeTop();
        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        m_reportHelper = new ScheduleReportHelper(userData);
    }

    /**
     * Find masters that apply to the room that the room is not primary for based on the sectionOids
     * passed in.
     *
     * @param room SchoolRoom
     * @param sectionOids Collection<String>
     * @return Collection<MasterSchedule>
     */
    private Collection<Section> findSplitRoomSections(SchoolRoom room, Collection<String> sectionOids) {
        Collection<Section> otherSections = new ArrayList<Section>();
        X2Criteria sectionMultiRoom = new X2Criteria();

        if (m_reportHelper.getSectionClass().equals(MasterSchedule.class)) {
            // Find MMX records that are for this room but are not part of sections that are
            // previously processed.
            sectionMultiRoom.addEqualTo(MasterScheduleMatrix.COL_SCHOOL_ROOM_OID, room.getOid());
            sectionMultiRoom.addEqualTo(MasterScheduleMatrix.REL_MASTER_TERM + ModelProperty.PATH_DELIMITER +
                    MasterTerm.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    MasterSchedule.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());
            if (!CollectionUtils.isEmpty(sectionOids)) {
                sectionMultiRoom.addNotIn(MasterScheduleMatrix.REL_MASTER_TERM + ModelProperty.PATH_DELIMITER +
                        MasterTerm.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER +
                        X2BaseBean.COL_OID, sectionOids);
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
                    BuildMasterSchedule.REL_BUILD_ROOMS + "." + BuildStudentRoom.COL_SCHOOL_ROOM_OID, room.getOid());
            if (!CollectionUtils.isEmpty(sectionOids)) {
                sectionMultiRoom.addNotIn(X2BaseBean.COL_OID, sectionOids);
            }
            sectionMultiRoom.addEqualTo(BuildMasterSchedule.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());

            QueryByCriteria query = new QueryByCriteria(BuildMasterSchedule.class, sectionMultiRoom);

            otherSections = getBroker().getCollectionByQuery(query);
        }
        return otherSections;
    }

    /**
     * Returns the period ID's into a map by the period number.
     *
     * @return A Map of String objects keyed on Integer objects
     */
    private Map getPeriodIdMap() {
        HashMap periodIdLookup = new HashMap(15);

        Iterator periods = m_reportHelper.getSchedule().getSchedulePeriods(getBroker()).iterator();
        while (periods.hasNext()) {
            SchedulePeriod period = (SchedulePeriod) periods.next();
            periodIdLookup.put(Integer.valueOf(period.getNumber()), period.getId());
        }

        return periodIdLookup;
    }

    /**
     * Returns the ScheduleMap object represented by the passed section.
     *
     * @param section Section
     * @return ScheduleMap
     */
    private ScheduleMap getScheduleMap(Section section) {
        ScheduleMap map = null;

        if (section instanceof BuildMasterSchedule) {
            BuildMasterSchedule buildSection = (BuildMasterSchedule) section;

            if (buildSection.getTermMap() != null && buildSection.getScheduleMatrix() != null) {
                map = new ScheduleMap(buildSection.getTermMap(), buildSection.getScheduleMatrix());
            }
        } else if (section instanceof MasterSchedule) {
            ScheduleStructureManager structureManager = new ScheduleStructureManager(getBroker());

            map = structureManager.convertMasterToWorkspace((MasterSchedule) section);
        }

        return map;
    }

    /**
     * Returns true if at least two among the passed list of schedule maps are in conflict.
     *
     * @param scheduleMaps LinkedList
     * @return true, if is over booked
     */
    private boolean isOverBooked(LinkedList scheduleMaps) {
        boolean conflictFound = false;

        int index = 0;
        Iterator scheduleMapIterator = scheduleMaps.iterator();
        while (scheduleMapIterator.hasNext() && !conflictFound) {
            ScheduleMap scheduleMap = (ScheduleMap) scheduleMapIterator.next();

            int compareIndex = 0;
            Iterator compareIterator = scheduleMaps.iterator();
            while (compareIterator.hasNext() && !conflictFound) {
                ScheduleMap mapToCompare = (ScheduleMap) compareIterator.next();

                if (compareIndex != index) {
                    conflictFound = mapToCompare.conflicts(scheduleMap, null);
                }
                compareIndex++;
            }

            index++;
        }

        return conflictFound;
    }

    /**
     * Loads a map of room -> sections based on primary room oids.
     */
    private void loadSectionData() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Section.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());
        QueryByCriteria query = new QueryByCriteria(m_reportHelper.getSectionClass(), criteria);

        m_roomToSectionMap = getBroker().getGroupedCollectionByQuery(query, Section.COL_PRIMARY_ROOM_OID, 500);
    }

    /**
     * Return the time slots for the section that do not apply to the room.
     *
     * @param section MasterSchedule
     * @param room SchoolRoom
     * @return Collection
     */
    private Collection<String> removeScheduledSlotsNotForRoom(MasterSchedule section, SchoolRoom room) {
        Collection<String> roomNotScheduleSlots = new ArrayList<String>();

        Iterator itTerm = section.getMasterTerms(getBroker()).iterator();
        while (itTerm.hasNext()) {
            MasterTerm masterTerm = (MasterTerm) itTerm.next();
            ScheduleTerm term = masterTerm.getScheduleTerm();

            Iterator termIt = term.countUsedTermNumbers().iterator();
            while (termIt.hasNext()) {
                int termNumber = ((Integer) termIt.next()).intValue();

                Iterator matrixIt = masterTerm.getMasterMatrices(getBroker()).iterator();
                while (matrixIt.hasNext()) {
                    MasterScheduleMatrix mstMatrix = (MasterScheduleMatrix) matrixIt.next();
                    if (!room.getOid().equals(mstMatrix.getSchoolRoomOid())) {
                        /*
                         * Iterate through the MTM and MMX records removing any that are not
                         * scheduled for this
                         * room.
                         */
                        String scheduledSlot = String.valueOf(termNumber) +
                                ScheduleStructureManager.SCHEDULE_TERM_DAY_PERIOD_DELIMITER +
                                mstMatrix.getScheduleMatrix().getScheduleDay().getNumber() +
                                ScheduleStructureManager.SCHEDULE_TERM_DAY_PERIOD_DELIMITER +
                                mstMatrix.getScheduleMatrix().getSchedulePeriod().getNumber();
                        roomNotScheduleSlots.add(scheduledSlot);
                    }
                }
            }
        }

        return roomNotScheduleSlots;
    }
}
