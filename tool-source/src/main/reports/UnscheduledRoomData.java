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
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.*;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Unscheduled Room" report. This report shows, by period, a list of
 * rooms that are free and when they are free for that period.
 *
 * @author X2 Development Corporation
 */
public class UnscheduledRoomData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "selection" report parameter. The value is an String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the String "sort" report parameter. The value is an String.
     */
    public static final String SORT_PARAM = "sort";

    // Grid fields
    private static final String FIELD_DISPLAY = "display";
    private static final String FIELD_MASTER_SET = "masterSet";
    private static final String FIELD_PERIOD = "period";
    private static final String FIELD_ROOM = "room";

    private Map<String, Collection<String>> m_scheduledTimesForSection;
    private ScheduleStructureManager m_stuctureManager;

    private ScheduleReportHelper m_reportHelper;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.reporting.ReportDataSource#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid resultsGrid = new ReportDataGrid(4);

        if (m_reportHelper.getSchedule() != null) {
            m_scheduledTimesForSection = new HashMap<String, Collection<String>>(5000);
            m_stuctureManager = new ScheduleStructureManager(getBroker());

            ReportDataGrid roomGrid = getRoomGrid();

            // Iterate over each period looking for staff that do not have a class that period.
            Iterator periods = m_reportHelper.getSchedule().getSchedulePeriods().iterator();
            while (periods.hasNext()) {
                SchedulePeriod period = (SchedulePeriod) periods.next();
                Collection timeSlots =
                        m_stuctureManager.getTimeSlots(m_reportHelper.getSchedule(), period.getNumber(), false);

                while (roomGrid.next()) {
                    List<X2BaseBean> masterSet = (List<X2BaseBean>) roomGrid.get(FIELD_MASTER_SET);
                    SchoolRoom room = (SchoolRoom) roomGrid.get(FIELD_ROOM);

                    String unscheduledTimes = getUnscheduledForPeriod(room.getOid(), period, masterSet, timeSlots);
                    if (!StringUtils.isEmpty(unscheduledTimes)) {
                        resultsGrid.append();
                        resultsGrid.set(FIELD_PERIOD, String.valueOf(period.getNumber()));
                        resultsGrid.set(FIELD_ROOM, room);
                        resultsGrid.set(FIELD_DISPLAY, unscheduledTimes);
                    }
                }

                roomGrid.beforeTop();
            }

            resultsGrid.beforeTop();
        }

        return resultsGrid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see
     *      com.x2dev.sis.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
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
     * Builds a grid of SchoolRoom records and sets of Master sections as well as builds a Map of a
     * section's scheduled times keyed to the section OID.
     *
     * @return ReportDataGrid
     */
    private ReportDataGrid getRoomGrid() {
        X2Criteria roomCriteria = new X2Criteria();
        roomCriteria.addEqualTo(SchoolRoom.COL_SCHOOL_OID, getSchool().getOid());

        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        addUserCriteria(roomCriteria, queryBy, queryString, null, null);

        QueryByCriteria query = new QueryByCriteria(SchoolRoom.class, roomCriteria);

        String sortBy = (String) getParameter(SORT_PARAM);
        applyUserSort(query, sortBy);

        /*
         * Build room by section map
         */
        X2Criteria sectionCriteria = new X2Criteria();
        sectionCriteria.addEqualTo(Section.COL_SCHEDULE_OID, m_reportHelper.getScheduleOid());
        sectionCriteria.addNotEmpty(Section.COL_PRIMARY_ROOM_OID, getBroker().getPersistenceKey());

        QueryByCriteria sectionQuery = new QueryByCriteria(m_reportHelper.getSectionClass(), sectionCriteria);
        sectionQuery.addOrderByAscending(Section.COL_COURSE_VIEW);
        Map sectionsByRoomMap =
                getBroker().getGroupedCollectionByQuery(sectionQuery, Section.COL_PRIMARY_ROOM_OID, 1000);

        QueryIterator roomIterator = getBroker().getIteratorByQuery(query);
        ReportDataGrid grid = new ReportDataGrid(4);

        try {
            while (roomIterator.hasNext()) {
                SchoolRoom room = (SchoolRoom) roomIterator.next();

                grid.append();
                grid.set(FIELD_ROOM, room);
                grid.set(FIELD_MASTER_SET, new LinkedList());

                Collection<Section> sectionsForRoom = (Collection) sectionsByRoomMap.get(room.getOid());
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

                if (sectionsForRoom != null && !sectionsForRoom.isEmpty()) {
                    LinkedList masterSet = (LinkedList) grid.get(FIELD_MASTER_SET);

                    for (Section section : sectionsForRoom) {
                        masterSet.add(section);

                        if (!m_scheduledTimesForSection.containsKey(section.getOid() + "|" + room.getOid())) {
                            Collection<String> scheduledSlots =
                                    m_stuctureManager.getScheduledSlotsForSection(section, true);

                            if (section instanceof MasterSchedule &&
                                    section.getSplitRoomIndicator()) {
                                /*
                                 * If the section is split then remove any time slots that are not
                                 * covered by this room.
                                 */
                                Collection<String> notScheduledSlots =
                                        removeScheduledSlotsNotForRoom((MasterSchedule) section, room);
                                scheduledSlots.removeAll(notScheduledSlots);
                            } else if (section instanceof BuildMasterSchedule &&
                                    section.getSplitRoomIndicator()) {
                                /*
                                 * If the section is split then grab the schedule map for this room
                                 * and make those
                                 * the schedule time slots.
                                 */
                                Collection<BuildStudentRoom> buildRooms =
                                        ((BuildMasterSchedule) section).getBuildRooms(getBroker());
                                if (!CollectionUtils.isEmpty(buildRooms)) {
                                    for (BuildStudentRoom buildRoom : buildRooms) {
                                        if (buildRoom.getSchoolRoomOid().equals(room.getOid())) {
                                            scheduledSlots = m_stuctureManager.convertScheduleMapToScheduledSlots(
                                                    m_reportHelper.getSchedule(),
                                                    buildRoom.getScheduleMap(),
                                                    false,
                                                    null);
                                            break;
                                        }
                                    }
                                }
                            }
                            m_scheduledTimesForSection.put(section.getOid() + "|" + room.getOid(), scheduledSlots);
                        }
                    }
                }
            }
        } finally {
            roomIterator.close();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Checks if current room is available for the given period for any day or term. If so, returns
     * the schedule display of the unscheduled time.
     *
     * @param roomOid String
     * @param period SchedulePeriod
     * @param sections List<X2BaseBean>
     * @param timeSlots Collection
     * @return String
     */
    private String getUnscheduledForPeriod(String roomOid,
                                           SchedulePeriod period,
                                           List<X2BaseBean> sections,
                                           Collection timeSlots) {
        String unscheduledDisplay = "";
        Set<String> scheduledTimes = new HashSet<String>();

        if (sections != null && !sections.isEmpty()) {
            for (X2BaseBean section : sections) {
                ScheduleMap scheduleMap;
                if (section.getClass().equals(MasterSchedule.class)) {
                    scheduleMap = m_stuctureManager.convertMasterToWorkspace((MasterSchedule) section);
                } else {
                    scheduleMap = ((BuildMasterSchedule) section).getScheduleMap();
                }

                if (scheduleMap != null && scheduleMap.isScheduledForPeriod(period.getNumber())) {
                    scheduledTimes.addAll(m_scheduledTimesForSection.get(section.getOid() + "|" + roomOid));
                }
            }
        }

        /*
         * Look through scheduledTimes & extract unscheduled times for period
         */
        Collection unscheduledTimes = new ArrayList(timeSlots);
        if (unscheduledTimes.removeAll(scheduledTimes)) {
            Map<String, Collection<String>> termTimeSlots = new HashMap<String, Collection<String>>();

            /*
             * For all the unscheduled times place them in a map of term -> collection of times.
             */
            for (String unscheduledTime : (Collection<String>) unscheduledTimes) {
                String term = unscheduledTime.trim().split("\\|")[1];
                Collection<String> times = termTimeSlots.get(term);
                if (times == null) {
                    times = new ArrayList<String>();
                }
                times.add(unscheduledTime);
                termTimeSlots.put(term, times);
            }

            Map<String, List<String>> expressionToTerm = new HashMap<String, List<String>>();

            /*
             * Convert the time slots for each term and place them into a map of schedule expression
             * -> collection of terms.
             */
            for (String term : termTimeSlots.keySet()) {
                String tempDisplay = m_stuctureManager.convertTimeSlotsToScheduleDisplay(m_reportHelper.getSchedule(),
                        termTimeSlots.get(term), true, false);

                String[] data = tempDisplay.split(" ");
                if (data != null && data.length > 1) {
                    List<String> terms = expressionToTerm.get(data[2]);
                    if (terms == null) {
                        terms = new ArrayList<String>();
                    }

                    terms.add(data[1]);
                    expressionToTerm.put(data[2], terms);
                }
            }

            /*
             * Iterate through the expression to term map grouping the terms together that have the
             * same display.
             */
            for (String expression : expressionToTerm.keySet()) {
                List<String> terms = expressionToTerm.get(expression);
                if (terms.size() != m_reportHelper.getSchedule().getTerms()) {
                    String termString = "[";

                    Collections.sort(terms);

                    for (String term : terms) {
                        term = term.replace('[', '\0');
                        term = term.replace(']', '\0');
                        termString += term.trim() + " ";
                    }
                    termString = termString.trim();
                    termString += "]";
                    unscheduledDisplay = unscheduledDisplay + termString + " " + expression + ", ";
                } else {
                    unscheduledDisplay = expression;
                }
            }

            /*
             * Remove a trailing comma.
             */
            if (!StringUtils.isEmpty(unscheduledDisplay) &&
                    unscheduledDisplay.lastIndexOf(',') != -1 &&
                    unscheduledDisplay.lastIndexOf(',') == (unscheduledDisplay.length() - 2)) {
                unscheduledDisplay = unscheduledDisplay.substring(0, unscheduledDisplay.lastIndexOf(','));
            }
        } else {
            unscheduledDisplay =
                    m_stuctureManager.buildAllDayScheduleDisplayForPeriod(m_reportHelper.getSchedule(), period);
        }

        return unscheduledDisplay;
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
