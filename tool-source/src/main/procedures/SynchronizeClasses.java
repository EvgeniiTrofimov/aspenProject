/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2015 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sched.SchedConstants;
import com.x2dev.sched.utils.ScheduleMap;
import com.x2dev.sis.model.beans.BuildClass;
import com.x2dev.sis.model.beans.BuildMasterSchedule;
import com.x2dev.sis.model.beans.ClassSection;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.Schedule.ScheduleMode;
import com.x2dev.sis.model.beans.ScheduleClass;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.business.schedule.ClassManager;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.model.business.schedule.ScheduleStructureManager;
import com.x2dev.sis.web.schedule.SchedContextList;
import com.x2dev.sis.web.schedule.ScheduleUtils;
import com.x2dev.utils.ObjectUtils;
import java.util.*;
import java.util.Map.Entry;
import org.apache.commons.collections.CollectionUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This procedure is used to synchronize classes. It will accomplish the following:
 *
 * 1. Evaluate existing class records and sections included in each class and determine
 * if they are still valid. Remove the sections that will not fulfill the class structure,
 * discard the class record if there is no more than one section left.
 *
 * 2. Regenerate new classes based on the criteria: same teacher, same room, and matching time slot
 * criteria.
 *
 * This should only be executed from Build View, Workspace > Classes or School View, Schedule >
 * Classes.
 *
 * @author Follett Software Company
 */
public class SynchronizeClasses extends ProcedureJavaSource {
    /**
     * The class manager object
     */
    private ClassManager m_classManager = null;

    /**
     * The current Schedule object.
     */
    private Schedule m_schedule = null;

    /**
     * The UserDataContainer object.
     */
    private UserDataContainer m_userData = null;

    /**
     * The class ID field length.
     */
    private int m_classIdFieldLength = 0;


    /**
     * Comparator used to sort sections by most coverage.
     */
    private Comparator<Map.Entry<Section, Collection>> m_comparator = null;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        if (m_schedule != null) {
            /*
             * Determine if existing ScheduleClass records are still valid. If not, remove sections
             * from them.
             * If a ScheduleClass does not contain any sections, delete it.
             *
             * Criteria to determine if sections within a ScheduleClass are valid
             * 1) All sections must have the same primary teacher and room.
             *
             * 2) The primary section(s) will be determine for a class. If any sections are not
             * fully covered by the
             * primary section, those sections will be removed from the class.
             *
             * 3) If at any point a class contains less than 2 sections, it will be deleted.
             *
             */
            Collection<ClassSection> classesToDelete = new ArrayList<ClassSection>();
            Collection<ClassSection> classes = new ArrayList<ClassSection>();

            ScheduleManager scheduleManager = new ScheduleManager(getBroker());
            Collection<String> courseWeightExemptOids = scheduleManager.getCourseOidsExemptWeight(m_schedule);

            if (m_userData.getApplicationContext() == ApplicationContext.SCHOOL) {
                classes.addAll(m_schedule.getScheduleClasses(getBroker()));
            } else if (m_userData.getApplicationContext() == ApplicationContext.BUILD ||
                    m_userData.getApplicationContext() == ApplicationContext.ELEMENTARY) {
                classes.addAll(m_schedule.getBuildClasses(getBroker()));
            }

            for (ClassSection classSection : classes) {
                updateClass(classesToDelete, classSection, courseWeightExemptOids);
            }

            deleteMarkedClasses(classesToDelete);

            /*
             * Create new classes for any sections meeting class criteria that are not currently in
             * a class.
             *
             * Criteria
             * 1) All sections in a class must have the same primary teacher and room.
             * 2) All sections must have a schedule and term assigned.
             * 2) All sections in a class must have the same time slots or be covered fully by the
             * primary section.
             */

            // Query out all sections for review.
            X2Criteria sectionCriteria = new X2Criteria();
            sectionCriteria.addEqualTo(Section.COL_SCHEDULE_OID, m_schedule.getOid());
            sectionCriteria.addIsNull(Section.COL_SECTION_CLASS_OID);
            sectionCriteria.addNotNull(Section.COL_PRIMARY_STAFF_OID);

            if (isElementaryContext()) {
                sectionCriteria.addNotNull(Section.COL_TEAM_CODE);
            } else {
                sectionCriteria.addNotNull(Section.COL_SCHEDULE_TERM_OID);
                sectionCriteria.addNotNull(Section.COL_PRIMARY_ROOM_OID);
                sectionCriteria.addNotNull(Section.COL_SCHEDULE_DISPLAY);
            }

            QueryByCriteria query = null;
            if (m_userData.getApplicationContext() == ApplicationContext.SCHOOL) {
                query = new QueryByCriteria(MasterSchedule.class, sectionCriteria);
            } else if (m_userData.getApplicationContext() == ApplicationContext.BUILD ||
                    m_userData.getApplicationContext() == ApplicationContext.ELEMENTARY) {
                query = new QueryByCriteria(BuildMasterSchedule.class, sectionCriteria);
            }

            // Map<TeacherOid, Map<RoomOid, Collection<Section>>>
            String[] columns = isElementaryContext()
                    ? new String[] {Section.COL_PRIMARY_STAFF_OID, Section.COL_TEAM_CODE}
                    : new String[] {Section.COL_PRIMARY_STAFF_OID, Section.COL_PRIMARY_ROOM_OID};
            Map<String, Map<String, Collection<Section>>> teacherRoomSections =
                    getBroker().getGroupedCollectionByQuery(query, columns, new int[] {1, 1});

            createNewClasses(teacherRoomSections, courseWeightExemptOids);
        } else {
            logMessage("No Schedule could be identified.");
        }
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        setClassIdFieldLength();
        initializeCoverageComparator();
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
        m_schedule = ScheduleUtils.getSchedule(userData);
        m_userData = userData;
        m_classManager = new ClassManager(getBroker());
    }

    /**
     * Creates and saves a ClassSection with the information contained in the ClassDetail.
     *
     * @param classDetail ClassDetail
     * @param courseWeightExemptOids Collection<String>
     * @param elementaryIndicator boolean
     */
    private void createClassSection(ClassDetail classDetail,
                                    Collection<String> courseWeightExemptOids,
                                    boolean elementaryIndicator) {
        ClassSection classSection = null;

        if (m_userData.getApplicationContext() == ApplicationContext.SCHOOL) {
            classSection = new ScheduleClass(getBroker().getPersistenceKey());
        } else if (m_userData.getApplicationContext() == ApplicationContext.BUILD ||
                m_userData.getApplicationContext() == ApplicationContext.ELEMENTARY) {
            classSection = new BuildClass(getBroker().getPersistenceKey());
        }

        // Update the Class information
        classSection.setEnrollmentMaxCloseIndicator(classDetail.isClosedAtMaxIndicator());
        classSection.setMaxEnrollment(classDetail.getEnrollmentMax());

        String classId = classDetail.getClassId().toString();
        if (classId.length() > m_classIdFieldLength) {
            classId = classId.substring(0, m_classIdFieldLength);
        }

        classSection.setId(classId);
        classSection.setScheduleOid(m_schedule.getOid());
        classSection.setElementaryIndicator(elementaryIndicator);

        if (m_userData.getApplicationContext() == ApplicationContext.SCHOOL) {
            getBroker().saveBeanForced((ScheduleClass) classSection);
        } else if (m_userData.getApplicationContext() == ApplicationContext.BUILD ||
                m_userData.getApplicationContext() == ApplicationContext.ELEMENTARY) {
            getBroker().saveBeanForced((BuildClass) classSection);
        }

        logMessage("Created Class: " + classSection.getId());

        for (Section section : classDetail.getSections()) {
            // Update the individual section to point to the class
            section.setSectionClassOid(classSection.getOid());
            getBroker().saveBeanForced((X2BaseBean) section);

            logMessage("Added section " + section.getCourseView() + " to class " + classSection.getId());
        }

        /*
         * Update the new enrollment total for new classes
         */
        updateClassTotal(courseWeightExemptOids, classSection);

        logMessage("\r\n");
    }

    /**
     * Calculates and updates the class total.
     *
     * @param courseWeightExemptOids Collection<String>
     * @param classSection ClassSection
     */
    private void updateClassTotal(Collection<String> courseWeightExemptOids, ClassSection classSection) {
        Map<ScheduleMap, Number> classTotal = new HashMap<ScheduleMap, Number>();
        Map<ScheduleMap, Number> classWeightTotal = new HashMap<ScheduleMap, Number>();

        m_classManager.calculateTotal(classSection, classTotal, classWeightTotal, courseWeightExemptOids);
        classSection.setEnrollmentTotal(ClassManager.getTotalCountString(classTotal));
        classSection.setWeightTotal(ClassManager.getTotalCountString(classWeightTotal));

        if (m_userData.getApplicationContext() == ApplicationContext.SCHOOL) {
            getBroker().saveBeanForced((ScheduleClass) classSection);
        } else if (m_userData.getApplicationContext() == ApplicationContext.BUILD ||
                m_userData.getApplicationContext() == ApplicationContext.ELEMENTARY) {
            getBroker().saveBeanForced((BuildClass) classSection);
        }
    }

    /**
     * Creates new classes for sections meeting all class requirements.
     *
     * @param teacherGroupSections Map<String,Map<String,Collection<Section>>>
     * @param courseWeightExemptOids Collection<String>
     */
    private void createNewClasses(Map<String, Map<String, Collection<Section>>> teacherGroupSections,
                                  Collection<String> courseWeightExemptOids) {
        ScheduleStructureManager structureManager = new ScheduleStructureManager(getBroker());

        for (String staffOid : teacherGroupSections.keySet()) {
            Map<String, Collection<Section>> groupToSections = teacherGroupSections.get(staffOid);

            if (groupToSections != null) {
                for (String key : groupToSections.keySet()) {
                    Collection<Section> sections = groupToSections.get(key);
                    if (sections != null && sections.size() > 1) {

                        /*
                         * There are multiple sections for this teacher and room. Determine if
                         * any
                         * overlap
                         * and create classes as needed.
                         */
                        Map<Section, Collection> sortedSections = getSectionsByCoverage(structureManager, sections);

                        // sortedSections contains the sections with the most coverage first.
                        Collection<String> sectionsInClass = new ArrayList<String>();

                        for (Section primarySection : sortedSections.keySet()) {
                            if (!sectionsInClass.contains(primarySection.getOid())) {
                                // Add the current section to reviewed sections
                                sectionsInClass.add(primarySection.getOid());

                                ClassDetail classDetail = new ClassDetail();
                                classDetail.updateClassDetail(primarySection);
                                boolean elementary = false;

                                Collection<String> primarySectionTimeSlots = sortedSections.get(primarySection);
                                for (Section section : sections) {
                                    if (!section.equals(primarySection)
                                            && !sectionsInClass.contains(section.getOid())) {
                                        if (section.getElementaryIndicator() &&
                                                CollectionUtils.isEqualCollection(primarySectionTimeSlots,
                                                        sortedSections.get(section))) {
                                            // The primary section completely covers this
                                            // section.
                                            sectionsInClass.add(section.getOid());
                                            classDetail.updateClassDetail(section);
                                            elementary = true;
                                        } else if (!section.getElementaryIndicator() && primarySectionTimeSlots
                                                .containsAll(sortedSections.get(section))) {
                                            // The primary section completely covers this
                                            // section.
                                            sectionsInClass.add(section.getOid());
                                            classDetail.updateClassDetail(section);
                                        }
                                    }
                                }

                                if (classDetail.getSectionCount() > 1) {
                                    // This class contains multiple sections. Create the
                                    // appropriate
                                    // ClassSection object.
                                    createClassSection(classDetail, courseWeightExemptOids, elementary);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Deletes Class objects and logs results.
     *
     * @param classesToDelete Collection<ClassSection>
     */
    private void deleteMarkedClasses(Collection<ClassSection> classesToDelete) {
        // Delete any classes marked for delete and log a message.
        logMessage("\r\n");
        for (ClassSection clazz : classesToDelete) {
            if (m_userData.getApplicationContext() == ApplicationContext.SCHOOL) {
                getBroker().deleteBean((ScheduleClass) clazz);
            } else if (m_userData.getApplicationContext() == ApplicationContext.BUILD ||
                    m_userData.getApplicationContext() == ApplicationContext.ELEMENTARY) {
                getBroker().deleteBean((BuildClass) clazz);
            }

            logMessage("Deleted Class " + clazz.getId());
            logMessage("\r\n");
        }
    }

    /**
     * Creates a comparator used to sort sections by the most coverage.
     */
    private void initializeCoverageComparator() {
        m_comparator = new Comparator<Map.Entry<Section, Collection>>() {
            @Override
            public int compare(Map.Entry<Section, Collection> o1, Map.Entry<Section, Collection> o2) {
                // Descending sort
                return (Integer.valueOf(o2.getValue().size())).compareTo(Integer.valueOf(o1.getValue().size()));
            }
        };
    }

    /**
     * Returns a map of Sections containing all covered time slot information. The Map is sorted by
     * the sections
     * with the most coverage descending.
     *
     * @param structureManager ScheduleStructureManager
     * @param sections Collection<Section>
     * @return Map<Section, Collection>
     */
    private Map<Section, Collection> getSectionsByCoverage(ScheduleStructureManager structureManager,
                                                           Collection<Section> sections) {
        Map timeSlotsBySection = new HashMap<Section, Collection<String>>();

        for (Section section : sections) {
            Collection<String> timeSlots = structureManager.getScheduledSlotsForSection(section, true);
            timeSlotsBySection.put(section, timeSlots);
        }

        // Sort the timeSlotsBySection by sections with the most coverage first.
        List<Entry<Section, Collection>> list = new LinkedList<>(timeSlotsBySection.entrySet());
        Collections.sort(list, m_comparator);

        Map<Section, Collection> sortedSections = new LinkedHashMap<Section, Collection>();
        for (Map.Entry<Section, Collection> entry : list) {
            sortedSections.put(entry.getKey(), entry.getValue());
        }

        return sortedSections;
    }

    /**
     * Return whether we should process elementary classes or not.
     *
     * @return boolean
     */
    private boolean isElementaryContext() {
        return m_schedule.isElementary() ||
                (m_userData.getCurrentList() instanceof SchedContextList
                        && ((SchedContextList) m_userData.getCurrentList())
                                .getSelectedSchedMode() == ScheduleMode.Elementary.ordinal());
    }

    /**
     * Sets the Class Id field length.
     *
     * @return int
     */
    private void setClassIdFieldLength() {
        DataFieldConfig classIdField = null;

        if (m_userData.getApplicationContext() == ApplicationContext.BUILD ||
                m_userData.getApplicationContext() == ApplicationContext.ELEMENTARY) {
            classIdField =
                    DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey()).findDataDictionaryField(
                            BuildClass.class.getName(), BuildClass.COL_ID).getDataFieldConfig();

        } else if (m_userData.getApplicationContext() == ApplicationContext.SCHOOL) {
            classIdField =
                    DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey()).findDataDictionaryField(
                            ScheduleClass.class.getName(), ScheduleClass.COL_ID).getDataFieldConfig();
        }

        /*
         * The classIdField should never be null. It can only be null in the event the procedure is
         * not being ran
         * from a supported view (Build view or School view).
         */
        m_classIdFieldLength = classIdField.getDataField().getDatabaseLength();
    }

    /**
     * Removes the section from the class and logs the reason.
     *
     * @param classId String
     * @param section Section
     * @param reason String
     */
    private void removeSectionFromClass(String classId, Section section, String reason) {
        section.setSectionClassOid(null);

        if (m_userData.getApplicationContext() == ApplicationContext.SCHOOL) {
            getBroker().saveBeanForced((MasterSchedule) section);
        } else if (m_userData.getApplicationContext() == ApplicationContext.BUILD ||
                m_userData.getApplicationContext() == ApplicationContext.ELEMENTARY) {
            getBroker().saveBeanForced((BuildMasterSchedule) section);
        }

        logMessage("Removed section " + section.getCourseView() + "from class " + classId + "." + reason);
    }

    /**
     * Removes any invalid sections from a ClassSection. If less than two sections remain in the
     * class, the class
     * is marked for delete.
     *
     * @param classesToDelete Collection<ClassSection>
     * @param classSection ClassSection
     * @param courseWeightExemptOids Collection<String>
     */
    private void updateClass(Collection<ClassSection> classesToDelete,
                             ClassSection classSection,
                             Collection<String> courseWeightExemptOids) {
        Collection<Section> sections = classSection.getSections();

        /*
         * If no more than 1 section exists for a class, delete it.
         */
        if (sections != null && sections.size() > 1) {
            HashMap<String, Collection<String>> timeSlotsBySection = new HashMap<String, Collection<String>>();

            if (classSection.getElementaryIndicator()) {
                Section primarySection = sections.iterator().next();

                for (Section section : sections) {
                    boolean removeSectionFromClass = false;
                    String reason = null;

                    if (!ObjectUtils.match(primarySection.getTeamCode(), section.getTeamCode()) ||
                            !ObjectUtils.match(primarySection.getPrimaryStaffOid(), section.getPrimaryStaffOid())) {
                        removeSectionFromClass = true;
                        reason = "\r\nSection does not have matching primary team code and/or staff.";
                    }

                    if (removeSectionFromClass) {
                        removeSectionFromClass(classSection.getId(), section, reason);
                    }
                }
            } else {
                Collection<Section> primarySections = new HashSet<Section>();
                Collection<String> primarySectionTimeSlots = new HashSet<String>();
                m_classManager.identifyPrimarySections(sections, timeSlotsBySection, primarySections,
                        primarySectionTimeSlots);

                for (Section primarySection : primarySections) {
                    for (Section section : sections) {
                        if (!section.equals(primarySection)) {
                            boolean removeSectionFromClass = false;
                            String reason = null;

                            if (primarySection.getRoomView() != null &&
                                    primarySection.getRoomView().equals(section.getRoomView())) {
                                Collection<String> timeSlotsForSection =
                                        new ArrayList<String>(timeSlotsBySection.get(section.getOid()));
                                if (!primarySectionTimeSlots.containsAll(timeSlotsForSection)) {
                                    // This section is not fully contained by the primary section.
                                    // Remove this section from the class.
                                    removeSectionFromClass = true;
                                    reason = "\r\nSection is not fully contained by the Class primary section";
                                }
                            } else {
                                removeSectionFromClass = true;
                                reason = "\r\nSection does not have matching primary staff and/or room.";
                            }

                            if (removeSectionFromClass) {
                                removeSectionFromClass(classSection.getId(), section, reason);
                            }
                        }
                    }
                }
            }
            // Remove the class if it contains less than two sections.
            if (classSection.getSections().size() < 2) {
                classesToDelete.add(classSection);
            } else {
                /*
                 * For all other classes, update the class total
                 */
                updateClassTotal(courseWeightExemptOids, classSection);
            }
        } else {
            // This class contains one or less sections. Delete it.
            classesToDelete.add(classSection);
        }
    }

    /**
     * This class is used to store information required to create a ScheduleClass object from a
     * group of matching sections.
     *
     * @author Follett Software Company
     */
    private class ClassDetail {
        private StringBuilder m_classId = null;
        private boolean m_closedAtMaxIndicator = true;
        private int m_enrollmentMax;
        private Collection<Section> m_sections = new ArrayList<Section>();

        /**
         * Creates a new ClassDetail.
         */
        protected ClassDetail() {
            m_classId = new StringBuilder("CLS");
        }

        /**
         * Gets the classId.
         *
         * @return String
         */
        protected StringBuilder getClassId() {
            return m_classId;
        }

        /**
         * Gets the enrollment max.
         *
         * @return int
         */
        protected int getEnrollmentMax() {
            return m_enrollmentMax;
        }

        /**
         * Returns the sectionCount.
         *
         * @return int
         */
        protected int getSectionCount() {
            return m_sections.size();
        }

        /**
         * Returns the sections.
         *
         * @return Collection<Section>
         */
        protected Collection<Section> getSections() {
            return m_sections;
        }

        /**
         * Gets the closed at max indicator.
         *
         * @return boolean
         */
        protected boolean isClosedAtMaxIndicator() {
            return m_closedAtMaxIndicator;
        }

        /**
         * Updates the ClassDetail values based on values on the passed in section. This will update
         * the following:
         *
         * Enrollment Max
         * Closed at Max Indicator
         * Class Id
         *
         * @param section Section
         */
        protected void updateClassDetail(Section section) {
            m_classId.append(" " + SchedConstants.SYSTEM_CLASS_ID_DELIMITER + section.getCourseView());
            m_closedAtMaxIndicator = m_closedAtMaxIndicator && section.getEnrollmentMaxCloseIndicator();
            m_enrollmentMax = Math.max(section.getEnrollmentMax(), m_enrollmentMax);
            m_sections.add(section);
        }
    }
}
