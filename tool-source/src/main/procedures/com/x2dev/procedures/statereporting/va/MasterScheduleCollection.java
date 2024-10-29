/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.va;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Virginia state export procedure for Master Schedule Collection.
 *
 * The Master Schedule Collection collects a variety of data records, many of
 * which are unrelated or related in a non-hierarchical network. As a result,
 * there is not a single query or bean list that can source or drive the entire
 * export. As a result, this export will be driven by a single query to the root
 * organization. This will result in a single instance of StateReportEntity on
 * that organization. The entity will
 *
 * @author X2 Development Corporation
 */
public class MasterScheduleCollection extends StateReportData {
    @SuppressWarnings("unused")
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Implementation of StateReportEntity to be used by the VA Master Schedule
     * Collection. This must be a public static inner class with a public no
     * argument constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class MasterScheduleEntity extends StateReportEntity {
        @SuppressWarnings({"unused", "hiding"})
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";
        /**
         * List of row definitions indexed by row number. These definitions
         * select an export format definition to use for the row. Valid values
         * are: null,C,D,E,F,G,H (null represents row type B).
         */
        private List<String> m_definitionId;

        /**
         * List of X2BaseBeans indexed by row number. These represent the source
         * data bean for the current row. This is used in conjunction with the
         * export format definition for the row.
         */
        private List<X2BaseBean> m_elements;
        private List<X2BaseBean> m_elementsTemp;

        private MasterScheduleCollection m_data;

        /**
         * Set of staffOid that have reportable teacher schedules in type C.
         * This is used to identify teachers to report in type B.
         */
        private Set<String> m_staffWithSchedules;
        private Map<String, Set<String>> m_gradesForSectionsCRecords = new HashMap<>();
        private Map<String, Set<String>> m_gradesForSectionsDRecords = new HashMap<>();

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public MasterScheduleEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Override getBean() to return alternate types of beans for different
         * rows. Staff type rows should return a staff record. Student schedule
         * rows should return a student schedule record. The default is to
         * return the super
         *
         * @return X 2 base bean
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getBean()
         */
        @Override
        public X2BaseBean getBean() {
            X2BaseBean bean = super.getBean();
            if ((getCurrentRow() >= 0) &&
                    (getCurrentRow() < m_elements.size()) &&
                    (m_elements.get(getCurrentRow()) != null)) {
                bean = m_elements.get(getCurrentRow());
            }
            return bean;
        }

        /**
         * Specify the definition Id for the current row based on getCurrentRow().
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#
         *      getCurrentFormatDefinitionId()
         */
        @Override
        public String getCurrentFormatDefinitionId() {
            String definitionId = null;
            int rownum = getCurrentRow();
            if ((rownum >= 0) && (rownum < m_definitionId.size())) {
                definitionId = m_definitionId.get(rownum);
            }
            return definitionId;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            X2BaseBean bean = getBean();
            String definitionId = getCurrentFormatDefinitionId();
            String name = (definitionId == null ? "B" : definitionId) + ": ";
            if (definitionId == null) {
                // Teaching staff "B"
                SisStaff staff = (SisStaff) bean;
                String schoolName = staff.getSchool().getName();
                name += "School Name: " + schoolName + ", Staff - " + staff.getNameView() + " [Provider ID: " +
                        staff.getLocalId() + "]";
            } else if (definitionId.equals("C")) {
                // Master Schedule (section)
                MasterSchedule section = (MasterSchedule) bean;
                String schoolName = section.getSchoolCourse().getSchool().getName();
                name += "School Name: " + schoolName + ", Master Section - " + section.getCourseView();
            } else if (definitionId.equals("D")) {
                // Schedule Master Teacher (teacher Schedule)
                ScheduleTeacher teacherSchedule = (ScheduleTeacher) bean;
                String schoolName = teacherSchedule.getStaff().getSchool().getName();
                name += "School Name: " + schoolName + ", Teacher Schedule - "
                        + teacherSchedule.getStaff().getNameView()
                        + " [Provider ID: " + teacherSchedule.getStaff().getLocalId() + "] "
                        + teacherSchedule.getSection().getCourseView();
            } else if (definitionId.equals("E")) {
                // Administrative/support staff "E"
                SisStaff staff = (SisStaff) bean;
                String schoolName = staff.getSchool().getName();
                name += "School Name: " + schoolName + ", Staff - " + staff.getNameView() + " [Provider ID: " +
                        staff.getLocalId() + "]";
            } else if (definitionId.equals("F")) {
                // Student Schedule "F"
                StudentSchedule schedule = (StudentSchedule) bean;
                String schoolName = schedule.getSection().getSchoolCourse().getSchool().getName();
                name += "School Name: " + schoolName + ", Student Schedule - "
                        + schedule.getStudent().getNameView()
                        + " [Testing ID: " + schedule.getStudent().getStateId()
                        + ", Local ID: " + schedule.getStudent().getLocalId()
                        + "] " + schedule.getSection().getCourseView();
            } else if (definitionId.equals("G")) {
                // Staff Position "G"
                StaffPosition position = (StaffPosition) bean;
                String schoolName = "";
                if (position != null && position.getStaff() != null && position.getStaff().getSchool() != null) {
                    schoolName = position.getStaff().getSchool().getName();
                    name += "School Name: " + schoolName + ", Staff - " + position.getStaff().getNameView()
                            + " [Provider ID: " + position.getStaff().getLocalId()
                            + "] " + position.getJobCode();
                }
            } else if (definitionId.equals("I")) {
                MasterSchedule section = (MasterSchedule) bean;
                String schoolName = section.getSchoolCourse().getSchool().getName();
                name += "School Name: " + schoolName + ", Master Section - " + section.getCourseView();
            } else if (definitionId.equals("J")) {
                SisStaff staff = (SisStaff) bean;
                String schoolName = staff.getSchool().getName();
                name += "School Name: " + schoolName + ", Staff - " + staff.getNameView() + " [Provider ID: " +
                        staff.getLocalId() + "]";
            }
            return name;
        }

        /**
         * Initialize the entity. Find all required data, count required rows
         * and row types.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_data = (MasterScheduleCollection) data;
            /*
             * The sections parameter contains the sections of the export to
             * include. This can include a combination of: B,C,D,E,F,G,H Each
             * letter determines the inclusion of this section of data.
             */
            String sections = (String) data.getParameter(PARAM_SECTION);
            // List of report beans and export format ids.
            m_definitionId = new ArrayList<String>();
            m_elements = new ArrayList<X2BaseBean>();
            /*
             * Preload a collection of section OIDs that will be reported in "F" records.
             * This is a hackish workaround to ensure that we only include sections
             * which have students enrolled in them as-of report date in "C" and "D" records.
             */
            if (sections.contains("B") || sections.contains("C") || sections.contains("D")) {
                loadFRecordSectionOids();
            }
            // Load B records for teaching staff.
            if (sections.contains("B")) {
                loadTeachingStaff();
            }
            // Load C records for master sections.
            if (sections.contains("C")) {
                loadMasterSections("C");
            }
            // Load D records for teacher schedule.
            if (sections.contains("D")) {
                loadTeacherSchedule("D", true);
            }
            // Load E records for other provider staff.
            if (sections.contains("E")) {
                loadProviderStaff();
            }
            // Load F records for student schedules.
            if (sections.contains("F")) {
                loadStudentSchedule("F");
            }
            // Load G records for staff positions.
            if (sections.contains("G")) {
                loadStaffPositions();
            }
            // Load I records for master sections (connected sections).
            if (sections.contains("I")) {
                loadConnectedSections("I");
            }
            // Load J records for co-op provider staff.
            if (sections.contains("J")) {
                loadCoopProviderStaff();
            }
            // Load K records for master sections (connected sections, filtered by
            // interdisciplinary).
            if (sections.contains("K")) {
                loadConnectedSections("K");
            }
            // Set row counts.
            m_data.m_rowCount += m_definitionId.size();
            setRowCount(m_definitionId.size());
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }

        /**
         * Builds a criteria for staff members to include.
         *
         * @param mscData MasterScheduleCollection
         * @return X2Criteria
         */
        private X2Criteria getTeachingStaffCriteria(MasterScheduleCollection mscData) {
            X2Criteria criteria = new X2Criteria();
            // Check user exclude indicator.
            if (mscData.m_fieldExcludeStf != null) {
                criteria.addNotEqualTo(mscData.m_fieldExcludeStf, BooleanAsStringConverter.TRUE);
            }
            // Check school selection.
            if (getData().isSchoolContext()) {
                criteria.addEqualTo(Staff.COL_SCHOOL_OID, getData().getSchool().getOid());
            } else {
                criteria.addNotEqualTo(Staff.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                        Boolean.TRUE);
                criteria.addNotEqualTo(Staff.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                        Boolean.TRUE);
            }
            criteria.addLessOrEqualThan(Staff.COL_HIRE_DATE, mscData.m_reportDate);
            return criteria;
        }

        /**
         * Returns a criteria for staff position records to include.
         *
         * @param mscData MasterScheduleCollection
         * @return X2Criteria
         */
        private X2Criteria getStaffPositionsCriteria(MasterScheduleCollection mscData) {
            String staffActiveStatus = PreferenceManager.getPreferenceValue(mscData.getOrganization(),
                    SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StaffPosition.REL_STAFF + PATH_DELIMITER + Staff.COL_STATUS, staffActiveStatus);
            criteria.addIn(StaffPosition.REL_STAFF + PATH_DELIMITER + Staff.COL_STAFF_TYPE,
                    mscData.m_adminStaffCodesList);
            criteria.addLessOrEqualThan(StaffPosition.COL_START_DATE, mscData.m_reportDate);
            criteria.addLessOrEqualThan(StaffPosition.REL_STAFF + PATH_DELIMITER + Staff.COL_HIRE_DATE,
                    mscData.m_reportDate);
            X2Criteria criteria1 = new X2Criteria();
            X2Criteria criteria2 = new X2Criteria();
            criteria1.addEmpty(StaffPosition.COL_END_DATE, mscData.getBroker().getPersistenceKey());
            criteria2.addGreaterOrEqualThan(StaffPosition.COL_END_DATE, mscData.m_reportDate);
            criteria1.addOrCriteria(criteria2);
            criteria.addAndCriteria(criteria1);
            // Check user exclude indicator.
            if (mscData.m_fieldExcludeStf != null) {
                criteria.addNotEqualTo(StaffPosition.REL_STAFF + PATH_DELIMITER + mscData.m_fieldExcludeStf,
                        BooleanAsStringConverter.TRUE);
            }
            // Check school selection.
            if (getData().isSchoolContext()) {
                criteria.addEqualTo(StaffPosition.COL_SCHOOL_OID, getData().getSchool().getOid());
            } else {
                criteria.addNotEqualTo(StaffPosition.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                        Boolean.TRUE);
                criteria.addNotEqualTo(StaffPosition.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                        Boolean.TRUE);
            }
            return criteria;
        }

        /**
         * Checks if is teacher included.
         *
         * @param staff Staff
         * @param teacher ScheduleTeacher
         * @return true, if is teacher included
         */
        private boolean isTeacherIncluded(Staff staff, ScheduleTeacher teacher) {
            boolean value = true;
            if (teacher != null && "1".equals(teacher.getFieldValueByAlias(ALIAS_EXCLUDE_MTC))) {
                value = false;
            } else if (staff == null || (staff.getHireDate() != null && staff.getHireDate().after(m_data.m_reportDate))
                    || "1".equals(staff.getFieldValueByBeanPath(m_data.m_fieldExcludeStf))) {
                value = false;
            }
            return value;
        }

        /**
         * Loads 'connected' section pairs into the element list for type "I" and "K" records.
         */
        private void loadConnectedSections(String recordType) {
            MasterScheduleCollection mscData = (MasterScheduleCollection) getData();
            Iterator iterator = mscData.m_connectedSections.keySet().iterator();
            while (iterator.hasNext()) {
                MasterSchedule connectedSection = (MasterSchedule) iterator.next();
                boolean isInterdisciplinary = BooleanAsStringConverter.TRUE
                        .equals(connectedSection.getFieldValueByBeanPath(mscData.m_fieldMTCisInterdisciplinary));

                if (mscData.m_gradesForSectionsIRecords.containsKey(connectedSection.getOid())) {
                    Set<String> grades = mscData.m_gradesForSectionsIRecords.get(connectedSection.getOid());
                    if (grades.size() > 0) {
                        for (int i = 0; i < grades.size(); ++i) {
                            if (!recordType.equals("K") || (recordType.equals("K") && isInterdisciplinary)) {
                                m_definitionId.add(recordType);
                                m_elements.add(connectedSection);
                            }
                        }
                    }
                } else {
                    if (!recordType.equals("K") || (recordType.equals("K") && isInterdisciplinary)) {
                        m_definitionId.add(recordType);
                        m_elements.add(connectedSection);
                    }
                }
            }
        }

        /**
         * Adds staff with co-op counts to the elements list for "J" type records.
         */
        private void loadCoopProviderStaff() {
            MasterScheduleCollection mscData = (MasterScheduleCollection) getData();
            String staffActiveCode =
                    PreferenceManager.getPreferenceValue(mscData.getOrganization(),
                            SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SisStaff.COL_STATUS, staffActiveCode);
            criteria.addLessOrEqualThan(SisStaff.COL_HIRE_DATE, mscData.m_reportDate);
            // Check user exclude indicator.
            if (mscData.m_fieldExcludeStf != null) {
                criteria.addNotEqualTo(mscData.m_fieldExcludeStf, BooleanAsStringConverter.TRUE);
            }
            // Check school selection.
            if (getData().isSchoolContext()) {
                criteria.addEqualTo(Staff.COL_SCHOOL_OID, getData().getSchool().getOid());
            } else {
                criteria.addNotEqualTo(Staff.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                        Boolean.TRUE);
                criteria.addNotEqualTo(Staff.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                        Boolean.TRUE);
            }
            QueryByCriteria query = new QueryByCriteria(SisStaff.class, criteria);
            // query.addOrderByAscending(SisStaff.COL_LOCAL_ID);
            mscData.applyInputSort(query, null);
            QueryIterator iterator = mscData.getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    SisStaff staff = (SisStaff) iterator.next();
                    if (mscData.m_staffCoopCounts.get(staff.getLocalId()) != null) {
                        m_definitionId.add("J");
                        m_elements.add(staff);
                    }
                }
            } finally {
                iterator.close();
            }
        }

        /**
         * Loads a collection of section OIDs which will be reported in "F" records. This is used
         * to determine whether a section will need to be included in "C" or "D" records.
         * This method is also ensures that only sections that have students enrolled as of report
         * date
         * are being included in "C" and "D" records.
         */
        private void loadFRecordSectionOids() {
            MasterScheduleCollection mscData = (MasterScheduleCollection) getData();
            for (Map<String, ScheduleInfo> scheduleInfos : mscData.m_scheduleInfoMap.values()) {
                mscData.m_FRecordSectionOids.addAll(scheduleInfos.keySet());
            }
        }

        /**
         * Load master sections into the elements list for type C records.
         *
         * @param element String
         */
        private void loadMasterSections(String element) {
            MasterScheduleCollection mscData = (MasterScheduleCollection) getData();
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE,
                    SchoolCourse.MASTER_TYPE_CLASS);
            criteria.addEqualToField(MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER + SisSchool.REL_ACTIVE_SCHOOL_SCHED +
                    PATH_DELIMITER +
                    SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, MasterSchedule.COL_SCHEDULE_OID);
            // Check user exclude indicators.
            if (mscData.m_fieldExcludeCrs != null) {
                criteria.addNotEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE +
                        PATH_DELIMITER + mscData.m_fieldExcludeCrs, BooleanAsStringConverter.TRUE);
            }
            if (mscData.m_fieldExcludeMst != null) {
                criteria.addNotEqualTo(mscData.m_fieldExcludeMst, BooleanAsStringConverter.TRUE);
            }
            // Check the require subject code option.
            if ((mscData.m_paramRequireSubject != null) && mscData.m_paramRequireSubject.booleanValue()) {
                criteria.addNotEmpty(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE +
                        PATH_DELIMITER + mscData.m_fieldScedCourse, mscData.getBroker().getPersistenceKey());
            }
            // Check school selection.
            if (getData().isSchoolContext()) {
                criteria.addEqualTo(MasterSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID,
                        getData().getSchool().getOid());
            } else {
                criteria.addNotEqualTo(
                        MasterSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER
                                + SisSchool.COL_INACTIVE_INDICATOR,
                        Boolean.TRUE);
                criteria.addNotEqualTo(
                        MasterSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER
                                + SisSchool.COL_ARCHIVE_INDICATOR,
                        Boolean.TRUE);
            }
            QueryByCriteria query = new QueryByCriteria(MasterSchedule.class, criteria);
            mscData.applyInputSort(query, MasterSchedule.REL_PRIMARY_STAFF);
            QueryIterator iterator = mscData.getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    MasterSchedule section = (MasterSchedule) iterator.next();
                    boolean includeSection = mscData.isIncludeSection(section) &&
                            mscData.m_FRecordSectionOids.contains(section.getOid());
                    // Check the require students option.
                    if ((mscData.m_paramRequireStudents != null) && mscData.m_paramRequireStudents.booleanValue() &&
                            includeSection) {
                        if (mscData.m_sectionsWithStudents.contains(section.getOid())) {
                            if (mscData.m_refTableMultiGradeCrossSplittedCodes
                                    .contains(section.getSchoolCourse().getCourse().getNumber())) {
                                Collection<StudentSchedule> sscsForSection =
                                        mscData.m_studentScheduleMapBySection.get(section.getOid());
                                if (sscsForSection != null) {
                                    Set<String> grades = m_gradesForSectionsCRecords.get(section.getOid());
                                    if (grades == null) {
                                        grades = new HashSet<String>();
                                        m_gradesForSectionsCRecords.put(section.getOid(), grades);
                                    }
                                    for (StudentSchedule ssc : sscsForSection) {
                                        if (!grades.contains(ssc.getStudent().getGradeLevel())) {
                                            grades.add(ssc.getStudent().getGradeLevel());
                                            m_definitionId.add(element);
                                            m_elements.add(section);
                                        }
                                    }
                                }
                            } else {
                                m_definitionId.add(element);
                                m_elements.add(section);
                            }
                        }
                    } else if (includeSection) {
                        m_definitionId.add(element);
                        m_elements.add(section);
                    }
                }
            } finally {
                iterator.close();
            }
        }

        /**
         * Load other teaching service provider staff into elements list for type E records.
         */
        private void loadProviderStaff() {
            // Lookup preferences.
            MasterScheduleCollection mscData = (MasterScheduleCollection) getData();

            String staffActiveStatus = PreferenceManager.getPreferenceValue(mscData.getOrganization(),
                    SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(Staff.COL_STATUS, staffActiveStatus);
            criteria.addLessOrEqualThan(Staff.COL_HIRE_DATE, mscData.m_reportDate);
            criteria.addNotEmpty(mscData.m_fieldStaffProviderType, mscData.getBroker().getPersistenceKey());
            // Check user exclude indicator.
            if (mscData.m_fieldExcludeStf != null) {
                criteria.addNotEqualTo(mscData.m_fieldExcludeStf, BooleanAsStringConverter.TRUE);
            }
            // Check school selection.
            if (getData().isSchoolContext()) {
                criteria.addEqualTo(Staff.COL_SCHOOL_OID, getData().getSchool().getOid());
            } else {
                criteria.addNotEqualTo(Staff.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                        Boolean.TRUE);
                criteria.addNotEqualTo(Staff.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                        Boolean.TRUE);
            }
            QueryByCriteria query = new QueryByCriteria(Staff.class, criteria);
            mscData.applyInputSort(query, null);
            QueryIterator iterator = mscData.getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    m_definitionId.add("E");
                    m_elements.add((X2BaseBean) iterator.next());
                }
            } finally {
                iterator.close();
            }
        }

        /**
         * Load Staff Positions as elements for type G records.
         */
        private void loadStaffPositions() {
            // Lookup preferences.
            MasterScheduleCollection mscData = (MasterScheduleCollection) getData();
            String staffActiveStatus = PreferenceManager.getPreferenceValue(mscData.getOrganization(),
                    SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StaffPosition.REL_STAFF + PATH_DELIMITER + Staff.COL_STATUS, staffActiveStatus);
            criteria.addIn(StaffPosition.REL_STAFF + PATH_DELIMITER + Staff.COL_STAFF_TYPE,
                    mscData.m_adminStaffCodesList);
            criteria.addLessOrEqualThan(StaffPosition.COL_START_DATE, mscData.m_reportDate);
            X2Criteria criteria1 = new X2Criteria();
            X2Criteria criteria2 = new X2Criteria();
            criteria1.addEmpty(StaffPosition.COL_END_DATE, mscData.getBroker().getPersistenceKey());
            criteria2.addGreaterOrEqualThan(StaffPosition.COL_END_DATE, mscData.m_reportDate);
            criteria1.addOrCriteria(criteria2);
            criteria.addAndCriteria(criteria1);
            // Check user exclude indicator.
            if (mscData.m_fieldExcludeStf != null) {
                criteria.addNotEqualTo(StaffPosition.REL_STAFF + PATH_DELIMITER + mscData.m_fieldExcludeStf,
                        BooleanAsStringConverter.TRUE);
            }
            // Check school selection.
            if (getData().isSchoolContext()) {
                criteria.addEqualTo(StaffPosition.COL_SCHOOL_OID, getData().getSchool().getOid());
            } else {
                criteria.addNotEqualTo(StaffPosition.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                        Boolean.TRUE);
                criteria.addNotEqualTo(StaffPosition.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                        Boolean.TRUE);
            }
            QueryByCriteria query = new QueryByCriteria(StaffPosition.class, criteria);
            mscData.applyInputSort(query, StaffPosition.REL_STAFF);
            QueryIterator iterator = mscData.getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    m_definitionId.add("G");
                    m_elements.add((X2BaseBean) iterator.next());
                }
            } finally {
                iterator.close();
            }
        }

        /**
         * Load student schedules, along with transcript, into the elements list for C type records.
         * Load in two parts: 1. Identify all historic student schedules through Student Schedule
         * and
         * Student Schedule Change. 2. Identify all transcript records this year.
         * 3. Put the accumulation of one or both into a list for the entity.
         * 4. Construct artificial StudentSchedule for X2BaseBean if necessary, but keep
         * related schedule and transcript info in the Entity.
         *
         * @param definitionID String
         */
        private void loadStudentSchedule(String definitionID) {
            MasterScheduleCollection mscData = (MasterScheduleCollection) getData();
            for (Map<String, ScheduleInfo> scheduleInfos : mscData.m_scheduleInfoMap.values()) {
                for (ScheduleInfo info : scheduleInfos.values()) {
                    Collection<ScheduleTeacher> teachers =
                            getCorrectTeacherSections(info.m_schedule.getSection().getTeacherSections());
                    for (ScheduleTeacher teacher : teachers) {
                        Staff staff = m_data.m_staffOid.get(teacher.getStaffOid());
                        if (isTeacherIncluded(staff, teacher)) {
                            String role = teacher.getRole();
                            if ("1".equals(role) || (role == null)) {
                                info.m_scheduleTeacherList.add(teacher.getStaff().getLocalId());
                                m_definitionId.add(definitionID);
                                X2BaseBean copyBean = info.m_schedule.copyBean();
                                m_elements.add(copyBean);
                            }
                        }
                    }
                    if (!StringUtils.isEmpty((String) info.m_schedule.getFieldValueByAlias(ALIAS_TEACHER_ONE))) {
                        String localId =
                                mscData.m_teacherCodeMap.get(info.m_schedule.getFieldValueByAlias(ALIAS_TEACHER_ONE));
                        if (isTeacherIncluded(mscData.m_staffLocalId.get(localId), null)
                                && !info.m_scheduleTeacherList.contains(localId)) {
                            info.m_scheduleTeacherList.add(localId);
                            m_definitionId.add(definitionID);
                            X2BaseBean copyBean = info.m_schedule.copyBean();
                            m_elements.add(copyBean);
                        }
                    }
                    if (!StringUtils.isEmpty((String) info.m_schedule.getFieldValueByAlias(ALIAS_TEACHER_TWO))) {
                        String localId =
                                mscData.m_teacherCodeMap.get(info.m_schedule.getFieldValueByAlias(ALIAS_TEACHER_TWO));
                        if (isTeacherIncluded(mscData.m_staffLocalId.get(localId), null)
                                && !info.m_scheduleTeacherList.contains(localId)) {
                            info.m_scheduleTeacherList.add(localId);
                            m_definitionId.add(definitionID);
                            X2BaseBean copyBean = info.m_schedule.copyBean();
                            m_elements.add(copyBean);
                        }
                    }
                }
            }
        }

        /**
         * Load teaching staff into the elements list for B type records.
         *
         * @throws X2BaseException exception
         */
        private void loadTeachingStaff() throws X2BaseException {
            Set<String> BRecordStaffOIDs = new HashSet<String>();
            // Load a list of staffOid that have reportable teacher schedules.
            loadTeacherSchedule(null, false);
            // Lookup preferences.
            MasterScheduleCollection mscData = (MasterScheduleCollection) getData();
            // Initialize criteria, form query and get iterator
            X2Criteria teachingStaffCriteria = getTeachingStaffCriteria(mscData);
            QueryByCriteria teachingStaffQuery = new QueryByCriteria(Staff.class, teachingStaffCriteria);
            mscData.applyInputSort(teachingStaffQuery, null);
            QueryIterator teachingStaffIterator = mscData.getBroker().getIteratorByQuery(teachingStaffQuery);
            try {
                while (teachingStaffIterator.hasNext()) {
                    Staff staff = (Staff) teachingStaffIterator.next();
                    String providerType = (String) staff.getFieldValueByBeanPath(mscData.m_fieldStaffProviderType);
                    providerType = getData().lookupReferenceCodeByBeanPath(Staff.class,
                            mscData.m_fieldStaffProviderType,
                            providerType,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                    if (m_staffWithSchedules.contains(staff.getOid()) && !BRecordStaffOIDs.contains(staff.getOid()) &&
                            ("4".equals(providerType) || "6".equals(providerType)
                                    || StringUtils.isEmpty(providerType))) {
                        m_definitionId.add(null); // 'B' record.
                        m_elements.add(staff);
                        BRecordStaffOIDs.add(staff.getOid());
                    }
                }
            } finally {
                teachingStaffIterator.close();
            }
            // Initialize criteria, form query and get iterator
            X2Criteria staffPositionsCriteria = getStaffPositionsCriteria(mscData);
            SubQuery staffPositionsSubQuery =
                    new SubQuery(StaffPosition.class, StaffPosition.COL_STAFF_OID, staffPositionsCriteria);
            X2Criteria staffPostionsInCriteria = new X2Criteria();
            staffPostionsInCriteria.addIn(X2BaseBean.COL_OID, staffPositionsSubQuery);
            QueryByCriteria staffPostionsQuery = new QueryByCriteria(Staff.class, staffPostionsInCriteria);
            QueryIterator staffPostionsIterator = mscData.getBroker().getIteratorByQuery(staffPostionsQuery);
            try {
                while (staffPostionsIterator.hasNext()) {
                    Staff staff = (Staff) staffPostionsIterator.next();
                    String providerType = (String) staff.getFieldValueByBeanPath(mscData.m_fieldStaffProviderType);
                    providerType = getData().lookupReferenceCodeByBeanPath(Staff.class,
                            mscData.m_fieldStaffProviderType,
                            providerType,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                    if (!BRecordStaffOIDs.contains(staff.getOid())) {
                        m_definitionId.add(null); // 'B' record.
                        m_elements.add(staff);
                        BRecordStaffOIDs.add(staff.getOid());
                    }
                }
            } finally {
                staffPostionsIterator.close();
            }
            // Load staff race codes for the race retriever.
            mscData.loadRaceCodes(teachingStaffCriteria);
            mscData.loadRaceCodes(staffPostionsInCriteria);
        }

        /**
         * Load teacher schedules into elements list for type D records. <br>
         * This method can be called at two times. <br>
         * The first time is to identify which staffOid have schedules so that
         * they may be reported in type B teaching staff. <br>
         * The second time to actually put the records into the elements list.
         * <p>
         * Build the list the first time into temporary fields. <br>
         * The second time, copy those fields to m_elements.
         *
         * @param rowType String
         * @param addToElements boolean
         * @throws X2BaseException exception
         */
        private void loadTeacherSchedule(String rowType, boolean addToElements) throws X2BaseException {
            if (m_staffWithSchedules == null || addToElements) {
                m_staffWithSchedules = new HashSet<String>();
                m_elementsTemp = new ArrayList<X2BaseBean>();
                MasterScheduleCollection mscData = (MasterScheduleCollection) getData();
                X2Criteria criteria = new X2Criteria();
                criteria.addLessOrEqualThan(ScheduleTeacher.REL_STAFF + PATH_DELIMITER + Staff.COL_HIRE_DATE,
                        mscData.m_reportDate);
                criteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE +
                        PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);
                criteria.addEqualToField(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHEDULE +
                        PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED +
                        PATH_DELIMITER + SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                        MasterSchedule.COL_SCHEDULE_OID);
                // Check user exclude indicators.
                if (mscData.m_fieldExcludeStf != null) {
                    criteria.addNotEqualTo(ScheduleTeacher.REL_STAFF + PATH_DELIMITER + mscData.m_fieldExcludeStf,
                            BooleanAsStringConverter.TRUE);
                }
                if (mscData.m_fieldExcludeCrs != null) {
                    criteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                            MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE +
                            PATH_DELIMITER +
                            mscData.m_fieldExcludeCrs, BooleanAsStringConverter.TRUE);
                }
                if (mscData.m_fieldExcludeMst != null) {
                    criteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + mscData.m_fieldExcludeMst,
                            BooleanAsStringConverter.TRUE);
                }
                if (mscData.m_fieldExcludeMtc != null) {
                    criteria.addNotEqualTo(mscData.m_fieldExcludeMtc, BooleanAsStringConverter.TRUE);
                }
                // Check the require subject code option.
                if ((mscData.m_paramRequireSubject != null) && mscData.m_paramRequireSubject.booleanValue()) {
                    criteria.addNotEmpty(ScheduleTeacher.REL_SECTION + PATH_DELIMITER
                            + MasterSchedule.REL_SCHOOL_COURSE
                            + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER +
                            mscData.m_fieldScedCourse,
                            mscData.getBroker().getPersistenceKey());
                }
                // Check school selection.
                if (getData().isSchoolContext()) {
                    criteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHEDULE +
                            PATH_DELIMITER + Schedule.COL_SCHOOL_OID, getData().getSchool().getOid());
                } else {
                    criteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHEDULE +
                            PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER +
                            SisSchool.COL_INACTIVE_INDICATOR,
                            Boolean.TRUE);
                    criteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHEDULE +
                            PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER +
                            SisSchool.COL_ARCHIVE_INDICATOR,
                            Boolean.TRUE);
                }
                QueryByCriteria query = new QueryByCriteria(ScheduleTeacher.class, criteria);
                mscData.applyInputSort(query, ScheduleTeacher.REL_STAFF);
                QueryIterator iterator = mscData.getBroker().getIteratorByQuery(query);
                // Need to avoid multiple processing of one section
                try {
                    while (iterator.hasNext()) {
                        ScheduleTeacher scheduleTeacher = (ScheduleTeacher) iterator.next();
                        PlainDate startDate = (PlainDate) getData().getPropertyAsJavaType(scheduleTeacher,
                                mscData.m_fieldMTCStartDate);
                        PlainDate endDate = (PlainDate) getData().getPropertyAsJavaType(scheduleTeacher,
                                mscData.m_fieldMTCEndDate);
                        boolean includeSection = mscData.m_FRecordSectionOids.contains(scheduleTeacher.getSectionOid());
                        if ((startDate == null || !startDate.after(mscData.m_reportDate)) &&
                                (endDate == null || !endDate.before(mscData.m_reportDate)) && includeSection) {
                            if ((mscData.m_paramRequireStudents != null)
                                    && mscData.m_paramRequireStudents.booleanValue()) {
                                MasterSchedule section = scheduleTeacher.getSection();
                                if (mscData.m_refTableMultiGradeCrossSplittedCodes
                                        .contains(scheduleTeacher.getSection().getSchoolCourse().getCourse()
                                                .getNumber())) {
                                    Collection<StudentSchedule> sscsForSection =
                                            mscData.m_studentScheduleMapBySection.get(section.getOid());
                                    if (sscsForSection != null) {
                                        Set<String> grades = m_gradesForSectionsDRecords.get(section.getOid());
                                        if (grades == null) {
                                            grades = new HashSet<String>();
                                            m_gradesForSectionsDRecords.put(section.getOid(), grades);
                                        }
                                        for (StudentSchedule ssc : sscsForSection) {
                                            if (mscData.m_sectionsWithStudents.contains(scheduleTeacher.getSectionOid())
                                                    && !grades.contains(ssc.getStudent().getGradeLevel())) {
                                                grades.add(ssc.getStudent().getGradeLevel());
                                                m_elementsTemp.add(scheduleTeacher);
                                                m_staffWithSchedules.add(scheduleTeacher.getStaffOid());
                                            }
                                        }
                                    }
                                } else if (mscData.m_sectionsWithStudents.contains(scheduleTeacher.getSectionOid())) {
                                    m_elementsTemp.add(scheduleTeacher);
                                    m_staffWithSchedules.add(scheduleTeacher.getStaffOid());
                                }
                            } else {
                                m_elementsTemp.add(scheduleTeacher);
                                m_staffWithSchedules.add(scheduleTeacher.getStaffOid());
                            }
                        }
                    }
                } finally {
                    iterator.close();
                }
            }
            // Write the results into the elements list.
            if (addToElements) {
                for (int i = 0; i < m_elementsTemp.size(); i++) {
                    m_definitionId.add(rowType);
                    m_elements.add(m_elementsTemp.get(i));
                }
            }
        }
    }

    /*
     * Alias names.
     */
    private static final String ALIAS_ANNIVERSARY_DATE = "all-sfp-AnniversaryDate";
    private static final String ALIAS_COURSE_MINUTES = "DOE VA COURSE MINUTES";
    private static final String ALIAS_CRS_COURSE_MINUTES = "all-crs-MinutePerCourseOveride";
    private static final String ALIAS_CRS_MOPID = "all-crs-MOPID";
    private static final String ALIAS_CSK_COURSE_MINUTES = "all-csk-MinutePerCourseSchoolOveride";
    private static final String ALIAS_DEFINED_CLASS_TYPE = "DOE CLASS TYPE";
    private static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    private static final String ALIAS_ENR_EXCLUDE_ENR = "DOE EXCLUDE ENR";
    private static final String ALIAS_EXCLUDE_CRS = "DOE EXCLUDE CRS";
    private static final String ALIAS_EXCLUDE_MST = "DOE EXCLUDE MST";
    private static final String ALIAS_EXCLUDE_MST_FROM_CONNECTED = "DOE EXCLUDE MST FROM CONNECTED";
    private static final String ALIAS_EXCLUDE_MTC = "DOE EXCLUDE MTC";
    private static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    private static final String ALIAS_EXCLUDE_STF = "DOE EXCLUDE STF";
    private static final String ALIAS_MST_COURSE_MINUTES = "all-mst-MinutePerCourseMasterOveride";
    private static final String ALIAS_MST_INTERDISCIPLINARY = "all-mst-isInterdisciplinary";
    private static final String ALIAS_MTC_END_DATE = "DOE TEACHER END DATE";
    private static final String ALIAS_MTC_START_DATE = "DOE TEACHER START DATE";
    private static final String ALIAS_PRIMARY_DISABILITY = "DOE SPED DISABILITY";
    private static final String ALIAS_RCD_COURSE_MINUTES = "all-rcd-MinutesPerCourseTerm";
    private static final String ALIAS_SCED_COURSE = "DOE SCED COURSE";
    private static final String ALIAS_SCED_SUBJECT = "DOE SCED SUBJECT";
    private static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    private static final String ALIAS_SEDF_REPORT_FLAG = "SEDF REPORT FLAG";
    private static final String ALIAS_SEQUENCED_COURSE = "DOE SEQUENCED COURSE";
    private static final String ALIAS_STAFF_PROVIDER_TYPE = "DOE STAFF PROVIDER TYPE";
    private static final String ALIAS_TEACHER_TWO = "DOE TEACHER TWO";
    private static final String ALIAS_TEACHER_ONE = "DOE TEACHER ONE";
    private static final String ALIAS_WORK_BASED_LEARNING_CODE = "DOE WORK-BASED LEARNING CODE";

    /*
     * Tool input parameters.
     */
    private static final String PARAM_EOY_BEHAVIOR = "eoyBehavior";
    private static final String PARAM_REPORT_DATE = "reportDate";
    private static final String PARAM_REQUIRE_STUDENT = "requireStudents";
    private static final String PARAM_REQUIRE_SUBJECT = "requireSubject";
    private static final String PARAM_SECTION = "section";
    private static final String PARAM_SUBMISSION = "submission";

    /*
     * Other constants
     */
    protected static final String COOP_CODE = "1";
    protected static final String DEFAULT_CALENDAR_NAME = "Standard";
    protected static final String RTB_NAME_MULTI_GRADE_CROSS = "MultiGradeCrossReference";

    /*
     * Retriever parameter keys.
     */
    // Staff certificate
    private static final String PARAM_CERT_NUMBER = "NUMBER";
    private static final String PARAM_CERT_PREFIX = "PREFIX";
    private static final String PARAM_COOP_AVG_MINS = "AVERAGE MINUTES";
    private static final String PARAM_COOP_COUNT = "STUDENT COUNT";
    private static final String PARAM_COURSE_DIVISION = "DIVISION";
    private static final String PARAM_COURSE_SCHOOL = "SCHOOL";

    /*
     * Export fields
     */
    private static final String EXPORT_FIELD_C_RECORD_TYPE = "Record Type";
    private static final String EXPORT_FIELD_C_SECTION_ID = "Section Id";

    /*
     * Local variables.
     */
    protected ArrayList<String> m_adminStaffCodesList;
    protected Map<String, StaffCertification> m_certificates;
    protected Map<MasterSchedule, MasterSchedule> m_connectedSections =
            new LinkedHashMap<MasterSchedule, MasterSchedule>();
    protected DateAsStringConverter m_dateAsStringConverter;
    protected String m_districtId;
    protected EnrollmentManager m_enrollmentManager;
    protected String m_eoyBehavior;
    protected String m_fieldAdditionalStaff1;
    protected String m_fieldAdditionalStaff2;
    protected String m_fieldAnniversaryDate;
    protected String m_fieldDistrictId;
    protected String m_fieldDefinedClassType;
    protected String m_fieldEnrExcludeEnr;
    protected String m_fieldExcludeCrs;
    protected String m_fieldExcludeMst;
    protected String m_fieldExcludeMstFromConnected;
    protected String m_fieldExcludeMtc;
    protected String m_fieldExcludeStd;
    protected String m_fieldExcludeStf;
    protected String m_fieldCrsMopId;
    protected String m_fieldScedCourse;
    protected String m_fieldScedSubject;
    protected String m_fieldSequencedCourse;
    protected String m_fieldSchoolId;
    protected String m_fieldStaffProviderType;
    protected String m_fieldMTCStartDate;
    protected String m_fieldMTCEndDate;
    protected String m_fieldMTCisInterdisciplinary;
    protected String m_fieldPrimaryDisability;
    protected String m_fieldWorkBasedLearningCode;
    protected Collection<String> m_FRecordSectionOids;
    protected Map<String, Set<String>> m_gradesForSectionsIRecords = new HashMap<>();
    protected Map<String, String> m_maxGradesForSectionsIRecords = new HashMap<>();
    protected Map<String, String> m_maxGradesForSectionMap = new HashMap<>();
    protected HashMap<String, Integer> m_numericGradeLevelMap;
    protected String m_javanameSedfReport;
    protected Boolean m_paramRequireStudents;
    protected Boolean m_paramRequireSubject;
    protected Map<String, Collection<Race>> m_raceCodeMap = new HashMap<String, Collection<Race>>();
    protected Map<String, ReferenceCode> m_refTableMultiGradeCrossCodes;
    protected Set<String> m_refTableMultiGradeCrossSplittedCodes = new HashSet<>();
    protected PlainDate m_reportDate;
    protected Date m_reportDate1YearBefore;
    protected int m_rowCount;
    protected Map<String, ArrayList<String>> m_sectionIdMap = new HashMap<String, ArrayList<String>>();
    protected Map<String, ReferenceCode> m_raceCodes;
    protected Map<String, List<StudentScheduleChange>> m_scheduleChangeMap;
    protected Map<String, Map<String, ScheduleInfo>> m_scheduleInfoMap;
    protected Map<String, String> m_schoolIdAliasBySchoolCourseOid = new HashMap<String, String>();
    protected Map<String, String> m_schoolIdsBySchoolCourseOid = new HashMap<String, String>();
    protected Map<String, Staff> m_staffByScheduleTeacher = new HashMap<String, Staff>();
    protected int m_schoolYear;
    protected Map<String, ArrayList<String>> m_sectionsWithMultipleTeachers;
    protected Set<String> m_sectionsWithStudents;
    protected Map<String, List<PlainDate>> m_sessionDatesBySeciton = new HashMap<>();
    protected ScheduleManager m_scheduleManager;
    protected Map<String, Integer> m_staffCoopCounts = new HashMap<String, Integer>();
    protected Map<String, Integer> m_staffCoopSectionCounts = new HashMap<String, Integer>();
    protected Map<String, Integer> m_staffCoopSectionMinutes = new HashMap<String, Integer>();
    protected Map<String, Staff> m_staffLocalId;
    protected Map<String, Staff> m_staffName;
    protected Map<String, Staff> m_staffOid;
    protected Map<String, StaffPosition> m_staffPositions;
    protected Map m_studentSchoolMap;
    protected Map<String, List<StudentSchedule>> m_studentScheduleMap;
    protected Map<String, List<StudentSchedule>> m_studentScheduleMapBySection;
    protected Map<String, Map<String, Set<PlainDate>>> m_schoolsToCalendars;
    protected String m_submission;
    protected Map<String, String> m_teacherCodeMap = new HashMap<String, String>();
    protected ArrayList<String> m_teachingStaffCodesList;
    protected Map<String, Collection<ScheduleTermDate>> m_termDateMap;
    protected Map<String, List<Transcript>> m_transcriptMap;
    protected Character m_vamscDelimiterChar;


    /*
     * Translated a sum of race code state codes (1,2,4,8,16) into VA race code
     * representation.
     */
    protected int[] m_raceValueTranslator = new int[] {0, // 0
            5, // 1 W
            3, // 2 B
            14, // 3 WB
            2, // 4 A
            12, // 5 AW
            11, // 6 AB
            20, // 7 AWB
            1, // 8 I
            9, // 9 IW
            8, // 10 IB
            24, // 11 IWB
            7, // 12 IA
            18, // 13 IAW
            17, // 14 IAB
            27, // 15 IAWB
            6, // 16 P
            16, // 17 PW
            15, // 18 PB
            22, // 19 PWB
            13, // 20 PA
            26, // 21 PAW
            21, // 22 PAB
            28, // 23 PAWB
            10, // 24 PI
            25, // 25 PIW
            23, // 26 PIB
            29, // 27 PIWB
            19, // 28 PIA
            30, // 29 PIAW
            31, // 30 PIAB
            32};
    private Set<String> m_coopCodes;


    /**
     * Retirver to return Y or N for student schedule course where the student had 50% or less
     * enrollment.
     *
     * The Class Retrieve50CourseEnrFlag.
     */
    protected static class Retrieve50CourseEnrFlag implements FieldRetriever {

        private static final String CALC_ID = "CRS-ENR-FLAG";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            MasterScheduleCollection mscData = (MasterScheduleCollection) data;
            StudentSchedule ssc = (StudentSchedule) entity.getBean();
            String valueToReturn = "N";
            List<PlainDate> inSessionDates = mscData.getSessionDatesBySection(ssc.getSection());
            if (inSessionDates != null && inSessionDates.size() > 0) {
                PlainDate startDate = inSessionDates.get(0);
                PlainDate lastDate = inSessionDates.get(inSessionDates.size() - 1);
                int membeshipDays =
                        mscData.m_enrollmentManager.getMembershipDays(ssc.getStudent(), startDate, lastDate, true);
                int inSessionDays = inSessionDates.size();
                if (membeshipDays * 2 > inSessionDays) {
                    valueToReturn = "Y";
                }
            }
            return valueToReturn;
        }
    }

    /**
     * Compare the value of another field specified in field.parameter. If the
     * other value is not empty, then blank out the value of this field. This is
     * used to hide the VA course code if the SCED code is not empty.
     *
     * @author X2 Development Corporation
     *
     */
    protected static class RetrieveBlankIfNotEmpty implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            String param = (String) field.getParameter();
            String SCEDCode = entity.getFieldValue(param);
            if (StringUtils.isEmpty(SCEDCode)) {
                value = (String) data.getProperty(entity.getBean(), field.getBeanPath());
            }
            return value;
        }
    }

    /**
     * Retrieve a staff certification record for the given staff oid. Return
     * that certification prefix or number.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCertification implements FieldRetriever {
        private static final String ALIAS_LICENSE_NUMBER = "DOE LICENSE NUMBER";
        private static final String ALIAS_LICENSE_PREFIX = "DOE LICENSE PREFIX";
        private String m_fieldNumber;
        private String m_fieldPrefix;

        /**
         * Instantiates a new retrieve certification.
         */
        public RetrieveCertification() {
            super();
            m_fieldPrefix = translateAliasToJavaName(ALIAS_LICENSE_PREFIX, false);
            m_fieldNumber = translateAliasToJavaName(ALIAS_LICENSE_NUMBER, false);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            MasterScheduleCollection mscData = (MasterScheduleCollection) data;
            String param = (String) field.getParameter();
            String result = null;
            // Find the staff oid. find a certificate record in the map. Check
            // for a cert number.
            String staffOid = (String) data.getProperty(entity.getBean(), field.getBeanPath());
            if (staffOid != null) {
                StaffCertification cert = mscData.m_certificates.get(staffOid);
                if (cert != null) {
                    if (PARAM_CERT_PREFIX.equals(param)) {
                        result = getPrefix(cert);
                    } else if (PARAM_CERT_NUMBER.equals(param)) {
                        result = getNumber(cert);
                    }
                }
            }
            return result;
        }

        /**
         * Gets the prefix.
         *
         * @param cert StaffCertification
         * @return String
         */
        String getPrefix(StaffCertification cert) {
            String value = null;
            if (m_fieldPrefix != null) {
                value = (String) cert.getFieldValueByBeanPath(m_fieldPrefix);
            }
            if (StringUtils.isEmpty(value) && m_fieldNumber != null) {
                String certNum = (String) cert.getFieldValueByBeanPath(m_fieldNumber);
                if (certNum == null || certNum.indexOf("-") < 0) {
                    certNum = cert.getCertificationNumber();
                }
                if (!StringUtils.isEmpty(certNum)) {
                    int pos = certNum.indexOf("-");
                    if (pos >= 0) {
                        value = certNum.substring(0, pos);
                    }
                }
            }
            return value;
        }

        /**
         * Gets the number.
         *
         * @param cert StaffCertification
         * @return String
         */
        String getNumber(StaffCertification cert) {
            String value = null;
            String certNum = (String) cert.getFieldValueByBeanPath(m_fieldNumber);
            if (StringUtils.isEmpty(certNum)) {
                certNum = cert.getCertificationNumber();
            }
            if (!StringUtils.isEmpty(certNum)) {
                int pos = certNum.indexOf("-");
                if (pos >= 0) {
                    value = certNum.substring(pos + 1);
                } else {
                    value = certNum;
                }
            }
            return value;
        }
    }

    /**
     * Pulls the data from a previously built map to show how many students are
     * in a coop in that particular class and section.
     */
    protected static class RetrieveCoopCounts implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        /*
         * Number of Students in Co-Op Program (There is currently a
         * "Co-op Student" field on the Student Schedule table; currently
         * reported in SEDF)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            MasterScheduleCollection mscData = (MasterScheduleCollection) data;
            String param = (String) field.getParameter();
            SisStaff staff = (SisStaff) entity.getBean();
            Integer value = null;
            if (PARAM_COOP_COUNT.equals(param)) {
                value = mscData.m_staffCoopCounts.get(staff.getLocalId());
            } else if (PARAM_COOP_AVG_MINS.equals(param)) {
                Integer aggregateMinutes = mscData.m_staffCoopSectionMinutes.get(staff.getLocalId());
                Integer coopSectionCount = mscData.m_staffCoopSectionCounts.get(staff.getLocalId());
                if ((aggregateMinutes != null) && (coopSectionCount != null) && (coopSectionCount.intValue() > 0)) {
                    value = Integer.valueOf(aggregateMinutes.intValue() / coopSectionCount.intValue());
                }
            }
            return value;
        }
    }

    /**
     * Retrieve the override division and school for a course.
     *
     * @author X2 Development Corporation
     *
     */
    protected static class RetrieveCourseOverride implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            MasterScheduleCollection mscData = (MasterScheduleCollection) data;
            X2BaseBean bean = entity.getBean();
            String param = (String) field.getParameter();
            String result = null;
            if (PARAM_COURSE_DIVISION.equals(param)) {
                if (bean instanceof StudentSchedule) {
                    StudentSchedule ssc = (StudentSchedule) entity.getBean();
                    Course crs = null;
                    if (ssc.getSection() != null && ssc.getSection().getSchoolCourse() != null
                            && (crs = ssc.getSection().getSchoolCourse().getCourse()) != null) {
                        result = (String) crs.getFieldValueByAlias("DOE DISTRICT CRS");
                    }
                }
                if (StringUtils.isBlank(result)) {
                    result = mscData.getOrganizationDistrictId();
                }
            } else if (PARAM_COURSE_SCHOOL.equals(param)) {
                result = (String) data.getProperty(bean, field.getBeanPath());
                if (StringUtils.isBlank(result)) {
                    if (bean instanceof MasterSchedule) {
                        MasterSchedule mst = (MasterSchedule) bean;
                        result = mscData.getSchoolIdAlias(mst);
                    } else if (bean instanceof ScheduleTeacher) {
                        ScheduleTeacher mtc = (ScheduleTeacher) bean;
                        MasterSchedule section = mtc.getSection();
                        result = mscData.getSchoolIdAlias(section);
                    }
                }
            }
            return result;
        }
    }

    /**
     * Retrieve course minutes from the alias fields setup on section.<br>
     * Minutes per period * Periods per day * Days per week * Weeks per year.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCourseMinutes implements FieldRetriever {
        private String m_beanPathCrs;
        private String m_beanPathCsk;
        private String m_beanPathMst;
        private SystemStringConverter m_converter = null;
        private DataDictionary m_extendedDDX = null;
        private Map<String, ReferenceCode> m_scheduleTermRcdMap = null;

        /**
         * Instantiates a new retrieve course minutes.
         */
        public RetrieveCourseMinutes() {
            m_beanPathMst = MasterScheduleCollection.this.translateAliasToJavaName(ALIAS_MST_COURSE_MINUTES, true);
            m_beanPathCsk = MasterScheduleCollection.this.translateAliasToJavaName(ALIAS_CSK_COURSE_MINUTES, true);
            m_beanPathCrs = MasterScheduleCollection.this.translateAliasToJavaName(ALIAS_CRS_COURSE_MINUTES, true);
            DataDictionary ddx = MasterScheduleCollection.this.getDataDictionary();
            DataDictionaryField ddxField =
                    ddx.findDataDictionaryField(ScheduleTerm.class.getName(), ScheduleTerm.COL_CODE);
            ReferenceTable refTable = ddxField.getReferenceTable();
            if (refTable != null) {
                if (m_extendedDDX == null && refTable.getExtendedDataDictionary() != null) {
                    ExtendedDataDictionary extDdx = refTable.getExtendedDataDictionary();
                    m_extendedDDX =
                            DataDictionary.getDistrictDictionary(extDdx,
                                    MasterScheduleCollection.this.getBroker().getPersistenceKey());
                    DataDictionaryField dataField =
                            m_extendedDDX.findDataDictionaryFieldByAlias(ALIAS_RCD_COURSE_MINUTES);
                    if (dataField != null) {
                        Converter baseConverter = ConverterFactory.getConverterForClass(
                                dataField.getEffectiveJavaType(),
                                LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()),
                                dataField.isString());
                        if (baseConverter instanceof SystemStringConverter) {
                            m_converter = ((SystemStringConverter) baseConverter);
                        }
                    }
                }
                refTable.getExtendedDataDictionary();
                m_scheduleTermRcdMap = refTable.getCodeMap();
            } else {
                m_scheduleTermRcdMap = new HashMap<String, ReferenceCode>();
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            MasterSchedule section = (MasterSchedule) entity.getBean();
            Number number = getCourseMinutesFromBean(section, m_beanPathMst);
            if (number == null || number.intValue() == 0) {
                SchoolCourse schoolCourse = section.getSchoolCourse();
                number = getCourseMinutesFromBean(schoolCourse, m_beanPathCsk);
            }
            if (number == null || number.intValue() == 0) {
                Course course = section.getSchoolCourse().getCourse();
                number = getCourseMinutesFromBean(course, m_beanPathCrs);
            }
            if (number == null || number.intValue() == 0) {
                ScheduleTerm scheduleTerm = section.getScheduleTerm();
                number = getCourseMinutesFromSchedule(scheduleTerm.getCode());
            }
            return number;
        }

        /**
         * Gets the course minutes from bean.
         *
         * @param bean X2BaseBean
         * @param beanPath String
         * @return Number
         * @throws X2BaseException exception
         */
        private Number getCourseMinutesFromBean(X2BaseBean bean, String beanPath) throws X2BaseException {
            Number number = null;
            Object value = MasterScheduleCollection.this.getPropertyAsJavaType(bean, beanPath);
            if (value instanceof Number) {
                number = (Number) value;
            }
            return number;
        }

        /**
         * Lookup "all-rcd-MinutesPerCourseTerm" value for "scheduleTermCode" input
         * param
         * Details:
         * ScheduleTerm table -> Code field -> attached reference table -> find ref code like
         * "scheduleTermCode" input param
         * and return "all-rcd-MinutesPerCourseTerm" value for founded refcode.
         *
         * @param scheduleTermCode String
         * @return Number
         */
        private Number getCourseMinutesFromSchedule(String scheduleTermCode) {
            Number number = null;
            ReferenceCode code = m_scheduleTermRcdMap.get(scheduleTermCode);
            if (code != null && m_extendedDDX != null) {
                String value = (String) code.getFieldValueByAlias(ALIAS_RCD_COURSE_MINUTES, m_extendedDDX);
                if (m_converter != null && !StringUtils.isEmpty(value)) {
                    Object property = m_converter.parseSystemString(value);
                    if (property != null && property instanceof Number) {
                        number = (Number) property;
                    }
                }
            }
            return number;
        }
    }

    /**
     * Set to Y based upon the existence of a value greater than zero in the TRN_TOTAL_CREDIT field
     * on the
     * Student Transcript.
     *
     * @author X2 Development Corporation
     *
     */
    protected static class RetrieveCredit implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean creditEarned = false;
            MasterScheduleCollection mscData = (MasterScheduleCollection) data;
            StudentSchedule ssc = (StudentSchedule) entity.getBean();

            Map<String, ScheduleInfo> sscMap = mscData.m_scheduleInfoMap.get(ssc.getStudentOid());
            if (sscMap != null) {
                ScheduleInfo info = sscMap.get(ssc.getSectionOid());
                if (info != null) {
                    if (info.m_transcript != null) {
                        Transcript transcript = info.m_transcript;
                        if (transcript.getTotalCredit() != null &&
                                transcript.getTotalCredit().compareTo(new BigDecimal(0)) > 0) {
                            creditEarned = true;
                        }
                    }
                }
            }
            return Boolean.valueOf(creditEarned);
        }
    }

    /**
     * For a student schedule, find the appropriate transcript and report the
     * final grade.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveFinalGrade implements FieldRetriever {
        GradesManager m_gradeManager = null;

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            MasterScheduleCollection mscData = (MasterScheduleCollection) data;
            StudentSchedule ssc = (StudentSchedule) entity.getBean();
            String result = null;

            Map<String, ScheduleInfo> sscMap = mscData.m_scheduleInfoMap.get(ssc.getStudentOid());
            if (sscMap != null) {
                ScheduleInfo info = sscMap.get(ssc.getSectionOid());
                if (info != null) {
                    if (info.m_transcript != null) {
                        result = info.m_transcript.getFinalGrade();
                        Transcript transcript = info.m_transcript;
                        TranscriptDefinition transcriptDefinition = transcript.getTranscriptDefinition();
                        if (transcriptDefinition != null) {
                            TranscriptColumnDefinition column = transcriptDefinition.getFinalColumnDefinition();
                            if (column != null) {
                                String grade = transcript.getFinalGrade();
                                grade = grade == null ? grade : grade.trim();
                                GradeScaleGradeDefinition gradeDefinition = getGradeDefinition(grade, column);
                                if (gradeDefinition != null) {
                                    result = gradeDefinition.getGradeCode();
                                }
                            }
                        }
                    }
                }
            }
            return result;
        }

        /**
         * Gets the grade manager.
         *
         * @return Grades manager
         */
        private GradesManager getGradeManager() {
            if (m_gradeManager == null) {
                m_gradeManager = new GradesManager(getBroker());
            }
            return m_gradeManager;
        }

        /**
         * Returns the grade definition for the passed grade and column. Null is returned if one
         * cannot
         * be determined.
         *
         * @param grade String
         * @param column TranscriptColumnDefinition
         * @return GradeScaleGradeDefinition
         */
        @SuppressWarnings("deprecation")
        private GradeScaleGradeDefinition getGradeDefinition(String grade, TranscriptColumnDefinition column) {
            GradeScaleGradeDefinition gradeDefinition = null;
            GradeScale gradeScale = column.getGradeScale();
            GradesManager gradesManager = getGradeManager();
            if (gradeScale != null) {
                // Deprecated method is being used for 5.6 - 5.8 compatibility
                if (StringUtils.isNumeric(grade)) {
                    gradeDefinition = gradesManager.getGradeDefinition(
                            new BigDecimal(grade), gradeScale);
                } else {
                    gradeDefinition = gradesManager.getGradeDefinition(
                            grade, gradeScale);
                }
            }
            return gradeDefinition;
        }
    }

    /**
     * Retrieve if anniversary date of staff position less than report date.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveFirstYear implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            boolean firstYear = false;
            StaffPosition staffPosition = null;
            if (entity.getBean() instanceof Staff) {
                String staffOid = entity.getBean().getOid();
                staffPosition = m_staffPositions.get(staffOid);
            } else if (entity.getBean() instanceof StaffPosition) {
                staffPosition = (StaffPosition) entity.getBean();
            }
            if (staffPosition != null) {
                String anniversaryDateSystem = (String) staffPosition.getFieldValueByBeanPath(m_fieldAnniversaryDate);
                PlainDate anniversaryDate =
                        (PlainDate) m_dateAsStringConverter.parseSystemString(anniversaryDateSystem);

                if (anniversaryDate != null) {
                    if (anniversaryDate.after(m_reportDate1YearBefore)) {
                        firstYear = true;
                    }
                }
            }
            return Boolean.valueOf(firstYear);
        }
    }

    /**
     * For the value retrieved (section course view), find the school ID and put
     * it in front of the section course view.
     *
     * @author X2 Development Corporation
     */
    protected static class RetrievePrefix implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            X2BaseBean bean = entity.getBean();
            String calculatedMstId = null;
            MasterScheduleCollection mscData = (MasterScheduleCollection) data;
            MasterScheduleEntity msEntity = (MasterScheduleEntity) entity;
            if ("C".equals(entity.getFieldValue(EXPORT_FIELD_C_RECORD_TYPE))) {
                calculatedMstId = getCalculatedIdForCRecord((MasterSchedule) bean, mscData, msEntity);
            }
            if ("D".equals(entity.getFieldValue(EXPORT_FIELD_C_RECORD_TYPE))) {
                calculatedMstId = getCalculatedIdForDRecord(((ScheduleTeacher) bean).getSection(), mscData, msEntity);
            }
            if ("F".equals(entity.getFieldValue(EXPORT_FIELD_C_RECORD_TYPE))) {
                calculatedMstId = getCalculatedIdForFRecord((StudentSchedule) bean, mscData);
            }
            if ("I".equals(entity.getFieldValue(EXPORT_FIELD_C_RECORD_TYPE))) {
                calculatedMstId = getCalculatedIdForIRecord((MasterSchedule) bean, mscData);
            }
            String value = (String) data.getProperty(bean, field.getBeanPath());
            String schoolId = (String) data.getProperty(bean, (String) field.getParameter());
            value = schoolId + "-" + (!StringUtils.isEmpty(calculatedMstId) ? calculatedMstId : value);
            return value;
        }

        /**
         * Gets the calculated id for C record.
         *
         * @param mst MasterSchedule
         * @param mscData MasterScheduleCollection
         * @param msEntity MasterScheduleEntity
         * @return String
         */
        private String getCalculatedIdForCRecord(MasterSchedule mst,
                                                 MasterScheduleCollection mscData,
                                                 MasterScheduleEntity msEntity) {
            String value = null;
            Set<String> grades = msEntity.m_gradesForSectionsCRecords.get(mst.getOid());
            if (grades != null && grades.size() > 0) {
                String grade = grades.iterator().next();
                grades.remove(grade);
                String code = mst.getSchoolCourse().getCourse().getNumber() + "-" + grade;
                if (!StringUtils.isEmpty(code)) {
                    ReferenceCode calcRefCode = mscData.m_refTableMultiGradeCrossCodes.get(code);
                    if (calcRefCode != null) {
                        String calcStateCode = calcRefCode.getStateCode();
                        String[] splittedCourseView = (mst.getCourseView()).split("-");
                        if (splittedCourseView != null && splittedCourseView.length >= 2) {
                            String crsViewSuffix = splittedCourseView[1];
                            if (!StringUtils.isEmpty(crsViewSuffix)) {
                                value = calcStateCode + "-" + crsViewSuffix;
                            }
                        }
                    }
                }
            }
            return value;
        }

        /**
         * Gets the calculated id for D record.
         *
         * @param mst MasterSchedule
         * @param mscData MasterScheduleCollection
         * @param msEntity MasterScheduleEntity
         * @return String
         */
        private String getCalculatedIdForDRecord(MasterSchedule mst,
                                                 MasterScheduleCollection mscData,
                                                 MasterScheduleEntity msEntity) {
            String value = null;
            Set<String> grades = msEntity.m_gradesForSectionsDRecords.get(mst.getOid());
            if (grades != null && grades.size() > 0) {
                String grade = grades.iterator().next();
                grades.remove(grade);
                String code = mst.getSchoolCourse().getCourse().getNumber() + "-" + grade;
                if (!StringUtils.isEmpty(code)) {
                    ReferenceCode calcRefCode = mscData.m_refTableMultiGradeCrossCodes.get(code);
                    if (calcRefCode != null) {
                        String calcStateCode = calcRefCode.getStateCode();
                        String[] splittedCourseView = (mst.getCourseView()).split("-");
                        if (splittedCourseView != null && splittedCourseView.length >= 2) {
                            String crsViewSuffix = splittedCourseView[1];
                            if (!StringUtils.isEmpty(crsViewSuffix)) {
                                value = calcStateCode + "-" + crsViewSuffix;
                            }
                        }
                    }
                }
            }
            return value;
        }

        /**
         * Gets the calculated id for F record.
         *
         * @param ssc StudentSchedule
         * @param mscData MasterScheduleCollection
         * @return String
         */
        private String getCalculatedIdForFRecord(StudentSchedule ssc,
                                                 MasterScheduleCollection mscData) {
            String value = null;
            String grade = ssc.getStudent().getGradeLevel();
            if (!StringUtils.isEmpty(grade)) {
                MasterSchedule mst = ssc.getSection();
                String code = mst.getSchoolCourse().getCourse().getNumber() + "-" + grade;
                if (!StringUtils.isEmpty(code)) {
                    ReferenceCode calcRefCode = mscData.m_refTableMultiGradeCrossCodes.get(code);
                    if (calcRefCode != null) {
                        String calcStateCode = calcRefCode.getStateCode();
                        String[] splittedCourseView = (mst.getCourseView()).split("-");
                        if (splittedCourseView != null && splittedCourseView.length >= 2) {
                            String crsViewSuffix = splittedCourseView[1];
                            if (!StringUtils.isEmpty(crsViewSuffix)) {
                                value = calcStateCode + "-" + crsViewSuffix;
                            }
                        }
                    }
                }
            }
            return value;
        }

        /**
         * Gets the calculated id for I record.
         *
         * @param mst MasterSchedule
         * @param mscData MasterScheduleCollection
         * @param msEntity MasterScheduleEntity
         * @return String
         */
        private String getCalculatedIdForIRecord(MasterSchedule mst,
                                                 MasterScheduleCollection mscData) {
            String value = null;
            Set<String> grades = mscData.m_gradesForSectionsIRecords.get(mst.getOid());
            if (grades != null && grades.size() > 0) {
                String grade = grades.iterator().next();
                grades.remove(grade);
                String code = mst.getSchoolCourse().getCourse().getNumber() + "-" + grade;
                if (!StringUtils.isEmpty(code)) {
                    ReferenceCode calcRefCode = mscData.m_refTableMultiGradeCrossCodes.get(code);
                    if (calcRefCode != null) {
                        String calcStateCode = calcRefCode.getStateCode();
                        String[] splittedCourseView = (mst.getCourseView()).split("-");
                        if (splittedCourseView != null && splittedCourseView.length >= 2) {
                            String crsViewSuffix = splittedCourseView[1];
                            if (!StringUtils.isEmpty(crsViewSuffix)) {
                                value = calcStateCode + "-" + crsViewSuffix;
                            }
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retrieve the VA specific race code.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRace implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String personOid = (String) data.getProperty(entity.getBean(), field.getBeanPath());

            int totalRace = 0;
            Collection<Race> races = m_raceCodeMap.get(personOid);
            if (races != null) {
                for (Race race : races) {
                    ReferenceCode code = m_raceCodes.get(race.getRaceCode());
                    if (code != null) {
                        int codeNum = 0;
                        try {
                            codeNum = Integer.parseInt(code.getStateCode());
                        } catch (NumberFormatException nfe) {
                            // not parsable, ignore.
                        }
                        totalRace += codeNum;
                    }
                }
            }
            if ((totalRace > 0) && (totalRace < 32)) {
                totalRace = m_raceValueTranslator[totalRace];
            } else {
                totalRace = 0;
            }
            return (totalRace == 0) ? null : Integer.valueOf(totalRace);
        }
    }

    /**
     * Concatenate schoolCourse.course.[DOE SCED SUBJECT] and schoolCourse.course.[DOE SCED COURSE]
     *
     * @author X2 Development Corporation
     */
    protected static class RetrieveScedCode implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            MasterScheduleCollection mscData = (MasterScheduleCollection) data;
            MasterSchedule mst = (MasterSchedule) entity.getBean();
            MasterScheduleEntity msEntity = (MasterScheduleEntity) entity;
            String calculatedScedCode = null;
            if (msEntity.m_gradesForSectionsCRecords.containsKey(mst.getOid())) {
                String sectionId = msEntity.getFieldValue(EXPORT_FIELD_C_SECTION_ID);
                if (!StringUtils.isEmpty(sectionId)) {
                    String[] splittedSectionId = sectionId.split("-");
                    if (splittedSectionId.length >= 3) {
                        calculatedScedCode = splittedSectionId[1];
                    }
                }
            }
            if (StringUtils.isEmpty(calculatedScedCode)) {
                String scedSubject =
                        (String) mst.getSchoolCourse().getCourse().getFieldValueByBeanPath(mscData.m_fieldScedSubject);
                if (scedSubject != null) {
                    scedSubject = data.lookupStateValue(Course.class, mscData.m_fieldScedSubject, scedSubject);
                }
                String scedCourse =
                        (String) mst.getSchoolCourse().getCourse().getFieldValueByBeanPath(mscData.m_fieldScedCourse);
                return (scedSubject == null ? EMPTY_STRING : scedSubject)
                        + (scedCourse == null ? EMPTY_STRING : scedCourse);
            }
            return calculatedScedCode;
        }
    }

    /**
     * Concatenate schoolCourse.course.[DOE SCED SUBJECT] and schoolCourse.course.[DOE SCED COURSE]
     *
     * @author X2 Development Corporation
     */
    protected static class RetrieveLocalCrsNumber implements FieldRetriever {

        private static final String CALC_ID = "MSC-LOCAL-CRS-NUM";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            MasterSchedule mst = (MasterSchedule) entity.getBean();
            MasterScheduleEntity msEntity = (MasterScheduleEntity) entity;
            String calculatedScedCode = null;
            if (msEntity.m_gradesForSectionsCRecords.containsKey(mst.getOid())) {
                String sectionId = msEntity.getFieldValue(EXPORT_FIELD_C_SECTION_ID);
                if (!StringUtils.isEmpty(sectionId)) {
                    String[] splittedSectionId = sectionId.split("-");
                    if (splittedSectionId.length >= 3) {
                        calculatedScedCode = splittedSectionId[1] + "-" + splittedSectionId[2];
                    }
                }
            }
            if (StringUtils.isEmpty(calculatedScedCode)) {
                return mst.getSchoolCourse().getNumber();
            }
            return calculatedScedCode;
        }
    }

    /**
     * Earlier we created a map to find the 'Highest' section taught in one
     * classroom in conjuction with other classes being taught at the same time.
     * We now retrieve that information
     */
    protected static class RetrieveSectionIDs implements FieldRetriever {
        /*
         * Add field 'Sequenced course' to the School Schedule table. This field
         * will allow you to put in the sequence number of a course that is
         * taught in the same classroom at the same time by the same teacher.
         * (You will only have to enter 1, 2, 3, etc.) By doing, we will be able
         * to tell which course/section is sequenced and of those courses in
         * the sequence, which is the highest level as well as created the
         * required connected section.
         */

        /*
         * Section ID: This is the highest level of the classes being taught (in
         * the example, this would be Carpentry 3). If the class is not
         * sequenced, then use the section with the most students. Connected
         * Section ID: This is the lowest level of the classes being taught (in
         * the example, this would be Carpentry 2). If the class is not
         * sequenced, then use the section with the least students. NOTE: If
         * there are 3 courses taught (for example, Carpentry 1, Carpentry 2,
         * and Carpentry 3), then there would be 2 lines for the highest level
         * (sequence) course or the one with the most students. This course
         * would have a Connected Section ID of both of the lower sequenced
         * courses or the ones with the least students. For example:
         *
         * Section ID: Carpentry 3 Connected Section ID: Carpentry 1
         *
         * Section ID: Carpentry 3 Connected Section ID: Carpentry 2
         */

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            MasterScheduleCollection mscData = (MasterScheduleCollection) data;
            MasterSchedule section = (MasterSchedule) entity.getBean();
            MasterSchedule primarySection = mscData.m_connectedSections.get(section);
            String schoolId = (String) data.getProperty(entity.getBean(), (String) field.getParameter());
            if (mscData.m_gradesForSectionsIRecords.containsKey(primarySection.getOid())) {
                value = getCalculatedIdForIRecord(primarySection, mscData,
                        mscData.m_maxGradesForSectionsIRecords.get(primarySection.getOid()), schoolId);
            } else {
                value = mscData.getSchoolId(primarySection) + "-" + primarySection.getCourseView();
            }
            return value;
        }

        /**
         * Gets the calculated id for C record.
         *
         * @param mst MasterSchedule
         * @param mscData MasterScheduleCollection
         * @param msEntity MasterScheduleEntity
         * @return String
         */
        private String getCalculatedIdForIRecord(MasterSchedule mst,
                                                 MasterScheduleCollection mscData,
                                                 String grade,
                                                 String sklId) {
            String value = null;
            String code = mst.getSchoolCourse().getCourse().getNumber() + "-" + grade;
            if (!StringUtils.isEmpty(code)) {
                ReferenceCode calcRefCode = mscData.m_refTableMultiGradeCrossCodes.get(code);
                if (calcRefCode != null) {
                    String calcStateCode = calcRefCode.getStateCode();
                    String[] splittedCourseView = (mst.getCourseView()).split("-");
                    if (splittedCourseView != null && splittedCourseView.length >= 2) {
                        String crsViewSuffix = splittedCourseView[1];
                        if (!StringUtils.isEmpty(crsViewSuffix)) {
                            value = calcStateCode + "-" + crsViewSuffix;
                        }
                    }
                }
            }
            return sklId + "-" + value;
        }

    }

    /**
     * Retrieve value stored in a map of the current teacher. This will scroll
     * through teachers per section if there are more than one to report on.
     *
     * @author X2 Development Corporation
     *
     */
    protected static class RetrieveStaffID implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String result = EMPTY_STRING;
            MasterScheduleCollection mscData = (MasterScheduleCollection) data;
            if (entity.getBean() instanceof StudentSchedule) {
                StudentSchedule ssc = (StudentSchedule) entity.getBean();
                Map<String, ScheduleInfo> sscMap = mscData.m_scheduleInfoMap.get(ssc.getStudentOid());
                if (sscMap != null) {
                    ScheduleInfo info = sscMap.get(ssc.getSectionOid());
                    if (info != null) {
                        result = info.getCurrentTeacher();
                    }
                }
            } else if (entity.getBean() instanceof ScheduleTeacher) {
                // so the first one through is a normal, then from the top of
                // the list
                ScheduleTeacher teacher = (ScheduleTeacher) entity.getBean();
                result = mscData.getStaff(teacher).getLocalId();
                if (mscData.m_sectionsWithMultipleTeachers.containsKey(teacher.getSection().getOid())) {
                    // ok now we know there are multiple, how do we know we're
                    // the first time?
                    ArrayList<String> list = mscData.m_sectionsWithMultipleTeachers.get(teacher.getSection().getOid());
                    if ("CONTINUE".equals(list.get(0))) {
                        Staff staff = mscData.m_staffName.get(list.get(1));
                        list.remove(1);
                        result = staff.getLocalId();
                    } else {
                        list.add(0, "CONTINUE");
                    }
                    mscData.m_sectionsWithMultipleTeachers.put(teacher.getSection().getOid(), list);
                }
            }
            return result;
        }
    }

    /**
     * Return a field value as an override, or an default value from the staff
     * record.
     *
     * @author X2 Development Corporation
     *
     */
    protected static class RetrieveStaffOverride implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            ScheduleTeacher bean = (ScheduleTeacher) entity.getBean();
            MasterScheduleCollection mscData = (MasterScheduleCollection) data;
            String result = (String) data.getProperty(bean, field.getBeanPath());
            if (StringUtils.isEmpty(result)) {
                result = (String) mscData.getStaff(bean).getFieldValueByAlias(param);
            }
            return result;
        }
    }

    /**
     * Container object for student schedule information.
     *
     * @author X2 Development Corporation
     */
    protected static class ScheduleInfo {
        private int m_currentTeacherCount = 0;
        public PlainDate m_entryDate;
        public PlainDate m_exitDate;
        public MasterSchedule m_section;
        public StudentSchedule m_schedule;
        public List<String> m_scheduleTeacherList = new ArrayList<String>();
        public Transcript m_transcript;
        public boolean m_withdrew;

        /**
         * Return the name of the section for use in sorting.
         *
         * @return String
         */
        protected String getCourseView() {
            String name = EMPTY_STRING;
            if (m_section != null) {
                name = m_section.getCourseView();

            }
            return name;
        }

        /**
         * Scrolls through save teachers from each section for an individual student.
         *
         * @return String
         */
        public String getCurrentTeacher() {
            if (m_currentTeacherCount > m_scheduleTeacherList.size()) {
                m_currentTeacherCount = 0;
            }
            String result = m_scheduleTeacherList.get(m_currentTeacherCount);
            m_currentTeacherCount++;
            return result;
        }

        /**
         * Return the StudentSchedule.
         *
         * @return StudentSchedule
         */
        protected StudentSchedule getSchedule() {
            return m_schedule;
        }

        /**
         * Return the section.
         *
         * @return MasterSchedule
         */
        protected MasterSchedule getSection() {
            return m_section;
        }
    }

    /**
     * Validation class for whole entity with MasterSchedule class bean.
     * It is designed as FieldValidator, because "legal" way of validating entity don't exist.
     */
    protected static class ValidateMasterScheduleEntity implements FieldValidator {
        private static final String ERROR_CODE_CLASS_TYPE =
                "The Defined Class Type has not been assigned or is incorrect";
        private static final String ERROR_CODE_CLASS_TYPE_DD_5_8 =
                "A Defined Class Type of 7882 can only be used for students who are ages 5-8";
        private static final String ERROR_CODE_CLASS_TYPE_DD =
                "A Defined Class Type of 7884 can only be used for students who are not ages 5-8.";
        private static final String ERROR_CODE_MOP_ID =
                "If MOP ID is not null, then Virtual Course Indicator in the F Record is required.";
        private static final String ERROR_CODE_TEACHER_ROLE_4 =
                "If a Teacher has a Teacher Role Code of 4, the teacher should be assigned to specific students as Additional Teacher One or Additional Teacher Two";
        private static final String ERROR_CODE_TEACHER_ROLE_NOT_4 =
                "If a Teacher does not have a Teacher Role Code of 4, the teacher should not be assigned to specific students as Additional Teacher One or Additional Teacher Two";
        private static final String ERROR_CODE_ADDITIONAL_TEACHER =
                "A teacher assigned as Additional Teacher One or Additional Teacher Two must be added as a teacher on the section";
        private static final String FIELD_ID_FILLER = "Entity";
        /**
         * Need for having mapping of disability codes and appropriate class type.
         * Don't include special cases then one class type cover two or more disability codes,
         * or depends on student age.
         */
        private Map m_disabilityCodes = new HashMap<String, String>();

        /**
         * Instantiates a new validate master schedule entity.
         */
        public ValidateMasterScheduleEntity() {
            // Key for student disabilityCode, value for Class Type
            m_disabilityCodes.put("AUT", "7805");
            m_disabilityCodes.put("MD", "7810");
            m_disabilityCodes.put("OHI", "7815");
            m_disabilityCodes.put("ID", "7822");
            m_disabilityCodes.put("HI", "7848");
            m_disabilityCodes.put("VI", "7850");
            m_disabilityCodes.put("DB", "7855");
            m_disabilityCodes.put("ED", "7863");
            m_disabilityCodes.put("SLD", "7865");
            m_disabilityCodes.put("OI", "7873");
            m_disabilityCodes.put("TBI", "7874");
            m_disabilityCodes.put("SLI", "7875");
            m_disabilityCodes.put("SD", "7880");
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            MasterScheduleCollection mscData = (MasterScheduleCollection) data;
            MasterScheduleEntity msEntity = (MasterScheduleEntity) entity;
            X2BaseBean bean = msEntity.getBean();
            MasterSchedule mst = (MasterSchedule) bean;
            List<StudentSchedule> stdSchedules = mscData.m_studentScheduleMapBySection.get(mst.getOid());
            String crsMopId =
                    (String) mst.getSchoolCourse().getCourse().getFieldValueByBeanPath(mscData.m_fieldCrsMopId);
            if (!StringUtils.isEmpty(crsMopId)) {
                String stateMopId = mscData.lookupStateValue(Course.class, mscData.m_fieldCrsMopId, crsMopId);
                if (!StringUtils.isEmpty(stateMopId)) {
                    validateMOPID(msEntity, stdSchedules);
                }
            }
            if (stdSchedules == null) {
                stdSchedules = new ArrayList<StudentSchedule>();
            }
            List<ScheduleTeacher> teacherSchedules =
                    (List<ScheduleTeacher>) getCorrectTeacherSections(mst.getTeacherSections());

            errors.addAll(validateAssignedStaff(entity, mscData, stdSchedules, teacherSchedules));
            errors.addAll(validateClassType(entity, mscData, stdSchedules, teacherSchedules));

            return errors;
        }

        /**
         * Validate three kind of situations:
         * 1) Teacher assigned in Additional Teacher One of Additional Teacher Two field is not
         * assigned on the section as a teacher.
         * 2) Teacher on the section has a Teacher Role Code of 4 and there is no teacher assigned
         * in Additional
         * Teacher One or Additional Teacher two
         * 3) Teacher on the section has a Teacher Role Code that is not 4 and there is a teacher
         * assigned in Additional Teacher
         * One or Additional Teacher Two
         *
         * @param entity StateReportEntity
         * @param mscData MasterScheduleCollection
         * @param stdSchedules List<StudentSchedule>
         * @param teacherSchedules List<ScheduleTeacher>
         * @return List
         */
        private List<StateReportValidationError> validateAssignedStaff(StateReportEntity entity,
                                                                       MasterScheduleCollection mscData,
                                                                       List<StudentSchedule> stdSchedules,
                                                                       List<ScheduleTeacher> teacherSchedules) {
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            Set<String> additionalStaffs = new HashSet();
            Map<String, SisStudent> studentsWithTeacher = new HashMap<String, SisStudent>();
            for (StudentSchedule schedule : stdSchedules) {
                String additionalStaffOne = (String) schedule.getFieldValueByBeanPath(mscData.m_fieldAdditionalStaff1);
                if (additionalStaffOne != null) {
                    additionalStaffs.add(additionalStaffOne);
                    studentsWithTeacher.put(additionalStaffOne, schedule.getStudent());
                }
                String additionalStaffTwo = (String) schedule.getFieldValueByBeanPath(mscData.m_fieldAdditionalStaff2);
                if (additionalStaffTwo != null) {
                    additionalStaffs.add(additionalStaffTwo);
                    studentsWithTeacher.put(additionalStaffTwo, schedule.getStudent());
                }
            }
            Set<String> sectionStaffs = new HashSet();
            for (ScheduleTeacher teacherSchedule : teacherSchedules) {
                String teacherRole = teacherSchedule.getRole();
                if ("4".equals(teacherRole)
                        && !additionalStaffs.contains(mscData.getStaff(teacherSchedule).getNameView())) {
                    StateReportValidationError error = new StateReportValidationError(entity.getEntityName(),
                            FIELD_ID_FILLER, ERROR_CODE_TEACHER_ROLE_4,
                            "Teacher: " +
                                    teacherSchedule.getStaff().getNameView());
                    errors.add(error);
                }
                if (!"4".equals(teacherRole)
                        && additionalStaffs.contains(mscData.getStaff(teacherSchedule).getNameView())) {
                    String teacherName = mscData.getStaff(teacherSchedule).getNameView();
                    StringBuilder errorMessage = getErrorMessage(Arrays.asList("Teacher: ",
                            teacherName,
                            ", Teacher Role: ",
                            teacherRole,
                            ", Student: ",
                            studentsWithTeacher.get(teacherName).getNameView()));
                    StateReportValidationError error = new StateReportValidationError(entity.getEntityName(),
                            FIELD_ID_FILLER, ERROR_CODE_TEACHER_ROLE_NOT_4,
                            errorMessage.toString());
                    errors.add(error);
                }
                sectionStaffs.add(mscData.getStaff(teacherSchedule).getNameView());
            }

            for (String additionalStaff : additionalStaffs) {
                if (!sectionStaffs.contains(additionalStaff)) {
                    StringBuilder errorMessage = getErrorMessage(Arrays.asList("Teacher: ",
                            additionalStaff,
                            ", Student: ",
                            studentsWithTeacher.get(additionalStaff).getNameView()));
                    StateReportValidationError error = new StateReportValidationError(entity.getEntityName(),
                            FIELD_ID_FILLER, ERROR_CODE_ADDITIONAL_TEACHER,
                            errorMessage.toString());
                    errors.add(error);
                }
            }
            return errors;
        }

        /**
         * Validate Class Type [DOE CLASS TYPE] values assigned to Schedule Master Teacher beans,
         * Class Type calculates from collection of student's Primary Disability Code's.
         *
         * @param entity StateReportEntity
         * @param mscData MasterScheduleCollection
         * @param stdSchedules List<StudentSchedule>
         * @param teacherSchedules List<ScheduleTeacher>
         * @return List
         */
        private List<StateReportValidationError> validateClassType(StateReportEntity entity,
                                                                   MasterScheduleCollection mscData,
                                                                   List<StudentSchedule> stdSchedules,
                                                                   List<ScheduleTeacher> teacherSchedules) {
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            Set<String> disabilityCodes = new HashSet<String>();
            boolean isAtLeastOneStudentAgeOf5Till8 = false;
            for (StudentSchedule stdSchedule : stdSchedules) {
                SisStudent student = stdSchedule.getStudent();
                String code = (String) student.getFieldValueByBeanPath(mscData.m_fieldPrimaryDisability);
                if (!StringUtils.isEmpty(code) && !"504".equals(code)) {
                    disabilityCodes.add(code);
                    int age = student.getPerson().getAgeAsOfDate(mscData.m_reportDate);
                    if (age >= 5 && age <= 8) {
                        isAtLeastOneStudentAgeOf5Till8 = true;
                    }
                }
            }
            Set<String> definedClassTypes = new HashSet<String>();
            for (ScheduleTeacher scheduleTeacher : teacherSchedules) {
                String definedClassType =
                        (String) scheduleTeacher.getFieldValueByBeanPath(mscData.m_fieldDefinedClassType);
                if (!StringUtils.isEmpty(definedClassType)) {
                    definedClassTypes.add(definedClassType);
                }
            }
            String classType = null;
            if (disabilityCodes.size() == 0) {
                boolean isErrorExist = false;
                for (String definedClassType : definedClassTypes) {
                    if (!StringUtils.isEmpty(definedClassType)) {
                        isErrorExist = true;
                    }
                }
                if (isErrorExist) {
                    String errorMessage = getErrorMessage(Arrays.asList("Defined Class Types are ",
                            definedClassTypes.toString(),
                            ", Students disability codes are empty")).toString();
                    StateReportValidationError error = new StateReportValidationError(entity.getEntityName(),
                            FIELD_ID_FILLER, ERROR_CODE_CLASS_TYPE, errorMessage);
                    errors.add(error);
                }
            } else if (disabilityCodes.size() == 1) {
                String disabilityCode = disabilityCodes.iterator().next();
                if (!"DD".equals(disabilityCode)) {
                    classType = (String) m_disabilityCodes.get(disabilityCode);
                } else if (isAtLeastOneStudentAgeOf5Till8) {
                    classType = "7882";
                    if (!definedClassTypes.contains(classType)) {
                        String errorMessage = getErrorMessage(Arrays.asList("Defined Class Types are ",
                                definedClassTypes.toString(),
                                ", Students disability code is ",
                                disabilityCode,
                                ". Class Type should be ", classType)).toString();
                        StateReportValidationError error = new StateReportValidationError(entity.getEntityName(),
                                FIELD_ID_FILLER, ERROR_CODE_CLASS_TYPE_DD_5_8, errorMessage);
                        errors.add(error);
                    }
                } else {
                    classType = "7884";
                    if (!definedClassTypes.contains(classType)) {
                        String errorMessage = getErrorMessage(Arrays.asList("Defined Class Types are ",
                                definedClassTypes.toString(),
                                ", Students disability code is ",
                                disabilityCode,
                                ". Class Type should be ", classType)).toString();
                        StateReportValidationError error = new StateReportValidationError(entity.getEntityName(),
                                FIELD_ID_FILLER, ERROR_CODE_CLASS_TYPE_DD, errorMessage);
                        errors.add(error);
                    }
                }
                if (!definedClassTypes.contains(classType)) {
                    String errorMessage = getErrorMessage(Arrays.asList("Defined Class Types are ",
                            definedClassTypes.toString(),
                            ", Students disability code is ",
                            disabilityCode,
                            ". Class Type should be ", classType)).toString();
                    StateReportValidationError error = new StateReportValidationError(entity.getEntityName(),
                            FIELD_ID_FILLER, ERROR_CODE_CLASS_TYPE, errorMessage);
                    errors.add(error);
                }
            } else if (Arrays.asList("ED", "ID").containsAll(disabilityCodes)) {
                classType = "7857";
                if (!definedClassTypes.contains(classType)) {
                    String errorMessage = getErrorMessage(Arrays.asList("Defined Class Types are ",
                            definedClassTypes.toString(),
                            ", Students disability codes are ",
                            disabilityCodes.toString(),
                            ". Class Type should be ", classType)).toString();
                    StateReportValidationError error = new StateReportValidationError(entity.getEntityName(),
                            FIELD_ID_FILLER, ERROR_CODE_CLASS_TYPE, errorMessage);
                    errors.add(error);
                }
            } else if (Arrays.asList("SLD", "ID").containsAll(disabilityCodes)) {
                classType = "7860";
                if (!definedClassTypes.contains(classType)) {
                    String errorMessage = getErrorMessage(Arrays.asList("Defined Class Types are ",
                            definedClassTypes.toString(),
                            ", Students disability codes are ",
                            disabilityCodes.toString(),
                            ". Class Type should be ", classType)).toString();
                    StateReportValidationError error = new StateReportValidationError(entity.getEntityName(),
                            FIELD_ID_FILLER, ERROR_CODE_CLASS_TYPE, errorMessage);
                    errors.add(error);
                }
            } else if (Arrays.asList("ED", "SLD").containsAll(disabilityCodes)) {
                classType = "7868";
                if (!definedClassTypes.contains(classType)) {
                    String errorMessage = getErrorMessage(Arrays.asList("Defined Class Types are ",
                            definedClassTypes.toString(),
                            ", Students disability codes are ",
                            disabilityCodes.toString(),
                            ". Class Type should be ", classType)).toString();
                    StateReportValidationError error = new StateReportValidationError(entity.getEntityName(),
                            FIELD_ID_FILLER, ERROR_CODE_CLASS_TYPE, errorMessage);
                    errors.add(error);
                }
            } else if (Arrays.asList("SLD", "ED", "ID").containsAll(disabilityCodes)) {
                classType = "7871";
                if (!definedClassTypes.contains(classType)) {
                    String errorMessage = getErrorMessage(Arrays.asList("Defined Class Types are ",
                            definedClassTypes.toString(),
                            ", Students disability codes are ",
                            disabilityCodes.toString(),
                            ". Class Type should be ", classType)).toString();
                    StateReportValidationError error = new StateReportValidationError(entity.getEntityName(),
                            FIELD_ID_FILLER, ERROR_CODE_CLASS_TYPE, errorMessage);
                    errors.add(error);
                }
            } else {
                classType = "7810";
                if (!definedClassTypes.contains(classType)) {
                    String errorMessage = getErrorMessage(Arrays.asList("Defined Class Types are ",
                            definedClassTypes.toString(),
                            ", Students disability codes are ",
                            disabilityCodes.toString(),
                            ". Class Type should be ", classType)).toString();
                    StateReportValidationError error = new StateReportValidationError(entity.getEntityName(),
                            FIELD_ID_FILLER, ERROR_CODE_CLASS_TYPE, errorMessage);
                    errors.add(error);
                }
            }
            return errors;
        }

        /**
         * If MOP ID is not null, then Virtual Course Indicator in the F Record is required.
         *
         * @param entity
         * @param stdSchedules
         * @return
         */
        private List<StateReportValidationError> validateMOPID(StateReportEntity entity,
                                                               List<StudentSchedule> stdSchedules) {
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            for (StudentSchedule ssc : stdSchedules) {
                if (ssc.getSection().getSchoolCourse().getCourse()
                        .getFieldValueByAlias("DOE VIRTUAL COURSE TYPE") == null) {
                    StringBuilder errorMessage = getErrorMessage(Arrays.asList("Student: ",
                            ssc.getStudent().getNameView(),
                            ", Schedule: ",
                            ssc.getScheduleDisplay()));
                    StateReportValidationError error = new StateReportValidationError(entity.getEntityName(),
                            FIELD_ID_FILLER, ERROR_CODE_MOP_ID,
                            errorMessage.toString());
                    errors.add(error);
                }
            }
            return errors;
        }

        /**
         * Helper method to construct one string from a batch of strings.
         *
         * @param messageParts List<String>
         * @return String builder
         */
        private StringBuilder getErrorMessage(List<String> messageParts) {
            StringBuilder errorMessage = new StringBuilder();
            for (String part : messageParts) {
                errorMessage.append(part);
            }
            return errorMessage;
        }
    }

    /**
     * Return a custom heading line.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        // get values used in the heading.
        String divId = (String) getOrganization().getFieldValueByAlias(ALIAS_DISTRICT_ID);
        String email = getUser().getPerson().getEmail01();
        if (StringUtils.isEmpty(email)) {
            email = "{email}";
        }
        // Header row
        java.util.Date currDate = new Date();
        SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");
        SimpleDateFormat timeFormat = new SimpleDateFormat("hh:mm:ss");
        // MSC header record
        String section = (String) getParameter(PARAM_SECTION);
        section = (section + "         ").substring(0, 9);
        int year = getOrganization().getCurrentContext().getSchoolYear() - 1;
        StringBuilder divisionNumber = new StringBuilder("000");
        if (divId != null) {
            divisionNumber.append(divId);
            while (divisionNumber.length() != 3) {
                divisionNumber.delete(0, 1);
            }
        }
        StringBuilder header = new StringBuilder(70);
        header.append("SenderID=").append(divId).append(System.lineSeparator());
        header.append("CreateDate=").append(dateFormat.format(currDate)).append(System.lineSeparator());
        header.append("CreateTime=").append(timeFormat.format(currDate)).append(System.lineSeparator());
        header.append("EMAIL=").append(email).append(System.lineSeparator());
        header.append("~~").append(System.lineSeparator());
        header.append("DATATYPE=");
        if ("1".equals(m_submission)) {
            header.append("MSC_FALL").append(System.lineSeparator());
        } else if ("3".equals(m_submission)) {
            header.append("MSC_EOY").append(System.lineSeparator());
        }
        header.append("~").append(System.lineSeparator());
        // A record.
        header.append("AMSC_IPAL").append(m_submission).append(Integer.toString(year)).append(divisionNumber)
                .append(section).append(System.lineSeparator());
        return header.toString();
    }

    /**
     * Return a custom trailer line.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getTrailer()
     */
    @Override
    public String getTrailer() {
        StringBuilder trailer = new StringBuilder(30);
        trailer.append("RecordCount=").append(Integer.toString(m_rowCount)).append(System.lineSeparator());
        return trailer.toString();
    }

    /**
     * Initialize date export object.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        m_numericGradeLevelMap = StudentManager.buildNumericGradeLevelMap(getBroker());
        m_submission = (String) getParameter(PARAM_SUBMISSION);
        m_eoyBehavior = (String) getParameter(PARAM_EOY_BEHAVIOR);
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        m_schoolYear = getOrganization().getCurrentContext().getSchoolYear();
        m_scheduleManager = new ScheduleManager(getBroker());
        m_dateAsStringConverter =
                (DateAsStringConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER,
                        LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()),
                        true);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(m_reportDate);
        calendar.add(Calendar.YEAR, -1);
        m_reportDate1YearBefore = calendar.getTime();
        initializeFields();
        // Count of rows processed. Initially 1 for the A record in the heading.
        m_rowCount = 1;
        if (getSetupErrors().size() == 0) {
            // build the query for students to report.
            Criteria criteria = new Criteria();
            criteria.addEqualTo(X2BaseBean.COL_OID, getOrganization().getOid());
            QueryByCriteria query = new QueryByCriteria(Organization.class, criteria);
            setQuery(query);
            setEntityClass(MasterScheduleEntity.class);
            loadTeacherCodes();
            loadTeacherMaps();
            // If reporting student schedules, preload and build all student
            // schedule information:
            // Student Schedule, Student Schedule Change, Transcript.
            // Schedule and Teacher Schedule depend on knowing what classes have
            // students, so
            // Load students schedules for those also.
            String sections = (String) getParameter(PARAM_SECTION);
            // Load staff positions map.
            if (sections.contains("B")) {
                loadStaffPositions();
            }
            if (sections.contains("B") || sections.contains("C") || sections.contains("D") || sections.contains("F")
                    || sections.contains("I") || sections.contains("J")) {
                // Alternate EOY behavior does not use schedules, just
                // transcripts.
                if ("1".equals(m_submission) || "ssc".equals(m_eoyBehavior)) // FALL
                // Submission
                // or
                // EOY
                // +
                // Schedule
                // behavior
                {
                    loadStudentSchedules(true);
                    loadStudentScheduleChanges();
                } else {
                    // m_studentScheduleMap = new HashMap<String,
                    // List<StudentSchedule>>();
                    loadStudentSchedules(false);
                    m_scheduleChangeMap = new HashMap<String, List<StudentScheduleChange>>();
                }
                loadTranscripts();
                buildStudentScheduleInfo();
            }

            // load information for student coop
            if (sections.contains("J")) {
                loadStudentMapForReportDate();
            }
            // load information for sequenced courses
            if (sections.contains("I")) {
                loadConnectedSectionsMap();
            }
            // Load staff certification map.
            if (sections.contains("B") || sections.contains("D") || sections.contains("G")) {
                loadCertifications();
            }
            // Add any retrievers or validators.
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("MSC-BLANK", new RetrieveBlankIfNotEmpty());
            calcs.put("MSC-CERT", new RetrieveCertification());
            calcs.put("MSC-COOP", new RetrieveCoopCounts());
            calcs.put("MSC-COURSE", new RetrieveCourseOverride());
            calcs.put("MSC-FINAL-GRADE", new RetrieveFinalGrade());
            calcs.put("MSC-MINUTES", new RetrieveCourseMinutes());
            calcs.put("MSC-RACE", new RetrieveRace());
            calcs.put("MSC-PREFIX", new RetrievePrefix());
            calcs.put("MSC-SECTION", new RetrieveSectionIDs());
            calcs.put("MSC-STAFF", new RetrieveStaffOverride());
            calcs.put("MSC-STAFF-ID", new RetrieveStaffID());
            calcs.put("MSC-FIRST-YEAR", new RetrieveFirstYear());
            calcs.put("MSC-SCED-CODE", new RetrieveScedCode());
            calcs.put("MSC-CREDIT", new RetrieveCredit());
            calcs.put(RetrieveLocalCrsNumber.CALC_ID, new RetrieveLocalCrsNumber());
            calcs.put(Retrieve50CourseEnrFlag.CALC_ID, new Retrieve50CourseEnrFlag());
            super.addCalcs(calcs);
            Map<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put("MSC-MST-ENTITY", new ValidateMasterScheduleEntity());
            super.addValidators(validators);
        }
    }

    /**
     * Returns only teacher sections with correct staff oid.
     *
     * @param teacherSections Collection<ScheduleTeacher>
     * @return Collection
     */
    protected static Collection<ScheduleTeacher> getCorrectTeacherSections(Collection<ScheduleTeacher> teacherSections) {
        Iterator<ScheduleTeacher> iterator = teacherSections.iterator();
        while (iterator.hasNext()) {
            ScheduleTeacher teacherSection = iterator.next();
            if (teacherSection.getStaff() == null) {
                iterator.remove();
            }
        }
        return teacherSections;
    }

    /**
     * Gather the District Id for the Organization if it hasn't already been populated.
     *
     * @return String
     */
    protected String getOrganizationDistrictId() {
        if (StringUtils.isEmpty(m_districtId)) {
            m_districtId = (String) getOrganization().getFieldValueByBeanPath(m_fieldDistrictId);
        }
        return m_districtId;
    }

    /**
     * Gather the SchoolId alias by MasterSchedule.
     *
     * @param section MasterSchedule
     * @return String
     */
    protected String getSchoolIdAlias(MasterSchedule section) {
        String schoolId = m_schoolIdAliasBySchoolCourseOid.get(section.getSchoolCourseOid());
        if (StringUtils.isEmpty(schoolId)) {
            schoolId = (String) section.getSchoolCourse().getSchool().getFieldValueByBeanPath(m_fieldSchoolId);
            m_schoolIdAliasBySchoolCourseOid.put(section.getSchoolCourseOid(), schoolId);
        }
        return schoolId;
    }

    /**
     * Gather the SchoolId by MasterSchedule.
     *
     * @param section MasterSchedule
     * @return String
     */
    protected String getSchoolId(MasterSchedule section) {
        String schoolId = m_schoolIdsBySchoolCourseOid.get(section.getSchoolCourseOid());
        if (StringUtils.isEmpty(schoolId)) {
            schoolId = section.getSchoolCourse().getSchool().getSchoolId();
            m_schoolIdsBySchoolCourseOid.put(section.getSchoolCourseOid(), schoolId);
        }
        return schoolId;
    }

    /**
     * Gather the Staff by ScheduleTeacher.
     *
     * @param st ScheduleTeacher
     * @return Staff
     */
    protected Staff getStaff(ScheduleTeacher st) {
        Staff staff = m_staffByScheduleTeacher.get(st.getStaffOid());
        if (staff == null) {
            staff = st.getStaff();
            m_staffByScheduleTeacher.put(st.getStaffOid(), staff);
        }
        return staff;
    }

    /**
     * Load maps for race codes reference codes and person race for students.
     *
     * @param staffCriteria Criteria
     */
    protected void loadRaceCodes(Criteria staffCriteria) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);
        // Get race code reference codes for use in the race retriever.
        Criteria raceCriteria = new Criteria();
        raceCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, raceCriteria);
        m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 5);
        // Load the race codes for all students included in the export.
        SubQuery subQuery = new SubQuery(Staff.class, Staff.COL_PERSON_OID, staffCriteria);
        raceCriteria = new Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID, subQuery);
        QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
        m_raceCodeMap.putAll(getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 100));
    }

    /**
     * Load maps for race codes reference codes and person race for students.
     */
    protected void loadTeacherCodes() {
        String javaname = translateAliasToJavaName(ALIAS_TEACHER_ONE, true);
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(StudentSchedule.class.getName(), javaname);
        if (field != null) {
            ReferenceTable refTbl = field.getReferenceTable();
            if (refTbl != null) {
                for (ReferenceCode refCode : refTbl.getReferenceCodes()) {
                    m_teacherCodeMap.put(refCode.getCode(), refCode.getLocalCode());
                }
            }
        }
    }

    /**
     * We need to know when there are additional teaches in a single section. We
     * go through the students schedules to see if they have the UDF filled in,
     * then add it to a map for later use.
     *
     * @param schedule StudentSchedule
     */
    private void addAdditionalTeachersToMap(StudentSchedule schedule) {
        ArrayList<String> additionalTeachers = new ArrayList<String>();
        if (schedule.getFieldValueByAlias(ALIAS_TEACHER_ONE) != null) {
            additionalTeachers.add((String) schedule.getFieldValueByAlias(ALIAS_TEACHER_ONE));
        }
        if (schedule.getFieldValueByAlias(ALIAS_TEACHER_TWO) != null) {
            additionalTeachers.add((String) schedule.getFieldValueByAlias(ALIAS_TEACHER_TWO));
        }
        if (additionalTeachers.size() > 0) {
            m_sectionsWithMultipleTeachers.put(schedule.getSectionOid(), additionalTeachers);
        }
    }

    /**
     * Returns a concatenation of the passed strings with a dash in between them.
     *
     * @param str1 String
     * @param str2 String
     * @param str3 String
     * @return String
     */
    private String buildKey(String str1, String str2, String str3) {
        return str1 + "-" + str2 + "-" + str3;
    }

    /**
     * Build all student schedule information from the schedule data available.
     * 1. Include existing Student Schedule. 2. Use Student Schedule Change to
     * build additional Student Schedule for past or dropped classes. 3. Use
     * Transcript to attach to Student Schedule, or build more if necessary.
     *
     * Result: A List of objects containing a Student Schedule, and potentially
     * a Transcript.
     *
     * @throws X2BaseException exception
     */
    private void buildStudentScheduleInfo() throws X2BaseException {
        excludeSchedulesWithExcludedEnr();
        Set<String> studentOids = new HashSet<String>(m_studentScheduleMap.size());
        studentOids.addAll(m_studentScheduleMap.keySet());
        studentOids.addAll(m_scheduleChangeMap.keySet());
        studentOids.addAll(m_transcriptMap.keySet());
        /*
         * Build a set of student OID's who were enrolled in district as of report date.
         */
        Set<String> enrolledStdOidsAsOfReportDate = new HashSet<String>();
        if (isSchoolContext()) {
            Map<String, Collection<StudentSchool>> secondarySchools = getSecondarySchools(studentOids);

            Set<String> secondaryStudentOids = new HashSet<String>();
            for (String studentOid : studentOids) {
                Collection<StudentSchool> secondarySchoolCollection = secondarySchools.get(studentOid);
                if (secondarySchoolCollection != null && secondarySchoolCollection.size() != 0) {
                    for (StudentSchool secondarySchool : secondarySchoolCollection) {
                        if (secondarySchool.getType() == 1) {
                            secondaryStudentOids.add(studentOid);
                        }
                    }
                }
            }
            enrolledStdOidsAsOfReportDate =
                    m_enrollmentManager.getMembershipAsOf(m_reportDate, ((SisSchool) getSchool()));
            enrolledStdOidsAsOfReportDate.addAll(secondaryStudentOids);
        } else {
            enrolledStdOidsAsOfReportDate = m_enrollmentManager.getMembershipAsOf(m_reportDate, getOrganization());
        }
        // Exclude students who were not active as of report date.
        studentOids.retainAll(enrolledStdOidsAsOfReportDate);
        m_FRecordSectionOids = new HashSet<String>();
        m_scheduleInfoMap = new HashMap<String, Map<String, ScheduleInfo>>();
        m_sectionsWithStudents = new HashSet<String>();
        m_sectionsWithMultipleTeachers = new HashMap<String, ArrayList<String>>();
        for (String studentOid : studentOids) {
            HashMap<String, ScheduleInfo> scheduleInfoMap = new HashMap<String, ScheduleInfo>();
            List<StudentSchedule> studentSchedules = m_studentScheduleMap.remove(studentOid);
            List<StudentScheduleChange> scheduleChanges = m_scheduleChangeMap.remove(studentOid);
            List<Transcript> transcripts = m_transcriptMap.remove(studentOid);
            // Add current schedule sections to the map.
            if (studentSchedules != null) {
                for (StudentSchedule studentSchedule : studentSchedules) {
                    if (isIncludeSection(studentSchedule.getSection())) {
                        ScheduleInfo info = new ScheduleInfo();
                        info.m_schedule = studentSchedule;
                        info.m_section = studentSchedule.getSection();
                        info.m_withdrew = false;
                        scheduleInfoMap.put(studentSchedule.getSectionOid(), info);
                        addAdditionalTeachersToMap(studentSchedule);
                    }
                }
            }
            // Add reportable dropped schedule add/drop sections to the map.
            if (scheduleChanges != null) {
                checkScheduleChanges(scheduleChanges, scheduleInfoMap);
            }
            // Fill in any empty entry/exit dates with term dates for the
            // section term.
            fillTermDates(scheduleInfoMap);
            // Filter out spans that are less than 20 days in attendance.
            // filter20Days(scheduleInfoMap);
            // Match transcript to schedule on master schedule oid.
            // Add transcript as individual row if the section is not there.
            if (transcripts != null) {
                for (Transcript trn : transcripts) {
                    if (isIncludeSection(trn.getMasterSchedule())) {
                        /*
                         * For each transcript, find a scheduleMap entry with the
                         * same section.
                         */
                        ScheduleInfo info = scheduleInfoMap.get(trn.getMasterScheduleOid());
                        if ((info != null) && (info.m_transcript == null)) {
                            info.m_transcript = trn;
                        } else {
                            // Add the transcript.
                            // Add transcripts for classes the student has taken
                            // and received any grade.
                            // This case is for EOY, and can come up for a
                            // student whose schedule was dropped without
                            // record.
                            StudentSchedule ssc = X2BaseBean.newInstance(StudentSchedule.class,
                                    getBroker().getPersistenceKey());
                            ssc.setStudentOid(trn.getStudentOid());
                            ssc.setSectionOid(trn.getMasterScheduleOid());
                            info = new ScheduleInfo();
                            info.m_section = trn.getMasterSchedule();
                            info.m_transcript = trn;
                            info.m_schedule = ssc;
                            scheduleInfoMap.put(trn.getMasterScheduleOid(), info);
                        }
                    }
                }
            }
            m_scheduleInfoMap.put(studentOid, scheduleInfoMap);
            // Load a set of section oids that have student schedules on them.
            if ((m_paramRequireStudents != null) && m_paramRequireStudents.booleanValue()) {
                m_sectionsWithStudents.addAll(scheduleInfoMap.keySet());
            }
        }
    }

    /**
     * Search through schedule change records for alternate start dates and
     * other sections to report.
     *
     * Loop through schedule change records for each section, in date order. If
     * a section is found that is not already in the students schedule map,
     * check it's chain of change records against the report dates and its term
     * dates. If it was in session on a report date or at the end of its term,
     * include it.
     *
     * Some will have been dropped after the end of a term but should still be
     * counted.
     *
     * @param scheduleChanges Collection<StudentScheduleChange>
     * @param scheduleMap Map<String,ScheduleInfo>
     * @throws X2BaseException exception
     */
    private void checkScheduleChanges(Collection<StudentScheduleChange> scheduleChanges,
                                      Map<String, ScheduleInfo> scheduleMap)
            throws X2BaseException {
        MasterSchedule lastSection = null;
        StudentScheduleChange lastChange = null;
        PlainDate termStart = null;
        PlainDate termEnd = null;

        /*
         * Iterate over the collection once and remove the sections where a
         * staff is flagged as exclude from reporting.
         */
        Collection<StudentScheduleChange> changesToRemove = new HashSet<StudentScheduleChange>();
        for (StudentScheduleChange change : scheduleChanges) {
            if (!isIncludeSection(change.getMasterSchedule())) {
                changesToRemove.add(change);
            }
        }
        scheduleChanges.removeAll(changesToRemove);

        /*
         * Work backward in time through schedule changes. DROP will open a new
         * section and the ADD before it will finish that section. A DROP
         * without a following ADD will be considered open at start of term. Any
         * activity entirely before start of term will be ignored.
         */
        for (StudentScheduleChange change : scheduleChanges) {
            // Check for a new section.
            if ((lastSection == null) || !lastSection.getOid().equals(change.getMasterScheduleOid())) {
                // Save the working section if necessary.
                if (lastChange != null) {
                    // The last change record for this section (in reverse
                    // chronological order)
                    // was a drop. Assume the section was scheduled from the
                    // beginning of the term/year.
                    StudentSchedule ssc = X2BaseBean.newInstance(StudentSchedule.class,
                            getBroker().getPersistenceKey());
                    ssc.setStudentOid(lastChange.getStudentOid());
                    ssc.setSectionOid(lastSection.getOid());
                    ssc.setScheduleOid(lastChange.getScheduleOid());
                    ScheduleInfo info = new ScheduleInfo();
                    info.m_section = lastSection;
                    info.m_schedule = ssc;
                    info.m_entryDate = termStart;
                    PlainDate effectiveDate = lastChange.getEffectiveDate();
                    if (effectiveDate == null) {
                        effectiveDate = lastChange.getDate();
                    }
                    if (effectiveDate.after(termEnd)) {
                        info.m_exitDate = termEnd;
                        info.m_withdrew = false;
                    } else {
                        info.m_exitDate = effectiveDate;
                        info.m_withdrew = true;
                    }
                    // Avoid recording sections scheduled out entirely
                    // before the start of it's term. This is just scheduling
                    // activity.
                    if (!info.m_exitDate.before(termStart)) {
                        if (scheduleMap.keySet().contains(lastSection.getOid())) {
                            // Merge, keep the best dates.
                            ScheduleInfo prevInfo = scheduleMap.get(lastSection.getOid());
                            if ((prevInfo.m_entryDate == null) ||
                                    ((info.m_entryDate != null) && prevInfo.m_entryDate.after(info.m_entryDate))) {
                                prevInfo.m_entryDate = info.m_entryDate;
                            }
                            if ((prevInfo.m_exitDate == null) ||
                                    ((info.m_exitDate != null) && prevInfo.m_exitDate.before(info.m_exitDate))) {
                                prevInfo.m_exitDate = info.m_exitDate;
                            }
                        } else {
                            scheduleMap.put(lastSection.getOid(), info);
                            addAdditionalTeachersToMap(info.m_schedule);
                        }
                    }
                }
                // Initialize the new section
                lastChange = null;
                lastSection = change.getMasterSchedule();
                termStart = null;
                termEnd = null;
                Collection<ScheduleTermDate> termDates = getTermDates(lastSection.getScheduleTermOid());
                for (ScheduleTermDate termDate : termDates) {
                    if ((termStart == null) || termStart.after(termDate.getStartDate())) {
                        termStart = termDate.getStartDate();
                    }
                    if ((termEnd == null) || termEnd.before(termDate.getEndDate())) {
                        termEnd = termDate.getEndDate();
                    }
                }
            }
            // For a section, see if its dates compare with report dates or term
            // dates.
            if (StudentScheduleChange.CODE_DROP.equals(change.getChangeTypeCode())) {
                lastChange = change;
            } else if (StudentScheduleChange.CODE_ADD.equals(change.getChangeTypeCode())) {
                if (lastChange == null) {
                    // No previous record, assume current student schedule. Find
                    // based on master OID.
                    ScheduleInfo info = null;
                    for (ScheduleInfo sinfo : scheduleMap.values()) {
                        if ((sinfo.m_section != null) &&
                                sinfo.m_section.getOid().equals(change.getMasterScheduleOid()) &&
                                (sinfo.m_entryDate == null)) {
                            info = sinfo;
                            break;
                        }
                    }
                    if (info != null) {
                        PlainDate effectiveDate = change.getEffectiveDate();
                        if (effectiveDate == null) {
                            effectiveDate = change.getDate();
                        }
                        if ((info.m_entryDate == null) || info.m_entryDate.after(effectiveDate)) {
                            info.m_entryDate = effectiveDate;
                        }
                        if (termStart != null && info.m_entryDate.before(termStart)) {
                            info.m_entryDate = termStart;
                        }
                    }
                } else {
                    StudentSchedule ssc = X2BaseBean.newInstance(StudentSchedule.class,
                            getBroker().getPersistenceKey());
                    ssc.setStudentOid(change.getStudentOid());
                    ssc.setSectionOid(lastSection.getOid());
                    ssc.setScheduleOid(change.getScheduleOid());
                    ScheduleInfo info = new ScheduleInfo();
                    info.m_section = change.getMasterSchedule();
                    info.m_schedule = ssc;
                    info.m_entryDate = change.getEffectiveDate();
                    if (info.m_entryDate == null) {
                        info.m_entryDate = change.getDate();
                    }
                    if (info.m_entryDate != null && termStart != null && info.m_entryDate.before(termStart)) {
                        info.m_entryDate = termStart;
                    }
                    info.m_exitDate = lastChange.getEffectiveDate();
                    if (info.m_exitDate == null) {
                        info.m_exitDate = lastChange.getDate();
                    }
                    info.m_withdrew = true;
                    // Avoid recording sections scheduled out entirely before
                    // the start of it's term.
                    // This is just scheduling activity.
                    if (info.m_exitDate != null && termStart != null &&
                            !info.m_exitDate.before(termStart)
                            && !scheduleMap.keySet().contains(lastSection.getOid())) {
                        if (scheduleMap.keySet().contains(lastSection.getOid())) {
                            // Merge, keep the best dates.
                            ScheduleInfo prevInfo = scheduleMap.get(lastSection.getOid());
                            if ((prevInfo.m_entryDate == null) ||
                                    ((info.m_entryDate != null) && prevInfo.m_entryDate.after(info.m_entryDate))) {
                                prevInfo.m_entryDate = info.m_entryDate;
                            }
                            if ((prevInfo.m_exitDate == null) ||
                                    ((info.m_exitDate != null) && prevInfo.m_exitDate.before(info.m_exitDate))) {
                                prevInfo.m_exitDate = info.m_exitDate;
                            }
                        } else {
                            scheduleMap.put(lastSection.getOid(), info);
                            addAdditionalTeachersToMap(info.m_schedule);
                        }
                    }
                }
                lastChange = null;
            }
        }
        if (lastChange != null) {
            // The last change record for this section (in reverse chronological
            // order)
            // was a drop. Assume the section was scheduled from the beginning
            // of the term/year.
            StudentSchedule ssc = X2BaseBean.newInstance(StudentSchedule.class,
                    getBroker().getPersistenceKey());
            ssc.setStudentOid(lastChange.getStudentOid());
            ssc.setSectionOid(lastSection.getOid());
            ssc.setScheduleOid(lastChange.getScheduleOid());
            ScheduleInfo info = new ScheduleInfo();
            info.m_section = lastSection;
            info.m_schedule = ssc;
            info.m_entryDate = termStart;
            PlainDate effectiveDate = lastChange.getEffectiveDate();
            if (effectiveDate == null) {
                effectiveDate = lastChange.getDate();
            }
            if (effectiveDate.after(termEnd)) {
                info.m_exitDate = termEnd;
                info.m_withdrew = false;
            } else {
                info.m_exitDate = effectiveDate;
                info.m_withdrew = true;
            }
            // Avoid recording sections scheduled out entirely
            // before the start of it's term. This is just scheduling activity.
            if (!info.m_exitDate.before(termStart) && !scheduleMap.keySet().contains(lastSection.getOid())) {
                if (scheduleMap.keySet().contains(lastSection.getOid())) {
                    // Merge, keep the best dates.
                    ScheduleInfo prevInfo = scheduleMap.get(lastSection.getOid());
                    if ((prevInfo.m_entryDate == null) ||
                            ((info.m_entryDate != null) && prevInfo.m_entryDate.after(info.m_entryDate))) {
                        prevInfo.m_entryDate = info.m_entryDate;
                    }
                    if ((prevInfo.m_exitDate == null) ||
                            ((info.m_exitDate != null) && prevInfo.m_exitDate.before(info.m_exitDate))) {
                        prevInfo.m_exitDate = info.m_exitDate;
                    }
                } else {
                    scheduleMap.put(lastSection.getOid(), info);
                    addAdditionalTeachersToMap(info.m_schedule);
                }
            }
        }
    }

    /**
     * Exclude schedules with students that have enrollment flagged to exclude from reporting.
     */
    private void excludeSchedulesWithExcludedEnr() {
        Set<String> studentOids = new HashSet<String>(m_studentScheduleMap.size());
        studentOids.addAll(m_studentScheduleMap.keySet());
        studentOids.addAll(m_scheduleChangeMap.keySet());
        studentOids.addAll(m_transcriptMap.keySet());
        Map<String, Collection<String>> schoolsToExcludeByEnr = new HashMap<String, Collection<String>>();
        List<String> enrollmentTypes = Arrays.asList(StudentEnrollment.ENTRY);
        Criteria criteria = new Criteria();
        if (!studentOids.isEmpty()) {
            criteria.addIn(StudentEnrollment.COL_STUDENT_OID, studentOids);
        } else {
            criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, "___dummy___");
        }
        criteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, enrollmentTypes);
        criteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_reportDate);
        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
        query.addOrderBy(StudentEnrollment.COL_STUDENT_OID, true);
        query.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, false);
        query.addOrderBy(StudentEnrollment.COL_TIMESTAMP, false);
        Map<String, Map<String, LinkedList<StudentEnrollment>>> enrollmentMap =
                getBroker().getGroupedCollectionByQuery(query,
                        new String[] {StudentEnrollment.COL_STUDENT_OID, StudentEnrollment.COL_SCHOOL_OID},
                        new int[] {256, 256});
        if (!enrollmentMap.isEmpty()) {
            for (Entry<String, Map<String, LinkedList<StudentEnrollment>>> entry : enrollmentMap.entrySet()) {
                Collection<String> sklOidsToExclude = new ArrayList<>();
                Collection<String> sklOidsEnrolled = new ArrayList<>();
                schoolsToExcludeByEnr.put(entry.getKey(), sklOidsToExclude);
                Map<String, LinkedList<StudentEnrollment>> enrMapToExclude = entry.getValue();
                if (enrMapToExclude != null) {
                    for (Entry<String, LinkedList<StudentEnrollment>> entryToOperate : enrMapToExclude.entrySet()) {
                        String sklOid = entryToOperate.getKey();
                        StudentEnrollment enrToOperate = entryToOperate.getValue().get(0);
                        if (BooleanAsStringConverter.TRUE
                                .equals(enrToOperate.getFieldValueByBeanPath(m_fieldEnrExcludeEnr))) {
                            if (!sklOidsToExclude.contains(sklOid) && !sklOidsEnrolled.contains(sklOid)) {
                                sklOidsToExclude.add(sklOid);
                            }
                        } else {
                            sklOidsEnrolled.add(sklOid);
                        }
                    }
                }
            }
        }
        if (!schoolsToExcludeByEnr.isEmpty()) {
            for (Entry<String, Collection<String>> entry : schoolsToExcludeByEnr.entrySet()) {
                String stdOid = entry.getKey();
                Collection<String> sklOids = entry.getValue();
                Collection<StudentSchedule> schedules = m_studentScheduleMap.get(stdOid);
                if (schedules != null) {
                    Iterator<StudentSchedule> schIter = schedules.iterator();
                    while (schIter.hasNext()) {
                        StudentSchedule sch = schIter.next();
                        if (sklOids.contains(sch.getSchedule().getSchoolOid())) {
                            schIter.remove();
                        }
                    }
                }
                Collection<StudentScheduleChange> scheduleChanges = m_scheduleChangeMap.get(stdOid);
                if (scheduleChanges != null) {
                    Iterator<StudentScheduleChange> sccIter = scheduleChanges.iterator();
                    while (sccIter.hasNext()) {
                        StudentScheduleChange scc = sccIter.next();
                        if (sklOids.contains(scc.getSchedule().getSchoolOid())) {
                            sccIter.remove();
                        }
                    }
                }
                Collection<Transcript> transcripts = m_transcriptMap.get(stdOid);
                if (transcripts != null) {
                    Iterator<Transcript> trnIter = transcripts.iterator();
                    while (trnIter.hasNext()) {
                        Transcript trn = trnIter.next();
                        if (sklOids.contains(trn.getSchoolOid())) {
                            trnIter.remove();
                        }
                    }
                }
            }
        }
    }

    /**
     * For all populated sections, if the entry date or exit date is missing,
     * populate with term dates.
     *
     * @param scheduleMap Map<String,ScheduleInfo>
     */
    private void fillTermDates(Map<String, ScheduleInfo> scheduleMap) {
        PlainDate termStart = null;
        PlainDate termEnd = null;
        Iterator<ScheduleInfo> iterator = scheduleMap.values().iterator();
        while (iterator.hasNext()) {
            ScheduleInfo info = iterator.next();
            if ((info.m_entryDate == null) || (info.m_exitDate == null)) {
                termStart = null;
                termEnd = null;
                Collection<ScheduleTermDate> termDates = getTermDates(info.m_section.getScheduleTermOid());
                for (ScheduleTermDate termDate : termDates) {
                    if ((termStart == null) || termStart.after(termDate.getStartDate())) {
                        termStart = termDate.getStartDate();
                    }
                    if ((termEnd == null) || termEnd.before(termDate.getEndDate())) {
                        termEnd = termDate.getEndDate();
                    }
                }

                if (info.m_entryDate == null) {
                    info.m_entryDate = termStart;
                }
                if (info.m_exitDate == null) {
                    info.m_exitDate = termEnd;
                }
                /*
                 * If the entry/exit dates are out of order, remove the entry.
                 * This can be caused by drop/re-add after the end of term. The
                 * original entry will exist before the drop, so this record is
                 * extra.
                 */
                if (info.m_exitDate != null && info.m_entryDate != null && info.m_exitDate.before(info.m_entryDate)) {
                    iterator.remove();
                }
            }
        }
    }

    /**
     * Gets the secondary schools.
     *
     * @param studentOids Collection<String>
     * @return Map
     */
    private Map<String, Collection<StudentSchool>> getSecondarySchools(Collection<String> studentOids) {
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addIn(StudentSchool.COL_STUDENT_OID, studentOids);
        QueryByCriteria query = new QueryByCriteria(StudentSchool.class, schoolCriteria);
        m_studentSchoolMap = getBroker().getGroupedCollectionByQuery(query, StudentSchool.COL_STUDENT_OID, 128);
        return m_studentSchoolMap;
    }

    /**
     * Gets the session dates by section.
     *
     * @param section MasterSchedule
     * @return Collection
     */
    private List<PlainDate> getSessionDatesBySection(MasterSchedule section) {
        List<PlainDate> sessionDatesToReturn = m_sessionDatesBySeciton.get(section.getOid());
        if (sessionDatesToReturn == null || sessionDatesToReturn.isEmpty()) {
            sessionDatesToReturn = new ArrayList<>();
            sessionDatesToReturn.addAll(m_scheduleManager.getSectionMeetingDates(section));
            Collections.sort(sessionDatesToReturn);
            m_sessionDatesBySeciton.put(section.getOid(), sessionDatesToReturn);
        }
        return sessionDatesToReturn;
    }

    /**
     * Load the schedule term dates for a schedule term oid. Keep a map of
     * existing codes for lookup.
     *
     * @param scheduleTermOid String
     * @return Collection<ScheduleTermDate>
     */
    private Collection<ScheduleTermDate> getTermDates(String scheduleTermOid) {
        Collection<ScheduleTermDate> dates = null;
        if (m_termDateMap == null) {
            m_termDateMap = new HashMap<String, Collection<ScheduleTermDate>>();
        }
        if (!m_termDateMap.containsKey(scheduleTermOid)) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ScheduleTermDate.COL_SCHEDULE_TERM_OID, scheduleTermOid);
            QueryByCriteria query = new QueryByCriteria(ScheduleTermDate.class, criteria);
            dates = getBroker().getCollectionByQuery(query);
            m_termDateMap.put(scheduleTermOid, dates);
        }
        return m_termDateMap.get(scheduleTermOid);
    }

    /**
     * Initialize aliases and other report data.
     */
    private void initializeFields() {
        m_fieldAnniversaryDate = translateAliasToJavaName(ALIAS_ANNIVERSARY_DATE, true);
        m_fieldDistrictId = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_fieldSchoolId = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldStaffProviderType = translateAliasToJavaName(ALIAS_STAFF_PROVIDER_TYPE, true);
        m_fieldMTCStartDate = translateAliasToJavaName(ALIAS_MTC_START_DATE, true);
        m_fieldMTCEndDate = translateAliasToJavaName(ALIAS_MTC_END_DATE, true);
        m_fieldMTCisInterdisciplinary = translateAliasToJavaName(ALIAS_MST_INTERDISCIPLINARY, true);
        m_fieldExcludeStd = translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);
        m_fieldExcludeStf = translateAliasToJavaName(ALIAS_EXCLUDE_STF, false);
        m_fieldExcludeCrs = translateAliasToJavaName(ALIAS_EXCLUDE_CRS, false);
        m_fieldEnrExcludeEnr = translateAliasToJavaName(ALIAS_ENR_EXCLUDE_ENR, true);
        m_fieldExcludeMst = translateAliasToJavaName(ALIAS_EXCLUDE_MST, false);
        m_fieldCrsMopId = translateAliasToJavaName(ALIAS_CRS_MOPID, true);
        m_fieldExcludeMstFromConnected = translateAliasToJavaName(ALIAS_EXCLUDE_MST_FROM_CONNECTED, false);
        m_fieldExcludeMtc = translateAliasToJavaName(ALIAS_EXCLUDE_MTC, false);
        m_fieldScedCourse = translateAliasToJavaName(ALIAS_SCED_COURSE, false);
        m_fieldScedSubject = translateAliasToJavaName(ALIAS_SCED_SUBJECT, false);
        m_javanameSedfReport = translateAliasToJavaName(ALIAS_SEDF_REPORT_FLAG, true);
        m_fieldSequencedCourse = translateAliasToJavaName(ALIAS_SEQUENCED_COURSE, true);
        m_fieldWorkBasedLearningCode = translateAliasToJavaName(ALIAS_WORK_BASED_LEARNING_CODE, true);
        m_fieldAdditionalStaff1 = translateAliasToJavaName(ALIAS_TEACHER_ONE, true);
        m_fieldAdditionalStaff2 = translateAliasToJavaName(ALIAS_TEACHER_TWO, true);
        m_fieldPrimaryDisability = translateAliasToJavaName(ALIAS_PRIMARY_DISABILITY, true);
        m_fieldDefinedClassType = translateAliasToJavaName(ALIAS_DEFINED_CLASS_TYPE, true);
        m_paramRequireStudents = (Boolean) getParameter(PARAM_REQUIRE_STUDENT);
        m_paramRequireSubject = (Boolean) getParameter(PARAM_REQUIRE_SUBJECT);
        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        m_schoolsToCalendars = new HashMap();

        /*
         * Lookup the staff type codes for teaching staff. These will have a
         * staff type reference code with the letter "B" in the local code.
         */
        m_teachingStaffCodesList = new ArrayList();
        m_adminStaffCodesList = new ArrayList();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(Staff.class.getName(), Staff.COL_STAFF_TYPE);
        if (field != null) {
            ReferenceTable refTable = field.getReferenceTable();
            if (refTable != null) {
                for (ReferenceCode code : refTable.getReferenceCodes()) {
                    // Teaching staff
                    if ("B".equals(code.getLocalCode())) {
                        m_teachingStaffCodesList.add(code.getCode());
                    }
                    // Administrative and service staff
                    if ("7".equals(code.getStateCode()) || "8".equals(code.getStateCode())) {
                        m_adminStaffCodesList.add(code.getCode());
                    }
                }
            }
        }
        if (m_teachingStaffCodesList.size() == 0) {
            addSetupError("Staff Type Codes reference table",
                    "No staff type code contains local code 'B' for teaching staff.");
        }

        X2Criteria refTableCriteria = new X2Criteria();
        refTableCriteria.addEqualTo(ReferenceTable.COL_USER_NAME, RTB_NAME_MULTI_GRADE_CROSS);
        ReferenceTable refTable = (ReferenceTable) getBroker()
                .getBeanByQuery(new QueryByCriteria(ReferenceTable.class, refTableCriteria));
        if (refTable != null) {
            m_refTableMultiGradeCrossCodes = refTable.getCodeMap();
            for (String code : m_refTableMultiGradeCrossCodes.keySet()) {
                if (code.split("-").length > 0) {
                    m_refTableMultiGradeCrossSplittedCodes.add(code.split("-")[0]);
                }
            }
        }
    }

    /**
     * determine if work based learning code is a coop code.
     *
     * @param code String
     * @return true, if is coop code
     */
    private boolean isCoopCode(String code) {
        if (m_coopCodes == null) {
            m_coopCodes = new HashSet();
            DataDictionaryField dictionaryField =
                    getDataDictionaryField(StudentSchedule.class, m_fieldWorkBasedLearningCode);
            if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
                for (ReferenceCode item : getReferenceCodes(dictionaryField.getReferenceTableOid()).values()) {
                    if (COOP_CODE.equals(item.getStateCode())) {
                        m_coopCodes.add(item.getCode());
                    }
                }
            }
        }
        return m_coopCodes.contains(code);
    }

    /**
     * Determines whether to include a section by checking the exclude from reporting flag on staff
     * record.
     * If there is only one teacher teaching a section and is flagged as exclude from reporting, do
     * not include the section.
     * If there are multiple teachers, and at least one teacher DOES NOT have the flag set, then
     * include the section.
     *
     * @param section MasterSchedule
     * @return boolean
     */
    private boolean isIncludeSection(MasterSchedule section) {
        boolean includeSection = false;
        Collection<ScheduleTeacher> teacherSections =
                getCorrectTeacherSections(section.getTeacherSections(getBroker()));
        if (!CollectionUtils.isEmpty(teacherSections)) {
            for (ScheduleTeacher teacherSection : teacherSections) {
                SisStaff staff = (SisStaff) getStaff(teacherSection);
                String excludeStaff = (String) staff.getFieldValueByBeanPath(m_fieldExcludeStf);
                if (!BooleanAsStringConverter.TRUE.equals(excludeStaff)) {
                    includeSection = true;
                    break;
                }
            }
        }
        return includeSection;
    }

    /**
     * Load primary Staff Certifications into a map by staff Oid.
     */
    private void loadCertifications() {
        // Map<String, StaffCertification> m_certificates;
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StaffCertification.COL_PRIMARY_INDICATOR, Boolean.TRUE);
        criteria.addLessOrEqualThan(StaffCertification.COL_ISSUE_DATE, m_reportDate);
        // Check expire date.
        Criteria criteria1 = new Criteria();
        Criteria criteria2 = new Criteria();
        criteria1.addGreaterOrEqualThan(StaffCertification.COL_EXPIRATION_DATE, m_reportDate);
        criteria2.addIsNull(StaffCertification.COL_EXPIRATION_DATE);
        criteria1.addOrCriteria(criteria2);
        criteria.addAndCriteria(criteria1);
        QueryByCriteria query = new QueryByCriteria(StaffCertification.class, criteria);
        m_certificates = getBroker().getMapByQuery(query, StaffCertification.COL_STAFF_OID, 100);
    }

    /**
     * A pair of sections are considered as "connected" if they meet at the same time, at
     * the same room, during same term, and is taught by the same teacher.
     */
    private void loadConnectedSectionsMap() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE,
                SchoolCourse.MASTER_TYPE_CLASS);
        criteria.addEqualToField(MasterSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL +
                PATH_DELIMITER + SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, MasterSchedule.COL_SCHEDULE_OID);
        // Only include courses flagged for "SEDF REPORT FLAG" alias
        // criteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
        // SchoolCourse.REL_COURSE
        // + PATH_DELIMITER + m_javanameSedfReport, BooleanAsStringConverter.TRUE);
        // Filter out sections which don't have any enrolled students
        criteria.addGreaterThan(MasterSchedule.COL_ENROLLMENT_TOTAL, Integer.valueOf(0));
        // Filter out sections not linked to a primary staff, or a room
        criteria.addNotNull(MasterSchedule.COL_PRIMARY_STAFF_OID);
        criteria.addNotNull(MasterSchedule.COL_PRIMARY_ROOM_OID);
        criteria.addNotEmpty(MasterSchedule.COL_SCHEDULE_DISPLAY, getBroker().getPersistenceKey());
        // Check user exclude indicators.
        if (m_fieldExcludeCrs != null) {
            criteria.addNotEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE +
                    PATH_DELIMITER + m_fieldExcludeCrs, BooleanAsStringConverter.TRUE);
        }
        if (m_fieldExcludeMst != null) {
            criteria.addNotEqualTo(m_fieldExcludeMst, BooleanAsStringConverter.TRUE);
        }
        if (m_fieldExcludeMstFromConnected != null) {
            criteria.addNotEqualTo(m_fieldExcludeMstFromConnected, BooleanAsStringConverter.TRUE);
        }

        // Check the require subject code option.
        if ((m_paramRequireSubject != null) && m_paramRequireSubject.booleanValue()) {
            criteria.addNotEmpty(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE +
                    PATH_DELIMITER + m_fieldScedCourse, getBroker().getPersistenceKey());
        }

        // Check school selection.
        if (isSchoolContext()) {
            criteria.addEqualTo(MasterSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else {
            criteria.addNotEqualTo(MasterSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER
                    + SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            criteria.addNotEqualTo(MasterSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER
                    + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }

        QueryByCriteria query = new QueryByCriteria(MasterSchedule.class, criteria);

        // Sort records by school first and then by enrollment total
        // (descending)
        query.addOrderByAscending(MasterSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL +
                PATH_DELIMITER + m_fieldSchoolId);
        query.addOrderByDescending(MasterSchedule.COL_ENROLLMENT_TOTAL);

        Map<String, List<MasterSchedule>> sectionsMap = new LinkedHashMap<String, List<MasterSchedule>>();
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                MasterSchedule section = (MasterSchedule) iterator.next();
                if (m_refTableMultiGradeCrossSplittedCodes
                        .contains(section.getSchoolCourse().getCourse().getNumber())) {
                    Collection<StudentSchedule> sscsForSection = m_studentScheduleMapBySection.get(section.getOid());
                    if (sscsForSection != null) {
                        Set<String> grades = new HashSet<String>();
                        String maxGrade = null;
                        for (StudentSchedule ssc : sscsForSection) {
                            if (!grades.contains(ssc.getStudent().getGradeLevel())) {
                                String gradeLevel = ssc.getStudent().getGradeLevel();
                                grades.add(gradeLevel);
                                Integer intGradeLevel = m_numericGradeLevelMap.get(gradeLevel);
                                if (maxGrade == null
                                        || m_numericGradeLevelMap.get(maxGrade).intValue() < intGradeLevel.intValue()) {
                                    maxGrade = gradeLevel;
                                }
                            }
                        }
                        grades.remove(maxGrade);
                        m_gradesForSectionsIRecords.put(section.getOid(), grades);
                        m_maxGradesForSectionsIRecords.put(section.getOid(), maxGrade);
                        m_connectedSections.put(section, section);
                    }
                } else if (isIncludeSection(section)) {
                    String scheduleDisplay = section.getScheduleDisplay();
                    String primaryRoomOid = section.getPrimaryRoomOid();
                    String scheduleTermOid = section.getScheduleTermOid();
                    String primaryStaffOid = section.getPrimaryStaffOid();

                    String key = scheduleDisplay + primaryRoomOid + scheduleTermOid + primaryStaffOid;

                    List<MasterSchedule> sections = sectionsMap.get(key);
                    if (sections == null) {
                        sections = new ArrayList<MasterSchedule>();
                    }
                    sections.add(section);
                    sectionsMap.put(key, sections);
                }
            }
        } finally

        {
            iterator.close();
        }

        for (String key : sectionsMap.keySet()) {
            List<MasterSchedule> sections = sectionsMap.get(key);
            if (sections.size() > 1) {
                MasterSchedule primarySection = null;
                for (MasterSchedule section : sections) {
                    /*
                     * First identify the primary section. A primary section is
                     * one which has the lowest sequence number if it has been
                     * set, or the section with most number of enrolled
                     * students.
                     */
                    String sequenceNumber = (String) section.getFieldValueByBeanPath(m_fieldSequencedCourse);
                    if (!StringUtils.isNumeric(sequenceNumber)) {
                        /*
                         * If there is no sequence number defined on the first
                         * connected section, make the first section with most
                         * number of enrolled students as the primary section.
                         * This collection was built based on descending order
                         * of enrollment totals, so, first section on the list
                         * will be the primary section.
                         */
                        primarySection = section;
                        break;
                    }

                    if (primarySection == null) {
                        primarySection = section;
                    } else {
                        String primarySequenceNumber =
                                (String) primarySection.getFieldValueByBeanPath(m_fieldSequencedCourse);

                        int primarySectionSeq = Integer.parseInt(primarySequenceNumber);
                        int currentSectionSeq = Integer.parseInt(sequenceNumber);
                        if (currentSectionSeq < primarySectionSeq) {
                            primarySection = section;
                        }
                    }
                }

                // After identifying the primary section, build pairs of
                // connected sections.
                for (MasterSchedule section : sections) {
                    if (!ObjectUtils.matchStrict(primarySection, section)) {
                        m_connectedSections.put(section, primarySection);
                    }
                }
            }
        }
    }

    /**
     * Loads staff positions for report date.
     */
    private void loadStaffPositions() {
        String staffActiveStatus = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StaffPosition.REL_STAFF + PATH_DELIMITER + Staff.COL_STATUS, staffActiveStatus);
        criteria.addLessOrEqualThan(StaffPosition.COL_START_DATE, m_reportDate);
        X2Criteria criteria1 = new X2Criteria();
        X2Criteria criteria2 = new X2Criteria();
        criteria1.addEmpty(StaffPosition.COL_END_DATE, getBroker().getPersistenceKey());
        criteria2.addGreaterOrEqualThan(StaffPosition.COL_END_DATE, m_reportDate);
        criteria1.addOrCriteria(criteria2);
        criteria.addAndCriteria(criteria1);
        // Check user exclude indicator.
        if (m_fieldExcludeStf != null) {
            criteria.addNotEqualTo(StaffPosition.REL_STAFF + PATH_DELIMITER + m_fieldExcludeStf,
                    BooleanAsStringConverter.TRUE);
        }
        // Check school selection.
        if (isSchoolContext()) {
            criteria.addEqualTo(StaffPosition.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addNotEqualTo(StaffPosition.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            criteria.addNotEqualTo(StaffPosition.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }
        QueryByCriteria query = new QueryByCriteria(StaffPosition.class, criteria);
        m_staffPositions = getBroker().getMapByQuery(query, StaffPosition.COL_STAFF_OID, 100);
    }

    /**
     * Loads all the student schedules for report date.
     *
     * @throws X2BaseException exception
     */
    private void loadStudentMapForReportDate() throws X2BaseException {
        List<String> completedStudentSections = new ArrayList<String>();
        Map<String, Set<String>> sectionsByStaffId = new HashMap<String, Set<String>>();
        for (String studentOid : m_scheduleInfoMap.keySet()) {
            Map<String, ScheduleInfo> sectionsForStudent = m_scheduleInfoMap.get(studentOid);
            for (String sectionOid : sectionsForStudent.keySet()) {
                ScheduleInfo info = sectionsForStudent.get(sectionOid);
                MasterSchedule masterSchedule = info.getSection();
                if (masterSchedule != null) {
                    String courseOid = masterSchedule.getSchoolCourseOid();
                    StudentSchedule studentSchedule = info.getSchedule();
                    String courseView = masterSchedule.getCourseView();
                    SisStaff primaryStaff = masterSchedule.getPrimaryStaff();
                    if ((primaryStaff != null) &&
                            !Boolean.TRUE.equals(primaryStaff.getFieldValueByBeanPath(m_fieldExcludeStf))) {
                        if (!completedStudentSections.contains(buildKey(courseOid, courseView, studentOid)) &&
                                (studentSchedule != null)) {
                            String staffLocalId = primaryStaff.getLocalId();

                            if (isCoopCode((String) studentSchedule
                                    .getFieldValueByBeanPath(m_fieldWorkBasedLearningCode))) {
                                Integer currentCount = m_staffCoopCounts.get(staffLocalId);
                                if (currentCount == null) {
                                    currentCount = Integer.valueOf(0);
                                }
                                currentCount = Integer.valueOf(currentCount.intValue() + 1);
                                m_staffCoopCounts.put(staffLocalId, currentCount);
                                Set<String> sectionOids = sectionsByStaffId.get(staffLocalId);
                                if (sectionOids == null) {
                                    sectionOids = new HashSet<String>();
                                }
                                if (!sectionOids.contains(masterSchedule.getOid())) {
                                    sectionOids.add(masterSchedule.getOid());
                                    sectionsByStaffId.put(staffLocalId, sectionOids);
                                    Integer staffSectionCount = m_staffCoopSectionCounts.get(staffLocalId);
                                    if (staffSectionCount == null) {
                                        staffSectionCount = Integer.valueOf(0);
                                    }
                                    staffSectionCount = Integer.valueOf(staffSectionCount.intValue() + 1);
                                    m_staffCoopSectionCounts.put(staffLocalId, staffSectionCount);
                                    Integer staffCoopMinutes = m_staffCoopSectionMinutes.get(staffLocalId);
                                    if (staffCoopMinutes == null) {
                                        staffCoopMinutes = Integer.valueOf(0);
                                    }
                                    String sectionMinutes =
                                            (String) masterSchedule.getFieldValueByAlias(ALIAS_COURSE_MINUTES);
                                    if (StringUtils.isNumeric(sectionMinutes)) {
                                        int minutes = (int) Math.round(Double.parseDouble(sectionMinutes));

                                        staffCoopMinutes = Integer.valueOf(staffCoopMinutes.intValue() + minutes);
                                        m_staffCoopSectionMinutes.put(staffLocalId, staffCoopMinutes);
                                    }
                                    completedStudentSections.add(buildKey(courseOid, courseView, studentOid));
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Load a map of sets of student schedule change records by student oid for
     * the students in the export.
     */
    private void loadStudentScheduleChanges() {
        X2Criteria m_scheduleChangeCriteria = new X2Criteria();
        // From master type class
        m_scheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE,
                SchoolCourse.MASTER_TYPE_CLASS);
        // From active Schedule
        m_scheduleChangeCriteria.addEqualToField(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentScheduleChange.COL_SCHEDULE_OID);
        // check school or organization selection.
        if (isSchoolContext()) {
            m_scheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else {
            m_scheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL +
                    PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            m_scheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL +
                    PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        // Check exclusion flags for student, section and student schedule.
        if (!StringUtils.isEmpty(m_fieldExcludeStd)) {
            m_scheduleChangeCriteria.addNotEqualTo(
                    StudentScheduleChange.REL_STUDENT + PATH_DELIMITER + m_fieldExcludeStd,
                    BooleanAsStringConverter.TRUE);
        }
        if (!StringUtils.isEmpty(m_fieldExcludeMst)) {
            m_scheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    m_fieldExcludeMst,
                    BooleanAsStringConverter.TRUE);
        }
        if (!StringUtils.isEmpty(m_fieldExcludeCrs)) {
            m_scheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_fieldExcludeCrs, BooleanAsStringConverter.TRUE);
        }
        // Check the require subject code option.
        if ((m_paramRequireSubject != null) && m_paramRequireSubject.booleanValue()) {
            m_scheduleChangeCriteria.addNotEmpty(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER
                    + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE +
                    PATH_DELIMITER + m_fieldScedCourse, getBroker().getPersistenceKey());
        }
        m_scheduleChangeCriteria.addNotNull(StudentScheduleChange.COL_ACTION_DATE);
        QueryByCriteria query = new QueryByCriteria(StudentScheduleChange.class, m_scheduleChangeCriteria);
        query.addOrderBy(StudentScheduleChange.COL_STUDENT_OID, true);
        query.addOrderBy(StudentScheduleChange.COL_MASTER_SCHEDULE_OID, true);
        query.addOrderBy(StudentScheduleChange.COL_EFFECTIVE_DATE, false);
        query.addOrderBy(StudentScheduleChange.COL_TIMESTAMP, false);
        m_scheduleChangeMap =
                getBroker().getGroupedCollectionByQuery(query, StudentScheduleChange.COL_STUDENT_OID, 500);
    }

    /**
     * Load a map of sets of student schedule records by student oid for the
     * students in the export.
     *
     * @param isStudentScheduleMapRequered boolean
     */
    private void loadStudentSchedules(boolean isStudentScheduleMapRequered) {
        X2Criteria scheduleCriteria = new X2Criteria();
        // Master type Class
        scheduleCriteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE +
                PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);
        // From active Schedule
        scheduleCriteria.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER + SisSchool.REL_ACTIVE_SCHOOL_SCHED +
                PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, StudentSchedule.COL_SCHEDULE_OID);
        // check school selection.
        if (isSchoolContext()) {
            scheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else {
            scheduleCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL +
                    PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            scheduleCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL +
                    PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        // Check exclusion flags for student, section and student schedule.
        if (!StringUtils.isEmpty(m_fieldExcludeStd)) {
            scheduleCriteria.addNotEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER + m_fieldExcludeStd,
                    BooleanAsStringConverter.TRUE);
        }
        if (!StringUtils.isEmpty(m_fieldExcludeMst)) {
            scheduleCriteria.addNotEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER + m_fieldExcludeMst,
                    BooleanAsStringConverter.TRUE);
        }
        if (!StringUtils.isEmpty(m_fieldExcludeCrs)) {
            scheduleCriteria
                    .addNotEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE +
                            PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER +
                            m_fieldExcludeCrs,
                            BooleanAsStringConverter.TRUE);
        }
        // Check the require subject code option.
        if ((m_paramRequireSubject != null) && m_paramRequireSubject.booleanValue()) {
            scheduleCriteria
                    .addNotEmpty(StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE +
                            PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER + m_fieldScedCourse,
                            getBroker().getPersistenceKey());
        }
        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, scheduleCriteria);
        query.addOrderBy(StudentSchedule.COL_STUDENT_OID, true);
        query.addOrderBy(StudentSchedule.COL_SECTION_OID, true);
        m_studentScheduleMapBySection =
                getBroker().getGroupedCollectionByQuery(query, StudentSchedule.COL_SECTION_OID, 500);
        if (isStudentScheduleMapRequered) {
            m_studentScheduleMap = getBroker().getGroupedCollectionByQuery(query, StudentSchedule.COL_STUDENT_OID, 500);
        } else {
            m_studentScheduleMap = new HashMap<String, List<StudentSchedule>>();
        }
    }

    /**
     * Load Staff into a map by staff Oid.
     */
    private void loadTeacherMaps() {
        QueryByCriteria query = new QueryByCriteria(Staff.class, new X2Criteria());
        m_staffLocalId = getBroker().getMapByQuery(query, Staff.COL_LOCAL_ID, 100);
        m_staffName = getBroker().getMapByQuery(query, Staff.COL_NAME_VIEW, 100);
        m_staffOid = getBroker().getMapByQuery(query, X2BaseBean.COL_OID, 100);
    }

    /**
     * Loads a map of sets of transcript records by studentOid for students in the export.
     */
    private void loadTranscripts() {
        X2Criteria transcriptCriteria = new X2Criteria();
        transcriptCriteria.addNotEmpty(Transcript.COL_MASTER_SCHEDULE_OID, getBroker().getPersistenceKey());
        transcriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getOrganization().getCurrentContextOid());
        // From active Schedule
        transcriptCriteria
                .addEqualToField(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER + MasterSchedule.REL_SCHEDULE +
                        PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                        Transcript.REL_MASTER_SCHEDULE +
                                PATH_DELIMITER +
                                MasterSchedule.COL_SCHEDULE_OID);
        // check school or organization selection.
        if (isSchoolContext()) {
            transcriptCriteria.addEqualTo(Transcript.REL_SCHOOL, getSchool().getOid());
        } else {
            transcriptCriteria.addNotEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            transcriptCriteria.addNotEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }
        // Check exclusion flags for student, section and student schedule.
        if (!StringUtils.isEmpty(m_fieldExcludeStd)) {
            transcriptCriteria.addNotEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER + m_fieldExcludeStd,
                    BooleanAsStringConverter.TRUE);
        }
        if (!StringUtils.isEmpty(m_fieldExcludeMst)) {
            transcriptCriteria.addNotEqualTo(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER + m_fieldExcludeMst,
                    BooleanAsStringConverter.TRUE);
        }
        if (!StringUtils.isEmpty(m_fieldExcludeCrs)) {
            transcriptCriteria.addNotEqualTo(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE +
                    PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_fieldExcludeCrs,
                    BooleanAsStringConverter.TRUE);
        }
        // Check the require subject code option.
        if ((m_paramRequireSubject != null) && m_paramRequireSubject.booleanValue()) {
            transcriptCriteria.addNotEmpty(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE +
                    PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_fieldScedCourse,
                    getBroker().getPersistenceKey());
        }
        QueryByCriteria query = new QueryByCriteria(Transcript.class, transcriptCriteria);
        query.addOrderBy(Transcript.COL_STUDENT_OID, true);
        m_transcriptMap = getBroker().getGroupedCollectionByQuery(query, Transcript.COL_STUDENT_OID, 500);
    }
}
