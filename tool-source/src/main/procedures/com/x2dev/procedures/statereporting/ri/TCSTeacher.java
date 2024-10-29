/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ri;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.OrganizationDefinition;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * RI state report for TCS Section Teacher export. This class implements the data
 * export for the TCS Section Teacher export.
 *
 * @author X2 Development Corporation
 */
public class TCSTeacher extends RIStateReportData {
    /**
     * Implementation of StateReportEntity to be used by the TCS Section Teacher
     * export. This must be a public static inner class with a public no
     * argument constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class TCSTeacherEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";
        private List<String[]> dateArray = new ArrayList<String[]>();

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public TCSTeacherEntity() {
            // Empty constructor.
        }

        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            ScheduleTeacher teacher = (ScheduleTeacher) bean;
            String startDate = (String) teacher.getFieldValueByAlias("DOE TEACHER START DATE");
            String endDate = (String) teacher.getFieldValueByAlias("DOE TEACHER END DATE");
            String secondaryStartDate = (String) teacher.getFieldValueByAlias("all-mtc-SecondaryEntryDate");
            String secondaryEndDate = (String) teacher.getFieldValueByAlias("all-mtc-SecondaryExitDate");

            dateArray.add(new String[] {startDate, endDate});

            if (!StringUtils.isEmpty(secondaryStartDate)) {
                dateArray.add(new String[] {secondaryStartDate, secondaryEndDate});
            }

            setRowCount(dateArray.size());
        }

        public String[] getCurrentDates() {
            return dateArray.get(getCurrentRow());
        }

        /**
         * Filter sections that have not started yet.
         * This includes future sections, such as term 2 sections during term 1.
         *
         * @return StateReportValidationError
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;
            String startDate = getFieldValue(FIELD_START_DATE);
            if (StringUtils.isEmpty(startDate)) {
                error = new StateReportValidationError(getEntityName(), "", "No term start date.", startDate);
            }
            return error;
        }

        /**
         * Generate a display name to print on the validation report for the
         * entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            ScheduleTeacher teacher = (ScheduleTeacher) getBean();

            String name = teacher.getStaff().getNameView() +
                    " [LOCALID: " + teacher.getStaff().getLocalId() +
                    "] " + teacher.getSection().getCourseView() +
                    " " + teacher.getSection().getSchoolCourse().getDescription();

            return name;
        }
    }

    /**
     * Input parameter: Select organization level.
     */
    public static final String ORGANIZATION_PARAM = "orgOid";

    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "requireSced" parameter. The value is an Boolean.
     */
    public static final String EXCLUDE_SECTIONS_WITHOUT_STDS = "excludeSectionsWithoutStds";

    /**
     * Name for the enumerated "requireSced" parameter. The value is an Boolean.
     */
    public static final String REQUIRE_SCED_PARAM = "requireSced";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Aliases for excluding specific records from the export.
     */
    protected static final String ALIAS_CSK_EXCLUDE_FROM_TCS = "all-csk-ExcludeFromTCS";
    protected static final String ALIAS_EXCLUDE_COURSE = "DOE EXCLUDE CRS";
    protected static final String ALIAS_EXCLUDE_STAFF = "DOE EXCLUDE STF";
    protected static final String ALIAS_EXCLUDE_STAFF_SCHEDULE = "DOE EXCLUDE MTC";
    protected static final String ALIAS_EXCLUDE_SECTION = "DOE EXCLUDE MST";
    protected static final String ALIAS_PRIMARY_OVERRIDE = "DOE PRIMARY OVERRIDE";
    protected static final String ALIAS_SCED_CODE = "RI Course ID";
    protected static final String FIELD_START_DATE = "StartDate";

    /**
     * Instance variables.
     */
    protected PlainDate m_reportDate;
    protected Map<String, Collection<ScheduleTermDate>> m_termDateMap;

    private String m_excludeCourseField = null;
    private String m_excludeSchedField = null;
    private String m_excludeSectionField = null;
    private String m_excludeStaffField = null;
    private String m_fieldCskExcludeFromTcs = null;
    private String m_orgFieldStr = null;
    private String m_orgOid = null;

    /**
     * Retrieve a property with an optional override field.
     * Retrieve the value from the bean path.
     * The field parameter should be an alias on the current record with an override
     * field. If this is field is not empty, it will be used to override the original value.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveOverride implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            Object value = data.getProperty(entity.getBean(), field.getBeanPath());

            String overrideField = data.translateAliasToJavaName(param, false);
            if (!StringUtils.isEmpty(overrideField)) {
                String overrideValue = (String) data.getProperty(entity.getBean(), overrideField);
                if (!StringUtils.isEmpty(overrideValue)) {
                    /*
                     * In the case of primary override, must translate from the reference
                     * table values of Yes/No to the logical value of "1" and "0" to match
                     * primary logical field.
                     */
                    if (ALIAS_PRIMARY_OVERRIDE.equals(param)) {
                        if ("Yes".equals(overrideValue)) {
                            value = Boolean.TRUE;
                        } else if ("No".equals(overrideValue)) {
                            value = Boolean.FALSE;
                        }
                    } else {
                        value = overrideValue;
                    }
                }
            }

            return value;
        }
    }

    /**
     * Retrieve the term start/end date for the section.
     * Check start/end date override on teacher schedule record.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTermDate implements FieldRetriever {
        private final String PARAM_START = "START";
        private final String PARAM_END = "END";
        private SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd");

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            ScheduleTeacher teacher = (ScheduleTeacher) entity.getBean();
            TCSTeacherEntity tcsEntity = (TCSTeacherEntity) entity;
            MasterSchedule section = teacher.getSection();
            PlainDate termDate = null;

            String[] entityDates = tcsEntity.getCurrentDates();
            String startDateAsString = entityDates[0];
            String endDateAsString = entityDates[1];

            // If the override value is empty, lookup the schedule term dates for start or end.
            Collection<ScheduleTermDate> termDates = ((TCSTeacher) data).getTermDates(section.getScheduleTermOid());

            if (PARAM_START.equals(param)) {
                if (!StringUtils.isEmpty(startDateAsString)) {
                    try {
                        termDate = new PlainDate(format.parse(startDateAsString));
                    } catch (ParseException fe) {
                        // leave null, invalid value
                    }
                } else {
                    for (ScheduleTermDate schedTermDate : termDates) {
                        if (termDate == null || termDate.after(schedTermDate.getStartDate())) {
                            termDate = schedTermDate.getStartDate();
                        }
                    }
                    if (termDate != null && !m_reportDate.after(termDate)) {
                        termDate = null;
                    }
                }
            } else if (PARAM_END.equals(param)) {
                if (!StringUtils.isEmpty(endDateAsString)) {
                    try {
                        termDate = new PlainDate(format.parse(endDateAsString));
                    } catch (ParseException fe) {
                        // leave null, invalid value
                    }
                } else {
                    for (ScheduleTermDate schedTermDate : termDates) {
                        if (termDate == null || termDate.before(schedTermDate.getEndDate())) {
                            termDate = schedTermDate.getEndDate();
                        }
                    }
                    if (termDate != null && !m_reportDate.after(termDate)) {
                        termDate = null;
                    }
                }
            }
            return termDate;
        }
    }


    /**
     * Initialize the data module.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        m_reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));

        // initialize report fields.
        initializeFields();

        /*
         * Build criteria, query and retrievers map.
         */
        if (getSetupErrors().size() == 0) {
            X2Criteria sectionCriteria = getScheduleCriteria();
            QueryByCriteria sectionQuery = new QueryByCriteria(ScheduleTeacher.class,
                    sectionCriteria, true);

            Integer sort = (Integer) getParameter(SORT_PARAM);
            switch (sort == null ? 0 : sort.intValue()) {
                case 1: // School, Staff, section
                    sectionQuery.addOrderByAscending(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                            MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                            SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                            SisSchool.COL_NAME);
                    sectionQuery.addOrderByAscending(ScheduleTeacher.REL_STAFF + PATH_DELIMITER +
                            SisStaff.COL_NAME_VIEW);
                    sectionQuery.addOrderByAscending(ScheduleTeacher.COL_STAFF_OID);
                    sectionQuery.addOrderByAscending(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                            MasterSchedule.COL_COURSE_VIEW);
                    break;

                case 2: // School, section
                    sectionQuery.addOrderByAscending(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                            MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                            SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                            SisSchool.COL_NAME);
                    sectionQuery.addOrderByAscending(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                            MasterSchedule.COL_COURSE_VIEW);
                    break;

                default: // Staff, section
                    sectionQuery.addOrderByAscending(ScheduleTeacher.REL_STAFF + PATH_DELIMITER +
                            SisStaff.COL_NAME_VIEW);
                    sectionQuery.addOrderByAscending(ScheduleTeacher.COL_STAFF_OID);
                    sectionQuery.addOrderByAscending(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                            MasterSchedule.COL_COURSE_VIEW);
                    break;
            }

            // Set the query to be used for student selection.
            setQuery(sectionQuery);
            setEntityClass(TCSTeacherEntity.class);

            // Build maps of retriever functions
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("TCS-OVERRIDE", new RetrieveOverride());
            calcs.put("TCS-TERM-DATE", new RetrieveTermDate());
            addCalcs(calcs);
        }
    }

    /**
     * Load the schedule term dates for a schedule term oid.
     * Keep a map of existing codes for lookup.
     *
     * @param scheduleTermOid String
     * @return Collection<ScheduleTermDate>
     */
    protected Collection<ScheduleTermDate> getTermDates(String scheduleTermOid) {
        Collection<ScheduleTermDate> dates = null;

        if (m_termDateMap == null) {
            m_termDateMap = new HashMap<String, Collection<ScheduleTermDate>>();
        }

        if (!m_termDateMap.containsKey(scheduleTermOid)) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ScheduleTermDate.COL_SCHEDULE_TERM_OID, scheduleTermOid);
            QueryByCriteria query = new QueryByCriteria(ScheduleTermDate.class, criteria);
            dates = getBroker().getCollectionByQuery(query);
            m_termDateMap.put(scheduleTermOid, dates);
        }

        return m_termDateMap.get(scheduleTermOid);
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(X2Criteria criteria, String recordSetName) {
        X2Criteria recordSetCriteria = new X2Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + PATH_DELIMITER +
                RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(ScheduleTeacher.COL_STAFF_OID,
                new SubQuery(RecordSetKey.class,
                        RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Returns the criteria that retrieves all school courses that should be
     * included in the export.
     *
     * @return Criteria
     */
    private X2Criteria getScheduleCriteria() {
        X2Criteria scheduleCriteria = new X2Criteria();

        scheduleCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, "Class");
        scheduleCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + m_fieldCskExcludeFromTcs, Boolean.TRUE);
        if (isSchoolContext()) {
            scheduleCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
                scheduleCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                        m_orgFieldStr,
                        m_orgOid);
            }
            scheduleCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            scheduleCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        // Check Exclude sections without students
        Boolean excludeSection = (Boolean) getParameter(EXCLUDE_SECTIONS_WITHOUT_STDS);
        if (excludeSection != null && excludeSection.booleanValue()) {
            X2Criteria notEmptyStudentsCriteria = new X2Criteria();
            X2Criteria.addNoMatchCriteria(notEmptyStudentsCriteria);
            X2Criteria studentOrCriteria1 = new X2Criteria();
            studentOrCriteria1.addNotEmpty(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_STUDENT_SCHEDULE_CHANGES + PATH_DELIMITER + X2BaseBean.COL_OID,
                    getBroker().getPersistenceKey());
            X2Criteria studentOrCriteria2 = new X2Criteria();
            studentOrCriteria2.addNotEmpty(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_STUDENT_SECTIONS + PATH_DELIMITER + X2BaseBean.COL_OID,
                    getBroker().getPersistenceKey());
            notEmptyStudentsCriteria.addOrCriteria(studentOrCriteria1);
            notEmptyStudentsCriteria.addOrCriteria(studentOrCriteria2);
            scheduleCriteria.addAndCriteria(notEmptyStudentsCriteria);
        }

        // Check if the SCED code is required.
        Boolean requireSced = (Boolean) getParameter(REQUIRE_SCED_PARAM);
        if (requireSced != null && requireSced.booleanValue()) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SCED_CODE);
            if (field != null) {
                scheduleCriteria.addNotEmpty(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + PATH_DELIMITER +
                        field.getJavaName(),
                        getBroker().getPersistenceKey());
            }
        }

        // Check exclusion flags for staff, section and teacher schedule.
        if (!StringUtils.isEmpty(m_excludeStaffField)) {
            scheduleCriteria.addNotEqualTo(ScheduleTeacher.REL_STAFF + PATH_DELIMITER +
                    m_excludeStaffField,
                    BooleanAsStringConverter.TRUE);
        }

        if (!StringUtils.isEmpty(m_excludeSectionField)) {
            scheduleCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    m_excludeSectionField,
                    BooleanAsStringConverter.TRUE);
        }

        if (!StringUtils.isEmpty(m_excludeCourseField)) {
            scheduleCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_excludeCourseField,
                    BooleanAsStringConverter.TRUE);
        }

        if (!StringUtils.isEmpty(m_excludeSchedField)) {
            scheduleCriteria.addNotEqualTo(m_excludeSchedField,
                    BooleanAsStringConverter.TRUE);
        }

        // Check user selection criteria.
        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        takeAllCoursesInCurrentSchoolYear(scheduleCriteria);

        Integer queryBy = (Integer) getParameter(QUERY_BY_PARAM);
        switch ((queryBy == null ? 0 : queryBy.intValue())) {
            case 1: // Snapshot of Staff
                addRecordSetCriteria(scheduleCriteria, queryString);
                break;
            default:
                // no action
                break;
        }

        return scheduleCriteria;
    }

    /**
     * Take all course in current school year.
     *
     * @param scheduleCriteria X2Criteria
     */
    private void takeAllCoursesInCurrentSchoolYear(X2Criteria scheduleCriteria) {
        // Take all course in the current school year.
        scheduleCriteria.addEqualToField(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                        MasterSchedule.COL_SCHEDULE_OID);
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        // Get organization Oid and level and field.
        m_orgOid = (String) getParameter(ORGANIZATION_PARAM);
        SisOrganization organization = null;
        OrganizationDefinition def = null;
        if (!StringUtils.isEmpty(m_orgOid)) {
            organization = (SisOrganization) getBroker().getBeanByOid(SisOrganization.class, m_orgOid);
            if (organization != null) {
                def = organization.getOrganizationDefinition();
                m_orgFieldStr = "organization" + Integer.toString(def.getLevel() + 1) + "Oid";
            }
        }

        // Lookup potential exclude fields. Not required.
        m_excludeCourseField = translateAliasToJavaName(ALIAS_EXCLUDE_COURSE, false);
        m_excludeStaffField = translateAliasToJavaName(ALIAS_EXCLUDE_STAFF, false);
        m_excludeSchedField = translateAliasToJavaName(ALIAS_EXCLUDE_STAFF_SCHEDULE, false);
        m_excludeSectionField = translateAliasToJavaName(ALIAS_EXCLUDE_SECTION, false);
        m_fieldCskExcludeFromTcs = translateAliasToJavaName(ALIAS_CSK_EXCLUDE_FROM_TCS, true);
    }
}
