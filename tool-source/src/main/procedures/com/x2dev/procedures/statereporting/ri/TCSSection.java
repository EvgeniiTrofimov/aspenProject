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
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * RI state report for TCS Section Section export. This class implements the data
 * export for the TCS Section Section export.
 *
 * @author X2 Development Corporation
 */
public class TCSSection extends RIStateReportData {
    /**
     * Implementation of StateReportEntity to be used by the TCS Section Section
     * export. This must be a public static inner class with a public no
     * argument constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class TCSSectionEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public TCSSectionEntity() {
            // Empty constructor.
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
            MasterSchedule section = (MasterSchedule) getBean();

            String name = section.getCourseView() + " " + section.getDescription() +
                    " [" + section.getSchoolCourse().getSchool().getName() + "]";

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
    public static final String REQUIRE_SCED_PARAM = "requireSced";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Alias fields
     */
    protected static final String ALIAS_CSK_EXCLUDE = "all-csk-ExcludeFromTCS";
    protected static final String ALIAS_EXCLUDE_COURSE = "DOE EXCLUDE CRS";
    protected static final String ALIAS_EXCLUDE_SECTION = "DOE EXCLUDE MST";
    protected static final String ALIAS_SCED_CODE = "RI Course ID";

    /**
     * Instance variables.
     */
    protected Map<String, Collection<ScheduleTermDate>> m_termDateMap;

    protected String m_fieldCskExclude;
    protected String m_fieldExlcudeCrs;
    protected String m_fieldExcludeSection;
    private String m_orgFieldStr = null;
    private String m_orgOid = null;


    /**
     * Retrieve the term start/end date for the section.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTermDate implements FieldRetriever {
        private final String PARAM_START = "START";
        private final String PARAM_END = "END";

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
            MasterSchedule masterSchedule = (MasterSchedule) entity.getBean();
            PlainDate termDate = null;

            Collection<ScheduleTermDate> termDates =
                    ((TCSSection) data).getTermDates(masterSchedule.getScheduleTermOid());

            if (PARAM_START.equals(param)) {
                for (ScheduleTermDate schedTermDate : termDates) {
                    if (termDate == null || termDate.after(schedTermDate.getStartDate())) {
                        termDate = schedTermDate.getStartDate();
                    }
                }
            } else if (PARAM_END.equals(param)) {
                for (ScheduleTermDate schedTermDate : termDates) {
                    if (termDate == null || termDate.before(schedTermDate.getEndDate())) {
                        termDate = schedTermDate.getEndDate();
                    }
                }
            }

            return termDate;
        }

    }

    /**
     * Validator to check the Total Days in Cycle is blank or lenght not exceeded 2.
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateTotalDaysInCycle implements FieldValidator {
        public static final String VAL_ID = "VAL-TOTAL-DAYS";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            if (!StringUtils.isEmpty(value) && value.length() > 2) {
                errors.add(new StateReportValidationError(entity, field,
                        "Value too long.",
                        "Total Days in Cycle =" + STYLE_BOLD + value + STYLE_END));
            }


            return errors;
        }

    }

    /**
     * Validator to check the Meeting Days in Cycle is blank or lenght not exceeded 2.
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateMeetingDaysInCycle implements FieldValidator {
        public static final String VAL_ID = "VAL-MEETING-DAYS";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            if (!StringUtils.isEmpty(value) && value.length() > 2) {
                errors.add(new StateReportValidationError(entity, field,
                        "Value too long.",
                        "Meeting Days in Cyc =" + STYLE_BOLD + value + STYLE_END));
            }


            return errors;
        }

    }

    /**
     * Validator to check the Minutes per Meeting is blank or lenght not exceeded 3.
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateMinutesPerMeeting implements FieldValidator {
        public static final String VAL_ID = "VAL-MIN-PER_MEETING";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            if (!StringUtils.isEmpty(value) && value.length() > 3) {
                errors.add(new StateReportValidationError(entity, field,
                        "Value too long.",
                        "Minutes per Meeting =" + STYLE_BOLD + value + STYLE_END));
            }


            return errors;
        }

    }

    /**
     * Validator to check the Course Id is present and length is less or equals then 10.
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateCourseId implements FieldValidator {
        public static final String VAL_ID = "VAL-COURSE-ID";


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            if (StringUtils.isEmpty(value) || value.length() > 20) {
                errors.add(new StateReportValidationError(entity, field,
                        "Value too long.",
                        "Course ID =" + STYLE_BOLD + value + STYLE_END));
            }
            /*
             * if (!StringUtils.isEmpty(value) && !value.matches("-?\\d+")) {
             * errors.add(new StateReportValidationError(entity, field,
             * "Course ID invalid.",
             * "Course ID =" + STYLE_BOLD + value + STYLE_END));
             * }
             */

            return errors;
        }

    }


    /**
     * Validator to check the School Code is present and length is 5.
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateSchoolCode implements FieldValidator {
        public static final String VAL_ID = "VAL-SCHOOL-CODE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            if (StringUtils.isEmpty(value) || value.length() != 5) {
                errors.add(new StateReportValidationError(entity, field,
                        "School Code invalid.",
                        "School Code =" + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }

    }

    /**
     * Validator to check the District Code is present and length equals 2.
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateDistrCode implements FieldValidator {
        public static final String VAL_ID = "VAL-DISTR-CODE";


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            if (StringUtils.isEmpty(value) || value.length() != 2) {
                errors.add(new StateReportValidationError(entity, field,
                        "District Code invalid.",
                        "District Code =" + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }

    }



    /**
     * Returns the heading with an end of line character appended.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        String heading = super.getHeading();
        if (heading != null && !StringUtils.isEmpty(heading) && !heading.endsWith("\n")) {
            heading += "\r\n";
        }
        return heading;
    }

    /**
     * Initialize the data module.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.x2dev.sis.model.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        initializeFields();

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

        /*
         * Build criteria, query and retrievers map.
         */
        if (getSetupErrors().size() == 0) {
            Criteria sectionCriteria = getSectionCriteria();
            QueryByCriteria sectionQuery = new QueryByCriteria(MasterSchedule.class,
                    sectionCriteria);

            Integer sort = (Integer) getParameter(SORT_PARAM);
            switch (sort == null ? 0 : sort.intValue()) {
                case 1: // School, Number
                    sectionQuery.addOrderByAscending(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                            SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                            SisSchool.COL_NAME);
                    sectionQuery.addOrderByAscending(MasterSchedule.COL_COURSE_VIEW);
                    break;

                default: // Number, School
                    sectionQuery.addOrderByAscending(MasterSchedule.COL_COURSE_VIEW);
                    sectionQuery.addOrderByAscending(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                            SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                            SisSchool.COL_NAME);
                    break;
            }

            // Set the query to be used for student selection.
            setQuery(sectionQuery);
            setEntityClass(TCSSectionEntity.class);

            // Build maps of retriever functions
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("TermDate", new RetrieveTermDate());
            addCalcs(calcs);

            // Build maps of validator functions
            HashMap validators = new HashMap<String, FieldRetriever>();
            validators.put(ValidateTotalDaysInCycle.VAL_ID, new ValidateTotalDaysInCycle());
            validators.put(ValidateMeetingDaysInCycle.VAL_ID, new ValidateMeetingDaysInCycle());
            validators.put(ValidateMinutesPerMeeting.VAL_ID, new ValidateMinutesPerMeeting());
            validators.put(ValidateCourseId.VAL_ID, new ValidateCourseId());
            validators.put(ValidateSchoolCode.VAL_ID, new ValidateSchoolCode());
            validators.put(ValidateDistrCode.VAL_ID, new ValidateDistrCode());
            addValidators(validators);
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
            Criteria criteria = new Criteria();
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
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + PATH_DELIMITER +
                RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(X2BaseBean.COL_OID,
                new SubQuery(RecordSetKey.class,
                        RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Returns the criteria that retrieves all school courses that should be
     * included in the export.
     *
     * @return Criteria
     */
    private X2Criteria getSectionCriteria() {
        X2Criteria sectionCriteria = new X2Criteria();

        sectionCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, "Class");
        if (!StringUtils.isEmpty(m_fieldCskExclude)) {
            sectionCriteria.addNotEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + m_fieldCskExclude,
                    Boolean.TRUE);
        }
        if (isSchoolContext()) {
            sectionCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.COL_SCHOOL_OID, getSchool().getOid());
        } else if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
            sectionCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                    m_orgFieldStr,
                    m_orgOid);
            sectionCriteria.addNotEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            sectionCriteria.addNotEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        } else {
            sectionCriteria.addNotEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            sectionCriteria.addNotEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }

        // Check if the course exclusion custom field is present.
        if (!StringUtils.isEmpty(m_fieldExlcudeCrs)) {
            sectionCriteria.addNotEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_fieldExlcudeCrs,
                    BooleanAsStringConverter.TRUE);
        }

        // Check if the section exclusion custom field is present.
        if (!StringUtils.isEmpty(m_fieldExcludeSection)) {
            sectionCriteria.addNotEqualTo(m_fieldExcludeSection,
                    BooleanAsStringConverter.TRUE);
        }

        // Check if the SCED code is required.
        Boolean requireSced = (Boolean) getParameter(REQUIRE_SCED_PARAM);
        if (requireSced != null && requireSced.booleanValue()) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SCED_CODE);
            if (field != null) {
                sectionCriteria.addNotEmpty(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + PATH_DELIMITER +
                        field.getJavaName(),
                        getBroker().getPersistenceKey());
            }
        }

        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        Integer queryBy = (Integer) getParameter(QUERY_BY_PARAM);
        switch ((queryBy == null ? 0 : queryBy.intValue())) {
            case 1: // Snapshot
                addRecordSetCriteria(sectionCriteria, queryString);
                break;

            default:
                // Take all course in the current school year.
                sectionCriteria.addEqualToField(MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                        MasterSchedule.COL_SCHEDULE_OID);
                break;
        }

        return sectionCriteria;
    }

    /**
     * Lookup aliases, fields, maps.
     */
    private void initializeFields() {
        m_fieldExcludeSection = translateAliasToJavaName(ALIAS_EXCLUDE_SECTION, false);
        m_fieldExlcudeCrs = translateAliasToJavaName(ALIAS_EXCLUDE_COURSE, false);
        m_fieldCskExclude = translateAliasToJavaName(ALIAS_CSK_EXCLUDE, false);
    }
}
