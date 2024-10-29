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
package com.x2dev.procedures.statereporting.wa;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField.ReferenceMapTypeCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export procedure for Staff Schedule File.
 *
 * @author X2 Development Corporation
 */
public class WAStaffSchedule extends StateReportData {
    /**
     * Entity class for Staff Schedule export.
     *
     */
    public static class StaffScheduleEntity extends StateReportEntity {

        private static final String ALIAS_DOE_STAFF_CLASS_END_DATE = "DOE STAFF CLASS END DATE";
        private static final String ALIAS_DOE_STAFF_CLASS_START_DATE = "DOE STAFF CLASS START DATE";
        private static final String ALIAS_DOE_TEACHER_INDICATOR = "DOE TEACHER INDICATOR";
        private static final String PRIMARY_TEACHER_INDICATOR = "P";

        /**
         * Public no argument constructor for dynamic instantiation.
         */

        private WAStaffSchedule m_data = null;
        private String m_teacherIndicator = null;
        private PlainDate m_termStartDate = null;
        private PlainDate m_termEndDate = null;
        private PlainDate m_instructionEndDate = null;
        private PlainDate m_instructionStartDate = null;
        private DistrictSchoolYearContext m_currentContext = null;
        private ScheduleTeacher m_schedule = null;
        private Collection<ScheduleTermDate> m_collectionScheduleTermDate = null;

        /**
         * Instantiates a new staff schedule entity.
         */
        public StaffScheduleEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            ScheduleTeacher schedule = (ScheduleTeacher) getBean();
            Staff staff = schedule.getStaff();
            String name = staff.getNameView() +
                    " [LASID: " + staff.getLocalId() +
                    ", SASID: " + staff.getStateId() +
                    "] " + schedule.getSection().getCourseView();

            return name;
        }

        /**
         * Gets the instruction end date.
         *
         * @return the instructionEndDate
         */
        public PlainDate getInstructionEndDate() {
            return m_instructionEndDate;
        }

        /**
         * Gets the instruction start date.
         *
         * @return the instructionStartDate
         */
        public PlainDate getInstructionStartDate() {
            return m_instructionStartDate;
        }

        /**
         * Gets the section.
         *
         * @return School course
         */
        public SchoolCourse getSection() {
            ScheduleTeacher schedule = (ScheduleTeacher) getBean();
            return schedule.getSection().getSchoolCourse();
        }

        /**
         * Gets the teacher indicator.
         *
         * @return the m_teacherIndicator
         */
        public String getTeacherIndicator() {
            return m_teacherIndicator;
        }

        /**
         * Gets the term end date.
         *
         * @return the termEndDate
         */
        public PlainDate getTermEndDate() {
            return m_termEndDate;
        }

        /**
         * Gets the term start date.
         *
         * @return the m_termStartDate
         */
        public PlainDate getTermStartDate() {
            return m_termStartDate;
        }



        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_schedule = (ScheduleTeacher) getBean();
            m_data = (WAStaffSchedule) data;
            m_currentContext = data.getCurrentContext();
            initTeacherIndicator(data);
            m_collectionScheduleTermDate = m_schedule.getSection().getScheduleTerm().getScheduleTermDates();
            initTermStartAndEndDate();
            if (m_termStartDate != null) {
                initInstructionStartAndEndDate();
            }

        }

        /**
         * Inits the instruction start and end date.
         */
        private void initInstructionStartAndEndDate() {
            String instructionStartDate = (String) m_schedule.getFieldValueByAlias(ALIAS_DOE_STAFF_CLASS_START_DATE);
            if (!StringUtils.isEmpty(instructionStartDate)) {
                m_instructionStartDate = (PlainDate) m_data.m_dateConverter.parseSystemString(instructionStartDate);
            }

            String instructionEndDate = (String) m_schedule.getFieldValueByAlias(ALIAS_DOE_STAFF_CLASS_END_DATE);
            if (!StringUtils.isEmpty(instructionEndDate)) {
                m_instructionEndDate = (PlainDate) m_data.m_dateConverter.parseSystemString(instructionEndDate);
            }

            if (m_instructionStartDate == null) {
                m_instructionStartDate = m_termStartDate;
            } else if (m_instructionStartDate.before(m_termStartDate)) {
                m_instructionStartDate = m_termStartDate;
            } else if (m_instructionStartDate.after(m_termEndDate)) {
                m_instructionStartDate = null;
            }

            if (m_instructionEndDate != null) {
                if (!m_instructionEndDate.before(m_termEndDate)) {
                    m_instructionEndDate = null;
                }
            }
        }

        /**
         * Inits the teacher indicator.
         *
         * @param data StateReportData
         */
        private void initTeacherIndicator(StateReportData data) {

            if (m_schedule.getPrimaryTeacherIndicator()) {
                m_teacherIndicator = PRIMARY_TEACHER_INDICATOR;
            } else {
                m_teacherIndicator = data.lookupReferenceCodeByAlias(ALIAS_DOE_TEACHER_INDICATOR,
                        (String) m_schedule.getFieldValueByAlias(ALIAS_DOE_TEACHER_INDICATOR),
                        ReferenceMapTypeCode.STATE.ordinal());
            }

        }

        /**
         * Inits the term start and end date.
         */
        private void initTermStartAndEndDate() {
            for (ScheduleTermDate scheduleTermDate : m_collectionScheduleTermDate) {
                if (m_termStartDate == null) {
                    m_termStartDate = scheduleTermDate.getStartDate();
                } else {
                    m_termStartDate = m_termStartDate.before(scheduleTermDate.getStartDate()) ? m_termStartDate
                            : scheduleTermDate.getStartDate();
                }

                if (m_termEndDate == null) {
                    m_termEndDate = scheduleTermDate.getEndDate();
                } else {
                    m_termEndDate = m_termEndDate.after(scheduleTermDate.getEndDate()) ? m_termEndDate
                            : scheduleTermDate.getEndDate();
                }


            }

            if (m_termStartDate != null && m_termStartDate.before(m_currentContext.getStartDate())) {
                m_termStartDate = m_currentContext.getStartDate();
            }

            if (m_termEndDate != null && m_termEndDate.after(m_currentContext.getEndDate())) {
                m_termEndDate = m_currentContext.getEndDate();
            }
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

    }

    /*
     * Constants:
     * Alias definitions
     */
    private static final String ALIAS_EXCLUDE_STF = "DOE EXCLUDE STF";
    private static final String ALIAS_EXCLUDE_MST = "DOE EXCLUDE MST";
    private static final String ALIAS_EXCLUDE_CRS = "DOE EXCLUDE CRS";
    private static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    private static final String PARAM_PRIMARY_ONLY = "primaryOnly";
    private static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";
    private static final String RETRIVE_INDICATOR_AND_DATES = "RTRV-I-AND-D";

    /*
     * Instance variables:
     * Fields for aliases
     */
    protected DateAsStringConverter m_dateConverter;
    protected String m_fieldExcludeCourse;
    protected String m_fieldExcludeSection;
    protected String m_fieldExcludeStaff;
    protected Boolean m_primaryOnly;
    protected PlainDate m_reportDate;
    protected Map m_excludeSchoolMap;
    protected String m_excludeSchool;
    protected Boolean m_excludeSchoolParam;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for staff schedule to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        m_reportDate = new PlainDate();
        m_dateConverter = (DateAsStringConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER,
                LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()),
                true);
        initializeFields();

        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_excludeSchoolParam = (Boolean) getParameter(PARAM_EXCLUDE_SCHOOL);

        if (((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue()) {
            loadSchoolExcludeMap();
        }

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Set the query to be used for student selection.
            setEntityClass(StaffScheduleEntity.class);
            setQuery(getStaffScheduleQuery());
        }

        // Build a map of calculations/retrievers
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put("TRN-COURSE", new RetriveCourse());
        calcs.put(RETRIVE_INDICATOR_AND_DATES, new RetriveIndicatorAndDates());

        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(ValidateInstruction.CALC_ID, new ValidateInstruction());
        super.addValidators(validators);

        super.addCalcs(calcs);
    }

    /**
     * Build a query of staff schedules for the current year.
     *
     * @return QueryByCriteria
     */
    private QueryByCriteria getStaffScheduleQuery() {
        X2Criteria criteria = new X2Criteria();

        // Active schedule classes.
        criteria.addEqualToField(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                Section.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                        Section.COL_SCHEDULE_OID);

        // "Class" type classes.
        criteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                Section.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE,
                SchoolCourse.MASTER_TYPE_CLASS);

        // Exclude flags.
        criteria.addNotEqualTo(ScheduleTeacher.REL_STAFF + PATH_DELIMITER + m_fieldExcludeStaff,
                BooleanAsStringConverter.TRUE);
        criteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + m_fieldExcludeSection,
                BooleanAsStringConverter.TRUE);
        criteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER + m_fieldExcludeCourse,
                BooleanAsStringConverter.TRUE);

        // Check the primary only parameter.
        if (m_primaryOnly != null && m_primaryOnly.booleanValue()) {
            criteria.addEqualTo(ScheduleTeacher.COL_PRIMARY_TEACHER_INDICATOR, Boolean.TRUE);
        }

        if (m_excludeSchoolParam != null && m_excludeSchoolParam.booleanValue()) {
            criteria.addNotEqualTo(ScheduleTeacher.REL_STAFF + PATH_DELIMITER + Staff.REL_SCHOOL + PATH_DELIMITER +
                    m_excludeSchool,
                    BooleanAsStringConverter.TRUE);
            criteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    Section.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER + m_excludeSchool,
                    BooleanAsStringConverter.TRUE);
        }

        // criteria from user input definition
        applyInputCriteria(criteria, false, ScheduleTeacher.REL_STAFF);

        // criteria for school selection.
        if (isSchoolContext()) {
            criteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else {
            criteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);

            criteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }

        // create query
        QueryByCriteria query = new QueryByCriteria(ScheduleTeacher.class, criteria);

        // Apply user sort.
        applyInputSort(query, ScheduleTeacher.REL_STAFF);
        query.addOrderBy(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.COL_COURSE_VIEW, true);

        return query;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldExcludeCourse = translateAliasToJavaName(ALIAS_EXCLUDE_CRS, true);
        m_fieldExcludeSection = translateAliasToJavaName(ALIAS_EXCLUDE_MST, true);
        m_fieldExcludeStaff = translateAliasToJavaName(ALIAS_EXCLUDE_STF, true);
        m_primaryOnly = (Boolean) getParameter(PARAM_PRIMARY_ONLY);
    }

    /**
     * Validate the Instruction Dates.
     */
    protected class ValidateInstruction implements FieldValidator {
        private static final String FIELD_TERM_END_DATE = "TermEndDate";
        private static final String FIELD_TERM_START_DATE = "TermStartDate";
        private static final String CALC_ID = "INSTR-VAL";
        private static final String PARAM_START = "InstructionStartDate";
        private static final String PARAM_END = "InstructionEndDate";
        private static final String DATE_FORMAT = "MM/dd/yyyy";
        private SimpleDateFormat m_dateFormatt = new SimpleDateFormat(DATE_FORMAT);

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String param = (String) field.getParameter();
            PlainDate date = formattDate(value);
            PlainDate neighbor = null;
            if (param.equals(PARAM_START)) {
                neighbor = formattDate(entity.getFieldValue(FIELD_TERM_START_DATE));
                if (neighbor != null && date != null) {
                    if (date.before(neighbor)) {
                        errors.add(new StateReportValidationError(entity, field,
                                param + " must be on or after TermStartDate",
                                field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                        "; TermStartDate = " + STYLE_BOLD + neighbor + STYLE_END));
                    }
                }
            }
            if (param.equals(PARAM_END)) {
                neighbor = formattDate(entity.getFieldValue(FIELD_TERM_END_DATE));
                if (neighbor != null && date != null) {
                    if (date.after(neighbor)) {
                        errors.add(new StateReportValidationError(entity, field,
                                param + " must be on or before TermEndDate",
                                field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                        "; TermStartDate = " + STYLE_BOLD + neighbor + STYLE_END));
                    }
                }
            }

            return errors;
        }

        /**
         * Formatt date.
         *
         * @param date String
         * @return PlainDate
         */
        private PlainDate formattDate(String date) {
            PlainDate returnDate = null;
            if (!StringUtils.isEmpty(date)) {
                try {
                    returnDate = new PlainDate(m_dateFormatt.parse(date));
                } catch (ParseException e) {
                    // Nothing to do

                }
            }
            return returnDate;
        }
    }

    /**
     * The Class RetriveCourse.
     */
    protected class RetriveCourse implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            StaffScheduleEntity myEntity = (StaffScheduleEntity) entity; // cast bean and data to
                                                                         // correct types

            if ("SCHOOL_CODE".equals(param)) {
                return myEntity.getSection().getSchool().getSchoolId();
            }
            return null;
        }
    }

    /**
     * The Class RetriveIndicatorAndDates.
     */
    protected class RetriveIndicatorAndDates implements FieldRetriever {
        private static final String PARAM_INSTRUCTION_END_DATE = "InstructionEndDate";
        private static final String PARAM_INSTRUCTION_START_DATE = "InstructionStartDate";
        private static final String PARAM_TERM_END_DATE = "TermEndDate";
        private static final String PARAM_TERM_START_DATE = "TermStartDate";
        private static final String PARAM_TEACHER_INDICATOR = "TeacherIndicator";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            StaffScheduleEntity myEntity = (StaffScheduleEntity) entity;

            String patameter = (String) field.getParameter();
            if (patameter.equals(PARAM_TEACHER_INDICATOR)) {
                return myEntity.getTeacherIndicator();
            }

            if (patameter.equals(PARAM_TERM_START_DATE)) {
                return myEntity.getTermStartDate();
            }

            if (patameter.equals(PARAM_TERM_END_DATE)) {
                return myEntity.getTermEndDate();
            }

            if (patameter.equals(PARAM_INSTRUCTION_START_DATE)) {
                return myEntity.getInstructionStartDate();
            }

            if (patameter.equals(PARAM_INSTRUCTION_END_DATE)) {
                return myEntity.getInstructionEndDate();
            }

            return null;

        }

    }

    /**
     * Load school exclude map.
     */
    private void loadSchoolExcludeMap() {
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(m_excludeSchool, BooleanAsStringConverter.TRUE);
        BeanQuery query = new BeanQuery(School.class, schoolCriteria);
        m_excludeSchoolMap = getBroker().getGroupedCollectionByQuery(query, X2BaseBean.COL_OID, 128);
    }

    /**
     * Include school.
     *
     * @param schoolOid String
     * @return true, if successful
     */
    public boolean includeSchool(String schoolOid) {
        boolean returnValue = false;
        if (m_excludeSchoolMap != null) {
            returnValue = m_excludeSchoolMap.containsKey(schoolOid);
        }
        return returnValue;
    }

}
