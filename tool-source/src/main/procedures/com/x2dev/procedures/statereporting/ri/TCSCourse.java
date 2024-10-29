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
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * RI state report for TCS Section Course export. This class implements the data
 * export for the TCS Section Course export.
 *
 * @author X2 Development Corporation
 */
public class TCSCourse extends RIStateReportData {
    /**
     * Implementation of StateReportEntity to be used by the TCS Section Course
     * export. This must be a public static inner class with a public no
     * argument constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class TCSCourseEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";
        TCSCourse m_TCSCourse;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public TCSCourseEntity() {
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
            SchoolCourse course = (SchoolCourse) getBean();

            String name = course.getNumber() + " " + course.getDescription() +
                    " [" + course.getSchool().getName() + "]";

            return name;
        }

        /**
         * Initialize the entity for the school course bean provided.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.
         *      stateexports.StateReportData,
         *      com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean)
                throws X2BaseException {
            super.intitialize(data, bean);
        }
    }

    /**
     * Alias fields
     */
    public static final String ALIAS_CSK_EXCLUDE = "all-csk-ExcludeFromTCS";
    public static final String ALIAS_EXCLUDE_CRS = "DOE EXCLUDE CRS";
    public static final String ALIAS_SCED_CODE = "RI Course ID";

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
     * Instance variables.
     */
    private String m_fieldCskExclude = null;
    private String m_fieldExlcudeCrs = null;
    private String m_orgFieldStr = null;
    private String m_orgOid = null;

    /**
     * Validator to check the District Code is present and length is 5.
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
     * Validator to check the Course Id is present and length is less or equals then 20.
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
                        "Course ID invalid.",
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
     * Validator to check the Course Title is present and length is less or equals then 100.
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateCourseTitle implements FieldValidator {
        public static final String VAL_ID = "VAL-COURSE-TITLE";


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
            if (StringUtils.isEmpty(value) || value.length() > 100) {
                errors.add(new StateReportValidationError(entity, field,
                        "Course Title invalid.",
                        "Course Title =" + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }

    }

    /**
     * Validator to check the SCED Course is present and length is 5.
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateSCEDCourse implements FieldValidator {
        public static final String VAL_ID = "VAL-SCED";


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
                        "SCED Code invalid.",
                        "SCED Course =" + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }

    }

    /**
     * Validator to check the Grade Span is present and length is 4.
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateGradeSpan implements FieldValidator {
        public static final String VAL_ID = "VAL-GRADE-SPAN";


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
            if (StringUtils.isEmpty(value) || value.length() < 4) {
                errors.add(new StateReportValidationError(entity, field,
                        "Value missing or too short.",
                        "Grade Span =" + STYLE_BOLD + value + STYLE_END));
            } else if (value.length() > 4) {
                errors.add(new StateReportValidationError(entity, field,
                        "Value too long.",
                        "Grade Span =" + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }

    }


    /**
     * Validator to check the assessment indicator is present when CIP is present.
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateAssessment implements FieldValidator {
        public static final String VAL_ID = "VAL-TCS-ASSESSMENT";

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
            String CIP = entity.getFieldValue("CIP");
            if (StringUtils.isEmpty(CIP)) {
                if (!StringUtils.isEmpty(value)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Assessment must be blank when CIP is blank.",
                            "Assessment=" + STYLE_BOLD + value + STYLE_END + ", CIP="));
                }
            } else if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Missing value.",
                        "Assessment= , CIP=" + STYLE_BOLD + CIP + STYLE_END));
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
            Criteria courseCriteria = getSchoolCourseCriteria();
            QueryByCriteria courseQuery = new QueryByCriteria(SchoolCourse.class,
                    courseCriteria);

            Integer sort = (Integer) getParameter(SORT_PARAM);
            switch (sort == null ? 0 : sort.intValue()) {
                case 1: // School, Number
                    courseQuery.addOrderByAscending(SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                            SisSchool.COL_NAME);
                    courseQuery.addOrderByAscending(SchoolCourse.COL_NUMBER);
                    break;

                default: // Number, School
                    courseQuery.addOrderByAscending(SchoolCourse.COL_NUMBER);
                    courseQuery.addOrderByAscending(SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                            SisSchool.COL_NAME);
                    break;
            }

            // Set the query to be used for student selection.
            setQuery(courseQuery);
            setEntityClass(TCSCourseEntity.class);

            HashMap validators = new HashMap<String, FieldRetriever>();
            validators.put(ValidateAssessment.VAL_ID, new ValidateAssessment());
            validators.put(ValidateGradeSpan.VAL_ID, new ValidateGradeSpan());
            validators.put(ValidateSCEDCourse.VAL_ID, new ValidateSCEDCourse());
            validators.put(ValidateCourseTitle.VAL_ID, new ValidateCourseTitle());
            validators.put(ValidateCourseId.VAL_ID, new ValidateCourseId());
            validators.put(ValidateSchoolCode.VAL_ID, new ValidateSchoolCode());
            validators.put(ValidateDistrCode.VAL_ID, new ValidateDistrCode());
            super.addValidators(validators);

        }
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
    private Criteria getSchoolCourseCriteria() {
        X2Criteria courseCriteria = new X2Criteria();
        courseCriteria.addEqualTo(SchoolCourse.COL_MASTER_TYPE, "Class");
        if (m_fieldCskExclude != null) {
            courseCriteria.addNotEqualTo(m_fieldCskExclude, BooleanAsStringConverter.TRUE);
        }
        if (isSchoolContext()) {
            courseCriteria.addEqualTo(SchoolCourse.COL_SCHOOL_OID, getSchool().getOid());
        } else if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
            courseCriteria.addEqualTo(SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                    m_orgFieldStr,
                    m_orgOid);
            courseCriteria.addNotEqualTo(SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            courseCriteria.addNotEqualTo(SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        } else {
            courseCriteria.addNotEqualTo(SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            courseCriteria.addNotEqualTo(SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }

        // Check if the course exclusion custom field is present.
        if (!StringUtils.isEmpty(m_fieldExlcudeCrs)) {
            courseCriteria.addNotEqualTo(SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_fieldExlcudeCrs,
                    BooleanAsStringConverter.TRUE);
        }

        // Check if the SCED code is required.
        Boolean requireSced = (Boolean) getParameter(REQUIRE_SCED_PARAM);
        if (requireSced != null && requireSced.booleanValue()) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SCED_CODE);
            if (field != null) {
                courseCriteria.addNotEmpty(SchoolCourse.REL_COURSE + PATH_DELIMITER +
                        field.getJavaName(),
                        getBroker().getPersistenceKey());
            }
        }

        // Check other user criteria.
        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        Integer queryBy = (Integer) getParameter(QUERY_BY_PARAM);
        switch (queryBy == null ? 0 : queryBy.intValue()) {
            case 1: // Only where a section exists.
                Criteria sectionCriteria = new Criteria();
                sectionCriteria.addEqualToField(MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                        MasterSchedule.COL_SCHEDULE_OID);
                sectionCriteria.addEqualToField(MasterSchedule.COL_SCHOOL_COURSE_OID,
                        Criteria.PARENT_QUERY_PREFIX + PATH_DELIMITER +
                                X2BaseBean.COL_OID);
                SubQuery sectionQuery = new SubQuery(MasterSchedule.class, X2BaseBean.COL_OID, sectionCriteria);
                courseCriteria.addExists(sectionQuery);
                break;

            case 2: // Snapshot of School Courses
                addRecordSetCriteria(courseCriteria, queryString);
                break;

            default:
                // Take all course in the current school year.
                courseCriteria.addEqualTo(SchoolCourse.REL_COURSE + PATH_DELIMITER +
                        Course.COL_DISTRICT_CONTEXT_OID,
                        getCurrentContext().getOid());
                break;
        }

        return courseCriteria;
    }

    /**
     * Initialize static report elements. Aliases.
     */
    private void initializeFields() {
        m_fieldExlcudeCrs = translateAliasToJavaName(ALIAS_EXCLUDE_CRS, false);
        m_fieldCskExclude = translateAliasToJavaName(ALIAS_CSK_EXCLUDE, false);
    }
}
