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

package com.x2dev.procedures.statereporting.pa;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.NumberUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Export procedure for PIMS Course export.
 *
 */
public class PACourseCatalog extends StateReportData {

    /**
     * Entity class .
     */
    public static class PACourseCatalogEntity extends StateReportEntity {
        PACourseCatalog m_exportData;

        /**
         * Instantiates a new PA course catalog entity.
         */
        public PACourseCatalogEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SchoolCourse schoolCourse = ((MasterSchedule) getBean()).getSchoolCourse();

            String name = schoolCourse.getNumber() + " " + schoolCourse.getDescription()
                    + " [School: " + schoolCourse.getSchool().getName() + "]";
            return name;
        }

        /**
         * Intitialize.
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
            m_exportData = (PACourseCatalog) data;

            MasterSchedule mst = (MasterSchedule) bean;
            String distinctRowKey = mst.getSchoolCourseOid() + mst.getScheduleTerm().getCode();
            if (m_exportData.m_distinctCourses.contains(distinctRowKey)) {
                setRowCount(0);
            } else {
                m_exportData.m_distinctCourses.add(distinctRowKey);
            }
        }
    }

    /**
     * Field retriever for end of course exam.
     */
    protected class RetrieveEndCrsExam implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param reportEntity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity reportEntity, FieldDefinition field) {
            PACourseCatalogEntity entity = (PACourseCatalogEntity) reportEntity;
            PACourseCatalog paData = (PACourseCatalog) data;
            String crsAlternateCode = entity.getFieldValue(EXPORT_FIELD_ALTERNATE_CODE);
            String asdName = paData.m_asdNameMapByAlternateCode.get(crsAlternateCode);
            if (!StringUtils.isEmpty(asdName) && (asdName.contains(ASD_NAME_KALG) || asdName.contains(ASD_NAME_KBIO)
                    || asdName.contains(ASD_NAME_KLIT))) {
                return asdName;
            }
            return null;
        }
    }

    /**
     * Field retriever for course credits.
     */
    protected class RetrieveCourseCredits implements FieldRetriever {

        private static final String CALC_ID = "CRS_CREDITS";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity reportEntity, FieldDefinition field) {
            BigDecimal valueToReturn = null;
            PACourseCatalogEntity entity = (PACourseCatalogEntity) reportEntity;
            MasterSchedule mst = (MasterSchedule) entity.getBean();
            PACourseCatalog paData = (PACourseCatalog) data;
            Course crs = null;
            if (mst != null && mst.getSchoolCourse() != null && (crs = mst.getSchoolCourse().getCourse()) != null
                    && BooleanAsStringConverter.TRUE.equals(crs.getFieldValueByBeanPath(paData.m_fieldCrsDualFlag))
                    && crs.getFieldValueByBeanPath(paData.m_fieldCrsCredits) != null) {
                valueToReturn =
                        NumberUtils.getBigDecimal((String) crs.getFieldValueByBeanPath(paData.m_fieldCrsCredits));
                valueToReturn.setScale(2, RoundingMode.CEILING);
            }
            return valueToReturn;
        }
    }

    /**
     * Field retriever for course name with no commas.
     */
    protected class RetrieveCourseName implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param reportEntity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity reportEntity, FieldDefinition field) {
            PACourseCatalogEntity entity = (PACourseCatalogEntity) reportEntity;
            MasterSchedule mst = (MasterSchedule) entity.getBean();

            return mst.getSchoolCourse().getDescription().replaceAll(",", "/");
        }
    }

    /**
     * The Class RetrieveCourseOrgDef.
     */
    protected class RetrieveCourseOrgDef implements FieldRetriever {

        private static final String CALC_ID = "CSK_ORG_DEF_COURSE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param reportEntity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity reportEntity, FieldDefinition field) {
            PACourseCatalogEntity entity = (PACourseCatalogEntity) reportEntity;
            MasterSchedule mst = (MasterSchedule) entity.getBean();
            PACourseCatalog paData = (PACourseCatalog) data;
            String returnValue = null;
            Course crs = null;
            if (mst != null && mst.getSchoolCourse() != null && (crs = mst.getSchoolCourse().getCourse()) != null
                    && BooleanAsStringConverter.TRUE.equals(crs.getFieldValueByBeanPath(paData.m_fieldCrsDualFlag))) {
                returnValue = (String) crs.getFieldValueByBeanPath(paData.m_fieldCrsDualAun);
            }
            return returnValue;
        }
    }

    /**
     * Field retriever Course Terms.
     */
    protected class RetrieveCourseTerm implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param reportEntity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity reportEntity, FieldDefinition field) {
            PACourseCatalogEntity entity = (PACourseCatalogEntity) reportEntity;
            MasterSchedule mst = (MasterSchedule) entity.getBean();
            return mst.getScheduleTerm().getCode();
        }
    }

    /**
     * Field retriever Level Indicator.
     */
    protected class RetrieveIndicators implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param reportEntity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity reportEntity, FieldDefinition field) {
            PACourseCatalogEntity entity = (PACourseCatalogEntity) reportEntity;
            MasterSchedule mst = (MasterSchedule) entity.getBean();
            String parameter = (String) field.getParameter();
            Boolean value = Boolean.FALSE;
            String courseLevel = m_academicLevels.get(mst.getSchoolCourse().getCourseOid());
            if (CALC_PARAM_AP.equals(parameter)) {
                if (LEVELS_AP.equalsIgnoreCase(courseLevel)) {
                    value = Boolean.TRUE;
                }
            }
            return value;
        }
    }

    /**
     * Custom validation for required fields.
     */
    protected class ValidateFieldValue implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value required", "Value is empty"));
            }

            return errors;
        }
    }

    /**
     * Constants: Aliases, Fields, IDs, Parameters
     */
    private static final String ASD_NAME_KALG = "KALG";
    private static final String ASD_NAME_KBIO = "KBIO";
    private static final String ASD_NAME_KLIT = "KLIT";

    protected static final String ALIAS_ALTCOURSECODE = "DOE ALTERNATE";
    protected static final String ALIAS_ASD_PSSA_KEYSTONE_COURSES = "pssa-keystone-courses";
    protected static final String ALIAS_CRS_CREDITS = "all-crs-CollegeCredit";
    protected static final String ALIAS_CRS_DUAL_AUN = "all-crs-DualEnrollAUN";
    protected static final String ALIAS_CRS_DUAL_CREDIT_FLAG = "DOE DUAL";
    protected static final String ALIAS_EXCLUDE_COURSE = "DOE EXCLUDE CRS";
    protected static final String ALIAS_EXCLUDE_MST = "DOE EXCLUDE MST";

    private static final String CALC_ID_COURSENAME = "CSK_CALC_COURSENAME";
    private static final String CALC_ID_END_CRS_EXAM = "CSK_CALC_EXAM";
    private static final String CALC_ID_INDICATORS = "CSK_CALC_IND";
    private static final String CALC_ID_TERM = "CSK_CALC_TERM";

    private static final String CALC_PARAM_AP = "CSK_PARAM_AP";

    private static final String EXPORT_FIELD_ALTERNATE_CODE = "ALTERNATE COURSECODE";

    private static final String LEVELS_AP = "AP";

    private static final String VAL_ID_ISEMPTY = "CSK_VAL_ISEMPTY";

    /**
     * Instance variables.
     */
    protected Map<String, String> m_academicLevels;
    protected Map<String, String> m_asdNameMapByAlternateCode = new HashMap<>();
    protected Set<String> m_distinctCourses;
    protected String m_fieldAltCourseCode;
    protected String m_fieldASDKeystoneCourses;
    protected String m_fieldCrsCredits;
    protected String m_fieldCrsDualAun;
    protected String m_fieldCrsDualFlag;
    protected String m_fieldExcludeCourse;
    protected String m_fieldExcludeMst;

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    /*
     * Initialize the data module.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     * com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        m_distinctCourses = new HashSet<String>();
        initializeFields();
        populateASDMapByAlternateCode();

        if (getSetupErrors().size() != 0) {
            return;
        }

        /*
         * Build criteria, query and retrievers map.
         */
        X2Criteria mstCriteria = getMstCriteria();

        // Add criteria from input definition
        applyInputCriteria(mstCriteria, false, null);
        QueryByCriteria mstQuery = new QueryByCriteria(MasterSchedule.class, mstCriteria, true);
        applyInputSort(mstQuery, null);

        // Set the query to be used for selection.
        setQuery(mstQuery);
        setEntityClass(PACourseCatalogEntity.class);

        initCourseLevels();

        // Add retrievers & validation
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(CALC_ID_TERM, new RetrieveCourseTerm());
        calcs.put(CALC_ID_COURSENAME, new RetrieveCourseName());
        calcs.put(CALC_ID_INDICATORS, new RetrieveIndicators());
        calcs.put(CALC_ID_END_CRS_EXAM, new RetrieveEndCrsExam());
        calcs.put(RetrieveCourseOrgDef.CALC_ID, new RetrieveCourseOrgDef());
        calcs.put(RetrieveCourseCredits.CALC_ID, new RetrieveCourseCredits());
        super.addCalcs(calcs);

        HashMap validators = new HashMap<String, FieldValidator>();
        validators.put(VAL_ID_ISEMPTY, new ValidateFieldValue());
        super.addValidators(validators);
    }


    /**
     * Function for building custom Course Criteria.
     *
     * @return criteria
     */
    private X2Criteria getMstCriteria() {
        X2Criteria mstCriteria = new X2Criteria();

        // Filter by current year context
        mstCriteria.addEqualTo(
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER
                        + Course.COL_DISTRICT_CONTEXT_OID,
                getOrganization().getCurrentContextOid());

        // Courses with MASTER TYPE = Class
        mstCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE,
                SchoolCourse.MASTER_TYPE_CLASS);

        // Include Scheduled courses only
        mstCriteria.addEqualToField(MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, MasterSchedule.COL_SCHEDULE_OID);

        // Exclude sections where there is no enrollment, unless there were students earlier in the
        // year
        X2Criteria mstEnrCriteria = new X2Criteria();
        mstEnrCriteria.addGreaterOrEqualThan(MasterSchedule.COL_ENROLLMENT_TOTAL, Integer.valueOf(1));

        X2Criteria mstDroppedCriteria = new X2Criteria();
        mstDroppedCriteria.addIn(X2BaseBean.COL_OID,
                new SubQuery(StudentScheduleChange.class,
                        StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER + X2BaseBean.COL_OID,
                        getSccDroppedCriteria()));

        X2Criteria courseEmptyCriteria = new X2Criteria();
        courseEmptyCriteria.addOrCriteria(mstEnrCriteria);
        courseEmptyCriteria.addOrCriteria(mstDroppedCriteria);
        mstCriteria.addAndCriteria(courseEmptyCriteria);

        // Exclude sections where there is no primary staff
        mstCriteria.addNotEmpty(MasterSchedule.COL_PRIMARY_STAFF_OID, getBroker().getPersistenceKey());

        // Exclude courses manually flagged for exclusion by the user
        mstCriteria.addNotEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE +
                PATH_DELIMITER + m_fieldExcludeCourse, BooleanAsStringConverter.TRUE);

        // Exclude sections manually flagged for exclusion by the user
        mstCriteria.addNotEqualTo(m_fieldExcludeMst, BooleanAsStringConverter.TRUE);

        // Filter by selected school or eliminate unused schools.
        if (isSchoolContext()) {
            mstCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else {
            mstCriteria.addNotEqualTo(
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                            SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            mstCriteria.addNotEqualTo(
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                            SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }

        return mstCriteria;
    }

    /**
     * Gets the scc subquery criteria for dropped courses.
     *
     * @return X2Criteria
     */
    private X2Criteria getSccDroppedCriteria() {
        X2Criteria sccCriteria = new X2Criteria();

        sccCriteria.addEqualTo(StudentScheduleChange.COL_CHANGE_TYPE_CODE, StudentScheduleChange.CODE_DROP);

        sccCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER
                + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER
                + Course.COL_DISTRICT_CONTEXT_OID, getOrganization().getCurrentContextOid());

        sccCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER
                + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE,
                SchoolCourse.MASTER_TYPE_CLASS);

        sccCriteria.addEqualToField(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                        MasterSchedule.COL_SCHEDULE_OID);

        sccCriteria.addGreaterThanField(StudentScheduleChange.COL_EFFECTIVE_DATE,
                StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER + MasterSchedule.REL_SCHEDULE_TERM +
                        PATH_DELIMITER + ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER
                        + ScheduleTermDate.COL_START_DATE);

        sccCriteria.addNotEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER
                + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER
                + m_fieldExcludeCourse, BooleanAsStringConverter.TRUE);

        if (isSchoolContext()) {
            sccCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER
                    + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else {
            sccCriteria.addNotEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER
                    + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
        }

        return sccCriteria;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldExcludeCourse = translateAliasToJavaName(ALIAS_EXCLUDE_COURSE, true);
        m_fieldExcludeMst = translateAliasToJavaName(ALIAS_EXCLUDE_MST, true);
        m_fieldAltCourseCode = translateAliasToJavaName(ALIAS_ALTCOURSECODE, true);
        m_fieldASDKeystoneCourses = translateAliasToJavaName(ALIAS_ASD_PSSA_KEYSTONE_COURSES, true);
        m_fieldCrsDualAun = translateAliasToJavaName(ALIAS_CRS_DUAL_AUN, true);
        m_fieldCrsDualFlag = translateAliasToJavaName(ALIAS_CRS_DUAL_CREDIT_FLAG, true);
        m_fieldCrsCredits = translateAliasToJavaName(ALIAS_CRS_CREDITS, true);
    }

    /**
     * Inits the course levels.
     */
    private void initCourseLevels() {
        X2Criteria courseCriteria = new X2Criteria();
        courseCriteria.addEqualTo(SchoolCourse.REL_COURSE + PATH_DELIMITER + Course.COL_DISTRICT_CONTEXT_OID,
                getOrganization().getCurrentContextOid());

        String[] columns = new String[] {SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + Course.COL_ACADEMIC_LEVEL};
        ReportQueryByCriteria query = new ReportQueryByCriteria(SchoolCourse.class, columns, courseCriteria);
        query.addOrderByAscending(X2BaseBean.COL_OID);

        m_academicLevels = new HashMap<String, String>(1000);
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] record = (Object[]) iterator.next();
                String courseOid = (String) record[0];
                String academicLevel = (String) record[1];
                m_academicLevels.put(courseOid, academicLevel);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Get ASD records wherethe [pssa-keystone-courses] is not empty.
     *
     */
    private void populateASDMapByAlternateCode() {
        X2Criteria asdCrideria = new X2Criteria();
        asdCrideria.addNotEmpty(m_fieldASDKeystoneCourses, getBroker().getPersistenceKey());
        QueryByCriteria asdQuery = new QueryByCriteria(AssessmentDefinition.class, asdCrideria);
        Collection<AssessmentDefinition> asdList = getBroker().getCollectionByQuery(asdQuery);
        for (AssessmentDefinition asd : asdList) {
            String stateCodesValue = (String) asd.getFieldValueByBeanPath(m_fieldASDKeystoneCourses);
            if (!StringUtils.isEmpty(stateCodesValue)) {
                Set<String> stateCodesSetFromField = new HashSet<String>();
                stateCodesSetFromField.addAll(Arrays.asList(stateCodesValue.split(",")));
                for (String code : stateCodesSetFromField) {
                    if (!m_asdNameMapByAlternateCode.containsKey(code)) {
                        m_asdNameMapByAlternateCode.put(code, asd.getName());
                    }
                }
            }
        }
    }
}
