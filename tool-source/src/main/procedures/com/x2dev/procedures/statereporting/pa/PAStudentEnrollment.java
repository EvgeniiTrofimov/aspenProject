/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.pa;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.DataTableConfig.OrganizationAccess;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.ojb.broker.query.Criteria;

/**
 * The Class PAStudentEnrollment.
 */
public class PAStudentEnrollment extends StateReportData {

    /**
     * The entity class generates a separate record for each qualifying StudentEnrollment.
     */
    public static class PAStudentEnrollmentEntity extends StateReportEntity {

        /**
         * Helper class to store the state report values generated for a particular
         * StudentEnrollment.
         */
        public class EntityVars {
            private PlainDate m_activityDate;
            private String m_enrollmentCode;
            private PlainDate m_enrollmentDate;
            private String m_enrollmentGradeLevel;
            private String m_residenceStatus;
            private String m_locationCode;

            /**
             * Instantiates a new entity vars.
             *
             * @param locationCode String
             * @param activityDate PlainDate
             * @param enrollmentDate PlainDate
             * @param enrollmentCode String
             * @param enrollmentGradeLevel String
             * @param residenceStatus String
             */
            public EntityVars(String locationCode, PlainDate activityDate, PlainDate enrollmentDate,
                    String enrollmentCode,
                    String enrollmentGradeLevel, String residenceStatus) {
                m_locationCode = locationCode;
                m_activityDate = (activityDate != null) ? activityDate : enrollmentDate;
                m_enrollmentDate = enrollmentDate;
                m_enrollmentCode = enrollmentCode;
                m_enrollmentGradeLevel = enrollmentGradeLevel;
                m_residenceStatus = residenceStatus;
            }

            /**
             * @see java.lang.Object#equals(java.lang.Object)
             */
            @Override
            public boolean equals(Object obj) {
                if (!(obj instanceof EntityVars)) {
                    return false;
                }
                if (obj == this) {
                    return true;
                }

                EntityVars rhs = (EntityVars) obj;
                return new EqualsBuilder().
                // if deriving: appendSuper(super.equals(obj)).
                        append(m_locationCode, rhs.m_locationCode).append(m_activityDate, rhs.m_activityDate)
                        .append(m_enrollmentDate, rhs.m_enrollmentDate).append(m_enrollmentCode, rhs.m_enrollmentCode)
                        .append(m_enrollmentGradeLevel, rhs.m_enrollmentGradeLevel)
                        .append(m_residenceStatus, rhs.m_residenceStatus).isEquals();
            }

            /**
             * @see java.lang.Object#hashCode()
             */
            @Override
            public int hashCode() {
                return m_locationCode.hashCode() + m_enrollmentDate.hashCode() + m_enrollmentCode.hashCode();
            }

            /**
             * Gets the activity date.
             *
             * @return the activityDate
             */
            public PlainDate getActivityDate() {
                return m_activityDate;
            }

            /**
             * Gets the enrollment code.
             *
             * @return the enrollmentCode
             */
            public String getEnrollmentCode() {
                return m_enrollmentCode;
            }

            /**
             * Gets the enrollment date.
             *
             * @return the enrollmentDate
             */
            public PlainDate getEnrollmentDate() {
                return m_enrollmentDate;
            }

            /**
             * Gets the enrollment grade level.
             *
             * @return the enrollmentGradeLevel
             */
            public String getEnrollmentGradeLevel() {
                return m_enrollmentGradeLevel;
            }

            /**
             * Gets the location code.
             *
             * @return the location code
             */
            public String getLocationCode() {
                return m_locationCode;
            }

            /**
             * Gets the residence status.
             *
             * @return the enrollmentGradeLevel
             */
            public String getResidenceStatus() {
                return m_residenceStatus;
            }

        }

        PAStudentEnrollment m_sData;
        protected List<EntityVars> m_entVars;

        /**
         * Instantiates a new PA student enrollment entity.
         */
        public PAStudentEnrollmentEntity() {

        }

        /**
         * Return the vars for the current row.
         *
         * @return Entity vars
         */
        public EntityVars getCurrentVars() {
            return m_entVars.get(getCurrentRow());
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            EntityVars vars = getCurrentVars();
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    ", DATE: " + vars.getEnrollmentDate() +
                    ", LOC_CODE: " + vars.getLocationCode() +
                    "] ";

            return name;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_sData = (PAStudentEnrollment) data;
            SisStudent student = (SisStudent) getBean();
            m_entVars = new LinkedList<EntityVars>();

            StudentEnrollment processed = setFirstDayEnrollment(student);

            PlainDate startDate = getData().getOrganization().getCurrentContext().getStartDate();
            List<StudentEnrollment> enrollments = m_sData.m_helper.getStudentEnrollments(student);
            for (StudentEnrollment enrollment : enrollments) {
                if (enrollment.equals(processed)) {
                    // all enrollments after processed have been included
                    break;
                }
                if (!startDate.after(enrollment.getEnrollmentDate()) &&
                        !m_sData.m_reportDate.before(enrollment.getEnrollmentDate())) {
                    if (!data.isSchoolContext() || data.getSchool().getOid().equals(enrollment.getSchoolOid())) {
                        addEnrollment(enrollment, null);
                    }
                }
            }

            setRowCount(m_entVars.size());
            Collections.sort(m_entVars, new Comparator<EntityVars>() {
                @Override
                public int compare(EntityVars o1, EntityVars o2) {
                    return o1.getEnrollmentDate().compareTo(o2.getEnrollmentDate());
                }

            });
        }

        /**
         * Add an enrollment record to the list. Resolve state report values for various codes.
         *
         * @param enrollment StudentEnrollment
         * @param startDate PlainDate
         */
        private void addEnrollment(StudentEnrollment enrollment, PlainDate startDate) {
            String schoolCode = (String) enrollment.getSchool().getFieldValueByBeanPath(m_sData.m_fieldLocationCode);
            String activityDateString = (String) enrollment.getFieldValueByBeanPath(m_sData.m_fieldActivityDate);

            PlainDate activityDate = (PlainDate) m_sData.m_dateConverter.parseSystemString(activityDateString);

            PlainDate enrollmentDate = startDate == null ? enrollment.getEnrollmentDate() : startDate;

            String enrollmentCode = (String) enrollment.getFieldValueByBeanPath(m_sData.m_fieldSclEnrStatus);

            int yog = enrollment.getYog();
            String gradeLevel = m_sData.getGradeLevel(yog);
            gradeLevel = m_sData.lookupReferenceCodeByBeanPath(SisStudent.class,
                    SisStudent.COL_GRADE_LEVEL,
                    gradeLevel,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            String residenceStatus = (String) enrollment.getFieldValueByBeanPath(m_sData.m_fieldResStatus);
            residenceStatus = m_sData.lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                    m_sData.m_fieldResStatus,
                    residenceStatus,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());


            if (schoolCode != null) {
                EntityVars thisEntVars = new EntityVars(schoolCode, activityDate, enrollmentDate, enrollmentCode,
                        gradeLevel, residenceStatus);
                if (m_entVars != null && !m_entVars.contains(thisEntVars)) {
                    m_entVars.add(thisEntVars);
                }
            }

        }

        /**
         * return the last enrollment on or before the start date.
         *
         * @param span StudentEnrollmentSpan
         * @param startDate PlainDate
         * @return Student enrollment
         */
        private StudentEnrollment getFirstEnrollment(StudentEnrollmentSpan span, PlainDate startDate) {
            StudentEnrollment value = null;
            if (span.getFirstActiveEnrollment() != null) {
                Iterator<StudentEnrollment> iterator = span.getEnrollments().iterator();
                while (iterator.hasNext()) {
                    StudentEnrollment test = iterator.next();
                    if (test.equals(span.getFirstActiveEnrollment())) {
                        // found first candidate
                        value = test;
                        while (iterator.hasNext()) {
                            test = iterator.next();
                            if (span.getFirstInactiveEnrollment() != null
                                    && span.getFirstInactiveEnrollment().equals(test)) {
                                // no more active enrollments
                                break;
                            }
                            if (!test.getEnrollmentDate().after(startDate)) {
                                // additional candidate
                                value = test;
                            } else {
                                // past start date
                                break;
                            }
                        }
                        break;
                    }
                }
            }
            return value;
        }

        /**
         * Generate a record for the first day of school.
         *
         * This record should use the first matching enrollment span for the student in the current
         * year context.
         * It should then find the first active E record in the span and use this enrollment for the
         * initial enrollment.
         * Only add the enrollment if the span starts on or before the enrollment date.
         *
         * StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG and
         * StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS must both be false to insure
         * that the span includes an enrollment record. Records prior to the enrollment record
         * should be discarded and the last record
         * before or on the current context end date should be used.
         *
         * @param student SisStudent
         * @return StudentEnrollment
         */
        private StudentEnrollment setFirstDayEnrollment(SisStudent student) {
            StudentEnrollment value = null;
            PlainDate startDate = getData().getOrganization().getCurrentContext().getStartDate();
            for (StudentEnrollmentSpan span : m_sData.m_helper.getStudentEnrollmentSpans(student, true)) {
                if (!startDate.before(span.getFirstActiveEnrollment().getEnrollmentDate()) &&
                        (span.getFirstInactiveEnrollment() == null
                                || !startDate.after(span.getFirstInactiveEnrollment().getEnrollmentDate()))) {
                    if (!m_sData.isSchoolContext() ||
                            m_sData.getSchool().getOid().equals(span.getFirstActiveEnrollment().getSchool().getOid())) {
                        value = getFirstEnrollment(span, startDate);
                        if (value != null) {
                            addEnrollment(value, startDate);
                        }
                        break;
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retriever used to return value of the Enrollment Description export field.
     */
    public class RetrieveEnrollmentDesc implements FieldRetriever {

        /**
         * Constants
         */
        private static final String DISABLED_VALUE = "";
        private static final String ENABLED_VALUE = "DELETE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = DISABLED_VALUE;

            String fieldValue = (String) entity.getBean().getFieldValueByBeanPath(m_fieldEnrollDescField);

            if (!StringUtils.isEmpty(fieldValue) && BooleanAsStringConverter.TRUE.equals(fieldValue)) {
                value = ENABLED_VALUE;
            }

            return value;
        }

    }

    /**
     * Retriever used to return the appropriate value from EntitiyVars.
     */
    public class RetrieveEnrollmentInfo implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            PAStudentEnrollmentEntity ent = (PAStudentEnrollmentEntity) entity;
            PAStudentEnrollmentEntity.EntityVars vars = ent.getCurrentVars();
            String parameter = (String) field.getParameter();
            if (CALC_PARAM_LOCATION_CODE.equals(parameter)) {
                SisStudent std = (SisStudent) entity.getBean();
                PAStudentEnrollment enrData = (PAStudentEnrollment) data;
                if (std.getFieldValueByBeanPath(enrData.m_fieldStdSklOverride2) != null) {
                    return std.getFieldValueByBeanPath(enrData.m_fieldStdSklOverride2);
                }
                return vars.getLocationCode();
            } else if (CALC_PARAM_ACTIVDATE.equals(parameter)) {
                return vars.getActivityDate();
            } else if (CALC_PARAM_ENRCODE.equals(parameter)) {
                return vars.getEnrollmentCode();
            } else if (CALC_PARAM_ENRDATE.equals(parameter)) {
                return vars.getEnrollmentDate();
            } else if (CALC_PARAM_GRADE.equals(parameter)) {
                return vars.getEnrollmentGradeLevel();
            } else if (CALC_PARAM_RES_STATUS.equals(parameter)) {
                return vars.getResidenceStatus();
            }
            return null;
        }
    }

    /**
     * Validator to insure dates are not after the current date.
     */
    public class ValidateDate implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();
            PlainDate activeDate = (PlainDate) m_dateConverter.parseSystemString(value);
            Date now = new Date();

            if (activeDate != null && activeDate.after(now)) {
                errors.add(new StateReportValidationError(entity, field,
                        "The date can not be greater than current date", "Field value date " +
                                value +
                                " is greater than now"));
            }

            return errors;
        }
    }

    public static final String ALIAS_ACTIVITY_DATE = "DOE ACTIVITY DATE";
    public static final String ALIAS_DOE_ENROLL = "DOE ENROLL";
    public static final String ALIAS_RESIDENCE_STATUS = "DOE RESIDENCE STATUS";
    public static final String ALIAS_SCHOOL_LOCATION = "DOE SCHOOL STATE ID";
    public static final String ALIAS_SCHOOL_ENR_STATUS = "PA_School_Enrollment_Status";
    public static final String ALIAS_STD_SKL_OVERRIDE_2 = "all-std-LocationCodeOverride2";

    private static final Object CALC_ID_ENR_DESC = "ENR_DESC";
    public static final String CALC_ID = "ENR_CALC";
    public static final String CALC_PARAM_ACTIVDATE = "ACTIVE_DATE";
    public static final String CALC_PARAM_ENRCODE = "CODE";
    public static final String CALC_PARAM_ENRDATE = "DATE";
    public static final String CALC_PARAM_GRADE = "GRADE";
    public static final String CALC_PARAM_LOCATION_CODE = "LOC_CODE";
    public static final String CALC_PARAM_RES_STATUS = "RES_STATUS";

    public static final String PARAM_REPORT_DATE = "reportDate";

    public static final String VAL_ID_ACTIVDATE = "ENR_VAL_ACTIVDATE";
    public static final String VAL_ID_ENRDATE = "ENR_VAL_ENRDATE";

    DateAsStringConverter m_dateConverter =
            (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                    Locale.getDefault(),
                    true);
    String m_fieldActivityDate;
    String m_fieldEnrollDescField;
    String m_fieldLocationCode;
    String m_fieldResStatus;
    String m_fieldSclEnrStatus;
    String m_fieldStdSklOverride2;
    StudentHistoryHelper m_helper;
    PlainDate m_reportDate;
    TreeMap m_sortedGradeLevels;

    /**
     * Generate the appropriate grade level for the yog.
     *
     * @param yog int
     * @return String
     */
    String getGradeLevel(int yog) {
        if (m_sortedGradeLevels == null) {
            m_sortedGradeLevels = StudentManager.buildGradeLevelMap(getBroker());
        }
        List<String> matchingGradeLevels =
                StudentManager.getMatchingGradeLevels(StudentManager.getMaxGradeLevel(getBroker()),
                        yog,
                        getOrganization().getCurrentContext().getSchoolYear(),
                        m_sortedGradeLevels);
        return matchingGradeLevels == null || matchingGradeLevels.size() == 0 ? "" : matchingGradeLevels.get(0);
    }

    /**
     * Apply the standard user input criteria from input template to a Criteria object.
     *
     * <p>
     * Multiple criteria supported.
     * <br>
     * Iterate through enumerated values of PARAM_QUERY_BY_FIELD + "#" as "#" increments from 1
     * to n.
     *
     * <p>
     * The prefix path is the relationship bean path from to apply to the current bean to take it
     * to the
     * expected base bean of the criteria.
     * <br>
     * EX: If the user input selection criteria is for the Student table, but the criteria is
     * for a related
     * table StudentEnrollment, the prefix path would be "student" (or
     * StudentEnrollment.REL_STUDENT)
     * to get from the StudentEnrollment to the Student table.
     *
     * @param criteria Criteria
     * @param applySchool boolean
     * @param prefixPath String
     */
    @Override
    public void applyInputCriteria(Criteria criteria, boolean applySchool, String prefixPath) {
        int queryCount = 1;
        String fullPrefixPath = EMPTY_STRING;
        if (!StringUtils.isEmpty(prefixPath)) {
            fullPrefixPath = prefixPath + ModelProperty.PATH_DELIMITER;
        }

        String queryBy = (String) getParameter(PARAM_QUERY_BY_FIELD + Integer.toString(queryCount));
        String queryString = (String) getParameter(PARAM_QUERY_BY_CRITERIA + Integer.toString(queryCount));
        while (!StringUtils.isEmpty(queryBy) && !StringUtils.isEmpty(queryString)) {
            if (queryBy.equals("##all")) {
                // Do nothing, select all.
            } else if (queryBy.equals("##snapshot")) {
                SubQuery recordSetSubquery = ReportUtils.getRecordSetSubQuery(queryString, getUser(), getSchool());
                Collection<String> objectOids = getBroker().getSubQueryCollectionByQuery(recordSetSubquery);
                criteria.addIn(fullPrefixPath + X2BaseBean.COL_OID, objectOids);
            } else if (queryBy.startsWith("a:")) {
                String resolvedAliasBeanPath = getResolvedAliasBeanPath(queryBy);
                if (resolvedAliasBeanPath != null) {
                    criteria.addEqualTo(fullPrefixPath + resolvedAliasBeanPath, queryString);
                }
            } else if (queryBy.equals("stateIds")) {
                if (!StringUtils.isEmpty(queryString)) {
                    List<String> idList = StringUtils.convertDelimitedStringToList(queryString, ",", true);
                    criteria.addIn(fullPrefixPath + Student.COL_STATE_ID, idList);
                }
            } else if (queryBy.equals("localIds")) {
                if (!StringUtils.isEmpty(queryString)) {
                    List<String> idList = StringUtils.convertDelimitedStringToList(queryString, ",", true);
                    criteria.addIn(fullPrefixPath + Student.COL_LOCAL_ID, idList);
                }
            } else {
                criteria.addEqualTo(fullPrefixPath + queryBy, queryString);
            }

            queryCount++;
            queryBy = (String) getParameter(PARAM_QUERY_BY_FIELD + Integer.toString(queryCount));
            queryString = (String) getParameter(PARAM_QUERY_BY_CRITERIA + Integer.toString(queryCount));
        }

        if (applySchool) {
            /*
             * Add School criteria.
             */
            if (isSchoolContext()) {
                DataDictionaryTable table =
                        getDataDictionary().findDataDictionaryTableByClass(getBeanClass().getName());
                if (table != null && !StringUtils.isEmpty(table.getDataTableConfig().getSchoolPath())) {
                    ModelProperty property = new ModelProperty(table.getDataTableConfig().getSchoolPath(),
                            getBroker().getPersistenceKey());
                    criteria.addEqualTo(fullPrefixPath + property.getBeanPath(), getSchool().getOid());
                }
            }

            /*
             * Add Organization criteria.
             */
            DataDictionaryTable table = getDataDictionary().findDataDictionaryTableByClass(getBeanClass().getName());
            if (table != null && !StringUtils.isEmpty(table.getDataTableConfig().getOrganizationPath())) {
                int level = getOrganization().getOrganizationDefinition().getLevel();
                DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
                Collection<ModelProperty> properties =
                        OrganizationManager.getOrganizationPaths(table.getBeanClass(), dictionary, level);

                if (!CollectionUtils.isEmpty(properties)) {
                    for (ModelProperty property : properties) {
                        criteria.addAndCriteria(OrganizationManager.getOrganizationAccessCriteria(getOrganization(),
                                property, OrganizationAccess.NONE, OrganizationAccess.READ_WRITE));
                    }
                }
            }
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() {
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        initializeFields();
        if (getSetupErrors().size() == 0) {
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.FALSE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.FALSE);

            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(PAStudentEnrollmentEntity.class);

            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALC_ID, new RetrieveEnrollmentInfo());
            calcs.put(CALC_ID_ENR_DESC, new RetrieveEnrollmentDesc());
            super.addCalcs(calcs);

            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put(VAL_ID_ACTIVDATE, new ValidateDate());
            validators.put(VAL_ID_ENRDATE, new ValidateDate());
            super.addValidators(validators);
        }
    }

    /**
     * Initialize fields.
     */
    private void initializeFields() {
        m_fieldEnrollDescField = translateAliasToJavaName(ALIAS_DOE_ENROLL, true);
        m_fieldResStatus = translateAliasToJavaName(ALIAS_RESIDENCE_STATUS, true);
        m_fieldActivityDate = translateAliasToJavaName(ALIAS_ACTIVITY_DATE, true);
        m_fieldLocationCode = translateAliasToJavaName(ALIAS_SCHOOL_LOCATION, true);
        m_fieldSclEnrStatus = translateAliasToJavaName(ALIAS_SCHOOL_ENR_STATUS, true);
        m_fieldStdSklOverride2 = translateAliasToJavaName(ALIAS_STD_SKL_OVERRIDE_2, true);
    }
}
