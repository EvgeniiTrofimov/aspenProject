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

package com.x2dev.procedures.statereporting.tn;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper.Strategy;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStaffMultiYearHelper;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisSchoolCalendarDate;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffCertification;
import com.x2dev.sis.model.beans.StaffPosition;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class for student extract export.
 */
public class TNStaffAssignmentData extends TNStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Entity class for staff member export.
     *
     */
    public static class TNStaffAssignmentEntity extends TNStateReportEntity {

        /**
         * Instantiates a new TN staff assignment entity.
         */
        public TNStaffAssignmentEntity() {
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
            Staff staff = ((StaffPosition) getBean()).getStaff();
            String name = staff.getNameView() +
                    " [LASID: " + staff.getLocalId() +
                    ", SASID: " + staff.getStateId() +
                    "] ";

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

            TNStaffAssignmentData tnData = (TNStaffAssignmentData) data;
            StaffPosition staffPosition = (StaffPosition) bean;
            PlainDate earliestDate = ((TNStaffAssignmentData) data).getFirstDate(staffPosition.getSchool(),
                    staffPosition.getStaff().getCalendarId());
            PlainDate sfpEndDate = staffPosition.getEndDate();

            if (sfpEndDate != null && earliestDate != null && earliestDate.after(sfpEndDate)) {
                setRowCount(0);
            }

            tnData.addEntityRowsCount(getRowCount());
        }
    }

    /**
     *
     * Field retriever for SSN.
     * This retriever normalize SSN value to format 999999999 (nine digits without any other
     * characters)
     *
     */
    protected class FieldRetrieverSSN implements FieldRetriever {
        protected static final String SFP_CALC_ID = "SFP_CALC_SSN";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStaffAssignmentEntity seEntity = (TNStaffAssignmentEntity) entity;

            StaffPosition position = (StaffPosition) seEntity.getBean();

            Staff staff = position.getStaff();

            if (staff == null) {
                return "";
            }

            Person psn = staff.getPerson();

            if (psn == null) {
                return "";
            }

            String ssn = psn.getPersonId();
            if (StringUtils.isEmpty(ssn)) {
                return "";
            }

            return ssn.replaceAll("([^\\d]?)", "");
        }

    }

    /**
     * Field validator for SSN.
     * Validates SSN against non valid formats
     * Valid ssn format: 999999999
     */
    protected class FiledValidatorSSN implements FieldValidator {
        protected static final String SFP_VAL_ID = "SFP_VAL_SSN";
        private static final String patternSSN = "^[0-9]{9}$|^$";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (value != null && !value.matches(patternSSN)) {
                errors.add(new StateReportValidationError(entity, field, "Invalid format",
                        "SSN value must be 9 digits long"));
            }

            return errors;
        }

    }

    /**
     * Field retriever for Current Assignment field.
     */
    protected class RetrieveCurrentAssignment implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStaffAssignmentEntity seEntity = (TNStaffAssignmentEntity) entity;
            StaffPosition staffPosition = (StaffPosition) seEntity.getBean();

            if (staffPosition == null) {
                return null;
            }
            String jobCode = staffPosition.getJobCode();
            String result = data.lookupReferenceCodeByBeanPath(StaffPosition.class,
                    StaffPosition.COL_JOB_CODE,
                    jobCode,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            if (!StringUtils.isEmpty(result)) {
                result = result.toUpperCase();
            }
            return result;
        }
    }

    /**
     * Field retriever for Current Assignment Begin Date field.
     */
    protected class RetrieveCurrentAssignmentBeginDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStaffAssignmentEntity seEntity = (TNStaffAssignmentEntity) entity;
            StaffPosition staffPosition = (StaffPosition) seEntity.getBean();

            PlainDate date = staffPosition.getStartDate();
            PlainDate earliestDate = ((TNStaffAssignmentData) data).getFirstDate(staffPosition.getSchool(),
                    staffPosition.getStaff().getCalendarId());

            if (earliestDate != null && earliestDate.after(date)) {
                date = earliestDate;
            }
            return date;
        }
    }


    /**
     * The Class RetrieveFundingSource.
     */
    protected class RetrieveFundingSource implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStaffAssignmentEntity seEntity = (TNStaffAssignmentEntity) entity;
            StaffPosition staffPosition = (StaffPosition) seEntity.getBean();
            String beanPath = data.translateAliasToJavaName((String) field.getParameter(), false);
            BigDecimal value = null;
            if (!StringUtils.isEmpty(beanPath)) {
                try {
                    value = (BigDecimal) data.getPropertyAsJavaType(staffPosition, beanPath);
                } catch (X2BaseException e) {
                    // do nothing if cast error
                }
            }
            if (value == null) {
                value = BigDecimal.ZERO;
            }
            return value.multiply(BigDecimal.valueOf(1000));
        }

    }

    /**
     * Field retriever for Current Assignment End Date field.
     */
    protected class RetrieveCurrentAssignmentEndDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStaffAssignmentEntity seEntity = (TNStaffAssignmentEntity) entity;
            StaffPosition staffPosition = (StaffPosition) seEntity.getBean();

            if (staffPosition == null) {
                return null;
            }
            return staffPosition.getEndDate();
        }
    }

    /**
     * Field retriever for School Year field.
     */
    protected class RetrieveSchoolYear implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStaffAssignmentData seData = (TNStaffAssignmentData) data;
            return seData.m_schoolYear;
        }
    }

    /**
     * Field retriever for Teacher LN.
     */
    protected class RetrieveTeacherLN implements FieldRetriever {
        private Map<String, String> m_licenses;

        /**
         * Instantiates a new retrieve teacher LN.
         *
         * @param broker X2Broker
         * @param staffPositionCriteria X2Criteria
         */
        public RetrieveTeacherLN(X2Broker broker, X2Criteria staffPositionCriteria) {
            PlainDate date = new PlainDate();
            X2Criteria criteria = new X2Criteria();

            // Select proper date range
            criteria.addLessOrEqualThan(StaffCertification.COL_ISSUE_DATE, date);
            X2Criteria endDate1Criteria = new X2Criteria();
            endDate1Criteria.addIsNull(StaffCertification.COL_EXPIRATION_DATE);
            X2Criteria endDate2Criteria = new X2Criteria();
            endDate2Criteria.addGreaterOrEqualThan(StaffCertification.COL_EXPIRATION_DATE, date);
            endDate1Criteria.addOrCriteria(endDate2Criteria);
            criteria.addAndCriteria(endDate1Criteria);

            // select primary only
            criteria.addEqualTo(StaffCertification.COL_PRIMARY_INDICATOR, Boolean.TRUE);

            // select proper staff
            criteria.addIn(StaffCertification.COL_STAFF_OID,
                    new SubQuery(StaffPosition.class, StaffPosition.COL_STAFF_OID, staffPositionCriteria));
            QueryByCriteria query = new QueryByCriteria(StaffCertification.class, criteria);
            query.addOrderByAscending(StaffCertification.COL_ISSUE_DATE);

            m_licenses = new HashMap<String, String>();
            QueryIterator iterator = broker.getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    StaffCertification cert = (StaffCertification) iterator.next();
                    m_licenses.put(cert.getStaffOid(), cert.getCertificationNumber());
                }
            } finally {
                iterator.close();
            }
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StaffPosition staffPosition = (StaffPosition) entity.getBean();
            SisStaff staff = staffPosition.getStaff();

            return m_licenses.get(staff.getOid());
        }
    }

    /**
     * Constants: Aliases, Fields, IDs, Parameters
     */
    protected static final String ALIAS_EXCLUDE_STF = "DOE EXCLUDE STF";
    protected static final String ALIAS_STATE_ID = "DOE SCHOOL STATE ID";

    private static final String CALC_ID_CURASGMNTBD = "SFP_CALC_CURASGMNTBD";
    private static final String CALC_ID_CURASGMNTED = "SFP_CALC_CURASGMNTED";
    private static final String CALC_ID_CURRASGMNT = "SFP_CALC_CURRASGMNT";
    private static final String CALC_ID_FUNDING_SOURCE = "SFP_CALC_FUNDSRC";
    private static final String CALC_ID_SCHOOLYEAR = "SFP_CALC_SCHOOLYEAR";
    private static final String CALC_ID_TEACHERLN = "SFP_CALC_TEACHERLN";

    /**
     * Instance variables.
     */
    protected String m_activeStatus;
    protected String m_schoolYear;
    private String m_fieldExcludeStf;
    private String m_fieldSchoolId;
    private Map<String, PlainDate> m_firstDates = new HashMap();

    private Boolean m_paramEntireSchool;
    private String m_paramSchoolOidStaff;
    private String m_paramStaffsOids;
    private TNMultiYearHelper m_multiYearHelper;

    /**
     * @see com.x2dev.procedures.statereporting.tn.TNStateReportData#initialize()
     */
    /*
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     * com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();
        // insure that all aliases and any other resources needed to operate the
        // report are available in the database.
        // setup error is created if the alias is not found.
        initializeFields();
        m_multiYearHelper = new TNStaffMultiYearHelper(getOrganization(), getCurrentContext(), getBroker());
        m_paramEntireSchool = Boolean.FALSE;
        m_paramSchoolOidStaff = (String) getParameter("schoolOidStaff");
        m_paramStaffsOids = (String) getParameter("staffsOids");
        if (m_paramStaffsOids == null || m_paramStaffsOids.isEmpty()) {
            m_paramEntireSchool = Boolean.TRUE;
        }
        if (getSetupErrors().size() != 0) {
            return;
        }

        initializeCriteriaAndQuery();

        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Sets the version as soon as the current context is determined.
     *
     * @param currentContext void
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#setCurrentContext(com.follett.fsc.core.k12.beans.DistrictSchoolYearContext)
     */
    @Override
    public void setCurrentContext(DistrictSchoolYearContext currentContext) {
        if (currentContext.getSchoolYear() > 2021) {
            setExportVersion(2);
        }
        super.setCurrentContext(currentContext);
    }

    /**
     * Gets the first date.
     *
     * @param school SisSchool
     * @param calendar String
     * @return Plain date
     */
    protected PlainDate getFirstDate(SisSchool school, String calendar) {
        PlainDate date = null;
        String key = StringUtils.isEmpty(calendar) ? school.getOid() : school.getOid() + calendar;
        if (m_firstDates.containsKey(key)) {
            date = m_firstDates.get(key);
        } else {
            date = getFirstCalendarDay(school, calendar);
            if (date == null) {
                date = getFirstCalendarDay(school, null);
            }
            m_firstDates.put(key, date);
        }
        return date;
    }

    /**
     * Method for implementing business rule for schoolYear
     * (CTX_SCHOOL_YEAR - 1) where reporting date falls within `CTX_START_DATE` and `CTX_END_DATE`.
     * else CTX_SCHOOL_YEAR
     *
     * @return string representation of school year
     */
    private final String getCurentSchoolYear() {
        return Integer.toString(getCurrentContext().getSchoolYear() - 1);
    }

    /**
     * Returns a set of days-in-session for the given school and calendar ID combination.
     *
     * @param school SisSchool
     * @param calendar String
     * @return Set of PlainDate objects
     */
    private PlainDate getFirstCalendarDay(SisSchool school, String calendar) {
        PlainDate firstDate = null;

        Criteria criteria = new Criteria();
        criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_SCHOOL_OID,
                school.getOid());
        criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                m_contextOid);
        if (!StringUtils.isEmpty(calendar)) {
            criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_CALENDAR_ID,
                    calendar);
        }
        criteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);

        QueryByCriteria calendarQuery = new QueryByCriteria(SisSchoolCalendarDate.class, criteria);
        calendarQuery.addOrderByAscending(SisSchoolCalendarDate.COL_DATE);
        QueryIterator calendars = null;
        try {
            calendars = getBroker().getIteratorByQuery(calendarQuery);
            if (calendars.hasNext()) {
                SisSchoolCalendarDate calendarDate = (SisSchoolCalendarDate) calendars.next();
                firstDate = calendarDate.getDate();
            }
        } finally {
            if (calendars != null) {
                calendars.close();
            }
        }

        return firstDate;
    }

    /**
     * Function for building custom StaffPosition criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getStaffPositionCriteria() {
        X2Criteria staffPositionCriteria = new X2Criteria();
        if (!StringUtils.isEmpty(m_fieldExcludeStf)) {
            m_multiYearHelper.adjustCriteria(staffPositionCriteria, Strategy.NOT_EQUAL_TO,
                    StaffPosition.REL_STAFF + PATH_DELIMITER + m_fieldExcludeStf, BooleanAsStringConverter.TRUE);
        }
        staffPositionCriteria.addNotEmpty(StaffPosition.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_fieldSchoolId,
                getBroker().getPersistenceKey());
        staffPositionCriteria.addNotEqualTo(StaffPosition.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
        staffPositionCriteria.addNotEqualTo(StaffPosition.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        addNotNullStateCodeCriteria(staffPositionCriteria);

        limitToSchoolYear(staffPositionCriteria);

        // add school context
        if (isSchoolContext()) {
            m_multiYearHelper.adjustCriteria(staffPositionCriteria, Strategy.EQUAL_TO,
                    StaffPosition.COL_SCHOOL_OID, getSchool().getOid());
        } else if (!StringUtils.isEmpty(m_paramSchoolOidStaff)) {
            m_multiYearHelper.adjustCriteria(staffPositionCriteria, Strategy.EQUAL_TO,
                    StaffPosition.COL_SCHOOL_OID, m_paramSchoolOidStaff);
        }
        if (m_paramStaffsOids != null && m_paramEntireSchool.booleanValue() == false) {
            String[] staffOids = m_paramStaffsOids.split(",");
            Collection<String> collection = Arrays.asList(staffOids);
            staffPositionCriteria.addIn(StaffPosition.REL_STAFF + PATH_DELIMITER + X2BaseBean.COL_OID, collection);
        }
        return staffPositionCriteria;
    }

    /**
     * Adds restriction to not take staffPosition records, which state code is null/empty
     *
     * @param staffPositionCriteria
     */
    private void addNotNullStateCodeCriteria(X2Criteria staffPositionCriteria) {
        staffPositionCriteria.addNotEmpty(StaffPosition.COL_JOB_CODE, getBroker().getPersistenceKey());
        DataDictionaryField dictionaryField = getDataDictionaryField(StaffPosition.class, StaffPosition.COL_JOB_CODE);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            X2Criteria jobCodeRefTablecriteria = new X2Criteria();
            // Criteria to get job codes with not empty state code
            jobCodeRefTablecriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID,
                    dictionaryField.getReferenceTableOid());
            jobCodeRefTablecriteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
            // Subquery which selects codes from 'job codes' reference table
            SubQuery jobCodeSubquery =
                    new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, jobCodeRefTablecriteria);
            staffPositionCriteria.addIn(StaffPosition.COL_JOB_CODE, jobCodeSubquery);
        }
    }

    /**
     * Initialize criteria and query.
     */
    private void initializeCriteriaAndQuery() {
        X2Criteria staffPositionCriteria = getStaffPositionCriteria();
        this.applyInputCriteria(staffPositionCriteria, true, StaffPosition.REL_STAFF);
        QueryByCriteria query = new QueryByCriteria(StaffPosition.class, staffPositionCriteria);

        setQuery(query);
        setEntityClass(TNStaffAssignmentEntity.class);
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_activeStatus = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
        m_schoolYear = getCurentSchoolYear();
        if (getParameter(PARAM_REPORT_DATE) != null) {
            m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        } else {
            m_reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
        }
        m_fieldExcludeStf = translateAliasToJavaName(ALIAS_EXCLUDE_STF, true);
        m_fieldSchoolId = translateAliasToJavaName(ALIAS_STATE_ID, true);
    }

    /**
     * Limit criteria by begin of school year.
     *
     * @param criteria X2Criteria
     */
    private void limitToSchoolYear(X2Criteria criteria) {
        // This criteria to provide match when school year and StaffPosition
        // overlap. Consider case with null end date for staff position
        PlainDate startDate = getCurrentContext().getStartDate();
        PlainDate endDate = getCurrentContext().getEndDate();

        X2Criteria criteriaStartDate = new X2Criteria();
        criteriaStartDate.addLessOrEqualThan(StaffPosition.COL_START_DATE, endDate);
        criteria.addAndCriteria(criteriaStartDate);

        X2Criteria criteriaEndDate = new X2Criteria();
        criteriaEndDate.addGreaterOrEqualThan(StaffPosition.COL_END_DATE, startDate);
        X2Criteria criteriaEndDateIsNull = new X2Criteria();
        criteriaEndDateIsNull.addIsNull(StaffPosition.COL_END_DATE);
        criteriaEndDate.addOrCriteria(criteriaEndDateIsNull);
        criteria.addAndCriteria(criteriaEndDate);
    }

    /**
     * Register custom field Retrievers & validators.
     */
    private void registerFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(CALC_ID_SCHOOLYEAR, new RetrieveSchoolYear());
        calcs.put(CALC_ID_CURRASGMNT, new RetrieveCurrentAssignment());
        calcs.put(CALC_ID_CURASGMNTBD, new RetrieveCurrentAssignmentBeginDate());
        calcs.put(CALC_ID_CURASGMNTED, new RetrieveCurrentAssignmentEndDate());
        calcs.put(CALC_ID_FUNDING_SOURCE, new RetrieveFundingSource());
        calcs.put(CALC_ID_TEACHERLN, new RetrieveTeacherLN(getBroker(), getStaffPositionCriteria()));
        calcs.put(FieldRetrieverSSN.SFP_CALC_ID, new FieldRetrieverSSN());
        super.addCalcs(calcs);
    }

    /**
     * Register custom field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(FiledValidatorSSN.SFP_VAL_ID, new FiledValidatorSSN());
        super.addValidators(validators);
    }
}
