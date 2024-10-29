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

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffPosition;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.time.Period;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class PAStaffExport.
 */
public class PAStaffExport extends StateReportData {
    /**
     * Entity class for PA Staff Instructor export.
     *
     */
    public static class PAStaffExportEntity extends StateReportEntity {
        /**
         * Entity instance variables.
         */
        PAStaffExport m_exportData;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public PAStaffExportEntity() {
            // Empty constructor.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            Staff staff = (Staff) getBean();
            String name = staff.getNameView() +
                    " [LASID: " + staff.getLocalId() +
                    ", SASID: " + staff.getStateId() +
                    "]";

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
        }
    }

    /**
     * The Class RetrieverGenderIdentity.
     */
    protected class RetrieverGenderIdentity implements FieldRetriever {

        private static final String CALC_ID = "STF_GENDER_IDENTITY";

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
            PAStaffExport stfData = (PAStaffExport) data;
            Staff staff = (Staff) entity.getBean();
            return BooleanAsStringConverter.TRUE.equals(staff.getFieldValueByBeanPath(stfData.m_stfGenderIndentity))
                    ? "X"
                    : "";
        }
    }

    /**
     * The Class RetrieverJobCode.
     */
    protected class RetrieverJobCode implements FieldRetriever {

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
            PAStaffExport stfData = (PAStaffExport) data;
            Staff staff = (Staff) entity.getBean();
            String jobCode = null;
            Collection<StaffPosition> positions = stfData.m_positions.get(staff.getOid());
            if (positions != null && !positions.isEmpty()) {
                Optional<StaffPosition> match = positions.stream().filter(sfp -> BooleanAsStringConverter.TRUE
                        .equals(sfp.getFieldValueByBeanPath(m_sfpPrimaryAssignment))).findFirst();
                if (match.isPresent()) {
                    jobCode = data.lookupStateValue(StaffPosition.class, StaffPosition.COL_JOB_CODE,
                            match.get().getJobCode());
                } else {
                    jobCode = data.lookupStateValue(StaffPosition.class, StaffPosition.COL_JOB_CODE,
                            positions.iterator().next().getJobCode());
                }
            }
            return jobCode;
        }

    }

    /**
     * Retriever for mailing address info.
     */
    protected class RetrieveMailingAddrInfo implements FieldRetriever {
        private static final String PARAM_ADDRESS01 = "ADDRESS01";
        private static final String PARAM_ADDRESS02 = "ADDRESS02";
        private static final String PARAM_CITY = "CITY";
        private static final String PARAM_STATE = "STATE";
        private static final String PARAM_ZIPCODE = "ZIPCODE";
        public static final String STF_CALC_MAILING = "STF_CALC_MAILADDRESS";

        /**
         * Gets value for corresponding field.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return value or null
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Staff staff = (Staff) entity.getBean();
            Person staffPerson = staff.getPerson();

            Address mailingAddr = staffPerson.getMailingAddress();
            Address physicalAddr = staffPerson.getPhysicalAddress();
            boolean useMailingAddr = staffPerson.getUseMailAddressIndicator();

            Address pullAddress = useMailingAddr ? mailingAddr : physicalAddr;

            if (pullAddress == null) {
                return null;
            }

            String param = (String) field.getParameter();

            if (param.equals(PARAM_ADDRESS01)) {
                return pullAddress.getAddressLine01();
            }

            if (param.equals(PARAM_ADDRESS02)) {
                return pullAddress.getAddressLine02();
            }

            if (param.equals(PARAM_CITY)) {
                return pullAddress.getCity();
            }

            if (param.equals(PARAM_STATE)) {
                return pullAddress.getState();
            }

            if (param.equals(PARAM_ZIPCODE)) {
                return pullAddress.getPostalCode();
            }

            return null;
        }
    }

    /**
     * Retriever for Race field.
     */
    protected class RetrieveRaceCodeField implements FieldRetriever {

        /**
         * Gets value for corresponding field.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return value or null
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            PAStaffExport stfData = (PAStaffExport) data;
            Staff staff = (Staff) entity.getBean();
            Person staffPerson = staff.getPerson();
            if (staffPerson == null) {
                return null;
            }

            String key = staffPerson.getOid();
            if (!stfData.m_races.containsKey(key)) {
                return null;
            }

            if (staffPerson.getHispanicLatinoIndicator()) {
                return "4";
            }

            Collection<Race> races = stfData.m_races.get(key);
            if (races == null) {
                return null;
            }

            if (races.size() > 1) {
                return "6";
            }

            if (races.size() == 1) {
                String raceCode = races.iterator().next().getRaceCode();

                return lookupReferenceCodeByBeanPath(Race.class,
                        Race.COL_RACE_CODE,
                        raceCode,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }

            return null;
        }
    }

    /**
     * Retriever for report date.
     */
    protected class RetrieveReportDate implements FieldRetriever {

        /**
         * Gets value for corresponding field.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return m_reportDate or empty string
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            PAStaffExport reportData = (PAStaffExport) data;

            return reportData.m_isSnapshot.booleanValue() ? m_reportDate : "";
        }
    }

    /**
     * Retriever for count of years experience.
     */
    protected class RetrieveYearsExperience implements FieldRetriever {

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
            PAStaffExportEntity entity = (PAStaffExportEntity) reportEntity;
            PAStaffExport reportData = (PAStaffExport) data;

            PlainDate hireDate = ((Staff) entity.getBean()).getHireDate();
            PlainDate reportDate = reportData.m_reportDate;

            // if hire date empty in database
            if (hireDate == null) {
                return Integer.valueOf(1);
            }

            Calendar hireCalendar = Calendar.getInstance();
            hireCalendar.clear();
            hireCalendar.setTime(hireDate);

            Calendar reportCalendar = Calendar.getInstance();
            reportCalendar.clear();
            reportCalendar.setTime(reportDate);

            int diff = reportCalendar.get(Calendar.YEAR) - hireCalendar.get(Calendar.YEAR);
            if (diff < 2) {
                diff = 1;
            }

            return Integer.valueOf(diff);
        }
    }

    /**
     * Retrieve teaching years of the staff.
     *
     * @author X2 Development Corporation
     *
     */
    public class RetrieveYearsTeaching implements FieldRetriever {

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
            int teachYear = 9999;
            int value = 0;
            if (m_stfYearStartTeachPath != null) {
                SisStaff staff = (SisStaff) entity.getBean();
                Object yearStartTeach = getPropertyAsJavaType(staff, m_stfYearStartTeachPath);

                if (yearStartTeach != null) {
                    int currYear = getCurrentContext().getSchoolYear();

                    if (yearStartTeach instanceof String) {
                        try {
                            teachYear = Integer.valueOf((String) yearStartTeach).intValue();
                        } catch (NumberFormatException nfe) {
                            // Do nothing - bad string value.
                        }
                    } else if (yearStartTeach instanceof Number) {
                        teachYear = ((Number) yearStartTeach).intValue();
                    }

                    if (currYear - teachYear >= 0) {
                        value = currYear - teachYear;
                    }
                }
            }
            return Integer.valueOf(value);
        }
    }

    /**
     * *
     *
     * Field validator for empty values for security staff.
     */
    protected class ValidateAge implements FieldValidator {
        protected static final String VAL_ID = "STF_VAL_AGE";

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
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            PlainDate birthDate = ((SisStaff) entity.getBean()).getPerson().getDob();
            if (birthDate != null && birthDate.before(m_reportDate)) {
                int age = Period.between(birthDate.toLocalDate(), m_reportDate.toLocalDate()).getYears();
                if (age < 22 || age > 80) {
                    errors.add(new StateReportValidationError(entity, field, "Incorrect value",
                            "Age must be within the range of 22 and 80. Age = " + age));
                }
            }
            return errors;
        }
    }

    /**
     * *
     *
     * Field validator for empty values.
     */
    protected class ValidateEmptySecurityValue implements FieldValidator {
        public static final String STF_VAL_EMPTY_SECURITY = "STF_VAL_EMPTY_SEC";

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
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            PAStaffExport reportData = (PAStaffExport) data;

            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            value = value == null ? "" : value.trim();

            if (StringUtils.isEmpty(value) && reportData.m_securityStaff.booleanValue()) {
                errors.add(new StateReportValidationError(entity, field, "Value Required",
                        "This field must contain a value"));
                return errors;
            }
            return errors;
        }
    }

    /**
     * *
     *
     * Field validator for empty values for security staff.
     */
    protected class ValidateEmptyValue implements FieldValidator {
        public static final String STF_VAL_EMPTY = "STF_VAL_EMPTY";

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
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            value = value == null ? "" : value.trim();

            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value Required",
                        "This field must contain a value"));
                return errors;
            }
            return errors;
        }
    }

    /**
     * Validator for Exit date of Staff.
     */
    protected class ValidateTermination implements FieldValidator {
        private static final String FIELD_EXIT_DATE = "EXIT DATE";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            String exitDateString = entity.getFieldValue(FIELD_EXIT_DATE);

            if (exitDateString.length() != 0) {
                if (StringUtils.isEmpty(value)) {
                    errors.add(new StateReportValidationError(entity, field, "Value Required",
                            "This field must contain a Termination code value"));
                }
            }
            return errors;
        }
    }

    /**
     * Constants: Aliases, Fields, IDs, Parameters
     */
    protected static final String ALIAS_DOE_SEC_QUAL = "DOE SECURITY QUALIFICATION";
    protected static final String ALIAS_EXCLUDE_STF = "DOE EXCLUDE STF";
    protected static final String ALIAS_EXIT_DATE = "DOE EXIT DATE";
    protected static final String ALIAS_SFP_PRIMARY_INDICATOR = "all-sfp-PrimaryIndicator";
    protected static final String ALIAS_STF_GENDER_IDENTITY = "all-stf-GenderIdentity";
    protected static final String ALIAS_STF_BEGAN_TEACH = "all-stf-CRDCYearBeganTeaching";


    protected static final String CALC_ID_HIREDATE = "STF_CALC_HIREDATE";
    protected static final String CALC_ID_JOBCODE = "STF_CALC_JOBCODE";
    protected static final String CALC_ID_RACECODE = "STF_CALC_RACECODE";
    protected static final String CALC_ID_REPORT_DATE = "STF_CALC_REPORTDATE";
    protected static final String CALC_ID_YEARS_TEACH = "STF_YEARS_TEACH";

    protected static final String PARAM_IS_SNAPSHOT = "isSnapshot";

    protected static final String PARAM_SECURITY_STAFF = "securityStaff";
    protected static final String VAL_ID_TERMINATION = "STF_VAL_TERMINATION";
    /*
     * User input parameters
     */
    private static final String PARAM_PREVIOUS_DATE = "previousDate";
    private static final String PARAM_REPORT_DATE = "reportDate";

    /**
     * Instance variables.
     */
    protected String m_activeStatus;
    protected String m_fieldExcludeStaff;
    protected String m_fieldExitDate;
    protected String m_sfpPrimaryAssignment;
    protected String m_stfGenderIndentity;
    protected String m_stfYearStartTeachPath;

    protected Boolean m_isSnapshot;
    protected PlainDate m_previousDate;

    protected Map<String, Collection<StaffPosition>> m_positions;
    protected Map<String, Collection<Race>> m_races;
    protected PlainDate m_reportDate;

    protected String m_securityQualification;
    protected Boolean m_securityStaff;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @throws X2BaseException
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();
        initializeFields();

        if (getSetupErrors().size() != 0) {
            return;
        }

        X2Criteria staffCriteria = new X2Criteria();

        if (m_securityStaff.booleanValue()) {
            staffCriteria.addNotEmpty(m_securityQualification, getBroker().getPersistenceKey());
        } else {
            staffCriteria.addEmpty(m_securityQualification, getBroker().getPersistenceKey());
        }
        // Staff Status Codes
        // Security Qualification Codes

        // add standard staff exclude indicator
        staffCriteria.addNotEqualTo(m_fieldExcludeStaff, BooleanAsStringConverter.TRUE);
        // add Active status
        staffCriteria.addEqualTo(Staff.COL_STATUS, m_activeStatus);
        // add criteria from input definition
        applyInputCriteria(staffCriteria, true, null);
        // add school context
        if (isSchoolContext()) {
            staffCriteria.addEqualTo(Staff.COL_SCHOOL_OID, getSchool().getOid());
        }

        if (m_isSnapshot != null && m_isSnapshot.booleanValue()) {
            X2Criteria greater = new X2Criteria();
            greater.addGreaterOrEqualThan(m_fieldExitDate, m_previousDate.toString());
            X2Criteria empty = new X2Criteria();
            empty.addEmpty(m_fieldExitDate, getBroker().getPersistenceKey());
            empty.addOrCriteria(greater);
            staffCriteria.addAndCriteria(empty);
        }

        // create query - use the appropriate class
        QueryByCriteria query = new QueryByCriteria(Staff.class, staffCriteria);
        applyInputSort(query, null);

        setQuery(query);
        setEntityClass(PAStaffExportEntity.class);

        // load race
        SubQuery raceSubquery = new SubQuery(Staff.class, Staff.COL_PERSON_OID, staffCriteria);
        X2Criteria raceCriteria = new X2Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID, raceSubquery);
        BeanQuery raceQuery = new BeanQuery(Race.class, raceCriteria);
        raceQuery.addOrderBy(Race.COL_PERSON_OID, true);
        m_races = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 1000);

        // load positions
        SubQuery positionSubquery = new SubQuery(Staff.class, X2BaseBean.COL_OID, staffCriteria);
        X2Criteria positionCriteria = new X2Criteria();
        positionCriteria.addIn(StaffPosition.COL_STAFF_OID, positionSubquery);
        positionCriteria.addLessOrEqualThan(StaffPosition.COL_START_DATE, m_reportDate);


        X2Criteria endDateCriteria = new X2Criteria();
        endDateCriteria.addEmpty(StaffPosition.COL_END_DATE, getBroker().getPersistenceKey());
        X2Criteria notBeforeSubmissionStart = new X2Criteria();
        notBeforeSubmissionStart.addGreaterThan(StaffPosition.COL_END_DATE, m_reportDate);
        endDateCriteria.addOrCriteria(notBeforeSubmissionStart);
        positionCriteria.addAndCriteria(endDateCriteria);
        BeanQuery positionQuery = new BeanQuery(StaffPosition.class, positionCriteria);
        positionQuery.addOrderBy(StaffPosition.COL_START_DATE, false);
        m_positions = getBroker().getGroupedCollectionByQuery(positionQuery, StaffPosition.COL_STAFF_OID, 1000);

        registerFieldRetrievers();
        initFieldValidators();
    }

    /**
     * Register custom fieldValidators.
     */
    private void initFieldValidators() {
        HashMap validators = new HashMap<String, FieldRetriever>();
        validators.put(VAL_ID_TERMINATION, new ValidateTermination());
        validators.put(ValidateEmptyValue.STF_VAL_EMPTY, new ValidateEmptyValue());
        validators.put(ValidateEmptySecurityValue.STF_VAL_EMPTY_SECURITY, new ValidateEmptySecurityValue());
        validators.put(ValidateAge.VAL_ID, new ValidateAge());
        addValidators(validators);
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldExcludeStaff = translateAliasToJavaName(ALIAS_EXCLUDE_STF, true);
        m_fieldExitDate = translateAliasToJavaName(ALIAS_EXIT_DATE, true);
        m_securityQualification = translateAliasToJavaName(ALIAS_DOE_SEC_QUAL, true);
        m_sfpPrimaryAssignment = translateAliasToJavaName(ALIAS_SFP_PRIMARY_INDICATOR, true);
        m_stfYearStartTeachPath = translateAliasToJavaName(ALIAS_STF_BEGAN_TEACH, false);
        m_stfGenderIndentity = translateAliasToJavaName(ALIAS_STF_GENDER_IDENTITY, true);

        m_activeStatus = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        m_previousDate = (PlainDate) getParameter(PARAM_PREVIOUS_DATE);

        m_securityStaff = (Boolean) getParameter(PARAM_SECURITY_STAFF);
        m_isSnapshot = (Boolean) getParameter(PARAM_IS_SNAPSHOT);
    }

    /**
     * Register custom fieldRetrievers.
     */
    private void registerFieldRetrievers() {
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(CALC_ID_RACECODE, new RetrieveRaceCodeField());
        calcs.put(CALC_ID_JOBCODE, new RetrieverJobCode());
        calcs.put(CALC_ID_HIREDATE, new RetrieveYearsExperience());
        calcs.put(CALC_ID_REPORT_DATE, new RetrieveReportDate());
        calcs.put(CALC_ID_YEARS_TEACH, new RetrieveYearsTeaching());
        calcs.put(RetrieveMailingAddrInfo.STF_CALC_MAILING, new RetrieveMailingAddrInfo());
        calcs.put(RetrieverGenderIdentity.CALC_ID, new RetrieverGenderIdentity());
        super.addCalcs(calcs);

    }
}
