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
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class PAIncidentOffender.
 */
public class PAIncidentOffender extends StateReportData {

    /**
     * Entity class.
     */
    public static class PAIncidentOffenderEntity extends StateReportEntity {

        /**
         * Instantiates a new PA incident offender entity.
         */
        public PAIncidentOffenderEntity() {
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
            ConductIncident incident = (ConductIncident) getBean();
            SisStudent student = incident.getStudent();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] +" +
                    " [Incident No: " + incident.getIncidentId() +
                    ", Offense Code: " + incident.getIncidentCode() + "] ";
            return name;
        }
    }

    /**
     * Retriever to calculate age at time of incident.
     */
    public class RetrieveAgeAtTimeOfIncident implements FieldRetriever {

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
            ConductIncident incident = (ConductIncident) entity.getBean();
            PlainDate dob = incident.getStudent().getPerson().getDob();
            PlainDate incidentDate = incident.getIncidentDate();

            return Integer.valueOf(calculateDifferenceInYears(dob, incidentDate));
        }
    }

    /**
     * Retriever offender type.
     *
     */
    public class RetrieveOffenderType implements FieldRetriever {

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
            ConductIncident incident = (ConductIncident) entity.getBean();
            SisStudent std = null;
            String value = "";

            if ((std = incident.getStudent()) != null) {
                value = "1";
                if (ACTIVE_STATUS.equals(std.getSpedStatusCode())) {
                    value = "2";
                }
            }

            return value;
        }
    }

    /**
     * Retriever offender type.
     *
     */
    public class RetrieveResidence implements FieldRetriever {
        public static final String ALIAS_DISTRICT_RESIDENCE = "DOE DISTRICT RESIDENCE";
        public static final String ALIAS_CND_DISTRICT_RESIDENCE = "all-cnd-OffenderDistrictofResidence";
        public static final String FIELD_SUBMITTING_DISTRICT = "SUBMITTING DISTRICT";

        private String m_fieldCndResidence;
        private String m_fieldStdResidence;

        /**
         * Instantiates a new retrieve residence.
         */
        public RetrieveResidence() {
            super();
            m_fieldCndResidence = translateAliasToJavaName(ALIAS_CND_DISTRICT_RESIDENCE, true);
            m_fieldStdResidence = translateAliasToJavaName(ALIAS_DISTRICT_RESIDENCE, true);
        }

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
            ConductIncident incident = (ConductIncident) entity.getBean();
            String value = (String) incident.getFieldValueByBeanPath(m_fieldCndResidence);
            if (!StringUtils.isEmpty(value)) {
                value = lookupStateValue(ConductIncident.class, m_fieldCndResidence, value);
            }
            if (StringUtils.isEmpty(value)) {
                SisStudent student = incident.getStudent();
                if (student != null) {
                    value = (String) student.getFieldValueByBeanPath(m_fieldStdResidence);
                    if (!StringUtils.isEmpty(value)) {
                        value = lookupStateValue(SisStudent.class, m_fieldStdResidence, value);
                    }
                }
            }
            if (StringUtils.isEmpty(value)) {
                value = entity.getFieldValue(FIELD_SUBMITTING_DISTRICT);
            }
            return value;
        }
    }

    public class RetrieveSendDistrict implements FieldRetriever {

        private static final String CALC_ID = "CNDO_CALC_SEND_DISTR";
        private static final String CURRENT_STATUS_U = "U";

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
            PAIncidentOffender paData = (PAIncidentOffender) data;
            ConductIncident incident = (ConductIncident) entity.getBean();
            SisStudent std = null;
            String value = "";
            if ((std = incident.getStudent()) != null) {
                String currentStatus = (String) std.getFieldValueByBeanPath(paData.m_fieldStdCurrentStatus);
                if (!StringUtils.isEmpty(currentStatus) && CURRENT_STATUS_U.equals(
                        paData.lookupStateValue(SisStudent.class, paData.m_fieldStdCurrentStatus, currentStatus))) {
                    value = (String) std.getFieldValueByBeanPath(paData.m_fieldStdFosterAUN);
                }
            }
            if (StringUtils.isEmpty(value)) {
                String fundingValue = (String) std.getFieldValueByBeanPath(paData.m_fieldStdFunding);
                if (!StringUtils.isEmpty(fundingValue)) {
                    value = paData.lookupStateValue(SisStudent.class, paData.m_fieldStdFunding,
                            fundingValue);
                }
            }
            return value;
        }
    }

    /**
     * Validation for adjudication comment.
     */
    public class ValidateAdjudication implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();

            String arrestedCodeValue = entity.getFieldValue(FIELD_ARRESTED_CODE);

            if (!StringUtils.isEmpty(arrestedCodeValue) && (arrestedCodeValue.equals(ARRESTED_CODE_WEAPONS_VIOLATION)
                    || arrestedCodeValue.equals(ARRESTED_CODE_NOT_FOR_WEAPONS)) && StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value required", "Value is empty"));

            }

            return errors;
        }
    }

    /**
     * Validation for LLE Contacted.
     */
    public class ValidateLLEContactedMethod implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();

            String lleNotifiedValue = entity.getFieldValue(FIELD_LLE_NOTIFIED);

            if (!StringUtils.isEmpty(lleNotifiedValue) && lleNotifiedValue.equals("Y") && StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value required", "Value is empty"));

            }

            return errors;
        }
    }

    /**
     * Validation primary disability field.
     */
    public class ValidateOffenderDistrictResidence implements FieldValidator {

        private static final String VAL_ID = "CNDO_VAL_DIS_RES";

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
            String offenderType = entity.getFieldValue(FIELD_OFFENDER_TYPE);
            if (!StringUtils.isEmpty(offenderType) && Arrays.asList("1", "2", "3").contains(offenderType)
                    && StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Value required when value for field [" + FIELD_OFFENDER_TYPE + "] is \"1\", \"2\" or \"3\".",
                        "Value is empty"));
            }
            return errors;
        }
    }

    /**
     * Validation primary disability field.
     */
    public class ValidatePrimaryDisability implements FieldValidator {

        private static final String VAL_ID = "CNDO_VAL_PR_DIS";

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
            String offenderType = entity.getFieldValue(FIELD_OFFENDER_TYPE);
            if (!StringUtils.isEmpty(offenderType) && "2".equals(offenderType) && StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Value required when value for field [" + FIELD_OFFENDER_TYPE + "] is \"2\".",
                        "Value is empty"));
            }
            return errors;
        }
    }

    /**
     * Validation for required fields.
     */
    public class ValidateToEmptyValue implements FieldValidator {

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
     * Validation for weapon comment.
     */
    public class ValidateWeaponDetectedComment implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();

            String weaponDetectedCode = entity.getFieldValue(FIELD_WEAPON_DETECT_METHOD);

            if (!StringUtils.isEmpty(weaponDetectedCode) && weaponDetectedCode.equals(WEAPON_DETECTED_CODE_OTHER) &&
                    StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value required", "Value is empty"));

            }

            return errors;
        }
    }

    /**
     * Validation for weapon .
     */
    public class ValidateWeaponDetectedMethod implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();

            String arrestedCodeValue = entity.getFieldValue(FIELD_ARRESTED_CODE);

            if (!StringUtils.isEmpty(arrestedCodeValue) && arrestedCodeValue.equals(ARRESTED_CODE_WEAPONS_VIOLATION) &&
                    StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value required", "Value is empty"));

            }

            return errors;
        }
    }

    public static final String ALIAS_ARRESTED = "DOE ARRESTED";
    public static final String ALIAS_STD_CURRENT_STATUS = "DOE CURRENT";
    public static final String ALIAS_STD_FOSTER_AUN = "all-std-FosterAUN";
    public static final String ALIAS_STD_FUNDING = "DOE FUNDING";
    public static final String ALIAS_WEAPONDTECTEDCODE = "DOE WEAPON DETECT";

    public static final String ARRESTED_CODE_NOT_FOR_WEAPONS = "YN";
    public static final String ARRESTED_CODE_WEAPONS_VIOLATION = "YW";

    public static final String CALC_ID_AGEATTIME = "CNDO_CALC_AGEATTIME";
    public static final String CALC_ID_OFFENDER_TYPE = "CNDO_OFFENDER_TYPE";
    public static final String CALC_ID_RESIDENCE = "CNDO_RESIDENCE";

    // Export input parameters
    public static final String PARAM_END_DATE = "endDate";
    public static final String PARAM_SCHOOL_OID = "schoolOid";
    public static final String PARAM_START_DATE = "startDate";

    public static final String VAL_ID_ADJUCTION = "CNDO_VAL_ADJUCTION";
    public static final String VAL_ID_EMPTY = "CNDO_VAL_EMPTY";
    public static final String VAL_ID_LLE_CONTACT = "CNDO_VAL_LLE_CONTACT";
    public static final String VAL_ID_WEAPONDETECTEDCODE = "CNDO_VAL_WEAPON";
    public static final String VAL_ID_WEAPONDETECTEDCOMMENT = "CNDO_VAL_WEAPONCOM";

    public static final String WEAPON_DETECTED_CODE_OTHER = "5";

    private static final String FIELD_ARRESTED_CODE = "ARRESTED CODE";
    private static final String FIELD_LLE_NOTIFIED = "LLE NOTIFIED INDICAT";
    private static final String FIELD_OFFENDER_TYPE = "OFFENDER TYPE";
    private static final String FIELD_WEAPON_DETECT_METHOD = "WEAPON DETECTION MET";

    // Other constants
    private static final String ACTIVE_STATUS = "Active";

    // Class members
    protected String m_fieldArestedCode;
    protected String m_fieldStdCurrentStatus;
    protected String m_fieldStdFosterAUN;
    protected String m_fieldStdFunding;
    protected String m_fieldWeaponDetectCode;

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();
        if (getSetupErrors().size() == 0) {
            Criteria incidentCriteria = getIncidentCriteria();
            applyInputCriteria(incidentCriteria, false, null);
            QueryByCriteria incidentQuery = new QueryByCriteria(ConductIncident.class, incidentCriteria, true);
            setQuery(incidentQuery);
            setEntityClass(PAIncidentOffenderEntity.class);

            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALC_ID_AGEATTIME, new RetrieveAgeAtTimeOfIncident());
            calcs.put(CALC_ID_OFFENDER_TYPE, new RetrieveOffenderType());
            calcs.put(CALC_ID_RESIDENCE, new RetrieveResidence());
            calcs.put(RetrieveSendDistrict.CALC_ID, new RetrieveSendDistrict());
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldValidator>();
            validators.put(VAL_ID_ADJUCTION, new ValidateAdjudication());
            validators.put(VAL_ID_LLE_CONTACT, new ValidateLLEContactedMethod());
            validators.put(VAL_ID_WEAPONDETECTEDCODE, new ValidateWeaponDetectedMethod());
            validators.put(VAL_ID_WEAPONDETECTEDCOMMENT, new ValidateWeaponDetectedMethod());
            validators.put(VAL_ID_EMPTY, new ValidateToEmptyValue());
            validators.put(ValidatePrimaryDisability.VAL_ID, new ValidatePrimaryDisability());
            validators.put(ValidateOffenderDistrictResidence.VAL_ID, new ValidateOffenderDistrictResidence());
            super.addValidators(validators);
        }
    }

    /**
     * calculates difference between dates in years.
     *
     * @param date1 PlainDate
     * @param date2 PlainDate
     * @return count finded differences between dates in years
     */
    static int calculateDifferenceInYears(PlainDate date1, PlainDate date2) {
        Calendar calDate1 = getCalendarFromDate(date1);
        Calendar calDate2 = getCalendarFromDate(date2);
        Calendar clone = (Calendar) calDate1.clone();
        int years = -1;

        while (!clone.after(calDate2)) {
            clone.add(Calendar.YEAR, 1);
            years++;
        }

        return years;
    }

    /**
     * Gets calendar from date.
     *
     * @param t Date
     * @return java.util.Calendar ret
     */
    private static Calendar getCalendarFromDate(java.sql.Date t) {
        java.util.Calendar ret = java.util.Calendar.getInstance();
        ret.setTimeInMillis(t.getTime());

        return ret;
    }

    /**
     * Incident Criteria used to get corresponding actions.
     *
     * @return Criteria incidentCriteria
     */
    private Criteria getIncidentCriteria() {
        X2Criteria incidentCriteria = new X2Criteria();

        incidentCriteria.addEqualTo(ConductIncident.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                SisSchool.COL_ORGANIZATION1_OID, super.getOrganization().getOid());

        incidentCriteria.addIn(ConductIncident.COL_INCIDENT_CODE, getStateReportableCodes());

        if (isSchoolContext()) {
            incidentCriteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            incidentCriteria.addNotEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            incidentCriteria.addNotEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }

        Date startDate = (Date) getParameter(PARAM_START_DATE);
        if (startDate != null) {
            incidentCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, startDate);
        }

        Date endDate = (Date) getParameter(PARAM_END_DATE);
        if (endDate != null) {
            incidentCriteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, endDate);
        }

        return incidentCriteria;

    }

    /**
     * Returns state reportable codes.
     *
     * @return Collection<String> codes
     */
    private Collection<String> getStateReportableCodes() {
        Collection<String> codes = null;
        Collection<String> allowedIncidentCodes = Arrays.asList(
                "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
                "41", "42", "43", "44", "45", "46", "47", "48", "49", "50",
                "51", "52", "53", "54", "A", "C");
        DataDictionaryField field = getDataDictionaryField(ConductIncident.class, ConductIncident.COL_INCIDENT_CODE);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
        criteria.addIn(ReferenceCode.COL_STATE_CODE, allowedIncidentCodes);

        SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
        codes = getBroker().getSubQueryCollectionByQuery(query);

        return codes;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldArestedCode = translateAliasToJavaName(ALIAS_ARRESTED, true);
        m_fieldStdCurrentStatus = translateAliasToJavaName(ALIAS_STD_CURRENT_STATUS, true);
        m_fieldStdFosterAUN = translateAliasToJavaName(ALIAS_STD_FOSTER_AUN, true);
        m_fieldStdFunding = translateAliasToJavaName(ALIAS_STD_FUNDING, true);
        m_fieldWeaponDetectCode = translateAliasToJavaName(ALIAS_WEAPONDTECTEDCODE, true);
    }
}
