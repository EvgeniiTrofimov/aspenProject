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
package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentConductDataset;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class FLSchoolEnvSafetyIncidentData.
 */
public class FLSchoolEnvSafetyIncidentData extends FLStateReportData {

    /**
     * The Class FLSchoolEnvSafetyIncidentEntity.
     */
    public static class FLSchoolEnvSafetyIncidentEntity extends FLStateReportEntity {
        List<List<ConductIncident>> m_incidentList;
        private FLSchoolEnvSafetyIncidentData m_data;

        /**
         * Instantiates a new FL school env safety incident entity.
         */
        public FLSchoolEnvSafetyIncidentEntity() {
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
            ConductIncident incident = getIncidents().iterator().next();

            SisStudent student = incident.getStudent();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    ", INCID: " + incident.getIncidentId() +
                    "] ";
            return name;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_data = (FLSchoolEnvSafetyIncidentData) data;
            m_incidentList = new ArrayList(m_data.m_conductDataset.getConductIncidentsByIdMap().values());

            setRowCount(m_data.m_conductDataset.getConductIncidentsByIdMap().size());
        }

        /**
         * Gets the incidents.
         *
         * @return List
         */
        List<ConductIncident> getIncidents() {
            return m_incidentList.get(getCurrentRow());
        }
    }

    /**
     * Field retriever for School Environmental Safety Incident Report fields.
     */
    protected class RetrieveSchoolEnvSafIncidentRepFields implements FieldRetriever {

        public static final String CALC_ID = "CALC_SESIR";

        private static final String ALIAS_ALCOHOL_RELATED = "all-cnd-AlcoholRelated";
        private static final String ALIAS_BULLYING_RELATED = "all-cnd-BullyingRelated";
        private static final String ALIAS_CONTEXT = "all-cnd-Context";
        private static final String ALIAS_DRUG_RELATED = "all-cnd-DrugRelated";
        private static final String ALIAS_DRUG_TYPE = "all-cnd-DrugType";
        private static final String ALIAS_GANG_RELATED = "all-cnd-GangRelated";
        private static final String ALIAS_HAR_DISABILITY = "all-cnd-HarassmentBasedOnDisability";
        private static final String ALIAS_HAR_RACE = "all-cnd-HarassmentBasedOnRace";
        private static final String ALIAS_HAR_RELIGION = "all-cnd-HarassmentBasedOnReligion";
        private static final String ALIAS_HAR_SEX = "all-cnd-HarassmentBasedOnSex";
        private static final String ALIAS_HAR_SEX_ORIENTATION = "all-cnd-HarassmentBasedOnSexualOrientation";
        private static final String ALIAS_HATE_CRIME = "all-cnd-HateCrimeRelated";
        private static final String ALIAS_HAZING_RELATED = "all-cnd-HazingRelated";
        private static final String ALIAS_HOMICIDE_VICTIMS = "all-cnd-HomicideVictims";
        private static final String ALIAS_INCIDENT_TYPE = "all-cnd-IncidentType";
        private static final String ALIAS_INJURY_RELATED = "all-cnd-InjuryRelated";
        private static final String ALIAS_LAW_ENFORSMENT = "all-cnd-ReportedToLawEnforcement";
        private static final String ALIAS_OFFENDER_TYPE = "all-cnd-OffenderType";
        private static final String ALIAS_SKL_ID = "all-skl-StateId";
        private static final String ALIAS_WEAPON_DISCHARGED = "all-cnd-WeaponDischarged";
        private static final String ALIAS_WEAPON_RELATED = "all-cnd-WeaponRelated";
        private static final String ALIAS_WEAPON_TYPE = "all-cnd-WeaponType";

        private static final String BASIS_SEX_VALUE_HAR = "HAR";
        private static final String BASIS_SEX_VALUE_SXH = "SXH";

        private static final String PARAM_INCIDENT_DATE = "INCIDENT_DATE";
        private static final String PARAM_INCIDENT_ID = "INCIDENT_ID";
        private static final String PARAM_INCIDENT_LOCATION = "INCIDENT_LOCATION";

        private static final String UBL_UHR_VALUE = "Z";
        private static final String VALUE_Y = "Y";
        private static final String VALUE_N = "N";

        private DataDictionaryField m_fieldAlcoholRelated;
        private DataDictionaryField m_fieldBullyingRelated;
        private DataDictionaryField m_fieldContext;
        private DataDictionaryField m_fieldDrugRelated;
        private DataDictionaryField m_fieldDrugType;
        private DataDictionaryField m_fieldGangRelated;
        private DataDictionaryField m_fieldHarassmentBasedOnDisability;
        private DataDictionaryField m_fieldHarassmentBasedOnRace;
        private DataDictionaryField m_fieldHarassmentBasedOnReligion;
        private DataDictionaryField m_fieldHarassmentBasedOnSex;
        private DataDictionaryField m_fieldHarassmentBasedOnSexualOrientation;
        private DataDictionaryField m_fieldHateCrimeRelated;
        private DataDictionaryField m_fieldHazingRelated;
        private DataDictionaryField m_fieldHomicideVictims;
        private DataDictionaryField m_fieldIncidentType;
        private DataDictionaryField m_fieldInjuryRelated;
        private DataDictionaryField m_fieldOffenderType;
        private DataDictionaryField m_fieldReportedToLawEnforcement;
        private DataDictionaryField m_fieldSklId;
        private DataDictionaryField m_fieldWeaponDischarged;
        private DataDictionaryField m_fieldWeaponRelated;
        private DataDictionaryField m_fieldWeaponType;

        /**
         * Instantiates a new retrieve school env saf incident rep fields.
         *
         * @throws X2BaseException exception
         */
        public RetrieveSchoolEnvSafIncidentRepFields() throws X2BaseException {

            m_fieldIncidentType = translateAliasToDictionaryField(ALIAS_INCIDENT_TYPE, true);
            m_fieldOffenderType = translateAliasToDictionaryField(ALIAS_OFFENDER_TYPE, true);
            m_fieldReportedToLawEnforcement = translateAliasToDictionaryField(ALIAS_LAW_ENFORSMENT, true);
            m_fieldGangRelated = translateAliasToDictionaryField(ALIAS_GANG_RELATED, true);
            m_fieldAlcoholRelated = translateAliasToDictionaryField(ALIAS_ALCOHOL_RELATED, true);
            m_fieldDrugRelated = translateAliasToDictionaryField(ALIAS_DRUG_RELATED, true);
            m_fieldHateCrimeRelated = translateAliasToDictionaryField(ALIAS_HATE_CRIME, true);
            m_fieldWeaponRelated = translateAliasToDictionaryField(ALIAS_WEAPON_RELATED, true);
            m_fieldWeaponType = translateAliasToDictionaryField(ALIAS_WEAPON_TYPE, true);
            m_fieldContext = translateAliasToDictionaryField(ALIAS_CONTEXT, true);
            m_fieldDrugType = translateAliasToDictionaryField(ALIAS_DRUG_TYPE, true);
            m_fieldBullyingRelated = translateAliasToDictionaryField(ALIAS_BULLYING_RELATED, true);
            m_fieldInjuryRelated = translateAliasToDictionaryField(ALIAS_INJURY_RELATED, true);
            m_fieldHomicideVictims = translateAliasToDictionaryField(ALIAS_HOMICIDE_VICTIMS, true);
            m_fieldSklId = translateAliasToDictionaryField(ALIAS_SKL_ID, true);
            m_fieldWeaponDischarged = translateAliasToDictionaryField(ALIAS_WEAPON_DISCHARGED, true);
            m_fieldHarassmentBasedOnDisability = translateAliasToDictionaryField(ALIAS_HAR_DISABILITY, true);
            m_fieldHarassmentBasedOnRace = translateAliasToDictionaryField(ALIAS_HAR_RACE, true);
            m_fieldHarassmentBasedOnSex = translateAliasToDictionaryField(ALIAS_HAR_SEX, true);
            m_fieldHarassmentBasedOnReligion = translateAliasToDictionaryField(ALIAS_HAR_RELIGION, true);
            m_fieldHarassmentBasedOnSexualOrientation =
                    translateAliasToDictionaryField(ALIAS_HAR_SEX_ORIENTATION, true);
            m_fieldHazingRelated = translateAliasToDictionaryField(ALIAS_HAZING_RELATED, true);

        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            FLSchoolEnvSafetyIncidentEntity flEntity = (FLSchoolEnvSafetyIncidentEntity) entity;
            FLSchoolEnvSafetyIncidentData flData = (FLSchoolEnvSafetyIncidentData) data;

            List<ConductIncident> currIncidents = flEntity.getIncidents();

            String parameter = (String) field.getParameter();
            Object value = null;



            if (currIncidents != null) {
                ConductIncident currInc = currIncidents.get(0);
                String incidentType = currInc.getIncidentCode();
                Boolean incidentBulRel = (Boolean) flData.getFieldValue(currInc, m_fieldBullyingRelated);

                switch (parameter) {
                    case ALIAS_SKL_ID:
                        value = flData.getFieldValue(currInc.getSchool(), m_fieldSklId);
                        break;
                    case ALIAS_INCIDENT_TYPE:
                        value = flData.getFieldValue(currInc, m_fieldIncidentType);
                        break;
                    case ALIAS_OFFENDER_TYPE:
                        value = flData.getFieldValue(currInc, m_fieldOffenderType);
                        if (value != null) {
                            if (UHR_UBS_VALID_CODES.contains(incidentType)) {
                                value = UBL_UHR_VALUE;
                                return value;
                            }
                        }
                        break;
                    case ALIAS_LAW_ENFORSMENT:
                        value = flData.getFieldValue(currInc, m_fieldReportedToLawEnforcement);
                        break;
                    case ALIAS_GANG_RELATED:
                        value = flData.getFieldValue(currInc, m_fieldGangRelated);
                        if (value != null) {
                            if (UHR_UBS_VALID_CODES.contains(incidentType)) {
                                value = UBL_UHR_VALUE;
                                return value;
                            }
                        }
                        break;
                    case ALIAS_ALCOHOL_RELATED:
                        value = flData.getFieldValue(currInc, m_fieldAlcoholRelated);
                        if (value != null) {
                            if (UHR_UBS_VALID_CODES.contains(incidentType)) {
                                value = UBL_UHR_VALUE;
                                return value;
                            }
                        }
                        break;
                    case ALIAS_DRUG_RELATED:
                        value = flData.getFieldValue(currInc, m_fieldDrugRelated);
                        if (value != null) {
                            if (UHR_UBS_VALID_CODES.contains(incidentType)) {
                                value = UBL_UHR_VALUE;
                                return value;
                            }
                        }
                        break;
                    case ALIAS_HATE_CRIME:
                        value = flData.getFieldValue(currInc, m_fieldHateCrimeRelated);
                        if (value != null) {
                            if (UHR_UBS_VALID_CODES.contains(incidentType)) {
                                value = UBL_UHR_VALUE;
                                return value;
                            }
                        }
                        break;
                    case ALIAS_WEAPON_RELATED:
                        value = flData.getFieldValue(currInc, m_fieldWeaponRelated);
                        if (value != null) {
                            if (UHR_UBS_VALID_CODES.contains(incidentType)) {
                                value = UBL_UHR_VALUE;
                                return value;
                            }
                        }
                        break;
                    case ALIAS_WEAPON_TYPE:
                        value = flData.getFieldValue(currInc, m_fieldWeaponType);
                        if (value != null) {
                            if (UHR_UBS_VALID_CODES.contains(incidentType)) {
                                value = UBL_UHR_VALUE;
                                return value;
                            }
                        }
                        break;
                    case ALIAS_CONTEXT:
                        value = flData.getFieldValue(currInc, m_fieldContext);
                        break;
                    case ALIAS_DRUG_TYPE:
                        value = flData.getFieldValue(currInc, m_fieldDrugType);
                        if (value != null) {
                            if (UHR_UBS_VALID_CODES.contains(incidentType)) {
                                value = UBL_UHR_VALUE;
                                return value;
                            }
                        }
                        break;
                    case ALIAS_BULLYING_RELATED:
                        value = flData.getFieldValue(currInc, m_fieldBullyingRelated);
                        if (value != null) {
                            if (UHR_UBS_VALID_CODES.contains(incidentType)) {
                                value = UBL_UHR_VALUE;
                                return value;
                            }
                        }
                        break;
                    case ALIAS_INJURY_RELATED:
                        value = flData.getFieldValue(currInc, m_fieldInjuryRelated);
                        if (value != null) {
                            if (UHR_UBS_VALID_CODES.contains(incidentType)) {
                                value = UBL_UHR_VALUE;
                                return value;
                            }
                        }
                        break;
                    case ALIAS_HOMICIDE_VICTIMS:
                        value = flData.getFieldValue(currInc, m_fieldHomicideVictims);
                        break;
                    case ALIAS_WEAPON_DISCHARGED:
                        value = flData.getFieldValue(currInc, m_fieldWeaponDischarged);
                        break;
                    case ALIAS_HAR_DISABILITY:
                        value = flData.getFieldValue(currInc, m_fieldHarassmentBasedOnDisability);
                        if (value != null) {
                            if (INCIDENT_BASIS_VALID_CODES.contains(incidentType)
                                    && incidentBulRel.booleanValue()) {
                                return value;
                            }
                            value = UBL_UHR_VALUE;
                        }
                        break;
                    case ALIAS_HAR_RACE:
                        value = flData.getFieldValue(currInc, m_fieldHarassmentBasedOnRace);
                        if (value != null) {
                            if (INCIDENT_BASIS_VALID_CODES.contains(incidentType)
                                    && incidentBulRel.booleanValue()) {
                                return value;
                            }
                            value = UBL_UHR_VALUE;
                        }
                        break;
                    case ALIAS_HAR_SEX:
                        value = flData.getFieldValue(currInc, m_fieldHarassmentBasedOnSex);
                        if (value != null) {
                            if (INCIDENT_BASIS_SEX_VALID_CODES.contains(incidentType)
                                    && incidentBulRel.booleanValue()) {
                                return value;
                            }
                            if (incidentType.contentEquals(BASIS_SEX_VALUE_HAR)
                                    && value.toString().contentEquals(VALUE_Y)) {
                                value = VALUE_N;
                                return value;
                            }
                            if (incidentType.contentEquals(BASIS_SEX_VALUE_SXH)
                                    && value.toString().contentEquals(VALUE_Y)) {
                                value = VALUE_Y;
                                return value;
                            }
                            value = UBL_UHR_VALUE;
                        }
                        break;
                    case ALIAS_HAR_RELIGION:
                        value = flData.getFieldValue(currInc, m_fieldHarassmentBasedOnReligion);
                        if (value != null) {
                            if (INCIDENT_BASIS_VALID_CODES.contains(incidentType)
                                    && incidentBulRel.booleanValue()) {
                                return value;
                            }
                            value = UBL_UHR_VALUE;
                        }
                        break;
                    case ALIAS_HAR_SEX_ORIENTATION:
                        value = flData.getFieldValue(currInc,
                                m_fieldHarassmentBasedOnSexualOrientation);
                        break;
                    case ALIAS_HAZING_RELATED:
                        value = flData.getFieldValue(currInc, m_fieldHazingRelated);
                        if (UHR_UBS_VALID_CODES.contains(incidentType)) {
                            value = UBL_UHR_VALUE;
                            return value;
                        }
                        break;
                    case PARAM_INCIDENT_ID:
                        value = currInc.getIncidentId();
                        break;
                    case PARAM_INCIDENT_DATE:
                        value = currInc.getIncidentDate();
                        break;
                    case PARAM_INCIDENT_LOCATION:
                        value = currInc.getIncidentLocation();
                        break;
                }
            }

            return value;
        }
    }

    /**
     * Map incidentID to List&lt;ConductIncident&gt;
     */

    // Export input parameters
    static final String PARAM_END_DATE = "endDate";
    static final String PARAM_START_DATE = "startDate";

    /**
     * Instance variables.
     */
    protected static final List<String> INCIDENT_BASIS_SEX_VALID_CODES = Arrays.asList("BUL",
            "TRE", "UBL", "UHR");

    protected static final List<String> INCIDENT_BASIS_VALID_CODES = Arrays.asList("BUL",
            "HAR", "SXH", "TRE", "UBL", "UHR");

    protected static final List<String> SESIR_SURVEY_PERIOD_VALID_CODES =
            Arrays.asList(FLStateReportData.SURVEY_PERIOD_2,
                    FLStateReportData.SURVEY_PERIOD_3, FLStateReportData.SURVEY_PERIOD_5);

    protected static final List<String> UHR_UBS_VALID_CODES = Arrays.asList("UBL", "UHR");

    protected StudentConductDataset m_conductDataset;

    /**
     * Initialize the data module. Initialize necessary working resources. Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        Collection<StateReportValidationError> errors = getSetupErrors();
        if (errors.size() != 0) {
            return;
        }

        m_conductDataset = getStudentHelper().getStudentConductDataset(getCurrentContext().getStartDate(),
                getSurveyPeriod().getEndDate());

        Criteria orgCriteria = new Criteria();
        orgCriteria.addEqualTo(X2BaseBean.COL_OID, getOrganization().getOid());
        QueryByCriteria OrganizationQuery = new QueryByCriteria(Organization.class, orgCriteria, true);
        setQuery(OrganizationQuery);

        setEntityClass(FLSchoolEnvSafetyIncidentEntity.class);

        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#getValidSurveyPeriods()
     */
    @Override
    public Collection<String> getValidSurveyPeriods() {
        return SESIR_SURVEY_PERIOD_VALID_CODES;
    }

    /**
     * Register custom field Retrievers.
     *
     * @throws X2BaseException exception
     */
    private void registerFieldRetrievers() throws X2BaseException {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveFiscalYear.CALC_ID, new RetrieveFiscalYear());
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveSchoolEnvSafIncidentRepFields.CALC_ID, new RetrieveSchoolEnvSafIncidentRepFields());
        super.addCalcs(calcs);

    }

    /**
     * method for initialization of field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<>();
        super.addValidators(validators);
    }

}
