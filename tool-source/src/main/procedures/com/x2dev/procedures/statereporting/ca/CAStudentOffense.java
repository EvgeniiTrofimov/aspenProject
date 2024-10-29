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

package com.x2dev.procedures.statereporting.ca;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.framework.persistence.adjusters.DistinctAdjuster;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export Data Module for CA Student Discipline.
 *
 * @author X2 Development Corporation
 */

public class CAStudentOffense extends StateReportData {
    /**
     * Entity class for CA Student Discipline export.
     *
     */
    public static class CAStudentDisciplineEntity extends StateReportEntity {
        /*
         * List of Action codes ordered by priority for incident
         */
        protected static final String[] ARRAY_ACTION_CODE = {"200", "100", "110", "300"};

        /**
         * Entity instance variables.
         */

        CAStudentOffense m_data;
        Map<String, ConductAction> m_incidentActions;
        String m_incidentMostSevereOffense;
        ConductIncident m_incident;
        List<String> m_offenses;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public CAStudentDisciplineEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Returns the Action record for the current index.
         *
         * @return StudentEnrollmentSpan
         */
        public ConductAction getConductAction() {
            return m_incidentActions.get(m_incident.getOid());
        }

        /**
         * Returns the Incident record for the current index.
         *
         * @return StudentEnrollmentSpan
         */
        public ConductIncident getConductIncident() {
            return m_incident;
        }

        /**
         * Returns the Offense record for the current index.
         *
         * @return StudentEnrollmentSpan
         */
        public String getConductOffense() {
            return m_offenses.get(getCurrentRow());
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            StringBuilder buffer = new StringBuilder();
            ConductIncident incident = (ConductIncident) getBean();

            buffer.append(incident.getIncidentId());
            buffer.append("[ DATE=");
            buffer.append(incident.getIncidentDate());
            buffer.append(", CODE=");
            buffer.append(incident.getIncidentCode());
            buffer.append("] ");
            buffer.append(incident.getStudent().getNameView());

            return buffer.toString();
        }

        /**
         * Returns the Most Severe Offense record for the current index.
         *
         * @return StudentEnrollmentSpan
         */
        public String getMostSevereOffense() {
            return m_incidentMostSevereOffense;
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

            m_data = (CAStudentOffense) data;
            m_incident = (ConductIncident) bean;
            m_incidentActions = new HashMap<String, ConductAction>();
            m_offenses = new ArrayList<String>();
            m_offenses = m_data.m_incidentOffensesMap.get(m_incident.getOid());

            initializeAction(m_incident);
            initializeMostSevereOffense(m_incident);

            setRowCount(m_offenses.size());
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

        /**
         * Populates a single Action record for the incident.
         *
         * @param incident ConductIncident
         */
        private void initializeAction(ConductIncident incident) {
            // Retrieve Actions list for incident
            Collection<ConductAction> actions = m_data.getIncidentActions(incident);
            if (actions != null) {
                Map<String, ConductAction> actionsMap = new HashMap<String, ConductAction>();

                // Order Actions by ActionCode
                for (ConductAction action : actions) {
                    String code = (String) action.getFieldValueByBeanPath(m_data.m_fieldDisciplineAction);
                    code = m_data.lookupReferenceCodeByBeanPath(ConductAction.class, m_data.m_fieldDisciplineAction,
                            code,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    actionsMap.put(code, action);
                }

                // Select the first Action with code listed in array of Action
                // codes
                for (int i = 0; i < ARRAY_ACTION_CODE.length; i++) {
                    if (actionsMap.containsKey(ARRAY_ACTION_CODE[i])) {
                        m_incidentActions.put(incident.getOid(), actionsMap.get(ARRAY_ACTION_CODE[i]));
                        break;
                    }
                }
            }
        }

        /**
         * Populates MostSevereOffense value for the incident.
         *
         * @param incident ConductIncident
         */
        private void initializeMostSevereOffense(ConductIncident incident) {
            ArrayList<String> offenses = m_data.getIncidentOffenses(incident);
            m_incidentMostSevereOffense = offenses.get(MOST_SEVERE_OFFENSE_INDEX);
        }

    }

    /**
     * Retrieve data from the student conduct incident record based on field parameter
     *
     * "AUTHORITY_CODE" - Disciplinary Action Authority Code
     * "EXPULSION" - Expulsion Modification Category Code
     * "INCIDENT_ID" - Disciplinary Incident ID Local
     * "INSTR_SUPPORT" - Student Instructional Support Indicator
     * "OCCURANCE_DATE" - Disciplinary Incident Occurrence Date
     * "WEAPON_CODE" - Weapon Category Code.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveIncident implements FieldRetriever {
        private final String PARAM_INCIDENT_ID = "INCIDENT_ID";
        private final String PARAM_WEAPON_CODE = "WEAPON_CODE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String parameter = (String) field.getParameter();
            ConductIncident incident = ((CAStudentDisciplineEntity) entity).getConductIncident();

            if (incident != null) {
                if (PARAM_INCIDENT_ID.equals(parameter)) {
                    value = incident.getIncidentId();
                } else if (PARAM_WEAPON_CODE.equals(parameter)) {
                    String weaponCode = null;
                    if (((CAStudentDisciplineEntity) entity).getCurrentRow() == MOST_SEVERE_OFFENSE_INDEX) {
                        weaponCode = (String) incident.getFieldValueByBeanPath(m_fieldWeaponCode);
                    } else if (((CAStudentDisciplineEntity) entity).getCurrentRow() == SECOND_OFFENSE_INDEX) {
                        weaponCode = (String) incident.getFieldValueByBeanPath(m_field2ndWeaponCode);
                    } else if (((CAStudentDisciplineEntity) entity).getCurrentRow() == THIRD_OFFENSE_INDEX) {
                        weaponCode = (String) incident.getFieldValueByBeanPath(m_field3rdWeaponCode);
                    }

                    if (weaponCode != null) {
                        value = data.lookupReferenceCodeByBeanPath(ConductIncident.class, m_fieldWeaponCode, weaponCode,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                }
            }

            return value;
        }
    }

    /**
     * Retrieve data from the student conduct offense record based on field parameter
     *
     * "MOST_SEVERE" - Incident Most Severe Offense Code
     * "OFFENSE_CODE" - Student Offense Code
     * "OID" - Local Record ID.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveOffense implements FieldRetriever {
        private final String PARAM_OFFENSE_CODE = "OFFENSE_CODE";
        private final String PARAM_OID = "OID";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String parameter = (String) field.getParameter();
            String offenseCode = ((CAStudentDisciplineEntity) entity).getConductOffense();

            if (offenseCode != null) {
                if (PARAM_OFFENSE_CODE.equals(parameter)) {
                    value = data.lookupReferenceCodeByBeanPath(ConductIncident.class, m_fieldOffenseCode, offenseCode,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                } else if (PARAM_OID.equals(parameter)) {
                    String suffix = String.valueOf((((CAStudentDisciplineEntity) entity).getCurrentRow() + 1));
                    value = ((CAStudentDisciplineEntity) entity).getConductIncident().getOid() + "-" + suffix;
                }
            }

            return value;
        }
    }

    /**
     * Validate values of:
     *
     * Weapon Category Code.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateWeapon implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String offenseCode = ((CAStudentDisciplineEntity) entity).getConductOffense();

            if (offenseCode != null && offenseCode.matches(MATCH_OFFENSE_CODE) && value == null) {
                errors.add(new StateReportValidationError(entity, field,
                        "If Student Offense Code = '100', '101', '102', '103', '104', or '105' then this field is required",
                        "Student Offense Code = " + STYLE_BOLD + offenseCode +
                                STYLE_END +
                                " This field value = " + STYLE_BOLD + "NULL" +
                                STYLE_END));
            }

            return errors;
        }
    }

    /*
     * Aliases for fields to look up.
     */
    protected static final String ALIAS_2ND_OFFENSE_CODE = "DOE 2ND OFFENSE";
    protected static final String ALIAS_2ND_WEAPON_CODE = "DOE 2ND WEAPON CODE";
    protected static final String ALIAS_3RD_OFFENSE_CODE = "DOE 3RD OFFENSE";
    protected static final String ALIAS_3RD_WEAPON_CODE = "DOE 3RD WEAPON CODE";
    protected static final String ALIAS_DISCIPLINE_ACTION = "DOE DISCIPLINE ACTION";
    protected static final String ALIAS_EXPULSION_MOD_CODE = "DOE EXPULSION MOD CODE";
    protected static final String ALIAS_OFFENSE_CODE = "DOE OFFENSE CODE";
    protected static final String ALIAS_WEAPON_CODE = "DOE WEAPON CODE";

    protected static final String INCIDENT_EXCLUDED_CODE = "900";
    /*
     * Match constants
     */
    protected static final String MATCH_ACTION_CODE = "300";
    protected static final String MATCH_EDUCATION_PROGRAM_CODE = "144";
    protected static final String MATCH_EXPULSION_CODE = "200";
    protected static final String MATCH_OFFENSE_CODE = "^10[0-5]$";

    protected static final int MOST_SEVERE_OFFENSE_INDEX = 0;
    protected static final int SECOND_OFFENSE_INDEX = 1;
    protected static final int THIRD_OFFENSE_INDEX = 2;
    /*
     * Instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_fieldDisciplineAction;
    protected String m_fieldExpulsionModCode;
    protected String m_fieldOffenseCode;
    protected String m_fieldSecondOffense;
    protected String m_fieldThirdOffense;
    protected String m_fieldWeaponCode;
    protected String m_field2ndWeaponCode;
    protected String m_field3rdWeaponCode;
    protected StudentHistoryHelper m_helper;
    protected Map<String, Collection<ConductAction>> m_incidentActionsMap;
    protected Map<String, ArrayList<String>> m_incidentOffensesMap;
    protected Collection<ConductIncident> m_incidents;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {

        initializeFields();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build helper object.
             */
            m_helper = new StudentHistoryHelper(this);
            PlainDate expEndDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));

            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            if (getCurrentContext().getEndDate().before(expEndDate)) {
                expEndDate = getCurrentContext().getEndDate();
            }

            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, expEndDate);

            // get student criteria
            X2Criteria studentCriteria = m_helper.getStudentCriteria();

            // Create incident selection criteria
            X2Criteria incidentCriteria = new X2Criteria();
            incidentCriteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, expEndDate);
            incidentCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE,
                    getCurrentContext().getStartDate());
            List<String> codes = getMostSevereOffenseCodes();
            incidentCriteria.addIn(ConductIncident.COL_INCIDENT_CODE, codes);
            incidentCriteria.addNotEqualTo(ConductIncident.COL_INCIDENT_CODE, INCIDENT_EXCLUDED_CODE);

            SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

            incidentCriteria.addIn(ConductIncident.COL_STUDENT_OID, studentSubQuery);

            BeanQuery incidentQuery = new BeanQuery(ConductIncident.class, incidentCriteria);
            incidentQuery.setDistinct(true);
            DataDictionaryTable table = getDataDictionary().findDataDictionaryTableByClass(getBeanClass().getName());
            DistinctAdjuster adjuster =
                    new DistinctAdjuster(table.getPrimaryKeyColumn(), getBroker().getPersistenceKey());
            incidentQuery.addQueryAdjuster(adjuster);
            applyInputSort(incidentQuery, ConductIncident.REL_STUDENT);
            incidentQuery.addOrderBy(ConductIncident.COL_STUDENT_OID, true);

            setQuery(incidentQuery);

            m_incidents = getBroker().getCollectionByQuery(incidentQuery);

            // Initialize offenses
            m_incidentOffensesMap = new HashMap<String, ArrayList<String>>();

            for (ConductIncident incident : m_incidents) {
                if (incident != null) {
                    String severeOffense = incident.getIncidentCode();
                    String secondOffense = (String) incident.getFieldValueByBeanPath(m_fieldSecondOffense);
                    String thirdOffense = (String) incident.getFieldValueByBeanPath(m_fieldThirdOffense);

                    ArrayList<String> incidentOffenses = m_incidentOffensesMap.get(incident.getOid());
                    if (incidentOffenses == null) {
                        incidentOffenses = new ArrayList<String>();
                        m_incidentOffensesMap.put(incident.getOid(), incidentOffenses);
                    }
                    // Add severe offense
                    incidentOffenses.add(severeOffense);
                    // Add second offense
                    if (secondOffense != null) {
                        incidentOffenses.add(secondOffense);
                    }
                    // Add third offense
                    if (thirdOffense != null) {
                        incidentOffenses.add(thirdOffense);
                    }
                }
            }

            SubQuery incidentSubQuery = new SubQuery(ConductIncident.class, X2BaseBean.COL_OID, incidentCriteria);

            // Create action selection criteria
            X2Criteria actionMapCriteria = new X2Criteria();
            actionMapCriteria.addIn(ConductAction.COL_INCIDENT_OID, incidentSubQuery);

            QueryByCriteria actionQuery = new QueryByCriteria(ConductAction.class, actionMapCriteria);
            actionQuery.addOrderBy(ConductAction.COL_INCIDENT_OID, true);
            m_incidentActionsMap =
                    getBroker().getGroupedCollectionByQuery(actionQuery, ConductAction.COL_INCIDENT_OID, 5000);

            setEntityClass(CAStudentDisciplineEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("CND-INCIDENT", new RetrieveIncident());
            calcs.put("CND-OFFENSE", new RetrieveOffense());
            super.addCalcs(calcs);

            // Build a map of validators
            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put("CND-WEAPON-VAL", new ValidateWeapon());
            super.addValidators(validators);
        }
    }

    /**
     * Returns the ConductAction records for the incident.
     *
     * @param incident ConductIncident
     * @return Collection containing the records for the student
     */
    protected Collection<ConductAction> getIncidentActions(ConductIncident incident) {
        return m_incidentActionsMap.get(incident.getOid());
    }

    /**
     * Returns the ConductOffense records for the incident.
     *
     * @param incident ConductIncident
     * @return Collection containing the records for the student
     */
    protected ArrayList<String> getIncidentOffenses(ConductIncident incident) {
        return m_incidentOffensesMap.get(incident.getOid());
    }

    /**
     * Gets the most severe offense codes.
     *
     * @return a list of conduct incident codes with state reference codes
     */
    protected List<String> getMostSevereOffenseCodes() {
        DataDictionaryField dictionaryField = getDataDictionaryField(ConductIncident.class,
                ConductIncident.COL_INCIDENT_CODE);
        String referenceTableOid = dictionaryField.getReferenceTableOid();
        ReferenceTable refTable = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, referenceTableOid);
        List<String> codeList = new ArrayList<String>();
        if (refTable != null) {
            Collection<ReferenceCode> codes = refTable.getReferenceCodes(getBroker());
            for (ReferenceCode code : codes) {
                if (!StringUtils.isEmpty(code.getStateCode())) {
                    codeList.add(code.getCode());
                }
            }
        }
        return codeList;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldDisciplineAction = translateAliasToJavaName(ALIAS_DISCIPLINE_ACTION, true);
        m_fieldExpulsionModCode = translateAliasToJavaName(ALIAS_EXPULSION_MOD_CODE, true);
        m_fieldOffenseCode = translateAliasToJavaName(ALIAS_OFFENSE_CODE, true);
        m_fieldSecondOffense = translateAliasToJavaName(ALIAS_2ND_OFFENSE_CODE, true);
        m_fieldThirdOffense = translateAliasToJavaName(ALIAS_3RD_OFFENSE_CODE, true);
        m_fieldWeaponCode = translateAliasToJavaName(ALIAS_WEAPON_CODE, true);
        m_field2ndWeaponCode = translateAliasToJavaName(ALIAS_2ND_WEAPON_CODE, true);
        m_field3rdWeaponCode = translateAliasToJavaName(ALIAS_3RD_WEAPON_CODE, true);
    }
}
