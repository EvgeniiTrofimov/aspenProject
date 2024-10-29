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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.*;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * <b>Incident Offender Disciplinary Action</b> export data module<br>
 * and
 * <b>Incident Offender Parent Involvement</b> export data module.
 */
public class PAIncidentAction extends StateReportData {

    /**
     * Entity class.
     */
    public static class PAIncidentActionEntity extends StateReportEntity {

        List<String> m_involvementCodes;

        /**
         * Instantiates a new PA incident action entity.
         */
        public PAIncidentActionEntity() {}

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            ConductAction action = (ConductAction) getBean();
            ConductIncident incident = action.getIncident();
            SisStudent student = incident.getStudent();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] " +
                    "[ID: " + incident.getIncidentId() + ", CODE: " + incident.getIncidentCode() + "]";

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
            int rowCount = 1;
            ConductAction action = (ConductAction) bean;
            PAIncidentAction actpData = (PAIncidentAction) data;
            ConductIncident incident = action.getIncident();
            if (actpData.m_isParentalExport) {
                m_involvementCodes = new ArrayList(10);
                if (!actpData.m_setProcessedIncidents.contains(incident.getOid())) {
                    actpData.m_setProcessedIncidents.add(incident.getOid());
                    String involvement = (String) incident.getFieldValueByAlias(ALIAS_PARENTAL_INVOLVEMENT_INCIDENT);
                    if (!StringUtils.isEmpty(involvement)) {
                        for (String code : Arrays.asList(involvement.split(","))) {
                            ReferenceCode reference = actpData.m_incidentCodes.get(code.trim());
                            if (reference != null) {
                                m_involvementCodes.add(reference.getStateCode());
                            }
                        }
                    }
                }

                String involvement = (String) action.getFieldValueByAlias(ALIAS_PARENTAL_INVOLVEMENT_ACTION);
                if (!StringUtils.isEmpty(involvement)) {
                    for (String code : Arrays.asList(involvement.split(","))) {
                        ReferenceCode reference = actpData.m_actionCodes.get(code.trim());
                        if (reference != null) {
                            m_involvementCodes.add(reference.getStateCode());
                        }
                    }
                }

                Collections.sort(m_involvementCodes);
                rowCount = m_involvementCodes.size();
            }

            setRowCount(rowCount);
        }

        /**
         * Gets the current involvement.
         *
         * @return String
         */
        public String getCurrentInvolvement() {
            return m_involvementCodes.get(getCurrentRow());
        }
    }


    /**
     * Retrieve classes as an example.
     */
    protected class RetrieveParentalInvolvement implements FieldRetriever {
        protected static final String CALC_ID = "PARENT_INVOLVEMENT";

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
            return ((PAIncidentActionEntity) entity).getCurrentInvolvement();
        }
    }


    /**
     * Required if DISCIPLINARY ACTION CODE is S10, R9,
     * or if incident involved a firearm and student was not expelled.
     */
    class ValidateActionComment implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();

            ConductAction action = (ConductAction) entity.getBean();

            String code = action.getActionCode();

            if (StringUtils.isEmpty(value)
                    && ("S10".equals(code) || "R9".equals(code)
                    /*
                     * ||(action.getActionPenaltyTime().floatValue()==0 &&
                     * action.getIncident().[firearm involved])
                     */
                    )) {
                errors.add(new StateReportValidationError(entity, field, "Field must be populated", ""));
            }

            return errors;
        }
    }

    /**
     * Validator for ACTION COMMENT<br>
     * <i>Required if the PARENTAL INVOLVEMENT CODE is 6 - other code</i>.
     */
    class ValidateActionComment_Parental implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();
            Object code = entity.getFieldValue(FIELD_PARENTAL_INVOLVEMENT);

            int intCode = Integer.MIN_VALUE;
            if (code != null) {
                try {
                    intCode = Integer.parseInt(code.toString());
                } catch (NumberFormatException e) {
                    intCode = Integer.MIN_VALUE;
                }
            }
            if (intCode == PARENT_INVOLVMENT_CODE_OTHER && StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Field must be populated", ""));
            }
            return errors;
        }

    }

    /**
     * Required if DISCIPLINARY ACTION CODE is S3 � S7.
     */
    class ValidateActionDuration implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();
            String code = entity.getFieldValue(EXPORT_FIELD_ACTION_CODE);
            if (StringUtils.isEmpty(value) && Arrays.asList("S3", "S4", "S5", "S6", "S7", "S8", "S9").contains(code)) {
                errors.add(new StateReportValidationError(entity, field, "Required",
                        "Required when value for field [" + EXPORT_FIELD_ACTION_CODE + "] is S3-S9"));
            }
            return errors;
        }
    }

    /**
     * Required if DISCIPLINARY ACTION CODE is S5, S6, or S7.
     */
    class ValidateReceivedServices implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();
            ConductAction action = (ConductAction) entity.getBean();

            String code = action.getActionCode();

            if (StringUtils.isEmpty(value)
                    && ("S5".equals(code) || "S6".equals(code) || "S7".equals(code))) {
                errors.add(new StateReportValidationError(entity, field, "Field must be populated", ""));
            }
            return errors;
        }
    }

    // Aliases
    private static final String ALIAS_PARENTAL_INVOLVEMENT_INCIDENT = "DOE CND PARENT";
    private static final String ALIAS_PARENTAL_INVOLVEMENT_ACTION = "DOE PARENT INVOLVEMENT";

    // Fields
    private static final String FIELD_PARENTAL_INVOLVEMENT = "PARENTAL INVOLVEMENT";

    // Export input parameters
    private static final String PARAM_END_DATE = "endDate";
    private static final String PARAM_START_DATE = "startDate";

    // Validator IDs
    private static final String VAL_ID_ACT_COMMENT = "ACT_VAL_COMMENT";
    private static final String VAL_ID_ACTION_DURATION = "ACTION DURATION";
    private static final String VAL_ID_ACTP_COMMENT = "ACTP_VAL_COMMENT";
    private static final String VAL_ID_RECEIVED_SERVICES = "RECEIVED SERVICES";

    /**
     * Other constants
     */
    private static final int PARENT_INVOLVMENT_CODE_OTHER = 6;

    /**
     * Export Fields
     */
    private static final String EXPORT_FIELD_ACTION_CODE = "ACTION CODE";

    protected Map<String, ReferenceCode> m_incidentCodes;
    protected Map<String, ReferenceCode> m_actionCodes;
    protected boolean m_isParentalExport = false;
    protected Set<String> m_setProcessedIncidents = new HashSet();

    private String m_fieldParentalInvolvement;

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        if (getParameter("isParental") != null && !StringUtils.isEmpty((String) getParameter("isParental"))) {
            m_isParentalExport = true;
        }

        initializeFields();

        if (getSetupErrors().size() == 0) {
            Criteria actionCriteria = getActionsCriteria();
            QueryByCriteria actionQuery = new QueryByCriteria(ConductAction.class, actionCriteria, true);
            actionQuery.addOrderByAscending(ConductAction.REL_INCIDENT);
            setQuery(actionQuery);
            setEntityClass(PAIncidentActionEntity.class);
        }
    }

    /**
     * Action criteria to select included actions.
     *
     * @return Criteria actionCriteria
     */
    private Criteria getActionsCriteria() {
        X2Criteria incidentCriteria = getIncidentCriteria();
        applyInputCriteria(incidentCriteria, false, null);
        incidentCriteria.addNotNull(ConductIncident.COL_STUDENT_OID);

        SubQuery incidentQuery = new SubQuery(ConductIncident.class, X2BaseBean.COL_OID, incidentCriteria);
        Collection c = getBroker().getSubQueryCollectionByQuery(incidentQuery);

        ArrayList<String> incidentOids = new ArrayList<String>();
        incidentOids.addAll(c);
        if (incidentOids.size() == 0) {
            incidentOids.add("##InvalidOid##");
        }

        X2Criteria actionCriteria = new X2Criteria();
        actionCriteria.addIn(ConductAction.COL_INCIDENT_OID, incidentOids);
        actionCriteria.addIn(ConductAction.COL_ACTION_CODE,
                getStateReportableCodes(ConductAction.class, ConductAction.COL_ACTION_CODE));
        // filtering of empty parental involvement codes
        if (m_isParentalExport) {
            actionCriteria.addNotEmpty(ConductAction.REL_INCIDENT + PATH_DELIMITER + m_fieldParentalInvolvement,
                    getBroker().getPersistenceKey());
        }

        return actionCriteria;
    }

    /**
     * Incident Criteria used to get corresponding actions.
     *
     * @return X2Criteria incidentCriteria
     */
    private X2Criteria getIncidentCriteria() {
        X2Criteria incidentCriteria = new X2Criteria();

        incidentCriteria.addEqualTo(ConductIncident.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                SisSchool.COL_ORGANIZATION1_OID, super.getOrganization().getOid());
        incidentCriteria.addIn(ConductIncident.COL_INCIDENT_CODE,
                getStateReportableCodes(ConductIncident.class, ConductIncident.COL_INCIDENT_CODE));

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
     * Get the list of reportable state codes.
     *
     * @param beanClass Class
     * @param path String
     * @return Set<String> codes
     */
    private Collection<String> getStateReportableCodes(Class beanClass, String path) {
        Collection<String> codes = null;
        Collection<String> allowedIncidentCodes = Arrays.asList(
                "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
                "41", "42", "43", "44", "45", "46", "47", "48", "49", "50",
                "51", "52", "53", "54", "A", "C");
        Collection<String> allowedActionCodes = Arrays.asList(
                "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10",
                "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10");

        DataDictionaryField field = getDataDictionaryField(beanClass, path);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());

        if (beanClass.equals(ConductIncident.class)) {
            criteria.addIn(ReferenceCode.COL_STATE_CODE, allowedIncidentCodes);
        }
        if (beanClass.equals(ConductAction.class)) {
            criteria.addIn(ReferenceCode.COL_STATE_CODE, allowedActionCodes);
        }
        SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
        codes = getBroker().getSubQueryCollectionByQuery(query);

        return codes;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        DataDictionaryField dictionaryField =
                getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_PARENTAL_INVOLVEMENT_INCIDENT);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            m_incidentCodes = getReferenceCodes(dictionaryField.getReferenceTableOid());
        }
        dictionaryField =
                getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_PARENTAL_INVOLVEMENT_ACTION);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            m_actionCodes = getReferenceCodes(dictionaryField.getReferenceTableOid());
        }

        m_fieldParentalInvolvement = translateAliasToJavaName(ALIAS_PARENTAL_INVOLVEMENT_INCIDENT, true);

        // Add validators
        HashMap validators = new HashMap<String, FieldValidator>();

        translateAliasToJavaName(ALIAS_PARENTAL_INVOLVEMENT_INCIDENT, true);

        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveParentalInvolvement.CALC_ID, new RetrieveParentalInvolvement());
        super.addCalcs(calcs);

        validators.put(VAL_ID_ACTION_DURATION, new ValidateActionDuration());
        validators.put(VAL_ID_RECEIVED_SERVICES, new ValidateReceivedServices());
        validators.put(VAL_ID_ACT_COMMENT, new ValidateActionComment());
        validators.put(VAL_ID_ACTP_COMMENT, new ValidateActionComment_Parental());

        addValidators(validators);
    }
}
