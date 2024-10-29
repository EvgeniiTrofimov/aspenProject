/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2013 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.pa;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Person;
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
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export Data Module for PIMS Incident Victim.
 */
public class PAIncidentVictim extends StateReportData {

    /**
     * Entity class .
     */
    public static class PAIncidentVictimEntity extends StateReportEntity {

        /**
         * Instantiates a new PA incident victim entity.
         */
        public PAIncidentVictimEntity() {

        }

        /**
         * Gets the entity name.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            ConductIncident cnd = (ConductIncident) getBean();
            String name = "[Incident No: " + cnd.getIncidentId() + "] ";
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
     * Retriever for victim id field.
     */
    class RetrieveVictimId implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String result = null;

            PAIncidentVictim srData = (PAIncidentVictim) data;
            ConductIncident incident = (ConductIncident) entity.getBean();
            result = incident.getVictim() == null ? (String) incident.getFieldValueByBeanPath(srData.getFieldVictimId())
                    : incident.getVictim().getPersonOid();

            if (StringUtils.isEmpty(result) &&
                    (!StringUtils.isEmpty((String) incident.getFieldValueByBeanPath(srData.getFieldVictimLname())) ||
                            !StringUtils.isEmpty(
                                    (String) incident.getFieldValueByBeanPath(srData.getFieldVictimFname())))) {
                result = incident.getOid();
            }

            return result;
        }
    }

    /**
     * Retriever offender type.
     *
     */
    public class RetrieveVictimType implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            PAIncidentVictim srData = (PAIncidentVictim) data;
            ConductIncident incident = (ConductIncident) entity.getBean();
            SisStudent std = null;
            String value = "";

            if ((std = incident.getVictim()) != null) {
                value = "1";
                if (ACTIVE_STATUS.equals(std.getSpedStatusCode())) {
                    value = "2";
                }
            } else {
                value = (String) incident.getFieldValueByBeanPath(srData.getFieldVictimType());
                value = data.lookupReferenceCodeByBeanPath(ConductIncident.class, srData.getFieldVictimType(), value,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }

            return value;
        }
    }

    /**
     * Required if MEDICAL TREATMENT REQUIRED INDICATOR (Field 10) is Y.
     */
    class ValidateVictimComment implements FieldValidator {

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

            String medicalTreatmentRequired =
                    (String) entity.getBean().getFieldValueByBeanPath(getFieldVictimMedical());
            if (StringUtils.isEmpty(value) && "1".equalsIgnoreCase(medicalTreatmentRequired)) {
                errors.add(new StateReportValidationError(entity, field, "Required field",
                        "Comment is required if victim required medical treatment"));
            }
            return errors;
        }
    }

    /**
     * Validation for victim id.
     */
    class ValidateVictimId implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(2);

            ConductIncident incident = (ConductIncident) entity.getBean();

            String victimId = incident.getVictimOid();
            String udfPersonVictimId = (String) entity.getBean().getFieldValueByBeanPath(getFieldVictimId());

            if (StringUtils.isEmpty(victimId) && StringUtils.isEmpty(udfPersonVictimId)) {
                errors.add(new StateReportValidationError(entity, field, "Value required", ""));
            }

            if (!StringUtils.isEmpty(udfPersonVictimId) && !StringUtils.isEmpty(victimId)
                    && !victimId.equals(udfPersonVictimId)) {
                Person personVictim = (Person) getBroker().getBeanByOid(Person.class, udfPersonVictimId);

                String personName =
                        (personVictim != null) ? personVictim.getNameView() : ">personId does not exist in DB <";

                String detailMessage = null;
                detailMessage =
                        "A single conduct incident record cannot refer to both a person victim and a student victim."
                                + " This record refers to person "
                                + " [PersonId: " + udfPersonVictimId + " Name: " + personName + "]"
                                + " and student"
                                + " [ LASID: " + victimId + " Name: "
                                + (incident.getVictim() == null ? "Unknown" : incident.getVictim().getNameView())
                                + "].";
                errors.add(new StateReportValidationError(entity, field,
                        "Record cannot include both a person victim and a student victims", detailMessage));
            }

            return errors;
        }
    }

    /**
     * Validation for victim severity.
     */
    class ValidateVictimInjSeverity implements FieldValidator {

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

            String udfValue = (String) entity.getBean().getFieldValueByBeanPath(getFieldVictimSeverity());
            if (StringUtils.isEmpty(udfValue)) {
                errors.add(new StateReportValidationError(entity, field, "Value required", ""));
            }
            return errors;
        }
    }

    /**
     * The Class ValidateVictimType.
     */
    class ValidateVictimType implements FieldValidator {

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

            String udfValue = (String) entity.getBean().getFieldValueByBeanPath(getFieldVictimType());
            if (StringUtils.isEmpty(udfValue)) {
                errors.add(new StateReportValidationError(entity, field, "Value required", ""));
            }
            return errors;
        }
    }

    /**
     * Aliases
     */
    private static final String ALIAS_VICTIM_FNAME = "DOE EXT VIC FNAME";
    private static final String ALIAS_VICTIM_LNAME = "DOE EXT VIC LNAME";
    private static final String ALIAS_INCIDENT_INCLUDE = "DOE PIMS INCIDENT INCLUDE";
    private static final String ALIAS_VICTIM_ID = "DOE VICTIM ID";
    private static final String ALIAS_VICTIM_INJ_SEV = "DOE VICTIM SEVERITY";
    private static final String ALIAS_VICTIM_MEDICAL = "DOE VICTIM MEDICAL";
    private static final String ALIAS_VICTIM_TYPE = "DOE VICTIM TYPE";

    /**
     * Retrievers' IDs
     */
    private static final String CALC_ID_VICTIM_ID = "VICTIM ID";
    private static final String CALC_ID_VICTIM_TYPE = "VICTIM TYPE";

    /**
     * Parameters
     */
    private static final String PARAM_END_DATE = "endDate";
    private static final String PARAM_START_DATE = "startDate";

    /**
     * Validators' IDs
     */
    private static final String VAL_ID_INJURITY_SEVERITY = "VAL_CNDV_SEVERITY";
    private static final String VAL_ID_VICTIM_COMMENT = "VICTIM COMMENT";
    private static final String VAL_ID_VICTIM_ID = "VICTIM ID";
    private static final String VAL_ID_VICTIM_TYPE = "VAL_CNDV_VICTIM_TYPE";

    /**
     * Other constants
     */
    private static final String ACTIVE_STATUS = "Active";

    /**
     * Class members
     */
    private String m_fieldIncidentInclude;
    private String m_fieldVictimFname;
    private String m_fieldVictimId;
    private String m_fieldVictimLname;
    private String m_fieldVictimMedical;
    private String m_fieldVictimSeverity;
    private String m_fieldVictimType;

    /**
     * Gets the field victim fname.
     *
     * @return the m_fieldVictimFname
     */
    public String getFieldVictimFname() {
        return m_fieldVictimFname;
    }

    /**
     * Gets the field victim id.
     *
     * @return the m_fieldVictimId
     */
    public String getFieldVictimId() {
        return m_fieldVictimId;
    }

    /**
     * Gets the field victim lname.
     *
     * @return the m_fieldVictimLname
     */
    public String getFieldVictimLname() {
        return m_fieldVictimLname;
    }

    /**
     * Gets the field victim medical.
     *
     * @return the m_fieldVictimMedical
     */
    public String getFieldVictimMedical() {
        return m_fieldVictimMedical;
    }

    /**
     * Gets the field victim severity.
     *
     * @return the m_fieldVictimSeverity
     */
    public String getFieldVictimSeverity() {
        return m_fieldVictimSeverity;
    }

    /**
     * Gets the field victim type.
     *
     * @return the m_fieldVictimType
     */
    public String getFieldVictimType() {
        return m_fieldVictimType;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        initializeFields();

        if (getSetupErrors().size() == 0) {
            // Resulting incidentCriteria is similar to
            // WHERE [incidentCtiteria]
            // AND ([DOE VICTIM ID] IS NOT NULL OR [victimOid] IS NOT NULL)

            Criteria incidentCriteria = getIncidentCriteria();
            applyInputCriteria(incidentCriteria, false, null);

            Criteria andCriteria = new Criteria();

            X2Criteria victimIdCriteria = new X2Criteria();
            victimIdCriteria.addNotEmpty(m_fieldVictimId, getBroker().getPersistenceKey());
            andCriteria.addOrCriteria(victimIdCriteria);

            X2Criteria victimOidCriteria = new X2Criteria();
            victimOidCriteria.addNotEmpty(ConductIncident.REL_VICTIM + PATH_DELIMITER + SisStudent.COL_PERSON_OID,
                    getBroker().getPersistenceKey());
            andCriteria.addOrCriteria(victimOidCriteria);

            X2Criteria fNameCriteria = new X2Criteria();
            fNameCriteria.addNotEmpty(m_fieldVictimFname, getBroker().getPersistenceKey());
            andCriteria.addOrCriteria(fNameCriteria);

            X2Criteria lNameCriteria = new X2Criteria();
            lNameCriteria.addNotEmpty(m_fieldVictimLname, getBroker().getPersistenceKey());
            andCriteria.addOrCriteria(lNameCriteria);

            incidentCriteria.addAndCriteria(andCriteria);

            QueryByCriteria incidentQuery = new QueryByCriteria(ConductIncident.class, incidentCriteria, true);
            setQuery(incidentQuery);
            setEntityClass(PAIncidentVictimEntity.class);
        }
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
        Collection<String> codes = new HashSet<String>();
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
        m_fieldIncidentInclude = translateAliasToJavaName(ALIAS_INCIDENT_INCLUDE, true);
        m_fieldVictimMedical = translateAliasToJavaName(ALIAS_VICTIM_MEDICAL, true);
        m_fieldVictimId = translateAliasToJavaName(ALIAS_VICTIM_ID, true);

        m_fieldVictimType = translateAliasToJavaName(ALIAS_VICTIM_TYPE, true);
        m_fieldVictimSeverity = translateAliasToJavaName(ALIAS_VICTIM_INJ_SEV, true);

        m_fieldVictimFname = this.translateAliasToJavaName(ALIAS_VICTIM_FNAME, true);
        m_fieldVictimLname = this.translateAliasToJavaName(ALIAS_VICTIM_LNAME, true);

        // Add retrievers
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(CALC_ID_VICTIM_ID, new RetrieveVictimId());
        calcs.put(CALC_ID_VICTIM_TYPE, new RetrieveVictimType());
        addCalcs(calcs);

        // Add validators
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(VAL_ID_VICTIM_ID, new ValidateVictimId());
        validators.put(VAL_ID_VICTIM_COMMENT, new ValidateVictimComment());
        validators.put(VAL_ID_VICTIM_TYPE, new ValidateVictimType());
        validators.put(VAL_ID_INJURITY_SEVERITY, new ValidateVictimInjSeverity());
        addValidators(validators);
    }

}