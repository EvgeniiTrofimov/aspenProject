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
import com.x2dev.sis.model.beans.ConductOffense;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Incident Offender Infraction Export.
 */
public class PAIncidentInfraction extends StateReportData {

    /**
     * Entity class .
     */
    public static class PAIncidentInfractionEntity extends StateReportEntity {

        /**
         * Instantiates a new PA incident infraction entity.
         */
        public PAIncidentInfractionEntity() {}

        /**
         * Gets the entity name.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            ConductOffense cno = (ConductOffense) getBean();
            ConductIncident incident = cno.getIncident();
            SisStudent student = incident.getStudent();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] +" +
                    " [Incident No: " + incident.getIncidentId() +
                    ", Offense Code: " + cno.getIncidentCode() + "] ";
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
     * Retriever for offense code.
     */
    class RetrieveOffenseCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String result = null;

            ConductOffense cno = (ConductOffense) entity.getBean();

            if (cno != null) {
                result = lookupStateValue(ConductOffense.class, ConductOffense.COL_INCIDENT_CODE,
                        cno.getIncidentCode());
            }

            return result;
        }
    }

    /**
     * Required if INFRACTION CODE is 16, 41, or 46.
     */
    class ValidateInfractionComment implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();

            int incidentCode = -1;
            try {
                incidentCode = Integer.parseInt(entity.getFieldValue(FIELD_INFRACTION_CODE));
            } catch (Exception e) {
                // unable to get numeric code
            }

            if (StringUtils.isEmpty(value) && (incidentCode == 16 || incidentCode == 41 || incidentCode == 46)) {
                errors.add(new StateReportValidationError(entity, field, "Field must be populated",
                        "This comment is required for infractions 16, 41 & 46"));
            }
            return errors;
        }
    }

    static final String ALIAS_INCIDENT_INCLUDE = "DOE PIMS INCIDENT INCLUDE";

    static final String FIELD_INFRACTION_CODE = "INFRACTION CODE";

    // Export input parameters
    static final String PARAM_END_DATE = "endDate";
    static final String PARAM_START_DATE = "startDate";

    // Retirevers IDs
    static final String CALC_ID_CNO_OFFENSE_CODE = "CNO_OFFENSE_CODE";

    // Validator IDs
    static final String VAL_ID_INFRACTION_COMMENT = "INFRACTION COMMENT";

    /**
     * Class members
     */
    private String m_fieldIncidentInclude;

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
            Criteria infractionsCriteria = getInfractionsCriteria();
            QueryByCriteria infractionsQuery = new QueryByCriteria(ConductOffense.class, infractionsCriteria, true);
            setQuery(infractionsQuery);
            setEntityClass(PAIncidentInfractionEntity.class);
        }
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
     * Infractions Criteria used to get corresponding actions.
     *
     * @return Criteria incidentCriteria
     */
    private Criteria getInfractionsCriteria() {
        X2Criteria incidentCriteria = getIncidentCriteria();
        incidentCriteria.addNotNull(ConductIncident.COL_STUDENT_OID);
        applyInputCriteria(incidentCriteria, false, null);

        SubQuery incidentQuery = new SubQuery(ConductIncident.class, X2BaseBean.COL_OID, incidentCriteria);

        X2Criteria infractionsCriteria = new X2Criteria();

        infractionsCriteria.addIn(ConductOffense.REL_INCIDENT + ModelProperty.PATH_DELIMITER +
                X2BaseBean.COL_OID, incidentQuery);
        infractionsCriteria.addIn(ConductOffense.COL_INCIDENT_CODE, getStateReportableCodes());

        return infractionsCriteria;
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
        m_fieldIncidentInclude = translateAliasToJavaName(ALIAS_INCIDENT_INCLUDE, true);

        // Add retrievers
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(CALC_ID_CNO_OFFENSE_CODE, new RetrieveOffenseCode());
        addCalcs(calcs);

        // Add validators
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(VAL_ID_INFRACTION_COMMENT, new ValidateInfractionComment());
        addValidators(validators);
    }
}