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
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.UserDefinedTableB;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Incident Offender weapon Export.
 */
public class PAIncidentWeapon extends StateReportData {

    /**
     * Entity class .
     */
    public static class PAIncidentWeaponEntity extends StateReportEntity {

        /**
         * Instantiates a new PA incident weapon entity.
         */
        public PAIncidentWeaponEntity() {}

        /**
         * Gets the entity name.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            UserDefinedTableB udb = (UserDefinedTableB) getBean();
            ConductIncident incident = udb.getConductIncident();
            SisStudent student = incident.getStudent();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() + "] +";
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
     * Required if INFRACTION CODE is between 39 and 46.
     */
    class ValidateWeaponCount implements FieldValidator {

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

            int weaponCount = 0;
            try {
                weaponCount = Integer.parseInt(value);
            } catch (Exception e) {
                // unable to get numeric code
            }

            if ((StringUtils.isEmpty(value) || weaponCount == 0) && 39 <= incidentCode && incidentCode <= 46) {
                errors.add(new StateReportValidationError(entity, field, "Field must be greater than 0",
                        "This value is required for infractions 39 - 46"));
            }

            return errors;
        }
    }

    static final String ALIAS_INCIDENT_INCLUDE = "DOE PIMS INCIDENT INCLUDE";
    static final String ID_DDX_CONDUCT_WEAPON = "CND-WPN";
    static final String FIELD_INFRACTION_CODE = "INFRACTION CODE";

    // Export input parameters
    static final String PARAM_END_DATE = "endDate";
    static final String PARAM_START_DATE = "startDate";

    // Validator IDs
    static final String VAL_ID_WEAPON_COUNT = "WEAPON COUNT";

    static final String STATE_CODE_NO = "N";
    static final String STATE_CODE_YES = "Y";

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
        initializeDictionary();
        initializeFields();
        if (getSetupErrors().size() == 0) {
            Criteria incidentCriteria = getIncidentCriteria();
            QueryByCriteria infractionsQuery = new QueryByCriteria(UserDefinedTableB.class, incidentCriteria, true);
            setQuery(infractionsQuery);
            setEntityClass(PAIncidentWeaponEntity.class);
        }
    }

    /**
     * Incident Criteria used to get corresponding actions.
     *
     * @return X2Criteria incidentCriteria
     */
    private X2Criteria getIncidentCriteria() {
        X2Criteria incidentCriteria = new X2Criteria();

        incidentCriteria.addNotEqualTo(
                UserDefinedTableB.REL_CONDUCT_INCIDENT + ModelProperty.PATH_DELIMITER + m_fieldIncidentInclude,
                STATE_CODE_NO);
        incidentCriteria.addEqualTo(UserDefinedTableB.REL_CONDUCT_INCIDENT + ModelProperty.PATH_DELIMITER +
                ConductIncident.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                SisSchool.COL_ORGANIZATION1_OID, super.getOrganization().getOid());
        incidentCriteria.addIn(UserDefinedTableB.REL_CONDUCT_INCIDENT + ModelProperty.PATH_DELIMITER +
                ConductIncident.COL_INCIDENT_CODE, getStateReportableCodes());

        if (isSchoolContext()) {
            incidentCriteria.addEqualTo(UserDefinedTableB.REL_CONDUCT_INCIDENT + ModelProperty.PATH_DELIMITER +
                    ConductIncident.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            incidentCriteria.addNotEqualTo(UserDefinedTableB.REL_CONDUCT_INCIDENT + ModelProperty.PATH_DELIMITER +
                    ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            incidentCriteria.addNotEqualTo(UserDefinedTableB.REL_CONDUCT_INCIDENT + ModelProperty.PATH_DELIMITER +
                    ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }

        Date startDate = (Date) getParameter(PARAM_START_DATE);
        if (startDate != null) {
            incidentCriteria
                    .addGreaterOrEqualThan(UserDefinedTableB.REL_CONDUCT_INCIDENT + ModelProperty.PATH_DELIMITER +
                            ConductIncident.COL_INCIDENT_DATE, startDate);
        }

        Date endDate = (Date) getParameter(PARAM_END_DATE);
        if (endDate != null) {
            incidentCriteria.addLessOrEqualThan(UserDefinedTableB.REL_CONDUCT_INCIDENT + ModelProperty.PATH_DELIMITER +
                    ConductIncident.COL_INCIDENT_DATE, endDate);
        }
        incidentCriteria.addNotNull(UserDefinedTableB.REL_CONDUCT_INCIDENT + ModelProperty.PATH_DELIMITER +
                ConductIncident.COL_STUDENT_OID);
        applyInputCriteria(incidentCriteria, false, UserDefinedTableB.REL_CONDUCT_INCIDENT);

        return incidentCriteria;
    }

    /**
     * Set the extended data dictionary for the UDB table record.
     */
    private void initializeDictionary() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, ID_DDX_CONDUCT_WEAPON);
        BeanQuery query = new BeanQuery(ExtendedDataDictionary.class, criteria);
        ExtendedDataDictionary ddx = getBroker().getBeanByQuery(query);
        setDataDictionary(DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey()));

    }

    /**
     * Returns state reportable codes.
     *
     * @return Collection<String> codes
     */
    private Collection<String> getStateReportableCodes() {
        Set<String> codes = new HashSet<String>();
        DataDictionaryField field = getDataDictionaryField(ConductIncident.class, ConductIncident.COL_INCIDENT_CODE);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());

        String[] columns = new String[] {ReferenceCode.COL_CODE};
        ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] record = (Object[]) iterator.next();
                String code = (String) record[0];
                codes.add(code);
            }
        } finally {
            iterator.close();
        }
        return codes;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldIncidentInclude = translateAliasToJavaName(ALIAS_INCIDENT_INCLUDE, true);

        // Add retrievers
        // Add validators
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(VAL_ID_WEAPON_COUNT, new ValidateWeaponCount());
        addValidators(validators);
    }
}
