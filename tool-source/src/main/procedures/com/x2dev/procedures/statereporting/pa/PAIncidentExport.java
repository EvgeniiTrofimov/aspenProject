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
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.QueryIterator;
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
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Incident Export.
 */
public class PAIncidentExport extends StateReportData {

    /**
     * Entity class.
     */
    public static class PAIncidentEntity extends StateReportEntity {
        PAIncidentExport m_exportData;
        List<List<ConductIncident>> m_incidentList;

        /**
         * Instantiates a new PA incident entity.
         */
        public PAIncidentEntity() {

        }

        /**
         * Gets the entity name.
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
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            m_exportData = (PAIncidentExport) data;
            m_incidentList = new ArrayList(m_exportData.m_conductIncidents.values());
            setRowCount(m_exportData.m_conductIncidents.size());
            super.intitialize(data, bean);
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
     * Default field retriever gets value from conductIncidents map
     * according to given field definition.
     */
    class DefaultFieldRetriever implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity reportEntity, FieldDefinition field)
                throws X2BaseException {
            PAIncidentEntity entity = (PAIncidentEntity) reportEntity;
            List<ConductIncident> currIncidents = entity.getIncidents();

            Object value = null;
            List<Object> values = new ArrayList<Object>(currIncidents.size());
            String calcParam = (String) field.getParameter();

            if (CALC_PARAM_AGAINST_PROPERTY.equals(calcParam)) {
                for (ConductIncident incident : currIncidents) {
                    value = incident.getFieldValueByBeanPath(m_fieldProperty);
                    if (value == null) {
                        continue;
                    }
                    values.add(value);
                }
                if (values.size() == 0) {
                    values.add("N");
                }
            } else if (CALC_PARAM_UNFOUNDED_IND.equals(calcParam)) {
                for (ConductIncident incident : currIncidents) {
                    value = incident.getFieldValueByBeanPath(m_fieldCndUnfoundedInd);
                    if (value == null) {
                        continue;
                    }
                    values.add(value);
                }
                if (values.size() == 0) {
                    values.add("N");
                }
            }

            for (ConductIncident incident : currIncidents) {
                value = null;
                if (CALC_PARAM_SCHOOL_YEAR.equals(calcParam)) {
                    try {
                        int year = incident.getSchool().getOrganization1().getCurrentContext().getSchoolYear();
                        Calendar schoolYear = Calendar.getInstance();
                        schoolYear.set(year, Calendar.JUNE, 30);
                        value = schoolYear.getTime();
                    } catch (NullPointerException e) {
                        // null result desired for parsing error
                    }
                } else if (CALC_PARAM_INCIDENT_ID.equals(calcParam)) {
                    value = incident.getIncidentId();
                    values.add(value);
                    break;
                } else if (CALC_PARAM_INCIDENT_DATE.equals(calcParam)) {
                    value = incident.getIncidentDate();
                } else if (CALC_PARAM_LOCATION_CODE.equals(calcParam)) {
                    if (!StringUtils.isEmpty(m_fieldLocationCode)) {
                        value = incident.getFieldValueByBeanPath(m_fieldLocationCode);
                    }
                    if (value == null) {
                        value = incident.getSchool().getSchoolId();
                    }
                } else if (CALC_PARAM_PLACE_CODE.equals(calcParam)) {
                    value = data.lookupReferenceCodeByBeanPath(ConductIncident.class,
                            ConductIncident.COL_INCIDENT_LOCATION,
                            incident.getIncidentLocation(),
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                } else if (CALC_PARAM_TIME_FRAME_CODE.equals(calcParam)) {
                    value = data.lookupReferenceCodeByBeanPath(ConductIncident.class,
                            m_fieldIncidentTime,
                            (String) incident.getFieldValueByBeanPath(m_fieldIncidentTime),
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                } else if (CALC_PARAM_LLE_NUMBER.equals(calcParam)) {
                    value = incident.getFieldValueByBeanPath(m_fieldLLENumber);
                }

                if (value != null) {
                    values.add(value);
                }
            }

            // For the multiple values checking, we want to accept not empty
            // values as potentials. If the field is empty in one
            // ConductIncident and populated with the same value in two other
            // ConductIncidents, the not empty value should be used without
            // error.
            // The error occurs when there are differing non-empty values.
            // For a field where ambiguous values are detected, simply return
            // ##dup from the field retriever.
            // This will fail validation in most cases with validation patterns
            // and patterns should be added to other fields to insure this value
            // will fail.
            if (values.size() == 1) {
                value = values.get(0);
            } else if (values.size() > 1) {
                value = values.get(0);
                boolean hasErrors = false;
                for (Object v : values) {
                    if (!value.equals(v)) {
                        hasErrors = true;
                        break;
                    }
                }
                if (hasErrors) {
                    value = "##dup";
                }
            }

            return value;
        }
    }

    /**
     * TIME FRAME CODE field validator <br>
     * TIME FRAME CODE is required when PLACE CODE field equals to "1".
     */
    class TimeFrameCodeFieldValidator implements FieldValidator {

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

            String placeCode = entity.getFieldValue(FIELD_PLACE_CODE);
            if ("1".equals(placeCode) && StringUtils.isEmpty(value)) {
                StateReportValidationError e =
                        new StateReportValidationError(entity, field, "Field value is not defined",
                                "TIME FRAME CODE is required when PLACE CODE field equals to '1'");
                errors.add(e);
            }

            return errors;
        }
    }

    static final String ALIAS_CND_UNFOUNDED_IND = "all-cnd-UnfoundedIndicator";
    static final String ALIAS_INCIDENT_INCLUDE = "DOE PIMS INCIDENT INCLUDE";
    static final String ALIAS_INCIDENT_TIME = "DOE TIME";
    static final String ALIAS_LLE_NUMBER = "all-cnd-LLEIncidentNumber";
    static final String ALIAS_LOCATION_CODE = "DOE LOCATION CODE";
    static final String ALIAS_PROPERTY = "DOE PROPERTY";

    static final String CALC_ID_CND_CALC_DEFAULT = "CND_CALC_DEFAULT";

    static final String CALC_PARAM_AGAINST_PROPERTY = "AGAINST PROPERTY";
    static final String CALC_PARAM_INCIDENT_DATE = "INCIDENT DATE";
    static final String CALC_PARAM_INCIDENT_ID = "INCIDENT ID";
    static final String CALC_PARAM_LLE_NUMBER = "LLE NUMBER";
    static final String CALC_PARAM_LOCATION_CODE = "LOCATION CODE";
    static final String CALC_PARAM_PLACE_CODE = "PLACE CODE";
    static final String CALC_PARAM_SCHOOL_YEAR = "SCHOOL YEAR";
    static final String CALC_PARAM_TIME_FRAME_CODE = "TIME FRAME CODE";
    static final String CALC_PARAM_UNFOUNDED_IND = "UNFOUNDED-IND";

    static final String FIELD_PLACE_CODE = "PLACE CODE";

    // Export input parameters
    static final String PARAM_END_DATE = "endDate";
    static final String PARAM_START_DATE = "startDate";

    static final String VAL_ID_TIME_FRAME_CODE = "TIME FRAME CODE";

    /**
     * Map incidentID to List&lt;ConductIncident&gt;
     */
    Map<String, List<ConductIncident>> m_conductIncidents;
    String m_fieldCndUnfoundedInd;
    String m_fieldIncidentInclude;
    String m_fieldIncidentTime;
    String m_fieldLLENumber;
    String m_fieldLocationCode;
    String m_fieldProperty;

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();
        m_conductIncidents = new HashMap<String, List<ConductIncident>>();

        if (getSetupErrors().size() == 0) {
            // Set query for organization
            Criteria orgCriteria = new Criteria();
            orgCriteria.addEqualTo(X2BaseBean.COL_OID, getOrganization().getOid());
            setQuery(new QueryByCriteria(Organization.class, orgCriteria));

            Criteria incidentCriteria = getIncidentCriteria();
            applyInputCriteria(incidentCriteria, false, null);
            QueryByCriteria incidentQuery = new QueryByCriteria(ConductIncident.class, incidentCriteria, true);
            setEntityClass(PAIncidentEntity.class);

            QueryIterator iterator = getBroker().getIteratorByQuery(incidentQuery);
            try {
                while (iterator.hasNext()) {
                    ConductIncident incident = (ConductIncident) iterator.next();
                    String incidentId = incident.getIncidentId();
                    if (!m_conductIncidents.containsKey(incidentId)) {
                        m_conductIncidents.put(incidentId, new ArrayList<ConductIncident>());
                    }
                    m_conductIncidents.get(incidentId).add(incident);
                }
            } finally {
                iterator.close();
            }
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
        // Initialize fields from aliases
        m_fieldLocationCode = translateAliasToJavaName(ALIAS_LOCATION_CODE, false);
        m_fieldIncidentTime = translateAliasToJavaName(ALIAS_INCIDENT_TIME, true);
        m_fieldLLENumber = translateAliasToJavaName(ALIAS_LLE_NUMBER, true);
        m_fieldProperty = translateAliasToJavaName(ALIAS_PROPERTY, true);
        m_fieldIncidentInclude = translateAliasToJavaName(ALIAS_INCIDENT_INCLUDE, true);
        m_fieldCndUnfoundedInd = translateAliasToJavaName(ALIAS_CND_UNFOUNDED_IND, true);

        // Add retrievers
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(CALC_ID_CND_CALC_DEFAULT, new DefaultFieldRetriever());
        addCalcs(calcs);

        // Add validators
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(VAL_ID_TIME_FRAME_CODE, new TimeFrameCodeFieldValidator());
        addValidators(validators);
    }

}
