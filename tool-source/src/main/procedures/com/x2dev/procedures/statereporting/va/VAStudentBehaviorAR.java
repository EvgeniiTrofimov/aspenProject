/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.va;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.ConductOffense;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.UserDefinedTableA;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * @author Follett Software Company
 * @copyright 2021
 */
public class VAStudentBehaviorAR extends StateReportData {

    /**
     * Entity class for VA Student Behavior export.
     *
     * @author Follett Software Company
     * @copyright 2021
     */
    public static class VAStudentBehaviorAREntity extends StateReportEntity {
        /**
         * List of row definitions indexed by row number. These definitions
         * select an export format definition to use for the row. Valid values
         * are: null,C,D,E (null represents row type B).
         */
        private List<String> m_definitionId;
        private List<X2BaseBean> m_elements;

        // Keep lists if offense and action codes for comparison in validation.
        private List<String> m_offenses;
        private List<String> m_actions;
        private List<String[]> m_actionItem;

        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            VAStudentBehaviorAR sbarData = (VAStudentBehaviorAR) getData();
            m_definitionId = new ArrayList<String>();
            m_elements = new ArrayList<X2BaseBean>();
            m_actionItem = new ArrayList<String[]>();

            ConductIncident incident = (ConductIncident) bean;
            Collection<ConductOffense> offenses = incident.getConductOffenses(data.getBroker());
            Collection<ConductAction> actions = incident.getConductActions(data.getBroker());
            Collection<UserDefinedTableA> victims = incident.getUserDefinedRecordsA(data.getBroker());

            m_offenses = new ArrayList<String>();
            m_actions = new ArrayList<String>();

            // We only need the first "B" row for the first incident
            // with the common Incident ID.
            // We only add the final "E" row at the end of the Incident ID.
            // When an incident ID is on more than one incident record,
            // we need to know when we are on the LAST incident with that ID.
            // We count the current iterating incidents for an ID.
            // When we are on an incident id with multiple records, count the
            // records that we have iterated over.
            String incidentId = incident.getIncidentId();
            if (sbarData.m_currentIncidentId == null || !sbarData.m_currentIncidentId.equals(incidentId)) {
                // new incident ID: restart count, reset victim count.
                sbarData.m_currentIncidentId = incidentId;
                sbarData.m_currentIncidentCount = 1;
                sbarData.m_victimCounts = new int[5];
                sbarData.m_victimEntries = new HashSet<String>();
            } else {
                // repeat incident ID.
                sbarData.m_currentIncidentCount++;
            }

            // Add the first "B" row, if the incident is the first with the ID.
            if (sbarData.m_currentIncidentCount == 1) {
                m_elements.add(incident);
                m_definitionId.add(null); // The incident will be the B rows.
                m_actionItem.add(null); // Not an action record, not relevant.
            }
            // Add any additional
            for (ConductOffense offense : offenses) {
                if (offense.getPrimary()) {
                    offense = copyPrimaryOffense(incident, offense);
                }
                m_elements.add(offense);
                m_definitionId.add("C");
                m_actionItem.add(null); // Not an action record, not relevant.
                m_offenses.add(
                        data.lookupReferenceCodeByBeanPath(
                                ConductOffense.class,
                                ConductOffense.COL_INCIDENT_CODE,
                                offense.getIncidentCode(),
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal()));
            }
            for (ConductAction action : actions) {
                // Expand multi-value interventions and instructional supports.
                // enter an Action/D record for each expanded value pair.
                List<String> interventions = new ArrayList<>();
                List<String> instructional = new ArrayList<>();
                int dRows = 0;
                String l_intervention = (String) action.getFieldValueByBeanPath(sbarData.m_fieldIntervention);
                if (!StringUtils.isEmpty(l_intervention)) {
                    interventions = StringUtils.convertDelimitedStringToList(l_intervention, ',', true);
                    for (int i = 0; i < interventions.size(); i++) {
                        String intervention = interventions.get(i);
                        String code =
                                data.lookupReferenceCodeByBeanPath(ConductAction.class,
                                        sbarData.m_fieldIntervention,
                                        intervention, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        interventions.set(i, code);
                    }
                    dRows = interventions.size();
                }
                String l_instructional = (String) action.getFieldValueByBeanPath(sbarData.m_fieldInstructional);
                if (!StringUtils.isEmpty(l_instructional)) {
                    instructional = StringUtils.convertDelimitedStringToList(l_instructional, ',', true);
                    for (int i = 0; i < instructional.size(); i++) {
                        String instruction = instructional.get(i);
                        String code =
                                data.lookupReferenceCodeByBeanPath(ConductAction.class,
                                        sbarData.m_fieldInstructional,
                                        instruction, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        instructional.set(i, code);
                    }
                    dRows = instructional.size() > dRows ? instructional.size() : dRows;
                }
                if (dRows > 0) {
                    for (int i = 0; i < dRows; i++) {
                        String[] actionElement = new String[2];
                        if (i < interventions.size()) {
                            actionElement[0] = interventions.get(i);
                        }
                        if (i < instructional.size()) {
                            actionElement[1] = instructional.get(i);
                        }
                        m_elements.add(action);
                        m_definitionId.add("D");
                        m_actionItem.add(actionElement);
                    }
                } else {
                    m_elements.add(action);
                    m_definitionId.add("D");
                    m_actionItem.add(null);
                }
                m_actions.add(
                        data.lookupReferenceCodeByBeanPath(
                                ConductAction.class,
                                ConductAction.COL_ACTION_CODE,
                                action.getActionCode(),
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal()));
            }
            int totalVictims = countVictims(victims, incident, sbarData);

            // if the incident is not in the map, or we are on the last/max incident
            // with this ID, add the "E" record.
            Integer max = sbarData.m_incidentCountsById.get(incidentId);
            if (max == null || max.intValue() == sbarData.m_currentIncidentCount) {
                if (totalVictims > 0) {
                    m_elements.add(incident);
                    m_definitionId.add("E");
                }
            }

            setRowCount(m_elements.size());
        }

        /**
         * Override getBean() to return alternate types of beans for different
         * rows. Offense type rows should return a offense record. Action
         * rows should return an action record. The default is to
         * return the super
         *
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getBean()
         */
        @Override
        public X2BaseBean getBean() {
            X2BaseBean bean = super.getBean();
            if ((getCurrentRow() >= 0) &&
                    (getCurrentRow() < m_elements.size()) &&
                    (m_elements.get(getCurrentRow()) != null)) {
                bean = m_elements.get(getCurrentRow());
            }
            return bean;
        }

        /**
         * For actions/D rows, return the extra element for the Intervention and Instruction
         * support.
         *
         * @return String[]
         */
        public String[] getCurrentActionElements() {
            String[] element = null;
            if ((getCurrentRow() >= 0) &&
                    (getCurrentRow() < m_actionItem.size())) {
                element = m_actionItem.get(getCurrentRow());
            }
            return element;
        }

        /**
         * Return a list if all action codes on the incident.
         * For use with validation rules.
         *
         * @return List<String>
         */
        public List<String> getActionCodes() {
            return m_actions;
        }

        /**
         * Return a list if all offense codes on the incident.
         * For use with validation rules.
         *
         * @return List<String>
         */
        public List<String> getOffenseCodes() {
            return m_offenses;
        }

        /**
         * Specify the definition Id for the current row based on getCurrentRow().
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#
         *      getCurrentFormatDefinitionId()
         */
        @Override
        public String getCurrentFormatDefinitionId() {
            String definitionId = null;
            int rownum = getCurrentRow();
            if ((rownum >= 0) && (rownum < m_definitionId.size())) {
                definitionId = m_definitionId.get(rownum);
            }
            return definitionId;
        }

        /**
         * Specify the previous definition Id for the current row based on getCurrentRow().
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#
         *      getCurrentFormatDefinitionId()
         */
        public String getPreviousFormatDefinitionId() {
            String definitionId = null;
            int rownum = getCurrentRow();
            if ((rownum >= 1) && (rownum < m_definitionId.size())) {
                definitionId = m_definitionId.get(rownum - 1);
            }
            return definitionId;
        }

        /**
         * Return the incident ID and student name as the entity name.
         *
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            // Get the true incident bean instead of the current bean type.
            ConductIncident incident = (ConductIncident) super.getBean();
            return incident.getIncidentId() + " " + incident.getStudent().getNameView();
        }

        /**
         * Clone the primary offense and fill in the custom fields
         * from the equivalent fields on the incident record.
         * This allows the export to treat the primary offense as just another offense record
         * when the primary behavior data is on the incident record.
         *
         * @param offense
         *
         * @return ConductOffense
         */
        private ConductOffense copyPrimaryOffense(ConductIncident incident, ConductOffense offense) {
            ConductOffense newOffense = offense.clone();
            DataDictionary dictionary = getData().getDataDictionary();
            newOffense.setFieldValueByAlias(ALIAS_CNO_AGGREV,
                    incident.getFieldValueByAlias(ALIAS_CND_AGGREV, dictionary), dictionary);
            newOffense.setFieldValueByAlias(ALIAS_CNO_DOE_AUTH,
                    incident.getFieldValueByAlias(ALIAS_CND_DOE_AUTH, dictionary), dictionary);
            newOffense.setFieldValueByAlias(ALIAS_CNO_LAW_ENFORCE,
                    incident.getFieldValueByAlias(ALIAS_CND_LAW_ENFORCE, dictionary), dictionary);
            newOffense.setFieldValueByAlias(ALIAS_CNO_UNKNOWN_OFFENDER,
                    incident.getFieldValueByAlias(ALIAS_CND_UNKNOWN_OFFENDER, dictionary), dictionary);
            return newOffense;
        }

        /**
         * Count all victim records in UDA to count out all victim types into the victimCounts
         * array.
         *
         * @param victims
         * @param incident
         * @param VAStudentBehaviorAR
         *
         * @return a total of counts, or the indicator if true.
         */
        private int countVictims(Collection<UserDefinedTableA> victims,
                                 ConductIncident incident,
                                 VAStudentBehaviorAR sbarData) {
            String undeterminateVictim =
                    (String) incident.getFieldValueByAlias(ALIAS_INDETERMINATE_VICTIM, sbarData.m_udaDictionary);
            boolean bUndeterminateVictim = BooleanAsStringConverter.TRUE.equals(undeterminateVictim);
            if (bUndeterminateVictim) {
                sbarData.m_victimCounts[4] = 1;
            }
            if (sbarData.m_udaDictionary != null) {
                for (UserDefinedTableA victim : victims) {
                    String victimType =
                            (String) victim.getFieldValueByAlias(ALIAS_VICTIM_TYPE, sbarData.m_udaDictionary);
                    String victimName =
                            (String) victim.getFieldValueByAlias(ALIAS_VICTIM_NAME, sbarData.m_udaDictionary);
                    Person victimPerson = victim.getPerson();
                    if (victimPerson != null) {
                        if (!sbarData.m_victimEntries.contains(victimPerson.getOid())) {
                            if (victimPerson.getStudentIndicator()) {
                                sbarData.m_victimCounts[0]++;
                            }
                            if (victimPerson.getStaffIndicator()) {
                                sbarData.m_victimCounts[1]++;
                            }
                            sbarData.m_victimEntries.add(victimPerson.getOid());
                        }
                    }
                    if (sbarData.m_victimTypeCodes != null && !StringUtils.isEmpty(victimType)) {
                        if (victimName == null || !sbarData.m_victimEntries.contains(victimName)) {
                            for (ReferenceCode code : sbarData.m_victimTypeCodes) {
                                if (victimType.equals(code.getCode())) {
                                    if ("1".equals(code.getStateCode())) {
                                        sbarData.m_victimCounts[2]++;
                                    } else if ("2".equals(code.getStateCode())) {
                                        sbarData.m_victimCounts[3]++;
                                    }
                                    break;
                                }
                            }
                            if (victimName != null) {
                                sbarData.m_victimEntries.add(victimName);
                            }
                        }
                    }
                }
            }

            if (!bUndeterminateVictim) {
                String studentVictims =
                        (String) incident.getFieldValueByAlias(ALIAS_STUDENT_VICTIM, sbarData.m_udaDictionary);
                String staffVictims =
                        (String) incident.getFieldValueByAlias(ALIAS_STAFF_VICTIM, sbarData.m_udaDictionary);
                String adultVictims =
                        (String) incident.getFieldValueByAlias(ALIAS_ADULT_VICTIM, sbarData.m_udaDictionary);
                String unknownVictims =
                        (String) incident.getFieldValueByAlias(ALIAS_OTHER_VICTIM, sbarData.m_udaDictionary);

                try {
                    int iVictims = Integer.parseInt(studentVictims);
                    if (sbarData.m_victimCounts[0] < iVictims) {
                        sbarData.m_victimCounts[0] = iVictims;
                    }
                } catch (NumberFormatException nfe) {
                    // No value, default to zero.
                }
                try {
                    int iVictims = Integer.parseInt(staffVictims);
                    if (sbarData.m_victimCounts[1] < iVictims) {
                        sbarData.m_victimCounts[1] = iVictims;
                    }
                } catch (NumberFormatException nfe) {
                    // No value, default to zero.
                }
                try {
                    int iVictims = Integer.parseInt(adultVictims);
                    if (sbarData.m_victimCounts[2] < iVictims) {
                        sbarData.m_victimCounts[2] = iVictims;
                    }
                } catch (NumberFormatException nfe) {
                    // No value, default to zero.
                }
                try {
                    int iVictims = Integer.parseInt(unknownVictims);
                    if (sbarData.m_victimCounts[3] < iVictims) {
                        sbarData.m_victimCounts[3] = iVictims;
                    }
                } catch (NumberFormatException nfe) {
                    // No value, default to zero.
                }
            }

            int sum = 0;
            for (int i = 0; i < 5; i++) {
                sum += sbarData.m_victimCounts[i];
            }
            return sum;
        }
    }

    /**
     * Retrieve the district or school ID based on either the custom INCIDENT DISTRICT/SCHOOL
     * fields or the standard district/school ID.
     *
     * @author Follett Software Company
     * @copyright 2021
     */
    protected static class RetrieveIncidentEAID implements FieldRetriever {
        protected static final String RETRIEVER_ID = "IncidentEAID";

        private static final String PARAM_DISTRICT = "district";
        private static final String PARAM_SCHOOL = "school";

        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            VAStudentBehaviorAR sbarData = (VAStudentBehaviorAR) data;
            ConductIncident incident = (ConductIncident) entity.getBean();
            String param = (String) field.getParameter();
            String value = null;
            if (PARAM_DISTRICT.equals(param)) {
                value = (String) incident.getFieldValueByAlias(ALIAS_CND_INCIDENT_DISTRICT, data.getDataDictionary());
                if (!StringUtils.isEmpty(value)) {
                    value = data.lookupReferenceCodeByAlias(ALIAS_CND_INCIDENT_DISTRICT, value,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (StringUtils.isEmpty(value)) {
                    value = (String) data.getOrganization().getFieldValueByBeanPath(sbarData.m_fieldDistrictId);
                }
            } else if (PARAM_SCHOOL.equals(param)) {
                value = (String) incident.getFieldValueByAlias(ALIAS_CND_INCIDENT_SCHOOL, data.getDataDictionary());
                if (!StringUtils.isEmpty(value)) {
                    value = data.lookupReferenceCodeByAlias(ALIAS_CND_INCIDENT_SCHOOL, value,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (StringUtils.isEmpty(value)) {
                    School school = null;
                    if (data != null && data.getSchool() != null) {
                        school = data.getSchool();
                    } else {
                        school = incident.getStudent().getSchool();
                    }
                    if (school != null) {
                        value = (String) school.getFieldValueByBeanPath(sbarData.m_fieldSchoolId);
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retrieve the district or school ID based on either the custom ENROLLED DISTRICT/SCHOOL
     * fields or the student district/school ID.
     *
     * @author Follett Software Company
     * @copyright 2021
     */
    protected static class RetrieveEnrollEAID implements FieldRetriever {
        protected static final String RETRIEVER_ID = "EnrollEAID";

        private static final String PARAM_DISTRICT = "district";
        private static final String PARAM_SCHOOL = "school";

        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            VAStudentBehaviorAR sbarData = (VAStudentBehaviorAR) data;
            ConductOffense offense = (ConductOffense) entity.getBean();
            ConductIncident incident = offense.getIncident();
            String param = (String) field.getParameter();
            String value = null;

            if (PARAM_DISTRICT.equals(param)) {
                value = (String) incident.getFieldValueByAlias(ALIAS_CND_ENROLL_DISTRICT, data.getDataDictionary());
                if (!StringUtils.isEmpty(value)) {
                    value = data.lookupReferenceCodeByAlias(ALIAS_CND_ENROLL_DISTRICT, value,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (StringUtils.isEmpty(value)) {
                    value = (String) incident.getStudent().getOrganization1()
                            .getFieldValueByBeanPath(sbarData.m_fieldDistrictId);
                }
            } else if (PARAM_SCHOOL.equals(param)) {
                value = (String) incident.getFieldValueByAlias(ALIAS_CND_ENROLL_SCHOOL, data.getDataDictionary());
                if (!StringUtils.isEmpty(value)) {
                    value = data.lookupReferenceCodeByAlias(ALIAS_CND_ENROLL_SCHOOL, value,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (StringUtils.isEmpty(value)) {
                    if (data != null && data.getSchool() != null) {
                        value = (String) data.getSchool().getFieldValueByBeanPath(sbarData.m_fieldSchoolId);
                    } else {
                        value = (String) incident.getStudent().getSchool()
                                .getFieldValueByBeanPath(sbarData.m_fieldSchoolId);
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retrieve incremental elements from the multi-value Administrative Intervention and
     * Instructional Support fields.
     */
    protected static class RetrieveActionElement implements FieldRetriever {
        protected static final String RETRIEVER_ID = "action";

        private static final String PARAM_INTERVENTION = "intervention";
        private static final String PARAM_INSTRUCTION = "instruction";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            VAStudentBehaviorAREntity sbarEntity = (VAStudentBehaviorAREntity) entity;
            String value = null;
            String param = (String) field.getParameter();
            String[] element = sbarEntity.getCurrentActionElements();
            if (element != null) {
                if (PARAM_INTERVENTION.equals(param)) {
                    value = element[0];
                } else if (PARAM_INSTRUCTION.equals(param)) {
                    value = element[1];
                }
            }
            return value;
        }
    }

    /**
     * Retrieve the number of days sanctioned.
     */
    protected static class RetrieveDaysSanctioned implements FieldRetriever {
        protected static final String RETRIEVER_ID = "sanction";
        private static final String PARAM_CODE = "code";
        private static final String PARAM_DAYS = "days";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            VAStudentBehaviorAREntity sbarEntity = (VAStudentBehaviorAREntity) entity;
            String prevId = sbarEntity.getPreviousFormatDefinitionId();
            X2BaseBean bean = entity.getBean();
            Object value = null;
            if (!"D".equals(prevId)) {
                if (bean != null && bean instanceof ConductAction) {
                    ConductAction action = (ConductAction) bean;
                    if (PARAM_DAYS.equals(field.getParameter())) {
                        value = action.getActionPenaltyTime();
                        if (value != null && ((BigDecimal) value).doubleValue() <= 0.0d) {
                            value = null;
                        }
                    } else if (PARAM_CODE.equals(field.getParameter())) {
                        String code = action.getActionCode();
                        if (!StringUtils.isEmpty(code)) {
                            value = data.lookupReferenceCodeByBeanPath(ConductAction.class,
                                    ConductAction.COL_ACTION_CODE, code,
                                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        }
                    }
                }
            }
            if (PARAM_CODE.equals(field.getParameter()) && value == null) {
                value = "NONE";
            }
            return value;
        }
    }

    /**
     * Retrieve counts of victim types from calculated maps.
     */
    protected static class RetrieveVictimCounts implements FieldRetriever {
        protected static final String RETRIEVER_ID = "victims";

        private static final String PARAM_STUDENT = "student";
        private static final String PARAM_STAFF = "staff";
        private static final String PARAM_ADULT = "adult";
        private static final String PARAM_UNKNOWN = "unknown";
        private static final String PARAM_INDETERMINATE = "indeterminate";

        /**
         * Retrieve the calculated victim count values from the Entity calculated maps.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         *
         * @return Object
         *
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            VAStudentBehaviorAR sbarData = (VAStudentBehaviorAR) data;
            String param = (String) field.getParameter();
            Object value = null;
            if (PARAM_STUDENT.equals(param)) {
                value = Integer.valueOf(sbarData.m_victimCounts[0]);
            } else if (PARAM_STAFF.equals(param)) {
                value = Integer.valueOf(sbarData.m_victimCounts[1]);
            } else if (PARAM_ADULT.equals(param)) {
                value = Integer.valueOf(sbarData.m_victimCounts[2]);
            } else if (PARAM_UNKNOWN.equals(param)) {
                value = Integer.valueOf(sbarData.m_victimCounts[3]);
            } else if (PARAM_INDETERMINATE.equals(param)) {
                value = Integer.valueOf(sbarData.m_victimCounts[4]);
            }
            return value;
        }
    }

    /**
     * Validate incident
     *
     * @author Follett Software Company
     * @copyright 2021
     */
    protected class ValidateIncident implements FieldValidator {
        public static final String VALIDATOR_ID = "incident";

        private Set<String> m_firearmCountRequired;
        private Set<String> m_weaponCountRequired;

        public ValidateIncident() {
            m_firearmCountRequired = new HashSet<String>();
            m_firearmCountRequired.add("PD1");
            m_firearmCountRequired.add("PD11");
            m_firearmCountRequired.add("PD12");
            m_firearmCountRequired.add("PD15");

            m_weaponCountRequired = new HashSet<String>();
            m_weaponCountRequired.add("BSC24");
            m_weaponCountRequired.add("PD2");
            m_weaponCountRequired.add("PD13");
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {

            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(2);
            VAStudentBehaviorAREntity sbarEntity = (VAStudentBehaviorAREntity) entity;
            Collection<String> offenseCodes = sbarEntity.getOffenseCodes();

            String firearmCountStr = entity.getFieldValue(FIELD_B_FIREARM_COUNT);
            String weaponCountStr = entity.getFieldValue(FIELD_B_WEAPON_COUNT);
            int firearmCount = StringUtils.isEmpty(firearmCountStr) ? 0 : Integer.parseInt(firearmCountStr);
            int weaponCount = StringUtils.isEmpty(weaponCountStr) ? 0 : Integer.parseInt(weaponCountStr);

            if (CollectionUtils.intersects(offenseCodes, m_firearmCountRequired) && firearmCount < 1) {
                errors.add(new StateReportValidationError(entity, field,
                        "Behavior requires a count of firearms confiscated.",
                        "Offense code = " + STYLE_BOLD + offenseCodes.toString() + STYLE_END));
            }
            if (CollectionUtils.intersects(offenseCodes, m_weaponCountRequired) && weaponCount < 1) {
                errors.add(new StateReportValidationError(entity, field,
                        "Behavior requires a count of weapons confiscated.",
                        "Offense code = " + STYLE_BOLD + offenseCodes.toString() + STYLE_END));
            }
            return errors;
        }
    }

    /**
     * Validate offense code.
     *
     * 1. Incidents with the following offense codes
     * MUST have more than 1 offense code:
     *
     * A1T (except for action code 1 or 4)
     * BA1
     * BA3
     *
     * 2. Incidents with the following offense codes
     * MUST have more than one offender:
     *
     * FA2
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateOffense implements FieldValidator {
        public static final String VALIDATOR_ID = "offense";

        private Map<String, Integer> m_reportToLawRequired;
        private Set<String> m_victimCountRequired;
        private Set<String> m_AuthCodeRequired;

        public ValidateOffense() {
            // Integer lawConditional = Integer.valueOf(1);
            Integer lawRequired = Integer.valueOf(2);
            m_reportToLawRequired = new HashMap<String, Integer>();
            // listed on Behavior codes page as required.
            m_reportToLawRequired.put("PD1", lawRequired);
            m_reportToLawRequired.put("PD2", lawRequired);
            m_reportToLawRequired.put("PD3", lawRequired);
            m_reportToLawRequired.put("PD4", lawRequired);
            m_reportToLawRequired.put("PD5", lawRequired);
            m_reportToLawRequired.put("PD6", lawRequired);
            m_reportToLawRequired.put("PD7", lawRequired);
            m_reportToLawRequired.put("PD8", lawRequired);
            m_reportToLawRequired.put("PD9", lawRequired);
            m_reportToLawRequired.put("PD10", lawRequired);
            m_reportToLawRequired.put("PD11", lawRequired);
            m_reportToLawRequired.put("PD12", lawRequired);
            m_reportToLawRequired.put("PD13", lawRequired);
            m_reportToLawRequired.put("PD14", lawRequired);
            m_reportToLawRequired.put("PD15", lawRequired);
            m_reportToLawRequired.put("PD16", lawRequired);
            // listed on Row C field Law Enforcement as required.
            m_reportToLawRequired.put("BSC1", lawRequired);
            m_reportToLawRequired.put("BSC2", lawRequired);
            m_reportToLawRequired.put("BSC18", lawRequired);
            m_reportToLawRequired.put("BESO2", lawRequired);
            m_reportToLawRequired.put("BESO5", lawRequired);
            m_reportToLawRequired.put("BESO7", lawRequired);
            m_reportToLawRequired.put("BESO8", lawRequired);
            m_reportToLawRequired.put("BESO14", lawRequired);
            m_reportToLawRequired.put("BESO15", lawRequired);
            m_reportToLawRequired.put("BESO16", lawRequired);

            m_victimCountRequired = new HashSet<String>();
            m_victimCountRequired.add("BSO10");
            m_victimCountRequired.add("RB1");
            m_victimCountRequired.add("RB2");
            m_victimCountRequired.add("RB5");
            m_victimCountRequired.add("RB9");
            m_victimCountRequired.add("BSC6");
            m_victimCountRequired.add("BSC7");
            m_victimCountRequired.add("BSC18");
            m_victimCountRequired.add("BSC19");
            m_victimCountRequired.add("BSC21");
            m_victimCountRequired.add("BSC22");
            m_victimCountRequired.add("BESO1");
            m_victimCountRequired.add("BESO2");
            m_victimCountRequired.add("BESO4");
            m_victimCountRequired.add("BESO11");
            m_victimCountRequired.add("BESO12");
            m_victimCountRequired.add("BESO13");
            m_victimCountRequired.add("BESO15");
            m_victimCountRequired.add("BESO16");
            m_victimCountRequired.add("BESO17");
            m_victimCountRequired.add("PD1");
            m_victimCountRequired.add("PD2");
            m_victimCountRequired.add("PD3");
            m_victimCountRequired.add("PD4");
            m_victimCountRequired.add("PD5");
            m_victimCountRequired.add("PD6");
            m_victimCountRequired.add("PD7");
            m_victimCountRequired.add("PD8");
            m_victimCountRequired.add("PD9");
            m_victimCountRequired.add("PD10");

            m_AuthCodeRequired = new HashSet<String>();
            m_AuthCodeRequired.add("BAPOTH");
            m_AuthCodeRequired.add("BSOOTH");
            m_AuthCodeRequired.add("RBOTH");
            m_AuthCodeRequired.add("BSCOTH");
            m_AuthCodeRequired.add("BESOOTH");

        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(2);

            // Report To Law Enformcement required.
            String reportToLaw = entity.getFieldValue(FIELD_C_LAW_ENFORCEMENT);
            if (m_reportToLawRequired.containsKey(value) && !"Y".equals(reportToLaw)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Behavior requires the incident is reported to law enformcement.",
                        "Offense code = " + STYLE_BOLD + value + STYLE_END));
            }

            // Victim count is required.
            int victimCount = getVictimCount();
            if (m_victimCountRequired.contains(value) && victimCount == 0) {
                errors.add(new StateReportValidationError(entity, field,
                        "Behavior requires a victim count or unknown victims indication.",
                        "Offense code = " + STYLE_BOLD + value + STYLE_END));
            }

            String authCode = entity.getFieldValue(FIELD_C_AUTH_CODE);
            if (m_AuthCodeRequired.contains(value) && StringUtils.isEmpty(authCode)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Behavior requires a DOE Authorization code.",
                        "Offense code = " + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }

        /**
         * return the total victim count for the incident.
         *
         * @return int
         */
        private int getVictimCount() {
            int sum = 0;
            for (int i = 0; i < 5; i++) {
                sum += m_victimCounts[i];
            }
            return sum;
        }
    }


    // Tool Input Parameters.
    protected static final String ALIAS_VICTIM_TYPE = "cnd-uda-type";
    protected static final String ALIAS_VICTIM_NAME = "cnd-uda-name";
    protected static final String ALIAS_INDETERMINATE_VICTIM = "all-cnd-IndeterminateVictimCount";
    protected static final String ALIAS_STUDENT_VICTIM = "all-cnd-VictimCountStudent";
    protected static final String ALIAS_STAFF_VICTIM = "all-cnd-VictimCountStaff";
    protected static final String ALIAS_ADULT_VICTIM = "all-cnd-VictimCountOtherAdult";
    protected static final String ALIAS_OTHER_VICTIM = "all-cnd-VictimCountOtherUnknown";
    protected static final String ALIAS_CND_DOE_AUTH = "all-cnd-doeAuth";
    protected static final String ALIAS_CND_UNKNOWN_OFFENDER = "all-cnd-doeUnknownOffender";
    protected static final String ALIAS_CND_AGGREV = "all-cnd-AggravatingCircumstances";
    protected static final String ALIAS_CND_INCIDENT_DISTRICT = "DOE INCIDENT DISTRICT";
    protected static final String ALIAS_CND_INCIDENT_SCHOOL = "DOE INCIDENT SCHOOL";
    protected static final String ALIAS_CND_ENROLL_DISTRICT = "DOE ENROLLED DISTRICT";
    protected static final String ALIAS_CND_ENROLL_SCHOOL = "DOE ENROLLED SCHOOL";
    protected static final String ALIAS_CND_LAW_ENFORCE = "all-cnd-LawEnforcementChargesFiled";
    protected static final String ALIAS_CNO_DOE_AUTH = "all-cno-doeAuth";
    protected static final String ALIAS_CNO_UNKNOWN_OFFENDER = "all-cno-doeUnknownOffender";
    protected static final String ALIAS_CNO_AGGREV = "all-cno-AggravatingCircumstances";
    protected static final String ALIAS_CNO_LAW_ENFORCE = "all-cno-lawEnforcement";
    protected static final String ALIAS_ACT_INTERVENTION = "all-act-BehaviorIntervention";
    protected static final String ALIAS_ACT_INSTRUCTIONAL = "all-act-instructionalSupports";
    protected static final String ALIAS_SKL_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL ID";

    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";

    protected static final String FIELD_B_FIREARM_COUNT = "Firearms Confiscated";
    protected static final String FIELD_B_WEAPON_COUNT = "Non-firearm Weapons Confiscated";

    protected static final String FIELD_C_LAW_ENFORCEMENT = "Law Enforcement";
    protected static final String FIELD_C_AUTH_CODE = "Authorization Code";


    protected static final char CHAR_TAB = '\t';
    protected static final String DICTIONARY_ID_UDA = "CND-SBAR";
    protected static final String PARAM_FULL_YEAR = "fullYear";
    protected static final String PARAM_LIMIT_OFFENSES = "limitOffenses";
    protected static final String PARAM_HEADERS = "headers";
    protected static final List<String> FLAG_FEDERAL_DCV = Arrays.asList("DCV", "SBAR");

    protected DataDictionary m_udaDictionary;
    protected Map<String, Integer> m_incidentCountsById;
    protected String m_currentIncidentId;
    protected int m_currentIncidentCount;
    protected String m_fieldIntervention;
    protected String m_fieldInstructional;
    protected String m_fieldDistrictId;
    protected String m_fieldSchoolId;
    protected int[] m_victimCounts;
    protected Set<String> m_victimEntries;
    protected Collection<ReferenceCode> m_victimTypeCodes;
    protected Collection<ReferenceCode> m_districtCodes;
    protected Collection<ReferenceCode> m_schoolCodes;

    /**
     * Construct the A row as the heading.
     * It will be generated once at the top of the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        Boolean fullYear = (Boolean) getParameter(PARAM_FULL_YEAR);
        Boolean header = (Boolean) getParameter(PARAM_HEADERS);
        StringBuilder heading = new StringBuilder(16);
        if (header != null && header.booleanValue()) {
            heading.append(super.getHeading());
        }
        heading.append("A");
        if (fullYear != null && fullYear.booleanValue()) {
            heading.append("03");
        } else {
            heading.append("17");
        }
        heading.append(Integer.toString(getCurrentContext().getSchoolYear() - 1));
        heading.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictId));
        heading.append(System.lineSeparator());
        return heading.toString();
    }

    /**
     * Initialize,
     * Query across StudentConductIncident as primary iteration.
     *
     * @throws X2BaseException
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();
        lookupExtendedDictionary();

        m_fieldDistrictId = translateAliasToJavaName(ALIAS_SKL_DISTRICT_ID, true);
        m_fieldSchoolId = translateAliasToJavaName(ALIAS_SKL_STATE_ID, true);
        m_fieldIntervention = translateAliasToJavaName(ALIAS_ACT_INTERVENTION, true);
        m_fieldInstructional = translateAliasToJavaName(ALIAS_ACT_INSTRUCTIONAL, true);

        X2Criteria criteria = getIncidentCriteria();
        BeanQuery query = new BeanQuery(ConductIncident.class, criteria, true);
        query.addOrderBy(ConductIncident.COL_INCIDENT_ID, true);
        query.addOrderBy(X2BaseBean.COL_OID, true);
        query.setFetchSize(2048);
        setQuery(query);
        setEntityClass(VAStudentBehaviorAREntity.class);

        loadCountByIdMap();
        loadRefMaps();
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveVictimCounts.RETRIEVER_ID, new RetrieveVictimCounts());
        calcs.put(RetrieveDaysSanctioned.RETRIEVER_ID, new RetrieveDaysSanctioned());
        calcs.put(RetrieveIncidentEAID.RETRIEVER_ID, new RetrieveIncidentEAID());
        calcs.put(RetrieveEnrollEAID.RETRIEVER_ID, new RetrieveEnrollEAID());
        calcs.put(RetrieveActionElement.RETRIEVER_ID, new RetrieveActionElement());
        super.addCalcs(calcs);

        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(ValidateIncident.VALIDATOR_ID, new ValidateIncident());
        validators.put(ValidateOffense.VALIDATOR_ID, new ValidateOffense());
        super.addValidators(validators);

    }

    /**
     * Construct the criteria for selecting conduct incidents.
     *
     * @return X2Criteria
     */
    private X2Criteria getIncidentCriteria() {
        String excludeField = translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);

        X2Criteria incidentCriteria = new X2Criteria();
        incidentCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getStartDate());
        incidentCriteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getEndDate());
        incidentCriteria.addNotEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                Boolean.TRUE);
        if (excludeField != null) {
            incidentCriteria.addNotEqualTo(ConductIncident.REL_STUDENT + PATH_DELIMITER +
                    excludeField, BooleanAsStringConverter.TRUE);
        }

        if (isSchoolContext()) {
            School school = getSchool();
            incidentCriteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, school.getOid());
        }

        // If the user selects to limit the offenses, identify reportable offenses and actions.
        Boolean objLimitOffenses = (Boolean) getParameter(PARAM_LIMIT_OFFENSES);
        if (objLimitOffenses != null && objLimitOffenses.booleanValue()) {
            String offenseRefTblOid = null;
            String actionRefTblOid = null;

            // 1. Find the reference code tables for incidents and actions.
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(ConductIncident.class.getName(),
                    ConductIncident.COL_INCIDENT_CODE);
            if (field != null) {
                offenseRefTblOid = field.getReferenceTableOid();
            }
            field = dictionary.findDataDictionaryField(ConductAction.class.getName(), ConductAction.COL_ACTION_CODE);
            if (field != null) {
                actionRefTblOid = field.getReferenceTableOid();
            }

            // 2. Load lists of reportable codes (codes that contain "DCV" in the Federal column).
            List<String> incidentCodes = new ArrayList<String>();
            List<String> actionCodes = new ArrayList<String>();
            if (!StringUtils.isEmpty(offenseRefTblOid)) {
                Criteria criteria = new Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, offenseRefTblOid);
                criteria.addIn(ReferenceCode.COL_FEDERAL_CODE, FLAG_FEDERAL_DCV);
                ReportQueryByCriteria query =
                        new ReportQueryByCriteria(ReferenceCode.class, new String[] {ReferenceCode.COL_CODE}, criteria);
                ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        Object[] code = (Object[]) iterator.next();
                        incidentCodes.add((String) code[0]);
                    }
                } finally {
                    iterator.close();
                }
            }

            if (!StringUtils.isEmpty(actionRefTblOid)) {
                Criteria criteria = new Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, actionRefTblOid);
                criteria.addIn(ReferenceCode.COL_FEDERAL_CODE, FLAG_FEDERAL_DCV);
                ReportQueryByCriteria query =
                        new ReportQueryByCriteria(ReferenceCode.class, new String[] {ReferenceCode.COL_CODE}, criteria);
                ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        Object[] code = (Object[]) iterator.next();
                        actionCodes.add((String) code[0]);
                    }
                } finally {
                    iterator.close();
                }
            }

            // 3. Add criteria for reportable incident codes or reportable action codes.
            Criteria codeCriteria = new Criteria();
            Criteria codeOCriteria = new Criteria();
            Criteria codeACriteria = new Criteria();
            if (incidentCodes.size() > 0 && actionCodes.size() > 0) {
                // Not using offense sub table, check each of three code fields individually.
                codeCriteria.addIn(ConductIncident.COL_INCIDENT_CODE, incidentCodes);
                codeOCriteria.addIn(ConductIncident.REL_CONDUCT_OFFENSES + PATH_DELIMITER +
                        ConductOffense.COL_INCIDENT_CODE, incidentCodes);
                codeACriteria.addIn(ConductIncident.REL_CONDUCT_ACTIONS + PATH_DELIMITER +
                        ConductAction.COL_ACTION_CODE, actionCodes);
                codeCriteria.addOrCriteria(codeACriteria);
                codeCriteria.addOrCriteria(codeOCriteria);
                incidentCriteria.addAndCriteria(codeCriteria);
            }
        }


        applyInputCriteria(incidentCriteria, false, ConductIncident.REL_STUDENT);
        return incidentCriteria;
    }

    /**
     * Build a map by INCIDENT_ID of row counts greater than 1.
     * This identifies all multi student incidents and gives us a count of
     * students in the incident.
     * The entity can use this to decide when to add the E record for victim counts.
     */
    private void loadCountByIdMap() {
        String[] columns = new String[2];
        columns[0] = ConductIncident.COL_INCIDENT_ID;
        columns[1] = "count(*)";
        X2Criteria criteria = getIncidentCriteria();
        ColumnQuery idQuery = new ColumnQuery(ConductIncident.class, columns, criteria);
        idQuery.addGroupBy(ConductIncident.COL_INCIDENT_ID);
        X2Criteria havingCriteria = new X2Criteria();
        havingCriteria.addGreaterThan("count(*)", Integer.valueOf(1));
        idQuery.setHavingCriteria(havingCriteria);
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(idQuery);
        m_incidentCountsById = new HashMap<String, Integer>();
        while (iterator.hasNext()) {
            Object[] row = (Object[]) iterator.next();
            m_incidentCountsById.put((String) row[0], (Integer) row[1]);
        }
    }

    /**
     * Load maps of reference codes for translation.
     */
    private void loadRefMaps() {
        DataDictionaryField field = m_udaDictionary.findDataDictionaryFieldByAlias(ALIAS_VICTIM_TYPE);
        if (field != null) {
            ReferenceTable table = field.getReferenceTable();
            if (table != null) {
                m_victimTypeCodes = table.getReferenceCodes(getBroker());
            }
        }
        field = m_udaDictionary.findDataDictionaryFieldByAlias(ALIAS_CND_INCIDENT_DISTRICT);
        if (field != null) {
            ReferenceTable table = field.getReferenceTable();
            if (table != null) {
                m_districtCodes = table.getReferenceCodes(getBroker());
            }
        }
        field = m_udaDictionary.findDataDictionaryFieldByAlias(ALIAS_CND_INCIDENT_SCHOOL);
        if (field != null) {
            ReferenceTable table = field.getReferenceTable();
            if (table != null) {
                m_schoolCodes = table.getReferenceCodes(getBroker());
            }
        }
    }

    /**
     * Lookup the Extended Dictionary for the UDA victims table.
     */
    private void lookupExtendedDictionary() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, DICTIONARY_ID_UDA);
        BeanQuery query = new BeanQuery(ExtendedDataDictionary.class, criteria);
        ExtendedDataDictionary ddx = getBroker().getBeanByQuery(query);
        if (ddx != null) {
            m_udaDictionary = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());
        }
    }
}
