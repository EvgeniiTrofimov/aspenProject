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
package com.x2dev.procedures.statereporting.va;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.ConductOffense;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * This class implements the data export for VA's Discipline, Crime, and Violence (DCV) Data
 * Collection.
 *
 * @author X2 Development Corporation
 */
public class DisciplineCollection extends StateReportData {
    @SuppressWarnings("unused")
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Entity class for VA's Discipline, Crime, and Violence (DCV) Data Collection export.
     *
     * @author X2 Development Corporation
     */
    public static class DisciplineEntity extends StateReportEntity {
        /**
         * The ConductIncident's ConductAction
         *
         */
        protected ConductAction m_action;

        /**
         * The ConductIncident's ConductOffense(s).
         *
         * Up to three offenses per incident may be used.
         * The first offense listed is considered the primary offense.
         */
        protected List<ConductOffense> m_offenses;

        /**
         * The ConductIncident's student's enrollment(s)
         * Used for retrieving the student's district and school ID
         */
        private Collection<StudentEnrollment> m_enrollments;

        /**
         * The ConductIncident's Student's recent ENTRY enrollment
         */
        private StudentEnrollment m_recentEntryEnrollment;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public DisciplineEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Get the action state code from this entity.
         *
         * @return the action's state code
         */
        public String getActionCode() {
            String value = null;
            DisciplineCollection dc = (DisciplineCollection) getData();
            if (m_action != null) {
                String actionCode = m_action.getActionCode();
                ReferenceCode refCode = dc.m_actionCodesMap.get(actionCode);
                if (refCode != null) {
                    value = refCode.getStateCode();
                }
            }
            return value;
        }

        /**
         * Get the number of suspension/penalty days (rounded up).
         *
         * @return total penalty time (rounded up) for all actions for this conduct
         */
        public Integer getDaysPunished() {
            int days = 0;
            BigDecimal penaltyTime = m_action.getActionPenaltyTime();
            if (penaltyTime != null) {
                days = penaltyTime.setScale(0, RoundingMode.CEILING).intValue();
            }
            return Integer.valueOf(days);
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            ConductIncident incident = (ConductIncident) getBean();
            String schoolName = incident.getSchool().getName();
            String name = "";
            if (incident.getStudent() != null) {
                name += "School Name: " + schoolName + ", Student: " + incident.getStudent().getNameView() +
                        " [Local ID: " + incident.getStudent().getLocalId() +
                        ", State ID: " + incident.getStudent().getStateId() +
                        "] ";
            }
            name += "Incident ID:" + incident.getIncidentId();

            return name;
        }

        /**
         * Get the student's most recent StudentEnrollment (used for retrieving division ID and
         * school ID).
         *
         * @return student's most recent StudentEnrollment record
         */
        public StudentEnrollment getRecentEnrollment() {
            if (m_recentEntryEnrollment == null) {
                ConductIncident incident = (ConductIncident) getBean();
                PlainDate incidentDate = incident.getIncidentDate();
                for (StudentEnrollment enrollment : m_enrollments) {
                    if (enrollment.getEnrollmentType().equals(StudentEnrollment.ENTRY) &&
                            incidentDate != null && enrollment.getEnrollmentDate().before(incidentDate)) {
                        m_recentEntryEnrollment = enrollment;
                        break;
                    }
                }
            }
            return m_recentEntryEnrollment;
        }

        /**
         * Initialize.
         *
         * 1. Get and set the incident's conduct action
         * 2. Get and set the incident's conduct offenses
         * 2. Get and set the incident's student's enrollments
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean)
                throws X2BaseException {
            super.intitialize(data, bean);

            DisciplineCollection dc = (DisciplineCollection) data;
            ConductIncident incident = (ConductIncident) bean;

            // m_action = dc.m_actionsMap.get(incident.getOid());
            List<ConductAction> actions = dc.m_actionsMap.get(incident.getOid());

            // if there are 2 or more primary actions, log this as a validation error
            if (actions == null) {
                m_action = new ConductAction(dc.getBroker().getPersistenceKey());
            } else if (actions.size() == 1) {
                m_action = actions.get(0);
            } else {
                int primaryActionsCount = 0;
                for (ConductAction action : actions) {
                    String isPrimary = (String) WebUtils.getProperty(action, dc.m_doeIsPrimaryAction);
                    if (BooleanAsStringConverter.TRUE.equals(isPrimary)) {
                        m_action = action;
                        primaryActionsCount++;
                    }
                }
                if (primaryActionsCount != 1) {
                    // This error never came out to report, because setRowCount function overrides
                    // array with all retrieve errors
                    FieldDefinition fieldDefinition = data.getFieldDefinition(FIELD_ACTION_CODE);
                    StateReportValidationError error = new StateReportValidationError(this, fieldDefinition,
                            "Incident must have only 1 primary action",
                            "Incident ID = " + STYLE_BOLD + incident.getIncidentId() + STYLE_END +
                                    ", number of primary actions = " + STYLE_BOLD + primaryActionsCount + STYLE_END);
                    this.addRetrievalError(FIELD_ACTION_CODE, error);
                }
            }

            // ignore this entity if this incident has an offense code in which if its sanction code
            // was 99 that should not be reported
            List<String> incidentStateCodes = getIncidentStateCodes(dc, incident);
            String offenseCode1 = incidentStateCodes.size() > 0 ? incidentStateCodes.get(0) : "";
            String offenseCode2 = incidentStateCodes.size() > 1 ? incidentStateCodes.get(1) : "";
            String offenseCode3 = incidentStateCodes.size() > 2 ? incidentStateCodes.get(2) : "";
            String actionCode = "";
            if (!incidentStateCodes.isEmpty()) {
                actionCode = getActionCode();
                if (actionCode == null) {
                    actionCode = "";
                }
            }

            // Offense codes that should not be reported when the action is 99.
            if (offenseCode1.matches(REGEX_NO_SANCTION_99) &&
                    offenseCode2.matches(REGEX_NO_SANCTION_99) &&
                    offenseCode3.matches(REGEX_NO_SANCTION_99) &&
                    OTHER_ACTION_STATE_CODE.equals(actionCode)) {
                if (dc.m_limitOffenses.booleanValue()) {
                    setRowCount(0);
                }
            }
            // Offense codes that should only be reported when the action is 01 or 04.
            else if (offenseCode1.matches(REGEX_ONLY_SANCTION_01_04) &&
                    offenseCode2.matches(REGEX_ONLY_SANCTION_01_04) &&
                    offenseCode3.matches(REGEX_ONLY_SANCTION_01_04) &&
                    !actionCode.matches(REGEX_SANCTION_01_04)) {
                if (dc.m_limitOffenses.booleanValue()) {
                    setRowCount(0);
                }
            } else {
                Student student = incident.getStudent();
                m_offenses = (List<ConductOffense>) dc.m_offensesLookupMap.get(incident.getOid());
                if (m_offenses == null) {
                    m_offenses = new ArrayList<ConductOffense>();
                }

                m_enrollments = dc.m_enrollmentMap.get(student.getOid());
            }
        }

        /**
         * @param dc
         * @param incident
         * @return
         */
        private List<String> getIncidentStateCodes(DisciplineCollection dc, ConductIncident incident) {
            String codes[] = {incident.getIncidentCode(), (String) incident.getFieldValueByBeanPath(dc.m_fieldOffense2),
                    (String) incident.getFieldValueByBeanPath(dc.m_fieldOffense3)};
            List<String> stateCodes = new ArrayList(codes.length);
            for (int i = 0; i < codes.length; ++i) {
                ReferenceCode refCode = dc.m_offenseCodesMap.get(codes[i]);
                if (refCode != null && !StringUtils.isEmpty(refCode.getStateCode())) {
                    stateCodes.add(refCode.getStateCode());
                }
            }
            return stateCodes;
        }
    }

    /**
     * Retrieves (1) the incident's action code (param: "CODE"), or
     * (2) the incident's suspension days (param: "DAYS"), or
     * (3) whether the incident resulted in expulsion (param: "EXPULSION").
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveAction implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            DisciplineCollection dcData = (DisciplineCollection) data;
            DisciplineEntity de = (DisciplineEntity) entity;
            ConductIncident incident = (ConductIncident) entity.getBean();
            List<ConductAction> actions = dcData.m_actionsMap.get(incident.getOid());
            String actionCode = de.getActionCode();
            String param = (String) field.getParameter();
            Object value = null;
            if (PARAM_ACTION_CODE.equals(param)) {
                if (StringUtils.isEmpty(actionCode)) {
                    int primaryActionsCount = 0;
                    if (actions == null || actions.isEmpty()) {
                        StateReportValidationError error = new StateReportValidationError(entity, field,
                                "Incident have no assigned actions",
                                "Incident ID = " + STYLE_BOLD + incident.getIncidentId() + STYLE_END);
                        entity.addRetrievalError(field.getFieldId(), error);
                    } else {
                        for (ConductAction action : actions) {
                            String isPrimary = (String) WebUtils.getProperty(action, dcData.m_doeIsPrimaryAction);
                            if (BooleanAsStringConverter.TRUE.equals(isPrimary)) {
                                primaryActionsCount++;
                            }
                        }
                        if (primaryActionsCount < 1) {
                            StateReportValidationError error = new StateReportValidationError(entity, field,
                                    "Incident must have at least 1 primary action",
                                    "Incident ID = " + STYLE_BOLD + incident.getIncidentId() + STYLE_END +
                                            ", number of assigned actions = " + STYLE_BOLD + actions.size()
                                            + STYLE_END);
                            entity.addRetrievalError(field.getFieldId(), error);
                        } else if (primaryActionsCount > 1) {
                            StateReportValidationError error = new StateReportValidationError(entity, field,
                                    "Incident must have only 1 primary action",
                                    "Incident ID = " + STYLE_BOLD + incident.getIncidentId() + STYLE_END +
                                            ", number of primary actions = " + STYLE_BOLD + primaryActionsCount
                                            + STYLE_END);
                            entity.addRetrievalError(field.getFieldId(), error);
                        }
                    }
                } else {
                    value = de.getActionCode();
                }
            } else if (PARAM_ACTION_DAYS.equals(param)) {
                if (actions == null || actions.isEmpty()) {
                    StateReportValidationError error = new StateReportValidationError(entity, field,
                            "Incident have no assigned actions",
                            "Incident ID = " + STYLE_BOLD + incident.getIncidentId() + STYLE_END);
                    entity.addRetrievalError(field.getFieldId(), error);
                } else {
                    if (!StringUtils.isEmpty(actionCode)) {
                        if (OTHER_ACTION_STATE_CODE.equals(actionCode)) {
                            value = Integer.valueOf(0);
                        } else {
                            value = de.getDaysPunished();
                        }
                    } else {
                        StateReportValidationError error = new StateReportValidationError(entity, field,
                                field.getFieldId()
                                        + " cannot be determine, because Action not specified or incident has multiple actions without primary one.",
                                "Incident ID = " + STYLE_BOLD + incident.getIncidentId() + STYLE_END +
                                        ", number of assigned actions = " + STYLE_BOLD + actions.size() + STYLE_END);
                        entity.addRetrievalError(field.getFieldId(), error);
                    }
                }
            } else if (PARAM_ACTION_EXPULSION.equals(param)) {
                if (actions == null || actions.isEmpty()) {
                    StateReportValidationError error = new StateReportValidationError(entity, field,
                            "Incident have no assigned actions",
                            "Incident ID = " + STYLE_BOLD + incident.getIncidentId() + STYLE_END);
                    entity.addRetrievalError(field.getFieldId(), error);
                } else {
                    if (!StringUtils.isEmpty(actionCode)) {
                        Boolean expelled = Boolean.FALSE;
                        if (!StringUtils.isEmpty(actionCode) && actionCode.equals(EXPULSION_STATE_CODE)) {
                            expelled = Boolean.TRUE;
                        }
                        value = expelled;
                    } else {
                        StateReportValidationError error = new StateReportValidationError(entity, field,
                                field.getFieldId()
                                        + " cannot be determine, because Action not specified or incident has multiple actions without primary one.",
                                "Incident ID = " + STYLE_BOLD + incident.getIncidentId() + STYLE_END +
                                        ", number of assigned actions = " + STYLE_BOLD + actions.size() + STYLE_END);
                        entity.addRetrievalError(field.getFieldId(), error);
                    }
                }
                if (value == null) {
                    value = Boolean.FALSE;
                }
            }
            return value;
        }
    }

    /**
     * Return "Y" if the primary action (m_action) has the "all-act-AggravatingCircumstances"
     * aliased field set as true
     * Else, use default value from export format of "N".
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveAggrCircumstances implements FieldRetriever {

        protected static final String CALC_ID = "AGGR-CIRCUM";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            DisciplineEntity de = (DisciplineEntity) entity;
            String value = "N";
            if (de.m_action != null) {
                if (BooleanAsStringConverter.TRUE
                        .equals(de.m_action.getFieldValueByBeanPath(m_actAggravatingCircumstances))) {
                    value = "Y";
                }
            }
            return value;
        }
    }

    /**
     * Retrieve the associated conduct actions and see if any have a 'perminant placement change'.
     * If so, report yes.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrievePerminantChange implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Boolean perminantChange = Boolean.FALSE;
            ConductIncident incident = (ConductIncident) entity.getBean();
            for (ConductAction action : incident.getConductActions()) {
                if (BooleanAsStringConverter.TRUE.equals(action.getFieldValueByAlias("DOE PERM CHANGE"))) {
                    perminantChange = Boolean.TRUE;
                    break;
                }
            }
            return (perminantChange.booleanValue()) ? "Y" : "N";
        }

    }
    /**
     * Retrieve the VA specific race code.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveRace implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String personOid = (String) data.getProperty(entity.getBean(), field.getBeanPath());

            int totalRace = 0;
            Collection<Race> races = m_raceCodeMap.get(personOid);
            if (races != null) {
                for (Race race : races) {
                    ReferenceCode code = m_raceCodes.get(race.getRaceCode());
                    if (code != null) {
                        int codeNum = 0;
                        try {
                            codeNum = Integer.parseInt(code.getStateCode());
                        } catch (NumberFormatException nfe) {
                            // not parsable, ignore.
                        }
                        totalRace += codeNum;
                    }
                }
            }
            if (totalRace > 0 && totalRace < 32) {
                totalRace = m_raceValueTranslator[totalRace];
            } else {
                totalRace = 0;
            }

            return Integer.valueOf(totalRace);
        }
    }

    /**
     * Get the ConductIncident's primary (1), second (2), and third (3) offense
     * and return its state code.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveOffense implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {

            String result = null;
            DisciplineCollection dc = (DisciplineCollection) data;
            DisciplineEntity de = (DisciplineEntity) entity;
            String param = (String) field.getParameter();
            int index = Integer.parseInt(param) - 1;
            if (0 <= index && index < de.m_offenses.size()) {
                ConductOffense offense = de.m_offenses.get(index);
                String incidentCode = offense.getIncidentCode();
                if (incidentCode != null) {
                    ReferenceCode refCode = dc.m_offenseCodesMap.get(incidentCode);
                    if (refCode != null) {
                        String stateCode = refCode.getStateCode();
                        result = stateCode;
                    }
                }
            }

            return result;
        }

    }

    /**
     * Retrieve the student's enrolled district or school from the StudentEnrollment record
     *
     * Available parameters:
     * - "DOE DISTRICT SERVE"
     * - "DOE SCHOOL SERVE"
     *
     * 1. Check if 'DOE SCHOOL SERVE' or 'DOE DISTRICT SERVE' has a value. Return it, otherwise
     * return...
     * 2a. Go to StudentEnrollment -> School -> School ID for "DOE SCHOOL SERVE"
     * 2b. Go to StudentEnrollment -> School -> Organization 1 -> [DOE DISTRICT ID] for
     * "DOE DISTRICT SERVE"
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveDistrictSchool implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            DisciplineEntity de = (DisciplineEntity) entity;
            ConductIncident incident = (ConductIncident) entity.getBean();
            String param = (String) field.getParameter();
            String result = (String) data.getProperty(entity.getBean(), field.getBeanPath());

            // These 4 calculations are for the "default" values. The export format also contains
            // source fields
            // which function as overrides for these. The state lookup for the district codes is
            // done by the export format
            if (ALIAS_DISTRICT_SERVE.equals(param)) // Student district
            {
                if (StringUtils.isEmpty(result)) {
                    StudentEnrollment enrollment = de.getRecentEnrollment();
                    if (enrollment != null) {
                        result = (String) enrollment.getFieldValueByAlias(ALIAS_DISTRICT_SERVE);
                        if (StringUtils.isEmpty(result)) {
                            result = (String) enrollment.getSchool().getOrganization1()
                                    .getFieldValueByAlias(ALIAS_DISTRICT_ID);
                        }
                    }
                }
            } else if (ALIAS_SCHOOL_SERVE.equals(param)) // Student school
            {
                if (StringUtils.isEmpty(result)) {
                    StudentEnrollment enrollment = de.getRecentEnrollment();
                    if (enrollment != null) {
                        result = (String) enrollment.getFieldValueByAlias(ALIAS_SCHOOL_SERVE);
                        if (!StringUtils.isEmpty(result)) {
                            result = lookupStateValue(StudentEnrollment.class, m_doeSchoolServe, result);
                        } else if (enrollment.getSchool() != null) {
                            result = (String) enrollment.getSchool().getFieldValueByAlias(ALIAS_SCHOOL_ID);
                        }
                    }
                } else {
                    result = lookupStateValue(ConductIncident.class, m_doeEnrolledSchool, result);
                }
            } else if (ALIAS_DISTRICT_ID.equals(param)) // Incident district
            {
                if (StringUtils.isEmpty(result)) {
                    result = (String) incident.getSchool().getOrganization1().getFieldValueByAlias(ALIAS_DISTRICT_ID);
                }
            } else if (ALIAS_SCHOOL_ID.equals(param)) // Incident school
            {
                if (StringUtils.isEmpty(result)) {
                    result = (String) incident.getSchool().getFieldValueByAlias(ALIAS_SCHOOL_ID);
                } else {
                    result = lookupStateValue(ConductIncident.class, m_doeIncidentSchool, result);
                }
            }

            return result;
        }

    }

    /**
     * Return "Y" if the primary action (m_action) has the "all-act-ProvidingEducationServices"
     * aliased field set as true
     * Else, use default value from export format of "N".
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveServicesProvided implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            DisciplineEntity de = (DisciplineEntity) entity;
            Object value = null;
            if (de.m_action != null) {
                if (BooleanAsStringConverter.TRUE.equals(de.m_action.getFieldValueByBeanPath(m_edServicesProvided))) {
                    value = "Y";
                }
            }

            return value;
        }
    }

    /**
     * Validate action code.
     *
     * 1. Incidents with the following offense codes needs to have action codes 4, 5, or 7:
     *
     * DR1 DR4 WP1 WP2 WP4 WP5 WP6 WP7 WP8
     *
     * 2. Incidents with the following offense codes AND has a sanction of 99
     * should not be included in this DCV submission:
     *
     * A1T AR1 BR1 C1M C2M C3M D1C D2C
     * D3C D4C D4G D5C D5G D6C D6G D8C
     * EX1 F1T G1B GA1 H1Z RT1 S1V S2V
     * S3V T1C T2C T3C T4B T4C TH1 TH2
     * TR1 VA1 W3P W8P W9P W1P
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateActionCode implements FieldValidator {

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
            String offenseCode = entity.getFieldValue(FIELD_PRIMARY_OFFENSE_CODE);
            String offenseCode2 = entity.getFieldValue(FIELD_OFFENSE_CODE_2);
            String offenseCode3 = entity.getFieldValue(FIELD_OFFENSE_CODE_3);

            // 1. needs to have action codes 4, 5, 6, 7 or 8
            if (offenseCode.matches(REGEX_WEAPON_OFFENSE_CODES)
                    && !value.matches("04|05|O6|07|08")) {
                errors.add(new StateReportValidationError(entity, field,
                        "Incident must have action code 4, 5, 6, 7, or 8",
                        "Offense code = " + STYLE_BOLD + offenseCode + STYLE_END +
                                ", action code = " + STYLE_BOLD + value + STYLE_END));
            }

            // 2. sanction of 99 should not be in this DCV submission
            if (offenseCode.matches(REGEX_NO_SANCTION_99) &&
                    offenseCode2.matches(REGEX_NO_SANCTION_99) &&
                    offenseCode3.matches(REGEX_NO_SANCTION_99) &&
                    OTHER_ACTION_STATE_CODE.equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Sanction of 99 should not be in this DCV submission",
                        "Offense code = " + STYLE_BOLD + offenseCode + STYLE_END +
                                ", action code = " + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }

    }

    /**
     * Validate days suspended.
     *
     * 1. Incidents with action code 08 must be 0 days
     * 2. Incidents with action code 99 must be 0 days
     * 3. Incidents with action code 02 (short-term suspension) must be between 1 and 10 days
     * 4. Incidents with action code 03 (long-term suspension) must be between 11 and 364 days
     * 5. Incidents with action code 04 (expulsion) must be 365 days
     * 6. Incidents with action code 05 and 06 must be between 1 and 45 days
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateActionDays implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String actionCode = entity.getFieldValue(FIELD_ACTION_CODE);
            int days = Integer.parseInt(value);

            // 1 & 2: incidents with action code 08 or 09 must be 0 days
            if (actionCode.matches("08|09") && days != 0) {
                errors.add(new StateReportValidationError(entity, field,
                        "Incidents with action code 08 or 09 must have 0 suspension days",
                        "Action code = " + STYLE_BOLD + FIELD_ACTION_CODE + STYLE_END +
                                ", suspension days = " + STYLE_BOLD + value + STYLE_END));
            }

            // 3: action code 02 (short-term suspension) must be between 1 and 10 days
            if (actionCode.equals("02") && !(1 <= days && days <= 10)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Incident's, with action code 02, suspension must be between 1 and 10 days",
                        "Action code = " + STYLE_BOLD + FIELD_ACTION_CODE + STYLE_END +
                                ", suspension days = " + STYLE_BOLD + value + STYLE_END));
            }

            // 4: action code 03 (long-term suspension) must be between 11 and 364 days
            if (actionCode.equals("03") && !(11 <= days && days <= 364)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Incident's, with action code 03, suspension must be between 11 and 364 days",
                        "Action code = " + STYLE_BOLD + FIELD_ACTION_CODE + STYLE_END +
                                ", suspension days = " + STYLE_BOLD + value + STYLE_END));
            }

            // 5: action code 04 (expulsion) must be 365 days
            if (actionCode.equals("04") && days != 365) {
                errors.add(new StateReportValidationError(entity, field,
                        "Incident's, with action code 04, suspension must be between 365 days",
                        "Action code = " + STYLE_BOLD + FIELD_ACTION_CODE + STYLE_END +
                                ", suspension days = " + STYLE_BOLD + value + STYLE_END));
            }

            // 6: action code 05 or 06 must be between 1 and 45 days
            if (actionCode.matches("05|06") && !(1 <= days && days <= 45)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Incident's, with action code 05 or 06, suspension must be between 1 and 45 days",
                        "Action code = " + STYLE_BOLD + FIELD_ACTION_CODE + STYLE_END +
                                ", suspension days = " + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }

    }

    /**
     * Validate firearms count.
     *
     * Incidents with the following offense codes
     * MUST have firearm count (cannot be zero):
     *
     * WP1 WP2 WP4 WP8
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateFirearms implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String offenseCode = entity.getFieldValue(FIELD_PRIMARY_OFFENSE_CODE);
            if (offenseCode.matches(REGEX_FIREARMS_OFFENSE_CODES)
                    && Integer.parseInt(value) == 0) {
                errors.add(new StateReportValidationError(entity, field,
                        "Incident requires a firearms count",
                        "Offense code = " + STYLE_BOLD + offenseCode + STYLE_END +
                                ", firearms count = " + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }

    }

    /**
     * Validate offense reported to law.
     *
     * Incidents with the following offense codes needs to be reported to law:
     *
     * AL1, BA1, BA2, BA3, BA4, BA5, BB1, BU2, DR1,
     * DR2, DR3, DR4, DR5, HO1, HO2, HO3, HO4, SB1,
     * SB2, ST1, SX3, SX4, SX5, SX6, SX7, SX8, TI1,
     * WP0, WP1, WP2, WP4, WP5, WP6, WP7, WP8, WP9
     *
     * added 6/20/13 - AS1, AS2, AS3, BK2, BK2, ET1, ET2, RG1, RG2, TF6
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateLawEnforcement implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String offenseCode = entity.getFieldValue(FIELD_PRIMARY_OFFENSE_CODE);
            if (offenseCode.matches(REGEX_REPORT_OFFENSE_CODES)
                    && value.equals("N")) {
                errors.add(new StateReportValidationError(entity, field,
                        "Incident must be reported to law enforcement",
                        "Offense code = " + STYLE_BOLD + offenseCode + STYLE_END +
                                ", reported to law = " + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }

    }

    /**
     * Validate non-firearms count.
     *
     * Incidents with the following offense codes
     * MUST have non-firearms count (cannot be zero):
     *
     * W1P W2P W3P W8P W9P WP0
     * WP5 WP6 WP7 WP9 WS1 WT1
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateNonFirearms implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String offenseCode = entity.getFieldValue(FIELD_PRIMARY_OFFENSE_CODE);
            if (offenseCode.matches(REGEX_NON_FIREARMS_OFFENSE_CODES)
                    && Integer.parseInt(value) == 0) {
                errors.add(new StateReportValidationError(entity, field,
                        "Incident requires a non-firearms count",
                        "Offense code = " + STYLE_BOLD + offenseCode + STYLE_END +
                                ", non-firearms count = " + STYLE_BOLD + value + STYLE_END));
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
            String offenseCode = entity.getFieldValue(FIELD_PRIMARY_OFFENSE_CODE);
            String offenseCode2 = entity.getFieldValue(FIELD_OFFENSE_CODE_2);
            String offenseCode3 = entity.getFieldValue(FIELD_OFFENSE_CODE_3);
            DisciplineEntity de = (DisciplineEntity) entity;
            ConductIncident incident = (ConductIncident) de.getBean();

            // 1. have more than one offense code
            if (offenseCode.matches("A1T|BA1|BA3") && StringUtils.isEmpty(offenseCode2)
                    && StringUtils.isEmpty(offenseCode3)) {
                // (except for action code 1 or 4 for A1T)
                String actionCode = de.getActionCode();
                if (!offenseCode.equals("A1T") || !actionCode.matches("01|04")) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Incident requires more than one offense code. Example: incidents with involving weapon must include a weapon offense code",
                            "Offense code = " + STYLE_BOLD + offenseCode + STYLE_END +
                                    ", incident ID = " + STYLE_BOLD + incident.getIncidentId() + STYLE_END));
                }
            }

            // 2. have more than one offender
            DisciplineCollection dc = (DisciplineCollection) data;
            Collection<ConductIncident> incidents = dc.m_fightMap.get(incident.getIncidentId());
            if (offenseCode.matches("FA1|FA2") && incidents.size() < 2) {
                errors.add(new StateReportValidationError(entity, field,
                        "Fighting Incident requires more than one offender",
                        "Offense code = " + STYLE_BOLD + offenseCode + STYLE_END +
                                ", incident ID = " + STYLE_BOLD + incident.getIncidentId() + STYLE_END));
            }

            return errors;
        }

    }

    /**
     * Validate Time.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateTime implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            ConductIncident incident = (ConductIncident) entity.getBean();
            String incTimeCode = (String) incident.getFieldValueByAlias(ALIAS_INC_TIME);

            if (lookupStateValue(ConductIncident.class, m_doeInkTime, incTimeCode) == null) {
                errors.add(new StateReportValidationError(entity, field, "No time of day selected.", ""));
            }

            return errors;
        }

    }

    /**
     * Validate victim count.
     *
     * Incidents with the following offense codes
     * MUST have a victim count (cannot be zero):
     *
     * BA1 BA2 BA3 BA4 BA5 BU1 BU2 EX1
     * H1Z HR1 HO1 HO2 HO3 HO4 KI1 RO1
     * SB1 SB2 ST1 SX0 SX1 SX2 SX3 SX4
     * SX5 SX6 SX7 SX8 TH1 TH2 TI1 TI2
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateVictimCount implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String offenseCode = entity.getFieldValue(FIELD_PRIMARY_OFFENSE_CODE);
            if (offenseCode.matches(REGEX_VICTIM_OFFENSE_CODES) && Integer.parseInt(value) == 0) {
                errors.add(new StateReportValidationError(entity, field,
                        "Incident requires a victim count",
                        "Offense code = " + STYLE_BOLD + offenseCode + STYLE_END +
                                ", victim count = " + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }

    }

    /*
     * End of line character
     */
    protected static final String EOL = System.getProperty("line.separator");

    /*
     * Aliases
     */
    protected static final String ALIAS_ACT_AGGR_CIRCUMSTANCES = "all-act-AggravatingCircumstances";
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_DISTRICT_SERVE = "DOE DISTRICT SERVE";
    protected static final String ALIAS_ED_SERVICES_PROVIDED = "all-act-ProvidingEducationServices";
    protected static final String ALIAS_ENROLLED_SCHOOL = "DOE ENROLLED SCHOOL";
    protected static final String ALIAS_INC_TIME = "DOE INC TIME";
    protected static final String ALIAS_INCIDENT_SCHOOL = "DOE INCIDENT SCHOOL";
    protected static final String ALIAS_OFFENSE_2 = "DOE OFFENSE 2";
    protected static final String ALIAS_OFFENSE_3 = "DOE OFFENSE 3";
    protected static final String ALIAS_PRIMARY_ACTION = "DOE PRIMARY ACTION";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SCHOOL_SERVE = "DOE SCHOOL SERVE";


    /*
     * Export Column Names
     */
    protected static final String COLUMN_ACTION_CODE = "Action Code";

    /*
     * Fields
     */
    protected static final String FIELD_ACTION_CODE = "Action Code";
    protected static final String FIELD_OFFENSE_CODE_2 = "Offense Code 2";
    protected static final String FIELD_OFFENSE_CODE_3 = "Offense Code 3";
    protected static final String FIELD_TIME = "Time";
    protected static final String FIELD_PRIMARY_OFFENSE_CODE = "Offense Code 1";

    /*
     * Export parameters
     */
    protected static final String PARAM_LIMIT_OFFENSES = "limitOffenses";
    protected static final String PARAM_QUERY_BY = "queryBy";
    protected static final String PARAM_QUERY_STRING = "queryString";
    protected static final String PARAM_SORT = "sort";
    protected static final String PARAM_SUMMARY = "summary";

    /*
     * Retriever parameters
     */
    protected static final String PARAM_ACTION_CODE = "CODE";
    protected static final String PARAM_ACTION_DAYS = "DAYS";
    protected static final String PARAM_ACTION_EXPULSION = "EXPULSION";

    protected static final String EXPULSION_STATE_CODE = "04";
    protected static final String OTHER_ACTION_STATE_CODE = "99";


    /*
     * Regular expressions for validations
     */
    protected static final String REGEX_VICTIM_OFFENSE_CODES = "BA1|BA2|BA3|BA4|BA5|BU1|BU2|ET1|ET2|EX1|H1Z|" +
            "HO1|HO2|HO3|HO4|HR1|KI1|RB1|RB2|RO1|SB1|SB2|" +
            "ST1|SX0|SX1|SX2|SX3|SX4|SX5|SX6|SX7|SX8|TF1|" +
            "TF2|TF3|TF4|TF6|TH1|TH2|TI1|TI2";
    protected static final String REGEX_FIREARMS_OFFENSE_CODES = "WP1|WP2|WP4|WP8";
    protected static final String REGEX_WEAPON_OFFENSE_CODES = "DR1|DR4|WP1|WP2|WP4|WP6|WP7|WP8";
    protected static final String REGEX_REPORT_OFFENSE_CODES = "AL1|BA1|BA2|BA3|BA4|BA5|BB1|BU2|DR1|DR2|DR3|DR4|" +
            "DR5|HO1|HO2|HO3|HO4|SB1|SB2|ST1|SX3|SX4|SX5|SX6|" +
            "SX7|SX8|TI1|WP0|WP1|WP2|WP4|WP5|WP6|WP7|WP8|" +
            "WP9|AS1|AS2|AS3|BK2|BK2|ET1|ET2|TF6";
    protected static final String REGEX_NON_FIREARMS_OFFENSE_CODES = "W1P|W2P|W3P|W8P|W9P|WP0|WP5|WP6|WP7|WP9|WS1|WT1";
    protected static final String REGEX_NO_SANCTION_99 = "A1T|AR1|BR1|C1M|C2M|C3M|D1C|D2C|D3C|D4C|D4G|D5C|" +
            "D5G|D6C|D6G|D8C|EX1|F1T|G1B|GA1|H1Z|RT1|RT2|S1V|" +
            "S2V|S3V|T1C|T2C|T3C|T4B|T4C|TH1|TF1|TF2|TF3|TF4|" +
            "TH2|TR1|VA1|VN1|VN2|VN3|W3P|W8P|W9P|";
    protected static final String REGEX_ONLY_SANCTION_01_04 = "A1T|";
    protected static final String REGEX_SANCTION_01_04 = "01|04";

    protected static final String FLAG_FEDERAL_DCV = "DCV";

    /*
     * Local variables
     */
    protected String m_actAggravatingCircumstances;
    protected Map<String, ReferenceCode> m_actionCodesMap;
    protected Map<String, List<ConductAction>> m_actionsMap;
    protected String m_doeEnrolledSchool;
    protected String m_doeIncidentSchool;
    protected String m_doeInkTime;
    protected String m_doeIsPrimaryAction;
    protected String m_doeSchoolServe;
    protected String m_edServicesProvided;
    protected Map<String, Collection<StudentEnrollment>> m_enrollmentMap;
    protected String m_fieldOffense2;
    protected String m_fieldOffense3;
    protected Map<String, Collection<ConductIncident>> m_fightMap;
    protected Boolean m_limitOffenses;
    protected Map<String, ReferenceCode> m_offenseCodesMap;
    protected Map<String, Collection<ConductOffense>> m_offensesLookupMap;
    protected Map<String, Collection<Race>> m_raceCodeMap;
    protected Map<String, ReferenceCode> m_raceCodes;
    protected PlainDate m_reportDate = new PlainDate();
    protected Character m_vadcvDelimiterChar;

    /*
     * Translated a sum of race code state codes (1,2,4,8,16) into VA race code representation.
     */
    protected int[] m_raceValueTranslator = new int[] {0, // 0
            5, // 1 W
            3, // 2 B
            14, // 3 WB
            2, // 4 A
            12, // 5 AW
            11, // 6 AB
            20, // 7 AWB
            1, // 8 I
            9, // 9 IW
            8, // 10 IB
            24, // 11 IWB
            7, // 12 IA
            18, // 13 IAW
            17, // 14 IAB
            27, // 15 IAWB
            6, // 16 P
            16, // 17 PW
            15, // 18 PB
            22, // 19 PWB
            13, // 20 PA
            26, // 21 PAW
            21, // 22 PAB
            28, // 23 PAWB
            10, // 24 PI
            25, // 25 PIW
            23, // 26 PIB
            29, // 27 PIWB
            19, // 28 PIA
            30, // 29 PIAW
            31, // 30 PIAB
            32}; // 31 PIAWB



    /**
     * Return a custom heading line.
     *
     * @return String
     * @see com.x2dev.sis.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        // get values used in the heading.
        String divId = (String) getOrganization().getFieldValueByAlias(ALIAS_DISTRICT_ID);
        String email = null;
        if (getUser() != null) {
            User user = getUser();
            if (user.getPerson() != null) {
                email = user.getPerson().getEmail01();
            }
        }

        if (StringUtils.isEmpty(email)) {
            email = "{email}";
        }

        // Header row
        Date currDate = new Date();
        SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");
        SimpleDateFormat timeFormat = new SimpleDateFormat("hh:mm:ss");

        // MSC header record
        int year = getCurrentContext().getSchoolYear() - 1;
        StringBuilder header = new StringBuilder(130);

        // ** RecordCount belongs in the footer, but we do not have a footer. Place here so they can
        // copy/paste it to the end.
        // header.append("RecordCount=").append(Integer.toString(m_rowCount)).append(EOL);

        header.append("SenderID=").append(divId).append(EOL);
        header.append("CreateDate=").append(dateFormat.format(currDate)).append(EOL);
        header.append("CreateTime=").append(timeFormat.format(currDate)).append(EOL);
        header.append("EMAIL=").append(email).append(EOL);
        header.append("~~").append(EOL);
        header.append("DATATYPE=DISCIPLN").append(EOL);
        header.append("~").append(EOL);

        // A record.
        header.append("A")
                .append("DISCIPLN")
                .append(Integer.toString(year))
                .append(getOrganization().getFieldValueByAlias(ALIAS_DISTRICT_ID))
                .append(EOL);

        return header.toString();
    }

    /**
     * Override the delimiter character. It may have been altered in the initialization method.
     *
     * @return Character
     * @see com.x2dev.sis.tools.stateexports.StateReportData#getValueDelimiter()
     */
    @Override
    public Character getValueDelimiter() {
        return m_vadcvDelimiterChar;
    }

    /**
     * Initialize the data module.
     */
    @Override
    protected void initialize() {
        initializeFields();

        /*
         * Define delimiter char. The char can be a tab.
         * The user may enter a tab as "\t" in the Export Format Definition.
         * Internally, only the "\" will come through, so assume it represents a tab
         * and change the delimiter char to a tab.
         */
        m_vadcvDelimiterChar = super.getValueDelimiter();
        if (m_vadcvDelimiterChar != null && m_vadcvDelimiterChar.charValue() == '\\') {
            m_vadcvDelimiterChar = Character.valueOf('\t');
        }

        Boolean objLimitOffenses = (Boolean) getParameter(PARAM_LIMIT_OFFENSES);
        m_limitOffenses = (objLimitOffenses != null ? objLimitOffenses : Boolean.TRUE);

        Criteria incidentCriteria = getIncidentCriteria();
        QueryByCriteria incidentQuery = new QueryByCriteria(ConductIncident.class, incidentCriteria, true);
        Integer sort = (Integer) getParameter(PARAM_SORT);
        switch (sort != null ? sort.intValue() : 0) {
            case 0: // Name
                incidentQuery.addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + Student.COL_NAME_VIEW);
                break;

            case 1: // YOG
                incidentQuery.addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + Student.COL_YOG);
                incidentQuery.addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + Student.COL_NAME_VIEW);
                break;

            case 2: // School
                incidentQuery.addOrderByAscending(ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                        School.COL_NAME);
                incidentQuery.addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER +
                        Student.COL_NAME_VIEW);
                break;

            case 3: // LASID
                incidentQuery.addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + Student.COL_LOCAL_ID);
                break;

            case 4: // SASID
                incidentQuery.addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + Student.COL_STATE_ID);
                break;

            case 5: // Error Type
                // Then sort grid.
                break;

            default:
                incidentQuery.addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + Student.COL_NAME_VIEW);
                break;
        }
        incidentQuery.addOrderBy(ConductIncident.COL_INCIDENT_ID, true);

        // Set the query to be used for student selection.
        setQuery(incidentQuery);
        setEntityClass(DisciplineEntity.class);

        // Get all the student enrollments (used for finding the student's school)
        SubQuery studentSubQuery =
                new SubQuery(ConductIncident.class, ConductIncident.COL_STUDENT_OID, incidentCriteria);
        Criteria enrollmentCriteria = new Criteria();
        enrollmentCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, studentSubQuery);
        QueryByCriteria enrollmentQuery = new QueryByCriteria(StudentEnrollment.class, enrollmentCriteria);
        enrollmentQuery.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
        enrollmentQuery.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);
        m_enrollmentMap =
                getBroker().getGroupedCollectionByQuery(enrollmentQuery, StudentEnrollment.COL_STUDENT_OID, 1000);

        // Load reference codes, action/offense maps, race codes
        loadActions(incidentCriteria);
        loadOffenses(incidentCriteria);
        loadRaceCodes(incidentCriteria);

        /*
         * If this is a validation report, load in the fighting incidents
         * (to check if incidents with offense codes "FA2" and "FA1" has more than 2 offenders)
         */
        Boolean summary = (Boolean) getParameter(PARAM_SUMMARY);
        if (summary != null) {
            Criteria fightCriteria = incidentCriteria.copy(false, false, false);
            fightCriteria.addIn(ConductIncident.COL_INCIDENT_CODE, Arrays.asList("FA1", "FA2"));
            QueryByCriteria fightQuery = new QueryByCriteria(ConductIncident.class, fightCriteria);
            m_fightMap = getBroker().getGroupedCollectionByQuery(fightQuery, ConductIncident.COL_INCIDENT_ID, 50);
        }

        // Add retrievers
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put("DCV-ACTION", new RetrieveAction());
        calcs.put("DCV-CHANGE", new RetrievePerminantChange());
        calcs.put("DCV-DISTRICT-SCHOOL", new RetrieveDistrictSchool());
        calcs.put("DCV-OFFENSE", new RetrieveOffense());
        calcs.put("DCV-RACE", new RetrieveRace());
        calcs.put("DCV-PROVSERV", new RetrieveServicesProvided());
        calcs.put(RetrieveAggrCircumstances.CALC_ID, new RetrieveAggrCircumstances());
        super.addCalcs(calcs);

        // Add validators
        HashMap<String, FieldValidator> vals = new HashMap<String, FieldValidator>();
        vals.put("DCV-ACTION-CODE-VAL", new ValidateActionCode());
        vals.put("DCV-FIREARMS-VAL", new ValidateFirearms());
        vals.put("DCV-LAW-VAL", new ValidateLawEnforcement());
        vals.put("DCV-NONFIREARMS-VAL", new ValidateNonFirearms());
        vals.put("DCV-VICTIM-VAL", new ValidateVictimCount());
        vals.put("DCV-ACTION-DAYS-VAL", new ValidateActionDays());
        vals.put("DCV-OFFENSE-VAL", new ValidateOffense());
        vals.put("DCV-TIME-VAL", new ValidateTime());
        super.addValidators(vals);
    }

    /**
     * Load maps for race codes reference codes and person race for students.
     *
     * @param incidentCriteria Criteria
     */
    protected void loadRaceCodes(Criteria incidentCriteria) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);

        // Get race code reference codes for use in the race retriever.
        Criteria raceCriteria = new Criteria();
        raceCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, raceCriteria);
        m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 5);

        // Load the race codes for all students included in the export.
        SubQuery subQuery = new SubQuery(ConductIncident.class,
                ConductIncident.REL_STUDENT + PATH_DELIMITER + Student.COL_PERSON_OID,
                incidentCriteria);
        raceCriteria = new Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID, subQuery);
        QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
        m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 100);
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + "." + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(ConductIncident.COL_STUDENT_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getIncidentCriteria() {
        // Look for all incidents in school/year.
        X2Criteria incidentCriteria = new X2Criteria();
        if (isSchoolContext()) {
            incidentCriteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            incidentCriteria.addEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                    School.COL_INACTIVE_INDICATOR, Boolean.FALSE);
            incidentCriteria.addEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                    School.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        }
        incidentCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE,
                getCurrentContext().getStartDate());
        // incidentCriteria.addNotEmpty(ConductIncident.COL_INCIDENT_CODE,
        // getBroker().getPersistenceKey());

        // Check student selection criteria user input.
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        switch (queryBy) {
            case 1: // YOG
                incidentCriteria.addEqualTo(ConductIncident.REL_STUDENT + PATH_DELIMITER +
                        Student.COL_YOG, queryString);
                break;

            case 2: // LASID
                incidentCriteria.addEqualTo(ConductIncident.REL_STUDENT + PATH_DELIMITER +
                        Student.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                incidentCriteria.addEqualTo(ConductIncident.REL_STUDENT + PATH_DELIMITER +
                        Student.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(incidentCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        // If the user selects to limit the offenses, identify reportable offenses and actions.
        if (m_limitOffenses.booleanValue()) {
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
                criteria.addEqualTo(ReferenceCode.COL_FEDERAL_CODE, FLAG_FEDERAL_DCV);
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
                criteria.addEqualTo(ReferenceCode.COL_FEDERAL_CODE, FLAG_FEDERAL_DCV);
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
            Criteria code2Criteria = new Criteria();
            Criteria code3Criteria = new Criteria();
            Criteria codeACriteria = new Criteria();
            if (incidentCodes.size() > 0 && actionCodes.size() > 0) {
                // Not using offense sub table, check each of three code fields individually.
                codeCriteria.addIn(ConductIncident.COL_INCIDENT_CODE, incidentCodes);
                code2Criteria.addIn(m_fieldOffense2, incidentCodes);
                code3Criteria.addIn(m_fieldOffense3, incidentCodes);
                codeACriteria.addIn(ConductIncident.REL_CONDUCT_ACTIONS + PATH_DELIMITER +
                        ConductAction.COL_ACTION_CODE, actionCodes);
                codeCriteria.addOrCriteria(code2Criteria);
                codeCriteria.addOrCriteria(code3Criteria);
                codeCriteria.addOrCriteria(codeACriteria);
                incidentCriteria.addAndCriteria(codeCriteria);
            }
        }
        return incidentCriteria;
    }

    /**
     * Initialize fields. Translate aliases to java names.
     */
    private void initializeFields() {
        m_doeInkTime = translateAliasToJavaName(ALIAS_INC_TIME, true);
        m_doeIsPrimaryAction = translateAliasToJavaName(ALIAS_PRIMARY_ACTION, true);
        m_doeSchoolServe = translateAliasToJavaName(ALIAS_SCHOOL_SERVE, true);
        m_fieldOffense2 = translateAliasToJavaName(ALIAS_OFFENSE_2, true);
        m_fieldOffense3 = translateAliasToJavaName(ALIAS_OFFENSE_3, true);
        m_doeEnrolledSchool = translateAliasToJavaName(ALIAS_ENROLLED_SCHOOL, true);
        m_doeIncidentSchool = translateAliasToJavaName(ALIAS_INCIDENT_SCHOOL, true);
        m_edServicesProvided = translateAliasToJavaName(ALIAS_ED_SERVICES_PROVIDED, true);
        m_actAggravatingCircumstances = translateAliasToJavaName(ALIAS_ACT_AGGR_CIRCUMSTANCES, true);
    }

    /**
     * Load supporting collections of actions.
     *
     * @param incidentCriteria Criteria
     */
    private void loadActions(Criteria incidentCriteria) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(ConductAction.class.getName(),
                ConductAction.COL_ACTION_CODE);
        String referenceTableOid = field.getReferenceTableOid();

        X2Criteria criteria = null;
        QueryByCriteria query = null;

        // Get a map of action codes to ReferenceCode.
        if (referenceTableOid != null) {
            criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
            query = new QueryByCriteria(ReferenceCode.class, criteria, false);
            m_actionCodesMap = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 50);
        }

        // Load all actions for the selected incidents into a map.
        SubQuery incidentQuery = new SubQuery(ConductIncident.class, X2BaseBean.COL_OID, incidentCriteria);

        criteria = new X2Criteria();
        criteria.addIn(ConductAction.COL_INCIDENT_OID, incidentQuery);
        query = new QueryByCriteria(ConductAction.class, criteria, false);
        m_actionsMap = getBroker().getGroupedCollectionByQuery(query, ConductAction.COL_INCIDENT_OID, 1500);
    }

    /**
     * Load supporting collections of offenses.
     *
     * @param incidentCriteria Criteria
     */
    private void loadOffenses(Criteria incidentCriteria) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field =
                dictionary.findDataDictionaryField(ConductOffense.class.getName(), ConductOffense.COL_INCIDENT_CODE);
        String referenceTableOid = field.getReferenceTableOid();

        X2Criteria criteria = null;
        QueryByCriteria query = null;

        // Get a map of offense codes to ReferenceCode.
        if (referenceTableOid != null) {
            criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
            query = new QueryByCriteria(ReferenceCode.class, criteria, false);
            m_offenseCodesMap = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 150);
        }

        // Load all offenses for the selected incidents into a map.
        SubQuery incidentQuery = new SubQuery(ConductIncident.class, X2BaseBean.COL_OID, incidentCriteria);

        criteria = new X2Criteria();
        criteria.addIn(ConductOffense.COL_INCIDENT_OID, incidentQuery);
        query = new QueryByCriteria(ConductOffense.class, criteria, false);
        query.addOrderByDescending(ConductOffense.COL_PRIMARY); // primary comes first
        m_offensesLookupMap = getBroker().getGroupedCollectionByQuery(query, ConductOffense.COL_INCIDENT_OID, 1500);

    }
}
