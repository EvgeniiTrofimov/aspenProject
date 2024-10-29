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
package com.x2dev.procedures.statereporting.ri;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictCalendar;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.OrganizationDefinition;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
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
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeSet;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.springframework.util.CollectionUtils;

/**
 *
 * RI state report for Discipline export. This class implements the data export
 * for the RI Discipline export.
 *
 * @author X2 Development Corporation
 */
public class Discipline extends RIStateReportData {
    /**
     * Implementation of Discipline Entity to be used by the Discipline export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class DisciplineEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";
        Discipline m_discipline;
        ConductIncident m_conductIncident;
        LinkedHashMap<String, ArrayList<ConductAction>> m_incidentActionsByCodes;

        /**
         * No argument constructor for dynamic instantiation.
         */
        public DisciplineEntity() {
            // No argument constructor for dynamic instantiation
        }

        /**
         * Initialize the entity for the student bean provided. This method
         * finds the student schedule and student schedule change records for
         * the student and generates a list of reportable schedule items. The
         * entity can produce multiple rows from these results.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.
         *      stateexports.StateReportData,
         *      com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean)
                throws X2BaseException {
            super.intitialize(data, bean);

            m_discipline = (Discipline) data;
            m_conductIncident = (ConductIncident) bean;
            m_incidentActionsByCodes = new LinkedHashMap<String, ArrayList<ConductAction>>();

            TreeSet<ConductAction> actionsOrdByDate = new TreeSet<ConductAction>(new Comparator<ConductAction>() {
                @Override
                public int compare(ConductAction action1, ConductAction action2) {
                    return action1.getActionStartDate().compareTo(action2.getActionStartDate());
                }
            });
            actionsOrdByDate.addAll(m_conductIncident.getConductActions());

            // TODO, is this an ordered collection?
            for (ConductAction conductAction : actionsOrdByDate) {
                String conductActionCode = conductAction.getActionCode();
                if (m_discipline.m_conductAction.containsKey(conductActionCode)) {
                    ReferenceCode refCode = m_discipline.m_conductAction.get(conductActionCode);
                    String stateDisciplineCode = refCode.getStateCode();
                    if (stateDisciplineCode != null) {
                        ArrayList<ConductAction> actions = m_incidentActionsByCodes.get(stateDisciplineCode);
                        if (actions == null) {
                            actions = new ArrayList<ConductAction>();
                            m_incidentActionsByCodes.put(stateDisciplineCode, actions);
                        }
                        actions.add(conductAction);
                    }
                }
            }

            setRowCount(m_incidentActionsByCodes.size() == 0 ? 0 : 1);
        }

        /**
         * Returns the display name of the represented entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
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
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }
    }

    /**
     * Input parameter: Select organization level.
     */
    public static final String ORGANIZATION_PARAM = "orgOid";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Name for the sasid students only parameter. The value is a boolean.
     */
    public static final String SASID_STUDENTS_ONLY_PARAM = "sasidStudentsOnly";
    private static final String EXCLUDE_ALIAS_STD = "DOE EXCLUDE STD";
    protected final String CALC_PARAM_SEC = "SEC";

    /**
     * Constants used in retrieving and validation
     */
    protected final String FIELD_INFRACTION = "INFRACTION";
    protected final String FIELD_WEAPON = "WEAPON";
    protected final String FIELD_DURATION = "DURATION";
    protected final String PARAM_WEAPON_POSSESSION = "39";

    protected Map<String, ReferenceCode> m_conductAction;
    protected PlainDate m_districtFirstSessionDate;
    protected String m_excludeStdField;
    protected PlainDate m_reportDate;
    protected boolean m_sasidStudentOnly;
    protected StudentHistoryHelper m_stdHelper;
    private String m_orgFieldStr = null;
    private String m_orgOid = null;

    /**
     * Initialize the data module. Initialize necessary working resources.
     * Define query for students to load. Define list of field definitions for
     * the export.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.x2dev.sis.model.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();
        initialConductActionCodes();
        m_sasidStudentOnly = true;
        Boolean sasidStudentOnly = (Boolean) getParameter(SASID_STUDENTS_ONLY_PARAM);
        if (sasidStudentOnly != null) {
            m_sasidStudentOnly = sasidStudentOnly.booleanValue();
        }

        // Get organization Oid and level and field.
        m_orgOid = (String) getParameter(ORGANIZATION_PARAM);
        Organization organization = null;
        OrganizationDefinition def = null;
        if (!StringUtils.isEmpty(m_orgOid)) {
            organization = (Organization) getBroker().getBeanByOid(Organization.class, m_orgOid);
            if (organization != null) {
                def = organization.getOrganizationDefinition();
                m_orgFieldStr = "organization"
                        + Integer.toString(def.getLevel() + 1) + "Oid";
            }
        }

        Organization district = OrganizationManager.getRootOrganization(getBroker());
        DistrictCalendar districtCalender = CalendarManager.getDistrictInSessionStartEndDate(
                organization == null ? district : organization,
                getCurrentContext(), true, getBroker());

        m_districtFirstSessionDate = districtCalender.getDate();
        m_excludeStdField = translateAliasToJavaName(EXCLUDE_ALIAS_STD, false);

        // default report to today - now
        // TODO change to be input from the input format definition, can default
        // to today in the input definition
        m_reportDate = new PlainDate(System.currentTimeMillis());

        m_stdHelper = new StudentHistoryHelper(this);
        m_stdHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_stdHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        m_stdHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_districtFirstSessionDate);

        if (getSetupErrors().size() == 0) {

            X2Criteria studentConductIncidentCriteria = new X2Criteria();
            studentConductIncidentCriteria.addAndCriteria(getStudentConductIncidentCriteria());

            applyInputCriteria(studentConductIncidentCriteria, false, ConductIncident.REL_STUDENT);

            QueryByCriteria studentConductIncidentQuery = new QueryByCriteria(ConductIncident.class,
                    studentConductIncidentCriteria);

            applyInputSort(studentConductIncidentQuery, ConductIncident.REL_STUDENT);

            // Set the query to be used for student selection.
            setQuery(studentConductIncidentQuery);
            setEntityClass(DisciplineEntity.class);

            // Build maps of retriever functions
            Map<String, FieldRetriever> calcs = new Hashtable<String, FieldRetriever>();
            calcs.put("DIS-RetrieveDisc", new RetrieveDiscipline());
            calcs.put("DIS-RetrieveDuration", new RetrieveDuration());
            calcs.put(RetrieveSchoolId.CALC_ID, new RetrieveSchoolId());
            super.addCalcs(calcs);

            // Build maps of validator functions
            Map<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put("DIS-ValidateInfract", new ValidateInfraction());
            super.addValidators(validators);
        }
    }

    /**
     * Retrieve the discipline associated with the conduct incident.
     */
    protected class RetrieveDiscipline implements FieldRetriever {
        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            DisciplineEntity disciplineEntity = ((DisciplineEntity) entity);

            return getStateDiscCode(field, disciplineEntity);
        }
    }

    /**
     * Calculate the total duration associated with the conduct incident.
     */
    protected class RetrieveDuration implements FieldRetriever {
        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            DisciplineEntity disciplineEntity = ((DisciplineEntity) entity);
            ConductIncident conductIncident = (ConductIncident) disciplineEntity.getBean();

            String stateDisciplineCode = getStateDiscCode(field, disciplineEntity);

            BigDecimal duration = null;

            Collection<ConductAction> conductActions = conductIncident.getConductActions();
            for (ConductAction conductAction : conductActions) {
                BigDecimal actionPenalty = conductAction.getActionPenaltyTime();
                String actionCode = conductAction.getActionCode();
                String actionStateCode = "";
                if (m_conductAction.containsKey(actionCode)) {
                    actionStateCode = m_conductAction.get(actionCode).getStateCode();
                }

                if (actionPenalty != null && stateDisciplineCode != null &&
                        stateDisciplineCode.equals(actionStateCode)) {
                    if (duration == null) {
                        duration = actionPenalty.setScale(0, RoundingMode.UP);
                    } else {
                        duration = duration.add(actionPenalty.setScale(0, RoundingMode.UP));
                    }
                }
            }

            return duration != null ? duration.toPlainString() : "";
        }
    }

    /**
     * Retrieve the school id based on primary school for enrollment for the incident date.
     */
    protected class RetrieveSchoolId implements FieldRetriever {
        private static final String CALC_ID = "SKL-STATE-ID";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            String sklId = null;
            ConductIncident conductIncident = (ConductIncident) entity.getBean();
            PlainDate cndDate = conductIncident.getIncidentDate();
            String stdOid = conductIncident.getStudentOid();
            if (cndDate != null && !StringUtils.isEmpty(stdOid)) {
                StudentEnrollment stdEnr = m_stdHelper.getEnrollmentForDate(stdOid, cndDate, "E");
                if (stdEnr != null) {
                    sklId = (String) stdEnr.getSchool().getFieldValueByBeanPath(m_sklIdField);
                }
            }
            return sklId;
        }
    }

    /**
     * The Class ValidateInfraction.
     */
    protected class ValidateInfraction implements FieldValidator {

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
            String infraction = entity.getFieldValue(FIELD_INFRACTION);
            String weapon = entity.getFieldValue(FIELD_WEAPON);
            String duration = entity.getFieldValue(FIELD_DURATION);
            double numericDuration = Double.NaN;
            double minimumDuration = 1.0;

            if (infraction.equals(PARAM_WEAPON_POSSESSION) && StringUtils.isEmpty(weapon)) {
                errors.add(new StateReportValidationError(entity, field,
                        "If INFRACTION is " + STYLE_BOLD + infraction + STYLE_END
                                + " then a value for WEAPON is required",
                        "INFRACTION=" + STYLE_BOLD + infraction + STYLE_END + ", WEAPON=" + weapon));
            }

            if (infraction.equals("SUSPENSION")) {

                if (!StringUtils.isEmpty(duration) && StringUtils.isNumeric(duration)) {
                    numericDuration = Float.parseFloat(duration);
                }

                if (Double.isNaN(numericDuration) || numericDuration <= minimumDuration) {
                    errors.add(new StateReportValidationError(entity, field,
                            "If INFRACTION is " + STYLE_BOLD + infraction + STYLE_END +
                                    " and DURATION is less than 1, then " + STYLE_BOLD + infraction + STYLE_END
                                    + " should not be reported",
                            "INFRACTION=" + STYLE_BOLD + infraction + STYLE_END + ", DURATION=" + duration));
                }


            }

            return errors;
        }

    }

    /**
     * Returns the criteria that retrieves all students that should be included
     * in the export.
     *
     * @param field FieldDefinition
     * @param disciplineEntity DisciplineEntity
     * @return Criteria
     */
    /*
     * private Criteria getStudentConductIncidentCriteria()
     * {
     * X2Criteria userCriteria = new X2Criteria();
     * userCriteria.addAndCriteria(getReportingCriteria());
     *
     * applyInputCriteria(userCriteria, false, ConductIncident.REL_STUDENT);
     *
     * return userCriteria;
     * }
     */
    private String getSecondaryCode(LinkedHashMap<String, ArrayList<ConductAction>> actions) {
        String secondaryCode = null;

        int indexCounter = 0;
        for (Entry<String, ArrayList<ConductAction>> entry : actions.entrySet()) {
            ++indexCounter;
            secondaryCode = indexCounter == 2 ? entry.getKey() : secondaryCode;
        }

        return secondaryCode;
    }

    /**
     * Returns the state discipline code.
     *
     * @param field FieldDefinition
     * @param disciplineEntity DisciplineEntity
     * @return String
     */
    private String getStateDiscCode(FieldDefinition field, DisciplineEntity disciplineEntity) {
        String stateDisciplineCode;
        LinkedHashMap<String, ArrayList<ConductAction>> actions = disciplineEntity.m_incidentActionsByCodes;

        String parameter = StringUtils.isEmpty((String) field.getParameter()) ? "" : (String) field.getParameter();

        switch (parameter) {
            case "SEC":
                stateDisciplineCode = (CollectionUtils.isEmpty(actions) || actions.size() < 2)
                        ? ""
                        : getSecondaryCode(actions);
                break;
            default:
                stateDisciplineCode = actions.keySet().iterator().next();
                break;
        }
        return stateDisciplineCode;
    }

    /**
     * Returns the criteria that retrieves all student that should be included
     * in the export.
     *
     * @return Criteria
     */
    private Criteria getStudentConductIncidentCriteria() {
        X2Criteria studentConductIncidentCriteria = new X2Criteria();

        studentConductIncidentCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE,
                m_districtFirstSessionDate);
        studentConductIncidentCriteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE,
                m_reportDate);

        // if "Student must have SASID" is checked, get only students with non-empty state ids
        if (m_sasidStudentOnly) {
            studentConductIncidentCriteria.addNotEmpty(ConductIncident.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());
        }

        if (isSchoolContext()) {
            studentConductIncidentCriteria.addEqualTo(ConductIncident.COL_SCHOOL_OID,
                    getSchool().getOid());

        } else {
            if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
                studentConductIncidentCriteria.addEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                        m_orgFieldStr,
                        m_orgOid);
            }
            studentConductIncidentCriteria.addNotEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                    School.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
            studentConductIncidentCriteria.addNotEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                    School.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
        }

        // Check exclude student from state reporting flag if present.
        if (!StringUtils.isEmpty(m_excludeStdField)) {
            studentConductIncidentCriteria.addNotEqualTo(ConductIncident.REL_STUDENT + PATH_DELIMITER +
                    m_excludeStdField,
                    BooleanAsStringConverter.TRUE);
        }

        return studentConductIncidentCriteria;
    }

    /**
     * Initialize the Conduct Action reference codes.
     */
    private void initialConductActionCodes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField conductionActionCodeField =
                dictionary.findDataDictionaryField(ConductAction.class.getName(),
                        ConductAction.COL_ACTION_CODE);

        ReferenceTable refTable = conductionActionCodeField.getReferenceTable();
        if (refTable == null) {
            addSetupError("missing reference table",
                    "reference table not found for the conduct action code column on the conduct action table ");
            m_conductAction = new HashMap<String, ReferenceCode>();
        } else {
            m_conductAction = refTable.getCodeMap(getBroker());
        }
    }
}
