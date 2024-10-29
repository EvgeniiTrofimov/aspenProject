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
package com.x2dev.procedures.statereporting;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.ConductOffense;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class CRDCConductHelper.
 */
public class CRDCConductHelper {
    public static final String NO_SCHOOL = "NO_SCHOOL";

    private static final String ALIAS_CND_OFFENCE_2 = "DOE OFFENSE 2";
    private static final String ALIAS_CND_OFFENCE_3 = "DOE OFFENSE 3";

    private static Set<String> m_crdcActionCodes = new HashSet(Arrays.asList(
            "CorporalPunish",
            "In-schoolSusp",
            "Out-schoolSusp",
            "Expelled",
            "EdServWhileExpelled",
            "ExpelledZeroTol",
            "ReferredLawEnforce",
            "SchoolRelatedArrest",
            "AltSchDiscipline",
            "MechanicalRestraint",
            "PhysicalRestraint",
            "Seclusion"));
    private static Set<String> m_crdcIncidentCodes = new HashSet(Arrays.asList(
            "Rape or attempted rape",
            "Sexual Battery other than rape",
            "Robbery with weapon",
            "Robbery with Firearm Explosive Dev",
            "Robbery without weapon",
            "Physical Attack Fight with weapon",
            "Physical Attack Fight with Firearm Explosive Dev",
            "Physical Attack Fight without weapon",
            "Threat of physical attack with weapon",
            "Threat of physical attack w Firearm Explosive Dev",
            "Threat of physical attack without weapon",
            "Possession of Firearm Explosive Dev",
            "Homicide",
            "Harassment Bullying Sex",
            "Harassment Bullying Race, Color or Nat'l origin",
            "Harassment Bullying Disability",
            "Harassment Bullying Sexual Orientation",
            "Harassment Bullying Religion",
            "Harassment Bullying Religion Atheist/Agnostic",
            "Harassment Bullying Religion Buddhist",
            "Harassment Bullying Religion Catholic",
            "Harassment Bullying Religion Eastern Orthodox",
            "Harassment Bullying Religion Hindu",
            "Harassment Bullying Religion Islamic",
            "Harassment Bullying Religion Jehovah's Witness",
            "Harassment Bullying Religion Jewish",
            "Harassment Bullying Religion Mormon",
            "Harassment Bullying Multiple Religions, groups",
            "Harassment Bullying Religion Other Christian",
            "Harassment Bullying Other Religion",
            "Harassment Bullying Religion Protestant",
            "Harassment Bullying Religion Sikh"));
    /**
     * An X2Broker for performing initialization and the data query.
     */
    private X2Broker m_broker = null;

    /**
     * A local copy of the data dictionary for use by various lookup utilities.
     */
    private DataDictionary m_dictionary;

    private boolean m_triedLoadActionsAlready = false;
    private boolean m_triedLoadIncidentsAlready = false;

    /**
     * Current school year context.
     */
    private DistrictSchoolYearContext m_districtContext;

    private Map<String, Map<String, Set<ConductIncident>>> m_mapSchoolIncidents;
    private Map<String, Map<String, Set<ConductAction>>> m_mapStudentActions;
    private Map<String, Map<String, Set<ConductIncident>>> m_mapStudentIncidents;
    private Map<String, Map<String, Set<ConductIncident>>> m_mapStudentVictims;

    /**
     * Instantiates a new CRDC conduct helper.
     *
     * @param broker X2Broker
     * @param districtContext DistrictSchoolYearContext
     */
    public CRDCConductHelper(X2Broker broker, DistrictSchoolYearContext districtContext) {
        super();
        this.m_broker = broker;
        this.m_districtContext = districtContext;
    }

    /**
     * Gets the actions for student.
     *
     * @param stdOid String
     * @param code String
     * @return Sets the
     */
    public Set<ConductAction> getActionsForStudent(String stdOid, String code) {
        String crdcCode = code == null ? null : code.toUpperCase();
        Set<ConductAction> value = null;
        if (m_mapStudentActions == null) {
            loadActions();
        }
        if (m_mapStudentActions != null) {
            Map<String, Set<ConductAction>> crdcCodeIncidents = m_mapStudentActions.get(stdOid);
            if (crdcCodeIncidents != null) {
                value = crdcCodeIncidents.get(crdcCode);
            }
        }

        return value;

    }

    /**
     * Gets the incidents for school.
     *
     * @param sklOid String
     * @param code String
     * @return Sets the
     */
    public Set<ConductIncident> getIncidentsForSchool(String sklOid, String code) {
        String crdcCode = code == null ? null : code.toUpperCase();
        Set<ConductIncident> value = null;
        if (m_mapSchoolIncidents == null) {
            loadIncidents();
        }
        if (m_mapSchoolIncidents != null) {
            Map<String, Set<ConductIncident>> crdcCodeIncidents = m_mapSchoolIncidents.get(sklOid);
            if (crdcCodeIncidents != null) {
                value = crdcCodeIncidents.get(crdcCode);
            }
        }

        return value;
    }


    /**
     * Gets the incidents for school code contains.
     *
     * @param sklOid String
     * @param code String
     * @return Sets the
     */
    public Set<ConductIncident> getIncidentsForSchoolCodeContains(String sklOid, String code) {
        String crdcCode = code == null ? null : code.toUpperCase();
        Set<ConductIncident> value = new HashSet();
        if (m_mapSchoolIncidents == null) {
            loadIncidents();
        }
        if (m_mapSchoolIncidents != null) {
            Map<String, Set<ConductIncident>> crdcCodeIncidents = m_mapSchoolIncidents.get(sklOid);
            if (crdcCodeIncidents != null) {
                for (Entry<String, Set<ConductIncident>> entry : crdcCodeIncidents.entrySet()) {
                    if (entry.getKey().contains(crdcCode)) {
                        value.addAll(entry.getValue());
                    }
                }
                value = crdcCodeIncidents.get(crdcCode);
            }
        }

        return value;
    }

    /**
     * Gets the incidents for student.
     *
     * @param stdOid String
     * @param code String
     * @return Sets the
     */
    public Set<ConductIncident> getIncidentsForStudent(String stdOid, String code) {
        String crdcCode = code == null ? null : code.toUpperCase();
        Set<ConductIncident> value = null;
        if (m_mapStudentIncidents == null) {
            loadIncidents();
        }
        if (m_mapStudentIncidents != null) {
            Map<String, Set<ConductIncident>> crdcCodeIncidents = m_mapStudentIncidents.get(stdOid);
            if (crdcCodeIncidents != null) {
                value = crdcCodeIncidents.get(crdcCode);
            }
        }

        return value;

    }

    /**
     * Gets the incidents for victim.
     *
     * @param stdOid String
     * @param code String
     * @return Sets the
     */
    /*
     * Return all incident where student is victim in category (crdcCode)
     * BASED_ON_DISABILITY,
     * BASED_ON_RACE,
     * BASED_ON_SEX
     * OTHER
     */
    public Set<ConductIncident> getIncidentsForVictim(String stdOid, String code) {
        String crdcCode = code == null ? null : code.toUpperCase();
        Set<ConductIncident> value = null;
        if (m_mapStudentVictims == null) {
            loadIncidents();
        }
        if (m_mapStudentVictims != null) {
            Map<String, Set<ConductIncident>> crdcCodeIncidents = m_mapStudentVictims.get(stdOid);
            if (crdcCodeIncidents != null) {
                value = crdcCodeIncidents.get(crdcCode);
            }
        }
        return value;
    }


    /**
     * Gets the student incidents map.
     *
     * @param stdOid String
     * @return the m_mapSchoolIncidents
     */
    public Map<String, Set<ConductIncident>> getStudentIncidentsMap(String stdOid) {
        if (m_mapStudentIncidents == null) {
            loadIncidents();
        }

        return m_mapStudentIncidents == null ? null : m_mapStudentIncidents.get(stdOid);
    }

    /**
     * Adds the action map.
     *
     * @param code String
     * @param action ConductAction
     */
    private void addActionMap(String code, ConductAction action) {
        String crdcCode = code == null ? null : code.toUpperCase();
        if (!StringUtils.isEmpty(crdcCode) && !StringUtils.isEmpty(action.getStudentOid())) {
            if (m_mapStudentActions == null) {
                m_mapStudentActions = new HashMap();
            }
            Map<String, Set<ConductAction>> crdcCodeIncidents = m_mapStudentActions.get(action.getStudentOid());
            if (crdcCodeIncidents == null) {
                crdcCodeIncidents = new HashMap();
                m_mapStudentActions.put(action.getStudentOid(), crdcCodeIncidents);
            }
            Set<ConductAction> actions = crdcCodeIncidents.get(crdcCode);
            if (actions == null) {
                actions = new HashSet();
                crdcCodeIncidents.put(crdcCode, actions);
            }
            actions.add(action);
        }
    }

    /**
     * Adds the incident map.
     *
     * @param code String
     * @param incident ConductIncident
     */
    private void addIncidentMap(String code, ConductIncident incident) {
        String crdcCode = code == null ? null : code.toUpperCase();
        String school = incident.getSchoolOid();
        if (StringUtils.isEmpty(school)) {
            school = NO_SCHOOL;
        }
        if (!StringUtils.isEmpty(crdcCode)) {
            // populate m_mapSchoolIncidents
            if (m_mapSchoolIncidents == null) {
                m_mapSchoolIncidents = new HashMap();
            }
            Map<String, Set<ConductIncident>> crdcCodeIncidents = m_mapSchoolIncidents.get(school);
            if (crdcCodeIncidents == null) {
                crdcCodeIncidents = new HashMap();
                m_mapSchoolIncidents.put(school, crdcCodeIncidents);
            }
            Set<ConductIncident> incidents = crdcCodeIncidents.get(crdcCode);
            if (incidents == null) {
                incidents = new HashSet();
                crdcCodeIncidents.put(crdcCode, incidents);
            }
            incidents.add(incident);

            // populate m_mapStudentIncidents
            if (!StringUtils.isEmpty(incident.getStudentOid())) {
                if (m_mapStudentIncidents == null) {
                    m_mapStudentIncidents = new HashMap();
                }
                crdcCodeIncidents = m_mapStudentIncidents.get(incident.getStudentOid());
                if (crdcCodeIncidents == null) {
                    crdcCodeIncidents = new HashMap();
                    m_mapStudentIncidents.put(incident.getStudentOid(), crdcCodeIncidents);
                }
                incidents = crdcCodeIncidents.get(crdcCode);
                if (incidents == null) {
                    incidents = new HashSet();
                    crdcCodeIncidents.put(crdcCode, incidents);
                }
                incidents.add(incident);
            }

            // populate m_mapStudentVictims
            String victimOid = incident.getVictimOid();
            if (!StringUtils.isEmpty(victimOid)) {
                if (m_mapStudentVictims == null) {
                    m_mapStudentVictims = new HashMap<String, Map<String, Set<ConductIncident>>>();
                }
                crdcCodeIncidents = m_mapStudentVictims.get(victimOid);
                if (crdcCodeIncidents == null) {
                    crdcCodeIncidents = new HashMap();
                    m_mapStudentVictims.put(victimOid, crdcCodeIncidents);
                }
                incidents = crdcCodeIncidents.get(crdcCode);
                if (incidents == null) {
                    incidents = new HashSet();
                    crdcCodeIncidents.put(crdcCode, incidents);
                }
                incidents.add(incident);
            }
        }
    }



    /**
     * Gets the data dictionary.
     *
     * @return Data dictionary
     */
    private DataDictionary getDataDictionary() {
        if (m_dictionary == null) {
            m_dictionary = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
        }
        return m_dictionary;
    }

    /**
     * Load actions.
     */
    private void loadActions() {
        if (!m_triedLoadActionsAlready) {
            m_triedLoadActionsAlready = true;

            DataDictionaryField aliasField =
                    getDataDictionary().findDataDictionaryFieldByAlias(CRDCReportData.ALIAS_CRDC_REF_CODE);
            ModelProperty prop =
                    new ModelProperty(ConductAction.class, ConductAction.COL_ACTION_CODE, m_broker.getPersistenceKey());
            DataDictionaryField field = getDataDictionary().findDataDictionaryField(prop.getFieldId());

            if (aliasField != null && !StringUtils.isEmpty(field.getReferenceTableOid())) {
                Map<String, String> crdcCodeLookup = CRDCReportData.getCRDCCodeLookup(m_broker,
                        aliasField.getJavaName(), field.getReferenceTableOid(), m_crdcActionCodes);

                // load Conduct Actions
                X2Criteria criteria = new X2Criteria();
                criteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_districtContext.getStartDate());
                criteria.addLessOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_districtContext.getEndDate());
                criteria.addIn(ConductAction.COL_ACTION_CODE, crdcCodeLookup.keySet());

                QueryByCriteria queryAction = new QueryByCriteria(ConductAction.class, criteria);
                QueryIterator actionIterator = m_broker.getIteratorByQuery(queryAction);
                try {
                    while (actionIterator.hasNext()) {
                        ConductAction action = (ConductAction) actionIterator.next();
                        addActionMap(crdcCodeLookup.get(action.getActionCode()), action);
                    }
                } finally {
                    actionIterator.close();
                }
            }
        }
    }

    /**
     * Load incidents.
     */
    private void loadIncidents() {
        if (!m_triedLoadIncidentsAlready) {
            m_triedLoadIncidentsAlready = true;

            DataDictionaryField aliasField =
                    getDataDictionary().findDataDictionaryFieldByAlias(CRDCReportData.ALIAS_CRDC_REF_CODE);
            ModelProperty prop = new ModelProperty(ConductIncident.class, ConductIncident.COL_INCIDENT_CODE,
                    m_broker.getPersistenceKey());
            DataDictionaryField field = getDataDictionary().findDataDictionaryField(prop.getFieldId());

            if (aliasField != null && !StringUtils.isEmpty(field.getReferenceTableOid())) {
                // Load conduct incident code lookup
                Map<String, String> crdcCodeLookup = CRDCReportData.getCRDCCodeLookup(m_broker,
                        aliasField.getJavaName(), field.getReferenceTableOid(), m_crdcIncidentCodes);

                // load Conduct Offenses
                X2Criteria criteria = new X2Criteria();
                criteria.addGreaterOrEqualThan(
                        ConductOffense.REL_INCIDENT + PATH_DELIMITER + ConductIncident.COL_INCIDENT_DATE,
                        m_districtContext.getStartDate());
                criteria.addLessOrEqualThan(
                        ConductOffense.REL_INCIDENT + PATH_DELIMITER + ConductIncident.COL_INCIDENT_DATE,
                        m_districtContext.getEndDate());
                criteria.addIn(ConductOffense.COL_INCIDENT_CODE, crdcCodeLookup.keySet());

                Map<String, Collection<ConductOffense>> offenses = m_broker.getGroupedCollectionByQuery(
                        new QueryByCriteria(ConductOffense.class, criteria), ConductOffense.COL_INCIDENT_OID, 256);

                // load Conduct Incidents
                X2Criteria orCriteria = new X2Criteria();
                X2Criteria criteriaIncident = new X2Criteria();
                criteriaIncident.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE,
                        m_districtContext.getStartDate());
                criteriaIncident.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_districtContext.getEndDate());

                X2Criteria criteriaAndCodesIncident = new X2Criteria();

                criteriaAndCodesIncident.addIn(ConductIncident.COL_INCIDENT_CODE, crdcCodeLookup.keySet());

                DataDictionaryField cndOffense2Field =
                        getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CND_OFFENCE_2);

                if (cndOffense2Field != null && !StringUtils.isEmpty(cndOffense2Field.getReferenceTableOid())) {
                    X2Criteria criteriaOrCodes2Incident = new X2Criteria();
                    Map<String, String> crdcOffense2CodeLookup = CRDCReportData.getCRDCCodeLookup(m_broker,
                            aliasField.getJavaName(), cndOffense2Field.getReferenceTableOid(), m_crdcIncidentCodes);
                    criteriaOrCodes2Incident.addIn(cndOffense2Field.getJavaName(), crdcOffense2CodeLookup.keySet());
                    criteriaAndCodesIncident.addOrCriteria(criteriaOrCodes2Incident);
                }

                DataDictionaryField cndOffense3Field =
                        getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CND_OFFENCE_3);

                if (cndOffense3Field != null && !StringUtils.isEmpty(cndOffense3Field.getReferenceTableOid())) {
                    X2Criteria criteriaOrCodes3Incident = new X2Criteria();
                    Map<String, String> crdcOffense3CodeLookup = CRDCReportData.getCRDCCodeLookup(m_broker,
                            aliasField.getJavaName(), cndOffense3Field.getReferenceTableOid(), m_crdcIncidentCodes);
                    criteriaOrCodes3Incident.addIn(cndOffense3Field.getJavaName(), crdcOffense3CodeLookup.keySet());
                    criteriaAndCodesIncident.addOrCriteria(criteriaOrCodes3Incident);
                }

                criteriaIncident.addAndCriteria(criteriaAndCodesIncident);
                orCriteria.addOrCriteria(criteriaIncident);

                X2Criteria criteriaOffense = new X2Criteria();
                criteriaOffense.addIn(X2BaseBean.COL_OID,
                        new SubQuery(ConductOffense.class, ConductOffense.COL_INCIDENT_OID, criteria));
                orCriteria.addOrCriteria(criteriaOffense);

                QueryByCriteria queryIncident = new QueryByCriteria(ConductIncident.class, orCriteria);
                QueryIterator incidentIterator = m_broker.getIteratorByQuery(queryIncident);
                try {
                    while (incidentIterator.hasNext()) {
                        ConductIncident incident = (ConductIncident) incidentIterator.next();
                        addIncidentMap(crdcCodeLookup.get(incident.getIncidentCode()), incident);
                        if (offenses.containsKey(incident.getOid())) {
                            for (ConductOffense offense : offenses.get(incident.getOid())) {
                                addIncidentMap(crdcCodeLookup.get(offense.getIncidentCode()), incident);
                            }
                        }
                    }
                } finally {
                    incidentIterator.close();
                }

            }
        }
    }
}
