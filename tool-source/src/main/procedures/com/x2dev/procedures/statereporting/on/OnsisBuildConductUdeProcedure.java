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
package com.x2dev.procedures.statereporting.on;

import com.follett.fsc.core.framework.persistence.CollectionCriteriaHelper;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolConductIncident;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolConductOffense;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchoolIncident;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.UserDefinedTableE;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.LoggerUtils;
import com.x2dev.utils.StringUtils;
import java.util.*;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * @author Follett Software Company
 * @copyright 2021
 */
public class OnsisBuildConductUdeProcedure extends ProcedureJavaSource {

    private static final long serialVersionUID = 1L;

    /*
     * Member data
     */
    private UserDataContainer m_userData = null;


    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {

        X2Broker broker = getBroker();
        Map<String, Object> params = getParameters();
        boolean isCommit = getBooleanParameter(params, "commit", false);

        String contextOid = getStringParameter(params, "contextOid", null);
        String schoolOidsStr = getStringParameter(params, "schoolOids", null);

        Collection<String> schoolOids = (StringUtils.isBlank(schoolOidsStr)) ? null
                : StringUtils.convertDelimitedStringToList(schoolOidsStr, ",", false);

        if (schoolOids == null || schoolOids.isEmpty()) {
            logMessage("ERROR: At least one school must be specified");
            return;
        }

        DistrictSchoolYearContext context = null;
        if (!StringUtils.isBlank(contextOid)) {
            context = broker.getBeanByOid(DistrictSchoolYearContext.class, contextOid);
        } else {
            context = getCurrentContext();
        }
        if (context == null) {
            logMessage("ERROR: Unable to determine school year for context [" + contextOid + "]");
            return;
        }

        try {
            broker.beginTransaction();
            doExecute(schoolOids, context, isCommit, broker);

        } catch (Exception e) {
            logMessage(LoggerUtils.convertThrowableToString(e));
        } finally {
            if (broker.isInTransaction()) {
                if (isCommit) {
                    logMessage("Transaction committed");
                    broker.commitTransaction();
                } else {
                    broker.rollbackTransaction();
                    logMessage("Transaction not committed");
                }
            }
        }
    }

    private static final String DDX_ID_INCIDENT = "ON-INCIDENT";

    public static final String ALIAS_UDE_INCIDENT_DATE = "inc-ude-incident-date";
    public static final String ALIAS_UDE_INCIDENT_TIME = "inc-ude-incident-time";
    public static final String ALIAS_UDE_INCIDENT_LOCATION = "inc-ude-incident-location";
    public static final String ALIAS_UDE_INCIDENT_ID = "inc-ude-incident-id";
    public static final String ALIAS_UDE_SCHOOL_OID = "inc-ude-school-oid";
    public static final String ALIAS_UDE_STAFF_OID = "inc-ude-staff-oid";
    public static final String ALIAS_UDE_INCIDENT_TYPE = "inc-ude-incident-type";
    public static final String ALIAS_UDE_INCIDENT_SCHOOL_NAME = "inc-ude-school-name";
    public static final String ALIAS_UDE_INCIDENT_SCHOOL_ID = "inc-ude-school-id";
    public static final String ALIAS_UDE_INCIDENT_SCHOOL_BSID = "inc-ude-school-bsid";
    public static final String ALIAS_UDE_INCIDENT_OFFENDER_VIEW = "inc-ude-offender-view";

    private static final String ALIAS_SCHOOL_BSID = "all-skl-BSID";

    public static final String COUNT_INCIDENT_IDS_FOR_SCHOOL = "Unique Incident IDs processed";
    public static final String COUNT_CND_FOR_SCHOOL = "Total Student Incidents (CND) found";
    public static final String COUNT_CND_ASSIGNED_ID =
            "Student Incidents (CND) missing Incident ID found by UDE and updated with ID";
    public static final String COUNT_CND_WITH_UDE = "Student Incidents (CND) with parent Incident (UDE)";
    public static final String COUNT_CND_WITHOUT_UDE = "Student Incidents (CND) missing parent Incident (UDE)";
    public static final String COUNT_UDE_CREATED = "Incidents records (UDE) added";



    /**
     *
     * @param schoolOids
     * @param context
     * @param isCommit
     * @param broker
     * @throws Exception
     */
    protected void doExecute(Collection<String> schoolOids,
                             DistrictSchoolYearContext context,
                             boolean isCommit,
                             X2Broker broker)
                                     throws Exception {
        for (String schoolOid : schoolOids) {
            processSchool(schoolOid, context, isCommit, broker);
        }
    }

    /**
     *
     * @param schoolOid
     * @param context
     * @param isCommit
     * @param broker
     * @throws Exception
     */
    protected void processSchool(String schoolOid,
                                 DistrictSchoolYearContext context,
                                 boolean isCommit,
                                 X2Broker broker)
                                         throws Exception {
        if (StringUtils.isBlank(schoolOid)) {
            logMessage("Blank schoolOid");
            return;
        }

        SisSchool school = broker.getBeanByOid(SisSchool.class, schoolOid);
        if (school == null) {
            logMessage("ERROR: Unable to locate school [" + schoolOid + "]");
            return;
        }
        logMessage("");
        logMessage("Processing school [" + school.getName() + "] for school year [" + context.getContextName() + "]");
        logMessage("");

        Map<String, Integer> counts = new LinkedHashMap<>();

        counts.put(COUNT_INCIDENT_IDS_FOR_SCHOOL, Integer.valueOf(0));
        counts.put(COUNT_CND_FOR_SCHOOL, Integer.valueOf(0));
        // counts.put(COUNT_CND_ASSIGNED_ID, Integer.valueOf(0));
        counts.put(COUNT_CND_WITH_UDE, Integer.valueOf(0));
        counts.put(COUNT_CND_WITHOUT_UDE, Integer.valueOf(0));
        counts.put(COUNT_UDE_CREATED, Integer.valueOf(0));

        //        ConductHelper conductHelper = new ConductHelper();
        DictionaryExtractor extractor = new DictionaryExtractor(broker);

        //        Collection<ConductIncident> incidentsForSchool = conductHelper.getSchoolIncidents(schoolOid, context, broker);
        Collection<ToolConductIncident> incidentsForSchool = getIncidents(schoolOid, broker, context);
        Map<String, List<ToolConductIncident>> conductIncidentsById = groupById(incidentsForSchool, broker);

        counts.put(COUNT_INCIDENT_IDS_FOR_SCHOOL, Integer.valueOf(conductIncidentsById.keySet().size()));

        for (String incidentId : conductIncidentsById.keySet()) {
            List<ToolConductIncident> incidentsForId = conductIncidentsById.get(incidentId);
            if (incidentsForId == null || incidentsForId.size() == 0) {
                continue;
            }

            add(COUNT_CND_FOR_SCHOOL, counts, incidentsForId.size());

            if (StringUtils.isBlank(incidentId)) {
                logMessage("Student Incident (CND) records missing Incident ID: " + incidentsForId);
                continue;
            }

            processIncidentsForId(incidentsForId, school, counts, extractor, broker);
        }

        dumpCounts(counts, school.getName());
    }


    /**
     * Gets incidents for school
     *
     * @param schoolOid
     * @param context
     * @param broker
     * @return Collection
     */
    private Collection<ToolConductIncident> getIncidents(String schoolOid, X2Broker broker,
                                                         DistrictSchoolYearContext context) {

        ToolBean.clearAllCachedToolBeans(ToolConductIncident.class);
        ToolBean.clearAllCachedToolBeans(ToolConductOffense.class);

        ToolBean.resetCriteria(broker, ToolConductIncident.class);

        DictionaryExtractor extractor = new DictionaryExtractor(broker);

        X2Criteria criteria = new X2Criteria();
        criteria.addGreaterOrEqualThan(ToolConductIncident.FIELD_INCIDENT_DATE.resolve(extractor),
                context.getStartDate());
        criteria.addLessOrEqualThan(ToolConductIncident.FIELD_INCIDENT_DATE.resolve(extractor),
                context.getEndDate());
        criteria.addEqualTo(ToolConductIncident.FIELD_SCHOOL_OID.resolve(extractor), schoolOid);

        FilterableFactory.create(broker, extractor, ToolConductIncident.class, criteria,
                Arrays.asList(ToolBean.FIELD_OID));

        ToolBean.preload(broker, extractor, Arrays.asList(ToolConductOffense.FIELD_OID),
                ToolConductIncident.CHILD_CONDUCT_OFFENSES);

        List<ToolConductIncident> incidents = ToolBean.getCachedToolBeans(ToolConductIncident.class).stream()
                .filter(incident -> !StringUtils.isEmpty(incident.getSchoolOid()))
                .filter(incident -> !StringUtils.isEmpty(incident.getIncidentId()))
                .filter(incident -> !StringUtils.isBlank(incident.getIncidentCode()))
                .filter(incident -> incident.getConductOffenses(broker).stream()
                        .anyMatch(offense -> !StringUtils.isBlank(offense.getIncidentCode())))
                .collect(Collectors.toList());

        return incidents;
    }

    /**
     *
     * @param incidentsForId
     * @param school
     * @param counts
     * @param extractor
     * @param broker
     */
    private void processIncidentsForId(List<ToolConductIncident> incidentsForId,
                                       SisSchool school,
                                       Map<String, Integer> counts,
                                       DictionaryExtractor extractor,
                                       X2Broker broker) {
        /*
         * Is there a UDE for this Incident ID?
         */
        DataDictionary udeDictionary = extractor.getDictionary(DDX_ID_INCIDENT);
        String udeOid = null;
        String stfOid = null;
        String schoolOid = school.getOid();
        String incidentId = null;
        ToolConductIncident firstConductIncident = null;

        Set<String> allUdeOids = new TreeSet<>();

        /*
         * Get the first ConductIncident in the set
         * to get the Incident Id
         * (they should all have the same Incident ID at this point).
         *
         * Get the first non-blank Staff Oid to put on the UDE if we create one.
         *
         * Collect all unique UDE OIDs and check that all CND point to the same UDE.
         */
        for (ToolConductIncident conductIncident : incidentsForId) {
            if (firstConductIncident == null) {
                firstConductIncident = conductIncident;
                incidentId = conductIncident.getIncidentId();
            }

            if (StringUtils.isBlank(stfOid)) {
                stfOid = conductIncident.getReferralStaffOid();
            }

            String testUdeOid = conductIncident.getUserDefinedTableEOid();
            if (!StringUtils.isBlank(testUdeOid)) {
                allUdeOids.add(testUdeOid);
            }
        }

        if (allUdeOids.size() > 1) {
            logMessage("ERROR: Student Incident (CND) records for Incident ID [" + incidentId
                    + "] point to different Incident (UDE) records " + allUdeOids);
            return;
        }

        if (allUdeOids.size() == 1) {
            udeOid = allUdeOids.iterator().next();
        }

        /*
         * Load the UDE
         */
        UserDefinedTableE ude = null;
        if (!StringUtils.isBlank(udeOid)) {
            ude = broker.getBeanByOid(UserDefinedTableE.class, udeOid);

            /*
             * If CND points to an invalid UDE OID,
             * tell user and skip
             */
            if (ude == null) {
                logMessage("ERROR: Unable to load Incident [" + udeOid
                        + "]. Skipping Incident ID [" + incidentId + "]");
                return;
            }
        }

        /*
         * If no UDE based on OID, search for one based on Incident ID + SchoolOid
         */
        if (ude == null) {
            X2Criteria criteria = new X2Criteria();

            criteria.addEqualTo(OnSchoolIncident.FIELD_INCIDENT_ID.resolve(extractor), incidentId);
            criteria.addEqualTo(OnSchoolIncident.FIELD_SCHOOL_OID.resolve(extractor), schoolOid);
            QueryByCriteria query = new QueryByCriteria(UserDefinedTableE.class, criteria);
            ude = broker.getBeanByQuery(query);
        }

        /*
         * If still no UDE, create a UDE record
         */
        if (ude == null) {
            try {
                String schoolName = school.getName();
                logMessage("Creating UDE for Incident ID [" + incidentId
                        + "] School [" + schoolName + "]");

                ude = X2BaseBean.newInstance(UserDefinedTableE.class, udeDictionary);
                OrganizationManager.cloneOrganizationOids(ude, school);
                extractor.setAliasAsJavaType(ude, ALIAS_UDE_INCIDENT_DATE, firstConductIncident.getIncidentDate(),
                        DDX_ID_INCIDENT);
                extractor.setAliasAsJavaType(ude, ALIAS_UDE_INCIDENT_TIME, firstConductIncident.getIncidentTime(),
                        DDX_ID_INCIDENT);
                ude.setFieldValueByAlias(ALIAS_UDE_INCIDENT_LOCATION, firstConductIncident.getIncidentLocation(),
                        udeDictionary);
                ude.setFieldValueByAlias(ALIAS_UDE_INCIDENT_ID, incidentId, udeDictionary);

                ude.setFieldValueByAlias(ALIAS_UDE_SCHOOL_OID, schoolOid, udeDictionary);
                ude.setFieldValueByAlias(ALIAS_UDE_INCIDENT_SCHOOL_NAME, school.getName(), udeDictionary);
                ude.setFieldValueByAlias(ALIAS_UDE_INCIDENT_SCHOOL_ID, school.getSchoolId(), udeDictionary);
                ude.setFieldValueByAlias(ALIAS_UDE_INCIDENT_SCHOOL_BSID, school.getFieldValueByAlias(ALIAS_SCHOOL_BSID),
                        udeDictionary);

                if (!StringUtils.isBlank(stfOid)) {
                    ude.setFieldValueByAlias(ALIAS_UDE_STAFF_OID, stfOid, udeDictionary);
                }
                broker.saveBeanForced(ude);
                udeOid = ude.getOid();
                increment(COUNT_UDE_CREATED, counts);
                logMessage("Created UDE [" + udeOid + "]");
            } catch (Exception e) {
                logMessage("ERROR: Unable to create/populate UDE: " + LoggerUtils.convertThrowableToString(e));
            }
        }

        if (ude == null) {
            return;
        }

        udeOid = ude.getOid();

        /*
         * Gather/dedup any CND that might already point to UDE
         * even if they're missing the Incident ID
         */
        Set<ConductIncident> gatheredConducts = new HashSet<>();


        // preload ConductIncident
        Set<String> conductIncidentOids =
                incidentsForId.stream().map(incident -> incident.getOid()).collect(Collectors.toSet());
        X2Criteria incidentCcriteria = new X2Criteria();
        CollectionCriteriaHelper helper = null;
        try {
            if (conductIncidentOids.size() > ToolBean.MAX_SAFE_PARAMETERS) {
                helper = new CollectionCriteriaHelper(conductIncidentOids, getBroker());
                helper.applyToCriteria(SisBeanPaths.STUDENT_CONDUCT_INCIDENT.oid().getPath(), incidentCcriteria);
            } else {
                incidentCcriteria.addIn(SisBeanPaths.STUDENT_CONDUCT_INCIDENT.oid().getPath(), conductIncidentOids);
            }
            QueryByCriteria query = new QueryByCriteria(ConductIncident.class, incidentCcriteria);
            gatheredConducts.addAll(getBroker().getCollectionByQuery(query));
        } finally {
            if (helper != null) {
                helper.cleanup();
            }
        }

        gatheredConducts.addAll(ude.getConductIncidents());

        /*
         * Apply the UDE OID to any CND that are missing it
         */
        boolean isChanged = false;
        for (ConductIncident conductIncident : gatheredConducts) {
            SisStudent student = conductIncident.getStudent();
            String studentName = (student == null) ? conductIncident.getStudentOid() : student.getNameView();

            String testCndIncidentId = conductIncident.getIncidentId();
            if (StringUtils.isBlank(testCndIncidentId)) {
                conductIncident.setIncidentId(incidentId);
                if (conductIncident.isDirty()) {
                    isChanged = true;
                    broker.saveBeanForced(conductIncident);
                    increment(COUNT_CND_ASSIGNED_ID, counts);
                    logMessage("Assigned Incident ID [" + incidentId + "] to CND [" + conductIncident.getOid()
                    + "] Student [" + studentName + "]");
                }
            }

            String testUdeOid = conductIncident.getUserDefinedTableEOid();
            if (!StringUtils.isBlank(testUdeOid)) {
                increment(COUNT_CND_WITH_UDE, counts);
                continue;
            }

            conductIncident.setUserDefinedTableEOid(udeOid);
            if (conductIncident.isDirty()) {
                isChanged = true;
                broker.saveBeanForced(conductIncident);
                increment(COUNT_CND_WITHOUT_UDE, counts);
                logMessage("Assigned UDE [" + udeOid + "] to CND [" + conductIncident.getOid() + "] Incident ID ["
                        + testCndIncidentId + "] Student [" + studentName + "]");
            }
        }

        /*
         * Rebuild the offender name view
         * See IncidentDynamicFormProcedure.java
         */
        DataDictionaryField offenderViewField =
                udeDictionary.findDataDictionaryFieldByAlias(ALIAS_UDE_INCIDENT_OFFENDER_VIEW);

        if (ude != null && isChanged && offenderViewField != null) {
            TreeSet<String> offenderNames = new TreeSet<>();
            for (ConductIncident conductIncident : gatheredConducts) {
                String offenderName = conductIncident.getStudent().getNameView();
                offenderNames.add(offenderName);
            }

            StringBuffer offenderView = new StringBuffer();

            for (String offenderName : offenderNames) {
                if (offenderView.length() > 0) {
                    offenderView.append("; ");
                }
                offenderView.append(offenderName);
            }
            ude.setFieldValueByAlias(ALIAS_UDE_INCIDENT_OFFENDER_VIEW, offenderView.toString(), udeDictionary);
            if (ude.isDirty()) {
                broker.saveBeanForced(ude);
            }
        }
    }

    /**
     * @param incidentsForSchool
     * @param broker
     * @return
     */
    private Map<String, List<ToolConductIncident>> groupById(Collection<ToolConductIncident> schoolIncidents,
                                                             X2Broker broker) {
        Map<String, List<ToolConductIncident>> incidentsById = new HashMap<>();

        if (schoolIncidents == null) {
            return incidentsById;
        }

        for (ToolConductIncident schoolIncident : schoolIncidents) {
            String incidentId = schoolIncident.getIncidentId();
            List<ToolConductIncident> incidentsForId = incidentsById.get(incidentId);
            if (incidentsForId == null) {
                incidentsForId = new ArrayList<>();
                incidentsById.put(incidentId, incidentsForId);
            }

            incidentsForId.add(schoolIncident);
        }

        return incidentsById;
    }

    /**
     *
     * @param key
     * @param map
     */
    private void increment(String key, Map<String, Integer> map) {
        add(key, map, 1);
    }

    /**
     *
     * @param key
     * @param map
     * @param delta
     */
    private void add(String key, Map<String, Integer> map, int delta) {
        Integer origValue = map.get(key);
        if (origValue == null) {
            origValue = Integer.valueOf(0);
        }
        Integer newValue = origValue + delta;
        map.put(key, newValue);
    }

    /**
     *
     * @param params
     * @param paramName
     * @param defaultValue
     * @return
     */
    private static boolean getBooleanParameter(Map<String, Object> params, String paramName, boolean defaultValue) {
        Boolean param = (Boolean) params.get(paramName);
        return (param != null) ? param.booleanValue() : defaultValue;
    }

    /**
     *
     * @param params
     * @param paramName
     * @param defaultValue
     * @return
     */
    private static String getStringParameter(Map<String, Object> params, String paramName, String defaultValue) {
        Object param = params.get(paramName);
        if (!(param instanceof String)) {
            param = null;
        }
        return (param != null) ? (String) param : defaultValue;
    }

    /**
     *
     * @param counts
     * @param schoolName
     */
    private void dumpCounts(Map<String, Integer> counts, String schoolName) {
        logMessage("");
        logMessage("Counts for " + schoolName);
        int maxMsgLen = 10;
        for (String key : counts.keySet()) {
            maxMsgLen = Math.max(maxMsgLen, key.length() + 3);
        }

        for (String key : counts.keySet()) {
            Integer count = counts.get(key);
            if (count == null) {
                count = Integer.valueOf(0);
            }
            logMessage(StringUtils.padRight(key + ": ", maxMsgLen) + count);
        }

        logMessage(StringUtils.repeat("-", maxMsgLen + 6));
    }
}
