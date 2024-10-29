/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2011 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.ny;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.ConductOffense;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleBell;
import com.x2dev.sis.model.beans.ScheduleBellPeriod;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.DateRange;
import com.x2dev.utils.types.PlainTime;
import java.util.*;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class NYSummaryIncidentsReportData.
 */
public class NYSummaryIncidentsReportData extends ReportJavaSourceNet {

    /**
     * The Class SchoolNameComparator.
     */
    public class SchoolNameComparator implements Comparator<SisSchool> {

        /**
         * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
         */
        @Override
        public int compare(SisSchool school1, SisSchool school2) {
            return school1.getName().compareToIgnoreCase(school2.getName());
        }

    }

    /**
     * The Class CategoryCounter.
     */
    public class CategoryCounter {
        private Map<String, Integer> m_counter = new HashMap<String, Integer>();

        /**
         * Instantiates a new category counter.
         */
        CategoryCounter() {
            // init
            m_counter.put(COUNTER_NUM_OF_INCIDENTS, Integer.valueOf(0));
            m_counter.put(COUNTER_NUM_OF_STD_OFFENDERS, Integer.valueOf(0));
            m_counter.put(COUNTER_NUM_OF_STF_OFFENDERS, Integer.valueOf(0));
            m_counter.put(COUNTER_NUM_OF_OTH_OFFENDERS, Integer.valueOf(0));
            m_counter.put(COUNTER_NUM_OF_STD_VICTIMS, Integer.valueOf(0));
            m_counter.put(COUNTER_NUM_OF_STF_VICTIMS, Integer.valueOf(0));
            m_counter.put(COUNTER_NUM_OF_OTH_VICTIMS, Integer.valueOf(0));
            m_counter.put(COUNTER_NUM_OF_INC_WITH_DRUGS, Integer.valueOf(0));
            m_counter.put(COUNTER_NUM_OF_INC_ON_TRANS, Integer.valueOf(0));
            m_counter.put(COUNTER_NUM_OF_STD_COUNSELING, Integer.valueOf(0));
            m_counter.put(COUNTER_NUM_OF_STD_REMOVAL, Integer.valueOf(0));
            m_counter.put(COUNTER_NUM_OF_STD_SUSPENSION, Integer.valueOf(0));
            m_counter.put(COUNTER_NUM_OF_STD_OUT_OF_SKL, Integer.valueOf(0));
            m_counter.put(COUNTER_NUM_OF_STD_ALT_ED, Integer.valueOf(0));
            m_counter.put(COUNTER_NUM_OF_STD_LAW, Integer.valueOf(0));
        }

        /**
         * Adds the value.
         *
         * @param key String
         * @param value Integer
         */
        public void addValue(String key, Integer value) {
            Integer oldValue = m_counter.get(key);
            m_counter.put(key, Integer.valueOf(oldValue.intValue() + value.intValue()));
        }

        /**
         * Gets the value.
         *
         * @param key String
         * @return Integer
         */
        public Integer getValue(String key) {
            return m_counter.get(key);
        }

        /**
         * Gets the counts map.
         *
         * @return Map
         */
        public Map getCountsMap() {
            return m_counter;
        }
    }

    private static final String ALIAS_BIAS_RELATED = "DOE BIAS RELATED";
    private static final String ALIAS_DRUGS_INVOLVED = "DOE INVOLVED ALCOHOL OR DRUGS";
    private static final String ALIAS_GANG_RELATED = "DOE GANG RELATED";
    private static final String ALIAS_VICTIM_NAME = "DOE VICTIM NAME";
    private static final String ALIAS_VICTIM_STAFF = "DOE VICTIM STAFF";
    private static final String ALIAS_WEAPON_INVOLVED = "DOE INVOLVED WEAPON";
    private static final String ALIAS_WEAPON_TYPE = "DOE WEAPON TYPE";
    private static final String ALIAS_BEDS_CODE = "BEDS CODE";
    // private static final String ALIAS_POLICE_OR_SAFETY =
    // "DOE POLICE OR SAFETY";

    private static final String COUNTER_NUM_OF_INCIDENTS = "a";
    private static final String COUNTER_NUM_OF_STD_OFFENDERS = "b";
    private static final String COUNTER_NUM_OF_STF_OFFENDERS = "c";
    private static final String COUNTER_NUM_OF_OTH_OFFENDERS = "d";
    private static final String COUNTER_NUM_OF_STD_VICTIMS = "e";
    private static final String COUNTER_NUM_OF_STF_VICTIMS = "f";
    private static final String COUNTER_NUM_OF_OTH_VICTIMS = "g";
    private static final String COUNTER_NUM_OF_INC_WITH_DRUGS = "h";
    private static final String COUNTER_NUM_OF_INC_ON_TRANS = "i";
    private static final String COUNTER_NUM_OF_STD_COUNSELING = "j";
    private static final String COUNTER_NUM_OF_STD_REMOVAL = "k";
    private static final String COUNTER_NUM_OF_STD_SUSPENSION = "l";
    private static final String COUNTER_NUM_OF_STD_OUT_OF_SKL = "m";
    private static final String COUNTER_NUM_OF_STD_ALT_ED = "n";
    private static final String COUNTER_NUM_OF_STD_LAW = "o";

    private static final String KEY_IS_AT_SCHOOL_FUNCTION = "isAtSchoolFunction";
    private static final String KEY_IS_BIAS_RELATED = "isBiasRelated";
    private static final String KEY_IS_GANG_RELATED = "isGangRelated";
    private static final String KEY_IS_INTIMIDATION = "isIntimidation";
    private static final String KEY_IS_ON_SCHOOL_PROPERTY = "isOnSchoolProperty";
    private static final String KEY_IS_ON_SCHOOL_TIME = "isOnSchoolTime";
    private static final String KEY_IS_NOT_ON_SCHOOL_TIME = "isNotOnSchoolTime";
    private static final String KEY_IS_ON_TRANSPORT = "isOnTransport";
    private static final String KEY_IS_WITH_DRUGS = "isWithDrugs";
    private static final String KEY_IS_WITH_WEAPON = "isWithWeapon";
    private static final String KEY_SECTION_TWO = "section2";

    private static final String KEY_HANDGUNS = "5A1";
    private static final String KEY_RIFLES = "5A2";
    private static final String KEY_OTHER_FIREARMS = "5A3";
    private static final String KEY_KNIVES = "5A4";
    private static final String KEY_CHEMICAL_AGENTS = "5A5";
    private static final String KEY_OTHER_WEAPONS = "5A6";

    private static final String RKEY_BEDS_NUMBER = "bedsNumber";
    private static final String FIELD_GROUP_NAME = "groupName";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_SCHOOL_NAME = "schoolName";
    private static final String FIELD_CATEGORY_NAME = "categoryName";
    private static final String FIELD_CATEGORY_CODE = "categoryCode";
    private static final String FIELD_CATEGORY_COMMENT = "categoryComment";
    private static final String FIELD_CATEGORY_COUNTS = "categoryCounts";
    private static final String FIELD_SUB_CATEGORY_NAME = "subCategoryName";

    private static final String PARAM_CONTEXT = "context";

    private static final String IPARAM_CONTEXT_OID = "contextOid";
    private static final String IPARAM_DELIMITER = "delimiter";
    private static final String IPARAM_INCIDENTS_STATE_CODE = "incidentsStateCodes";
    private static final String IPARAM_HAS_WEAPON_SUB_CATEGORIE = "hasWeaponSubCategorie";
    private static final String IPARAM_ACTIONS_STATE_CODES = "actionsStateCodes";
    private static final String IPARAM_WEAPON_OR_ACTION_RULE = "incidentsWeaponOrActionRule";

    // private static final String STATE_CODE_EVENT = "EVENT";
    private static final String STATE_CODE_INTIMIDATION = "10A";
    private static final String STATE_CODE_OFFSCHOOL = "OFFSCHOOL";
    private static final String STATE_CODE_TRANSPORTATION = "TRANS";
    private static final String STATE_CODE_WEAPON_POSSESSION = "17A";

    private static final String SUB_CAT_WITH_WEAPON = "withWeapon";
    private static final String SUB_CAT_WITHOUT_WEAPON = "withoutWeapon";
    private static final String SUB_CAT_WEAPON_SCREENING = "17.1A";
    private static final String SUB_CAT_WEAPON_OTHER = "17.2A";

    private HashSet<String> m_actionStateCodes;
    private DistrictSchoolYearContext m_context;
    private DataDictionary m_dict;
    private DataDictionaryField m_dictFieldWeapon;
    private Map<String, List<String>> m_groupedCategory;
    private ArrayList<String> m_groupSortedList;
    private ScheduleManager m_scheduleManager;
    private HashSet<String> m_incidentStateCodes;
    private HashSet<String> m_incidentsWithWeaponOrActionRule;
    private HashSet<String> m_hasWeaponSubCategory;
    private String m_rtbCndAction;
    private String m_rtbCndIncident;
    private String m_rtbCndLocation;
    private HashSet<String> m_secFifthStateCodes;
    private Map<String, CategoryCounter> m_secOneCounters;
    private Map<String, HashSet<SisStudent>> m_studentsCounts;
    private Map<String, Integer> m_otherSectionCounters;
    private List<String> m_otherSectionsKeys;

    /**
     * Only include incidents where the cndIncident has a state reference code in
     * the set {1A,2.1A,2.2A,3A,4A,5A,6A,7A,8A,9A,10A,11A,12A,13A,14A,15A,16A,17A,18A,19A,20A}
     * and cndIncidentDate between PARAM_START_DATE and PARAM_END_DATE
     *
     * @return QueryByCriteria
     */
    protected QueryByCriteria buildQuery() {
        X2Criteria criteria = new X2Criteria();
        criteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_context.getStartDate());
        criteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_context.getEndDate());
        if (isSchoolContext()) {
            criteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addNotEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    BooleanAsStringConverter.TRUE);
            criteria.addNotEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    BooleanAsStringConverter.TRUE);
        }

        HashSet incidentsCodes = new HashSet();
        // Place a non-matching value in the list to insure criteria operates
        // properly when there are no matching codes
        incidentsCodes.add("##dummy##");
        ReferenceTable refTable = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, m_rtbCndIncident);
        if (refTable != null) {
            Collection<ReferenceCode> codes = refTable.getReferenceCodes(getBroker());
            for (ReferenceCode code : codes) {
                if (m_incidentStateCodes.contains(code.getStateCode())) {
                    incidentsCodes.add(code.getCode());
                }
            }
        }
        criteria.addIn(ConductIncident.COL_INCIDENT_CODE, incidentsCodes);
        return new QueryByCriteria(ConductIncident.class, criteria);
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        HashSet<SisSchool> schools = new HashSet<SisSchool>();
        Map<String, List<ConductIncident>> incidentsGroups = null;
        incidentsGroups = getBroker().getGroupedCollectionByQuery(buildQuery(), ConductIncident.COL_INCIDENT_ID, 100);

        for (List<ConductIncident> incidentsGroup : incidentsGroups.values()) {
            // We can assume that all incidents in group represent the same
            // incident, except special flags and conditionals.
            // They need to be checked at each sub-incident.
            ConductIncident incident = incidentsGroup.get(0);
            SisSchool school = incident.getSchool();
            schools.add(school);
            String incidentCategory = m_dict.findStateReferenceCode(m_rtbCndIncident, incident.getIncidentCode());

            Map<String, Boolean> conditions = new TreeMap<String, Boolean>();
            Map<String, Integer> incidentSecOneCounters = new TreeMap<String, Integer>();

            parseIncidentGroup(incidentsGroup, conditions, incidentSecOneCounters);

            // Incidents of categories 9-13, 16, 20 include only if weapon were
            // involved or incident results in disciplinary action.
            boolean incidentResultActions = true;
            if (m_incidentsWithWeaponOrActionRule.contains(incidentCategory)) {
                incidentResultActions = (incidentSecOneCounters.get(COUNTER_NUM_OF_STD_COUNSELING).intValue() +
                        incidentSecOneCounters.get(COUNTER_NUM_OF_STD_ALT_ED).intValue() +
                        incidentSecOneCounters.get(COUNTER_NUM_OF_STD_LAW).intValue() +
                        incidentSecOneCounters.get(COUNTER_NUM_OF_STD_OUT_OF_SKL).intValue() +
                        incidentSecOneCounters.get(COUNTER_NUM_OF_STD_REMOVAL).intValue() +
                        incidentSecOneCounters.get(COUNTER_NUM_OF_STD_SUSPENSION).intValue()) > 0;

                // Section 2
                // flag what incident was included in section 2
                if ("10A".equals(incidentCategory)) {
                    if (!incidentResultActions) {
                        increaseSectionCounter(school.getSchoolId() + KEY_SECTION_TWO);
                    }
                }

                if (!incidentResultActions && !conditions.get(KEY_IS_WITH_WEAPON).booleanValue()) {
                    continue;
                }
            }

            // Section 1
            CategoryCounter categorieCounter = getCategoryCounter(school, incidentCategory,
                    conditions.get(KEY_IS_WITH_WEAPON));

            categorieCounter.addValue(COUNTER_NUM_OF_INCIDENTS, Integer.valueOf(1));
            if (conditions.get(KEY_IS_ON_TRANSPORT).booleanValue()) {
                categorieCounter.addValue(COUNTER_NUM_OF_INC_ON_TRANS, Integer.valueOf(1));
            }
            if (conditions.get(KEY_IS_WITH_DRUGS).booleanValue()) {
                categorieCounter.addValue(COUNTER_NUM_OF_INC_WITH_DRUGS, Integer.valueOf(1));
            }

            for (Entry counter : incidentSecOneCounters.entrySet()) {
                categorieCounter.addValue((String) counter.getKey(), (Integer) counter.getValue());
            }


            // Section 3
            // Currently only student offenders are tracked by aspen.
            // Blank section

            // Section 4
            // Here need to exclude incidents what
            // will be reported in section 2
            if (incidentResultActions) {
                if (conditions.get(KEY_IS_BIAS_RELATED).booleanValue()) {
                    increaseSectionCounter(school.getSchoolId() + KEY_IS_BIAS_RELATED);
                }
                if (conditions.get(KEY_IS_GANG_RELATED).booleanValue()) {
                    increaseSectionCounter(school.getSchoolId() + KEY_IS_GANG_RELATED);
                }
                if (conditions.get(KEY_IS_ON_SCHOOL_PROPERTY).booleanValue()) {
                    increaseSectionCounter(school.getSchoolId() + KEY_IS_ON_SCHOOL_PROPERTY);
                } else {
                    increaseSectionCounter(school.getSchoolId() + KEY_IS_AT_SCHOOL_FUNCTION);
                }
                if (conditions.get(KEY_IS_ON_SCHOOL_TIME).booleanValue()) {
                    increaseSectionCounter(school.getSchoolId() + KEY_IS_ON_SCHOOL_TIME);
                } else {
                    increaseSectionCounter(school.getSchoolId() + KEY_IS_NOT_ON_SCHOOL_TIME);
                }
            }

            // Section 5. Weapon offenders
            if (m_secFifthStateCodes.contains(incidentCategory) && conditions.get(KEY_IS_WITH_WEAPON).booleanValue() ||
                    SUB_CAT_WEAPON_SCREENING.equals(incidentCategory)
                    || SUB_CAT_WEAPON_OTHER.equals(incidentCategory)) {
                countWeaponOffenders(school, incidentsGroup);
            }

            // Section 6
            // Not enough information. Blank Section.

            // Section 7
            // Not enough information. Blank Section.

            // Section 8
            // Not enough information. Blank Section.

            // Section 9
            // Not enough information. Blank Section.
        }
        return prepareDataGrid(schools);
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_groupedCategory = new HashMap();
        m_dict = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        DataDictionaryField field =
                m_dict.findDataDictionaryField(ConductIncident.class.getName(), ConductIncident.COL_INCIDENT_CODE);
        m_rtbCndIncident = field.getReferenceTableOid();

        field = m_dict.findDataDictionaryField(ConductIncident.class.getName(), ConductIncident.COL_INCIDENT_LOCATION);
        m_rtbCndLocation = field.getReferenceTableOid();

        field = m_dict.findDataDictionaryField(ConductAction.class.getName(), ConductAction.COL_ACTION_CODE);
        m_rtbCndAction = field.getReferenceTableOid();

        m_dictFieldWeapon = m_dict.findDataDictionaryFieldByAlias(ALIAS_WEAPON_TYPE);
        m_scheduleManager = new ScheduleManager(getBroker());

        char delimiter = ((String) getParameter(IPARAM_DELIMITER)).charAt(0);
        m_incidentStateCodes =
                new HashSet(StringUtils.convertDelimitedStringToList((String) getParameter(IPARAM_INCIDENTS_STATE_CODE),
                        delimiter));
        m_hasWeaponSubCategory = new HashSet(
                StringUtils.convertDelimitedStringToList((String) getParameter(IPARAM_HAS_WEAPON_SUB_CATEGORIE),
                        delimiter));
        m_actionStateCodes =
                new HashSet(StringUtils.convertDelimitedStringToList((String) getParameter(IPARAM_ACTIONS_STATE_CODES),
                        delimiter));
        m_incidentsWithWeaponOrActionRule = new HashSet(
                StringUtils.convertDelimitedStringToList((String) getParameter(IPARAM_WEAPON_OR_ACTION_RULE),
                        delimiter));

        m_secOneCounters = new HashMap<String, CategoryCounter>();
        m_studentsCounts = new HashMap<String, HashSet<SisStudent>>();
        m_secFifthStateCodes = new HashSet<String>(m_hasWeaponSubCategory);
        m_secFifthStateCodes.add(STATE_CODE_WEAPON_POSSESSION);

        m_otherSectionCounters = new HashMap<String, Integer>();

        m_otherSectionsKeys = Arrays.asList(KEY_SECTION_TWO, KEY_IS_AT_SCHOOL_FUNCTION, KEY_IS_BIAS_RELATED,
                KEY_IS_GANG_RELATED, KEY_IS_NOT_ON_SCHOOL_TIME, KEY_IS_ON_SCHOOL_TIME,
                KEY_IS_ON_SCHOOL_PROPERTY, KEY_IS_ON_TRANSPORT, KEY_CHEMICAL_AGENTS,
                KEY_HANDGUNS, KEY_KNIVES, KEY_OTHER_FIREARMS, KEY_OTHER_WEAPONS, KEY_RIFLES);

        m_groupSortedList = new ArrayList<String>();
        String groupName = "";
        int i = 0;
        while (groupName != null) {
            i++;
            groupName = (String) getParameter("group" + String.valueOf(i) + "name");
            if (groupName != null) {
                String categoriesGroupingInput = (String) getParameter("group" + String.valueOf(i) + "categories");
                List<String> categoriesGrouping = StringUtils.convertDelimitedStringToList(categoriesGroupingInput,
                        delimiter);
                m_groupSortedList.add(groupName);
                m_groupedCategory.put(groupName, categoriesGrouping);
            }
        }

        String contextOid = (String) getParameter(IPARAM_CONTEXT_OID);
        m_context = (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class, contextOid);
        addParameter(PARAM_CONTEXT, m_context);

    }

    /**
     * Determine count's of unique offenders for each weapon type.
     *
     * @param school SisSchool
     * @param incidentsGroup List<ConductIncident>
     */
    private void countWeaponOffenders(SisSchool school, List<ConductIncident> incidentsGroup) {
        for (ConductIncident incident : incidentsGroup) {
            String weaponType = (String) incident.getFieldValueByBeanPath(m_dictFieldWeapon.getJavaName());
            String code = m_dict.findStateReferenceCode(m_dictFieldWeapon.getReferenceTableOid(), weaponType);
            if (!StringUtils.isEmpty(code)) {
                String key = school.getSchoolId() + code;
                HashSet<SisStudent> students = m_studentsCounts.get(key);
                if (students == null) {
                    students = new HashSet<SisStudent>();
                }
                students.add(incident.getStudent());
                m_studentsCounts.put(key, students);
            }
        }
    }

    /**
     * Determine conditions needed for section 1.
     *
     * @param conditions Map<String,Boolean>
     * @param dayTime DateRange
     * @param incident ConductIncident
     */
    private void determineConditions(Map<String, Boolean> conditions, DateRange dayTime, ConductIncident incident) {
        // init conditions
        conditions.put(KEY_IS_BIAS_RELATED, Boolean.FALSE);
        conditions.put(KEY_IS_GANG_RELATED, Boolean.FALSE);
        conditions.put(KEY_IS_INTIMIDATION, Boolean.FALSE);
        conditions.put(KEY_IS_ON_SCHOOL_PROPERTY, Boolean.TRUE);
        conditions.put(KEY_IS_ON_SCHOOL_TIME, Boolean.FALSE);
        conditions.put(KEY_IS_ON_TRANSPORT, Boolean.FALSE);
        conditions.put(KEY_IS_WITH_DRUGS, Boolean.FALSE);
        conditions.put(KEY_IS_WITH_WEAPON, Boolean.FALSE);

        String stateLocationCode = m_dict.findStateReferenceCode(m_rtbCndLocation, incident.getIncidentLocation());

        /*
         * Now only if DOE INCIDENT LOCATION state code is OFFSCHOOL.
         * then not on School Property/
         */
        if (STATE_CODE_OFFSCHOOL.equals(stateLocationCode)) {
            conditions.put(KEY_IS_ON_SCHOOL_PROPERTY, Boolean.FALSE);
        }

        if (STATE_CODE_TRANSPORTATION.equals(stateLocationCode)) {
            conditions.put(KEY_IS_ON_TRANSPORT, Boolean.TRUE);
        }

        if (BooleanAsStringConverter.TRUE.equals(incident.getFieldValueByAlias(ALIAS_BIAS_RELATED))) {
            conditions.put(KEY_IS_BIAS_RELATED, Boolean.TRUE);
        }

        if (BooleanAsStringConverter.TRUE.equals(incident.getFieldValueByAlias(ALIAS_GANG_RELATED))) {
            conditions.put(KEY_IS_GANG_RELATED, Boolean.TRUE);
        }

        if (BooleanAsStringConverter.TRUE.equals(incident.getFieldValueByAlias(ALIAS_DRUGS_INVOLVED))) {
            conditions.put(KEY_IS_WITH_DRUGS, Boolean.TRUE);
        }

        if (BooleanAsStringConverter.TRUE.equals(incident.getFieldValueByAlias(ALIAS_WEAPON_INVOLVED))) {
            conditions.put(KEY_IS_WITH_WEAPON, Boolean.TRUE);
        }

        // if dayTime not identified, assume that incident is not at school
        // time
        if (dayTime != null) {
            if (incident.getIncidentTime() == null || !dayTime.contains(incident.getIncidentTime())) {
                conditions.put(KEY_IS_ON_SCHOOL_TIME, Boolean.TRUE);
            }
        } else {
            conditions.put(KEY_IS_ON_SCHOOL_TIME, Boolean.TRUE);
        }

        // Need to check offenses at each incident.
        // Reason that each incident may have another student with offenses.
        for (ConductOffense offense : incident.getConductOffenses()) {
            String stateCode = m_dict.findStateReferenceCode(m_rtbCndIncident, offense.getIncidentCode());
            if (STATE_CODE_INTIMIDATION.equals(stateCode)) {
                conditions.put(KEY_IS_INTIMIDATION, Boolean.TRUE);
            }
        }
    }

    /**
     * The incident is during school hours if the incident time is between
     * the earliest start time and the latest end time for the ScheduleBellPeriods.
     * To retrieve these times for an incident:
     * a) retrieve the active schedule for the student's school
     * b) use the ScheduleManager.getStudentBellSchedule() method to return the ScheduleBell for the
     * student on the incident date.
     * c) iterate the ScheduleBellPeriods for the ScheduleBell gathering earliest start time and
     * lates end time
     * if no bell schedule is found, the time is before or after school hours.
     *
     * @param incidentsGroup List<ConductIncident>
     * @return DateRange
     */
    private DateRange determineSchoolTime(List<ConductIncident> incidentsGroup) {
        ConductIncident incident = incidentsGroup.get(0);
        Schedule schedule = incident.getSchool().getActiveSchedule();
        ScheduleBell scheduleBell = m_scheduleManager.getStudentBellSchedule(incident.getStudentOid(), schedule,
                incident.getIncidentDate());
        PlainTime startDayTime = null;
        PlainTime endDayTime = null;
        if (scheduleBell != null) {
            for (ScheduleBellPeriod period : scheduleBell.getScheduleBellPeriods()) {
                if (startDayTime == null) {
                    startDayTime = period.getStartTime();
                } else if ((period.getStartTime() != null) &&
                        startDayTime.after(period.getStartTime())) {
                    startDayTime = period.getStartTime();
                }
                if (endDayTime == null) {
                    endDayTime = period.getEndTime();
                } else if ((period.getEndTime() != null) &&
                        endDayTime.before(period.getEndTime())) {
                    endDayTime = period.getEndTime();
                }
            }
        }
        if (startDayTime != null && endDayTime != null) {
            return new DateRange(startDayTime, endDayTime);
        }
        return null;
    }

    /**
     * Take care about weapon sub category.
     *
     * @param school SisSchool
     * @param incidentCategory String
     * @param withWeapon Boolean
     * @return Category counter
     */
    private CategoryCounter getCategoryCounter(SisSchool school, String incidentCategory, Boolean withWeapon) {
        String key = null;
        if (m_hasWeaponSubCategory.contains(incidentCategory)) {
            key = school.getSchoolId() + incidentCategory +
                    (withWeapon.booleanValue() ? SUB_CAT_WITH_WEAPON : SUB_CAT_WITHOUT_WEAPON);
        } else {
            key = school.getSchoolId() + incidentCategory;
        }
        CategoryCounter counter = m_secOneCounters.get(key);
        if (counter == null) {
            counter = new CategoryCounter();
            m_secOneCounters.put(key, counter);
        }
        return counter;
    }

    /**
     * If counter not exist, will create it.
     * Add 1 to previous value.
     *
     * @param key String
     */
    private void increaseSectionCounter(String key) {
        Integer count = m_otherSectionCounters.get(key);
        if (count == null) {
            count = Integer.valueOf(1);
        } else {
            count = Integer.valueOf(count.intValue() + 1);
        }
        m_otherSectionCounters.put(key, count);
    }

    /**
     * Conditions of incident should be determine with rule:
     * If at least one of sub incidents has condition set to true,
     * then whole incident has this condition.
     * Exception for "is on school property" condition: it is by default set to true.
     *
     * @param incidentsGroup List<ConductIncident>
     * @param conditions Map<String,Boolean>
     * @param secOneCounters Map<String,Integer>
     */
    private void parseIncidentGroup(List<ConductIncident> incidentsGroup,
                                    Map<String, Boolean> conditions,
                                    Map<String, Integer> secOneCounters) {
        DateRange dayTime = determineSchoolTime(incidentsGroup);

        // currently only student offenders can be determine
        HashSet<SisStudent> studentOffenders = new HashSet<SisStudent>();

        // victims
        HashSet<SisStudent> studentsVictims = new HashSet<SisStudent>();
        HashSet<String> staffsVictims = new HashSet<String>();
        HashSet<String> othersVictims = new HashSet<String>();

        // actions
        HashSet<SisStudent> counselingStudents = new HashSet<SisStudent>();
        HashSet<SisStudent> removalStudents = new HashSet<SisStudent>();
        HashSet<SisStudent> suspensionStudents = new HashSet<SisStudent>();
        HashSet<SisStudent> outOfSklStudents = new HashSet<SisStudent>();
        HashSet<SisStudent> altEdPgmStudents = new HashSet<SisStudent>();
        HashSet<SisStudent> lawStudents = new HashSet<SisStudent>();

        for (ConductIncident incident : incidentsGroup) {
            determineConditions(conditions, dayTime, incident);

            // Determine offenders
            studentOffenders.add(incident.getStudent());

            // Determine victims
            if (incident.getVictim() != null) {
                studentsVictims.add(incident.getVictim());
            }
            String victimName = (String) incident.getFieldValueByAlias(ALIAS_VICTIM_NAME);
            boolean isStaff = BooleanAsStringConverter.TRUE.equals(incident.getFieldValueByAlias(ALIAS_VICTIM_STAFF));
            if (!StringUtils.isEmpty(victimName)) {
                if (isStaff) {
                    staffsVictims.add(victimName);
                } else {
                    othersVictims.add(victimName);
                }
            }

            for (ConductAction action : incident.getConductActions(getBroker())) {
                String actType = m_dict.findStateReferenceCode(m_rtbCndAction, action.getActionCode());
                if (!StringUtils.isEmpty(actType) && m_actionStateCodes.contains(actType)) {
                    if (actType.equals("J")) {
                        counselingStudents.add(action.getStudent());
                    } else if (actType.equals("K")) {
                        removalStudents.add(action.getStudent());
                    } else if (actType.equals("L")) {
                        suspensionStudents.add(action.getStudent());
                    } else if (actType.equals("M")) {
                        outOfSklStudents.add(action.getStudent());
                    } else if (actType.equals("N")) {
                        altEdPgmStudents.add(action.getStudent());
                    } else if (actType.equals("O")) {
                        lawStudents.add(action.getStudent());
                    }
                }
            }
        }

        secOneCounters.put(COUNTER_NUM_OF_STD_OFFENDERS, Integer.valueOf(studentOffenders.size()));
        secOneCounters.put(COUNTER_NUM_OF_STF_OFFENDERS, Integer.valueOf(0));
        secOneCounters.put(COUNTER_NUM_OF_OTH_OFFENDERS, Integer.valueOf(0));
        secOneCounters.put(COUNTER_NUM_OF_STD_VICTIMS, Integer.valueOf(studentsVictims.size()));
        secOneCounters.put(COUNTER_NUM_OF_STF_VICTIMS, Integer.valueOf(staffsVictims.size()));
        secOneCounters.put(COUNTER_NUM_OF_OTH_VICTIMS, Integer.valueOf(othersVictims.size()));

        // actions
        secOneCounters.put(COUNTER_NUM_OF_STD_COUNSELING, Integer.valueOf(counselingStudents.size()));
        secOneCounters.put(COUNTER_NUM_OF_STD_REMOVAL, Integer.valueOf(removalStudents.size()));
        secOneCounters.put(COUNTER_NUM_OF_STD_SUSPENSION, Integer.valueOf(suspensionStudents.size()));
        secOneCounters.put(COUNTER_NUM_OF_STD_OUT_OF_SKL, Integer.valueOf(outOfSklStudents.size()));
        secOneCounters.put(COUNTER_NUM_OF_STD_ALT_ED, Integer.valueOf(altEdPgmStudents.size()));
        secOneCounters.put(COUNTER_NUM_OF_STD_LAW, Integer.valueOf(lawStudents.size()));
    }

    /**
     * Convert all counter's to dataGrid.
     *
     * @param schools HashSet<SisSchool>
     * @return ReportDataGrid
     */
    private ReportDataGrid prepareDataGrid(HashSet<SisSchool> schools) {
        ReportDataGrid dataGrid = new ReportDataGrid();
        Map<String, Object> schoolsSectionValues = new HashMap<String, Object>();

        for (Entry students : m_studentsCounts.entrySet()) {
            schoolsSectionValues.put((String) students.getKey(), Integer.valueOf(((Set) students.getValue()).size()));
        }

        schoolsSectionValues.putAll(m_otherSectionCounters);

        ArrayList<SisSchool> sortedSchoolsByName = new ArrayList<SisSchool>(schools);
        Collections.sort(sortedSchoolsByName, new SchoolNameComparator());

        for (SisSchool school : sortedSchoolsByName) {
            for (String groupName : m_groupSortedList) {
                for (String incidentCategory : m_groupedCategory.get(groupName)) {
                    // Section 1.
                    if (m_hasWeaponSubCategory.contains(incidentCategory)) {
                        processCounterInGrid(dataGrid, school, groupName,
                                getCategoryCounter(school, incidentCategory, Boolean.TRUE),
                                incidentCategory, "With weapon(s)");
                        processCounterInGrid(dataGrid, school, groupName,
                                getCategoryCounter(school, incidentCategory, Boolean.FALSE),
                                incidentCategory, "Without weapon(s)");
                    } else {
                        processCounterInGrid(dataGrid, school, groupName,
                                getCategoryCounter(school, incidentCategory, Boolean.FALSE),
                                incidentCategory, "");
                    }
                }
            }
            String bedsNumber = (String) school.getFieldValueByAlias(ALIAS_BEDS_CODE);
            schoolsSectionValues.put(school.getSchoolId() + RKEY_BEDS_NUMBER, bedsNumber);

            // populate with zero values other sections,
            // to prevent null check in report
            for (String sectionKey : m_otherSectionsKeys) {
                if (schoolsSectionValues.get(school.getSchoolId() + sectionKey) == null) {
                    schoolsSectionValues.put(school.getSchoolId() + sectionKey, Integer.valueOf(0));
                }
            }
        }
        addParameter("schoolsSectionValues", schoolsSectionValues);
        addParameter("noVictimsCategorys", Arrays.asList("5A", "14A", "15A", "16A", "17A", "18A", "19A"));
        addParameter("noDrugsCategorys", Arrays.asList("18A", "19A"));
        dataGrid.beforeTop();
        return dataGrid;
    }

    /**
     * Process counter in grid. Merge category's 17.1 and 17.2 in one 17.
     * Replace code 2_1 to 2.1.
     *
     * @param dataGrid ReportDataGrid
     * @param school SisSchool
     * @param groupName String
     * @param categoryCounter CategoryCounter
     * @param categoryCode String
     * @param subCategoryName String
     */
    private void processCounterInGrid(ReportDataGrid dataGrid,
                                      SisSchool school,
                                      String groupName,
                                      CategoryCounter categoryCounter,
                                      String categoryCode,
                                      String subCategoryName) {
        // merge of category's 17.1 and 17.2
        if (SUB_CAT_WEAPON_SCREENING.equals(categoryCode)) {
            categoryCode = STATE_CODE_WEAPON_POSSESSION;
            subCategoryName = "17.1 Weapon(s) confiscated through screening";
        } else if (SUB_CAT_WEAPON_OTHER.equals(categoryCode)) {
            categoryCode = STATE_CODE_WEAPON_POSSESSION;
            subCategoryName = "17.2 Weapon(s) found under other circumstances";
        }
        if (categoryCode.contains(".")) {
            categoryCode = categoryCode.replace('.', '_');
        }

        dataGrid.append();
        dataGrid.set(FIELD_SCHOOL, school);
        dataGrid.set(FIELD_SCHOOL_NAME, school.getName());
        dataGrid.set(FIELD_GROUP_NAME, groupName);
        dataGrid.set(FIELD_CATEGORY_CODE, categoryCode);
        dataGrid.set(FIELD_CATEGORY_NAME, getParameter("category" + categoryCode + "name"));
        dataGrid.set(FIELD_CATEGORY_COMMENT, getParameter("category" + categoryCode + "comment"));
        dataGrid.set(FIELD_SUB_CATEGORY_NAME, subCategoryName);
        dataGrid.set(FIELD_CATEGORY_COUNTS, categoryCounter.getCountsMap());
    }
}
