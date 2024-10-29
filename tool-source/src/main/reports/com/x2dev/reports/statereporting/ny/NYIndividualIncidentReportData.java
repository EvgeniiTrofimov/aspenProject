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
package com.x2dev.reports.statereporting.ny;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.ConductOffense;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleBell;
import com.x2dev.sis.model.beans.ScheduleBellPeriod;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.DateRange;
import com.x2dev.utils.types.PlainTime;
/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
import java.io.ByteArrayInputStream;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class for populating JasperReport design for "VIOLENT AND DISRUPTIVE INCIDENT REPORT (VADIR)
 * FORM".
 */
public class NYIndividualIncidentReportData extends ReportJavaSourceNet {
    public static final String ALIAS_BIAS_RELATED = "DOE BIAS RELATED";
    public static final String ALIAS_DRUGS_INVOLVED = "DOE INVOLVED ALCOHOL OR DRUGS";
    public static final String ALIAS_GANG_RELATED = "DOE GANG RELATED";
    public static final String ALIAS_INCIDENT_LOCATION = "DOE INCIDENT LOCATION";
    public static final String ALIAS_VICTIM_NAME = "DOE VICTIM NAME";
    public static final String ALIAS_VICTIM_POSITION = "DOE VICTIM POSITION";
    public static final String ALIAS_VICTIM_STAFF = "DOE VICTIM STAFF";
    public static final String ALIAS_VICTIM_OTHER = "DOE VICTIM OTHER";
    public static final String ALIAS_WEAPON_INVOLVED = "DOE INVOLVED WEAPON";
    public static final String ALIAS_WEAPON_COUNT = "DOE WEAPON COUNT";
    public static final String ALIAS_WEAPON_TYPE = "DOE WEAPON TYPE";

    public static final String FIELD_INCIDENT = "incident";
    public static final String FIELD_INCIDENT_CATEGORY = "incidentCategory";
    public static final String FIELD_IS_BIAS_RELATED = "isBiasRelated";
    public static final String FIELD_IS_GANG_RELATED = "isGangRelated";
    public static final String FIELD_IS_INTIMIDATION = "isIntimidation";
    public static final String FIELD_IS_ON_SCHOOL_PROPERTY = "isOnSchoolProperty";
    public static final String FIELD_IS_ON_SCHOOL_TIME = "isOnSchoolTime";
    public static final String FIELD_IS_ON_TRANSPORT = "isOnTransport";
    public static final String FIELD_IS_WITH_DRUGS = "isWithDrugs";
    public static final String FIELD_IS_WITH_WEAPON = "isWithWeapon";
    public static final String FIELD_OFFENDER_ACT_DUR = "actionsDurations";
    public static final String FIELD_OFFENDER_ACT_TYPE = "actionsTypes";
    public static final String FIELD_OFFENDER_STUDENT = "offenderStudent";
    public static final String FIELD_OFFENDERS_FORMAT = "offendersFormat";
    public static final String FIELD_OFFENDERS_GRID = "offendersGrid";
    public static final String FIELD_SCHOOL = "school";
    public static final String FIELD_VICTIM_AGE = "victimAge";
    public static final String FIELD_VICTIM_GRADE = "victimGrade";
    public static final String FIELD_VICTIM_NAME = "victimName";
    public static final String FIELD_VICTIM_POSITION = "victimPosition";
    public static final String FIELD_VICTIM_TYPE = "victimType";
    public static final String FIELD_VICTIMS_FORMAT = "victimsFormat";
    public static final String FIELD_VICTIMS_GRID = "victimsGrid";
    public static final String FIELD_INVOLVED_WEAPONS = "weapons";

    public static final String PARAM_ACTIONS_STATE_CODES = "actionsStateCodes";
    public static final String PARAM_DELIMITER = "delimiter";
    public static final String PARAM_END_DATE = "endDate";
    public static final String PARAM_GENERATE_OFFENDERS_TABLES = "generateOffendersTables";
    public static final String PARAM_GENERATE_VICTIMS_TABLES = "generateVictimsTables";
    public static final String PARAM_INCIDENTS_STATE_CODE = "incidentsStateCodes";
    public static final String PARAM_QUERY_BY = "queryBy";
    public static final String PARAM_QUERY_STRING = "queryString";
    public static final String PARAM_REPORT_DATE = "reportDate";
    public static final String PARAM_START_DATE = "startDate";
    public static final String PARAM_SUBREPORT_OFFENDERS = "subReportOffenders";
    public static final String PARAM_SUBREPORT_VICTIMS = "subReportVictims";

    public static final String REPORT_PARAM_MONTH = "reportMonth";
    public static final String REPORT_PARAM_REPORT_DATE = "reportDate";
    public static final String REPORT_PARAM_YEAR = "reportYear";

    public static final String STATE_CODE_EVENT = "EVENT";
    public static final String STATE_CODE_INTIMIDATION = "10A";
    public static final String STATE_CODE_OFFSCHOOL = "OFFSCHOOL";
    public static final String STATE_CODE_TRANSPORTATION = "TRANS";

    public static final String VICTIM_TYPE_OTHERS = "others";
    public static final String VICTIM_TYPE_STAFF = "staff";
    public static final String VICTIM_TYPE_STUDENT = "student";

    private HashSet m_actionStateCodes;
    private DataDictionary m_dict;
    private DataDictionaryField m_dictFieldWeapon;
    private HashSet m_incidentStateCodes;
    private String m_referenceTableOidConductAction = "rtbCndAction";
    private String m_referenceTableOidConductIncident = "rtbCndIncident";
    private String m_referenceTableOidConductLocation = "rtbCndLocation";
    private ScheduleManager m_scheduleManager;
    private Report m_subReportOffenders;
    private Report m_subReportVictims;
    private ConductIncident m_currentIncident;


    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    public void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_currentIncident = userData.getCurrentRecord(ConductIncident.class);
    }

    /**
     * Only include incidents where the cndIncident has a state reference code in
     * the set {1A,2.1A,2.2A,3A,4A,5A,6A,7A,8A,9A,10A,11A,12A,13A,14A,15A,16A,17A,18A,19A,20A}
     * and cndIncidentDate between PARAM_START_DATE and PARAM_END_DATE
     *
     * @return QueryByCriteria
     */
    protected QueryByCriteria buildQuery() {
        X2Criteria criteria = new X2Criteria();
        if (m_currentIncident != null) {
            criteria.addEqualTo(ConductIncident.COL_INCIDENT_ID, m_currentIncident.getIncidentId());
        } else {
            criteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getParameter(PARAM_START_DATE));
            criteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getParameter(PARAM_END_DATE));
            if (isSchoolContext()) {
                criteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, getSchool().getOid());
            }
            String queryBy = (String) getParameter(PARAM_QUERY_BY);
            String queryString = (String) getParameter(PARAM_QUERY_STRING);
            addUserCriteria(criteria, queryBy, queryString, ConductIncident.class, ConductIncident.COL_INCIDENT_ID,
                    ConductIncident.COL_INCIDENT_ID);
        }

        HashSet incidentsCodes = new HashSet();
        // Place a non-matching value in the list to insure criteria operates
        // properly when there are no matching codes
        incidentsCodes.add("##dummy##");
        ReferenceTable refTable = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class,
                m_referenceTableOidConductIncident);
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
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();
        Map<String, List<ConductIncident>> incidentsGroups = getBroker().getGroupedCollectionByQuery(buildQuery(),
                ConductIncident.COL_INCIDENT_ID,
                0);

        for (List<ConductIncident> incidentsGroup : incidentsGroups.values()) {
            // We can assume that all incidents in group represent the same
            // incident, except special flags and conditionals.
            // They need to be checked at each sub-incident.
            ConductIncident incident = incidentsGroup.get(0);
            String incidentCategory = m_dict.findStateReferenceCode(m_referenceTableOidConductIncident,
                    incident.getIncidentCode());
            grid.append();
            grid.set(FIELD_SCHOOL, incident.getSchool());
            grid.set(FIELD_INCIDENT, incident);
            grid.set(FIELD_INCIDENT_CATEGORY, incidentCategory);

            HashMap<String, Boolean> conditions = determineConditions(incidentsGroup);
            grid.set(FIELD_IS_BIAS_RELATED, conditions.get(FIELD_IS_BIAS_RELATED));
            grid.set(FIELD_IS_GANG_RELATED, conditions.get(FIELD_IS_GANG_RELATED));
            grid.set(FIELD_IS_INTIMIDATION, conditions.get(FIELD_IS_INTIMIDATION));
            grid.set(FIELD_IS_ON_SCHOOL_PROPERTY, conditions.get(FIELD_IS_ON_SCHOOL_PROPERTY));
            grid.set(FIELD_IS_ON_SCHOOL_TIME, conditions.get(FIELD_IS_ON_SCHOOL_TIME));
            grid.set(FIELD_IS_ON_TRANSPORT, conditions.get(FIELD_IS_ON_TRANSPORT));
            grid.set(FIELD_IS_WITH_DRUGS, conditions.get(FIELD_IS_WITH_DRUGS));
            grid.set(FIELD_IS_WITH_WEAPON, conditions.get(FIELD_IS_WITH_WEAPON));
            grid.set(FIELD_OFFENDERS_FORMAT, new ByteArrayInputStream(m_subReportOffenders.getCompiledFormat()));
            grid.set(FIELD_OFFENDERS_GRID, buildOffendersGrid(incidentsGroup));
            grid.set(FIELD_VICTIMS_FORMAT, new ByteArrayInputStream(m_subReportVictims.getCompiledFormat()));
            grid.set(FIELD_VICTIMS_GRID, buildVictimsGrid(incidentsGroup));
            grid.set(FIELD_INVOLVED_WEAPONS, determineWeapons(incidentsGroup));
        }
        grid.beforeTop();
        return grid;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        m_subReportOffenders = ReportUtils.getReport((String) getParameter(PARAM_SUBREPORT_OFFENDERS), getBroker());
        m_subReportVictims = ReportUtils.getReport((String) getParameter(PARAM_SUBREPORT_VICTIMS), getBroker());
        m_dict = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_dictFieldWeapon = m_dict.findDataDictionaryFieldByAlias(ALIAS_WEAPON_TYPE);
        m_referenceTableOidConductAction = getReferenceTable(ConductAction.class,
                ConductAction.COL_ACTION_CODE, m_referenceTableOidConductAction);
        m_referenceTableOidConductIncident = getReferenceTable(ConductIncident.class,
                ConductIncident.COL_INCIDENT_CODE, m_referenceTableOidConductIncident);
        m_referenceTableOidConductLocation = getReferenceTable(ConductIncident.class,
                ConductIncident.COL_INCIDENT_LOCATION, m_referenceTableOidConductLocation);

        m_scheduleManager = new ScheduleManager(getBroker());
        m_actionStateCodes =
                new HashSet(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_ACTIONS_STATE_CODES),
                        ((String) getParameter(PARAM_DELIMITER)).charAt(0)));
        m_incidentStateCodes =
                new HashSet(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_INCIDENTS_STATE_CODE),
                        ((String) getParameter(PARAM_DELIMITER)).charAt(0)));

        addParameter(REPORT_PARAM_REPORT_DATE, getParameter(PARAM_END_DATE));

    }

    /**
     * Examine related ConductActions for each offender.
     * For ConductActions that have actActionCode with State reference code in the set
     * {J,K,L,M,N,O},
     * generate the accumulated duration. This information is then used to populate this section.
     * Only report student offenders at this time.
     *
     * @param incidentsGroup List<ConductIncident>
     * @return ReportDataGrid
     */
    private ReportDataGrid buildOffendersGrid(List<ConductIncident> incidentsGroup) {
        ReportDataGrid offendersGrid = new ReportDataGrid();
        int i = 0;
        HashSet<SisStudent> offenders = new HashSet<SisStudent>();
        for (ConductIncident incident : incidentsGroup) {
            SisStudent offender = incident.getStudent();
            if (offenders.contains(offender)) {
                continue;
            }
            offenders.add(offender);

            Map<String, Boolean> actTypes = new HashMap<String, Boolean>();
            Map<String, Integer> actDuration = new HashMap<String, Integer>();

            for (ConductAction action : incident.getConductActions()) {
                String actType =
                        m_dict.findStateReferenceCode(m_referenceTableOidConductAction, action.getActionCode());
                if (m_actionStateCodes.contains(actType)) {
                    int daysCount = 1;
                    if ((action.getActionStartDate() != null) && (action.getActionEndDate() != null)) {
                        daysCount = CalendarManager.getInSessionDates(action.getActionStartDate(),
                                action.getActionEndDate(), offender, getBroker()).size();
                    }
                    actTypes.put(actType, Boolean.TRUE);
                    int prevValue = 0;
                    if (actDuration.get(actType) != null) {
                        prevValue = actDuration.get(actType).intValue();
                    }
                    actDuration.put(actType, Integer.valueOf(prevValue + daysCount));

                }
            }
            offendersGrid.append();
            offendersGrid.set(FIELD_OFFENDER_STUDENT, offender);
            offendersGrid.set(FIELD_OFFENDER_ACT_TYPE, actTypes);
            offendersGrid.set(FIELD_OFFENDER_ACT_DUR, actDuration);
            i++;
        }
        for (; i < 4; i++) {
            offendersGrid.append();
        }
        offendersGrid.beforeTop();
        return offendersGrid;
    }

    /**
     * These are generated from the victims related to the ConductINcident beans for the incident.
     *
     * @param incidentsGroup List<ConductIncident>
     * @return ReportDataGrid
     */
    private ReportDataGrid buildVictimsGrid(List<ConductIncident> incidentsGroup) {
        ReportDataGrid victimsGrid = new ReportDataGrid(20);
        ReportDataGrid studentGrid = new ReportDataGrid(10);
        ReportDataGrid staffsGrid = new ReportDataGrid(10);
        ReportDataGrid othersGrid = new ReportDataGrid(10);

        Set<String> victims = new HashSet();
        int studentCount = 0;
        int staffCount = 0;
        int othersCount = 0;
        for (ConductIncident incident : incidentsGroup) {
            String victimOid = incident.getVictimOid();
            // standard incident.getVictim is student
            if ((victimOid != null) && !victims.contains(victimOid)) {
                SisStudent victim = incident.getVictim();
                studentGrid.append();
                studentGrid.set(FIELD_VICTIM_TYPE, VICTIM_TYPE_STUDENT);
                studentGrid.set(FIELD_VICTIM_NAME, victim.getNameView());
                studentGrid.set(FIELD_VICTIM_GRADE, victim.getGradeLevel());
                studentGrid.set(FIELD_VICTIM_AGE, Integer.toString(victim.getPerson().getAge()));
                victims.add(victimOid);
                studentCount++;
            }
            // Other kind of victims
            String victimName = (String) incident.getFieldValueByAlias(ALIAS_VICTIM_NAME);
            String victimPosition = (String) incident.getFieldValueByAlias(ALIAS_VICTIM_POSITION);
            boolean isStaff = BooleanAsStringConverter.TRUE.equals(incident.getFieldValueByAlias(ALIAS_VICTIM_STAFF));
            // if we have name, we have victim
            if (!StringUtils.isEmpty(victimName) && !victims.contains(victimName)) {
                victims.add(victimName);
                if (isStaff) {
                    staffsGrid.append();
                    staffsGrid.set(FIELD_VICTIM_TYPE, VICTIM_TYPE_STAFF);
                    staffsGrid.set(FIELD_VICTIM_NAME, victimName);
                    staffsGrid.set(FIELD_VICTIM_POSITION, victimPosition);
                    staffCount++;
                } else {
                    othersGrid.append();
                    othersGrid.set(FIELD_VICTIM_TYPE, VICTIM_TYPE_OTHERS);
                    othersGrid.set(FIELD_VICTIM_NAME, victimName);
                    othersGrid.set(FIELD_VICTIM_POSITION, victimPosition);
                    othersCount++;
                }
            }

        }
        // hook to provide empty rows in table for victims
        for (; studentCount < 10; studentCount++) {
            studentGrid.append();
            studentGrid.set(FIELD_VICTIM_TYPE, VICTIM_TYPE_STUDENT);
        }
        for (; staffCount < 8; staffCount++) {
            staffsGrid.append();
            staffsGrid.set(FIELD_VICTIM_TYPE, VICTIM_TYPE_STAFF);
        }
        for (; othersCount < 8; othersCount++) {
            othersGrid.append();
            othersGrid.set(FIELD_VICTIM_TYPE, VICTIM_TYPE_OTHERS);
        }

        studentGrid.sort(FIELD_VICTIM_NAME, false);
        staffsGrid.sort(FIELD_VICTIM_NAME, false);
        othersGrid.sort(FIELD_VICTIM_NAME, false);

        victimsGrid.append(studentGrid);
        victimsGrid.append(staffsGrid);
        victimsGrid.append(othersGrid);
        victimsGrid.beforeTop();
        return victimsGrid;
    }

    /**
     * Conditions of incident should be determine with rule:
     * If at least one of sub incidents has condition set to true,
     * then whole incident has this condition.
     * Exception for "is on school property" condition: it is by default set to true.
     *
     * @param incidentsGroup List<ConductIncident>
     * @return HashMap
     */
    private HashMap<String, Boolean> determineConditions(List<ConductIncident> incidentsGroup) {
        HashMap<String, Boolean> conditions = new HashMap<String, Boolean>();
        // init conditions
        conditions.put(FIELD_IS_BIAS_RELATED, Boolean.FALSE);
        conditions.put(FIELD_IS_GANG_RELATED, Boolean.FALSE);
        conditions.put(FIELD_IS_INTIMIDATION, Boolean.FALSE);
        conditions.put(FIELD_IS_ON_SCHOOL_PROPERTY, Boolean.TRUE);
        conditions.put(FIELD_IS_ON_SCHOOL_TIME, Boolean.FALSE);
        conditions.put(FIELD_IS_ON_TRANSPORT, Boolean.FALSE);
        conditions.put(FIELD_IS_WITH_DRUGS, Boolean.FALSE);
        conditions.put(FIELD_IS_WITH_WEAPON, Boolean.FALSE);

        DateRange dayTime = determineSchoolTime(incidentsGroup);

        for (ConductIncident incident : incidentsGroup) {
            String stateLocationCode = m_dict.findStateReferenceCode(m_referenceTableOidConductLocation,
                    incident.getIncidentLocation());

            /*
             * Now only if DOE INCIDENT LOCATION state code is OFFSCHOOL.
             * then not on School Property/
             */
            if (STATE_CODE_OFFSCHOOL.equals(stateLocationCode)) {
                conditions.put(FIELD_IS_ON_SCHOOL_PROPERTY, Boolean.FALSE);
            }

            if (STATE_CODE_TRANSPORTATION.equals(stateLocationCode)) {
                conditions.put(FIELD_IS_ON_TRANSPORT, Boolean.TRUE);
            }

            if (BooleanAsStringConverter.TRUE.equals(incident.getFieldValueByAlias(ALIAS_BIAS_RELATED))) {
                conditions.put(FIELD_IS_BIAS_RELATED, Boolean.TRUE);
            }

            if (BooleanAsStringConverter.TRUE.equals(incident.getFieldValueByAlias(ALIAS_GANG_RELATED))) {
                conditions.put(FIELD_IS_GANG_RELATED, Boolean.TRUE);
            }

            if (BooleanAsStringConverter.TRUE.equals(incident.getFieldValueByAlias(ALIAS_DRUGS_INVOLVED))) {
                conditions.put(FIELD_IS_WITH_DRUGS, Boolean.TRUE);
            }

            if (BooleanAsStringConverter.TRUE.equals(incident.getFieldValueByAlias(ALIAS_WEAPON_INVOLVED))) {
                conditions.put(FIELD_IS_WITH_WEAPON, Boolean.TRUE);
            }

            // if dayTime not identified, assume that incident is not at school
            // time
            if (dayTime != null) {
                if (incident.getIncidentTime() != null && dayTime.contains(incident.getIncidentTime())) {
                    conditions.put(FIELD_IS_ON_SCHOOL_TIME, Boolean.TRUE);
                }
            }

            // Need to check offenses at each incident.
            // Reason that each incident may have another student with offenses.
            for (ConductOffense offense : incident.getConductOffenses()) {
                String stateCode =
                        m_dict.findStateReferenceCode(m_referenceTableOidConductIncident, offense.getIncidentCode());
                if (STATE_CODE_INTIMIDATION.equals(stateCode)) {
                    conditions.put(FIELD_IS_INTIMIDATION, Boolean.TRUE);
                }
            }
        }
        return conditions;
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
     * Method to calculate map where will store counts of involved weapons by it's type.
     *
     * @param incidentsGroup List<ConductIncident>
     * @return HashMap
     */
    private HashMap determineWeapons(List<ConductIncident> incidentsGroup) {
        HashMap<String, Integer> weapons = new HashMap<String, Integer>();
        for (ConductIncident incident : incidentsGroup) {
            String weaponType = (String) incident.getFieldValueByBeanPath(m_dictFieldWeapon.getJavaName());
            String code = m_dict.findStateReferenceCode(m_dictFieldWeapon.getReferenceTableOid(), weaponType);
            if (!StringUtils.isEmpty(code)) {
                String rowValue = (String) incident.getFieldValueByAlias(ALIAS_WEAPON_COUNT);
                int weaponCount = 0;
                if (!StringUtils.isEmpty(rowValue)) {
                    weaponCount = (Double.valueOf(Double.parseDouble(rowValue))).intValue();
                }
                Integer prevValue = weapons.get(code);
                if (prevValue == null) {
                    prevValue = Integer.valueOf(0);
                }
                weapons.put(code, Integer.valueOf(prevValue.intValue() + weaponCount));
            }
        }
        return weapons;
    }

    /**
     * Gets the reference table.
     *
     * @param beanClass Class
     * @param columnName String
     * @param defaultValue String
     * @return String
     */
    private String getReferenceTable(Class beanClass, String columnName, String defaultValue) {
        String rtbOid = defaultValue;
        ModelProperty prop = new ModelProperty(beanClass, columnName, getBroker().getPersistenceKey());
        DataDictionaryField field = m_dict.findDataDictionaryField(prop.getFieldId());
        if (!StringUtils.isEmpty(field.getReferenceTableOid())) {
            rtbOid = field.getReferenceTableOid();
        }
        return rtbOid;
    }
}
