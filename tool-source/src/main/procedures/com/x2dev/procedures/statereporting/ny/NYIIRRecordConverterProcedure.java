/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ny;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.converters.IntegerAsStringConverter;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.DateRange;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class NYIIRRecordConverterProcedure.
 */
public class NYIIRRecordConverterProcedure extends ProcedureJavaSource {
    private static final String ALIAS_CND_BIAS_RELATED = "DOE BIAS RELATED";
    private static final String ALIAS_CND_DRUGS_INVOLVED = "DOE INVOLVED ALCOHOL OR DRUGS";
    private static final String ALIAS_CND_GANG_RELATED = "DOE GANG RELATED";
    private static final String ALIAS_CND_VICTIM_NAME = "DOE VICTIM NAME";
    private static final String ALIAS_CND_VICTIM_POSITION = "DOE VICTIM POSITION";
    private static final String ALIAS_CND_VICTIM_STAFF = "DOE VICTIM STAFF";
    private static final String ALIAS_CND_WEAPON_INVOLVED = "DOE INVOLVED WEAPON";
    private static final String ALIAS_HAR_COLOR = "DOE HAR COLOR";
    private static final String ALIAS_HAR_DISABILITY = "DOE HAR DISABLITY";
    private static final String ALIAS_HAR_ETHNIC = "DOE HAR ETHNIC GROUP";
    private static final String ALIAS_HAR_GENDER = "DOE HAR GENDER";
    private static final String ALIAS_HAR_NATIONAL = "DOE HAR NATIONAL ORIGIN";
    private static final String ALIAS_HAR_OTHER = "DOE HAR OTHER";
    private static final String ALIAS_HAR_RACE = "DOE HAR RACE";
    private static final String ALIAS_HAR_RELIGION = "DOE HAR RELIGION";
    private static final String ALIAS_HAR_RELIGIOUS = "DOE HAR RELIGIOUS PRACTICE";
    private static final String ALIAS_HAR_SEXUAL = "DOE HAR SEXUAL ORIENTATION";
    private static final String ALIAS_HAR_SEX = "DOE HAR SEX";
    private static final String ALIAS_HAR_WEIGHT = "DOE HAR WEIGHT";

    private static final String ALIAS_IRR_BIAS = "bias";
    private static final String ALIAS_IRR_CATEGORY = "primary-category";
    private static final String ALIAS_IRR_DATE = "date";
    private static final String ALIAS_IRR_GANG = "gang-related";
    private static final String ALIAS_IRR_INCIDENT = "incident-id";
    private static final String ALIAS_IRR_INVOLVING_ALCOHOL = "involving-alcohol";
    private static final String ALIAS_IRR_INVOLVING_DRUGS = "involving-drugs";
    private static final String ALIAS_IRR_PREPARED = "date-prepared";
    private static final String ALIAS_IRR_SECONDARY_CATEGORIES = "secondary-categories";

    private static final String ALIAS_OFFENDER_AGE = "irr-offender-age";
    private static final String ALIAS_OFFENDER_ALTERNATIVE_PLACEMENT = "irr-offender-alt";
    private static final String ALIAS_OFFENDER_COMMUNITY_SERVICE = "irr-offender-service";
    private static final String ALIAS_OFFENDER_COMMUNITY_SERVICE_TIME = "irr-offender-service-time";
    private static final String ALIAS_OFFENDER_COUNSELING = "irr-offender-counseling";
    private static final String ALIAS_OFFENDER_COUNSELING_TIME = "irr-offender-counseling-time";
    private static final String ALIAS_OFFENDER_GRADE = "irr-offender-grade";
    private static final String ALIAS_OFFENDER_IIS = "irr-offender-iss";
    private static final String ALIAS_OFFENDER_IIS_TIME = "irr-offender-iss-time";
    private static final String ALIAS_OFFENDER_JUSTICE_SYSTEM = "irr-offender-justice";
    private static final String ALIAS_OFFENDER_LAW_ENFORCEMENT = "irr-offender-law-enforcement";
    private static final String ALIAS_OFFENDER_NAME = "irr-offender-name";
    private static final String ALIAS_OFFENDER_OOS = "irr-offender-oss";
    private static final String ALIAS_OFFENDER_OOS_TIME = "irr-offender-oss-time";
    private static final String ALIAS_OFFENDER_TEACHER_REMOVAL = "irr-offender-teach-remove";
    private static final String ALIAS_OFFENDER_TEACHER_REMOVAL_TIME = "irr-offender-teach-remove-time";
    private static final String ALIAS_OFFENDER_TYPE = "irr-offender-type";

    private static final String ALIAS_VICTIM_AGE = "irr-victim-age";
    private static final String ALIAS_VICTIM_GRADE = "irr-victim-grade";
    private static final String ALIAS_VICTIM_NAME = "irr-victim-name";
    private static final String ALIAS_VICTIM_TYPE = "irr-victim-type";
    private static final String ALIAS_VICTIM_POSITION = "irr-victim-position";

    private static final String FIELD_IS_BIAS_RELATED = "isBiasRelated";
    private static final String FIELD_IS_GANG_RELATED = "isGangRelated";
    private static final String FIELD_IS_INTIMIDATION = "isIntimidation";
    private static final String FIELD_IS_ON_SCHOOL_PROPERTY = "isOnSchoolProperty";
    private static final String FIELD_IS_ON_SCHOOL_TIME = "isOnSchoolTime";
    private static final String FIELD_IS_ON_TRANSPORT = "isOnTransport";
    private static final String FIELD_IS_WITH_DRUGS = "isWithDrugs";
    private static final String FIELD_IS_WITH_WEAPON = "isWithWeapon";

    private static Map<String, String> HARRASSMENT_MAP = new HashMap();
    {
        HARRASSMENT_MAP.put(ALIAS_HAR_RACE, "Race");
        HARRASSMENT_MAP.put(ALIAS_HAR_ETHNIC, "Ethnic Group");
        HARRASSMENT_MAP.put(ALIAS_HAR_NATIONAL, "National Origin");
        HARRASSMENT_MAP.put(ALIAS_HAR_COLOR, "Color");
        HARRASSMENT_MAP.put(ALIAS_HAR_RELIGION, "Religion");
        HARRASSMENT_MAP.put(ALIAS_HAR_RELIGIOUS, "Religious Practices");
        HARRASSMENT_MAP.put(ALIAS_HAR_DISABILITY, "Disability");
        HARRASSMENT_MAP.put(ALIAS_HAR_GENDER, "Gender");
        HARRASSMENT_MAP.put(ALIAS_HAR_SEXUAL, "Sexual orientation");
        HARRASSMENT_MAP.put(ALIAS_HAR_SEX, "Sex");
        HARRASSMENT_MAP.put(ALIAS_HAR_WEIGHT, "Weight");
        HARRASSMENT_MAP.put(ALIAS_HAR_OTHER, "Other");
    }

    private static final String IRR_PERSON_TYPE_DISTRICT_STUDENT = "District Student";
    private static final String IRR_PERSON_TYPE_PERSON = "Person";
    private static final String IRR_PERSON_TYPE_STAFF = "Staff";

    private static final String PARAM_ACTIONS_STATE_CODES = "actionsStateCodes";
    private static final String PARAM_DELIMITER = "delimiter";
    private static final String PARAM_INCIDENTS_STATE_CODE = "incidentsStateCodes";
    private static final String PARAM_END_DATE = "endDate";
    private static final String PARAM_QUERY_BY = "queryBy";
    private static final String PARAM_QUERY_STRING = "queryString";
    private static final String PARAM_START_DATE = "startDate";

    private static final String STATE_CODE_INTIMIDATION = "10A";
    private static final String STATE_CODE_OFFSCHOOL = "OFFSCHOOL";
    private static final String STATE_CODE_TRANSPORTATION = "TRANS";

    private static final String USER_DEFINED_TABLEA_DDX_ID = "NY-IIR";

    /**
     * Members
     */
    private Set<String> m_actionStateCodes;
    private BooleanAsStringConverter m_booleanConverter;
    private DateAsStringConverter m_dateConverter;
    private DataDictionary m_dictionary = null;
    private IntegerAsStringConverter m_integerConverter = (IntegerAsStringConverter) ConverterFactory
            .getConverterForClass(Converter.INTEGER_CONVERTER, Locale.getDefault(), true);

    private Map<String, String> m_mapIncidentCategory;
    private String m_rtbOidConductAction;
    private String m_rtbOidConductIncident;
    private String m_rtbOidConductLocation;

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        Map<String, List<ConductIncident>> incidentsGroups =
                getBroker().getGroupedCollectionByQuery(buildIncidentQuery(), ConductIncident.COL_INCIDENT_ID, 0);

        for (List<ConductIncident> incidentsGroup : incidentsGroups.values()) {
            persistIIRTableData(incidentsGroup);
        }
    }



    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        m_booleanConverter = (BooleanAsStringConverter) ConverterFactory
                .getConverterForClass(Converter.BOOLEAN_CONVERTER, Locale.getDefault(), true);
        m_dateConverter = (DateAsStringConverter) ConverterFactory
                .getConverterForClass(PlainDate.class.getName(), Locale.getDefault(), true);
        m_integerConverter = (IntegerAsStringConverter) ConverterFactory
                .getConverterForClass(Converter.INTEGER_CONVERTER, Locale.getDefault(), true);

        ExtendedDataDictionary ddxById = getExtendedDataDictionaryById(USER_DEFINED_TABLEA_DDX_ID);
        m_dictionary = DataDictionary.getDistrictDictionary(ddxById, getBroker().getPersistenceKey());
        m_rtbOidConductAction = getReferenceTable(ConductAction.class, ConductAction.COL_ACTION_CODE);
        m_rtbOidConductIncident = getReferenceTable(ConductIncident.class, ConductIncident.COL_INCIDENT_CODE);
        m_rtbOidConductLocation = getReferenceTable(ConductIncident.class, ConductIncident.COL_INCIDENT_LOCATION);

        m_actionStateCodes =
                new HashSet(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_ACTIONS_STATE_CODES),
                        ((String) getParameter(PARAM_DELIMITER)).charAt(0)));
    }

    /**
     * Only include incidents where the cndIncident has a state reference code in
     * the set {1A,2.1A,2.2A,3A,4A,5A,6A,7A,8A,9A,10A,11A,12A,13A,14A,15A,16A,17A,18A,19A,20A}
     * and cndIncidentDate between PARAM_START_DATE and PARAM_END_DATE
     *
     * @return QueryByCriteria
     */
    private QueryByCriteria buildIncidentQuery() {
        X2Criteria criteria = new X2Criteria();

        criteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getParameter(PARAM_START_DATE));
        criteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getParameter(PARAM_END_DATE));
        if (isSchoolContext()) {
            criteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, getSchool().getOid());
        }
        String queryBy = (String) getParameter(PARAM_QUERY_BY);
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        addUserCriteria(criteria, queryBy, queryString, ConductIncident.class,
                ConductIncident.COL_INCIDENT_ID,
                ConductIncident.COL_INCIDENT_ID);


        Set<String> incidentStateCodes =
                new HashSet(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_INCIDENTS_STATE_CODE),
                        ((String) getParameter(PARAM_DELIMITER)).charAt(0)));
        HashSet incidentsCodes = new HashSet();
        // Place a non-matching value in the list to insure criteria operates
        // properly when there are no matching codes
        incidentsCodes.add("##dummy##");
        ReferenceTable refTable = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class,
                m_rtbOidConductIncident);
        if (refTable != null) {
            Collection<ReferenceCode> codes = refTable.getReferenceCodes(getBroker());
            for (ReferenceCode code : codes) {
                if (!code.getDisabledIndicator() && incidentStateCodes.contains(code.getStateCode())) {
                    incidentsCodes.add(code.getCode());
                }
            }
        }
        criteria.addIn(ConductIncident.COL_INCIDENT_CODE, incidentsCodes);
        criteria.addNotIn(ConductIncident.COL_INCIDENT_ID, getExistingIIRQuery());
        return new QueryByCriteria(ConductIncident.class, criteria);
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
            String stateLocationCode = m_dictionary.findStateReferenceCode(m_rtbOidConductLocation,
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

            if (BooleanAsStringConverter.TRUE.equals(incident
                    .getFieldValueByAlias(ALIAS_CND_BIAS_RELATED))) {
                conditions.put(FIELD_IS_BIAS_RELATED, Boolean.TRUE);
            }

            if (BooleanAsStringConverter.TRUE.equals(incident
                    .getFieldValueByAlias(ALIAS_CND_GANG_RELATED))) {
                conditions.put(FIELD_IS_GANG_RELATED, Boolean.TRUE);
            }

            if (BooleanAsStringConverter.TRUE.equals(incident
                    .getFieldValueByAlias(ALIAS_CND_DRUGS_INVOLVED))) {
                conditions.put(FIELD_IS_WITH_DRUGS, Boolean.TRUE);
            }

            if (BooleanAsStringConverter.TRUE.equals(incident
                    .getFieldValueByAlias(ALIAS_CND_WEAPON_INVOLVED))) {
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
                        m_dictionary.findStateReferenceCode(m_rtbOidConductIncident, offense.getIncidentCode());
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
        ScheduleBell scheduleBell =
                new ScheduleManager(getBroker()).getStudentBellSchedule(incident.getStudentOid(), schedule,
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
     * Determine natures.
     *
     * @param incidents ConductIncident
     * @return Set
     */
    private String getBiases(List<ConductIncident> incidents) {
        Set<String> biases = new HashSet();
        for (ConductIncident cnd : incidents) {
            for (String harrassmentAlias : HARRASSMENT_MAP.keySet()) {
                String harrassment = (String) cnd.getFieldValueByAlias(harrassmentAlias);
                if (BooleanAsStringConverter.TRUE.equals(harrassment)) {
                    biases.add(HARRASSMENT_MAP.get(harrassmentAlias));
                }
            }
        }

        return setToCSVString(biases);
    }

    /**
     * Gets the existing IIR query.
     *
     * @return Sub query
     */
    private SubQuery getExistingIIRQuery() {
        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_IRR_INCIDENT);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(UserDefinedTableA.COL_EXTENDED_DATA_DICTIONARY_OID,
                m_dictionary.getExtendedDictionaryOid());
        return new SubQuery(UserDefinedTableA.class, field.getJavaName(), criteria);
    }

    /**
     * Gets the extended data dictionary by id.
     *
     * @param ddxId String
     * @return Extended data dictionary
     */
    private ExtendedDataDictionary getExtendedDataDictionaryById(String ddxId) {
        ExtendedDataDictionary extendedDataDictionary = null;
        X2Criteria ddxCriteria = new X2Criteria();

        ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, ddxId);

        QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
        extendedDataDictionary =
                (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);
        return extendedDataDictionary;
    }



    /**
     * Gets the incident category.
     *
     * @param cndIncident String
     * @return String
     */
    private String getIncidentCategory(String cndIncident) {
        if (m_mapIncidentCategory == null) {
            m_mapIncidentCategory = new HashMap();
            Map<String, String> stateCodeMap = new HashMap();
            DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_IRR_CATEGORY);
            if (field != null && field.getReferenceTable() != null) {
                for (ReferenceCode code : field.getReferenceTable().getCodeMap().values()) {
                    if (!code.getDisabledIndicator() && !StringUtils.isEmpty(code.getStateCode())) {
                        stateCodeMap.put(code.getStateCode(), code.getCode());
                    }
                }
            }
            ModelProperty prop = new ModelProperty(ConductIncident.class, ConductIncident.COL_INCIDENT_CODE,
                    getBroker().getPersistenceKey());
            field = m_dictionary.findDataDictionaryField(prop.getFieldId());
            if (field != null && field.getReferenceTable() != null) {
                for (ReferenceCode code : field.getReferenceTable().getCodeMap().values()) {
                    if (!code.getDisabledIndicator() && !StringUtils.isEmpty(code.getStateCode())) {
                        m_mapIncidentCategory.put(code.getCode(), stateCodeMap.get(code.getStateCode()));
                    }
                }
            }
        }
        return m_mapIncidentCategory.get(cndIncident);
    }

    /**
     * Gets the reference table.
     *
     * @param beanClass Class
     * @param columnName String
     * @return String
     */
    private String getReferenceTable(Class beanClass, String columnName) {
        String rtbOid = null;
        ModelProperty prop = new ModelProperty(beanClass, columnName, getBroker().getPersistenceKey());
        DataDictionaryField field = m_dictionary.findDataDictionaryField(prop.getFieldId());
        if (!StringUtils.isEmpty(field.getReferenceTableOid())) {
            rtbOid = field.getReferenceTableOid();
        }
        return rtbOid;
    }

    /**
     * Gets the system string.
     *
     * @param converter SystemStringConverter
     * @param source Object
     * @return String
     */
    private String getSystemString(SystemStringConverter converter, Object source) {
        String value = null;
        if (source != null) {
            value = converter.getSystemString(source);
        }
        return value;
    }

    /**
     * Inits the conduc action params.
     *
     * @param actTypes Map<String,Boolean>
     * @param actDuration Map<String,Integer>
     * @param incident ConductIncident
     */
    private void initConducActionParams(Map<String, Boolean> actTypes,
                                        Map<String, Integer> actDuration,
                                        ConductIncident incident) {
        for (ConductAction action : incident.getConductActions()) {
            String actType =
                    m_dictionary.findStateReferenceCode(m_rtbOidConductAction, action.getActionCode());
            if (m_actionStateCodes.contains(actType)) {
                int daysCount = action.getActionPenaltyTime() == null ? 0 : action.getActionPenaltyTime().intValue();
                actTypes.put(actType, Boolean.TRUE);
                int prevValue = 0;
                if (actDuration.get(actType) != null) {
                    prevValue = actDuration.get(actType).intValue();
                }
                actDuration.put(actType, Integer.valueOf(prevValue + daysCount));

            }
        }
    }



    /**
     * Creates new IIR Victim record and persist to DB.
     *
     * @param incidents List<ConductIncident>
     * @param irrData UserDefinedTableA
     */
    private void persistIIROffenderTableData(List<ConductIncident> incidents, UserDefinedTableA irrData) {
        for (ConductIncident incident : incidents) {
            SisStudent offender = incident.getStudent();
            if (offender != null) {
                UserDefinedTableC offenderData =
                        X2BaseBean.newInstance(UserDefinedTableC.class, getBroker().getPersistenceKey());
                offenderData.setStudentOid(incident.getStudentOid());
                offenderData.setOrganization1Oid(incident.getStudent().getOrganization1Oid());
                offenderData.setExtendedDataDictionaryOid(m_dictionary.getExtendedDictionaryOid());
                offenderData.setConductIncidentOid(incident.getOid());

                offenderData.setFieldValueByAlias(ALIAS_OFFENDER_TYPE, IRR_PERSON_TYPE_DISTRICT_STUDENT,
                        m_dictionary);
                String age = m_integerConverter.getSystemString(Integer.valueOf(offender.getPerson().getAge()));
                offenderData.setFieldValueByAlias(ALIAS_OFFENDER_AGE, age, m_dictionary);
                offenderData.setFieldValueByAlias(ALIAS_OFFENDER_NAME, offender.getNameView(), m_dictionary);
                offenderData.setFieldValueByAlias(ALIAS_OFFENDER_GRADE, offender.getGradeLevel(), m_dictionary);

                Map<String, Boolean> actTypes = new HashMap<String, Boolean>();
                Map<String, Integer> actDuration = new HashMap<String, Integer>();
                initConducActionParams(actTypes, actDuration, incident);

                offenderData.setFieldValueByAlias(ALIAS_OFFENDER_COUNSELING,
                        getSystemString(m_booleanConverter, actTypes.get("J")),
                        m_dictionary);
                offenderData.setFieldValueByAlias(ALIAS_OFFENDER_COUNSELING_TIME,
                        getSystemString(m_integerConverter, actDuration.get("J")),
                        m_dictionary);
                offenderData.setFieldValueByAlias(ALIAS_OFFENDER_TEACHER_REMOVAL,
                        getSystemString(m_booleanConverter, actTypes.get("K")),
                        m_dictionary);
                offenderData.setFieldValueByAlias(ALIAS_OFFENDER_TEACHER_REMOVAL_TIME,
                        getSystemString(m_integerConverter, actDuration.get("K")),
                        m_dictionary);
                offenderData.setFieldValueByAlias(ALIAS_OFFENDER_IIS,
                        getSystemString(m_booleanConverter, actTypes.get("L")),
                        m_dictionary);
                offenderData.setFieldValueByAlias(ALIAS_OFFENDER_IIS_TIME,
                        getSystemString(m_integerConverter, actDuration.get("L")),
                        m_dictionary);
                offenderData.setFieldValueByAlias(ALIAS_OFFENDER_OOS,
                        getSystemString(m_booleanConverter, actTypes.get("M")),
                        m_dictionary);
                offenderData.setFieldValueByAlias(ALIAS_OFFENDER_OOS_TIME,
                        getSystemString(m_integerConverter, actDuration.get("M")),
                        m_dictionary);
                offenderData.setFieldValueByAlias(ALIAS_OFFENDER_ALTERNATIVE_PLACEMENT,
                        getSystemString(m_booleanConverter, actTypes.get("N")),
                        m_dictionary);

                offenderData.setFieldValueByAlias(ALIAS_OFFENDER_JUSTICE_SYSTEM,
                        getSystemString(m_booleanConverter, actTypes.get("O")),
                        m_dictionary);
                offenderData.setFieldValueByAlias(ALIAS_OFFENDER_LAW_ENFORCEMENT,
                        getSystemString(m_booleanConverter, actTypes.get("O")),
                        m_dictionary);
                irrData.addToUserDefinedRecordsC(offenderData);

                offenderData.setFieldValueByAlias(ALIAS_OFFENDER_COMMUNITY_SERVICE, "",
                        m_dictionary);
                offenderData.setFieldValueByAlias(ALIAS_OFFENDER_COMMUNITY_SERVICE_TIME, "",
                        m_dictionary);

                getBroker().saveBeanForced(offenderData, m_dictionary);

            }
        }

    }



    /**
     * Creates new IIR(UserDefinedTableA) record and persist to DB.
     *
     * @param incidents List<ConductIncident>
     */
    private void persistIIRTableData(List<ConductIncident> incidents) {
        ConductIncident incident = incidents.get(0);
        logMessage("Creating incident " + incident.getIncidentId());
        UserDefinedTableA irrData =
                X2BaseBean.newInstance(UserDefinedTableA.class, getBroker().getPersistenceKey());

        irrData.setOrganization1Oid(incident.getSchool().getOrganization1Oid());
        irrData.setSchoolOid(incident.getSchoolOid());
        irrData.setExtendedDataDictionaryOid(m_dictionary.getExtendedDictionaryOid());
        irrData.setFieldValueByAlias(ALIAS_IRR_DATE,
                getSystemString(m_dateConverter, incident.getIncidentDate()), m_dictionary);
        irrData.setFieldValueByAlias(ALIAS_IRR_INCIDENT, incident.getIncidentId(), m_dictionary);
        irrData.setFieldValueByAlias(ALIAS_IRR_PREPARED,
                getSystemString(m_dateConverter, new PlainDate()), m_dictionary);

        String primaryCategpory = getIncidentCategory(incident.getIncidentCode());
        Set<String> allCategories = new HashSet();
        for (ConductIncident cnd : incidents) {
            for (ConductOffense cno : cnd.getConductOffenses()) {
                String category = getIncidentCategory(cno.getIncidentCode());
                allCategories.add(category);
            }
        }
        allCategories.remove(primaryCategpory);
        irrData.setFieldValueByAlias(ALIAS_IRR_CATEGORY, primaryCategpory, m_dictionary);
        irrData.setFieldValueByAlias(ALIAS_IRR_SECONDARY_CATEGORIES, setToCSVString(allCategories), m_dictionary);

        irrData.setFieldValueByAlias(ALIAS_IRR_BIAS, getBiases(incidents), m_dictionary);

        HashMap<String, Boolean> conditions = determineConditions(incidents);
        irrData.setFieldValueByAlias(ALIAS_IRR_GANG,
                getSystemString(m_booleanConverter, conditions.get(FIELD_IS_GANG_RELATED)), m_dictionary);
        // TODO: Source data has alcohol and drugs combined
        irrData.setFieldValueByAlias(ALIAS_IRR_INVOLVING_ALCOHOL,
                getSystemString(m_booleanConverter, conditions.get(FIELD_IS_WITH_DRUGS)), m_dictionary);
        irrData.setFieldValueByAlias(ALIAS_IRR_INVOLVING_DRUGS,
                getSystemString(m_booleanConverter, conditions.get(FIELD_IS_WITH_DRUGS)), m_dictionary);

        persistIIROffenderTableData(incidents, irrData);
        persistIIRVictimTableData(incidents, irrData);

        getBroker().saveBeanForced(irrData, m_dictionary);

    }



    /**
     * Creates new IIR Victim record and persist to DB.
     *
     * @param incidents List<ConductIncident>
     * @param irrData UserDefinedTableA
     */
    private void persistIIRVictimTableData(List<ConductIncident> incidents, UserDefinedTableA irrData) {

        Set<String> victims = new HashSet();
        Integer studentVictimsCount = 0;
        Integer staffVictimsCount = 0;
        Integer otherVictimsCount = 0;
        for (ConductIncident incident : incidents) {
            String victimOid = incident.getVictimOid();
            if ((victimOid != null) && !victims.contains(victimOid)) {
                UserDefinedTableB victimData =
                        X2BaseBean.newInstance(UserDefinedTableB.class, getBroker().getPersistenceKey());
                victimData.setStudentOid(incident.getStudentOid());
                SisStudent victim = incident.getVictim();
                // isContinue = false;
                victimData.setStudentOid(victim.getOid());
                victimData.setSchoolOid(incident.getSchoolOid());
                victimData.setOrganization1Oid(incident.getStudent().getOrganization1Oid());
                victimData.setExtendedDataDictionaryOid(m_dictionary.getExtendedDictionaryOid());
                victimData.setConductIncidentOid(incident.getOid());
                victimData.setFieldValueByAlias(ALIAS_VICTIM_TYPE, IRR_PERSON_TYPE_DISTRICT_STUDENT, m_dictionary);
                victimData.setFieldValueByAlias(ALIAS_VICTIM_NAME, victim.getNameView(), m_dictionary);
                String age = m_integerConverter.getSystemString(Integer.valueOf(victim.getPerson().getAge()));
                victimData.setFieldValueByAlias(ALIAS_VICTIM_AGE, age, m_dictionary);
                victimData.setFieldValueByAlias(ALIAS_VICTIM_GRADE, victim.getGradeLevel(), m_dictionary);
                studentVictimsCount++;
                irrData.addToUserDefinedRecordsB(victimData);
                getBroker().saveBeanForced(victimData, m_dictionary);
            } else {
                // Other kind of victims
                String victimName = (String) incident.getFieldValueByAlias(ALIAS_CND_VICTIM_NAME);
                String victimPosition = (String) incident.getFieldValueByAlias(ALIAS_CND_VICTIM_POSITION);
                boolean isStaff =
                        BooleanAsStringConverter.TRUE.equals(incident.getFieldValueByAlias(ALIAS_CND_VICTIM_STAFF));
                // if we have name, we have victim
                if (!StringUtils.isEmpty(victimName) && !victims.contains(victimName)) {
                    UserDefinedTableB victimData =
                            X2BaseBean.newInstance(UserDefinedTableB.class, getBroker().getPersistenceKey());
                    victims.add(victimName);
                    victimData.setExtendedDataDictionaryOid(m_dictionary.getExtendedDictionaryOid());
                    victimData.setConductIncidentOid(incident.getOid());
                    victimData.setSchoolOid(incident.getSchoolOid());
                    if (isStaff) {
                        victimData.setFieldValueByAlias(ALIAS_VICTIM_TYPE, IRR_PERSON_TYPE_STAFF, m_dictionary);
                        staffVictimsCount++;
                    } else {
                        victimData.setFieldValueByAlias(ALIAS_VICTIM_TYPE, IRR_PERSON_TYPE_PERSON, m_dictionary);
                        otherVictimsCount++;
                    }
                    victimData.setFieldValueByAlias(ALIAS_VICTIM_NAME, victimName, m_dictionary);
                    victimData.setFieldValueByAlias(ALIAS_VICTIM_POSITION, victimPosition, m_dictionary);
                    getBroker().saveBeanForced(victimData, m_dictionary);
                }
            }
        }
    }



    /**
     * Sets the to CSV string.
     *
     * @param set Set<String>
     * @return String
     */
    private String setToCSVString(Set<String> set) {
        StringBuilder value = new StringBuilder();
        for (String category : set) {
            if (value.length() > 0) {
                value.append(",");
            }
            value.append(category);
        }
        return value.toString();
    }



}
