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
package com.x2dev.procedures.statereporting.md;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * This class implements the data export for Maryland Student Discipline export.
 *
 * @author X2 Development Corporation
 */
public class StudentDiscipline extends StateReportData {
    /**
     * Entity class for Maryland Student Discipline export.
     *
     * @author X2 Development Corporation
     */
    public static class StudentDisciplineEntity extends StateReportEntity {
        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StudentDisciplineEntity() {
            // public no argument constructor for dynamic instantiation.
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
            String name = "";
            if (incident.getStudent() != null) {
                name += incident.getStudent().getNameView() +
                        " [Local ID: " + incident.getStudent().getLocalId() +
                        ", State ID: " + incident.getStudent().getStateId() +
                        "] ";
            }
            name += incident.getIncidentId();

            return name;
        }
    }

    /**
     * Aliases on the action record for days counts.
     */
    protected static final String DOE_PROG_CODE_ELL = "DOE PR ELL";
    protected static final String ALIAS_DAYS_SANCTION = "DOE DAYS SANCTION";
    protected static final String ALIAS_DAYS_SERVED = "DOE DAYS SERVED";
    protected static final String ALIAS_DAYS_CARRYOVER = "DOE DAYS CARRYOVER";

    /**
     * Constants for reporting information.
     */
    private static final String PARAM_QUERY_BY = "queryBy";
    private static final String PARAM_QUERY_STRING = "queryString";
    private static final String PARAM_REPORT_DATE = "reportDate";
    private static final String PARAM_SORT = "sort";

    /**
     * constants for field parameters for the CND-ACTION retriever.
     */
    protected static final String PARAM_ACTION_CODE = "CODE";
    protected static final String PARAM_ACTION_PENALTY = "PENALTY";

    /**
     * Local variables for reporting information.
     */
    protected Map<String, Collection<ConductAction>> m_actions = null;
    protected Map<String, ReferenceCode> m_actionCodes = null;
    protected PlainDate m_reportDate;
    protected String m_spedActiveCode;


    /**
     * Retrieve the action code state value for the action.
     * Or default to continuation for no offense.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveAction implements FieldRetriever {
        // Map of aliases and their corresponding fields.
        private Map<String, String> m_aliasMap = new HashMap<String, String>(10);

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            StudentDiscipline sdData = (StudentDiscipline) data;
            ConductIncident incident = (ConductIncident) entity.getBean();
            Collection<ConductAction> actions = sdData.m_actions.get(incident.getOid());
            String param = (String) field.getParameter();
            Object value = null;

            if (PARAM_ACTION_CODE.equals(param) && actions != null) {
                // Action code. The most severe action taken for this incident.
                // Loop through actions, find one with a state code value on the action code.
                Map<String, ReferenceCode> refCodes = sdData.m_actionCodes;
                String stateCode = null;
                if (refCodes != null) {
                    for (ConductAction action : actions) {
                        String code = action.getActionCode();
                        if (!StringUtils.isEmpty(code)) {
                            ReferenceCode refCode = refCodes.get(code);
                            if (refCode != null && !StringUtils.isEmpty(refCode.getStateCode())) {
                                if (stateCode == null) {
                                    stateCode = refCode.getStateCode();
                                    break;
                                }
                            }
                        }
                    }
                }
                value = stateCode;
            } else if (PARAM_ACTION_PENALTY.equals(param) && actions != null) {
                // Total of action sanctioned days on all action records.
                float days = 0;
                Map<String, ReferenceCode> refCodes = sdData.m_actionCodes;
                String stateCode = null;
                if (refCodes != null) {
                    for (ConductAction action : actions) {
                        String code = action.getActionCode();
                        if (!StringUtils.isEmpty(code)) {
                            ReferenceCode refCode = refCodes.get(code);
                            if (refCode != null && !StringUtils.isEmpty(refCode.getStateCode())) {
                                if (stateCode == null) {
                                    stateCode = refCode.getStateCode();
                                }
                                if (action.getActionPenaltyTime() != null) {
                                    days += action.getActionPenaltyTime().floatValue();
                                }
                            }
                        }
                    }
                }
                value = Float.valueOf(days);
            } else if (actions != null) {
                // Treat the parameter as an alias on the action record.
                // This is used to retrieve the DOE ARRESTED alias for any action record.
                // If the alias is prefixed with t:, translate it into state code value.
                boolean prefixed = param.startsWith("t:");
                if (prefixed) {
                    param = param.substring(2);
                }
                value = Boolean.FALSE;
                for (ConductAction action : actions) {
                    String actionValue = (String) action.getFieldValueByAlias(param);
                    if (prefixed) {
                        if (!m_aliasMap.keySet().contains(param)) {
                            m_aliasMap.put(param, translateAliasToJavaName(param, true));
                        }
                        String beanPath = m_aliasMap.get(param);
                        actionValue = lookupStateValue(ConductAction.class, beanPath, actionValue);
                        if (!StringUtils.isEmpty(actionValue)) {
                            value = actionValue;
                        }
                    } else if (actionValue != null && (actionValue.startsWith("Y") || actionValue.equals("1"))) {
                        value = Boolean.TRUE;
                        break;
                    }
                }
            }

            return value;
        }
    }

    /**
     * Retrieve the value specified in the field.
     * If the field is marked for reference translation, translate it.
     * Then check if the resulting value is contained in the parameter string.
     * Return true or false based on the result of the contains check.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveContains implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Boolean value = Boolean.FALSE;
            String param = (String) field.getParameter();
            String fieldValue = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (!StringUtils.isEmpty(fieldValue) && field.getMappedLookup() == 1) {
                fieldValue = data.lookupStateValue(entity.getBean().getClass(), field.getBeanPath(), fieldValue);
            }

            if (!StringUtils.isEmpty(fieldValue) && !StringUtils.isEmpty(param)) {
                value = Boolean.valueOf(param.contains(fieldValue));
            }

            return value;
        }
    }

    /**
     * Returns an indicator as to whether the student is participating in a program.
     * 
     * The program is identified by the field.getParameter() value matching the State code value
     * of the reference code behind the program code.
     * 
     * To match, the student program must have a start date before the report date
     * and the end date must be after the report date or must be empty.
     * 
     * @author X2 Development Corporation
     */
    protected class RetrieveProgram implements FieldRetriever {
        String m_fieldToRetreive = null;
        String m_programStateCode = null;
        Map<String, Collection<StudentProgramParticipation>> m_studentPrograms;

        /**
         * Constructor loads a map of programs by student.
         *
         * @param programStateCode The state code of the reference code for the program code.
         * @param fieldToRetreive String
         */
        public RetrieveProgram(String programStateCode, String fieldToRetreive) {
            m_fieldToRetreive = fieldToRetreive;
            m_programStateCode = programStateCode;

            DataDictionaryField programCodeField = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                    .findDataDictionaryField(StudentProgramParticipation.class.getName(),
                            StudentProgramParticipation.COL_PROGRAM_CODE);
            List<String> ellProgCodes = new ArrayList<String>();
            ReferenceTable refTable = programCodeField.getReferenceTable();
            for (ReferenceCode refCode : refTable.getReferenceCodes()) {
                if (programStateCode.equals(refCode.getStateCode())) {
                    ellProgCodes.add(refCode.getCode());
                }
            }

            // Load programs for use in the retriever.
            if (ellProgCodes.size() > 0) {
                Criteria progCriteria = new Criteria();
                progCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, ellProgCodes);
                QueryByCriteria progQuery = new QueryByCriteria(StudentProgramParticipation.class, progCriteria);
                m_studentPrograms = getBroker().getGroupedCollectionByQuery(progQuery,
                        StudentProgramParticipation.COL_STUDENT_OID, 100);
            }
        }

        /**
         * Retrieve programs that match the field parameter as a state code for the program code,
         * and
         * included in the program start/end date range.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.FieldRetriever#getFieldValue(com.x2dev.sis.tools.
         *      stateexports.StateReportData, com.x2dev.sis.tools.stateexports.StateReportEntity,
         *      com.x2dev.sis.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            ConductIncident incident = (ConductIncident) entity.getBean();

            if (m_studentPrograms != null) {
                Collection<StudentProgramParticipation> programs = m_studentPrograms.get(incident.getStudentOid());
                if (programs != null && !StringUtils.isEmpty(m_programStateCode)) {
                    for (StudentProgramParticipation program : programs) {
                        String code = program.getProgramCode();
                        String alias = lookupStateValue(StudentProgramParticipation.class,
                                StudentProgramParticipation.COL_PROGRAM_CODE, code);
                        if (m_programStateCode.equals(alias)) {
                            if (!StringUtils.isEmpty(m_fieldToRetreive)) {
                                value = getProperty(program, m_fieldToRetreive);
                            } else if (program.getStartDate() != null &&
                                    ((m_reportDate.after(program.getStartDate()) ||
                                            m_reportDate.equals(program.getStartDate())) &&
                                            (program.getEndDate() == null ||
                                                    m_reportDate.before(program.getEndDate()) ||
                                                    m_reportDate.equals(program.getEndDate())))) {

                                value = "Y";
                            }
                            break;
                        }
                    }
                }
            }

            return value;
        }
    }

    /**
     * Returns an indicator if the person is included in the specified race.
     * This implementation looks up all race codes for a person and finds
     * the race entry for the specified race. It returns a string value
     * indicating the presence of the race code record.
     * 
     * The parameter input can represent one or more race indicators to display.
     * Each separate race indicator should be separated by a semi-colon (;)
     * 
     * Each race indicator should have three parts separated by a comma:
     * 1. true value, the value to output if the race code is present.
     * 2. false value, the value to output if the race code is not present.
     * 3. The state code value in the reference code for the race code in the person_race table.
     * 
     * EX: param = 1,0,1;2,0,2;3,0,3;4,0,4;5,0,5
     * 
     * The expected output would be five parts with vaue or zero in each position.
     * 10040
     * 00300
     * 
     * The reference code state code value in the reference table for race codes.
     * In MD, this is:
     * "1" - Indian/Native/Alaskan
     * "2" - Asian
     * "3" - Black
     * "4" - Pacific
     * "5" - White
     * 
     * @author X2 Development Corporation
     */
    protected class RetrieveRace implements FieldRetriever {
        private Map<String, Collection<Race>> m_raceCodeMap = new HashMap<String, Collection<Race>>();
        private Map<String, ReferenceCode> m_raceCodes;

        /**
         * Constructor.
         * Lookup race codes reference codes.
         */
        public RetrieveRace() {
            // Get race code reference codes for use in the race retriever.
            DataDictionaryField raceCodeField = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                    .findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);
            if (raceCodeField != null && raceCodeField.getReferenceTableOid() != null) {
                Criteria raceCriteria = new Criteria();
                raceCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, raceCodeField.getReferenceTableOid());
                raceCriteria.addNotEqualTo(ReferenceCode.COL_DISABLED_INDICATOR, Boolean.TRUE);
                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, raceCriteria);
                m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 5);
            }
        }

        /**
         * Lookup race codes for a person and generate race codes output.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            // Get the person oid from the field specification.
            String personOid = (String) getProperty(entity.getBean(), field.getBeanPath());
            return lookupCodes(personOid, (String) field.getParameter());
        }

        /**
         * Perform the race lookup based on the personOid provided.
         * This allows alternate person source for the teacher retriever.
         *
         * @param personOid String
         * @param param String
         * @return Object
         */
        protected Object lookupCodes(String personOid, String param) {
            String raceCode = "";
            String[] raceIndList = param.split(";");

            if (!StringUtils.isEmpty(personOid)) {
                // Parse the parameter to determine what to look up.
                for (String raceInd : raceIndList) {
                    String[] raceIndParts = raceInd.split(",");
                    if (raceIndParts.length >= 3) {
                        String yesCode = raceIndParts[0];
                        String noCode = raceIndParts[1];
                        String stateCode = raceIndParts[2];
                        String appender = noCode;

                        Collection<Race> races = getRaces(personOid, getBroker());

                        // Find the reference code that we are looking for.
                        ReferenceCode refCode = m_raceCodes.get(stateCode);
                        if (refCode != null && races != null) {
                            for (Race race : races) {
                                if (refCode.getCode().equals(race.getRaceCode())) {
                                    appender = yesCode;
                                    break;
                                }
                            }
                        }
                        raceCode += appender;
                    }
                }
            }

            return raceCode;
        }

        /**
         * Lookup the races collection for the given personOid. Use a local cache
         * for repeats.
         *
         * @param personOid String
         * @param broker X2Broker
         * @return Collection
         */
        private Collection<Race> getRaces(String personOid, X2Broker broker) {
            Collection<Race> races = null;
            if (!m_raceCodeMap.containsKey(personOid)) {
                Criteria criteria = new Criteria();
                criteria.addEqualTo(Race.COL_PERSON_OID, personOid);
                QueryByCriteria query = new QueryByCriteria(Race.class, criteria);
                races = broker.getCollectionByQuery(query);
                m_raceCodeMap.put(personOid, races);
            }
            races = m_raceCodeMap.get(personOid);
            return races;
        }
    }

    /**
     * Returns the 504 status value.
     */
    protected class Retrieve504 implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = Boolean.FALSE;

            // Get 504 status and dates from standard 504 student field.
            String status504Code = (String) getProperty(entity.getBean(), ConductIncident.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.COL_SECTION504_STATUS_CODE);

            if (m_spedActiveCode.equals(status504Code)) {
                value = Boolean.TRUE;
            }

            return value;
        }
    }

    /**
     * Initialize the data module.
     * 
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    protected void initialize() {
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        m_spedActiveCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.SPED_ACTIVE_CODE);

        Criteria incidentCriteria = getIncidentCriteria();
        QueryByCriteria incidentQuery = new QueryByCriteria(ConductIncident.class, incidentCriteria, true);
        int sort = ((Integer) getParameter(PARAM_SORT)).intValue();
        switch (sort) {
            case 0: // Name
                incidentQuery
                        .addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;

            case 1: // YOG
                incidentQuery.addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG);
                incidentQuery
                        .addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;

            case 2: // School
                incidentQuery.addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL
                        + PATH_DELIMITER + School.COL_NAME);
                incidentQuery
                        .addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;

            case 3: // LASID
                incidentQuery
                        .addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_LOCAL_ID);
                break;

            case 4: // SASID
                incidentQuery
                        .addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID);
                break;

            default:
                incidentQuery
                        .addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;
        }
        incidentQuery.addOrderBy(ConductIncident.COL_INCIDENT_ID, true);

        // Set the query to be used for student selection.
        setQuery(incidentQuery);
        setEntityClass(StudentDisciplineEntity.class);

        // Load reference codes and action/offense maps.
        loadActions(incidentCriteria);

        // Add any retrievers or validators.
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put("CND-504", new Retrieve504());
        calcs.put("CND-ACTION", new RetrieveAction());
        calcs.put("CND-CONTAINS", new RetrieveContains());
        calcs.put("CND-ELL", new RetrieveProgram(DOE_PROG_CODE_ELL, null));
        calcs.put("CND-RACE", new RetrieveRace());
        super.addCalcs(calcs);
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
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
            incidentCriteria.addEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        }
        incidentCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getStartDate());

        // Check student selection criteria user input.
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        switch (queryBy) {
            case 1: // YOG
                incidentCriteria.addEqualTo(ConductIncident.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG,
                        queryString);
                break;

            case 2: // LASID
                incidentCriteria.addEqualTo(ConductIncident.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_LOCAL_ID,
                        queryString);
                break;

            case 3: // SASID
                incidentCriteria.addEqualTo(ConductIncident.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID,
                        queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(incidentCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        // identify reportable offenses and actions.
        // String offenseRefTblOid = null;
        String actionRefTblOid = null;

        // 1. Find the reference code tables for incidents and actions.
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        /*
         * DataDictionaryField field =
         * dictionary.findDataDictionaryField(ConductIncident.class.getName(),
         * ConductIncident.COL_INCIDENT_CODE);
         * if (field != null)
         * {
         * offenseRefTblOid = field.getReferenceTableOid();
         * }
         */
        DataDictionaryField field =
                dictionary.findDataDictionaryField(ConductAction.class.getName(), ConductAction.COL_ACTION_CODE);
        if (field != null) {
            actionRefTblOid = field.getReferenceTableOid();
        }

        // 2. Load lists of reportable codes (codes that contain "ED166" in the Federal column).
        // List<String> incidentCodes = new ArrayList<String>();
        List<String> actionCodes = new ArrayList<String>();
        /*
         * if (!StringUtils.isEmpty(offenseRefTblOid))
         * {
         * X2Criteria criteria = new X2Criteria();
         * criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, offenseRefTblOid);
         * criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
         * ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, new String[]
         * {ReferenceCode.COL_CODE}, criteria);
         * ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
         * try
         * {
         * while (iterator.hasNext())
         * {
         * Object[] code = (Object[]) iterator.next();
         * incidentCodes.add((String) code[0]);
         * }
         * }
         * finally
         * {
         * iterator.close();
         * }
         * }
         */
        if (!StringUtils.isEmpty(actionRefTblOid)) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, actionRefTblOid);
            criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
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
        // Criteria codeCriteria = new Criteria();
        Criteria code2Criteria = new Criteria();
        if (actionCodes.size() > 0) // && incidentCodes.size() > 0
        {
            // codeCriteria.addIn(ConductIncident.REL_CONDUCT_OFFENSES + PATH_DELIMITER +
            // ConductOffense.COL_INCIDENT_CODE, incidentCodes);
            code2Criteria.addIn(ConductIncident.REL_CONDUCT_ACTIONS + PATH_DELIMITER +
                    ConductAction.COL_ACTION_CODE, actionCodes);
            // codeCriteria.addOrCriteria(code2Criteria);
            incidentCriteria.addAndCriteria(code2Criteria);
        }

        return incidentCriteria;
    }

    /**
     * Load supporting collections of actions.
     *
     * @param incidentCriteria Criteria
     */
    private void loadActions(Criteria incidentCriteria) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field =
                dictionary.findDataDictionaryField(ConductAction.class.getName(), ConductAction.COL_ACTION_CODE);
        String referenceTableOid = field.getReferenceTableOid();

        X2Criteria criteria = null;
        BeanQuery query = null;

        // Get a map of action codes to ReferenceCode.
        if (referenceTableOid != null) {
            criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
            query = new BeanQuery(ReferenceCode.class, criteria, false);
            m_actionCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 50);
        }

        // Load all actions for the selected incidents into a map.
        SubQuery incidentQuery = new SubQuery(ConductIncident.class, X2BaseBean.COL_OID, incidentCriteria);

        criteria = new X2Criteria();
        criteria.addIn(ConductAction.COL_INCIDENT_OID, incidentQuery);
        query = new BeanQuery(ConductAction.class, criteria, false);
        m_actions = getBroker().getGroupedCollectionByQuery(query, ConductAction.COL_INCIDENT_OID, 100);
    }
}
