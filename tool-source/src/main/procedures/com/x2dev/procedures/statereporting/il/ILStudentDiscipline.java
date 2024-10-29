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
package com.x2dev.procedures.statereporting.il;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Illinois state export procedure for Student Discipline.
 *
 * @author X2 Development Corporation
 */
public class ILStudentDiscipline extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used by IL Student Course Assignment
     *
     * Maintains a student's enrollments, conduct actions, and disability.
     *
     * @author X2 Development Corporation
     */
    public static class StudentDisciplineEntity extends StateReportEntity {
        private static final String CODE_EFFECTIVE_ENROLLMENT = "E";

        /**
         * The Class RowData.
         */
        final private class RowData {
            final private String m_disciplinaryAct;
            final private BigDecimal m_disciplinaryDur;
            final private ConductIncident m_incident;
            final private String m_incidentCaseId;
            final private PlainDate m_incidentDate;
            final private int m_incidentNum;
            final private String m_incidentTypeCode;

            /**
             * Instantiates a new row data.
             *
             * @param incident ConductIncident
             * @param incidentDate PlainDate
             * @param incidentCaseId String
             * @param incidentNum int
             * @param disciplinaryAct String
             * @param incidentTypeCode String
             * @param disciplinaryDuration BigDecimal
             */
            RowData(ConductIncident incident, PlainDate incidentDate, String incidentCaseId, int incidentNum,
                    String disciplinaryAct, String incidentTypeCode,
                    BigDecimal disciplinaryDuration) {
                this.m_incident = incident;
                this.m_incidentDate = incidentDate;
                this.m_incidentCaseId = incidentCaseId;
                this.m_incidentNum = incidentNum;
                this.m_disciplinaryAct = disciplinaryAct;
                this.m_incidentTypeCode = incidentTypeCode;
                this.m_disciplinaryDur = disciplinaryDuration;
            }

            /**
             * Gets the disciplinary act.
             *
             * @return String
             */
            public String getDisciplinaryAct() {
                return m_disciplinaryAct;
            }

            /**
             * Gets the disciplinary duration.
             *
             * @return Big decimal
             */
            public BigDecimal getDisciplinaryDuration() {
                return m_disciplinaryDur;
            }

            /**
             * Gets the incident.
             *
             * @return Conduct incident
             */
            public ConductIncident getIncident() {
                return m_incident;
            }

            /**
             * Gets the incident case id.
             *
             * @return String
             */
            public String getIncidentCaseId() {
                return m_incidentCaseId;
            }

            /**
             * Gets the incident date.
             *
             * @return Plain date
             */
            public PlainDate getIncidentDate() {
                return m_incidentDate;
            }

            /**
             * Gets the incident num.
             *
             * @return int
             */
            public int getIncidentNum() {
                return m_incidentNum;
            }

            /**
             * Gets the incident type code.
             *
             * @return String
             */
            public String getIncidentTypeCode() {
                return m_incidentTypeCode;
            }
        }

        String m_disability;
        List<RowData> m_rowDataCollection;
        ILStudentDiscipline m_sdData;
        SisStudent m_student;
        Collection<ConductIncident> m_studentIncidents;

        /**
         * Return the current row's conduct action.
         *
         * @return current row's conduct action
         */
        public RowData getCurrentRowData() {
            return m_rowDataCollection.get(getCurrentRow());
        }

        /**
         * Gets the disability codes.
         *
         * @return List
         */
        public List<String> getDisabilityCodes() {
            return m_sdData.m_iepDisabilityMap.get(getCurrentRowData().getIncident().getOid());
        }

        /**
         * Returns the Entry enrollment record.
         *
         * @param incidentDate PlainDate
         * @return StudentEnrollment
         */
        public StudentEnrollment getEffectiveEnrollment(PlainDate incidentDate) {
            StudentEnrollment enrollment = m_sdData.m_helper.getEnrollmentForDate(m_student.getOid(),
                    incidentDate,
                    CODE_EFFECTIVE_ENROLLMENT);

            return enrollment;
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() + "]";

            return name;
        }

        /**
         * Initialize entity.
         *
         * Set conduct actions
         * If there aren't any actions, report an error to validation
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean)
                throws X2BaseException {
            super.intitialize(data, bean);

            m_sdData = (ILStudentDiscipline) data;
            m_student = (SisStudent) bean;
            m_studentIncidents = m_sdData.m_conductIncidentMap.get(m_student.getOid());

            // Exclude all incidents with excluded Home School
            Iterator<ConductIncident> incidentIterator = m_studentIncidents.iterator();
            while (incidentIterator.hasNext()) {
                ConductIncident incident = incidentIterator.next();
                StudentEnrollment effectiveEnrollment = getEffectiveEnrollment(incident.getIncidentDate());
                if (effectiveEnrollment == null ||
                        effectiveEnrollment.getSchool() == null ||
                        BooleanAsStringConverter.TRUE.equals(
                                effectiveEnrollment.getSchool().getFieldValueByBeanPath(m_sdData.m_excludeSklField))) {
                    incidentIterator.remove();
                }
            }

            /**
             * Structure of the map:
             * <Incident Date,
             * <Incident Case ID,
             * <ConductAction,
             * <Incident Type Code, Disciplinary Dur>>>>
             *
             * ConductAction used instead of Disciplinary Action code because it is possible case
             * where will several
             * actions with the same Disciplinary Action code, but each action should be pulling
             * separately with its own
             * Disciplinary Duration, that will be divided equally between all
             * "Incident Date-Incident Case ID" groups.
             * Tree Maps are used to maintain order of export
             */
            Map<PlainDate, Map<String, Map<ConductAction, Map<String, BigDecimal>>>> rowDataMap =
                    new TreeMap<PlainDate, Map<String, Map<ConductAction, Map<String, BigDecimal>>>>();

            /*
             * Fill map.
             * Each action of each incident from common group "Incident Date-Incident Case ID" is
             * divided between
             * all these incidents by Disciplinary duration. So we use prepared map of actions
             * (m_conductActionMap) where
             * are stored all needed actions for group "Incident Date-Incident Case ID" and these
             * actions are used by
             * each incident as its own.
             */
            for (ConductIncident incident : m_studentIncidents) {
                PlainDate incidentDate = incident.getIncidentDate();
                String incidentCaseID = incident.getIncidentId();
                String incidentCode = incident.getIncidentCode();

                Collection<ConductAction> actions = new ArrayList<ConductAction>();
                Map<String, Collection<ConductAction>> actionsByIncID = m_sdData.m_conductActionMap.get(incidentDate);
                if (actionsByIncID != null) {
                    if (actionsByIncID.get(incidentCaseID) != null) {
                        actions.addAll(actionsByIncID.get(incidentCaseID));
                    }
                }

                for (ConductAction action : actions) {
                    String disciplinaryAct = action.getActionCode();
                    if (!StringUtils.isEmpty(disciplinaryAct)) {
                        Map<String, Map<ConductAction, Map<String, BigDecimal>>> incidentIdInnerMap =
                                rowDataMap.get(incidentDate);
                        if (incidentIdInnerMap == null) {
                            incidentIdInnerMap = new TreeMap<String, Map<ConductAction, Map<String, BigDecimal>>>();
                            rowDataMap.put(incidentDate, incidentIdInnerMap);
                        }

                        Map<ConductAction, Map<String, BigDecimal>> actionInnerMap =
                                incidentIdInnerMap.get(incidentCaseID);
                        if (actionInnerMap == null) {
                            actionInnerMap = new TreeMap<ConductAction, Map<String, BigDecimal>>(
                                    new Comparator<ConductAction>() {

                                        @Override
                                        public int compare(ConductAction o1, ConductAction o2) {
                                            int retValue = o1.getActionStartDate().compareTo(o2.getActionStartDate());
                                            if (retValue == 0) {
                                                retValue = o1.getActionCode().compareTo(o2.getActionCode());
                                            }
                                            return retValue;
                                        }

                                    });
                            incidentIdInnerMap.put(incidentCaseID, actionInnerMap);
                        }

                        Map<String, BigDecimal> typeCodeDurInnerMap = actionInnerMap.get(action);

                        if (typeCodeDurInnerMap == null) {
                            typeCodeDurInnerMap = new TreeMap<String, BigDecimal>();
                            actionInnerMap.put(action, typeCodeDurInnerMap);
                        }

                        /*
                         * Disciplinary duration is null because it will recalculate below with new
                         * number of
                         * incident type code.
                         */
                        if (incidentCode != null) {
                            typeCodeDurInnerMap.put(incidentCode, null);
                        }
                        /*
                         * Reinitialize Disciplinary Duration equally between all elements of
                         * "Incident Date-Incident Case ID" group.
                         */
                        if (!typeCodeDurInnerMap.isEmpty()) {
                            BigDecimal commonActDuration = action.getActionPenaltyTime();
                            BigDecimal numOfTypeCodes = new BigDecimal(typeCodeDurInnerMap.size());
                            BigDecimal eachRecordDuration =
                                    commonActDuration.divide(numOfTypeCodes, 2, RoundingMode.HALF_UP);

                            for (Entry<String, BigDecimal> entry : typeCodeDurInnerMap.entrySet()) {
                                entry.setValue(eachRecordDuration);
                            }
                        }
                    }
                }
            }

            /*
             * Initialize list of records.
             */
            m_rowDataCollection = new ArrayList<RowData>();

            Collection<PlainDate> dates = rowDataMap.keySet();
            for (PlainDate incidentDate : dates) {
                int incidentNum = 1;
                Map<String, Map<ConductAction, Map<String, BigDecimal>>> incidentCaseIDMap =
                        rowDataMap.get(incidentDate);
                Collection<String> incidentCaseIDs = incidentCaseIDMap.keySet();
                for (String incidentCaseID : incidentCaseIDs) {
                    Map<ConductAction, Map<String, BigDecimal>> actionsMap = incidentCaseIDMap.get(incidentCaseID);
                    Collection<ConductAction> actions = actionsMap.keySet();
                    for (ConductAction action : actions) {
                        Map<String, BigDecimal> incidentTypeCodeMap = actionsMap.get(action);
                        Collection<String> incidentTypeCodes = incidentTypeCodeMap.keySet();
                        for (String incidentTypeCode : incidentTypeCodes) {
                            BigDecimal disciplinaryDuration = incidentTypeCodeMap.get(incidentTypeCode);

                            RowData rowData = new RowData(action.getIncident(),
                                    incidentDate,
                                    incidentCaseID,
                                    incidentNum,
                                    action.getActionCode(),
                                    incidentTypeCode,
                                    disciplinaryDuration);

                            m_rowDataCollection.add(rowData);
                            ++incidentNum;
                        }
                    }
                }
            }

            setRowCount(m_rowDataCollection.size());
            m_sdData.m_totalCount += m_rowDataCollection.size();
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

        /**
         * Gets the multiple primary disabilities error.
         *
         * @return State report validation error
         */
        protected StateReportValidationError getMultiplePrimaryDisabilitiesError() {
            FieldDefinition fieldDefinition = m_sdData.getFieldDefinition(FIELD_DISABILITY_TYPE);
            StateReportValidationError error = new StateReportValidationError(this, fieldDefinition,
                    "Student must have only 1 primary disability, choice of disability is not determinant",
                    "Student's name = " + STYLE_BOLD + m_student.getNameView() + STYLE_END);
            return error;
        }

    }

    /**
     * Retrieve the disability of the conduct incident's student.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveDisability implements FieldRetriever {
        private static final String CODE_NONE = "99";

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
            StudentDisciplineEntity sde = (StudentDisciplineEntity) entity;

            String result = null;

            List<String> disabilityCodes = sde.getDisabilityCodes();

            if (disabilityCodes != null && disabilityCodes.size() >= 1) {
                // if primary disabilities codes are different,
                // create retrieval error and export this record with code from any of these Primary
                // Disabilities
                if (codesAreDifferent(disabilityCodes)) {
                    sde.addRetrievalError(FIELD_DISABILITY_TYPE, sde.getMultiplePrimaryDisabilitiesError());
                }

                result = getDisabilityType(disabilityCodes);
            }
            if (StringUtils.isEmpty(result)) {
                SisStudent std = (SisStudent) entity.getBean();
                String code = (String) std.getFieldValueByBeanPath(m_fieldStdSpedDiagnosis);
                if (!StringUtils.isEmpty(code)) {
                    result = lookupStateValue(SisStudent.class, m_fieldStdSpedDiagnosis, code);
                }
            }
            // if still empty, use none
            if (StringUtils.isEmpty(result)) {
                result = CODE_NONE;
            }
            return result;
        }

        /**
         * Determine if codes are different.
         *
         * @param disabilityCodes List<String>
         * @return true, if successful
         */
        private boolean codesAreDifferent(List<String> disabilityCodes) {
            boolean codesAreDifferent = false;

            String comparedCode = null;
            for (String code : disabilityCodes) {
                if (comparedCode == null) {
                    comparedCode = code;
                    continue;
                }

                if (!comparedCode.equals(code)) {
                    codesAreDifferent = true;
                    break;
                }
            }

            return codesAreDifferent;
        }

        /**
         * Return first any disability code.
         *
         * @param disabilityCodes List<String>
         * @return String
         */
        private String getDisabilityType(List<String> disabilityCodes) {
            String result = null;

            String disabilityCode = disabilityCodes.iterator().next();
            if (!StringUtils.isEmpty(disabilityCode)) {
                result = lookupStateValue(IepDisability.class,
                        IepDisability.COL_DISABILITY_CODE,
                        disabilityCode);
            }

            return result;
        }

    }

    /**
     * Retrieve the disciplinary action code/duration
     *
     * Acceptable parameters:
     * - "ACTION" : returns the ConductAction actionCode's state code
     * - "DURATION" : returns the ConductAction's penaltyTime's state code.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveDisciplinary implements FieldRetriever {
        private static final String PARAM_ACTION = "ACTION";
        private static final String PARAM_DURATION = "DURATION";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StudentDisciplineEntity sde = (StudentDisciplineEntity) entity;
            String param = (String) field.getParameter();
            Object result = null;

            if (param.equals(PARAM_ACTION)) {
                String disciplinaryAct = sde.getCurrentRowData().getDisciplinaryAct();
                if (!StringUtils.isEmpty(disciplinaryAct)) {
                    result = lookupStateValue(ConductAction.class, ConductAction.COL_ACTION_CODE, disciplinaryAct);
                }
            } else if (param.equals(PARAM_DURATION)) {
                result = sde.getCurrentRowData().getDisciplinaryDuration();
            }

            return result;
        }

    }

    /**
     * Retrieves incident number
     *
     * A sequential number that identifies multiple incidents received by an
     * individual in a single day. This number will be "01" unless a student
     * has multiple incidents in one day or if the student has multiple
     * Disciplinary Action codes for the same incident
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveIncident implements FieldRetriever {
        private static final String PARAM_INCIDENT_CODE = "CODE";
        private static final String PARAM_INCIDENT_DATE = "DATE";
        private static final String PARAM_INCIDENT_ID = "ID";
        private static final String PARAM_INCIDENT_NUMBER = "NUMBER";

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
            StudentDisciplineEntity sde = (StudentDisciplineEntity) entity;

            Object result = null;

            if (PARAM_INCIDENT_CODE.equals(field.getParameter())) {
                String incidentTypeCode = sde.getCurrentRowData().getIncidentTypeCode();
                if (incidentTypeCode != null) {
                    result = lookupStateValue(ConductIncident.class, ConductIncident.COL_INCIDENT_CODE,
                            incidentTypeCode);
                }
            } else if (PARAM_INCIDENT_DATE.equals(field.getParameter())) {
                result = sde.getCurrentRowData().getIncidentDate();
            } else if (PARAM_INCIDENT_NUMBER.equals(field.getParameter())) {
                result = Integer.valueOf(sde.getCurrentRowData().getIncidentNum());
            } else if (PARAM_INCIDENT_ID.equals(field.getParameter())) {
                result = sde.getCurrentRowData().getIncidentCaseId();
            }

            return result;
        }

    }

    /**
     * Retrieve the RCDTS of the student's home/serving school
     *
     * Parameter this retriever accepts:
     * - "H" for the home school's RCDTS
     * - "S" for the serving school's RCDTS.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRcdts implements FieldRetriever {
        private static final String HOME_SCHOOL = "H";
        private static final String SERVING_SCHOOL = "S";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            StudentDisciplineEntity sde = (StudentDisciplineEntity) entity;

            ILStudentDiscipline sdData = (ILStudentDiscipline) data;
            String rcdts = null;
            PlainDate incidentDate = sde.getCurrentRowData().getIncidentDate();
            StudentEnrollment effectiveEnr = sde.getEffectiveEnrollment(incidentDate);
            if (param.equals(HOME_SCHOOL) &&
                    effectiveEnr != null && effectiveEnr.getSchool() != null) {
                String codeForNonFte = (String) effectiveEnr.getFieldValueByBeanPath(sdData.m_fieldEnrSklHome);
                if (!StringUtils.isEmpty(codeForNonFte)) {
                    rcdts = sdData.lookupStateValue(StudentEnrollment.class, m_fieldEnrSklHome, codeForNonFte);
                }
                if (StringUtils.isEmpty(rcdts)) {
                    rcdts = (String) effectiveEnr.getSchool().getFieldValueByBeanPath(sdData.m_fieldSchoolCode);
                }
            } else if (param.equals(SERVING_SCHOOL)) {
                if (StringUtils.isEmpty(rcdts) && sde.getCurrentRowData().getIncident() != null
                        && sde.getCurrentRowData().getIncident().getSchool() != null) {
                    rcdts = (String) sde.getCurrentRowData().getIncident().getSchool()
                            .getFieldValueByBeanPath(m_fieldSchoolCode);
                }
            }
            return rcdts;
        }
    }

    /**
     * Retrieve a bean property and strip off invalid characters.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {
        private static final String CALC_ID = "DISC-STRIPCHAR";
        private static final String ILLEGAL_NAME_CHARACTERS = "[^A-Za-z -]";
        private Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);

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
            String cleanValue = null;
            String nameValue = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (nameValue != null) {
                Matcher matcher = m_illegalNameCharacters.matcher(nameValue);
                cleanValue = matcher.replaceAll("");
            } else {
                cleanValue = "";
            }

            return cleanValue;
        }
    }

    protected static final String ACTION_STATUS_RESCHEDULED = "R";
    protected static final String ALIAS_CONDUCT_ACT_STATUS = "DOE CNDT ACT STATUS";
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_ENR_SKL_HOME = "DOE SCHOOL HOME";
    protected static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_RCDTS_FOR_SERVING_SCHOOL = "DOE OUTPLACEMENT DI";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVICE";
    protected static final String ALIAS_STD_SPED_DIANOSIS = "DOE SPED DIAGNOSIS";
    protected static final String FIELD_DISABILITY_TYPE = "Disability Type";
    protected static final String FIELD_DISCIPLINARY_ACTION = "Disciplinary Act";
    protected static final String FIELD_DISCIPLINARY_DURATION = "Disciplinary Dur";

    protected static final BigDecimal MIN_PENALTY_TIME = new BigDecimal(0.5);

    protected static final String PARAM_REPORT_DATE = "reportDate";

    protected static final String RETRIEVER_DISC_DISABILITY = "DISC-DISABILITY";
    protected static final String RETRIEVER_DISC_RCDTS = "DISC-RCDTS";
    protected static final String RETRIEVER_DISC_INCIDENT = "DISC-INCIDENT";
    protected static final String RETRIEVER_DISC_DISCIPLINARY = "DISC-DISCIPLINARY";

    protected static final String SCHOOL_NAME_OUTPLACEMENT = "OUTPLACEMENT";

    protected static final String SQL_COUNT = "count(*)";

    protected Map<PlainDate, Map<String, Collection<ConductAction>>> m_conductActionMap;
    protected Map<String, List<ConductIncident>> m_conductIncidentMap;
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected String m_excludeSklField;
    protected String m_fieldConductActStatus;
    protected String m_fieldDistrictCode;
    protected String m_fieldEnrSklHome;
    protected String m_fieldRcdtsForServingSchool;
    protected String m_fieldSchoolCode;
    protected String m_fieldServiceSchoolCode;
    protected String m_fieldStdSpedDiagnosis;
    protected StudentHistoryHelper m_helper;
    protected Map<String, List<String>> m_iepDisabilityMap;
    protected PlainDate m_reportDate;
    protected Map<String, List<StudentEnrollment>> m_studentEnrollmentMap;
    protected Map<String, Collection<StudentSchool>> m_secondaryOutplacementSchoolMap;
    protected int m_totalCount;

    /**
     * The header prints out the name of the export, total count of records,
     * the file name, and the date.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        StringBuilder heading = new StringBuilder(100);
        heading.append("Student Discipline Groups");
        heading.append(',');
        heading.append(m_totalCount);
        heading.append(',');
        heading.append(getFileName());
        heading.append(',');
        heading.append(m_dateFormat.format(m_reportDate));
        heading.append(',');
        heading.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        heading.append("\n");
        return heading.toString();
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        // initialize fields
        initializeFields();

        X2Criteria conductIncidentCriteria = getConductIncidentCriteria();
        QueryByCriteria incidentQuery = new QueryByCriteria(ConductIncident.class, conductIncidentCriteria);
        m_conductIncidentMap = getBroker().getGroupedCollectionByQuery(incidentQuery,
                ConductIncident.COL_STUDENT_OID,
                200);

        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

        // load conduct actions
        loadConductActions();

        // load disabilities (for use in RetrieveDisability)
        loadIepDisabilitiesCodes();

        // Additional rule for secondary OUTPLACEMENT school
        // X2Criteria secondaryOutplacementCriteria = new X2Criteria();
        // SubQuery studentEnrollmentSubQuery = new SubQuery(ConductIncident.class,
        // ConductIncident.COL_STUDENT_OID, getConductIncidentCriteria());
        // secondaryOutplacementCriteria.addIn(StudentSchool.COL_STUDENT_OID,
        // studentEnrollmentSubQuery);
        // secondaryOutplacementCriteria.addEqualTo(StudentSchool.REL_SCHOOL + PATH_DELIMITER +
        // SisSchool.COL_NAME, SCHOOL_NAME_OUTPLACEMENT);
        // secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_TYPE,
        // Integer.valueOf(StudentSchool.SECONDARY));
        // secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID,
        // getOrganization().getCurrentContextOid());
        // QueryByCriteria secondaryOutplacementQuery = new QueryByCriteria(StudentSchool.class,
        // secondaryOutplacementCriteria);
        // m_secondaryOutplacementSchoolMap =
        // getBroker().getGroupedCollectionByQuery(secondaryOutplacementQuery,
        // StudentSchool.COL_STUDENT_OID, 1024);

        SubQuery incidentSubQuery = new SubQuery(ConductIncident.class,
                ConductIncident.COL_STUDENT_OID,
                conductIncidentCriteria);

        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addIn(X2BaseBean.COL_OID, incidentSubQuery);
        applyInputCriteria(studentCriteria, true, null);
        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
        applyInputSort(studentQuery, null);

        // set the entity and query
        setEntityClass(StudentDisciplineEntity.class);
        setQuery(studentQuery);

        Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RETRIEVER_DISC_DISCIPLINARY, new RetrieveDisciplinary());
        calcs.put(RETRIEVER_DISC_INCIDENT, new RetrieveIncident());
        calcs.put(RETRIEVER_DISC_RCDTS, new RetrieveRcdts());
        calcs.put(RETRIEVER_DISC_DISABILITY, new RetrieveDisability());
        calcs.put(RetrieveStripNameChar.CALC_ID, new RetrieveStripNameChar());
        super.addCalcs(calcs);
    }

    /**
     * Gets the codes with state.
     *
     * @param beanClass Class<? extends X2BaseBean>
     * @param beanPath String
     * @return collection of codes that have state values in reference table.
     */
    private Collection<String> getCodesWithState(Class<? extends X2BaseBean> beanClass, String beanPath) {
        ArrayList<String> codes = new ArrayList<String>();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty property = new ModelProperty(beanClass, beanPath, dictionary);
        DataDictionaryField field = dictionary.findDataDictionaryField(property.getFieldId());
        ReferenceTable refTable = field.getReferenceTable();
        Collection<ReferenceCode> refCodes = refTable.getReferenceCodes();

        for (ReferenceCode code : refCodes) {
            if (!StringUtils.isEmpty(code.getStateCode())) {
                codes.add(code.getCode());
            }
        }

        return codes;
    }

    /**
     * Retrieve ConductIncident records.
     *
     * @return the query to report
     */
    private X2Criteria getConductIncidentCriteria() {
        X2Criteria conductIncidentCriteria = new X2Criteria();
        conductIncidentCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE,
                getOrganization().getCurrentContext().getStartDate());
        return conductIncidentCriteria;
    }

    /**
     * Generate the filename for this export.
     *
     * @return <district name>_<report date>_001.csv
     */
    private String getFileName() {
        StringBuilder fileName = new StringBuilder();
        fileName.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        fileName.append("_");
        fileName.append(new SimpleDateFormat("MMddyyyy").format(m_reportDate));
        fileName.append("_");
        fileName.append("001.csv");
        return fileName.toString();
    }

    /**
     * Initialize fields. Translate aliases to javanames
     */
    private void initializeFields() {
        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldServiceSchoolCode = translateAliasToJavaName(ALIAS_SERVICE_SCHOOL_CODE, true);
        m_fieldConductActStatus = translateAliasToJavaName(ALIAS_CONDUCT_ACT_STATUS, true);
        m_fieldRcdtsForServingSchool = translateAliasToJavaName(ALIAS_RCDTS_FOR_SERVING_SCHOOL, true);
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_fieldStdSpedDiagnosis = translateAliasToJavaName(ALIAS_STD_SPED_DIANOSIS, true);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }
        m_fieldEnrSklHome = translateAliasToJavaName(ALIAS_ENR_SKL_HOME, true);
    }

    /**
     * Load conduct actions for each "Incident Date-Incident Case ID" group.
     */
    private void loadConductActions() {
        SubQuery conductIncidentSubQuery = new SubQuery(ConductIncident.class,
                X2BaseBean.COL_OID,
                getConductIncidentCriteria());
        X2Criteria conductActionCriteria = new X2Criteria();
        conductActionCriteria.addIn(ConductAction.COL_INCIDENT_OID, conductIncidentSubQuery);

        // Get action codes that have state value.
        Collection<String> codes = getCodesWithState(ConductAction.class, ConductAction.COL_ACTION_CODE);

        conductActionCriteria.addIn(ConductAction.COL_ACTION_CODE, codes);

        // Exclude actions having status with state code "R"
        Collection<String> rStatusCodes = new ArrayList<String>();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_CONDUCT_ACT_STATUS);
        ReferenceTable refTable = field.getReferenceTable();
        if (refTable != null) {
            Collection<ReferenceCode> refCodes = refTable.getReferenceCodes();
            for (ReferenceCode code : refCodes) {
                if (!StringUtils.isEmpty(code.getStateCode())
                        && ACTION_STATUS_RESCHEDULED.equals(code.getStateCode())) {
                    rStatusCodes.add(code.getCode());
                }
            }
            conductActionCriteria.addNotIn(m_fieldConductActStatus, rStatusCodes);
        }

        conductActionCriteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_PENALTY_TIME, MIN_PENALTY_TIME);

        BeanQuery conductActionQuery = new BeanQuery(ConductAction.class, conductActionCriteria);

        String[] columns = {ConductAction.REL_INCIDENT + PATH_DELIMITER + ConductIncident.COL_INCIDENT_DATE,
                ConductAction.REL_INCIDENT + PATH_DELIMITER + ConductIncident.COL_INCIDENT_ID};
        int[] sizes = {200, 200};

        m_conductActionMap = getBroker().getGroupedCollectionByQuery(conductActionQuery, columns, sizes);
    }

    /**
     * Load disabilities for incidents.
     */
    private void loadIepDisabilitiesCodes() {
        SubQuery iepDisabilitySubQuery = new SubQuery(ConductIncident.class,
                ConductIncident.COL_STUDENT_OID,
                getConductIncidentCriteria());
        X2Criteria iepDisabilityCodesCriteria = new X2Criteria();
        iepDisabilityCodesCriteria.addIn(IepDisability.COL_STUDENT_OID, iepDisabilitySubQuery);

        iepDisabilityCodesCriteria.addGreaterOrEqualThanField(IepDisability.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                SisStudent.REL_CONDUCT_INCIDENTS + ModelProperty.PATH_DELIMITER + ConductIncident.COL_INCIDENT_DATE,
                IepDisability.REL_IEP_DATA + ModelProperty.PATH_DELIMITER + IepData.COL_START_DATE);
        iepDisabilityCodesCriteria.addLessOrEqualThanField(IepDisability.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                SisStudent.REL_CONDUCT_INCIDENTS + ModelProperty.PATH_DELIMITER + ConductIncident.COL_INCIDENT_DATE,
                IepDisability.REL_IEP_DATA + ModelProperty.PATH_DELIMITER + IepData.COL_END_DATE);

        iepDisabilityCodesCriteria.addEqualTo(IepDisability.COL_PRIMARY_INDICATOR, BooleanAsStringConverter.TRUE);

        String[] columns =
                {IepDisability.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.REL_CONDUCT_INCIDENTS +
                        ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID, IepDisability.COL_DISABILITY_CODE};
        ReportQueryByCriteria reportQueryByCriteria =
                new ReportQueryByCriteria(IepDisability.class, columns, iepDisabilityCodesCriteria);
        m_iepDisabilityMap = getBroker().getGroupedColumnCollectionByQuery(reportQueryByCriteria, 200);
    }

}
