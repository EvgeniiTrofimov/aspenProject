/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2012 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ga;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.ConductOffense;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class implements the data export for Student Record Discipline Level export.
 *
 * @author X2 Development Corporation
 */
public class SRDisciplineLevel extends StateReportData {
    /**
     * Entity class for Student Record Discipline Level export.
     *
     * @author X2 Development Corporation
     */
    public static class SRDisciplineLevelEntity extends StateReportEntity {
        List<IncidentInfo> m_incidentInfos = null;

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;

            String[] incidentTypeSplitted = getFieldValue(DIS009_INCIDENT_TYPE).split(DOT_REGEX);
            String incidentType = incidentTypeSplitted.length > 0 ? incidentTypeSplitted[0] : null;

            if (("1".equals(getFieldValue(DIS007_DATA_TYPE))) && (INCIDENT_CODES_LIST.contains(incidentType))
                    && (getFieldValue(DIS012_ACTION_CODE).equals("80"))) {
                error =
                        new StateReportValidationError(getEntityName(), "",
                                DIS012_ACTION_CODE + " = '80' cannot be with " + DIS009_INCIDENT_TYPE + " = "
                                        + INCIDENT_CODES_LIST,
                                DIS012_ACTION_CODE + " = "
                                        + getFieldValue(DIS012_ACTION_CODE) + ", " + DIS009_INCIDENT_TYPE + " = "
                                        + incidentType);
            }

            return error;
        }

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public SRDisciplineLevelEntity() {
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

        /**
         * Retrieve the info record for the current row/index.
         *
         * @return IncidentInfo
         */
        public IncidentInfo getIncidentInfo() {
            IncidentInfo info = null;
            int index = getCurrentRow();

            if (m_incidentInfos.size() > index) {
                info = m_incidentInfos.get(index);
            }

            return info;
        }

        /**
         * Initialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.
         *      stateexports.StateReportData, com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            // If the student or course are missing, skip this transcript record.
            ConductIncident incident = (ConductIncident) getBean();

            Map<String, ReferenceCode> reportableIncidentCodes = ((SRDisciplineLevel) data).m_reportableIncidentCodes;
            Map<String, ReferenceCode> reportableActionCodes = ((SRDisciplineLevel) data).m_reportableActionCodes;
            Collection<ConductOffense> offenses = ((SRDisciplineLevel) data).m_offenses.get(incident.getOid());
            Collection<ConductAction> actions = ((SRDisciplineLevel) data).m_actions.get(incident.getOid());

            int offenseCount = 0;
            int actionCount = 0;
            int rowCount = 0;
            m_incidentInfos = new ArrayList<IncidentInfo>();

            // Make a list of incident types which are non reportable if its corresponding action
            // codes are blank or null.
            ArrayList<String> incidentStateCodeList = new ArrayList<String>();
            incidentStateCodeList.add(CODE_30);
            incidentStateCodeList.add(CODE_31);
            incidentStateCodeList.add(CODE_33);
            incidentStateCodeList.add(CODE_34);

            if (incident.getStudent() != null) {
                boolean isReportable = true;
                // include all reportable offenses.
                if (offenses != null) {
                    for (ConductOffense offense : offenses) {
                        if (reportableIncidentCodes.keySet().contains(offense.getIncidentCode())) {
                            offenseCount++;
                            String incidentStateCode = "";
                            ReferenceCode incidentRefCode = reportableIncidentCodes.get(offense.getIncidentCode());
                            if (null != incidentRefCode) {
                                incidentStateCode = incidentRefCode.getStateCode();
                            }
                            if (incidentStateCodeList.contains(incidentStateCode)) {
                                isReportable = false;
                            }
                            if (offenseCount > 1 && !isReportable) {
                                IncidentInfo incidentInfo = m_incidentInfos.get(0);
                                if (incidentInfo.offense.getIncidentOid().equals(offense.getIncidentOid())) {
                                    break;
                                }
                            }
                            IncidentInfo info = new IncidentInfo();
                            info.offense = offense;
                            info.offensePlace = offenseCount;
                            m_incidentInfos.add(info);
                            rowCount = m_incidentInfos.size();
                        }
                    }
                }

                if (null == actions && !isReportable) {
                    rowCount = 0;
                }

                // Include all reportable actions.
                if (null != actions) {
                    for (ConductAction action : actions) {
                        if (reportableActionCodes.keySet().contains(action.getActionCode())) {
                            actionCount++;
                            IncidentInfo info = null;
                            if (rowCount > 0) {
                                info = m_incidentInfos.get(0);
                                if (info.action == null) {
                                    info.action = action;
                                    info.actionPlace = actionCount;
                                } else {
                                    info = new IncidentInfo();
                                    /*
                                     * if(offenseCount == 1 && actionCount > 1)
                                     * {
                                     *
                                     * if (offenses != null)
                                     * {
                                     * for (ConductOffense offense : offenses)
                                     * {
                                     * if (reportableIncidentCodes.keySet().contains(offense.
                                     * getIncidentCode()))
                                     * {
                                     * info.offense = offense;
                                     * info.offensePlace = offenseCount;
                                     * }
                                     * }
                                     * }
                                     * }
                                     */
                                    info.action = action;
                                    info.actionPlace = actionCount;
                                    m_incidentInfos.add(info);
                                    rowCount = m_incidentInfos.size();
                                }
                            } else {
                                info = new IncidentInfo();
                                info.action = action;
                                info.actionPlace = actionCount;
                                m_incidentInfos.add(info);
                                rowCount = m_incidentInfos.size();
                            }



                        }
                    }
                }
            }

            setRowCount(rowCount);
        }
    }

    /**
     * Row data information. For each row, indicate the offense or action being reported.
     * The first action will have actionPlace = 0, others > 0.
     * The first offense will have offensePlace = 0, others > 0.
     *
     * @author X2 Development Corporation
     */
    protected static class IncidentInfo {
        ConductAction action;
        ConductOffense offense;
        int actionPlace = 0;
        int offensePlace = 0;
    }

    /**
     * Constants for field names. Must reflect field names in export format.
     */
    protected static final String ALIAS_CONTINUATION = "DOE CONTINUATION";
    protected static final String ALIAS_DISCIPLINE_PROCESS = "DOE Discipline Process";
    protected static final String DIS007_DATA_TYPE = "Data Type";
    protected static final String DIS012_ACTION_CODE = "Action Code";
    protected static final String DIS011_REPORTABLE_ACTION_CODE = "90";
    protected static final String DIS009_INCIDENT_TYPE = "Incident Type";
    protected static final int DIGIT_TEN = 10;
    protected static final String FOUR_DIGIT_FORMAT = "0000";
    protected static final String TWO_DIGIT_CODE = "00";
    protected static final String SCHOOL_CODE_ALIAS = "DOE School";
    protected static final String OVERRIDE_SCHOOL_CODE_ALIAS = "DOE Override School Code";
    protected static final String DOT_STRING = ".";
    protected static final String DOT_REGEX = "\\.";
    protected static final String CODE_24 = "24";
    protected static final String CODE_30 = "30";
    protected static final String CODE_31 = "31";
    protected static final String CODE_33 = "33";
    protected static final String CODE_34 = "34";
    protected static final List<String> INCIDENT_CODES_LIST = Arrays
            .asList(CODE_24, CODE_30, CODE_31, CODE_33, CODE_34);

    /**
     * Constants for reporting information.
     */
    private static final String PARAM_QUERY_BY = "queryBy";
    private static final String PARAM_QUERY_STRING = "queryString";
    private static final String PARAM_SORT = "sort";

    /**
     * Local variables for reporting information.
     */
    protected String m_continuationOfServicesField;
    protected String m_schoolCodeField;
    protected String m_overrideSchoolCodeField;
    protected String m_disciplineProcess;

    protected Map<String, ReferenceCode> m_reportableIncidentCodes = null;
    protected Map<String, ReferenceCode> m_reportableActionCodes = null;

    protected Map<String, Collection<ConductAction>> m_actions = null;
    protected Map<String, Collection<ConductOffense>> m_offenses = null;

    /**
     * Generate and return a record type code in the format "Ennn" where "nnn" is an integer 1-999.
     * Do this by counting how many incident records we have processed for the current student.
     * When changing students, reset the count.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRecordType implements FieldRetriever {
        private DecimalFormat m_format = new DecimalFormat("000");
        private Set<String> m_incidentOids = new HashSet<String>();
        private String m_studentOid = null;

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            ConductIncident incident = (ConductIncident) entity.getBean();
            if (!incident.getStudentOid().equals(m_studentOid)) {
                m_incidentOids.clear();
                m_studentOid = incident.getStudentOid();
            }
            m_incidentOids.add(incident.getOid());

            return "E" + m_format.format(m_incidentOids.size());
        }
    }

    /**
     * Retrieve the right four digits of the incident ID.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveIncidentID implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            ConductIncident incident = (ConductIncident) entity.getBean();
            String incidentId = incident.getIncidentId();
            if (!StringUtils.isEmpty(incidentId)) {
                incidentId = incidentId.substring(incidentId.length() - 4);
            }

            return incidentId;
        }
    }

    /**
     * Retrieve the school code for the incident.
     * Check students override school code field.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSchoolCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            ConductIncident incident = (ConductIncident) entity.getBean();
            Student student = incident.getStudent();
            String schoolCode = (String) student.getFieldValueByBeanPath(m_overrideSchoolCodeField);
            if (StringUtils.isEmpty(schoolCode)) {
                School school = incident.getSchool();
                schoolCode = (String) school.getFieldValueByBeanPath(m_schoolCodeField);
            }

            return schoolCode;
        }
    }

    /**
     * Retrieve the row type.
     * Type 1 indicates the primary offense and action row.
     * Type 2 indicates an extra offense.
     * Type 3 indicates an extra action.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveDataType implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            IncidentInfo info = ((SRDisciplineLevelEntity) entity).getIncidentInfo();
            String type = null;
            if (info.actionPlace <= 1 && info.offensePlace <= 1) {
                type = "1";
            } else if (info.offensePlace > 1) {
                type = "2";
            } else if (info.actionPlace > 1) {
                type = "3";
            }

            return type;
        }
    }

    /**
     * Retrieve the incident code state value for the offense.
     * Or default to continuation for no offense.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveIncidentCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            IncidentInfo info = ((SRDisciplineLevelEntity) entity).getIncidentInfo();
            Map<String, ReferenceCode> reportableIncidentCodes = ((SRDisciplineLevel) data).m_reportableIncidentCodes;
            String code = null;

            if (info.offensePlace > 0) {
                ReferenceCode refCode = reportableIncidentCodes.get(info.offense.getIncidentCode());
                if (refCode != null) {
                    code = refCode.getStateCode();
                }
            } else if (info.actionPlace > 1) {
                code = TWO_DIGIT_CODE;
            }
            return code;
        }
    }

    /**
     * Retrieve the incident severity level-local code value for the offense.
     * Or default to continuation for no offense.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveIncidentLocalCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            IncidentInfo info = ((SRDisciplineLevelEntity) entity).getIncidentInfo();

            Map<String, ReferenceCode> reportableIncidentCodes = ((SRDisciplineLevel) data).m_reportableIncidentCodes;

            String code = "";

            if (info.offensePlace > 0) {
                ReferenceCode refCode = reportableIncidentCodes.get(info.offense.getIncidentCode());
                if (refCode != null) {
                    String[] severityStateCodes = null;
                    String stateCode = refCode.getStateCode();
                    if (stateCode != null && stateCode.contains(DOT_STRING)) {
                        severityStateCodes = stateCode.split(DOT_REGEX);
                        code = severityStateCodes[1];
                    }
                }
            }


            return code;
        }
    }


    /**
     * Retrieve the action code state value for the action.
     * Or default to continuation for no offense.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveActionCode implements FieldRetriever {
        private static final String OTHER_CODE_80 = "80";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            IncidentInfo info = ((SRDisciplineLevelEntity) entity).getIncidentInfo();

            Map<String, ReferenceCode> reportableActionCodes = ((SRDisciplineLevel) data).m_reportableActionCodes;

            String code = OTHER_CODE_80;
            if (info.actionPlace > 0) {
                ReferenceCode refActionCode = reportableActionCodes.get(info.action.getActionCode());
                if (refActionCode != null) {
                    code = refActionCode.getStateCode();
                }
            } else if (info.offensePlace > 1) {
                code = TWO_DIGIT_CODE;
            }
            return code;
        }
    }

    /**
     * Retrieve the action additional code information.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveActionAdditional implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            // ConductIncident incident = (ConductIncident) entity.getBean();
            IncidentInfo info = ((SRDisciplineLevelEntity) entity).getIncidentInfo();

            Map<String, ReferenceCode> reportableActionCodes = ((SRDisciplineLevel) data).m_reportableActionCodes;

            String code = null;
            if (info.actionPlace > 0) {
                ReferenceCode refCode = reportableActionCodes.get(info.action.getActionCode());
                if (refCode != null) {
                    String actionStatecode = refCode.getStateCode();
                    if (!StringUtils.isEmpty(actionStatecode)) {
                        code = actionStatecode.substring(2);
                    }
                }
            }

            return code;
        }
    }

    /**
     * Retrieve the action additional code information.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveActionService implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            IncidentInfo info = ((SRDisciplineLevelEntity) entity).getIncidentInfo();
            String flag = null;
            if (info.actionPlace > 0) {
                flag = (String) info.action.getFieldValueByBeanPath(m_continuationOfServicesField);
                if (StringUtils.isEmpty(flag)) {
                    String actionCode = entity.getFieldValue("Action Code");
                    if ("30".equals(actionCode) || "40".equals(actionCode)) {
                        flag = "N";
                    }
                }
            }

            return flag;
        }
    }

    /**
     * Retrieve the discipline process information.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveDisciplineProcess implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            IncidentInfo info = ((SRDisciplineLevelEntity) entity).getIncidentInfo();

            String disciplineProcess = null;
            if (info.actionPlace > 0) {
                disciplineProcess = (String) info.action.getFieldValueByBeanPath(m_disciplineProcess);
            }

            return disciplineProcess;
        }
    }

    /**
     * Retrieve the location code state value for the incident.
     * Report only for offense codes, not for action codes.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveLocationCode implements FieldRetriever {

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
            String code = TWO_DIGIT_CODE;
            IncidentInfo info = ((SRDisciplineLevelEntity) entity).getIncidentInfo();
            if (info.actionPlace > 1) {
                code = "";
            } else {
                code = (String) data.getProperty(entity.getBean(), field.getBeanPath());
            }
            return code;
        }
    }

    /**
     * Retrieve the no of penalty days and converting into 4 digit format.
     *
     * @author X2 Development Corporation
     */

    protected class RetrievePenaltyDays implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            IncidentInfo info = ((SRDisciplineLevelEntity) entity).getIncidentInfo();
            BigDecimal penaltyDays = new BigDecimal(0);
            if (null != info && null != info.action) {
                penaltyDays = info.action.getActionPenaltyTime();
            }
            double doubleValue = penaltyDays.floatValue() * DIGIT_TEN;
            DecimalFormat formatter = new DecimalFormat(FOUR_DIGIT_FORMAT);
            String value = formatter.format(doubleValue);
            return value;
        }
    }

    /**
     * Retrieve the Staff Id of removing staff in specific action cases.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStaffId implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            ConductIncident incident = (ConductIncident) entity.getBean();
            String staffIdPath = (String) field.getParameter();
            String actionCode = entity.getFieldValue(DIS012_ACTION_CODE);
            Staff staff = incident.getReferralStaff();
            String staffId = null;

            if (DIS011_REPORTABLE_ACTION_CODE.equals(actionCode) && staff != null && staffIdPath != null) {
                staffId = (String) staff.getFieldValueByBeanPath(staffIdPath);
            }

            return staffId;
        }
    }

    /**
     * Initialize the data module.
     */
    @Override
    protected void initialize() {
        m_schoolCodeField = translateAliasToJavaName(SCHOOL_CODE_ALIAS, true);
        m_overrideSchoolCodeField = translateAliasToJavaName(OVERRIDE_SCHOOL_CODE_ALIAS, true);
        m_continuationOfServicesField = translateAliasToJavaName(ALIAS_CONTINUATION, true);
        m_disciplineProcess = translateAliasToJavaName(ALIAS_DISCIPLINE_PROCESS, true);

        Criteria incidentCriteria = getIncidentCriteria();
        QueryByCriteria incidentQuery = new QueryByCriteria(ConductIncident.class, incidentCriteria);
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
                incidentQuery.addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + Student.REL_SCHOOL
                        + PATH_DELIMITER + School.COL_NAME);
                incidentQuery.addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + Student.COL_NAME_VIEW);
                break;

            case 3: // LASID
                incidentQuery.addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + Student.COL_LOCAL_ID);
                break;

            case 4: // SASID
                incidentQuery.addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + Student.COL_STATE_ID);
                break;

            default:
                incidentQuery.addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + Student.COL_NAME_VIEW);
                break;
        }
        incidentQuery.addOrderByAscending(ConductIncident.COL_STUDENT_OID);
        incidentQuery.addOrderBy(ConductIncident.COL_INCIDENT_ID, true);

        // Set the query to be used for student selection.
        setQuery(incidentQuery);
        setEntityClass(SRDisciplineLevelEntity.class);

        // Load reference codes and action/offense maps.
        loadCodes(incidentCriteria);

        // Add any retrievers or validators.
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put("GA-SR-RECORD", new RetrieveRecordType());
        calcs.put("GA-SR-ID", new RetrieveIncidentID());
        calcs.put("GA-SR-SCHOOL", new RetrieveSchoolCode());
        calcs.put("GA-SR-STAFFID", new RetrieveStaffId());
        calcs.put("GA-SR-TYPE", new RetrieveDataType());
        calcs.put("GA-SR-INC-CODE", new RetrieveIncidentCode());
        calcs.put("GA-SR-INC-LOCAL-CODE", new RetrieveIncidentLocalCode());
        calcs.put("GA-SR-DISC-PROC", new RetrieveDisciplineProcess());
        calcs.put("GA-SR-ACT-CODE", new RetrieveActionCode());
        calcs.put("GA-SR-ACT-ADDL", new RetrieveActionAdditional());
        calcs.put("GA-SR-ACT-SVC", new RetrieveActionService());
        calcs.put("GA-SR-LOC-CODE", new RetrieveLocationCode());
        calcs.put("GA-SR-PENALTY", new RetrievePenaltyDays());
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
                    School.COL_INACTIVE_INDICATOR, Boolean.FALSE);
            incidentCriteria.addEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                    School.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        }
        incidentCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE,
                getOrganization().getCurrentContext().getStartDate());

        // Check student selection criteria user input.
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        switch (queryBy) {
            case 1: // YOG
                incidentCriteria.addEqualTo(ConductIncident.REL_STUDENT + PATH_DELIMITER + Student.COL_YOG,
                        queryString);
                break;

            case 2: // LASID
                incidentCriteria.addEqualTo(ConductIncident.REL_STUDENT + PATH_DELIMITER + Student.COL_LOCAL_ID,
                        queryString);
                break;

            case 3: // SASID
                incidentCriteria.addEqualTo(ConductIncident.REL_STUDENT + PATH_DELIMITER + Student.COL_STATE_ID,
                        queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(incidentCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        return incidentCriteria;
    }

    /**
     * Load supporting maps of reference codes.
     *
     * @param incidentCriteria Criteria
     */
    private void loadCodes(Criteria incidentCriteria) {
        // Get reference table for incident codes.
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field =
                dictionary.findDataDictionaryField(ConductIncident.class.getName(), ConductIncident.COL_INCIDENT_CODE);
        String referenceTableOid = field.getReferenceTableOid();

        X2Criteria criteria = null;
        BeanQuery query = null;
        // Get a collection of reportable incident codes.
        if (referenceTableOid != null) {
            criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
            query = new BeanQuery(ReferenceCode.class, criteria, false);
            m_reportableIncidentCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 50);
        }

        // Get reference table for action codes.
        field = dictionary.findDataDictionaryField(ConductAction.class.getName(), ConductAction.COL_ACTION_CODE);
        referenceTableOid = field.getReferenceTableOid();

        // Get a collection of reportable incident codes.
        if (referenceTableOid != null) {
            criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
            query = new BeanQuery(ReferenceCode.class, criteria, false);
            m_reportableActionCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 50);
        }

        // Load all actions for the selected incidents into a map.
        SubQuery incidentQuery = new SubQuery(ConductIncident.class, X2BaseBean.COL_OID, incidentCriteria);
        criteria = new X2Criteria();
        criteria.addIn(ConductAction.COL_INCIDENT_OID, incidentQuery);
        criteria.addIn(ConductAction.COL_ACTION_CODE, m_reportableActionCodes.keySet());
        query = new BeanQuery(ConductAction.class, criteria, false);
        m_actions = getBroker().getGroupedCollectionByQuery(query, ConductAction.COL_INCIDENT_OID, 100);

        // Load all offenses for the selected incidents into a map.
        criteria = new X2Criteria();
        criteria.addIn(ConductOffense.COL_INCIDENT_OID, incidentQuery);
        criteria.addIn(ConductOffense.COL_INCIDENT_CODE, m_reportableIncidentCodes.keySet());
        query = new BeanQuery(ConductOffense.class, criteria, false);
        m_offenses = getBroker().getGroupedCollectionByQuery(query, ConductOffense.COL_INCIDENT_OID, 100);
    }
}
