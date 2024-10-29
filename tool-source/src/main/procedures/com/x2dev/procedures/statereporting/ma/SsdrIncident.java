/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ma;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
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
import com.x2dev.sis.model.beans.ConductOffense;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Massachusetts state report for SSDR Incident reporting.
 * This class implements the data export for Mass SSDR Incident report.
 *
 * @author X2 Development Corporation
 */
public class SsdrIncident extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the SSDR Incident export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class SsdrEntity extends StateReportEntity {
        private int m_actionCount;
        private Boolean m_isSped;
        private List<String> m_offensesIncidentCodes;
        private SsdrIncident m_data;
        private String m_incidentOid;
        private String m_incidentPrimaryCode;
        private String m_incidentSchoolOid;

        /**
         * Constructor, no argument for dynamic creation.
         */
        public SsdrEntity() {
            super();
        }

        /**
         * Filter out some incidents that may not have reportable offense code or action
         * discipline code.
         *
         * @return StateReportValidationError
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;

            return error;
        }

        /**
         * Returns the display name of the represented entity.
         *
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            StringBuilder buffer = new StringBuilder();
            ConductIncident incident = (ConductIncident) getBean();

            buffer.append("[OFF ID: ");
            buffer.append(incident.getIncidentId());
            buffer.append(", DATE: ");
            buffer.append(incident.getIncidentDate());
            buffer.append(", CODE: ");
            buffer.append(incident.getIncidentCode());
            buffer.append("]");

            return buffer.toString();
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_data = (SsdrIncident) data;
            ConductIncident incident = (ConductIncident) bean;

            m_incidentOid = incident.getOid();
            m_incidentPrimaryCode = incident.getIncidentCode();
            m_incidentSchoolOid = incident.getSchoolOid();
            m_offensesIncidentCodes = new ArrayList<String>();
            m_actionCount = 0;

            SisStudent student = incident.getStudent();
            String spedPlacement = (String) student.getFieldValueByAlias(ALIAS_DOE_34_SPED_PLACEMENT);
            m_isSped = Boolean.valueOf(
                    !(StringUtils.isEmpty(spedPlacement) || m_data.m_doe34NonSpedCodes.contains(spedPlacement)));

            // Add the primary offense code if reportable.
            if (m_data.m_incidentCodes.keySet().contains(incident.getIncidentCode())) {
                m_offensesIncidentCodes.add(incident.getIncidentCode());
            }

            // Search for secondary reportable offenses in the multiple offenses list.
            Collection<ConductOffense> offenses = m_data.m_offenses.get(incident.getOid());
            if (offenses != null) {
                for (ConductOffense offense : offenses) {
                    if (m_data.m_incidentCodes.keySet().contains(offense.getIncidentCode())) {
                        m_offensesIncidentCodes.add(offense.getIncidentCode());
                    }
                }
            }

            // Search for reportable disciplinary actions.
            Collection<ConductAction> actions = m_data.m_actions.get(incident.getOid());
            if (actions != null) {
                for (ConductAction action : actions) {
                    if (m_data.m_actionCodes.keySet().contains(action.getActionCode())) {
                        // For non-SPED, only include actions where penalty count > 10.
                        if (m_isSped.booleanValue() || m_offensesIncidentCodes.size() > 0
                                || (action.getActionPenaltyTime() != null
                                        && action.getActionPenaltyTime().intValue() > 10)) {
                            m_actionCount++;
                        }
                    }
                }
            }
        }

        /**
         * Returns the number of reportable discipline actions are on this incident.
         *
         * @return int count of reportable action codes.
         */
        protected int getActionCount() {
            return m_actionCount;
        }

        /**
         * Returns one incident code from the incident.
         *
         * @param index int
         * @return String
         */
        protected String getIncidentCode(int index) {
            String code = null;
            if (index >= 0 && index < m_offensesIncidentCodes.size()) {
                code = m_offensesIncidentCodes.get(index);
            }
            return code;
        }

        /**
         * Returns the number of incident codes in this incident.
         *
         * @return int
         */
        protected int getIncidentCount() {
            return m_offensesIncidentCodes.size();
        }

        /**
         * Returns the incident Oid.
         *
         * @return String
         */
        protected String getIncidentOid() {
            return m_incidentOid;
        }

        /**
         * Returns the incident Primary Offense Code.
         *
         * @return String
         */
        protected String getIncidentPrimaryCode() {
            return m_incidentPrimaryCode;
        }

        /**
         * Returns the incident School Oid.
         *
         * @return String
         */
        protected String getIncidentSchoolOid() {
            return m_incidentSchoolOid;
        }

        /**
         * Return indicator of whether the student on the incident is a SPED student.
         *
         * @return true, if is sped
         */
        protected boolean isSped() {
            return m_isSped.booleanValue();
        }
    }

    /**
     * Input Definition Parameters
     */
    public static final String PARAM_CONSOLIDATE_ACTIONS = "consolidateActions";
    public static final String PARAM_END_DATE = "endDate";
    public static final String PARAM_EXPORT_TYPE = "exportType";
    public static final String PARAM_REQUIRE_REPORT_STATUS = "requireReportStatus";
    public static final String PARAM_START_DATE = "startDate";
    public static final String PARAM_INCLUDE_SIF_SCHOOL = "includeSifSchoolId";

    /**
     * Column names/headings for the export file.
     */
    private static final String FIELD_ID_SSDR_A_OFF_ID = "OFF ID";
    private static final String FIELD_ID_SSDR_B_OFF_DATE = "OFF DATE";
    private static final String FIELD_ID_SSDR_C_SCH_NAME = "SCH NAME";
    private static final String FIELD_ID_SSDR_D_SCH_CODE = "SCH CODE";
    private static final String FIELD_ID_SSDR_E_OFF_IND = "OFF IND";
    private static final String FIELD_ID_SSDR_F_DISC_IND = "DISC IND";
    private static final String FIELD_ID_SSDR_G_OT1 = "OT1";
    private static final String FIELD_ID_SSDR_H_OT2 = "OT2";
    private static final String FIELD_ID_SSDR_I_OT3 = "OT3";
    private static final String FIELD_ID_SSDR_J_OT4 = "OT4";
    private static final String FIELD_ID_SSDR_K_OT5 = "OT5";
    private static final String FIELD_ID_SSDR_L_VS = "VS";
    private static final String FIELD_ID_SSDR_M_VSCHP = "VSCHP";
    private static final String FIELD_ID_SSDR_N_VNSCHP = "VNSCHP";
    private static final String FIELD_ID_SSDR_O_VU = "VU";
    private static final String FIELD_ID_SSDR_P_OFFES = "OFFES";
    private static final String FIELD_ID_SSDR_Q_OFFENS = "OFFENS";
    private static final String FIELD_ID_SSDR_R_OFFEU = "OFFEU";
    private static final String FIELD_ID_SSDR_S_OFF_DESC = "OFF DESC";
    private static final String FIELD_ID_SSDR_T_PHYS_INJ = "PHYS INJ";
    private static final String FIELD_ID_SSDR_U_OFF_DESC2 = "OFF DESC2";
    private static final String FIELD_ID_SSDR_V_OFF_DESC3 = "OFF DESC3";
    private static final String FIELD_ID_SSDR_W_OFF_DESC4 = "OFF DESC4";
    private static final String FIELD_ID_SSDR_X_OFF_DESC5 = "OFF DESC5";
    private static final String FIELD_ID_SSDR_Y_INC_DESC = "INC DESC";

    /**
     * Aliases
     */
    // STUDENT aliases
    private static final String ALIAS_DOE_15_SCHOOL = "DOE 15";
    private static final String ALIAS_DOE_34_SPED_PLACEMENT = "DOE 34";
    private static final String ALIAS_DOE_STATUS_REPORT = "DOE Status";
    private static final String ALIAS_SKL_SIF_DISTRICT_ID = "skl-sif-district-id";

    // STUDENT_CONDUCT_INCIDENT aliases
    private static final String ALIAS_SSDR_12_NUMBER_OF_STUDENT_VICTIMS = "SSDR 12";
    private static final String ALIAS_SSDR_13_NUMBER_OF_STAFF_VICTIMS = "SSDR 13";
    private static final String ALIAS_SSDR_14_NUMBER_OF_NON_SCHOOL_VICTIMS = "SSDR 14";
    private static final String ALIAS_SSDR_15_NUMBER_OF_UNKNOWN_VICTIMS = "SSDR 15";
    private static final String ALIAS_SSDR_16_NUMBER_OF_STUDENT_OFFENDERS = "SSDR 16";
    private static final String ALIAS_SSDR_17_NUMBER_OF_NON_STUDENT_OFFENDERS = "SSDR 17";
    private static final String ALIAS_SSDR_18_NUMBER_OF_UNKNOWN_OFFENDERS = "SSDR 18";
    private static final String ALIAS_SSDR_19_DESCRIPTION = "SSDR 19";
    private static final String ALIAS_SSDR_20_PHYSICAL_INJURY = "SSDR 20";

    // STUDENT_CONDUCT_OFFENSE aliases
    private static final String ALIAS_SSDR_OFFENSE_DESCRIPTION = "DOE OFFENSE DESCRIPTION";

    /*
     * Other internal constants
     */
    private static final String CODE_DEFAULT_OFFENSE = "500";
    private static final String CODE_DISTRICT_OID = "*dst";
    private static final String CODE_DOE_34_NON_SPED_STATE = "00";
    private static final String CODE_X2_NONCRIMINAL = "18";
    private static final String CODE_X2_TRUANCY = "XTR";
    private static final String FORMAT_DATE = "MM/dd/yyyy";
    private static final String REGEX_FILTER_NON_ALPHA_NUMERIC = "[^\\p{Alnum}]";
    private static final String REGEX_FILTER_NON_ASCII = "[^\\p{ASCII}]";
    private static final String REGEX_NUMERIC = "[0123456789]*";
    private static final String STRING_N = "N";
    private static final String STRING_Y = "Y";
    private static final String STRING_ZERO = "0";
    private static final String SEPARATOR_COMMA = ",";

    /*
     * Instance variables.
     */
    protected ArrayList<String> m_nonCriminalCodes;
    protected List<String> m_doe34NonSpedCodes;
    protected Map<String, Collection<ConductAction>> m_actions;
    protected Map<String, Collection<ConductOffense>> m_offenses;
    protected Map<String, ReferenceCode> m_offenseCodesMap = new HashMap<String, ReferenceCode>();
    protected Map<String, String> m_actionCodes;
    protected Map<String, String> m_incidentCodes;
    protected String m_lastIncidentId;

    private PlainDate m_endDate;
    private boolean m_requireReportStatus;
    private PlainDate m_startDate;
    private String m_sklDstrIdField = null;
    private List<String> m_includeSifSchoolIds = null;

    /**
     * Retrieves the discipline indicator. This indicates one of the action codes
     * is reportable.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveDisciplineIndicator implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            String indicator = STRING_N;
            SsdrEntity ssdrEntity = (SsdrEntity) entity;

            if (ssdrEntity.getActionCount() > 0) {
                indicator = STRING_Y;
            }

            return indicator;
        }
    }

    /**
     * Retrieves the incident indicator. This indicates one of the incident codes
     * is reportable.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveIncidentIndicator implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            String indicator = STRING_N;
            SsdrEntity ssdrEntity = (SsdrEntity) entity;

            if (ssdrEntity.getIncidentCount() > 0) {
                indicator = STRING_Y;
            }

            return indicator;
        }
    }

    /**
     * Some values stored as 'Integer' are formatted as string "00000000"
     * and need to be reformatted to included in the export.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveNumberValue implements FieldRetriever {

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
            String resultValue = null;
            String numberField = (String) data.getProperty(entity.getBean(), field.getBeanPath());

            if (numberField != null) {
                try {
                    resultValue = Integer.toString((int) Double.parseDouble(numberField));
                } catch (NumberFormatException nfe) {
                    StateReportValidationError error =
                            new StateReportValidationError(entity, field, "Invalid number", numberField);
                    entity.addRetrievalError(field.getFieldId(), error);
                }
            }

            return resultValue;
        }
    }

    /**
     * Returns an incident offense code for the incident.
     * More than one can be retrieved.
     * The field.other value must contain an Integer(1-5) to select which
     * offense code to return.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveOffenseCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SsdrEntity ssdrEntity = (SsdrEntity) entity;

            String offenseCode = null;
            String fieldId = field.getFieldId();
            String incidentOid = ssdrEntity.getIncidentOid();
            String incidentSchoolOid = ssdrEntity.getIncidentSchoolOid();
            String firstIncidentCode = ssdrEntity.getIncidentPrimaryCode();
            String stateOffenseCode = null;
            boolean isPrimaryCode = false;

            if (FIELD_ID_SSDR_G_OT1.equals(fieldId)) {
                isPrimaryCode = true;
                offenseCode = firstIncidentCode;
            } else {
                Collection<ConductOffense> offenses = m_offenses.get(incidentOid);
                int i = 0;
                if (offenses != null && offenses.size() > 0) {
                    for (ConductOffense conductOffense : offenses) {
                        if (!conductOffense.getPrimary()) {
                            i++;

                            if (i == 1 && FIELD_ID_SSDR_H_OT2.equals(fieldId)) {
                                offenseCode = conductOffense.getIncidentCode();
                                break;
                            } else if (i == 2 && FIELD_ID_SSDR_I_OT3.equals(fieldId)) {
                                offenseCode = conductOffense.getIncidentCode();
                                break;
                            } else if (i == 3 && FIELD_ID_SSDR_J_OT4.equals(fieldId)) {
                                offenseCode = conductOffense.getIncidentCode();
                                break;
                            } else if (i == 4 && FIELD_ID_SSDR_K_OT5.equals(fieldId)) {
                                offenseCode = conductOffense.getIncidentCode();
                                break;
                            }
                        }
                    }
                }
            }

            if (StringUtils.isEmpty(offenseCode)) {
                stateOffenseCode = CODE_DEFAULT_OFFENSE;
            } else {
                String referenceCodeKey = incidentSchoolOid + "-" + offenseCode;

                if (m_offenseCodesMap.containsKey(referenceCodeKey)) {
                    ReferenceCode referenceCode = m_offenseCodesMap.get(referenceCodeKey);
                    stateOffenseCode = referenceCode.getStateCode();
                }

                if (StringUtils.isEmpty(stateOffenseCode)) {
                    referenceCodeKey = CODE_DISTRICT_OID + "-" + offenseCode;

                    if (m_offenseCodesMap.containsKey(referenceCodeKey)) {
                        ReferenceCode referenceCode = m_offenseCodesMap.get(referenceCodeKey);
                        stateOffenseCode = referenceCode.getStateCode();
                    }

                    if (StringUtils.isEmpty(stateOffenseCode)) {
                        stateOffenseCode =
                                lookupReferenceCodeByBeanPath(ConductOffense.class, ConductOffense.COL_INCIDENT_CODE,
                                        offenseCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                }

                // If the first code is 18 then all codes after should be 500
                // If the second to fifth code is 18 then it should be 500
                if (!StringUtils.isEmpty(stateOffenseCode)
                        && (!isPrimaryCode)
                        && (m_nonCriminalCodes.contains(firstIncidentCode)
                                || m_nonCriminalCodes.contains(stateOffenseCode))) {
                    stateOffenseCode = CODE_DEFAULT_OFFENSE;
                }

                if (StringUtils.isEmpty(stateOffenseCode)) {
                    stateOffenseCode = CODE_DEFAULT_OFFENSE;
                }
            }

            return stateOffenseCode;

        }
    }

    /**
     * Returns Offense Description field from the Conduct Incident table.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveOffenseDescription implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            String offenseDescription = "";
            String fieldId = field.getFieldId();

            ConductIncident incident = (ConductIncident) entity.getBean();
            Collection<ConductOffense> offenses = incident.getConductOffenses();

            int i = 0;
            if (FIELD_ID_SSDR_S_OFF_DESC.equals(fieldId)) {
                offenseDescription = (String) incident.getFieldValueByAlias(ALIAS_SSDR_19_DESCRIPTION);
            } else if (FIELD_ID_SSDR_U_OFF_DESC2.equals(fieldId)) {
                for (ConductOffense conductOffense : offenses) {
                    if (!conductOffense.getPrimary()) {
                        i++;
                        if (i == 1) {
                            // offenseDescription = conductOffense.getDescription();
                            offenseDescription =
                                    (String) conductOffense.getFieldValueByAlias(ALIAS_SSDR_OFFENSE_DESCRIPTION);
                            break;
                        }
                    }
                }
            } else if (FIELD_ID_SSDR_V_OFF_DESC3.equals(fieldId)) {
                for (ConductOffense conductOffense : offenses) {
                    if (!conductOffense.getPrimary()) {
                        i++;
                        if (i == 2) {
                            // offenseDescription = conductOffense.getDescription();
                            offenseDescription =
                                    (String) conductOffense.getFieldValueByAlias(ALIAS_SSDR_OFFENSE_DESCRIPTION);
                            break;
                        }
                    }
                }
            } else if (FIELD_ID_SSDR_W_OFF_DESC4.equals(fieldId)) {
                for (ConductOffense conductOffense : offenses) {
                    if (!conductOffense.getPrimary()) {
                        i++;
                        if (i == 3) {
                            // offenseDescription = conductOffense.getDescription();
                            offenseDescription =
                                    (String) conductOffense.getFieldValueByAlias(ALIAS_SSDR_OFFENSE_DESCRIPTION);
                            break;
                        }
                    }
                }
            } else if (FIELD_ID_SSDR_X_OFF_DESC5.equals(fieldId)) {
                for (ConductOffense conductOffense : offenses) {
                    if (!conductOffense.getPrimary()) {
                        i++;
                        if (i == 4) {
                            // offenseDescription = conductOffense.getDescription();
                            offenseDescription =
                                    (String) conductOffense.getFieldValueByAlias(ALIAS_SSDR_OFFENSE_DESCRIPTION);
                            break;
                        }
                    }
                }
            } else if (FIELD_ID_SSDR_Y_INC_DESC.equals(fieldId)) {
                offenseDescription = incident.getDescription();
            }

            if (offenseDescription != null) {
                // Remove comma, quotes, carriage return, line feed and non-ASCII characters.
                offenseDescription = cleanUpFreeTextField(offenseDescription);

                // Trim to 50 characters
                if (offenseDescription != null && offenseDescription.length() > 50) {
                    offenseDescription = offenseDescription.substring(0, 50);
                }
            }

            return offenseDescription;
        }
    }


    /**
     * Returns the class of the base bean for this export.
     *
     * @return Class
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getBeanClass()
     */
    @Override
    public Class getBeanClass() {
        return ConductIncident.class;
    }

    /**
     * Returns the title of the export for the validation report.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getExportTitle()
     */
    @Override
    public String getExportTitle() {
        return "SSDR Incident Report";
    }

    /**
     * Gets the include header row.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return true;
    }

    /**
     * Returns an indicator for the export formatter indicating:
     * This SSDR export should not use value wrappers.
     *
     * @return boolean
     */
    @Override
    public boolean getUseValueWrappers() {
        return false;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();

        /*
         * Define all export fields.
         */
        ArrayList<FieldDefinition> fieldDefinitions = new ArrayList<FieldDefinition>(26);
        fieldDefinitions.add(getIA_OFF_ID());
        fieldDefinitions.add(getIB_OFF_DATE());
        fieldDefinitions.add(getIC_SCH_NAME());
        fieldDefinitions.add(getID_SCH_CODE());
        fieldDefinitions.add(getIE_OFF_IND());
        fieldDefinitions.add(getIF_DISC_IND());
        fieldDefinitions.add(getIG_OT1());
        fieldDefinitions.add(getIH_OT2());
        fieldDefinitions.add(getII_OT3());
        fieldDefinitions.add(getIJ_OT4());
        fieldDefinitions.add(getIK_OT5());
        fieldDefinitions.add(getIL_VS());
        fieldDefinitions.add(getIM_VSCHP());
        fieldDefinitions.add(getIN_VNSCHP());
        fieldDefinitions.add(getIO_VU());
        fieldDefinitions.add(getIP_OFFES());
        fieldDefinitions.add(getIQ_OFFENS());
        fieldDefinitions.add(getIR_OFFEU());
        fieldDefinitions.add(getIS_OFF_DESC());
        fieldDefinitions.add(getIT_PHYS_INJ());
        fieldDefinitions.add(getIU_OFF_DESC2());
        fieldDefinitions.add(getIV_OFF_DESC3());
        fieldDefinitions.add(getIW_OFF_DESC4());
        fieldDefinitions.add(getIX_OFF_DESC5());
        fieldDefinitions.add(getIY_INC_DESC());
        setFieldDefinitions(fieldDefinitions);

        // Set the entity class for this export.
        setEntityClass(SsdrEntity.class);

        if (getSetupErrors().size() == 0) {
            // Build the query for the incidents.
            Criteria incidentCriteria = buildQueryCriteria();
            QueryByCriteria query = new QueryByCriteria(ConductIncident.class, incidentCriteria);
            query.setPathOuterJoin(ConductIncident.REL_CONDUCT_ACTIONS);
            query.setPathOuterJoin(ConductIncident.REL_CONDUCT_OFFENSES);
            query.addOrderBy(ConductIncident.COL_INCIDENT_ID, true);
            query.addOrderBy(ConductIncident.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_STATE_ID,
                    true);

            // Set the Incident query for the export.
            setQuery(query);

            // Load action and offense maps by incident.
            loadActionMaps(incidentCriteria);
            loadOffenseMaps(incidentCriteria);
            loadOffenseCodes();
        }
    }

    /**
     * Initialize fields.
     */
    public void initializeFields() {
        m_sklDstrIdField = translateAliasToJavaName(ALIAS_SKL_SIF_DISTRICT_ID, true);
        // Load Input Definition Parameters
        String includeIds = (String) getParameter(PARAM_INCLUDE_SIF_SCHOOL);
        if (!StringUtils.isEmpty(includeIds)) {
            List<String> rcdOids = new ArrayList<String>(Arrays.asList(includeIds.split(SEPARATOR_COMMA)));
            X2Criteria sifDistrIdCriteria = new X2Criteria();
            sifDistrIdCriteria.addIn(X2BaseBean.COL_OID, rcdOids);
            QueryByCriteria byCriteria = new QueryByCriteria(ReferenceCode.class, sifDistrIdCriteria);
            Collection<ReferenceCode> refCodes = getBroker().getCollectionByQuery(byCriteria);
            m_includeSifSchoolIds = new ArrayList();
            for (ReferenceCode code : refCodes) {
                m_includeSifSchoolIds.add(code.getCode());
            }
        }
        // Load Input Definition Parameters
        m_startDate = (PlainDate) getParameter(PARAM_START_DATE);
        m_endDate = (PlainDate) getParameter(PARAM_END_DATE);
        m_requireReportStatus = ((Boolean) getParameter(PARAM_REQUIRE_REPORT_STATUS)).booleanValue();

        // Maps of incident and action codes values to state code value.
        m_incidentCodes = getReferenceMap(ConductIncident.class, ConductIncident.COL_INCIDENT_CODE);
        m_actionCodes = getReferenceMap(ConductAction.class, ConductAction.COL_ACTION_CODE);

        m_nonCriminalCodes = lookupRefCodesByStateCode(ConductIncident.class, ConductIncident.COL_INCIDENT_CODE,
                CODE_X2_NONCRIMINAL);

        if (m_incidentCodes.size() == 0) {
            addSetupError("No conduct incident state codes found", "");
        }
        if (m_actionCodes.size() == 0) {
            addSetupError("No conduct action state codes found", "");
        }

        loadNonSpedCodes();
        m_lastIncidentId = "";
    }


    /**
     * Clean Up a Text field by removing commas, quotes, carriage returns, line feeds and non-ASCII
     * characters from a string.
     *
     * @param textField
     *
     * @return String
     */
    protected String cleanUpFreeTextField(String textField) {
        String cleanField = textField;

        if (cleanField != null && cleanField.length() > 0) {
            cleanField = cleanField.replaceAll(REGEX_FILTER_NON_ASCII, " ");
            cleanField = cleanField.replaceAll(REGEX_FILTER_NON_ALPHA_NUMERIC, " ");
        }

        return cleanField;
    }

    /**
     * Build Field definition for Field A, Incident Id (OFF ID).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIA_OFF_ID() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_A_OFF_ID,
                ConductIncident.COL_INCIDENT_ID,
                null, false, 1, 12, REGEX_NUMERIC,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for Field B, Incident date (OFF DATE).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIB_OFF_DATE() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_B_OFF_DATE,
                ConductIncident.COL_INCIDENT_DATE,
                null, false, 10, 10, null,
                new SimpleDateFormat(FORMAT_DATE),
                null, null, null);
        return field;
    }

    /**
     * Build Field definition for Field C, School Name (SCH NAME).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIC_SCH_NAME() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_C_SCH_NAME,
                ConductIncident.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_NAME,
                null, false, 1, 50, null,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for Field D, School Code (SCH CODE).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getID_SCH_CODE() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_D_SCH_CODE,
                ConductIncident.REL_SCHOOL + ModelProperty.PATH_DELIMITER
                        + translateAliasToJavaName(ALIAS_DOE_15_SCHOOL, true),
                null, false, 8, 8, REGEX_NUMERIC,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for Field E, Incident indicator (OFF IND).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIE_OFF_IND() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_E_OFF_IND,
                LABEL_PREFIX_CHAR + "Incident Indicator",
                STRING_Y, false, 1, 1, "Y|N",
                null, new RetrieveIncidentIndicator(), null, null);
        return field;
    }

    /**
     * Build Field definition for Field F, Discipline indicator (DISC IND).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIF_DISC_IND() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_F_DISC_IND,
                LABEL_PREFIX_CHAR + "Discipline Indicator",
                STRING_Y, false, 1, 1, "Y|N",
                null, new RetrieveDisciplineIndicator(), null, null);
        return field;
    }

    /**
     * Build Field definition for Field G, Incident type code 1 (OT1).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIG_OT1() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_G_OT1,
                null,
                null, false, 0, 50, null,
                null, new RetrieveOffenseCode(), null, null);

        return field;
    }

    /**
     * Build Field definition for Field H, Incident type code 2 (OT2).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIH_OT2() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_H_OT2,
                null,
                null, false, 0, 50, null,
                null, new RetrieveOffenseCode(), null, null);

        return field;
    }

    /**
     * Build Field definition for Field I, Incident type code 3 (OT3).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getII_OT3() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_I_OT3,
                null,
                null, false, 0, 50, null,
                null, new RetrieveOffenseCode(), null, null);

        return field;
    }

    /**
     * Build Field definition for Field J, Incident type code 4 (OT4).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIJ_OT4() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_J_OT4,
                null,
                null, false, 0, 50, null,
                null, new RetrieveOffenseCode(), null, null);

        return field;
    }

    /**
     * Build Field definition for Field K, Incident type code 5 (OT5).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIK_OT5() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_K_OT5,
                null,
                null, false, 0, 50, null,
                null, new RetrieveOffenseCode(), null, null);

        return field;
    }

    /**
     * Build Field definition for Field L, Student victims (VS).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIL_VS() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_L_VS,
                translateAliasToJavaName(ALIAS_SSDR_12_NUMBER_OF_STUDENT_VICTIMS, true),
                STRING_ZERO, false, 1, 3, REGEX_NUMERIC,
                null, new RetrieveNumberValue(), null, null);
        return field;
    }

    /**
     * Build Field definition for Field M, School staff victims (VSCHP).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIM_VSCHP() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_M_VSCHP,
                translateAliasToJavaName(ALIAS_SSDR_13_NUMBER_OF_STAFF_VICTIMS, true),
                STRING_ZERO, false, 1, 3, REGEX_NUMERIC,
                null, new RetrieveNumberValue(), null, null);
        return field;
    }

    /**
     * Build Field definition for Field N, Student victims (VNSCHP).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIN_VNSCHP() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_N_VNSCHP,
                translateAliasToJavaName(ALIAS_SSDR_14_NUMBER_OF_NON_SCHOOL_VICTIMS, true),
                STRING_ZERO, false, 1, 3, REGEX_NUMERIC,
                null, new RetrieveNumberValue(), null, null);
        return field;
    }

    /**
     * Build Field definition for Field O, Unknown victims (VU).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIO_VU() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_O_VU,
                translateAliasToJavaName(ALIAS_SSDR_15_NUMBER_OF_UNKNOWN_VICTIMS, true),
                STRING_ZERO, false, 1, 3, REGEX_NUMERIC,
                null, new RetrieveNumberValue(), null, null);
        return field;
    }

    /**
     * Build Field definition for Field P, Student offenders(OFFES).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIP_OFFES() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_P_OFFES,
                translateAliasToJavaName(ALIAS_SSDR_16_NUMBER_OF_STUDENT_OFFENDERS, true),
                STRING_ZERO, false, 1, 3, REGEX_NUMERIC,
                null, new RetrieveNumberValue(), null, null);
        return field;
    }

    /**
     * Build Field definition for Field Q, Student offenders(OFFENS).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIQ_OFFENS() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_Q_OFFENS,
                translateAliasToJavaName(ALIAS_SSDR_17_NUMBER_OF_NON_STUDENT_OFFENDERS, true),
                STRING_ZERO, false, 1, 3, REGEX_NUMERIC,
                null, new RetrieveNumberValue(), null, null);
        return field;
    }

    /**
     * Build Field definition for Field R, Student offenders(OFFEU).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIR_OFFEU() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_R_OFFEU,
                translateAliasToJavaName(ALIAS_SSDR_18_NUMBER_OF_UNKNOWN_OFFENDERS, true),
                STRING_ZERO, false, 1, 3, REGEX_NUMERIC,
                null, new RetrieveNumberValue(), null, null);
        return field;
    }

    /**
     * Build Field definition for Field T, Physical Injury (PHYS_INJ).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIT_PHYS_INJ() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_T_PHYS_INJ,
                translateAliasToJavaName(ALIAS_SSDR_20_PHYSICAL_INJURY, true),
                "X", true, 1, 1, "Y|N|X",
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for Field S, School Name (OFF_DESC).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIS_OFF_DESC() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_S_OFF_DESC,
                translateAliasToJavaName(ALIAS_SSDR_19_DESCRIPTION, true),
                null, false, 0, 50, null,
                null, new RetrieveOffenseDescription(),
                null, null);
        return field;
    }

    /**
     * Build Field definition for Field U, Offense 2 Description (OFF_DESC2).
     *
     * @return a FieldDefinition translateAliasToJavaName(SSDR_OFF_DESC2, true),
     */
    protected FieldDefinition getIU_OFF_DESC2() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_U_OFF_DESC2,
                translateAliasToJavaName(ALIAS_SSDR_OFFENSE_DESCRIPTION, false),
                null, false, 0, 50, null,
                null, new RetrieveOffenseDescription(),
                null, null);
        return field;
    }

    /**
     * Build Field definition for Field V, Offense 3 Description (OFF_DESC3).
     *
     * @return a FieldDefinition translateAliasToJavaName(SSDR_OFF_DESC3, true),
     */
    protected FieldDefinition getIV_OFF_DESC3() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_V_OFF_DESC3,
                translateAliasToJavaName(ALIAS_SSDR_OFFENSE_DESCRIPTION, false),
                null, false, 0, 50, null,
                null, new RetrieveOffenseDescription(),
                null, null);
        return field;
    }

    /**
     * Build Field definition for Field W, Offense 4 Description (OFF_DESC4).
     *
     * @return a FieldDefinition translateAliasToJavaName(SSDR_OFF_DESC4, true),
     */
    protected FieldDefinition getIW_OFF_DESC4() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_W_OFF_DESC4,
                translateAliasToJavaName(ALIAS_SSDR_OFFENSE_DESCRIPTION, false),
                null, false, 0, 50, null,
                null, new RetrieveOffenseDescription(),
                null, null);
        return field;
    }

    /**
     * Build Field definition for Field X, Offense 4 Description (OFF_DESC5).
     *
     * @return a FieldDefinition translateAliasToJavaName(SSDR_OFF_DESC5, true),
     */
    protected FieldDefinition getIX_OFF_DESC5() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_X_OFF_DESC5,
                translateAliasToJavaName(ALIAS_SSDR_OFFENSE_DESCRIPTION, false),
                null, false, 0, 50, null,
                null, new RetrieveOffenseDescription(),
                null, null);
        return field;
    }

    /**
     * Build Field definition for Field Y, Offense 4 Description (OFF_DESC5).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIY_INC_DESC() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_Y_INC_DESC,
                translateAliasToJavaName(ALIAS_SSDR_OFFENSE_DESCRIPTION, false),
                null, false, 0, 50, null,
                null, new RetrieveOffenseDescription(),
                null, null);
        return field;
    }

    /**
     * Returns a Query object for the incidents that should be reported
     * in the export.
     *
     * This uses the input parameters to generate the query.
     *
     * @return criteria
     */
    private Criteria buildQueryCriteria() {
        /*
         * Build selection criteria for incidents in three parts.
         * The incident must occur within the date range specified and
         * contain one of the state reported incident codes.
         *
         * Avoid SQLServer no distinct on BLOB restriction.
         */
        Criteria subCriteria = new Criteria();
        if (isSchoolContext()) {
            subCriteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, getSchool().getOid());
        } else if (m_includeSifSchoolIds != null) {
            subCriteria.addIn(ConductIncident.REL_SCHOOL + ModelProperty.PATH_DELIMITER
                    + m_sklDstrIdField, m_includeSifSchoolIds);
        }

        subCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_startDate);
        subCriteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_endDate);
        if (m_requireReportStatus) {
            subCriteria.addEqualTo(ConductIncident.REL_STUDENT + ModelProperty.PATH_DELIMITER
                    + translateAliasToJavaName(ALIAS_DOE_STATUS_REPORT, true), "Report");
        }

        Criteria orCriteria1 = new Criteria();
        if (m_nonCriminalCodes.size() > 0) {
            orCriteria1.addNotIn(ConductIncident.COL_INCIDENT_CODE, m_nonCriminalCodes);
        }
        orCriteria1.addIn(ConductIncident.COL_INCIDENT_CODE, m_incidentCodes.keySet());

        Criteria orCriteria2 = new Criteria();
        if (m_nonCriminalCodes.size() > 0) {
            orCriteria2.addIn(ConductIncident.COL_INCIDENT_CODE, m_nonCriminalCodes);
        }
        orCriteria2.addIn(
                ConductIncident.REL_CONDUCT_ACTIONS + ModelProperty.PATH_DELIMITER + ConductAction.COL_ACTION_CODE,
                m_actionCodes.keySet());

        Criteria orCriteria = new Criteria();
        orCriteria.addOrCriteria(orCriteria1);
        orCriteria.addOrCriteria(orCriteria2);

        subCriteria.addAndCriteria(orCriteria);

        SubQuery query = new SubQuery(ConductIncident.class, X2BaseBean.COL_OID, subCriteria, true);
        query.setPathOuterJoin(ConductIncident.REL_CONDUCT_ACTIONS);

        Criteria incidentCriteria = new Criteria();
        incidentCriteria.addIn(X2BaseBean.COL_OID, query);

        return incidentCriteria;
    }

    /**
     * Returns the reference code map for the given bean and property.
     *
     * @param beanClass Class
     * @param property String
     * @return A Map of base codes (String objects) to state codes (String objects); all base codes
     *         will correspond to non-empty state codes
     */
    private Map getReferenceMap(Class beanClass, String property) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());

        Map codeMap = new HashMap();

        DataDictionaryField field = dictionary.findDataDictionaryField(beanClass.getName(), property);
        Iterator codes = field.getReferenceTable().getReferenceCodes(getBroker()).iterator();

        while (codes.hasNext()) {
            ReferenceCode code = (ReferenceCode) codes.next();
            if (!StringUtils.isEmpty(code.getStateCode()) &&
                    !code.getStateCode().equals(CODE_X2_TRUANCY)) {
                codeMap.put(code.getCode(), code.getStateCode());
            }
        }

        return codeMap;
    }

    /**
     * Build a map of actions by incident for faster loading.
     *
     * @param incidentCriteria Criteria
     */
    private void loadActionMaps(Criteria incidentCriteria) {
        SubQuery incidentQuery = new SubQuery(ConductIncident.class, X2BaseBean.COL_OID, incidentCriteria);
        incidentQuery.setPathOuterJoin(ConductIncident.REL_CONDUCT_ACTIONS);
        incidentQuery.setPathOuterJoin(ConductIncident.REL_CONDUCT_OFFENSES);

        Criteria criteria = new Criteria();
        criteria.addIn(ConductAction.COL_INCIDENT_OID, incidentQuery);
        criteria.addIn(ConductAction.COL_ACTION_CODE, m_actionCodes.keySet());
        QueryByCriteria query = new QueryByCriteria(ConductAction.class, criteria);

        m_actions = getBroker().getGroupedCollectionByQuery(query, ConductAction.COL_INCIDENT_OID, 20);
    }

    /**
     * Loads the "DOE 34" codes that have "00" as the state code.
     */
    private void loadNonSpedCodes() {
        m_doe34NonSpedCodes = new ArrayList<String>(128);

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());

        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_DOE_34_SPED_PLACEMENT);

        if (field != null) {
            ReferenceTable referenceTable = field.getReferenceTable();

            if (referenceTable != null) {
                Collection<ReferenceCode> referenceCodes = referenceTable.getReferenceCodes();

                Iterator<ReferenceCode> iterator = referenceCodes.iterator();
                while (iterator.hasNext()) {
                    ReferenceCode code = iterator.next();

                    if (CODE_DOE_34_NON_SPED_STATE.equals(code.getStateCode())) {
                        m_doe34NonSpedCodes.add(code.getCode());
                    }
                }
            }
        }
    }

    /**
     * Load Offense Codes.
     */
    private void loadOffenseCodes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field =
                dictionary.findDataDictionaryField(ConductOffense.class.getName(), ConductOffense.COL_INCIDENT_CODE);
        String referenceTableOid = field.getReferenceTableOid();

        Collection<ReferenceCode> offenseCodes = new ArrayList<ReferenceCode>();

        if (referenceTableOid != null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);

            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria, false);

            offenseCodes = getBroker().getCollectionByQuery(query);
        }

        if (offenseCodes != null) {
            for (ReferenceCode referenceCode : offenseCodes) {
                String code = referenceCode.getCode();
                String ownerOid = referenceCode.getOwnerOid();
                String key = ownerOid + "-" + code;
                m_offenseCodesMap.put(key, referenceCode);
            }
        }
    }

    /**
     * Build a map of offenses by incident for faster loading.
     *
     * @param incidentCriteria Criteria
     */
    private void loadOffenseMaps(Criteria incidentCriteria) {
        SubQuery incidentQuery = new SubQuery(ConductIncident.class, X2BaseBean.COL_OID, incidentCriteria);
        incidentQuery.setPathOuterJoin(ConductIncident.REL_CONDUCT_ACTIONS);
        incidentQuery.setPathOuterJoin(ConductIncident.REL_CONDUCT_OFFENSES);

        Criteria criteria = new Criteria();
        criteria.addIn(ConductOffense.COL_INCIDENT_OID, incidentQuery);
        criteria.addIn(ConductOffense.COL_INCIDENT_CODE, m_incidentCodes.keySet());
        criteria.addNotEqualTo(ConductOffense.COL_PRIMARY, Boolean.valueOf(true));
        QueryByCriteria query = new QueryByCriteria(ConductOffense.class, criteria);

        m_offenses = getBroker().getGroupedCollectionByQuery(query, ConductOffense.COL_INCIDENT_OID, 20);
    }

    /**
     * lookup RefCodes by it's State Code.
     *
     * @param beanClass Class
     * @param beanPath String
     * @param lookupStateCode String
     * @return ArrayList
     */
    private ArrayList lookupRefCodesByStateCode(Class beanClass, String beanPath, String lookupStateCode) {
        ArrayList refCodes = new ArrayList();
        Map<String, ReferenceCode> m_referenceCodeMap = new HashMap<String, ReferenceCode>();

        DataDictionaryField dataDictionaryField = getDataDictionaryField(beanClass, beanPath);
        ReferenceTable referenceTable = dataDictionaryField.getReferenceTable();
        m_referenceCodeMap = referenceTable.getCodeMap(getBroker());
        for (ReferenceCode referenceCode : m_referenceCodeMap.values()) {
            String stateCode = referenceCode.getStateCode();
            if (stateCode != null && stateCode.equals(lookupStateCode)) {
                refCodes.add(referenceCode.getCode());
            }
        }

        return refCodes;
    }

}
