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
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.FieldPosition;
import java.text.Format;
import java.text.Normalizer;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Massachusetts state report for SSDR Discipline reporting.
 * This class implements the data export for Mass SSDR Discipline report.
 *
 * @author X2 Development Corporation
 */
public class SsdrDiscipline extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the SSDR Discipline export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class SsdrEntity extends StateReportEntity {

        private List<ConductAction> m_actions;
        private java.util.Date m_eligibleReturnDate;
        private boolean m_isSped;
        private boolean m_isReportableIncident;
        private SsdrDiscipline m_data;

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
         * Returns the current action based on the entity incident
         * record and the entity current row index. The action comes
         * from the List of actions built in the initialization phase.
         *
         * Though the bean passed in is a ConductIncident, the repeating
         * entity is a ConductAction. To simplify field definition construction
         * change the reported bean type to action.
         *
         *
         * @return X 2 base bean
         */
        @Override
        public X2BaseBean getBean() {
            ConductAction action = null;
            int index = getCurrentRow();

            if (m_actions != null && index >= 0 && index < m_actions.size()) {
                action = m_actions.get(index);
            }

            return action;
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
            // Get the incident from super bean as that is the bean from the initialization.
            // Get the action from the current index position.

            ConductIncident incident = (ConductIncident) super.getBean();
            ConductAction action = (ConductAction) getBean();

            buffer.append("[OFF ID: ");
            buffer.append(incident.getIncidentId());
            buffer.append(", DATE: ");
            buffer.append(incident.getIncidentDate());
            buffer.append(", CODE: ");
            buffer.append(action.getActionCode());
            buffer.append("]");

            return buffer.toString();
        }

        /**
         * Initialize the entity. Get all staff work assignment information
         * and get a count of exportable records.
         * The initializer takes a Conduct Incident bean and makes a list of the actions.
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
            m_data = (SsdrDiscipline) data;

            ConductIncident incident = (ConductIncident) bean;

            ConductAction firstAction = null;

            m_actions = new ArrayList<ConductAction>();

            // See if the student is a SPED student.
            String spedPlacement = (String) incident.getStudent().getFieldValueByAlias(ALIAS_DOE_34_SPED_PLACEMENT);

            m_isSped = !(StringUtils.isEmpty(spedPlacement) || m_data.m_doe34NonSpedCodes.contains(spedPlacement));

            // See if the incident contains a reportable incident code.
            m_isReportableIncident = m_data.m_incidentCodes.containsKey(incident.getIncidentCode());
            Collection<ConductOffense> offenses = m_data.m_offenses.get(incident.getOid());

            if (offenses != null) {
                for (ConductOffense offense : offenses) {
                    m_isReportableIncident |= m_data.m_incidentCodes.containsKey(offense.getIncidentCode());
                }
            }

            // Search for reportable disciplinary actions.
            Collection<ConductAction> actions = m_data.m_actions.get(incident.getOid());

            if (actions != null) {
                for (ConductAction action : actions) {
                    if (m_data.m_actionCodes.keySet().contains(action.getActionCode())) {
                        // For SPED, state reportable offense or actions where penalty count > 10.
                        if (m_isSped
                                || m_isReportableIncident
                                || (action.getActionPenaltyTime() != null
                                        && action.getActionPenaltyTime().intValue() > 10)) {
                            if (m_data.m_consolidateActions) {
                                if (firstAction == null) {
                                    firstAction = action.clone();
                                    m_actions.add(firstAction);
                                } else {
                                    // Add the penalty times.
                                    BigDecimal penalty = firstAction.getActionPenaltyTime();
                                    BigDecimal penalty2 = action.getActionPenaltyTime();

                                    if (penalty == null) {
                                        penalty = penalty2;
                                    } else if (penalty2 != null) {
                                        penalty = penalty.add(penalty2);
                                    }

                                    firstAction.setActionPenaltyTime(penalty);

                                    // Set the earliest start date.
                                    PlainDate startDate = firstAction.getActionStartDate();
                                    PlainDate startDate2 = action.getActionStartDate();

                                    if (startDate == null) {
                                        startDate = startDate2;
                                    } else if (startDate2 != null) {
                                        startDate = (startDate.after(startDate2) ? startDate2 : startDate);
                                    }

                                    firstAction.setActionStartDate(startDate);
                                }
                            } else {
                                m_actions.add(action);
                            }
                        }
                    }
                }
            }

            setRowCount(m_actions.size());
        }

        /**
         * When the row is incremented, reset all row based local variables.
         *
         *
         * @param currentRow void
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#setCurrentRow(int)
         */
        @Override
        public void setCurrentRow(int currentRow) {
            super.setCurrentRow(currentRow);
            m_eligibleReturnDate = null;
        }

        /**
         * Returns the calculated return date.
         *
         * @return java.util.Date
         */
        protected java.util.Date getEligibleReturnDate() {
            return m_eligibleReturnDate;
        }

        /**
         * Return indicator of whether the student on the incident is a SPED student.
         *
         * @return true, if is sped
         */
        protected boolean isSped() {
            return m_isSped;
        }

        /**
         * Sets the eligible return date.
         *
         * @param eligibleReturnDate void
         */
        protected void setEligibleReturnDate(java.util.Date eligibleReturnDate) {
            m_eligibleReturnDate = eligibleReturnDate;
        }
    }

    /**
     * A formatter for indicator fields.
     * An indicator returns "0" or "1" from the database.
     * The output needs to be "N" or "Y" respectively.
     * this format object will make the translation
     * and allow those fields to be completed without
     * a retriever to perform the translation.
     *
     * @author X2 Development Corporation
     */
    class IndicatorFormat extends Format {

        private static final long serialVersionUID = 1L;

        /**
         * Parse the indicator value "0" or "1" into "N" or "Y" output.
         *
         * @param arg0 Object
         * @param buffer StringBuffer
         * @param position FieldPosition
         * @return StringBuffer
         * @see java.text.Format#format(java.lang.Object, java.lang.StringBuffer,
         *      java.text.FieldPosition)
         */

        @Override
        public StringBuffer format(Object arg0, StringBuffer buffer, FieldPosition position) {
            if (STRING_ONE.equals(arg0) || (arg0 != null && (arg0.toString()).startsWith(STRING_Y))) {
                buffer.append(STRING_Y);
            } else {
                buffer.append(STRING_N);
            }

            return buffer;
        }

        /**
         * Reverse translation.
         *
         * @param arg0 String
         * @param arg1 ParsePosition
         * @return Object
         * @see java.text.Format#parseObject(java.lang.String, java.text.ParsePosition)
         */
        @Override
        public Object parseObject(String arg0, ParsePosition arg1) {
            String output = null;

            if (STRING_Y.equals(arg0)) {
                output = STRING_ONE;
            } else {
                output = STRING_ZERO;
            }

            return output;
        }

    }

    /**
     * Input Definition Parameters
     */
    private static final String PARAM_CONSOLIDATE_ACTIONS = "consolidateActions";
    private static final String PARAM_END_DATE_AS_RETURN_DATE = "endDateAsReturnDate";
    private static final String PARAM_END_DATE = "endDate";
    private static final String PARAM_REQUIRE_REPORT_STATUS = "requireReportStatus";
    private static final String PARAM_START_DATE = "startDate";
    private static final String PARAM_INCLUDE_SIF_SCHOOL = "includeSifSchoolId";

    /**
     * Column names/headings for the export file.
     */
    private static final String FIELD_ID_SSDR_A_OFF_ID = "OFF ID";
    private static final String FIELD_ID_SSDR_B_OFF_DATE = "OFF DATE";
    private static final String FIELD_ID_SSDR_C_SASID = "SASID";
    private static final String FIELD_ID_SSDR_D_FN = "FN";
    private static final String FIELD_ID_SSDR_E_LN = "LN";
    private static final String FIELD_ID_SSDR_F_DOB = "DOB";
    private static final String FIELD_ID_SSDR_G_PST = "PST";
    private static final String FIELD_ID_SSDR_H_DAT = "DAT";
    private static final String FIELD_ID_SSDR_I_BI_OFF = "BI OFF";
    private static final String FIELD_ID_SSDR_J_SD = "SD";
    private static final String FIELD_ID_SSDR_K_RD = "RD";
    private static final String FIELD_ID_SSDR_L_ERD = "ERD";
    private static final String FIELD_ID_SSDR_M_DM = "DM";
    private static final String FIELD_ID_SSDR_N_AEINC = "AEINC";
    private static final String FIELD_ID_SSDR_O_AE = "AE";
    private static final String FIELD_ID_SSDR_P_NAE = "NAE";
    private static final String FIELD_ID_SSDR_Q_ERDX = "ERDX";
    private static final String FIELD_ID_SSDR_R_APPEAL = "APPEAL";
    private static final String FIELD_ID_SSDR_S_AEX = "AEX";
    private static final String FIELD_ID_SSDR_T_SOT1 = "SOT1";
    private static final String FIELD_ID_SSDR_U_SOT2 = "SOT2";
    private static final String FIELD_ID_SSDR_V_SOT3 = "SOT3";
    private static final String FIELD_ID_SSDR_W_SOT4 = "SOT4";
    private static final String FIELD_ID_SSDR_X_SOT5 = "SOT5";

    /**
     * Aliases
     */
    // STUDENT aliases
    private static final String ALIAS_DOE_34_SPED_PLACEMENT = "DOE 34";
    private static final String ALIAS_DOE_STATUS_REPORT = "DOE Status";

    // STUDENT_CONDUCT_ACTION aliases
    private static final String ALIAS_SSDR_30_ALTERNATIVE_EDUCATION_INDICATOR = "SSDR 30";
    private static final String ALIAS_SSDR_31_ALTERNATIVE_EDUCATION_TYPE = "SSDR 31";
    private static final String ALIAS_SSDR_32_NO_ALTERNATIVE_EDUCATION = "SSDR 32";
    private static final String ALIAS_SSDR_AEX = "SSDR AEX";
    private static final String ALIAS_SSDR_APPEAL = "SSDR APPEAL";
    private static final String ALIAS_SKL_SIF_DISTRICT_ID = "skl-sif-district-id";

    /*
     * Other internal constants
     */
    private static final String APPEAL_DEFAULT_NO = "No";
    private static final String CODE_DEFAULT_OFFENSE = "500";
    private static final String CODE_DEFAULT_VALUE_SSDR_32 = "4";
    private static final String CODE_DISTRICT_OID = "*dst";
    private static final String CODE_DOE_34_NON_SPED_STATE = "00";
    private static final String CODE_PROGRAM_STATUS_GENERAL_EDUCATION = "1";
    private static final String CODE_PROGRAM_STATUS_SPECIAL_EDUCATION = "2";
    private static final String CODE_X2_NONCRIMINAL = "18";
    private static final String CODE_X2_TRUANCY = "XTR";
    private static final String FORMAT_DATE = "MM/dd/yyyy";
    private static final String REGEX_ALPHANUMERIC = "[A-Za-z0123456789]*";
    private static final String REGEX_FILTER_NON_ALPHA_NUMERIC = "[^\\p{Alnum}]";
    private static final String REGEX_FILTER_NON_ASCII = "[^\\p{ASCII}]";
    private static final String REGEX_ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";
    private static final String REGEX_NUMERIC = "[0123456789]*";
    private static final String SEPARATOR_COMMA = ",";
    private static final String STRING_N = "N";
    private static final String STRING_ONE = "1";
    private static final String STRING_Y = "Y";
    private static final String STRING_ZERO = "0";

    /*
     * Date class instance variables.
     */
    protected boolean m_requireReportStatus;
    protected boolean m_consolidateActions;
    protected boolean m_useEndDateAsReturnDate;
    protected EnrollmentManager m_enrollmentManager;
    protected IndicatorFormat m_indicatorFormat;
    protected List<String> m_doe34NonSpedCodes;
    protected Map m_calendarsBySchool;
    protected Map<String, Collection<ConductAction>> m_actions;
    protected Map<String, Collection<ConductOffense>> m_offenses;
    protected Map<String, ReferenceCode> m_offenseCodesMap = new HashMap<String, ReferenceCode>();
    protected Map<String, String> m_actionCodes;
    protected Map<String, String> m_incidentCodes;
    protected Pattern m_diacriticalMarks;
    protected Pattern m_illegalNameCharacters;
    protected PlainDate m_endDate;
    protected PlainDate m_startDate;
    protected List<String> m_includeSifSchoolIds = null;
    protected SimpleDateFormat m_dateFormat;
    protected ArrayList<String> m_nonCriminalCodes;
    protected String m_sklDstrIdField = null;

    /**
     * Returns Appeal Code field from the Conduct Action.
     */
    protected class RetrieveAppealCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            ConductAction action = (ConductAction) entity.getBean();
            String extraSuspensionDays = (String) action.getFieldValueByAlias(ALIAS_SSDR_APPEAL);

            if (extraSuspensionDays == null) {
                extraSuspensionDays = APPEAL_DEFAULT_NO;
            }

            return extraSuspensionDays;
        }
    }

    /**
     * Returns the student's eligible return date.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEligibleReturnDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            SsdrEntity ssdrEntity = ((SsdrEntity) entity);

            PlainDate eligibleReturnDate = null;
            if (ssdrEntity.getEligibleReturnDate() != null) {
                eligibleReturnDate = new PlainDate(ssdrEntity.getEligibleReturnDate());
            }

            return eligibleReturnDate;
        }
    }

    /**
     * Returns Offense Code field from the Conduct Offense table.
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
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            boolean isPrimaryCode = false;
            ConductAction action = (ConductAction) entity.getBean();
            ConductIncident incident = action.getIncident();
            String fieldId = field.getFieldId();
            String firstIncidentCode = incident.getIncidentCode();
            String incidentOid = action.getIncidentOid();
            String incidentSchoolOid = incident.getSchoolOid();
            String offenseCode = null;
            String stateOffenseCode = null;

            if (FIELD_ID_SSDR_T_SOT1.equals(fieldId)) {
                isPrimaryCode = true;
                offenseCode = firstIncidentCode;
            } else {
                Collection<ConductOffense> offenses = m_offenses.get(incidentOid);
                int i = 0;
                if (offenses != null && offenses.size() > 0) {
                    for (ConductOffense conductOffense : offenses) {
                        if (!conductOffense.getPrimary()) {
                            i++;

                            if (i == 1 && FIELD_ID_SSDR_U_SOT2.equals(fieldId)) {
                                offenseCode = conductOffense.getIncidentCode();
                                break;
                            } else if (i == 2 && FIELD_ID_SSDR_V_SOT3.equals(fieldId)) {
                                offenseCode = conductOffense.getIncidentCode();
                                break;
                            } else if (i == 3 && FIELD_ID_SSDR_W_SOT4.equals(fieldId)) {
                                offenseCode = conductOffense.getIncidentCode();
                                break;
                            } else if (i == 4 && FIELD_ID_SSDR_X_SOT5.equals(fieldId)) {
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
     * Returns Other Specification field from the Conduct Incident.
     */
    protected class RetrieveOtherSpecification implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            ConductAction action = (ConductAction) entity.getBean();
            String otherSpecification = (String) action.getFieldValueByAlias(ALIAS_SSDR_AEX);

            otherSpecification = cleanUpFreeTextField(otherSpecification);

            return otherSpecification;
        }
    }

    /**
     * Returns the student's No Alternative Education type.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveNoAlternativeEducation implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            ConductAction action = (ConductAction) entity.getBean();
            String noAlternativeEducation =
                    (String) action.getFieldValueByAlias(ALIAS_SSDR_32_NO_ALTERNATIVE_EDUCATION);
            String altEdIndicator = entity.getFieldValue(ALIAS_SSDR_30_ALTERNATIVE_EDUCATION_INDICATOR);

            if (STRING_N.equals(altEdIndicator)
                    && StringUtils.isEmpty(noAlternativeEducation)) {
                noAlternativeEducation = CODE_DEFAULT_VALUE_SSDR_32;
            }

            return noAlternativeEducation;
        }
    }

    /**
     * Returns the penalty time for the action.
     *
     * @author X2 Development Corporation
     */
    protected class RetrievePenaltyTime implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            int time = 0;
            ConductAction action = (ConductAction) entity.getBean();

            if (action.getActionPenaltyTime() != null) {
                time = action.getActionPenaltyTime().toBigInteger().intValue();
            }

            return Integer.toString(time);
        }
    }

    /**
     * Returns the Return Date after checking if it is in fact the Eligible Return Date.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveReturnDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            java.util.Date returnDate = null;
            StateReportValidationError dateError = null;

            SsdrDiscipline ssdrDiscipline = (SsdrDiscipline) data;
            ConductAction action = (ConductAction) entity.getBean();
            String schoolOid = action.getSchoolOid();

            Calendar calendar = Calendar.getInstance();
            SimpleDateFormat dateFormat = ssdrDiscipline.m_dateFormat;

            java.util.Date startDate = action.getActionStartDate();
            java.util.Date endDate = action.getActionEndDate();

            /*
             * Check if action is linked to a school
             */
            if (!ssdrDiscipline.m_calendarsBySchool.containsKey(schoolOid)) {
                SisSchool school = action.getSchool();
                if (school != null) {
                    Map calendarLookup =
                            ssdrDiscipline.m_enrollmentManager.getCalendarLookup(school, m_startDate, m_endDate);

                    ssdrDiscipline.m_calendarsBySchool.put(schoolOid, calendarLookup);
                } else {
                    entity.addRetrievalError(field.getFieldId(),
                            new StateReportValidationError(entity, field, "No school for action.",
                                    "IndicentID=" + action.getIncident().getIncidentId() + ", Action="
                                            + action.getActionCode()));
                }
            }

            /*
             * Calculate a Return Date
             */
            if (endDate == null && startDate != null) {
                BigDecimal penaltyTimeRaw = action.getActionPenaltyTime();
                int penaltyTime = 0;

                if (penaltyTimeRaw != null) {
                    penaltyTime = penaltyTimeRaw.toBigInteger().intValue();
                }

                calendar.setTime(startDate);
                calendar.add(Calendar.DATE, penaltyTime);
            } else if (endDate != null) {
                calendar.setTime(endDate);

                if (!m_useEndDateAsReturnDate) {
                    calendar.add(Calendar.DATE, 1);
                }
            } else {
                String incidentId = action.getIncident().getIncidentId();

                dateError =
                        new StateReportValidationError(entity, field, "No Return Date could be produced for action. " +
                                "The action must have at least either a start or an end date.",
                                "IndicentID=" + incidentId + ", Action=" + action.getActionCode());
                entity.addRetrievalError(field.getFieldId(), dateError);
            }

            /*
             * Check if Return Date is in fact the Eligible Return Date. Only one of these values is
             * populated.
             */
            if (dateError == null) {
                SisStudent student = action.getStudent();

                if (student != null) {
                    String calendarCode = student.getCalendarCode();
                    String sisId = entity.getFieldValue(FIELD_ID_SSDR_C_SASID);

                    if (calendarCode != null) {
                        returnDate = getSessionDay(entity, ssdrDiscipline.m_calendarsBySchool, schoolOid, calendarCode,
                                calendar, sisId, dateFormat);

                        if (returnDate.after(m_endDate)) {
                            ((SsdrEntity) entity).setEligibleReturnDate(returnDate);

                            returnDate = null;
                        }
                    }
                }
            }

            return returnDate;
        }
    }

    /**
     * Returns the sped indicator for the entity.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStatusCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            String statusCode = CODE_PROGRAM_STATUS_GENERAL_EDUCATION;
            SsdrEntity ssdrEntity = (SsdrEntity) entity;

            if (ssdrEntity.isSped()) {
                statusCode = CODE_PROGRAM_STATUS_SPECIAL_EDUCATION;
            }

            return statusCode;
        }
    }

    /**
     * Returns a field value that has been stripped of illegal characters for a name field.
     *
     * For first and last names.
     *
     * @author Follett Software Company
     *
     */
    protected class RetrieveStripName implements FieldRetriever {

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
            String value = (String) getProperty(entity.getBean(), field.getBeanPath());
            value = Normalizer.normalize(value, Normalizer.Form.NFD);
            value = m_diacriticalMarks.matcher(value).replaceAll("");
            Matcher matcher = m_illegalNameCharacters.matcher(value);

            return matcher.replaceAll("");
        }
    }

    /**
     * Though the query returns ConductIncident, indicate an action here.
     * The entity generates a list of actions for its iterator and returns
     * action beans in the getBean() override.
     *
     * @return Class
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getBeanClass()
     */
    @Override
    public Class getBeanClass() {
        return ConductAction.class;
    }

    /**
     * Gets the export title.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getExportTitle()
     */
    @Override
    public String getExportTitle() {
        return "SSDR Discipline";
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

        loadNonSpedCodes();

        /*
         * Define all export fields.
         */
        ArrayList<FieldDefinition> fieldDefinitions = new ArrayList<FieldDefinition>(26);
        fieldDefinitions.add(getIA_OFF_ID());
        fieldDefinitions.add(getIB_OFF_DATE());
        fieldDefinitions.add(getIC_SASID());
        fieldDefinitions.add(getID_FN());
        fieldDefinitions.add(getIE_LN());
        fieldDefinitions.add(getIF_DOB());
        fieldDefinitions.add(getIG_PST());
        fieldDefinitions.add(getIH_DAT());
        fieldDefinitions.add(getII_BI_OFF());
        fieldDefinitions.add(getIJ_SD());
        fieldDefinitions.add(getIK_RD());
        fieldDefinitions.add(getIL_ERD());
        fieldDefinitions.add(getIM_DM());
        fieldDefinitions.add(getIN_AEINC());
        fieldDefinitions.add(getIO_AE());
        fieldDefinitions.add(getIP_NAE());
        fieldDefinitions.add(getIQ_ERDX());
        fieldDefinitions.add(getIR_APPEAL());
        fieldDefinitions.add(getIS_AEX());
        fieldDefinitions.add(getIT_SOT1());
        fieldDefinitions.add(getIU_SOT2());
        fieldDefinitions.add(getIV_SOT3());
        fieldDefinitions.add(getIW_SOT4());
        fieldDefinitions.add(getIX_SOT5());
        setFieldDefinitions(fieldDefinitions);

        // Define the entity class.
        setEntityClass(SsdrEntity.class);

        if (getSetupErrors().size() == 0) {
            // Build the query for the incidents.
            Criteria incidentCriteria = buildQueryCriteria();
            // Build the query for the incidents.
            QueryByCriteria query = new QueryByCriteria(ConductIncident.class, incidentCriteria);
            query.addOrderBy(ConductIncident.COL_INCIDENT_ID, true);
            query.addOrderBy(ConductIncident.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_STATE_ID,
                    true);

            // Set the Incident query for the export.
            setQuery(query);

            // Load action and offense maps by incident.
            loadActionMaps(incidentCriteria);
            loadOffenseCodes();
            loadOffenseMaps(incidentCriteria);
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

        m_startDate = (PlainDate) getParameter(PARAM_START_DATE);
        m_endDate = (PlainDate) getParameter(PARAM_END_DATE);
        m_requireReportStatus = false;
        if (getParameter(PARAM_REQUIRE_REPORT_STATUS) != null) {
            m_requireReportStatus = ((Boolean) getParameter(PARAM_REQUIRE_REPORT_STATUS)).booleanValue();
        }
        m_consolidateActions = false;
        if (getParameter(PARAM_CONSOLIDATE_ACTIONS) != null) {
            m_consolidateActions = ((Boolean) getParameter(PARAM_CONSOLIDATE_ACTIONS)).booleanValue();
        }
        m_useEndDateAsReturnDate = false;
        if (getParameter(PARAM_END_DATE_AS_RETURN_DATE) != null) {
            m_useEndDateAsReturnDate = ((Boolean) getParameter(PARAM_END_DATE_AS_RETURN_DATE)).booleanValue();
        }

        m_diacriticalMarks = Pattern.compile("\\p{InCombiningDiacriticalMarks}+");
        m_illegalNameCharacters = Pattern.compile(REGEX_ILLEGAL_NAME_CHARACTERS);

        // Maps of incident and action codes values to state code value.
        m_incidentCodes = getReferenceMap(ConductIncident.class, ConductIncident.COL_INCIDENT_CODE);
        m_actionCodes = getReferenceMap(ConductAction.class, ConductAction.COL_ACTION_CODE);
        m_calendarsBySchool = new HashMap();

        if (m_incidentCodes.size() == 0) {
            addSetupError("No conduct incident state codes found", "");
        }

        if (m_actionCodes.size() == 0) {
            addSetupError("No conduct action state codes found", "");
        }

        m_nonCriminalCodes = lookupRefCodesByStateCode(ConductIncident.class, ConductIncident.COL_INCIDENT_CODE,
                CODE_X2_NONCRIMINAL);

        // Enrollment manager
        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());

        // Formatter.
        m_dateFormat = new SimpleDateFormat(FORMAT_DATE);
        m_indicatorFormat = new IndicatorFormat();
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
                ConductAction.REL_INCIDENT + ModelProperty.PATH_DELIMITER +
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
                ConductAction.REL_INCIDENT + ModelProperty.PATH_DELIMITER +
                        ConductIncident.COL_INCIDENT_DATE,
                null, false, 10, 10, null,
                m_dateFormat,
                null, null, null);
        return field;
    }

    /**
     * Build Field definition for Field C, Student State Id (SASID).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIC_SASID() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_C_SASID,
                ConductAction.REL_INCIDENT + ModelProperty.PATH_DELIMITER +
                        ConductIncident.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_STATE_ID,
                null, false, 10, 10, REGEX_ALPHANUMERIC,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for Field D, Student First Name (FN).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getID_FN() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_D_FN,
                ConductAction.REL_INCIDENT + ModelProperty.PATH_DELIMITER +
                        ConductIncident.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.REL_PERSON +
                        ModelProperty.PATH_DELIMITER + SisPerson.COL_FIRST_NAME,
                null, false, 1, 30, null,
                null, new RetrieveStripName(), null, null);
        return field;
    }

    /**
     * Build Field definition for Field E, Student Last Name (LN).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIE_LN() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_E_LN,
                ConductAction.REL_INCIDENT + ModelProperty.PATH_DELIMITER +
                        ConductIncident.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.REL_PERSON +
                        ModelProperty.PATH_DELIMITER + SisPerson.COL_LAST_NAME,
                null, false, 1, 30, null,
                null, new RetrieveStripName(), null, null);
        return field;
    }

    /**
     * Build Field definition for Field F, Student Date of birth (DOB).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIF_DOB() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_F_DOB,
                ConductAction.REL_INCIDENT + ModelProperty.PATH_DELIMITER +
                        ConductIncident.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.REL_PERSON +
                        ModelProperty.PATH_DELIMITER + SisPerson.COL_DOB,
                null, false, 10, 10, null,
                m_dateFormat,
                null, null, null);
        return field;
    }

    /**
     * Build Field definition for Field G, Generalized status code (PST).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIG_PST() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_G_PST,
                LABEL_PREFIX_CHAR + "Sped Status",
                null, false, 1, 1, "[12]",
                null, new RetrieveStatusCode(), null, null);
        return field;
    }

    /**
     * Build Field definition for Field H, Penalty code (DAT).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIH_DAT() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_H_DAT,
                ConductAction.COL_ACTION_CODE,
                null, true, 1, 1, "[12345]",
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for Field I, Bodily injury incident (BI_OFF).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getII_BI_OFF() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_I_BI_OFF,
                null,
                null, false, 1, 1, null,
                null, new ParameterRetriever(), null, null);
        return field;
    }

    /**
     * Build Field definition for Field J, Start date of discipline action (SD).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIJ_SD() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_J_SD,
                ConductAction.COL_ACTION_START_DATE,
                null, false, 10, 10, null,
                m_dateFormat,
                null, null, null);
        return field;
    }

    /**
     * Build Field definition for Field K, Return date of discipline action (RD).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIK_RD() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_K_RD,
                ConductAction.COL_ACTION_END_DATE,
                null, false, 0, 10, null,
                m_dateFormat,
                new RetrieveReturnDate(), null, null);
        return field;
    }

    /**
     * Build Field definition for Field L, Eligible Return date (ERD).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIL_ERD() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_L_ERD,
                LABEL_PREFIX_CHAR + "Eligible return date",
                null, false, 0, 10, null,
                m_dateFormat,
                new RetrieveEligibleReturnDate(), null, null);
        return field;
    }

    /**
     * Build Field definition for Field M, Number of school days missed (DM).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIM_DM() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_M_DM,
                ConductAction.COL_ACTION_PENALTY_TIME,
                null, false, 1, 3, REGEX_NUMERIC,
                null, new RetrievePenaltyTime(), null, null);
        return field;
    }

    /**
     * Build Field definition for Field N, Alternate Education Indicator (AEINC).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIN_AEINC() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_N_AEINC,
                null,
                null, false, 1, 1, null,
                null, new ParameterRetriever(), null, null);
        return field;
    }

    /**
     * Build Field definition for Field O, Alternate Education Type (AE).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIO_AE() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_O_AE,
                translateAliasToJavaName(ALIAS_SSDR_31_ALTERNATIVE_EDUCATION_TYPE, true),
                null, true, 0, 1, "[12345]",
                null, null,
                null, null);
        return field;
    }

    /**
     * Build Field definition for Field P, No Alternate Education Reason (NAE).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIP_NAE() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_P_NAE,
                translateAliasToJavaName(ALIAS_SSDR_32_NO_ALTERNATIVE_EDUCATION, true),
                null, true, 0, 1, "[1234]",
                null, new RetrieveNoAlternativeEducation(),
                null, null);
        return field;
    }

    /**
     * Build Field definition for Field Q, Extra Suspension Days (ERDX).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIQ_ERDX() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_Q_ERDX,
                String.valueOf(LABEL_PREFIX_CHAR),
                null, false, 0, 0, null,
                null, null,
                null, null);
        return field;
    }

    /**
     * Build Field definition for Field R, Appeal Code (APPEAL).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIR_APPEAL() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_R_APPEAL,
                translateAliasToJavaName(ALIAS_SSDR_APPEAL, true),
                null, false, 0, 50, null,
                null, new RetrieveAppealCode(),
                null, null);
        return field;
    }

    /**
     * Build Field definition for Field S, Other Specification (AEX).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIS_AEX() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_S_AEX,
                translateAliasToJavaName(ALIAS_SSDR_AEX, true),
                null, false, 0, 50, null,
                null, new RetrieveOtherSpecification(),
                null, null);
        return field;
    }

    /**
     * Build Field definition for Field T, Type Of Office 1 (SOT1).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIT_SOT1() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_T_SOT1,
                null,
                null, false, 0, 50, null,
                null, new RetrieveOffenseCode(), null, null);

        return field;
    }

    /**
     * Build Field definition for Field U, Type Of Office 2 (SOT2).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIU_SOT2() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_U_SOT2,
                null,
                null, false, 0, 50, null,
                null, new RetrieveOffenseCode(), null, null);

        return field;
    }

    /**
     * Build Field definition for Field V, Type Of Office 3 (SOT3).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIV_SOT3() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_V_SOT3,
                null,
                null, false, 0, 50, null,
                null, new RetrieveOffenseCode(), null, null);

        return field;
    }

    /**
     * Build Field definition for Field W, Type Of Office 4 (SOT4).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIW_SOT4() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_W_SOT4,
                null,
                null, false, 0, 50, null,
                null, new RetrieveOffenseCode(), null, null);

        return field;
    }

    /**
     * Build Field definition for Field X, Type Of Office 5 (SOT5).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getIX_SOT5() {
        FieldDefinition field = new FieldDefinition(FIELD_ID_SSDR_X_SOT5,
                null,
                null, false, 0, 50, null,
                null, new RetrieveOffenseCode(), null, null);

        return field;
    }

    /**
     * Returns the first session day on or after the current date in the calendar. Session days are
     * calculated using the school OID and calendar code. If such a session day could not be found
     * then a validation error is logged and the original date is returned.
     *
     * @param entity StateReportEntity
     * @param calendarsBySchool Map
     * @param schoolOid String
     * @param calendarCode String
     * @param calendar Calendar
     * @param sasid String
     * @param dateFormat DateFormat
     * @return java.util.Date
     */
    protected java.util.Date getSessionDay(StateReportEntity entity,
                                           Map calendarsBySchool,
                                           String schoolOid,
                                           String calendarCode,
                                           Calendar calendar,
                                           String sasid,
                                           DateFormat dateFormat) {
        java.util.Date originalDate = calendar.getTime();

        Set sessionDays = null;
        Map calendarLookup = (Map) calendarsBySchool.get(schoolOid);
        if (calendarLookup != null) {
            sessionDays = (Set) calendarLookup.get(calendarCode);
        }

        int i = 0;

        if (sessionDays != null) {
            while (!sessionDays.contains(new java.sql.Date(calendar.getTimeInMillis())) && i < 20) {
                calendar.add(Calendar.DATE, 1);
                i++;
            }

            if (i == 20) {
                FieldDefinition field = getFieldDefinition(FIELD_ID_SSDR_K_RD);
                StateReportValidationError error = new StateReportValidationError(entity, field,
                        "WARNING: Could not find a session day. Using September 1st of next school year.",
                        "Date on or after " + dateFormat.format(originalDate));
                entity.addRetrievalError(FIELD_ID_SSDR_K_RD, error);

                calendar.setTime(m_endDate);
                calendar.set(Calendar.MONTH, Calendar.SEPTEMBER);
                calendar.set(Calendar.DAY_OF_MONTH, 1);
            }
        } else {
            FieldDefinition field = getFieldDefinition(FIELD_ID_SSDR_K_RD);
            StateReportValidationError error = new StateReportValidationError(entity, field,
                    "WARNING: Could not find calendar for student.", "Student (SASID: " + sasid + ").");
            entity.addRetrievalError(FIELD_ID_SSDR_K_RD, error);

            calendar.setTime(originalDate);
        }

        return calendar.getTime();
    }

    /**
     * Returns a Query object for the incidents that should be reported
     * in the export.
     *
     * This uses the input parameters to generate the query.
     *
     * @return Criteria
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

        subCriteria.addGreaterOrEqualThan(ConductIncident.REL_CONDUCT_ACTIONS + ModelProperty.PATH_DELIMITER
                + ConductAction.COL_ACTION_START_DATE, m_startDate);
        subCriteria.addLessOrEqualThan(ConductIncident.REL_CONDUCT_ACTIONS + ModelProperty.PATH_DELIMITER
                + ConductAction.COL_ACTION_START_DATE, m_endDate);

        if (m_requireReportStatus) {
            subCriteria.addEqualTo(ConductIncident.REL_STUDENT + ModelProperty.PATH_DELIMITER
                    + translateAliasToJavaName(ALIAS_DOE_STATUS_REPORT, true), "Report");
        }

        subCriteria.addIn(
                ConductIncident.REL_CONDUCT_ACTIONS + ModelProperty.PATH_DELIMITER + ConductAction.COL_ACTION_CODE,
                m_actionCodes.keySet());
        SubQuery query = new SubQuery(ConductIncident.class, X2BaseBean.COL_OID, subCriteria, true);

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

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_DOE_34_SPED_PLACEMENT);

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
