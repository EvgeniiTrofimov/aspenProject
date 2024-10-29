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
package com.x2dev.procedures.statereporting.ct;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.framework.persistence.X2Query;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.ConductOffense;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * This class implements the data export for Connecticut ED166 Discipline export.
 *
 * @author X2 Development Corporation
 */
public class Ed166 extends StateReportData {
    /**
     * Entity class for Connecticut ED166 Discipline export.
     *
     * @author X2 Development Corporation
     */
    public static class Ed166Entity extends StateReportEntity {

        protected ConductAction m_primaryAction;
        protected ConductOffense m_primaryOffense;
        protected String m_primarySanctionCode;
        protected ConductAction m_secondaryAction;
        protected ConductOffense m_secondaryOffense;
        protected String m_secondarySanctionCode;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public Ed166Entity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
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
         * Initialize the entity for the Conduct Incident bean provided.
         * The primary offense and secondary offense are found and initialized,
         * so that they may be used in the retriever classes. The actions and sanction codes
         * are set.
         *
         * Note this export only works with incidents which have associated offenses.
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
            Ed166 edData = (Ed166) data;
            ConductIncident incident = (ConductIncident) getBean();
            Collection<ConductOffense> offenses = edData.m_offenses.get(incident.getOid());
            if (offenses == null || offenses.size() == 0) {
                // Ignore records with not offenses.
                setRowCount(0);
            } else {
                initializeOffenses(offenses);
            }
            Collection<ConductAction> actions = edData.m_actions.get(incident.getOid());
            initializeActionsAndSanctions(actions, edData);
            boolean has166Code = false;
            String incidentCode = incident.getIncidentCode();
            if (edData.m_limitOffenses) {
                if (incident.getFieldValueByBeanPath(edData.m_fieldBullying) != null) {
                    String bullying = (String) incident.getFieldValueByBeanPath(edData.m_fieldBullying);
                    String bullyingState =
                            edData.lookupStateValue(ConductIncident.class, edData.m_fieldBullying, bullying);
                    if (!StringUtils.isEmpty(bullyingState) && (CODE_BULLYING_YES_NON_PROTECTED.equals(bullyingState)
                            || CODE_BULLYING_YES_PROTECTED.equals(bullyingState))) {
                        has166Code = true;
                    }
                }
                if (edData.m_incidentCodes166.containsKey(incidentCode)) {
                    has166Code = true;
                }
                if (!has166Code && offenses != null) {
                    for (ConductOffense offense : offenses) {
                        String conductOffense = offense.getIncidentCode();
                        if (edData.m_offenseCodes166.containsKey(conductOffense)) {
                            has166Code = true;
                            break;
                        }
                    }
                }
                if (!has166Code && actions != null) {
                    for (ConductAction action : actions) {
                        String conductAction = action.getActionCode();
                        if (edData.m_actionCodes166.containsKey(conductAction)) {
                            has166Code = true;
                            break;
                        }
                    }
                }
                if (!has166Code) {
                    setRowCount(0);
                }
            }
        }

        /**
         * Set the primary and secondary offenses.
         *
         * @param offenses Collection<ConductOffense>
         */
        private void initializeOffenses(Collection<ConductOffense> offenses) {
            if (offenses.size() == 1) {
                for (ConductOffense offense : offenses) {
                    m_primaryOffense = offense;
                }
            }

            // consider case that one offense, of 1+ offenses, is marked as primary,
            // or that multiple offenses exist, none of which is marked as primary
            else {
                for (ConductOffense offense : offenses) {
                    // if primary offense is null use current offense as primary,
                    // otherwise if current offense is primary, use this offense as primary
                    // only after checking whether secondary offense needs to be populated.
                    Ed166 data = (Ed166) getData();
                    if (!StringUtils.isEmpty(offense.getIncidentCode()) &&
                            data.m_incidentTypeCodes.containsKey(offense.getIncidentCode())) {
                        if (m_primaryOffense == null || offense.getPrimary()) {
                            m_primaryOffense = offense;
                        } else if (m_secondaryOffense == null) {
                            m_secondaryOffense = offense;
                        }
                    }
                }
            }
        }

        /**
         * Set primary and secondary action and action code.
         *
         * @param actions Collection<ConductAction>
         * @param edData Ed166
         */
        private void initializeActionsAndSanctions(Collection<ConductAction> actions, Ed166 edData) {
            if (actions != null) {
                Map<String, ReferenceCode> refCodes = edData.m_actionCodes;
                if (refCodes != null) {
                    for (ConductAction action : actions) {
                        String code = action.getActionCode();
                        if (!StringUtils.isEmpty(code)) {
                            ReferenceCode refCode = refCodes.get(code);
                            if (refCode != null && !StringUtils.isEmpty(refCode.getStateCode())) {
                                if (m_primarySanctionCode == null ||
                                        m_primarySanctionCode.compareTo(refCode.getStateCode()) > 0) {
                                    if (m_secondarySanctionCode == null ||
                                            m_secondarySanctionCode.compareTo(m_primarySanctionCode) > 0)
                                    // check that secondary sanction code does not need to be set to
                                    // what will become the previous value of the primary action and
                                    // primary sanction code.
                                    {
                                        m_secondarySanctionCode = m_primarySanctionCode;
                                        m_secondaryAction = m_primaryAction;
                                    }
                                    // set primary action code and primary sanction code.
                                    m_primaryAction = action;
                                    m_primarySanctionCode = refCode.getStateCode();
                                } else if (m_secondarySanctionCode == null ||
                                        m_secondarySanctionCode.compareTo(refCode.getStateCode()) > 0) {
                                    m_secondaryAction = action;
                                    m_secondarySanctionCode = refCode.getStateCode();
                                }
                            }
                        }
                    }
                }
            }
        }
    }


    /**
     * Aliases on the action record for days counts.
     */
    protected static final String ALIAS_ARRESTED = "DOE ARRESTED";
    protected static final String ALIAS_BULLYING = "DOE BULLYING";
    protected static final String ALIAS_DAYS_CARRYOVER = "DOE DAYS CARRYOVER";
    protected static final String ALIAS_DAYS_SANCTION = "DOE DAYS SANCTION";
    protected static final String ALIAS_DAYS_SERVED = "DOE DAYS SERVED";
    protected static final String ALIAS_ED_PROVIDED = "DOE ED PROVIDE ACT";
    protected static final String ALIAS_SKL_EXCLUDE_FROM_SR = "all-skl-ExcludefromStateReporting";
    protected static final String ALIAS_SKL_SPED = "Special Education School";
    protected static final String ALIAS_SKL_STATE_ID = "StateId";
    protected static final String ALIAS_STD_OUTSIDE_PL_FACILITY = "Outside Placement Facility";
    protected static final String ALIAS_STD_SPED_FACILITY = "Special Education Facility";
    protected static final String ALIAS_WEAPON_INVOLVEMENT = "DOE WEAPON";

    /**
     * Codes on the Bullying field used in ValidateBullyingField
     */
    protected static final String CODE_BULLYING_YES_PROTECTED = "02";
    protected static final String CODE_BULLYING_YES_NON_PROTECTED = "03";
    protected static final String CODE_WEAPON_NONE = "0000";

    /**
     * Constants for reporting information.
     */
    private static final String PARAM_INCIDENT_NUMBER = "incidentNumber";
    private static final String PARAM_INCLUDE_HEADING = "includeHeading";
    private static final String PARAM_LIMIT_OFFENSES = "limitOffenses";
    private static final String PARAM_QUERY_BY = "queryBy";
    private static final String PARAM_QUERY_STRING = "queryString";
    private static final String PARAM_REPORT_END_DATE = "reportEndDate";
    private static final String PARAM_REPORT_START_DATE = "reportStartDate";
    private static final String PARAM_SORT = "sort";
    private static final String PARAM_YES = "Y";

    /**
     * constants for field parameters for the ED166-ACTION retriever.
     */
    protected static final String PARAM_ACTION_ARRESTED = "ARRESTED";
    protected static final String PARAM_ACTION_CODE = "CODE";
    protected static final String PARAM_ACTION_DAYS_SANCTION = "DAYS_SANCTION";
    protected static final String PARAM_ACTION_DAYS_SERVED = "DAYS_SERVED";
    protected static final String PARAM_ACTION_DAYS_CARRYOVER = "DAYS_CARRYOVER";
    protected static final String PARAM_ACTION_ED_PROVIDE = "ED_PROVIDE";
    protected static final String PARAM_CODE = "CODE";
    protected static final String PARAM_PRIMARY_INCIDENT = "PRIMARY";
    protected static final String PARAM_SECONDARY_INCIDENT = "SECONDARY";

    /**
     * Field values needed for validations
     */
    protected static final String FIELD_BULLYING = "Bullying";
    protected static final String FIELD_INCIDENT_ID = "Incident ID";
    protected static final String FIELD_INCIDENT_TYPE_1 = "Incident Type 1";
    protected static final String FIELD_INCIDENT_TYPE_2 = "Incident Type 2";
    protected static final String FIELD_SANCTION_TYPE_1 = "Sanction 1";
    protected static final String FIELD_SANCTION_TYPE_2 = "Sanction 2";
    protected static final String FIELD_VICTIM_STUDENT = "Victim Student";
    protected static final String FIELD_VICTIM_CERT_STAFF = "Victim Cert Staff";
    protected static final String FIELD_VICTIM_SUBSTITUTE = "Victim Substitute";
    protected static final String FIELD_VICTIM_OTHER_STAFF = "Victim Other Staff";
    protected static final String FIELD_VICTIM_NON_SCHOOL = "Victim Non-School";

    /**
     * constants for deleting reportable incidents and actions.
     */
    private static final String FLAG_FEDERAL_ED166 = "ED166";

    /**
     * Local variables for reporting information.
     */
    protected Map<String, Collection<ConductAction>> m_actions;
    protected Map<String, ReferenceCode> m_actionCodes;
    protected Map<String, ReferenceCode> m_actionCodes166;
    protected CTStudentHelper m_ctHelper;
    protected Map<String, ReferenceCode> m_incidentCodes166;
    protected Map<String, ReferenceCode> m_offenseCodes166;
    protected String m_fieldBullying;
    protected String m_fieldEducationProvided;
    protected String m_fieldSklExcludeFromStateReporting;
    protected String m_fieldSklSped;
    protected String m_fieldSklStateId;
    protected String m_fieldStdOutFacility;
    protected String m_fieldStdSpedFacility;
    protected String m_fieldWeaponInvolvement;
    protected boolean m_incidentNumber = true;
    protected Map<String, ReferenceCode> m_incidentTypeCodes;
    protected boolean m_limitOffenses = false;
    protected Map<String, Collection<ConductOffense>> m_offenses;
    protected PlainDate m_reportEndDate;
    protected PlainDate m_reportStartDate;

    /**
     * Retrieve the action code state value for the action.
     * Or default to continuation for no offense.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveAction implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            Ed166Entity ed166Entity = (Ed166Entity) entity;
            ConductAction action = null;
            String sanctionCode = null;
            Object value = null;
            // The first part of the parameter couple details whether the incident is
            // primary or secondary. The second part indicates which action type to return.
            String[] parameters = ((String) field.getParameter()).split(",");
            String incidentPriority = parameters[0];
            String actionParameter = parameters[1];
            if (PARAM_PRIMARY_INCIDENT.equals(incidentPriority)) {
                action = ed166Entity.m_primaryAction;
                sanctionCode = ed166Entity.m_primarySanctionCode;
            } else if (PARAM_SECONDARY_INCIDENT.equals(incidentPriority)) {
                action = ed166Entity.m_secondaryAction;
                sanctionCode = ed166Entity.m_secondarySanctionCode;
            }
            if (PARAM_ACTION_CODE.equals(actionParameter)) {
                value = sanctionCode;
            } else if (PARAM_ACTION_DAYS_SANCTION.equals(actionParameter) && action != null) {
                float days = 0;
                String daysStr = action.getFieldValueByAlias(ALIAS_DAYS_SANCTION) != null
                        ? action.getFieldValueByAlias(ALIAS_DAYS_SANCTION).toString()
                        : null;
                if (!StringUtils.isEmpty(daysStr)) {
                    try {
                        days = Float.parseFloat(daysStr);
                    } catch (NumberFormatException nfe) {
                        // Do nothing. Ignore this record.
                    }
                }
                value = Float.valueOf(days);
            } else if (PARAM_ACTION_DAYS_SERVED.equals(actionParameter) && action != null) {
                float days = 0;
                String daysStr = (String) action.getFieldValueByAlias(ALIAS_DAYS_SERVED);
                if (!StringUtils.isEmpty(daysStr)) {
                    try {
                        days = Float.parseFloat(daysStr);
                    } catch (NumberFormatException nfe) {
                        // Do nothing. Ignore this record.
                    }
                }
                value = Float.valueOf(days);
            } else if (PARAM_ACTION_DAYS_CARRYOVER.equals(actionParameter) && action != null) {
                float days = 0;
                String daysStr = (String) action.getFieldValueByAlias(ALIAS_DAYS_CARRYOVER);
                if (!StringUtils.isEmpty(daysStr)) {
                    try {
                        days = Float.parseFloat(daysStr);
                    } catch (NumberFormatException nfe) {
                        // Do nothing. Ignore this record.
                    }
                }
                value = Float.valueOf(days);
            } else if (PARAM_ACTION_ED_PROVIDE.equals(actionParameter) && action != null) {
                String typeEducationProvided = (String) action.getFieldValueByAlias(ALIAS_ED_PROVIDED);
                if (!StringUtils.isEmpty(typeEducationProvided) &&
                        !StringUtils.isEmpty(m_fieldEducationProvided)) {
                    typeEducationProvided =
                            lookupStateValue(ConductAction.class, m_fieldEducationProvided, typeEducationProvided);
                }
                value = typeEducationProvided;
            } else if (PARAM_ACTION_ARRESTED.equals(actionParameter) && action != null) {
                value = Boolean.FALSE;
                String actionArrested = (String) action.getFieldValueByAlias(ALIAS_ARRESTED);

                if (actionArrested != null && (actionArrested.startsWith("Y") || actionArrested.equals("1"))) {
                    value = Boolean.TRUE;
                }
            }
            return value;
        }
    }

    /**
     * Retrieve the incident code for the requested incident.
     * The incident will be either the primary or secondary incident indicated by the parameter.
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
                                    FieldDefinition field)
                throws X2BaseException {
            String incidentCode = null;
            String incidentPriority = (String) field.getParameter();
            Ed166Entity ed166Entity = (Ed166Entity) entity;
            if (PARAM_PRIMARY_INCIDENT.equals(incidentPriority) && ed166Entity.m_primaryOffense != null) {
                incidentCode = ed166Entity.m_primaryOffense.getIncidentCode();
            } else if (PARAM_SECONDARY_INCIDENT.equals(incidentPriority) && ed166Entity.m_secondaryOffense != null) {
                incidentCode = ed166Entity.m_secondaryOffense.getIncidentCode();
            }
            // get state value of incidentCode, if one exists
            if (!StringUtils.isEmpty(incidentCode)) {
                Ed166 edData = (Ed166) data;
                Map<String, ReferenceCode> refCodes = edData.m_incidentTypeCodes;
                if (refCodes != null && refCodes.containsKey(incidentCode)) {
                    ReferenceCode refCode = refCodes.get(incidentCode);
                    incidentCode = refCode.getStateCode();
                }
            }
            if (PARAM_CODE.equals(incidentPriority) && m_incidentNumber) {
                incidentCode = ((ConductIncident) entity.getBean()).getIncidentId();
            } else if (PARAM_CODE.equals(incidentPriority) && !m_incidentNumber) {
                incidentCode = "";
            }
            return incidentCode;
        }
    }

    /**
     * Retrieves school.
     */
    protected class RetrieveSchoolId implements FieldRetriever {
        private static final String CACL_ID = "ED166-SKL-ID";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            ConductIncident cnd = (ConductIncident) entity.getBean();
            String schoolIdToReturn = null;
            String sklSped = (String) cnd.getSchool().getFieldValueByBeanPath(m_fieldSklSped);
            if (BooleanAsStringConverter.TRUE.equalsIgnoreCase(sklSped)) {
                String schoolId = (String) cnd.getStudent().getFieldValueByBeanPath(m_fieldStdSpedFacility);
                if (!StringUtils.isEmpty(schoolId)) {
                    schoolIdToReturn = lookupStateValue(SisStudent.class, m_fieldStdSpedFacility, schoolId);
                }
            }
            if (StringUtils.isEmpty(schoolIdToReturn)) {
                String stdOutFacility = (String) cnd.getStudent().getFieldValueByBeanPath(m_fieldStdOutFacility);
                if (!StringUtils.isEmpty(stdOutFacility)) {
                    schoolIdToReturn = lookupStateValue(SisStudent.class, m_fieldStdOutFacility, stdOutFacility);
                }
            }
            if (StringUtils.isEmpty(schoolIdToReturn)) {
                StudentEnrollment recentEnr =
                        m_ctHelper.getEnrollmentForDate(cnd.getStudentOid(), cnd.getIncidentDate(),
                                StudentEnrollment.ENTRY + StudentEnrollment.YOG_CHANGE
                                        + StudentEnrollment.STATUS_CHANGE);
                if (recentEnr != null) {
                    String enrSklOverride =
                            (String) recentEnr.getFieldValueByBeanPath(m_ctHelper.m_fieldEnrSklOverride);
                    if (!StringUtils.isEmpty(enrSklOverride)) {
                        schoolIdToReturn =
                                lookupStateValue(StudentEnrollment.class, m_ctHelper.m_fieldEnrSklOverride,
                                        enrSklOverride);
                    }
                }
            }
            return !StringUtils.isEmpty(schoolIdToReturn) ? schoolIdToReturn
                    : cnd.getSchool().getFieldValueByBeanPath(m_fieldSklStateId);
        }
    }

    /**
     * Validate that if there is an affirmative code for bullying that
     * there is at least one victim.
     */
    protected class ValidateBullyingField implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String bullying = entity.getFieldValue(FIELD_BULLYING);
            String victimStudent = entity.getFieldValue(FIELD_VICTIM_STUDENT);
            String victimCertStaff = entity.getFieldValue(FIELD_VICTIM_CERT_STAFF);
            String victimSubstitute = entity.getFieldValue(FIELD_VICTIM_SUBSTITUTE);
            String victimOtherStaff = entity.getFieldValue(FIELD_VICTIM_OTHER_STAFF);
            String victimNonSchool = entity.getFieldValue(FIELD_VICTIM_NON_SCHOOL);
            if (StringUtils.isEqual(bullying, CODE_BULLYING_YES_NON_PROTECTED) ||
                    StringUtils.isEqual(bullying, CODE_BULLYING_YES_PROTECTED)) {
                if (!PARAM_YES.equals(victimStudent) &&
                        !PARAM_YES.equals(victimCertStaff) &&
                        !PARAM_YES.equals(victimSubstitute) &&
                        !PARAM_YES.equals(victimOtherStaff) &&
                        !PARAM_YES.equals(victimNonSchool)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "There must be at least one victim when " +
                                    FIELD_BULLYING + " = " + CODE_BULLYING_YES_NON_PROTECTED + ", or " +
                                    FIELD_BULLYING + " = " + CODE_BULLYING_YES_PROTECTED + ".",
                            FIELD_BULLYING + " = " + STYLE_BOLD + bullying + STYLE_END +
                                    ", but there are no victims."));
                }
            }
            return errors;
        }
    }

    /**
     * Validate that incident type 1 does not equal incident type 2.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateIncidentTypes implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String incidentType1 = entity.getFieldValue(FIELD_INCIDENT_TYPE_1);
            String incidentType2 = entity.getFieldValue(FIELD_INCIDENT_TYPE_2);
            if (StringUtils.isEqual(incidentType1, incidentType2)) {
                errors.add(new StateReportValidationError(entity, field,
                        FIELD_INCIDENT_TYPE_1 + " cannot equal " + FIELD_INCIDENT_TYPE_2 + ".",
                        FIELD_INCIDENT_TYPE_1 + " = " + STYLE_BOLD + incidentType1 + STYLE_END + ", " +
                                FIELD_INCIDENT_TYPE_2 + " = " + STYLE_BOLD + incidentType2 + STYLE_END));
            }
            return errors;
        }
    }

    /**
     * Validate that sanction 1 does not equal sanction 2.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateSanctionTypes implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String sanctionType1 = entity.getFieldValue(FIELD_SANCTION_TYPE_1);
            String sanctionType2 = entity.getFieldValue(FIELD_SANCTION_TYPE_2);
            if (StringUtils.isEqual(sanctionType1, sanctionType2)) {
                errors.add(new StateReportValidationError(entity, field,
                        FIELD_SANCTION_TYPE_1 + " cannot equal " + FIELD_SANCTION_TYPE_2 + ".",
                        FIELD_SANCTION_TYPE_1 + " = " + STYLE_BOLD + sanctionType1 + STYLE_END + ", " +
                                FIELD_SANCTION_TYPE_2 + " = " + STYLE_BOLD + sanctionType2 + STYLE_END));
            }
            return errors;
        }
    }

    /**
     * Gets the heading.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        String heading = "";
        Boolean includeHeading = (Boolean) getParameter(PARAM_INCLUDE_HEADING);
        if (includeHeading != null && includeHeading.booleanValue()) {
            heading = super.getHeading();
        }
        return heading;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        initializeFields();
        Criteria incidentCriteria = getIncidentCriteria();
        QueryByCriteria incidentQuery = new QueryByCriteria(ConductIncident.class, incidentCriteria, true);
        Integer sort = (Integer) getParameter(PARAM_SORT);
        switch (sort == null ? 0 : sort.intValue()) {
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
                        + PATH_DELIMITER + SisSchool.COL_NAME);
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
        setEntityClass(Ed166Entity.class);
        // Load reference codes and action/offense maps.
        loadActions(incidentCriteria);
        loadOffenses(incidentCriteria);
        // Load reference codes for incident types.
        loadIncidentTypes();
        // Add any retrievers or validators.
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put("ED166-ACTION", new RetrieveAction());
        calcs.put("ED166-INCIDENT-CODE", new RetrieveIncidentCode());
        calcs.put(RetrieveSchoolId.CACL_ID, new RetrieveSchoolId());
        super.addCalcs(calcs);
        HashMap validators = new HashMap<String, FieldValidator>();
        validators.put("ED166-BULLYING-VAL", new ValidateBullyingField());
        validators.put("ED166-INCIDENT-VAL", new ValidateIncidentTypes());
        validators.put("ED166-SANCTION-VAL", new ValidateSanctionTypes());
        super.addValidators(validators);
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
            incidentCriteria.addNotEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            incidentCriteria.addNotEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        incidentCriteria.addNotEqualTo(
                ConductIncident.REL_SCHOOL + PATH_DELIMITER + m_fieldSklExcludeFromStateReporting, Boolean.TRUE);
        incidentCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_reportStartDate);
        incidentCriteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_reportEndDate);
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

        // If the user selects to limit the offenses, identify reportable offenses and actions.
        if (m_limitOffenses) {
            X2Criteria reportableIncidentsCriteria = new X2Criteria();
            // 1. Limit selection to incidents with Bullying of types "Bullying yes non protected"
            // and "Bullying yes protected"
            if (!StringUtils.isEmpty(m_fieldBullying)) {
                X2Criteria bullyingCriteria = new X2Criteria();
                String[] stateCodeArray = {CODE_BULLYING_YES_NON_PROTECTED, CODE_BULLYING_YES_PROTECTED};
                Collection<String> stateCodes = Arrays.asList(stateCodeArray);
                Collection<String> reportableCodes = getReportableCodes(m_fieldBullying, stateCodes);
                if (reportableCodes != null && reportableCodes.size() > 0) {
                    bullyingCriteria.addIn(m_fieldBullying, reportableCodes);
                    reportableIncidentsCriteria.addOrCriteria(bullyingCriteria);
                }
            }
            // 2. Limit selection to incidents with Weapon Involvement types other than "None"
            if (!StringUtils.isEmpty(m_fieldWeaponInvolvement)) {
                X2Criteria weaponInvolvementCriteria = new X2Criteria();
                String[] stateCodeArray = {CODE_WEAPON_NONE};
                Collection<String> stateCodes = Arrays.asList(stateCodeArray);
                Collection<String> reportableCodes = getReportableCodes(m_fieldWeaponInvolvement, stateCodes);
                if (reportableCodes != null && reportableCodes.size() > 0) {
                    weaponInvolvementCriteria.addNotIn(m_fieldWeaponInvolvement, reportableCodes);
                    reportableIncidentsCriteria.addOrCriteria(weaponInvolvementCriteria);
                }
            }
            // 3 Limit to reportable offenses and actions
            String offenseRefTblOid = null;
            String actionRefTblOid = null;
            // 3a. Find the reference code tables for incidents and actions.
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(ConductIncident.class.getName(),
                    ConductIncident.COL_INCIDENT_CODE);
            if (field != null) {
                offenseRefTblOid = field.getReferenceTableOid();
            }
            field = dictionary.findDataDictionaryField(ConductAction.class.getName(), ConductAction.COL_ACTION_CODE);
            if (field != null) {
                actionRefTblOid = field.getReferenceTableOid();
            }
            // 3b. Load lists of reportable codes (codes that contain "ED166" in the Federal
            // column).
            List<String> incidentCodes = new ArrayList<String>();
            List<String> actionCodes = new ArrayList<String>();
            if (!StringUtils.isEmpty(offenseRefTblOid)) {
                Criteria criteria = new Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, offenseRefTblOid);
                criteria.addEqualTo(ReferenceCode.COL_FEDERAL_CODE, FLAG_FEDERAL_ED166);
                ReportQueryByCriteria query =
                        new ReportQueryByCriteria(ReferenceCode.class, new String[] {ReferenceCode.COL_CODE}, criteria);
                ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        Object[] code = (Object[]) iterator.next();
                        incidentCodes.add((String) code[0]);
                    }
                } finally {
                    iterator.close();
                }
            }
            if (!StringUtils.isEmpty(actionRefTblOid)) {
                Criteria criteria = new Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, actionRefTblOid);
                criteria.addEqualTo(ReferenceCode.COL_FEDERAL_CODE, FLAG_FEDERAL_ED166);
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
            // 3c. Add criteria for reportable incident codes or reportable action codes.
            if (incidentCodes.size() > 0) {
                Criteria codeCriteria = new Criteria();
                codeCriteria.addIn(ConductIncident.REL_CONDUCT_OFFENSES + PATH_DELIMITER +
                        ConductOffense.COL_INCIDENT_CODE, incidentCodes);
                reportableIncidentsCriteria.addOrCriteria(codeCriteria);
            }
            if (actionCodes.size() > 0) {
                Criteria codeCriteria = new Criteria();
                codeCriteria.addIn(ConductIncident.REL_CONDUCT_ACTIONS + PATH_DELIMITER +
                        ConductAction.COL_ACTION_CODE, actionCodes);
                reportableIncidentsCriteria.addOrCriteria(codeCriteria);
            }
            // 4 add criteria for reportable incidents
            incidentCriteria.addAndCriteria(reportableIncidentsCriteria);
        }
        return incidentCriteria;
    }

    /**
     * Look up a map of reference codes for a field, based on the alias of the field.
     *
     * @param className String
     * @param propertyName String
     * @return Map<String, ReferenceCode>
     */
    private Map<String, ReferenceCode> getReferenceCodes166ForField(String className, String propertyName) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField dictionaryField =
                dictionary.findDataDictionaryField(className, propertyName);
        Map<String, ReferenceCode> refCodesMap = new HashMap<String, ReferenceCode>();
        Map<String, ReferenceCode> refCodesFiltered = new HashMap<String, ReferenceCode>();
        if (dictionaryField != null) {
            ReferenceTable refTable = dictionaryField.getReferenceTable();
            if (refTable == null) {
                addSetupError("missing reference table",
                        "reference table not found for the " + className + " className " +
                                propertyName + " propertyName and ");
            } else {
                refCodesMap = refTable.getCodeMap(getBroker());
            }
        }
        Collection<ReferenceCode> refCodes = refCodesMap.values();
        for (ReferenceCode refCode : refCodes) {
            String fedCode = refCode.getFederalCode();
            if (FLAG_FEDERAL_ED166.equals(fedCode)) {
                refCodesFiltered.put(refCode.getCode(), refCode);
            }
        }
        return refCodesFiltered;
    }

    /**
     * Returns reportable codes for a given field matching state codes provided in the collection.
     *
     * @param fieldName String
     * @param stateCodes Collection<String>
     * @return Collection<String>
     */
    private Collection<String> getReportableCodes(String fieldName, Collection<String> stateCodes) {
        Collection<String> reportableCodes = null;
        X2Criteria criteria = null;
        DataDictionaryField field = getDataDictionaryField(ConductIncident.class, fieldName);
        if (field != null) {
            String referenceTableOid = field.getReferenceTableOid();
            if (!StringUtils.isEmpty(referenceTableOid)) {
                criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
                criteria.addIn(ReferenceCode.COL_STATE_CODE, stateCodes);
                SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
                reportableCodes = getBroker().getSubQueryCollectionByQuery(query);
            }
        }
        return reportableCodes;
    }

    /**
     * Initialize member fields.
     *
     * @throws X2BaseException
     */
    private void initializeFields() throws X2BaseException {
        Boolean objLimitOffenses = (Boolean) getParameter(PARAM_LIMIT_OFFENSES);
        m_limitOffenses = (objLimitOffenses != null && objLimitOffenses.booleanValue());
        Boolean incidentNumber = (Boolean) getParameter(PARAM_INCIDENT_NUMBER);
        m_incidentNumber = (incidentNumber != null && incidentNumber.booleanValue());
        m_reportStartDate = (PlainDate) getParameter(PARAM_REPORT_START_DATE);
        if (m_reportStartDate == null) {
            m_reportStartDate = getOrganization().getCurrentContext().getStartDate();
        }
        m_reportEndDate = (PlainDate) getParameter(PARAM_REPORT_END_DATE);
        if (m_reportEndDate == null) {
            m_reportStartDate = getOrganization().getCurrentContext().getEndDate();
        }
        m_actionCodes166 = getReferenceCodes166ForField("com.x2dev.sis.model.beans.ConductAction", "actionCode");
        m_incidentCodes166 = getReferenceCodes166ForField("com.x2dev.sis.model.beans.ConductIncident", "incidentCode");
        m_offenseCodes166 = getReferenceCodes166ForField("com.x2dev.sis.model.beans.ConductOffense", "incidentCode");
        m_fieldEducationProvided = translateAliasToJavaName(ALIAS_ED_PROVIDED, false);
        m_fieldBullying = translateAliasToJavaName(ALIAS_BULLYING, false);
        m_fieldWeaponInvolvement = translateAliasToJavaName(ALIAS_WEAPON_INVOLVEMENT, false);
        m_fieldSklSped = translateAliasToJavaName(ALIAS_SKL_SPED, true);
        m_fieldStdSpedFacility = translateAliasToJavaName(ALIAS_STD_SPED_FACILITY, true);
        m_fieldSklStateId = translateAliasToJavaName(ALIAS_SKL_STATE_ID, true);
        m_fieldStdOutFacility = translateAliasToJavaName(ALIAS_STD_OUTSIDE_PL_FACILITY, true);
        m_fieldSklExcludeFromStateReporting = translateAliasToJavaName(ALIAS_SKL_EXCLUDE_FROM_SR, true);
        m_ctHelper = new CTStudentHelper(this, m_reportEndDate);
        m_ctHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_ctHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_ctHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportEndDate);
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

    /**
     * Load incident types.
     */
    private void loadIncidentTypes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field =
                dictionary.findDataDictionaryField(ConductIncident.class.getName(), ConductIncident.COL_INCIDENT_CODE);
        String referenceTableOid = field.getReferenceTableOid();
        X2Criteria criteria = null;
        BeanQuery query = null;
        // Get a map of action codes to ReferenceCode.
        if (referenceTableOid != null) {
            criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
            query = new BeanQuery(ReferenceCode.class, criteria, false);
            m_incidentTypeCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 50);
        }
    }

    /**
     * Load supporting collections of offenses.
     *
     * @param incidentCriteria Criteria
     */
    private void loadOffenses(Criteria incidentCriteria) {
        // Load all offenses for the selected incidents into a map.
        X2Criteria criteria = new X2Criteria();
        SubQuery incidentQuery = new SubQuery(ConductIncident.class, X2BaseBean.COL_OID, incidentCriteria);
        criteria.addIn(ConductOffense.COL_INCIDENT_OID, incidentQuery);
        X2Query query = new BeanQuery(ConductOffense.class, criteria, false);
        m_offenses = getBroker().getGroupedCollectionByQuery(query, ConductOffense.COL_INCIDENT_OID, 100);
    }
}
