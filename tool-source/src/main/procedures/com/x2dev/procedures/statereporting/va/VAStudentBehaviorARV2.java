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
package com.x2dev.procedures.statereporting.va;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
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
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.math.BigDecimal;
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
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * @author Follett Software Company
 * @copyright 2023
 */
public class VAStudentBehaviorARV2 extends StateReportData {

    /**
     * Entity class for VA Student Behavior export.
     *
     * @author Follett Software Company
     * @copyright 2023
     */
    public static class VAStudentBehaviorAREntityV2 extends StateReportEntity {

        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
        }

        /**
         * Return the incident ID and student name as the entity name.
         *
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            // Get the true incident bean instead of the current bean type.
            ConductIncident incident = (ConductIncident) super.getBean();
            return incident.getIncidentId() + " " + incident.getStudent().getNameView();
        }
    }

    /**
     * Retrieve the district or school ID based on either the custom INCIDENT DISTRICT/SCHOOL
     * fields or the standard district/school ID.
     *
     * @author Follett Software Company
     * @copyright 2021
     */
    protected static class RetrieveIncidentEAID implements FieldRetriever {
        protected static final String RETRIEVER_ID = "IncidentEAID";

        private static final String PARAM_DISTRICT = "district";
        private static final String PARAM_SCHOOL = "school";

        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            VAStudentBehaviorARV2 sbarData = (VAStudentBehaviorARV2) data;
            ConductIncident incident = (ConductIncident) entity.getBean();
            String param = (String) field.getParameter();
            String value = null;
            if (PARAM_DISTRICT.equals(param)) {
                value = (String) incident.getFieldValueByAlias(ALIAS_CND_INCIDENT_DISTRICT, data.getDataDictionary());
                if (!StringUtils.isEmpty(value)) {
                    value = data.lookupReferenceCodeByAlias(ALIAS_CND_INCIDENT_DISTRICT, value,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (StringUtils.isEmpty(value)) {
                    value = (String) data.getOrganization().getFieldValueByBeanPath(sbarData.m_fieldDistrictId);
                }
            } else if (PARAM_SCHOOL.equals(param)) {
                value = (String) incident.getFieldValueByAlias(ALIAS_CND_INCIDENT_SCHOOL, data.getDataDictionary());
                if (!StringUtils.isEmpty(value)) {
                    value = data.lookupReferenceCodeByAlias(ALIAS_CND_INCIDENT_SCHOOL, value,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (StringUtils.isEmpty(value)) {
                    School school = null;
                    if (data != null && data.getSchool() != null) {
                        school = data.getSchool();
                    } else {
                        school = incident.getStudent().getSchool();
                    }
                    if (school != null) {
                        value = (String) school.getFieldValueByBeanPath(sbarData.m_fieldSchoolId);
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retrieve the district or school ID based on either the custom ENROLLED DISTRICT/SCHOOL
     * fields or the student district/school ID.
     *
     * @author Follett Software Company
     * @copyright 2021
     */
    protected static class RetrieveEnrollEAID implements FieldRetriever {
        protected static final String RETRIEVER_ID = "EnrollEAID";

        private static final String PARAM_DISTRICT = "district";
        private static final String PARAM_SCHOOL = "school";

        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            VAStudentBehaviorARV2 sbarData = (VAStudentBehaviorARV2) data;
            ConductIncident incident = (ConductIncident) entity.getBean();
            String param = (String) field.getParameter();
            String value = null;
            if (PARAM_DISTRICT.equals(param)) {
                value = (String) incident.getFieldValueByAlias(ALIAS_CND_ENROLL_DISTRICT, data.getDataDictionary());
                if (!StringUtils.isEmpty(value)) {
                    value = data.lookupReferenceCodeByAlias(ALIAS_CND_ENROLL_DISTRICT, value,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (StringUtils.isEmpty(value)) {
                    value = (String) incident.getStudent().getOrganization1()
                            .getFieldValueByBeanPath(sbarData.m_fieldDistrictId);
                }
            } else if (PARAM_SCHOOL.equals(param)) {
                value = (String) incident.getFieldValueByAlias(ALIAS_CND_ENROLL_SCHOOL, data.getDataDictionary());
                if (!StringUtils.isEmpty(value)) {
                    value = data.lookupReferenceCodeByAlias(ALIAS_CND_ENROLL_SCHOOL, value,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (StringUtils.isEmpty(value)) {
                    if (data != null && data.getSchool() != null) {
                        value = (String) data.getSchool().getFieldValueByBeanPath(sbarData.m_fieldSchoolId);
                    } else {
                        value = (String) incident.getStudent().getSchool()
                                .getFieldValueByBeanPath(sbarData.m_fieldSchoolId);
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retrieve incremental elements from the multi-value Administrative Intervention and
     * Instructional Support fields.
     */
    protected static class RetrieveActionElement implements FieldRetriever {
        private static final String RETRIEVER_ID = "action";

        private static final String PARAM_ACT_AGGR_CIRCUMSTANCES = "AggravatingCircumstances";
        private static final String PARAM_ACT_ALT_AGENCY = "PlacementEdAgency";
        private static final String PARAM_ACT_PLACEMENT_SKL = "PlacementSchool";
        private static final String PARAM_CND_CANO_FLAG = "CANO_FLAG";
        private static final String PARAM_CND_NOTE_CONVICTION = "NOTE_CONVICTION";
        private static final String PARAM_CND_VICTIM_INDETERM = "VICTIM_INDETERM";
        private static final String PARAM_INSTRUCTION_1 = "instruction1";
        private static final String PARAM_INSTRUCTION_2 = "instruction2";
        private static final String PARAM_INSTRUCTION_3 = "instruction3";
        private static final String PARAM_INTERVENTION_1 = "intervention1";
        private static final String PARAM_INTERVENTION_2 = "intervention2";
        private static final String PARAM_INTERVENTION_3 = "intervention3";

        private static final String EXPORT_FIELD_INTERV_1 = "Behavioral Interv 1";
        private static final String EXPORT_FIELD_INTERV_2 = "Behavioral Interv 2";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            ConductIncident incident = (ConductIncident) entity.getBean();
            ConductAction primaryAction = incident.getConductActions().stream()
                    .filter(act -> BooleanAsStringConverter.TRUE.equals(act.getFieldValueByAlias("DOE PRIMARY ACTION")))
                    .findAny().orElse(null);
            String value = "";
            String param = (String) field.getParameter();
            VAStudentBehaviorARV2 sbarData = (VAStudentBehaviorARV2) data;
            if (PARAM_CND_CANO_FLAG.equals(param)) {
                value = BooleanAsStringConverter.TRUE.equals(incident.getFieldValueByAlias(ALIAS_CND_CANO_FLAG))
                        ? "Y"
                        : "N";
            } else if (PARAM_CND_NOTE_CONVICTION.equals(param)) {
                value = BooleanAsStringConverter.TRUE.equals(incident.getFieldValueByAlias(ALIAS_CND_NOTE_CONVICTION))
                        ? "Y"
                        : "N";
            } else if (PARAM_CND_VICTIM_INDETERM.equals(param)) {
                value = BooleanAsStringConverter.TRUE.equals(incident.getFieldValueByAlias(ALIAS_CND_VICTIM_INDETERM))
                        ? "Y"
                        : "N";
            }
            if (primaryAction != null) {
                if (PARAM_ACT_AGGR_CIRCUMSTANCES.equals(param)) {
                    value = BooleanAsStringConverter.TRUE.equals(
                            primaryAction.getFieldValueByBeanPath(sbarData.m_fieldActAggravatingCircumstances)) ? "Y"
                                    : "N";
                } else if (PARAM_ACT_ALT_AGENCY.equals(param)) {
                    value = (String) primaryAction.getFieldValueByBeanPath(sbarData.m_fieldActAltPlacementEdAgency);
                } else if (PARAM_ACT_PLACEMENT_SKL.equals(param)) {
                    value = (String) primaryAction.getFieldValueByBeanPath(sbarData.m_fieldActAltPlacementSchool);
                } else {
                    String intervention = (String) primaryAction.getFieldValueByBeanPath(sbarData.m_fieldIntervention);
                    String instructional =
                            (String) primaryAction.getFieldValueByBeanPath(sbarData.m_fieldInstructional);
                    if (!StringUtils.isEmpty(intervention)) {
                        List<String> interventions = StringUtils.convertDelimitedStringToList(intervention, ',', true);
                        if (interventions != null && !interventions.isEmpty()) {
                            int interventionsSize = interventions.size();
                            if (PARAM_INTERVENTION_1.equals(param)) {
                                value = interventionsSize > 0 ? data.lookupReferenceCodeByBeanPath(
                                        ConductAction.class,
                                        sbarData.m_fieldIntervention,
                                        interventions.get(0),
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal()) : "";
                            } else if (PARAM_INTERVENTION_2.equals(param)) {
                                String valueToCheck2 = interventionsSize > 1 ? data.lookupReferenceCodeByBeanPath(
                                        ConductAction.class,
                                        sbarData.m_fieldIntervention,
                                        interventions.get(1),
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal()) : "";
                                String interven1 = entity.getFieldValue(EXPORT_FIELD_INTERV_1);
                                value = !interven1.equals(valueToCheck2) ? valueToCheck2 : "";
                            } else if (PARAM_INTERVENTION_3.equals(param)) {
                                String valueToCheck3 = interventionsSize > 2 ? data.lookupReferenceCodeByBeanPath(
                                        ConductAction.class,
                                        sbarData.m_fieldIntervention,
                                        interventions.get(2),
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal()) : "";
                                String interven1 = entity.getFieldValue(EXPORT_FIELD_INTERV_1);
                                String interven2 = entity.getFieldValue(EXPORT_FIELD_INTERV_2);
                                value = !interven1.equals(valueToCheck3) && !interven2.equals(valueToCheck3)
                                        ? valueToCheck3
                                        : "";

                            }
                        }
                    }
                    if (!StringUtils.isEmpty(instructional)) {
                        List<String> instructionals =
                                StringUtils.convertDelimitedStringToList(instructional, ',', true);
                        if (instructionals != null && !instructionals.isEmpty()) {
                            int instructionalsSize = instructionals.size();
                            if (PARAM_INSTRUCTION_1.equals(param)) {
                                value = instructionalsSize > 0 ? data.lookupReferenceCodeByBeanPath(
                                        ConductAction.class,
                                        sbarData.m_fieldInstructional,
                                        instructionals.get(0),
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal()) : "";
                            } else if (PARAM_INSTRUCTION_2.equals(param)) {
                                value = instructionalsSize > 1 ? data.lookupReferenceCodeByBeanPath(
                                        ConductAction.class,
                                        sbarData.m_fieldInstructional,
                                        instructionals.get(1),
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal()) : "";
                            } else if (PARAM_INSTRUCTION_3.equals(param)) {
                                value = instructionalsSize > 2 ? data.lookupReferenceCodeByBeanPath(
                                        ConductAction.class,
                                        sbarData.m_fieldInstructional,
                                        instructionals.get(2),
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal()) : "";
                            }
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retrieve the number of days sanctioned.
     */
    protected static class RetrieveDaysSanctioned implements FieldRetriever {
        protected static final String RETRIEVER_ID = "ACT_SANCTIONS";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            ConductIncident incident = (ConductIncident) entity.getBean();
            String valueToReturn = null;
            String param = (String) field.getParameter();
            if (!StringUtils.isEmpty(param)) {
                ArrayList<String> params = StringUtils.convertDelimitedStringToList(param, ',', true);
                String code = params.size() > 0 ? params.get(0) : "";
                String decimalNumberStr = params.size() > 1 ? params.get(1) : "0";
                String format = "%." + decimalNumberStr + "f";
                String beanPath = params.size() > 2 ? params.get(2) : "";
                boolean isAlias = params.size() > 3 ? ("Y".equals(params.get(3)) ? true : false) : false;
                ConductAction actionToConsider = incident.getConductActions().stream()
                        .filter(act -> BooleanAsStringConverter.TRUE
                                .equals(act.getFieldValueByAlias("DOE PRIMARY ACTION"))
                                && code.equals(data.lookupReferenceCodeByBeanPath(
                                        ConductAction.class,
                                        ConductAction.COL_ACTION_CODE,
                                        act.getActionCode(),
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal())))
                        .findAny().orElse(null);
                if (actionToConsider != null) {
                    Object valueByBeanPath = isAlias ? actionToConsider.getFieldValueByAlias(beanPath)
                            : actionToConsider.getFieldValueByBeanPath(beanPath);
                    if (valueByBeanPath != null) {
                        try {
                            valueToReturn = String.format(format, valueByBeanPath);
                        } catch (Exception exc) {
                            if (valueByBeanPath instanceof String && ((String) valueByBeanPath).matches("\\d+")
                                    || ((String) valueByBeanPath).matches("\\d+.\\d+")) {
                                BigDecimal bdNumeric = new BigDecimal((String) valueByBeanPath);
                                try {
                                    valueToReturn = String.format(format, bdNumeric);
                                } catch (Exception e) {
                                    valueToReturn = "0";
                                }
                            }
                        }
                    } else {
                        valueToReturn = "0";
                    }
                } else {
                    valueToReturn = "0";
                }
            }
            return valueToReturn;
        }
    }

    /**
     * Validate incident
     *
     * @author Follett Software Company
     * @copyright 2021
     */
    protected class ValidateIncident implements FieldValidator {
        public static final String VALIDATOR_ID = "incident";

        private Set<String> m_firearmCountRequired;
        private Set<String> m_weaponCountRequired;

        public ValidateIncident() {
            m_firearmCountRequired = new HashSet<String>();
            m_firearmCountRequired.add("PD1");
            m_firearmCountRequired.add("PD11");
            m_firearmCountRequired.add("PD12");
            m_firearmCountRequired.add("PD15");

            m_weaponCountRequired = new HashSet<String>();
            m_weaponCountRequired.add("BSC24");
            m_weaponCountRequired.add("PD2");
            m_weaponCountRequired.add("PD13");
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {

            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(2);
            // VAStudentBehaviorAREntityV2 sbarEntity = (VAStudentBehaviorAREntityV2) entity;
            //
            // String firearmCountStr = entity.getFieldValue(FIELD_B_FIREARM_COUNT);
            // String weaponCountStr = entity.getFieldValue(FIELD_B_WEAPON_COUNT);
            // int firearmCount = StringUtils.isEmpty(firearmCountStr) ? 0 :
            // Integer.parseInt(firearmCountStr);
            // int weaponCount = StringUtils.isEmpty(weaponCountStr) ? 0 :
            // Integer.parseInt(weaponCountStr);
            return errors;
        }
    }


    /**
     * Validate offense code.
     *
     * 1. Incidents with the following offense codes
     * MUST have more than 1 offense code:
     *
     * A1T (except for action code 1 or 4)
     * BA1
     * BA3
     *
     * 2. Incidents with the following offense codes
     * MUST have more than one offender:
     *
     * FA2
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateOffense implements FieldValidator {
        public static final String VALIDATOR_ID = "offense";

        private Map<String, Integer> m_reportToLawRequired;
        private Set<String> m_victimCountRequired;
        private Set<String> m_AuthCodeRequired;

        public ValidateOffense() {
            // Integer lawConditional = Integer.valueOf(1);
            Integer lawRequired = Integer.valueOf(2);
            m_reportToLawRequired = new HashMap<String, Integer>();
            // listed on Behavior codes page as required.
            m_reportToLawRequired.put("PD1", lawRequired);
            m_reportToLawRequired.put("PD2", lawRequired);
            m_reportToLawRequired.put("PD3", lawRequired);
            m_reportToLawRequired.put("PD4", lawRequired);
            m_reportToLawRequired.put("PD5", lawRequired);
            m_reportToLawRequired.put("PD6", lawRequired);
            m_reportToLawRequired.put("PD7", lawRequired);
            m_reportToLawRequired.put("PD8", lawRequired);
            m_reportToLawRequired.put("PD9", lawRequired);
            m_reportToLawRequired.put("PD10", lawRequired);
            m_reportToLawRequired.put("PD11", lawRequired);
            m_reportToLawRequired.put("PD12", lawRequired);
            m_reportToLawRequired.put("PD13", lawRequired);
            m_reportToLawRequired.put("PD14", lawRequired);
            m_reportToLawRequired.put("PD15", lawRequired);
            m_reportToLawRequired.put("PD16", lawRequired);
            // listed on Row C field Law Enforcement as required.
            m_reportToLawRequired.put("BSC1", lawRequired);
            m_reportToLawRequired.put("BSC2", lawRequired);
            m_reportToLawRequired.put("BSC18", lawRequired);
            m_reportToLawRequired.put("BESO2", lawRequired);
            m_reportToLawRequired.put("BESO5", lawRequired);
            m_reportToLawRequired.put("BESO7", lawRequired);
            m_reportToLawRequired.put("BESO8", lawRequired);
            m_reportToLawRequired.put("BESO14", lawRequired);
            m_reportToLawRequired.put("BESO15", lawRequired);
            m_reportToLawRequired.put("BESO16", lawRequired);

            m_victimCountRequired = new HashSet<String>();
            m_victimCountRequired.add("BSO10");
            m_victimCountRequired.add("RB1");
            m_victimCountRequired.add("RB2");
            m_victimCountRequired.add("RB5");
            m_victimCountRequired.add("RB9");
            m_victimCountRequired.add("BSC6");
            m_victimCountRequired.add("BSC7");
            m_victimCountRequired.add("BSC18");
            m_victimCountRequired.add("BSC19");
            m_victimCountRequired.add("BSC21");
            m_victimCountRequired.add("BSC22");
            m_victimCountRequired.add("BESO1");
            m_victimCountRequired.add("BESO2");
            m_victimCountRequired.add("BESO4");
            m_victimCountRequired.add("BESO11");
            m_victimCountRequired.add("BESO12");
            m_victimCountRequired.add("BESO13");
            m_victimCountRequired.add("BESO15");
            m_victimCountRequired.add("BESO16");
            m_victimCountRequired.add("BESO17");
            m_victimCountRequired.add("PD1");
            m_victimCountRequired.add("PD2");
            m_victimCountRequired.add("PD3");
            m_victimCountRequired.add("PD4");
            m_victimCountRequired.add("PD5");
            m_victimCountRequired.add("PD6");
            m_victimCountRequired.add("PD7");
            m_victimCountRequired.add("PD8");
            m_victimCountRequired.add("PD9");
            m_victimCountRequired.add("PD10");

            m_AuthCodeRequired = new HashSet<String>();
            m_AuthCodeRequired.add("BAPOTH");
            m_AuthCodeRequired.add("BSOOTH");
            m_AuthCodeRequired.add("RBOTH");
            m_AuthCodeRequired.add("BSCOTH");
            m_AuthCodeRequired.add("BESOOTH");

        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(2);

            // Report To Law Enformcement required.
            String reportToLaw = entity.getFieldValue(FIELD_C_LAW_ENFORCEMENT);
            if (m_reportToLawRequired.containsKey(value) && !"Y".equals(reportToLaw)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Behavior requires the incident is reported to law enformcement.",
                        "Offense code = " + STYLE_BOLD + value + STYLE_END));
            }

            // Victim count is required.
            String authCode = entity.getFieldValue(FIELD_C_AUTH_CODE);
            if (m_AuthCodeRequired.contains(value) && StringUtils.isEmpty(authCode)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Behavior requires a DOE Authorization code.",
                        "Offense code = " + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }
    }

    /**
     * Constants
     */
    protected static final String ALIAS_ACT_AGGR_CIRCUMSTANCES = "all-act-AggravatingCircumstances";
    protected static final String ALIAS_ACT_ALT_AGENCY = "all-act-altPlacementEdAgency";
    protected static final String ALIAS_ACT_INTERVENTION = "all-act-BehaviorIntervention";
    protected static final String ALIAS_ACT_INSTRUCTIONAL = "all-act-instructionalSupports";
    protected static final String ALIAS_ACT_PLACEMENT_SKL = "all-act-altPlacementSchool";
    protected static final String ALIAS_CND_AGGREV = "all-cnd-AggravatingCircumstances";
    protected static final String ALIAS_CND_CANO_FLAG = "all-cnd-CAHOFlag";
    protected static final String ALIAS_CND_DOE_AUTH = "all-cnd-doeAuth";
    protected static final String ALIAS_CND_ENROLL_DISTRICT = "DOE ENROLLED DISTRICT";
    protected static final String ALIAS_CND_ENROLL_SCHOOL = "DOE ENROLLED SCHOOL";
    protected static final String ALIAS_CND_INCIDENT_DISTRICT = "DOE INCIDENT DISTRICT";
    protected static final String ALIAS_CND_INCIDENT_SCHOOL = "DOE INCIDENT SCHOOL";
    protected static final String ALIAS_CND_LAW_ENFORCE = "all-cnd-LawEnforcementChargesFiled";
    protected static final String ALIAS_CND_NOTE_CONVICTION = "all-cnd-NotifiedOfConviction";
    protected static final String ALIAS_CND_VICTIM_INDETERM = "all-cnd-IndeterminateVictimCount";
    protected static final String ALIAS_CND_UNKNOWN_OFFENDER = "all-cnd-doeUnknownOffender";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_SKL_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL ID";
    protected static final char CHAR_TAB = '\t';
    protected static final String DICTIONARY_ID_UDA = "CND-SBAR";
    protected static final String FIELD_B_FIREARM_COUNT = "Firearms Confiscated";
    protected static final String FIELD_B_WEAPON_COUNT = "Non-firearm Weapons Confiscated";
    protected static final String FIELD_C_AUTH_CODE = "Authorization Code";
    protected static final String FIELD_C_LAW_ENFORCEMENT = "Law Enforcement";
    protected static final List<String> FLAG_FEDERAL_DCV = Arrays.asList("DCV", "SBAR");
    protected static final String PARAM_FULL_YEAR = "fullYear";
    protected static final String PARAM_HEADERS = "headers";
    protected static final String PARAM_LIMIT_OFFENSES = "limitOffenses";
    protected static final String PARAM_QUERY_BY = "queryBy";
    protected static final String PARAM_QUERY_STRING = "queryString";
    protected static final String PARAM_SORT = "sort";

    /**
     * Members
     */
    protected String m_currentIncidentId;
    protected int m_currentIncidentCount;
    protected DataDictionary m_udaDictionary;
    protected String m_fieldActAggravatingCircumstances;
    protected String m_fieldActAltPlacementEdAgency;
    protected String m_fieldActAltPlacementSchool;
    protected String m_fieldDistrictId;
    protected String m_fieldIntervention;
    protected String m_fieldInstructional;
    protected String m_fieldSchoolId;

    /**
     * Construct the A row as the heading.
     * It will be generated once at the top of the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        Boolean fullYear = (Boolean) getParameter(PARAM_FULL_YEAR);
        Boolean header = (Boolean) getParameter(PARAM_HEADERS);
        StringBuilder heading = new StringBuilder(16);
        if (header != null && header.booleanValue()) {
            heading.append(super.getHeading());
        }
        heading.append("A");
        if (fullYear != null && fullYear.booleanValue()) {
            heading.append("03");
        } else {
            heading.append("17");
        }
        heading.append(Integer.toString(getCurrentContext().getSchoolYear() - 1));
        heading.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictId));
        heading.append(System.lineSeparator());
        return heading.toString();
    }

    /**
     * Initialize,
     * Query across StudentConductIncident as primary iteration.
     *
     * @throws X2BaseException
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        m_fieldDistrictId = translateAliasToJavaName(ALIAS_SKL_DISTRICT_ID, true);
        m_fieldSchoolId = translateAliasToJavaName(ALIAS_SKL_STATE_ID, true);
        m_fieldIntervention = translateAliasToJavaName(ALIAS_ACT_INTERVENTION, true);
        m_fieldInstructional = translateAliasToJavaName(ALIAS_ACT_INSTRUCTIONAL, true);
        m_fieldActAggravatingCircumstances = translateAliasToJavaName(ALIAS_ACT_AGGR_CIRCUMSTANCES, true);
        m_fieldActAltPlacementEdAgency = translateAliasToJavaName(ALIAS_ACT_ALT_AGENCY, true);
        m_fieldActAltPlacementSchool = translateAliasToJavaName(ALIAS_ACT_PLACEMENT_SKL, true);

        X2Criteria incidentCriteria = getIncidentCriteria();
        QueryByCriteria incidentQuery = new QueryByCriteria(ConductIncident.class, incidentCriteria, true);
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
                incidentQuery.addOrderByAscending(ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                        School.COL_NAME);
                incidentQuery.addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER +
                        Student.COL_NAME_VIEW);
                break;

            case 3: // LASID
                incidentQuery.addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + Student.COL_LOCAL_ID);
                break;

            case 4: // SASID
                incidentQuery.addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + Student.COL_STATE_ID);
                break;

            case 5: // Error Type
                // Then sort grid.
                break;

            default:
                incidentQuery.addOrderByAscending(ConductIncident.REL_STUDENT + PATH_DELIMITER + Student.COL_NAME_VIEW);
                break;
        }
        incidentQuery.addOrderBy(ConductIncident.COL_INCIDENT_ID, true);

        setQuery(incidentQuery);
        setEntityClass(VAStudentBehaviorAREntityV2.class);

        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveDaysSanctioned.RETRIEVER_ID, new RetrieveDaysSanctioned());
        calcs.put(RetrieveIncidentEAID.RETRIEVER_ID, new RetrieveIncidentEAID());
        calcs.put(RetrieveEnrollEAID.RETRIEVER_ID, new RetrieveEnrollEAID());
        calcs.put(RetrieveActionElement.RETRIEVER_ID, new RetrieveActionElement());
        super.addCalcs(calcs);

        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(ValidateIncident.VALIDATOR_ID, new ValidateIncident());
        validators.put(ValidateOffense.VALIDATOR_ID, new ValidateOffense());
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
     * Construct the criteria for selecting conduct incidents.
     *
     * @return X2Criteria
     */
    private X2Criteria getIncidentCriteria() {
        String excludeField = translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);

        X2Criteria incidentCriteria = new X2Criteria();
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        switch (queryBy) {
            case 1: // YOG
                incidentCriteria.addEqualTo(ConductIncident.REL_STUDENT + PATH_DELIMITER +
                        Student.COL_YOG, queryString);
                break;

            case 2: // LASID
                incidentCriteria.addEqualTo(ConductIncident.REL_STUDENT + PATH_DELIMITER +
                        Student.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                incidentCriteria.addEqualTo(ConductIncident.REL_STUDENT + PATH_DELIMITER +
                        Student.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(incidentCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        incidentCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getStartDate());
        incidentCriteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getEndDate());
        incidentCriteria.addNotEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                Boolean.TRUE);
        if (excludeField != null) {
            incidentCriteria.addNotEqualTo(ConductIncident.REL_STUDENT + PATH_DELIMITER +
                    excludeField, BooleanAsStringConverter.TRUE);
        }
        if (isSchoolContext()) {
            School school = getSchool();
            incidentCriteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, school.getOid());
        }
        // If the user selects to limit the offenses, identify reportable offenses and actions.
        Boolean objLimitOffenses = (Boolean) getParameter(PARAM_LIMIT_OFFENSES);
        if (objLimitOffenses != null && objLimitOffenses.booleanValue()) {
            String offenseRefTblOid = null;
            String actionRefTblOid = null;

            // 1. Find the reference code tables for incidents and actions.
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

            // 2. Load lists of reportable codes (codes that contain "DCV" in the Federal column).
            List<String> incidentCodes = new ArrayList<String>();
            List<String> actionCodes = new ArrayList<String>();
            if (!StringUtils.isEmpty(offenseRefTblOid)) {
                Criteria criteria = new Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, offenseRefTblOid);
                criteria.addIn(ReferenceCode.COL_FEDERAL_CODE, FLAG_FEDERAL_DCV);
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
                criteria.addIn(ReferenceCode.COL_FEDERAL_CODE, FLAG_FEDERAL_DCV);
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
            Criteria codeCriteria = new Criteria();
            Criteria codeOCriteria = new Criteria();
            Criteria codeACriteria = new Criteria();
            if (incidentCodes.size() > 0 && actionCodes.size() > 0) {
                // Not using offense sub table, check each of three code fields individually.
                codeCriteria.addIn(ConductIncident.COL_INCIDENT_CODE, incidentCodes);
                codeOCriteria.addIn(ConductIncident.REL_CONDUCT_OFFENSES + PATH_DELIMITER +
                        ConductOffense.COL_INCIDENT_CODE, incidentCodes);
                codeACriteria.addIn(ConductIncident.REL_CONDUCT_ACTIONS + PATH_DELIMITER +
                        ConductAction.COL_ACTION_CODE, actionCodes);
                codeCriteria.addOrCriteria(codeACriteria);
                codeCriteria.addOrCriteria(codeOCriteria);
                incidentCriteria.addAndCriteria(codeCriteria);
            }
        }
        applyInputCriteria(incidentCriteria, false, ConductIncident.REL_STUDENT);
        return incidentCriteria;
    }
}
