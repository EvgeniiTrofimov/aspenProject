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

package com.x2dev.procedures.statereporting.wa;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
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
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.QueryFactory;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Export procedure for WA StudentDiscipline.
 *
 * @author X2 Development Corporation
 */
public class WAStudentDiscipline extends StateReportData {
    /**
     * Entity class for WA StudentDiscipline export.
     *
     */
    public static class WAStudentDisciplineEntity extends StateReportEntity {

        private static final String STATE_CODE_8 = "8";

        /**
         * The Class EntityItem.
         */
        private class EntityItem {
            ConductAction m_action;
            String m_schoolId;

            /**
             * Instantiates a new entity item.
             *
             * @param action ConductAction
             * @param schoolId String
             */
            public EntityItem(ConductAction action, String schoolId) {
                m_action = action;
                m_schoolId = schoolId;
            }

            /**
             * Gets the action.
             *
             * @return Conduct action
             */
            public ConductAction getAction() {
                return m_action;
            }

            /**
             * Gets the school id.
             *
             * @return String
             */
            public String getSchoolId() {
                return m_schoolId;
            }
        }

        private List<EntityItem> m_entityItems = new ArrayList<EntityItem>();

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public WAStudentDisciplineEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);


            WAStudentDiscipline sdData = (WAStudentDiscipline) data;
            ConductIncident incident = (ConductIncident) bean;
            String incidentStateCode =
                    data.lookupReferenceCodeByBeanPath(ConductIncident.class, ConductIncident.COL_INCIDENT_CODE,
                            incident.getIncidentCode(), ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            Collection<ConductAction> actions = sdData.m_actions.get(bean.getOid());
            WAStudentDiscipline cndData = (WAStudentDiscipline) data;
            if (actions != null && actions.size() > 0) {
                for (ConductAction action : actions) {
                    processRow(cndData, incident, action);
                }
            } else if (STATE_CODE_8.equals(incidentStateCode)) // report incident even if there are
                                                               // no actions associated
            {
                processRow(cndData, incident, null);
            }
            setRowCount(m_entityItems.size());
        }

        /**
         * return current action associated with this incident.
         * This would be any action with a reportable action code.
         *
         * @return ConductAction
         */
        public ConductAction getCurrentAction() {
            return m_entityItems.isEmpty() ? null : m_entityItems.get(getCurrentRow()).getAction();
        }

        /**
         * return current school ID associated with this action.
         *
         * @return ConductAction
         */
        public String getCurrentSchoolID() {
            return m_entityItems.isEmpty() ? null : m_entityItems.get(getCurrentRow()).getSchoolId();
        }

        /**
         *
         * return current state code for appropriated beanPath.
         *
         * @param data StateReportData
         * @param beanPath String
         * @return String
         * @throws X2BaseException exception
         */
        public String getCurrentStateCode(StateReportData data, String beanPath) throws X2BaseException {
            String incidentCode = (String) data.getProperty(this.getBean(), beanPath);
            String stateCode =
                    data.lookupReferenceCodeByBeanPath(ConductIncident.class, ConductIncident.COL_INCIDENT_CODE,
                            incidentCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            ConductAction action = this.getCurrentAction();
            String value = null;
            if (action != null) {
                String actionCode = action.getActionCode();
                value = data.lookupReferenceCodeByBeanPath(ConductAction.class, ConductAction.COL_ACTION_CODE,
                        actionCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }
            if (value != null && !value.isEmpty() && (stateCode == null || stateCode.isEmpty())) {
                stateCode = DEFAULT_BEHAVIOR;
            }
            return stateCode;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            ConductIncident incident = (ConductIncident) getBean();
            String name = incident.getStudent().getNameView() +
                    " [LASID: " + incident.getStudent().getLocalId() +
                    ", SASID: " + incident.getStudent().getStateId() +
                    "] ID:" +
                    incident.getIncidentId();

            return name;
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
         * Process each row of data (conduct incident and action combinations). Create an EntityItem
         * for each reportable incident/action.
         *
         * @param data WAStudentDiscipline
         * @param incident ConductIncident
         * @param action ConductAction
         */
        private void processRow(WAStudentDiscipline data, ConductIncident incident, ConductAction action) {
            Set<String> reportableActions = new HashSet<String>(Arrays.asList("EX", "LS", "SS", "IS", "EE"));
            String schoolId = EMPTY_STRING;
            String actionCode = EMPTY_STRING;
            String actionStateCode = EMPTY_STRING;
            SisStudent student = incident.getStudent();
            String incidentCode = incident.getIncidentCode();
            String incidentStateCode =
                    data.lookupReferenceCodeByBeanPath(ConductIncident.class, ConductIncident.COL_INCIDENT_CODE,
                            incidentCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            boolean mustIncludeIncident = STATE_CODE_8.equals(incidentStateCode);
            List<StudentEnrollmentSpan> spans = null;
            StudentEnrollment enrollment = null;
            if (data.m_helper != null && student != null) {
                spans = data.m_helper.getStudentEnrollmentSpans(student, true);
            }
            if (spans != null && spans.size() > 0) {
                for (StudentEnrollmentSpan span : spans) {
                    if (span != null) {
                        enrollment = span.getFirstActiveEnrollment();
                    }
                }
            }
            if (enrollment != null) {
                schoolId = (String) enrollment.getFieldValueByBeanPath(data.m_fieldLocationOverride);
            }
            if (StringUtils.isEmpty(schoolId)) {
                if (action != null && action.getSchool() != null) {
                    schoolId = action.getSchool().getSchoolId();
                }
            }
            // If value is still empty, get LocationId from incident
            if (StringUtils.isEmpty(schoolId)) {
                schoolId = incident.getSchool().getSchoolId();
            }

            if (data.includeSchool(schoolId)) {
                if (action != null) {
                    actionCode = action.getActionCode();
                    actionStateCode =
                            data.lookupReferenceCodeByBeanPath(ConductAction.class, ConductAction.COL_ACTION_CODE,
                                    actionCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    if (reportableActions.contains(actionStateCode) || mustIncludeIncident) {
                        m_entityItems.add(new EntityItem(action, schoolId));
                    }
                }
                // Create an action/record for required incidents with no associated actions found
                else if (mustIncludeIncident) {
                    action = new ConductAction(data.getBroker().getPersistenceKey());
                    action.setStudentOid(student.getOid());
                    action.setIncidentOid(incident.getOid());
                    action.setSchoolOid(incident.getSchoolOid());
                    m_entityItems.add(new EntityItem(action, schoolId));
                }
            }
        }
    }

    /*
     * Constants
     */
    protected static final String CODE_WEAPON_POSESSION = "8";
    protected static final String DEFAULT_BEHAVIOR = "9";

    protected static final String VALIDATION_ID_WEAPON = "WASD-WEAPON";

    protected static final String PARAM_ACTION_CODE = "ACTION_CODE";
    protected static final String PARAM_ACTION_DATE = "ACTION_DATE";
    protected static final String PARAM_ACTION_DAYS = "ACTION_DAYS";
    protected static final String PARAM_ACTION_IAES = "IAES";
    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";
    protected static final String PARAM_SCHOOL_ID = "SCHOOL_ID";

    protected static final String ALIAS_ACADEMIC_SERVICES = "DOE ACADEMIC SERVICES";
    protected static final String ALIAS_APPEAL_CODE = "DOE APPEAL CODE";
    protected static final String ALIAS_BEHAVIOR_SERVICES = "DOE BEHAVIOR SERVICES";
    protected static final String ALIAS_DATE_PETITION_FOR_READMISSION = "DOE DATE PETITION FOR READMISSION";
    protected static final String ALIAS_EMERGENCY_EXPULSION = "DOE EMERGENCY EXPULSION";
    protected static final String ALIAS_EMERGENCY_EXPULSION_DAYS = "DOE EMERGENCY EXPULSION DAYS";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_IAES = "IAES";
    protected static final String ALIAS_LOCATION_OVERRIDE = "DOE LOC OVERRIDE";
    protected static final String ALIAS_PETITION_FOR_READMISSION_GR = "DOE PETITION FOR READMISSION GRANTED";
    protected static final String ALIAS_PETITION_TO_EXCEED_ONE_YEAR = "DOE PETITION TO EXCEED ONE YEAR";
    protected static final String ALIAS_REENGAGEMENT_MEETING_DATE = "DOE REENGAGEMENT MEETING DATE";
    protected static final String ALIAS_REENGAGEMENT_PLAN = "DOE REENGAGEMENT PLAN";


    /*
     * Instance variables.
     */
    protected Map<String, Collection<ConductAction>> m_actions = null;
    protected Criteria m_actionCriteria = null;
    protected StudentHistoryHelper m_helper;
    protected String m_fieldLocationOverride;
    protected Map<String, Collection<ConductOffense>> m_otherBehaviors;
    protected Collection<String> m_incidentCodes = null;
    protected Map<String, String> m_stateIncidentCodes;

    /*
     * UDF names
     */
    protected String m_fieldAcademicServices;
    protected String m_fieldAppealCode;
    protected String m_fieldBehaviorServices;
    protected String m_fieldDatePetitionForReadmission;
    protected String m_fieldEmergencyExpulsionDays;
    protected String m_fieldIsEmergencyExpulsion;
    protected String m_fieldPetitionForReadmissionGranted;
    protected String m_fieldPetitionToExceedOneYear;
    protected String m_fieldReengagementMeetingDate;
    protected String m_fieldReengagementPlan;

    public Set m_excludeSchool;

    /**
     * Retrieve the school Id/Override Location Code.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSchool implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            WAStudentDisciplineEntity sdEntity = (WAStudentDisciplineEntity) entity;
            String value = sdEntity.getCurrentSchoolID();
            return value;
        }
    }

    /**
     * Retrieve the action code state value for the action.
     * Or default to continuation for no offense.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveAction implements FieldRetriever {
        private static final String NO_ACTION = "NA";
        private static final String OTHER_ACTION = "OT";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            WAStudentDiscipline sdData = (WAStudentDiscipline) data;
            WAStudentDisciplineEntity sdEntity = (WAStudentDisciplineEntity) entity;
            ConductAction action = sdEntity.getCurrentAction();
            String param = (String) field.getParameter();
            Object value = null;

            if (PARAM_ACTION_CODE.equals(param)) {
                WAStudentDisciplineEntity currentEntity = (WAStudentDisciplineEntity) entity;
                Object behaviorStateCode = currentEntity.getCurrentStateCode(data, ConductIncident.COL_INCIDENT_CODE);
                if (behaviorStateCode != null) {
                    // We must check that the ConductAction oid is not null/empty, as we are
                    // creating actions for required incidents with no actions defined in the
                    // database.
                    // These temporary ConductAction objects have no OID, as they are never saved to
                    // the database and are only used for generating rows in this export.
                    if (action != null && !StringUtils.isEmpty(action.getOid())) {
                        String actionCode = action.getActionCode();
                        value = data.lookupReferenceCodeByBeanPath(ConductAction.class, ConductAction.COL_ACTION_CODE,
                                actionCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        if (value == null || (value instanceof String && StringUtils.isEmpty((String) value))) {
                            value = OTHER_ACTION;
                        }
                    } else if (sdData.m_actions.get(entity.getBean().getOid()) != null) {
                        value = OTHER_ACTION; // Other action
                    } else {
                        value = NO_ACTION; // No action
                    }
                }
            } else if (PARAM_ACTION_DAYS.equals(param) && action != null) {
                if (action.getActionServedTime() != null && action.getActionServedTime().doubleValue() != 0) {
                    value = action.getActionServedTime();
                } else {
                    // Total of action sanctioned days on all action records.
                    value = action.getActionPenaltyTime();
                }
            } else if (PARAM_ACTION_DATE.equals(param)) {
                if (action != null) {
                    value = action.getActionStartDate();
                } else {
                    Collection<ConductAction> actions = sdData.m_actions.get(entity.getBean().getOid());
                    if (actions != null) {
                        return actions.iterator().next().getActionStartDate();
                    }
                }
            } else if (PARAM_ACTION_IAES.equals(param)) {
                if (action == null) {
                    Collection<ConductAction> actions = sdData.m_actions.get(entity.getBean().getOid());
                    if (actions != null) {
                        action = actions.iterator().next();
                    }
                }
                if (action != null) {
                    String iaes = (String) action.getFieldValueByAlias(ALIAS_IAES);

                    if (!StringUtils.isEmpty(iaes)) {
                        iaes = iaes.substring(0, 1);
                    }
                    value = iaes;
                }
            }

            return value;
        }
    }

    /**
     * The Class RetrieveActionAlias.
     */
    protected class RetrieveActionAlias implements FieldRetriever {
        private static final String PARAM_ACADEMIC_SERVICES = "ACADEMIC_SERVICES";
        private static final String PARAM_APPEAL_CODE = "APPEAL_CODE";
        private static final String PARAM_BEHAVIOR_SERVICES = "BEHAVIOR_SERVICES";
        private static final String PARAM_DATE_PETITION_FOR_READMISSION = "DATE_PETITION_FOR_READMISSION";
        private static final String PARAM_PETITION_FOR_READMISSION_GRANTED = "PETITION_FOR_READMISSION_GRANTED";
        private static final String PARAM_PETITION_TO_EXCEED_ONE_YEAR = "PETITION_TO_EXCEED_ONE_YEAR";
        private static final String PARAM_REENGAGEMENT_MEETING_DATE = "REENGAGEMENT_MEETING_DATE";
        private static final String PARAM_REENGAGEMENT_PLAN = "REENGAGEMENT_PLAN";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            WAStudentDisciplineEntity sdEntity = (WAStudentDisciplineEntity) entity;
            ConductAction action = sdEntity.getCurrentAction();

            String param = (String) field.getParameter();

            Object value = null;
            if (action != null) {
                if (PARAM_ACADEMIC_SERVICES.equals(param)) {
                    value = data.lookupReferenceCodeByBeanPath(ConductAction.class, m_fieldAcademicServices,
                            (String) action.getFieldValueByBeanPath(m_fieldAcademicServices),
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                } else if (PARAM_APPEAL_CODE.equals(param)) {
                    value = data.lookupReferenceCodeByBeanPath(ConductAction.class, m_fieldAppealCode,
                            (String) action.getFieldValueByBeanPath(m_fieldAppealCode),
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                } else if (PARAM_BEHAVIOR_SERVICES.equals(param)) {
                    value = data.lookupReferenceCodeByBeanPath(ConductAction.class, m_fieldBehaviorServices,
                            (String) action.getFieldValueByBeanPath(m_fieldBehaviorServices),
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                } else if (PARAM_DATE_PETITION_FOR_READMISSION.equals(param)) {
                    value = action.getFieldValueByBeanPath(m_fieldDatePetitionForReadmission);
                } else if (PARAM_PETITION_FOR_READMISSION_GRANTED.equals(param)) {
                    value = action.getFieldValueByBeanPath(m_fieldPetitionForReadmissionGranted);
                } else if (PARAM_PETITION_TO_EXCEED_ONE_YEAR.equals(param)) {
                    value = data.lookupReferenceCodeByBeanPath(ConductAction.class, m_fieldPetitionToExceedOneYear,
                            (String) action.getFieldValueByBeanPath(m_fieldPetitionToExceedOneYear),
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                } else if (PARAM_REENGAGEMENT_MEETING_DATE.equals(param)) {
                    value = action.getFieldValueByBeanPath(m_fieldReengagementMeetingDate);
                } else if (PARAM_REENGAGEMENT_PLAN.equals(param)) {
                    value = data.lookupReferenceCodeByBeanPath(ConductAction.class, m_fieldReengagementPlan,
                            (String) action.getFieldValueByBeanPath(m_fieldReengagementPlan),
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }
            return value;
        }

    }

    /**
     * Retrieve the incident code and state equivalent. If it does not have a state code,
     * use the default value instead.
     */
    protected class RetrieveBehavior implements FieldRetriever {

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
            WAStudentDisciplineEntity currentEntity = (WAStudentDisciplineEntity) entity;
            String stateCode = currentEntity.getCurrentStateCode(data, field.getBeanPath());
            return stateCode;
        }
    }

    /**
     * Get state codes of additional incidents by primary incident oid.
     */
    protected class RetrieveOtherBehaviors implements FieldRetriever {
        private static final String COMMA = ",";


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String incidentOid = entity.getBean().getOid();
            Collection<ConductOffense> otherBehaviors = m_otherBehaviors.get(incidentOid);

            if (otherBehaviors == null) {
                return "";
            }

            WAStudentDisciplineEntity currentEntity = (WAStudentDisciplineEntity) entity;
            String behaviorCode = currentEntity.getCurrentStateCode(data, ConductIncident.COL_INCIDENT_CODE);
            Set<String> stateCodes = new TreeSet<String>();
            for (ConductOffense conductOffense : otherBehaviors) {
                String behaviorLocalCode = conductOffense.getFirstIdentifyingValue();
                if (m_stateIncidentCodes.containsKey(behaviorLocalCode)) {
                    String stateCode = m_stateIncidentCodes.get(behaviorLocalCode);
                    if (behaviorCode != null && behaviorCode.equals(stateCode)) {
                        continue;
                    }
                    if (stateCode.length() == 1) {
                        stateCodes.add("0" + stateCode);
                    } else {
                        stateCodes.add(stateCode);
                    }
                }
            }
            return StringUtils.convertCollectionToDelimitedString(stateCodes, COMMA);
        }
    }

    /**
     * Get emergency expulsion marker and emergency expulsion duration by incident oid.
     */
    protected class RetrieveEmergencyExpulsion implements FieldRetriever {
        private static final String PARAM_IS_EMERGENCY_EXPULSION = "EE_CODE";
        private static final String PARAM_EMERGENCY_EXPULSION_DAYS = "EE_DAYS_CODE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            WAStudentDisciplineEntity sdEntity = (WAStudentDisciplineEntity) entity;
            ConductAction action = sdEntity.getCurrentAction();

            String param = (String) field.getParameter();

            Object value = null;
            if (action != null && PARAM_IS_EMERGENCY_EXPULSION.equals(param)) {
                value = action.getFieldValueByBeanPath(m_fieldIsEmergencyExpulsion);
            } else if (action != null && PARAM_EMERGENCY_EXPULSION_DAYS.equals(param)) {
                String tempValue = (String) action.getFieldValueByBeanPath(m_fieldEmergencyExpulsionDays);

                try {
                    if (tempValue != null && Double.valueOf(tempValue).doubleValue() > 0.0) {
                        value = Double.valueOf(tempValue);
                    }
                } catch (NumberFormatException nfe) {
                    // Do nothing
                }
            }
            return value;
        }
    }

    /**
     * Validate the appeal code.
     */
    protected class ValidateAppealCode implements FieldValidator {
        private static final String CALC_ID = "APPEAL_CODE_VAL";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (StringUtils.isEmpty(value)) {
                String interventionApplied = entity.getFieldValue(FIELD_INTERVENTION_APPLIED);
                if (!StringUtils.isEmpty(interventionApplied) &&
                        (interventionApplied.equals(CODE_LS) || interventionApplied.equals(CODE_EX))) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + "  must contain one of the valid values 0,1,2,3,4  if " +
                                    FIELD_INTERVENTION_APPLIED + " is " + CODE_LS + " or " + CODE_EX,
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                    "; " + FIELD_INTERVENTION_APPLIED + " = " + STYLE_BOLD + interventionApplied
                                    + STYLE_END));
                }
            }

            return errors;
        }

    }

    /**
     * Validate the BEHAVIOR.
     */
    protected class ValidateBehavior implements FieldValidator {
        private static final String CALC_ID = "BEHAVIOR_VAL";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            // Element P07
            // Element P07 may not be reported with a valid value of 9 or 12, 13 or 17-21 if
            // Element P16 - Other Behaviors contains a valid value of 2-8, 10, 11 or 14-16.
            if (!StringUtils.isEmpty(value) && StringUtils.isInteger(value)) {
                int behCode = Integer.valueOf(value).intValue();
                if (behCode == 9 || behCode == 12 || behCode == 13 || (behCode >= 17 && behCode <= 21)) {
                    String otherBehs = entity.getFieldValue(FIELD_OTHER_BEHAVIORS);
                    otherBehs = otherBehs == null ? EMPTY_STRING : otherBehs;
                    String otherBehsM[] = otherBehs.split(ELEMENT_SEPARATOR);
                    for (String otherBeh : otherBehsM) {
                        if (!StringUtils.isEmpty(otherBeh) && StringUtils.isInteger(otherBeh)) {
                            int otherCode = Integer.valueOf(otherBeh).intValue();
                            if ((otherCode >= 2 && otherCode <= 8) || otherCode == 10 || otherCode == 11
                                    || (otherCode >= 14 && otherCode <= 16)) {
                                errors.add(new StateReportValidationError(entity, field,
                                        field.getFieldId()
                                                + " may not be reported with a valid value of 9 or 12, 13 or 17-21 if OtherBehaviors contains a valid value of 2-8, 10, 11 or 14-16.",
                                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                                "; " + FIELD_OTHER_BEHAVIORS + " = " + STYLE_BOLD + otherBeh
                                                + STYLE_END));
                            }
                        }

                    }
                }
            }
            if (!StringUtils.isEmpty(value) && !StringUtils.isInteger(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " is not numeric",
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
            }


            return errors;
        }
    }

    /**
     * Validate the OTHER BEHAVIOR.
     */
    protected class ValidateOtherBehavior implements FieldValidator {
        private static final String FIELD_BEHAVIOR_CODE = "Behavior Code";
        private static final String CALC_ID = "OTHER_BEHAVIOR_VAL";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            // Element P16
            // Behavior codes submitted in Element P16 must be a valid
            // value listed in Element P07 - Behavior Code. Valid values submitted in
            // Element P16 may not also be listed in Element P07 - Behavior Code. All
            // behaviors reported in this element must contain two digits and be reported as a
            // string with commas separating the valid values.
            String behCode = entity.getFieldValue(FIELD_BEHAVIOR_CODE);
            if (StringUtils.isEmpty(behCode) && !StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " may be reported  if " + FIELD_BEHAVIOR_CODE + " is not null",
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                "; " + FIELD_BEHAVIOR_CODE + " = " + STYLE_BOLD + behCode + STYLE_END));
            }

            if (!StringUtils.isEmpty(value) && !StringUtils.isEmpty(behCode)) {

                String otherBehsM[] = value.split(ELEMENT_SEPARATOR);
                for (String otherBeh : otherBehsM) {
                    if (StringUtils.isInteger(otherBeh)) {
                        int otherBehInt = Integer.valueOf(otherBeh).intValue();
                        if (!(otherBehInt >= 2 && otherBehInt <= 21)) {
                            errors.add(new StateReportValidationError(entity, field,
                                    field.getFieldId() + " is not valid. Should be from 2 to 21",
                                    field.getFieldId() + " = " + STYLE_BOLD + otherBeh + STYLE_END));
                        }

                        if (behCode.equals(otherBeh)) {
                            errors.add(new StateReportValidationError(entity, field,
                                    field.getFieldId() + " may not also be listed in " + FIELD_BEHAVIOR_CODE,
                                    field.getFieldId() + " = " + STYLE_BOLD + otherBeh + STYLE_END +
                                            "; " + FIELD_BEHAVIOR_CODE + " = " + STYLE_BOLD + behCode + STYLE_END));
                        }
                    } else {
                        errors.add(new StateReportValidationError(entity, field,
                                field.getFieldId() + " is not numeric",
                                field.getFieldId() + " = " + STYLE_BOLD + otherBeh + STYLE_END));
                    }
                }
            }

            return errors;
        }
    }

    /**
     * Validate the Conversion.
     */
    protected class ValidateBehaviorServices implements FieldValidator {
        private static final String CALC_ID = "BEHAVIOR-SRVS-VAL";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (StringUtils.isEmpty(value)) {
                String interventionApplied = entity.getFieldValue(FIELD_INTERVENTION_APPLIED);
                if (!StringUtils.isEmpty(interventionApplied) &&
                        (interventionApplied.equals(CODE_SS) || interventionApplied.equals(CODE_LS)
                                || interventionApplied.equals(CODE_EX))) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + "  may not be NULL if  Intervention Applied is " + CODE_SS
                                    + ELEMENT_SEPARATOR + CODE_LS + " or " + CODE_EX,
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                    "; " + FIELD_INTERVENTION_APPLIED + " = " + STYLE_BOLD + interventionApplied
                                    + STYLE_END));
                }

            }

            return errors;
        }
    }

    /**
     * Validate the Conversion.
     */
    protected class ValidateConversion implements FieldValidator {
        private static final String CALC_ID = "CONVERSION-DAYS-VAL";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String emergencyExp = entity.getFieldValue(FIEDL_EMERGENCY_EXP);
            if (StringUtils.isEmpty(value)) {
                if (!StringUtils.isEmpty(emergencyExp) && emergencyExp.equals(CODE_Y)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + "  is required if " + FIEDL_EMERGENCY_EXP + " is " + CODE_Y,
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                    "; " + FIEDL_EMERGENCY_EXP + " = " + STYLE_BOLD + emergencyExp + STYLE_END));
                }

            } else {
                if (!StringUtils.isEmpty(emergencyExp) && emergencyExp.equals(CODE_N)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + "  should be empty if " + FIEDL_EMERGENCY_EXP + " is " + CODE_N,
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                    "; " + FIEDL_EMERGENCY_EXP + " = " + STYLE_BOLD + emergencyExp + STYLE_END));
                }
            }



            return errors;
        }
    }

    /**
     * Validate the IAES.
     */
    protected class ValidateIAES implements FieldValidator {
        private static final String CALC_ID = "IAES-VAL";

        private Collection<String> m_spedProgramCodes = null;
        private String m_fieldExcludePgm = null;
        private Map<String, Boolean> m_receiveSpedMap = new HashMap<String, Boolean>();

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String interVentionApplied = entity.getFieldValue(FIELD_INTERVENTION_APPLIED);
            if (StringUtils.isEmpty(value) && !StringUtils.isEmpty(interVentionApplied)
                    && !CODE_NA.equals(interVentionApplied)) {
                ConductIncident incident = (ConductIncident) entity.getBean();
                String studentOid = incident.getStudentOid();
                PlainDate incidentDate = formatDate(entity.getFieldValue(FIELD_INCIDENT_DATE));
                if (incidentDate != null && isReceiveSped(studentOid, incidentDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + " must be reported if " + FIELD_INTERVENTION_APPLIED +
                                    " other than " + CODE_NA
                                    + " and student is reported as receiving special education services",
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                    "; " + FIELD_INTERVENTION_APPLIED + " = " + STYLE_BOLD + interVentionApplied
                                    + STYLE_END + ". Student has IEP programm"));
                }

            }

            return errors;
        }

        /**
         * Gets the receive sped key.
         *
         * @param studentOid String
         * @param date PlainDate
         * @return String
         */
        private String getReceiveSpedKey(String studentOid, PlainDate date) {
            return studentOid + date.toString();
        }

        /**
         * Gets the sped program codes.
         *
         * @return Collection
         */
        private Collection<String> getSpedProgramCodes() {
            if (m_spedProgramCodes == null) {
                X2Criteria criteria = null;
                DataDictionaryField field = getDataDictionaryField(StudentProgramParticipation.class,
                        StudentProgramParticipation.COL_PROGRAM_CODE);
                String referenceTableOid = field.getReferenceTableOid();

                if (!StringUtils.isEmpty(referenceTableOid)) {
                    criteria = new X2Criteria();
                    criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
                    criteria.addEqualTo(ReferenceCode.COL_LOCAL_CODE, CODE_SPED);
                    SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
                    m_spedProgramCodes = getBroker().getSubQueryCollectionByQuery(query);
                }

                if (m_spedProgramCodes == null) {
                    m_spedProgramCodes = new ArrayList<String>();
                }
            }

            return m_spedProgramCodes;
        }

        /**
         * Checks if is receive sped.
         *
         * @param studentOid String
         * @param date PlainDate
         * @return true, if is receive sped
         */
        private boolean isReceiveSped(String studentOid, PlainDate date) {
            String key = getReceiveSpedKey(studentOid, date);
            Boolean isReceive = m_receiveSpedMap.get(key);
            if (isReceive == null) {
                Criteria criteria = new X2Criteria();
                criteria.addEqualTo(StudentProgramParticipation.COL_STUDENT_OID, studentOid);
                criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, getSpedProgramCodes());
                criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, date);
                criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, date);

                loadExcludePGMfield();
                if (m_fieldExcludePgm != null) {
                    criteria.addNotEqualTo(m_fieldExcludePgm, BooleanAsStringConverter.TRUE);
                }
                QueryByCriteria byCriteria = new QueryByCriteria(StudentProgramParticipation.class, criteria);
                int programmCount = getBroker().getCount(byCriteria);
                isReceive = Boolean.valueOf(programmCount > 0);
                m_receiveSpedMap.put(key, isReceive);
            }
            return isReceive.booleanValue();
        }

        /**
         * Load exclude PG mfield.
         */
        private void loadExcludePGMfield() {
            if (m_fieldExcludePgm == null) {
                m_fieldExcludePgm = translateAliasToJavaName(ALIAS_EXCLUDE_PGM, false);
            }
        }
    }

    /**
     * Validate the BEHAVIOR.
     */
    protected class ValidateIntervention implements FieldValidator {
        private static final String CALC_ID = "INTERV_DATE_VAL";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (StringUtils.isEmpty(value)) {
                String applied = entity.getFieldValue(FIELD_INTERVENTION_APPLIED);
                if (StringUtils.isEmpty(applied) || !applied.equals(CODE_NA)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + " may only be null if Intervention Applied is " + CODE_NA,
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                    "; " + FIELD_INTERVENTION_APPLIED + " = " + STYLE_BOLD + applied + STYLE_END));
                }

            }

            return errors;
        }
    }


    /**
     * Validate the meeting held.
     */
    protected class ValidateMeetingHeld implements FieldValidator {
        private static final String CALC_ID = "MEETING-HELD-VAL";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (!StringUtils.isEmpty(value)) {
                PlainDate meetingDate = formatDate(value);
                PlainDate incidentDate = null;
                String incidentSDate = entity.getFieldValue(FIELD_INCIDENT_DATE);
                incidentDate = formatDate(incidentSDate);
                if (incidentDate != null && meetingDate != null && meetingDate.before(incidentDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + "  it must be on or after the date entered in " + FIELD_INCIDENT_DATE,
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                    "; " + FIELD_INCIDENT_DATE + " = " + STYLE_BOLD + incidentSDate + STYLE_END));
                }
            }

            return errors;
        }

    }

    /**
     * Validate the Petition.
     */
    protected class ValidatePetition implements FieldValidator {
        private static final String CALC_ID = "PETITION-VAL";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (!StringUtils.isEmpty(value)) {
                PlainDate petitionDate = formatDate(value);
                String interventionSDate = entity.getFieldValue(FIELD_INTERVENTION_DATE);
                PlainDate intervDate = formatDate(interventionSDate);
                if (petitionDate != null && intervDate != null && petitionDate.before(intervDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + "  it must be on or after the date entered in "
                                    + FIELD_INTERVENTION_DATE,
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                    "; " + FIELD_INTERVENTION_DATE + " = " + STYLE_BOLD + interventionSDate
                                    + STYLE_END));
                }

                if (FIELD_PETITION_READMIN_GRA.equals(field.getFieldId())) {
                    String petitionSReadminSub = entity.getFieldValue(FIELD_PETITION_READMIN_SUB);
                    PlainDate petitionReadminSub = formatDate(petitionSReadminSub);
                    if (petitionDate != null && petitionReadminSub != null && petitionDate.before(petitionReadminSub)) {
                        errors.add(new StateReportValidationError(entity, field,
                                field.getFieldId() + "  it must be on or after the date entered in "
                                        + FIELD_PETITION_READMIN_SUB,
                                field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                        "; " + FIELD_PETITION_READMIN_SUB + " = " + STYLE_BOLD + petitionSReadminSub
                                        + STYLE_END));
                    }

                } else {
                    String petitionGRA = entity.getFieldValue(FIELD_PETITION_READMIN_GRA);
                    if (StringUtils.isEmpty(petitionGRA)) {
                        errors.add(new StateReportValidationError(entity, field,
                                field.getFieldId() + "  if it is enter - " + FIELD_PETITION_READMIN_GRA
                                        + " should be enter too",
                                field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                        "; " + FIELD_PETITION_READMIN_GRA + " = " + STYLE_BOLD + petitionGRA
                                        + STYLE_END));
                    }

                }

            }

            return errors;
        }

    }

    /**
     * Validate the Petition exceed 1 year.
     */
    protected class ValidatePetitionExceed implements FieldValidator {
        private static final String CALC_ID = "PETITIONEXCEEDYR-VAL";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (StringUtils.isEmpty(value)) {
                String interventionApplied = entity.getFieldValue(FIELD_INTERVENTION_APPLIED);
                if (!StringUtils.isEmpty(interventionApplied)
                        && (interventionApplied.equals(CODE_LS) || (interventionApplied.equals(CODE_EX)))) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + "  must contain one of the valid values: 0,1,2 if "
                                    + FIELD_INTERVENTION_APPLIED + " is " + CODE_LS + " or " + CODE_EX,
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                    "; " + FIELD_INTERVENTION_APPLIED + " = " + STYLE_BOLD + interventionApplied
                                    + STYLE_END));
                }
            }

            return errors;
        }

    }

    /**
     * Validate the reengagement plan.
     */
    protected class ValidateReengagementPlan implements FieldValidator {
        private static final String CALC_ID = "REENGAGEMENT-PLN-VAL";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (StringUtils.isEmpty(value)) {
                String interventionApplied = entity.getFieldValue(FIELD_INTERVENTION_APPLIED);
                if (!StringUtils.isEmpty(interventionApplied) &&
                        (interventionApplied.equals(CODE_SS) || interventionApplied.equals(CODE_LS)
                                || interventionApplied.equals(CODE_EX))) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + "  must contain one of the valid values 0,1,2,3  if " +
                                    FIELD_INTERVENTION_APPLIED + " is SS LS or EX",
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                    "; " + FIELD_INTERVENTION_APPLIED + " = " + STYLE_BOLD + interventionApplied
                                    + STYLE_END));
                }
            }

            return errors;
        }

    }

    /**
     * Validate weapon is populated for Possession of Weapon behavior.
     */
    protected class ValidateWeapon implements FieldValidator {
        private static final String FIELD_BEHAVIOR_CODE = "Behavior Code";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String incidentType = entity.getFieldValue(FIELD_BEHAVIOR_CODE);

            if (CODE_WEAPON_POSESSION.equals(incidentType) && StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "A weapon type is required whenever the " + FIELD_BEHAVIOR_CODE + " is "
                                + CODE_WEAPON_POSESSION,
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                " ;" + FIELD_BEHAVIOR_CODE + " = " + STYLE_BOLD + incidentType + STYLE_END));
            }

            return errors;
        }
    }

    private static final String ALIAS_EXCLUDE_PGM = "DOE EXCLUDE PGM";


    private static final String CODE_EX = "EX";
    private static final String CODE_LS = "LS";
    private static final String CODE_NA = "NA";
    private static final String CODE_SS = "SS";
    private static final String CODE_SPED = "SPED";
    private static final String CODE_Y = "Y";
    private static final String CODE_N = "N";

    private static final String DATE_FORMAT = "MM/dd/yyyy";

    private static final String ELEMENT_SEPARATOR = ",";

    private static final String FIEDL_EMERGENCY_EXP = "EmergencyExp";
    private static final String FIELD_INCIDENT_DATE = "Incident Date";
    private static final String FIELD_INTERVENTION_APPLIED = "Intervention Applied";
    private static final String FIELD_INTERVENTION_DATE = "Intervention Date";
    private static final String FIELD_OTHER_BEHAVIORS = "OtherBehaviors";
    private static final String FIELD_PETITION_READMIN_SUB = "Petition Readmin Sub";
    private static final String FIELD_PETITION_READMIN_GRA = "Petition Readmin Gra";

    private SimpleDateFormat m_dateFormatt = new SimpleDateFormat(DATE_FORMAT);

    /**
     * Checks if schoolId given is a school to exclude. If that school is excluded this method will
     * return false ie -
     * !included.
     *
     * @param schoolId String
     * @return boolean
     */
    public boolean includeSchool(String schoolId) {
        return !((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue() || ((m_excludeSchool == null)
                || !m_excludeSchool.contains(schoolId));
    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    /*
     * Initialize the data module.
     * Initialize necessary working resources.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     * com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        initFields();

        if (getSetupErrors().size() == 0) {
            loadIncidentCodes();
            loadActions();
            loadSchoolExcludeMap();
            loadOtherBehaviors();
            // Build query
            X2Criteria criteria = getDisciplineCriteria();
            QueryByCriteria query = new QueryByCriteria(ConductIncident.class, criteria);
            applyInputSort(query, ConductIncident.REL_STUDENT);
            setQuery(query);

            // Set Custom Entity
            setEntityClass(WAStudentDisciplineEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("WASD-SCHOOL", new RetrieveSchool());
            calcs.put("WASD-ACTION", new RetrieveAction());
            calcs.put("WASD-ACTION-ALIAS", new RetrieveActionAlias());
            calcs.put("WASD-BEHAVIOR", new RetrieveBehavior());
            calcs.put("WASD-EMERGENCY-EXP", new RetrieveEmergencyExpulsion());
            calcs.put("WASD-OTHER-BEHAVIORS", new RetrieveOtherBehaviors());
            super.addCalcs(calcs);

            // Build a map of validators
            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put(VALIDATION_ID_WEAPON, new ValidateWeapon());
            validators.put(ValidateAppealCode.CALC_ID, new ValidateAppealCode());
            validators.put(ValidateBehavior.CALC_ID, new ValidateBehavior());
            validators.put(ValidateBehaviorServices.CALC_ID, new ValidateBehaviorServices());
            validators.put(ValidateConversion.CALC_ID, new ValidateConversion());
            validators.put(ValidateIAES.CALC_ID, new ValidateIAES());
            validators.put(ValidateIntervention.CALC_ID, new ValidateIntervention());
            validators.put(ValidateMeetingHeld.CALC_ID, new ValidateMeetingHeld());
            validators.put(ValidatePetition.CALC_ID, new ValidatePetition());
            validators.put(ValidatePetitionExceed.CALC_ID, new ValidatePetitionExceed());
            validators.put(ValidateReengagementPlan.CALC_ID, new ValidateReengagementPlan());
            validators.put(ValidateOtherBehavior.CALC_ID, new ValidateOtherBehavior());



            super.addValidators(validators);
        }
    }

    /**
     * format date from string to plainDate.
     *
     * @param date String
     * @return PlainDate
     */
    PlainDate formatDate(String date) {
        PlainDate returnDate = null;
        if (!StringUtils.isEmpty(date)) {
            try {
                returnDate = new PlainDate(m_dateFormatt.parse(date));
            } catch (ParseException e) {
                // Nothing to do

            }
        }
        return returnDate;
    }

    /**
     * Adds ConductIncident selection by selected school and current context.
     *
     * @param criteria X2Criteria
     * @param relationToConductIncident String
     */
    private void addSchoolAndDateSelection(X2Criteria criteria, String relationToConductIncident) {
        // add school selection
        if (isSchoolContext()) {
            criteria.addEqualTo(relationToConductIncident + ConductIncident.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addEqualTo(relationToConductIncident + ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
            criteria.addEqualTo(relationToConductIncident + ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        }
        // add date selection
        criteria.addGreaterOrEqualThan(relationToConductIncident + ConductIncident.COL_INCIDENT_DATE,
                getOrganization().getCurrentContext().getStartDate());
        criteria.addLessOrEqualThan(relationToConductIncident + ConductIncident.COL_INCIDENT_DATE,
                getOrganization().getCurrentContext().getEndDate());
    }

    /**
     * Loads UDF names by aliases.
     */
    private void initFields() {
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_fieldLocationOverride = translateAliasToJavaName(ALIAS_LOCATION_OVERRIDE, true);
        m_fieldIsEmergencyExpulsion = translateAliasToJavaName(ALIAS_EMERGENCY_EXPULSION, true);
        m_fieldEmergencyExpulsionDays = translateAliasToJavaName(ALIAS_EMERGENCY_EXPULSION_DAYS, true);
        m_fieldAcademicServices = translateAliasToJavaName(ALIAS_ACADEMIC_SERVICES, true);
        m_fieldAppealCode = translateAliasToJavaName(ALIAS_APPEAL_CODE, true);
        m_fieldBehaviorServices = translateAliasToJavaName(ALIAS_BEHAVIOR_SERVICES, true);
        m_fieldDatePetitionForReadmission = translateAliasToJavaName(ALIAS_DATE_PETITION_FOR_READMISSION, true);
        m_fieldPetitionForReadmissionGranted = translateAliasToJavaName(ALIAS_PETITION_FOR_READMISSION_GR, true);
        m_fieldPetitionToExceedOneYear = translateAliasToJavaName(ALIAS_PETITION_TO_EXCEED_ONE_YEAR, true);
        m_fieldReengagementMeetingDate = translateAliasToJavaName(ALIAS_REENGAGEMENT_MEETING_DATE, true);
        m_fieldReengagementPlan = translateAliasToJavaName(ALIAS_REENGAGEMENT_PLAN, true);

    }

    /**
     * Develop the criteria for state reportable incidents.
     *
     * @return X 2 criteria
     */
    private X2Criteria getDisciplineCriteria() {
        X2Criteria criteria1 = new X2Criteria();
        X2Criteria criteria2 = new X2Criteria();

        if (m_incidentCodes != null && m_incidentCodes.size() > 0) {
            addSchoolAndDateSelection(criteria1, "");
            criteria1.addIn(ConductIncident.COL_INCIDENT_CODE, m_incidentCodes);

            applyInputCriteria(criteria1, false, ConductIncident.REL_STUDENT);

            // Add an OR condition for reportable actions.
            SubQuery actionsQuery = new SubQuery(ConductAction.class, ConductAction.COL_INCIDENT_OID, m_actionCriteria);
            criteria2.addIn(X2BaseBean.COL_OID, actionsQuery);
            criteria2.addOrCriteria(criteria1);
        } else {
            addSetupError("No reportable incident codes found", "Conduct Incident Reference table state codes");
        }
        return criteria2;
    }

    /**
     * Load supporting collections of actions.
     */
    private void loadActions() {
        m_actionCriteria = new X2Criteria();

        if (isSchoolContext()) {
            m_actionCriteria.addEqualTo(ConductAction.REL_INCIDENT + PATH_DELIMITER +
                    ConductIncident.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            m_actionCriteria.addEqualTo(ConductAction.REL_INCIDENT + PATH_DELIMITER +
                    ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
            m_actionCriteria.addEqualTo(ConductAction.REL_INCIDENT + PATH_DELIMITER +
                    ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        }
        // add date selection
        m_actionCriteria.addGreaterOrEqualThan(ConductAction.REL_INCIDENT + PATH_DELIMITER +
                ConductIncident.COL_INCIDENT_DATE,
                getOrganization().getCurrentContext().getStartDate());
        m_actionCriteria.addLessOrEqualThan(ConductAction.REL_INCIDENT + PATH_DELIMITER +
                ConductIncident.COL_INCIDENT_DATE,
                getOrganization().getCurrentContext().getEndDate());

        applyInputCriteria(m_actionCriteria, false,
                ConductAction.REL_INCIDENT + PATH_DELIMITER +
                        ConductIncident.REL_STUDENT);
        BeanQuery actionsQuery = new BeanQuery(ConductAction.class, m_actionCriteria, false);
        actionsQuery.addOrderBy(ConductAction.COL_ACTION_START_DATE, false);
        m_actions = getBroker().getGroupedCollectionByQuery(actionsQuery, ConductAction.COL_INCIDENT_OID, 128);
    }

    /**
     * Loads a map of schools that have been selected to be excluded from state reporting. (exclude
     * from reporting on
     * school table is selected)
     */
    private void loadSchoolExcludeMap() {
        String schoolExclude = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(schoolExclude, BooleanAsStringConverter.TRUE);
        BeanQuery query = new BeanQuery(School.class, schoolCriteria);
        m_excludeSchool = new HashSet<String>();
        for (Object item : getBroker().getCollectionByQuery(query)) {
            School school = (School) item;
            m_excludeSchool.add(school.getSchoolId());
        }
    }

    /**
     * Loads additional offenses assigned to actual inclusions.
     */
    private void loadOtherBehaviors() {
        Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ConductOffense.COL_PRIMARY,
                BooleanAsStringConverter.FALSE);
        criteria.addIn(ConductOffense.COL_INCIDENT_OID,
                getConductIncidentOidSubQuery());
        BeanQuery offenseQuery = new BeanQuery(ConductOffense.class, criteria,
                false);
        m_otherBehaviors = getBroker().getGroupedCollectionByQuery(
                offenseQuery, ConductOffense.COL_INCIDENT_OID, 256);
    }

    /**
     * Loads incident codes and state incident codes attached to them.
     */
    private void loadIncidentCodes() {
        DataDictionaryField incidentField =
                getDataDictionaryField(ConductIncident.class, ConductIncident.COL_INCIDENT_CODE);
        String incidentReferenceTableOid = incidentField.getReferenceTableOid();

        m_stateIncidentCodes = new HashMap<String, String>();

        // Get a list of reportable incident codes.
        if (incidentReferenceTableOid != null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, incidentReferenceTableOid);
            criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
            criteria.addNotEmpty(ReferenceCode.COL_CODE, getBroker().getPersistenceKey());
            BeanQuery query = new BeanQuery(ReferenceCode.class, criteria, false);
            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            while (iterator.hasNext()) {
                ReferenceCode code = (ReferenceCode) iterator.next();
                m_stateIncidentCodes.put(code.getCode(), code.getStateCode());
            }
        }
        m_incidentCodes = m_stateIncidentCodes.keySet();
    }

    /**
     * Sub query which returns incident oids that will be presented in export.
     *
     * @return ReportQueryByCriteria
     */
    private ReportQueryByCriteria getConductIncidentOidSubQuery() {
        Criteria criteria = getDisciplineCriteria();
        ReportQueryByCriteria query = QueryFactory.newReportQuery(
                ConductIncident.class, criteria);
        query.setAttributes(new String[] {X2BaseBean.COL_OID});
        return query;
    }

}
