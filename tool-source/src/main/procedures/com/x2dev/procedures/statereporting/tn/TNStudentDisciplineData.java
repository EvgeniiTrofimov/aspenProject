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

package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.DateRange;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Class for student extract export.
 */
public class TNStudentDisciplineData extends TNStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Entity class for student extract export.
     *
     */
    public static class TNStudentDisciplineEntity extends TNStateReportEntity {
        private ArrayList<DateRange> m_actionDates = new ArrayList<DateRange>();

        /**
         * Instantiates a new TN student discipline entity.
         */
        public TNStudentDisciplineEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            ConductAction action = (ConductAction) getBean();
            ConductIncident incident = action.getIncident();
            SisStudent student = incident.getStudent();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] " +
                    "[ID: " + incident.getIncidentId() + ", CODE: " + incident.getIncidentCode() + "]";

            return name;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            TNStudentDisciplineData tnData = (TNStudentDisciplineData) data;

            splitYearLongActions(tnData, (ConductAction) bean);

            setRowCount(m_actionDates.size());

            tnData.addEntityRowsCount(getRowCount());
        }

        /**
         * if action start in previous year or ends in next
         * we need to "split" action for two.
         *
         * @param tnData TNStudentDisciplineData
         * @param originAction ConductAction
         */
        private void splitYearLongActions(TNStudentDisciplineData tnData, ConductAction originAction) {
            PlainDate actionStartDate = originAction.getActionStartDate();
            PlainDate actionEndDate = originAction.getActionEndDate();

            int currentYear = tnData.getCurrentContext().getSchoolYear();

            String previousYearKey = (currentYear - 1) + originAction.getSchoolOid() +
                    originAction.getStudent().getCalendarCode();
            DateRange previousYearDates = tnData.m_schoolDateRanges.get(previousYearKey);

            if (actionStartDate != null && previousYearDates != null &&
                    actionStartDate.before(previousYearDates.getUpperPlainDate())) {
                m_actionDates.add(new DateRange(actionStartDate, previousYearDates.getUpperPlainDate()));
            }

            String nextYearKey = (currentYear + 1) + originAction.getSchoolOid() +
                    originAction.getStudent().getCalendarCode();
            DateRange nextYearDates = tnData.m_schoolDateRanges.get(nextYearKey);

            if (actionEndDate != null && nextYearDates != null
                    && actionEndDate.after(nextYearDates.getLowerPlainDate())) {
                m_actionDates.add(new DateRange(nextYearDates.getLowerPlainDate(), actionEndDate));
            }

            String currentYearKey =
                    currentYear + originAction.getSchoolOid() + originAction.getStudent().getCalendarCode();
            DateRange currentYearDates = tnData.m_schoolDateRanges.get(currentYearKey);

            if ((actionStartDate == null && currentYearDates != null) ||
                    (actionStartDate != null && currentYearDates != null
                            && actionStartDate.before(currentYearDates.getLowerPlainDate()))) {
                actionStartDate = currentYearDates.getLowerPlainDate();
            }
            if ((actionEndDate == null && currentYearDates != null) ||
                    (actionEndDate != null && currentYearDates != null
                            && actionEndDate.after(currentYearDates.getUpperPlainDate()))) {
                actionEndDate = currentYearDates.getUpperPlainDate();
            }
            if (actionStartDate != null && actionEndDate != null) {
                m_actionDates.add(new DateRange(actionStartDate, actionEndDate));
            }
        }

        /**
         * Gets the action start date.
         *
         * @return Plain date
         */
        public PlainDate getActionStartDate() {
            return new PlainDate(m_actionDates.get(getCurrentRow()).getLowerPlainDate().getTime());
        }

        /**
         * Gets the action end date.
         *
         * @return Plain date
         */
        public PlainDate getActionEndDate() {
            return new PlainDate(m_actionDates.get(getCurrentRow()).getUpperPlainDate().getTime());
        }
    }

    /**
     *
     * Field retriever for SSN.
     * This retriever normalizes SSN value to format 999999999 (nine digits without any other
     * characters)
     *
     */
    protected class FieldRetrieverSSN implements FieldRetriever {
        protected static final String CND_CALC_ID = "CND_CALC_SSN";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentDisciplineEntity seEntity = (TNStudentDisciplineEntity) entity;
            ConductAction action = (ConductAction) seEntity.getBean();

            if (action == null) {
                return "";
            }

            SisStudent student = action.getStudent();

            if (student == null) {
                return "";
            }

            Person psn = student.getPerson();
            if (psn == null) {
                return "";
            }

            String ssn = psn.getPersonId();
            if (StringUtils.isEmpty(ssn)) {
                return "";
            }

            return ssn.replaceAll("([^\\d]?)", "");
        }

    }

    /**
     * Field validator for SSN.
     * Validates SSN against non valid formats
     * Valid ssn format: 999999999
     */
    protected class FiledValidatorSSN implements FieldValidator {
        protected static final String CND_VAL_ID = "CND_VAL_SSN";
        private static final String patternSSN = "^[0-9]{9}$|^$";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (value != null && !value.matches(patternSSN)) {
                errors.add(new StateReportValidationError(entity, field, "Invalid format",
                        "SSN value must be 9 digits long"));
            }

            return errors;
        }

    }

    /**
     * Field retriever for action start and end dates.
     */
    protected class RetrieveActionDates implements FieldRetriever {
        public static final String CALC_ID = "CND_CALC_DATES";

        private static final String CALC_PARAM_START_DATE = "START_DATE";
        private static final String CALC_PARAM_END_DATE = "END_DATE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentDisciplineEntity tnEntity = (TNStudentDisciplineEntity) entity;
            String param = (String) field.getParameter();
            if (CALC_PARAM_START_DATE.equalsIgnoreCase(param)) {
                return tnEntity.getActionStartDate();
            } else if (CALC_PARAM_END_DATE.equalsIgnoreCase(param)) {
                return tnEntity.getActionEndDate();
            }
            return null;
        }

    }

    /**
     * Field retriever for Instructional program and School Year fields.
     */
    protected class RetrieveDefault implements FieldRetriever {
        public static final String CALC_ID_DEFAULT = "CND_CALC_DEFAULT";

        protected static final String CALC_PARAM_SCHOOLYEAR = "SCHOOLYEAR";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            TNStudentDisciplineData seData = (TNStudentDisciplineData) data;

            String param = (String) field.getParameter();

            if (param.equalsIgnoreCase(CALC_PARAM_SCHOOLYEAR)) {
                return seData.m_schoolYear;
            }

            return "";
        }
    }

    /**
     * Field retriever for Instructional program field.
     * Can be used only with SisStudent beans.
     */
    protected class RetrieveInstProgram implements FieldRetriever {
        public static final String TN_CALC_INSTPGM_ID = "TN_INSTRPGM_ACT";

        private static final String INSTPGM_EMPTY_ERROR = "Instructional Program is empty.";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            TNStateReportData tnData = (TNStateReportData) data;

            ConductAction action = (ConductAction) entity.getBean();
            SisStudent student = action.getStudent();
            SisSchool school = action.getSchool();
            String message = null;

            StudentRecordHelper record = null;
            if (action.getActionStartDate() != null) {
                record = getRecordByDate(student.getOid(), action.getActionStartDate(),
                        (String) action.getSchool().getFieldValueByBeanPath(m_fieldStateSchoolId));
            }
            if (record == null && action.getActionEndDate() != null) {
                record = getRecordByDate(student.getOid(), action.getActionEndDate(),
                        (String) action.getSchool().getFieldValueByBeanPath(m_fieldStateSchoolId));
            }

            String instrPgm = null;

            if (record == null) {
                String calendarCode = !StringUtils.isEmpty(student.getCalendarCode()) ? student.getCalendarCode()
                        : CALENDAR_ID_STANDARD;

                instrPgm = field.getDefaultValue();

                message = "Value not specified. Set to " + field.getDefaultValue() + " by default. Context:" +
                        tnData.getCurrentContext().getContextId() + ". School:" + student.getSchool().getName() +
                        ". Calendar code:" + calendarCode;
            } else if (StringUtils.isEmpty(instrPgm = record.getInstrProgram())) {
                instrPgm = field.getDefaultValue();
                message = "Value not specified. Set to " + field.getDefaultValue() + " by default. Context:" +
                        tnData.getCurrentContext().getContextId() + ". School:" + school.getName() +
                        ". Exit date: " + record.getExitDate();
            }

            if (!StringUtils.isEmpty(message)) {
                StateReportValidationError error =
                        new StateReportValidationError(entity, field, INSTPGM_EMPTY_ERROR, message);
                entity.addRetrievalError(field.getFieldId(), error);
            }

            return instrPgm;
        }
    }

    /**
     * Field retriever for Student PIN.
     */
    protected class RetrieveStudentPIN implements FieldRetriever {
        public static final String CALC_ID_STUDENT_PIN = "CND_CALC_STUDENT_PIN";

        private static final String ERROR_MESSAGE_IS_NUMERIC = "PIN must be numeric only";

        private static final String ERROR_MISSED_IS_NUMERIC = ERROR_MESSAGE_IS_NUMERIC;
        private static final String FIELD_ALIAS_STUDENT_PIN = "DOE PIN";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Integer value = null;

            TNStudentDisciplineEntity seEntity = (TNStudentDisciplineEntity) entity;
            ConductAction action = (ConductAction) seEntity.getBean();
            SisStudent student = action.getStudent();
            String studentPINvalue = (String) student.getFieldValueByAlias(FIELD_ALIAS_STUDENT_PIN);
            if (studentPINvalue != null) {
                if (!isNumeric(studentPINvalue)) {
                    seEntity.addRetrievalError(field.getFieldId(),
                            new StateReportValidationError(entity, field, ERROR_MISSED_IS_NUMERIC,
                                    ERROR_MESSAGE_IS_NUMERIC));
                    value = Integer.valueOf(0);
                } else {
                    value = Integer.valueOf(Integer.parseInt(studentPINvalue));
                }
            }

            return value;
        }

        /**
         * Check that argument contains numbers.
         *
         * @param value String
         * @return true, if is numeric
         */
        protected boolean isNumeric(String value) {
            return value.matches("-?\\d+(\\.\\d+)?");
        }
    }

    /**
     * Student PIN and Student SSN validation.
     */
    protected class ValidateActionDates implements FieldValidator {
        private static final String ERROR_BEGIN_END_DATE_VALIDATION =
                "Disciplinary Begin date is not before Disciplinary End Date";
        private static final String ERROR_BEGIN_OFFENCE_DATE_VALIDATION =
                "Disciplinary Begin date is not after Disciplinary Offense Date";

        private static final String ERROR_MESSAGE_BEGIN_END_DATE_VALIDATION = ERROR_BEGIN_END_DATE_VALIDATION;
        private static final String ERROR_MESSAGE_BEGIN_OFFENCE_DATE_VALIDATION = ERROR_BEGIN_OFFENCE_DATE_VALIDATION;

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
            TNStudentDisciplineEntity cndEntity = (TNStudentDisciplineEntity) entity;
            ConductAction action = (ConductAction) cndEntity.getBean();
            if (action != null && action.getActionStartDate() != null && action.getActionEndDate() != null) {
                if (action.getActionStartDate().after(action.getActionEndDate())) {
                    errors.add(new StateReportValidationError(entity, field, ERROR_BEGIN_END_DATE_VALIDATION,
                            ERROR_MESSAGE_BEGIN_END_DATE_VALIDATION));
                }
            }

            if (action != null && action.getActionStartDate() != null && action.getIncident() != null &&
                    action.getIncident().getIncidentDate() != null) {
                if (action.getActionStartDate().before(action.getIncident().getIncidentDate())) {
                    errors.add(new StateReportValidationError(entity, field, ERROR_BEGIN_OFFENCE_DATE_VALIDATION,
                            ERROR_MESSAGE_BEGIN_OFFENCE_DATE_VALIDATION));
                }
            }

            return errors;
        }
    }

    /**
     * Aliases.
     */
    private static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL STATE ID";

    /**
     * User input parameters
     *
     */
    private static final String PARAM_END_DATE = "endDate";
    private static final String PARAM_START_DATE = "startDate";
    private static final String PARAM_USE_DATE_RANGE = "useReportDateRange";

    private static final String STATE_CODE000 = "000";
    private static final String STATE_CODE999 = "999";

    private static final String VAL_ID_ACT_DATES = "VAL_ID_ACT_DATES";


    /**
     * Instance variables.
     *
     */
    protected Date m_endDate;
    protected String m_schoolYear;
    protected Map<String, DateRange> m_schoolDateRanges;
    protected String m_fieldSklStateId;
    protected Date m_startDate;
    protected TNStudentHistoryHelper m_studentHelper;
    protected Boolean m_useReportDateRange;

    /**
     * @see com.x2dev.procedures.statereporting.tn.TNStateReportData#initialize()
     */
    /*
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     * com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();
        // insure that all aliases and any other resources needed to operate the
        // report are available in the database.
        // setup error is created if the alias is not found.
        initializeFields();

        getCalendarsForContextOid(m_contextOid);

        if (getSetupErrors().size() != 0) {
            return;
        }

        initializeCriteriaAndQuery();
        defineSchoolsFirstAndLastDays();
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, m_studentHelper.getStudentCriteria());
        initStudentHelperMap(m_studentHelper, query);

        // Add any necessary FieldRetrievers an Field Validators
        initFieldRetrievers();
        initFieldValidators();
    }

    /**
     * Return INSTR PROGRAM based on StudentRecordHelpers.
     *
     * @param studentOid String
     * @param date Date
     * @param schoolId String
     * @return Student record helper
     */
    protected StudentRecordHelper getRecordByDate(String studentOid, Date date, String schoolId) {
        StudentRecordHelper record = null;
        List<StudentRecordHelper> records = getStudentHelperMap().get(studentOid);
        if (records != null) {
            for (StudentRecordHelper curRecord : records) {
                if (!date.before(curRecord.getEnrollDate())
                        && (curRecord.getExitDate() == null || !date.after(curRecord.getExitDate()))
                        && schoolId.equals(curRecord.getSchoolId())) {
                    record = curRecord;
                }
            }
            // if there is no intersected record, use most recent record for date.
            if (record == null) {
                for (StudentRecordHelper curRecord : records) {
                    if (curRecord.getExitDate() != null && curRecord.getExitDate().before(date) &&
                            (record == null || record.getExitDate().before(curRecord.getExitDate()))) {
                        record = curRecord;
                    }
                }
            }
        }
        return record;
    }

    /**
     * Define schools first and last days.
     */
    private void defineSchoolsFirstAndLastDays() {
        m_schoolDateRanges = new HashMap<String, DateRange>();

        X2Criteria calendarCriteria = new X2Criteria();
        if (isSchoolContext()) {
            calendarCriteria.addEqualTo(SchoolCalendar.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            calendarCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                    School.COL_ARCHIVE_INDICATOR, BooleanAsStringConverter.TRUE);
            calendarCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                    School.COL_INACTIVE_INDICATOR, BooleanAsStringConverter.TRUE);
        }
        calendarCriteria.addNotEmpty(SchoolCalendar.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                School.COL_SCHOOL_ID, getBroker().getPersistenceKey());
        calendarCriteria.addNotEmpty(SchoolCalendar.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                m_fieldSklStateId, getBroker().getPersistenceKey());
        calendarCriteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_contextOid);

        QueryIterator calendars =
                getBroker().getIteratorByQuery(new QueryByCriteria(SchoolCalendar.class, calendarCriteria));
        try {
            while (calendars.hasNext()) {
                SchoolCalendar calendar = (SchoolCalendar) calendars.next();

                X2Criteria datesCriteria = new X2Criteria();
                datesCriteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, BooleanAsStringConverter.TRUE);
                datesCriteria.addEqualTo(SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID, calendar.getOid());

                QueryByCriteria query = new QueryByCriteria(SchoolCalendarDate.class, datesCriteria);
                query.addOrderByAscending(SchoolCalendarDate.COL_DATE);

                List<SchoolCalendarDate> dates = (List<SchoolCalendarDate>) getBroker().getCollectionByQuery(query);
                if (dates.isEmpty()) {
                    this.addSetupError("In-Session Calendar Dates Not Found",
                            "School: " + calendar.getSchool().getName() + " Calendar Name: " + calendar.getCalendarId()
                                    + " criteria: " + datesCriteria.toString());
                } else {
                    PlainDate calendarStartDate = dates.get(0).getDate();
                    PlainDate calendarEndDate = dates.get(dates.size() - 1).getDate();

                    String key = calendar.getDistrictContext().getSchoolYear() + calendar.getSchoolOid() +
                            calendar.getCalendarId();
                    m_schoolDateRanges.put(key, new DateRange(calendarStartDate, calendarEndDate));
                }
            }
        } finally {
            calendars.close();
        }

    }

    /**
     * Action criteria to select included actions.
     *
     * @return Criteria
     */
    private Criteria getActionsCriteria() {
        X2Criteria incidentCriteria = getIncidentCriteria();

        X2Criteria actionCriteria = new X2Criteria();
        actionCriteria.addIn(ConductAction.REL_INCIDENT + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                new SubQuery(ConductIncident.class, X2BaseBean.COL_OID, incidentCriteria));

        Collection actionCodes = getActionStateReportableCodes();
        if (!actionCodes.isEmpty()) {
            actionCriteria.addIn(ConductAction.COL_ACTION_CODE, actionCodes);
        }
        if (m_startDate != null) {
            actionCriteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_END_DATE, m_startDate);
        }

        if (m_endDate != null) {
            actionCriteria.addLessOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_endDate);
        }

        return actionCriteria;
    }

    /**
     * Method for implementing business rule for schoolYear.
     *
     * @return string representation of school year
     */
    private String getCurrentSchoolYear() {
        return Integer.toString(getCurrentContext().getSchoolYear() - 1);
    }

    /**
     * Incident Criteria used to get corresponding actions.
     *
     * @return X 2 criteria
     */
    private X2Criteria getIncidentCriteria() {
        X2Criteria incidentCriteria = new X2Criteria();

        applyInputCriteria(incidentCriteria, true, ConductIncident.REL_STUDENT);

        incidentCriteria.addIn(ConductIncident.COL_INCIDENT_CODE, getIncidentStateReportableCodes());

        incidentCriteria.addNotNull(ConductIncident.COL_STUDENT_OID);

        return incidentCriteria;
    }

    /**
     * Get the list of reportable state codes.
     *
     * @return Collection
     */
    private Collection<String> getIncidentStateReportableCodes() {
        Set<String> codes = new HashSet<String>();
        DataDictionaryField field = getDataDictionaryField(ConductIncident.class, ConductIncident.COL_INCIDENT_CODE);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        criteria.addBetween(ReferenceCode.COL_STATE_CODE, STATE_CODE000, STATE_CODE999);

        String[] columns = new String[] {ReferenceCode.COL_CODE};
        ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] record = (Object[]) iterator.next();
                String code = (String) record[0];
                codes.add(code);
            }
        } finally {
            iterator.close();
        }
        return codes;
    }

    /**
     * Gets the action state reportable codes.
     *
     * @return Collection
     */
    private Collection<String> getActionStateReportableCodes() {
        Set<String> codes = new HashSet<String>();
        DataDictionaryField field = getDataDictionaryField(ConductAction.class, ConductAction.COL_ACTION_CODE);
        if (field != null && field.hasReferenceTable()) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
            criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());

            Collection refCodes = getBroker().getCollectionByQuery(new QueryByCriteria(ReferenceCode.class, criteria));
            codes.addAll(CollectionUtils.getPropertyCollection(refCodes, ReferenceCode.COL_CODE));
        }
        return codes;
    }

    /**
     * Register custom field Retrievers.
     */
    private void initFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveDefault.CALC_ID_DEFAULT, new RetrieveDefault());
        calcs.put(RetrieveStudentPIN.CALC_ID_STUDENT_PIN, new RetrieveStudentPIN());
        calcs.put(FieldRetrieverSSN.CND_CALC_ID, new FieldRetrieverSSN());
        calcs.put(RetrieveInstProgram.TN_CALC_INSTPGM_ID, new RetrieveInstProgram());
        calcs.put(RetrieveActionDates.CALC_ID, new RetrieveActionDates());
        super.addCalcs(calcs);
    }

    /**
     * Inits the field validators.
     */
    private void initFieldValidators() {
        // custom validators
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(VAL_ID_ACT_DATES, new ValidateActionDates());
        validators.put(FiledValidatorSSN.CND_VAL_ID, new FiledValidatorSSN());
        super.addValidators(validators);
    }

    /**
     * Initialize criteria and query.
     */
    private void initializeCriteriaAndQuery() {
        Criteria actionCriteria = getActionsCriteria();
        QueryByCriteria actionQuery = new QueryByCriteria(ConductAction.class, actionCriteria, true);
        setQuery(actionQuery);

        setEntityClass(TNStudentDisciplineEntity.class);
    }

    /**
     * Lookup field aliases and paths.
     *
     */
    private void initializeFields() {
        m_schoolYear = getCurrentSchoolYear();

        m_useReportDateRange = (Boolean) getParameter(PARAM_USE_DATE_RANGE);

        if (m_useReportDateRange.booleanValue()) {
            m_endDate = (Date) getParameter(PARAM_END_DATE);
            m_startDate = (Date) getParameter(PARAM_START_DATE);
        } else {
            m_startDate = getCurrentContext().getStartDate();
            m_endDate = getCurrentContext().getEndDate();
        }

        TNEnrollmentHelper helper = new TNEnrollmentHelper(this);
        m_studentHelper = helper.getStudentHistoryHelper();
        m_studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);
        m_fieldSklStateId = translateAliasToJavaName(ALIAS_SKL_STATE_ID, true);

        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveStudentContextAttribute.CALC_ID, new RetrieveStudentContextAttribute(helper));
        super.addCalcs(calcs);
    }
}
