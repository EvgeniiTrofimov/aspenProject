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

package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentEnrollmentSpan;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DecimalAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class for student extract export.
 */
public class TNStudentStandardDayData extends TNStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 

    /**
     * Entity class for student extract export.
     *
     */
    public static class TNStudentStandardDayEntity extends TNStateReportEntity
            implements TNStateReportData.HasStudentRecordHelper {
        private List<KeyValuePair<Integer, StudentRecordHelper>> m_list;

        /**
         * Instantiates a new TN student standard day entity.
         */
        public TNStudentStandardDayEntity() {
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
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] ";

            return name;
        }

        /**
         * Get the record helper for the current row.
         *
         * @return Student record helper
         */
        @Override
        public StudentRecordHelper getCurrentRecord() {
            return m_list.get(getCurrentRow()).getValue();
        }

        /**
         * Get the standard day for the current row.
         *
         * @return Integer
         */
        public Integer getCurrentStandardDay() {
            return m_list.get(getCurrentRow()).getKey();
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            TNStudentStandardDayData tnData = (TNStudentStandardDayData) data;
            SisStudent student = (SisStudent) getBean();

            m_list = new ArrayList();
            List<StudentRecordHelper> list = tnData.getStudentHelperMap().get(student.getOid());
            if (list != null && !list.isEmpty()) {
                KeyValuePair<Integer, StudentRecordHelper> prior = null;
                for (StudentRecordHelper item : list) {
                    KeyValuePair<Integer, StudentRecordHelper> current =
                            new KeyValuePair(getStandardDay(tnData, item), item);
                    if (prior == null ||
                            (prior.getKey() != null && !prior.getKey().equals(current.getKey())) ||
                            (prior.getKey() == null && current.getKey() != null) ||
                            (prior.getValue().getSchoolId() != null
                                    && !prior.getValue().getSchoolId().equals(current.getValue().getSchoolId()))
                            ||
                            (prior.getValue().getSchoolId() == null && current.getValue().getSchoolId() != null) ||
                            (prior.getValue().getInstrProgram() != null
                                    && !prior.getValue().getInstrProgram().equals(current.getValue().getInstrProgram()))
                            ||
                            (prior.getValue().getInstrProgram() == null && current.getValue().getInstrProgram() != null)
                            ||
                            isWithdrawalRecord(tnData, prior.getValue())) {
                        m_list.add(current);
                        prior = current;
                    }
                }
            }

            setRowCount(m_list == null ? 0 : m_list.size());

            tnData.addEntityRowsCount(getRowCount());
        }

        /**
         * Determine if this record ended with a withdrawal.
         *
         * @param tnData TNStudentStandardDayData
         * @param record StudentRecordHelper
         * @return true, if is withdrawal record
         */
        private boolean isWithdrawalRecord(TNStudentStandardDayData tnData, StudentRecordHelper record) {
            if (record.getExitDate() != null) {
                List<TNStudentEnrollmentSpan> spans =
                        tnData.m_studentHelper.getTNStudentEnrollmentSpans((SisStudent) getBean(), true);

                Collections.sort(spans, new Comparator<TNStudentEnrollmentSpan>() {
                    @Override
                    public int compare(TNStudentEnrollmentSpan o1, TNStudentEnrollmentSpan o2) {
                        return o1.getFirstActiveEnrollment().getEnrollmentDate()
                                .compareTo(o2.getFirstActiveEnrollment().getEnrollmentDate());
                    }
                });

                for (TNStudentEnrollmentSpan span : spans) {
                    if (!span.getFirstActiveDate().after(record.getExitDate()) &&
                            (span.getLastActiveDate() == null
                                    || !span.getLastActiveDate().before(record.getExitDate()))) {
                        if (span.getFirstInactiveEnrollment() != null &&
                                StudentEnrollment.WITHDRAWAL
                                        .equals(span.getFirstInactiveEnrollment().getEnrollmentType())) {
                            return true;
                        }
                    }
                }
            }
            return false;
        }

        /**
         * Calculate the standard day value for the student record .
         *
         * @param data TNStudentStandardDayData
         * @param record StudentRecordHelper
         * @return Integer
         */
        private Integer getStandardDay(TNStudentStandardDayData data, StudentRecordHelper record) {
            SisStudent student = (SisStudent) getBean();
            PlainDate startDate = record.getEnrollDate();
            PlainDate endDate = record.getExitDate();
            if (endDate == null) {
                endDate = data.getCurrentContext().getEndDate();
            }

            ReferenceCode code = data.m_studentHelper.getGradeLevelByDates(student, startDate, endDate);

            if (code != null) {
                BigDecimal value = null;
                try {
                    String standardDayString =
                            data.getStudentMultiYearHelper().getHistoryValueByAlias(code, ALIAS_HISTORY_STANDARD_DAY,
                                    ALIAS_STANDARD_DAY);
                    value = ((BigDecimal) data.m_decimalConverter.parseSystemString(standardDayString));
                } catch (Exception e) {
                    // Use null if conversion error occurs
                }
                if (value != null) {
                    int minutes = value.intValue();
                    return Integer.valueOf(minutes);
                }
            }
            return null;
        }

    }

    /**
     *
     * Field retriever for SSN.
     * This retriever normalize SSN value to format 999999999 (nine digits without any other
     * characters)
     *
     */
    protected class FieldRetrieverSSN implements FieldRetriever {
        protected static final String STDD_CALC_ID = "STDD_CALC_SSN";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentStandardDayEntity seEntity = (TNStudentStandardDayEntity) entity;

            SisStudent student = (SisStudent) seEntity.getBean();
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
        protected static final String STDD_VAL_ID = "STDD_VAL_SSN";
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
     * Field retriever for Effective Date field - based on grade association.
     */
    protected class RetrieveEffectiveDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentStandardDayEntity seEntity = (TNStudentStandardDayEntity) entity;
            StudentRecordHelper studentData = seEntity.getCurrentRecord();

            return studentData.getEnrollDate();
        }
    }

    /**
     * Field retriever for School Year field.
     */
    protected class RetrieveSchoolId implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentStandardDayEntity seEntity = (TNStudentStandardDayEntity) entity;
            StudentRecordHelper studentData = seEntity.getCurrentRecord();

            return studentData.getSchoolId();
        }

    }

    /**
     * Field retriever for School Year field.
     */
    protected class RetrieveSchoolYear implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentStandardDayData seData = (TNStudentStandardDayData) data;
            return seData.m_schoolYear;
        }

    }

    /**
     * Field retriever for Standard Day field.
     */
    protected class RetrieveStandardDay implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentStandardDayEntity seEntity = (TNStudentStandardDayEntity) entity;
            return seEntity.getCurrentStandardDay();
        }
    }

    /**
     * Field retriever for Student PIN.
     */
    protected class RetrieveStudentPIN implements FieldRetriever {
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

            TNStudentStandardDayEntity seEntity = (TNStudentStandardDayEntity) entity;
            SisStudent student = (SisStudent) seEntity.getBean();
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
     * Constants: Aliases, Fields, IDs, Parameters
     *
     */
    protected static final String ALIAS_HISTORY_STANDARD_DAY = "all-rcd-StdStandardDayHistory";
    protected static final String ALIAS_STANDARD_DAY = "DOE STUDENT STANDARD DAY";
    protected static final String ALIAS_STUDENT_DAY_LENGTH = "DOE STUDENT DAY LENGTH";

    private static final String CALC_ID_EFFECTDAY = "STDD_CALC_EFFECTDAY";
    private static final String CALC_ID_SCHOOL = "STDD_CALC_SCHOOL";
    private static final String CALC_ID_SCHOOLYEAR = "STDD_CALC_SCHOOLYEAR";
    private static final String CALC_ID_STDDAY = "STDD_CALC_STDDAY";
    private static final String CALC_ID_STUDENT_PIN = "STDD_CALC_PIN";

    /**
     * Instance variables.
     *
     */
    protected DecimalAsStringConverter m_decimalConverter;
    protected String m_schoolYear;
    protected TNStudentHistoryHelper m_studentHelper;
    protected String m_fieldDayLength;

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

        setOnFirstDayWithdrew(true);

        initializeFields();

        getCalendarsForContextOid(m_contextOid);

        if (getSetupErrors().size() != 0) {
            return;
        }

        QueryByCriteria query = initializeCriteriaAndQuery();
        setQuery(query);

        setEntityClass(TNStudentStandardDayEntity.class);

        initStudentHelperMap(m_studentHelper, query);

        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Method for implementing business rule for schoolYear
     * (CTX_SCHOOL_YEAR - 1) where reporting date falls within `CTX_START_DATE` and `CTX_END_DATE`.
     * else CTX_SCHOOL_YEAR
     *
     * @return string representation of school year
     */
    private String getCurentSchoolYear() {
        return Integer.toString(getCurrentContext().getSchoolYear() - 1);
    }

    /**
     * Function for building custom Student criteria.
     *
     * @return criteria for query for list of active students
     *         limited by reportDate, school and not excluded students
     */
    private X2Criteria getStudentCriteria() {
        return m_studentHelper.getStudentCriteria();
    }

    /**
     * Initialize criteria and query.
     *
     * @return QueryByCriteria
     */
    private QueryByCriteria initializeCriteriaAndQuery() {
        X2Criteria criteria = getStudentCriteria();
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);

        applyInputSort(query, null);
        return query;
    }

    /**
     * Lookup field aliases and paths.
     *
     */
    private void initializeFields() {
        m_decimalConverter = (DecimalAsStringConverter) ConverterFactory
                .getConverterForClass(Converter.BIG_DECIMAL_CONVERTER, null, true);
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        TNEnrollmentHelper helper = new TNEnrollmentHelper(this);
        m_studentHelper = helper.getStudentHistoryHelper();
        m_studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.TRUE);

        m_schoolYear = getCurentSchoolYear();
        m_fieldDayLength = translateAliasToJavaName(ALIAS_STUDENT_DAY_LENGTH, true);
    }

    /**
     * Register custom field Retrievers.
     */
    private void registerFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveInstProgramStdBean.TN_CALC_INSTPGM_ID, new RetrieveInstProgramStdBean());
        calcs.put(CALC_ID_SCHOOL, new RetrieveSchoolId());
        calcs.put(CALC_ID_SCHOOLYEAR, new RetrieveSchoolYear());
        calcs.put(CALC_ID_STDDAY, new RetrieveStandardDay());
        calcs.put(CALC_ID_EFFECTDAY, new RetrieveEffectiveDate());
        calcs.put(CALC_ID_STUDENT_PIN, new RetrieveStudentPIN());
        calcs.put(FieldRetrieverSSN.STDD_CALC_ID, new FieldRetrieverSSN());
        super.addCalcs(calcs);
    }

    /**
     * Register custom field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(FiledValidatorSSN.STDD_VAL_ID, new FiledValidatorSSN());
        super.addValidators(validators);
    }
}
