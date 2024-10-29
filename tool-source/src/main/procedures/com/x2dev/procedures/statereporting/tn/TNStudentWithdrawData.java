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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper.Strategy;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentMultiYearHelper;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class for student extract export.
 */
public class TNStudentWithdrawData extends TNEnrollReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 

    /**
     * Entity class for student extract export.
     *
     */
    public static class TNStudentWithdrawEntity extends TNStateReportEntity implements HasStudentRecordHelper {

        private List<StudentRecordHelper> m_list;

        /**
         * Instantiates a new TN student withdraw entity.
         */
        public TNStudentWithdrawEntity() {
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
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            TNStudentWithdrawData tnData = (TNStudentWithdrawData) data;
            SisStudent student = (SisStudent) getBean();
            List<StudentRecordHelper> list = tnData.getStudentHelperMap().get(student.getOid());
            if (list == null) {
                setRowCount(m_list.size());
            } else {
                m_list = new ArrayList<StudentRecordHelper>(list.size());

                for (StudentRecordHelper item : list) {
                    if (item.getExitDate() != null &&
                    // If context overridden, restrict by end date of school year
                            (!tnData.m_stdMultiYearHelper.isContextOverride() ||
                                    !item.getExitDate().after(tnData.getCurrentContext().getEndDate()))
                            &&

                            // Withdraw codes for which there is no State Code should not be
                            // included in this extract.
                            !StringUtils.isEmpty(item.getWithdrawReason()) &&

                            !StringUtils.isEmpty(item.getEnrollReason()) && item.getIsPrimary()) {
                        m_list.add(item);
                    }
                }

                setRowCount(m_list.size());
            }

            tnData.addEntityRowsCount(getRowCount());
        }

        /**
         * Get the record helper for the current row.
         *
         * @return Student record helper
         */
        @Override
        public StudentRecordHelper getCurrentRecord() {
            return m_list.get(getCurrentRow());
        }

    }

    /**
     * Field retriever for Enrollment fields.
     */
    protected class FieldRetrieverEnrollment implements FieldRetriever {
        protected static final String CALC_PARAM_WITHDRAWDATE = "ENR_WITHDDRAWDATE";
        protected static final String CALC_PARAM_WITHDRAWREASON = "ENR_WITHDRAWREASON";
        protected static final String CALC_PARAM_SCHOOLID = "ENR_SCHOOLID";
        protected static final String ENR_CALC_ID = "ENR_CALC_ENROLL";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentWithdrawEntity seEntity = (TNStudentWithdrawEntity) entity;
            StudentRecordHelper studentData = seEntity.getCurrentRecord();

            String param = (String) field.getParameter();
            Object value = "##Invalid-Param: " + param;

            if (param.equalsIgnoreCase(CALC_PARAM_WITHDRAWDATE)) {
                value = studentData.getExitDate();
            } else if (param.equalsIgnoreCase(CALC_PARAM_WITHDRAWREASON)) {
                value = studentData.getWithdrawReason();
            } else if (param.equalsIgnoreCase(CALC_PARAM_SCHOOLID)) {
                value = studentData.getSchoolId();
            }

            return value;
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
        protected static final String ENRW_CALC_ID = "ENRW_CALC_SSN";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentWithdrawEntity seEntity = (TNStudentWithdrawEntity) entity;
            SisStudent student = (SisStudent) seEntity.getBean();

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
     * Field validator for withdrawal reason as of 2017-18.
     */
    protected class FieldValidatorReason implements FieldValidator {
        protected static final String VAL_ID = "VAL_REASON";

        private Collection<String> m_obsAsOf17_18;

        /**
         * Instantiates a new field validator reason.
         */
        public FieldValidatorReason() {
            m_obsAsOf17_18 = Arrays.asList("14");
        }

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

            if (getCurrentContext().getSchoolYear() > 2017) {
                if (m_obsAsOf17_18.contains(value)) {
                    String error = field.getFieldId() + " codes " + m_obsAsOf17_18 + " are obsolete as of 2017-18";
                    String message = field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                            "school year = " + STYLE_BOLD + getCurrentContext().getSchoolYear() + STYLE_END;
                    errors.add(new StateReportValidationError(entity, field, error, message));
                }
            }

            return errors;
        }

    }

    /**
     * Field validator for SSN.
     * Validates SSN against non valid formats
     * Valid ssn format: 999999999
     */
    protected class FieldValidatorSSN implements FieldValidator {
        protected static final String ENRW_VAL_ID = "ENRW_VAL_SSN";
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
            TNStudentWithdrawData seData = (TNStudentWithdrawData) data;
            return seData.m_schoolYear;
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

            TNStudentWithdrawEntity seEntity = (TNStudentWithdrawEntity) entity;
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

    protected static final String ALIAS_SCHOOL_STATE_ID = "DOE SCHOOL STATE ID";
    private static final String CALC_ID_SCHOOLYEAR = "ENRW_CALC_SCHOOLYEAR";
    private static final String CALC_ID_STUDENT_PIN = "ENRW_CALC_STDNTPIN";

    /**
     * Instance variables.
     *
     */
    protected String m_enrfield;
    protected TNEnrollmentHelper m_enrollmentHelper;
    protected String m_schoolYear;
    protected TNStudentHistoryHelper m_studentHelper;
    protected TNStudentMultiYearHelper m_stdMultiYearHelper;

    /**
     * @see com.x2dev.procedures.statereporting.tn.TNEnrollReportData#initialize()
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

        initializeFields();

        getCalendarsForContextOid(m_contextOid);

        if (getSetupErrors().size() != 0) {
            return;
        }

        X2Criteria criteria = getStudentCriteria();

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);

        applyInputSort(query, null);
        setQuery(query);

        setEntityClass(TNStudentWithdrawEntity.class);

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
        m_studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.TRUE);
        /*
         * m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL,
         * isSchoolContext() ? Boolean.TRUE : Boolean.FALSE);
         */

        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria = m_studentHelper.getStudentCriteria();
        studentCriteria.addAndCriteria(m_stdMultiYearHelper.getWithAttributesCriteria());
        if (isSchoolContext()) {
            m_stdMultiYearHelper.adjustCriteria(studentCriteria, Strategy.EQUAL_TO, SisStudent.COL_SCHOOL_OID,
                    getSchool().getOid());
        }

        return studentCriteria;
    }

    /**
     * Lookup field aliases and paths.
     * 
     */
    private void initializeFields() {

        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        m_schoolYear = getCurentSchoolYear();

        m_enrollmentHelper = new TNEnrollmentHelper(this);
        m_studentHelper = m_enrollmentHelper.getStudentHistoryHelper();
        m_stdMultiYearHelper = m_enrollmentHelper.getStudentMultiYearHelper();
    }

    /**
     * Register custom field Retrievers.
     */
    private void registerFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(FieldRetrieverEnrollment.ENR_CALC_ID, new FieldRetrieverEnrollment());
        calcs.put(RetrieveInstProgramStdBean.TN_CALC_INSTPGM_ID, new RetrieveInstProgramStdBean());
        calcs.put(CALC_ID_SCHOOLYEAR, new RetrieveSchoolYear());
        calcs.put(CALC_ID_STUDENT_PIN, new RetrieveStudentPIN());
        calcs.put(FieldRetrieverSSN.ENRW_CALC_ID, new FieldRetrieverSSN());
        super.addCalcs(calcs);
    }

    /**
     * method for initialization of field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(FieldValidatorSSN.ENRW_VAL_ID, new FieldValidatorSSN());
        validators.put(FieldValidatorReason.VAL_ID, new FieldValidatorReason());

        super.addValidators(validators);
    }
}
