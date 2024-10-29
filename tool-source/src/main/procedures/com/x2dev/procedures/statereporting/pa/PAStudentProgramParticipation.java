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

package com.x2dev.procedures.statereporting.pa;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
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
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class for student program participation export.
 */
public class PAStudentProgramParticipation extends StateReportData {

    /**
     * Entity class for student program participation export.
     *
     */
    public static class PAStudentProgramParticipationEntity extends StateReportEntity {

        /**
         * Helper class to manage reportable entities.
         */
        public class ProgramInterval {
            private PlainDate m_beginDate;
            private PlainDate m_endDate;
            private String m_school;

            /**
             * Instantiates a new program interval.
             *
             * @param beginDate PlainDate
             * @param endDate PlainDate
             * @param school String
             */
            public ProgramInterval(PlainDate beginDate, PlainDate endDate, String school) {
                this.m_beginDate = beginDate;
                this.m_endDate = endDate;
                this.m_school = school;
            }

            /**
             * Getter.
             *
             * @return m_beginDate
             */
            public PlainDate getBeginDate() {
                return m_beginDate;
            }

            /**
             * Getter.
             *
             * @return m_endDate
             */
            public PlainDate getEndDate() {
                return m_endDate;
            }

            /**
             * Getter.
             *
             * @return m_school
             */
            public String getSchool() {
                return m_school;
            }
        }

        private StudentProgramParticipation m_bean;
        private PAStudentProgramParticipation m_data;
        private List<ProgramInterval> m_intervals;

        /**
         * Default public constructor.
         */
        public PAStudentProgramParticipationEntity() {}

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            StudentProgramParticipation spp = (StudentProgramParticipation) getBean();
            return spp.getStudent().getNameView()
                    + "(" + spp.getStudent().getLocalId() + ")"
                    + " [ProgramCode: " + spp.getProgramCode()
                    + ", Start Date: " + spp.getStartDate()
                    + ", End Date:" + spp.getEndDate()
                    + "] ";
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
            m_bean = (StudentProgramParticipation) bean;
            m_data = (PAStudentProgramParticipation) data;
            StudentHistoryHelper helper = m_data.m_studentHelper;
            PlainDate programBeginDate = getProgramBeginDate();
            PlainDate programEndDate = getProgramEndDate();
            helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, programBeginDate);
            helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, programEndDate);
            List<StudentEnrollmentSpan> spans =
                    m_data.m_studentHelper.getStudentEnrollmentSpans(m_bean.getStudent(), true);
            m_intervals = new ArrayList<ProgramInterval>(spans.size());
            for (StudentEnrollmentSpan span : spans) {
                PlainDate beginDate = programBeginDate;
                PlainDate endDate = programEndDate;
                if (span.getFirstActiveEnrollment().getEnrollmentDate().after(programBeginDate)) {
                    beginDate = span.getFirstActiveEnrollment().getEnrollmentDate();
                }
                if (span.getFirstInactiveEnrollment() != null &&
                        ((endDate == null
                                && m_data.m_reportDate.after(span.getFirstInactiveEnrollment().getEnrollmentDate()))
                                || (endDate != null
                                        && endDate.after(span.getFirstInactiveEnrollment().getEnrollmentDate())))) {
                    endDate = span.getFirstInactiveEnrollment().getEnrollmentDate();
                }
                m_intervals.add(new ProgramInterval(beginDate, endDate,
                        (String) span.getSchool().getFieldValueByBeanPath(m_data.m_fieldSchoolStateId)));
            }
            setRowCount(m_intervals.size());
        }

        /**
         * Gets end date for the current interval.
         *
         * @return Plain date
         */
        PlainDate getCurrentBeginDate() {
            return m_intervals.get(getCurrentRow()).getBeginDate();
        }

        /**
         * Gets begin date for the current interval.
         *
         * @return Plain date
         */
        PlainDate getCurrentEndDate() {
            return m_intervals.get(getCurrentRow()).getEndDate();
        }

        /**
         * Gets school for the current interval.
         *
         * @return String
         */
        String getCurrentSchool() {
            return m_intervals.get(getCurrentRow()).getSchool();
        }

        /**
         * Gets begin date for program.
         *
         * @return PlainDate
         */
        private PlainDate getProgramBeginDate() {
            DistrictSchoolYearContext ctx = m_data.getOrganization().getCurrentContext();
            if (ctx.getStartDate() == null) {
                return m_bean.getStartDate();
            }
            return m_bean.getStartDate().before(ctx.getStartDate()) ? ctx.getStartDate() : m_bean.getStartDate();
        }

        /**
         * Gets end date for program.
         *
         * @return PlainDate
         */
        private PlainDate getProgramEndDate() {
            DistrictSchoolYearContext ctx = m_data.getOrganization().getCurrentContext();
            if (REPORT_TYPE_LAST.equals(m_data.m_reportType)) {
                return m_bean.getEndDate() == null || m_bean.getEndDate().after(ctx.getEndDate()) ? ctx.getEndDate()
                        : m_bean.getEndDate();
            }

            return m_bean.getEndDate() == null || m_bean.getEndDate().after(m_data.m_reportDate) ? null
                    : m_bean.getEndDate();
        }
    }

    /**
     * Field retriever for date fields.
     */
    protected class FieldRetrieverProgramParticipation implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            PAStudentProgramParticipationEntity sppEntity = (PAStudentProgramParticipationEntity) entity;
            String param = (String) field.getParameter();
            if (param.equalsIgnoreCase(CALC_PARAM_PGM_BEGINDATE)) {
                return sppEntity.getCurrentBeginDate();
            } else if (param.equalsIgnoreCase(CALC_PARAM_PGM_ENDDATE)) {
                return sppEntity.getCurrentEndDate();
            } else if (param.equalsIgnoreCase(CALC_PARAM_PGM_SCHOOL)) {
                return sppEntity.getCurrentSchool();
            }
            return "##Invalid-PGM-Param: " + param;
        }
    }

    /**
     * Validator for BEGINNING DATE field. It does two things.
     * 1. Validates correctness of field format
     * 2. Validates if field value is not before this school year start date
     */
    protected class ValidatorBeginDate implements FieldValidator {
        private static final String FIELD_BEGIN_DATE = "BEGINNING DATE";

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
            Date programBeginDate = null;
            try {
                String fieldValue = entity.getFieldValue(FIELD_BEGIN_DATE);
                SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                programBeginDate = sdf.parse(fieldValue);
            } catch (ParseException ex) {
                errors.add(new StateReportValidationError(entity, field, "Bad format",
                        "Field is in not valid yyyy-MM-dd format"));
                return errors;
            }
            PAStudentProgramParticipation sppData = (PAStudentProgramParticipation) data;
            int diff = programBeginDate.compareTo(sppData.m_contextStartDate);
            if (diff < 0) {
                errors.add(new StateReportValidationError(entity, field, "Value is out of range",
                        "Program begin date is before this year startDate"));
            }
            return errors;
        }
    }

    /**
     * Validator for ENDING DATE field. It does two things.
     * 1. Validates correctness of field format
     * 2. Validates if field value is not before program begin date
     */
    protected class ValidatorEndDate implements FieldValidator {
        private static final String FIELD_BEGIN_DATE = "BEGINNING DATE";
        private static final String FIELD_END_DATE = "ENDING DATE";

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
            String fieldValue = entity.getFieldValue(FIELD_END_DATE);
            if (fieldValue == null || fieldValue.isEmpty()) {
                // End date is not set. Exit from validator
                return errors;
            }
            Date programBeginDate = parseAndValidateDateField(entity, field, FIELD_BEGIN_DATE, errors);
            Date programEndDate = parseAndValidateDateField(entity, field, FIELD_END_DATE, errors);
            if (programBeginDate == null || programEndDate == null) {
                return errors;
            }
            int diff = programBeginDate.compareTo(programEndDate);
            if (diff > 0) {
                errors.add(new StateReportValidationError(entity, field, "Value is out of range",
                        "Program end date is before than program begin date"));
            }
            return errors;
        }

        /**
         * Parses the and validate date field.
         *
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param fieldName String
         * @param errors Collection<StateReportValidationError>
         * @return Date
         */
        private Date parseAndValidateDateField(StateReportEntity entity,
                                               FieldDefinition field,
                                               String fieldName,
                                               Collection<StateReportValidationError> errors) {
            Date date_ = null;
            try {
                String fieldValue = entity.getFieldValue(fieldName);
                SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                date_ = sdf.parse(fieldValue);
            } catch (ParseException ex) {
                errors.add(new StateReportValidationError(entity, field, "Bad format", "Field " + fieldName +
                        " is in not valid yyyy-MM-dd format"));
            }
            return date_;
        }
    }

    /**
     *
     * Validator for fields with conditionally required values.
     */
    protected class ValidatorFieldRequired implements FieldValidator {
        private static final String FIELD_PROGRAM_CODE = "PROGRAM CODE";

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
            Collection<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();

            if (StringUtils.isEmpty(value)) {
                String param = (String) field.getParameter();
                String code = entity.getFieldValue(FIELD_PROGRAM_CODE);
                if (CALC_PARAM_PGM_HOMELESS.equals(param) && "032".equals(code)) {
                    errors.add(new StateReportValidationError(entity, field, "Value Required",
                            "This field must contain a value for code 031"));
                }
            }
            return errors;
        }
    }


    /**
     * Constants: Aliases, Fields, IDs, Parameters
     */
    protected static final String ALIAS_EXCLUDE_PROGRAM = "DOE EXCLUDE PGM";
    protected static final String ALIAS_SCHOOL_STATE_ID = "DOE SCHOOL STATE ID";
    protected static final String CALC_ID_PGM_CALC_PROGRAM = "PGM_CALC_PROGRAM";
    protected static final String CALC_PARAM_PGM_BEGINDATE = "PGM_BEGINDATE";
    protected static final String CALC_PARAM_PGM_ENDDATE = "PGM_ENDDATE";
    protected static final String CALC_PARAM_PGM_HOMELESS = "PGM_HOMELESS";
    protected static final String CALC_PARAM_PGM_SCHOOL = "PGM_SCHOOL";
    protected static final String INPUT_PREVIOUS_DATE = "previousDate";
    protected static final String INPUT_REPORT_DATE = "reportDate";
    protected static final String INPUT_REPORT_TYPE = "reportPeriod";
    protected static final String REPORT_TYPE_FIRST = "first";
    protected static final String REPORT_TYPE_INTERMEDIATE = "intermediate";
    protected static final String REPORT_TYPE_LAST = "last";
    protected static final String VAL_ID_PGM_VAL_BEGINDATE = "PGM_VAL_STARTDATE";
    protected static final String VAL_ID_PGM_VAL_ENDDATE = "PGM_VAL_ENDDATE";
    protected static final String VAL_ID_PGM_VAL_REQUIRED = "PGM_VAL_REQUIRED";

    /**
     * Instance variables.
     */
    protected String m_contextOid;
    protected PlainDate m_contextStartDate;
    protected String m_fieldExcludeProgram;
    protected String m_fieldSchoolStateId;
    protected PlainDate m_previousDate;
    protected PlainDate m_reportDate;
    protected String m_reportType;
    protected StudentHistoryHelper m_studentHelper;

    /**
     * Collection of state program codes for filtering StudentProgramParticipation records
     */
    private Collection<String> m_reportableCodes;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        initializeFields();
        getReportableProgramCodes();
        if (getSetupErrors().size() != 0) {
            return;
        }
        m_studentHelper = new StudentHistoryHelper(this);
        m_contextOid = getOrganization().getCurrentContextOid();
        m_contextStartDate = getOrganization().getCurrentContext().getStartDate();
        X2Criteria programCriteria = getStudentProgramParticipationCriteria();
        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, programCriteria);
        applyInputSort(query, null);
        setQuery(query);
        setEntityClass(PAStudentProgramParticipationEntity.class);
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Method for construction composite key using input parameters.
     *
     * @param contextOid String
     * @param schoolOid String
     * @param calendarCode String
     * @return composite key for given parameters
     */
    protected static String makeKey(String contextOid, String schoolOid, String calendarCode) {
        return contextOid + ":" + schoolOid + ":" + calendarCode;
    }

    /**
     * Retrieve collecion of reportable numeric program codes.
     *
     * @return void
     */
    private void getReportableProgramCodes() {
        DataDictionaryField field = getDataDictionaryField(StudentProgramParticipation.class,
                StudentProgramParticipation.COL_PROGRAM_CODE);
        String referenceTableOid = field.getReferenceTableOid();
        Collection<ReferenceCode> refCodes = null;
        if (StringUtils.isEmpty(referenceTableOid)) {
            addSetupError("No reportable program codes", "Student Programs reference table state codes");
            return;
        }
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
        criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        refCodes = getBroker().getCollectionByQuery(query);
        // Only include if state code is numeric. Ignore non-numeric state
        // codes.
        m_reportableCodes = new ArrayList<String>(refCodes.size());
        Iterator<ReferenceCode> codesIterator = refCodes.iterator();
        while (codesIterator.hasNext()) {
            ReferenceCode code = codesIterator.next();
            if (StringUtils.isNumeric(code.getStateCode())) {
                m_reportableCodes.add(code.getCode());
            }
        }
        if (m_reportableCodes.size() == 0) {
            addSetupError("No reportable program codes", "Student Programs reference table state codes");
        }
    }

    /**
     * Function for building custom StudentProgramParticipation criteria.
     *
     * @return criteria for query for list of SPP for active students and programs
     *         limited by reportDate, school and not excluded programs
     */
    private X2Criteria getStudentProgramParticipationCriteria() {
        m_studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.FALSE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.FALSE);
        X2Criteria studentCriteria = m_studentHelper.getStudentCriteria();
        SubQuery studentSubquery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubquery);
        criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, m_reportableCodes);
        // gather programs with the correct dates
        if (REPORT_TYPE_INTERMEDIATE.equals(m_reportType)) {
            X2Criteria criteriaStartDate = new X2Criteria();
            criteriaStartDate.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDate);
            criteriaStartDate.addGreaterThan(StudentProgramParticipation.COL_START_DATE, m_previousDate);
            X2Criteria criteriaEndDate = new X2Criteria();
            criteriaEndDate.addLessOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDate);
            criteriaEndDate.addGreaterThan(StudentProgramParticipation.COL_END_DATE, m_previousDate);
            X2Criteria criteriaOr = new X2Criteria();
            criteriaOr.addAndCriteria(criteriaStartDate);
            criteriaOr.addOrCriteria(criteriaEndDate);
            criteria.addAndCriteria(criteriaOr);
        } else {
            // if start or end reporting, add all programs for this year prior to report date
            criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDate);
            X2Criteria criteriaEndDateIsGE = new X2Criteria();
            criteriaEndDateIsGE.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE,
                    getOrganization().getCurrentContext().getStartDate());
            X2Criteria criteriaEndDate = new X2Criteria();
            criteriaEndDate.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
            criteriaEndDate.addOrCriteria(criteriaEndDateIsGE);
            criteria.addAndCriteria(criteriaEndDate);
        }
        if (isSchoolContext()) {
            criteria.addEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER
                    + Student.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addNotEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER
                    + Student.REL_SCHOOL + PATH_DELIMITER
                    + SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            criteria.addNotEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER
                    + Student.REL_SCHOOL + PATH_DELIMITER
                    + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        applyInputCriteria(criteria, false, StudentProgramParticipation.REL_STUDENT);
        if (m_fieldExcludeProgram != null) {
            criteria.addNotEqualTo(m_fieldExcludeProgram, BooleanAsStringConverter.TRUE);
        }
        return criteria;
    }

    /**
     * Lookup field aliases and paths.
     * Get data from input definition
     */
    private void initializeFields() {
        m_fieldExcludeProgram = translateAliasToJavaName(ALIAS_EXCLUDE_PROGRAM, true);
        m_fieldSchoolStateId = translateAliasToJavaName(ALIAS_SCHOOL_STATE_ID, true);
        m_reportDate = (PlainDate) getParameter(INPUT_REPORT_DATE);
        m_previousDate = (PlainDate) getParameter(INPUT_PREVIOUS_DATE);
        m_reportType = (String) getParameter(INPUT_REPORT_TYPE);
        if (REPORT_TYPE_INTERMEDIATE.equals(m_reportType)) {
            if (m_previousDate == null) {
                addSetupError("Previous Date Required",
                        "You must enter a previous date for intermediate reporting periods");
            } else if (!m_previousDate.before(m_reportDate)) {
                addSetupError("Previous Date Is Incorrect",
                        "You must enter a previous date earlier than the report date");
            }
        }
    }

    /**
     * Register custom fieldRetrievers.
     */
    private void registerFieldRetrievers() {
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(CALC_ID_PGM_CALC_PROGRAM, new FieldRetrieverProgramParticipation());
        super.addCalcs(calcs);
    }

    /**
     * Register custom field Validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(VAL_ID_PGM_VAL_BEGINDATE, new ValidatorBeginDate());
        validators.put(VAL_ID_PGM_VAL_ENDDATE, new ValidatorEndDate());
        validators.put(VAL_ID_PGM_VAL_REQUIRED, new ValidatorFieldRequired());
        super.addValidators(validators);
    }
}
