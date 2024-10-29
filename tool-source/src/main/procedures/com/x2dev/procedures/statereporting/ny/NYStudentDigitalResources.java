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
package com.x2dev.procedures.statereporting.ny;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.ny.NYStudentLite.NYStudentHistoryHelper;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.UserDefinedTableA;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.apache.commons.lang.StringUtils;

/**
 * New York state procedure for Student Digital Resources export.
 *
 * @author X2 Development Corporation
 */

public class NYStudentDigitalResources extends StateReportData {

    /**
     * Entity class for Student Digital Resources export.
     *
     * @author X2 Development Corporation
     */

    public static class NYStudentDigitalResourcesEntity extends StateReportEntity {

        NYStudentDigitalResources m_reportData;
        List<UserDefinedTableA> m_currentYearSDRRecords;

        /**
         * Public no argument constructor for dynamic instantiation.
         */

        public NYStudentDigitalResourcesEntity() {
            // no argument constructor
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            return getSDRRecordValidationError(this);
        }

        /**
         * Gets current SDR record
         *
         * @return UserDefinedTableA
         */
        public UserDefinedTableA getCurrentSDRRecord() {
            return m_currentYearSDRRecords != null && m_currentYearSDRRecords.size() > getCurrentRow()
                    ? m_currentYearSDRRecords.get(getCurrentRow())
                    : null;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getFieldValidations()
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidations() {
            StateReportValidationError error = filterEntity();
            if (error != null) {
                return Arrays.asList(error);
            }
            return super.getFieldValidations();
        }

        /**
         * Gets bean SDR validation error
         *
         * @param reportEntity
         * @param student
         *
         * @return StateReportValidationError
         */
        public StateReportValidationError getSDRRecordValidationError(NYStudentDigitalResourcesEntity reportEntity) {
            StateReportValidationError error = null;
            Collection<UserDefinedTableA> sdrRecords = null;
            NYStudentDigitalResources reportData = reportEntity.m_reportData;
            SisStudent student = (SisStudent) getBean();
            sdrRecords = reportData.getSDRRecords(student);
            if (sdrRecords != null && !sdrRecords.isEmpty()
                    && (reportEntity.m_currentYearSDRRecords == null
                            || reportEntity.m_currentYearSDRRecords.isEmpty())) {
                String contextId = reportData.getCurrentContext().getContextId();
                error = new StateReportValidationError(this, reportData.getFieldDefinition(FIELD_SURVEY_DATE),
                        "No SDR data for " + contextId, "");
            }
            if (sdrRecords == null || sdrRecords.isEmpty()) {
                error = new StateReportValidationError(this, reportData.getFieldDefinition(FIELD_SURVEY_DATE),
                        "No Existing SDR data.", "");
            }
            return error;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_reportData = (NYStudentDigitalResources) data;
            SisStudent student = (SisStudent) bean;
            Collection<UserDefinedTableA> studentSDRRecords = m_reportData.getSDRRecords(student);
            m_currentYearSDRRecords = new ArrayList<>();
            if (studentSDRRecords != null && !studentSDRRecords.isEmpty()) {
                m_currentYearSDRRecords.addAll(studentSDRRecords.stream()
                        .filter(m_reportData::isCurrentYearRecord)
                        .collect(Collectors.toList()));
            }
            setRowCount(m_currentYearSDRRecords.size() > 0 ? m_currentYearSDRRecords.size() : 1);
        }

    }

    /**
     * Get SDR data
     *
     * @author Follett Software Company
     */
    public class SDRDataRetriever implements FieldRetriever {

        public static final String CALC_ID = "SDR-DATA";

        private static final String STATE_CODE = "STATE";

        private Map<String, String> m_aliasMap = new HashMap();

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = StringUtils.EMPTY;
            NYStudentDigitalResourcesEntity reportEntity = (NYStudentDigitalResourcesEntity) entity;
            UserDefinedTableA sdrRecord = reportEntity.getCurrentSDRRecord();
            String[] params = ((String) field.getParameter()).split(",");
            if (sdrRecord != null) {
                String beanPath = getBeanPath(data, params[0]);
                value = data.getPropertyAsJavaType(sdrRecord, beanPath);
                if ((value instanceof String) && ((String) value).length() > 0 && params.length > 1
                        && STATE_CODE.equals(params[1])) {
                    value = data.lookupStateValue(UserDefinedTableA.class, beanPath, (String) value);
                }
            }
            return value;
        }

        private String getBeanPath(StateReportData data, String alias) {
            if (m_aliasMap.get(alias) == null) {
                m_aliasMap.put(alias, data.translateAliasToJavaName(alias, true));
            }
            return m_aliasMap.get(alias);
        }

    }

    private class ValidateRequiredFields implements FieldValidator {
        private static final String MESSAGE_REQUIRED = "Field is required.";
        private static final String MESSAGE_REQUIRED_ANY = "At least one of the fields must be present.";
        private static final String VAL_ID = "VAL_REQUIRED";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String distrCode = entity.getFieldValue(EXP_FIELD_DISTRICT_CODE);
            if (StringUtils.isEmpty(distrCode)) {
                errors.add(new StateReportValidationError("", EXP_FIELD_DISTRICT_CODE, "", MESSAGE_REQUIRED));
            }
            String locCode = entity.getFieldValue(EXP_FIELD_LOCATION_CODE);
            if (StringUtils.isEmpty(locCode)) {
                errors.add(new StateReportValidationError("", EXP_FIELD_DISTRICT_CODE, "", MESSAGE_REQUIRED));
            }
            String ctxDate = entity.getFieldValue(EXP_FIELD_SCHOOL_YEAR_DATE);
            if (StringUtils.isEmpty(ctxDate)) {
                errors.add(new StateReportValidationError("", EXP_FIELD_SCHOOL_YEAR_DATE, "", MESSAGE_REQUIRED));
            }
            String stdId = entity.getFieldValue(EXP_FIELD_STUDENT_ID);
            if (StringUtils.isEmpty(stdId)) {
                errors.add(new StateReportValidationError("", EXP_FIELD_STUDENT_ID, "", MESSAGE_REQUIRED));
            }
            String surveyDate = entity.getFieldValue(EXP_FIELD_SURVEY_DATE);
            if (StringUtils.isEmpty(surveyDate)) {
                errors.add(new StateReportValidationError("", EXP_FIELD_SURVEY_DATE, "", MESSAGE_REQUIRED));
            }
            boolean addEror = true;
            for (String expField : EXP_FIELDS_LIST) {
                String fieldValue = entity.getFieldValue(expField);
                if (!StringUtils.isEmpty(fieldValue)) {
                    addEror = false;
                    break;
                }
            }
            if (addEror) {
                errors.add(new StateReportValidationError("", EXP_FIELDS_LIST.toString(), "", MESSAGE_REQUIRED_ANY));
            }
            return errors;
        }
    }

    /**
     * Aliases
     */
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_SURVEY_DATE = "ny-sdr-survey-date";

    protected static final String DDX_ID_NYSED_SDR = "NYSED-SDR";

    protected static final String FIELD_SURVEY_DATE = "Survey Date";

    /**
     * Input Definition Parameters
     */
    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";
    protected static final String PARAM_REMOVE_HEADER = "removeHeader";
    protected static final String PARAM_REPORT_DATE = "reportDate";

    /**
     * Names of Export Fields Required.
     */
    protected static final String EXP_FIELD_DISTRICT_CODE = "DISTRICT CODE";
    protected static final String EXP_FIELD_LOCATION_CODE = "LOCATION CODE";
    protected static final String EXP_FIELD_SCHOOL_YEAR_DATE = "SCHOOL YEAR DATE";
    protected static final String EXP_FIELD_STUDENT_ID = "STUDENT ID";
    protected static final String EXP_FIELD_SURVEY_DATE = "Survey Date";

    /**
     * Names of Export Fields Required Any.
     */
    protected static final Collection<String> EXP_FIELDS_LIST = Arrays.asList("Internet Access Barrier Code",
            "Internet Performance Code", "Internet Access In Residence Indicator", "Internet Access Type Code",
            "Primary Learnng Device Access Code", "Primary Learning Device Provider Code",
            "Primary Learning Device Provider Code", "Primary Learning Device Sufficiency Indicator",
            "Primary Learning Device Type Code", "School Provided Device Indicator");

    /**
     * Local Variables
     */
    protected String m_excludeSchool;
    protected boolean m_excludeSchoolIndicator = false;
    protected String m_fieldSurveyDate;
    protected StudentHistoryHelper m_helper;
    protected boolean m_removeHeaderIndicator;
    protected PlainDate m_reportDate;
    protected Map<String, Collection<UserDefinedTableA>> m_sdrByStdMap;
    protected String m_servDistrict;

    /**
     * Gets the heading.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        if (m_removeHeaderIndicator) {
            return null;
        }
        return super.getHeading();
    }

    /**
     * @param student
     * @return
     */
    public Collection<UserDefinedTableA> getSDRRecords(SisStudent student) {
        Collection<UserDefinedTableA> records = null;
        if (m_sdrByStdMap != null && student != null) {
            records = m_sdrByStdMap.get(student.getOid());
        }
        return records;
    }

    /**
     * Initialize the data module.
     */
    @Override
    public void initialize() {
        initializeFields();
        m_fieldSurveyDate = translateAliasToJavaName(ALIAS_SURVEY_DATE, true);

        m_helper = new NYStudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        X2Criteria studentCriteria = m_helper.getStudentCriteria();

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(UserDefinedTableA.REL_EXTENDED_DATA_DICTIONARY + ModelProperty.PATH_DELIMITER
                + ExtendedDataDictionary.COL_ID, DDX_ID_NYSED_SDR);
        if (m_excludeSchoolIndicator) {
            criteria.addNotEqualTo(UserDefinedTableA.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.REL_SCHOOL
                    + ModelProperty.PATH_DELIMITER + m_excludeSchool, Boolean.TRUE);
        }
        criteria.addIn(UserDefinedTableA.REL_STUDENT,
                new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria));
        BeanQuery query = new BeanQuery(UserDefinedTableA.class, criteria);

        m_sdrByStdMap = getBroker().getGroupedCollectionByQuery(query, UserDefinedTableA.COL_STUDENT_OID, 100);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(NYStudentDigitalResourcesEntity.class);
        }

        // Build and attach retrievers
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(SDRDataRetriever.CALC_ID, new SDRDataRetriever());
        super.addCalcs(calcs);

        HashMap validators = new HashMap<String, FieldRetriever>();
        validators.put(ValidateRequiredFields.VAL_ID, new ValidateRequiredFields());
        super.addValidators(validators);

    }

    public boolean isCurrentYearRecord(UserDefinedTableA record) {
        PlainDate surveyDate = PlainDate.fromString((String) record.getFieldValueByBeanPath(m_fieldSurveyDate));
        return surveyDate != null && surveyDate.after(getCurrentContext().getStartDate())
                && surveyDate.before(getCurrentContext().getEndDate());
    }

    /**
     * Initialize fields.
     */
    private void initializeFields() {

        // Load Parameters
        m_reportDate = new PlainDate();
        if (getParameter(PARAM_REPORT_DATE) != null) {
            m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        }
        m_removeHeaderIndicator = false;
        if (getParameter(PARAM_REMOVE_HEADER) != null) {
            m_removeHeaderIndicator = ((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue();
        }
        m_excludeSchoolIndicator = false;
        if (getParameter(PARAM_EXCLUDE_SCHOOL) != null) {
            m_excludeSchoolIndicator = ((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue();
        }

        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
    }
}
