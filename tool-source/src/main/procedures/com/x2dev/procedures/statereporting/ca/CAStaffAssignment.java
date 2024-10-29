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

package com.x2dev.procedures.statereporting.ca;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.StaffPosition;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export Data Module for CA Staff Assignment.
 *
 * @author X2 Development Corporation
 */

public class CAStaffAssignment extends StateReportData {
    /**
     * Entity class for CA Student Discipline export.
     *
     */
    public static class CAStaffAssignmentEntity extends StateReportEntity {
        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public CAStaffAssignmentEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            StaffPosition position = (StaffPosition) getBean();
            String name = position.getStaff().getNameView() +
                    " [LASID: " + position.getStaff().getLocalId() +
                    ", SASID: " + position.getStaff().getStateId() +
                    "] ";

            return name;
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
    }

    /**
     * Retrieve the FTE from staff position and covert to percent.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveFtePercent implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            StaffPosition position = (StaffPosition) entity.getBean();
            BigDecimal fte = position.getFte();
            return fte == null ? new BigDecimal(0) : fte.multiply(BIG_DECIMAL_100);
        }
    }

    /**
     * If Staff Job Classification Code = 10, 11, or 25
     * Then one or more of the Non Classroom Based Job Assignment Codes must be populated.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateJobClass implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            StaffPosition position = (StaffPosition) entity.getBean();

            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value Required",
                        "This field must contain a value"));
                return errors;
            }

            if (value.matches(MATCH_NON_CLASSROOM_JOB_REQ) &&
                    StringUtils.isEmpty((String) position.getFieldValueByBeanPath(m_fieldSupportAssignment1)) &&
                    StringUtils.isEmpty((String) position.getFieldValueByBeanPath(m_fieldSupportAssignment2)) &&
                    StringUtils.isEmpty((String) position.getFieldValueByBeanPath(m_fieldSupportAssignment3)) &&
                    StringUtils.isEmpty((String) position.getFieldValueByBeanPath(m_fieldSupportAssignment4)) &&
                    StringUtils.isEmpty((String) position.getFieldValueByBeanPath(m_fieldSupportAssignment5)) &&
                    StringUtils.isEmpty((String) position.getFieldValueByBeanPath(m_fieldSupportAssignment6)) &&
                    StringUtils.isEmpty((String) position.getFieldValueByBeanPath(m_fieldSupportAssignment7))) {
                errors.add(new StateReportValidationError(entity, field,
                        "If Staff Job Classification Code = 10, 11, or 25 Then one or more of the Non Classroom Based Job Assignment Codes must be populated",
                        "Staff Job Classification = " + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }
    }

    protected static final String ALIAS_EXCLUDE_STF = "DOE EXCLUDE STF";
    protected static final String ALIAS_JOB_CLASS_CODE = "DOE EDU JOB CLASS CODE";
    protected static final String ALIAS_SUPPORT_ASSIGN_1 = "DOE EDU SUPPORT ASSIGN 1";
    protected static final String ALIAS_SUPPORT_ASSIGN_2 = "DOE EDU SUPPORT ASSIGN 2";
    protected static final String ALIAS_SUPPORT_ASSIGN_3 = "DOE EDU SUPPORT ASSIGN 3";
    protected static final String ALIAS_SUPPORT_ASSIGN_4 = "DOE EDU SUPPORT ASSIGN 4";
    protected static final String ALIAS_SUPPORT_ASSIGN_5 = "DOE EDU SUPPORT ASSIGN 5";
    protected static final String ALIAS_SUPPORT_ASSIGN_6 = "DOE EDU SUPPORT ASSIGN 6";
    protected static final String ALIAS_SUPPORT_ASSIGN_7 = "DOE EDU SUPPORT ASSIGN 7";

    protected static final BigDecimal BIG_DECIMAL_100 = new BigDecimal("100");
    protected static final String CALCULATION_ID = "SFP-CALC-ID";
    protected static final String MATCH_NON_CLASSROOM_JOB_REQ = "^(10|11|25)$";

    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_ACTIVE_ONLY = "activeOnly";

    protected static final String VALIDATION_ID = "SFP-VALID-ID";

    /*
     * Instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_fieldExcludeStaff;
    protected String m_fieldJobClassCode;
    protected String m_fieldSupportAssignment1;
    protected String m_fieldSupportAssignment2;
    protected String m_fieldSupportAssignment3;
    protected String m_fieldSupportAssignment4;
    protected String m_fieldSupportAssignment5;
    protected String m_fieldSupportAssignment6;
    protected String m_fieldSupportAssignment7;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    protected void initialize() throws X2BaseException {
        initializeFields();
        if (this.getSetupErrors().size() == 0) {
            PlainDate reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
            if (getParameter(PARAM_REPORT_DATE) != null) {
                reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
            }
            PlainDate lastDate = DateUtils.add(reportDate, 30);

            // Create program selection criteria
            X2Criteria positionCriteria = new X2Criteria();
            positionCriteria.addNotEqualTo(StaffPosition.REL_STAFF + PATH_DELIMITER + m_fieldExcludeStaff,
                    BooleanAsStringConverter.TRUE);
            positionCriteria.addNotEmpty(m_fieldJobClassCode, getBroker().getPersistenceKey());
            positionCriteria.addLessOrEqualThan(StaffPosition.COL_START_DATE, lastDate);
            X2Criteria endDate1Criteria = new X2Criteria();
            endDate1Criteria.addEmpty(StaffPosition.COL_END_DATE, getBroker().getPersistenceKey());
            X2Criteria endDate2Criteria = new X2Criteria();
            endDate2Criteria.addGreaterOrEqualThan(StaffPosition.COL_END_DATE, reportDate);
            endDate1Criteria.addOrCriteria(endDate2Criteria);
            positionCriteria.addAndCriteria(endDate1Criteria);
            applyInputCriteria(positionCriteria, true, null);
            // COL_FIELD_A012 it is field Do Not Report
            positionCriteria.addNotEqualTo(StaffPosition.REL_STAFF + PATH_DELIMITER + Staff.COL_FIELD_A012,
                    BooleanAsStringConverter.TRUE);
            Boolean isActiveOnly = (Boolean) getParameter(PARAM_ACTIVE_ONLY);
            if (isActiveOnly.booleanValue()) {
                String activeStatus = PreferenceManager.getPreferenceValue(getOrganization(),
                        SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
                positionCriteria.addEqualTo(StaffPosition.REL_STAFF + PATH_DELIMITER + Staff.COL_STATUS, activeStatus);
                positionCriteria.addNotNull(StaffPosition.REL_STAFF + PATH_DELIMITER + Staff.COL_STATE_ID);
            }

            QueryByCriteria query = new QueryByCriteria(StaffPosition.class, positionCriteria);
            this.applyInputSort(query, null);
            this.setQuery(query);

            // Set Custom Entity
            this.setEntityClass(CAStaffAssignmentEntity.class);

            // Build and attach retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALCULATION_ID, new RetrieveFtePercent());
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldValidator>();
            validators.put(VALIDATION_ID, new ValidateJobClass());
            super.addValidators(validators);
        }
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldExcludeStaff = translateAliasToJavaName(ALIAS_EXCLUDE_STF, true);
        m_fieldJobClassCode = translateAliasToJavaName(ALIAS_JOB_CLASS_CODE, true);
        m_fieldSupportAssignment1 = translateAliasToJavaName(ALIAS_SUPPORT_ASSIGN_1, true);
        m_fieldSupportAssignment2 = translateAliasToJavaName(ALIAS_SUPPORT_ASSIGN_2, true);
        m_fieldSupportAssignment3 = translateAliasToJavaName(ALIAS_SUPPORT_ASSIGN_3, true);
        m_fieldSupportAssignment4 = translateAliasToJavaName(ALIAS_SUPPORT_ASSIGN_4, true);
        m_fieldSupportAssignment5 = translateAliasToJavaName(ALIAS_SUPPORT_ASSIGN_5, true);
        m_fieldSupportAssignment6 = translateAliasToJavaName(ALIAS_SUPPORT_ASSIGN_6, true);
        m_fieldSupportAssignment7 = translateAliasToJavaName(ALIAS_SUPPORT_ASSIGN_7, true);
    }

}
