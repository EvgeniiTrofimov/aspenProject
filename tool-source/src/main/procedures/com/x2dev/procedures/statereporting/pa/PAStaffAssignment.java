/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2013 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.pa;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.StaffPosition;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class PAStaffAssignment.
 */
public class PAStaffAssignment extends StateReportData {
    /**
     * Entity class for PAStaffAssignment export.
     *
     */
    public static class PAStaffAssignmentEntity extends StateReportEntity {
        PAStaffAssignment m_exportData;
        StaffPosition m_staffPos;

        /**
         * Instantiates a new PA staff assignment entity.
         */
        public PAStaffAssignmentEntity() {
            //
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            StaffPosition staffPosition = getCurrentStaffPosition();
            Staff staff = staffPosition.getStaff();
            String name = staff.getNameView() +
                    " [LASID: " + staff.getLocalId() +
                    ", SASID: " + staff.getStateId() +
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
            m_exportData = (PAStaffAssignment) data;
            m_staffPos = (StaffPosition) bean;

            super.intitialize(data, bean);
        }

        /**
         * Gets current staff position.
         *
         * @return Staff position
         */
        protected StaffPosition getCurrentStaffPosition() {
            return m_staffPos;
        }
    }

    /**
     * Retriever for full time equivalent field.
     */
    class RetrieveFTEField implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity reportEntity, FieldDefinition field)
                throws X2BaseException {
            PAStaffAssignmentEntity entity = (PAStaffAssignmentEntity) reportEntity;
            StaffPosition position = entity.getCurrentStaffPosition();

            BigDecimal fte = position.getFte();

            if (fte == null) {
                return Integer.valueOf(0);
            }

            return Integer.valueOf((int) (fte.floatValue() * 100));
        }
    }

    /**
     * Validation for completion date field.
     */
    class ValidateCompletionDateField implements FieldValidator {

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
            PAStaffAssignmentEntity entity1 = (PAStaffAssignmentEntity) entity;
            StaffPosition position = entity1.getCurrentStaffPosition();
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            PlainDate startDate = position.getStartDate();
            PlainDate endDate = position.getEndDate();
            if (endDate == null) {
                endDate = startDate;
            }

            if (startDate.compareTo(endDate) > 0) {
                errors.add(new StateReportValidationError(entity, field, "StartDate is greater than EndDate",
                        "startDate:" + startDate.toString() + " ; endDate: " + endDate.toString()));
            }
            return errors;
        }

    }

    /**
     * Constants: Aliases, Fields, IDs, Parameters
     * public, package, protected, private
     */
    protected static final String ALIAS_EXCLUDE_STAFF = "DOE EXCLUDE STF";

    protected static final String CALC_ID_SFP_CALC_FTE = "SFP_CALC_FTE";

    protected static final String INPUT_REPORT_DATE = "inputReportDate";

    protected static final String VAL_ID_SFP_VAL_CMPLTNDATE = "SFP_VAL_CMPLTNDATE";


    /**
     * Instance variables.
     */
    protected String m_fieldExcludeStaff;
    protected Date m_reportDate;

    /**
     * Instance Methods
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

        if (getSetupErrors().size() != 0) {
            return;
        }

        registerFieldRetrievers();
        registerFieldValidators();

        initCustomQuery();

        setEntityClass(PAStaffAssignmentEntity.class);
    }

    /**
     * This method construct and return custom criteria to filter records.
     *
     * @return Criteria
     */
    private Criteria getCustomCriteria() {
        X2Criteria criteria = new X2Criteria();
        applyInputCriteria(criteria, false, null);

        limitCriteriaToActiveSchools(criteria);
        limitExcludedStaff(criteria);
        limitToReportDate(criteria);

        return criteria;
    }

    /**
     * This method inits custom query based on custom criteria .
     */
    private void initCustomQuery() {
        QueryByCriteria query = new QueryByCriteria(StaffPosition.class, getCustomCriteria());
        applyInputSort(query, null);
        setQuery(query);
    }

    /**
     * Filters criteria to selected isSchoolContext or active schools.
     *
     * @param criteria - criteria to filter
     */
    private void limitCriteriaToActiveSchools(X2Criteria criteria) {
        if (isSchoolContext()) {
            criteria.addEqualTo(StaffPosition.REL_SCHOOL + PATH_DELIMITER
                    + X2BaseBean.COL_OID,
                    getSchool().getOid());
        } else {
            criteria.addNotEqualTo(StaffPosition.REL_SCHOOL + PATH_DELIMITER
                    + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);

            criteria.addNotEqualTo(StaffPosition.REL_SCHOOL + PATH_DELIMITER
                    + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }
    }

    /**
     * Filter for excluded staff for criteria which transmitted in parameter.
     *
     * @param criteria - criteria to filter
     */
    private void limitExcludedStaff(X2Criteria criteria) {
        // Exclude staff manually flagged for exclusion by the user
        criteria.addNotEqualTo(StaffPosition.REL_STAFF + PATH_DELIMITER + m_fieldExcludeStaff,
                BooleanAsStringConverter.TRUE);
    }

    /**
     * Filter for start and end date for criteria which transmitted in parameter.
     *
     * @param criteria - criteria to filter
     */
    private void limitToReportDate(X2Criteria criteria) {
        if (m_reportDate != null) {
            // end date >= school district context start date
            // or end date is null
            // StaffPosition.COL_END_DATE >= getOrganization().getCurrentContext().getStartDate()
            X2Criteria criteriaEndDateLimit = new X2Criteria();
            criteriaEndDateLimit.addEmpty(StaffPosition.COL_END_DATE, getBroker().getPersistenceKey());

            X2Criteria c1 = new X2Criteria();
            c1.addGreaterOrEqualThan(StaffPosition.COL_END_DATE, getOrganization().getCurrentContext().getStartDate());
            criteriaEndDateLimit.addOrCriteria(c1);

            criteria.addAndCriteria(criteriaEndDateLimit);

            // and start date is less+equal than the report date
            X2Criteria criteriaStartDateLEReportDate = new X2Criteria();
            criteriaStartDateLEReportDate.addLessOrEqualThan(StaffPosition.COL_START_DATE, m_reportDate);
            criteria.addAndCriteria(criteriaStartDateLEReportDate);
        }
    }

    /**
     * Lookup field aliases and paths.
     * Get data from input definition
     */
    private void initializeFields() {
        m_fieldExcludeStaff = translateAliasToJavaName(ALIAS_EXCLUDE_STAFF, true);

        m_reportDate = (Date) getParameter(INPUT_REPORT_DATE);
    }

    /**
     * Register custom fieldRetrievers.
     */
    private void registerFieldRetrievers() {
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(CALC_ID_SFP_CALC_FTE, new RetrieveFTEField());
        super.addCalcs(calcs);

    }

    /**
     * Register custom field Validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(VAL_ID_SFP_VAL_CMPLTNDATE, new ValidateCompletionDateField());
        super.addValidators(validators);
    }
}
