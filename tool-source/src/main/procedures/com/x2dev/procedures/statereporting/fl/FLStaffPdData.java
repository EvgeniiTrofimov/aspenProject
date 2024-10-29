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

package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffPdPlan;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import org.apache.commons.lang3.StringUtils;

/**
 * FL Staff Professional Development report
 *
 * http://www.fldoe.org/accountability/data-sys/database-manuals-updates/2016-17-staff-info-system/
 * staff-professional-dev.stml
 *
 * @author Follett Software Company
 */
public class FLStaffPdData extends FLStateReportData {

    /**
     * The Class FLStaffPdEntity.
     */
    public static class FLStaffPdEntity extends FLStateReportEntity {
        private List<StaffPdPlan> m_plans;

        private SisStaff m_record;

        /**
         * Instantiates a new FL staff pd entity.
         */
        public FLStaffPdEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the current record.
         *
         * @return Sis staff
         */
        public SisStaff getCurrentRecord() {
            return m_record;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStaff staff = (SisStaff) getBean();
            String name = staff.getNameView() +
                    " [LASID: " + staff.getLocalId() +
                    "] ";
            return name;
        }

        /**
         * Gets the plan.
         *
         * @return Professional Development Plan record
         */
        public StaffPdPlan getPlan() {
            return m_plans.get(getCurrentRow());
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

            m_record = (SisStaff) getBean();

            FLStaffPdData flData = (FLStaffPdData) data;

            m_plans = new ArrayList<>();

            Collection<StaffPdPlan> plans = flData.getStaffHelper().getStaffPdPlans(m_record);
            if (plans != null && !plans.isEmpty()) {
                m_plans = new ArrayList<>(plans);
            }
            setRowCount(m_plans != null ? m_plans.size() : 0);
        }
    }

    /**
     * Retriever for SDP record UDFs.
     *
     * @author Follett Software Company
     */
    protected class RetrieveSdp implements FieldRetriever {
        public static final String CALC_ID = "SDP";

        private static final String ALIAS_COMPONENT_NUMBER = "all-sdp-ComponentNumber";
        private static final String ALIAS_DISTRICT_NUMBER = "all-sdp-DistrictNumber";
        private static final String ALIAS_EVALUATION_METHOD_STAFF = "all-sdp-EvaluationMethodStaff";
        private static final String ALIAS_EVALUATION_METHOD_STUDENT = "all-sdp-EvaluationMethodStudent";
        private static final String ALIAS_IMPLEMENTATION_METHOD = "all-sdp-ImplementationMethod";
        private static final String ALIAS_LEARNING_METHOD = "all-sdp-LearningMethod";
        private static final String ALIAS_PARTICIPATION_HOURS = "all-sdp-ParticipationHours";
        private static final String ALIAS_PRIMARY_PURPOSE = "all-sdp-PrimaryPurpose";

        private static final String PARAM_COMPONENT_NUMBER = "COMPONENT_NUMBER";
        private static final String PARAM_DISTRICT_NUMBER = "DISTRICT_NUMBER";
        private static final String PARAM_EVALUATION_METHOD_STAFF = "EVALUATION_METHOD_STAFF";
        private static final String PARAM_EVALUATION_METHOD_STUDENT = "EVALUATION_METHOD_STUDENT";
        private static final String PARAM_IMPLEMENTATION_METHOD = "IMPLEMENTATION_METHOD";
        private static final String PARAM_LEARNING_METHOD = "LEARNING_METHOD";
        private static final String PARAM_PARTICIPATION_HOURS = "PARTICIPATION_HOURS";
        private static final String PARAM_PRIMARY_PURPOSE = "PRIMARY_PURPOSE";

        private DataDictionaryField m_fieldComponentNumber;
        private DataDictionaryField m_fieldDistrictNumber;
        private DataDictionaryField m_fieldEvaluationMethodStaff;
        private DataDictionaryField m_fieldEvaluationMethodStudent;
        private DataDictionaryField m_fieldImplementationMethod;
        private DataDictionaryField m_fieldLearningMethod;
        private DataDictionaryField m_fieldParticipationHours;
        private DataDictionaryField m_fieldPrimaryPurpose;

        /**
         * Instantiates a new retrieve sdp.
         */
        public RetrieveSdp() {
            m_fieldComponentNumber = translateAliasToDictionaryField(ALIAS_COMPONENT_NUMBER, true);
            m_fieldDistrictNumber = translateAliasToDictionaryField(ALIAS_DISTRICT_NUMBER, true);
            m_fieldEvaluationMethodStaff = translateAliasToDictionaryField(ALIAS_EVALUATION_METHOD_STAFF, true);
            m_fieldEvaluationMethodStudent = translateAliasToDictionaryField(ALIAS_EVALUATION_METHOD_STUDENT, true);
            m_fieldImplementationMethod = translateAliasToDictionaryField(ALIAS_IMPLEMENTATION_METHOD, true);
            m_fieldLearningMethod = translateAliasToDictionaryField(ALIAS_LEARNING_METHOD, true);
            m_fieldParticipationHours = translateAliasToDictionaryField(ALIAS_PARTICIPATION_HOURS, true);
            m_fieldPrimaryPurpose = translateAliasToDictionaryField(ALIAS_PRIMARY_PURPOSE, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            FLStateReportData flData = (FLStateReportData) data;
            FLStaffPdEntity seEntity = (FLStaffPdEntity) entity;
            StaffPdPlan plan = seEntity.getPlan();

            Object value = null;
            String param = field.getParameter() != null ? field.getParameter().toString() : "";
            switch (param) {
                case PARAM_COMPONENT_NUMBER:
                    value = flData.getFieldValue(plan, m_fieldComponentNumber);
                    break;
                case PARAM_DISTRICT_NUMBER:
                    value = flData.getFieldValue(plan, m_fieldDistrictNumber);
                    if (value == null || (value instanceof String && StringUtils.isEmpty((String) value))) {
                        value = entity.getFieldValue(0);
                    }

                    break;
                case PARAM_EVALUATION_METHOD_STAFF:
                    value = flData.getFieldValue(plan, m_fieldEvaluationMethodStaff);
                    break;
                case PARAM_EVALUATION_METHOD_STUDENT:
                    value = flData.getFieldValue(plan, m_fieldEvaluationMethodStudent);
                    break;
                case PARAM_IMPLEMENTATION_METHOD:
                    value = flData.getFieldValue(plan, m_fieldImplementationMethod);
                    break;
                case PARAM_LEARNING_METHOD:
                    value = flData.getFieldValue(plan, m_fieldLearningMethod);
                    break;
                case PARAM_PARTICIPATION_HOURS:
                    value = flData.getFieldValue(plan, m_fieldParticipationHours);
                    break;
                case PARAM_PRIMARY_PURPOSE:
                    value = flData.getFieldValue(plan, m_fieldPrimaryPurpose);
                    break;
            }
            return value;
        }
    }

    protected static final List<String> SPD_SURVEY_PERIOD_VALID_CODES = Arrays.asList(
            FLStateReportData.SURVEY_PERIOD_5);

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        if (getSetupErrors().size() != 0) {
            return;
        }

        getStaffHelper().setSelectionProperty(FLStaffHelper.PROPERTY_BEGIN_DATE, getSurveyPeriod().getStartDate());
        getStaffHelper().setSelectionProperty(FLStaffHelper.PROPERTY_END_DATE, getSurveyPeriod().getEndDate());
        setQuery(getStaffHelper().getStaffQuery(false));
        setEntityClass(FLStaffPdEntity.class);

        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Gets the valid survey periods.
     *
     * @return the valid survey periods
     */
    @Override
    public Collection<String> getValidSurveyPeriods() {
        return SPD_SURVEY_PERIOD_VALID_CODES;
    }

    /**
     * Register custom field Retrievers.
     *
     * @throws X2BaseException exception
     */
    private void registerFieldRetrievers() throws X2BaseException {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveFiscalYear.CALC_ID, new RetrieveFiscalYear());
        calcs.put(RetrieveStaffPrimarySchoolNumber.CALC_ID, new RetrieveStaffPrimarySchoolNumber());
        calcs.put(RetrieveStaffSsn.CALC_ID, new RetrieveStaffSsn());
        calcs.put(RetrieveSdp.CALC_ID, new RetrieveSdp());
        super.addCalcs(calcs);
    }

    /**
     * method for initialization of field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        super.addValidators(validators);
    }
}
