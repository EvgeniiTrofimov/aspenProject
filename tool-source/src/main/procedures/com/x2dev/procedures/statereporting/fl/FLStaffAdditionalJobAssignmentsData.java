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
import com.x2dev.sis.model.beans.StaffPosition;
import com.x2dev.utils.X2BaseException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import org.apache.commons.lang.StringUtils;

/**
 * FL Staff Additional Job Assignments report
 *
 * http://www.fldoe.org/accountability/data-sys/database-manuals-updates/2017-18-staff-info-system/
 * staff-additional-job-assignments.stml
 *
 * @author Follett Software Company
 */
public class FLStaffAdditionalJobAssignmentsData extends FLStateReportData {

    /**
     * The Class FLStaffAdditionalJobAssignmentsEntity.
     */
    public static class FLStaffAdditionalJobAssignmentsEntity extends FLStateReportEntity {
        private List<StaffPosition> m_positions;
        private SisStaff m_record;

        /**
         * Instantiates a new FL staff additional job assignments entity.
         */
        public FLStaffAdditionalJobAssignmentsEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the current position.
         *
         * @return Staff position
         */
        public StaffPosition getCurrentPosition() {
            return m_positions.get(getCurrentRow());
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

            FLStaffAdditionalJobAssignmentsData flData = (FLStaffAdditionalJobAssignmentsData) data;
            Collection<StaffPosition> positions = flData.getStaffHelper().getStaffPositions(m_record);
            if (positions != null && !positions.isEmpty()) {
                m_positions = new ArrayList<StaffPosition>(positions);
                StaffPosition primary = flData.getStaffHelper().getPrimaryStaffPosition(m_record);
                if (primary != null) {
                    int index = m_positions.indexOf(primary);
                    if (index >= 0) {
                        m_positions.remove(index);
                    }
                }
            }
            setRowCount(m_positions != null ? m_positions.size() : 0);
        }
    }

    /**
     * Retriever for Additional Job Code
     *
     * http://www.fldoe.org/core/fileparse.php/18495/urlt/1718-208830.pdf
     *
     * @author Follett Software Company
     */
    protected class RetrieveAdditionalJobCode implements FieldRetriever {
        public static final String CALC_ID = "JOB_CODE";

        DataDictionaryField m_codeField = null;

        /**
         * Instantiates a new retrieve additional job code.
         */
        public RetrieveAdditionalJobCode() {
            m_codeField = FLStaffAdditionalJobAssignmentsData.this.getDataDictionaryField(StaffPosition.class,
                    StaffPosition.COL_JOB_CODE);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StaffPosition current = ((FLStaffAdditionalJobAssignmentsEntity) entity).getCurrentPosition();
            return FLStaffAdditionalJobAssignmentsData.this.getFieldValue(current, m_codeField);
        }
    }

    /**
     * Retriever for Job Code Fund Source
     *
     * http://www.fldoe.org/core/fileparse.php/15228/urlt/1617-209070.pdf
     *
     * @author Follett Software Company
     */
    protected class RetrieveFundSource implements FieldRetriever {
        public static final String CALC_ID = "FUND_SOURCE";

        private static final String ALIAS_PERCENTAGE_1 = "all-sfp-FundSourcePercentage1";
        private static final String ALIAS_PERCENTAGE_2 = "all-sfp-FundSourcePercentage2";
        private static final String ALIAS_PERCENTAGE_3 = "all-sfp-FundSourcePercentage3";

        private static final String ALIAS_SOURCE_1 = "all-sfp-FundSource1";
        private static final String ALIAS_SOURCE_2 = "all-sfp-FundSource2";
        private static final String ALIAS_SOURCE_3 = "all-sfp-FundSource3";

        private DataDictionaryField m_fieldPercentage1;
        private DataDictionaryField m_fieldPercentage2;
        private DataDictionaryField m_fieldPercentage3;

        private DataDictionaryField m_fieldSource1;
        private DataDictionaryField m_fieldSource2;
        private DataDictionaryField m_fieldSource3;

        /**
         * Instantiates a new retrieve fund source.
         */
        public RetrieveFundSource() {
            m_fieldPercentage1 = translateAliasToDictionaryField(ALIAS_PERCENTAGE_1, true);
            m_fieldPercentage2 = translateAliasToDictionaryField(ALIAS_PERCENTAGE_2, true);
            m_fieldPercentage3 = translateAliasToDictionaryField(ALIAS_PERCENTAGE_3, true);

            m_fieldSource1 = translateAliasToDictionaryField(ALIAS_SOURCE_1, true);
            m_fieldSource2 = translateAliasToDictionaryField(ALIAS_SOURCE_2, true);
            m_fieldSource3 = translateAliasToDictionaryField(ALIAS_SOURCE_3, true);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StaffPosition current = ((FLStaffAdditionalJobAssignmentsEntity) entity).getCurrentPosition();
            FLStateReportData flData = (FLStateReportData) data;
            StringBuilder value = new StringBuilder();
            addFundSource(value, flData, current, m_fieldSource1, m_fieldPercentage1);
            addFundSource(value, flData, current, m_fieldSource2, m_fieldPercentage2);
            addFundSource(value, flData, current, m_fieldSource3, m_fieldPercentage3);
            return value.toString();
        }

        /**
         * Adds the fund source.
         *
         * @param value StringBuilder
         * @param flData FLStateReportData
         * @param current StaffPosition
         * @param fieldSource DataDictionaryField
         * @param fieldPercentage DataDictionaryField
         * @throws X2BaseException exception
         */
        private void addFundSource(StringBuilder value,
                                   FLStateReportData flData,
                                   StaffPosition current,
                                   DataDictionaryField fieldSource,
                                   DataDictionaryField fieldPercentage)
                throws X2BaseException {
            String source = (String) flData.getFieldValue(current, fieldSource);
            if (!StringUtils.isEmpty(source)) {
                BigDecimal percentage = (BigDecimal) flData.getFieldValue(current, fieldPercentage);
                value.append(source);
                value.append(String.format("%03d", bigDecimalToInteger(percentage)));
            }
        }
    }

    /**
     * Retriever for Qualified Paraprof.
     */
    protected class RetrieveHQParaprof implements FieldRetriever {
        public static final String CALC_ID = "HQ_PARAPROF";

        private static final String ALIAS_PARAPROF = "all-stf-HighQualifiedParaprof";

        DataDictionaryField m_hqParaprofField = null;

        /**
         * Instantiates a new retrieve HQ paraprof.
         */
        public RetrieveHQParaprof() {
            m_hqParaprofField =
                    FLStaffAdditionalJobAssignmentsData.this.translateAliasToDictionaryField(ALIAS_PARAPROF, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStaff staff = ((FLStaffAdditionalJobAssignmentsEntity) entity).getCurrentRecord();
            return FLStaffAdditionalJobAssignmentsData.this.getFieldValue(staff, m_hqParaprofField);
        }
    }

    /**
     * Retriever for Job Code FTE
     *
     * http://www.fldoe.org/core/fileparse.php/15228/urlt/1617-208950.pdf
     *
     * @author Follett Software Company
     */
    protected class RetrieveJobCodeFte implements FieldRetriever {
        public static final String CALC_ID = "FTE";
        public final BigDecimal BIG_100 = new BigDecimal(100);

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StaffPosition current = ((FLStaffAdditionalJobAssignmentsEntity) entity).getCurrentPosition();
            return current.getFte() == null ? BigDecimal.ZERO : current.getFte().multiply(BIG_100);
        }
    }

    protected static final List<String> SAJA_SURVEY_PERIOD_VALID_CODES = Arrays.asList(
            FLStateReportData.SURVEY_PERIOD_2, FLStateReportData.SURVEY_PERIOD_3);

    /**
     * Big decimal to integer.
     *
     * @param bd BigDecimal
     * @return Integer
     */
    private static Integer bigDecimalToInteger(BigDecimal bd) {
        int value = 0;
        if (bd != null) {
            bd = bd.setScale(0, RoundingMode.HALF_UP);
            value = bd.intValue();
        }
        return Integer.valueOf(value);
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query to load.
     *
     * @throws X2BaseException exception
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
        setEntityClass(FLStaffAdditionalJobAssignmentsEntity.class);

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
        return SAJA_SURVEY_PERIOD_VALID_CODES;
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
        calcs.put(RetrieveAdditionalJobCode.CALC_ID, new RetrieveAdditionalJobCode());
        calcs.put(RetrieveJobCodeFte.CALC_ID, new RetrieveJobCodeFte());
        calcs.put(RetrieveFundSource.CALC_ID, new RetrieveFundSource());
        calcs.put(RetrieveHQParaprof.CALC_ID, new RetrieveHQParaprof());
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
