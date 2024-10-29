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
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

/**
 * FL Staff Experience report
 *
 * http://www.fldoe.org/accountability/data-sys/database-manuals-updates/2017-18-staff-info-system/
 * staff-experience.stml
 *
 * @author Follett Software Company
 */
public class FLStaffExperienceData extends FLStateReportData {

    /**
     * The Class FLStaffExperienceEntity.
     */
    public static class FLStaffExperienceEntity extends FLStateReportEntity {
        private List<DataDictionaryField> m_experienceFields;
        private SisStaff m_record;

        /**
         * Instantiates a new FL staff experience entity.
         */
        public FLStaffExperienceEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the current record.
         *
         * @return the current record
         */
        public SisStaff getCurrentRecord() {
            return m_record;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return the entity name
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
         * Gets the experience field.
         *
         * @return reported experience type
         */
        public DataDictionaryField getExperienceField() {
            return m_experienceFields.get(getCurrentRow());
        }

        /**
         * Intitialize.
         *
         * @param data the data
         * @param bean the bean
         * @throws X2BaseException the x 2 base exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_record = (SisStaff) getBean();

            FLStaffExperienceData flData = (FLStaffExperienceData) data;

            m_experienceFields = flData.getExperiences(m_record);

            Collection<StaffPosition> positions = flData.getStaffHelper().getStaffPositions(m_record);
            if (positions != null && !positions.isEmpty()) {
                for (StaffPosition sfp : positions) {
                    String experienceType = (String) flData.getFieldValue(sfp, flData.m_fieldExperienceType);
                    if (!StringUtils.isEmpty(experienceType)) {
                        DataDictionaryField field = flData.getExperienceField(experienceType);
                        if (field != null & !m_experienceFields.contains(field)) {
                            m_experienceFields.add(field);
                        }
                    }
                }
            }


            setRowCount(m_experienceFields.size());
        }
    }

    /**
     * Retriever for Experience Length
     *
     * http://www.fldoe.org/core/fileparse.php/18495/urlt/1718-204665.pdf
     *
     * @author Follett Software Company
     */
    protected class RetrieveExperienceLength implements FieldRetriever {
        public static final String CALC_ID = "EXPERIENCE_LENGTH";

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
            FLStaffExperienceEntity seEntity = (FLStaffExperienceEntity) entity;
            DataDictionaryField dataField = seEntity.getExperienceField();
            BigDecimal value = (BigDecimal) ((FLStateReportData) data).getFieldValue(entity.getBean(), dataField);
            return Integer.valueOf(value == null ? 0 : value.intValue());
        }
    }

    /**
     * Retriever for Experience Type
     *
     * http://www.fldoe.org/core/fileparse.php/18495/urlt/1718-204760.pdf
     *
     * @author Follett Software Company
     */
    protected class RetrieveExperienceType implements FieldRetriever {
        public static final String CALC_ID = "EXPERIENCE_TYPE";

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
            FLStaffExperienceEntity seEntity = (FLStaffExperienceEntity) entity;
            DataDictionaryField dataField = seEntity.getExperienceField();
            String alias = dataField.getAlias();
            String value = alias.substring(alias.length() - 1).toUpperCase();
            return value;
        }
    }

    protected static final List<String> SXP_SURVEY_PERIOD_VALID_CODES = Arrays.asList(
            FLStateReportData.SURVEY_PERIOD_2, FLStateReportData.SURVEY_PERIOD_3);

    private static final String ALIAS_EXPERIENCE_PREFIX = "all-stf-ExperienceType";
    private static final String ALIAS_EXPERIENCE_TYPE = "all-sfp-ExperienceType";

    private static final String[] EXPERIENCES = {"A", "C", "D", "F", "M", "N", "P", "S"};

    private Map<String, DataDictionaryField> m_fieldExperienceMap;
    private DataDictionaryField m_fieldExperienceType;

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

        m_fieldExperienceType = translateAliasToDictionaryField(ALIAS_EXPERIENCE_TYPE, true);
        initExperienceMap();

        if (getSetupErrors().size() != 0) {
            return;
        }

        getStaffHelper().setSelectionProperty(FLStaffHelper.PROPERTY_BEGIN_DATE, getSurveyPeriod().getStartDate());
        getStaffHelper().setSelectionProperty(FLStaffHelper.PROPERTY_END_DATE, getSurveyPeriod().getEndDate());
        setQuery(getStaffHelper().getStaffQuery(false));
        setEntityClass(FLStaffExperienceEntity.class);

        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Gets the experience field.
     *
     * @param experience the experience
     * @return the experience field
     */
    protected DataDictionaryField getExperienceField(String experience) {
        return m_fieldExperienceMap.get(experience);
    }

    /**
     * Gets the experiences.
     *
     * @param staff the staff
     * @return the experiences
     * @throws X2BaseException the x 2 base exception
     */
    protected List<DataDictionaryField> getExperiences(SisStaff staff) throws X2BaseException {
        List<DataDictionaryField> fields = new ArrayList(EXPERIENCES.length);
        for (Entry<String, DataDictionaryField> entry : m_fieldExperienceMap.entrySet()) {
            BigDecimal experience = (BigDecimal) getFieldValue(staff, entry.getValue());
            if (experience != null && experience.compareTo(BigDecimal.ZERO) > 0) {
                fields.add(entry.getValue());
            }
        }
        return fields;
    }

    /**
     * Gets the valid survey periods.
     *
     * @return the valid survey periods
     */
    @Override
    public Collection<String> getValidSurveyPeriods() {
        return SXP_SURVEY_PERIOD_VALID_CODES;
    }

    /**
     * Inits the experience map.
     */
    private void initExperienceMap() {
        m_fieldExperienceMap = new TreeMap<String, DataDictionaryField>();
        for (String experience : EXPERIENCES) {
            m_fieldExperienceMap.put(experience,
                    translateAliasToDictionaryField(ALIAS_EXPERIENCE_PREFIX + experience, true));
        }
    }

    /**
     * Register custom field Retrievers.
     *
     * @throws X2BaseException the x 2 base exception
     */
    private void registerFieldRetrievers() throws X2BaseException {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveFiscalYear.CALC_ID, new RetrieveFiscalYear());
        calcs.put(RetrieveStaffSsn.CALC_ID, new RetrieveStaffSsn());
        calcs.put(RetrieveExperienceLength.CALC_ID, new RetrieveExperienceLength());
        calcs.put(RetrieveExperienceType.CALC_ID, new RetrieveExperienceType());
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
