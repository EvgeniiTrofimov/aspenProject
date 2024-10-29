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
package com.x2dev.procedures.statereporting.fl;

import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_1ST_ENC_DATE;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_1ST_ENC_DATE_FORMAT;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_DIS_COND_RESOLUTION;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_DIS_COND_TYPE_CODE;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_MED_TREATMENT_TYPE;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo;
import com.x2dev.sis.model.beans.HealthCondition;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class FLFasterMerge99HCProcedure.
 */
public class FLFasterMerge99HCProcedure extends RecordTypeMergeData {

    /**
     * The Enum FieldMergeAttributes.
     */
    private enum FieldMergeAttributes implements FieldMergeAttributesInterface {
        DIS_COND_TYPE_CODE(FIELD_DIS_COND_TYPE_CODE, new FieldMerger("all-hcn-DiseaseTypeCode")),
        //
        MED_TREATMENT_TYPE(FIELD_MED_TREATMENT_TYPE, new FieldMerger("all-hcn-MedTreatmentCode")),
        //
        FIRST_ENC_DATE_FORMAT(FIELD_1ST_ENC_DATE_FORMAT, new FieldMerger() {
            @Override
            public boolean isMergeNeeded() {
                return false;
            }
        }),
        //
        FIRST_ENC_DATE(FIELD_1ST_ENC_DATE, new FieldMerger("all-hcn-FirstEncounterDate"),
                new ValueAdjusterWithFieldInfos()),
        //
        DIS_COND_RESOLUTION(FIELD_DIS_COND_RESOLUTION, new FieldMerger("all-hcn-DiseaseCondResolution"));

        private FieldMerger m_fieldMerger = null;
        private String m_fieldName = null;
        private ValueAdjuster m_valueAdjuster = null;


        /**
         * Instantiates a new field merge attributes.
         *
         * @param fieldName String
         * @param fieldMerger FieldMerger
         */
        private FieldMergeAttributes(String fieldName, FieldMerger fieldMerger) {
            m_fieldName = fieldName;
            m_fieldMerger = fieldMerger;
        }


        /**
         * Instantiates a new field merge attributes.
         *
         * @param fieldName String
         * @param fieldMerger FieldMerger
         * @param valueAdjuster ValueAdjuster
         */
        private FieldMergeAttributes(String fieldName, FieldMerger fieldMerger, ValueAdjuster valueAdjuster) {
            this(fieldName, fieldMerger);
            m_valueAdjuster = valueAdjuster;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface#getFieldName()
         */
        @Override
        public String getFieldName() {
            return m_fieldName;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface#getMerger()
         */
        @Override
        public FieldMerger getMerger() {
            return m_fieldMerger;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface#getValueAdjuster()
         */
        @Override
        public ValueAdjuster getValueAdjuster() {
            return m_valueAdjuster;
        }
    }


    /**
     * The Class ValueAdjusterWithFieldInfos.
     */
    private static class ValueAdjusterWithFieldInfos extends ValueAdjuster {
        private Map<String, FieldInfo> m_fieldInfos = null;


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster#getAdjustedValue(java.lang.String)
         */
        @Override
        public Object getAdjustedValue(String value) {
            if (getFieldInfos() == null) {
                throw new X2RuntimeException();
            }
            String dateFormatCode = getFieldInfos().get(FIELD_1ST_ENC_DATE_FORMAT).getValue().trim();
            String dateFormat = getDateFormat(dateFormatCode);

            return parseDate(value, dateFormat).toString();
        }


        /**
         * Gets the field infos.
         *
         * @return Map
         */
        public Map<String, FieldInfo> getFieldInfos() {
            return m_fieldInfos;
        }


        /**
         * Sets the field infos.
         *
         * @param fieldInfos Map<String,FieldInfo>
         */
        public void setFieldInfos(Map<String, FieldInfo> fieldInfos) {
            m_fieldInfos = fieldInfos;
        }
    }

    private Map<String, HealthCondition> m_healthConditions = new HashMap<>();


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanMergeTo(java.lang.String)
     */
    @Override
    protected X2BaseBean getBeanMergeTo(String fieldName) {
        String key = getHealthConditionKey();
        if (key == null) {
            return null;
        }

        HealthCondition condition = m_healthConditions.get(key);
        if (condition == null) {
            condition = getNewHealthCondition();
            m_healthConditions.put(key, condition);
        }

        return condition;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanDescriptor(java.lang.String)
     */
    @Override
    protected String getBeanDescriptor(String fieldName) {
        HealthCondition condition = (HealthCondition) getBeanMergeTo(fieldName);
        String disConditionTypeCodeField = getImportingFieldInfo(FIELD_DIS_COND_TYPE_CODE).getBeanPath();
        String medTreatmentTypeCodeField = getImportingFieldInfo(FIELD_MED_TREATMENT_TYPE).getBeanPath();
        String firstEndDateField = getImportingFieldInfo(FIELD_1ST_ENC_DATE).getBeanPath();

        return "Health Condition with Disease Condition Type Code "
                + condition.getFieldValueByBeanPath(disConditionTypeCodeField)
                + " and Medical Treatment Type Code " +
                condition.getFieldValueByBeanPath(medTreatmentTypeCodeField)
                + " and Date of First Encounter " + condition.getFieldValueByBeanPath(firstEndDateField);
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getClassMergeTo()
     */
    @Override
    protected Class<?> getClassMergeTo() {
        return HealthCondition.class;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getCurrentPlainRow()
     */
    @Override
    protected String getCurrentPlainRow() {
        return null;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getFieldMergeAttributes()
     */
    @Override
    protected FieldMergeAttributesInterface[] getFieldMergeAttributes() {
        return FieldMergeAttributes.values();
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        loadHealthConditions();
        assignAdjusterFieldInfos();
    }


    /**
     * Load health conditions.
     */
    protected void loadHealthConditions() {
        X2Criteria hsCriteria = new X2Criteria();
        hsCriteria.addEqualTo(HealthCondition.COL_STUDENT_OID, getStudent().getOid());
        QueryByCriteria hsQuery = new QueryByCriteria(HealthCondition.class, hsCriteria);
        Collection<HealthCondition> conditions = getBroker().getCollectionByQuery(hsQuery);
        for (HealthCondition condition : conditions) {
            String key = getHealthConditionKey(condition);
            m_healthConditions.put(key, condition);
        }
    }


    /**
     * Assign adjuster field infos.
     */
    private void assignAdjusterFieldInfos() {
        Map<String, FieldInfo> fieldInfosMap = getFieldInfosForPlainRow(getImportingPlainRow());
        for (String fieldName : getMergeableFieldNames()) {
            FieldMergeAttributesInterface mergeAttributes = findFieldMergeAttributesByFieldName(fieldName);
            ValueAdjuster adjuster = mergeAttributes.getValueAdjuster();
            if (adjuster != null && adjuster instanceof ValueAdjusterWithFieldInfos) {
                ((ValueAdjusterWithFieldInfos) adjuster).setFieldInfos(fieldInfosMap);
            }
        }
    }


    /**
     * Gets the health condition key.
     *
     * @return String
     */
    private String getHealthConditionKey() {
        StringBuilder key = new StringBuilder();
        String conditionTypeCode = getImportingFieldInfo(FIELD_DIS_COND_TYPE_CODE).getValue().trim();
        String medTreatmentType = getImportingFieldInfo(FIELD_MED_TREATMENT_TYPE).getValue().trim();
        String dateFormatCode = getImportingFieldInfo(FIELD_1ST_ENC_DATE_FORMAT).getValue().trim();
        String conditionDate = getImportingFieldInfo(FIELD_1ST_ENC_DATE).getValue().trim();

        if (StringUtils.isEmpty(conditionTypeCode) || StringUtils.isEmpty(medTreatmentType)
                || StringUtils.isEmpty(conditionDate)) {
            return null;
        }

        String dateFormatToParse = getDateFormat(dateFormatCode);
        String formattedConditionDate = parseDate(conditionDate, dateFormatToParse).toString();

        if (formattedConditionDate == null) {
            return null;
        }

        return key.append(conditionTypeCode).append(medTreatmentType).append(formattedConditionDate).toString();
    }


    /**
     * Gets the health condition key.
     *
     * @param condition HealthCondition
     * @return String
     */
    private String getHealthConditionKey(HealthCondition condition) {
        String conditionTypeCodeField = getImportingFieldInfo(FIELD_DIS_COND_TYPE_CODE).getBeanPath();
        String medTreatmentTypeField = getImportingFieldInfo(FIELD_MED_TREATMENT_TYPE).getBeanPath();
        String conditionDateField = getImportingFieldInfo(FIELD_1ST_ENC_DATE).getBeanPath();

        String conditionTypeCode = (String) condition.getFieldValueByBeanPath(conditionTypeCodeField);
        String medTreatmentType = (String) condition.getFieldValueByBeanPath(medTreatmentTypeField);
        String conditionDate = (String) condition.getFieldValueByBeanPath(conditionDateField);

        StringBuilder key = new StringBuilder();
        return key.append(conditionTypeCode).append(medTreatmentType).append(conditionDate).toString();
    }


    /**
     * Gets the new health condition.
     *
     * @return Health condition
     */
    private HealthCondition getNewHealthCondition() {
        HealthCondition condition = X2BaseBean.newInstance(HealthCondition.class, getBroker().getPersistenceKey());
        condition.setFieldValueByBeanPath(HealthCondition.COL_STUDENT_OID, getStudent().getOid());
        return condition;
    }
}
