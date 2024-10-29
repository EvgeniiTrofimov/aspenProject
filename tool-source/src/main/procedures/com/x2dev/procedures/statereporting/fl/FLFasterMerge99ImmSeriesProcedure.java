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

import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_IMM_DATE;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_IMM_DATE_FORMAT;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_IMM_SOURCE_CODE;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_IMM_STATUS_CODE;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_IMM_TYPE_CODE;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo;
import com.x2dev.sis.model.beans.HealthImmunizationDefinition;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class FLFasterMerge99ImmSeriesProcedure.
 */
public class FLFasterMerge99ImmSeriesProcedure extends RecordTypeMergeData {

    /**
     * The Enum FieldMergeAttributes.
     */
    private enum FieldMergeAttributes implements FieldMergeAttributesInterface {
        IMM_TYPE_CODE(FIELD_IMM_TYPE_CODE, new FieldMerger() {
            @Override
            public boolean isMergeNeeded() {
                String fieldName = getFieldName();
                X2BaseBean bean = getMergeData().getBeanMergeTo(fieldName);
                return bean == null || bean.getOid() == null;
            }
        }),
        //
        IMM_DATE_FORMAT(FIELD_IMM_DATE_FORMAT, new FieldMerger() {
            @Override
            public boolean isMergeNeeded() {
                return false;
            }
        }),
        //
        IMM_DATE(FIELD_IMM_DATE, new FieldMerger("all-his-FLImmunizationDate") {
            @Override
            public boolean isMergeNeeded() {
                String fieldName = getFieldName();
                FieldInfo importingFieldInfo = getMergeData().getImportingFieldInfo(fieldName);
                return FLFasterMerge99ImmSeriesProcedure.s_seriesWithoutDosesCodes
                        .contains(importingFieldInfo.getValue().trim())
                        && super.isMergeNeeded();
            }
        }, new ValueAdjusterWithFieldInfos()),
        //
        IMM_STATUS_CODE(FIELD_IMM_STATUS_CODE, new FieldMerger() {
            @Override
            public void initialize(String fieldName, RecordTypeMergeData mergeData) {
                super.initialize(fieldName, mergeData);
                FieldInfo importingFieldInfo = mergeData.getImportingFieldInfo(fieldName);
                DataDictionary dictionary = importingFieldInfo.getDictionary();
                String value = importingFieldInfo.getValue().trim();
                String exemptionBeanPath =
                        dictionary.findDataDictionaryFieldByAlias("all-his-FLExemption").getJavaName();
                String diseaseBeanPath =
                        dictionary.findDataDictionaryFieldByAlias("all-his-FLHadDisease").getJavaName();
                switch (value) {
                    case IMM_STATUS_CODE_MEDICAL_EXEMPTION:
                        addFieldToMerge(exemptionBeanPath, CODE_EXEMPTION_MEDICAL);
                        return;
                    case IMM_STATUS_CODE_PERSONAL_EXEMPTION:
                        addFieldToMerge(exemptionBeanPath, CODE_EXEMPTION_PERSONAL);
                        return;
                    case IMM_STATUS_CODE_RELIGIOUS_EXEMPTION:
                        addFieldToMerge(exemptionBeanPath, CODE_EXEMPTION_RELIGIOUS);
                        return;
                    case IMM_STATUS_CODE_DISEASE:
                        addFieldToMerge(diseaseBeanPath, BooleanAsStringConverter.TRUE);
                        return;
                    case IMM_STATUS_CODE_HAS_NOT_HAD_DISEASE:
                        addFieldToMerge(diseaseBeanPath, BooleanAsStringConverter.FALSE);
                        return;
                    default:
                        break;
                }
            }

            @Override
            public boolean isMergeNeeded() {
                String fieldName = getFieldName();
                FieldInfo importingFieldInfo = getMergeData().getImportingFieldInfo(fieldName);
                X2BaseBean bean = getMergeData().getBeanMergeTo(fieldName);

                DataDictionary dictionary = importingFieldInfo.getDictionary();
                String value = importingFieldInfo.getValue().trim();
                String exemptionBeanPath =
                        dictionary.findDataDictionaryFieldByAlias("all-his-FLExemption").getJavaName();
                String diseaseBeanPath =
                        dictionary.findDataDictionaryFieldByAlias("all-his-FLHadDisease").getJavaName();
                if (bean == null) {
                    return false;
                }
                switch (value) {
                    case IMM_STATUS_CODE_MEDICAL_EXEMPTION:
                        return !CODE_EXEMPTION_MEDICAL.equals(bean.getFieldValueByBeanPath(exemptionBeanPath));
                    case IMM_STATUS_CODE_PERSONAL_EXEMPTION:
                        return !CODE_EXEMPTION_PERSONAL.equals(bean.getFieldValueByBeanPath(exemptionBeanPath));
                    case IMM_STATUS_CODE_RELIGIOUS_EXEMPTION:
                        return !CODE_EXEMPTION_RELIGIOUS.equals(bean.getFieldValueByBeanPath(exemptionBeanPath));
                    case IMM_STATUS_CODE_DISEASE:
                        return !BooleanAsStringConverter.TRUE.equals(bean.getFieldValueByBeanPath(diseaseBeanPath));
                    case IMM_STATUS_CODE_HAS_NOT_HAD_DISEASE:
                        return !BooleanAsStringConverter.FALSE.equals(bean.getFieldValueByBeanPath(diseaseBeanPath));
                    default:
                        return false;
                }
            }
        }),
        //
        IMM_SOURCE_CODE(FIELD_IMM_SOURCE_CODE, new FieldMerger("all-his-FLSourceCode"));

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
            String dateFormatCode = getFieldInfos().get(FIELD_IMM_DATE_FORMAT).getValue().trim();
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

    public static final String REF_TABLE_IMMUNIZATION_TYPES_TO_FASTER = "FL ICD CPT Immunization Types";

    protected static final String CODE_EXEMPTION_MEDICAL = "Medical";
    protected static final String CODE_EXEMPTION_PERSONAL = "Personal";
    protected static final String CODE_EXEMPTION_RELIGIOUS = "Religious";

    protected static final String IMM_STATUS_CODE_DISEASE = "13";
    protected static final String IMM_STATUS_CODE_HAS_NOT_HAD_DISEASE = "14";
    protected static final String IMM_STATUS_CODE_MEDICAL_EXEMPTION = "10";
    protected static final String IMM_STATUS_CODE_PERSONAL_EXEMPTION = "11";
    protected static final String IMM_STATUS_CODE_RELIGIOUS_EXEMPTION = "12";

    protected static final List<String> s_seriesWithoutDosesCodes = Arrays.asList(IMM_STATUS_CODE_DISEASE,
            IMM_STATUS_CODE_HAS_NOT_HAD_DISEASE, IMM_STATUS_CODE_MEDICAL_EXEMPTION, IMM_STATUS_CODE_PERSONAL_EXEMPTION,
            IMM_STATUS_CODE_RELIGIOUS_EXEMPTION);

    private Map<String, HealthImmunizationSeries> m_healthImmunizationSeries = new HashMap<>();
    private Map<String, HealthImmunizationDefinition> m_immDefinitionById = new HashMap<>();
    private Map<String, Map<String, String>> m_refCodesDependencyCode = null;


    /**
     * Find dependency by code.
     *
     * @param refTableName String
     * @param code String
     * @return String
     */
    public String findDependencyByCode(String refTableName, String code) {
        return getRefCodeDepCodesMap(refTableName).get(code);
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanDescriptor(java.lang.String)
     */
    @Override
    protected String getBeanDescriptor(String fieldName) {
        HealthImmunizationSeries series = (HealthImmunizationSeries) getBeanMergeTo(fieldName);
        return "Health Immunization Series with Series ID " + series.getImmunizationDefinition().getId();
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanMergeTo(java.lang.String)
     */
    @Override
    protected X2BaseBean getBeanMergeTo(String fieldName) {
        String key = getHealthImmunizationSeriesKey();
        if (key == null) {
            return null;
        }

        HealthImmunizationSeries series = m_healthImmunizationSeries.get(key);
        if (series == null) {
            series = getNewHealthImmunizationSeries();
            m_healthImmunizationSeries.put(key, series);
        }

        return series;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getClassMergeTo()
     */
    @Override
    protected Class<?> getClassMergeTo() {
        return HealthImmunizationSeries.class;
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
        loadHealthImmunizationDefinitions();
        loadHealthImmunizationSeries();
        assignAdjusterFieldInfos();
    }


    /**
     * Load health immunization definitions.
     */
    protected void loadHealthImmunizationDefinitions() {
        X2Criteria criteria = new X2Criteria();
        QueryByCriteria query = new QueryByCriteria(HealthImmunizationDefinition.class, criteria);
        m_immDefinitionById = getBroker().getMapByQuery(query, HealthImmunizationDefinition.COL_SERIES_ID, 20);
    }


    /**
     * Load health immunization series.
     */
    protected void loadHealthImmunizationSeries() {
        X2Criteria hisCriteria = new X2Criteria();
        hisCriteria.addEqualTo(HealthImmunizationSeries.COL_STUDENT_OID, getStudent().getOid());
        QueryByCriteria hisQuery = new QueryByCriteria(HealthImmunizationSeries.class, hisCriteria);
        Collection<HealthImmunizationSeries> seriess = getBroker().getCollectionByQuery(hisQuery);
        for (HealthImmunizationSeries series : seriess) {
            String key = getHealthImmunizationSeriesKey(series);
            m_healthImmunizationSeries.put(key, series);
        }
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#showMergeDescription(com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger,
     *      com.follett.fsc.core.k12.beans.X2BaseBean,
     *      com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo)
     */
    @Override
    protected void showMergeDescription(FieldMerger merger, X2BaseBean beanMergeTo, FieldInfo importingFieldInfo) {
        if (beanMergeTo == null) {
            addMessageLine("\t\tCannot be merged: Health Immunization Definition for immunization type code "
                    + getImportingFieldInfo(FIELD_IMM_TYPE_CODE).getValue().trim() + " is not found");
        } else {
            super.showMergeDescription(merger, beanMergeTo, importingFieldInfo);
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
     * Gets the health immunization series key.
     *
     * @return String
     */
    private String getHealthImmunizationSeriesKey() {
        String immTypeCode = getImportingFieldInfo(FIELD_IMM_TYPE_CODE).getValue().trim();

        if (StringUtils.isEmpty(immTypeCode)) {
            return null;
        }

        return findDependencyByCode(REF_TABLE_IMMUNIZATION_TYPES_TO_FASTER, immTypeCode);
    }


    /**
     * Gets the health immunization series key.
     *
     * @param series HealthImmunizationSeries
     * @return String
     */
    private String getHealthImmunizationSeriesKey(HealthImmunizationSeries series) {
        String immId = series.getImmunizationDefinition().getId();

        if (StringUtils.isEmpty(immId)) {
            return null;
        }

        return immId;
    }


    /**
     * Gets the new health immunization series.
     *
     * @return Health immunization series
     */
    private HealthImmunizationSeries getNewHealthImmunizationSeries() {
        HealthImmunizationSeries series =
                X2BaseBean.newInstance(HealthImmunizationSeries.class, getBroker().getPersistenceKey());
        series.setFieldValueByBeanPath(HealthImmunizationSeries.COL_STUDENT_OID, getStudent().getOid());
        String immTypeCode = getImportingFieldInfo(FIELD_IMM_TYPE_CODE).getValue().trim();
        String immId = findDependencyByCode(REF_TABLE_IMMUNIZATION_TYPES_TO_FASTER, immTypeCode);
        if (immId == null) {
            return null;
        }
        HealthImmunizationDefinition immDefinition = m_immDefinitionById.get(immId);
        series.setFieldValueByBeanPath(HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID,
                immDefinition.getOid());
        return series;
    }


    /**
     * Gets the ref code dep codes map.
     *
     * @param refTableName String
     * @return Map
     */
    private Map<String, String> getRefCodeDepCodesMap(String refTableName) {
        if (m_refCodesDependencyCode == null) {
            m_refCodesDependencyCode = new HashMap<>();
        }
        if (m_refCodesDependencyCode.get(refTableName) == null) {
            Map<String, String> codeDependency = new HashMap<>();
            m_refCodesDependencyCode.put(refTableName, codeDependency);
            X2Criteria refCodesCriteria = new X2Criteria();
            refCodesCriteria.addEqualTo(
                    ReferenceCode.REL_REFERENCE_TABLE + ModelProperty.PATH_DELIMITER + ReferenceTable.COL_USER_NAME,
                    refTableName);
            QueryByCriteria refCodesQuery = new QueryByCriteria(ReferenceCode.class, refCodesCriteria);
            Collection<ReferenceCode> refCodes = getBroker().getCollectionByQuery(refCodesQuery);
            for (ReferenceCode refCode : refCodes) {
                codeDependency.put(refCode.getCode(), refCode.getDependencyCode());
            }
        }
        return m_refCodesDependencyCode.get(refTableName);
    }
}
