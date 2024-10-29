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

import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_STATUS_DATE;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo;
import com.x2dev.sis.model.beans.HealthImmunizationDefinition;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class FLFasterMerge02ImmunizationSeriesProcedure.
 */
public class FLFasterMerge02ImmunizationSeriesProcedure extends RecordTypeMergeData {

    /**
     * The Class FieldMergerSeries.
     */
    private static class FieldMergerSeries extends FieldMerger {
        private String m_fieldExemption = null;
        private String m_fieldImmunizedEligibleForm = null;
        private Map<String, Map<String, Object>> m_fieldsToMergeBySeriesId = new HashMap<>();


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger#getBeanPath()
         */
        @Override
        public String getBeanPath() {
            return null;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger#getExtendedDictionaryId()
         */
        @Override
        public String getExtendedDictionaryId() {
            return null;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger#initialize(java.lang.String,
         *      com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData)
         */
        @Override
        public void initialize(String fieldName, RecordTypeMergeData mergeData) {
            super.initialize(fieldName, mergeData);
            X2BaseBean bean = getMergeData().getBeanMergeTo(fieldName);
            FieldInfo importingFieldInfo = getMergeData().getImportingFieldInfo(fieldName);

            String value = importingFieldInfo.getValue();
            String vaccineType = value.substring(0, 1);
            String vaccineDate = value.substring(2);
            Map<String, Object> fieldsForSeries = m_fieldsToMergeBySeriesId.get(vaccineType);
            if (fieldsForSeries == null) {
                fieldsForSeries = new HashMap<>();
                m_fieldsToMergeBySeriesId.put(vaccineType, fieldsForSeries);
            }

            HealthImmunizationSeries series = (HealthImmunizationSeries) bean;

            if (CODE_DATE_WO_DATE.equals(vaccineDate)) {
                String isImmunizedEligibleForm =
                        (String) series.getFieldValueByAlias(ALIAS_HIS_IMMUNIZED_ELIGIBLE_FORM);
                if (isImmunizedEligibleForm == null || BooleanAsStringConverter.TRUE.equals(isImmunizedEligibleForm)) {
                    if (m_fieldImmunizedEligibleForm == null) {
                        m_fieldImmunizedEligibleForm = importingFieldInfo.getDictionary()
                                .findDataDictionaryFieldByAlias(ALIAS_HIS_IMMUNIZED_ELIGIBLE_FORM).getJavaName();
                    }
                    fieldsForSeries.put(m_fieldImmunizedEligibleForm, BooleanAsStringConverter.TRUE);
                }
            }

            if (CODE_DATE_EXEMPT.equals(vaccineDate)) {
                String exemptionCode = (String) series.getFieldValueByAlias(ALIAS_HIS_EXEMPTION);
                if (StringUtils.isEmpty(exemptionCode)) {
                    if (m_fieldExemption == null) {
                        m_fieldExemption = importingFieldInfo.getDictionary()
                                .findDataDictionaryFieldByAlias(ALIAS_HIS_EXEMPTION).getJavaName();
                    }
                    // we cannot determine code of exemption here so use "Personal" for
                    // now
                    fieldsForSeries.put(m_fieldExemption, "Personal");
                }
            }
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger#isMergeNeeded()
         */
        @Override
        public boolean isMergeNeeded() {
            String fieldName = getFieldName();
            X2BaseBean bean = getMergeData().getBeanMergeTo(fieldName);
            FieldInfo importingFieldInfo = getMergeData().getImportingFieldInfo(fieldName);

            HealthImmunizationSeries series = (HealthImmunizationSeries) bean;

            if (series == null) {
                return false;
            }

            String value = importingFieldInfo.getValue();
            String vaccineType = value.substring(0, 1);

            if (!vaccineType.matches(PATTERN_VACCINE_TYPE)) {
                return false;
            }

            String vaccineDate = value.substring(2);

            if (series.getOid() == null) {
                return true;
            }

            if (CODE_DATE_WO_DATE.equals(vaccineDate)) {
                String isImmunizedEligibleForm =
                        (String) series.getFieldValueByAlias(ALIAS_HIS_IMMUNIZED_ELIGIBLE_FORM);
                if (StringUtils.isEmpty(isImmunizedEligibleForm)
                        || !BooleanAsStringConverter.TRUE.equals(isImmunizedEligibleForm)) {
                    return true;
                }
            }

            if (CODE_DATE_EXEMPT.equals(vaccineDate)) {
                String exemptionCode = (String) series.getFieldValueByAlias(ALIAS_HIS_EXEMPTION);
                if (StringUtils.isEmpty(exemptionCode)) {
                    return true;
                }
            }

            return false;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger#merge()
         */
        @Override
        public void merge() {
            String fieldName = getFieldName();
            FieldInfo importingFieldInfo = getMergeData().getImportingFieldInfo(fieldName);
            X2BaseBean beanMergeTo = getMergeData().getBeanMergeTo(fieldName);

            String value = importingFieldInfo.getValue();
            String vaccineType = value.substring(0, 1);
            for (Entry<String, Object> fieldToMerge : m_fieldsToMergeBySeriesId.get(vaccineType).entrySet()) {
                beanMergeTo.setFieldValueByBeanPath(fieldToMerge.getKey(), fieldToMerge.getValue());
            }
        }
    }

    private static final String ALIAS_HIS_EXEMPTION = "all-his-FLExemption";
    private static final String ALIAS_HIS_IMMUNIZED_ELIGIBLE_FORM = "all-his-FLImmunizedEligibleForm";

    private static final String CODE_DATE_EXEMPT = "EXEMPT  ";
    private static final String CODE_DATE_WO_DATE = "99999999";

    private static final String PATTERN_VACCINE_TYPE = "^[A-M]$";

    private static final String REF_TABLE_NAME_VACCINE_TYPE = "FL Vaccine Type";

    private static FieldMergeAttributesInterface s_mergeAttributes = new FieldMergeAttributesInterface() {
        @Override
        public ValueAdjuster getValueAdjuster() {
            return null;
        }

        @Override
        public FieldMerger getMerger() {
            return new FieldMergerSeries();
        }

        @Override
        public String getFieldName() {
            return null;
        }
    };

    private FieldMerger m_fieldMergerSeries = null;
    private Map<String, HealthImmunizationSeries> m_seriesById = new HashMap<>();
    private Map<String, ReferenceCode> m_vaccineTypeCodes = null;


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#findFieldMergeAttributesByFieldName(java.lang.String)
     */
    @Override
    protected FieldMergeAttributesInterface findFieldMergeAttributesByFieldName(String fieldName) {
        if (fieldName.startsWith(FIELD_STATUS_DATE)) {
            return s_mergeAttributes;
        }
        return null;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanDescriptor(java.lang.String)
     */
    @Override
    protected String getBeanDescriptor(String fieldName) {
        return "Health Immunization Series with type "
                + ((HealthImmunizationSeries) getBeanMergeTo(fieldName)).getImmunizationDefinition().getId();
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanMergeTo(java.lang.String)
     */
    @Override
    protected X2BaseBean getBeanMergeTo(String fieldName) {
        FieldInfo importingFieldInfo = getFieldInfosForPlainRow(getImportingPlainRow()).get(fieldName);
        String statusDate = importingFieldInfo.getValue().trim();
        if (StringUtils.isEmpty(statusDate)) {
            return null;
        }
        String vaccineTypeStateCode = statusDate.substring(0, 1);

        HealthImmunizationSeries series = m_seriesById.get(vaccineTypeStateCode);
        if (series == null) {
            X2Criteria seriesCriteria = new X2Criteria();
            seriesCriteria.addEqualTo(HealthImmunizationSeries.COL_STUDENT_OID, getStudent().getOid());
            ReferenceCode vaccineTypeCode = getVaccineTypeByStateCode(vaccineTypeStateCode);
            if (vaccineTypeCode != null) {
                seriesCriteria
                        .addEqualTo(HealthImmunizationSeries.REL_IMMUNIZATION_DEFINITION + ModelProperty.PATH_DELIMITER
                                + HealthImmunizationDefinition.COL_SERIES_ID, vaccineTypeCode.getCode());
            }
            QueryByCriteria seriesQuery = new QueryByCriteria(HealthImmunizationSeries.class, seriesCriteria);
            series = (HealthImmunizationSeries) getBroker().getBeanByQuery(seriesQuery);

            if (series == null) {
                series = X2BaseBean.newInstance(HealthImmunizationSeries.class, getBroker().getPersistenceKey());
                X2Criteria definitionCriteria = new X2Criteria();
                definitionCriteria.addEqualTo(HealthImmunizationDefinition.COL_SERIES_ID, vaccineTypeCode.getCode());
                QueryByCriteria definitionQuery =
                        new QueryByCriteria(HealthImmunizationDefinition.class, definitionCriteria);
                HealthImmunizationDefinition definition =
                        (HealthImmunizationDefinition) getBroker().getBeanByQuery(definitionQuery);
                if (definition == null) {
                    throw new X2RuntimeException();
                }
                series.setFieldValueByBeanPath(HealthImmunizationSeries.COL_STUDENT_OID, getStudent().getOid());
                series.setFieldValueByBeanPath(HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID,
                        definition.getOid());
            }

            m_seriesById.put(vaccineTypeStateCode, series);
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
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getFieldMergeAttributes()
     */
    @Override
    protected FieldMergeAttributesInterface[] getFieldMergeAttributes() {
        throw new X2RuntimeException();
    }


    /**
     * Gets the vaccine type by state code.
     *
     * @param stateCode String
     * @return Reference code
     */
    private ReferenceCode getVaccineTypeByStateCode(String stateCode) {
        if (m_vaccineTypeCodes == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(
                    ReferenceCode.REL_REFERENCE_TABLE + ModelProperty.PATH_DELIMITER + ReferenceTable.COL_USER_NAME,
                    REF_TABLE_NAME_VACCINE_TYPE);
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
            m_vaccineTypeCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 10);
        }
        return m_vaccineTypeCodes.get(stateCode);
    }
}
