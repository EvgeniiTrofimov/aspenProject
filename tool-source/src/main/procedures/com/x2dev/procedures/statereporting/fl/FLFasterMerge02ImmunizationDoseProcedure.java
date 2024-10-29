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
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.ibm.icu.text.SimpleDateFormat;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo;
import com.x2dev.sis.model.beans.HealthImmunizationDefinition;
import com.x2dev.sis.model.beans.HealthImmunizationDose;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class FLFasterMerge02ImmunizationDoseProcedure.
 */
public class FLFasterMerge02ImmunizationDoseProcedure extends RecordTypeMergeData {

    /**
     * The Class FieldMergerDose.
     */
    private static class FieldMergerDose extends FieldMerger {

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

            if (isMergeNeeded()) {
                FieldInfo importingFieldInfo = getMergeData().getImportingFieldInfo(fieldName);

                String value = importingFieldInfo.getValue();
                String vaccineDate = value.substring(2);

                try {
                    addFieldToMerge(HealthImmunizationDose.COL_DATE, new PlainDate(s_format.parse(vaccineDate)));
                } catch (ParseException e) {
                    e.printStackTrace();
                }
            }
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger#isMergeNeeded()
         */
        @Override
        public boolean isMergeNeeded() {
            String fieldName = getFieldName();
            FieldInfo importingFieldInfo = getMergeData().getImportingFieldInfo(fieldName);
            String vaccineType = importingFieldInfo.getValue().substring(0, 1);
            if (!vaccineType.matches(PATTERN_VACCINE_TYPE)) {
                return false;
            }
            X2BaseBean bean = getMergeData().getBeanMergeTo(fieldName);
            HealthImmunizationDose dose = (HealthImmunizationDose) bean;
            if (dose == null) {
                return false;
            }
            String doseDate = importingFieldInfo.getValue().substring(2);
            try {
                s_format.parse(doseDate);
            } catch (ParseException e) {
                return false;
            }
            if (dose.getOid() != null) {
                return false;
            }
            return true;
        }
    }


    /**
     * The Class ValueAdjusterDate.
     */
    private static class ValueAdjusterDate extends ValueAdjuster {


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster#getAdjustedValue(java.lang.String)
         */
        @Override
        public Object getAdjustedValue(String value) {
            try {
                String doseDate = value.substring(2);
                return new PlainDate(s_format.parse(doseDate));
            } catch (ParseException e) {
                e.printStackTrace();
            }
            return null;
        }

    }

    private static final String PATTERN_VACCINE_TYPE = "^[A-M]$";
    private static final String REF_TABLE_NAME_VACCINE_TYPE = "FL Vaccine Type";

    private static final SimpleDateFormat s_format = new SimpleDateFormat("yyyyMMdd");
    static {
        s_format.setLenient(false);
    }

    private static final FieldMergeAttributesInterface s_mergeAttributes = new FieldMergeAttributesInterface() {

        @Override
        public ValueAdjuster getValueAdjuster() {
            return s_valueAdjusterDate;
        }

        @Override
        public FieldMerger getMerger() {
            return new FieldMergerDose();
        }

        @Override
        public String getFieldName() {
            return null;
        }
    };

    private static final ValueAdjuster s_valueAdjusterDate = new ValueAdjusterDate();

    private Map<String, HealthImmunizationSeries> m_seriesById = new HashMap<>();
    private Map<String, Map<PlainDate, HealthImmunizationDose>> m_dosesById = new HashMap<>();
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
        FieldInfo importingFieldInfo = getFieldInfosForPlainRow(getImportingPlainRow()).get(fieldName);
        String vaccineType = importingFieldInfo.getValue().substring(0, 1);
        return super.getBeanDescriptor(fieldName) + " for vaccine type "
                + m_seriesById.get(vaccineType).getImmunizationDefinition().getId();
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

        String vaccineType = importingFieldInfo.getValue().substring(0, 1);

        HealthImmunizationSeries series = m_seriesById.get(vaccineType);

        if (series == null) {
            throw new X2RuntimeException();
        }

        HealthImmunizationDose dose = null;
        String doseDate = importingFieldInfo.getValue().substring(2);
        PlainDate doseDatePlain = null;
        try {
            doseDatePlain = new PlainDate(s_format.parse(doseDate));
        } catch (Exception e) {
            return null;
        }
        if (doseDatePlain != null) {
            dose = findDose(vaccineType, doseDatePlain);
        }

        if (dose == null) {
            dose = X2BaseBean.newInstance(HealthImmunizationDose.class, getBroker().getPersistenceKey());
            dose.setFieldValueByBeanPath(HealthImmunizationDose.COL_STUDENT_OID, getStudent().getOid());
            dose.setFieldValueByBeanPath(HealthImmunizationDose.COL_IMMUNIZATION_SERIES_OID, series.getOid());

            m_dosesById.get(vaccineType).put(doseDatePlain, dose);
        }

        return dose;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getClassMergeTo()
     */
    @Override
    protected Class<?> getClassMergeTo() {
        return HealthImmunizationDose.class;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getFieldMergeAttributes()
     */
    @Override
    protected FieldMergeAttributesInterface[] getFieldMergeAttributes() {
        throw new X2RuntimeException();
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        loadSeries();
    }

    /**
     * Find dose.
     *
     * @param seriesId String
     * @param date PlainDate
     * @return HealthImmunizationDose
     */
    private HealthImmunizationDose findDose(String seriesId, PlainDate date) {
        Map<PlainDate, HealthImmunizationDose> doses = m_dosesById.get(seriesId);
        if (doses == null) {
            doses = new HashMap<>();
            m_dosesById.put(seriesId, doses);
            HealthImmunizationSeries series = m_seriesById.get(seriesId);

            Collection<HealthImmunizationDose> seriesDoses = series.getImmunizationDoses();
            if (seriesDoses != null) {
                for (HealthImmunizationDose dose : seriesDoses) {
                    PlainDate doseDate = dose.getDate();
                    if (doseDate != null) {
                        doses.put(dose.getDate(), dose);
                    }
                }
            }
            m_dosesById.put(seriesId, doses);
        }

        return doses.get(date);
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

    /**
     * Load series.
     *
     * @throws X2BaseException exception
     */
    private void loadSeries() throws X2BaseException {
        X2Criteria seriesCriteria = new X2Criteria();
        seriesCriteria.addEqualTo(HealthImmunizationSeries.COL_STUDENT_OID, getStudent().getOid());
        QueryByCriteria seriesQuery = new QueryByCriteria(HealthImmunizationSeries.class, seriesCriteria);
        Collection<HealthImmunizationSeries> seriesCollection = getBroker().getCollectionByQuery(seriesQuery);
        if (seriesCollection != null) {
            for (HealthImmunizationSeries currentSeries : seriesCollection) {
                DataDictionaryField vaccineTypeField = getFieldByBeanPath(HealthImmunizationDefinition.class,
                        HealthImmunizationDefinition.COL_SERIES_ID, getDictionary());
                HealthImmunizationDefinition immDefinition = currentSeries.getImmunizationDefinition();
                String vaccineType = (String) getFieldValue(immDefinition, vaccineTypeField);
                if (!StringUtils.isEmpty(vaccineType)) {
                    m_seriesById.put(vaccineType, currentSeries);
                }
            }
        }
    }
}
