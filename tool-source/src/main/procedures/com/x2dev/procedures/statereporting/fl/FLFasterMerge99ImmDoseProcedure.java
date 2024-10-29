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
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_IMM_STATUS_CODE;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_IMM_TYPE_CODE;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.BeanManager.PersistenceKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.ValidationError;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo;
import com.x2dev.sis.model.beans.HealthImmunizationDose;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class FLFasterMerge99ImmDoseProcedure.
 */
public class FLFasterMerge99ImmDoseProcedure extends RecordTypeMergeData {

    /**
     * The Enum FieldMergeAttributes.
     */
    private enum FieldMergeAttributes implements FieldMergeAttributesInterface {
        IMM_TYPE_CODE(FIELD_IMM_TYPE_CODE, new FieldMerger() {
            @Override
            public boolean isMergeNeeded() {
                String fieldName = getFieldName();
                X2BaseBean bean = getMergeData().getBeanMergeTo(fieldName);
                return bean == null;
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
        IMM_DATE(FIELD_IMM_DATE, new FieldMerger() {
            @Override
            public boolean isMergeNeeded() {
                return false;
            }
        }, new ValueAdjusterWithFieldInfos()),
        //
        IMM_STATUS_CODE(FIELD_IMM_STATUS_CODE, new FieldMergerDose());

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
     * The Class FieldMergerDose.
     */
    private static class FieldMergerDose extends FieldMerger {
        private List<HealthImmunizationDose> m_doses;


        /**
         * Gets the doses.
         *
         * @return List
         */
        public List<HealthImmunizationDose> getDoses() {
            return m_doses;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger#initialize(java.lang.String,
         *      com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData)
         */
        @Override
        public void initialize(String fieldName, RecordTypeMergeData mergeData) {
            super.initialize(fieldName, mergeData);
            FieldInfo importingFieldInfo = getMergeData().getImportingFieldInfo(fieldName);
            X2BaseBean bean = getMergeData().getBeanMergeTo(fieldName);
            m_doses = null;
            String value = importingFieldInfo.getValue();
            if (areDosesDifferent(value, (HealthImmunizationSeries) bean)) {
                int numOfDoses = Integer.parseInt(value);
                Collection<HealthImmunizationDose> doses = ((HealthImmunizationSeries) bean).getImmunizationDoses();
                if (doses != null && numOfDoses > doses.size()) {
                    int numOfDosesToCreate = numOfDoses - doses.size();
                    m_doses = createDoses((HealthImmunizationSeries) bean, numOfDosesToCreate, null,
                            importingFieldInfo.getDictionary().getPersistenceKey());
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
            X2BaseBean bean = getMergeData().getBeanMergeTo(fieldName);
            String value = importingFieldInfo.getValue();
            return bean != null && areDosesDifferent(value, (HealthImmunizationSeries) bean);
        }


        /**
         * Are doses different.
         *
         * @param value String
         * @param series HealthImmunizationSeries
         * @return true, if successful
         */
        private static boolean areDosesDifferent(String value, HealthImmunizationSeries series) {
            if (!s_seriesWithoutDosesCodes.contains(value)) {
                int numOfDoses = Integer.parseInt(value);
                Collection<HealthImmunizationDose> doses = series.getImmunizationDoses();
                if (doses != null && numOfDoses > doses.size()) {
                    return true;
                }
            }
            return false;
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

            return parseDate(value, dateFormat);
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
     * Creates the doses.
     *
     * @param series HealthImmunizationSeries
     * @param numOfDoses int
     * @param dateOfDose PlainDate
     * @param key PersistenceKey
     * @return List
     */
    protected static List<HealthImmunizationDose> createDoses(HealthImmunizationSeries series,
                                                              int numOfDoses,
                                                              PlainDate dateOfDose,
                                                              PersistenceKey key) {
        List<HealthImmunizationDose> doses = new ArrayList<>();
        for (int i = 0; i < numOfDoses; i++) {
            HealthImmunizationDose dose =
                    X2BaseBean.newInstance(HealthImmunizationDose.class, key);
            dose.setImmunizationSeriesOid(series.getOid());
            dose.setStudentOid(series.getStudentOid());
            dose.setDate(dateOfDose);
            doses.add(dose);
        }
        return doses;
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
            throw new X2RuntimeException();
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
        loadHealthImmunizationSeries();
        assignAdjusterFieldInfos();
    }


    /**
     * Load health immunization series.
     */
    protected void loadHealthImmunizationSeries() {
        X2Criteria hisCriteria = new X2Criteria();
        hisCriteria.addEqualTo(HealthImmunizationSeries.COL_STUDENT_OID, getStudent().getOid());
        QueryByCriteria hisQuery = new QueryByCriteria(HealthImmunizationSeries.class, hisCriteria);
        Collection<HealthImmunizationSeries> series = getBroker().getCollectionByQuery(hisQuery);
        for (HealthImmunizationSeries currentSeries : series) {
            String key = getHealthImmunizationSeriesKey(currentSeries);
            m_healthImmunizationSeries.put(key, currentSeries);
        }
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#mergeField(com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger,
     *      com.follett.fsc.core.k12.beans.X2BaseBean,
     *      com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo)
     */
    @Override
    protected List<ValidationError> mergeField(FieldMerger merger, X2BaseBean bean, FieldInfo fieldInfo) {
        if (merger instanceof FieldMergerDose) {
            FieldMergerDose fieldMergerDose = (FieldMergerDose) merger;
            List<HealthImmunizationDose> doses = fieldMergerDose.getDoses();
            for (HealthImmunizationDose dose : doses) {
                PlainDate date = (PlainDate) getAdjustedValue(FIELD_IMM_DATE);
                dose.setFieldValueByBeanPath(HealthImmunizationDose.COL_DATE, date);
                persistChanges(dose, fieldInfo);
            }
        }
        return super.mergeField(merger, bean, fieldInfo);
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#showMergeDescription(com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger,
     *      com.follett.fsc.core.k12.beans.X2BaseBean,
     *      com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo)
     */
    @Override
    protected void showMergeDescription(FieldMerger merger, X2BaseBean beanMergeTo, FieldInfo importingFieldInfo) {
        if (beanMergeTo == null) {
            addMessageLine("\t\tCannot be merged: Health Immunization Series for immunization type code "
                    + getImportingFieldInfo(FIELD_IMM_TYPE_CODE).getValue().trim() + " is not found");
        } else {
            super.showMergeDescription(merger, beanMergeTo, importingFieldInfo);
        }
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanMergeDescription(java.lang.String)
     */
    @Override
    protected String getBeanMergeDescription(String fieldName) {
        if (fieldName.equals(FIELD_IMM_STATUS_CODE)) {
            FieldMergerDose doseMerger = (FieldMergerDose) FieldMergeAttributes.IMM_STATUS_CODE.getMerger();
            List<HealthImmunizationDose> doses = doseMerger.getDoses();
            String dosesDate = getAdjustedValue(FIELD_IMM_DATE).toString();
            String immId = doses.iterator().next().getImmunizationSeries().getImmunizationDefinition().getId();
            return "\t\tNew Health Immunization Doses for immunization series " + immId + " were created with date "
                    + dosesDate + ". Number of new Health Immunization Doses: " + doses.size();
        }
        return super.getBeanMergeDescription(fieldName);
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
        return series.getImmunizationDefinition().getId();
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
