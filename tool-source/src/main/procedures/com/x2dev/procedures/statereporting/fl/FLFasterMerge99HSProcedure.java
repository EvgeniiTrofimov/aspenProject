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

import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_HLTH_SCR_DATE;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_HLTH_SCR_DATE_FORMAT;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_HLTH_SCR_RESULTS;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_HLTH_SCR_TYPE_CODE;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo;
import com.x2dev.sis.model.beans.HealthScreening;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class FLFasterMerge99HSProcedure.
 */
public class FLFasterMerge99HSProcedure extends RecordTypeMergeData {

    /**
     * The Enum FieldMergeAttributes.
     */
    private enum FieldMergeAttributes implements FieldMergeAttributesInterface {
        HLTH_SCR_TYPE_CODE(FIELD_HLTH_SCR_TYPE_CODE, new FieldMerger("all-hsc-IcdCptScreenType")),
        //
        HLTH_SCR_DATE_FORMAT(FIELD_HLTH_SCR_DATE_FORMAT, new FieldMerger() {
            @Override
            public boolean isMergeNeeded() {
                return false;
            }
        }),
        //
        HLTH_SCR_DATE(FIELD_HLTH_SCR_DATE, new FieldMerger("date"), new ValueAdjusterWithFieldInfos()),
        //
        HLTH_SCR_RESULTS(FIELD_HLTH_SCR_RESULTS, new FieldMerger("resultCode"), new ValueAdjuster() {
            @Override
            public Object getAdjustedValue(String value) {
                if (CODE_FASTER_SCR_RESULT_FAIL.equals(value)) {
                    return CODE_ASPEN_SCR_RESULT_FAIL;
                }
                if (CODE_FASTER_SCR_RESULT_PASS.equals(value)) {
                    return CODE_ASPEN_SCR_RESULT_PASS;
                }
                if (CODE_FASTER_SCR_RESULT_NOT_AVAILABLE.equals(value)) {
                    return null;
                }
                return null;
            }
        });

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
            String dateFormatCode = getFieldInfos().get(FIELD_HLTH_SCR_DATE_FORMAT).getValue().trim();
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

    private static final String CODE_ASPEN_SCR_RESULT_FAIL = "Fail";
    private static final String CODE_ASPEN_SCR_RESULT_PASS = "Pass";

    private static final String CODE_FASTER_SCR_RESULT_FAIL = "ABN";
    private static final String CODE_FASTER_SCR_RESULT_NOT_AVAILABLE = "B33";
    private static final String CODE_FASTER_SCR_RESULT_PASS = "NOR";

    private Map<String, HealthScreening> m_healthScreenings = new HashMap<>();


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanDescriptor(java.lang.String)
     */
    @Override
    protected String getBeanDescriptor(String fieldName) {
        return "Health Screening with type " + getBeanMergeTo(fieldName)
                .getFieldValueByBeanPath(getImportingFieldInfo(FIELD_HLTH_SCR_TYPE_CODE).getBeanPath()) + " and date " +
                getBeanMergeTo(fieldName)
                        .getFieldValueByBeanPath(getImportingFieldInfo(FIELD_HLTH_SCR_DATE).getBeanPath());
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanMergeTo(java.lang.String)
     */
    @Override
    protected X2BaseBean getBeanMergeTo(String fieldName) {
        String key = getHealthScreeningKey();
        if (key == null) {
            return null;
        }

        HealthScreening screening = m_healthScreenings.get(key);
        if (screening == null) {
            screening = getNewHealthScreening();
            m_healthScreenings.put(key, screening);
        }

        return screening;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getClassMergeTo()
     */
    @Override
    protected Class<?> getClassMergeTo() {
        return HealthScreening.class;
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
        loadHealthScreenings();
        assignAdjusterFieldInfos();
    }


    /**
     * Load health screenings.
     */
    protected void loadHealthScreenings() {
        X2Criteria hsCriteria = new X2Criteria();
        hsCriteria.addEqualTo(HealthScreening.COL_STUDENT_OID, getStudent().getOid());
        QueryByCriteria hsQuery = new QueryByCriteria(HealthScreening.class, hsCriteria);
        Collection<HealthScreening> screenings = getBroker().getCollectionByQuery(hsQuery);
        for (HealthScreening screening : screenings) {
            String key = getHealthScreeningKey(screening);
            if (key != null) {
                m_healthScreenings.put(key, screening);
            }
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
     * Gets the health screening key.
     *
     * @return String
     */
    private String getHealthScreeningKey() {
        StringBuilder key = new StringBuilder();
        String typeCode = getImportingFieldInfo(FIELD_HLTH_SCR_TYPE_CODE).getValue().trim();
        String dateFormatCode = getImportingFieldInfo(FIELD_HLTH_SCR_DATE_FORMAT).getValue().trim();
        String screeningDate = getImportingFieldInfo(FIELD_HLTH_SCR_DATE).getValue().trim();

        if (StringUtils.isEmpty(typeCode) || StringUtils.isEmpty(dateFormatCode)
                || StringUtils.isEmpty(screeningDate)) {
            return null;
        }

        String dateFormatToParse = getDateFormat(dateFormatCode);
        String formattedScreeningDate = parseDate(screeningDate, dateFormatToParse).toString();

        if (formattedScreeningDate == null) {
            return null;
        }

        return key.append(typeCode).append(formattedScreeningDate).toString();
    }


    /**
     * Gets the health screening key.
     *
     * @param screening HealthScreening
     * @return String
     */
    private String getHealthScreeningKey(HealthScreening screening) {
        StringBuilder key = new StringBuilder();
        String typeCode = (String) screening
                .getFieldValueByBeanPath(getImportingFieldInfo(FIELD_HLTH_SCR_TYPE_CODE).getBeanPath());
        PlainDate screeningDate =
                (PlainDate) screening.getFieldValueByBeanPath(getImportingFieldInfo(FIELD_HLTH_SCR_DATE).getBeanPath());
        if (StringUtils.isEmpty(typeCode) || screeningDate == null) {
            return null;
        }
        return key.append(typeCode).append(screeningDate).toString();
    }


    /**
     * Gets the new health screening.
     *
     * @return Health screening
     */
    private HealthScreening getNewHealthScreening() {
        HealthScreening screening = X2BaseBean.newInstance(HealthScreening.class, getBroker().getPersistenceKey());
        screening.setFieldValueByBeanPath(HealthScreening.COL_STUDENT_OID, getStudent().getOid());
        return screening;
    }
}
