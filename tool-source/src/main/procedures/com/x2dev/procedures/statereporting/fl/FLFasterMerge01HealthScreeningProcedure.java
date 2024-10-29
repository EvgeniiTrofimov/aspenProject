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

import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_HEARING_PROBLEMS;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_VISION_PROBLEM;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo;
import com.x2dev.sis.model.beans.HealthScreening;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class FLFasterMerge01HealthScreeningProcedure.
 */
public class FLFasterMerge01HealthScreeningProcedure extends RecordTypeMergeData {

    /**
     * The Enum FieldMergeAttributes.
     */
    private enum FieldMergeAttributes implements FieldMergeAttributesInterface {
        HEARING_PROBLEMS(FIELD_HEARING_PROBLEMS, SCREENING_ID_HEARING, new FieldMerger01HS()),
        //
        VISION_PROBLEM(FIELD_VISION_PROBLEM, SCREENING_ID_VISION, new FieldMerger01HS());

        private FieldMerger m_fieldMerger = null;
        private String m_fieldName = null;
        private String m_scrId = null;


        /**
         * Instantiates a new field merge attributes.
         *
         * @param fieldName String
         * @param scrId String
         * @param fieldMerger FieldMerger
         */
        private FieldMergeAttributes(String fieldName, String scrId, FieldMerger fieldMerger) {
            m_fieldName = fieldName;
            m_fieldMerger = fieldMerger;
            m_scrId = scrId;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface#getFieldName()
         */
        @Override
        public String getFieldName() {
            return m_fieldName;
        }


        /**
         * Gets the screening ddx id.
         *
         * @return String
         */
        public String getScreeningDdxId() {
            return m_scrId;
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
            return null;
        }
    }


    /**
     * The Class FieldMerger01HS.
     */
    private static class FieldMerger01HS extends FieldMerger {

        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger#initialize(java.lang.String,
         *      com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData)
         */
        @Override
        public void initialize(String fieldName, RecordTypeMergeData mergeData) {
            super.initialize(fieldName, mergeData);
            FieldInfo importingFieldInfo = mergeData.getImportingFieldInfo(fieldName);

            switch (importingFieldInfo.getValue().substring(1)) {
                case "Y":
                    addFieldToMerge(HealthScreening.COL_RESULT_CODE, CODE_HS_RESULT_PASS);
                    break;

                case "N":
                    addFieldToMerge(HealthScreening.COL_RESULT_CODE, CODE_HS_RESULT_FAIL);
                    addFieldToMerge(importingFieldInfo.getDictionary()
                            .findDataDictionaryFieldByAlias(ALIAS_TREATMENT_RECEIVED).getJavaName(),
                            BooleanAsStringConverter.FALSE);
                    break;

                case "T":
                    addFieldToMerge(HealthScreening.COL_RESULT_CODE, CODE_HS_RESULT_FAIL);
                    addFieldToMerge(importingFieldInfo.getDictionary()
                            .findDataDictionaryFieldByAlias(ALIAS_TREATMENT_RECEIVED).getJavaName(),
                            BooleanAsStringConverter.TRUE);
                    break;

                default:
                    break;
            }
        }
    }

    private static final String ALIAS_TREATMENT_RECEIVED = "all-hsc-TreatmentReceived";

    private static final String CODE_HS_RESULT_FAIL = "Fail";
    private static final String CODE_HS_RESULT_PASS = "Pass";

    private static final String SCREENING_ID_HEARING = "HSC-HEARING";
    private static final String SCREENING_ID_VISION = "HSC-VISION";


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanDescriptor(java.lang.String)
     */
    @Override
    protected String getBeanDescriptor(String fieldName) {
        return "Health Screening with type "
                + ((HealthScreening) getBeanMergeTo(fieldName)).getExtendedDataDictionary().getId();
    }

    private Map<String, X2BaseBean> m_beansForFields = new HashMap<>();

    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanMergeTo(java.lang.String)
     */
    @Override
    protected X2BaseBean getBeanMergeTo(String fieldName) {
        X2BaseBean screening = m_beansForFields.get(fieldName);

        if (screening == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(HealthScreening.COL_STUDENT_OID, getStudent().getOid());
            FieldMergeAttributes mergeAttributes =
                    (FieldMergeAttributes) findFieldMergeAttributesByFieldName(fieldName);
            String ddxOid = getExtendedDataDictionaryById(mergeAttributes.getScreeningDdxId()).getOid();
            criteria.addEqualTo(HealthScreening.COL_EXTENDED_DATA_DICTIONARY_OID, ddxOid);
            screening = getBroker().getBeanByQuery(new QueryByCriteria(HealthScreening.class, criteria));
            if (screening != null) {
                return screening;
            }
            screening = X2BaseBean.newInstance(HealthScreening.class, getBroker().getPersistenceKey());
            screening.setFieldValueByBeanPath(HealthScreening.COL_STUDENT_OID, getStudent().getOid());
            screening.setFieldValueByBeanPath(HealthScreening.COL_EXTENDED_DATA_DICTIONARY_OID, ddxOid);

            m_beansForFields.put(fieldName, screening);
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
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getFieldMergeAttributes()
     */
    @Override
    protected FieldMergeAttributesInterface[] getFieldMergeAttributes() {
        return FieldMergeAttributes.values();
    }
}
