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

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import org.apache.commons.lang3.StringUtils;


/**
 * The Class FLFasterMerge02StatusToStudentProcedure.
 */
public class FLFasterMerge02StatusToStudentProcedure extends RecordTypeMergeData {

    /**
     * The Class FieldMergerVaccineType8.
     */
    private static class FieldMergerVaccineType8 extends FieldMerger {
        private static final String ALIAS_TO_MERGE = "all-std-FLImmunizedEligibleForm";
        private static final String VACCINE_TYPE = "8";


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger#getBeanPath()
         */
        @Override
        public String getBeanPath() {
            return ALIAS_TO_MERGE;
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
            FieldInfo importingFieldInfo = getMergeData().getImportingFieldInfo(fieldName);
            addFieldToMerge(importingFieldInfo.getBeanPath(), BooleanAsStringConverter.TRUE);
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger#isMergeNeeded()
         */
        @Override
        public boolean isMergeNeeded() {
            String fieldName = getFieldName();
            X2BaseBean bean = getMergeData().getBeanMergeTo(fieldName);

            String vaccineType = getMergeData().getImportingFieldValue(fieldName).substring(0, 1);
            if (!VACCINE_TYPE.equals(vaccineType)) {
                return false;
            }
            FieldInfo importingFieldInfo = getMergeData().getImportingFieldInfo(fieldName);
            String isEligible = (String) bean.getFieldValueByBeanPath(importingFieldInfo.getBeanPath());
            if (isEligible == null || !BooleanAsStringConverter.TRUE.equals(isEligible)) {
                return true;
            }
            return false;
        }
    }


    /**
     * The Class FieldMergerVaccineType9.
     */
    private static class FieldMergerVaccineType9 extends FieldMerger {
        private static final String ALIAS_TO_MERGE = "all-std-FLExemption";
        private static final String VACCINE_TYPE = "9";

        private String m_beanPath = null;


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger#getBeanPath()
         */
        @Override
        public String getBeanPath() {
            return ALIAS_TO_MERGE;
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
            FieldInfo importingFieldInfo = getMergeData().getImportingFieldInfo(fieldName);
            addFieldToMerge(importingFieldInfo.getBeanPath(), "Personal");
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger#isMergeNeeded()
         */
        @Override
        public boolean isMergeNeeded() {
            String fieldName = getFieldName();
            X2BaseBean bean = getMergeData().getBeanMergeTo(fieldName);

            String vaccineType = getMergeData().getImportingFieldValue(fieldName).substring(0, 1);
            if (!VACCINE_TYPE.equals(vaccineType)) {
                return false;
            }
            FieldInfo importingFieldInfo = getMergeData().getImportingFieldInfo(fieldName);
            String exemption = (String) bean.getFieldValueByBeanPath(importingFieldInfo.getBeanPath());
            if (StringUtils.isEmpty(exemption)) {
                return true;
            }
            return false;
        }
    }

    private static final String REF_TABLE_NAME_VACCINE_TYPE = "FL Vaccine Type";

    private static final FieldMergeAttributesInterface s_mergeAttributes8 = new FieldMergeAttributesInterface() {
        private FieldMerger m_fieldMerger = null;

        {
            m_fieldMerger = new FieldMergerVaccineType8();
        }

        @Override
        public ValueAdjuster getValueAdjuster() {
            return null;
        }

        @Override
        public FieldMerger getMerger() {
            return m_fieldMerger;
        }

        @Override
        public String getFieldName() {
            return null;
        }
    };

    private static final FieldMergeAttributesInterface s_mergeAttributes9 = new FieldMergeAttributesInterface() {

        private FieldMerger m_fieldMerger = null;

        {
            m_fieldMerger = new FieldMergerVaccineType9();
        }

        @Override
        public ValueAdjuster getValueAdjuster() {
            return null;
        }

        @Override
        public FieldMerger getMerger() {
            return m_fieldMerger;
        }

        @Override
        public String getFieldName() {
            return null;
        }
    };


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#findFieldMergeAttributesByFieldName(java.lang.String)
     */
    @Override
    protected FieldMergeAttributesInterface findFieldMergeAttributesByFieldName(String fieldName) {
        FieldInfo importingFieldInfo = getFieldInfo(fieldName, getImportingPlainRow(), null, null);
        String value = importingFieldInfo.getValue();
        String vaccineType = value.substring(0, 1);
        if ("8".equals(vaccineType)) {
            return s_mergeAttributes8;
        }
        if ("9".equals(vaccineType)) {
            return s_mergeAttributes9;
        }
        return null;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanMergeTo(java.lang.String)
     */
    @Override
    protected X2BaseBean getBeanMergeTo(String fieldName) {
        return getStudent();
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getClassMergeTo()
     */
    @Override
    protected Class<?> getClassMergeTo() {
        return SisStudent.class;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getFieldMergeAttributes()
     */
    @Override
    protected FieldMergeAttributesInterface[] getFieldMergeAttributes() {
        throw new X2RuntimeException();
    }
}
