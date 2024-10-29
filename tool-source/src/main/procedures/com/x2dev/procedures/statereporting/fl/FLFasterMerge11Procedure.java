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

import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_MIGRANT_RESID_DATE;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_QA_DATE;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_QA_FROM_CITY;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_QA_FROM_COUNTRY;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_QA_FROM_STATE;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_QA_TO_CITY;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_QA_TO_STATE;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class FLFasterMerge11Procedure.
 */
public class FLFasterMerge11Procedure extends RecordTypeMergeData {

    /**
     * The Enum FieldMergeAttributes.
     */
    private enum FieldMergeAttributes implements FieldMergeAttributesInterface {
        MIGRANT_RESID_DATE(FIELD_MIGRANT_RESID_DATE, new FieldMerger(DDX_ID_MIGRANT, "pgm-residency-date") {
            @Override
            public boolean isMergeNeeded() {
                return false;
            }
        }, s_pgmDateValueAdjuster),
        //
        QA_DATE(FIELD_QA_DATE, new FieldMerger(DDX_ID_MIGRANT, "pgm-qualarrival-date"), s_pgmDateValueAdjuster),
        //
        QA_FROM_CITY(FIELD_QA_FROM_CITY, new FieldMerger(DDX_ID_MIGRANT, "pgm-qad-from-city")),
        //
        QA_FROM_STATE_1(FIELD_QA_FROM_STATE + GROUP_INDEX_1, new FieldMerger(DDX_ID_MIGRANT, "pgm-qad-from-state"),
                new ValueAdjusterWithFieldInfos(GROUP_INDEX_1)),
        //
        QA_FROM_STATE_2(FIELD_QA_FROM_STATE + GROUP_INDEX_2, new FieldMerger(DDX_ID_MIGRANT, "pgm-qad-from-state"),
                new ValueAdjusterWithFieldInfos(GROUP_INDEX_2)),
        //
        QA_FROM_STATE_3(FIELD_QA_FROM_STATE + GROUP_INDEX_3, new FieldMerger(DDX_ID_MIGRANT, "pgm-qad-from-state"),
                new ValueAdjusterWithFieldInfos(GROUP_INDEX_3)),
        //
        QA_FROM_STATE_4(FIELD_QA_FROM_STATE + GROUP_INDEX_4, new FieldMerger(DDX_ID_MIGRANT, "pgm-qad-from-state"),
                new ValueAdjusterWithFieldInfos(GROUP_INDEX_4)),
        //
        QA_FROM_STATE_5(FIELD_QA_FROM_STATE + GROUP_INDEX_5, new FieldMerger(DDX_ID_MIGRANT, "pgm-qad-from-state"),
                new ValueAdjusterWithFieldInfos(GROUP_INDEX_5)),
        //
        QA_FROM_STATE_6(FIELD_QA_FROM_STATE + GROUP_INDEX_6, new FieldMerger(DDX_ID_MIGRANT, "pgm-qad-from-state"),
                new ValueAdjusterWithFieldInfos(GROUP_INDEX_6)),
        //
        QA_FROM_STATE_7(FIELD_QA_FROM_STATE + GROUP_INDEX_7, new FieldMerger(DDX_ID_MIGRANT, "pgm-qad-from-state"),
                new ValueAdjusterWithFieldInfos(GROUP_INDEX_7)),
        //
        QA_FROM_STATE_8(FIELD_QA_FROM_STATE + GROUP_INDEX_8, new FieldMerger(DDX_ID_MIGRANT, "pgm-qad-from-state"),
                new ValueAdjusterWithFieldInfos(GROUP_INDEX_8)),
        //
        QA_FROM_STATE_9(FIELD_QA_FROM_STATE + GROUP_INDEX_9, new FieldMerger(DDX_ID_MIGRANT, "pgm-qad-from-state"),
                new ValueAdjusterWithFieldInfos(GROUP_INDEX_9)),
        //
        QA_FROM_STATE_10(FIELD_QA_FROM_STATE + GROUP_INDEX_10, new FieldMerger(DDX_ID_MIGRANT, "pgm-qad-from-state"),
                new ValueAdjusterWithFieldInfos(GROUP_INDEX_10)),
        //
        QA_FROM_COUNTRY(FIELD_QA_FROM_COUNTRY, new FieldMerger(DDX_ID_MIGRANT, "pgm-qad-from-country")),
        //
        QA_TO_CITY(FIELD_QA_TO_CITY, new FieldMerger(DDX_ID_MIGRANT, "pgm-qad-to-city")),
        //
        QA_TO_STATE(FIELD_QA_TO_STATE, new FieldMerger(DDX_ID_MIGRANT, "pgm-qad-to-state"));

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
        private FieldMergeAttributes(String fieldName, FieldMerger fieldMerger,
                ValueAdjuster valueAdjuster) {
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
        private int m_groupNum;


        /**
         * Instantiates a new value adjuster with field infos.
         *
         * @param groupNum int
         */
        public ValueAdjusterWithFieldInfos(int groupNum) {
            m_groupNum = groupNum;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster#getAdjustedValue(java.lang.String)
         */
        @Override
        public Object getAdjustedValue(String value) {
            if (getFieldInfos() == null) {
                throw new X2RuntimeException();
            }
            String country = getFieldInfos().get(FIELD_QA_FROM_COUNTRY + getGroupNum()).getValue();
            if (!StringUtils.isEmpty(country)) {
                return country + "-" + value;
            }
            return value;
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
         * Gets the group num.
         *
         * @return int
         */
        public int getGroupNum() {
            return m_groupNum;
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

    private static final String DDX_ID_MIGRANT = "FL-PGM-MIGRANT";

    private static final int GROUP_INDEX_1 = 1;
    private static final int GROUP_INDEX_2 = 2;
    private static final int GROUP_INDEX_3 = 3;
    private static final int GROUP_INDEX_4 = 4;
    private static final int GROUP_INDEX_5 = 5;
    private static final int GROUP_INDEX_6 = 6;
    private static final int GROUP_INDEX_7 = 7;
    private static final int GROUP_INDEX_8 = 8;
    private static final int GROUP_INDEX_9 = 9;
    private static final int GROUP_INDEX_10 = 10;

    private static final String PGM_CODE_MIGRANT = "MIGRANT";

    private static final ValueAdjuster s_pgmDateValueAdjuster = new ValueAdjusterPgmDate("MMddyyyy");

    private Map<String, StudentProgramParticipation> m_migrantPrograms = new HashMap<>();


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#beforeMerge(java.lang.String)
     */
    @Override
    protected void beforeMerge(String fieldName) {
        FieldInfo currentFieldInfo = getImportingFieldInfo(fieldName);
        if (currentFieldInfo.getFieldName().startsWith(FIELD_QA_DATE)) {
            FieldInfo migrantDateInfo = getImportingFieldInfo(FIELD_MIGRANT_RESID_DATE);
            FieldMergeAttributes.QA_DATE.getMerger().addFieldToMerge(migrantDateInfo.getBeanPath(),
                    getAdjustedValue(FIELD_MIGRANT_RESID_DATE));
        }
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#findFieldMergeAttributesByFieldName(java.lang.String)
     */
    @Override
    protected FieldMergeAttributesInterface findFieldMergeAttributesByFieldName(String fieldName) {
        FieldMergeAttributes mergeAttributes =
                (FieldMergeAttributes) super.findFieldMergeAttributesByFieldName(fieldName);
        if (mergeAttributes != null) {
            return mergeAttributes;
        }
        String adjustedFieldName = fieldName.replace(getGroupNumToSearch(fieldName), "");

        return super.findFieldMergeAttributesByFieldName(adjustedFieldName);
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanDescriptor(java.lang.String)
     */
    @Override
    protected String getBeanDescriptor(String fieldName) {
        FieldMergeAttributesInterface mergeAttributes = findFieldMergeAttributesByFieldName(fieldName);
        return super.getBeanDescriptor(fieldName) + " for extended dictionary "
                + mergeAttributes.getMerger().getExtendedDictionaryId() + " with Qualifying Arrival Date "
                + getBeanMergeTo(fieldName).getFieldValueByBeanPath(
                        getImportingFieldInfo(FIELD_QA_DATE + getGroupNumToSearch(fieldName)).getBeanPath());
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanMergeTo(java.lang.String)
     */
    @Override
    protected X2BaseBean getBeanMergeTo(String fieldName) {
        if (FIELD_MIGRANT_RESID_DATE.equals(fieldName)) {
            return null;
        }
        Object qaDate = getValueOfGroupField(fieldName, FIELD_QA_DATE);
        if (qaDate == null || StringUtils.isEmpty(qaDate.toString())) {
            return null;
        }
        StudentProgramParticipation migrantProgram = m_migrantPrograms.get(qaDate);
        if (migrantProgram == null) {
            migrantProgram = getNewMigrantProgram();
            m_migrantPrograms.put(qaDate.toString(), migrantProgram);
        }
        return migrantProgram;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getClassMergeTo()
     */
    @Override
    protected Class<?> getClassMergeTo() {
        return StudentProgramParticipation.class;
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
        loadPrograms();
        assignAdjusterFieldInfos();
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
     * Gets the group num to search.
     *
     * @param fieldName String
     * @return String
     */
    private static String getGroupNumToSearch(String fieldName) {
        if (fieldName.startsWith(FIELD_QA_DATE)) {
            return fieldName.replace(FIELD_QA_DATE, "");
        }
        if (fieldName.startsWith(FIELD_QA_FROM_CITY)) {
            return fieldName.replace(FIELD_QA_FROM_CITY, "");
        }
        if (fieldName.startsWith(FIELD_QA_FROM_STATE)) {
            return fieldName.replace(FIELD_QA_FROM_STATE, "");
        }
        if (fieldName.startsWith(FIELD_QA_FROM_COUNTRY)) {
            return fieldName.replace(FIELD_QA_FROM_COUNTRY, "");
        }
        if (fieldName.startsWith(FIELD_QA_TO_CITY)) {
            return fieldName.replace(FIELD_QA_TO_CITY, "");
        }
        if (fieldName.startsWith(FIELD_QA_TO_STATE)) {
            return fieldName.replace(FIELD_QA_TO_STATE, "");
        }

        return fieldName;
    }


    /**
     * Gets the new migrant program.
     *
     * @return Student program participation
     */
    private StudentProgramParticipation getNewMigrantProgram() {
        StudentProgramParticipation newProgram =
                X2BaseBean.newInstance(StudentProgramParticipation.class, getBroker().getPersistenceKey());
        newProgram.setFieldValueByBeanPath(StudentProgramParticipation.COL_STUDENT_OID,
                getStudent().getOid());
        newProgram.setFieldValueByBeanPath(StudentProgramParticipation.COL_EXTENDED_DATA_DICTIONARY_OID,
                getExtendedDataDictionaryById(DDX_ID_MIGRANT).getOid());
        newProgram.setFieldValueByBeanPath(StudentProgramParticipation.COL_PROGRAM_CODE, PGM_CODE_MIGRANT);

        return newProgram;
    }


    /**
     * Gets the value of group field.
     *
     * @param fieldToGetNum String
     * @param fieldToGetValue String
     * @return Object
     */
    private Object getValueOfGroupField(String fieldToGetNum, String fieldToGetValue) {
        String fieldNum = getGroupNumToSearch(fieldToGetNum);
        String fieldToGetValueWithNum = fieldToGetValue + fieldNum;
        return getAdjustedValue(fieldToGetValueWithNum);
    }


    /**
     * Load programs.
     */
    private void loadPrograms() {
        X2Criteria migrantPgmCriteria = new X2Criteria();
        migrantPgmCriteria.addEqualTo(StudentProgramParticipation.COL_STUDENT_OID, getStudent().getOid());
        migrantPgmCriteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, PGM_CODE_MIGRANT);
        migrantPgmCriteria.addEqualTo(StudentProgramParticipation.COL_EXTENDED_DATA_DICTIONARY_OID,
                getDictionaryById(DDX_ID_MIGRANT).getExtendedDictionaryOid());
        QueryByCriteria migrantPgmQuery = new QueryByCriteria(getClassMergeTo(), migrantPgmCriteria);
        Collection<StudentProgramParticipation> migrantPrograms = getBroker().getCollectionByQuery(migrantPgmQuery);
        if (migrantPrograms != null) {
            for (StudentProgramParticipation migrantProgram : migrantPrograms) {
                m_migrantPrograms.put(
                        (String) migrantProgram
                                .getFieldValueByBeanPath(
                                        getImportingFieldInfo(FIELD_QA_DATE + GROUP_INDEX_1).getBeanPath()),
                        migrantProgram);
            }
        }
    }
}
