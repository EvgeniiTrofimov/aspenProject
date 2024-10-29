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

import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_RACE_AFRICAN_BLACK;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_RACE_AMERICAALASKAINDIAN;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_RACE_ASIAN;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_RACE_PACIFIC_ISLANDER;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_RACE_WHITE;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo;
import java.util.HashMap;
import java.util.Map;


/**
 * The Class FLFasterMerge01RaceProcedure.
 */
public class FLFasterMerge01RaceProcedure extends RecordTypeMergeData {


    /**
     * The Enum FieldMergeAttributes.
     */
    private enum FieldMergeAttributes implements FieldMergeAttributesInterface {
        RACE_AMERICAALASKAINDIAN(FIELD_RACE_AMERICAALASKAINDIAN, new FieldMergerRace(), "I"),
        //
        RACE_ASIAN(FIELD_RACE_ASIAN, new FieldMergerRace(), "A"),
        //
        RACE_AFRICAN_BLACK(FIELD_RACE_AFRICAN_BLACK, new FieldMergerRace(), "B"),
        //
        RACE_PACIFIC_ISLANDER(FIELD_RACE_PACIFIC_ISLANDER, new FieldMergerRace(), "P"),
        //
        RACE_WHITE(FIELD_RACE_WHITE, new FieldMergerRace(), "W");

        private FieldMergerRace m_fieldMerger = null;
        private String m_fieldName = null;
        private String m_raceCode = null;


        /**
         * Instantiates a new field merge attributes.
         *
         * @param fieldName String
         * @param fieldMerger FieldMergerRace
         * @param raceCode String
         */
        private FieldMergeAttributes(String fieldName, FieldMergerRace fieldMerger, String raceCode) {
            m_fieldName = fieldName;
            m_fieldMerger = fieldMerger;
            m_raceCode = raceCode;
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
        public FieldMergerRace getMerger() {
            return m_fieldMerger;
        }


        /**
         * Gets the race code.
         *
         * @return String
         */
        public String getRaceCode() {
            return m_raceCode;
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
     * The Class FieldMergerRace.
     */
    private static class FieldMergerRace extends FieldMerger {

        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger#initialize(java.lang.String,
         *      com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData)
         */
        @Override
        public void initialize(String fieldName, RecordTypeMergeData mergeData) {
            super.initialize(fieldName, mergeData);
            FieldMergeAttributes mergeAttributes =
                    (FieldMergeAttributes) getMergeData().findFieldMergeAttributesByFieldName(fieldName);
            addFieldToMerge(Race.COL_RACE_CODE, mergeAttributes.getRaceCode());
        }

        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger#isMergeNeeded()
         */
        @Override
        public boolean isMergeNeeded() {
            String fieldName = getFieldName();
            String currentPlainRow = getMergeData().getCurrentPlainRow();
            FieldInfo currentFieldInfo = getMergeData().getFieldInfo(fieldName, currentPlainRow);
            FieldInfo importingFieldInfo = getMergeData().getImportingFieldInfo(fieldName);
            return currentFieldInfo != null && "N".equals(currentFieldInfo.getValue())
                    && "Y".equals(importingFieldInfo.getValue());
        }
    }

    private Map<String, X2BaseBean> m_beansForFields = new HashMap();

    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanDescriptor(java.lang.String)
     */
    @Override
    protected String getBeanDescriptor(String fieldName) {
        return super.getBeanDescriptor(fieldName) + " with race code '"
                + ((FieldMergeAttributes) findFieldMergeAttributesByFieldName(fieldName)).getRaceCode() + "'";
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanMergeTo(java.lang.String)
     */
    @Override
    protected X2BaseBean getBeanMergeTo(String fieldName) {
        Race race = (Race) m_beansForFields.get(fieldName);
        if (race == null) {
            race = X2BaseBean.newInstance(Race.class, getBroker().getPersistenceKey());
            race.setPersonOid(getStudent().getPersonOid());
            race.setRaceCode(""); // Cannot be null
            m_beansForFields.put(fieldName, race);
        }

        return race;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getClassMergeTo()
     */
    @Override
    protected Class<?> getClassMergeTo() {
        return Race.class;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getFieldMergeAttributes()
     */
    @Override
    protected FieldMergeAttributesInterface[] getFieldMergeAttributes() {
        return FieldMergeAttributes.values();
    }
}
