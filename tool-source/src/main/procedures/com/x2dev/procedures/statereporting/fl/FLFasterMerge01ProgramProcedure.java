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

import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_HOME_LANG_SURV_DATE;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_MIGRANT_CONTINUATION;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_MIGRANT_ENR_DATE;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_MIGRANT_PRIORITY;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_MILITARY_FAMILY;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_MIN_EXCEPT_DATE;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.types.PlainDate;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class FLFasterMerge01ProgramProcedure.
 */
public class FLFasterMerge01ProgramProcedure extends RecordTypeMergeData {


    /**
     * The Enum FieldMergeAttributes.
     */
    private enum FieldMergeAttributes implements FieldMergeAttributesInterface {
        MILITARY_FAMILY(FIELD_MILITARY_FAMILY,
                new ProgramMerger(DDX_ID_MFS) {

                    @Override
                    public void merge() {
                        // just save bean if needed
                    }

                    @Override
                    public boolean isMergeNeeded() {
                        String fieldName = getFieldName();
                        String currentPlainRow = getMergeData().getCurrentPlainRow();
                        FieldInfo currentFieldInfo = getMergeData().getFieldInfo(fieldName, currentPlainRow);
                        FieldInfo importingFieldInfo = getMergeData().getImportingFieldInfo(fieldName);
                        return super.isMergeNeeded()
                                && !"Y".equals(currentFieldInfo.getValue())
                                && "Y".equals(importingFieldInfo.getValue());
                    }

                }),
        //
        MIN_EXCEPT_DATE(FIELD_MIN_EXCEPT_DATE,
                new ProgramMergerDate(DDX_ID_EXCEPT, "[pgm-min-edu-perf-st-date]"),
                new ValueAdjusterPgmDate("MMddyyyy")),
        //
        MIGRANT_ENR_DATE(FIELD_MIGRANT_ENR_DATE,
                new ProgramMergerDate(DDX_ID_MIGRANT, "startDate"), new ValueAdjusterPgmDate("MMddyyyy")),
        //
        MIGRANT_CONTINUATION(FIELD_MIGRANT_CONTINUATION,
                new ProgramMergerZ(DDX_ID_MIGRANT, "[pgm-continuation]")),
        //
        MIGRANT_PRIORITY(FIELD_MIGRANT_PRIORITY,
                new ProgramMergerZ(DDX_ID_MIGRANT, "[pgm-priority]")),
        //
        HOME_LANG_SURV_DATE(FIELD_HOME_LANG_SURV_DATE,
                new ProgramMergerDate(DDX_ID_ELL, "[pgm-survey-date]"), new ValueAdjusterPgmDate("yyyyMMdd"));

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


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface#getFieldName()
         */
        @Override
        public String getFieldName() {
            return m_fieldName;
        }
    }


    /**
     * The Class ProgramMerger.
     */
    public static class ProgramMerger extends FieldMerger {
        private String m_beanPath = null;
        private String m_ddxId = null;


        /**
         * Instantiates a new program merger.
         *
         * @param ddxId String
         */
        public ProgramMerger(String ddxId) {
            this(ddxId, null);
        }


        /**
         * Instantiates a new program merger.
         *
         * @param ddxId String
         * @param beanPath String
         */
        public ProgramMerger(String ddxId, String beanPath) {
            m_beanPath = beanPath;
            m_ddxId = ddxId;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger#getBeanPath()
         */
        @Override
        public String getBeanPath() {
            return m_beanPath;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger#getExtendedDictionaryId()
         */
        @Override
        public String getExtendedDictionaryId() {
            return m_ddxId;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger#isMergeNeeded()
         */
        @Override
        public boolean isMergeNeeded() {
            String fieldName = getFieldName();
            X2BaseBean beanMergeTo = getMergeData().getBeanMergeTo(fieldName);
            return beanMergeTo != null;
        }
    }


    /**
     * The Class ProgramMergerDate.
     */
    public static class ProgramMergerDate extends ProgramMerger {

        /**
         * Instantiates a new program merger date.
         *
         * @param programId String
         * @param beanPath String
         */
        public ProgramMergerDate(String programId, String beanPath) {
            super(programId, beanPath);
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMerge01ProgramProcedure.ProgramMerger#isMergeNeeded()
         */
        @Override
        public boolean isMergeNeeded() {
            String fieldName = getFieldName();
            String currentPlainRow = getMergeData().getCurrentPlainRow();
            FieldInfo currentFieldInfo = getMergeData().getFieldInfo(fieldName, currentPlainRow);
            FieldInfo importingFieldInfo = getMergeData().getImportingFieldInfo(fieldName);
            return super.isMergeNeeded()
                    && StringUtils.isEmpty(currentFieldInfo.getValue().trim())
                    && !StringUtils.isEmpty(importingFieldInfo.getValue().trim());
        }
    }


    /**
     * The Class ProgramMergerZ.
     */
    public static class ProgramMergerZ extends ProgramMerger {

        /**
         * Instantiates a new program merger Z.
         *
         * @param programId String
         * @param beanPath String
         */
        public ProgramMergerZ(String programId, String beanPath) {
            super(programId, beanPath);
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMerge01ProgramProcedure.ProgramMerger#isMergeNeeded()
         */
        @Override
        public boolean isMergeNeeded() {
            String fieldName = getFieldName();
            String currentPlainRow = getMergeData().getCurrentPlainRow();
            FieldInfo currentFieldInfo = getMergeData().getFieldInfo(fieldName, currentPlainRow);
            FieldInfo importingFieldInfo = getMergeData().getImportingFieldInfo(fieldName);
            return super.isMergeNeeded() &&
                    (StringUtils.isEmpty(currentFieldInfo.getValue())
                            || "Z".equals(currentFieldInfo.getValue()))
                    && !StringUtils.isEmpty(importingFieldInfo.getValue())
                    && !"Z".equals(importingFieldInfo.getValue());
        }
    }

    private static final String DDX_ID_ELL = "FL-PGM-ELL";
    private static final String DDX_ID_EXCEPT = "FL-PGM-EXCEPT";
    private static final String DDX_ID_MFS = "FL-PGM-MFS";
    private static final String DDX_ID_MIGRANT = "FL-PGM-MIGRANT";

    private Map<String, StudentProgramParticipation> m_programsById = new HashMap<>();


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanDescriptor(java.lang.String)
     */
    @Override
    protected String getBeanDescriptor(String fieldName) {
        FieldMergeAttributesInterface mergeAttributes = findFieldMergeAttributesByFieldName(fieldName);
        return super.getBeanDescriptor(fieldName) + " for extended dictionary "
                + mergeAttributes.getMerger().getExtendedDictionaryId();
    }

    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanMergeTo(java.lang.String)
     */
    @Override
    protected X2BaseBean getBeanMergeTo(String fieldName) {
        FieldMerger programMerger =
                getFieldMerger(fieldName);

        if (programMerger != null) {
            String ddxId = programMerger.getExtendedDictionaryId();

            StudentProgramParticipation currentProgram = m_programsById.get(ddxId);
            if (currentProgram != null) {
                return currentProgram;
            }

            currentProgram = getCurrentProgram(getStudent().getOid(), programMerger.getExtendedDictionaryId());
            if (currentProgram != null) {
                return currentProgram;
            }
            currentProgram =
                    X2BaseBean.newInstance(StudentProgramParticipation.class, getBroker().getPersistenceKey());
            currentProgram.setFieldValueByBeanPath(StudentProgramParticipation.COL_STUDENT_OID,
                    getStudent().getOid());
            currentProgram.setFieldValueByBeanPath(StudentProgramParticipation.COL_EXTENDED_DATA_DICTIONARY_OID,
                    getExtendedDataDictionaryById(ddxId).getOid());
            currentProgram.setStartDate(getCurrentContext().getStartDate());
            String pgmCode = null;
            switch (programMerger.getExtendedDictionaryId()) {
                case DDX_ID_ELL:
                    pgmCode = "ELL";
                    break;
                case DDX_ID_EXCEPT:
                    pgmCode = "EXCEPT";
                    break;
                case DDX_ID_MFS:
                    pgmCode = "MILITARY";
                    break;
                case DDX_ID_MIGRANT:
                    pgmCode = "MIGRANT";
                    break;

                default:
                    break;
            }

            if (pgmCode != null) {
                currentProgram.setFieldValueByBeanPath(StudentProgramParticipation.COL_PROGRAM_CODE, pgmCode);
            }
            m_programsById.put(ddxId, currentProgram);

            return currentProgram;
        }

        return null;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getClassMergeTo()
     */
    @Override
    protected Class<?> getClassMergeTo() {
        return StudentProgramParticipation.class;
    }


    /**
     * Gets the current program.
     *
     * @param studentOid String
     * @param ddxId String
     * @return Student program participation
     */
    protected StudentProgramParticipation getCurrentProgram(String studentOid, String ddxId) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentProgramParticipation.COL_STUDENT_OID, studentOid);
        criteria.addEqualTo(StudentProgramParticipation.COL_EXTENDED_DATA_DICTIONARY_OID,
                getExtendedDataDictionaryById(ddxId).getOid());
        criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, new PlainDate());
        X2Criteria endDateCriteria = new X2Criteria();
        endDateCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, new PlainDate());
        X2Criteria emptyEndDateCriteria = new X2Criteria();
        emptyEndDateCriteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
        endDateCriteria.addOrCriteria(emptyEndDateCriteria);
        criteria.addAndCriteria(endDateCriteria);
        return (StudentProgramParticipation) getBroker()
                .getBeanByQuery(new QueryByCriteria(StudentProgramParticipation.class, criteria));
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getFieldMergeAttributes()
     */
    @Override
    protected FieldMergeAttributesInterface[] getFieldMergeAttributes() {
        return FieldMergeAttributes.values();
    }
}
