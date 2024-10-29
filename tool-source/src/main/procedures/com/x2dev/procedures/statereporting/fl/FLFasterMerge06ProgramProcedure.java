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

import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_CONSENT_DATE_EV;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_DISMISSAL_DATE;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_ELIG_DETERM_DATE;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_EVAL_COMPL_DATE;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_EXCEPTIONALITY;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_PLACEMENT_DATE;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_PLACEMENT_STATUS;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_REFERRAL_DATE;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class FLFasterMerge06ProgramProcedure.
 */
public class FLFasterMerge06ProgramProcedure extends RecordTypeMergeData {

    /**
     * The Enum FieldMergeAttributes.
     */
    private enum FieldMergeAttributes implements FieldMergeAttributesInterface {
        PLACEMENT_DATE(FIELD_PLACEMENT_DATE, new FieldMerger(DDX_ID_EXCEPT, "pgm-placement-date"),
                s_pgmDateValueAdjuster),
        //
        ELIG_DETERM_DATE(FIELD_ELIG_DETERM_DATE, new FieldMerger(DDX_ID_EXCEPT, "pgm-eligibility-determination"),
                s_pgmDateValueAdjuster),
        //
        PLACEMENT_STATUS(FIELD_PLACEMENT_STATUS, new FieldMerger(DDX_ID_EXCEPT, "pgm-placement-status")),
        //
        REFERRAL_DATE(FIELD_REFERRAL_DATE, new FieldMerger(DDX_ID_EXCEPT, "pgm-referral-date"), s_pgmDateValueAdjuster),
        //
        DISMISSAL_DATE(FIELD_DISMISSAL_DATE, new FieldMerger(DDX_ID_EXCEPT, "pgm-dismissal-date"),
                s_pgmDateValueAdjuster),
        //
        EXCEPTIONALITY(FIELD_EXCEPTIONALITY, new FieldMerger(DDX_ID_EXCEPT, "pgm-primary")),
        //
        EVAL_COMPL_DATE(FIELD_EVAL_COMPL_DATE, new FieldMerger(DDX_ID_EXCEPT, "pgm-eval-completion-date"),
                s_pgmDateValueAdjuster),
        //
        CONSENT_DATE_EV(FIELD_CONSENT_DATE_EV, new FieldMerger(DDX_ID_EXCEPT, "pgm-evaluation-consent-date"),
                s_pgmDateValueAdjuster);

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
         * Find merge attributes by prefix.
         *
         * @param fieldName String
         * @return FieldMergeAttributes
         */
        public static FieldMergeAttributes findMergeAttributesByPrefix(String fieldName) {
            for (FieldMergeAttributes mergeAttributes : values()) {
                if (fieldName.startsWith(mergeAttributes.getFieldName())) {
                    return mergeAttributes;
                }
            }
            return null;
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

    private static final String DDX_ID_EXCEPT = "FL-PGM-EXCEPT";
    private static final String PGM_CODE_EXCEPT = "EXCEPT";

    private static final ValueAdjuster s_pgmDateValueAdjuster = new ValueAdjusterPgmDate("MMddyyyy");

    private Map<String, StudentProgramParticipation> m_programs = new HashMap<>();


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
        return FieldMergeAttributes.findMergeAttributesByPrefix(fieldName);
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanDescriptor(java.lang.String)
     */
    @Override
    protected String getBeanDescriptor(String fieldName) {
        StudentProgramParticipation pgm = (StudentProgramParticipation) getBeanMergeTo(fieldName);
        String beanPath = getImportingFieldInfo(FIELD_EXCEPTIONALITY + 1).getBeanPath();
        String primeExceptionality = (String) pgm.getFieldValueByBeanPath(beanPath);
        FieldMergeAttributesInterface mergeAttributes = findFieldMergeAttributesByFieldName(fieldName);
        return super.getBeanDescriptor(fieldName) + " for extended dictionary "
                + mergeAttributes.getMerger().getExtendedDictionaryId() + " and Primary Exceptionality "
                + primeExceptionality;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanMergeTo(java.lang.String)
     */
    @Override
    protected X2BaseBean getBeanMergeTo(String fieldName) {
        String key = getProgramKey(fieldName);
        if (StringUtils.isEmpty(key)) {
            return null;
        }
        StudentProgramParticipation program = m_programs.get(key);
        if (program == null) {
            program = getNewProgram();
            m_programs.put(key, program);
        }
        return program;
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
    }


    /**
     * Gets the group number.
     *
     * @param fieldName String
     * @return int
     */
    private int getGroupNumber(String fieldName) {
        String prefix = FieldMergeAttributes.findMergeAttributesByPrefix(fieldName).getFieldName();
        String groupNumber = fieldName.replace(prefix, "");
        return Integer.parseInt(groupNumber);
    }


    /**
     * Gets the new program.
     *
     * @return Student program participation
     */
    private StudentProgramParticipation getNewProgram() {
        StudentProgramParticipation newProgram =
                X2BaseBean.newInstance(StudentProgramParticipation.class, getBroker().getPersistenceKey());
        newProgram.setFieldValueByBeanPath(StudentProgramParticipation.COL_STUDENT_OID,
                getStudent().getOid());
        newProgram.setFieldValueByBeanPath(StudentProgramParticipation.COL_EXTENDED_DATA_DICTIONARY_OID,
                getExtendedDataDictionaryById(DDX_ID_EXCEPT).getOid());
        newProgram.setFieldValueByBeanPath(StudentProgramParticipation.COL_PROGRAM_CODE, PGM_CODE_EXCEPT);
        newProgram.setStartDate(getCurrentContext().getStartDate());

        return newProgram;
    }


    /**
     * Gets the program key.
     *
     * @param fieldName String
     * @return String
     */
    private String getProgramKey(String fieldName) {
        int groupNumber = getGroupNumber(fieldName);
        return getImportingFieldInfo(FIELD_EXCEPTIONALITY + groupNumber).getValue().trim();
    }


    /**
     * Gets the program key.
     *
     * @param pgm StudentProgramParticipation
     * @return String
     */
    private String getProgramKey(StudentProgramParticipation pgm) {
        FieldInfo exceptFieldInfo = getImportingFieldInfo(FIELD_EXCEPTIONALITY + 1);
        return (String) pgm.getFieldValueByBeanPath(exceptFieldInfo.getBeanPath());
    }


    /**
     * Load programs.
     */
    private void loadPrograms() {
        X2Criteria pgmCriteria = new X2Criteria();
        pgmCriteria.addEqualTo(StudentProgramParticipation.COL_STUDENT_OID, getStudent().getOid());
        DataDictionary dictionary = getDictionaryById(DDX_ID_EXCEPT);
        pgmCriteria.addEqualTo(StudentProgramParticipation.COL_EXTENDED_DATA_DICTIONARY_OID,
                dictionary.getExtendedDictionaryOid());
        QueryByCriteria pgmQuery = new QueryByCriteria(getClassMergeTo(), pgmCriteria);
        Collection<StudentProgramParticipation> programs = getBroker().getCollectionByQuery(pgmQuery);
        if (programs != null) {
            for (StudentProgramParticipation program : programs) {
                m_programs.put(getProgramKey(program), program);
            }
        }
    }
}
