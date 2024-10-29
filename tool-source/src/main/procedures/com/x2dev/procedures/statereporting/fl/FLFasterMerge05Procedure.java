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

import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.*;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class FLFasterMerge05Procedure.
 */
public class FLFasterMerge05Procedure extends RecordTypeMergeData {

    /**
     * The Enum FieldMergeAttributes.
     */
    private enum FieldMergeAttributes implements FieldMergeAttributesInterface {
        CTE_PGM_CODE(FIELD_CTE_PGM_CODE, new FieldMerger(DDX_ID_CTE, "pgm-cte-program-id")),
        //
        CTE_PGM_NAME(FIELD_CTE_PGM_NAME, new FieldMerger(DDX_ID_CTE, "pgm-cte-pgm-name")),
        //
        CTE_FULL_PGM_COMPL(FIELD_CTE_FULL_PGM_COMPL, new FieldMerger(DDX_ID_CTE, "pgm-cte-full-program-compl")),
        //
        CTE_PGM_CERT_EARNED_DATE(FIELD_CTE_PGM_CERT_EARNED_DATE,
                new FieldMerger(DDX_ID_CTE, "pgm-cte-cert-earned-date"), s_pgmDateValueAdjuster),
        //
        CTE_INDUS_CERT_ID(FIELD_CTE_INDUS_CERT_ID, new FieldMerger(DDX_ID_CTE, "pgm-cte-industry-cert-id")),
        //
        CTE_INDUS_CERT_OUT(FIELD_CTE_INDUS_CERT_OUT, new FieldMerger(DDX_ID_CTE, "pgm-cte-industry-cert")),
        //
        CTE_ACADEMY_ID(FIELD_CTE_ACADEMY_ID, new FieldMerger(DDX_ID_CTE, "pgm-cte-cape-id")),
        //
        ELL_PK_12(FIELD_ELL_PK_12, new FieldMerger(DDX_ID_ELL, "pgm-ell-code")),
        //
        ELL_ENTRY_BASIS(FIELD_ELL_ENTRY_BASIS, new FieldMerger(DDX_ID_ELL, "pgm-basis-of-entry")),
        //
        ELL_ENTRY_DATE(FIELD_ELL_ENTRY_DATE, new FieldMerger(DDX_ID_ELL, "startDate"), s_pgmDateValueAdjuster),
        //
        ELL_CLASSIF_DATE(FIELD_ELL_CLASSIF_DATE, new FieldMerger(DDX_ID_ELL, "pgm-classification-date"),
                s_pgmDateValueAdjuster),
        //
        ELL_STD_PLAN_DATE(FIELD_ELL_STD_PLAN_DATE, new FieldMerger(DDX_ID_ELL, "pgm-student-plan-date"),
                s_pgmDateValueAdjuster),
        //
        ELL_REEVAL_DATE(FIELD_ELL_REEVAL_DATE, new FieldMerger(DDX_ID_ELL, "pgm-reevaluation-date"),
                s_pgmDateValueAdjuster),
        //
        ELL_INSTRUCT_EXT(FIELD_ELL_INSTRUCT_EXT, new FieldMerger(DDX_ID_ELL, "pgm-extension-of-instruction"),
                new ValueAdjuster() {

                    @Override
                    public Object getAdjustedValue(String value) {
                        return "Y".equals(value) ? BooleanAsStringConverter.TRUE : BooleanAsStringConverter.FALSE;
                    }

                }),
        //
        ELL_EXIT_DATE(FIELD_ELL_EXIT_DATE, new FieldMerger(DDX_ID_ELL, "endDate"), s_pgmDateValueAdjuster),
        //
        ELL_RECLAS_DATE(FIELD_ELL_RECLAS_DATE, new FieldMerger(DDX_ID_ELL, "pgm-reclassification-date"),
                s_pgmDateValueAdjuster),
        //
        ELL_RECLAS_EXIT_DATE(FIELD_ELL_RECLAS_EXIT_DATE,
                new FieldMerger(DDX_ID_ELL, "pgm-reclassification-exit-date"), s_pgmDateValueAdjuster),
        //
        ELL_REPORT_CARD_1(FIELD_ELL_REPORT_CARD_1, new FieldMerger(DDX_ID_ELL, "pgm-report-card-date"),
                new ValueAdjusterPgmDate("'A'yyyyMMdd")),
        //
        ELL_SEMIANN_REVIEW_1(FIELD_ELL_SEMIANN_REVIEW + 1, new FieldMerger(DDX_ID_ELL, "pgm-first-semi-rev-date"),
                new ValueAdjusterPgmDate("'B'yyyyMMdd")),
        //
        ELL_SEMIANN_REVIEW_2(FIELD_ELL_SEMIANN_REVIEW + 2, new FieldMerger(DDX_ID_ELL, "pgm-second-semi-rev-date"),
                new ValueAdjusterPgmDate("'C'yyyyMMdd")),
        //
        ELL_YEAR_2_END(FIELD_ELL_YEAR_2_END, new FieldMerger(DDX_ID_ELL, "pgm-second-year-date"),
                new ValueAdjusterPgmDate("'D'yyyyMMdd")),
        //
        ELL_EXIT_BASIS_1(FIELD_ELL_EXIT_BASIS + 1, new FieldMerger(DDX_ID_ELL, "pgm-basis-of-exit-first")),
        //
        ELL_EXIT_BASIS_2(FIELD_ELL_EXIT_BASIS + 2, new FieldMerger(DDX_ID_ELL, "pgm-basis-of-exit-second")),
        //
        DRP_PROGRAM_CODE(FIELD_DRP_PROGRAM_CODE, new FieldMerger(DDX_ID_DROP, "pgm-dropout-code")),
        //
        DRP_PLACEMNT_REASONS(FIELD_DRP_PLACEMNT_REASONS, new FieldMerger(DDX_ID_DROP, "pgm-drp-placement-reasons"),
                new ValueAdjusterMulticode()),
        //
        DRP_OUTCOMES(FIELD_DRP_OUTCOMES, new FieldMerger(DDX_ID_DROP, "pgm-drp-outcome"), new ValueAdjusterMulticode());

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

    private static final String DDX_ID_CTE = "FL-PGM-CTE";
    private static final String DDX_ID_DROP = "FL-PGM-DROP";
    private static final String DDX_ID_ELL = "FL-PGM-ELL";

    private static final String PGM_CODE_CTE = "CTE";
    private static final String PGM_CODE_DROPOUT = "DROPOUT";
    private static final String PGM_CODE_ELL = "ELL";

    private static final ValueAdjuster s_pgmDateValueAdjuster = new ValueAdjusterPgmDate("yyyyMMdd");

    private Map<String, StudentProgramParticipation> m_ctePrograms = new HashMap<>();
    private Map<String, StudentProgramParticipation> m_dropPrograms = new HashMap<>();
    private StudentProgramParticipation m_ellProgram = null;


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
        String adjustedFieldName = getAdjustedFieldNameToSearch(fieldName);
        return super.findFieldMergeAttributesByFieldName(adjustedFieldName);
    }


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
        FieldMerger merger =
                getFieldMerger(fieldName);

        String ddxId = merger.getExtendedDictionaryId();

        switch (ddxId) {
            case DDX_ID_CTE: {
                String pgmCode = getValueOfGroupField(fieldName, FIELD_CTE_PGM_CODE);
                return getCteProgram(pgmCode.trim());
            }

            case DDX_ID_DROP: {
                String pgmCode = getValueOfGroupField(fieldName, FIELD_DRP_PROGRAM_CODE);
                return getDropProgram(pgmCode.trim());
            }

            case DDX_ID_ELL:
                if (m_ellProgram == null) {
                    m_ellProgram = getNewProgram(DDX_ID_ELL);
                }
                return m_ellProgram;

            default:
                break;
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
     * Gets the adjusted field name to search.
     *
     * @param fieldName String
     * @return String
     */
    private String getAdjustedFieldNameToSearch(String fieldName) {
        return fieldName.substring(0, fieldName.length() - 1);
    }


    /**
     * Gets the cte program.
     *
     * @param pgmCode String
     * @return Student program participation
     */
    private StudentProgramParticipation getCteProgram(String pgmCode) {
        if (StringUtils.isEmpty(pgmCode)) {
            return null;
        }
        StudentProgramParticipation cteProgram = m_ctePrograms.get(pgmCode);
        if (cteProgram == null) {
            cteProgram = getNewProgram(DDX_ID_CTE);
            m_ctePrograms.put(pgmCode, cteProgram);
        }
        return cteProgram;
    }


    /**
     * Gets the drop program.
     *
     * @param pgmCode String
     * @return Student program participation
     */
    private StudentProgramParticipation getDropProgram(String pgmCode) {
        StudentProgramParticipation dropProgram = m_dropPrograms.get(pgmCode);
        if (dropProgram == null) {
            dropProgram = getNewProgram(DDX_ID_DROP);
            m_dropPrograms.put(pgmCode, dropProgram);
        }
        return dropProgram;
    }


    /**
     * Gets the new program.
     *
     * @param ddxId String
     * @return Student program participation
     */
    private StudentProgramParticipation getNewProgram(String ddxId) {
        StudentProgramParticipation newProgram =
                X2BaseBean.newInstance(StudentProgramParticipation.class, getBroker().getPersistenceKey());
        newProgram.setFieldValueByBeanPath(StudentProgramParticipation.COL_STUDENT_OID,
                getStudent().getOid());
        newProgram.setFieldValueByBeanPath(StudentProgramParticipation.COL_EXTENDED_DATA_DICTIONARY_OID,
                getExtendedDataDictionaryById(ddxId).getOid());
        newProgram.setStartDate(getCurrentContext().getStartDate());
        String pgmCode = null;
        switch (ddxId) {
            case DDX_ID_ELL:
                pgmCode = "ELL";
                break;
            case DDX_ID_CTE:
                pgmCode = "CTE";
                break;
            case DDX_ID_DROP:
                pgmCode = "DROPOUT";
                break;

            default:
                break;
        }

        if (pgmCode != null) {
            newProgram.setFieldValueByBeanPath(StudentProgramParticipation.COL_PROGRAM_CODE, pgmCode);
        }

        return newProgram;
    }


    /**
     * Gets the value of group field.
     *
     * @param fieldToGetNum String
     * @param fieldToGetValue String
     * @return String
     */
    private String getValueOfGroupField(String fieldToGetNum, String fieldToGetValue) {
        String fieldNum = fieldToGetNum.replace(getAdjustedFieldNameToSearch(fieldToGetNum), "");
        String cteCodeFieldName = fieldToGetValue + fieldNum;
        FieldInfo cteCodeFieldInfo = getImportingFieldInfo(cteCodeFieldName);
        return cteCodeFieldInfo.getValue();
    }


    /**
     * Load programs.
     */
    private void loadPrograms() {
        X2Criteria pgmCriteria = new X2Criteria();
        pgmCriteria.addEqualTo(StudentProgramParticipation.COL_STUDENT_OID, getStudent().getOid());
        QueryByCriteria pgmQuery = new QueryByCriteria(getClassMergeTo(), pgmCriteria);
        Collection<StudentProgramParticipation> programs = getBroker().getCollectionByQuery(pgmQuery);
        if (programs != null) {
            for (StudentProgramParticipation program : programs) {
                String pgmCode = program.getProgramCode();
                switch (pgmCode) {
                    case PGM_CODE_CTE: {
                        FieldInfo cteCodeFieldInfo = getImportingFieldInfo(FIELD_CTE_PGM_CODE + 1);
                        String ctePgmCode = (String) program.getFieldValueByBeanPath(cteCodeFieldInfo.getBeanPath());
                        m_ctePrograms.put(ctePgmCode, program);
                    }
                        break;

                    case PGM_CODE_DROPOUT: {
                        FieldInfo dropCodeFieldInfo = getImportingFieldInfo(FIELD_DRP_PROGRAM_CODE + 1);
                        String dropPgmCode = (String) program.getFieldValueByBeanPath(dropCodeFieldInfo.getBeanPath());
                        m_dropPrograms.put(dropPgmCode, program);
                    }
                        break;

                    case PGM_CODE_ELL:
                        m_ellProgram = program;
                        break;

                    default:
                        break;
                }
            }
        }
    }
}
