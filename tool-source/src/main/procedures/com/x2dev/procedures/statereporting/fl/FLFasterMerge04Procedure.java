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
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldBeanInfo;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.Arrays;
import java.util.List;
import org.apache.commons.lang3.StringUtils;


/**
 * The Class FLFasterMerge04Procedure.
 */
public class FLFasterMerge04Procedure extends RecordTypeMergeData {

    /**
     * The Enum FieldMergeAttributes.
     */
    private enum FieldMergeAttributes implements FieldMergeAttributesInterface {
        DST_NUM_CRED_EARNED(FIELD_DST_NUM_CRED_EARNED, new FieldMerger04(ALIAS_TRN_CRED_EARNED_DST_NUM)),
        //
        SKL_NUM_EARNED_TAKEN(FIELD_SKL_NUM_EARNED_TAKEN, new FieldMerger04(ALIAS_TRN_CRED_EARNED_TAKEN_SKL_NUM)),
        //
        SKL_NAME_EARNED_TAKEN(FIELD_SKL_NAME_CRS_TAKEN, new FieldMerger04(ALIAS_TRN_CRED_EARNED_TAKEN_SKL_NAME)),
        //
        SKL_YEAR_WITH_CENTURY(FIELD_SKL_YEAR_WITH_CENT, new FieldMerger04(Transcript.COL_SCHOOL_YEAR),
                new ValueAdjuster() {
                    @Override
                    public Object getAdjustedValue(String value) {
                        return value.length() == 8 ? Integer.valueOf(value.substring(4)) : Integer.valueOf(0);
                    }
                }),
        //
        GRADE_LEVEL(FIELD_GRADE_LEVEL, new FieldMerger04(Transcript.COL_GRADE_LEVEL)),
        //
        TERM(FIELD_TERM, new FieldMerger04(ALIAS_TRN_TERM)),
        //
        COURSE_NUMBER(FIELD_COURSE_NUMBER, new FieldMerger04(ALIAS_TRN_COURSE_NUMBER)),
        //
        TITLE_ABBREVIATION(FIELD_COURSE_TITLE_ABBREV, new FieldMerger04(ALIAS_TRN_COURSE_ABBREVIATED_TITLE)),
        //
        CRS_STATE_SUBJ_REQS(FIELD_CRS_STATE_SUBJ_REQS, new FieldMerger04(ALIAS_TRN_SUBJ_AREA_REQS)),
        //
        CRS_FLAG(FIELD_COURSE_FLAG, new FieldMerger04(ALIAS_TRN_COURSE_FLAG)),
        //
        CRS_ONLINE(FIELD_ONLINE_CRS_INDICATOR, new FieldMerger04(ALIAS_TRN_COURSE_ONLINE)),
        //
        CRS_IN_PROGRESS_HRS(FIELD_CRS_IN_PROGRESS_HRS, new FieldMerger04(ALIAS_TRN_IN_PROGRESS_HRS)),
        //
        CRS_ABSENCES(FIELD_CRS_ABSENCES, new FieldMerger04(ALIAS_TRN_ABSENCES)),
        //
        GRADING_CYCLE_WEEKS(FIELD_GRADING_CYCLE_WEEKS, new FieldMerger04(ALIAS_TRN_GRADING_CYCLE_WEEKS)),
        //
        TERM_NAME(FIELD_TERM_NAME, new FieldMerger04(ALIAS_TRN_TERM_NAME)),
        //
        TERM_START_DATE(FIELD_TERM_START_DATE, new FieldMerger04(ALIAS_TRN_TERM_START_DATE)),
        //
        TERM_END_DATE(FIELD_TERM_END_DATE, new FieldMerger04(ALIAS_TRN_TERM_END_DATE)),
        //
        SKL_CRED_CODE_TYPE(FIELD_SKL_CRED_CODE_TYPE, new FieldMerger04(ALIAS_TRN_CRED_CODE_TYPE)),
        //
        SKL_CRED_CODE(FIELD_SKL_CRED_CODE, new FieldMerger04(ALIAS_TRN_CRED_EARNED_SKL_NUM_CODE)),
        //
        SBT_CRS_NUMBER(FIELD_SBT_CRS_NUMBER, new FieldMerger04(ALIAS_TRN_SBT_CRS_NUM)),
        //
        SBT_STATE_SUBJ_REQS(FIELD_SBT_STATE_SUBJ_REQS, new FieldMerger04(ALIAS_TRN_SBT_CRS_SUBJ_REQS)),
        //
        AGE_PROGRAM_CODE(FIELD_AGE_PROGRAM_CODE, new FieldMerger04(ALIAS_TRN_AGE_PGM_CODE)),
        //
        CRED_EARNED_CODE_TYPE(FIELD_SKL_CRED_CODE_TYPE, new FieldMerger04(ALIAS_TRN_CRED_EARNED_CODE_TYPE)),
        //
        CRED_EARNED_SKL_NUM_CODE(FIELD_SKL_CRED_CODE, new FieldMerger04(ALIAS_TRN_CRED_EARNED_SKL_NUM));

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
            m_fieldMerger = fieldMerger;
            m_fieldName = fieldName;
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

    private static final List<String> s_fieldsToIdentifyBean = Arrays.asList(FIELD_DST_NUM_CRED_EARNED,
            FIELD_SKL_NUM_EARNED_TAKEN, FIELD_SKL_YEAR_WITH_CENT, FIELD_TERM, FIELD_COURSE_NUMBER);


    /**
     * The Class FieldMerger04.
     */
    private static class FieldMerger04 extends FieldMerger {
        private String m_beanPath = null;


        /**
         * Instantiates a new field merger 04.
         *
         * @param beanPath String
         */
        public FieldMerger04(String beanPath) {
            m_beanPath = beanPath;
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
            return null;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger#isMergeNeeded()
         */
        @Override
        public boolean isMergeNeeded() {
            String fieldName = getFieldName();
            X2BaseBean bean = getMergeData().getBeanMergeTo(fieldName);
            if (bean == null) {
                return false;
            }
            FieldInfo importingFieldInfo = getMergeData().getImportingFieldInfo(fieldName);

            Object currentValue = bean.getFieldValueByBeanPath(importingFieldInfo.getBeanPath());
            Object adjustedValue = getMergeData().getAdjustedValue(fieldName);
            return adjustedValue != null && !StringUtils.isEmpty(adjustedValue.toString())
                    && !adjustedValue.equals(currentValue);
        }
    }

    private static final String ALIAS_TRN_ABSENCES = "all-trn-Absences";
    private static final String ALIAS_TRN_AGE_PGM_CODE = "all-trn-AgePgmCode";
    private static final String ALIAS_TRN_COURSE_ABBREVIATED_TITLE = "all-trn-CourseAbbrevTitle";
    private static final String ALIAS_TRN_COURSE_FLAG = "all-trn-CourseFlag";
    private static final String ALIAS_TRN_COURSE_NUMBER = "all-trn-CourseNumber";
    private static final String ALIAS_TRN_COURSE_ONLINE = "all-trn-CourseOnline";
    private static final String ALIAS_TRN_CRED_CODE_TYPE = "all-trn-CredCodeType";
    private static final String ALIAS_TRN_CRED_EARNED_CODE_TYPE = "all-trn-CredEarnedCodeType";
    private static final String ALIAS_TRN_CRED_EARNED_DST_NUM = "all-trn-CredEarnedDstNum";
    private static final String ALIAS_TRN_CRED_EARNED_SKL_NUM = "all-trn-CredEarnedSklNumCode";
    private static final String ALIAS_TRN_CRED_EARNED_SKL_NUM_CODE = "all-trn-CredEarnedSklNumCode";
    private static final String ALIAS_TRN_CRED_EARNED_TAKEN_SKL_NAME = "all-trn-CredEarnedTakenSklName";
    private static final String ALIAS_TRN_CRED_EARNED_TAKEN_SKL_NUM = "all-trn-CredEarnedTakenSklNum";
    private static final String ALIAS_TRN_FROM_FASTER_INDICATOR = "all-trn-ImportedFromFasterInd";
    private static final String ALIAS_TRN_GRADING_CYCLE_WEEKS = "all-trn-GradingCycleWeeks";
    private static final String ALIAS_TRN_IN_PROGRESS_HRS = "all-trn-InProgressHrs";
    private static final String ALIAS_TRN_SBT_CRS_NUM = "all-trn-SbtCrsNumber";
    private static final String ALIAS_TRN_SBT_CRS_SUBJ_REQS = "all-trn-SbtCrsSubjReq";
    private static final String ALIAS_TRN_SUBJ_AREA_REQS = "all-trn-SubjAreaReqs";
    private static final String ALIAS_TRN_TERM = "all-trn-Term";
    private static final String ALIAS_TRN_TERM_END_DATE = "all-trn-TermEndDate";
    private static final String ALIAS_TRN_TERM_NAME = "all-trn-TermName";
    private static final String ALIAS_TRN_TERM_START_DATE = "all-trn-TermStartDate";

    private static final String SCHOOL_ASSIGN_TO_NAME = "Archive School";

    private Transcript m_transcriptToMerge = null;


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanDescriptor(java.lang.String)
     */
    @Override
    protected String getBeanDescriptor(String fieldName) {
        FieldInfo dstNum = getFieldInfosForPlainRow(getImportingPlainRow()).get(FIELD_DST_NUM_CRED_EARNED);
        FieldInfo sklNum = getFieldInfosForPlainRow(getImportingPlainRow()).get(FIELD_SKL_NUM_EARNED_TAKEN);
        FieldInfo year = getFieldInfosForPlainRow(getImportingPlainRow()).get(FIELD_SKL_YEAR_WITH_CENT);
        FieldInfo term = getFieldInfosForPlainRow(getImportingPlainRow()).get(FIELD_TERM);
        FieldInfo course = getFieldInfosForPlainRow(getImportingPlainRow()).get(FIELD_COURSE_NUMBER);

        return super.getBeanDescriptor(fieldName) + " for course '" + course.getValue() + "' (" +
                "district number: " + dstNum.getValue() + ", " +
                "school number: " + sklNum.getValue() + ", " +
                "year: " + year.getValue() + ", " +
                "term: " + term.getValue() + ")";
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanMergeTo(java.lang.String)
     */
    @Override
    protected Transcript getBeanMergeTo(String currentFieldName) {
        if (getCurrentPlainRow() != null) {
            // it's local transcript (only local transcripts shown in our response), cannot be
            // imported as transcript of another school
            return null;
        }
        if (m_transcriptToMerge == null) {
            X2Criteria beanCriteria = new X2Criteria();
            beanCriteria.addEqualTo(Transcript.COL_STUDENT_OID, getStudent().getOid());
            for (String fieldName : getMergeableFieldNames()) {
                if (s_fieldsToIdentifyBean.contains(fieldName)) {
                    FieldInfo importingFieldInfo = getFieldInfosForPlainRow(getImportingPlainRow()).get(fieldName);
                    beanCriteria.addEqualTo(importingFieldInfo.getBeanPath(), getAdjustedValue(fieldName));
                }
            }

            BeanQuery beanQuery = new BeanQuery(getClassMergeTo(), beanCriteria);
            m_transcriptToMerge = (Transcript) getBroker().getBeanByQuery(beanQuery);
            if (m_transcriptToMerge == null) {
                m_transcriptToMerge = new Transcript(getBroker().getPersistenceKey());
                m_transcriptToMerge.setStudentOid(getStudent().getOid());
                m_transcriptToMerge.setFieldValueByAlias(ALIAS_TRN_FROM_FASTER_INDICATOR,
                        BooleanAsStringConverter.TRUE);
                X2Criteria ctxCriteria = new X2Criteria();
                Object schoolYear = getAdjustedValue(FIELD_SKL_YEAR_WITH_CENT);
                ctxCriteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR, schoolYear);
                BeanQuery ctxQuery = new BeanQuery(DistrictSchoolYearContext.class, ctxCriteria);
                DistrictSchoolYearContext context = (DistrictSchoolYearContext) getBroker().getBeanByQuery(ctxQuery);
                if (context == null) {
                    throw new X2RuntimeException();
                }
                m_transcriptToMerge.setFieldValueByBeanPath(Transcript.COL_DISTRICT_CONTEXT_OID, context.getOid());

                X2Criteria archiveSchoolCriteria = new X2Criteria();
                archiveSchoolCriteria.addEqualTo(SisSchool.COL_NAME, SCHOOL_ASSIGN_TO_NAME);
                BeanQuery archiveQuery = new BeanQuery(SisSchool.class, archiveSchoolCriteria);
                X2BaseBean archiveSchool = getBroker().getBeanByQuery(archiveQuery);
                if (archiveSchool == null) {
                    throw new X2RuntimeException();
                }
                m_transcriptToMerge.setFieldValueByBeanPath(Transcript.COL_SCHOOL_OID, archiveSchool.getOid());
            }
        }


        return m_transcriptToMerge;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getClassMergeTo()
     */
    @Override
    protected Class<?> getClassMergeTo() {
        return Transcript.class;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getFieldMergeAttributes()
     */
    @Override
    protected FieldMergeAttributesInterface[] getFieldMergeAttributes() {
        return FieldMergeAttributes.values();
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getFieldNamesToIdentifyRow()
     */
    @Override
    protected List<String> getFieldNamesToIdentifyRow() {
        return s_fieldsToIdentifyBean;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        setFieldInfosPathsToMerge();
    }


    /**
     * Sets the field infos paths to merge.
     */
    private void setFieldInfosPathsToMerge() {
        for (String fieldName : getMergeableFieldNames()) {
            FieldInfo fieldInfo = getFieldInfosForPlainRow(getImportingPlainRow()).get(fieldName);
            FieldMerger merger = getFieldMerger(fieldName);
            String beanPath = merger.getBeanPath();
            if (!StringUtils.isEmpty(beanPath)) {
                fieldInfo.setFieldBeanInfo(new FieldBeanInfo(getClassMergeTo(), beanPath, getDictionary()));
            }
        }
    }
}
