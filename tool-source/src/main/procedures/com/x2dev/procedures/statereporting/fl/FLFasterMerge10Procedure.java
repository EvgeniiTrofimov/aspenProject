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

import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_TEST_DATE;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_TEST_GRADE_LEVEL;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_TEST_NAME;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_TEST_PAS_NUM;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_TEST_SC;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData;
import com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldBeanInfo;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class FLFasterMerge10Procedure.
 */
public class FLFasterMerge10Procedure extends RecordTypeMergeData {

    /**
     * The Enum FieldMergeAttributes.
     */
    private enum FieldMergeAttributes implements FieldMergeAttributesInterface {
        TEST_GRADE_LEVEL(FIELD_TEST_GRADE_LEVEL, new FieldMerger("gradeLevelCode") {
            @Override
            public boolean isMergeNeeded() {
                return false;
            }
        }),
        //
        TEST_DATE(FIELD_TEST_DATE, new FieldMerger("date") {
            @Override
            public boolean isMergeNeeded() {
                return false;
            }
        }, new ValueAdjuster() {
            @Override
            public Object getAdjustedValue(String value) {
                SimpleDateFormat format = new SimpleDateFormat("MMddyyyy");
                try {
                    return new PlainDate(format.parse(value));
                } catch (ParseException e) {
                    return null;
                }
            }
        }),
        //
        TEST_NAME(FIELD_TEST_NAME, new FieldMerger(DDX_ID, "asm-test-name") {
            @Override
            public boolean isMergeNeeded() {
                String fieldName = getFieldName();
                X2BaseBean bean = getMergeData().getBeanMergeTo(fieldName);
                FieldInfo importingFieldInfo = getMergeData().getImportingFieldInfo(fieldName);
                return !StringUtils.isEmpty(importingFieldInfo.getValue().trim()) && bean == null;
            }
        }),
        //
        TEST_SC(FIELD_TEST_SC, new FieldMerger(DDX_ID, null) {
            @Override
            public boolean isMergeNeeded() {
                return false;
            }
        }, "asm-test-subjectContent"),
        //
        TEST_PAS_NUMBER(FIELD_TEST_PAS_NUM, new FieldMerger(DDX_ID, null), "asm-test-pasNumber");

        private String m_aliasToAdjust = null;
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
         * @param aliasToAdjust String
         */
        private FieldMergeAttributes(String fieldName, FieldMerger fieldMerger,
                String aliasToAdjust) {
            this(fieldName, fieldMerger);
            m_aliasToAdjust = aliasToAdjust;
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
         * Find field merge attribute by field name.
         *
         * @param fieldName String
         * @return FieldMergeAttributes
         */
        public static FieldMergeAttributes findFieldMergeAttributeByFieldName(String fieldName) {
            for (FieldMergeAttributes mergeAttributes : values()) {
                String fieldPrefix = mergeAttributes.getFieldName();
                if (isFieldIdentifiedByPrefix(fieldName, fieldPrefix)) {
                    return mergeAttributes;
                }
            }
            return null;
        }


        /**
         * Gets the alias to adjust.
         *
         * @return String
         */
        public String getAliasToAdjust() {
            return m_aliasToAdjust;
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
         * Gets the test num.
         *
         * @param fieldName String
         * @return int
         */
        public static int getTestNum(String fieldName) {
            for (FieldMergeAttributes mergeAttributes : values()) {
                String fieldPrefix = mergeAttributes.getFieldName();
                if (isFieldIdentifiedByPrefix(fieldName, fieldPrefix)) {
                    return Integer.parseInt(fieldName.replace(fieldPrefix, ""));
                }
            }
            return -1;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMergeAttributesInterface#getValueAdjuster()
         */
        @Override
        public ValueAdjuster getValueAdjuster() {
            return m_valueAdjuster;
        }
    }

    private static final String DDX_ID = "TEST";

    private static final int MAX_NUM_SUBJECT_CONTENTS = 9;

    private AssessmentDefinition m_assessmentDefinition = null;
    private Map<String, StudentAssessment> m_assessments = new HashMap<>();


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

        return FieldMergeAttributes.findFieldMergeAttributeByFieldName(fieldName);
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanDescriptor(java.lang.String)
     */
    @Override
    protected String getBeanDescriptor(String fieldName) {
        FieldInfo importingFieldInfo = getImportingFieldInfo(fieldName);
        int testNum = FieldMergeAttributes.getTestNum(importingFieldInfo.getFieldName());
        String gradeLevel = getImportingFieldInfo(FIELD_TEST_GRADE_LEVEL + testNum).getValue().trim();
        PlainDate date = (PlainDate) getAdjustedValue(FIELD_TEST_DATE + testNum);
        String testName = getImportingFieldInfo(FIELD_TEST_NAME + testNum).getValue().trim();

        return super.getBeanDescriptor(fieldName) + " for assessment definition " + DDX_ID + " with test name "
                + testName + ", date " + date + " and grade level " + gradeLevel;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getBeanMergeTo(java.lang.String)
     */
    @Override
    protected X2BaseBean getBeanMergeTo(String fieldName) {
        String key = getAssessmentKey(fieldName);
        if (key == null) {
            throw new X2RuntimeException();
        }

        return m_assessments.get(key);
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#getClassMergeTo()
     */
    @Override
    protected Class<?> getClassMergeTo() {
        return StudentAssessment.class;
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
        loadAssessmentDefinition();
        loadAssessments();
        adjustAliases();
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.RecordTypeMergeData#showMergeDescription(com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.FieldMerger,
     *      com.follett.fsc.core.k12.beans.X2BaseBean,
     *      com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo)
     */
    @Override
    protected void showMergeDescription(FieldMerger merger, X2BaseBean beanMergeTo, FieldInfo importingFieldInfo) {
        if (beanMergeTo == null) {
            int testNum = FieldMergeAttributes.getTestNum(importingFieldInfo.getFieldName());
            String gradeLevel = getImportingFieldInfo(FIELD_TEST_GRADE_LEVEL + testNum).getValue().trim();
            PlainDate date = (PlainDate) getAdjustedValue(FIELD_TEST_DATE + testNum);
            String testName = getImportingFieldInfo(FIELD_TEST_NAME + testNum).getValue().trim();

            addMessageLine("\t\tCannot be merged: Student Assessment with test name "
                    + testName + ", date " + date + " and grade level " + gradeLevel + " is not found");
        } else {
            super.showMergeDescription(merger, beanMergeTo, importingFieldInfo);
        }
    }


    /**
     * Adjust aliases.
     */
    private void adjustAliases() {
        for (String fieldName : getMergeableFieldNames()) {
            if (fieldName.startsWith(FIELD_TEST_SC) || fieldName.startsWith(FIELD_TEST_PAS_NUM)) {
                StudentAssessment assessment = (StudentAssessment) getBeanMergeTo(fieldName);
                if (assessment != null) {
                    FieldInfo scFieldInfo =
                            getImportingFieldInfo(FIELD_TEST_SC + FieldMergeAttributes.getTestNum(fieldName));
                    String subjectContent = scFieldInfo.getValue().trim();
                    if (!StringUtils.isEmpty(subjectContent)) {
                        boolean found = false;
                        for (int aliasIndex = 1; aliasIndex <= MAX_NUM_SUBJECT_CONTENTS; aliasIndex++) {
                            String scAliasToAdjust = FieldMergeAttributes.TEST_SC.getAliasToAdjust();
                            String subjectContentByIndex =
                                    (String) assessment.getFieldValueByAliasExtended(scAliasToAdjust + aliasIndex,
                                            scFieldInfo.getDictionary());
                            if (subjectContent.equals(subjectContentByIndex)) {
                                FieldMergeAttributes mergeAttributes =
                                        (FieldMergeAttributes) findFieldMergeAttributesByFieldName(fieldName);
                                String aliasToAdjust = mergeAttributes.getAliasToAdjust();
                                String ajustedAlias = aliasToAdjust + aliasIndex;

                                FieldInfo importingFieldInfo = getImportingFieldInfo(fieldName);
                                FieldBeanInfo fieldBeanInfo =
                                        new FieldBeanInfo(importingFieldInfo.getBaseClass(), ajustedAlias,
                                                importingFieldInfo.getDictionary());
                                importingFieldInfo.setFieldBeanInfo(fieldBeanInfo);
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            throw new X2RuntimeException();
                        }
                    }
                }
            }
        }
    }


    /**
     * Gets the assessment key.
     *
     * @param fieldName String
     * @return String
     */
    private String getAssessmentKey(String fieldName) {
        int testNum = FieldMergeAttributes.getTestNum(fieldName);
        if (testNum != -1) {
            String gradeLevel = getImportingFieldInfo(FIELD_TEST_GRADE_LEVEL + testNum).getValue().trim();
            PlainDate date = (PlainDate) getAdjustedValue(FIELD_TEST_DATE + testNum);
            String testName = getImportingFieldInfo(FIELD_TEST_NAME + testNum).getValue().trim();

            return new StringBuilder().append(gradeLevel).append(date).append(testName).toString();
        }
        return null;
    }


    /**
     * Gets the assessment key.
     *
     * @param assessment StudentAssessment
     * @return String
     */
    private String getAssessmentKey(StudentAssessment assessment) {
        String gradeLevelField = getImportingFieldInfo(FIELD_TEST_GRADE_LEVEL + 1).getBeanPath();
        String dateField = getImportingFieldInfo(FIELD_TEST_DATE + 1).getBeanPath();
        String testNameField = getImportingFieldInfo(FIELD_TEST_NAME + 1).getBeanPath();

        String gradeLevel = (String) assessment.getFieldValueByBeanPath(gradeLevelField);
        PlainDate date = (PlainDate) assessment.getFieldValueByBeanPath(dateField);
        String testName = (String) assessment.getFieldValueByBeanPath(testNameField);

        return new StringBuilder().append(gradeLevel).append(date).append(testName).toString();
    }


    /**
     * Checks if is field identified by prefix.
     *
     * @param fieldName String
     * @param fieldPrefix String
     * @return true, if is field identified by prefix
     */
    private static boolean isFieldIdentifiedByPrefix(String fieldName, String fieldPrefix) {
        return fieldName.startsWith(fieldPrefix) && fieldName.replace(fieldPrefix, "").length() < 2;
    }


    /**
     * Load assessment definition.
     */
    private void loadAssessmentDefinition() {
        X2Criteria asdCriteria = new X2Criteria();
        asdCriteria.addEqualTo(AssessmentDefinition.COL_ID, DDX_ID);
        QueryByCriteria asdQuery = new QueryByCriteria(AssessmentDefinition.class, asdCriteria);
        m_assessmentDefinition = (AssessmentDefinition) getBroker().getBeanByQuery(asdQuery);
        if (m_assessmentDefinition == null) {
            throw new X2RuntimeException();
        }
    }


    /**
     * Load assessments.
     */
    private void loadAssessments() {
        X2Criteria asmCriteria = new X2Criteria();
        asmCriteria.addEqualTo(StudentAssessment.COL_STUDENT_OID, getStudent().getOid());
        asmCriteria.addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER
                + AssessmentDefinition.COL_ID, DDX_ID);
        QueryByCriteria asmQuery = new QueryByCriteria(StudentAssessment.class, asmCriteria);
        Collection<StudentAssessment> assessments = getBroker().getCollectionByQuery(asmQuery);
        if (assessments != null) {
            for (StudentAssessment assessment : assessments) {
                String key = getAssessmentKey(assessment);
                m_assessments.put(key, assessment);
            }
        }
    }
}
