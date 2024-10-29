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
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_TEST_FORM;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_TEST_GRADE_LEVEL;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_TEST_LEVEL;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_TEST_NAME;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_TEST_S;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_TEST_SC;
import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_TEST_ST;
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
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class FLFasterMerge08Procedure.
 */
public class FLFasterMerge08Procedure extends RecordTypeMergeData {

    /**
     * The Enum FieldMergeAttributes.
     */
    private enum FieldMergeAttributes implements FieldMergeAttributesInterface {
        TEST_GRADE_LEVEL(FIELD_TEST_GRADE_LEVEL, new FieldMerger("gradeLevelCode")),
        //
        TEST_DATE(FIELD_TEST_DATE, new FieldMerger("date"), new ValueAdjuster() {
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
        TEST_NAME(FIELD_TEST_NAME, new FieldMerger(DDX_ID, "asm-test-name")),
        //
        TEST_FORM(FIELD_TEST_FORM, new FieldMerger(DDX_ID, "asm-test-form")),
        //
        TEST_LEVEL(FIELD_TEST_LEVEL, new FieldMerger(DDX_ID, "asm-test-level")),


        TEST_SC(FIELD_TEST_SC, new FieldMerger(DDX_ID, null), "asm-test-subjectContent"),
        //
        TEST_ST_A(FIELD_TEST_ST, new FieldMerger(DDX_ID, null), "asm-test-scoreTypeA"),
        //
        TEST_S_A(FIELD_TEST_S, new FieldMerger(DDX_ID, null), "asm-test-scoreA"),
        //
        TEST_ST_B(FIELD_TEST_ST, new FieldMerger(DDX_ID, null), "asm-test-scoreTypeB"),
        //
        TEST_S_B(FIELD_TEST_S, new FieldMerger(DDX_ID, null), "asm-test-scoreB");

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
                if (fieldName.startsWith(fieldPrefix) && fieldName.replace(fieldPrefix, "").length() < 4) {
                    if (fieldPrefix.equals(FIELD_TEST_ST)) {
                        int lastNum = Integer.parseInt(fieldName.substring(fieldName.length() - 1));
                        switch (lastNum) {
                            case SCORE_TYPE_NUM_A:
                                return TEST_ST_A;

                            case SCORE_TYPE_NUM_B:
                                return TEST_ST_B;

                            default:
                                throw new X2RuntimeException();
                        }
                    }

                    if (fieldPrefix.equals(FIELD_TEST_S)) {
                        int lastNum = Integer.parseInt(fieldName.substring(fieldName.length() - 1));
                        switch (lastNum) {
                            case SCORE_TYPE_NUM_A:
                                return TEST_S_A;

                            case SCORE_TYPE_NUM_B:
                                return TEST_S_B;

                            default:
                                throw new X2RuntimeException();
                        }
                    }

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
         * Gets the subject content num.
         *
         * @param fieldName String
         * @return int
         */
        public static int getSubjectContentNum(String fieldName) {
            if (!(fieldName.startsWith(FIELD_TEST_SC) || fieldName.startsWith(FIELD_TEST_S)
                    || fieldName.startsWith(FIELD_TEST_ST))) {
                throw new X2RuntimeException();
            }
            for (FieldMergeAttributes mergeAttributes : values()) {
                String fieldPrefix = mergeAttributes.getFieldName();
                if (fieldName.startsWith(fieldPrefix) && fieldName.replace(fieldPrefix, "").length() < 4) {
                    return Integer.parseInt(fieldName.replace(fieldPrefix, "").substring(1, 2));
                }
            }
            return -1;
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
                if (fieldName.startsWith(fieldPrefix) && fieldName.replace(fieldPrefix, "").length() < 4) {
                    return Integer.parseInt(fieldName.replace(fieldPrefix, "").substring(0, 1));
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

    protected static final int SCORE_TYPE_NUM_A = 1;
    protected static final int SCORE_TYPE_NUM_B = 2;

    private static final String DDX_ID = "TEST";

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
        return super.getBeanDescriptor(fieldName) + " for assessment definition " + DDX_ID;
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
        StudentAssessment assessment = m_assessments.get(key);
        if (assessment == null) {
            assessment = getNewAssessment();
            m_assessments.put(key, assessment);
        }

        return assessment;
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
        adjustAliases();
        loadAssessmentDefinition();
        loadAssessments();
    }


    /**
     * Adjust aliases.
     */
    private void adjustAliases() {
        for (String fieldName : getMergeableFieldNames()) {
            if (fieldName.startsWith(FIELD_TEST_S) || fieldName.startsWith(FIELD_TEST_ST)
                    || fieldName.startsWith(FIELD_TEST_SC)) {
                FieldInfo fieldInfo = getImportingFieldInfo(fieldName);
                FieldMergeAttributes mergeAttributes =
                        (FieldMergeAttributes) findFieldMergeAttributesByFieldName(fieldName);
                String alias = mergeAttributes.getAliasToAdjust();
                String ajustedAlias = alias + FieldMergeAttributes.getSubjectContentNum(fieldName);
                FieldBeanInfo fieldBeanInfo =
                        new FieldBeanInfo(fieldInfo.getBaseClass(), ajustedAlias, fieldInfo.getDictionary());
                fieldInfo.setFieldBeanInfo(fieldBeanInfo);
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
            String testForm = getImportingFieldInfo(FIELD_TEST_FORM + testNum).getValue().trim();
            String testLevel = getImportingFieldInfo(FIELD_TEST_LEVEL + testNum).getValue().trim();

            return new StringBuilder().append(gradeLevel).append(date).append(testName).append(testForm)
                    .append(testLevel).toString();
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
        String testFormField = getImportingFieldInfo(FIELD_TEST_FORM + 1).getBeanPath();
        String testLevelField = getImportingFieldInfo(FIELD_TEST_LEVEL + 1).getBeanPath();

        String gradeLevel = (String) assessment.getFieldValueByBeanPath(gradeLevelField);
        PlainDate date = (PlainDate) assessment.getFieldValueByBeanPath(dateField);
        String testName = (String) assessment.getFieldValueByBeanPath(testNameField);
        String testForm = (String) assessment.getFieldValueByBeanPath(testFormField);
        String testLevel = (String) assessment.getFieldValueByBeanPath(testLevelField);

        return new StringBuilder().append(gradeLevel).append(date).append(testName).append(testForm)
                .append(testLevel).toString();
    }


    /**
     * Gets the new assessment.
     *
     * @return Student assessment
     */
    private StudentAssessment getNewAssessment() {
        StudentAssessment assessment = X2BaseBean.newInstance(StudentAssessment.class, getBroker().getPersistenceKey());
        assessment.setFieldValueByBeanPath(StudentAssessment.COL_STUDENT_OID, getStudent().getOid());
        assessment.setFieldValueByBeanPath(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID,
                m_assessmentDefinition.getOid());
        return assessment;
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
