/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ct;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.business.X2Broker;
import com.x2dev.procedures.statereporting.common.CriteriaHelper;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.function.BiFunction;

/**
 * The Class CTCPFAExportData.
 */
public class CTCPFAExportData extends StateReportData {

    /**
     * The Class CTCPFARecordEntity.
     */
    public static class CTCPFARecordEntity extends ToolsSharedContainer.StateReportEntity {

        List<CTToolStudentAssessment> m_stdAsms;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public CTCPFARecordEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Initialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(ToolsSharedContainer.StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            X2Broker broker = data.getBroker();
            CTToolStudent student = (CTToolStudent) bean;
            m_stdAsms = new ArrayList<>();
            if (student.getAssessmentsCU(broker) != null) {
                m_stdAsms.addAll(student.getAssessmentsCU(broker));
            }
            if (student.getAssessmentsMR(broker) != null) {
                m_stdAsms.addAll(student.getAssessmentsMR(broker));
            }
            if (student.getAssessmentsP15(broker) != null) {
                m_stdAsms.addAll(student.getAssessmentsP15(broker));
            }
            if (student.getAssessmentsP20(broker) != null) {
                m_stdAsms.addAll(student.getAssessmentsP20(broker));
            }
            if (student.getAssessmentsPU(broker) != null) {
                m_stdAsms.addAll(student.getAssessmentsPU(broker));
            }
            if (student.getAssessmentsSR(broker) != null) {
                m_stdAsms.addAll(student.getAssessmentsSR(broker));
            }
            if (!m_stdAsms.isEmpty()) {
                setRowCount(m_stdAsms.size());
            } else {
                setRowCount(0);
            }
        }

        /**
         * Gets the current record.
         *
         * @return CT tool student assessment
         */
        public CTToolStudentAssessment getCurrentRecord() {
            CTToolStudentAssessment currentAsm = null;
            if (m_stdAsms != null && getCurrentRow() < m_stdAsms.size() && getCurrentRow() >= 0) {
                currentAsm = m_stdAsms.get(getCurrentRow());
            }
            return currentAsm;
        }

        @Override
        public String getEntityName() {
            CTToolStudent student = (CTToolStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] ";

            return name;
        }
    }

    /**
     * The Class Student.
     */
    public static class CTToolStudent extends ToolStudent {
        private static final String ALIAS_STD_EXCLUDE_FROM_REPORTING = "DOE EXCLUDE STD";

        private static final String CHILD_KEY_ASM_CU = "asmCU";
        private static final String CHILD_KEY_ASM_MR = "asmMR";
        private static final String CHILD_KEY_ASM_P15 = "asmP15";
        private static final String CHILD_KEY_ASM_P20 = "asmP20";
        private static final String CHILD_KEY_ASM_PU = "asmPU";
        private static final String CHILD_KEY_ASM_SR = "asmSR";


        // Nonquery Fields
        public static final ToolBeanColumn FIELD_STD_DOB = new ToolBeanColumn(SisBeanPaths.STUDENT.person().dob());
        public static final ToolBeanColumn FIELD_STD_EXCLUDE_FROM_REPORTING =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_EXCLUDE_FROM_REPORTING);
        public static final ToolBeanColumn FIELD_STD_LOCAL_ID = new ToolBeanColumn(SisBeanPaths.STUDENT.localId());
        public static final ToolBeanColumn FIELD_STD_ORG_ID =
                new ToolBeanColumn(SisBeanPaths.STUDENT.school().organization1().id());
        public static final ToolBeanColumn FIELD_STD_STATE_ID = new ToolBeanColumn(SisBeanPaths.STUDENT.stateId());


        public static ToolBeanRelationship CHILD_ASSESSMENTS_CU =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.getBeanType(),
                        CTToolStudentAssessmentCU.class,
                        CHILD_KEY_ASM_CU,
                        SisBeanPaths.STUDENT_ASSESSMENT.studentOid().toString(),
                        SisBeanPaths.STUDENT.studentAssessments().getRelationshipType());
        public static ToolBeanRelationship CHILD_ASSESSMENTS_MR =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.getBeanType(),
                        CTToolStudentAssessmentMR.class,
                        CHILD_KEY_ASM_MR,
                        SisBeanPaths.STUDENT_ASSESSMENT.studentOid().toString(),
                        SisBeanPaths.STUDENT.studentAssessments().getRelationshipType());

        public static ToolBeanRelationship CHILD_ASSESSMENTS_P15 =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.getBeanType(),
                        CTToolStudentAssessmentP15.class,
                        CHILD_KEY_ASM_P15,
                        SisBeanPaths.STUDENT_ASSESSMENT.studentOid().toString(),
                        SisBeanPaths.STUDENT.studentAssessments().getRelationshipType());
        public static ToolBeanRelationship CHILD_ASSESSMENTS_P20 =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.getBeanType(),
                        CTToolStudentAssessmentP20.class,
                        CHILD_KEY_ASM_P20,
                        SisBeanPaths.STUDENT_ASSESSMENT.studentOid().toString(),
                        SisBeanPaths.STUDENT.studentAssessments().getRelationshipType());
        public static ToolBeanRelationship CHILD_ASSESSMENTS_PU =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.getBeanType(),
                        CTToolStudentAssessmentPU.class,
                        CHILD_KEY_ASM_PU,
                        SisBeanPaths.STUDENT_ASSESSMENT.studentOid().toString(),
                        SisBeanPaths.STUDENT.studentAssessments().getRelationshipType());
        public static ToolBeanRelationship CHILD_ASSESSMENTS_SR =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.getBeanType(),
                        CTToolStudentAssessmentSR.class,
                        CHILD_KEY_ASM_SR,
                        SisBeanPaths.STUDENT_ASSESSMENT.studentOid().toString(),
                        SisBeanPaths.STUDENT.studentAssessments().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStudent.FULL_DEFINITION
                .expand(FIELD_STD_DOB,
                        FIELD_STD_EXCLUDE_FROM_REPORTING,
                        FIELD_STD_LOCAL_ID,
                        FIELD_STD_ORG_ID,
                        FIELD_STD_STATE_ID)
                .expandRelationships(CHILD_ASSESSMENTS_CU,
                        CHILD_ASSESSMENTS_MR,
                        CHILD_ASSESSMENTS_P15,
                        CHILD_ASSESSMENTS_P20,
                        CHILD_ASSESSMENTS_PU,
                        CHILD_ASSESSMENTS_SR);

        /**
         * Instantiates a new Student.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public CTToolStudent(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the assessments CU.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<CTToolStudentAssessmentCU> getAssessmentsCU(X2Broker broker) {
            return (List<CTToolStudentAssessmentCU>) getChildren(broker, CHILD_ASSESSMENTS_CU);
        }

        /**
         * Gets the assessments MR.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<CTToolStudentAssessmentMR> getAssessmentsMR(X2Broker broker) {
            return (List<CTToolStudentAssessmentMR>) getChildren(broker, CHILD_ASSESSMENTS_MR);
        }

        /**
         * Gets the assessments P 15.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<CTToolStudentAssessmentP15> getAssessmentsP15(X2Broker broker) {
            return (List<CTToolStudentAssessmentP15>) getChildren(broker, CHILD_ASSESSMENTS_P15);
        }

        /**
         * Gets the assessments P 20.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<CTToolStudentAssessmentP20> getAssessmentsP20(X2Broker broker) {
            return (List<CTToolStudentAssessmentP20>) getChildren(broker, CHILD_ASSESSMENTS_P20);
        }

        /**
         * Gets the assessments PU.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<CTToolStudentAssessmentPU> getAssessmentsPU(X2Broker broker) {
            return (List<CTToolStudentAssessmentPU>) getChildren(broker, CHILD_ASSESSMENTS_PU);
        }

        /**
         * Gets the assessments SR.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<CTToolStudentAssessmentSR> getAssessmentsSR(X2Broker broker) {
            return (List<CTToolStudentAssessmentSR>) getChildren(broker, CHILD_ASSESSMENTS_SR);
        }
    }

    /**
     * The Class VAStudentAssessment.
     */
    public static class CTToolStudentAssessment extends ToolBean {

        public static final String ALIAS_ASM_RESULT_CODE = "all-asm-CPFAResultCode";
        public static final String ALIAS_SKL_STATE_ID = "StateId";

        public static final ToolBeanColumn FIELD_ASM_ASD_ID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT.assessmentDefinition().id());
        public static final ToolBeanColumn FIELD_ASM_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT.date(), false);
        public static final ToolBeanColumn FIELD_ASM_ASD_SUBJECT =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT.assessmentDefinition().subject());
        public static final ToolBeanColumn FIELD_ASM_SKL_STATE_ID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT.school(), ALIAS_SKL_STATE_ID);
        public static final ToolBeanColumn FIELD_ASM_STUDENT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT.studentOid());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION =
                ToolBean.FULL_DEFINITION
                        .expand(FIELD_ASM_ASD_ID,
                                FIELD_ASM_DATE,
                                FIELD_ASM_ASD_SUBJECT,
                                FIELD_ASM_SKL_STATE_ID,
                                FIELD_ASM_STUDENT_OID);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT_ASSESSMENT.getBeanType();
        }

        /**
         * Instantiates a new CT tool student assessment.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public CTToolStudentAssessment(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the asm asd subject.
         *
         * @return String
         */
        public String getAsmAsdSubject() {
            return getValueString(FIELD_ASM_ASD_SUBJECT);
        }

        /**
         * Gets the asm result code.
         * Override in child classes.
         *
         * @return String
         */
        public String getAsmResultCode() {
            return "";
        }

        /**
         * Gets the asm skl state id.
         *
         * @return the fieldAsmSklStateId
         */
        public String getAsmSklStateId() {
            return getValueString(FIELD_ASM_SKL_STATE_ID);
        }

        /**
         * Gets the student oid.
         *
         * @return String
         */
        public String getAsmStudentOid() {
            return getValueString(FIELD_ASM_STUDENT_OID);
        }
    }

    /**
     * The Class CTToolStudentAssessmentCU.
     */
    public static class CTToolStudentAssessmentCU extends CTToolStudentAssessment {

        private static final String ASD_ID = "CPFA CU";

        public static final ToolBeanColumn FIELD_ASM_RESULT_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT, ALIAS_ASM_RESULT_CODE, ASD_ID);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = CTToolStudentAssessment.FULL_DEFINITION
                .expand(FIELD_ASM_RESULT_CODE)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(FIELD_ASM_ASD_ID.resolve(getDictionaryExtractor()), ASD_ID);
                        return criteria;
                    }
                });

        /**
         * Instantiates a new CT tool student assessment CU.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public CTToolStudentAssessmentCU(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the asm result code.
         *
         * @return String
         * @see com.x2dev.procedures.statereporting.ct.CTCPFAExportData.CTToolStudentAssessment#getAsmResultCode()
         */
        @Override
        public String getAsmResultCode() {
            return getValueReferenceState(FIELD_ASM_RESULT_CODE);
        }
    }


    /**
     * The Class CTToolStudentAssessmentMR.
     */
    public static class CTToolStudentAssessmentMR extends CTToolStudentAssessment {

        private static final String ASD_ID = "CPFA MR";
        public static final ToolBeanColumn FIELD_ASM_RESULT_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT, ALIAS_ASM_RESULT_CODE, ASD_ID);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = CTToolStudentAssessment.FULL_DEFINITION
                .expand(FIELD_ASM_RESULT_CODE)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(FIELD_ASM_ASD_ID.resolve(getDictionaryExtractor()), ASD_ID);
                        return criteria;
                    }
                });

        /**
         * Instantiates a new CT tool student assessment MR.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public CTToolStudentAssessmentMR(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the asm result code.
         *
         * @return String
         * @see com.x2dev.procedures.statereporting.ct.CTCPFAExportData.CTToolStudentAssessment#getAsmResultCode()
         */
        @Override
        public String getAsmResultCode() {
            return getValueReferenceState(FIELD_ASM_RESULT_CODE);
        }
    }

    /**
     * The Class CTToolStudentAssessmentP15.
     */
    public static class CTToolStudentAssessmentP15 extends CTToolStudentAssessment {

        private static final String ASD_ID = "CPFA P15";

        public static final ToolBeanColumn FIELD_ASM_RESULT_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT, ALIAS_ASM_RESULT_CODE, ASD_ID);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = CTToolStudentAssessment.FULL_DEFINITION
                .expand(FIELD_ASM_RESULT_CODE)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(FIELD_ASM_ASD_ID.resolve(getDictionaryExtractor()), ASD_ID);
                        return criteria;
                    }
                });

        /**
         * Instantiates a new CT tool student assessment P 15.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public CTToolStudentAssessmentP15(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the asm result code.
         *
         * @return String
         * @see com.x2dev.procedures.statereporting.ct.CTCPFAExportData.CTToolStudentAssessment#getAsmResultCode()
         */
        @Override
        public String getAsmResultCode() {
            return getValueReferenceState(FIELD_ASM_RESULT_CODE);
        }
    }

    /**
     * The Class CTToolStudentAssessmentP20.
     */
    public static class CTToolStudentAssessmentP20 extends CTToolStudentAssessment {
        private static final String ASD_ID = "CPFA P20";

        public static final ToolBeanColumn FIELD_ASM_RESULT_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT, ALIAS_ASM_RESULT_CODE, ASD_ID);
        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = CTToolStudentAssessment.FULL_DEFINITION
                .expand(FIELD_ASM_RESULT_CODE)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(FIELD_ASM_ASD_ID.resolve(getDictionaryExtractor()), ASD_ID);
                        return criteria;
                    }
                });

        /**
         * Instantiates a new CT tool student assessment P 20.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public CTToolStudentAssessmentP20(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the asm result code.
         *
         * @return String
         * @see com.x2dev.procedures.statereporting.ct.CTCPFAExportData.CTToolStudentAssessment#getAsmResultCode()
         */
        @Override
        public String getAsmResultCode() {
            return getValueReferenceState(FIELD_ASM_RESULT_CODE);
        }
    }

    /**
     * The Class CTToolStudentAssessmentPU.
     */
    public static class CTToolStudentAssessmentPU extends CTToolStudentAssessment {

        private static final String ASD_ID = "CPFA PU";

        public static final ToolBeanColumn FIELD_ASM_RESULT_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT, ALIAS_ASM_RESULT_CODE, ASD_ID);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = CTToolStudentAssessment.FULL_DEFINITION
                .expand(FIELD_ASM_RESULT_CODE)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(FIELD_ASM_ASD_ID.resolve(getDictionaryExtractor()), ASD_ID);
                        return criteria;
                    }
                });

        /**
         * Instantiates a new CT tool student assessment PU.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public CTToolStudentAssessmentPU(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the asm result code.
         *
         * @return String
         * @see com.x2dev.procedures.statereporting.ct.CTCPFAExportData.CTToolStudentAssessment#getAsmResultCode()
         */
        @Override
        public String getAsmResultCode() {
            return getValueReferenceState(FIELD_ASM_RESULT_CODE);
        }
    }

    /**
     * The Class CTToolStudentAssessmentSR.
     */
    public static class CTToolStudentAssessmentSR extends CTToolStudentAssessment {
        private static final String ASD_ID = "CPFA SR";

        public static final ToolBeanColumn FIELD_ASM_RESULT_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT, ALIAS_ASM_RESULT_CODE, ASD_ID);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = CTToolStudentAssessment.FULL_DEFINITION
                .expand(FIELD_ASM_RESULT_CODE)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(FIELD_ASM_ASD_ID.resolve(getDictionaryExtractor()), ASD_ID);
                        return criteria;
                    }
                });

        /**
         * Instantiates a new CT tool student assessment SR.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public CTToolStudentAssessmentSR(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the asm result code.
         *
         * @return String
         * @see com.x2dev.procedures.statereporting.ct.CTCPFAExportData.CTToolStudentAssessment#getAsmResultCode()
         */
        @Override
        public String getAsmResultCode() {
            return getValueReferenceState(FIELD_ASM_RESULT_CODE);
        }
    }

    /**
     * The Class RetrieveASMData.
     */
    protected class RetrieveASMData implements FieldRetriever {

        private static final String CALC_ID = "CPFA_ASM";

        private static final String CALC_PARAM_ASM_RESULT = "ASM-RESULT";
        private static final String CALC_PARAM_ASM_SKL_ID = "SKL-ID";
        private static final String CALC_PARAM_ASM_TYPE = "ASM-TYPE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            CTCPFARecordEntity ctEntity = (CTCPFARecordEntity) entity;
            CTToolStudentAssessment asmToOperate = ctEntity.getCurrentRecord();
            String valueToReturn = null;
            if (asmToOperate != null) {
                String param = (String) field.getParameter();
                if (CALC_PARAM_ASM_RESULT.equals(param)) {
                    valueToReturn = asmToOperate.getAsmResultCode();
                } else if (CALC_PARAM_ASM_SKL_ID.equals(param)) {
                    valueToReturn = asmToOperate.getAsmSklStateId();
                } else if (CALC_PARAM_ASM_TYPE.equals(param)) {
                    valueToReturn = asmToOperate.getAsmAsdSubject();
                }
            }
            return valueToReturn;
        }
    }

    /**
     * Input parameters
     */
    public static final String INPUT_ASD_CU = "asdCU";
    public static final String INPUT_ASD_MR = "asdMR";
    public static final String INPUT_ASD_P15 = "asdP15";
    public static final String INPUT_ASD_P20 = "asdP20";
    public static final String INPUT_ASD_PU = "asdPU";
    public static final String INPUT_ASD_SR = "asdSR";
    public static final String INPUT_ASM_DATE_END = "asmDateEnd";
    public static final String INPUT_ASM_DATE_START = "asmDateStart";

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        if (getSetupErrors().size() == 0) {
            ToolBean.setDictionaryExtractor(getDictionaryExtractor());
            ToolBean.registerClass(CTToolStudent.class);
            ToolBean.registerClass(CTToolStudentAssessment.class);

            EnrollmentSpanCriteria spanCriteria = new EnrollmentSpanCriteria()
                    .setCurrentContext(getCurrentContext())
                    .setEndDate((PlainDate) getParameter(INPUT_ASM_DATE_END))
                    .setIncludeSecondarySpans(false)
                    .setExcludeStudent(CTToolStudent.FIELD_STD_EXCLUDE_FROM_REPORTING);
            if (isSchoolContext()) {
                spanCriteria.setSchoolOids(Arrays.asList(getSchool().getOid()));
            }
            X2Criteria inputCriteria = new X2Criteria();
            applyInputCriteria(inputCriteria, false, null);
            if (!inputCriteria.isEmpty()) {
                spanCriteria.setStudentLimitingCriteria(inputCriteria);
            }

            X2Criteria candidateCriteria = CriteriaHelper.getStudentCandidateCriteria(spanCriteria, getBroker());
            setFilterable(FilterableFactory.create(getBroker(), getDictionaryExtractor(), CTToolStudent.class,
                    candidateCriteria, null));
            setEntityClass(CTCPFARecordEntity.class);
            loadAssessments();

            // Add any retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveASMData.CALC_ID, new RetrieveASMData());
            super.addCalcs(calcs);
        }
    }

    /**
     * Load assessments.
     */
    private void loadAssessments() {
        PlainDate asmDateEnd = (PlainDate) getParameter(INPUT_ASM_DATE_END);
        PlainDate asmDateStart = (PlainDate) getParameter(INPUT_ASM_DATE_START);

        // Preload assessments
        X2Criteria criteria = new X2Criteria();
        criteria.addGreaterOrEqualThan(CTToolStudentAssessment.FIELD_ASM_DATE.resolve(getDictionaryExtractor()),
                asmDateStart);
        criteria.addLessOrEqualThan(CTToolStudentAssessment.FIELD_ASM_DATE.resolve(getDictionaryExtractor()),
                asmDateEnd);
        ToolBean.addAndCriteria(getBroker(), CTToolStudentAssessment.class, criteria);
        if (((Boolean) getParameter(INPUT_ASD_CU)).booleanValue()) {
            ToolBean.addAndCriteria(getBroker(), CTToolStudentAssessmentCU.class, criteria);
            ToolBean.preload(getBroker(), getDictionaryExtractor(),
                    Arrays.asList(CTToolStudentAssessment.FIELD_ASM_DATE),
                    CTToolStudent.CHILD_ASSESSMENTS_CU);
        }
        if (((Boolean) getParameter(INPUT_ASD_MR)).booleanValue()) {
            ToolBean.addAndCriteria(getBroker(), CTToolStudentAssessmentMR.class, criteria);
            ToolBean.preload(getBroker(), getDictionaryExtractor(),
                    Arrays.asList(CTToolStudentAssessment.FIELD_ASM_DATE),
                    CTToolStudent.CHILD_ASSESSMENTS_MR);
        }
        if (((Boolean) getParameter(INPUT_ASD_P15)).booleanValue()) {
            ToolBean.addAndCriteria(getBroker(), CTToolStudentAssessmentP15.class, criteria);
            ToolBean.preload(getBroker(), getDictionaryExtractor(),
                    Arrays.asList(CTToolStudentAssessment.FIELD_ASM_DATE),
                    CTToolStudent.CHILD_ASSESSMENTS_P15);
        }
        if (((Boolean) getParameter(INPUT_ASD_P20)).booleanValue()) {
            ToolBean.addAndCriteria(getBroker(), CTToolStudentAssessmentP20.class, criteria);
            ToolBean.preload(getBroker(), getDictionaryExtractor(),
                    Arrays.asList(CTToolStudentAssessment.FIELD_ASM_DATE),
                    CTToolStudent.CHILD_ASSESSMENTS_P20);
        }
        if (((Boolean) getParameter(INPUT_ASD_PU)).booleanValue()) {
            ToolBean.addAndCriteria(getBroker(), CTToolStudentAssessmentPU.class, criteria);
            ToolBean.preload(getBroker(), getDictionaryExtractor(),
                    Arrays.asList(CTToolStudentAssessment.FIELD_ASM_DATE),
                    CTToolStudent.CHILD_ASSESSMENTS_PU);
        }
        if (((Boolean) getParameter(INPUT_ASD_SR)).booleanValue()) {
            ToolBean.addAndCriteria(getBroker(), CTToolStudentAssessmentSR.class, criteria);
            ToolBean.preload(getBroker(), getDictionaryExtractor(),
                    Arrays.asList(CTToolStudentAssessment.FIELD_ASM_DATE),
                    CTToolStudent.CHILD_ASSESSMENTS_SR);
        }
    }
}
