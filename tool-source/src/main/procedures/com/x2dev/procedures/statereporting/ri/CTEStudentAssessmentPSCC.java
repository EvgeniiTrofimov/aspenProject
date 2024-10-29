/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ri;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.AssessmentColumnDefinition;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.GraduationProgram;
import com.x2dev.sis.model.beans.GraduationStudentProgram;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import org.apache.commons.lang.StringUtils;


/**
 * RI state report for Class Roster export. This class implements the data
 * export for the RI Class Roster export.
 *
 * @author X2 Development Corporation
 */
public class CTEStudentAssessmentPSCC extends CTEStateReportData {

    /**
     * Implementation of StateReportEntity to be used by the Class Roster export. This
     * must be a public static inner class with a public no argument constructor
     * so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class CTEStudentAssessmentEntityPSCC extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";

        private GraduationProgram m_gradProgram;

        /**
         * Empty no argument constructor for dynamic instantiation.
         */
        public CTEStudentAssessmentEntityPSCC() {
            // Empty no argument constructor for dynamic instantiation.
        }

        /**
         * Returns the display name of the represented entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            StringBuilder buffer = new StringBuilder();
            buffer.append("Student Name: " + ((StudentAssessment) getBean()).getStudent().getNameView());
            return buffer.toString();
        }

        /**
         * @return the m_gradProgram
         */
        public GraduationProgram getGradProgram() {
            return m_gradProgram;
        }

        /**
         * initialize the entity.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            StudentAssessment assessment = (StudentAssessment) bean;
            String cteProgCode =
                    (String) assessment.getFieldValueByAlias(ALIAS_RI_CTE_CAREER_PROGRAM_CATAGORY,
                            data.getDataDictionary());
            Collection<GraduationStudentProgram> progs = assessment.getStudent().getProgramStudies();
            if (progs.size() == 1) {
                m_gradProgram = progs.iterator().next().getProgramStudies();
            } else {
                for (GraduationStudentProgram gradProg : progs) {
                    String cteProgType =
                            (String) gradProg.getProgramStudies().getFieldValueByAlias(ALIAS_DOE_RI_CTE_PROGRAM_TYPE);
                    if (StringUtils.equals(cteProgType, cteProgCode)) {
                        m_gradProgram = gradProg.getProgramStudies();
                        break;
                    } else if (StringUtils.isNotEmpty(cteProgType)) {
                        m_gradProgram = gradProg.getProgramStudies();
                    }
                }
            }
        }

    }

    /**
     * The Class RetrieveStudent.
     */
    class RetrieveGraduationProgram implements FieldRetriever {
        /*
         * Retrievers id
         */
        public static final String RETRIVE_ID = "GRADPROGRAM";
        public static final String DIST_ID_PARAM = "CALC-DIST-ID";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;

            CTEStudentAssessmentEntityPSCC psccEntity = (CTEStudentAssessmentEntityPSCC) entity;
            GraduationProgram gradProgram = psccEntity.getGradProgram();

            String param = (String) field.getParameter();
            if (StringUtils.equals(DIST_ID_PARAM, param)) {
                value = gradProgram.getFieldValueByBeanPath(m_cteProgramDistrictId);
                if (StringUtils.isEmpty((String) value)) {
                    value = getOrganization().getFieldValueByBeanPath(m_districtCode);
                }
                return value;
            }
            return gradProgram.getFieldValueByAlias(param);

        }

    }
    /**
     * The Class RetrieveStudent.
     */
    class RetrievePSCC implements FieldRetriever {
        /*
         * Retrievers id
         */
        public static final String RETRIVE_ID = "PSCC_VALUE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            CTEStudentAssessmentEntityPSCC psccEntity = (CTEStudentAssessmentEntityPSCC) entity;
            StudentAssessment asm = (StudentAssessment) psccEntity.getBean();

            String param = (String) field.getParameter();
            return asm.getFieldValueByAlias(param, data.getDataDictionary());

        }

    }

    /**
     * Validate assessment date.
     *
     * @author Follett Software Company
     */
    public class ValidateAssessmentDate implements FieldValidator {
        public static final String VAL_ID = "VAL-ASM-DATE";

        private static final String PARAM_APPRENTICESHIPCODE = "APPRENTICESHIPCODE";
        private static final String PARAM_PSCREDIT = "PSCREDITCODE";
        private static final String PARAM_RECCREDEARNEDCODE = "RECCREDEARNEDCODE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            CTEStudentAssessmentEntityPSCC cteEntity = (CTEStudentAssessmentEntityPSCC) entity;

            StudentAssessment asm = (StudentAssessment) cteEntity.getBean();
            if (asm.getDate() == null) {
                Student student = asm.getStudent();
                String studentName = student.getNameView();
                String studentID = student.getLocalId();
                String assessmentName = asm.getAssessmentDefinition().getName();
                String assessmentValueName = null;

                switch (field.getFieldId()) {
                    case PARAM_APPRENTICESHIPCODE:
                        assessmentValueName = ALIAS_RI_CTE_APPRENTICESHIP;
                        break;
                    case PARAM_PSCREDIT:
                        assessmentValueName = ALIAS_RI_CTE_POSTSECONDARY_CREDIT;
                        break;
                    case PARAM_RECCREDEARNEDCODE:
                        assessmentValueName = ALIAS_RI_CTE_CREDENTIAL_EARNED;
                        break;

                    default:
                        break;
                }

                StringBuilder message = new StringBuilder();
                message.append("Student Name: " + studentName);
                message.append(", ");
                message.append("Student Id: " + studentID);
                message.append(", ");
                message.append("Assessment Definition Name: " + assessmentName);
                message.append(", ");
                message.append(assessmentValueName + ": " + value);

                String errorText = "Student Assesment date is missed";

                StateReportValidationError error =
                        new StateReportValidationError(cteEntity, field, errorText, message.toString());

                errors.add(error);
            }

            return errors;
        }

    }

    protected PlainDate m_startDate = null;
    protected PlainDate m_endDate = null;
    protected StudentHistoryHelper m_scheduleHelper;

    /**
     * @see com.x2dev.procedures.statereporting.ri.CTEStateReportData#initialize()
     */
    @Override
    protected void initialize() {
        initializeEntityClass();
        initializeVals();
        initializeCalcs();
        initializeField();

        m_startDate = getCurrentContext().getStartDate();
        m_endDate = getCurrentContext().getEndDate();
        m_scheduleHelper = new StudentHistoryHelper(this);
        m_scheduleHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        if (getParameter(PARAM_INCLUDE_DOE_EXCLUDE_STUDENTS) != null
                && getParameter(PARAM_INCLUDE_DOE_EXCLUDE_STUDENTS) instanceof Boolean) {
            Boolean exclude = new Boolean(!((Boolean) getParameter(PARAM_INCLUDE_DOE_EXCLUDE_STUDENTS)).booleanValue());
            m_scheduleHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, exclude);
        }

        X2Criteria studentCriteria = m_scheduleHelper.getStudentCriteria();
        studentCriteria.addIn(X2BaseBean.COL_OID, getCTEProgramQuery());
        SubQuery studentQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);

        X2Criteria studentAssessmentCriteria = new X2Criteria();
        studentAssessmentCriteria.addIn(StudentAssessment.COL_STUDENT_OID, studentQuery);
        studentAssessmentCriteria.addNotEmpty(m_assessmentFieldJavaName, getBroker().getPersistenceKey());
        studentAssessmentCriteria
                .addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                        AssessmentDefinition.COL_ID, ASSMT_DEF_ID_CTE);

        X2Criteria studentAssessmentDateCriteria = new X2Criteria();
        studentAssessmentDateCriteria.addBetween(StudentAssessment.COL_DATE, m_startDate, m_endDate);

        X2Criteria studentAssessmentNullCriteria = new X2Criteria();
        studentAssessmentNullCriteria.addIsNull(StudentAssessment.COL_DATE);
        studentAssessmentDateCriteria.addOrCriteria(studentAssessmentNullCriteria);

        studentAssessmentCriteria.addAndCriteria(studentAssessmentDateCriteria);

        BeanQuery assessmentQuery = new BeanQuery(StudentAssessment.class, studentAssessmentCriteria);
        setQuery(assessmentQuery);

        X2Criteria cteAssessmentDefCriteria = new X2Criteria();
        cteAssessmentDefCriteria.addEqualTo(AssessmentDefinition.COL_ID, ASSMT_DEF_ID_CTE);
        BeanQuery cteAssessmentDefQuery = new BeanQuery(AssessmentDefinition.class, cteAssessmentDefCriteria);
        AssessmentDefinition definition = getBroker().getBeanByQuery(cteAssessmentDefQuery);
        DataDictionary dict = DataDictionary.getDistrictDictionary(definition, getBroker().getPersistenceKey());
        setDataDictionary(dict);

    }

    /**
     * Initialize calcs.
     *
     * @see com.x2dev.procedures.statereporting.ri.CTEStateReportData#initializeCalcs()
     */
    @Override
    protected void initializeCalcs() {
        super.initializeCalcs();
        m_calcs.put(RetrieveGraduationProgram.RETRIVE_ID, new RetrieveGraduationProgram());
        m_calcs.put(RetrievePSCC.RETRIVE_ID, new RetrievePSCC());
        super.addCalcs(m_calcs);
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.ri.CTEStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        super.setEntityClass(CTEStudentAssessmentEntityPSCC.class);
    }


    /**
     * @see com.x2dev.procedures.statereporting.ri.CTEStateReportData#initializeField()
     */
    @Override
    protected void initializeField() {
        super.initializeField();
        m_assessmentFieldJavaName = getStdAssesmentFieldByAlias(ALIAS_RI_CTE_POSTSECONDARY_CREDIT, true);
        AssessmentColumnDefinition col = getStdAssesmentColumnByAlias(ALIAS_RI_CTE_POSTSECONDARY_CREDIT);
        if (col != null) {
            m_assessmentFieldReferenceTableOid = col.getReferenceTableOid();
        }
    }

    /**
     * Initialize validations.
     */
    protected void initializeVals() {
        HashMap<String, FieldValidator> valsMap = new HashMap<String, FieldValidator>();

        valsMap.put(ValidateAssessmentDate.VAL_ID, new ValidateAssessmentDate());
    }

    /**
     * Load amp of GraduationStudentProgram keyed by GraduationProgram Oid.
     */
    private SubQuery getCTEProgramQuery() {
        X2Criteria gsrCriteria = new X2Criteria();
        SubQuery gprSubQuery = new SubQuery(GraduationProgram.class, X2BaseBean.COL_OID, getGraduationPrgrmCriteria());
        gsrCriteria.addIn(GraduationStudentProgram.COL_PROGRAM_STUDIES_OID, gprSubQuery);
        SubQuery query =
                new SubQuery(GraduationStudentProgram.class, GraduationStudentProgram.COL_STUDENT_OID, gsrCriteria);
        return query;
    }
}
