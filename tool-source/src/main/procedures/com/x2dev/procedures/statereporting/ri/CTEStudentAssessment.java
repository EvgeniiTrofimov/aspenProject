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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.AssessmentColumnDefinition;
import com.x2dev.sis.model.beans.GraduationProgram;
import com.x2dev.sis.model.beans.GraduationStudentProgram;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * RI state report for Class Roster export. This class implements the data
 * export for the RI Class Roster export.
 *
 * @author X2 Development Corporation
 */
public class CTEStudentAssessment extends CTEStateReportData {

    /**
     * Implementation of StateReportEntity to be used by the Class Roster export. This
     * must be a public static inner class with a public no argument constructor
     * so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class CTEStudentAssessmentEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";
        /*
         * Aliases
         */
        // STUDENT table
        private static final String ALIAS_TECH_CENTER_CODE = "Tech Center Code";

        /*
         * others
         */
        private CTEStudentAssessment m_CTEStudentAssessment;
        private List<String> m_studentAssessmentOids;

        /**
         * Empty no argument constructor for dynamic instantiation.
         */
        public CTEStudentAssessmentEntity() {
            // Empty no argument constructor for dynamic instantiation.
        }

        /**
         * get value from Student by alias DOE APRENTICESHIP CODE.
         *
         * @return String
         */
        public String getAssessmentFieldValue() {
            StudentAssessment studentAssessment = getCurrentStudentAssessment();
            String returnValue = (String) studentAssessment
                    .getFieldValueByBeanPath(m_CTEStudentAssessment.m_assessmentFieldJavaName);
            Map<String, ReferenceCode> map =
                    m_CTEStudentAssessment.getReferenceCodes(m_CTEStudentAssessment.m_assessmentFieldReferenceTableOid);
            if (map != null) {
                ReferenceCode code = map.get(returnValue);
                if (code != null) {
                    returnValue = code.getStateCode();
                }
            }
            return returnValue;
        }

        /**
         * get value from Student by alias Tech Center Code.
         *
         * @return String
         */
        public String getCareerTechCenterSchoolCode() {
            Student student = getCurrentStudent();
            return (String) student.getFieldValueByAlias(ALIAS_TECH_CENTER_CODE);
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
            buffer.append("Graduation Program Name: " + ((GraduationProgram) getBean()).getName());
            return buffer.toString();
        }

        /**
         * get State Id from Student.
         *
         * @return String
         */
        public String getSasid() {
            Student student = getCurrentStudent();
            return student.getStateId();

        }

        /**
         * get value from School by alias State School Id.
         *
         * @return String
         */
        public String getSchoolCode() {
            Student student = getCurrentStudent();
            return (String) student.getSchool().getFieldValueByAlias(ALIAS_STATE_SCHOOL_ID);
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
            GraduationProgram graduationProgram = (GraduationProgram) bean;
            m_CTEStudentAssessment = (CTEStudentAssessment) data;
            m_studentAssessmentOids = new ArrayList<String>();
            String programType = (String) graduationProgram.getFieldValueByAlias(ALIAS_DOE_RI_CTE_PROGRAM_TYPE);
            String key = programType;
            if (m_CTEStudentAssessment.m_studentAssesmentOidsMap.containsKey(key)) {
                Set<String> allAssessments = m_CTEStudentAssessment.m_studentAssesmentOidsMap.get(key);
                Set<String> filteredAssessments = new HashSet<String>();
                for (String asmOid : allAssessments) {
                    StudentAssessment asm = m_CTEStudentAssessment.m_studentAssesmentMap.get(asmOid);
                    SisStudent student = asm.getStudent();
                    if (m_CTEStudentAssessment.m_gsrMapByGprOid.get(graduationProgram.getOid()) != null
                            && m_CTEStudentAssessment.m_gsrMapByGprOid.get(graduationProgram.getOid())
                                    .get(student.getOid()) != null) {
                        String asmCteResult = null;
                        if (m_CTEStudentAssessment.m_asmCteResult != null) {
                            asmCteResult = (String) asm.getFieldValueByBeanPath(m_CTEStudentAssessment.m_asmCteResult);
                        }
                        if (!StringUtils.isEmpty(asmCteResult)
                                && m_CTEStudentAssessment.m_asmResultReferenceTableOid != null) {
                            asmCteResult = m_CTEStudentAssessment.lookupReferenceCodeByRefTbl(
                                    m_CTEStudentAssessment.m_asmResultReferenceTableOid, asmCteResult,
                                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        }
                        if (m_CTEStudentAssessment.m_asmCteResult == null
                                || (!StringUtils.isEmpty(asmCteResult) && "P".equals(asmCteResult))) {
                            PlainDate asmDate = asm.getDate();
                            if (asmDate == null || (!asmDate.before(m_CTEStudentAssessment.m_startDate) &&
                                    !asmDate.after(m_CTEStudentAssessment.m_endDate))) {
                                List<StudentScheduleSpan> scheduleSpans =
                                        m_CTEStudentAssessment.m_scheduleHelper.getStudentScheduleSpans(student);
                                for (StudentScheduleSpan scheduleSpan : scheduleSpans) {
                                    String schoolCourseOid = scheduleSpan.getSection().getSchoolCourseOid();
                                    if (m_CTEStudentAssessment.m_schoolCourseMap.keySet().contains(schoolCourseOid)) {
                                        PlainDate schEntryDate = scheduleSpan.getEntryDate();
                                        PlainDate schExitDate = scheduleSpan.getExitDate();
                                        if ((schExitDate == null
                                                || !m_CTEStudentAssessment.m_startDate.after(schExitDate))
                                                &&
                                                !schEntryDate.after(m_CTEStudentAssessment.m_endDate)) {
                                            filteredAssessments.add(asmOid);
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                m_studentAssessmentOids.addAll(filteredAssessments);
            }
            if (PROCEDURE_ID_EXPDATA_RI_CTE_PSCC.equals(m_CTEStudentAssessment.m_cteProcedureId)) {
                for (String asmOid : m_studentAssessmentOids) {
                    StudentAssessment studentAssessment = m_CTEStudentAssessment.m_studentAssesmentMap.get(asmOid);
                    String sasid = studentAssessment.getStudent().getStateId();
                    Collection<StudentAssessment> asms = m_CTEStudentAssessment.m_asmMap.get(sasid);
                    if (asms == null) {
                        asms = new ArrayList<>();
                        asms.add(studentAssessment);
                        m_CTEStudentAssessment.m_asmMap.put(sasid, asms);
                    } else {
                        asms.add(studentAssessment);
                    }
                }
                for (Entry<String, Collection<StudentAssessment>> entry : m_CTEStudentAssessment.m_asmMap.entrySet()) {
                    ArrayList<String> codes = new ArrayList<>();
                    for (StudentAssessment asm : entry.getValue()) {
                        String creditValue = (String) asm
                                .getFieldValueByBeanPath(m_CTEStudentAssessment.m_assessmentFieldJavaName);
                        Map<String, ReferenceCode> map =
                                m_CTEStudentAssessment
                                        .getReferenceCodes(m_CTEStudentAssessment.m_assessmentFieldReferenceTableOid);
                        if (map != null) {
                            ReferenceCode code = map.get(creditValue);
                            if (code != null) {
                                creditValue = code.getStateCode();
                                if (!StringUtils.isEmpty(creditValue)) {
                                    if (!codes.contains(creditValue)) {
                                        codes.add(creditValue);
                                    } else {
                                        m_studentAssessmentOids.remove(asm.getOid());
                                    }
                                }
                            }
                        }
                    }
                }
            }
            setRowCount(m_studentAssessmentOids.size());
        }

        /**
         * get current StudentAssessment.
         *
         * @return Student assessment
         */
        StudentAssessment getCurrentStudentAssessment() {
            String studentAssesmentOid = m_studentAssessmentOids.get(getCurrentRow());
            return m_CTEStudentAssessment.m_studentAssesmentMap.get(studentAssesmentOid);
        }

        /**
         * get current Student.
         *
         * @return Student
         */
        Student getCurrentStudent() {
            String studentAssesmentOid = m_studentAssessmentOids.get(getCurrentRow());
            StudentAssessment studentAssessment = m_CTEStudentAssessment.m_studentAssesmentMap.get(studentAssesmentOid);
            return studentAssessment.getStudent();
        }

    }

    /**
     * The Class RetrieveStudent.
     */
    class RetrieveStudent implements FieldRetriever {

        /*
         * Retriever Parameters
         */
        private static final String PARAM_APPRENTICESHIPCODE = "APPRENTICESHIPCODE";
        private static final String PARAM_CTECHCTR = "CTECHCTR";
        private static final String PARAM_PSCREDIT = "PSCREDITCODE";
        private static final String PARAM_RECCREDEARNEDCODE = "RECCREDEARNEDCODE";
        private static final String PARAM_SASID = "SASID";
        private static final String PARAM_SCHCODE = "SCHCODE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;

            CTEStudentAssessmentEntity courseEntity = (CTEStudentAssessmentEntity) entity;

            String param = (String) field.getParameter();
            if (PARAM_SASID.equals(param)) {
                value = courseEntity.getSasid();
            } else if (PARAM_APPRENTICESHIPCODE.equals(param) || PARAM_RECCREDEARNEDCODE.equals(param)
                    || PARAM_PSCREDIT.equals(param)) {
                value = courseEntity.getAssessmentFieldValue();
            } else if (PARAM_SCHCODE.equals(param)) {
                value = courseEntity.getSchoolCode();
            } else if (PARAM_CTECHCTR.equals(param)) {
                value = courseEntity.getCareerTechCenterSchoolCode();
            }
            return value;
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

            CTEStudentAssessmentEntity cteEntity = (CTEStudentAssessmentEntity) entity;

            StudentAssessment asm = cteEntity.getCurrentStudentAssessment();
            if (asm.getDate() == null) {
                Student student = cteEntity.getCurrentStudent();
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

    /**
     * Aliases
     */
    protected static final String ALIAS_ASM_RESULT = "RI CTE Assessment Result";
    /*
     * Retrievers id
     */
    private static final String RETRIVE_ID_STUDENT = "STUDENT";

    protected String m_asmCteResult = null;
    protected String m_asmResultReferenceTableOid = null;
    protected Map<String, Collection<StudentAssessment>> m_asmMap = new HashMap<>();
    protected PlainDate m_endDate = null;
    protected Map<String, Map<String, GraduationStudentProgram>> m_gsrMapByGprOid;
    protected String m_cteProcedureId;
    protected StudentHistoryHelper m_scheduleHelper;
    protected PlainDate m_startDate = null;

    /**
     * @see com.x2dev.procedures.statereporting.ri.CTEStateReportData#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();
        loadGsrMapByGpr();
        m_endDate = getCurrentContext().getEndDate();
        m_startDate = getCurrentContext().getStartDate();
        m_scheduleHelper = new StudentHistoryHelper(this);
        m_scheduleHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
        if (getParameter(PARAM_INCLUDE_DOE_EXCLUDE_STUDENTS) != null
                && getParameter(PARAM_INCLUDE_DOE_EXCLUDE_STUDENTS) instanceof Boolean) {
            Boolean exclude =
                    Boolean.valueOf(!((Boolean) getParameter(PARAM_INCLUDE_DOE_EXCLUDE_STUDENTS)).booleanValue());
            m_scheduleHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, exclude);
        }
        initializeVals();
    }

    /**
     * Initialize calcs.
     *
     * @see com.x2dev.procedures.statereporting.ri.CTEStateReportData#initializeCalcs()
     */
    @Override
    protected void initializeCalcs() {
        super.initializeCalcs();
        m_calcs.put(RETRIVE_ID_STUDENT, new RetrieveStudent());
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.ri.CTEStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        m_cteEntityClass = CTEStudentAssessmentEntity.class;
    }


    /**
     * @see com.x2dev.procedures.statereporting.ri.CTEStateReportData#initializeField()
     */
    @Override
    protected void initializeField() {
        super.initializeField();
        String assesmentFieldAlias = null;
        m_cteProcedureId = (String) getParameter(PARAM_PROCEDURE_ID);
        if (m_cteProcedureId != null && m_cteProcedureId.equals(PROCEDURE_ID_EXPDATA_RI_CTE_APRPR)) {
            assesmentFieldAlias = ALIAS_RI_CTE_APPRENTICESHIP;
        } else if (m_cteProcedureId != null && m_cteProcedureId.equals(PROCEDURE_ID_EXPDATA_RI_CTE_RCGNZ)) {
            assesmentFieldAlias = ALIAS_RI_CTE_CREDENTIAL_EARNED;
            m_asmCteResult = getStdAssesmentFieldByAlias(ALIAS_ASM_RESULT, true);
            AssessmentColumnDefinition colAsmResult = getStdAssesmentColumnByAlias(ALIAS_ASM_RESULT);
            if (colAsmResult != null) {
                m_asmResultReferenceTableOid = colAsmResult.getReferenceTableOid();
            }
        } else if (m_cteProcedureId != null && m_cteProcedureId.equals(PROCEDURE_ID_EXPDATA_RI_CTE_PSCC)) {
            assesmentFieldAlias = ALIAS_RI_CTE_POSTSECONDARY_CREDIT;
        }
        m_assessmentFieldJavaName = getStdAssesmentFieldByAlias(assesmentFieldAlias, true);
        AssessmentColumnDefinition col = getStdAssesmentColumnByAlias(assesmentFieldAlias);
        if (col != null) {
            m_assessmentFieldReferenceTableOid = col.getReferenceTableOid();
        }
    }

    /**
     * initialize Maps.
     *
     * @param graduationPrgrmCriteria X2Criteria
     */
    @Override
    protected void initializeMaps(X2Criteria graduationPrgrmCriteria) {
        initializeStudentsAssesmMap();
        initializeGraduationPrgmSchoolCourseMap(getGraduationPrgrmCriteria());
        initializeSchoolCourseMap();
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
    private void loadGsrMapByGpr() {
        X2Criteria gsrCriteria = new X2Criteria();
        SubQuery gprSubQuery = new SubQuery(GraduationProgram.class, X2BaseBean.COL_OID, getGraduationPrgrmCriteria());
        gsrCriteria.addIn(GraduationStudentProgram.COL_PROGRAM_STUDIES_OID, gprSubQuery);
        QueryByCriteria query = new QueryByCriteria(GraduationStudentProgram.class, gsrCriteria);
        m_gsrMapByGprOid = getBroker().getGroupedCollectionByQuery(query,
                new String[] {GraduationStudentProgram.COL_PROGRAM_STUDIES_OID,
                        GraduationStudentProgram.COL_STUDENT_OID},
                new int[] {256, 256});
    }
}
