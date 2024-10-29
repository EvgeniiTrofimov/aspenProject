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
package com.x2dev.procedures.statereporting.ri;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * RI state report for Class Roster export. This class implements the data
 * export for the RI Class Roster export.
 *
 * @author X2 Development Corporation
 */
public class ProgramStatus extends RIStateReportData {

    /**
     * The Class CTEApprenticeship.
     */
    public class CTEApprenticeship {
        public static final String ALIAS_CTE_RESULT = "RI CTE Assessment Result";
        public static final String ALIAS_CTE_PGRM_TYPE = "DOE RI CTE PROGRAM TYPE";
        public static final String ALIAS_RI_CTE_CREDENTIAL_EARNED = "RI CTE REC Credential";
        protected String m_asmFieldCteResultField = null;
        protected String m_assessmentFieldJavaName = null;
        protected String m_fieldCTEPgrmType = null;
        protected X2Criteria m_studentCritera = null;
        protected Map<String, Collection<GraduationStudentProgram>> m_stdGraduationProgramMap = null;
        protected Map<String, Collection<StudentAssessment>> m_studentAssesmentMap = null;

        /**
         * Instantiates a new CTE apprenticeship.
         *
         * @param stdCriteria X2Criteria
         */
        CTEApprenticeship(X2Criteria stdCriteria) {
            m_studentCritera = stdCriteria.copy();
        }

        /**
         * Gets the new student criteria.
         *
         * @return X 2 criteria
         */
        public X2Criteria getNewStudentCriteria() {
            List<String> selection = new ArrayList<String>();
            for (String stdOid : m_studentAssesmentMap.keySet()) {
                Collection<GraduationStudentProgram> grPrograms = m_stdGraduationProgramMap.get(stdOid);
                if (grPrograms != null && grPrograms.size() > 0) {
                    selection.add(stdOid);
                }
            }
            X2Criteria stdCritera = new X2Criteria();
            stdCritera.addIn(X2BaseBean.COL_OID, selection);
            return stdCritera;
        }

        /**
         * Gets the graduation programm.
         *
         * @param stdOid String
         * @return List
         */
        public List<GraduationStudentProgram> getGraduationProgramm(String stdOid) {
            Collection<GraduationStudentProgram> collection = m_stdGraduationProgramMap.get(stdOid);
            collection = collection == null ? new ArrayList<GraduationStudentProgram>() : collection;
            return new ArrayList(collection);
        }

        /**
         * Gets the graduation program map.
         *
         * @return Map
         */
        public Map<String, Collection<GraduationStudentProgram>> getGraduationProgrammMap() {
            return m_stdGraduationProgramMap;
        }

        /**
         * Gets the student assessment map.
         *
         * @return Map
         */
        public Map<String, Collection<StudentAssessment>> getStudentAssessmentMap() {
            return m_studentAssesmentMap;
        }

        /**
         * Gets the student assessments.
         *
         * @param stdOid String
         * @return List
         */
        public List<StudentAssessment> getStudentAssessments(String stdOid) {
            Collection<StudentAssessment> collection = m_studentAssesmentMap.get(stdOid);
            collection = collection == null ? new ArrayList<StudentAssessment>() : collection;
            return new ArrayList(collection);
        }

        /**
         * Initialize.
         */
        public void initialize() {
            this.initializeFields();
            loadCteResultCodes();
            initializeStdGraduationProgramMap();
            initStudentsAssesmMap();
        }

        /**
         * Initialize std graduation program map.
         */
        private void initializeStdGraduationProgramMap() {
            if (m_fieldCTEPgrmType != null) {
                X2Criteria graduationProgramMap = new X2Criteria();
                graduationProgramMap.addEqualTo(GraduationStudentProgram.REL_PROGRAM_STUDIES + PATH_DELIMITER
                        + GraduationProgram.COL_OBSOLETE_INDICATOR, Boolean.FALSE);
                graduationProgramMap.addNotEmpty(
                        GraduationStudentProgram.REL_PROGRAM_STUDIES + PATH_DELIMITER + m_fieldCTEPgrmType,
                        getBroker().getPersistenceKey());
                graduationProgramMap.addIn(GraduationStudentProgram.COL_STUDENT_OID,
                        new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentCritera));
                QueryByCriteria byCriteria = new QueryByCriteria(GraduationStudentProgram.class, graduationProgramMap);

                m_stdGraduationProgramMap = getBroker().getGroupedCollectionByQuery(byCriteria,
                        GraduationStudentProgram.COL_STUDENT_OID, 100);
            } else {
                m_stdGraduationProgramMap = new HashMap();
            }
        }

        /**
         * Inits the students assesm map.
         */
        private void initStudentsAssesmMap() {
            if (m_assessmentFieldJavaName != null) {
                Criteria studentsAssesmCriteria = new X2Criteria();
                studentsAssesmCriteria
                        .addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                                AssessmentDefinition.COL_ID, ASSMT_DEF_ID_CTE);
                studentsAssesmCriteria.addIn(m_asmFieldCteResultField, m_cteResultCodes);
                studentsAssesmCriteria.addNotNull(m_assessmentFieldJavaName);
                studentsAssesmCriteria.addIn(StudentAssessment.COL_STUDENT_OID,
                        new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentCritera));
                studentsAssesmCriteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE,
                        getCurrentContext().getStartDate());
                studentsAssesmCriteria.addLessOrEqualThan(StudentAssessment.COL_DATE, getCurrentContext().getEndDate());
                if (isSchoolContext()) {
                    studentsAssesmCriteria.addEqualTo(StudentAssessment.COL_SCHOOL_OID, getSchool().getOid());
                }
                QueryByCriteria byCriteria = new QueryByCriteria(StudentAssessment.class, studentsAssesmCriteria);
                m_studentAssesmentMap =
                        getBroker().getGroupedCollectionByQuery(byCriteria, StudentAssessment.COL_STUDENT_OID, 100);
            } else {
                m_studentAssesmentMap = new HashMap();
            }
        }

        /**
         * Initialize fields.
         */
        private void initializeFields() {
            m_fieldCTEPgrmType = translateAliasToJavaName(ALIAS_CTE_PGRM_TYPE, false);
            m_assessmentFieldJavaName = getStdAssesmentFieldByAlias(ALIAS_RI_CTE_CREDENTIAL_EARNED, false);
            m_asmFieldCteResultField = getStdAssesmentFieldByAlias(ALIAS_CTE_RESULT, false);
        }

        /**
         * Load codes by asd.[RI CTE Assessment Result] mapped to "P" state code.
         */
        private void loadCteResultCodes() {
            m_cteResultCodes = new ArrayList<String>();
            String referenceTableOid = findReferenceTableInStAssessmentFieldByAlias(
                    CTEApprenticeship.ALIAS_CTE_RESULT);
            if (!StringUtils.isEmpty(referenceTableOid)) {
                ReferenceTable referenceTable = getBroker().getBeanByOid(ReferenceTable.class, referenceTableOid);
                Collection<ReferenceCode> codes = referenceTable.getReferenceCodes();
                for (ReferenceCode code : codes) {
                    if ("P".equals(code.getStateCode())) {
                        m_cteResultCodes.add(code.getCode());
                    }
                }
            }
        }
    }
    /**
     * The Class ProgramDetail.
     */
    protected class ProgramDetail {
        String m_schCode;
        SisSchool m_school;
        String m_programCode;
        PlainDate m_programStartDate;
        PlainDate m_programEndDate;
        String m_programExitCode;

        /**
         * Gets the sch code.
         *
         * @return the schCode
         */
        public String getSchCode() {
            return m_schCode;
        }

        /**
         * Gets the school.
         *
         * @return the schCode
         */
        public SisSchool getSchool() {
            return m_school;
        }

        /**
         * Sets the sch code.
         *
         * @param schCode the m_schCode to set
         */
        public void setSchCode(String schCode) {
            m_schCode = schCode;
        }

        /**
         * Sets the school.
         *
         * @param school void
         */
        public void setSchool(SisSchool school) {
            m_school = school;
        }

        /**
         * Gets the program code.
         *
         * @return the programCode
         */
        public String getProgramCode() {
            return m_programCode;
        }

        /**
         * Sets the program code.
         *
         * @param programCode the m_programCode to set
         */
        public void setProgramCode(String programCode) {
            m_programCode = programCode;
        }

        /**
         * Gets the program start date.
         *
         * @return the programStartDate
         */
        public PlainDate getProgramStartDate() {
            return m_programStartDate;
        }

        /**
         * Sets the program start date.
         *
         * @param programStartDate the programStartDate to set
         */
        public void setProgramStartDate(PlainDate programStartDate) {
            m_programStartDate = programStartDate;
        }

        /**
         * Gets the program end date.
         *
         * @return the programEndDate
         */
        public PlainDate getProgramEndDate() {
            return m_programEndDate;
        }

        /**
         * Sets the program end date.
         *
         * @param programEndDate the programEndDate to set
         */
        public void setProgramEndDate(PlainDate programEndDate) {
            m_programEndDate = programEndDate;
        }

        /**
         * Gets the program exit code.
         *
         * @return the programExitCode
         */
        public String getProgramExitCode() {
            return m_programExitCode;
        }

        /**
         * Sets the program exit code.
         *
         * @param programExitCode the programExitCode to set
         */
        public void setProgramExitCode(String programExitCode) {
            m_programExitCode = programExitCode;
        }
    }
    /**
     * Implementation of StateReportEntity to be used by the Class Roster export. This
     * must be a public static inner class with a public no argument constructor
     * so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class ProgramStatusEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";

        ArrayList<ProgramDetail> m_studentPgmDetail = new ArrayList();

        /**
         * Empty no argument constructor for dynamic instantiation.
         */
        public ProgramStatusEntity() {
            // Empty no argument constructor for dynamic instantiation.
        }

        /**
         * initialize the entity.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean)
                throws X2BaseException {
            super.intitialize(data, bean);
            SisStudent student = (SisStudent) bean;
            ProgramStatus pgmData = (ProgramStatus) data;
            m_studentPgmDetail.addAll(pgmData.getFilteredRows(student));
            setRowCount(m_studentPgmDetail.size());
        }

        /**
         * Returns the current program participation record.
         *
         * @return StudentProgramParticipation
         */
        public ProgramDetail getStudentPgmDetail() {
            return m_studentPgmDetail.get(getCurrentRow());
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
            ProgramDetail program = getStudentPgmDetail();
            SisStudent student = (SisStudent) getBean();
            buffer.append(student.getNameView());
            buffer.append(" [LASID=");
            buffer.append(student.getLocalId());
            buffer.append(", SASID=");
            buffer.append(student.getStateId());
            buffer.append("] ");
            buffer.append(program.getProgramCode());
            return buffer.toString();
        }
    }

    /**
     * Retriever used to get program data.
     */
    protected class RetrievePgmData implements FieldRetriever {
        public static final String PARAM_PROGRAM_CODE = "PGM_CODE";
        public static final String PARAM_PROGRAM_START_DATE = "PGM_START";
        public static final String PARAM_PROGRAM_END_DATE = "PGM_END";
        public static final String PARAM_PROGRAM_EXIT_CODE = "PGM_EXIT_CODE";
        public static final String PARAM_PROGRAM_SCHCODE = "SCHCODE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            ProgramStatusEntity pgmsEntity = (ProgramStatusEntity) entity;
            String param = (String) field.getParameter();
            if (PARAM_PROGRAM_CODE.equals(param)) {
                value = pgmsEntity.getStudentPgmDetail().getProgramCode();
            } else if (PARAM_PROGRAM_START_DATE.equals(param)) {
                value = pgmsEntity.getStudentPgmDetail().getProgramStartDate();
            } else if (PARAM_PROGRAM_END_DATE.equals(param)) {
                value = pgmsEntity.getStudentPgmDetail().getProgramEndDate();
            } else if (PARAM_PROGRAM_EXIT_CODE.equals(param)) {
                value = pgmsEntity.getStudentPgmDetail().getProgramExitCode();
            } else if (PARAM_PROGRAM_SCHCODE.equals(param)) {
                value = pgmsEntity.getStudentPgmDetail().getSchCode();
            }
            return value;
        }
    }

    /**
     * The Class UnpackedEnrollmentSpan.
     */
    protected class UnpackedEnrollmentSpan {
        private PlainDate m_firstActiveEnrollmentDate;
        private PlainDate m_lastActiveDate;
        private PlainDate m_withdrawalDate;
        private SisSchool m_school;

        /**
         * Instantiates a new unpacked enrollment span.
         *
         * @param school SisSchool
         * @param lastActiveDate PlainDate
         * @param firstActiveEnrollmentDate PlainDate
         * @param withdrawalDate PlainDate
         */
        public UnpackedEnrollmentSpan(SisSchool school, PlainDate lastActiveDate, PlainDate firstActiveEnrollmentDate,
                PlainDate withdrawalDate) {
            super();
            m_school = school;
            m_lastActiveDate = lastActiveDate;
            m_firstActiveEnrollmentDate = firstActiveEnrollmentDate;
            m_withdrawalDate = withdrawalDate;
        }

        /**
         * Gets the first active enrollment date.
         *
         * @return Plain date
         */
        public PlainDate getFirstActiveEnrollmentDate() {
            return m_firstActiveEnrollmentDate;
        }

        /**
         * Gets the withdrawal date.
         *
         * @return Plain date
         */
        public PlainDate getWithdrawalDate() {
            return m_withdrawalDate;
        }

        /**
         * Gets the last active date.
         *
         * @return Plain date
         */
        public PlainDate getLastActiveDate() {
            return m_lastActiveDate;
        }

        /**
         * Gets the school.
         *
         * @return Sis school
         */
        public SisSchool getSchool() {
            return m_school;
        }
    }

    /**
     * Validate end date.
     */
    protected class ValidateEndDate implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            ProgramDetail programDetail = ((ProgramStatusEntity) entity).getStudentPgmDetail();
            ProgramStatus psData = (ProgramStatus) data;
            SisStudent student = (SisStudent) entity.getBean();
            SisSchool school = programDetail.getSchool();
            String programCode = programDetail.getProgramCode();
            SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");
            // Validate PROGRAM_EDATE. It is required for these types of program data.
            // When required, this field must be a valid date between the first and last day of
            // school for the SCHCODE.
            if (PROGRAM_CODES_EIS.contains(programCode)
                    || PROGRAM_CODES_HOMELESS.contains(programCode)
                    || PROGRAM_CODES_DUAL_LANGUAGE.contains(programCode)
                    || PROGRAM_CODES_21ST_CENTURY.contains(programCode)
                    || PROGRAM_CODES_ACADEMIC_ENRICHMENT.contains(programCode)
                    || PROGRAM_CODES_HEAD_START.contains(programCode)
                    || PROGRAM_CODES_4001_4012.contains(programCode)) {
                if (StringUtils.isEmpty(value)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "End date is required for program with code " + STYLE_BOLD + programCode + STYLE_END,
                            "PROGRAM_CODE=" + STYLE_BOLD + programCode + STYLE_END));
                } else {
                    PlainDate schoolFirstDate = psData.getFirstDateForStudent(student, school);
                    PlainDate schoolLastDate = psData.getLastDateForStudent(student, school);
                    PlainDate endDate = null;
                    try {
                        endDate = new PlainDate(dateFormat.parse(value));
                    } catch (ParseException e) {
                        e.printStackTrace();
                    }

                    if (endDate != null &&
                            (endDate.before(schoolFirstDate) || endDate.after(schoolLastDate))) {
                        errors.add(new StateReportValidationError(entity, field,
                                "End date for program with code " + STYLE_BOLD + programCode + STYLE_END +
                                        " must be between the first (" + schoolFirstDate + ") and last ("
                                        + schoolLastDate + ") day of school with SCHCODE " +
                                        programDetail.getSchCode() + " and calendar ID " + student.getCalendarCode(),
                                "PROGRAM_CODE=" + STYLE_BOLD + programCode + STYLE_END));
                    }
                }
            }
            return errors;
        }
    }
    /**
     * Validate program data.
     */
    protected class ValidateProgramCode implements FieldValidator {

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

            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            ProgramDetail programDetail = ((ProgramStatusEntity) entity).getStudentPgmDetail();
            String programCode = value;
            // Validate acceptable values for PROGRAM_CODE
            if (!PROGRAM_CODES_21ST_CENTURY.contains(programCode)
                    && !PROGRAM_CODES_ACADEMIC_ENRICHMENT.contains(programCode)
                    && !PROGRAM_CODES_DUAL_LANGUAGE.contains(programCode)
                    && !PROGRAM_CODES_EIS.contains(programCode)
                    && !PROGRAM_CODES_HEAD_START.contains(programCode)
                    && !PROGRAM_CODES_HOMELESS.contains(programCode)
                    && !PROGRAM_CODES_STUDENT_EARN_CRED.contains(programCode)
                    && !PROGRAM_CODES_TITLE_I.contains(programCode)
                    && !PROGRAM_CODES_4001_4012.contains(programCode)) {
                errors.add(new StateReportValidationError(entity, field,
                        STYLE_BOLD + programCode + STYLE_END +
                                " is not acceptable value for PROGRAM_CODE.",
                        "PROGRAM_CODE=" + STYLE_BOLD + programCode + STYLE_END));
            }
            // When student is reported with program_code of 1035, then there must also be a second
            // record
            // with 1031, 1032, 1033, or 1034 program codes for the same time period
            if (programCode.equals(UNACCOMPANIED_HOMELESS_YOUTH_PARAM)) {
                PlainDate startDate = programDetail.getProgramStartDate();
                PlainDate endDate = programDetail.getProgramEndDate();
                SisStudent student = (SisStudent) entity.getBean();
                Collection<ProgramDetail> studentPgmDetails =
                        ((ProgramStatus) data).m_studentPgmDetailMap.get(student.getOid());
                boolean hasSecondHomelessRecord = false;
                for (ProgramDetail program : studentPgmDetails) {
                    if (program.getProgramCode().equals(HOMELESS_SHELTERED_PARAM) ||
                            program.getProgramCode().equals(HOMELESS_DOUBLED_UP_PARAM) ||
                            program.getProgramCode().equals(HOMELESS_UNSHELTERED_PARAM) ||
                            program.getProgramCode().equals(HOMELESS_HOTELS_MOTELS_PARAM)) {
                        PlainDate secondStartDate = program.getProgramStartDate();
                        PlainDate secondEndDate = program.getProgramEndDate();
                        // If first start (end) date and second start (end) date are the same object
                        // or are both nulls or are equals
                        // it's ok, otherwise create error.
                        if ((startDate == secondStartDate || (startDate != null && startDate.equals(secondStartDate)))
                                &&
                                (endDate == secondEndDate || (endDate != null && endDate.equals(secondEndDate)))) {
                            hasSecondHomelessRecord = true;
                            break;
                        }
                    }
                }

                if (!hasSecondHomelessRecord) {
                    errors.add(new StateReportValidationError(entity, field,
                            "If PROGRAM_CODE is " + STYLE_BOLD + programCode + STYLE_END +
                                    " then there must also be a second record with " + HOMELESS_SHELTERED_PARAM + ", " +
                                    HOMELESS_DOUBLED_UP_PARAM + ", " + HOMELESS_UNSHELTERED_PARAM + ", or " +
                                    HOMELESS_HOTELS_MOTELS_PARAM + " program codes for the same time period.",
                            "PROGRAM_CODE=" + STYLE_BOLD + programCode + STYLE_END));
                }
            }

            return errors;
        }
    }

    /**
     * Validate SASID against the enrollment data submission to ensure that the student is enrolled
     * in the school.
     */
    protected class ValidateSASID implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SisStudent student = (SisStudent) entity.getBean();
            ProgramDetail programDetail = ((ProgramStatusEntity) entity).getStudentPgmDetail();
            String schcode = programDetail.getSchCode();
            String sasid = student.getStateId();
            // validate if result is selected
            if (!StringUtils.isEmpty((String) getParameter(PARAM_RESULT))) {
                Collection<String> schcodes = m_sasidSchsMap.get(sasid);
                if (schcodes == null || !schcodes.contains(schcode)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Student with SASID " + STYLE_BOLD + sasid + STYLE_END +
                                    " is not enrolled in the school with SCHCODE " + STYLE_BOLD + schcode + STYLE_END +
                                    " according to the selected enrollment data submission.",
                            "SASID=" + STYLE_BOLD + sasid + STYLE_END + ", SCHCODE=" + STYLE_BOLD + schcode
                                    + STYLE_END));
                }
            }
            return errors;
        }
    }
    /**
     * Validate start date.
     */
    protected class ValidateStartDate implements FieldValidator {

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

            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            ProgramDetail programDetail = ((ProgramStatusEntity) entity).getStudentPgmDetail();
            ProgramStatus psData = (ProgramStatus) data;
            SisStudent student = (SisStudent) entity.getBean();
            SisSchool school = programDetail.getSchool();
            String programCode = programDetail.getProgramCode();
            SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");
            // Validate PROGRAM_SDATE. It is required for these types of program data.
            // When required, this field must be a valid date between the first and last day of
            // school for the SCHCODE.
            if (PROGRAM_CODES_EIS.contains(programCode)
                    || PROGRAM_CODES_HOMELESS.contains(programCode)
                    || PROGRAM_CODES_DUAL_LANGUAGE.contains(programCode)
                    || PROGRAM_CODES_21ST_CENTURY.contains(programCode)
                    || PROGRAM_CODES_ACADEMIC_ENRICHMENT.contains(programCode)
                    || PROGRAM_CODES_HEAD_START.contains(programCode)
                    || PROGRAM_CODES_4001_4012.contains(programCode)) {
                if (StringUtils.isEmpty(value)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Start date is required for program with code " + STYLE_BOLD + programCode + STYLE_END,
                            "PROGRAM_CODE=" + STYLE_BOLD + programCode + STYLE_END));
                } else {
                    PlainDate schoolFirstDate = psData.getFirstDateForStudent(student, school);
                    PlainDate schoolLastDate = psData.getLastDateForStudent(student, school);
                    PlainDate startDate = null;
                    try {
                        startDate = new PlainDate(dateFormat.parse(value));
                    } catch (ParseException e) {
                        e.printStackTrace();
                    }
                    if (startDate != null &&
                            (startDate.before(schoolFirstDate) || startDate.after(schoolLastDate))) {
                        errors.add(new StateReportValidationError(entity, field,
                                "Start date for program with code " + STYLE_BOLD + programCode + STYLE_END +
                                        " must be between the first (" + schoolFirstDate + ") and last ("
                                        + schoolLastDate + ") day of school with SCHCODE " +
                                        programDetail.getSchCode() + " and calendar ID " + student.getCalendarCode(),
                                "PROGRAM_CODE=" + STYLE_BOLD + programCode + STYLE_END));
                    }
                }
            }
            return errors;
        }
    }

    /**
     * Aliases.
     */
    private static final String ALIAS_PROGRAM_EXIT_CODE = "Program Exit Code";
    private static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";

    /**
     * Program Exit Codes.
     */
    private static final String PGM_EXIT_CODE_WITHDRAWN_NOT_SET = "2";

    /**
     * Name for the active students only parameter. The value is a boolean.
     */
    public static final String ASSMT_DEF_ID_CTE = "CTE";
    public static final String SASID_STUDENTS_ONLY_PARAM = "sasidStudentsOnly";
    /**
     * Input parameter: Select organization level.
     */
    public static final String PARAM_ORGANIZATION = "orgOid";
    public static final String PARAM_RESULT = "resultOid";

    /**
     * Codes
     */
    public static final List<String> PROGRAM_CODES_21ST_CENTURY = Arrays.asList("1801");
    public static final List<String> PROGRAM_CODES_4001_4012 = Arrays.asList("4001", "4002", "4003", "4004", "4005",
            "4006", "4007", "4008", "4009", "4010", "4011", "4012");
    public static final List<String> PROGRAM_CODES_ACADEMIC_ENRICHMENT = Arrays.asList("3110", "3111");
    public static final List<String> PROGRAM_CODES_DUAL_LANGUAGE = Arrays.asList("1901", "1902", "1909");
    public static final List<String> PROGRAM_CODES_EIS = Arrays.asList("1410", "1411", "1412", "1413", "1414", "1415");
    public static final List<String> PROGRAM_CODES_HEAD_START = Arrays.asList("3112");
    public static final List<String> PROGRAM_CODES_HOMELESS = Arrays.asList("1031", "1032", "1033", "1034", "1035");
    public static final List<String> PROGRAM_CODES_STUDENT_EARN_CRED = new ArrayList<String>() {
        {
            for (int i = 101; i < 163; i++) {
                add(String.valueOf(i));
            }
        }
    };
    public static final List<String> PROGRAM_CODES_TITLE_I = Arrays.asList("1051", "1052", "1053", "1054", "1055",
            "1056", "1057", "1058", "1059");

    /**
     * Program code parameters
     */
    protected String HOMELESS_DOUBLED_UP_PARAM = "1032";
    protected String HOMELESS_HOTELS_MOTELS_PARAM = "1034";
    protected String HOMELESS_SHELTERED_PARAM = "1031";
    protected String HOMELESS_UNSHELTERED_PARAM = "1033";
    protected String UNACCOMPANIED_HOMELESS_YOUTH_PARAM = "1035";

    /**
     * Fields
     */
    protected CTEApprenticeship m_apprenticeshipRecords = null;
    protected Collection<String> m_cteResultCodes;
    protected String m_fieldPgmExitCode;
    protected Map<String, Map<String, PlainDate>> m_firstDaysOfSchools;
    protected Map<String, Map<String, PlainDate>> m_lastDaysOfSchools;
    protected String m_orgFieldStr = null;
    protected String m_orgOid = null;
    protected Collection<String> m_programStateCodes;
    protected PlainDate m_reportDate;
    protected Map<String, Collection<String>> m_sasidSchsMap;
    protected boolean m_sasidStudentsOnly;
    protected StudentHistoryHelper m_studentHistoryHelper;
    protected Map<String, Collection<ProgramDetail>> m_studentPgmDetailMap;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.x2dev.sis.model.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();
        initializeFields();
        m_studentHistoryHelper = new StudentHistoryHelper(this);
        m_studentHistoryHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        // If "Student must have SASID" is checked, get only students with non-empty state ids
        Boolean sasidStudentsOnly = (Boolean) getParameter(SASID_STUDENTS_ONLY_PARAM);
        if (sasidStudentsOnly.booleanValue()) {
            m_studentHistoryHelper.getStudentCriteria().addNotEmpty(SisStudent.COL_STATE_ID,
                    getBroker().getPersistenceKey());
        }
        // Limit the student criteria to students who have programs that are match the criteria in
        // getReportingCriteria().
        SubQuery subQuery = new SubQuery(StudentProgramParticipation.class, StudentProgramParticipation.COL_STUDENT_OID,
                getReportingCriteria());
        X2Criteria stdCriteria = new X2Criteria();
        m_apprenticeshipRecords = new CTEApprenticeship(m_studentHistoryHelper.getStudentCriteria());
        m_apprenticeshipRecords.initialize();
        X2Criteria orCriteria1 = m_apprenticeshipRecords.getNewStudentCriteria();
        X2Criteria orCriteria2 = new X2Criteria();
        orCriteria2.addIn(X2BaseBean.COL_OID, subQuery);
        stdCriteria.addOrCriteria(orCriteria1);
        stdCriteria.addOrCriteria(orCriteria2);
        m_studentHistoryHelper.getStudentCriteria().addIn(X2BaseBean.COL_OID,
                new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdCriteria));
        if (getSetupErrors().size() == 0) {
            loadProgramStateCodes();
            initializeStudentProgramDetailMap();
            initValidSasidSchMap();

            // Set the query to be used for student selection.
            setQuery(m_studentHistoryHelper.getStudentQuery(true));
            setEntityClass(ProgramStatusEntity.class);

            // Build maps of retrievers
            Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("PS_CALC", new RetrievePgmData());
            super.addCalcs(calcs);

            // Build maps of validator functions
            Map<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put("ValidateProgramCode", new ValidateProgramCode());
            validators.put("ValidateStartDate", new ValidateStartDate());
            validators.put("ValidateEndDate", new ValidateEndDate());
            validators.put("ValidateSASID", new ValidateSASID());
            super.addValidators(validators);
        }
    }

    /**
     * Determine first date for this student from school calendar with defaults if calendar is not
     * found.
     *
     * @param student SisStudent
     * @param school SisSchool
     * @return Plain date
     */
    protected PlainDate getFirstDateForStudent(SisStudent student, SisSchool school) {
        Map<String, PlainDate> codeFirstDay = m_firstDaysOfSchools.get(school.getOid());
        if (codeFirstDay == null) {
            codeFirstDay = new HashMap<String, PlainDate>();
            Criteria criteria = new Criteria();
            criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_SCHOOL_OID,
                    school.getOid());
            criteria.addEqualTo(
                    SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                    getCurrentContext().getOid());
            criteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
            QueryByCriteria calendarQuery = new QueryByCriteria(SisSchoolCalendarDate.class, criteria);
            calendarQuery.addOrderByAscending(SisSchoolCalendarDate.COL_DATE);
            QueryIterator calendars = null;
            try {
                calendars = getBroker().getIteratorByQuery(calendarQuery);
                while (calendars.hasNext()) {
                    SisSchoolCalendarDate calendarDate = (SisSchoolCalendarDate) calendars.next();
                    String calendarId = calendarDate.getSchoolCalendar().getCalendarId();
                    if (!codeFirstDay.containsKey(calendarId)) {
                        codeFirstDay.put(calendarId, calendarDate.getDate());
                    }
                }
            } finally {
                if (calendars != null) {
                    calendars.close();
                }
            }
            m_firstDaysOfSchools.put(school.getOid(), codeFirstDay);
        }
        PlainDate firstDate = null;
        if (student != null) {
            firstDate = codeFirstDay.get(student.getCalendarCode());
        }
        if (firstDate == null && !codeFirstDay.isEmpty()) {
            for (PlainDate date : codeFirstDay.values()) {
                if (firstDate == null) {
                    firstDate = date;
                } else if (firstDate.after(date)) {
                    firstDate = date;
                }
            }
        } else if (firstDate == null) {
            firstDate = getCurrentContext().getStartDate();
        }
        return firstDate;
    }

    /**
     * Determine last date for this student from school calendar with defaults if calendar is not
     * found.
     *
     * @param student SisStudent
     * @param school SisSchool
     * @return Plain date
     */
    protected PlainDate getLastDateForStudent(SisStudent student, SisSchool school) {
        Map<String, PlainDate> codeLastDay = m_lastDaysOfSchools.get(school.getOid());
        if (codeLastDay == null) {
            codeLastDay = new HashMap<String, PlainDate>();
            Criteria criteria = new Criteria();
            criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_SCHOOL_OID,
                    school.getOid());
            criteria.addEqualTo(
                    SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                    getCurrentContext().getOid());
            criteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
            QueryByCriteria calendarQuery = new QueryByCriteria(SisSchoolCalendarDate.class, criteria);
            calendarQuery.addOrderByDescending(SisSchoolCalendarDate.COL_DATE);
            QueryIterator calendars = null;
            try {
                calendars = getBroker().getIteratorByQuery(calendarQuery);
                while (calendars.hasNext()) {
                    SisSchoolCalendarDate calendarDate = (SisSchoolCalendarDate) calendars.next();
                    String calendarId = calendarDate.getSchoolCalendar().getCalendarId();
                    if (!codeLastDay.containsKey(calendarId)) {
                        codeLastDay.put(calendarId, calendarDate.getDate());
                    }
                }
            } finally {
                if (calendars != null) {
                    calendars.close();
                }
            }
            m_lastDaysOfSchools.put(school.getOid(), codeLastDay);
        }
        PlainDate lastDate = null;
        if (student != null) {
            lastDate = codeLastDay.get(student.getCalendarCode());
        }
        if (lastDate == null && !codeLastDay.isEmpty()) {
            for (PlainDate date : codeLastDay.values()) {
                if (lastDate == null) {
                    lastDate = date;
                } else if (lastDate.after(date)) {
                    lastDate = date;
                }
            }
        } else if (lastDate == null) {
            lastDate = getCurrentContext().getEndDate();
        }
        return lastDate;
    }

    /**
     * find student assessment column definition from alias.
     *
     * @param alias String
     * @return Assessment column definition
     */
    protected AssessmentColumnDefinition getStdAssesmentColumnByAlias(String alias) {
        AssessmentColumnDefinition assessmentColumnDefinition = null;
        if (alias != null) {
            Criteria assesmColumnCriteria = new X2Criteria();
            assesmColumnCriteria.addEqualTo(AssessmentColumnDefinition.COL_ALIAS, alias);
            assesmColumnCriteria.addEqualTo(
                    AssessmentColumnDefinition.REL_ASSESSMENT_DEFINITION + PATH_DELIMITER + AssessmentDefinition.COL_ID,
                    ASSMT_DEF_ID_CTE);
            QueryIterator queryIterator = getBroker()
                    .getIteratorByQuery(new QueryByCriteria(AssessmentColumnDefinition.class, assesmColumnCriteria));
            if (queryIterator.hasNext()) {
                assessmentColumnDefinition = (AssessmentColumnDefinition) queryIterator.next();
            }
        }
        return assessmentColumnDefinition;
    }

    /**
     * find stdAssesmentField in data dictionary by assessmentColumnDefinition alias.
     *
     * @param alias String
     * @param isRequired boolean
     * @return String
     */
    protected String getStdAssesmentFieldByAlias(String alias, boolean isRequired) {
        String returnValue = null;
        AssessmentColumnDefinition assessmentColumnDefinition = getStdAssesmentColumnByAlias(alias);
        DataFieldConfig dataFieldConfig = null;
        if (assessmentColumnDefinition != null) {
            dataFieldConfig = assessmentColumnDefinition.getDataFieldConfig();

            if (dataFieldConfig != null) {
                DataDictionaryField dataDictionaryField =
                        getDataDictionary().findDataDictionaryField(dataFieldConfig.getDataFieldOid());
                returnValue = dataDictionaryField.getSystemDataField().getJavaName();
            }
        }
        if (isRequired && returnValue == null) {
            String aliasMsg =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(ERROR_ALIAS_LOOKUP);
            addSetupError(aliasMsg, alias);
        }
        return returnValue;
    }

    /**
     * Find reference table in st assessment field by alias.
     *
     * @param alias String
     * @return String
     */
    private String findReferenceTableInStAssessmentFieldByAlias(String alias) {
        AssessmentColumnDefinition column = getStdAssesmentColumnByAlias(alias);
        String referenceTableOid = column == null ? null : column.getReferenceTableOid();
        return referenceTableOid;
    }

    /**
     * Filter records with codes 4001-4012 and add only one record for the same codes.
     *
     * @param std
     * @return
     */
    private Collection<ProgramDetail> getFilteredRows(SisStudent std) {
        Collection<ProgramDetail> pgmDetailsToReturn = new ArrayList<>();
        Map<String, Collection<ProgramDetail>> otherCodesMap = new HashMap<String, Collection<ProgramDetail>>();
        Collection<ProgramDetail> details = m_studentPgmDetailMap.get(std.getOid());
        if (details != null && details.size() > 0) {
            for (ProgramDetail pgmDetail : details) {
                if (PROGRAM_CODES_4001_4012.contains(pgmDetail.getProgramCode())) {
                    Collection<ProgramDetail> otherCodePgmDetailsList = otherCodesMap.get(pgmDetail.getProgramCode());
                    if (otherCodePgmDetailsList == null) {
                        otherCodePgmDetailsList = new ArrayList<>();
                        otherCodePgmDetailsList.add(pgmDetail);
                        otherCodesMap.put(pgmDetail.getProgramCode(), otherCodePgmDetailsList);
                    } else {
                        otherCodePgmDetailsList.add(pgmDetail);
                    }
                } else {
                    pgmDetailsToReturn.add(pgmDetail);
                }
            }
            for (String pgmCode : otherCodesMap.keySet()) {
                Collection<ProgramDetail> otherCodePgmDetailsList = otherCodesMap.get(pgmCode);
                if (otherCodePgmDetailsList.size() > 1) {
                    PlainDate startDateToCompare = null;
                    PlainDate endDateToCompare = null;
                    ProgramDetail pgmDetailToAdd = null;
                    for (ProgramDetail pgmDetail : otherCodePgmDetailsList) {
                        if (startDateToCompare == null) {
                            startDateToCompare = pgmDetail.getProgramStartDate();
                        }
                        if (endDateToCompare == null && pgmDetail.getProgramEndDate() != null) {
                            endDateToCompare = pgmDetail.getProgramEndDate();
                        }
                        if (pgmDetailToAdd == null) {
                            pgmDetailToAdd = pgmDetail;
                        }

                        if (pgmDetailToAdd.getProgramStartDate().after(pgmDetail.getProgramStartDate())) {
                            pgmDetailToAdd.setProgramStartDate(pgmDetail.getProgramStartDate());
                        }
                        if ((pgmDetailToAdd.getProgramEndDate() == null && pgmDetail.getProgramEndDate() != null)
                                || (pgmDetailToAdd.getProgramEndDate() != null
                                        && pgmDetailToAdd.getProgramEndDate().before(pgmDetail.getProgramEndDate()))) {
                            pgmDetailToAdd.setProgramEndDate(pgmDetail.getProgramEndDate());
                        }
                    }
                    if (pgmDetailToAdd != null
                            && StudentManager.isActiveStudent(getOrganization(), std.getEnrollmentStatus())) {
                        pgmDetailToAdd.setProgramEndDate(null);
                    }
                    pgmDetailsToReturn.add(pgmDetailToAdd);
                } else {
                    pgmDetailsToReturn.addAll(otherCodePgmDetailsList);
                }
            }
        }
        return pgmDetailsToReturn;
    }

    /**
     * Returns the criteria that retrieves current year transcript records that should be included
     * in the export.
     *
     * @return Criteria
     */
    private Criteria getReportingCriteria() {
        X2Criteria programStatusCriteria = new X2Criteria();

        X2Criteria programStatusStandardCriteria = new X2Criteria();
        programStatusStandardCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                getCurrentContext().getStartDate());

        X2Criteria programStatusLegacyCriteria = new X2Criteria();
        programStatusLegacyCriteria.addLessThan(StudentProgramParticipation.COL_START_DATE,
                getCurrentContext().getStartDate());
        programStatusLegacyCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, PROGRAM_CODES_4001_4012);

        X2Criteria programStatusOrCriteria = new X2Criteria();
        programStatusOrCriteria.addOrCriteria(programStatusStandardCriteria);
        programStatusOrCriteria.addOrCriteria(programStatusLegacyCriteria);

        programStatusCriteria.addAndCriteria(programStatusOrCriteria);
        programStatusCriteria.addNotNull(StudentProgramParticipation.COL_START_DATE);
        programStatusCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                getCurrentContext().getEndDate());
        if (!isSchoolContext()) {
            if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
                programStatusCriteria
                        .addEqualTo(StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                                SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                                m_orgFieldStr,
                                m_orgOid);
            }
        }

        return programStatusCriteria;
    }

    /**
     * Returns the criteria that retrieves all student programs that should be included in the
     * export.
     *
     * @return Criteria
     */
    private Criteria getStudentProgramParticipationCriteria() {
        X2Criteria studentProgramParticipationCriteria = new X2Criteria();

        Criteria reportingCriteria = getReportingCriteria();
        SubQuery studentSubQuery =
                new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentHistoryHelper.getStudentCriteria());
        reportingCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);

        studentProgramParticipationCriteria.addAndCriteria(reportingCriteria);

        studentProgramParticipationCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, m_programStateCodes);

        applyInputCriteria(studentProgramParticipationCriteria, false, StudentProgramParticipation.REL_STUDENT);

        return studentProgramParticipationCriteria;
    }

    /**
     * Generate a span starting at the first enrollment for the school overlapping with this year
     * ending at the last withdrawal date using the last active school.
     *
     * @param enrollmentSpans Collection<StudentEnrollmentSpan>
     * @param districtStartDate PlainDate
     * @return Unpacked enrollment span
     */
    private UnpackedEnrollmentSpan getUnpackedEnrollmentSpan(Collection<StudentEnrollmentSpan> enrollmentSpans,
                                                             PlainDate districtStartDate) {
        UnpackedEnrollmentSpan span = null;
        PlainDate firstActiveEnrollmentDate = null;
        PlainDate lastActiveDate = null;
        PlainDate withdrawalDate = null;
        SisSchool school = null;

        for (StudentEnrollmentSpan enrollmentSpan : enrollmentSpans) {
            school = enrollmentSpan.getSchool();
            PlainDate firstDate = getFirstDateForStudent(null, school);
            if (firstActiveEnrollmentDate == null && firstDate != null &&
                    (enrollmentSpan.getLastActiveDate() == null
                            || !enrollmentSpan.getLastActiveDate().before(firstDate))) {
                firstActiveEnrollmentDate = enrollmentSpan.getFirstActiveEnrollment().getEnrollmentDate();
            }
            for (StudentEnrollment enrollment : enrollmentSpan.getEnrollments()) {
                if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
                    school = enrollment.getSchool();
                }
            }
            withdrawalDate = enrollmentSpan.getFirstInactiveEnrollment() == null ? null
                    : enrollmentSpan.getFirstInactiveEnrollment().getEnrollmentDate();
            lastActiveDate = enrollmentSpan.getLastActiveDate();
        }
        if (firstActiveEnrollmentDate != null && school != null) {
            span = new UnpackedEnrollmentSpan(school, lastActiveDate, firstActiveEnrollmentDate, withdrawalDate);
        }
        return span;
    }

    /**
     * Inits the valid sasid sch map.
     */
    private void initValidSasidSchMap() {
        m_sasidSchsMap = new HashMap<String, Collection<String>>();
        String resultOid = (String) getParameter(PARAM_RESULT);
        if (!StringUtils.isEmpty(resultOid)) {
            X2Criteria defCriteria = new X2Criteria();
            defCriteria.addEqualTo(ExportFormatDefinition.COL_PROCEDURE_ID, "EXPDATA-RI-ECC");
            BeanQuery beanQuery = new BeanQuery(ExportFormatDefinition.class, defCriteria);
            ExportFormatDefinition formatDefinition = (ExportFormatDefinition) getBroker().getBeanByQuery(beanQuery);
            Collection<ExportFormatField> fields = formatDefinition.getFields();

            String sasidPath = null;
            String schcodePath = null;
            for (ExportFormatField field : fields) {
                String name = field.getName();
                if (name != null) {
                    if (name.equals("SASID")) {
                        sasidPath = field.getDataFieldConfig().getDataField().getJavaName();
                    } else if (name.equals("SCHCODE")) {
                        schcodePath = field.getDataFieldConfig().getDataField().getJavaName();
                    }
                }
            }

            if (StringUtils.isEmpty(sasidPath) || StringUtils.isEmpty(schcodePath)) {
                AppGlobals.getLog().log(Level.SEVERE, "There is no fields with name SASID or SCHCODE in the export "
                        + "format definition EXPDATA-RI-ECC. Cannot be validated against the enrollment data submission "
                        + "to ensure that the student is enrolled in the school.");
            } else {
                X2Criteria rowsCriteria = new X2Criteria();
                rowsCriteria.addEqualTo(ExportFormatRow.COL_RESULT_OID, resultOid);
                rowsCriteria.addNotEmpty(sasidPath, getBroker().getPersistenceKey());
                rowsCriteria.addNotEmpty(schcodePath, getBroker().getPersistenceKey());

                QueryByCriteria rowsQuery = new QueryByCriteria(ExportFormatRow.class, rowsCriteria);

                Collection<ExportFormatRow> rows = getBroker().getCollectionByQuery(rowsQuery);

                for (ExportFormatRow row : rows) {
                    String sasid = (String) row.getFieldValueByBeanPath(sasidPath);
                    String schcode = (String) row.getFieldValueByBeanPath(schcodePath);
                    if (!m_sasidSchsMap.containsKey(sasid)) {
                        m_sasidSchsMap.put(sasid, new ArrayList(Arrays.asList(schcode)));
                    } else {
                        m_sasidSchsMap.get(sasid).add(schcode);
                    }
                }
            }
        }
    }

    /**
     * Initialize fields.
     */
    private void initializeFields() {
        m_fieldPgmExitCode = translateAliasToJavaName(ALIAS_PROGRAM_EXIT_CODE, true);
        m_firstDaysOfSchools = new HashMap<String, Map<String, PlainDate>>();
        m_lastDaysOfSchools = new HashMap<String, Map<String, PlainDate>>();

        // Get organization Oid and level and field.
        m_orgOid = (String) getParameter(PARAM_ORGANIZATION);
        SisOrganization organization = null;
        OrganizationDefinition def = null;
        if (!StringUtils.isEmpty(m_orgOid)) {
            organization = (SisOrganization) getBroker().getBeanByOid(SisOrganization.class, m_orgOid);
            if (organization != null) {
                def = organization.getOrganizationDefinition();
                m_orgFieldStr = "organization" + Integer.toString(def.getLevel() + 1) + "Oid";
            }
        }
    }

    /**
     * Initialize Program Details for each Student.
     */
    private void initializeStudentProgramDetailMap() {
        m_studentPgmDetailMap = new HashMap<String, Collection<ProgramDetail>>();

        Criteria studentScheduleCriteria = getStudentProgramParticipationCriteria();
        QueryByCriteria studentScheduleQuery = new QueryByCriteria(StudentProgramParticipation.class,
                studentScheduleCriteria);

        QueryIterator items = getBroker().getIteratorByQuery(studentScheduleQuery);

        try {
            while (items.hasNext()) {
                StudentProgramParticipation program = (StudentProgramParticipation) items.next();

                SisStudent student = program.getStudent();

                String programCode = null;
                String programExitCode = null;
                PlainDate programStartDate = null;
                PlainDate programEndDate = null;
                String schCode = null;

                Collection<StudentEnrollmentSpan> enrollmentSpans =
                        m_studentHistoryHelper.getStudentEnrollmentSpans(program.getStudent(), true);
                PlainDate districtStartDate = getCurrentContext().getStartDate();
                UnpackedEnrollmentSpan span = getUnpackedEnrollmentSpan(enrollmentSpans, districtStartDate);

                if (span != null) {
                    // Get first in-session date for the school.
                    SisSchool school = span.getSchool();

                    PlainDate firstDate = getFirstDateForStudent(student, school);

                    if (program.getEndDate() != null && program.getEndDate().before(firstDate)) {
                        continue;
                    }
                    if (span.getLastActiveDate() != null && span.getLastActiveDate().before(firstDate)) {
                        continue;
                    }

                    // Apply the m_orgFieldStr when processing StudentEnrollmentSpans to filter the
                    // school if required.
                    if (!isSchoolContext()) {
                        if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
                            if (!m_orgOid.equals(span.getSchool().getFieldValueByBeanPath(m_orgFieldStr))) {
                                continue;
                            }
                        }
                    }

                    PlainDate pgmStartDate = program.getStartDate();
                    PlainDate pgmEndDate = program.getEndDate();

                    // If current Enrollment Span doesn't overlap with Program date range, skip the
                    // Enrollment Span.
                    if (!((span.getWithdrawalDate() == null || !pgmStartDate.after(span.getWithdrawalDate())) &&
                            (pgmEndDate == null || !pgmEndDate.before(span.getFirstActiveEnrollmentDate())))) {
                        continue;
                    }

                    // Ok, we have filtered (if required) StudentEnrollmentSpan by m_orgFieldStr
                    // and checked if StudentEnrollmentSpan overlaps with Program date range

                    // Initializing of Program Exit Code. Let's check if a student is withdrawn from
                    // school
                    // and the PROGRAM_EXIT_CODE is not set.
                    if (span.getWithdrawalDate() != null
                            && StringUtils.isEmpty((String) program.getFieldValueByBeanPath(m_fieldPgmExitCode))) {
                        // If true, set Program Exit Code as '2'
                        programExitCode = PGM_EXIT_CODE_WITHDRAWN_NOT_SET;
                    } else {
                        // otherwise, if program has PROGRAM_EXIT_CODE (the program isn't active),
                        // use it
                        if (!StringUtils.isEmpty((String) program.getFieldValueByBeanPath(m_fieldPgmExitCode))) {
                            programExitCode = (String) program.getFieldValueByBeanPath(m_fieldPgmExitCode);
                        }
                    }
                    programExitCode = lookupReferenceCodeByBeanPath(StudentProgramParticipation.class,
                            m_fieldPgmExitCode,
                            programExitCode,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                    // initialize program code
                    programCode = program.getProgramCode();

                    programCode = lookupReferenceCodeByBeanPath(StudentProgramParticipation.class,
                            StudentProgramParticipation.COL_PROGRAM_CODE,
                            programCode,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                    // Initializing of Program Start Date. If program starts before the first
                    // in-session date for the
                    // school, use the first in-session date as the start date.
                    programStartDate = program.getStartDate();
                    if (programStartDate.before(firstDate)) {
                        programStartDate = firstDate;
                    }

                    // if start date before enrollment date, use enrollment date
                    if (programStartDate.before(span.getFirstActiveEnrollmentDate())) {
                        programStartDate = span.getFirstActiveEnrollmentDate();
                    }

                    programEndDate = program.getEndDate();
                    // if after withdrawal date, use withdrawal date
                    if (span.getWithdrawalDate() != null
                            && (programEndDate == null || programEndDate.after(span.getWithdrawalDate()))) {
                        programEndDate = span.getWithdrawalDate();
                    }
                    // if still null, use last day of school
                    if (programEndDate == null &&
                            (PROGRAM_CODES_EIS.contains(programCode)
                                    || PROGRAM_CODES_HOMELESS.contains(programCode)
                                    || PROGRAM_CODES_DUAL_LANGUAGE.contains(programCode)
                                    || PROGRAM_CODES_21ST_CENTURY.contains(programCode)
                                    || PROGRAM_CODES_ACADEMIC_ENRICHMENT.contains(programCode)
                                    || PROGRAM_CODES_HEAD_START.contains(programCode))) {
                        programEndDate = getLastDateForStudent(student, school);
                    }
                    // Initializing of other fields.
                    schCode = (String) span.getSchool().getFieldValueByBeanPath(m_sklIdField);

                    // Initialize program detail.
                    // S-30888 return only Programs where state code is not blank/null
                    if (!StringUtils.isEmpty(programCode)) {
                        ProgramDetail programDetail = new ProgramDetail();
                        programDetail.setSchool(school);
                        programDetail.setSchCode(schCode);
                        programDetail.setProgramCode(programCode);
                        if (!PROGRAM_CODES_4001_4012.contains(programCode)) {
                            programDetail.setProgramStartDate(programStartDate);
                            programDetail.setProgramEndDate(programEndDate);
                        } else {
                            PlainDate dateToSet = span.getFirstActiveEnrollmentDate();
                            if (dateToSet != null && dateToSet.before(getFirstDateForStudent(student, school))) {
                                dateToSet = getFirstDateForStudent(student, school);
                            }
                            programDetail.setProgramStartDate(dateToSet);
                            if (span.getWithdrawalDate() != null) {
                                programDetail.setProgramEndDate(span.getWithdrawalDate());
                            }
                            if (PGM_EXIT_CODE_WITHDRAWN_NOT_SET.equals(programExitCode)) {
                                programExitCode = null;
                            }
                        }
                        programDetail.setProgramExitCode(programExitCode);
                        // Add programDetails
                        Collection<ProgramDetail> studentPgmDetail = m_studentPgmDetailMap.get(student.getOid());
                        if (studentPgmDetail == null) {
                            studentPgmDetail = new ArrayList<ProgramDetail>();
                            m_studentPgmDetailMap.put(student.getOid(), studentPgmDetail);
                        }
                        studentPgmDetail.add(programDetail);
                    }
                }
            }
        } finally {
            items.close();
        }

        Map<String, Collection<StudentAssessment>> stdAssessmtnMap = m_apprenticeshipRecords.getStudentAssessmentMap();
        for (String stdOid : stdAssessmtnMap.keySet()) {
            if (!m_apprenticeshipRecords.getGraduationProgramm(stdOid).isEmpty()) {
                for (StudentAssessment assessment : stdAssessmtnMap.get(stdOid)) {
                    SisSchool school = assessment.getSchool();
                    String schCode = (String) school.getFieldValueByBeanPath(m_sklIdField);

                    ProgramDetail programDetail = new ProgramDetail();
                    programDetail.setSchool(school);
                    programDetail.setSchCode(schCode);

                    String referenceTableOid = findReferenceTableInStAssessmentFieldByAlias(
                            CTEApprenticeship.ALIAS_RI_CTE_CREDENTIAL_EARNED);
                    String cteRecCredential = (String) assessment
                            .getFieldValueByBeanPath(m_apprenticeshipRecords.m_assessmentFieldJavaName);

                    String programCode = lookupReferenceCodeByRefTbl(referenceTableOid, cteRecCredential,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                    programDetail.setProgramCode(programCode);

                    programDetail.setProgramStartDate(assessment.getDate());
                    programDetail.setProgramEndDate(null);
                    programDetail.setProgramExitCode(null);
                    Collection<ProgramDetail> studentPgmDetail = m_studentPgmDetailMap.get(stdOid);
                    if (studentPgmDetail == null) {
                        studentPgmDetail = new ArrayList<ProgramDetail>();
                        m_studentPgmDetailMap.put(stdOid, studentPgmDetail);
                    }
                    studentPgmDetail.add(programDetail);
                }


            }
        }
    }

    /**
     * Load program state codes to include only PGM records with a value in the state reference code
     * for the programCode.
     */
    private void loadProgramStateCodes() {
        m_programStateCodes = new ArrayList<String>();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(StudentProgramParticipation.class.getName(),
                StudentProgramParticipation.COL_PROGRAM_CODE);
        ReferenceTable referenceTable = field.getReferenceTable();

        if (referenceTable != null) {
            Collection<ReferenceCode> codes = referenceTable.getReferenceCodes();
            for (ReferenceCode code : codes) {
                String stateCode = code.getStateCode();
                if (!StringUtils.isEmpty(stateCode) && !code.getDisabledIndicator()) {
                    m_programStateCodes.add(code.getCode());
                }
            }
        }
    }
}
