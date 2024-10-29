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

package com.x2dev.procedures.statereporting.wa;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.AssessmentColumnDefinition;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export procedure for Washington State CEDARS
 * Student Programs file.
 *
 * @author X2 Development Corporation
 *
 */
public class WAStudentProgram extends StateReportData {

    /**
     * Helper class to include needed information.
     *
     * @author X2 Development Corporation
     */
    public class StudentInfo {
        private PlainDate m_exitDate;
        private String m_exitReason;
        private String m_qualCode;
        private Object m_program;
        private String m_asmCode;
        private SisSchool m_school;

        /**
         * Gets the qual code.
         *
         * @return the m_qualCode
         */
        public String getQualCode() {
            return m_qualCode;
        }

        /**
         * Sets the qual code.
         *
         * @param qualCode void
         */
        public void setQualCode(String qualCode) {
            this.m_qualCode = qualCode;
        }

        /**
         * Gets the school.
         *
         * @return the m_school
         */
        public SisSchool getSchool() {
            return m_school;
        }

        /**
         * Sets the school.
         *
         * @param school void
         */
        public void setSchool(SisSchool school) {
            this.m_school = school;
        }

        /**
         * Gets the exit date.
         *
         * @return the m_exitDate
         */
        public PlainDate getExitDate() {
            return m_exitDate;
        }

        /**
         * Sets the exit date.
         *
         * @param exitDate void
         */
        public void setExitDate(PlainDate exitDate) {
            this.m_exitDate = exitDate;
        }

        /**
         * Gets the exit reason.
         *
         * @return the m_exitReason
         */
        public String getExitReason() {
            return m_exitReason;
        }

        /**
         * Sets the exit reason.
         *
         * @param exitReason void
         */
        public void setExitReason(String exitReason) {
            this.m_exitReason = exitReason;
        }

        /**
         * Gets the program.
         *
         * @return the m_program
         */
        public Object getProgram() {
            return m_program;
        }

        /**
         * Sets the program.
         *
         * @param program void
         */
        public void setProgram(Object program) {
            this.m_program = program;
        }

        /**
         * Gets the asm code.
         *
         * @return the m_programCode
         */
        public String getAsmCode() {
            return m_asmCode;
        }

        /**
         * Sets the asm code.
         *
         * @param asmCode void
         */
        public void setAsmCode(String asmCode) {
            this.m_asmCode = asmCode;
        }
    }

    /**
     * Entity class for WAStudentProgram.
     * This wraps a program and provides initialization and calculation.
     */
    public static class WAStudentProgramEntity extends StateReportEntity {
        List<StudentInfo> m_statistics;

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();

            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId();

            return name;
        }

        /**
         * Initialize method.
         * Determine if the program overlaps an active span to get the appropriate school at the
         * time.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_statistics = new ArrayList<WAStudentProgram.StudentInfo>();

            WAStudentProgram spData = (WAStudentProgram) data;

            SisStudent student = (SisStudent) bean;

            // Skip the program if the student is not in the District Student file.
            List<DistrictEnrollmentSpan> districtSpans = getDistrictSpans(student, spData);
            if (districtSpans.size() == 0) {
                setRowCount(0);
                return;
            }

            Collection<StudentProgramParticipation> pgms = spData.m_pgmMap.get(student.getOid());
            List<StudentEnrollmentSpan> enrollmentSpans = spData.m_helper.getStudentEnrollmentSpans(student, true);

            if (pgms != null) {
                for (StudentProgramParticipation program : pgms) {
                    if (program != null) {
                        String pgmStateCode = data.lookupStateValue(StudentProgramParticipation.class,
                                StudentProgramParticipation.COL_PROGRAM_CODE,
                                program.getProgramCode());
                        if (PROGRAM_CODE_36.equals(pgmStateCode) &&
                                StringUtils.isEmpty((String) program.getFieldValueByAlias(ALIAS_PGM_QUALIFICATION))) {
                            continue;
                        }
                    }

                    // Get most recent span
                    DistrictEnrollmentSpan distrSpan = districtSpans.get(districtSpans.size() - 1);
                    PlainDate exitDate = null;
                    String exitReason = null;

                    if (distrSpan.m_exitEnrollment != null) {
                        // Get exit reason as a translation of the withdrawal code.
                        StudentEnrollment withdrawal = distrSpan.m_exitEnrollment;
                        if (withdrawal != null) {
                            String code = withdrawal.getEnrollmentCode();
                            code = data.lookupReferenceCodeByRefTbl(spData.m_refTableWithdrawalCode, code,
                                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                            if ("G0".equals(code) || "GA".equals(code)) {
                                exitReason = spData.m_exitReasonCodesMap.get("B");
                                exitDate = distrSpan.m_exitDate;
                            }
                        }
                    }

                    SisSchool school = null;

                    // Stop matching when any rule returns a result
                    // Rule 1 - find school based on enrolled school when program started.
                    if (program.getStartDate() != null) {
                        school = withinSpans(program.getStartDate(), enrollmentSpans);
                    }

                    // Rule 2 - find the school based on enrolled school when program ended.
                    if (school == null && program.getEndDate() != null) {
                        school = withinSpans(program.getStartDate(), enrollmentSpans);
                    }

                    // Rule 3 - find the school based on the most current enrollment
                    if (school == null) {
                        for (StudentEnrollmentSpan span : enrollmentSpans) {
                            if (span.getSchool() != null) {
                                StudentEnrollment enrollment = getLastActiveEnrollment(span);
                                school = (enrollment != null && enrollment.getSchool() != null) ? enrollment.getSchool()
                                        : span.getSchool();
                            }
                        }
                    }

                    if (school == null) {
                        school = student.getSchool();
                    }

                    if (!spData.includeSchool(school.getOid())) {
                        break;
                    }

                    StudentInfo stdInfo = spData.new StudentInfo();
                    stdInfo.setExitDate(exitDate);
                    stdInfo.setExitReason(exitReason);
                    stdInfo.setProgram(program);
                    stdInfo.setSchool(school);

                    m_statistics.add(stdInfo);

                }

            }

            Collection<StudentAssessment> asms = spData.m_asmMap.get(student.getOid());

            if (asms != null) {
                for (StudentAssessment asm : asms) {
                    StudentInfo stdInfo = spData.new StudentInfo();
                    StudentInfo stdInfoAdditional = null;
                    String asdOid = asm.getAssessmentDefinitionOid();
                    Collection<AssessmentColumnDefinition> acdCollection = spData.m_acdMap.get(asdOid);
                    SisSchool skl = null;
                    String reason = null;
                    String qualCode = null;

                    if (acdCollection != null) {
                        for (AssessmentColumnDefinition acd : acdCollection) {

                            if (ALIAS_ASD_METHOD.equals(acd.getAlias())) {

                                String code = (String) asm
                                        .getFieldValueByBeanPath(acd.getDataFieldConfig().getDataField().getJavaName());

                                reason = data.lookupReferenceCodeByRefTbl(acd.getReferenceTableOid(),
                                        code,
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                            } else if (ALIAS_ASD_LANG.equals(acd.getAlias())) {

                                String code = (String) asm
                                        .getFieldValueByBeanPath(acd.getDataFieldConfig().getDataField().getJavaName());

                                qualCode = data.lookupReferenceCodeByRefTbl(acd.getReferenceTableOid(),
                                        code,
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                            }
                        }
                    }

                    stdInfo.setExitReason(reason);
                    stdInfo.setQualCode(qualCode);
                    stdInfo.setExitDate(asm.getDate());
                    stdInfo.setProgram(asm);
                    stdInfo.setAsmCode(PROGRAM_CODE_WAST_42);

                    if (!StudentManager.getActiveStudentCodeList(data.getOrganization())
                            .contains(student.getEnrollmentStatus())) {
                        StudentEnrollment enrollment = null;
                        List<StudentEnrollmentSpan> spans = spData.m_helper.getStudentEnrollmentSpans(student, true);
                        for (StudentEnrollmentSpan span : spans) {
                            enrollment = span.getFirstInactiveEnrollment();
                        }

                        if (enrollment != null) {
                            String enrStateCode = data.lookupStateValue(StudentEnrollment.class,
                                    StudentEnrollment.COL_ENROLLMENT_CODE,
                                    enrollment.getEnrollmentCode());

                            if (CODES_ENR_WITHDRAWAL.contains(enrStateCode)
                                    && spData.includeSchool(enrollment.getSchoolOid())) {
                                stdInfoAdditional = spData.new StudentInfo();
                                stdInfoAdditional.setExitReason(reason);
                                stdInfoAdditional.setQualCode(qualCode);
                                stdInfoAdditional.setExitDate(enrollment.getEnrollmentDate());
                                stdInfoAdditional.setProgram(asm);
                                stdInfoAdditional.setAsmCode(PROGRAM_CODE_WAST);
                                stdInfoAdditional.setSchool(enrollment.getSchool());
                                m_statistics.add(stdInfoAdditional);
                            }
                        }
                    }
                    // Stop matching when any rule returns a result
                    // Rule 1 - find school based on enrolled school when program started.
                    if (asm.getDate() != null) {
                        skl = withinSpans(asm.getDate(), enrollmentSpans);
                    }

                    // Rule 2 - find the school based on enrolled school when program ended.
                    if (skl == null && asm.getDate() != null) {
                        skl = withinSpans(asm.getDate(), enrollmentSpans);
                    }

                    // Rule 3 - find the school based on the most current enrollment
                    if (skl == null) {
                        for (StudentEnrollmentSpan span : enrollmentSpans) {
                            if (span.getSchool() != null) {
                                StudentEnrollment enrollment = getLastActiveEnrollment(span);
                                skl = (enrollment != null && enrollment.getSchool() != null) ? enrollment.getSchool()
                                        : span.getSchool();
                            }
                        }
                    }

                    if (skl == null) {
                        skl = student.getSchool();
                    }

                    if (spData.includeSchool(skl.getOid())) {
                        stdInfo.setSchool(skl);
                        m_statistics.add(stdInfo);
                    }
                }
            }

            setRowCount(m_statistics.size());
        }

        /**
         * Returns the StudentEnrollmentSpan record for the current index.
         *
         * @return DistrictEnrollmentSpan
         */
        public StudentInfo getStdInfo() {
            return m_statistics.get(getCurrentRow());
        }


        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }

        /**
         * Calculate district enrollment spans from the student school enrollment spans.
         * Look for spans with withdrawal codes that represent district withdrawal vs. in district
         * transfer.
         *
         * @param student Student
         * @param sdData WAStudentProgram
         * @return List<DistrictEnrollmentSpan>
         */
        private List<DistrictEnrollmentSpan> getDistrictSpans(Student student, WAStudentProgram sdData) {
            List<StudentEnrollmentSpan> enrollmentSpans = sdData.m_helper.getStudentEnrollmentSpans(student, false);
            List<DistrictEnrollmentSpan> districtSpans = new ArrayList<DistrictEnrollmentSpan>();
            DistrictEnrollmentSpan currentSpan = null;
            for (StudentEnrollmentSpan span : enrollmentSpans) {
                /*
                 * Check if the span is a no-show span. Do not include no-show spans.
                 * A no-show span represents an enrollment where the student never showed up.
                 * It is identified by a withdrawal code that has NS in the local code of the
                 * reference table.
                 */
                boolean noShow = false;
                StudentEnrollment enrollment = span.getFirstInactiveEnrollment();
                if (enrollment != null) {
                    String withdrawalCode = enrollment.getEnrollmentCode();
                    withdrawalCode = sdData.lookupReferenceCodeByRefTbl(sdData.m_refTableWithdrawalCode, withdrawalCode,
                            ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                    if (CODE_NO_SHOW.equals(withdrawalCode)) {
                        noShow = true;
                    }
                }

                if (!noShow) {
                    // Check the span for entry type (internal or external)
                    enrollment = span.getFirstActiveEnrollment();
                    if (enrollment != null) {
                        String code = enrollment.getEnrollmentCode();
                        String stateCode = sdData.lookupReferenceCodeByRefTbl(sdData.m_refTableEnrollmentCode, code,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        if ("1".equals(stateCode) || "2".equals(stateCode)) {
                            if (currentSpan != null &&
                                    (currentSpan.m_exitDate == null ||
                                            !sdData.getOrganization().getCurrentContext().getStartDate()
                                                    .after(currentSpan.m_exitDate))) {
                                districtSpans.add(currentSpan);
                            }
                            currentSpan = null;

                            currentSpan = new DistrictEnrollmentSpan();
                            currentSpan.m_entryEnrollment = enrollment;
                            currentSpan.m_exitDate = span.getLastActiveDate();
                            currentSpan.m_exitEnrollment = span.getFirstInactiveEnrollment();
                        } else {
                            if (currentSpan == null) {
                                currentSpan = new DistrictEnrollmentSpan();
                                currentSpan.m_entryEnrollment = enrollment;
                            }
                            currentSpan.m_exitDate = span.getLastActiveDate();
                            currentSpan.m_exitEnrollment = span.getFirstInactiveEnrollment();
                        }
                    }
                }
            }
            if (currentSpan != null &&
                    (currentSpan.m_exitDate == null ||
                            !sdData.getOrganization().getCurrentContext().getStartDate()
                                    .after(currentSpan.m_exitDate))) {
                districtSpans.add(currentSpan);
            }

            return districtSpans;
        }

        /**
         * Gets the last active enrollment.
         *
         * @param span StudentEnrollmentSpan
         * @return Student enrollment
         */
        private StudentEnrollment getLastActiveEnrollment(StudentEnrollmentSpan span) {
            StudentEnrollment value = span.getFirstActiveEnrollment();
            for (StudentEnrollment enrollment : span.getEnrollments()) {
                if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())
                        && "Active".equals(enrollment.getStatusCode())) {
                    value = enrollment;
                }
            }
            return value;
        }

        /**
         * Within spans.
         *
         * @param date PlainDate
         * @param spans List<StudentEnrollmentSpan>
         * @return SisSchool
         */
        private SisSchool withinSpans(PlainDate date, List<StudentEnrollmentSpan> spans) {
            SisSchool school = null;
            for (StudentEnrollmentSpan span : spans) {
                if (!span.getFirstActiveDate().after(date)
                        && (span.getLastActiveDate() == null || !span.getLastActiveDate().before(date))) {
                    StudentEnrollment enrollment = getLastActiveEnrollment(span);
                    if (enrollment != null && enrollment.getSchool() != null) {
                        school = enrollment.getSchool();
                        break;
                    }
                    if (span.getSchool() != null) {
                        school = span.getSchool();
                        break;
                    }
                }
            }
            return school;
        }
    }

    /**
     * A district span, similar to the student enrollment span, but covering all activity in a
     * district.
     * This will encompass one or more enrollment spans.
     */
    protected static class DistrictEnrollmentSpan {
        StudentEnrollment m_entryEnrollment;
        PlainDate m_exitDate;
        StudentEnrollment m_exitEnrollment;
    }

    /**
     * Returns calculated values for the program from the entity.
     * <p>
     * Param:
     * <br>
     * SCHOOL: The school id (LocationId)
     * <br>
     * EXIT_DATE: The program exit date if present,
     * or the last enrollment date if the student is inactive,
     * or null if the student is still active.
     * <br>
     * QUALIFICIATION_CODE: Retrieve the exit reason. If the program is a meals program, retrieve an
     * alternate exit reason field for meals.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveProgram implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            WAStudentProgramEntity spEntity = (WAStudentProgramEntity) entity;
            String parameter = (String) field.getParameter();
            Object value = null;
            WAStudentProgram sdData = (WAStudentProgram) data;
            SisStudent student = (SisStudent) entity.getBean();
            StudentInfo stdInfo = spEntity.getStdInfo();

            SisSchool school = stdInfo.getSchool();

            StudentProgramParticipation program = null;
            StudentAssessment stdAsmWSB = null;

            if (stdInfo.getProgram() instanceof StudentProgramParticipation) {
                program = (StudentProgramParticipation) stdInfo.getProgram();
            } else if (stdInfo.getProgram() instanceof StudentAssessment) {
                stdAsmWSB = (StudentAssessment) stdInfo.getProgram();
            }

            if (PARAM_QUALIFICATION_CODE.equals(parameter)) {
                if (program != null) {
                    value = program.getFieldValueByAlias(ALIAS_PGM_QUALIFICATION);
                } else if (stdAsmWSB != null) {
                    value = stdInfo.getQualCode();
                }

                String programCode = entity.getFieldValue(FIELD_PROGRAM_CODE);

                if (PROGRAM_CODE_TRUANCY.equals(programCode)) {
                    value = "1";
                }

                if ("40".equals(programCode)) {
                    value = "273";
                }
            } else if (PARAM_SCHOOL.equals(parameter)) {
                StudentEnrollment enrollment = null;
                List<StudentEnrollmentSpan> spans = sdData.m_helper.getStudentEnrollmentSpans(student, true);
                for (StudentEnrollmentSpan span : spans) {
                    enrollment = span.getFirstActiveEnrollment();
                }
                if (null != enrollment) {
                    value = enrollment.getFieldValueByBeanPath(m_fieldLocationOverride);
                }
                if (null == value) {
                    value = school.getSchoolId();
                }
            } else if (PARAM_PGM_CODE.equals(parameter)) {
                if (program != null) {
                    value = data.lookupStateValue(StudentProgramParticipation.class,
                            StudentProgramParticipation.COL_PROGRAM_CODE,
                            program.getProgramCode());
                } else if (stdAsmWSB != null) {
                    value = stdInfo.getAsmCode();
                }
            } else if (PARAM_EXIT_DATE.equals(parameter)) {
                if (program != null) {
                    PlainDate exitDate = program.getEndDate();
                    if (exitDate == null) {
                        exitDate = stdInfo.getExitDate();
                    }
                    // if exit date is after report date return null
                    if (exitDate != null && exitDate.after(m_reportDate)) {
                        exitDate = null;
                    }

                    value = exitDate;
                } else if (stdAsmWSB != null) {
                    value = stdInfo.getExitDate();
                }
            } else if (PARAM_START_DATE.equals(parameter)) {
                if (program != null) {
                    value = program.getStartDate();
                } else if (stdAsmWSB != null) {
                    value = stdInfo.getExitDate();
                }
            }

            else if (PARAM_EXIT_REASON.equals(parameter) &&
                    !PROGRAM_CODE_36.equals(entity.getFieldValue(FIELD_PROGRAM_CODE))) {
                if (program != null) {
                    String exitReason = null;
                    String code = (String) program.getFieldValueByBeanPath(m_fieldExitReason);

                    if (StringUtils.isEmpty(code) && !StringUtils.isEmpty(stdInfo.getExitReason())) {
                        code = stdInfo.getExitReason();
                    }

                    exitReason =
                            lookupReferenceCodeByBeanPath(StudentProgramParticipation.class, m_fieldExitReason, code,
                                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                    PlainDate exitDate = program.getEndDate();
                    if (exitDate == null) {
                        exitDate = stdInfo.getExitDate();
                    }
                    // if exit date is after report date return null
                    if (exitDate != null && exitDate.after(m_reportDate)) {
                        exitReason = null;
                    }

                    value = exitReason;

                } else if (stdAsmWSB != null) {
                    value = stdInfo.getExitReason();
                }
            }
            return value;
        }
    }

    /**
     * validate exit date.
     */
    protected class ValidateExitDate implements FieldValidator {
        private static final String FIELD_START_DATE = "Start Date";
        private static final String VAL_ID = "EXIT-VAL";

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

            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String start = entity.getFieldValue(FIELD_START_DATE);
            if (!StringUtils.isEmpty(start) && !StringUtils.isEmpty(value)) {
                PlainDate startDate = formatDate(start);
                PlainDate endDate = formatDate(value);

                if (startDate != null && endDate != null && startDate.after(endDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + "  must be equal to or greater than the date in " + FIELD_START_DATE,
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                    "; " + FIELD_START_DATE + " = " + STYLE_BOLD + start + STYLE_END));
                }

            }

            return errors;
        }
    }

    /**
     * Performs field validation for the exit reason.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateProgram implements FieldValidator {
        public static final String VAL_ID = "PGM-VALIDATE";

        private static final String CODE_36 = "36";
        private static final String CODE_42 = "42";
        private static final String CODE_41 = "41";
        private List<String> m_appendixLCodes =
                new ArrayList<String>(Arrays.asList("N", "O", "P", "Q", "R", "S", "T", "U", "V", "W"));
        private List<String> m_validCodes =
                new ArrayList<String>(Arrays.asList("A", "B", "C", "D", "F", "G", "H", "I", "K"));

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String parameter = (String) field.getParameter();
            if (PARAM_EXIT_REASON.equals(parameter)) {
                String exitDate = entity.getFieldValue("Exit Date");
                String programCode = entity.getFieldValue(FIELD_PROGRAM_CODE);

                if (StringUtils.isEmpty(value) &&
                        !StringUtils.isEmpty(exitDate) && !CODE_36.equals(programCode)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Exit Reason Required",
                            "An exit reason is required whenever the exit data is populated"));
                }

                if (CODE_41.equals(programCode) || CODE_42.equals(programCode)) {
                    if (!m_appendixLCodes.contains(value)) {
                        errors.add(new StateReportValidationError(entity, field,
                                field.getFieldId() + "  must be appendix L codes : " + m_appendixLCodes.toString() +
                                        " if " + FIELD_PROGRAM_CODE + " is " + CODE_41 + " or " + CODE_42,
                                field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                        "; " + FIELD_PROGRAM_CODE + " = " + STYLE_BOLD + programCode + STYLE_END));
                    }
                } else if (!StringUtils.isEmpty(value) && !m_validCodes.contains(value)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + "  must be valid codes : " + m_validCodes.toString(),
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
                }

            }
            return errors;
        }
    }

    /**
     * Performs field validation for program code.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateProgramCode implements FieldValidator {
        public static final String VAL_ID = "PGM-CODE-VALIDATE";

        private List<String> m_validCodes =
                new ArrayList<String>(Arrays.asList("1", "16", "36", "25", "19", "32", "33", "34",
                        "35", "30", "38", "37", "11", "6", "7", "39", "40", "31", "26", "22", "21", "27", "9", "10",
                        "8", "12", "23", "43", "24",
                        "44", "41", "42"));

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            if (!StringUtils.isEmpty(value) && !m_validCodes.contains(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " must be valid codes: " + m_validCodes.toString(),
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validate Qualification code.
     */
    protected class ValidateQualification implements FieldValidator {
        private static final String VAL_ID = "QUALIFICATION-VAL";

        private static final String CODE_19 = "19";
        private static final String CODE_30 = "30";
        private static final String CODE_36 = "36";
        private static final String CODE_40 = "40";
        private static final String CODE_41 = "41";
        private static final String CODE_42 = "42";
        private static final String CODE_43 = "43";

        private List<String> m_appendixICodes =
                new ArrayList<String>(Arrays.asList("1", "2", "3", "4", "5", "6", "7", "8", "9",
                        "10", "11", "12", "13", "14", "15"));

        private List<String> m_appendixKCodes =
                new ArrayList<String>(Arrays.asList("2", "3", "4", "5", "6", "9", "11", "12",
                        "13", "14", "15", "16", "17", "18", "19", "20", "21", "24", "25", "26", "28", "29", "30", "31",
                        "32", "34", "35", "36",
                        "36", "37", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50",
                        "51", "52", "53", "54",
                        "56", "57", "62", "64", "70", "71", "72", "74", "77", "78", "79", "80", "81", "82", "84", "85",
                        "86", "87", "88", "89",
                        "90", "91", "94", "95", "96", "97", "98", "100", "101", "103", "105", "106", "111", "112",
                        "113", "115", "117", "119",
                        "122", "123", "127", "128", "129", "130", "132", "136", "137", "139", "144", "145", "146",
                        "147", "148", "155", "158",
                        "161", "162", "163", "165", "166", "168", "170", "172", "175", "181", "183", "185", "186",
                        "187", "189", "192", "193",
                        "197", "199", "201", "203", "204", "205", "208", "215", "218", "220", "221", "222", "230",
                        "236", "237", "238", "242",
                        "248", "250", "254", "256", "259", "260", "264", "265", "266", "267", "269", "270", "271",
                        "273", "274", "275", "276",
                        "277", "278", "279", "280", "281", "282", "284", "285", "286", "287", "289", "291", "294",
                        "296", "297", "302", "303",
                        "305", "308", "311", "315", "316", "319", "320", "321", "322", "325", "326", "332", "337",
                        "342", "343", "344", "345",
                        "346", "347", "443", "501", "502", "503", "504", "505", "506", "507", "509", "510", "511",
                        "512", "513", "514", "515",
                        "516", "517", "518", "519", "520", "521", "522", "523", "524", "525", "526", "527", "528",
                        "529", "530", "531", "532",
                        "533", "534", "535", "536", "537", "538", "539", "540", "541", "542", "543", "544", "545",
                        "546", "547", "548", "549",
                        "550", "551", "552", "553", "554", "555", "556", "557", "558", "559", "560", "561", "562",
                        "563", "564", "565", "566",
                        "567", "568", "569", "570", "571", "572", "573", "574", "575", "576", "577", "578", "579",
                        "580", "581", "582", "583",
                        "584", "585", "586", "587", "588", "589", "590", "591", "592", "593", "594", "639", "640",
                        "641", "642", "643", "644",
                        "645", "646", "647", "648", "649", "650", "651", "652", "653", "654", "655", "656", "657",
                        "658", "659", "660", "661",
                        "662", "663", "664", "665", "666", "667", "668", "669", "670", "671", "672", "673", "674",
                        "675", "676", "677", "678",
                        "679", "680", "681", "681", "683", "684", "685", "686", "687", "999"));

        private List<String> m_appendixRCodes =
                new ArrayList<String>(Arrays.asList("1", "2", "3", "4", "5", "6", "7", "8", "9",
                        "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25",
                        "26", "27", "28", "29",
                        "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45",
                        "46", "47", "48", "49",
                        "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65",
                        "66", "67", "68", "69",
                        "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", "85",
                        "86", "87", "88", "89",
                        "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "100", "101", "102", "103", "104",
                        "105", "106", "107", "108",
                        "109", "110", "111", "112", "113", "114", "115", "116", "117", "118", "119", "120", "121",
                        "122", "123", "124", "125",
                        "126", "127", "128", "129", "130", "131", "132", "133", "134", "135", "136", "137", "138",
                        "139", "140", "141", "142",
                        "143", "144", "145", "146", "147", "148", "149", "150", "151", "152", "153", "154", "155",
                        "156", "157", "158", "159",
                        "160", "161", "162", "163", "164", "165", "166", "167", "168", "169", "170", "171", "172",
                        "173", "174", "175", "176",
                        "177", "178", "179", "180", "181", "182", "183", "184", "185", "186", "187", "188", "189",
                        "190", "191", "192", "193",
                        "194", "195", "196", "197", "198", "199", "200", "201", "202", "203", "204", "205", "206",
                        "207", "208", "209", "210",
                        "211", "212", "213", "214", "215", "216", "217", "218", "219", "220", "221", "222", "223",
                        "224", "225", "226", "227",
                        "228", "229", "230", "231", "232", "233", "234", "235", "236", "237", "238", "239", "240",
                        "241", "242", "243", "244",
                        "245", "246", "247", "248", "249", "250", "251", "252", "253", "254", "255", "256", "257",
                        "258", "259", "260", "261",
                        "262"));

        private List<String> m_appendixXCodes =
                new ArrayList<String>(Arrays.asList("1", "3", "4", "5", "7", "8", "11", "14",
                        "16", "17", "18", "19", "20", "21", "22", "23", "24", "25"));

        private List<String> m_validGradsCodes = new ArrayList<String>(Arrays.asList("A", "B", "C"));

        private List<String> m_validTruancyCodes = new ArrayList<String>(Arrays.asList("1"));

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            SisStudent student = (SisStudent) entity.getBean();
            String programCode = entity.getFieldValue(FIELD_PROGRAM_CODE);

            List<String> validCodes = null;
            String codesName = null;

            if (CODE_19.equals(programCode)) {
                validCodes = m_appendixXCodes;
                codesName = "appendix X";
            } else if (CODE_30.equals(programCode)) {
                validCodes = m_validGradsCodes;
                codesName = "GRADS Program qualification";
            } else if (CODE_36.equals(programCode)) {
                validCodes = m_appendixICodes;
                codesName = "appendix I";
            } else if (CODE_40.equals(programCode)) {
                validCodes = m_appendixRCodes;
                codesName = "appendix R";
            } else if (CODE_41.equals(programCode) || CODE_42.equals(programCode)) {
                validCodes = m_appendixKCodes;
                codesName = "appendix K";
            } else if (CODE_43.equals(programCode)) {
                validCodes = m_validTruancyCodes;
                codesName = "Truancy Action qualification";
            }

            if (validCodes != null && !validCodes.contains(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + "  must be " + codesName + " codes : " + validCodes.toString() +
                                " if " + FIELD_PROGRAM_CODE + " is " + programCode,
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                "; " + FIELD_PROGRAM_CODE + " = " + STYLE_BOLD + programCode + STYLE_END));
            }

            if (errors.isEmpty() && CODE_36.equals(programCode) && "1".equals(value)) {
                int age = student.getPerson().getAgeAsOfDate(m_reportDate);
                if (age >= 9) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + " must be changed from 1 to another valid value prior to the " +
                                    "student’s ninth birthday if " + FIELD_PROGRAM_CODE + " is " + programCode,
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                    "; " + FIELD_PROGRAM_CODE + " = " + STYLE_BOLD + programCode + STYLE_END +
                                    "; " + "Age = " + STYLE_BOLD + age + STYLE_END));
                }
            }

            return errors;
        }
    }

    /*
     * Constants: Aliases, Fields, Parameters, Codes.
     */
    protected static final String ALIAS_ASD_LANG = "DOE SOB LANGUAGE";
    protected static final String ALIAS_ASD_METHOD = "DOE SOB METHOD";
    protected static final String ALIAS_EXCLUDE_PGM = "DOE EXCLUDE PGM";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_EXIT_REASON = "DOE EXIT REASON";
    protected static final String ALIAS_LOCATION_OVERRIDE = "DOE LOC OVERRIDE";
    protected static final String ALIAS_MEAL_QUALIFICATION = "DOE MEAL QUAL CODE";
    protected static final String ALIAS_NON_PRIMARY = "DOE NON PRIMARY";
    protected static final String ALIAS_PGM_QUALIFICATION = "DOE PGM QUAL CODE";

    protected static final String CODE_ENR_STATUS_ACTIVE = "Active";
    protected static final String CODE_ENR_STATUS_GRAD = "Graduated";
    protected static final String CODE_ENR_STATUS_INACTIVE = "Inactive";
    protected static final String CODE_NO_SHOW = "NS";
    protected static final List CODES_ENR_WITHDRAWAL = Arrays.asList(new String[] {"GA", "GB", "G0"});
    protected static final String PROGRAM_CODE_36 = "36";
    protected static final String PROGRAM_CODE_MEAL = "19";
    protected static final String PROGRAM_CODE_TRUANCY = "43";
    protected static final String PROGRAM_CODE_WAST = "41";
    protected static final String PROGRAM_CODE_WAST_42 = "42";

    protected static final String FIELD_EXIT_DATE = "Exit Date";
    protected static final String FIELD_PROGRAM_CODE = "Program Code";

    protected static final String PARAM_EXIT_DATE = "EXIT_DATE";
    protected static final String PARAM_EXIT_REASON = "EXIT_REASON";
    protected static final String PARAM_PGM_CODE = "PGM_CODE";
    protected static final String PARAM_SCHOOL = "SCHOOL";
    protected static final String PARAM_START_DATE = "START_DATE";
    protected static final String PARAM_QUALIFICATION_CODE = "QUAL_CODE";

    /**
     * Maps
     */
    protected Map<String, Collection<AssessmentColumnDefinition>> m_acdMap;
    protected Map<String, Collection<StudentAssessment>> m_asmMap;
    protected Map<String, Collection<StudentProgramParticipation>> m_pgmMap;

    protected Map m_excludeSchool;
    protected Map<String, String> m_exitReasonCodesMap;
    protected String m_fieldExcludePgm;
    protected String m_fieldExitReason;
    protected String m_fieldMealQualification;
    protected String m_fieldNonPrimaryIndicator;
    protected StudentHistoryHelper m_helper;
    protected String m_refTableEnrollmentCode;
    protected String m_refTableWithdrawalCode;
    protected PlainDate m_reportDate;
    protected String m_fieldLocationOverride;
    private SimpleDateFormat m_dateFormatt = new SimpleDateFormat(DATE_FORMAT);
    protected static final String DATE_FORMAT = "MM/dd/yyyy";

    /**
     * format date from string to plainDate.
     *
     * @param date String
     * @return PlainDate
     */
    PlainDate formatDate(String date) {
        PlainDate returnDate = null;
        if (!StringUtils.isEmpty(date)) {
            try {
                returnDate = new PlainDate(m_dateFormatt.parse(date));
            } catch (ParseException e) {
                // Nothing to do

            }
        }
        return returnDate;
    }

    /**
     * Initialize the data module.
     * Set up field retrievers
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    protected void initialize() throws X2BaseException {
        initializeFields();
        loadSchoolExcludeMap();

        m_reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));

        if (getSetupErrors().size() == 0) {
            // Create program selection criteria
            X2Criteria programCriteria = getProgramCriteria();
            m_pgmMap = getBroker().getGroupedCollectionByQuery(
                    new QueryByCriteria(StudentProgramParticipation.class, programCriteria),
                    StudentProgramParticipation.COL_STUDENT_OID, 1024);

            loadStudentAssesments();

            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(WAStudentProgramEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("PGM-INFO", new RetrieveProgram());
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldValidator>();
            validators.put(ValidateProgram.VAL_ID, new ValidateProgram());
            validators.put(ValidateProgramCode.VAL_ID, new ValidateProgramCode());
            validators.put(ValidateExitDate.VAL_ID, new ValidateExitDate());
            validators.put(ValidateQualification.VAL_ID, new ValidateQualification());

            super.addValidators(validators);
        }
    }

    /**
     * Returns the Student Programs Participation selection criteria.
     *
     * @return X2Criteria
     */
    private X2Criteria getProgramCriteria() {
        X2Criteria criteria = null;
        DataDictionaryField field =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        String referenceTableOid = field.getReferenceTableOid();
        Collection<ReferenceCode> refCodes = null;
        Collection<String> reportableCodes = null;
        if (!StringUtils.isEmpty(referenceTableOid)) {
            criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
            refCodes = getBroker().getCollectionByQuery(query);

            // Only include if state code is numeric. Ignore non-numeric state codes.
            reportableCodes = new ArrayList<String>(refCodes.size());
            Iterator<ReferenceCode> codesIterator = refCodes.iterator();
            while (codesIterator.hasNext()) {
                ReferenceCode code = codesIterator.next();
                if (StringUtils.isNumeric(code.getStateCode())) {
                    reportableCodes.add(code.getCode());
                }
            }
        }

        if (reportableCodes != null && reportableCodes.size() > 0) {
            // Student subquery for active students this year.
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            X2Criteria studentCriteria = m_helper.getStudentCriteria();
            SubQuery studentSubquery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

            criteria = new X2Criteria();
            criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubquery);
            criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, reportableCodes);
            criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDate);

            X2Criteria criteria2 = new X2Criteria();
            X2Criteria criteria3 = new X2Criteria();
            criteria2.addIsNull(StudentProgramParticipation.COL_END_DATE);
            criteria3.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE,
                    getOrganization().getCurrentContext().getStartDate());
            criteria2.addOrCriteria(criteria3);
            criteria.addAndCriteria(criteria2);

            if (isSchoolContext()) {
                criteria.addEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER +
                        Student.COL_SCHOOL_OID, getSchool().getOid());
            } else {
                criteria.addNotEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER +
                        Student.REL_SCHOOL + PATH_DELIMITER +
                        School.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                criteria.addNotEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER +
                        Student.REL_SCHOOL + PATH_DELIMITER +
                        School.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            }

            if (m_fieldExcludePgm != null) {
                criteria.addNotEqualTo(m_fieldExcludePgm, BooleanAsStringConverter.TRUE);
            }

            applyInputCriteria(criteria, false, StudentProgramParticipation.REL_STUDENT);
        } else {
            addSetupError("No reportable program codes", "Student Programs reference table state codes");
        }

        return criteria;
    }

    /**
     * Loads field aliases.
     */
    private void initializeFields() {
        m_refTableEnrollmentCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.ENROLLMENT_ENTRY_CODES);
        m_refTableWithdrawalCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);

        m_fieldNonPrimaryIndicator = translateAliasToJavaName(ALIAS_NON_PRIMARY, true);
        m_fieldMealQualification = translateAliasToJavaName(ALIAS_MEAL_QUALIFICATION, true);
        m_fieldExitReason = translateAliasToJavaName(ALIAS_EXIT_REASON, true);
        m_fieldExcludePgm = translateAliasToJavaName(ALIAS_EXCLUDE_PGM, false);
        m_fieldLocationOverride = translateAliasToJavaName(ALIAS_LOCATION_OVERRIDE, true);

        // Lookup a reverse map of ELL exit reasons state code to user code.
        m_exitReasonCodesMap = new HashMap<String, String>();
        DataDictionaryField field = getDataDictionaryField(StudentProgramParticipation.class, m_fieldExitReason);
        if (field != null) {
            ReferenceTable refTable = field.getReferenceTable();
            if (refTable != null) {
                for (ReferenceCode code : refTable.getReferenceCodes()) {
                    if (!StringUtils.isEmpty(code.getStateCode())) {
                        m_exitReasonCodesMap.put(code.getStateCode(), code.getCode());
                    }
                }
            }
        }
    }

    /**
     * Loads a map of schools that have been selected to be excluded from state reporting. (exclude
     * from reporting on school table is selected)
     */
    private void loadSchoolExcludeMap() {
        X2Criteria schoolCriteria = new X2Criteria();
        String schoolExclude = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        schoolCriteria.addEqualTo(schoolExclude, BooleanAsStringConverter.TRUE);
        BeanQuery query = new BeanQuery(School.class, schoolCriteria);
        m_excludeSchool = getBroker().getGroupedCollectionByQuery(query, X2BaseBean.COL_OID, 128);
    }

    /**
     * Populate map of STUDENT_ASSESSMENT keyed on stdOid.</br>
     * Filter by ID of AssessmentDefinition = WAST-SEAL-BILIT
     */
    private void loadStudentAssesments() {
        X2Criteria asmCriteria = new X2Criteria();
        X2Criteria asdCriteria = new X2Criteria();
        X2Criteria acdCriteria = new X2Criteria();
        X2Criteria studentCriteria = m_helper.getStudentCriteria();
        SubQuery studentSubquery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        asmCriteria.addIn(StudentAssessment.COL_STUDENT_OID, studentSubquery);

        if (isSchoolContext()) {
            asmCriteria.addEqualTo(StudentAssessment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                    getSchool().getOid());
        }


        asdCriteria.addEqualTo(AssessmentDefinition.COL_ID, "WAST-SEAL-BILIT");
        SubQuery asdSubQuery = new SubQuery(AssessmentDefinition.class, X2BaseBean.COL_OID, asdCriteria);

        asmCriteria.addIn(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID, asdSubQuery);

        QueryByCriteria asmQuery = new QueryByCriteria(StudentAssessment.class, asmCriteria);
        asmQuery.addOrderByDescending(StudentAssessment.COL_DATE);

        acdCriteria.addIn(AssessmentColumnDefinition.COL_ASSESSMENT_DEFINITION_OID, asdSubQuery);
        acdCriteria.addIn(AssessmentColumnDefinition.COL_ALIAS,
                Arrays.asList(new String[] {ALIAS_ASD_LANG, ALIAS_ASD_METHOD}));
        QueryByCriteria acdQuery = new QueryByCriteria(AssessmentColumnDefinition.class, acdCriteria);

        m_acdMap = getBroker().getGroupedCollectionByQuery(acdQuery,
                AssessmentColumnDefinition.COL_ASSESSMENT_DEFINITION_OID, 1024);

        m_asmMap = getBroker().getGroupedCollectionByQuery(asmQuery, StudentAssessment.COL_STUDENT_OID, 1024);

    }


    /**
     * Checks if schoolId given is a school to exclude. If that school is excluded this method will
     * return false ie - !included.
     *
     * @param schoolOid String
     * @return boolean
     */
    public boolean includeSchool(String schoolOid) {
        boolean removeExcluded = ((Boolean) getParameter("excludeSchool")).booleanValue();
        if (removeExcluded) {
            return (m_excludeSchool == null) || !m_excludeSchool.containsKey(schoolOid);
        }

        return true;
    }
}
