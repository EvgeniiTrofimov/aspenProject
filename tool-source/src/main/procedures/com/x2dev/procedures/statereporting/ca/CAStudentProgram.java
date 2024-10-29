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

package com.x2dev.procedures.statereporting.ca;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Export Data Module for CA Student Program.
 *
 * @author X2 Development Corporation
 */

public class CAStudentProgram extends StateReportData {

    /**
     * Entity class for CA Student Program export.
     *
     */
    public static class CAStudentProgramEntity extends StateReportEntity {
        /**
         * Entity instance variables.
         */
        CAStudentProgram m_data;
        List<StudentEnrollmentSpan> m_enrollmentSpans;
        List<StudentProgramParticipation> m_programParticipationList;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public CAStudentProgramEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Returns the StudentEnrollmentSpan record for the current index.
         *
         * @return StudentEnrollmentSpan
         */
        public StudentEnrollmentSpan getEnrollmentSpan() {
            return m_enrollmentSpans.get(getCurrentRow());
        }

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
                    ", SASID: " + student.getStateId() +
                    "] ";
            StudentProgramParticipation participation = getProgramPaticipation();
            if (participation != null) {
                name += participation.getOid();
            }

            return name;
        }

        /**
         * Returns the StudentProgramParticipation record for the current index.
         *
         * @return StudentProgramParticipation
         */
        public StudentProgramParticipation getProgramPaticipation() {
            return m_programParticipationList.get(getCurrentRow());
        }

        /**
         * Intitialize.
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
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_data = (CAStudentProgram) data;
            SisStudent student = (SisStudent) bean;
            m_programParticipationList = m_data.getStudentPrograms(student);

            if (m_programParticipationList != null) {
                initializeEnrollmentSpans(student);
            }

            if (m_programParticipationList == null) {
                m_programParticipationList = new ArrayList<StudentProgramParticipation>();
            }

            setRowCount(m_programParticipationList.size());

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
         * Populates list of EnrollmentSpans aligned to the Programs for the student.
         *
         * @param student SisStudent
         */
        private void initializeEnrollmentSpans(SisStudent student) {
            m_enrollmentSpans = new ArrayList<StudentEnrollmentSpan>();
            List<StudentEnrollmentSpan> spans = m_data.m_helper.getStudentEnrollmentSpans(student, true);
            if (spans != null) {
                for (StudentProgramParticipation participation : m_programParticipationList) {
                    PlainDate eduPgmMshipEndDate = participation.getEndDate();
                    StudentEnrollmentSpan lastSpan = null;
                    // Search for the last Span with First Active Date before
                    // Program End Date.
                    for (StudentEnrollmentSpan span : spans) {
                        if ((eduPgmMshipEndDate == null || span.getFirstActiveDate().before(eduPgmMshipEndDate))
                                && (lastSpan == null
                                        || lastSpan.getFirstActiveDate().before(span.getFirstActiveDate()))) {
                            lastSpan = span;
                        }
                    }
                    m_enrollmentSpans.add(lastSpan);
                }
            }
        }
    }

    /**
     * Retrieve fields values Education Program Participation record (entity).
     */
    protected class RetrieveParticipation implements FieldRetriever {

        private static final String PROGRAM_CODE_113_CALIFORNIA_PARTNERSHIP_ACADEMY = "113";
        private static final String PROGRAM_CODE_144_SPECIAL_EDUCATION = "144";
        private static final String PROGRAM_CODE_191_HOMELESS = "191";

        private final String PARAM_CA_PSHIP_ACADEMY_ID = "CAPAID";
        private final String PARAM_EDU_PGM_MSHIP_ENDDATE = "EDU_PGM_MSHIP_ENDDATE";
        private final String PARAM_EDU_PGM_MSHIP_STARTDATE = "EDU_PGM_MSHIP_START_DATE";
        private final String PARAM_EDU_PROGRAM_CODE = "EDU_PROGRAM_CODE";
        private final String PARAM_EDU_SERVICE_CODE = "EDU_SERVICE_CODE";
        private final String PARAM_FOSTER_ID = "FOSTER_ID";
        private final String PARAM_PGM_HMLESS_DWELL = "PGM_HMLESS_DWELL";
        private final String PARAM_PGM_MEMBERSHIP_CODE = "EDU_PGM_MSHIP_CODE";
        private final String PARAM_PGM_MIGRANT_ID = "PGM-MIGRANTID";
        private final String PARAM_PGM_RUNAWAY_IND = "PGM_RUNAWAY_IND";
        private final String PARAM_PGM_UNACCOMPANIED_IND = "PGM_UNACCOMPANIED_IND";
        private final String PARAM_PRI_DISABILITY_CODE = "PRI_DISABILITY_CODE";
        private final String PARAM_SCHOOL_OF_ATTENDANCE = "SCHOOL_OF_ATTENDANCE";
        private final String PARAM_SPED_DISTRICT_ID = "SPED_DISTRICT_ID";
        private final String PARAM_STUDENT_PGM_PARTICIPATION_OID = "STUDENT_PGM_PARTICIPATION_OID";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String parameter = (String) field.getParameter();
            StudentProgramParticipation participation = ((CAStudentProgramEntity) entity).getProgramPaticipation();
            SisStudent std = (SisStudent) entity.getBean();
            Object value = null;

            if (participation != null) {
                if (PARAM_STUDENT_PGM_PARTICIPATION_OID.equals(parameter)) {
                    value = std.getOid() + participation.getOid();
                } else if (PARAM_EDU_PROGRAM_CODE.equals(parameter)) {
                    value = getProgramCode(data, participation);
                } else if (PARAM_PGM_MEMBERSHIP_CODE.equals(parameter)) {
                    String eduProgramMembershipCode =
                            (String) participation.getFieldValueByBeanPath(m_fieldProgramMemberCode);
                    value = data.lookupReferenceCodeByBeanPath(StudentProgramParticipation.class,
                            m_fieldProgramMemberCode, eduProgramMembershipCode,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                } else if (PARAM_PGM_UNACCOMPANIED_IND.equals(parameter)) {
                    String eduProgramCode = entity.getFieldValue(DATA_FIELD_FN_EDU_PGM_CODE);
                    boolean runawayInd =
                            BooleanAsStringConverter.TRUE.equals(entity.getFieldValue(DATA_FIELD_FN_EDU_PGM_CODE));
                    if (PROGRAM_CODE_191_HOMELESS.equals(eduProgramCode) && runawayInd) {
                        value = BooleanAsStringConverter.TRUE;
                    } else {
                        value = participation.getFieldValueByAlias(m_fieldUnaccompaniedInd);
                    }
                } else if (PARAM_EDU_PGM_MSHIP_STARTDATE.equals(parameter)) {
                    value = participation.getStartDate();
                } else if (PARAM_EDU_PGM_MSHIP_ENDDATE.equals(parameter)) {
                    value = participation.getEndDate();
                } else if (PARAM_PGM_HMLESS_DWELL.equals(parameter)) {
                    String eduProgramCode = entity.getFieldValue(DATA_FIELD_FN_EDU_PGM_CODE);
                    if (PROGRAM_CODE_191_HOMELESS.equals(eduProgramCode)) {
                        String eduHmlessDwellCode =
                                (String) participation.getStudent().getFieldValueByBeanPath(m_fieldPrimaryResCat);
                        value = lookupReferenceCodeByBeanPath(SisStudent.class,
                                m_fieldPrimaryResCat, eduHmlessDwellCode,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }

                } else if (PARAM_PGM_RUNAWAY_IND.equals(parameter)) {
                    if (m_fieldRunAwayInd != null) {
                        value = participation.getFieldValueByBeanPath(m_fieldRunAwayInd);
                    }
                } else if (PARAM_EDU_SERVICE_CODE.equals(parameter)) {
                    if (m_fieldEducationPgm != null) {
                        String eduServiceCode = (String) participation.getFieldValueByBeanPath(m_fieldEducationPgm);
                        value = lookupReferenceCodeByBeanPath(StudentProgramParticipation.class,
                                m_fieldEducationPgm, eduServiceCode,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                } else if (PARAM_CA_PSHIP_ACADEMY_ID.equals(parameter)) {
                    String programCode = getProgramCode(data, participation);
                    if (programCode != null &&
                            programCode.equals(PROGRAM_CODE_113_CALIFORNIA_PARTNERSHIP_ACADEMY)) {
                        value = participation.getFieldValueByBeanPath(m_fieldPartnerAcadID);
                    }
                } else if (PARAM_PRI_DISABILITY_CODE.equals(parameter)) {
                    String programCode = getProgramCode(data, participation);
                    if (programCode != null && programCode.equals(PROGRAM_CODE_144_SPECIAL_EDUCATION)) {
                        String priDisabilityCode =
                                (String) participation.getFieldValueByBeanPath(m_fieldPriDisabilityCode);
                        value = lookupReferenceCodeByBeanPath(StudentProgramParticipation.class,
                                m_fieldPriDisabilityCode, priDisabilityCode,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }

                } else if (PARAM_SPED_DISTRICT_ID.equals(parameter)) {
                    value = participation.getFieldValueByBeanPath(m_fieldSpedDistrictID);

                } else if (PARAM_SCHOOL_OF_ATTENDANCE.equals(parameter)) {
                    StudentEnrollmentSpan span = ((CAStudentProgramEntity) entity).getEnrollmentSpan();
                    if (span != null) {
                        value = span.getFirstActiveEnrollment().getSchool().getFieldValueByBeanPath(m_fieldSchoolId);
                    }
                } else if (PARAM_PGM_MIGRANT_ID.equals(parameter)) {
                    value = participation.getFieldValueByBeanPath(m_fieldMigrantID);
                } else if (PARAM_FOSTER_ID.equals(parameter)) {
                    value = participation.getFieldValueByBeanPath(m_fieldFosterID);
                }
            }

            return value;
        }

        /**
         * Gets the program code.
         *
         * @param data StateReportData
         * @param participation StudentProgramParticipation
         * @return String
         */
        private String getProgramCode(StateReportData data, StudentProgramParticipation participation) {
            String eduProgramCode = (String) participation.getFieldValueByBeanPath(m_fieldProgramCode);
            return data.lookupReferenceCodeByBeanPath(StudentProgramParticipation.class,
                    m_fieldProgramCode, eduProgramCode,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

        }
    }

    /**
     * FieldValidator for:
     * 3.19 California Partnership Academy ID <BR>
     * <BR>
     * VALIDATION: <BR>
     * <BR>
     * Must be a valid identifier on the CDE California Partnership Academy list, <BR>
     * found at http://www.cde.ca.gov/ci/gs/hs/documents/cpadirectory.xls <BR>
     * <BR>
     *
     * REQUIRED: If Education Program Code = 113 (California Partnership Academy) Then Y; Else N
     * <BR>
     */
    protected class ValidateCAPartnershipAcademyID implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String eduProgramCode = entity.getFieldValue(DATA_FIELD_FN_EDU_PGM_CODE);

            if (eduProgramCode.equals(MATCH_EPC_CAPA)) {
                if (value.isEmpty()) {
                    errors.add(new StateReportValidationError(entity, field,
                            "This field required if Education Program Code = " +
                                    MATCH_EPC_CAPA,
                            "Education Program Code = " + STYLE_BOLD + eduProgramCode +
                                    STYLE_END));
                }
            }

            return errors;
        }
    }

    /**
     * FieldValidator for:
     * 3.22 District of Special Education Accountability <BR>
     * <BR>
     * VALIDATION: <BR>
     * <BR>
     * If any other program than Special Education THEN field must be null <BR>
     * ELSE Must be valid 7-digit county/district code <BR>
     * Or <BR>
     * If a Charter School, must be a valid 7-digit school code <BR>
     * <BR>
     *
     * REQUIRED: If Education Program Code = 144 (Special Education)Then Y;Else N <BR>
     */
    protected class ValidateDistrictOfSpecialEducationAccountability implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String eduProgramCode = entity.getFieldValue(DATA_FIELD_FN_EDU_PGM_CODE);

            if (eduProgramCode.equals(MATCH_EPC_SPED)) {
                if (value == null || value.isEmpty()) {
                    errors.add(new StateReportValidationError(entity, field,
                            "This field required if Education Program Code = " +
                                    MATCH_EPC_SPED,
                            "Education Program Code = " + STYLE_BOLD + eduProgramCode +
                                    STYLE_END));
                }

            } else {
                if (value != null) {
                    errors.add(new StateReportValidationError(entity, field,
                            "If Education Program Code != " + MATCH_EPC_SPED +
                                    " District of Special Education Accountability must be null",
                            "Education Program Code = " + STYLE_BOLD + eduProgramCode +
                                    STYLE_END +
                                    " This field value = " + STYLE_BOLD + value +
                                    STYLE_END));
                }

            }
            return errors;
        }
    }

    /**
     * FieldValidator for:
     * 3.13 Education Program Code<BR>
     * <BR>
     * VALIDATION:<BR>
     * <BR>
     * 1) If Education Program Code = 131 (NCLB Title X Homeless) <BR>
     * Then Primary Residence Category in Student Information file must = 100, 110, 120, OR 130;
     * <BR>
     * <BR>
     * 2) If Primary Disability Code is populated <BR>
     * Then Education Program Code must equal 144 (Special Education); <BR>
     * <BR>
     * 3) If District of Special Education Accountability is populated <BR>
     * Then Education Program Code must equal 144 (Special Education) <BR>
     * <BR>
     * 4) If California Partnership Academy ID is populated <BR>
     * Then Education Program Code must equal 113 (California Partnership Academy) <BR>
     * <BR>
     * 5) If Migrant Student ID is populated <BR>
     * Then Education Program Code must equal 135 (Migrant) <BR>
     * <BR>
     * 6) Schools receiving NCLB Title I Schoolwide funding for the Reporting Academic Year <BR>
     * should not submit NCLB Title I Targeted Program records (Education Program Code = 122) <BR>
     * for the Reporting Academic Year. Participation in NCLB Title I Schoolwide programs is derived
     * <BR>
     * from the school's cumulative primary enrollment count. <BR>
     */

    protected class ValidateEducationProgramCode implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String eduProgramCode = entity.getFieldValue(DATA_FIELD_FN_EDU_PGM_CODE);

            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value Required",
                        "This field must contain a value"));
                return errors;
            }

            SisStudent student = (SisStudent) entity.getBean();

            String primResCat = (String) student.getFieldValueByBeanPath(m_fieldPrimaryResCat);

            if (eduProgramCode.equals(MATCH_EPC_NCLB_X) &&
                    (primResCat.matches(MATCH_RE_SI_PRI_RESIDENCE_CAT) && value == null)) {
                errors.add(new StateReportValidationError(entity, field,
                        "If Education Program Code = 131 (NCLB Title X Homeless) "
                                +
                                "Then Primary Residence Category in Student Information file must = 100, 110, 120, OR 130; ",
                        "Primary Residence Category Code = " + STYLE_BOLD + primResCat +
                                STYLE_END +
                                " This field value = " + STYLE_BOLD + value + STYLE_END));
            }

            String primaryDisabilityCode = entity.getFieldValue(DATA_FIELD_FN_PRIM_DISABILITY_CODE);
            if (primaryDisabilityCode != null && !primaryDisabilityCode.isEmpty()
                    && !eduProgramCode.equals(MATCH_EPC_SPED)) {
                errors.add(new StateReportValidationError(entity, field,
                        "If Primary Disability Code is populated " +
                                " Then Education Program Code must equal " +
                                MATCH_EPC_SPED + " (Special Education)",
                        "Primary Disability Code = " + STYLE_BOLD + primaryDisabilityCode +
                                STYLE_END +
                                " This field value = " + STYLE_BOLD + value + STYLE_END));
            }

            String districtAcctySPED = entity.getFieldValue(DATA_FIELD_FN_SPEC_EDU_DISTRICT_ACCTY);
            if (districtAcctySPED != null && !districtAcctySPED.isEmpty() && !eduProgramCode.equals(MATCH_EPC_SPED)) {
                errors.add(new StateReportValidationError(entity, field,
                        "If District of Special Education Accountability is populated " +
                                "Then Education Program Code must equal" + MATCH_EPC_SPED +
                                " (Special Education)",
                        " District of Special Education Accountability = " + STYLE_BOLD +
                                districtAcctySPED + STYLE_END +
                                " This field value = " + STYLE_BOLD + value + STYLE_END));
            }

            String CAPAID = entity.getFieldValue(DATA_FIELD_FN_CA_PSHIP_ACADEMY_ID);
            if (CAPAID != null && !CAPAID.isEmpty() && !eduProgramCode.equals(MATCH_EPC_CAPA)) {
                errors.add(new StateReportValidationError(entity, field,
                        "If California Partnership Academy ID is populated " +
                                "Then Education Program Code must equal " +
                                MATCH_EPC_CAPA + " (California Partnership Academy)",
                        "California Partnership Academy ID = " + STYLE_BOLD + CAPAID +
                                STYLE_END +
                                " This field value = " + STYLE_BOLD + value + STYLE_END));
            }

            String migrantStudentID = entity.getFieldValue("DATA_FIELD_FN_MIGRANT_STUDENT_ID");
            if (migrantStudentID != null && !migrantStudentID.isEmpty() && !eduProgramCode.equals(MATCH_EPC_MIGRANT)) {
                errors.add(new StateReportValidationError(entity, field,
                        "If Migrant Student ID is populated " +
                                "Then Education Program Code must equal " +
                                MATCH_EPC_MIGRANT + " (Migrant)",
                        "California Partnership Academy ID = " + STYLE_BOLD +
                                migrantStudentID + STYLE_END +
                                " This field value = " + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * FieldValidator for:
     * 3.14 Education Program Membership Code <BR>
     * <BR>
     * VALIDATION: <BR>
     * <BR>
     * 1) If Education Program Code = 127 (Gifted and Talented Education (GATE)), <BR>
     * 144 (Special Education), 175 (Free or Reduced Price Meal Program), (135) Title I Part C
     * Migrant <BR>
     * Then Education Program Membership Code can be 1 (Eligible) or 3 (Participating) <BR>
     * Else Education Program Membership Code must equal 3 (Participating) <BR>
     */
    protected class ValidateEducationProgramMembershipCode implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String eduProgramCode = entity.getFieldValue(DATA_FIELD_FN_EDU_PGM_CODE);

            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value Required",
                        "This field must contain a value"));
                return errors;
            }

            if ((!value.equals(MATCH_EPMC_ELIGIBLE) || !value.equals(MATCH_EPMC_PARTICIPATING)) &&
                    eduProgramCode.matches(MATCH_EPC_VALIDATE1)) {
                errors.add(new StateReportValidationError(entity, field,
                        "If Education Program Code in ("
                                + MATCH_EPC_GATE + "," + MATCH_EPC_SPED + "," +
                                MATCH_EPC_FRPMP + "," + MATCH_EPC_MIGRANT + ")" +
                                "Then Education Program Membership Code can be "
                                + MATCH_EPMC_ELIGIBLE + " (Eligible) or " +
                                MATCH_EPMC_PARTICIPATING + " (Participating)",
                        "Education Program Code = " + STYLE_BOLD + eduProgramCode +
                                STYLE_END +
                                " This field value = " + STYLE_BOLD + value + STYLE_END));
            } else if (!value.equals(MATCH_EPMC_PARTICIPATING)) {
                errors.add(new StateReportValidationError(entity, field,
                        "If Education Program Code not in ("
                                + MATCH_EPC_GATE + "," + MATCH_EPC_SPED + "," +
                                MATCH_EPC_FRPMP + "," + MATCH_EPC_MIGRANT + ")" +
                                "Then Education Program Membership must be " +
                                MATCH_EPMC_PARTICIPATING + " (Participating)",
                        "Education Program Code = " + STYLE_BOLD + eduProgramCode +
                                STYLE_END +
                                " This field value = " + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * FieldValidator for:
     * 3.16 Education Program Membership End Date <BR>
     * <BR>
     * VALIDATION: <BR>
     * <BR>
     * 1) Must be greater than or equal to Education Program Membership Start Date <BR>
     * 2) Must be less than or equal to Student School Exit date <BR>
     * 3) Must be greater than Student Birth Date <BR>
     * 4) Must be within Academic Year ID specified in file <BR>
     */
    protected class ValidateEducationProgramMembershipEndDate implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            SisStudent student = (SisStudent) entity.getBean();
            StudentProgramParticipation participation = ((CAStudentProgramEntity) entity).getProgramPaticipation();

            PlainDate birthDate = student.getPerson().getDob();

            PlainDate eduPgmMshipStartDate = participation.getStartDate();
            PlainDate eduPgmMshipEndDate = participation.getEndDate();

            // 1) Must be greater than or equal to Education Program Membership
            // Start Date
            if (eduPgmMshipEndDate != null &&
                    !(eduPgmMshipEndDate.after(eduPgmMshipStartDate)
                            || eduPgmMshipEndDate.equals(eduPgmMshipStartDate))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Must be greater than or equal to Education Program Membership Start Date",
                        " Educational Programm Membership Start Date = " + STYLE_BOLD +
                                eduPgmMshipStartDate + STYLE_END +
                                " Educational Programm Membership End Date = " +
                                STYLE_BOLD + eduPgmMshipEndDate + STYLE_END));
            }

            // 2) Must be less than or equal to Student School Exit date
            StudentEnrollmentSpan span = ((CAStudentProgramEntity) entity).getEnrollmentSpan();
            PlainDate exitDate = null;
            if (span != null && span.getFirstInactiveEnrollment() != null) {
                exitDate = span.getFirstInactiveEnrollment().getEnrollmentDate();
            }

            if (exitDate != null) {
                if (eduPgmMshipEndDate == null || eduPgmMshipEndDate.after(exitDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Must be less than or equal to Student School Exit date",
                            "Student School Exit date = " +
                                    STYLE_BOLD +
                                    span.getFirstInactiveEnrollment().getEnrollmentDate().toString() +
                                    STYLE_END));
                }
            }

            // 3) Must be greater than Student Birth Date
            if (eduPgmMshipEndDate != null && !eduPgmMshipEndDate.after(birthDate)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Must be greater than Student Birth Date",
                        "Student Birth Date = " + STYLE_BOLD + birthDate + STYLE_END +
                                " Educational Programm Membership End Date = " +
                                STYLE_BOLD + eduPgmMshipEndDate + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * FieldValidator for:
     * 3.15 Education Program Membership Start Date <BR>
     * <BR>
     * VALIDATION: <BR>
     * <BR>
     * 1) Must be greater than Student Birth Date <BR>
     * 2) If Education Program Membership Code equals 3 (Participating) Then student must be
     * enrolled in the <BR>
     * School of Attendance in this record during the Education Program Membership Date Range
     * specified. <BR>
     * 3) Must be less than or equal to current date plus six months <BR>
     */

    protected class ValidateEducationProgramMembershipStartDate implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            SisStudent student = (SisStudent) entity.getBean();
            StudentProgramParticipation participation = ((CAStudentProgramEntity) entity).getProgramPaticipation();

            PlainDate birthDate = student.getPerson().getDob();
            String eduPgmMembershipCode = entity.getFieldValue(DATA_FIELD_FN_EDU_PGM_MSHIP_CODE);
            PlainDate eduPgmMshipStartDate = participation.getStartDate();

            PlainDate currentDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
            PlainDate currentDatePlusSixMonth = DateUtils.add(currentDate, Calendar.MONTH, 6);

            String schoolOfAttendance = entity.getFieldValue(DATA_FIELD_FN_SCHOOL_OF_ATTENDANCE);

            if (eduPgmMshipStartDate != null) {
                // 1) Must be greater than Student Birth Date
                if (!eduPgmMshipStartDate.after(birthDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Must be greater than Student Birth Date",
                            "Student Birth Date = " + STYLE_BOLD + birthDate + STYLE_END +
                                    " Educational Programm Membership Start Date = " +
                                    STYLE_BOLD + eduPgmMshipStartDate + STYLE_END));
                }

                // 2) If Education Program Membership Code equals 3
                // (Participating) Then student must be enrolled in the
                // School of Attendance in this record during the Education
                // Program Membership Date Range specified.
                if (eduPgmMembershipCode.equals(MATCH_EPMC_PARTICIPATING)) {
                    StudentEnrollmentSpan span = ((CAStudentProgramEntity) entity).getEnrollmentSpan();

                    if (span == null || !span.getFirstActiveEnrollment().getSchoolOid().equals(schoolOfAttendance)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "Student must be enrolled in the School of Attendance in this record"
                                        +
                                        "during the Education Program Membership Date Range specified",
                                "School of Attendance = " + STYLE_BOLD +
                                        schoolOfAttendance + STYLE_END));

                    }

                }

                // 3) Must be less than or equal to current date plus six months
                if (eduPgmMshipStartDate.after(currentDatePlusSixMonth)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Must be less than or equal to current date plus six months",
                            "Current System Date plus six months = " + STYLE_BOLD +
                                    currentDatePlusSixMonth + STYLE_END +
                                    " Educational Programm Membership Start Date = " +
                                    STYLE_BOLD + eduPgmMshipStartDate + STYLE_END));
                }
            }

            return errors;
        }
    }

    /**
     * FieldValidator for:
     * 3.17 Education Service Academic Year <BR>
     * <BR>
     * REQUERED: N; May only be populated if Education Program Code = 122 (NCLB Title 1 Part A Basic
     * Targeted) <BR>
     */
    protected class ValidateEducationServiceAcademicYear implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String eduProgramCode = entity.getFieldValue(DATA_FIELD_FN_EDU_PGM_CODE);

            if (value != null && !value.isEmpty() && !eduProgramCode.equals(MATCH_EPC_NCLB_1A)) {
                errors.add(new StateReportValidationError(entity, field,
                        "This field may only be populated if Education Program Code = " +
                                MATCH_EPC_NCLB_1A,
                        " Education Program Code = " + STYLE_BOLD + eduProgramCode +
                                STYLE_END));
            }

            return errors;
        }
    }

    /**
     * FieldValidator for:
     * 3.18 Education Service Code <BR>
     * <BR>
     * VALIDATION: <BR>
     * <BR>
     * If Education Program Code does not equal 122 (NCLB Title 1 Part A Basic Targeted) <BR>
     * Then Education Service Code must be null <BR>
     * <BR>
     *
     * REQUIRED: If Education Program Code = 122 (NCLB Title 1 Part A Basic Targeted) Then Y; Else N
     * <BR>
     */
    protected class ValidateEducationServiceCode implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String eduProgramCode = entity.getFieldValue(DATA_FIELD_FN_EDU_PGM_CODE);

            if (eduProgramCode.equals(MATCH_EPC_NCLB_1A)) {
                if (StringUtils.isEmpty(value)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "This field required if Education Program Code = " +
                                    MATCH_EPC_NCLB_1A,
                            "Education Program Code = " + STYLE_BOLD + eduProgramCode +
                                    STYLE_END));
                }
            } else {
                if (!StringUtils.isEmpty(value)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "If Education Program Code != " + MATCH_EPC_NCLB_1A +
                                    " Education Service Code must be null",
                            "Education Program Code = " + STYLE_BOLD + eduProgramCode +
                                    STYLE_END +
                                    " This field value = " + STYLE_BOLD + value +
                                    STYLE_END));
                }
            }
            return errors;
        }
    }

    /**
     * Validator for empty fields.
     */
    protected class ValidateEmptyValue implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Value Required",
                        "This field must contain a value"));
                return errors;
            }
            return errors;
        }
    }


    /**
     * FieldValidator for:
     * 3.20 Migrant Student ID <BR>
     * <BR>
     * VALIDATION : <BR>
     * <BR>
     * If Education Program Code = 135 (Migrant) Then Y; Else N <BR>
     */
    protected class ValidateMigrantStudentID implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String eduProgramCode = entity.getFieldValue(DATA_FIELD_FN_EDU_PGM_CODE);

            if (eduProgramCode.equals(MATCH_EPC_MIGRANT)) {
                if (value == null || value.isEmpty()) {
                    errors.add(new StateReportValidationError(entity, field,
                            "This field required if Education Program Code = " +
                                    MATCH_EPC_MIGRANT,
                            "Education Program Code = " + STYLE_BOLD + eduProgramCode +
                                    STYLE_END));
                }
            }
            return errors;
        }
    }

    /**
     * FieldValidator for:
     * 3.21 Primary Disability Code <BR>
     * <BR>
     * VALIDATION: <BR>
     * <BR>
     * 1) If any other program than Special Education then field must be null <BR>
     * 2) If the Primary Disability Code changes for a student but the Education Program Membership
     * <BR>
     * Start Date does not change, then the prior Primary Disability Code will be overwritten. <BR>
     * <BR>
     *
     * REQUIRED: If Education Program Code = 144 (Special Education) Then Y; Else N <BR>
     */
    protected class ValidatePrimaryDisabilityCode implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String eduProgramCode = entity.getFieldValue(DATA_FIELD_FN_EDU_PGM_CODE);

            if (eduProgramCode.equals(MATCH_EPC_SPED)) {
                if (value.isEmpty()) {
                    errors.add(new StateReportValidationError(entity, field,
                            "This field required if Education Program Code = " +
                                    MATCH_EPC_SPED,
                            "Education Program Code = " + STYLE_BOLD + eduProgramCode +
                                    STYLE_END));
                }
            } else {
                if (value != null) {
                    errors.add(new StateReportValidationError(entity, field,
                            "If Education Program Code does != " + MATCH_EPC_SPED +
                                    "  Primary Disability Code must be null",
                            "Education Program Code = " + STYLE_BOLD + eduProgramCode +
                                    STYLE_END +
                                    " This field value = " + STYLE_BOLD + value +
                                    STYLE_END));
                }

            }
            return errors;
        }
    }

    /*
     * Aliases for fields to look up.
     */
    protected static final String ALIAS_EDU_TITLE_CODE = "DOE EDU TITLE I CODE";
    protected static final String ALIAS_EDUCATION_PGM = "DOE_ED_PGM_SVC_CD";

    protected static final String ALIAS_EXCLUDE_PGM = "DOE EXCLUDE PGM";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_FOSTER_ID = "DOE FOSTER ID";
    protected static final String ALIAS_MIGRANT_ID = "DOE MIGRANT STUDENT ID";
    protected static final String ALIAS_PARTNER_ACAD_ID = "DOE CA PARTNER ACAD ID";

    protected static final String ALIAS_PGM_CODE = "DOE EDUCATION PGM CODE";
    protected static final String ALIAS_PGM_MEMBER_CODE = "DOE PGM MEMBER CODE";
    protected static final String ALIAS_PGM_RUNAWAY_IND = "DOE RUNAWAY";
    protected static final String ALIAS_PGM_UNACCOMPANIED_IND = "DOE UNACCOMPANIED";
    protected static final String ALIAS_PRI_DISBILITY_CODE = "DOE PRI DISABILITY CODE";
    protected static final String ALIAS_PRIMARY_RES_CAT = "DOE PRIMARY RES CAT";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SPED_DISTRICT_ID = "DOE SPED DISTRICT ID";
    protected static final String ALIAS_SSID = "DOE SASID";

    /*
     * Constants for history helper parameters from user input template.
     */
    protected static final String PARAM_ALL_SCHOOLS = "allSchools";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_SCHOOLS = "schoolOids";
    protected static final String PARAM_WITHOUT_SSID = "withoutSsid";

    protected static final String DATA_FIELD_FN_CA_PSHIP_ACADEMY_ID = "CPAID";
    protected static final String DATA_FIELD_FN_EDU_PGM_CODE = "EduPgmCode";
    protected static final String DATA_FIELD_FN_EDU_PGM_MSHIP_CODE = "EduPgmMshipCode";
    protected static final String DATA_FIELD_FN_MIGRANT_STUDENT_ID = "MigrantStudentID";
    protected static final String DATA_FIELD_FN_PRIM_DISABILITY_CODE = "PrimDisabilityCode";
    protected static final String DATA_FIELD_FN_RUNAWAY_IND = "RunawayInd";
    protected static final String DATA_FIELD_FN_SCHOOL_OF_ATTENDANCE = "SchoolOfAttendance";
    protected static final String DATA_FIELD_FN_SPEC_EDU_DISTRICT_ACCTY = "SpecEduDistrictAccty";

    /* Education Program Codes */
    protected static final String MATCH_EPC_CAPA = "113";
    protected static final String MATCH_EPC_FRPMP = "175";
    protected static final String MATCH_EPC_GATE = "127";
    protected static final String MATCH_EPC_MIGRANT = "135";
    protected static final String MATCH_EPC_NCLB_1A = "122";
    protected static final String MATCH_EPC_NCLB_X = "131";
    protected static final String MATCH_EPC_SPED = "144";

    protected static final String MATCH_EPC_VALIDATE1 = "^(127|144|175|135)$";

    /* Education Program Membership Codes */
    protected static final String MATCH_EPMC_ELIGIBLE = "1";
    protected static final String MATCH_EPMC_PARTICIPATING = "3";

    // Primary Residence Category in Student Information file (homeless)
    protected static final String MATCH_RE_SI_PRI_RESIDENCE_CAT = "^(100|110|120|130)$";

    /*
     * Instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_fieldEduTitleCode;
    protected String m_fieldEducationPgm;
    protected String m_fieldExcludePgm;
    protected String m_fieldExcludeSchool;
    protected String m_fieldFosterID;
    protected String m_fieldMigrantID;
    protected String m_fieldPartnerAcadID;
    protected String m_fieldPriDisabilityCode;
    protected String m_fieldPrimaryResCat;
    protected String m_fieldProgramCode;
    protected String m_fieldProgramMemberCode;
    protected String m_fieldRunAwayInd;
    protected String m_fieldSchoolId;
    protected String m_fieldSpedDistrictID;
    protected String m_fieldSsid;
    protected String m_fieldUnaccompaniedInd;

    protected StudentHistoryHelper m_helper;

    private Map<String, List<StudentProgramParticipation>> m_studentProgramMap;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {

        initializeFields();

        if (getSetupErrors().size() != 0) {
            return;
        }

        initQuery();

        initStudentProgramMap();

        setEntityClass(CAStudentProgramEntity.class);

        initFieldRetrievers();

        initFieldValidators();

    }

    /**
     * Returns the StudentProgramParticipation records for the student.
     *
     * @param student SisStudent
     * @return List containing the records for the student
     */
    protected List<StudentProgramParticipation> getStudentPrograms(SisStudent student) {
        return m_studentProgramMap.get(student.getOid());
    }

    /**
     * Method for initialization of field retrievers.
     */
    private void initFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put("PGM-PARTICIPATION", new RetrieveParticipation());
        super.addCalcs(calcs);
    }

    /**
     * Method for initialization of field validators.
     */
    private void initFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put("PGM-EDUPGMCODE", new ValidateEducationProgramCode());
        validators.put("PGM-EDUPGMMSHIPCODE", new ValidateEducationProgramMembershipCode());
        validators.put("PGM-EDUPGMMSHIPSD", new ValidateEducationProgramMembershipStartDate());
        validators.put("PGM-EDUPGMMSHIPED", new ValidateEducationProgramMembershipEndDate());
        validators.put("PGM-EDUSRVCACADYEAR", new ValidateEducationServiceAcademicYear());
        validators.put("PGM-EDUSRVCCODE", new ValidateEducationServiceCode());
        validators.put("PGM-CAPAID", new ValidateCAPartnershipAcademyID());
        validators.put("PGM-MIGRANTSTUDENTID", new ValidateMigrantStudentID());
        validators.put("PGM-PRIMDISABILITYCODE", new ValidatePrimaryDisabilityCode());
        validators.put("PGM-DISTRICTSPEDACCTY", new ValidateDistrictOfSpecialEducationAccountability());
        validators.put("STD-EMP", new ValidateEmptyValue());
        super.addValidators(validators);
    }

    /**
     * Lookup field aliases and paths.s
     */
    private void initializeFields() {
        m_fieldEduTitleCode = translateAliasToJavaName(ALIAS_EDU_TITLE_CODE, true);
        m_fieldEducationPgm = translateAliasToJavaName(ALIAS_EDUCATION_PGM, true);
        m_fieldExcludePgm = translateAliasToJavaName(ALIAS_EXCLUDE_PGM, true);
        m_fieldExcludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_fieldFosterID = translateAliasToJavaName(ALIAS_FOSTER_ID, true);
        m_fieldMigrantID = translateAliasToJavaName(ALIAS_MIGRANT_ID, true);
        m_fieldPartnerAcadID = translateAliasToJavaName(ALIAS_PARTNER_ACAD_ID, true);
        m_fieldPriDisabilityCode = translateAliasToJavaName(ALIAS_PRI_DISBILITY_CODE, true);
        m_fieldPrimaryResCat = translateAliasToJavaName(ALIAS_PRIMARY_RES_CAT, true);
        m_fieldProgramCode = translateAliasToJavaName(ALIAS_PGM_CODE, true);
        m_fieldProgramMemberCode = translateAliasToJavaName(ALIAS_PGM_MEMBER_CODE, true);
        m_fieldRunAwayInd = translateAliasToJavaName(ALIAS_PGM_RUNAWAY_IND, true);
        m_fieldSchoolId = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldSpedDistrictID = translateAliasToJavaName(ALIAS_SPED_DISTRICT_ID, true);
        m_fieldSsid = translateAliasToJavaName(ALIAS_SSID, true);
        m_fieldUnaccompaniedInd = translateAliasToJavaName(ALIAS_PGM_UNACCOMPANIED_IND, true);

    }

    /**
     * Method for initialization of export query.
     * Internally it uses StudentHistoryHelper
     */
    private void initQuery() {
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        PlainDate reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (getCurrentContext().getEndDate().before(reportDate)) {
            reportDate = getCurrentContext().getEndDate();
        }
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, reportDate);

        Boolean isWithoutSsid = (Boolean) getParameter(PARAM_WITHOUT_SSID);
        String schoolOids = (String) getParameter(PARAM_SCHOOLS);
        Boolean isAllSchools = (Boolean) getParameter(PARAM_ALL_SCHOOLS);
        X2Criteria sklCriteria = new X2Criteria();

        if (isAllSchools.booleanValue()) {
            sklCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            sklCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        } else {
            Set<String> setSchoolOids = new HashSet<String>();
            setSchoolOids.addAll(Arrays.asList(schoolOids.split(",")));
            sklCriteria.addIn(X2BaseBean.COL_OID, setSchoolOids);
        }

        /**
         * Excluding schools with the Do not report indicator
         */
        sklCriteria.addNotEqualTo(m_fieldExcludeSchool, BooleanAsStringConverter.TRUE);

        SubQuery sklSubQuery = new SubQuery(SisSchool.class, X2BaseBean.COL_OID, sklCriteria);

        m_helper.getStudentCriteria().addIn(Student.COL_SCHOOL_OID, sklSubQuery);
        if (isWithoutSsid.booleanValue()) {
            m_helper.getStudentCriteria().addEmpty(m_fieldSsid, getBroker().getPersistenceKey());
        }

        QueryByCriteria query = m_helper.getStudentQuery(false);
        setQuery(query);
    }

    /**
     * Inits the student program map.
     */
    private void initStudentProgramMap() {
        X2Criteria studentCriteria = m_helper.getStudentCriteria();

        PlainDate reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));

        X2Criteria programCriteria = new X2Criteria();

        programCriteria.addNotEqualTo(m_fieldExcludePgm, BooleanAsStringConverter.TRUE);
        programCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                getCurrentContext().getStartDate());
        programCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, reportDate);
        programCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, getStateReportableCodes(
                StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE));

        X2Criteria endDate1Criteria = new X2Criteria();
        endDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());

        X2Criteria endDate2Criteria = new X2Criteria();
        endDate2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE,
                getCurrentContext().getStartDate());

        endDate1Criteria.addOrCriteria(endDate2Criteria);

        programCriteria.addAndCriteria(endDate1Criteria);

        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
        programCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);

        QueryByCriteria programQuery = new QueryByCriteria(StudentProgramParticipation.class, programCriteria);
        programQuery.addOrderBy(StudentProgramParticipation.COL_STUDENT_OID, true);
        m_studentProgramMap = getBroker().getGroupedCollectionByQuery(programQuery,
                StudentProgramParticipation.COL_STUDENT_OID, 1024);
    }

    private Collection<String> getStateReportableCodes(Class beanClass,
                                                       String beanPath) {
        Set<String> codes = new HashSet<String>();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(beanClass.getName(), beanPath);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());

        String[] columns = new String[] {ReferenceCode.COL_CODE};
        ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] record = (Object[]) iterator.next();
                String code = (String) record[0];
                codes.add(code);
            }
        } finally {
            iterator.close();
        }
        if (codes.isEmpty()) {
            codes.add("--No-Match--");
        }
        return codes;
    }
}
