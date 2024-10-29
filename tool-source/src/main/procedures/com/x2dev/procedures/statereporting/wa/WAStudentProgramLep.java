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
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
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
import java.util.Date;
import java.util.HashMap;
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
public class WAStudentProgramLep extends StateReportData {

    /**
     * The Class WAStudentProgramLepEntity.
     */
    public static class WAStudentProgramLepEntity extends StateReportEntity {
        private static final String FIELD_START_DATE = "Start Date";
        /**
         * The calculated school for the program.
         */
        private SisSchool m_school;
        /**
         * The calculated exit date for students who have withdrawn.
         */
        private ArrayList<DistrictEnrollmentSpan> m_distspans;

        /**
         * Filter entities where the Program Exit Date is before the Program Start Date.
         * 
         * @return StateReportValidationError
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;
            FieldDefinition fieldStartDate = getData().getFieldDefinition(FIELD_START_DATE);
            SimpleDateFormat dateFormat = (SimpleDateFormat) fieldStartDate.getFormatter();
            String valueStartDate = getFieldValue(FIELD_START_DATE);
            String valueExitDate = getFieldValue(FIELD_EXIT_DATE);
            if (!StringUtils.isEmpty(valueStartDate) && !StringUtils.isEmpty(valueExitDate)) {
                Date startDate = null;
                Date exitDate = null;
                try {
                    startDate = dateFormat.parse(valueStartDate);
                    exitDate = dateFormat.parse(valueExitDate);
                } catch (ParseException e) {
                    e.printStackTrace();
                }

                if (startDate != null && exitDate != null && startDate.after(exitDate)) {
                    error = new StateReportValidationError(this, fieldStartDate, "Start date can`t be after Exit date.",
                            "");
                }
            }
            return error;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            StudentProgramParticipation program = (StudentProgramParticipation) getBean();
            SisStudent student = program.getStudent();

            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] " + program.getProgramCode();

            return name;
        }

        /**
         * Gets the current span.
         *
         * @return District enrollment span
         */
        protected DistrictEnrollmentSpan getCurrentSpan() {
            return m_distspans.get(getCurrentRow());
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
            m_distspans = new ArrayList<WAStudentProgramLep.DistrictEnrollmentSpan>();
            WAStudentProgramLep spData = (WAStudentProgramLep) data;
            StudentProgramParticipation program = (StudentProgramParticipation) bean;
            SisStudent student = program.getStudent();
            List<StudentEnrollmentSpan> enrollmentSpans = spData.m_helper.getStudentEnrollmentSpans(student, true);

            // Skip the program if the student is not in the District Student file.
            List<DistrictEnrollmentSpan> districtSpans =
                    getDistrictSpans(((StudentProgramParticipation) bean).getStudent(), spData);
            if (districtSpans.size() == 0) {
                setRowCount(0);
            } else {
                // Get most recent span
                for (DistrictEnrollmentSpan dSpan : districtSpans) {
                    if (program.getEndDate() == null
                            || (dSpan.m_entryEnrollment.getEnrollmentDate() != null && program.getEndDate() != null &&
                                    !dSpan.m_entryEnrollment.getEnrollmentDate().after(program.getEndDate()))) {
                        if (dSpan.m_exitEnrollment == null || dSpan.m_exitEnrollment.getEnrollmentDate() == null ||
                                (dSpan.m_exitEnrollment.getEnrollmentDate() != null &&
                                        program.getStartDate() != null &&
                                        !dSpan.m_exitEnrollment.getEnrollmentDate().before(program.getStartDate()))) {

                            String exitReason = dSpan.m_exitReason;
                            // Get exit reason as a translation of the withdrawal code.
                            StudentEnrollment withdrawal = dSpan.m_exitEnrollment;
                            if (withdrawal != null) {
                                String code = withdrawal.getEnrollmentCode();
                                code = data.lookupReferenceCodeByRefTbl(spData.m_refTableWithdrawalCode, code,
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                                if (code != null) {
                                    if (WITHD_EQUALS_B.contains(code)) {
                                        exitReason = "B";
                                    } else if (WITHD_EQUALS_C.contains(code)) {
                                        exitReason = "C";
                                    } else if (WITHD_EQUALS_D.contains(code)) {
                                        exitReason = "D";
                                    } else if (WITHD_EQUALS_E.contains(code)) {
                                        exitReason = "E";
                                    } else if (WITHD_EQUALS_I.contains(code)) {
                                        exitReason = "I";
                                    } else if (WITHD_EQUALS_T.contains(code)) {
                                        exitReason = "T";
                                    }
                                }
                            }
                            dSpan.m_exitReason = spData.m_exitReasonCodesMap.get(exitReason);
                            m_distspans.add(dSpan);
                        }
                    }
                }
            }

            // Find the enrollment school and last exit date.
            for (StudentEnrollmentSpan span : enrollmentSpans) {
                if (program.getEndDate() == null ||
                        (span.getFirstActiveDate() != null &&
                                program.getEndDate() != null &&
                                !span.getFirstActiveDate().after(program.getEndDate()))) {
                    if (span.getLastActiveDate() == null ||
                            (span.getLastActiveDate() != null &&
                                    program.getStartDate() != null &&
                                    !span.getLastActiveDate().before(program.getStartDate()))) {
                        m_school = span.getSchool();
                    }
                }
            }
            if (m_school == null) {
                m_school = student.getSchool();
            }

            if (!spData.includeSchool(m_school.getOid())) {
                setRowCount(0);
            } else {
                setRowCount(m_distspans.size());
            }
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
         * Return the school associated with the specified program.
         *
         * @return SisSchool
         */
        protected SisSchool getSchool() {
            return m_school;
        }

        /**
         * Calculate district enrollment spans from the student school enrollment spans.
         * Look for spans with withdrawal codes that represent district withdrawal vs. in district
         * transfer.
         *
         * @param student Student
         * @param sdData WAStudentProgramLep
         * @return List<DistrictEnrollmentSpan>
         */
        private List<DistrictEnrollmentSpan> getDistrictSpans(Student student, WAStudentProgramLep sdData) {
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

                /*
                 * Check if the enrollment span is non-primary. If so, do not include.
                 */
                enrollment = span.getFirstActiveEnrollment();
                if (enrollment != null) {
                    String nonPrimary = (String) enrollment.getFieldValueByBeanPath(sdData.m_fieldNonPrimaryIndicator);
                    if (BooleanAsStringConverter.TRUE.equals(nonPrimary)) {
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
                            currentSpan.m_entryDate = span.getFirstActiveDate();
                            currentSpan.m_exitDate = span.getLastActiveDate();
                            currentSpan.m_exitEnrollment = span.getFirstInactiveEnrollment();
                            currentSpan.m_exitReason = "D";
                        } else {
                            if (currentSpan == null) {
                                currentSpan = new DistrictEnrollmentSpan();
                                currentSpan.m_entryEnrollment = enrollment;
                                currentSpan.m_entryDate = span.getFirstActiveDate();
                            }
                            currentSpan.m_exitDate = span.getLastActiveDate();
                            currentSpan.m_exitEnrollment = span.getFirstInactiveEnrollment();
                            currentSpan.m_exitReason = "D";
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
    }

    /**
     * A district span, similar to the student enrollment span, but covering all activity in a
     * district.
     * This will encompass one or more enrollment spans.
     */
    protected static class DistrictEnrollmentSpan {

        StudentEnrollment m_entryEnrollment;
        PlainDate m_entryDate;
        PlainDate m_exitDate;
        StudentEnrollment m_exitEnrollment;
        String m_exitReason;

        /**
         * Gets the entry enrollment.
         *
         * @return the m_entryEnrollment
         */
        public StudentEnrollment getEntryEnrollment() {
            return m_entryEnrollment;
        }

        /**
         * Gets the entry date.
         *
         * @return the m_entryDate
         */
        public PlainDate getEntryDate() {
            return m_entryDate;
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
         * Gets the exit enrollment.
         *
         * @return the m_exitEnrollment
         */
        public StudentEnrollment getExitEnrollment() {
            return m_exitEnrollment;
        }

        /**
         * Gets the exit reason.
         *
         * @return the m_exitReason
         */
        public String getExitReason() {
            return m_exitReason;
        }
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
        private PlainDate m_districtStartDate;
        private Map<String, PlainDate> m_schoolStartDate = new HashMap();

        /**
         * Instantiates a new retrieve program.
         *
         * @param districtStartDate PlainDate
         */
        public RetrieveProgram(PlainDate districtStartDate) {
            m_districtStartDate = districtStartDate;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            WAStudentProgramLepEntity spEntity = (WAStudentProgramLepEntity) entity;
            WAStudentProgramLep splData = (WAStudentProgramLep) data;
            String parameter = (String) field.getParameter();
            Object value = null;
            StudentProgramParticipation program = (StudentProgramParticipation) entity.getBean();
            SisStudent student = program.getStudent();

            if (PARAM_SCHOOL.equals(parameter)) {
                StudentEnrollment enrollment = null;
                List<StudentEnrollmentSpan> spans = splData.m_helper.getStudentEnrollmentSpans(student, true);
                for (StudentEnrollmentSpan span : spans) {
                    enrollment = span.getFirstActiveEnrollment();
                }
                if (null != enrollment) {
                    value = enrollment.getFieldValueByBeanPath(m_fieldLocationOverride);
                }
                if (null == value) {
                    SisSchool school = spEntity.getSchool();
                    value = school.getSchoolId();
                }
            } else if (PARAM_START_DATE.equals(parameter)) {
                String stateCode = data.lookupReferenceCodeByBeanPath(StudentProgramParticipation.class,
                        m_fieldExitReason,
                        (String) program.getFieldValueByBeanPath(m_fieldExitReason),
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                PlainDate startDate = null;

                if (CODE_DID_NOT_QUAL.equals(stateCode)) {
                    startDate = program.getStartDate();
                } else {
                    startDate = program.getStartDate();
                    PlainDate schoolStartDate = getSchoolStartDate(spEntity.getSchool());

                    PlainDate enrollmentDate = spEntity.getCurrentSpan().getEntryDate();
                    if (enrollmentDate != null) {
                        if (startDate == null || startDate.before(enrollmentDate)) {
                            startDate = enrollmentDate;
                        }
                    }
                    if (schoolStartDate != null) {
                        if (startDate == null || startDate.before(schoolStartDate)) {
                            startDate = schoolStartDate;
                        }
                    }

                }
                value = startDate;
            } else if (PARAM_EXIT_DATE.equals(parameter)) {
                PlainDate exitDate = program.getEndDate();

                if (exitDate == null) {
                    exitDate = spEntity.getCurrentSpan().getExitDate();
                }
                value = exitDate;
            } else if (PARAM_EXIT_REASON.equals(parameter)) {
                PlainDate exitDate = program.getEndDate();
                String exitReason = (String) program.getFieldValueByBeanPath(m_fieldExitReason);
                if (exitDate == null) {
                    exitDate = spEntity.getCurrentSpan().getExitDate();
                }
                // If there is an exit date, make sure there is an exit reason.
                if (exitDate != null) {
                    if (exitReason == null) {
                        exitReason = spEntity.getCurrentSpan().getExitReason();
                    }
                }
                value = exitReason;
            } else if (PARAM_LEP_TEST_CODE.equals(parameter)) {
                String localCode = (String) program.getFieldValueByBeanPath(splData.m_fieldLepTestCode);

                if (splData.m_lepTestCodes.containsKey(localCode)) {
                    value = splData.m_lepTestCodes.get(localCode);
                }
            } else if (PARAM_LEP_MODEL_CODE.equals(parameter)) {
                String localCode = (String) program.getFieldValueByBeanPath(splData.m_fieldLepModelCode);

                if (splData.m_lepModelCodes.containsKey(localCode)) {
                    value = splData.m_lepModelCodes.get(localCode);
                }
            } else if (PARAM_LEP_GRADE_CODE.equals(parameter)) {
                String localCode = (String) program.getFieldValueByBeanPath(splData.m_fieldLepGradeCode);

                if (splData.m_lepGradeCodes.containsKey(localCode)) {
                    value = splData.m_lepGradeCodes.get(localCode);
                }
            } else if (PARAM_PROGRAM_DESIGNATION.equals(parameter)) {
                String localCode = (String) program.getFieldValueByBeanPath(splData.m_fieldLepPgmDesignCode);

                if (splData.m_lepPgmDesignCodes.containsKey(localCode)) {
                    value = splData.m_lepPgmDesignCodes.get(localCode);
                }
            } else if (PARAM_LEP_TEST_LEVEL.equals(parameter)) {
                String level = (String) program.getFieldValueByBeanPath(splData.m_fieldLepTestLevel);

                if (splData.m_lepTestLevels.containsKey(level)) {
                    value = splData.m_lepTestLevels.get(level);
                }
            }

            return value;
        }

        /**
         * Gets the school start date.
         *
         * @param school SisSchool
         * @return Plain date
         */
        PlainDate getSchoolStartDate(SisSchool school) {
            PlainDate value = null;
            if (m_schoolStartDate.containsKey(school.getOid())) {
                value = m_schoolStartDate.get(school.getOid());
            } else {
                if (school.getActiveSchedule() != null) {
                    for (ScheduleTerm term : school.getActiveSchedule().getScheduleTerms()) {
                        for (ScheduleTermDate date : term.getScheduleTermDates()) {
                            if (value == null) {
                                value = date.getStartDate();
                            } else {
                                if (value.after(date.getStartDate())) {
                                    value = date.getStartDate();
                                }
                            }
                        }
                    }
                }
                if (value == null) {
                    value = m_districtStartDate;
                }
            }
            return value;
        }
    }

    /**
     * Performs field validation for the qualification code and exit reason.
     */
    protected class ValidateProgram implements FieldValidator {

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
            WAStudentProgramLep dataProgram = (WAStudentProgramLep) data;
            StudentProgramParticipation program = (StudentProgramParticipation) entity.getBean();
            String parameter = (String) field.getParameter();
            String exitDate = entity.getFieldValue(FIELD_EXIT_DATE);
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            if (PARAM_EXIT_REASON.equals(parameter)) {
                if (StringUtils.isEmpty(value) && !StringUtils.isEmpty(exitDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Exit Reason Required",
                            "An exit reason is required whenever the exit date is populated"));
                }
            } else if (PARAM_PROGRAM_DESIGNATION.equals(parameter) &&
                    VALUE_NATIVE_AMERICAN_PROGRAM_DESIGNATION.equals(value) &&
                    (!isNativeAmerican(dataProgram, program.getStudent()) ||
                            !isEnglishPrimaryLanguage(dataProgram, program.getStudent()))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Program designation for English Speaking Native Americans only",
                        "This student is not Native American or is not English Speaking"));
            }
            return errors;
        }

        /**
         *
         * Performs a test to determine if the student uses English as their primary language.
         *
         * @param data WAStudentProgramLep
         * @param student SisStudent
         * @return true if English is primary
         */
        private boolean isEnglishPrimaryLanguage(WAStudentProgramLep data, SisStudent student) {
            String code = (String) student.getFieldValueByBeanPath(m_fieldPrimaryLanguage);
            if (code == null) {
                return false;
            }
            String stateCode = data.lookupReferenceCodeByBeanPath(SisStudent.class,
                    m_fieldPrimaryLanguage,
                    code,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            return VALUE_ENGLISH_LANG_CODE.equals(stateCode) ? true : false;
        }

        /**
         * Performs a test to determine if the student is Native American.
         *
         * @param data WAStudentProgramLep
         * @param student SisStudent
         * @return true if the student has a Native American ethnic code
         */
        private boolean isNativeAmerican(WAStudentProgramLep data, SisStudent student) {
            Collection<Race> races = student.getPerson().getRaces();
            for (Race race : races) {
                String stateCode = data.lookupReferenceCodeByBeanPath(Race.class,
                        Race.COL_RACE_CODE,
                        race.getRaceCode(),
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                int codeValue;
                try {
                    codeValue = Integer.parseInt(stateCode);
                } catch (NumberFormatException e) {
                    codeValue = 0;
                }
                if (codeValue >= VALUE_MIN_NATIVE_AMERICAN_CODE && codeValue <= VALUE_MAX_NATIVE_AMERICAN_CODE) {
                    return true;
                }
            }

            return false;
        }
    }

    /*
     * Constants: Aliases, Fields, Parameters, Codes.
     */
    protected static final String ALIAS_EXCLUDE_PGM = "DOE EXCLUDE PGM";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_EXIT_REASON = "DOE EXIT REASON";
    protected static final String ALIAS_LEP_GRADE_LEV = "DOE LEP GRADE LEVEL";
    protected static final String ALIAS_LEP_MODEL_CODE = "DOE LEP MODEL CODE";
    protected static final String ALIAS_LEP_PRG_DESIGN = "DOE LEP PRG DESIGNATION";
    protected static final String ALIAS_LEP_TEST_CODE = "DOE LEP TEST CODE";
    protected static final String ALIAS_LEP_TEST_LEVEL = "DOE LEP TEST LEVEL";
    protected static final String ALIAS_LOCATION_OVERRIDE = "DOE LOC OVERRIDE";
    protected static final String ALIAS_NON_PRIMARY = "DOE NON PRIMARY";
    protected static final String ALIAS_PRIMARY_LANGUAGE = "DOE PRIMARY LANGUAGE CODE";

    protected static final String CODE_DID_NOT_QUAL = "O";
    protected static final String CODE_ELL = "ELL";
    protected static final String CODE_NO_SHOW = "NS";

    protected static final String FIELD_EXIT_DATE = "Exit Date";
    protected static final String INPUT_DNQ_START_DATE = "dnqStartDate";

    protected static final String PARAM_EXIT_DATE = "EXIT_DATE";
    protected static final String PARAM_START_DATE = "START_DATE";
    protected static final String PARAM_EXIT_REASON = "EXIT_REASON";
    protected static final String PARAM_LEP_GRADE_CODE = "LEP_GRADE_CODE";
    protected static final String PARAM_LEP_MODEL_CODE = "LEP_MODEL_CODE";
    protected static final String PARAM_LEP_TEST_CODE = "LEP_TEST_CODE";
    protected static final String PARAM_LEP_TEST_LEVEL = "LEP_TEST_LEVEL";
    protected static final String PARAM_PROGRAM_DESIGNATION = "PROGRAM_DESIGNATION";
    protected static final String PARAM_SCHOOL = "SCHOOL";

    protected static final String VALUE_NATIVE_AMERICAN_PROGRAM_DESIGNATION = "3";
    protected static final int VALUE_MIN_NATIVE_AMERICAN_CODE = 400;
    protected static final int VALUE_MAX_NATIVE_AMERICAN_CODE = 499;
    protected static final String VALUE_ENGLISH_LANG_CODE = "639";

    /**
     * Withdrawal codes
     */
    protected static final List<String> WITHD_EQUALS_B = Arrays.asList(new String[] {"G0", "GA", "GB", "C2"});
    protected static final List<String> WITHD_EQUALS_C =
            Arrays.asList(new String[] {"C1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D0"});
    protected static final List<String> WITHD_EQUALS_D = Arrays.asList(new String[] {"U1", "U2", "U3"});
    protected static final List<String> WITHD_EQUALS_E = Arrays.asList(new String[] {"T0", "T2", "T3"});
    protected static final List<String> WITHD_EQUALS_I = Arrays.asList(new String[] {"D1"});
    protected static final List<String> WITHD_EQUALS_T = Arrays.asList(new String[] {"T1"});

    /*
     * Instance variables.
     */
    protected Map m_excludeSchool;
    protected Map<String, String> m_exitReasonCodesMap;
    protected String m_fieldExcludePgm;
    protected String m_fieldExitReason;
    protected String m_fieldLepGradeCode;
    protected String m_fieldLepModelCode;
    protected String m_fieldLepPgmDesignCode;
    protected String m_fieldLepTestCode;
    protected String m_fieldLepTestLevel;
    protected String m_fieldLocationOverride;
    protected String m_fieldNonPrimaryIndicator;
    protected String m_fieldPrimaryLanguage;
    protected StudentHistoryHelper m_helper;
    protected Map<String, String> m_lepGradeCodes;
    protected Map<String, String> m_lepModelCodes;
    protected Map<String, String> m_lepPgmDesignCodes;
    protected Map<String, String> m_lepTestCodes;
    protected Map<String, String> m_lepTestLevels;
    protected String m_refTableEnrollmentCode;
    protected String m_refTableWithdrawalCode;
    protected PlainDate m_reportDate;


    /**
     * Initialize the data module.
     * Initialize program map.
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
            BeanQuery query = new BeanQuery(StudentProgramParticipation.class, programCriteria);
            applyInputSort(query, StudentProgramParticipation.REL_STUDENT);
            setQuery(query);

            setEntityClass(WAStudentProgramLepEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("PGM-INFO", new RetrieveProgram(getOrganization().getCurrentContext().getStartDate()));
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldValidator>();
            validators.put("PGM-VALIDATE", new ValidateProgram());
            addValidators(validators);
        }
    }

    /**
     * Loads field aliases.
     */
    private void initializeFields() {
        m_refTableEnrollmentCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.ENROLLMENT_ENTRY_CODES);
        m_refTableWithdrawalCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);

        m_fieldPrimaryLanguage = translateAliasToJavaName(ALIAS_PRIMARY_LANGUAGE, true);
        m_fieldNonPrimaryIndicator = translateAliasToJavaName(ALIAS_NON_PRIMARY, true);
        m_fieldExitReason = translateAliasToJavaName(ALIAS_EXIT_REASON, true);
        m_fieldExcludePgm = translateAliasToJavaName(ALIAS_EXCLUDE_PGM, false);
        m_fieldLocationOverride = translateAliasToJavaName(ALIAS_LOCATION_OVERRIDE, true);

        // Lookup a reverse map of ELL exit reasons state code to user code.
        m_exitReasonCodesMap = new HashMap<String, String>();
        m_lepTestCodes = new HashMap<String, String>();
        m_lepTestLevels = new HashMap<String, String>();
        m_lepModelCodes = new HashMap<String, String>();
        m_lepPgmDesignCodes = new HashMap<String, String>();
        m_lepGradeCodes = new HashMap<String, String>();

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

        m_fieldLepTestCode = translateAliasToJavaName(ALIAS_LEP_TEST_CODE, false);
        m_fieldLepTestLevel = translateAliasToJavaName(ALIAS_LEP_TEST_LEVEL, false);
        m_fieldLepModelCode = translateAliasToJavaName(ALIAS_LEP_MODEL_CODE, true);
        m_fieldLepPgmDesignCode = translateAliasToJavaName(ALIAS_LEP_PRG_DESIGN, true);
        m_fieldLepGradeCode = translateAliasToJavaName(ALIAS_LEP_GRADE_LEV, true);

        m_lepTestCodes.putAll(getAllCodesMapByBeanPath(m_fieldLepTestCode));
        m_lepTestLevels.putAll(getAllCodesMapByBeanPath(m_fieldLepTestLevel));
        m_lepModelCodes.putAll(getAllCodesMapByBeanPath(m_fieldLepModelCode));
        m_lepPgmDesignCodes.putAll(getAllCodesMapByBeanPath(m_fieldLepPgmDesignCode));
        m_lepGradeCodes.putAll(getAllCodesMapByBeanPath(m_fieldLepGradeCode));

    }

    /**
     * Populate map of all possible codes for the given bean path.<br>
     * Map is keyed on code and valued on stateCode.
     *
     * @param beanPath String
     * @return Map
     */
    private Map<String, String> getAllCodesMapByBeanPath(String beanPath) {
        Map<String, String> codesMap = new HashMap<String, String>();
        if (!StringUtils.isEmpty(beanPath)) {
            DataDictionaryField lepTestField = getDataDictionaryField(StudentProgramParticipation.class, beanPath);
            if (lepTestField != null) {
                ReferenceTable refLepTable = lepTestField.getReferenceTable();

                if (refLepTable != null) {
                    X2Criteria refCodesCriteria = new X2Criteria();
                    refCodesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, refLepTable.getOid());

                    QueryByCriteria refCodeQuery = new QueryByCriteria(ReferenceCode.class, refCodesCriteria);
                    QueryIterator refCodeIter = getBroker().getIteratorByQuery(refCodeQuery);

                    while (refCodeIter.hasNext()) {
                        ReferenceCode refCode = (ReferenceCode) refCodeIter.next();
                        codesMap.put(refCode.getCode(), refCode.getStateCode());
                    }
                }
            }
        }
        return codesMap;
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
        Collection<String> reportableCodes = null;
        if (!StringUtils.isEmpty(referenceTableOid)) {
            criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, CODE_ELL);
            SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
            reportableCodes = getBroker().getSubQueryCollectionByQuery(query);
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
            criteria.addNotEqualTo(m_fieldExitReason, m_exitReasonCodesMap.get(CODE_DID_NOT_QUAL));

            PlainDate dnqStartDate = (PlainDate) getParameter(INPUT_DNQ_START_DATE);
            X2Criteria dnqCriteria = new X2Criteria();
            dnqCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE, dnqStartDate);
            dnqCriteria.addEqualTo(m_fieldExitReason, m_exitReasonCodesMap.get(CODE_DID_NOT_QUAL));

            X2Criteria criteria2 = new X2Criteria();
            X2Criteria criteria3 = new X2Criteria();
            criteria2.addIsNull(StudentProgramParticipation.COL_END_DATE);
            criteria3.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE,
                    getOrganization().getCurrentContext().getStartDate());
            criteria2.addOrCriteria(criteria3);
            criteria.addAndCriteria(criteria2);

            criteria.addOrCriteria(dnqCriteria);

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
            addSetupError("No reportable program codes", "Student Programs, reference table, local code, ELL");
        }

        return criteria;
    }

    /**
     * Loads schools with excluded indicator set to true.
     */
    private void loadSchoolExcludeMap() {

        String schoolExclude = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(schoolExclude, BooleanAsStringConverter.TRUE);
        BeanQuery query = new BeanQuery(School.class, schoolCriteria);
        m_excludeSchool = getBroker().getGroupedCollectionByQuery(query, X2BaseBean.COL_OID, 128);
    }

    /**
     * Include school.
     *
     * @param schoolOid String
     * @return true, if successful
     */
    public boolean includeSchool(String schoolOid) {
        return (m_excludeSchool == null) || !m_excludeSchool.containsKey(schoolOid);
    }

}
