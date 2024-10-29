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
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export procedure for Washington State CEDARS
 * Student Special Education Programs file.
 *
 * @author X2 Development Corporation
 */
public class WAStudentProgramIep extends StateReportData {
    /**
     * Entity record for the Student Program IEP export.
     */
    public static class WAStudentProgramIepEntity extends StateReportEntity {
        /**
         * List of valid special ed programs this student have
         */
        private ArrayList<WAStudentProgramIepRecord> m_validPgms;

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            String name = null;

            StudentProgramParticipation program = getCurrentProgram();
            if (program != null) {
                SisStudent student = program.getStudent();

                name = student.getNameView() +
                        " [LASID: " + student.getLocalId() +
                        ", SASID: " + student.getStateId() +
                        "] " + program.getProgramCode();
            }

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

            WAStudentProgramIep spData = (WAStudentProgramIep) data;
            SisStudent student = (SisStudent) bean;

            // collect all the valid special ed programs
            Collection<StudentProgramParticipation> pgms = spData.m_stdPgmsMap.get(student.getOid());
            pgms = collectValidSpedPrograms(pgms);
            m_validPgms = new ArrayList<WAStudentProgramIepRecord>();

            // if there are any valid ones, we'll use the most recent one
            if (pgms != null && pgms.size() > 0) {
                List<StudentEnrollmentSpan> enrollmentSpans = spData.m_helper.getStudentEnrollmentSpans(student, true);
                // Do not include students in the export that only have an enrEnrStatus "PreReg" for
                // the school year.

                boolean include = false;
                if (!enrollmentSpans.isEmpty()) {
                    for (StudentEnrollmentSpan span : enrollmentSpans) {
                        if (!span.getFirstActiveEnrollment().getStatusCode().equals("PreReg")) {
                            include = true;
                        }
                    }
                } else {
                    StudentEnrollment enrollment =
                            spData.m_helper.getEnrollmentForDate(student.getOid(),
                                    spData.getCurrentContext().getStartDate(), "EWSY");
                    if (enrollment != null && !"PreReg".equals(enrollment.getStatusCode())) {
                        include = true;
                    }
                }

                if (!include) {
                    setRowCount(0);
                    return;
                }

                List<DistrictEnrollmentSpan> districtSpans = getDistrictSpans(student, spData);
                // We check each program to make sure it is acceptable, then find the enrollment
                // span that matches and pair them up
                for (StudentProgramParticipation program : pgms) {
                    PlainDate endDate = program.getEndDate();
                    // Current year or no end date
                    if (endDate == null ||
                            (program != null && endDate != null
                                    && !endDate.before(getData().getCurrentContext().getStartDate()))) {
                        WAStudentProgramIepRecord record = new WAStudentProgramIepRecord();
                        record.m_iepProgram = program;

                        for (DistrictEnrollmentSpan dSpan : districtSpans) {
                            PlainDate startDate = program.getStartDate();
                            // entry date for my span is not after start date of program
                            if ((dSpan.m_entryEnrollment.getEnrollmentDate().after(startDate)) &&
                            // program doesn't have an end date
                                    (program.getEndDate() == null ||
                                    // null checks for next question
                                            (dSpan.m_entryEnrollment.getEnrollmentDate() != null
                                                    && program.getEndDate() != null &&
                                                    // or the entry enrollment date is not after the
                                                    // program end date
                                                    !dSpan.m_entryEnrollment.getEnrollmentDate()
                                                            .after(program.getEndDate())))) {
                                // No exit or no exit date
                                if (dSpan.m_exitEnrollment == null || dSpan.m_exitEnrollment.getEnrollmentDate() == null
                                        ||
                                        // null checks for next question
                                        (dSpan.m_exitEnrollment.getEnrollmentDate() != null
                                                && program.getStartDate() != null &&
                                                // the span exit date is not before the program
                                                // start date
                                                !dSpan.m_exitEnrollment.getEnrollmentDate()
                                                        .before(program.getStartDate()))) {
                                    // Set the span, this means that there is an encompassing
                                    // enrollment span around the
                                    // program. This is used for later validation
                                    record.m_districtSpan = dSpan;
                                    record.m_exitDate = null;
                                    record.m_exitReason = null;
                                    // If during IEP span the student leaves the district, provide
                                    // the �6� exit reason
                                    PlainDate districtExitDate = dSpan.m_exitDate;
                                    if (districtExitDate != null) {
                                        if (program.getEndDate() == null
                                                || districtExitDate.before(program.getEndDate())) {
                                            record.m_exitReason = PGM_EXIT_REASON_6;
                                            record.m_exitDate = dSpan.m_exitDate;
                                        }
                                    }
                                }
                            }
                        }

                        // Stop matching when any rule returns a result
                        // Rule 1 - find school based on enrolled school when program started.
                        if (program.getStartDate() != null) {
                            record.m_school = withinSpans(program.getStartDate(), enrollmentSpans);
                        }

                        // Rule 2 - find the school based on enrolled school when program ended.
                        if (record.m_school == null && program.getEndDate() != null) {
                            record.m_school = withinSpans(program.getStartDate(), enrollmentSpans);
                        }

                        // Rule 3 - find the school based on the most current enrollment
                        if (record.m_school == null) {
                            for (StudentEnrollmentSpan span : enrollmentSpans) {
                                if (span.getSchool() != null) {
                                    StudentEnrollment enrollment = getLastActiveEnrollment(span);
                                    record.m_school = (enrollment != null && enrollment.getSchool() != null)
                                            ? enrollment.getSchool()
                                            : span.getSchool();
                                }
                            }
                        }

                        // If we didn't find the school for the student instead use the students
                        // current school
                        if (record.m_school == null) {
                            record.m_school = student.getSchool();
                        }

                        // Don't include the record if it comes from an excluded school
                        if (spData.includeSchool(record.m_school.getOid())) {
                            m_validPgms.add(record);
                        }
                    }
                }
            }
            setRowCount(m_validPgms.size());
        }

        /**
         * Data container object for each row in export.
         */
        protected static class WAStudentProgramIepRecord {
            public StudentProgramParticipation m_iepProgram;
            public DistrictEnrollmentSpan m_districtSpan;
            public SisSchool m_school;
            public PlainDate m_exitDate;
            public String m_exitReason;
            public String m_exitEnrollmentCode;
        }

        /**
         * Setup the valid special ed programs into a list
         * <p>
         * I expect <code>pgms</code> to be the student's special ed programs sorted by DESCENDING
         * date order, i.e. it
         * is from most-recent to least-recent (newest to oldest)
         * <p>
         * Valid means the exit reason != "Ineligible" or disability != "00"
         *
         * @param pgms Collection<StudentProgramParticipation>
         * @return ArrayList
         */
        private ArrayList<StudentProgramParticipation> collectValidSpedPrograms(Collection<StudentProgramParticipation> pgms) {
            ArrayList<StudentProgramParticipation> validPgms = new ArrayList<StudentProgramParticipation>();

            if (pgms != null && pgms.size() > 0) {
                for (StudentProgramParticipation pgm : pgms) {
                    String pgmExitReason = (String) pgm.getFieldValueByAlias(ALIAS_EXIT_REASON);
                    String pgmDisability = (String) pgm.getFieldValueByAlias(ALIAS_DISABILITY);

                    // convert to state code and pad left (length 2) with 0's
                    if (pgmDisability != null) {
                        int stateCodeRefTypeOrdinal = ExportFormatField.ReferenceMapTypeCode.STATE.ordinal();
                        String pgmDisabilityStateCode =
                                getData().lookupReferenceCodeByAlias(ALIAS_DISABILITY, pgmDisability,
                                        stateCodeRefTypeOrdinal);
                        pgmDisability = StringUtils.padLeft(pgmDisabilityStateCode, 2, '0');
                    }

                    if (!(PGM_EXIT_REASON_INEL.equals(pgmExitReason) && PGM_DISABILITY_CODE.equals(pgmDisability))) {
                        validPgms.add(pgm);
                    }
                }
            }

            return validPgms;
        }

        /**
         * Get the most recent valid special ed program
         * <p>
         * This just gets the first record in the list of valid special ed programs
         * <p>
         * i.e., the list is ordered from most recent to earliest
         *
         * @return Student program participation
         */
        public StudentProgramParticipation getCurrentProgram() {
            StudentProgramParticipation recentPgm = m_validPgms.get(getCurrentRow()).m_iepProgram;
            PlainDate endDate = recentPgm.getEndDate();
            if (endDate != null && endDate.before(getData().getCurrentContext().getStartDate())) {
                recentPgm = null;
            }
            return recentPgm;
        }

        /**
         * Get the most recent valid special ed program
         * <p>
         * This just gets the first record in the list of valid special ed programs
         * <p>
         * i.e., the list is ordered from most recent to earliest
         *
         * @return Sis school
         */
        public SisSchool getCurrentSchool() {
            return m_validPgms.get(getCurrentRow()).m_school;
        }

        /**
         * Get the most recent valid special ed program
         * <p>
         * This just gets the first record in the list of valid special ed programs
         * <p>
         * i.e., the list is ordered from most recent to earliest
         *
         * @return Student enrollment
         */
        public StudentEnrollment getCurrentEntryEnrollment() {
            DistrictEnrollmentSpan districtSpan = m_validPgms.get(getCurrentRow()).m_districtSpan;
            return (districtSpan != null) ? districtSpan.m_entryEnrollment : null;
        }

        /**
         * Get the most earliest valid special ed program
         * <p>
         * This just gets the last record in the list of valid special ed programs
         * <p>
         * i.e., the list is ordered from most recent to earliest
         *
         * @return Student program participation
         */
        protected StudentProgramParticipation getEarliestValidPgm() {
            return m_validPgms.get(m_validPgms.size() - 1).m_iepProgram;
        }

        /**
         * Return the calculated exit date. This is the latest exit date for all enrollment spans,
         * or null if a span is
         * still open (active).
         *
         * @return PlainDate
         */
        protected PlainDate getExitDate() {
            return m_validPgms.get(getCurrentRow()).m_exitDate;
        }

        /**
         * Return the calculated exit reason. This is from the latest exit enrollment span
         * withdrawal code.
         *
         * @return String
         */
        protected String getExitReason() {
            return m_validPgms.get(getCurrentRow()).m_exitReason;
        }

        /**
         * Return the enrollment code for the student's supposedly exit
         * <p>
         * Used for determining if the student was graduated.
         *
         * @return String
         */
        protected String getExitEnrollmentCode() {
            return m_validPgms.get(getCurrentRow()).m_exitEnrollmentCode;
        }

        /**
         * Return the school associated with the specified program.
         *
         * @return SisSchool
         */
        protected SisSchool getSchool() {
            return m_validPgms.get(getCurrentRow()).m_school;
        }

        /**
         * Calculate district enrollment spans from the student school enrollment spans. Look for
         * spans with withdrawal
         * codes that represent district withdrawal vs. in district transfer.
         *
         * @param student Student
         * @param sdData WAStudentProgramIep
         * @return List<DistrictEnrollmentSpan>
         */
        protected List<DistrictEnrollmentSpan> getDistrictSpans(Student student, WAStudentProgramIep sdData) {
            List<StudentEnrollmentSpan> enrollmentSpans = sdData.m_helper.getStudentEnrollmentSpans(student, false);
            List<DistrictEnrollmentSpan> districtSpans = new ArrayList<DistrictEnrollmentSpan>();
            DistrictEnrollmentSpan currentSpan = null;
            for (StudentEnrollmentSpan span : enrollmentSpans) {
                /*
                 * Check if the span is a no-show span. Do not include no-show spans. A no-show span
                 * represents an
                 * enrollment where the student never showed up. It is identified by a withdrawal
                 * code that has NS in
                 * the local code of the reference table.
                 */
                boolean doNotShow = false;
                StudentEnrollment enrollment = span.getFirstInactiveEnrollment();
                if (enrollment != null) {
                    String withdrawalCode = enrollment.getEnrollmentCode();
                    withdrawalCode = sdData.lookupReferenceCodeByRefTbl(sdData.m_refTableWithdrawalCode, withdrawalCode,
                            ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                    if (CODE_NO_SHOW.equals(withdrawalCode)) {
                        doNotShow = true;
                    }
                }

                /*
                 * Check if the enrollment span is non-primary. If so, do not include.
                 */
                enrollment = span.getFirstActiveEnrollment();
                if (enrollment != null) {
                    String nonPrimary = (String) enrollment.getFieldValueByBeanPath(sdData.m_fieldNonPrimaryIndicator);
                    if (BooleanAsStringConverter.TRUE.equals(nonPrimary)) {
                        doNotShow = true;
                    }
                }

                if (!doNotShow) {
                    // Check the span for entry type (internal or external)
                    enrollment = span.getFirstActiveEnrollment();
                    if (enrollment != null) {
                        String code = enrollment.getEnrollmentCode();
                        String stateCode = sdData.lookupReferenceCodeByRefTbl(sdData.m_refTableEnrollmentCode, code,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                        if (ENR_STATE_CODE_1.equals(stateCode) || ENR_STATE_CODE_2.equals(stateCode)) {
                            if (currentSpan != null &&
                                    (currentSpan.m_exitDate == null ||
                                            !sdData.getCurrentContext().getStartDate().after(currentSpan.m_exitDate))) {
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
                            !sdData.getCurrentContext().getStartDate().after(currentSpan.m_exitDate))) {
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
     * district. This will
     * encompass one or more enrollment spans.
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
     * <dl>
     *
     * <dt>SCHOOL</dt>
     * <dd>The school id (LocationId)</dd>
     *
     * <dt>LRE_CODE</dt>
     * <dd>Student's most RECENT valid special ed program's ALIAS_LRE_CODE</dd>
     *
     * <dt>START_DATE</dt>
     * <dd>Student's most RECENT valid special ed program's ALIAS_SPED_START_DATE.
     * If that's empty, then use Student's EARLIEST valid special ed program's PGM_START_DATE</dd>
     *
     * <dt>EXIT_DATE</dt>
     * <dd>1. Student's most RECENT valid special ed program's ALIAS_SPED_EXIT_DATE.
     * 2. If that's empty... check the student's withdrawn Enrollment record...
     * a. If that withdrawn enrollment record's enrollment code is a graduating state code (i.e,
     * "G0"), then use that enrollment date.
     * b. If that withdrawn enrollment record's enrollment code is not a graduate state code (i.e.,
     * anything not "G0"), then leave empty.</dd>
     *
     * <dt>EXIT_REASON</dt>
     * <dd>1. Check if the student withdrew with a graduating state code, "G0". If so, use "3".
     * 2. If not, use the most RECENT valid special ed program's ALIAS_EXIT_REASON.</dd>
     *
     * </dl>
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
            WAStudentProgramIep spData = (WAStudentProgramIep) data;
            WAStudentProgramIepEntity spEntity = (WAStudentProgramIepEntity) entity;
            SisSchool school = spEntity.getSchool();
            String parameter = (String) field.getParameter();
            StudentProgramParticipation program = spEntity.getCurrentProgram();
            SisStudent student = program.getStudent();

            Object value = null;

            if (PARAM_LRE_CODE.equals(parameter)) {
                // return LRE code state code
                value = program.getFieldValueByAlias(ALIAS_LRE_CODE);
                if (value != null && !value.toString().isEmpty()) {
                    value = lookupStateValue(StudentProgramParticipation.class, m_javaNameLreCode, value.toString());
                }
            } else if (PARAM_START_DATE.equals(parameter)) {
                PlainDate startDate = null;

                // use sped start date
                if (spData.m_fieldStartDate != null) {
                    value = program.getFieldValueByBeanPath(spData.m_fieldStartDate);
                    if (value != null) {
                        startDate = DateUtils.getDate(value.toString());
                    }
                }

                // only looked for the sped start date, not the program start date so defaulting to
                // earliest program
                // start date
                if (startDate == null) {
                    startDate = program.getStartDate();
                }

                // if value is still empty, use the earliest valid special ed program and use
                // pgm_start_date instead
                if (startDate == null) {
                    program = spEntity.getEarliestValidPgm();
                    startDate = program.getStartDate();
                }

                // if value is still empty, or the student start date is before enrollment start
                // date
                StudentEnrollment enrollment = spEntity.getCurrentEntryEnrollment();
                if (enrollment != null) {
                    PlainDate enrollmentDate = enrollment.getEnrollmentDate();
                    if (startDate == null) {
                        startDate = enrollmentDate;
                    }
                }

                // if value is still empty, or the student start date is before school start date
                Schedule schoolSched = spEntity.getSchool().getActiveSchedule();
                if (schoolSched != null) {
                    PlainDate schoolStartDate = schoolSched.getStartDate();
                    if (startDate == null) {
                        startDate = schoolStartDate;
                    }
                }

                // if value is still empty, or the student start date is before district start date
                PlainDate districtStartDate = data.getCurrentContext().getStartDate();
                if (districtStartDate != null) {
                    if (startDate == null) {
                        startDate = districtStartDate;
                    }
                }

                value = startDate;
            } else if (PARAM_EXIT_DATE.equals(parameter)) {
                PlainDate exitDate = null;
                if (spData.m_fieldExitDate != null) {
                    value = spData.getPropertyAsJavaType(program, spData.m_fieldExitDate);
                } else if (program != null) {
                    exitDate = program.getEndDate();
                }

                if (exitDate == null) {
                    exitDate = spEntity.getExitDate();
                }
                value = exitDate;
            } else if (PARAM_SCHOOL.equals(parameter)) {
                StudentEnrollment enrollment = null;
                List<StudentEnrollmentSpan> spans = spData.m_helper.getStudentEnrollmentSpans(student, true);
                for (StudentEnrollmentSpan span : spans) {
                    enrollment = span.getFirstActiveEnrollment();
                }
                if (enrollment != null) {
                    value = enrollment.getFieldValueByBeanPath(m_fieldLocationOverride);
                }
                if (value == null) {
                    value = school.getSchoolId();
                }
            } else if (PARAM_EXIT_REASON.equals(parameter)) {
                // if during IEP span the student leaves the district, provide the �6� exit
                // reason
                value = spEntity.getExitReason();

                // get whatever is in the program's exit reason field
                if (value == null || value.toString().isEmpty()) {
                    value = program.getFieldValueByAlias(ALIAS_EXIT_REASON);
                    if (value != null && !value.toString().isEmpty()) {
                        value = lookupStateValue(StudentProgramParticipation.class, m_javaNameExitReason,
                                value.toString());
                    }
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
            WAStudentProgramIepEntity spEntity = (WAStudentProgramIepEntity) entity;
            StudentProgramParticipation program = spEntity.getCurrentProgram();
            String parameter = (String) field.getParameter();
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            if (PARAM_EXIT_REASON.equals(parameter)) {
                if (StringUtils.isEmpty(value)) {
                    if (program.getEndDate() != null) {
                        errors.add(new StateReportValidationError(entity, field,
                                "Exit Reason Required",
                                "An exit reason is required whenever the exit date is populated"));
                    }
                }
            }
            if (spEntity.getCurrentEntryEnrollment() == null) {
                String errorMessage = "No matching enrollment data for program.";
                if (program.getStartDate() != null) {
                    errorMessage += " Program Start: " + program.getStartDate();
                }
                if (program.getEndDate() != null) {
                    errorMessage += " Program End: " + program.getEndDate();
                }
                errors.add(new StateReportValidationError(entity.toString(), "Program Details", "Invalid Program",
                        errorMessage));
            }
            return errors;
        }
    }

    protected static final String ALIAS_EXIT_REASON = "DOE EXIT REASON";
    protected static final String ALIAS_DISABILITY = "DOE DISABILITY";
    protected static final String ALIAS_LRE_CODE = "DOE LRE CODE";
    protected static final String ALIAS_LOCATION_OVERRIDE = "DOE LOC OVERRIDE";
    protected static final String ALIAS_NON_PRIMARY = "DOE NON PRIMARY";
    protected static final String ALIAS_EXCLUDE_PGM = "DOE EXCLUDE PGM";
    protected static final String ALIAS_SPED_START_DATE = "DOE SPED START DATE";
    protected static final String ALIAS_SPED_EXIT_DATE = "DOE SPED EXIT DATE";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";

    /*
     * Constants: Parameters, Codes.
     */
    protected static final String CODE_GRADUATE_ASSOCIATES_DEGREE = "GA";
    protected static final String CODE_GRADUATE_HS_DEGREE = "G0";
    protected static final String CODE_NEW_TO_DISTRICT = "1";
    protected static final String CODE_NO_SHOW = "NS";
    protected static final String CODE_RETURN_TO_DISTRICT = "2";
    protected static final String CODE_SPED = "SPED";
    protected static final String ENR_STATE_CODE_1 = "1";
    protected static final String ENR_STATE_CODE_2 = "2";
    protected static final String FIELD_EXIT_DATE = "Exit Date";
    protected static final String FIELD_EXIT_REASON_CODE = "Exit Reason Code";
    protected static final String PARAM_EXIT_DATE = "EXIT_DATE";
    protected static final String PARAM_EXIT_REASON = "EXIT_REASON";
    protected static final String PARAM_SCHOOL = "SCHOOL";
    protected static final String PARAM_LRE_CODE = "LRE_CODE";
    protected static final String PARAM_START_DATE = "START_DATE";
    protected static final String PGM_CODE_36 = "36";
    protected static final String PGM_DISABILITY_CODE = "00";
    protected static final String PGM_EXIT_REASON_6 = "6";
    protected static final String PGM_EXIT_REASON_INEL = "Ineligible";



    /*
     * Instance variables.
     */
    protected Map<String, String> m_exitReasonCodesMap;
    protected String m_fieldNonPrimaryIndicator;
    protected String m_fieldExcludePgm;
    protected String m_fieldExitDate;
    protected String m_fieldExitReason;
    protected String m_fieldStartDate;
    protected StudentHistoryHelper m_helper;
    protected String m_refTableEnrollmentCode;
    protected String m_refTableWithdrawalCode;
    protected PlainDate m_reportDate;
    protected Map<String, Collection<StudentProgramParticipation>> m_stdPgmsMap;
    protected String m_javaNameExitReason;
    protected String m_javaNameDisability;
    protected String m_javaNameLreCode;
    protected Map m_excludeSchool;
    protected String m_fieldLocationOverride;

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
        m_javaNameDisability = translateAliasToJavaName(ALIAS_DISABILITY, true);
        m_javaNameExitReason = translateAliasToJavaName(ALIAS_EXIT_REASON, true);
        m_javaNameLreCode = translateAliasToJavaName(ALIAS_LRE_CODE, true);
        m_fieldLocationOverride = translateAliasToJavaName(ALIAS_LOCATION_OVERRIDE, true);

        if (getSetupErrors().size() == 0) {
            // Setup helper class and set the query
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            QueryByCriteria studentQuery = m_helper.getStudentQuery(false);
            setQuery(studentQuery);
            setEntityClass(WAStudentProgramIepEntity.class);

            // Create program selection criteria and setup std-oid -> iep programs map
            X2Criteria programCriteria = getProgramCriteria(m_helper.getStudentCriteria());
            BeanQuery query = new BeanQuery(StudentProgramParticipation.class, programCriteria);
            query.addOrderBy(StudentProgramParticipation.COL_START_DATE, false);
            m_stdPgmsMap =
                    getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 128);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("PGM-INFO", new RetrieveProgram());
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldValidator>();
            validators.put("PGM-VALIDATE", new ValidateProgram());
            super.addValidators(validators);
        }
    }

    /**
     * Returns the Student Programs Participation selection criteria.
     *
     * @param studentCriteria X2Criteria
     * @return X2Criteria
     */
    private X2Criteria getProgramCriteria(X2Criteria studentCriteria) {
        X2Criteria criteria = null;
        DataDictionaryField field =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        String referenceTableOid = field.getReferenceTableOid();
        Collection<String> reportableCodes = null;
        if (!StringUtils.isEmpty(referenceTableOid)) {
            criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            criteria.addEqualTo(ReferenceCode.COL_LOCAL_CODE, CODE_SPED);
            SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
            reportableCodes = getBroker().getSubQueryCollectionByQuery(query);
        }

        if (reportableCodes != null && reportableCodes.size() > 0) {
            // Student subquery for active students this year.
            SubQuery studentSubquery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

            criteria = new X2Criteria();
            criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubquery);
            criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, reportableCodes);
            criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDate);

            if (isSchoolContext()) {
                criteria.addEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + Student.COL_SCHOOL_OID,
                        getSchool().getOid());
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
            addSetupError("No reportable program codes", "Student Programs, reference table, local code, SPED");
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

        m_fieldExitReason = translateAliasToJavaName(ALIAS_EXIT_REASON, true);
        m_fieldExitDate = translateAliasToJavaName(ALIAS_SPED_EXIT_DATE, false);
        m_fieldStartDate = translateAliasToJavaName(ALIAS_SPED_START_DATE, false);

        m_fieldNonPrimaryIndicator = translateAliasToJavaName(ALIAS_NON_PRIMARY, true);
        m_fieldExcludePgm = translateAliasToJavaName(ALIAS_EXCLUDE_PGM, false);

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
     * from reporting on
     * school table is selected)
     */
    private void loadSchoolExcludeMap() {
        String schoolExclude = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(schoolExclude, BooleanAsStringConverter.TRUE);
        BeanQuery query = new BeanQuery(School.class, schoolCriteria);
        m_excludeSchool = getBroker().getGroupedCollectionByQuery(query, X2BaseBean.COL_OID, 128);
    }

    /**
     * Checks if schoolOid given is a school to exclude. If that school is excluded this method will
     * return false ie -
     * !included.
     *
     * @param schoolOid String
     * @return boolean
     */
    public boolean includeSchool(String schoolOid) {
        return (m_excludeSchool == null) || !m_excludeSchool.containsKey(schoolOid);
    }
}
