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
package com.x2dev.procedures.statereporting.nj;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New Jersey state procedure for CTEStudent export.
 *
 * @author X2 Development Corporation
 */

public class CTEStudent extends StateReportData {
    /**
     * Entity class for CTE Student Level export.
     *
     * @author X2 Development Corporation
     */

    public static class CTEStudentEntity extends StateReportEntity {
        /**
         * Entity instance variables.
         */
        SisStudent m_student;
        List<StudentEnrollmentSpan> m_enrollmentSpans = new ArrayList<StudentEnrollmentSpan>();
        Map<String, String> countyDistrictSchoolCodeMap = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */

        public CTEStudentEntity() {
            // no argument constructor
        }

        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_student = (SisStudent) bean;
            CTEStudent cteData = (CTEStudent) data;

            StudentEnrollment withdrawnEnr = null;
            StudentEnrollment enroll =
                    cteData.m_helper.getEnrollmentForDate(m_student.getOid(), cteData.m_reportDate, "WE");

            if (enroll.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                withdrawnEnr = enroll;
            }
            if (withdrawnEnr == null || (withdrawnEnr != null && ("Graduate".equals(withdrawnEnr.getStatusCode()))
                    && cteData.m_reportingSklYear == m_student.getYog())) {
                String sameDistrictCodes = null;
                if (cteData.m_sameDistrictCodes != null) {
                    School school = m_student.getSchool();
                    sameDistrictCodes = (String) school.getFieldValueByBeanPath(cteData.m_sameDistrictCodes);
                }

                // Selecting only the enrollment spans that are after school's start date and those
                // that belong
                // to the related school districts only
                List<StudentEnrollmentSpan> spans =
                        cteData.getStudentHistoryHelper().getStudentEnrollmentSpans(m_student, true);
                for (StudentEnrollmentSpan span : spans) {
                    if (sameDistrictCodes != null) {
                        StudentEnrollment activeEnrollment = span.getFirstActiveEnrollment();
                        String countyDistrictSchoolCode =
                                (String) activeEnrollment.getFieldValueByBeanPath(cteData.m_fieldResidingSchool);
                        countyDistrictSchoolCode = cteData.lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                                cteData.m_fieldResidingSchool,
                                countyDistrictSchoolCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        countyDistrictSchoolCodeMap = cteData.getCountyDistrictSchoolCode(countyDistrictSchoolCode);
                        if (countyDistrictSchoolCodeMap != null
                                && countyDistrictSchoolCodeMap.containsKey(KEY_DISTRICT_CODE)) {
                            String residingDistrictCode = countyDistrictSchoolCodeMap.get(KEY_DISTRICT_CODE);
                            if (residingDistrictCode == null || !sameDistrictCodes.contains(residingDistrictCode)) {
                                continue;
                            }
                        }
                    }
                    if ((cteData.getStartDate() != null && !span.getFirstActiveDate().before(cteData.getStartDate()))
                            || span.getLastActiveDate() == null
                            || (cteData.getStartDate() != null
                                    && span.getLastActiveDate().after(cteData.getStartDate()))) {
                        m_enrollmentSpans.add(span);
                    }

                }
                if (m_enrollmentSpans.size() == 0) {
                    setRowCount(0);
                }
            } else {
                setRowCount(0);
            }

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

            return name;
        }

        /**
         * Returns the list of enrollment spans for a student.
         *
         * @return List<StudentEnrollmentSpan>
         */
        public List<StudentEnrollmentSpan> getEnrollmentSpans() {
            return m_enrollmentSpans;
        }

        /**
         * Returns the Student.
         *
         * @return SisStudent
         */
        public SisStudent getStudent() {
            return m_student;
        }
    }

    /**
     * Retrieves the Enrollment records and calculates the membership days, truant
     * and present days.
     */
    protected class RetrieveEnrollment implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            List<StudentEnrollmentSpan> enrollmentSpans = ((CTEStudentEntity) entity).getEnrollmentSpans();
            String param = (String) field.getParameter();

            if (PARAM_DAYS_MEMBERSHIP.equals(param)) {
                int totalMembershipDays = 0;
                for (StudentEnrollmentSpan span : enrollmentSpans) {
                    List<StudentAttendance> attendances = span.getStudentAttendance();
                    int excusedAbsentDays = 0;
                    for (StudentAttendance attendance : attendances) {
                        // Hunterdon doesn't follow the general rule for calculating the
                        // membership days. Hunterdon checks the reason for absent.
                        // If it is one of the below reasons, then that day is not counted
                        // towards actual membership days.
                        // State Code Description
                        // 3 Excused Absence - Religious Holiday or Bring Your Child to Work Day
                        // 7 Home Instruction
                        String reason = attendance.getReasonCode();
                        reason = lookupReferenceCodeByBeanPath(StudentAttendance.class,
                                StudentAttendance.COL_REASON_CODE, reason,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        if (attendance.getAbsentIndicator() && (ABSENT_REASON_EXCUSED_ABSENCE_STATE_CODE.equals(reason)
                                || ABSENT_REASON_HOME_INSTRUCTION_STATE_CODE.equals(reason))) {
                            excusedAbsentDays++;
                        }
                    }
                    totalMembershipDays = totalMembershipDays + span.getMembershipDays() - excusedAbsentDays;
                }
                value = Integer.valueOf(totalMembershipDays);
            } else if (PARAM_DAYS_TRUANT.equals(param)) {
                int truantDays = 0;
                for (StudentEnrollmentSpan span : enrollmentSpans) {
                    List<StudentAttendance> attendances = span.getStudentAttendance();
                    for (StudentAttendance attendance : attendances) {
                        // Truant days are the number of days the student is not excused.
                        /*
                         * if (!attendance.getExcusedIndicator())
                         * {
                         * truantDays++;
                         * }
                         */
                        // Hunterdon doesn't follow the above rule. They have a reason
                        // "Truant LOC", which basically mentions that the student is
                        // absent and is truant. They don't always use the truan indicator.
                        // There is no state code for "Truant LOC", so created a state code
                        // "TT" for this purpose.
                        String reason = attendance.getReasonCode();
                        reason = lookupReferenceCodeByBeanPath(StudentAttendance.class,
                                StudentAttendance.COL_REASON_CODE, reason,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        if (attendance.getAbsentIndicator()
                                && ABSENT_REASON_TRUANT_STATE_CODE.equalsIgnoreCase(reason)) {
                            truantDays++;
                        }
                    }
                }
                value = Integer.valueOf(truantDays);
            } else if (PARAM_DAYS_PRESENT.equals(param)) {
                int totalDaysPresent = 0;
                for (StudentEnrollmentSpan span : enrollmentSpans) {
                    List<StudentAttendance> attendances = span.getStudentAttendance();
                    int totalAbsentDays = 0;
                    int excusedAbsentDays = 0;

                    for (StudentAttendance attendance : attendances) {
                        // Hunterdon doesn't follow the general rule for calculating the
                        // membership days. Hunterdon checks the reason for absent.
                        // If it is one of the below reasons, then that day is not counted
                        // towards actual membership days.
                        // State Code Description
                        // 3 Excused Absence - Religious Holiday or Bring Your Child to Work Day
                        // 7 Home Instruction
                        String reason = attendance.getReasonCode();
                        reason = lookupReferenceCodeByBeanPath(StudentAttendance.class,
                                StudentAttendance.COL_REASON_CODE, reason,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        if (ABSENT_REASON_EXCUSED_ABSENCE_STATE_CODE.equals(reason)
                                || ABSENT_REASON_HOME_INSTRUCTION_STATE_CODE.equals(reason)) {
                            excusedAbsentDays++;
                        }
                        if (attendance.getAbsentIndicator()) {
                            totalAbsentDays++;
                        }
                    }
                    int membershipDays = span.getMembershipDays() - excusedAbsentDays;
                    int actualAbsentDays = totalAbsentDays - excusedAbsentDays;
                    totalDaysPresent = totalDaysPresent + membershipDays - actualAbsentDays;
                }
                value = Integer.valueOf(totalDaysPresent);
            }
            return value;
        }
    }

    /**
     * Retrieves the number of college credits.
     */
    protected class RetrieveNumOfCollegeCredits implements FieldRetriever {
        private static final String CAL_ID = "NUM-COL-CREDS";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Student std = (Student) entity.getBean();
            String value = (String) std.getFieldValueByBeanPath(m_fieldNumColCreds);
            if (!StringUtils.isEmpty(value)) {
                Integer intValue = Integer.parseInt(value);
                value = String.format("%02d", intValue);
            }
            return value;
        }
    }


    /**
     * Retrieves the Program Of Study of the Student.
     */
    protected class RetrieveOrganizationCodes implements FieldRetriever {
        private static final String CAL_ID = "ORG-CODES";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String param = (String) field.getParameter();
            CTEStudentEntity cTEStudentEntity = (CTEStudentEntity) entity;
            SisStudent student = cTEStudentEntity.getStudent();

            if (PARAM_ASSIGNED_COUNTRY.equals(param)) {
                value = getAsignedValue(student, 0, 2);
            } else if (PARAM_ASSIGNED_DISTR.equals(param)) {
                value = getAsignedValue(student, 2, 6);
            } else if (PARAM_ASSIGNED_SKL.equals(param)) {
                value = getAsignedValue(student, 6, 9);
            }

            return value;
        }

        /**
         * Returned substring from STUDENT.[DOE CTE OVERRIDE CNY DST SCH] or ENROLLMENT.[DOE
         * ATTENDING SCHOOL]
         *
         * @param student SisStudent
         * @param indStart for substring
         * @param indEnd for substring
         * @return String
         */
        private String getAsignedValue(SisStudent student, int indStart, int indEnd) {
            String value = null;
            String stdCnyValue = (String) student.getFieldValueByBeanPath(m_fieldStdCny);

            if (!StringUtils.isEmpty(stdCnyValue)) {
                String stateCnyCode = lookupStateValue(SisStudent.class, m_fieldStdCny, stdCnyValue);

                if (!StringUtils.isEmpty(stateCnyCode) && stateCnyCode.length() >= indEnd) {
                    value = stateCnyCode.substring(indStart, indEnd);
                }
            } else if (value == null) {
                StudentEnrollment lastEnr = m_helper.getEnrollmentForDate(student.getOid(), m_reportDate, "EWS");
                String attSchoolCode = (String) lastEnr.getFieldValueByBeanPath(m_fieldEnrAttSkl);

                if (!StringUtils.isEmpty(attSchoolCode)) {
                    String attSchoolState = lookupStateValue(StudentEnrollment.class, m_fieldEnrAttSkl, attSchoolCode);

                    if (!StringUtils.isEmpty(attSchoolState) && attSchoolState.length() >= indEnd) {
                        value = attSchoolState.substring(indStart, indEnd);
                    }
                }
            }

            return value;
        }
    }

    /**
     * Retrieves the Program Of Study of the Student.
     */
    protected class RetrieveProgramOfStudy implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;
            String param = (String) field.getParameter();
            CTEStudentEntity cTEStudentEntity = (CTEStudentEntity) entity;
            SisStudent student = cTEStudentEntity.getStudent();
            int credits = 0;
            if (PARAM_CREDITS.equals(param)) {
                String programStudy = (String) student.getFieldValueByAlias(ALIAS_CTE_PROGRAM_STUDY);

                if (!(StringUtils.isEmpty(programStudy) || "N".equals(programStudy))) {
                    String creditStr = (String) student.getFieldValueByAlias(ALIAS_CTE_PROGRAM_STUDY_CREDIT);
                    if (!StringUtils.isEmpty(creditStr)) {
                        Double credit = Double.valueOf(creditStr);
                        credits += credit.intValue();
                    }
                }
            }
            if (credits > 0) {
                value = String.valueOf(credits);
                if (value.length() < field.getMaxLength()) {
                    value = com.x2dev.utils.StringUtils.padLeft(value, field.getMaxLength(), '0');
                }
            }
            return value;
        }
    }

    /**
     * Retrieves the Program Of Study of the Student.
     */
    protected class RetrieveScheduleValues implements FieldRetriever {
        private static final String CAL_ID = "SSC-VALUES";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;

            String param = (String) field.getParameter();
            CTEStudentEntity cTEStudentEntity = (CTEStudentEntity) entity;
            SisStudent student = cTEStudentEntity.getStudent();

            if (PARAM_SUBJ_AREA.equals(param)) {
                value = getCalculatedValue(student, 0, 2);
            } else if (PARAM_CRS_ID.equals(param)) {
                value = getCalculatedValue(student, 2, 5);
            }

            return value;
        }

        /**
         * Gets the calculated value.
         *
         * @param student SisStudent
         * @param indStart for substring
         * @param indEnd for substring
         * @return String
         */
        private String getCalculatedValue(SisStudent student, int indStart, int indEnd) {
            String value = null;
            Set<String> crsScedValues = getCteStudentCourseCode(student);

            if (crsScedValues != null && !crsScedValues.isEmpty() && crsScedValues.size() == 1) {
                String crsScedValue = crsScedValues.iterator().next();

                String crsScedStateValue = lookupStateValue(Course.class, m_fieldCrsSced, crsScedValue);

                if (!StringUtils.isEmpty(crsScedStateValue) && crsScedStateValue.length() >= indEnd) {
                    value = crsScedStateValue.substring(indStart, indEnd);
                }
            }

            return value;
        }
    }

    /**
     * Validate Country, District, School codes assigned.
     */
    protected class ValidateAreasAssigned implements FieldValidator {
        private static final String VAL_ID = "CTE-AREAS-ASSIGNED";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Required value", "Missing " +
                        field.getFieldId() +
                        "." + System.lineSeparator()
                        + "Assign Attending School on enrollment  record OR populate CTE County District School Override field."));
            }

            return errors;
        }
    }

    /**
     * Validate College credits and Secondary Institution for program.
     */
    protected class ValidatePostSecInstitution implements FieldValidator {
        private static final String VAL_ID = "CTE-POST-INST";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String isProgOfStudy = entity.getFieldValue(EXP_FIELD_PROG_OF_STUDY);

            if ("Y".equals(isProgOfStudy) && StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Required value", field.getFieldId() +
                        " required when Program of Study = Y."));
            }

            return errors;
        }
    }

    /**
     * Validate College credits and Secondary Institution for program.
     */
    protected class ValidateCollegeCreds implements FieldValidator {
        private static final String VAL_ID = "CTE-COL-CRED";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String isProgOfStudy = entity.getFieldValue(EXP_FIELD_PROG_OF_STUDY);

            if ("Y".equals(isProgOfStudy) && (StringUtils.isEmpty(value) || "0".equals(value))) {
                errors.add(new StateReportValidationError(entity, field, "Required value", field.getFieldId() +
                        " required when Program of Study = Y."));
            }

            return errors;
        }
    }

    /**
     * Validate Student schedule values - Subject Area and Course ID.
     */
    protected class ValidateScheduleValues implements FieldValidator {
        private static final String VAL_ID = "VAL-SSC-VALUES";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            CTEStudentEntity cTEStudentEntity = (CTEStudentEntity) entity;
            SisStudent student = cTEStudentEntity.getStudent();
            Set<String> crsScedValues = getCteStudentCourseCode(student);
            if (crsScedValues != null && !crsScedValues.isEmpty() && crsScedValues.size() > 1) {
                errors.add(new StateReportValidationError(entity, field, "More than one Schedules",
                        "Student Last Name/First Name = " +
                                student.getNameView() +
                                " has more than one CTE STUDENT COURSE flag. Student should only have one CTE STUDENT COURSE flag per school year."));
            }
            return errors;
        }
    }

    /**
     * Validate Test Developer.
     */
    protected class ValidateTestDev implements FieldValidator {
        private static final String VAL_ID = "CTE-TESTDEV";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String testSkillValue = entity.getFieldValue(EXP_FIELD_TEST_SKILL);

            if (Arrays.asList("1", "2").contains(testSkillValue)) {
                if (StringUtils.isEmpty(value)) {
                    errors.add(new StateReportValidationError(entity, field, "Required value",
                            "If CTE Test Skill = 1 or 2 then CTE Test Developer is required."));
                } else if (!Arrays.asList("1", "2", "3", "4").contains(value)) {
                    errors.add(new StateReportValidationError(entity, field, "Invalid value",
                            "CTE Test Developer if not valid.  Must equal 1, 2, 3 or 4."));
                }
            }

            return errors;
        }
    }

    /**
     * Validate Test Name.
     */
    protected class ValidateTestName implements FieldValidator {
        private static final String VAL_ID = "CTE-TESTNAME";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String testSkillValue = entity.getFieldValue(EXP_FIELD_TEST_SKILL);

            if (Arrays.asList("1", "2").contains(testSkillValue) && StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Required value",
                        "If CTE Test Skill = 1 or 2 then CTE Test Name is required."));
            }

            return errors;
        }
    }

    /**
     * Validate Valued Credits.
     */
    protected class ValidateValuedCreds implements FieldValidator {
        private static final String VAL_ID = "VAL-VALUED-CRED";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (!StringUtils.isEmpty(value) && value.length() != 6) {
                errors.add(new StateReportValidationError(entity, field, "Invalid value",
                        field.getFieldId() + " must be 6 characters."));
            }

            return errors;
        }
    }

    /**
     * Aliases
     */
    protected static final String ALIAS_CIP_CODE = "DOE CIP CODE";
    protected static final String ALIAS_CRS_SCED = "DOE SCED CODE";
    protected static final String ALIAS_CTE_PROGRAM_STUDY = "DOE CTE PROG OF STUDY";
    protected static final String ALIAS_CTE_PROGRAM_STUDY_CREDIT = "DOE CTE PROG OF STUDY CREDITS";
    protected static final String ALIAS_ENR_ATT_SCHOOL = "DOE ATTENDING SCHOOL";
    protected static final String ALIAS_NUM_COL_CREDS = "all-std-NumberofCTECollegeCredits";
    protected static final String ALIAS_RESIDENT_SCHOOL = "DOE RESIDING SCHOOL";
    protected static final String ALIAS_SAME_DISTRICT_CODES_LIST = "RELATED RESIDENT DIST CODES";
    protected static final String ALIAS_SCC_COURSE_TO_REPORT = "all-scc-CTECoursetoReport";
    protected static final String ALIAS_SSC_COURSE_TO_REPORT = "DOE CTE COURSE TO REPORT";
    protected static final String ALIAS_STD_OVERRIDE_CNY_DST_SCH = "DOE CTE OVERRIDE CNY DST SCH";

    /**
     * Parameters
     */
    protected static final String PARAM_ASSIGNED_COUNTRY = "COUNTRY_ASSIGNED";
    protected static final String PARAM_ASSIGNED_DISTR = "DISTR_ASSIGNED";
    protected static final String PARAM_ASSIGNED_SKL = "SKL_ASSIGNED";
    protected static final String PARAM_CRS_ID = "CRS_ID";
    protected static final String PARAM_CREDITS = "CREDITS";
    protected static final String PARAM_DAYS_MEMBERSHIP = "MEMBERSHIP";
    protected static final String PARAM_DAYS_PRESENT = "PRESENT";
    protected static final String PARAM_DAYS_TRUANT = "TRUANT";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_SUBJ_AREA = "SUBJECT_AREA";

    /**
     * Other Constants
     */
    protected static final String ABSENT_REASON_EXCUSED_ABSENCE_STATE_CODE = "3";
    protected static final String ABSENT_REASON_HOME_INSTRUCTION_STATE_CODE = "7";
    protected static final String ABSENT_REASON_TRUANT_STATE_CODE = "TT";
    protected static final String KEY_COUNTY_CODE = "countyCode";
    protected static final String KEY_DISTRICT_CODE = "districtCode";
    protected static final String KEY_SCHOOL_CODE = "schoolCode";

    /**
     * Export fields
     */
    private static final String EXP_FIELD_PROG_OF_STUDY = "ProgramOfStudy";
    private static final String EXP_FIELD_TEST_SKILL = "CTE Test Skill";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_cipCode;
    protected StudentHistoryHelper m_helper;
    protected PlainDate m_reportDate;
    protected String m_sameDistrictCodes;
    protected PlainDate m_startDate;
    protected String m_fieldResidingSchool;
    protected String m_fieldStdCny;
    protected String m_fieldEnrAttSkl;
    protected String m_fieldNumColCreds;
    protected String m_fieldSccCteCrsToRepot;
    protected String m_fieldSscCteCrsToRepot;
    protected String m_fieldCrsSced;
    protected DecimalFormat m_numberFormat = new DecimalFormat("00");
    protected int m_reportingSklYear;

    /**
     * Maps
     */
    protected Map<String, Set<String>> m_cteStudentCourseCodeMap = new HashMap();

    /**
     * Initialize the data module.
     */
    @Override
    public void initialize() {
        initializeFields();

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SCHEDULE_SPANS, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        X2Criteria criteria = m_helper.getStudentCriteria();
        criteria.addNotNull(m_cipCode);
        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            initReportingYear();
            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(CTEStudentEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            HashMap validators = new HashMap<String, FieldRetriever>();
            calcs.put("CTE-PROGSTUDY", new RetrieveProgramOfStudy());
            calcs.put("CTE-ENROLL", new RetrieveEnrollment());
            calcs.put(RetrieveNumOfCollegeCredits.CAL_ID, new RetrieveNumOfCollegeCredits());
            calcs.put(RetrieveOrganizationCodes.CAL_ID, new RetrieveOrganizationCodes());
            calcs.put(RetrieveScheduleValues.CAL_ID, new RetrieveScheduleValues());
            validators.put(ValidateScheduleValues.VAL_ID, new ValidateScheduleValues());
            validators.put(ValidateTestDev.VAL_ID, new ValidateTestDev());
            validators.put(ValidateTestName.VAL_ID, new ValidateTestName());
            validators.put(ValidateCollegeCreds.VAL_ID, new ValidateCollegeCreds());
            validators.put(ValidateAreasAssigned.VAL_ID, new ValidateAreasAssigned());
            validators.put(ValidateValuedCreds.VAL_ID, new ValidateValuedCreds());
            validators.put(ValidatePostSecInstitution.VAL_ID, new ValidatePostSecInstitution());
            super.addCalcs(calcs);
            super.addValidators(validators);
        }
    }

    protected Set<String> getCteStudentCourseCode(SisStudent std) {
        Set<String> values = Collections.EMPTY_SET;
        if (m_cteStudentCourseCodeMap.containsKey(std.getOid())) {
            values = m_cteStudentCourseCodeMap.get(std.getOid());
        } else {
            values = m_helper.getStudentScheduleSpans(std).stream()
                    .filter(span -> {
                        if (span.getSchedule() == null) {
                            if (span.getExitChange() != null && BooleanAsStringConverter.TRUE
                                    .equals(span.getExitChange().getFieldValueByBeanPath(m_fieldSccCteCrsToRepot))) {
                                return true;
                            }
                        } else {
                            if (BooleanAsStringConverter.TRUE
                                    .equals(span.getSchedule().getFieldValueByBeanPath(m_fieldSscCteCrsToRepot))) {
                                return true;
                            }
                        }
                        return false;
                    })
                    .map(span -> (String) span.getSection().getSchoolCourse().getCourse()
                            .getFieldValueByBeanPath(m_fieldCrsSced))
                    .filter(crsScedValue -> !StringUtils.isEmpty(crsScedValue))
                    .collect(Collectors.toSet());
            m_cteStudentCourseCodeMap.put(std.getOid(), values);
        }
        return values;
    }

    /**
     * This method returns the start date.
     *
     * @return Plain date
     */
    protected PlainDate getStartDate() {
        return m_startDate;
    }

    /**
     * This method returns the StudentHistoryHelper.
     *
     * @return Student history helper
     */
    protected StudentHistoryHelper getStudentHistoryHelper() {
        return m_helper;
    }

    /**
     * This method returns the county, district, school code by splitting the
     * countyDistrictSchoolCode code.
     *
     * @param countyDistrictSchoolCode String
     * @return Map
     */
    protected Map<String, String> getCountyDistrictSchoolCode(String countyDistrictSchoolCode) {
        Map<String, String> codeMap = null;
        if (!StringUtils.isEmpty(countyDistrictSchoolCode) && countyDistrictSchoolCode.length() > 6)// check
        // for
        // length
        {
            codeMap = new HashMap<String, String>();
            codeMap.put(KEY_COUNTY_CODE, countyDistrictSchoolCode.substring(0, 2));
            codeMap.put(KEY_DISTRICT_CODE, countyDistrictSchoolCode.substring(2, 6));
            codeMap.put(KEY_SCHOOL_CODE, countyDistrictSchoolCode.substring(6));// can be 3-digit or
            // 4-digit
            // If it is 4-digit, then it is actually municipal code
            // and not school code.
        }
        return codeMap;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_cipCode = translateAliasToJavaName(ALIAS_CIP_CODE, true);
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }
        m_startDate = getCurrentContext().getStartDate();
        m_fieldResidingSchool = translateAliasToJavaName(ALIAS_RESIDENT_SCHOOL, true);
        m_sameDistrictCodes = translateAliasToJavaName(ALIAS_SAME_DISTRICT_CODES_LIST, false);
        m_fieldStdCny = translateAliasToJavaName(ALIAS_STD_OVERRIDE_CNY_DST_SCH, true);
        m_fieldNumColCreds = translateAliasToJavaName(ALIAS_NUM_COL_CREDS, true);
        m_fieldEnrAttSkl = translateAliasToJavaName(ALIAS_ENR_ATT_SCHOOL, true);
        m_fieldSccCteCrsToRepot = translateAliasToJavaName(ALIAS_SCC_COURSE_TO_REPORT, true);
        m_fieldSscCteCrsToRepot = translateAliasToJavaName(ALIAS_SSC_COURSE_TO_REPORT, true);
        m_fieldCrsSced = translateAliasToJavaName(ALIAS_CRS_SCED, true);

    }

    /**
     * Init skl year by reporting date.
     */
    private void initReportingYear() {
        X2Criteria ctxCriteria = new X2Criteria();
        ctxCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, m_reportDate);
        ctxCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, m_reportDate);

        DistrictSchoolYearContext ctx = (DistrictSchoolYearContext) getBroker()
                .getBeanByQuery(new QueryByCriteria(DistrictSchoolYearContext.class, ctxCriteria));

        if (ctx != null) {
            m_reportingSklYear = ctx.getSchoolYear();
        } else {
            addSetupError("Missing bean", "Missing District School Year Context for given date = " + m_reportDate);
        }
    }

}
