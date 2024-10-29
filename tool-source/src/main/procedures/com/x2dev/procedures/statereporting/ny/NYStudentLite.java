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
package com.x2dev.procedures.statereporting.ny;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisPerson;
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
import java.util.*;

/**
 * New York state procedure for StudentLite export.
 *
 * @author X2 Development Corporation
 */

public class NYStudentLite extends StateReportData {
    // static Logger m_logger = AppGlobals.getLog();

    /**
     * Entity class for Student Lite export.
     *
     * @author X2 Development Corporation
     */

    public static class NYStudentLiteEntity extends StateReportEntity {
        /**
         * Local variables for reporting information.
         */
        protected StudentEnrollmentSpan m_enrollment;
        private NYStudentLite m_data;
        private String m_gradeLevel;

        /**
         * Public no argument constructor for dynamic instantiation.
         */

        public NYStudentLiteEntity() {
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
            m_data = (NYStudentLite) data;

            Student student = (Student) bean;

            List<StudentEnrollmentSpan> enrollmentSpans = m_data.m_helper.getStudentEnrollmentSpans(student, true);

            // remove spans without school
            Iterator<StudentEnrollmentSpan> iterator = enrollmentSpans.iterator();
            while (iterator.hasNext()) {
                StudentEnrollmentSpan span = iterator.next();
                if (span.getSchool() == null) {
                    iterator.remove();
                    continue;
                }
            }

            for (StudentEnrollmentSpan span : enrollmentSpans) {
                String schoolOid = span.getSchool().getOid();

                // Only include spans from included schools.
                if (m_data.includeSchool(schoolOid)) {
                    StudentEnrollment entryEnrollment = span.getFirstActiveEnrollment();

                    // Exclude spans with the Enrollment status of Pre-enroll and PreReg
                    if (!(CODE_ENROLL_STATUS_PRE_ENROLL.equalsIgnoreCase(entryEnrollment.getStatusCode())
                            || CODE_ENROLL_STATUS_PRE_REG.equalsIgnoreCase(entryEnrollment.getStatusCode()))) {
                        m_enrollment = span;
                    }
                }
            }

            if (m_enrollment == null) {
                setRowCount(0);
            }
        }

        /**
         * Returns the First Active Enrollment.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getActiveEnrollment() {
            return m_enrollment.getFirstActiveEnrollment();
        }

        /**
         * Returns students grade level.
         *
         * @return String
         */
        public String getGrade() {
            if (m_gradeLevel == null) {
                StudentEnrollment entryEnrollment = getActiveEnrollment();
                StudentEnrollment withdrawEnrollment = getInactiveEnrollment();
                StudentEnrollment yogEnrollment = m_data.m_helper.getEnrollmentForDate(getStudent().getOid(),
                        m_data.m_reportDate, StudentEnrollment.YOG_CHANGE);

                if (entryEnrollment != null && StudentEnrollment.ENTRY.equals(entryEnrollment.getEnrollmentType())
                        && withdrawEnrollment != null
                        && StudentEnrollment.YOG_CHANGE.equals(withdrawEnrollment.getEnrollmentType())) {
                    // Take the GradeLevel form the Entry Enrollment GradeLevelOverRide
                    // the withdraw part of YOG split.
                    // calculate grade form the spans enrollment
                    m_gradeLevel = m_data.calcPreviousGradeLevel(entryEnrollment, withdrawEnrollment);
                    m_gradeLevel =
                            m_data.lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_data.m_enrollmentGradeLevel,
                                    m_gradeLevel, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (m_gradeLevel == null && yogEnrollment != null
                        && !m_data.m_schoolYearStartDate.after(yogEnrollment.getEnrollmentDate())) {
                    // If there is YOG Enrollment and its date is before the start of the school
                    // year, take it's GradeLevel.
                    m_gradeLevel = (String) yogEnrollment.getFieldValueByBeanPath(m_data.m_enrollmentGradeLevel);
                    m_gradeLevel =
                            m_data.lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_data.m_enrollmentGradeLevel,
                                    m_gradeLevel, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (entryEnrollment != null && m_gradeLevel == null) {
                    // Take the GradeLevel form the Entry Enrollment GradeLevelOverRide
                    m_gradeLevel =
                            (String) entryEnrollment.getFieldValueByBeanPath(m_data.m_enrollmentGradeLevelOverride);
                    m_gradeLevel = m_data.lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                            m_data.m_enrollmentGradeLevelOverride, m_gradeLevel,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (entryEnrollment != null && m_gradeLevel == null) {
                    // Take the GradeLevel form the Entry Enrollment GradeLevel
                    m_gradeLevel = (String) entryEnrollment.getFieldValueByBeanPath(m_data.m_enrollmentGradeLevel);
                    m_gradeLevel =
                            m_data.lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_data.m_enrollmentGradeLevel,
                                    m_gradeLevel, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (m_gradeLevel == null) {
                    // Take the GradeLevel form the Student
                    m_gradeLevel = getStudent().getGradeLevel();
                    m_gradeLevel = m_data.lookupReferenceCodeByBeanPath(SisStudent.class, SisStudent.COL_GRADE_LEVEL,
                            m_gradeLevel, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }

            return m_gradeLevel;
        }

        /**
         * Gets the guidance counselor.
         *
         * @return String
         */
        public String getGuidanceCounselor() {
            SisStudent student = (SisStudent) getBean();
            String value = (String) student.getFieldValueByBeanPath(m_data.m_guidanceCounselor);
            if (!StringUtils.isEmpty(value)) {
                value = m_data.lookupStateValue(SisStudent.class, m_data.m_guidanceCounselor, value);
            }
            return value;
        }

        /**
         * Returns the First Inactive Enrollment.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getInactiveEnrollment() {
            return m_enrollment.getFirstInactiveEnrollment();
        }

        /**
         * Returns the Race collection for the student in this entity.
         *
         * @return Collection<Race>
         */
        public Collection<Race> getRaces() {
            SisStudent student = (SisStudent) getBean();
            Collection<Race> races = null;
            SisPerson person = student.getPerson();
            if (person != null) {
                races = person.getRaces();
            }

            return races;
        }

        /**
         * Returns the current student.
         *
         * @return SisStudent
         */
        public SisStudent getStudent() {
            return (SisStudent) getBean();
        }
    }

    /**
     * The Class NYStudentHistoryHelper.
     * Needed to exclude enrollments with flag [all-enr-ExcludeStateReporting] set to true from use
     * by Student History Helper.
     */
    static final class NYStudentHistoryHelper extends StudentHistoryHelper {
        private static final String ALIAS_EXCLUDE_ENROLLMENT = "all-enr-ExcludeStateReporting";

        private String m_excludeEnrollment;

        /**
         * Instantiates a new NY student history helper.
         *
         * @param data StateReportData
         */
        public NYStudentHistoryHelper(StateReportData data) {
            super(data);
            m_excludeEnrollment = data.translateAliasToJavaName(ALIAS_EXCLUDE_ENROLLMENT, true);
        }

        /**
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#buildEnrollmentCriteria(com.follett.fsc.core.framework.persistence.SubQuery)
         */
        @Override
        protected X2Criteria buildEnrollmentCriteria(SubQuery studentSubQuery) {
            X2Criteria enrCriteria = super.buildEnrollmentCriteria(studentSubQuery);
            if (m_excludeEnrollment != null) {
                enrCriteria.addNotEqualTo(m_excludeEnrollment, BooleanAsStringConverter.TRUE);
            }
            return enrCriteria;
        }
    }

    /**
     * Retrieves the Enrollment data fields.
     */
    protected class RetrieveEnrollment implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            Object value = null;
            NYStudentLiteEntity stdLiteEntity = ((NYStudentLiteEntity) entity);
            StudentEnrollment enrollment = stdLiteEntity.getActiveEnrollment();

            if (enrollment != null) {
                if (CALC_PARAM_SERV_DISTRICT.equals(param)) {
                    value = enrollment.getFieldValueByBeanPath(m_servDistrict);
                    if (value == null) {
                        value = enrollment.getSchool().getOrganization1().getId();
                    }
                } else if (CALC_PARAM_SERV_SCHOOL.equals(param)) {
                    value = enrollment.getFieldValueByBeanPath(m_servSchool);
                    if (value == null) {
                        value = enrollment.getSchool().getSchoolId();
                    }
                } else if (CALC_PARAM_HOME_DISTRICT.equals(param)) {
                    value = enrollment.getFieldValueByBeanPath(m_enrResidentDistrict);
                    if (value == null) {
                        value = enrollment.getFieldValueByBeanPath(m_homeDistrict);
                    }
                    if (value == null) {
                        value = enrollment.getSchool().getOrganization1().getId();
                    }
                    if (value != null && !((String) value).contains(STATE_CODE_NY)) {
                        value = STATE_CODE_NY + value;
                    }
                } else if (CALC_PARAM_LOCATION_CODE.equals(param)) {
                    // First check the enrollment for an override.
                    if (enrollment != null) {
                        value = enrollment.getFieldValueByBeanPath(m_enrLocationOverride);

                        // If we don't have a value check the school of the enrollment, and check
                        // that location code
                        if (value == null) {
                            value = enrollment.getSchool().getFieldValueByBeanPath(m_enrLocationCode);
                        }
                    }

                    // If we still don't have a value get the code from the students school
                    if (value == null) {
                        value = ((SisStudent) entity.getBean()).getSchool().getFieldValueByBeanPath(m_enrLocationCode);
                    }
                }
            }

            return value;
        }
    }

    /**
     * Retrieves the Primary or Secondary Guardians for the given StudentLiteEntity.
     */
    protected class RetrieveGuardian implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            NYStudentLiteEntity stdLiteEntity = ((NYStudentLiteEntity) entity);
            SisStudent student = stdLiteEntity.getStudent();
            NYStudentLite stdData = (NYStudentLite) data;

            StudentContact guardian = null;
            Object value = null;
            if (param != null) {
                if (CALC_PARAM_PRIMARY.equals(param)) {
                    guardian = student.getPrimaryContact();
                } else if (CALC_PARAM_ALTERNATE.equals(param)) {
                    guardian = student.getSecondaryContact();
                }
                if (guardian != null && guardian.getContact() != null &&
                        guardian.getContact().getPerson() != null &&
                        BooleanAsStringConverter.TRUE.equals(guardian.getFieldValueByBeanPath(stdData.m_guardian))) {
                    value = guardian.getContact().getPerson().getFirstName() + " "
                            + guardian.getContact().getPerson().getLastName();
                }
            }

            return value;
        }
    }

    /**
     * Retrieves the Race field in order of importance as input for the StudentLiteEntity.
     */
    protected class RetrieveRace implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            Collection<Race> races = ((NYStudentLiteEntity) entity).getRaces();
            Race found = null;
            Object value = null;
            int count = 0;
            // Find the reference code that we are looking for.
            if (param != null) {
                for (Race race : races) {
                    count++;
                    if (Integer.toString(count).equals(param)) {
                        found = race;
                        break;
                    }
                }
            }

            if (found != null) {
                value = data.lookupReferenceCodeByBeanPath(Race.class, Race.COL_RACE_CODE, found.getRaceCode(),
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }

            return value;
        }
    }

    /**
     * Retrieves the Student status from the StudentLiteEntity.
     */
    protected class RetrieveStudentInfo implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = STUDENT_STATUS_INACTIVE;
            NYStudentLiteEntity stdLiteEntity = ((NYStudentLiteEntity) entity);
            SisStudent student = stdLiteEntity.getStudent();
            String param = (String) field.getParameter();

            if (CALC_PARAM_STD_STATUS.equals(param)) {
                StudentEnrollment enrollment = m_helper.getEnrollmentForDate(student.getOid(), m_reportDate,
                        StudentEnrollment.ENTRY + StudentEnrollment.WITHDRAWAL);

                if (enrollment != null
                        && (StudentEnrollment.ENTRY).equalsIgnoreCase(enrollment.getEnrollmentType())
                        && !(CODE_ENROLL_STATUS_PRE_ENROLL.equals(enrollment.getStatusCode())
                                || CODE_ENROLL_STATUS_PRE_REG.equalsIgnoreCase(enrollment.getStatusCode()))) {
                    if (includeSchool(enrollment.getSchoolOid())) {
                        value = STUDENT_STATUS_ACTIVE;
                    }
                }
            } else if (CALC_PARAM_GRADE_LEVEL.equals(param)) {
                value = stdLiteEntity.getGrade();
            } else if (CALC_PARAM_MIDDLE_INITIAL.equals(param)) {
                String middleInitial = student.getPerson().getMiddleName();
                if (!StringUtils.isEmpty(middleInitial)) {
                    middleInitial = middleInitial.substring(0, 1);
                }
                value = middleInitial;
            } else if (CALC_PARAM_STD_LEP_PARTICIPATION.equals(param)) {
                int monthsInProgram = 0;
                Collection<StudentProgramParticipation> prgParticipations = student.getProgramParticipation();
                for (StudentProgramParticipation prgParticipation : prgParticipations) {
                    String programCode = prgParticipation.getProgramCode();
                    if (CODE_PROGRAM_LEP.equals(programCode)) {
                        PlainDate startDate = prgParticipation.getStartDate();
                        PlainDate endDate = prgParticipation.getEndDate();

                        Calendar sd = Calendar.getInstance();
                        sd.setTime(startDate);

                        Calendar ed = Calendar.getInstance();
                        if (endDate != null) {
                            ed.setTime(endDate);
                        }

                        // Grab start and end month
                        int startMonth = sd.get(Calendar.MONTH);
                        int endMonth = ed.get(Calendar.MONTH);

                        // check to see if its they have been in most of the month
                        if (ed.get(Calendar.DAY_OF_MONTH) - sd.get(Calendar.DAY_OF_MONTH) > 27) {
                            monthsInProgram++;
                        }

                        // if the start month is at the end of the calendar year but at the
                        // beginning of the school year
                        if (startMonth > endMonth) {
                            endMonth += 12;
                        }

                        int years = ed.get(Calendar.YEAR) - sd.get(Calendar.YEAR);

                        monthsInProgram += endMonth - startMonth;
                        // Assume 10 months per year, school year
                        monthsInProgram += (years > 1) ? (years - 1) * 10 : 0;
                    }
                }

                // If a student received instruction for the majority of a school year (seven months
                // or more), count that year as a full year of instruction.
                Integer yearInProgram = Integer.valueOf(monthsInProgram + 3 / 10);
                if (yearInProgram.intValue() * 10 < monthsInProgram) {
                    yearInProgram = Integer.valueOf(yearInProgram.intValue() + 1);
                }
                value = yearInProgram;
            } else if (CALC_PARAM_STD_LAST_STATUS_DATE.equals(param)) {
                value = null;
                // Grab the last enrollment record, entry or withdrawal
                List<StudentEnrollment> enrollments = new LinkedList(m_helper.getStudentEnrollments(student));
                StudentEnrollment enrollment = popEnrollments(enrollments, m_reportDate,
                        StudentEnrollment.ENTRY + StudentEnrollment.WITHDRAWAL + StudentEnrollment.STATUS_CHANGE);

                // Keep scrolling through until we get the last one at a school that is not excluded
                while (enrollment != null
                        && enrollment.getSchool() != null
                        && (!includeSchool(enrollment.getSchoolOid())
                                || CODE_ENROLL_STATUS_PRE_REG.equals(enrollment.getStatusCode()))) {
                    // enrollment = m_helper.getEnrollmentForDate(student.getOid(),
                    // enrollment.getEnrollmentDate(), enrollment.getEnrollmentType().equals("W") ?
                    // "E" : "W");

                    String enrollmentType = "W";
                    if ("W".equals(enrollment.getEnrollmentType())) {
                        enrollmentType = "E";
                    }

                    enrollment = popEnrollments(enrollments, enrollment.getEnrollmentDate(),
                            enrollmentType);
                }

                // make sure its not from before this year
                if (enrollment != null && !enrollment.getEnrollmentDate().before(m_schoolYearStartDate)) {
                    String enrollmentType = enrollment.getEnrollmentType();

                    if (StudentEnrollment.ENTRY.equalsIgnoreCase(enrollmentType)) {
                        value = m_reportDate;
                    } else if (StudentEnrollment.WITHDRAWAL.equalsIgnoreCase(enrollmentType)) {
                        value = enrollment.getEnrollmentDate();
                    }
                }

                if (value == null) {
                    // Otherwise use the last day of school or the report date
                    value = m_schoolYearEndDate;
                    if (m_reportDate != null && m_schoolYearEndDate != null
                            && !m_reportDate.after(m_schoolYearEndDate)) {
                        value = m_reportDate;
                    }
                }
            }

            return value;
        }

        /**
         * Pop enrollments.
         *
         * @param enrollments List<StudentEnrollment>
         * @param date PlainDate
         * @param types String
         * @return StudentEnrollment
         */
        private StudentEnrollment popEnrollments(List<StudentEnrollment> enrollments,
                                                 PlainDate date,
                                                 String types) {
            StudentEnrollment enrollment = null;
            while ((enrollment = popEnrollment(enrollments)) != null) {
                if (enrollment.getEnrollmentDate() != null && !enrollment.getEnrollmentDate().after(date)) {
                    if (types.contains(enrollment.getEnrollmentType())) {
                        break;
                    }
                }
            }
            return enrollment;
        }

        /**
         * Pop enrollment.
         *
         * @param enrollments List<StudentEnrollment>
         * @return StudentEnrollment
         */
        private StudentEnrollment popEnrollment(List<StudentEnrollment> enrollments) {
            StudentEnrollment enrollment = null;
            if (enrollments != null && !enrollments.isEmpty()) {
                enrollment = enrollments.get(0);
                enrollments.remove(0);
            }
            return enrollment;
        }
    }

    /**
     * Retrieve the Guidance CounselorID.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveGuidanceCounselor implements FieldRetriever {
        private static final String DISTRICT_CODE = "DISTRICT CODE";
        private static final String PARAM_COUSELOR_ID = "COUNSELOR ID";
        private static final String PARAM_COUSELOR_DISTRICT_ID = "DISTRICT ID";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;
            String guidanceCounselor = ((NYStudentLiteEntity) entity).getGuidanceCounselor();
            if (PARAM_COUSELOR_ID.equals(field.getParameter())) {
                value = guidanceCounselor;
                if (!StringUtils.isEmpty(value)) {
                    value = StringUtils.padLeft(value, 9, '0');
                }
            } else if (PARAM_COUSELOR_DISTRICT_ID.equals(field.getParameter())) {
                if (!StringUtils.isEmpty(guidanceCounselor)) {
                    String overrideDistrictId =
                            (String) entity.getBean().getFieldValueByBeanPath(m_guidanceCounselorDistrictOverride);
                    if (!StringUtils.isEmpty(overrideDistrictId)) {
                        value = overrideDistrictId;
                    } else {
                        value = entity.getFieldValue(DISTRICT_CODE);
                    }
                }
            }
            return value;
        }
    }

    /**
     * Validator to check birthdate.
     *
     * @author X2 Development Corporation
     *
     */
    private class ValidateBirthdate implements FieldValidator {
        public static final String VAL_CALC_ID = "VAL_BIRTH";

        /**
         * Instantiates a new validate birthdate.
         */
        public ValidateBirthdate() {
            // to avoid warning
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
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

            NYStudentLiteEntity slEntity = (NYStudentLiteEntity) entity;
            SisStudent student = slEntity.getStudent();
            PlainDate birthDate = student.getPerson().getDob();

            if (birthDate.after(m_reportDate)) {
                errors.add(new StateReportValidationError(entity, field,
                        "The birth date cannot be greater than the current date.",
                        "Current date = " + STYLE_BOLD + m_formatter.format(m_reportDate) + STYLE_END +
                                ", birthdate = " + STYLE_BOLD + m_formatter.format(birthDate) + STYLE_END));
            }

            /*
             * A birthdate that is 23 years less than the current school year will receive the error
             */
            if (getYearsDiff(birthDate, m_schoolYearStartDate) >= 23) {
                errors.add(new StateReportValidationError(entity, field,
                        "Birth Date cannot be 23 years prior to school year.",
                        "School year start date = " + STYLE_BOLD + m_formatter.format(m_schoolYearStartDate) + STYLE_END
                                +
                                ", birthdate = " + STYLE_BOLD + m_formatter.format(birthDate) + STYLE_END));
            }

            return errors;
        }

    }

    /**
     * Validator to check homeless primary nighttime residence.
     *
     * @author X2 Development Corporation
     *
     */
    private class ValidateCareerPath implements FieldValidator {
        public static final String VAL_CALC_ID = "VAL_CAREER_PATH";

        private List<String> m_validCodes = null;
        private List<String> m_credTypeNoneCodes = null;

        /**
         * Instantiates a new validate career path.
         */
        public ValidateCareerPath() {
            m_validCodes =
                    Arrays.asList("ARTS", "CTE", "HUM", "HUMALT", "LOTE", "STEMMATH", "STEMSCIENCE", "CDOS", "NONE");
            m_credTypeNoneCodes = Arrays.asList("738", "085", "119", "136");
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            String carrerPath = "CAREER PATH CODE";

            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            /*
             * Career Path Code, if provided, must match a value in the COURSE OF STUDY Master
             * lookup table.
             */
            if (!StringUtils.isEmpty(value) && !m_validCodes.contains(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        carrerPath + " must be valid code " + m_validCodes,
                        carrerPath + " = " + STYLE_BOLD + value + STYLE_END));
            }

            /*
             * When Career Path Code is provided, Credential Type Code must also be provided.
             */
            String diplomaType = "DIPLOMA TYPE CODE";
            String diplomaCodeValue = entity.getFieldValue(diplomaType);

            if (!StringUtils.isEmpty(carrerPath) && StringUtils.isEmpty(diplomaType)) {
                errors.add(new StateReportValidationError(entity, field,
                        diplomaType + " must be present when " + carrerPath + " is present.",
                        carrerPath + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                diplomaType + " = " + STYLE_BOLD + diplomaCodeValue + STYLE_END));
            }

            /*
             * Any Credential Type Codes, other than 738, 085, 119 or 136, that are used in
             * conjunction with a
             * Career Path Code of "NONE" will receive the error
             */
            if ("NONE".equals(value) && !m_credTypeNoneCodes.contains(diplomaCodeValue)) {
                errors.add(new StateReportValidationError(entity, field,
                        "A " + field.getFieldId() + " of \"NONE\" is only available for students earning a GED or "
                                + "Commencement Credential (Credential Type Codes 738, 085, 119 or 136)",
                        carrerPath + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                diplomaType + " = " + STYLE_BOLD + diplomaCodeValue + STYLE_END));
            }

            return errors;
        }

    }

    /**
     * Validator to check the career path code is present when diploma type code is present.
     *
     * @author X2 Development Corporation
     *
     */
    private class ValidateDiplomaCode implements FieldValidator {
        private List<String> m_validCodes = null;

        /**
         * Instantiates a new validate diploma code.
         */
        public ValidateDiplomaCode() {
            m_validCodes =
                    Arrays.asList("762", "813", "779", "796", "068", "612", "680", "697", "714", "731", "738", "085",
                            "204", "221", "238", "255", "272", "289", "306", "323", "340", "357", "374", "391", "119",
                            "136");
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            String carrerPath = "CAREER PATH CODE";
            String diplomaType = "DIPLOMA TYPE CODE";
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String careerPathCodeValue = entity.getFieldValue(carrerPath);
            String diplomaCodeValue = entity.getFieldValue(diplomaType);

            if (!StringUtils.isEmpty(diplomaCodeValue) && StringUtils.isEmpty(careerPathCodeValue)) {
                errors.add(new StateReportValidationError(entity, field,
                        carrerPath + " must be present when " + diplomaType + " is present.",
                        carrerPath + " = " + STYLE_BOLD + careerPathCodeValue + STYLE_END + ", " +
                                diplomaType + " = " + STYLE_BOLD + diplomaCodeValue + STYLE_END));
            }

            if (!StringUtils.isEmpty(diplomaCodeValue) && !m_validCodes.contains(diplomaCodeValue)) {
                errors.add(new StateReportValidationError(entity, field,
                        diplomaType + " must be valid code " + m_validCodes,
                        diplomaType + " = " + STYLE_BOLD + diplomaCodeValue + STYLE_END));
            }

            return errors;
        }

    }

    /**
     * Validator to check date of entry grade 9.
     *
     * @author X2 Development Corporation
     *
     */
    private class ValidateEntryGrade9 implements FieldValidator {
        public static final String VAL_CALC_ID = "VAL_ENT_GRADE_9";

        private List m_validatedGrades;

        /**
         * Instantiates a new validate entry grade 9.
         */
        public ValidateEntryGrade9() {
            m_validatedGrades = Arrays.asList("09", "10", "11", "12");
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
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
            NYStudentLiteEntity stdLiteEntity = (NYStudentLiteEntity) entity;
            String studentGradeLvl = stdLiteEntity.getGrade();

            if (!StringUtils.isEmpty(value)) {
                Date entryGrade9Date = null;
                try {
                    entryGrade9Date = m_exportFormat.parse(value);
                } catch (ParseException e) {
                    e.printStackTrace();
                }

                /*
                 * A record with a grade level that equates to a grade ordinal of 9th, 10th, 11th,
                 * 12th, and a grade 9 entry
                 * date is greater than current date will receive the error.
                 */
                if (m_validatedGrades.contains(studentGradeLvl) && entryGrade9Date.after(m_reportDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + " cannot be greater than the current date.",
                            "Current date = " + STYLE_BOLD + m_formatter.format(m_reportDate) + STYLE_END +
                                    ", " + field.getFieldId() + " = " + STYLE_BOLD + m_formatter.format(entryGrade9Date)
                                    + STYLE_END));
                }

                /*
                 * A date of entry to Grade 9 that is less than or equal to the students birthdate
                 * will receive
                 * the following error.
                 */
                Date birthDate = null;
                try {
                    birthDate = m_exportFormat.parse(entity.getFieldValue("BIRTH DATE"));
                } catch (ParseException e) {
                    e.printStackTrace();
                }
                if (birthDate != null && !entryGrade9Date.after(birthDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + " is not later than birth date.",
                            field.getFieldId() + " = " + STYLE_BOLD + m_formatter.format(entryGrade9Date) + STYLE_END +
                                    ", birthdate = " + STYLE_BOLD + m_formatter.format(birthDate) + STYLE_END));
                }
            } else if (m_validatedGrades.contains(studentGradeLvl)) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " cannot be empty if grade level is " + m_validatedGrades,
                        "Grade level = " + STYLE_BOLD + studentGradeLvl + STYLE_END));
            }

            return errors;
        }

    }

    /**
     * Validator to check hispanic indicator.
     *
     * @author X2 Development Corporation
     *
     */
    private class ValidateHispIndicator implements FieldValidator {
        public static final String VAL_CALC_ID = "VAL_HISP_IND";

        private static final String RACE_1_CODE = "RACE 1 CODE";

        private Date m_priorDate;
        private List m_validRace1Codes;

        /**
         * Instantiates a new validate hisp indicator.
         */
        public ValidateHispIndicator() {
            Calendar cal = Calendar.getInstance();
            cal.set(Calendar.YEAR, 2010);
            cal.set(Calendar.MONTH, 5);
            cal.set(Calendar.DATE, 30);

            m_priorDate = cal.getTime();

            m_validRace1Codes = Arrays.asList("Hispanic", "Multiracial");
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
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

            if ("Y".equals(value) && !m_reportDate.after(m_priorDate)) {
                String race1 = entity.getFieldValue(RACE_1_CODE);
                if (!m_validRace1Codes.contains(race1)) {
                    errors.add(new StateReportValidationError(entity, field,
                            RACE_1_CODE + " must be valid code " + m_validRace1Codes + " if " + field.getFieldId()
                                    + " = Y "
                                    + "and report date 2010/06/30 and prior.",
                            RACE_1_CODE + " = " + STYLE_BOLD + race1 + STYLE_END + ", " +
                                    field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                    "Current date = " + STYLE_BOLD + m_formatter.format(m_reportDate) + STYLE_END));
                }
            }

            return errors;
        }

    }

    /**
     * Validator to check homeless indicator.
     *
     * @author X2 Development Corporation
     *
     */
    private class ValidateHomelessInd implements FieldValidator {
        public static final String VAL_CALC_ID = "VAL_HOMELESS_IND";

        private static final String FIELD_HOMELESS_RES = "HOMELESS RESIDENCE";

        /**
         * Instantiates a new validate homeless ind.
         */
        public ValidateHomelessInd() {
            // to avoid warning
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
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

            String homelessResValue = entity.getFieldValue(FIELD_HOMELESS_RES);

            if (!"Y".equals(value) && !StringUtils.isEmpty(homelessResValue)) {
                errors.add(new StateReportValidationError(entity, field,
                        "If " + FIELD_HOMELESS_RES + " has a value, then the " + field.getFieldId() + " must = Y",
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                FIELD_HOMELESS_RES + " = " + STYLE_BOLD + homelessResValue + STYLE_END));
            }

            return errors;
        }

    }

    /**
     * Validator to check homeless primary nighttime residence.
     *
     * @author X2 Development Corporation
     *
     */
    private class ValidateHomelessRes implements FieldValidator {
        public static final String VAL_CALC_ID = "VAL_HOMELESS_RES";

        private static final String FIELD_HOMELESS_INDICATOR = "HOMELESS INDICATOR";

        private List<String> m_validCodes = null;

        /**
         * Instantiates a new validate homeless res.
         */
        public ValidateHomelessRes() {
            m_validCodes = Arrays.asList("S", "T", "A", "D", "U", "H");
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
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
                        field.getFieldId() + " must be valid code " + m_validCodes,
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
            }

            /*
             * If Homeless indicator = "Y", Homeless Primary Nighttime Residence must also be
             * populated
             */
            if (StringUtils.isEmpty(value) && "Y".equals(entity.getFieldValue(FIELD_HOMELESS_INDICATOR))) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " must be populated if " + FIELD_HOMELESS_INDICATOR + " = \"Y\"",
                        field.getFieldId() + " is empty"));
            }

            return errors;
        }

    }

    /**
     * Validator to check last status date.
     *
     * @author X2 Development Corporation
     */
    private class ValidateLastStatusDate implements FieldValidator {
        public static final String VAL_CALC_ID = "VAL_LAST_STATUS_DATE";

        private Date m_30JuneDate;

        /**
         * Instantiates a new validate last status date.
         */
        public ValidateLastStatusDate() {
            Calendar cal = Calendar.getInstance();
            cal.set(Calendar.YEAR, getCurrentContext().getSchoolYear());
            cal.set(Calendar.MONTH, 5);
            cal.set(Calendar.DATE, 30);

            m_30JuneDate = cal.getTime();
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
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
            Date date = null;
            try {
                date = m_exportFormat.parse(value);
            } catch (ParseException e) {
                e.printStackTrace();
            }

            if (date != null && m_30JuneDate.before(date)) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " must not be later than June 30th of the user selected school year",
                        field.getFieldId() + " = " + STYLE_BOLD + m_formatter.format(date) + STYLE_END + ", " +
                                "Selected school year = " + STYLE_BOLD + getCurrentContext().getSchoolYear()
                                + STYLE_END));
            }

            return errors;
        }

    }

    /**
     * Validator to check post graduate.
     *
     * @author X2 Development Corporation
     *
     */
    private class ValidatePostGrad implements FieldValidator {
        public static final String VAL_CALC_ID = "VAL_POST_GRAD";

        private static final String FIELD_CRED_TYPE = "DIPLOMA TYPE CODE";

        /**
         * Instantiates a new validate post grad.
         */
        public ValidatePostGrad() {
            // to avoid warning
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
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

            String credTypeValue = entity.getFieldValue(FIELD_CRED_TYPE);

            if (StringUtils.isEmpty(value) && !StringUtils.isEmpty(credTypeValue)) {
                errors.add(new StateReportValidationError(entity, field,
                        "If " + FIELD_CRED_TYPE + " has a value, then the " + field.getFieldId()
                                + " must also be populated",
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                FIELD_CRED_TYPE + " = " + STYLE_BOLD + credTypeValue + STYLE_END));
            }

            return errors;
        }

    }

    /**
     * Validator to check race.
     *
     * @author X2 Development Corporation
     *
     */
    private class ValidateRaceCode implements FieldValidator {
        public static final String VAL_CALC_ID = "VAL_RACE_CODE";

        private static final String RACE_1_CODE = "RACE 1 CODE";

        private Date m_priorDate;

        private Map<String, Map<String, String>> m_beansValidatedValues = new HashMap<String, Map<String, String>>();
        private List<String> m_validCodes = null;
        private List m_validRace1Codes;

        /**
         * Instantiates a new validate race code.
         */
        public ValidateRaceCode() {
            m_validCodes = Arrays.asList("I", "A", "B", "P", "W");
            m_validRace1Codes = Arrays.asList("Multiracial");

            Calendar cal = Calendar.getInstance();
            cal.set(Calendar.YEAR, 2010);
            cal.set(Calendar.MONTH, 5);
            cal.set(Calendar.DATE, 30);

            m_priorDate = cal.getTime();
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
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

            String studentOid = entity.getBean().getOid();
            Map<String, String> validatedValues = m_beansValidatedValues.get(studentOid);
            if (validatedValues == null) {
                validatedValues = new HashMap<String, String>();
                m_beansValidatedValues.put(studentOid, validatedValues);
            }

            /*
             * Each Race Code can not be used more than once
             */
            if (!StringUtils.isEmpty(value)) {
                if (validatedValues.keySet().contains(value) &&
                        !validatedValues.get(value).equals(field.getFieldId())) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Each Race Code can not be used more than once",
                            validatedValues.get(value) + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                    field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
                }
                validatedValues.put(value, field.getFieldId());
            }

            if (!StringUtils.isEmpty(value) && !m_validCodes.contains(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " must be valid code " + m_validCodes,
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
            }

            /*
             * Race Codes must be filled consecutively
             */
            int seqNumber = Integer.parseInt((String) field.getParameter());
            if (seqNumber != 1) {
                int prevSeqNumber = seqNumber - 1;
                String previousRaceFieldId = "RACE " + prevSeqNumber + " CODE";
                if (!StringUtils.isEmpty(value) && StringUtils.isEmpty(entity.getFieldValue(previousRaceFieldId))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Race Codes must be filled consecutively",
                            previousRaceFieldId + " is empty, " +
                                    field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
                }
            }

            /*
             * School Year Ending 2010-06-30 and prior - If race code is provided in any of the
             * following fields
             * (Race 2 thru 5) and Race 1 is not Equal to "Multiracial"
             */
            if (!field.getFieldId().equals(RACE_1_CODE) && !StringUtils.isEmpty(value)
                    && !m_reportDate.after(m_priorDate)) {
                String race1 = entity.getFieldValue(RACE_1_CODE);
                if (!m_validRace1Codes.contains(race1)) {
                    errors.add(new StateReportValidationError(entity, field,
                            RACE_1_CODE + " must be valid code " + m_validRace1Codes + " if " + field.getFieldId() +
                                    " is not empty and report date 2010/06/30 and prior.",
                            RACE_1_CODE + " = " + STYLE_BOLD + race1 + STYLE_END + ", " +
                                    field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                    "Current date = " + STYLE_BOLD + m_formatter.format(m_reportDate) + STYLE_END));
                }
            }

            return errors;
        }

    }
    /**
     * Validator to check required fields.
     *
     * @author X2 Development Corporation
     *
     */
    private class ValidateRequiredField implements FieldValidator {
        public static final String VAL_CALC_ID = "VAL_REQ_FIELD";

        /**
         * Instantiates a new validate required field.
         */
        public ValidateRequiredField() {
            // stub
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
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

            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "This field is required.",
                        "Field " + STYLE_BOLD + field.getFieldId() + STYLE_END + " is empty."));
            }

            return errors;
        }

    }

    /**
     * Input Definition Parameters
     */
    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";
    protected static final String PARAM_REMOVE_HEADER = "removeHeader";
    protected static final String REPORT_DATE_PARAM = "reportDate";

    /**
     * Retriever Parameters
     */
    protected static final String CALC_ID_STDLITE_DIPLOMA = "STDLITE-DIPLOMA";
    protected static final String CALC_ID_STDLITE_DISTSKL = "STDLITE-DISTSKL";
    protected static final String CALC_ID_STDLITE_GUARDIAN = "STDLITE-GUARDIAN";
    protected static final String CALC_ID_STDLITE_RACE = "STDLITE-RACE";
    protected static final String CALC_ID_STDLITE_STDSTATUS = "STDLITE-STDSTATUS";
    protected static final String CALC_ID_GUIDANCE_COUNSELOR_CODE = "GUID-COUNS";
    protected static final String CALC_ID_COUNSELOR = "COUNSELOR";
    protected static final String CALC_PARAM_ALTERNATE = "ALTERNATE";
    protected static final String CALC_PARAM_GRADE_LEVEL = "GRADE-LEVEL";
    protected static final String CALC_PARAM_HOME_DISTRICT = "HOME-DISTRICT";
    protected static final String CALC_PARAM_LOCATION_CODE = "LOCATION-CODE";
    protected static final String CALC_PARAM_MIDDLE_INITIAL = "MIDDLE-INITIAL";
    protected static final String CALC_PARAM_PRIMARY = "PRIMARY";
    protected static final String CALC_PARAM_PROGRAM_LEP = "LEP";
    protected static final String CALC_PARAM_SERV_DISTRICT = "SERV-DISTRICT";
    protected static final String CALC_PARAM_SERV_SCHOOL = "SERV-SCHOOL";
    protected static final String CALC_PARAM_STD_LAST_STATUS_DATE = "LAST-STATUS-DATE";
    protected static final String CALC_PARAM_STD_LEP_PARTICIPATION = "STD-LEP-PARTICIPATION";
    protected static final String CALC_PARAM_STD_STATUS = "STD-STATUS";

    /**
     * Aliases
     */
    // SCHOOL
    protected static final String ALIAS_BEDS_CODE = "BEDS CODE";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_FEEDER_SCHOOL = "DOE FEEDER SCHOOL";
    protected static final String ALIAS_HIGHEST_GRADE = "HIGHEST GRADE";
    protected static final String ALIAS_LOCATION_CODE = "LOCATION CODE";

    // STUDENT
    protected static final String ALIAS_FIRST_US_DAY = "FIRST US DAY";
    protected static final String ALIAS_INIT_US_ENTRY_DATE = "DOE INITIAL US ENTRY DATE";
    protected static final String ALIAS_GUIDANCE_COUNSELORS = "all-std-GuidanceCounselor";

    // STUDENT_CONTACT
    protected static final String ALIAS_GUARDIAN = "DOE GUARDIAN";

    // STUDENT_ENROLLMENT
    protected static final String ALIAS_ENR_RES_DISTRICT = "DOE DISTRICT RESD";
    protected static final String ALIAS_GRADE_LEVEL = "DOE GRADE LEVELS";
    protected static final String ALIAS_GRADE_LEVEL_OVERRIDE = "DOE GRADE LEVEL OVERRIDE";
    protected static final String ALIAS_HOME_DISTRICT = "DOE DISTRICT HOME";
    protected static final String ALIAS_LOCATION_OVERRIDE = "DOE LOCATION OVERRIDE";
    protected static final String ALIAS_SERVE_DISTRICT = "DOE DISTRICT SERVE";
    protected static final String ALIAS_SERVE_SCHOOL = "DOE SCHOOL SERVE";
    protected static final String ALIAS_GUIDANCE_COUNSELOR_DISTRICT = "all-std-GuidanceCounselorDistrictCodeOverride";

    /**
     * Other Constants
     */
    protected static final String CODE_ENROLL_STATUS_PRE_REG = "PreReg";
    protected static final String CODE_ENROLL_STATUS_PRE_ENROLL = "Pre-enroll";

    protected static final String CODE_GRADE_THREE = "03";
    protected static final String CODE_PROGRAM_LEP = "LEP";
    protected static final String STATE_CODE_NY = "NY";
    protected static final String STATE_CODE_POLIO = "POLIO";
    protected static final String STUDENT_STATUS_ACTIVE = "A";
    protected static final String STUDENT_STATUS_INACTIVE = "I";

    /**
     * Local Variables
     */
    protected boolean m_excludeSchoolIndicator = false;
    protected SimpleDateFormat m_exportFormat = new SimpleDateFormat("yyyy-MM-dd");
    protected boolean m_removeHeaderIndicator;
    protected int m_schoolYear;
    protected Map m_excludeSchoolMap;
    protected Collection<DistrictSchoolYearContext> m_contextList;
    protected Map<String, ReferenceCode> m_raceCodes;
    protected PlainDate m_reportDate;
    protected PlainDate m_schoolYearEndDate;
    protected PlainDate m_schoolYearStartDate;
    protected SimpleDateFormat m_formatter = new SimpleDateFormat("MM/dd/yyyy");
    protected String m_enrLocationCode;
    protected String m_enrLocationOverride;
    protected String m_enrResidentDistrict;
    protected String m_enrollmentGradeLevel;
    protected String m_enrollmentGradeLevelOverride;
    protected String m_excludeSchool;
    protected String m_firstUSDay;
    protected String m_homeDistrict;
    protected String m_servDistrict;
    protected String m_servSchool;
    protected String m_guardian;
    protected String m_guidanceCounselor;
    protected String m_guidanceCounselorDistrictOverride;
    protected StudentHistoryHelper m_helper;

    /**
     * Calculate a Student previous Grave using Enrollment spans.
     *
     * @param entryEnrollment StudentEnrollment
     * @param withdrawEnrollment StudentEnrollment
     * @return String
     */
    public String calcPreviousGradeLevel(StudentEnrollment entryEnrollment, StudentEnrollment withdrawEnrollment) {
        String gradeLevel = null;

        String previousGradeLevelOverRide =
                (String) entryEnrollment.getFieldValueByBeanPath(m_enrollmentGradeLevelOverride);
        String previousGradeLevel = (String) entryEnrollment.getFieldValueByBeanPath(m_enrollmentGradeLevel);
        String currentGradeLevelOverRide =
                (String) withdrawEnrollment.getFieldValueByBeanPath(m_enrollmentGradeLevelOverride);
        String currentGradeLevel = (String) withdrawEnrollment.getFieldValueByBeanPath(m_enrollmentGradeLevel);

        if (!StringUtils.isEmpty(previousGradeLevelOverRide)) {
            gradeLevel = previousGradeLevelOverRide;
        } else if (!StringUtils.isEmpty(previousGradeLevel)) {
            gradeLevel = previousGradeLevel;
        } else if (!StringUtils.isEmpty(currentGradeLevelOverRide)) {
            gradeLevel = currentGradeLevelOverRide;
        } else if (!StringUtils.isEmpty(currentGradeLevel)) {
            // Calculate backwards form current YOG.
            if (StringUtils.isNumeric(currentGradeLevel)) {
                int oldYog = entryEnrollment.getYog();
                int newYog = withdrawEnrollment.getYog();
                int differenceInYears = newYog - oldYog;

                int currentGrade = Integer.parseInt(currentGradeLevel);
                int previousGrade = currentGrade + differenceInYears;

                gradeLevel = String.format("%02d", Integer.valueOf(previousGrade));
            } else {
                gradeLevel = currentGradeLevel;
            }
        } else {
            gradeLevel = calcGradeLevelBasedOnYog(entryEnrollment.getYog(), m_schoolYear, false);
        }

        return gradeLevel;
    }

    /**
     * Gets the heading.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        if (m_removeHeaderIndicator) {
            return null;
        }

        return super.getHeading();
    }

    /**
     * Checks for school to be in the map of excluded schools, or that the map is null.
     *
     * @param schoolOid String
     * @return true, if successful
     */
    public boolean includeSchool(String schoolOid) {
        return (m_excludeSchoolMap == null) || !m_excludeSchoolMap.containsKey(schoolOid);
    }

    /**
     * Initialize the data module.
     */
    @Override
    public void initialize() {
        initializeFields();

        /*
         * Build helper object.
         */
        m_helper = new NYStudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.TRUE);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            loadSchoolExcludeMap();

            loadDistrictSchoolYearContextList();

            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(NYStudentLiteEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALC_ID_STDLITE_DISTSKL, new RetrieveEnrollment());
            calcs.put(CALC_ID_STDLITE_GUARDIAN, new RetrieveGuardian());
            calcs.put(CALC_ID_STDLITE_RACE, new RetrieveRace());
            calcs.put(CALC_ID_STDLITE_STDSTATUS, new RetrieveStudentInfo());
            calcs.put(CALC_ID_COUNSELOR, new RetrieveGuidanceCounselor());
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldRetriever>();
            validators.put(CALC_ID_STDLITE_DIPLOMA, new ValidateDiplomaCode());
            validators.put(ValidateBirthdate.VAL_CALC_ID, new ValidateBirthdate());
            validators.put(ValidateEntryGrade9.VAL_CALC_ID, new ValidateEntryGrade9());
            validators.put(ValidateHispIndicator.VAL_CALC_ID, new ValidateHispIndicator());
            validators.put(ValidateRaceCode.VAL_CALC_ID, new ValidateRaceCode());
            validators.put(ValidateRequiredField.VAL_CALC_ID, new ValidateRequiredField());
            validators.put(ValidateHomelessRes.VAL_CALC_ID, new ValidateHomelessRes());
            validators.put(ValidateCareerPath.VAL_CALC_ID, new ValidateCareerPath());
            validators.put(ValidateLastStatusDate.VAL_CALC_ID, new ValidateLastStatusDate());
            validators.put(ValidateHomelessInd.VAL_CALC_ID, new ValidateHomelessInd());
            validators.put(ValidatePostGrad.VAL_CALC_ID, new ValidatePostGrad());
            super.addValidators(validators);
        }
    }

    /**
     * Initialize fields.
     */
    public void initializeFields() {
        // System Parameters
        m_schoolYear = getOrganization().getCurrentContext().getSchoolYear();
        m_schoolYearStartDate = getOrganization().getCurrentContext().getStartDate();
        m_schoolYearEndDate = getOrganization().getCurrentContext().getEndDate();

        // Load Parameters
        m_reportDate = new PlainDate();
        if (getParameter(REPORT_DATE_PARAM) != null) {
            m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);
        }
        m_removeHeaderIndicator = false;
        if (getParameter(PARAM_REMOVE_HEADER) != null) {
            m_removeHeaderIndicator = ((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue();
        }
        m_excludeSchoolIndicator = false;
        if (getParameter(PARAM_EXCLUDE_SCHOOL) != null) {
            m_excludeSchoolIndicator = ((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue();
        }

        // Load Alias database field Names
        m_enrollmentGradeLevel = translateAliasToJavaName(ALIAS_GRADE_LEVEL, true);
        m_enrollmentGradeLevelOverride = translateAliasToJavaName(ALIAS_GRADE_LEVEL_OVERRIDE, true);
        m_enrLocationOverride = translateAliasToJavaName(ALIAS_LOCATION_OVERRIDE, true);
        m_enrLocationCode = translateAliasToJavaName(ALIAS_LOCATION_CODE, true);
        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_firstUSDay = translateAliasToJavaName(ALIAS_FIRST_US_DAY, true);
        m_guardian = translateAliasToJavaName(ALIAS_GUARDIAN, true);
        m_guidanceCounselor = translateAliasToJavaName(ALIAS_GUIDANCE_COUNSELORS, true);
        m_guidanceCounselorDistrictOverride = translateAliasToJavaName(ALIAS_GUIDANCE_COUNSELOR_DISTRICT, true);
        m_homeDistrict = translateAliasToJavaName(ALIAS_HOME_DISTRICT, true);
        m_servDistrict = translateAliasToJavaName(ALIAS_SERVE_DISTRICT, true);
        m_servSchool = translateAliasToJavaName(ALIAS_SERVE_SCHOOL, true);
        m_enrResidentDistrict = translateAliasToJavaName(ALIAS_ENR_RES_DISTRICT, true);
    }

    /**
     * Using the Year of graduation at the time of enrollment and the school year,
     * determines and returns the student's grade level.
     *
     * @param yog int
     * @param schoolYear int
     * @param halfDay boolean
     * @return the calculated grade level for the student.
     */
    protected String calcGradeLevelBasedOnYog(int yog, int schoolYear, boolean halfDay) {
        int temp = 12 - (yog - schoolYear);
        String gradeLevel = null;
        if (temp > 0) {
            gradeLevel = Integer.toString(temp);

            if (gradeLevel.length() == 1) {
                gradeLevel = '0' + gradeLevel;
            }
        } else if (temp == 0) {
            gradeLevel = "K";
        } else if (temp < 0) {
            gradeLevel = "PK";
        }
        // else if ( temp == -2)
        // {
        // gradeLevel="PKH";
        // }
        if (temp < 1) {
            if (halfDay) {
                gradeLevel += "H";
            } else {
                gradeLevel += "F";
            }
        }

        return gradeLevel;
    }

    /**
     * Gets the years diff.
     *
     * @param firstDate Date
     * @param secondDate Date
     * @return int
     */
    protected int getYearsDiff(Date firstDate, Date secondDate) {
        Calendar cal1 = Calendar.getInstance();
        cal1.setTime(firstDate);

        Calendar cal2 = Calendar.getInstance();
        cal2.setTime(secondDate);

        return cal2.get(Calendar.YEAR) - cal1.get(Calendar.YEAR);
    }

    /**
     * Build a collection of schools years.
     */
    private void loadDistrictSchoolYearContextList() {
        X2Criteria districtSchoolYearContextCriteria = new X2Criteria();

        BeanQuery districtSchoolYearContextQuery =
                new BeanQuery(DistrictSchoolYearContext.class, districtSchoolYearContextCriteria);
        districtSchoolYearContextQuery.addOrderByDescending(DistrictSchoolYearContext.COL_SCHOOL_YEAR);

        m_contextList = getBroker().getCollectionByQuery(districtSchoolYearContextQuery);
    }

    /**
     * Build a map of schools with the alias set to exclude school.
     */
    private void loadSchoolExcludeMap() {
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(m_excludeSchool, BooleanAsStringConverter.TRUE);

        BeanQuery query = new BeanQuery(School.class, schoolCriteria);

        m_excludeSchoolMap = getBroker().getGroupedCollectionByQuery(query, X2BaseBean.COL_OID, 128);
    }
}
