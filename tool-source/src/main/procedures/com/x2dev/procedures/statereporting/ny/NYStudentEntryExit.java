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

package com.x2dev.procedures.statereporting.ny;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * New York state procedure for EntryExit export.
 *
 * @author X2 Development Corporation
 */

public class NYStudentEntryExit extends StateReportData {


    /**
     * Entity class for Entry Exit export.
     *
     * @author X2 Development Corporation
     */
    public static class EntryExitEntity extends StateReportEntity {
        /**
         * Entity instance variables.
         */
        private List<StudentEnrollmentSpan> m_enrollmentSpans = new ArrayList<StudentEnrollmentSpan>();
        private NYStudentEntryExit m_data;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public EntryExitEntity() {
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
            m_data = (NYStudentEntryExit) data;
            SisStudent student = (SisStudent) bean;
            // Selecting only the enrollment spans that are after school's start date

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

            String lastYOGChangeCode = null;
            m_previousYOGEnrollCodeMap = new HashMap();

            // Include EnrollmentSpans that were Active in the previous School Year
            int studentSpanCount = 0;
            for (StudentEnrollmentSpan span : enrollmentSpans) {
                String schoolId = span.getSchool().getSchoolId();

                StudentEnrollment entryEnrollment = span.getFirstActiveEnrollment();
                StudentEnrollment withdrawEnrollment = span.getFirstInactiveEnrollment();

                PlainDate entryEnrollmentDate = null;
                if (entryEnrollment != null) {
                    entryEnrollmentDate = entryEnrollment.getEnrollmentDate();
                }
                PlainDate withdrawEnrollmentDate = null;
                if (withdrawEnrollment != null) {
                    withdrawEnrollmentDate = withdrawEnrollment.getEnrollmentDate();
                }

                // Include if student's school in school list
                // and their first Active enrollment was after the start of the school year
                // or their first Inactive enrollment was within the school year
                if (m_data.includeSchool(schoolId)
                        && ((entryEnrollmentDate != null)
                                && (!entryEnrollmentDate.before(m_data.m_schoolYearStartDate)
                                        && (withdrawEnrollmentDate == null))
                                || ((withdrawEnrollmentDate != null)
                                        && (withdrawEnrollmentDate.after(m_data.m_schoolYearStartDate)
                                                && (entryEnrollmentDate.before(m_data.m_schoolYearEndDate)))))) {
                    // Exclude spans with the Enrollment status of Pre-enroll and PreReg
                    if (!(CODE_ENROLL_STATUS_PRE_ENROLL.equalsIgnoreCase(entryEnrollment.getStatusCode())
                            || CODE_ENROLL_STATUS_PRE_REG.equalsIgnoreCase(entryEnrollment.getStatusCode()))) {
                        // If YOG_CHANGE then assign the Enrollment code of the previous Span
                        lastYOGChangeCode = addYOGChangeCode(span, lastYOGChangeCode);

                        m_enrollmentSpans.add(span);

                        studentSpanCount++;
                    }
                }
            }

            // If no EnrollmentSpans were Active then include the most recent.
            if (studentSpanCount == 0) {
                StudentEnrollmentSpan lastSpan = null;
                for (StudentEnrollmentSpan span : enrollmentSpans) {
                    lastSpan = span;
                }

                if (lastSpan != null) {
                    String schoolId = lastSpan.getSchool().getSchoolId();

                    StudentEnrollment entryEnrollment = lastSpan.getFirstActiveEnrollment();

                    if (m_data.includeSchool(schoolId)) {
                        if (!(CODE_ENROLL_STATUS_PRE_ENROLL.equalsIgnoreCase(entryEnrollment.getStatusCode())
                                || CODE_ENROLL_STATUS_PRE_REG.equalsIgnoreCase(entryEnrollment.getStatusCode()))) {
                            // If YOG_CHANGE then assign the Enrollment code of the previous Span
                            lastYOGChangeCode = addYOGChangeCode(lastSpan, lastYOGChangeCode);

                            m_enrollmentSpans.add(lastSpan);

                            studentSpanCount++;
                        }
                    }
                }
            }

            setRowCount(m_enrollmentSpans.size());
        }

        /**
         * Save the previous StudentEnrollmentSpan YOC Code for retriever by the following
         * StudentEnrollmentSpan.
         *
         * @param span StudentEnrollmentSpan
         * @param lastYOGChangeCode String
         * @return String
         */
        public String addYOGChangeCode(StudentEnrollmentSpan span, String lastYOGChangeCode) {
            StudentEnrollment entryEnrollment = span.getFirstActiveEnrollment();
            StudentEnrollment withdrawEnrollment = span.getFirstInactiveEnrollment();
            if (entryEnrollment != null && StudentEnrollment.ENTRY.equals(entryEnrollment.getEnrollmentType())
                    && withdrawEnrollment != null
                    && StudentEnrollment.YOG_CHANGE.equals(withdrawEnrollment.getEnrollmentType())) {
                lastYOGChangeCode = entryEnrollment.getEnrollmentCode();
            }
            if (entryEnrollment != null && StudentEnrollment.YOG_CHANGE.equals(entryEnrollment.getEnrollmentType())) {
                if (lastYOGChangeCode != null) {
                    m_previousYOGEnrollCodeMap.put(entryEnrollment.getOid(), lastYOGChangeCode);
                }
            }

            return lastYOGChangeCode;
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
            StudentEnrollmentSpan span = getEnrollmentSpan();
            if (span != null) {
                name += span.getSchool().getName();
            }

            return name;
        }

        /**
         * Return the LastActiveEnrollment.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getLastActiveEnrollment() {
            return m_enrollmentSpans.get(m_enrollmentSpans.size() - 1).getFirstActiveEnrollment();
        }

        /**
         * Returns the current student.
         *
         * @return SisStudent
         */
        public SisStudent getStudent() {
            return (SisStudent) getBean();
        }

        /**
         * Gets the withdraw enroll date.
         *
         * @return Plain date
         */
        public PlainDate getWithdrawEnrollDate() {
            StudentEnrollmentSpan span = getEnrollmentSpan();
            StudentEnrollment withdrawEnrollment = span.getFirstInactiveEnrollment();
            PlainDate withdrawalDate = null;

            if (withdrawEnrollment != null) {
                if (!StringUtils.isEmpty(withdrawEnrollment.getEnrollmentCode())) {
                    PlainDate withdrawlEnrollmentDate = withdrawEnrollment.getEnrollmentDate();
                    if (withdrawlEnrollmentDate.before(m_data.m_schoolYearEndDate) ||
                            withdrawlEnrollmentDate.equals(m_data.m_schoolYearEndDate)) {
                        withdrawalDate = span.getLastActiveDate();
                    }
                }
            }
            return withdrawalDate;
        }

        /**
         * Return true if the following conditions are met:
         * a) The "End of School Report" checkbox is true,
         * b) The student nextSchoolOid is not empty, and
         * c) The student schoolOid is different than nexSchoolOid.
         *
         * @return boolean
         */
        public boolean reportStudentEndSchool() {
            boolean reportStudentEndSchool = m_data.m_endOfSchoolReport &&
                    !StringUtils.isEmpty(getStudent().getNextSchoolOid()) &&
                    !getStudent().getSchoolOid().equals(getStudent().getNextSchoolOid());

            return reportStudentEndSchool;
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
     * The Class EnrollmentRetriever.
     */
    protected class EnrollmentRetriever implements FieldRetriever {

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
            Object value = null;
            String parameter = (String) field.getParameter();
            EntryExitEntity enrEntity = ((EntryExitEntity) entity);
            StudentEnrollmentSpan span = enrEntity.getEnrollmentSpan();
            SisStudent student = enrEntity.getStudent();
            String studentOid = student.getOid();

            if (CALC_PARAM_DOE_DISTRICT.equals(parameter)) {
                StudentEnrollment enrollment = span.getFirstActiveEnrollment();
                if (enrollment != null) {
                    value = enrollment.getFieldValueByBeanPath(m_servDistrict);
                }
            } else if (CALC_PARAM_DOE_SCHOOL.equals(parameter)) {
                StudentEnrollment enrollment = span.getFirstActiveEnrollment();
                if (enrollment != null) {
                    value = enrollment.getFieldValueByBeanPath(m_servSchool);
                }
            } else if (CALC_PARAM_SCHOOL_ENTRY_DATE.equals(parameter)) {
                StudentEnrollment entryEnrollment = span.getFirstActiveEnrollment();
                if (entryEnrollment != null && entryEnrollment.getEnrollmentDate() != null) {
                    PlainDate entryDate = entryEnrollment.getEnrollmentDate();

                    if (StudentEnrollment.YOG_CHANGE.equals(entryEnrollment.getEnrollmentType())) {
                        entryDate = DateUtils.add(entryDate, 1);
                    }

                    value = entryDate;
                    if (entryDate.before(m_schoolYearStartDate)) {
                        value = m_schoolYearStartDate;
                    }
                }
            } else if (CALC_PARAM_SCHOOL_ENTRY_CODE.equals(parameter)) {
                StudentEnrollment entryEnrollment = span.getFirstActiveEnrollment();

                if (entryEnrollment != null) {
                    String code = entryEnrollment.getEnrollmentCode();

                    // If this is the Start of YOG span then use the previous Enrollment Code
                    if (StudentEnrollment.YOG_CHANGE.equals(entryEnrollment.getEnrollmentType())) {
                        String enrollmentOid = entryEnrollment.getOid();
                        if (m_previousYOGEnrollCodeMap != null
                                && m_previousYOGEnrollCodeMap.containsKey(enrollmentOid)) {
                            code = (String) m_previousYOGEnrollCodeMap.get(enrollmentOid);
                        }
                    }

                    value = code;
                    if (code != null) {
                        String stateCode = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                                StudentEnrollment.COL_ENROLLMENT_CODE, code,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                        if (stateCode != null) {
                            value = stateCode;
                        }
                    }
                }
            } else if (CALC_PARAM_SCHOOL_ENTRY_COMMENT.equals(parameter)) {
                StudentEnrollment enrollment = span.getFirstActiveEnrollment();
                if (enrollment != null) {
                    value = enrollment.getFieldValueByBeanPath(m_enrollmentEntryComment);
                }
            } else if (CALC_PARAM_GRADE_LEVEL_ENROLLMENT.equals(parameter)) {
                StudentEnrollment entryEnrollment = span.getFirstActiveEnrollment();
                StudentEnrollment withdrawEnrollment = span.getFirstInactiveEnrollment();
                StudentEnrollment yogEnrollment =
                        m_helper.getEnrollmentForDate(studentOid, m_reportDate, StudentEnrollment.YOG_CHANGE);
                String gradeLevel = null;

                if (entryEnrollment != null && StudentEnrollment.ENTRY.equals(entryEnrollment.getEnrollmentType())
                        && withdrawEnrollment != null
                        && StudentEnrollment.YOG_CHANGE.equals(withdrawEnrollment.getEnrollmentType())) {
                    // Take the GradeLevel form the Entry Enrollment GradeLevelOverRide
                    // the withdraw part of YOG split.
                    // calculate grade form the spans enrollment
                    gradeLevel = calcPreviousGradeLevel(entryEnrollment, withdrawEnrollment);
                    gradeLevel = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_enrollmentGradeLevel,
                            gradeLevel, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (StringUtils.isEmpty(gradeLevel) && yogEnrollment != null
                        && !m_schoolYearStartDate.after(yogEnrollment.getEnrollmentDate())) {
                    // If there is YOG Enrollment and its date is before the start of the school
                    // year, take it's GradeLevel.
                    gradeLevel = (String) yogEnrollment.getFieldValueByBeanPath(m_enrollmentGradeLevel);
                    gradeLevel = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_enrollmentGradeLevel,
                            gradeLevel, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (entryEnrollment != null && StringUtils.isEmpty(gradeLevel)) {
                    // Take the GradeLevel form the Entry Enrollment GradeLevelOverRide
                    gradeLevel = (String) entryEnrollment.getFieldValueByBeanPath(m_enrollmentGradeLevelOverride);
                    gradeLevel =
                            data.lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_enrollmentGradeLevelOverride,
                                    gradeLevel, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (entryEnrollment != null && StringUtils.isEmpty(gradeLevel)) {
                    // Take the GradeLevel form the Entry Enrollment GradeLevel
                    gradeLevel = (String) entryEnrollment.getFieldValueByBeanPath(m_enrollmentGradeLevel);
                    gradeLevel = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_enrollmentGradeLevel,
                            gradeLevel, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (StringUtils.isEmpty(gradeLevel)) {
                    // Take the GradeLevel form the Student
                    gradeLevel = ((SisStudent) entity.getBean()).getGradeLevel();
                    gradeLevel = data.lookupReferenceCodeByBeanPath(SisStudent.class, SisStudent.COL_GRADE_LEVEL,
                            gradeLevel, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                value = gradeLevel;
            } else if (CALC_PARAM_SCHOOL_WITHDRAWAL_DATE.equals(parameter)) {
                value = enrEntity.getWithdrawEnrollDate();

                // If student has not withdrawal enrollment date and reportStudentEndSchool()
                // returns true,
                // use last day of the district context.
                if (enrEntity.reportStudentEndSchool() && value == null) {
                    value = getCurrentContext().getEndDate();
                }
            } else if (CALC_PARAM_SCHOOL_WITHDRAWAL_CODE.equals(parameter)) {
                StudentEnrollment withdrawEnrollment = span.getFirstInactiveEnrollment();
                value = STRING_EMPTY;

                if (withdrawEnrollment != null) {
                    String code = withdrawEnrollment.getEnrollmentCode();

                    value = code;
                    if (code != null) {
                        String stateCode = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                                StudentEnrollment.COL_ENROLLMENT_CODE, code,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                        if (stateCode != null) {
                            value = stateCode;
                        }
                    }
                }

                // If student has not withdrawal enrollment date and reportStudentEndSchool()
                // returns true use
                // CODE_END_CURRENT_SCHOOL.
                if (enrEntity.reportStudentEndSchool() && enrEntity.getWithdrawEnrollDate() == null) {
                    value = CODE_END_CURRENT_SCHOOL;
                }
            } else if (CALC_PARAM_SCHOOL_WITHDRAWAL_COMMENT.equals(parameter)) {
                StudentEnrollment enrollment = span.getFirstInactiveEnrollment();
                if (enrollment != null) {
                    value = enrollment.getFieldValueByBeanPath(m_enrollmentEntryComment);
                }
            } else if (CALC_PARAM_LOCATION_CODE.equals(parameter)) {
                StudentEnrollment enrollment = span.getFirstActiveEnrollment();
                if (enrollment != null) {
                    value = enrollment.getFieldValueByBeanPath(m_enrollmentLocationOverride);
                }
                if (value == null) {
                    value = enrollment.getSchool().getFieldValueByBeanPath(m_enrollmentLocationCode);
                }
                if (value == null) {
                    School school = ((SisStudent) entity.getBean()).getSchool();
                    value = school.getFieldValueByBeanPath(m_enrollmentLocationCode);
                }
            }

            return value;
        }
    }

    /**
     * Validates entry type code.
     *
     * @author Follett Software Company
     */
    private class EntryCodeValidator implements FieldValidator {
        public static final String VAL_ID = "VAL_ENTRY_CODE";

        private List<String> m_validGdCodes = null;

        /**
         * Instantiates a new entry code validator.
         */
        public EntryCodeValidator() {
            m_validGdCodes = Arrays.asList("0011", "0022", "0033", "4034", "5544", "5555", "5654", "5905", "7000",
                    "7011", "8294");
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
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if ("0033".equals(value) || "8294".equals(value)) {
                String exitCode = entity.getFieldValue(FIELD_ID_SCHOOL_EXIT_CODE);
                if ("799".equals(exitCode) || "085".equals(exitCode)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Invalid Enrollment Exit Type for Enrollment Entry Type",
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                    FIELD_ID_SCHOOL_EXIT_CODE + " = " + STYLE_BOLD + exitCode + STYLE_END));
                }
            }

            if ("0055".equals(value)) {
                String exitCode = entity.getFieldValue(FIELD_ID_SCHOOL_EXIT_CODE);
                if (!"0066".equals(exitCode)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "A 0055 state entry code and 0066 state exit code can only be paired with one another",
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                    FIELD_ID_SCHOOL_EXIT_CODE + " = " + STYLE_BOLD + exitCode + STYLE_END));
                }
            }

            if ("4034".equals(value)) {
                String gradeLevel = entity.getFieldValue(FIELD_ID_GRADE_LEVEL);
                if (!"PS".equals(gradeLevel)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + " of 4034 requires a PRES grade ordinal",
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                    FIELD_ID_GRADE_LEVEL + " = " + STYLE_BOLD + gradeLevel + STYLE_END));
                }
                String exitCode = entity.getFieldValue(FIELD_ID_SCHOOL_EXIT_CODE);
                if (!"140".equals(exitCode)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "4034 state entry code and 140 state exit code can only be paired with one another",
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                    FIELD_ID_SCHOOL_EXIT_CODE + " = " + STYLE_BOLD + exitCode + STYLE_END));
                }
            }

            if ("5555".equals(value)) {
                String exitCode = entity.getFieldValue(FIELD_ID_SCHOOL_EXIT_CODE);
                if (!"8228".equals(exitCode)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "5555 state entry code and 8228 state exit code can only be paired with one another",
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                    FIELD_ID_SCHOOL_EXIT_CODE + " = " + STYLE_BOLD + exitCode + STYLE_END));
                }
            }

            if (m_validGdCodes.contains(value)) {
                String gradeLevel = entity.getFieldValue(FIELD_ID_GRADE_LEVEL);
                if ("GD".equals(gradeLevel)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + " of " + m_validGdCodes + " cannot have a GED grade ordinal",
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                    FIELD_ID_GRADE_LEVEL + " = " + STYLE_BOLD + gradeLevel + STYLE_END));
                }
            }

            return errors;
        }
    }

    /**
     * Validates entry date.
     *
     * @author Follett Software Company
     */
    private class EntryDateValidator implements FieldValidator {
        public static final String VAL_ID = "VAL_ENTRY_DATE";

        private PlainDate m_july01;
        private PlainDate m_june30;

        /**
         * Instantiates a new entry date validator.
         */
        public EntryDateValidator() {
            Calendar julyCal = Calendar.getInstance();
            julyCal.set(Calendar.DATE, 1);
            julyCal.set(Calendar.MONTH, 6);
            julyCal.set(Calendar.YEAR, getCurrentContext().getSchoolYear() - 1);
            m_july01 = new PlainDate(julyCal.getTime());

            Calendar juneCal = Calendar.getInstance();
            juneCal.set(Calendar.DATE, 30);
            juneCal.set(Calendar.MONTH, 5);
            juneCal.set(Calendar.YEAR, getCurrentContext().getSchoolYear());
            m_june30 = new PlainDate(juneCal.getTime());
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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            Date entryDate = null;

            try {
                entryDate = m_exportFormat.parse(value);
            } catch (ParseException e) {
                e.printStackTrace();
            }

            if (entryDate != null) {
                if (entryDate.before(m_july01) || entryDate.after(m_june30)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + " must fall between " + m_exportFormat.format(m_july01) +
                                    " and " + m_exportFormat.format(m_june30),
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
                }
            }

            return errors;
        }

    }

    /**
     * Validates exit date.
     *
     * @author Follett Software Company
     */
    private class ExitDateValidator implements FieldValidator {
        public static final String VAL_ID = "VAL_EXIT_DATE";

        private PlainDate m_july01;
        private PlainDate m_june30;

        private Map<String, Collection<CrossPeriod>> m_studentPeriodsMap = null;

        /**
         * Instantiates a new exit date validator.
         */
        public ExitDateValidator() {
            Calendar julyCal = Calendar.getInstance();
            julyCal.set(Calendar.DATE, 1);
            julyCal.set(Calendar.MONTH, 6);
            julyCal.set(Calendar.YEAR, getCurrentContext().getSchoolYear() - 1);
            m_july01 = new PlainDate(julyCal.getTime());

            Calendar juneCal = Calendar.getInstance();
            juneCal.set(Calendar.DATE, 30);
            juneCal.set(Calendar.MONTH, 5);
            juneCal.set(Calendar.YEAR, getCurrentContext().getSchoolYear());
            m_june30 = new PlainDate(juneCal.getTime());

            m_studentPeriodsMap = new HashMap<String, Collection<CrossPeriod>>();
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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String entryDateValue = entity.getFieldValue(FIELD_ID_SCHOOL_ENTRY_DATE);

            Date entryDate = null;
            Date exitDate = null;

            try {
                entryDate = m_exportFormat.parse(entryDateValue);
                if (!StringUtils.isEmpty(value)) {
                    exitDate = m_exportFormat.parse(value);
                }

            } catch (ParseException e) {
                e.printStackTrace();
            }

            if (exitDate != null) {
                if (exitDate.before(m_july01) || exitDate.after(m_june30)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + " must fall between " + m_exportFormat.format(m_july01) +
                                    " and " + m_exportFormat.format(m_june30),
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
                }
                if (exitDate.after(m_reportDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + " cannot be a future date",
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                    ", Report Date = " + STYLE_BOLD + m_exportFormat.format(m_reportDate) + STYLE_END));
                }
                if (exitDate.before(entryDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + " cannot be a earlier than " + FIELD_ID_SCHOOL_ENTRY_DATE,
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                    ", " + FIELD_ID_SCHOOL_ENTRY_DATE + " = " + STYLE_BOLD + entryDateValue
                                    + STYLE_END));
                }
            } else {
                String exitCode = entity.getFieldValue(FIELD_ID_SCHOOL_EXIT_CODE);
                if (!StringUtils.isEmpty(exitCode)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + " is missing and " + FIELD_ID_SCHOOL_EXIT_CODE + " exists",
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                    ", " + FIELD_ID_SCHOOL_EXIT_CODE + " = " + STYLE_BOLD + exitCode + STYLE_END));
                }

                EntryExitEntity eeEntity = (EntryExitEntity) entity;

                if (eeEntity.reportStudentEndSchool()) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + " and " + FIELD_ID_SCHOOL_EXIT_CODE
                                    + " required (End of School Report)",
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                    ", " + FIELD_ID_SCHOOL_EXIT_CODE + " = " + STYLE_BOLD + exitCode + STYLE_END));
                }
            }

            // Overlapping periods check
            String studentOid = entity.getBean().getOid();
            Collection<CrossPeriod> studentPeriods = m_studentPeriodsMap.get(studentOid);
            if (studentPeriods == null) {
                studentPeriods = new ArrayList<CrossPeriod>();
                m_studentPeriodsMap.put(studentOid, studentPeriods);
            }
            PlainDate plainEntryDate = new PlainDate(entryDate);
            PlainDate plainExitDate = (exitDate == null ? null : new PlainDate(exitDate));
            CrossPeriod newPeriod = new CrossPeriod(plainEntryDate, plainExitDate);

            CrossPeriod crossedPeriod = null;

            for (CrossPeriod curPeriod : studentPeriods) {
                if (curPeriod.isCross(newPeriod)) {
                    crossedPeriod = curPeriod;
                }
            }

            if (crossedPeriod != null && crossedPeriod.equals(newPeriod)) {
                String exitCode = entity.getFieldValue(FIELD_ID_SCHOOL_EXIT_CODE);
                if (!"0055".equals(exitCode)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "With the exception of \"0055\" enrollment, any situation in which a student has 2 or more "
                                    + "records with the same entry or same exit date will be seen as an error",
                            FIELD_ID_SCHOOL_ENTRY_DATE + " = " + STYLE_BOLD + entryDateValue + STYLE_END + ", " +
                                    FIELD_ID_SCHOOL_EXIT_DATE + " = " + STYLE_BOLD + value + STYLE_END));
                }
            } else if (crossedPeriod != null) {
                errors.add(new StateReportValidationError(entity, field,
                        "Multiple entry/exit records exist with overlapping dates",
                        "Current period: " + newPeriod.getStart() + " - " + newPeriod.getEnd() + ", " +
                                "overlapping period: " + crossedPeriod.getStart() + " - " + crossedPeriod.getEnd()));
            }

            return errors;
        }

    }

    /**
     * Validates exit type code.
     *
     * @author Follett Software Company
     */
    private class ExitCodeValidator implements FieldValidator {
        public static final String VAL_ID = "VAL_EXIT_CODE";

        private PlainDate m_june01;
        private PlainDate m_june30;
        private Map<String, ArrayList<ExitCodeValidatingFields>> m_studentValidatingFields;

        /**
         * Instantiates a new exit code validator.
         */
        public ExitCodeValidator() {
            Calendar june30Cal = Calendar.getInstance();
            june30Cal.set(Calendar.DATE, 30);
            june30Cal.set(Calendar.MONTH, 5);
            june30Cal.set(Calendar.YEAR, getCurrentContext().getSchoolYear());
            m_june30 = new PlainDate(june30Cal.getTime());

            Calendar june01Cal = Calendar.getInstance();
            june01Cal.set(Calendar.DATE, 1);
            june01Cal.set(Calendar.MONTH, 5);
            june01Cal.set(Calendar.YEAR, getCurrentContext().getSchoolYear());
            m_june01 = new PlainDate(june01Cal.getTime());

            m_studentValidatingFields = new HashMap<String, ArrayList<ExitCodeValidatingFields>>();
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
            ExitCodeValidatingFields validatingFields = new ExitCodeValidatingFields(
                    entity.getFieldValue(FIELD_ID_LOCATION),
                    entity.getFieldValue(FIELD_ID_GRADE_LEVEL),
                    entity.getFieldValue(FIELD_ID_SCHOOL_ENTRY_DATE),
                    entity.getFieldValue(FIELD_ID_SCHOOL_ENTRY_CODE),
                    entity.getFieldValue(FIELD_ID_SCHOOL_EXIT_DATE),
                    value);

            SisStudent student = (SisStudent) entity.getBean();

            ArrayList<ExitCodeValidatingFields> previousValidatingFields =
                    m_studentValidatingFields.get(student.getOid());
            if (previousValidatingFields == null) {
                previousValidatingFields = new ArrayList<ExitCodeValidatingFields>();
                m_studentValidatingFields.put(student.getOid(), previousValidatingFields);
            }
            previousValidatingFields.add(validatingFields);

            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (StringUtils.isEmpty(value)) {
                String exitDateValue = entity.getFieldValue(FIELD_ID_SCHOOL_EXIT_DATE);
                if (!StringUtils.isEmpty(exitDateValue)) {
                    errors.add(new StateReportValidationError(entity, field,
                            FIELD_ID_SCHOOL_EXIT_DATE + " exists and " + field.getFieldId() + " is missing",
                            FIELD_ID_SCHOOL_EXIT_DATE + " = " + STYLE_BOLD + exitDateValue + STYLE_END));
                }

                EntryExitEntity eeEntity = (EntryExitEntity) entity;
                if (eeEntity.reportStudentEndSchool()) {
                    errors.add(new StateReportValidationError(entity, field,
                            FIELD_ID_SCHOOL_EXIT_DATE + " and " + field.getFieldId()
                                    + " required (End of School Report)",
                            FIELD_ID_SCHOOL_EXIT_DATE + " = " + STYLE_BOLD + exitDateValue + STYLE_END +
                                    ", " + field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
                }
            } else {
                if ("EOY".equals(value)) {
                    String exitDateValue = entity.getFieldValue(FIELD_ID_SCHOOL_EXIT_DATE);

                    Date exitDate = null;

                    try {
                        exitDate = m_exportFormat.parse(exitDateValue);
                    } catch (ParseException e) {
                        e.printStackTrace();
                    }

                    if (exitDate != null && !exitDate.equals(m_june30)) {
                        errors.add(new StateReportValidationError(entity, field,
                                field.getFieldId() + " = \"EOY\" requires " + FIELD_ID_SCHOOL_EXIT_DATE +
                                        " = " + m_exportFormat.format(m_june30),
                                FIELD_ID_SCHOOL_EXIT_DATE + " = " + STYLE_BOLD + exitDateValue + STYLE_END +
                                        ", " + field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
                    }
                }

                if ("0066".equals(value) || "799".equals(value)) {
                    String entryCode = entity.getFieldValue(FIELD_ID_SCHOOL_ENTRY_CODE);
                    if ("0066".equals(value) && !"0055".equals(entryCode)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "A 0055 state entry code and 0066 state exit code can only be paired with one another",
                                FIELD_ID_SCHOOL_ENTRY_CODE + " = " + STYLE_BOLD + entryCode + STYLE_END + ", " +
                                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
                    }

                    PlainDate dob = student.getPerson().getDob();
                    String exitDateValue = entity.getFieldValue(FIELD_ID_SCHOOL_EXIT_DATE);
                    Date exitDate = null;

                    try {
                        exitDate = m_exportFormat.parse(exitDateValue);
                    } catch (ParseException e) {
                        e.printStackTrace();
                    }

                    if (exitDate != null && dob != null) {
                        int studentAge = getYearsDiff(dob, exitDate);
                        if (studentAge < 14) {
                            errors.add(new StateReportValidationError(entity, field,
                                    "Student with exit code " + value
                                            + " is less than 14 years of age - Please review.",
                                    field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                            "Student age = " + STYLE_BOLD + studentAge + STYLE_END));
                        }
                    }
                }

                if ("136".equals(value)) {
                    String exitDateValue = entity.getFieldValue(FIELD_ID_SCHOOL_EXIT_DATE);
                    if (exitDateValue != null) {
                        Date exitDate = null;
                        try {
                            exitDate = m_exportFormat.parse(exitDateValue);
                        } catch (ParseException e) {
                            e.printStackTrace();
                        }
                        PlainDate dob = student.getPerson().getDob();
                        int studentAge = getYearsDiff(dob, exitDate);
                        if (studentAge < 20) {
                            errors.add(new StateReportValidationError(entity, field,
                                    "State exit code 136 (Reached Max Age) error for student with age less than 20",
                                    field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                            "Student age = " + STYLE_BOLD + studentAge + STYLE_END));
                        }
                    }
                }

                if ("140".equals(value)) {
                    String gradeLevel = entity.getFieldValue(FIELD_ID_GRADE_LEVEL);
                    if (!"PS".equals(gradeLevel)) {
                        errors.add(new StateReportValidationError(entity, field,
                                field.getFieldId() + " of 140 requires a PRES grade ordinal",
                                field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                        FIELD_ID_GRADE_LEVEL + " = " + STYLE_BOLD + gradeLevel + STYLE_END));
                    }

                    String entryCode = entity.getFieldValue(FIELD_ID_SCHOOL_ENTRY_CODE);
                    if (!"4034".equals(entryCode)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "4034 state entry code and 140 state exit code can only be paired with one another",
                                FIELD_ID_SCHOOL_ENTRY_CODE + " = " + STYLE_BOLD + entryCode + STYLE_END + ", " +
                                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
                    }
                }

                if ("425".equals(value)) {
                    String entryCode = entity.getFieldValue(FIELD_ID_SCHOOL_ENTRY_CODE);
                    if (!"0011".equals(entryCode)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "425 No Doc. Of Transfer for currently enrolled student - Please review.",
                                FIELD_ID_SCHOOL_ENTRY_CODE + " = " + STYLE_BOLD + entryCode + STYLE_END + ", " +
                                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
                    }
                }

                if ("816".equals(value)) {
                    String gradeLevel = entity.getFieldValue(FIELD_ID_GRADE_LEVEL);
                    if (!"GD".equals(gradeLevel)) {
                        errors.add(new StateReportValidationError(entity, field,
                                field.getFieldId() + " of 816 requires a GED grade ordinal",
                                field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                        FIELD_ID_GRADE_LEVEL + " = " + STYLE_BOLD + gradeLevel + STYLE_END));
                    }
                }

                if ("8228".equals(value)) {
                    String entryCode = entity.getFieldValue(FIELD_ID_SCHOOL_ENTRY_CODE);
                    if (!"5555".equals(entryCode)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "5555 state entry code and 8228 state exit code can only be paired with one another",
                                FIELD_ID_SCHOOL_ENTRY_CODE + " = " + STYLE_BOLD + entryCode + STYLE_END + ", " +
                                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
                    }
                }

                if ("8305".equals(value)) {
                    String entryCode = entity.getFieldValue(FIELD_ID_SCHOOL_ENTRY_CODE);
                    if (!"5905".equals(entryCode)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "A state exit code 8305 requires a 5905 state entry code",
                                FIELD_ID_SCHOOL_ENTRY_CODE + " = " + STYLE_BOLD + entryCode + STYLE_END + ", " +
                                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
                    }
                }

                if ("8316".equals(value)) {
                    String entryCode = entity.getFieldValue(FIELD_ID_SCHOOL_ENTRY_CODE);
                    if (!"8294".equals(entryCode) && !"0033".equals(entryCode)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "A state exit code 8316 requires a corresponding 8294 or 0033 state entry code.",
                                FIELD_ID_SCHOOL_ENTRY_CODE + " = " + STYLE_BOLD + entryCode + STYLE_END + ", " +
                                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
                    }
                }
            }

            /*
             * These validations are requiring access to values of specific fields of next record,
             * so it should be
             * performed only if it's last row and validating values for all records were stored.
             */
            if (entity.getCurrentRow() == (entity.getRowCount() - 1)) {
                for (ExitCodeValidatingFields curValidatingFields : previousValidatingFields) {
                    String curExitCode = curValidatingFields.getExitCode();

                    int index = previousValidatingFields.indexOf(curValidatingFields);
                    ExitCodeValidatingFields nextValidatingFields = null;
                    try {
                        nextValidatingFields = previousValidatingFields.get(++index);
                    } catch (IndexOutOfBoundsException e) {
                        // there is nothing to do here
                    }

                    String curLocation = curValidatingFields.getLocationCode();
                    String curGradeLevel = curValidatingFields.getGradeLevel();
                    Date curExitDate = curValidatingFields.getExitDate();

                    String nextLocation = null;
                    String nextGradeLevel = null;
                    Date nextEntryDate = null;
                    String nextEntryCode = null;

                    if (nextValidatingFields != null) {
                        nextLocation = nextValidatingFields.getLocationCode();
                        nextGradeLevel = nextValidatingFields.getGradeLevel();
                        nextEntryDate = nextValidatingFields.getEntryDate();
                        nextEntryCode = nextValidatingFields.getEntryCode();
                    }


                    boolean isNextValid = false;

                    switch (curExitCode) {
                        case "782":
                            /*
                             * Should be with the same location, different grade level K-14, and/or
                             * entry date should be
                             * later than the 782 exit date.
                             */
                            if (nextValidatingFields != null) {
                                if (curLocation.equals(nextLocation) && !curGradeLevel.equals(nextGradeLevel) &&
                                        nextEntryDate.after(curExitDate)) {
                                    isNextValid = true;
                                }
                            }

                            if (!isNextValid) {
                                errors.add(new StateReportValidationError(entity, field,
                                        "A state exit code of 782 requires the next School Entry/Exit record to have the same location, "
                                                + "different grade, and later entry date",
                                        "Current " + FIELD_ID_LOCATION + " = " + STYLE_BOLD + curLocation + STYLE_END
                                                + ", " +
                                                "current " + FIELD_ID_GRADE_LEVEL + " = " + STYLE_BOLD + curGradeLevel
                                                + STYLE_END + ", " +
                                                "current " + FIELD_ID_SCHOOL_EXIT_DATE + " = " + STYLE_BOLD
                                                + curExitDate + STYLE_END + ". " +
                                                "Next " + FIELD_ID_LOCATION + " = " + STYLE_BOLD + nextLocation
                                                + STYLE_END + ", " +
                                                "next " + FIELD_ID_GRADE_LEVEL + " = " + STYLE_BOLD + nextGradeLevel
                                                + STYLE_END + ", " +
                                                "next " + FIELD_ID_SCHOOL_ENTRY_DATE + " = " + STYLE_BOLD
                                                + nextEntryDate + STYLE_END));
                            }

                            break;

                        case "5927":
                            /*
                             * Next record should be with an entry code of 7011 and a different
                             * location.
                             */
                            if (nextValidatingFields != null) {
                                if ("7011".equals(nextEntryCode) && !curLocation.equals(nextLocation)) {
                                    isNextValid = true;
                                }
                            }

                            if (!isNextValid) {
                                errors.add(new StateReportValidationError(entity, field,
                                        "A state exit code of 5927 requires the next School Entry/Exit record to have an entry code of 7011 "
                                                + "and a different location",

                                        "Current " + FIELD_ID_SCHOOL_EXIT_CODE + " = " + STYLE_BOLD + curExitCode
                                                + STYLE_END + ", " +
                                                "current " + FIELD_ID_LOCATION + " = " + STYLE_BOLD + curLocation
                                                + STYLE_END + ". " +
                                                "Next " + FIELD_ID_SCHOOL_ENTRY_CODE + " = " + STYLE_BOLD
                                                + nextEntryCode + STYLE_END + ", " +
                                                "next " + FIELD_ID_LOCATION + " = " + STYLE_BOLD + nextLocation
                                                + STYLE_END));
                            }

                            break;

                        case "289":
                            /*
                             * A state exit code of 289 prior to June 1 requires the next School
                             * Entry/Exit record to
                             * have an entry code of 5654.
                             */
                            if (!curExitDate.before(m_june01)) {
                                isNextValid = true;
                            } else if (nextValidatingFields != null) {
                                if (("7011".equals(nextEntryCode) && !curLocation.equals(nextLocation))) {
                                    isNextValid = true;
                                }
                            }

                            if (!isNextValid) {
                                errors.add(new StateReportValidationError(entity, field,
                                        "A state exit code of 289 requires the next School Entry/Exit record to have an entry code of 5654",
                                        "Current " + FIELD_ID_SCHOOL_EXIT_CODE + " = " + STYLE_BOLD + curExitCode
                                                + STYLE_END + ", " +
                                                "next " + FIELD_ID_SCHOOL_ENTRY_CODE + " = " + STYLE_BOLD
                                                + nextEntryCode + STYLE_END));
                            }

                            break;

                        case "153":
                            /*
                             * With the exception of UPK Location 0666, an exit code of 153 prior to
                             * June 1 that does
                             * not have the next School Entry/Exit record with an entry code 0011 or
                             * 0022 or 0033 or
                             * 5544 or 7000 with a different location, will receive error
                             */
                            if (!curExitDate.before(m_june01) || "0666".equals(curLocation)) {
                                isNextValid = true;
                            } else if (nextValidatingFields != null) {
                                if (("0011".equals(nextEntryCode) || "0022".equals(nextEntryCode) ||
                                        "0033".equals(nextEntryCode) || "5544".equals(nextEntryCode) ||
                                        "7000".equals(nextEntryCode)) && !curLocation.equals(nextLocation)) {
                                    isNextValid = true;
                                }
                            }

                            if (!isNextValid) {
                                errors.add(new StateReportValidationError(entity, field,
                                        "A state exit code of 153 requires the next School Entry/Exit record to have an entry code of 0011, "
                                                +
                                                "0022, 0033, 5544 or 7000 and a different location",
                                        "Current " + FIELD_ID_SCHOOL_EXIT_CODE + " = " + STYLE_BOLD + curExitCode
                                                + STYLE_END + ", " +
                                                "current " + FIELD_ID_LOCATION + " = " + STYLE_BOLD + curLocation
                                                + STYLE_END + ". " +
                                                "Next " + FIELD_ID_SCHOOL_ENTRY_CODE + " = " + STYLE_BOLD
                                                + nextEntryCode + STYLE_END + ", " +
                                                "next " + FIELD_ID_LOCATION + " = " + STYLE_BOLD + nextLocation
                                                + STYLE_END));
                            }

                            break;

                        case "238":

                            /*
                             * A state exit code of 238 requires the next School Entry/Exit record
                             * to have an entry code
                             * of 0011, 0022 or 0033 with BEDS location 0777
                             */
                            if (nextValidatingFields != null) {
                                if (("0011".equals(nextEntryCode) || "0022".equals(nextEntryCode) ||
                                        "0033".equals(nextEntryCode)) && "0777".equals(nextLocation)) {
                                    isNextValid = true;
                                }
                            }

                            if (!isNextValid) {
                                errors.add(new StateReportValidationError(entity, field,
                                        "A state exit code of 238 requires the next School Entry/Exit record to have an entry code of 0011, 0022 or 0033 with BEDS location 0777",
                                        "Current " + FIELD_ID_SCHOOL_EXIT_CODE + " = " + STYLE_BOLD + curExitCode
                                                + STYLE_END + ", " +
                                                "next " + FIELD_ID_SCHOOL_ENTRY_CODE + " = " + STYLE_BOLD
                                                + nextEntryCode + STYLE_END + ", " +
                                                "next " + FIELD_ID_LOCATION + " = " + STYLE_BOLD + nextLocation
                                                + STYLE_END));
                            }

                            break;

                        default:
                            break;
                    }
                }
            }

            return errors;
        }

    }

    /**
     * Validates exit comment.
     *
     * @author Follett Software Company
     */
    private class ExitCommentValidator implements FieldValidator {
        public static final String VAL_ID = "VAL_EXIT_COMMENT";

        /**
         * Instantiates a new exit comment validator.
         */
        public ExitCommentValidator() {
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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (!StringUtils.isEmpty(value)) {
                String exitDate = entity.getFieldValue(FIELD_ID_SCHOOL_EXIT_DATE);
                String exitCode = entity.getFieldValue(FIELD_ID_SCHOOL_EXIT_CODE);

                if (StringUtils.isEmpty(exitDate) || StringUtils.isEmpty(exitCode)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + " must be paired with " + FIELD_ID_SCHOOL_EXIT_DATE + " and "
                                    + FIELD_ID_SCHOOL_EXIT_CODE,
                            FIELD_ID_SCHOOL_EXIT_DATE + " = " + STYLE_BOLD + exitDate + STYLE_END +
                                    ", " + FIELD_ID_SCHOOL_EXIT_CODE + " = " + STYLE_BOLD + exitCode + STYLE_END +
                                    ", " + field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
                }
            }

            return errors;
        }

    }

    /**
     * Class provide method to determine if period crosses another period.
     *
     * @author Follett Software Company
     */
    private class CrossPeriod {
        private PlainDate m_end;
        private PlainDate m_start;

        /**
         * Instantiates a new cross period.
         *
         * @param start PlainDate
         * @param end PlainDate
         */
        public CrossPeriod(PlainDate start, PlainDate end) {
            m_start = start;
            m_end = end;
        }

        /**
         * Returns end date.
         *
         * @return PlainDate
         */
        public PlainDate getEnd() {
            return m_end;
        }

        /**
         * Returns start date.
         *
         * @return PlainDate
         */
        public PlainDate getStart() {
            return m_start;
        }

        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (!(obj instanceof CrossPeriod)) {
                return false;
            }
            if (obj == this) {
                return true;
            }

            CrossPeriod rhs = (CrossPeriod) obj;
            return new EqualsBuilder().append(m_start.toString(), rhs.m_start.toString())
                    .append(m_end.toString(), rhs.m_end.toString()).isEquals();
        }

        /**
         * Returns true if passed instance is overlapped with current instance, otherwise false.
         *
         * @param otherPeriod CrossPeriod
         * @return boolean
         */
        public boolean isCross(CrossPeriod otherPeriod) {
            boolean returnValue = false;

            if (otherPeriod.getEnd() == null && otherPeriod.getStart() == null) {
                returnValue = true;
            } else if (m_start != null && otherPeriod.getStart() != null) {
                if (m_end != null && otherPeriod.getEnd() != null) {
                    if (!m_start.after(otherPeriod.getEnd()) && !m_end.before(otherPeriod.getStart())) {
                        returnValue = true;
                    }
                } else if (m_end != null) {
                    if (!otherPeriod.getStart().after(m_end) && !otherPeriod.getStart().before(m_start)) {
                        returnValue = true;
                    }
                } else {
                    if (!m_start.after(otherPeriod.getEnd()) && !m_start.before(otherPeriod.getStart())) {
                        returnValue = true;
                    }
                }
            }

            return returnValue;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "CrossPeriod class " + m_start + " " + m_end;
        }

        /**
         * Hash code.
         *
         * @return int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return new HashCodeBuilder(17, 31).append(m_start.toString()).append(m_end.toString()).toHashCode();
        }
    }

    /**
     * Class container to store values needed to check EE2016, EE2017, EE2018, EE2019, EE2020
     * errors. These validations
     * are requiring access to values of specific fields of next record, so the values are stored
     * here.
     *
     * @author Follett Software Company
     */
    private class ExitCodeValidatingFields {
        private String m_locationCode;
        private String m_gradeLevel;
        private Date m_entryDate;
        private String m_entryCode;
        private Date m_exitDate;
        private String m_exitCode;

        /**
         * Instantiates a new exit code validating fields.
         *
         * @param locationCode String
         * @param gradeLevel String
         * @param entryDate String
         * @param entryCode String
         * @param exitDate String
         * @param exitCode String
         */
        public ExitCodeValidatingFields(String locationCode, String gradeLevel, String entryDate, String entryCode,
                String exitDate, String exitCode) {
            m_locationCode = locationCode;
            m_gradeLevel = gradeLevel;
            try {
                m_entryDate = m_exportFormat.parse(entryDate);
                if (!StringUtils.isEmpty(exitDate)) {
                    m_exitDate = m_exportFormat.parse(exitDate);
                }
            } catch (ParseException e) {
                e.printStackTrace();
            }
            m_entryCode = entryCode;
            m_exitCode = exitCode;
        }

        /**
         * Gets the entry code.
         *
         * @return String
         */
        public String getEntryCode() {
            return m_entryCode;
        }

        /**
         * Gets the entry date.
         *
         * @return Date
         */
        public Date getEntryDate() {
            return m_entryDate;
        }

        /**
         * Gets the exit code.
         *
         * @return String
         */
        public String getExitCode() {
            return m_exitCode;
        }

        /**
         * Gets the exit date.
         *
         * @return Date
         */
        public Date getExitDate() {
            return m_exitDate;
        }

        /**
         * Gets the grade level.
         *
         * @return String
         */
        public String getGradeLevel() {
            return m_gradeLevel;
        }

        /**
         * Gets the location code.
         *
         * @return String
         */
        public String getLocationCode() {
            return m_locationCode;
        }
    }

    /**
     * Input Definition Parameters
     */
    protected static final String PARAM_END_OF_SCHOOL_REPORT = "endOfSchoolReport";
    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";
    protected static final String PARAM_REMOVE_HEADER = "removeHeader";
    protected static final String PARAM_REPORT_DATE = "reportDate";

    /**
     * Retriever Parameters
     */
    protected static final String CALC_ID_CALC_ENROLL = "CALC-ENROLL";

    protected static final String CALC_PARAM_DOE_DISTRICT = "SERV-DISTRICT";
    protected static final String CALC_PARAM_DOE_SCHOOL = "SERV-SCHOOL";
    protected static final String CALC_PARAM_GRADE_LEVEL_ENROLLMENT = "GRADE-LEVEL";
    protected static final String CALC_PARAM_LOCATION_CODE = "LOCATION-CODE";
    protected static final String CALC_PARAM_SCHOOL_ENTRY_CODE = "ENTRY-CODE";
    protected static final String CALC_PARAM_SCHOOL_ENTRY_COMMENT = "ENTRY-COMMENT";
    protected static final String CALC_PARAM_SCHOOL_ENTRY_DATE = "ENTRY-DATE";
    protected static final String CALC_PARAM_SCHOOL_WITHDRAWAL_CODE = "WITHDRAWAL-CODE";
    protected static final String CALC_PARAM_SCHOOL_WITHDRAWAL_COMMENT = "WITHDRAWAL-COMMENT";
    protected static final String CALC_PARAM_SCHOOL_WITHDRAWAL_DATE = "WITHDRAWAL-DATE";

    /**
     * Aliases
     */
    // SCHOOL
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_LOCATION_CODE = "LOCATION CODE";

    // STUDENT_ENROLLMENT
    protected static final String ALIAS_DISTRICT_SERVE = "DOE DISTRICT SERVE";
    protected static final String ALIAS_ENROLLMENT_COMMENT = "DOE ENROLLMENT COMMENT";
    protected static final String ALIAS_GRADE_LEVEL = "DOE GRADE LEVELS";
    protected static final String ALIAS_GRADE_LEVEL_OVERRIDE = "DOE GRADE LEVEL OVERRIDE";
    protected static final String ALIAS_LOCATION_OVERRIDE = "DOE LOCATION OVERRIDE";
    protected static final String ALIAS_SCHOOL_SERVE = "DOE SCHOOL SERVE";

    /**
     * Other Constants
     */
    protected static final String CODE_END_CURRENT_SCHOOL = "153";
    protected static final String CODE_GRADE_CHANGE = "782";

    protected static final String CODE_ENROLL_STATUS_PRE_REG = "PreReg";
    protected static final String CODE_ENROLL_STATUS_PRE_ENROLL = "Pre-enroll";

    protected static final String FIELD_ID_GRADE_LEVEL = "Enroll Grade Level";
    protected static final String FIELD_ID_LOCATION = "Location Code";
    protected static final String FIELD_ID_SCHOOL_ENTRY_CODE = "Entry Type Code";
    protected static final String FIELD_ID_SCHOOL_ENTRY_DATE = "School Entry Date";
    protected static final String FIELD_ID_SCHOOL_EXIT_CODE = "Exit Type Code";
    protected static final String FIELD_ID_SCHOOL_EXIT_DATE = "School Exit Date";

    protected static final String STRING_EMPTY = "";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected boolean m_endOfSchoolReport = false;
    protected boolean m_excludeSchoolIndicator = false;
    protected SimpleDateFormat m_exportFormat;
    protected boolean m_removeHeader;
    protected int m_schoolYear;
    protected Map m_excludeSchoolMap;
    protected PlainDate m_schoolYearEndDate;
    protected PlainDate m_reportDate;
    protected PlainDate m_schoolYearStartDate;
    protected String m_enrollmentEntryComment;
    protected String m_enrollmentLocationCode;
    protected String m_enrollmentLocationOverride;
    protected String m_enrollmentGradeLevel;
    protected String m_enrollmentGradeLevelOverride;
    protected String m_excludeSchool;
    protected String m_servDistrict;
    protected String m_servSchool;
    protected StudentHistoryHelper m_helper;
    protected Map<String, String> m_mostCommonCalendars;
    protected static HashMap m_previousYOGEnrollCodeMap = new HashMap();

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
     * Over rid getHeading Method. The reason is so that the user can decide if the header is
     * included or not at runtime.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        if (m_removeHeader) {
            return null;
        }

        return super.getHeading();
    }

    /**
     * Build the list of most Common Calendars for schools.
     *
     * @return Collection of SchoolCalendars oids
     */
    public Map<String, String> getMostCommonCalendars() {
        Map<String, Map<String, Collection<SchoolCalendar>>> mapSchoolCalendars = getSchoolCalendars();

        HashMap<String, String> schoolCalendars = new HashMap();

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(SisStudent.COL_ENROLLMENT_STATUS, StudentManager.getActiveStudentCodeList(getOrganization()));

        String[] columns = new String[] {SisStudent.COL_SCHOOL_OID, SisStudent.COL_CALENDAR_CODE, "count(*)"};

        ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class, columns, criteria);
        query.addGroupBy(SisStudent.COL_SCHOOL_OID);
        query.addGroupBy(SisStudent.COL_CALENDAR_CODE);
        query.addOrderByDescending("count(*)");

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String schoolOid = (String) row[0];
                String calendarCode = (String) row[1];

                if (!schoolCalendars.containsKey(schoolOid)) {
                    Map<String, Collection<SchoolCalendar>> mapCalendars = mapSchoolCalendars.get(schoolOid);
                    if (mapCalendars != null && mapCalendars.containsKey(calendarCode)) {
                        SchoolCalendar schoolCalendar = mapCalendars.get(calendarCode).iterator().next();
                        schoolCalendars.put(schoolOid, schoolCalendar.getCalendarId());
                    }
                }
            }
        } finally {
            iterator.close();
        }

        // Add schools without students - any calendar will do
        for (String oid : mapSchoolCalendars.keySet()) {
            if (!schoolCalendars.containsKey(oid)) {
                Map<String, Collection<SchoolCalendar>> mapCalendars = mapSchoolCalendars.get(oid);
                SchoolCalendar schoolCalendar = mapCalendars.values().iterator().next().iterator().next();
                schoolCalendars.put(oid, schoolCalendar.getOid());
            }
        }

        return schoolCalendars;
    }

    /**
     * Checks for school to be in the map of excluded schools, or that the map is null.
     *
     * @param schoolId String
     * @return true, if successful
     */
    public boolean includeSchool(String schoolId) {
        return (m_excludeSchoolMap == null) || !m_excludeSchoolMap.containsKey(schoolId);
    }

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

        m_exportFormat = new SimpleDateFormat("yyyy-MM-dd");

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

            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(EntryExitEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALC_ID_CALC_ENROLL, new EnrollmentRetriever());
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldRetriever>();
            validators.put(EntryDateValidator.VAL_ID, new EntryDateValidator());
            validators.put(ExitDateValidator.VAL_ID, new ExitDateValidator());
            validators.put(ExitCodeValidator.VAL_ID, new ExitCodeValidator());
            validators.put(ExitCommentValidator.VAL_ID, new ExitCommentValidator());
            validators.put(EntryCodeValidator.VAL_ID, new EntryCodeValidator());
            super.addValidators(validators);
        }
    }

    /**
     * Initialize fields.
     */
    public void initializeFields() {
        // System Parameters
        m_schoolYear = getCurrentContext().getSchoolYear();
        m_schoolYearStartDate = getCurrentContext().getStartDate();
        m_schoolYearEndDate = getCurrentContext().getEndDate();

        // Load Parameters
        m_reportDate = new PlainDate();
        if (getParameter(PARAM_REPORT_DATE) != null) {
            m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        }
        m_removeHeader = false;
        if (getParameter(PARAM_REMOVE_HEADER) != null) {
            m_removeHeader = ((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue();
        }
        m_excludeSchoolIndicator = false;
        if (getParameter(PARAM_EXCLUDE_SCHOOL) != null) {
            m_excludeSchoolIndicator = ((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue();
        }
        if (getParameter(PARAM_END_OF_SCHOOL_REPORT) != null) {
            m_endOfSchoolReport = ((Boolean) getParameter(PARAM_END_OF_SCHOOL_REPORT)).booleanValue();
        }

        m_mostCommonCalendars = getMostCommonCalendars();

        // Load Alias database field Names
        m_enrollmentEntryComment = translateAliasToJavaName(ALIAS_ENROLLMENT_COMMENT, true);
        m_enrollmentGradeLevel = translateAliasToJavaName(ALIAS_GRADE_LEVEL, true);
        m_enrollmentGradeLevelOverride = translateAliasToJavaName(ALIAS_GRADE_LEVEL_OVERRIDE, true);
        m_enrollmentLocationCode = translateAliasToJavaName(ALIAS_LOCATION_CODE, true);
        m_enrollmentLocationOverride = translateAliasToJavaName(ALIAS_LOCATION_OVERRIDE, true);
        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_servDistrict = translateAliasToJavaName(ALIAS_DISTRICT_SERVE, true);
        m_servSchool = translateAliasToJavaName(ALIAS_SCHOOL_SERVE, true);
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
     * This method returns the school end date.
     *
     * @return PlainDate
     */
    protected PlainDate getEndDate() {
        return m_schoolYearEndDate;
    }

    /**
     * This method returns the school start date.
     *
     * @return PlainDate
     */
    protected PlainDate getStartDate() {
        return m_schoolYearStartDate;
    }

    /**
     * Difference in years between two dates.
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
     * Build map of maps of SchoolCalendars keyed on school oid and school calendar id.
     *
     * @return Map<String, Map<String, Collection<SchoolCalendar>>>
     */
    private Map<String, Map<String, Collection<SchoolCalendar>>> getSchoolCalendars() {
        X2Criteria casCriteria = new X2Criteria();

        casCriteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        // Filter to eliminate unused schools.
        casCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.COL_INACTIVE_INDICATOR,
                Boolean.TRUE);
        casCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.COL_ARCHIVE_INDICATOR,
                Boolean.TRUE);

        QueryByCriteria query = new QueryByCriteria(SchoolCalendar.class, casCriteria);

        return getBroker().getGroupedCollectionByQuery(query, new String[] {SchoolCalendar.COL_SCHOOL_OID,
                SchoolCalendar.COL_CALENDAR_ID}, new int[] {100, 5});
    }

    /**
     * Build a map of schools with the alias set to exclude school.
     */
    private void loadSchoolExcludeMap() {
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(m_excludeSchool, BooleanAsStringConverter.TRUE);

        BeanQuery query = new BeanQuery(School.class, schoolCriteria);

        m_excludeSchoolMap = getBroker().getGroupedCollectionByQuery(query, School.COL_SCHOOL_ID, 128);
    }
}
