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
package com.x2dev.procedures.statereporting.nj;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New Jersey state procedure for Student Course Roster export.
 *
 * @author X2 Development Corporation
 */

public class StudentCourseRoster extends StateReportData {
    /**
     * Entity class for Student Course Roster Student Level export.
     *
     * @author X2 Development Corporation
     */

    public static class StudentCourseRosterEntity extends StateReportEntity {
        List<StudentScheduleSpan> m_studentScheduleSpans = new ArrayList<StudentScheduleSpan>();

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StudentCourseRosterEntity() {
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
            SisStudent student = (SisStudent) bean;
            StudentCourseRoster stdCourseRoster = (StudentCourseRoster) data;
            List<StudentScheduleSpan> studentScheduleSpans = stdCourseRoster.m_helper.getStudentScheduleSpans(student);
            Comparator m_comparator = new Comparator() {
                @Override
                public int compare(Object o1, Object o2) {
                    StudentScheduleSpan span1 = (StudentScheduleSpan) o1;
                    StudentScheduleSpan span2 = (StudentScheduleSpan) o2;
                    return span1.getSection().getCourseView().compareTo(span2.getSection().getCourseView());
                }
            };
            for (StudentScheduleSpan scheduleSpan : studentScheduleSpans) {
                if ((scheduleSpan.getEntryChange() != null && BooleanAsStringConverter.TRUE
                        .equals(scheduleSpan.getEntryChange().getFieldValueByBeanPath(stdCourseRoster.m_exclCrsScc)))
                        || (scheduleSpan.getSchedule() != null && BooleanAsStringConverter.TRUE.equals(
                                scheduleSpan.getSchedule().getFieldValueByBeanPath(stdCourseRoster.m_exclCrsSsc)))
                        || (scheduleSpan.getTranscript() != null && BooleanAsStringConverter.TRUE.equals(
                                scheduleSpan.getTranscript().getFieldValueByBeanPath(stdCourseRoster.m_exclCrsTrn)))) {
                    continue;
                }
                String finalGrade = null;
                Transcript studentTranscript = scheduleSpan.getTranscript();
                if (studentTranscript != null) {
                    finalGrade = studentTranscript.getFinalGrade();
                }

                // Report the student's section if they have not exited the section as of the report
                // date,
                // indicated by null exit date or exit date after report date,
                // or they have exited and have a final grade.
                if (scheduleSpan.getExitDate() == null ||
                        scheduleSpan.getExitDate().after(stdCourseRoster.m_reportDate) ||
                        !StringUtils.isEmpty(finalGrade) ||
                        scheduleSpan.getExitDate()
                                .equals(((StudentCourseRoster) data).getTermEndDate(scheduleSpan.getSection()))) {
                    String schoolOid = scheduleSpan.getSection().getSchoolCourse().getSchoolOid();
                    if (!stdCourseRoster.m_excludedSchools.contains(schoolOid)) {
                        m_studentScheduleSpans.add(scheduleSpan);
                    }
                }
            }
            Collections.sort(m_studentScheduleSpans, m_comparator);
            setRowCount(m_studentScheduleSpans.size());
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
            MasterSchedule masterSchedule = getStudentScheduleSpan().getSection();
            String localCrsCode = "";
            if (masterSchedule != null) {
                SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                if (schoolCourse != null) {
                    localCrsCode = schoolCourse.getNumber();
                }
            }
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    ", LOCAL CRS CODE: " + localCrsCode +
                    "] ";

            return name;
        }

        /**
         * This method returns the current student schedule span.
         *
         * @return Student schedule span
         */
        public StudentScheduleSpan getStudentScheduleSpan() {
            return m_studentScheduleSpans.get(getCurrentRow());
        }
    }

    /**
     * This class returns the district code and school based on the current student schedule span.
     *
     * @author Follett Software Company
     *
     */
    protected class RetrieveCourseDistrictSchoolCode implements FieldRetriever {

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
            SisStudent std = (SisStudent) entity.getBean();
            String param = (String) field.getParameter();
            StudentScheduleSpan scheduleSpan = ((StudentCourseRosterEntity) entity).getStudentScheduleSpan();
            MasterSchedule masterSchedule = scheduleSpan.getSection();
            if (masterSchedule != null) {
                SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                if (schoolCourse != null) {
                    SisSchool school = schoolCourse.getSchool();
                    if (school != null) {
                        if (PARAM_DISTRICT_CODE.equalsIgnoreCase(param)) {
                            String districtCode = (String) school.getFieldValueByAlias(ALIAS_DISTRICT_CODE);
                            if (!StringUtils.isEmpty(districtCode)
                                    && districtCode.length() >= NJ_DOE_DISTRICT_CODE_LENGTH) {
                                value = districtCode.substring(0, NJ_DOE_DISTRICT_CODE_LENGTH);
                            }
                        } else if (PARAM_SCHOOL_CODE.equals(param)) {
                            String schoolCode = (String) school.getFieldValueByAlias(ALIAS_SCHOOL_CODE);
                            if (!StringUtils.isEmpty(schoolCode) && schoolCode.length() >= NJ_DOE_SCHOOL_CODE_LENGTH) {
                                value = schoolCode.substring(0, NJ_DOE_SCHOOL_CODE_LENGTH);
                            }
                        } else if (PARAM_COUNTRY_CODE_ASSIGN.equals(param)) {
                            if (m_ddOrgCountryCode.hasReferenceTable()) {
                                String schoolCodeAssign =
                                        (String) std.getOrganization1().getFieldValueByBeanPath(m_orgCountryCode);
                                if (!StringUtils.isEmpty(schoolCodeAssign)) {
                                    value = lookupStateValue(SisOrganization.class, m_orgCountryCode, schoolCodeAssign);
                                }
                            } else {
                                value = std.getOrganization1().getFieldValueByBeanPath(m_orgCountryCode);
                            }
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * This class returns the section entry and exit date using the current student schedule span.
     *
     * @author Follett Software Company
     *
     */
    protected class RetrieveCourseEntryExitDate implements FieldRetriever {

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
            String param = (String) field.getParameter();
            StudentScheduleSpan scheduleSpan = ((StudentCourseRosterEntity) entity).getStudentScheduleSpan();
            if (scheduleSpan != null) {
                if (PARAM_SECTION_ENTRY_DATE.equalsIgnoreCase(param)) {
                    value = scheduleSpan.getEntryDate();
                } else if (PARAM_SECTION_EXIT_DATE.equalsIgnoreCase(param)) {
                    PlainDate exitDate = scheduleSpan.getExitDate();
                    if (exitDate != null && !exitDate.after(m_reportDate)) {
                        value = exitDate;
                    }
                }
            }
            return value;
        }
    }

    /**
     * This class returns the subject area and course id using the current student schedule span.
     *
     * @author Follett Software Company
     *
     */
    protected class RetrieveCourseSCEDCode implements FieldRetriever {

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
            String param = (String) field.getParameter();
            StudentScheduleSpan scheduleSpan = ((StudentCourseRosterEntity) entity).getStudentScheduleSpan();
            if (scheduleSpan != null) {
                MasterSchedule masterSchedule = scheduleSpan.getSection();
                if (masterSchedule != null) {
                    SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                    if (schoolCourse != null) {
                        Course course = schoolCourse.getCourse();
                        if (course != null) {
                            String subjAreaCode = (String) course.getFieldValueByBeanPath(m_scedCode);
                            subjAreaCode = lookupReferenceCodeByBeanPath(Course.class, m_scedCode, subjAreaCode,
                                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                            if (subjAreaCode != null && subjAreaCode.length() > 2) {
                                if (PARAM_SUBJECT_AREA.equalsIgnoreCase(param)) {
                                    subjAreaCode = subjAreaCode.substring(0, 2);
                                    if (subjAreaCode != null) {
                                        value = Integer.valueOf(subjAreaCode);
                                    }
                                } else if (PARAM_COURSE_ID.equalsIgnoreCase(param)) {
                                    subjAreaCode = subjAreaCode.substring(2);
                                    if (subjAreaCode != null) {
                                        value = Integer.valueOf(subjAreaCode);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * This class returns some course related details using the current student schedule span.
     *
     * @author Follett Software Company
     *
     */
    protected class RetrieveCourseDetails implements FieldRetriever {
        protected GradesManager m_gradesManager;

        /**
         * Instantiates a new retrieve course details.
         */
        protected RetrieveCourseDetails() {
            super();
            m_gradesManager = new GradesManager(getBroker());
        }

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
            String param = (String) field.getParameter();
            StudentScheduleSpan scheduleSpan = ((StudentCourseRosterEntity) entity).getStudentScheduleSpan();

            if (scheduleSpan != null) {
                MasterSchedule masterSchedule = scheduleSpan.getSection();
                if (PARAM_COURSE_LEVEL.equalsIgnoreCase(param)) {
                    if (masterSchedule != null) {
                        SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                        if (schoolCourse != null) {
                            Course course = schoolCourse.getCourse();
                            if (course != null) {
                                value = course.getFieldValueByBeanPath(m_courseLevel);
                                value = lookupReferenceCodeByBeanPath(Course.class, m_courseLevel, (String) value,
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                            }
                        }
                    }
                }
                if (PARAM_COURSE_TYPE.equalsIgnoreCase(param)) {
                    if (masterSchedule != null) {
                        value = masterSchedule.getFieldValueByBeanPath(m_courseType);
                        value = lookupReferenceCodeByBeanPath(MasterSchedule.class, m_courseType,
                                (String) value, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                } else if (PARAM_GRADESPAN.equalsIgnoreCase(param)) {
                    if (masterSchedule != null) {
                        SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                        if (schoolCourse != null) {
                            Course course = schoolCourse.getCourse();
                            if (course != null) {
                                String code = (String) course.getFieldValueByBeanPath(m_gradeSpan);
                                if (code != null) {
                                    String stateCode = lookupReferenceCodeByBeanPath(Course.class, m_gradeSpan, code,
                                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                                    // If we haven't reference table for Grade Code we removed the
                                    // '-'in Grade Code.
                                    // If the result is exactly 2 characters, we will duplicate. In
                                    // this case,'09' becomes '0909'.
                                    if (stateCode == null) {
                                        String[] stateCodeArray = code.split("-");
                                        if (stateCodeArray.length == 2) {
                                            value = stateCodeArray[0] + stateCodeArray[1];
                                        }
                                        // If the result is exactly 2 characters, we will duplicate.
                                        // In this case,'09' becomes '0909'.
                                        else if (stateCodeArray.length == 1) {
                                            if (stateCodeArray[0].length() == 2) {
                                                value = stateCodeArray[0] + stateCodeArray[0];
                                            } else {
                                                value = stateCodeArray[0];
                                            }
                                        }
                                    } else {
                                        value = stateCode;
                                    }
                                }
                            }
                        }
                    }
                } else if (PARAM_COURSE_SEQ.equalsIgnoreCase(param)) {
                    if (masterSchedule != null) {
                        SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                        if (schoolCourse != null) {
                            Course course = schoolCourse.getCourse();
                            if (course != null) {
                                value = course.getFieldValueByBeanPath(m_courseSeq);
                                if (value != null) {
                                    value = Integer.valueOf((String) value);
                                }
                            }
                        }
                    }
                } else if (PARAM_CREDIT.equalsIgnoreCase(param)) {
                    Transcript studentTranscript = scheduleSpan.getTranscript();
                    if (studentTranscript != null && !StringUtils.isEmpty(studentTranscript.getPotentialCredit())) {
                        value = new BigDecimal(studentTranscript.getPotentialCredit());
                    }
                    if (value == null) {
                        SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                        if (schoolCourse != null) {
                            Course course = schoolCourse.getCourse();
                            if (course != null) {
                                value = course.getCredit();
                            }
                        }
                    }
                } else if (PARAM_LOCAL_COURSE_TITLE.equalsIgnoreCase(param)) {
                    if (masterSchedule != null) {
                        SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                        if (schoolCourse != null) {
                            value = schoolCourse.getDescription();
                        }
                    }
                } else if (PARAM_LOCAL_COURSE_CODE.equalsIgnoreCase(param)) {
                    if (masterSchedule != null) {
                        SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                        if (schoolCourse != null) {
                            value = schoolCourse.getNumber();
                        }
                    }
                } else if (PARAM_LOCAL_SECTION_CODE.equalsIgnoreCase(param)) {
                    if (masterSchedule != null) {
                        value = masterSchedule.getCourseView();
                    }
                } else if (PARAM_CREDITS_EARNED.equalsIgnoreCase(param)) {
                    Transcript studentTranscript = scheduleSpan.getTranscript();
                    if (studentTranscript != null) {
                        value = studentTranscript.getTotalCredit();
                    }
                } else if (PARAM_GRADE_ALPHA.equalsIgnoreCase(param)) {
                    Transcript studentTranscript = scheduleSpan.getTranscript();
                    if (studentTranscript != null) {
                        String finalGrade = studentTranscript.getFinalGrade();

                        // See if the grade in the final grade column is a grade scale
                        // value (letter grade).
                        if (!StringUtils.isEmpty(finalGrade) && !StringUtils.isNumeric(finalGrade)) {
                            GradeScale gradeScale = m_gradeScales.get(studentTranscript.getTranscriptDefinitionOid());
                            String schoolOid = null;
                            if (isSchoolContext()) {
                                School school = getSchool();
                                if (school != null) {
                                    schoolOid = school.getOid();
                                }
                            }
                            String schoolCourseOid = studentTranscript.getSchoolCourseOid();
                            GradeScaleGradeDefinition gsg = m_gradesManager.getGradeDefinition(finalGrade, gradeScale,
                                    schoolOid, schoolCourseOid);
                            if (gsg != null) {
                                String code =
                                        data.lookupStateValue(GradeScaleGradeDefinition.class, m_doeCompletionStatus,
                                                (String) gsg.getFieldValueByBeanPath(m_doeCompletionStatus));
                                if (StringUtils.isEmpty(code)) {
                                    value = finalGrade;
                                }
                            }
                        }
                    }
                } else if (PARAM_GRADE_NUMERIC.equalsIgnoreCase(param)) {
                    Transcript studentTranscript = scheduleSpan.getTranscript();
                    if (studentTranscript != null) {
                        String finalGrade = studentTranscript.getFinalGrade();
                        if (!StringUtils.isEmpty(finalGrade) && StringUtils.isNumeric(finalGrade)) {
                            float floatFinalGrade = Float.parseFloat(finalGrade);
                            value = Integer.valueOf(Math.round(floatFinalGrade));
                        }
                    }
                } else if (PARAM_COMPLETION_STATUS.equalsIgnoreCase(param)) {
                    Transcript studentTranscript = scheduleSpan.getTranscript();
                    if (studentTranscript != null) {
                        String finalGrade = studentTranscript.getFinalGrade();

                        // See if the grade in the final grade column is a grade scale
                        // value (letter grade).
                        if (!StringUtils.isEmpty(finalGrade) && !StringUtils.isNumeric(finalGrade)) {
                            GradeScale gradeScale =
                                    m_gradeScales.get(studentTranscript.getTranscriptDefinitionOid());
                            String schoolOid = null;
                            if (isSchoolContext()) {
                                School school = getSchool();
                                if (school != null) {
                                    schoolOid = school.getOid();
                                }
                            }
                            String schoolCourseOid = studentTranscript.getSchoolCourseOid();
                            GradeScaleGradeDefinition gsg =
                                    m_gradesManager.getGradeDefinition(finalGrade, gradeScale, schoolOid,
                                            schoolCourseOid);
                            if (gsg != null) {
                                value = data.lookupStateValue(GradeScaleGradeDefinition.class,
                                        m_doeCompletionStatus,
                                        (String) gsg.getFieldValueByBeanPath(m_doeCompletionStatus));
                            }
                        }
                    }
                } else if (PARAM_DUAL_INSTITUTION.equalsIgnoreCase(param)) {
                    if (masterSchedule != null) {
                        value = masterSchedule.getFieldValueByBeanPath(m_dualInstitution);
                        value = lookupReferenceCodeByBeanPath(MasterSchedule.class, m_dualInstitution,
                                (String) value, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                }
            }
            return value;
        }
    }

    /**
     * If Alpha Grade Earned not blank/null, must = A, A+, A-, B, B+, B-, C, C+, C-, D, D+, D-, E,
     * E+, E-, F, F+, F-
     *
     * @author Follett Software Company
     */
    public class ValidateAlphaGradeEarned implements FieldValidator {
        private static final String ERROR_MESSAGE = "Alpha Grade invalid.";
        private static final String INVALID_VALUE = "Invalid Value";
        private static final String VAL_ID = "A-GRADE-EARNED";
        private final Collection<String> VALID_VALUES = Arrays.asList("A", "A+", "A-", "B", "B+", "B-", "C", "C+", "C-",
                "D", "D", "D-", "E", "E+", "E-", "F", "F+", "F-");

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
            Collection<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();
            if (!m_bypassGradeValidations && !StringUtils.isEmpty(value) && !VALID_VALUES.contains(value)) {
                errors.add(new StateReportValidationError(entity, field, INVALID_VALUE, ERROR_MESSAGE));
            }
            return errors;
        }
    }

    /**
     * Numeric Grade Earned, Alpha Grade Earned, Completion Status fields are all blank/null and
     * student grade level => 06.
     *
     * @author Follett Software Company
     */
    public class ValidateBlankGrades implements FieldValidator {
        private static final String ERROR_MESSAGE =
                "Missing value for Numeric Grade Earned, Alpha Grade Earned and Completion Status.";
        private static final String FIELD_ALPHA_GRADE = "Alpha Grade Earned";
        private static final String FIELD_COMPL_STATUS = "Completion Status";
        private static final String MISSING_VALUE = "Missing Value";
        private static final String VAL_ID = "BLANK-GRADES";
        private final Collection<String> VALID_GRADES = Arrays.asList("06", "07", "08", "09", "10", "11", "12");

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
            SisStudent student = (SisStudent) entity.getBean();
            String grade = student.getGradeLevel();
            String alphaGrade = entity.getFieldValue(FIELD_ALPHA_GRADE);
            String complStatus = entity.getFieldValue(FIELD_COMPL_STATUS);
            Collection<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();
            if (!m_bypassGradeValidations && StringUtils.isEmpty(value) && StringUtils.isEmpty(alphaGrade)
                    && StringUtils.isEmpty(complStatus) && !StringUtils.isEmpty(grade)
                    && VALID_GRADES.contains(grade)) {
                errors.add(new StateReportValidationError(entity, field, MISSING_VALUE, ERROR_MESSAGE));
            }
            return errors;
        }
    }

    /**
     * If CompletionStatus not blank/null, must = P, F, W, I, N, G.
     *
     * @author Follett Software Company
     */
    public class ValidateCompletionStatus implements FieldValidator {
        private static final String ERROR_MESSAGE = "Completion Status invalid.";
        private static final String INVALID_VALUE = "Invalid Value";
        private static final String VAL_ID = "COMPL-STATUS";
        private final Collection<String> VALID_VALUES = Arrays.asList("P", "F", "W", "I", "N", "G");

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
            Collection<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();
            if (!m_bypassGradeValidations && !StringUtils.isEmpty(value) && !VALID_VALUES.contains(value)) {
                errors.add(new StateReportValidationError(entity, field, INVALID_VALUE, ERROR_MESSAGE));
            }
            return errors;
        }
    }

    /**
     * NJ DOE Constants.
     */
    protected static final int NJ_DOE_DISTRICT_CODE_LENGTH = 4;
    protected static final int NJ_DOE_SCHOOL_CODE_LENGTH = 3;

    /**
     * Alias
     */
    protected static final String ALIAS_COURSE_LEVEL = "DOE COURSE LEVEL";
    protected static final String ALIAS_COURSE_TYPE = "DOE COURSE TYPE";
    protected static final String ALIAS_COURSE_SEQ = "DOE COURSE SEQ";
    protected static final String ALIAS_DOE_COMPLETION_STATUS = "DOE COMPLETION STATUS";
    protected static final String ALIAS_DISTRICT_CODE = "DOE DISTRICT CODE";
    protected static final String ALIAS_DUAL_INSTITUTION = "all-mst-DualInstitutionID";
    protected static final String ALIAS_EXCLUDE_CRS_SCC = "all-scc-ExcludeCourse";
    protected static final String ALIAS_EXCLUDE_CRS_SSC = "all-ssc-ExcludeCourse";
    protected static final String ALIAS_EXCLUDE_CRS_TRN = "all-trn-ExcludeCourse";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_GRADE_SPAN = "DOE GRADE SPAN";
    protected static final String ALIAS_ORG_COUNTRY_CODE = "DOE COUNTY CODE";
    protected static final String ALIAS_SCED_CODE = "DOE SCED CODE";
    protected static final String ALIAS_SCHOOL_CODE = "DOE SCHOOL CODE";

    /**
     * Parameters
     */
    protected static final String PARAM_BYPASS_GRADE_VAL = "bypassGradeValidations";
    protected static final String PARAM_COMPLETION_STATUS = "COMP-STATUS";
    protected static final String PARAM_COUNTRY_CODE_ASSIGN = "COUNTRY-CODE-ASSIGN";
    protected static final String PARAM_COURSE_ID = "COURSE-ID";
    protected static final String PARAM_COURSE_LEVEL = "COURSE-LEVEL";
    protected static final String PARAM_COURSE_TYPE = "COURSE-TYPE";
    protected static final String PARAM_COURSE_SEQ = "COURSE-SEQ";
    protected static final String PARAM_CREDIT = "CREDIT";
    protected static final String PARAM_CREDITS_EARNED = "CREDITS-EARNED";
    protected static final String PARAM_DISTRICT_CODE = "DISTRICT-CODE";
    protected static final String PARAM_DUAL_INSTITUTION = "DUAL-INSTITUTION";
    protected static final String PARAM_GRADE_ALPHA = "GRADE-ALPHA";
    protected static final String PARAM_GRADE_NUMERIC = "GRADE-NUMERIC";
    protected static final String PARAM_GRADESPAN = "GRADESPAN";
    protected static final String PARAM_LOCAL_COURSE_CODE = "LOCAL-CRS-CODE";
    protected static final String PARAM_LOCAL_COURSE_TITLE = "LOCAL-CRS-TITLE";
    protected static final String PARAM_LOCAL_SECTION_CODE = "LOCAL-SECT-CODE";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_SCHOOL_CODE = "SCHOOL-CODE";
    protected static final String PARAM_SECTION_ENTRY_DATE = "ENTRY-DATE";
    protected static final String PARAM_SECTION_EXIT_DATE = "EXIT-DATE";
    protected static final String PARAM_SUBJECT_AREA = "SUBJ-AREA";

    /**
     * Other Constants
     */
    protected static final List<String> COMPLETION_STATUS_CODES =
            Arrays.asList(new String[] {"F", "I", "NG", "P", "W"});
    protected static final Pattern PATTERN_CONTAINS_NUMBER = Pattern.compile(".*\\d.*");

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected boolean m_bypassGradeValidations;
    protected String m_courseLevel;
    protected String m_courseType;
    protected String m_courseSeq;

    protected String m_districtCode;
    protected DataDictionaryField m_ddOrgCountryCode;
    protected String m_doeCompletionStatus;
    protected String m_dualInstitution;
    protected String m_exclCrsScc;
    protected String m_exclCrsSsc;
    protected String m_exclCrsTrn;

    protected Collection<String> m_excludedSchools;
    protected String m_excludeSkl;
    protected String m_gradeSpan;
    protected StudentHistoryHelper m_helper;
    protected String m_orgCountryCode;
    protected PlainDate m_reportDate;
    protected String m_schoolCode;
    protected String m_scedCode;
    private Map<String, GradeScale> m_gradeScales;
    private Map<String, Collection<ScheduleTermDate>> m_termDateMap;

    /**
     * Initialize the data module.
     */
    @Override
    public void initialize() {
        // initialize report fields.
        initializeFields();
        if (getSetupErrors().isEmpty()) {

            X2Criteria exclSklCriteria = new X2Criteria();
            exclSklCriteria.addEqualTo(m_excludeSkl, BooleanAsStringConverter.TRUE);
            QueryByCriteria exclSklQuery = new QueryByCriteria(SisSchool.class, exclSklCriteria);
            m_excludedSchools = getBroker().getMapByQuery(exclSklQuery, X2BaseBean.COL_OID, 3).keySet();

            /*
             * Build helper object.
             */
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
            /*
             * If no errors so far, continue with query.
             */
            if (getSetupErrors().size() == 0) {
                // Set the query to be used for student selection.
                setQuery(m_helper.getStudentQuery(false));
                setEntityClass(StudentCourseRosterEntity.class);

                // Build a map of calculations/retrievers
                HashMap calcs = new HashMap<String, FieldRetriever>();
                calcs.put("STD-SCHOOLCODE", new RetrieveCourseDistrictSchoolCode());
                calcs.put("STD-COURSE-DETAIL", new RetrieveCourseDetails());
                calcs.put("STD-DATE", new RetrieveCourseEntryExitDate());
                calcs.put("STD-SCED", new RetrieveCourseSCEDCode());
                HashMap validators = new HashMap<String, FieldRetriever>();
                validators.put(ValidateCompletionStatus.VAL_ID, new ValidateCompletionStatus());
                validators.put(ValidateAlphaGradeEarned.VAL_ID, new ValidateAlphaGradeEarned());
                validators.put(ValidateBlankGrades.VAL_ID, new ValidateBlankGrades());
                super.addCalcs(calcs);
                super.addValidators(validators);
            }

            loadGradeScales();
        }
    }

    /**
     * Gets the term end date for the section.
     *
     * @param section MasterSchedule
     * @return Plain date
     */
    protected PlainDate getTermEndDate(MasterSchedule section) {
        PlainDate termEnd = null;
        Collection<ScheduleTermDate> termDates = getTermDates(section.getScheduleTermOid());
        for (ScheduleTermDate termDate : termDates) {
            if (termEnd == null || termEnd.before(termDate.getEndDate())) {
                termEnd = termDate.getEndDate();
            }
        }
        return termEnd;
    }

    /**
     * Load the schedule term dates for a schedule term oid.
     * Keep a map of existing codes for lookup.
     *
     * @param scheduleTermOid String
     * @return Collection<ScheduleTermDate>
     */
    private Collection<ScheduleTermDate> getTermDates(String scheduleTermOid) {
        Collection<ScheduleTermDate> dates = null;

        if (m_termDateMap == null) {
            m_termDateMap = new HashMap<String, Collection<ScheduleTermDate>>();
        }

        if (!m_termDateMap.containsKey(scheduleTermOid)) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ScheduleTermDate.COL_SCHEDULE_TERM_OID, scheduleTermOid);
            QueryByCriteria query = new QueryByCriteria(ScheduleTermDate.class, criteria);
            dates = getBroker().getCollectionByQuery(query);
            m_termDateMap.put(scheduleTermOid, dates);
        }

        return m_termDateMap.get(scheduleTermOid);
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }
        m_courseLevel = translateAliasToJavaName(ALIAS_COURSE_LEVEL, true);
        m_courseType = translateAliasToJavaName(ALIAS_COURSE_TYPE, true);
        m_courseSeq = translateAliasToJavaName(ALIAS_COURSE_SEQ, true);
        m_districtCode = translateAliasToJavaName(ALIAS_DISTRICT_CODE, true);
        m_gradeSpan = translateAliasToJavaName(ALIAS_GRADE_SPAN, true);
        m_schoolCode = translateAliasToJavaName(ALIAS_SCHOOL_CODE, true);
        m_scedCode = translateAliasToJavaName(ALIAS_SCED_CODE, true);
        m_excludeSkl = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_orgCountryCode = translateAliasToJavaName(ALIAS_ORG_COUNTRY_CODE, true);
        m_doeCompletionStatus = translateAliasToJavaName(ALIAS_DOE_COMPLETION_STATUS, true);
        DataDictionary dictionary = getDataDictionary();
        if (dictionary != null) {
            m_ddOrgCountryCode = dictionary.findDataDictionaryFieldByAlias(ALIAS_ORG_COUNTRY_CODE);
        }
        m_bypassGradeValidations = ((Boolean) getParameter(PARAM_BYPASS_GRADE_VAL)).booleanValue();
        m_exclCrsScc = translateAliasToJavaName(ALIAS_EXCLUDE_CRS_SCC, true);
        m_exclCrsSsc = translateAliasToJavaName(ALIAS_EXCLUDE_CRS_SSC, true);
        m_exclCrsTrn = translateAliasToJavaName(ALIAS_EXCLUDE_CRS_TRN, true);
        m_dualInstitution = translateAliasToJavaName(ALIAS_DUAL_INSTITUTION, true);
    }

    /**
     * Load grade scales for transcript grade translation.
     */
    private void loadGradeScales() {
        /*
         * map grade scales by transcript definition Oid for easier retrieval.
         */
        m_gradeScales = new HashMap<String, GradeScale>();
        X2Criteria criteria = new X2Criteria();

        // Find the column definition that points to TRN_FINAL_GRADE
        criteria.addEqualTo(TranscriptColumnDefinition.COL_COLUMN_TYPE_CODE,
                Integer.valueOf(TranscriptColumnDefinition.COLUMN_TYPE_FINAL));
        QueryByCriteria query = new QueryByCriteria(TranscriptColumnDefinition.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                TranscriptColumnDefinition tcd = (TranscriptColumnDefinition) iterator.next();
                m_gradeScales.put(tcd.getTranscriptDefinitionOid(), tcd.getGradeScale());
            }
        } finally {
            iterator.close();
        }
    }
}
