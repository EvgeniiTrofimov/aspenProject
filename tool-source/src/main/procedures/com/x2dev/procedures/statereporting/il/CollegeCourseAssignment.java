/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.il;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.il.CollegeCourseAssignment.CourseAssignmentEntity.GradeData;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Illinois state export procedure for College Course Assignment.
 *
 * @author Follett Software Company
 */
public class CollegeCourseAssignment extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used by College Course Assignment.
     *
     * @author Follett Software Company
     */
    public static class CourseAssignmentEntity extends StateReportEntity {

        /**
         * The Class GradeData.
         */
        public static class GradeData {
            BigDecimal m_numericGrade = null;
            BigDecimal m_maxGrade = null;
            String m_letterGrade = null;
        }

        private SisStudent m_student = null;
        /**
         * Cached values
         */
        private CollegeCourseAssignment m_ccaData = null;

        /**
         * Helper class
         */
        private StudentHistoryHelper m_helper;

        /**
         * Student's most recent Entry enrollment record
         */
        private StudentEnrollment m_enrollment;

        /**
         * List of courses the student has taken. The bulk of this export.
         */
        private List<StudentScheduleSpan> m_schedules;

        /**
         * Collection of secondary schools oids for the student.
         */
        private Collection<String> m_secondarySchoolOids = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public CourseAssignmentEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Course taken in secondary.
         *
         * @param span StudentScheduleSpan
         * @return true if course taken in secondary school, otherwise false
         */
        public boolean courseTakenInSecondary(StudentScheduleSpan span) {
            boolean courseTakenInSecondary = false;
            MasterSchedule section = span.getSection();
            if (section != null && section.getSchoolCourse() != null
                    && section.getSchoolCourse().getSchoolOid() != null) {
                courseTakenInSecondary =
                        m_secondarySchoolOids.contains(section.getSchoolCourse().getSchoolOid()) ? true : false;
            }
            return courseTakenInSecondary;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;

            if (m_enrollment == null) {
                error = new StateReportValidationError("", "", "", "");
            }

            /*
             * If the (1) export type = Outside AND
             * (2) student's recent enrollment isn't an 200, 300, 600, 700, or 900...
             *
             * I will skip it.
             */
            else if (EXPORT_TYPE_OUTSIDE.equals(m_ccaData.m_exportType)) {
                String enrollmentCode = m_enrollment.getEnrollmentCode();
                if (!m_ccaData.m_outsideEnrollmentCodes.contains(enrollmentCode)) {
                    error = new StateReportValidationError("entity", "Enrollment", "", "");
                }
            }

            /*
             * If the (1) user turned on "Exclude students without final grades" AND
             * (2) the value for "Final Letter Grade" is empty...
             *
             * I will skip it.
             */
            else if (m_ccaData.m_excludeStudentsWoFinalGrades) {
                String finalLetterGrade = getFieldValue("Final Letter Grade");
                if (StringUtils.isEmpty(finalLetterGrade)) {
                    error = new StateReportValidationError("entity", "Final Letter Grade", "Missing final letter grade",
                            "Final letter grades will not appear");
                }
            }

            return error;
        }

        /**
         * By default, use the College Course Assignment (IL-EXPDATA-CCA-S) export definition.
         *
         * @return String
         */
        @Override
        public String getCurrentFormatDefinitionId() {
            String result = EXPORT_TYPE_STUDENT;
            if (m_ccaData.m_exportType.equals(EXPORT_TYPE_OUTSIDE)) {
                result = EXPORT_TYPE_OUTSIDE;
            }
            return result;
        }

        /**
         * Returns the Entry enrollment record.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getEffectiveEnrollment() {
            StudentEnrollment effectiveEnrollment = null;
            if (getScheduleInfo().getExitDate() != null) {
                effectiveEnrollment =
                        m_helper.getEnrollmentForDate(m_student.getOid(), getScheduleInfo().getExitDate(), "E");
            } else {
                effectiveEnrollment = m_enrollment;
            }

            return effectiveEnrollment;
        }

        public PlainDate getEndDate() {
            PlainDate result = null;
            StudentScheduleSpan info = getScheduleInfo();

            // Get the schedule change exit date if there is one.
            StudentScheduleChange exitChange = info.getExitChange();
            if (exitChange != null) {
                result = exitChange.getDate();
            }
            // Otherwise if there is no schedule change exit date or this date
            // is after the section exit date use the section exit date
            if (result == null || info.getExitDate().before(result)) {
                result = info.getExitDate();
            }

            /*
             * If the student has active in the course (does not have a drop date), and the
             * student has a final grade.
             * We must return the student course, with the final grade, and the Term End
             * Date associated with
             * the course term for the course.
             */
            if (result == null) {
                GradeData gradeData = CourseAssignmentEntity.getGradeData(m_ccaData, info.getTranscript(), null);

                if (!StringUtils.isEmpty(gradeData.m_letterGrade)) {
                    result = info.getExitDate();
                }
            }

            /*
             * The end date cannot be > "current date".
             */
            if (result != null && result.after(m_ccaData.m_reportDate)) {
                result = null;
            }
            return result;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] ";
            StudentScheduleSpan info = getScheduleInfo();
            if (info != null) {
                name += info.getSection().getCourseView();
            }

            return name;
        }

        /**
         * Gets the grade data.
         *
         * @param data StateReportData
         * @param transcript Transcript
         * @param parameter String
         * @return Grade data
         */
        public static GradeData getGradeData(StateReportData data, Transcript transcript, String parameter) {
            CollegeCourseAssignment scData = (CollegeCourseAssignment) data;

            GradeData gradeData = new GradeData();

            if (transcript != null) {
                String finalGrade = transcript.getFinalGrade();
                if (!StringUtils.isEmpty(finalGrade)) {
                    Collection<GradeScaleGradeDefinition> scales =
                            scData.m_gradeScaleDefinitions.get(transcript.getTranscriptDefinitionOid());
                    if (scales != null) {
                        // Search for the final grade as a grade code.
                        for (GradeScaleGradeDefinition gsg : scales) {
                            if (gsg.getGradeCode().equals(finalGrade)) {
                                gradeData.m_letterGrade = (String) gsg.getFieldValueByAlias(ALIAS_GRADE_CODE);
                                if (gsg.getNoNumericIndicator()) {
                                    gradeData.m_numericGrade = BigDecimal.valueOf(0);
                                } else {
                                    gradeData.m_numericGrade = gsg.getGradeValue();
                                }
                                gradeData.m_maxGrade = gsg.getGradeScale().getMaximumPoints();
                                break;
                            }
                        }
                    }

                    if (gradeData.m_letterGrade == null && gradeData.m_numericGrade == null) {
                        try {
                            // Search for the final grade as a numeric grade.
                            gradeData.m_numericGrade = new BigDecimal(finalGrade);
                            if (!RetrieveTranscriptGrade.PARAM_COURSE_NUMERIC_GRADE.equals(parameter)
                                    && scales != null) {
                                BigDecimal lastCutoff = BigDecimal.valueOf(-1);
                                // look up the letter grade from the numeric grade.
                                for (GradeScaleGradeDefinition gsg : scales) {
                                    if (!gsg.getNoNumericIndicator() &&
                                            gsg.getGradeCutoffValue().compareTo(gradeData.m_numericGrade) <= 0 &&
                                            gsg.getGradeCutoffValue().compareTo(lastCutoff) > 0) {
                                        gradeData.m_letterGrade = (String) gsg.getFieldValueByAlias(ALIAS_GRADE_CODE);
                                        lastCutoff = gsg.getGradeCutoffValue();
                                        gradeData.m_maxGrade = gsg.getGradeScale().getMaximumPoints();
                                    }
                                }
                            }
                        } catch (NumberFormatException nfe) {
                            // grade is not numeric.
                        }
                    }
                }
            }

            return gradeData;
        }

        /**
         * Returns the schedule info record for the schedule record(s)
         * based in the current row value.
         *
         * @return ScheduleInfo
         */
        public StudentScheduleSpan getScheduleInfo() {
            StudentScheduleSpan info = null;
            if (m_schedules != null && getCurrentRow() < m_schedules.size() && getCurrentRow() >= 0) {
                info = m_schedules.get(getCurrentRow());
            }

            return info;
        }

        /**
         * Gets the student.
         *
         * @return SisStudent
         */
        public SisStudent getStudent() {
            return m_student;
        }

        public PlainDate getTermEndDate() {
            PlainDate result = null;

            Collection<ScheduleTermDate> termDates =
                    m_ccaData.getTermDates(getScheduleInfo().getSection().getScheduleTermOid());
            for (ScheduleTermDate termDate : termDates) {
                if (result == null || result.before(termDate.getEndDate())) {
                    result = termDate.getEndDate();
                }
            }

            return result;
        }

        /**
         * Initialize the entity for the student bean provided.
         * This method finds the student schedule and student schedule change records for the
         * student
         * and generates a list of reportable schedule items.
         * The entity can produce multiple rows from these results.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.
         *      stateexports.StateReportData, com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_student = (SisStudent) bean;
            String studentOid = m_student.getOid();
            m_ccaData = (CollegeCourseAssignment) data;
            m_secondarySchoolOids = initSecondarySchools();
            m_helper = m_ccaData.m_helper;

            m_enrollment = m_helper.getEnrollmentForDate(studentOid, m_ccaData.m_reportDate, "E");

            if (m_enrollment != null && m_enrollment.getSchool() != null &&
                    !BooleanAsStringConverter.TRUE
                            .equals(m_enrollment.getSchool().getFieldValueByBeanPath(m_ccaData.m_fieldExcludeSkl))) {
                m_schedules = new ArrayList<StudentScheduleSpan>();
                List<StudentScheduleSpan> studentScheduleSpans = m_helper.getStudentScheduleSpans(m_student);
                for (StudentScheduleSpan span : studentScheduleSpans) {
                    // return only student courses with selected terms
                    String scheduleTermCode = null;
                    MasterSchedule section = span.getSection();
                    if (section != null) {
                        ScheduleTerm scheduleTerm = section.getScheduleTerm();
                        if (scheduleTerm != null) {
                            scheduleTermCode = scheduleTerm.getCode();
                        }
                    }

                    if (m_ccaData.m_selectedTermCodes == null ||
                            m_ccaData.m_selectedTermCodes.isEmpty() ||
                            m_ccaData.m_selectedTermCodes.contains(scheduleTermCode)) {
                        if (span.getExitDate().after(span.getEntryDate())) // catch some error spans
                                                                           // from history helper.
                        {
                            // return ONLY student courses records where ssc.[DOE DUAL CREDIT] =
                            // True
                            if (span.getSchedule() != null) {
                                String dualCredInd = (String) span.getSchedule()
                                        .getFieldValueByAlias(RetrieveCourse.DOE_DUAL_CREDIT);
                                if (BooleanAsStringConverter.TRUE.equals(dualCredInd)) {
                                    m_schedules.add(span);
                                }
                            }
                        }
                    }
                }

                setRowCount(m_schedules.size());
            } else {
                setRowCount(0);
            }

            m_ccaData.m_numOfRecords = m_ccaData.m_numOfRecords + getRowCount();
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
         * Inits the secondary schools.
         *
         * @return Collection
         */
        private Collection<String> initSecondarySchools() {
            // Get all student's school associations for current year.
            Collection<School> studSchools = m_ccaData.getStudentSchoolsForContextYear(m_student,
                    m_ccaData.getOrganization().getCurrentContextOid());

            // Determine secondary schools oids.
            Collection<String> secSchoolsOids = new ArrayList<String>();
            for (School stdSkl : studSchools) {
                secSchoolsOids.add(stdSkl.getOid());
            }

            return secSchoolsOids;
        }
    }

    /**
     * Returns course information associated with this section.
     *
     * @author Follett Software Company
     */
    protected class RetrieveCourse implements FieldRetriever {
        private final static String DOE_DUAL_CREDIT = "DOE DUAL CREDIT";

        private final String ALIAS_DOE_IPEDS_NBR = "DOE IPEDS NBR";

        private final String DOE_ARTICULATED_CREDIT = "DOE ARTICULATED CREDIT";
        private final String DOE_CLASS_COURSE_SETTING = "DOE CLASS COURSE SETTING";
        private final String DOE_COURSE_LEVEL = "DOE COURSE LEVEL";
        private final String DOE_COURSE_SETTING = "DOE COURSE SETTING";
        private final String DOE_STATE_COURSE_ID = "DOE STATE COURSE ID";
        private final String DOE_COURSE_FACILITY_TYPE = "DOE COURSE FACILITY TYPE";
        private final String DOE_COURSE_FACILITY_NAME = "DOE COURSE FACILITY NAME";

        private final String PARAM_ARTICULATED_CREDIT = "ARTICULATED CREDIT";
        private final String PARAM_COURSE_CREDIT = "COURSE CREDIT";
        private final String PARAM_COURSE_LEVEL = "COURSE LEVEL";
        private final String PARAM_COURSE_SETTING = "COURSE SETTING";
        private final String PARAM_DUAL_CREDIT = "DUAL CREDIT";
        private final String PARAM_IPEDS_NUMBER = "IPEDS NUMBER";
        private final String PARAM_LOCAL_COURSE_ID = "LOCAL COURSE ID";
        private final String PARAM_LOCAL_COURSE_TITLE = "LOCAL COURSE TITLE";
        private final String PARAM_STATE_COURSE_CODE = "STATE COURSE CODE";
        private final String PARAM_COURSE_SCHOOL_YEAR = "COURSE SCHOOL YEAR";
        private final String PARAM_COURSE_GRADE_LEVEL = "COURSE GRADE LEVEL";
        private final String PARAM_COURSE_FACILITY_TYPE = "COURSE FACILITY TYPE";
        private final String PARAM_COURSE_FACILITY_NAME = "COURSE FACILITY NAME";
        private final String PARAM_COURSE_END_DATE = "COURSE END DATE";
        private final String PARAM_COURSE_START_DATE = "COURSE START DATE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            Object result = null;
            CourseAssignmentEntity ccae = (CourseAssignmentEntity) entity;
            StudentScheduleSpan info = ccae.getScheduleInfo();
            String param = (String) field.getParameter();
            Course course = info.getSection().getSchoolCourse().getCourse();

            if (course != null) {
                if (param.equals(PARAM_STATE_COURSE_CODE)) {
                    // get from transcript if you can
                    if (info.getTranscript() != null) {
                        String courseOverride =
                                (String) info.getTranscript().getFieldValueByAlias(ALIAS_COURSE_OVERRIDE);
                        result = courseOverride;
                    }

                    if (result == null) {
                        result = info.getSection().getSchoolCourse().getCourse()
                                .getFieldValueByAlias(DOE_STATE_COURSE_ID);
                    }
                } else if (param.equals(PARAM_LOCAL_COURSE_ID)) {
                    result = info.getSection().getSchoolCourse().getCourse().getNumber();
                } else if (param.equals(PARAM_LOCAL_COURSE_TITLE)) {
                    result = info.getSection().getSchoolCourse().getCourse().getDescription();
                } else if (param.equals(PARAM_COURSE_LEVEL)) {
                    String value = (String) info.getSection().getSchoolCourse().getCourse()
                            .getFieldValueByAlias(DOE_COURSE_LEVEL);
                    result = lookupReferenceCodeByAlias(DOE_COURSE_LEVEL, value,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                } else if (param.equals(PARAM_COURSE_CREDIT)) {
                    result = info.getSection().getSchoolCourse().getCourse().getCredit();
                } else if (param.equals(PARAM_ARTICULATED_CREDIT)) {
                    result = info.getSection().getSchoolCourse().getCourse()
                            .getFieldValueByAlias(DOE_ARTICULATED_CREDIT);
                } else if (param.equals(PARAM_DUAL_CREDIT)) {
                    StudentSchedule schedule = info.getSchedule();
                    if (schedule != null) {
                        result = schedule.getFieldValueByAlias(DOE_DUAL_CREDIT);
                    }
                } else if (param.equals(PARAM_COURSE_SETTING)) {
                    String value = (String) info.getSection().getFieldValueByAlias(DOE_CLASS_COURSE_SETTING);
                    if (StringUtils.isEmpty(value)) {
                        value = (String) info.getSection().getSchoolCourse().getCourse()
                                .getFieldValueByAlias(DOE_COURSE_SETTING);
                    }
                    result = lookupReferenceCodeByAlias(DOE_COURSE_SETTING, value,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                } else if (param.equals(PARAM_COURSE_SCHOOL_YEAR)) {
                    result = Integer.valueOf(
                            info.getSection().getSchoolCourse().getCourse().getDistrictContext().getSchoolYear());
                } else if (param.equals(PARAM_COURSE_GRADE_LEVEL)) {
                    result = info.getSection().getSchoolCourse().getCourse().getGradeLevel();
                } else if (param.equals(PARAM_COURSE_FACILITY_TYPE)) {
                    String value = (String) info.getSection().getSchoolCourse().getCourse()
                            .getFieldValueByAlias(DOE_COURSE_FACILITY_TYPE);
                    result = lookupReferenceCodeByAlias(DOE_COURSE_FACILITY_TYPE, value,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                } else if (param.equals(PARAM_COURSE_FACILITY_NAME)) {
                    result = info.getSection().getSchoolCourse().getCourse()
                            .getFieldValueByAlias(DOE_COURSE_FACILITY_NAME);
                } else if (param.equals(PARAM_COURSE_START_DATE)) {
                    // Get the schedule change entry date if there is one.
                    StudentScheduleChange entryChange = info.getEntryChange();
                    if (entryChange != null) {
                        result = entryChange.getDate();
                        if (entryChange.getEffectiveDate() != null
                                && entryChange.getEffectiveDate().before(entryChange.getDate())) {
                            result = entryChange.getEffectiveDate();
                        }
                    }
                    // Otherwise if there is no schedule change entry date or this date
                    // is before the section entry date use the section entry date.
                    if (result == null || info.getEntryDate().after((PlainDate) result)) {
                        result = info.getEntryDate();
                    }
                } else if (param.equals(PARAM_COURSE_END_DATE)) {
                    result = ccae.getEndDate();
                } else if (param.equals(PARAM_IPEDS_NUMBER)) {
                    StudentSchedule schedule = info.getSchedule();
                    if (schedule != null) {
                        String dualCredit = (String) schedule.getFieldValueByAlias(DOE_DUAL_CREDIT);

                        if (dualCredit != null && BooleanAsStringConverter.TRUE.equals(dualCredit)) {
                            result = schedule.getFieldValueByAlias(ALIAS_DOE_IPEDS_NBR);
                        }
                    }
                }
            }

            return result;
        }
    }

    /**
     * Returns transcript grade from the transcript record associated with this section or class.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTranscriptGrade implements FieldRetriever {
        private final static String PARAM_COURSE_NUMERIC_GRADE = "COURSE NUMERIC GRADE";
        private final static String PARAM_FINAL_LETTER_GRADE = "FINAL LETTER GRADE";
        private final static String PARAM_MAX_NUMERIC_GRADE = "MAX NUMERIC GRADE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            CollegeCourseAssignment scData = (CollegeCourseAssignment) data;
            CourseAssignmentEntity scae = (CourseAssignmentEntity) entity;
            StudentScheduleSpan info = scae.getScheduleInfo();
            Transcript transcript = info.getTranscript();
            String parameter = (String) field.getParameter();

            GradeData gradeData = CourseAssignmentEntity.getGradeData(scData, transcript, parameter);

            // Check return type for the calculation.
            Object returnObject = null;
            if (PARAM_FINAL_LETTER_GRADE.equals(parameter)) {
                returnObject = gradeData.m_letterGrade;
                /*
                 * If:
                 * 1) The student has withdrawn from school and the course does not have a final
                 * grade OR
                 * the student has dropped a course and the course does not have a final grade.
                 * We must return the students course records with the drop date found in the
                 * student schedule history.
                 * We must return a value of "W" for the Final Grade.
                 */
                PlainDate date = scae.getEndDate();
                PlainDate termEndDate = scae.getTermEndDate();

                if (date != null && !date.equals(termEndDate)) {
                    returnObject = null;
                }
            } else if (PARAM_COURSE_NUMERIC_GRADE.equals(parameter)) {
                returnObject = gradeData.m_numericGrade;
            } else if (PARAM_MAX_NUMERIC_GRADE.equals(parameter)) {
                returnObject = gradeData.m_maxGrade;
            }

            return returnObject;
        }
    }

    /**
     * Retrieve the RCDTS of the student's home/serving school
     *
     * Parameter this retriever accepts:
     * - "H" for the home school's RCDTS
     * - "S" for the serving school's RCDTS.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRcdts implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            CourseAssignmentEntity ccaEntity = (CourseAssignmentEntity) entity;
            CollegeCourseAssignment ccaData = (CollegeCourseAssignment) data;
            String rcdts = null;

            String homeSchoolRCDTS = null;

            if (ccaEntity.getEffectiveEnrollment() != null && ccaEntity.getEffectiveEnrollment().getSchool() != null) {
                homeSchoolRCDTS = (String) ccaEntity.getEffectiveEnrollment().getSchool()
                        .getFieldValueByBeanPath(ccaData.m_fieldSchoolCode);
            }

            if (param.equals("H")) {
                rcdts = homeSchoolRCDTS;
            } else if (param.equals("S")) {
                StudentScheduleSpan currentSpan = ccaEntity.getScheduleInfo();
                if (currentSpan != null) {
                    if (currentSpan.getTranscript() != null && currentSpan.getTranscript().getSchool() != null
                            && !StringUtils.isEmpty(currentSpan.getTranscript().getFinalGrade())) // if
                                                                                                  // final
                                                                                                  // grade
                                                                                                  // exists
                                                                                                  // for
                                                                                                  // section/transcript,
                                                                                                  // pull
                                                                                                  // school
                                                                                                  // ID
                                                                                                  // from
                                                                                                  // transcript
                    {
                        rcdts = (String) currentSpan.getTranscript().getSchool()
                                .getFieldValueByBeanPath(ccaData.m_fieldSchoolCode);
                    } else // if no final grade or transcript exists, pull school ID from the
                           // section/span
                    {
                        if (currentSpan.getSection() != null && currentSpan.getSection().getSchoolCourse() != null
                                && currentSpan.getSection().getSchoolCourse().getSchool() != null) {
                            rcdts = (String) currentSpan.getSection().getSchoolCourse().getSchool()
                                    .getFieldValueByBeanPath(ccaData.m_fieldSchoolCode);
                        }
                    }
                }
                if (rcdts == null) {
                    rcdts = homeSchoolRCDTS;
                }
            }
            return rcdts;
        }
    }

    /**
     * Returns a schedule's section information.
     *
     * @author Follett Software Company
     */
    protected class RetrieveSection implements FieldRetriever {
        private final String PARAM_SECTION_NUMBER = "SECTION NUMBER";
        private final String PARAM_TERM = "TERM";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            Object result = null;
            CourseAssignmentEntity ccae = (CourseAssignmentEntity) entity;
            StudentScheduleSpan info = ccae.getScheduleInfo();
            String param = (String) field.getParameter();

            if (param.equals(PARAM_SECTION_NUMBER)) {
                String crsView = info.getSection().getCourseView();
                String cleanValue = EMPTY_STRING;

                if (!StringUtils.isEmpty(crsView)) {
                    crsView = crsView.replaceAll(REGEX_DOT, STRING_HYPHEN);
                    Matcher matcher = m_illegalCrsViewChars.matcher(crsView);
                    cleanValue = matcher.replaceAll(EMPTY_STRING);
                }

                result = cleanValue;
            } else if (param.equals(PARAM_TERM)) {
                String termView = info.getSection().getTermView();
                result = data.lookupReferenceCodeByBeanPath(ScheduleTerm.class, ScheduleTerm.COL_CODE, termView,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }

            return result;
        }

    }

    /**
     * Returns a field value that has been stripped of illegal characters for a name field.
     * For first and last names.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String value = (String) getProperty(entity.getBean(), field.getBeanPath());
            Matcher matcher = m_illegalNameCharacters.matcher(value);

            return matcher.replaceAll("");
        }
    }

    /**
     * Alias fields
     */
    protected static final char CHAR_COMMA = ',';
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_EXCLUDE_COURSE = "DOE EXCLUDE CRS";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_EXCLUDE_SECTION = "DOE EXCLUDE MST";
    protected static final String ALIAS_EXCLUDE_STUDENT = "DOE EXCLUDE STD";
    protected static final String ALIAS_GRADE_CODE = "DOE GRADE CODE";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVICE";
    protected static final String ALIAS_STATE_COURSE_ID = "DOE STATE COURSE ID";
    protected static final String ALIAS_COURSE_OVERRIDE = "DOE COURSE OVERRIDE TRN";
    protected static final String ALIAS_RCDTS_FOR_SERVING_SCHOOL = "DOE OUTPLACEMENT DI";

    protected static final String EXPORT_TYPE_OUTSIDE = "O";
    protected static final String EXPORT_TYPE_STUDENT = "S";

    protected static final String ILLEGAL_NAME_CHARACTERS = "[^-A-z ]";
    protected static final String ILLEGAL_CRS_VIEW_CHARACTERS = "[^-A-z0-9]";

    protected static final String INPUT_PARAM_TERM = "term";

    protected static final String OUTSIDE_ENROLLMENT_CODE = "outside";

    protected static final String PARAM_EXPORT_TYPE = "exportType";
    protected static final String PARAM_EXCLUDE_NO_FINAL_GRADES = "excludeStudentWoFinalGrade";
    protected static final String PARAM_REPORT_DATE = "reportDate";

    protected static final String REF_CODE_SCHEDULE_TERMS = "rtbSchTermCode";

    protected static final String REGEX_DOT = "[\\.]";
    protected static final String STRING_HYPHEN = "-";

    /*
     * Supporting instance variables.
     */
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected EnrollmentManager m_enrollmentManager;
    protected boolean m_excludeStudentsWoFinalGrades = false;
    protected String m_exportType;
    protected String m_fieldDistrictCode;
    protected String m_fieldExcludeSkl;
    protected String m_fieldTrnCourseOverride;
    protected String m_fieldSchoolCode;
    protected String m_fieldStateCourseId;
    protected Map<String, ReferenceCode> m_gradeCodes = new HashMap<String, ReferenceCode>();
    protected Map<String, GradeScale> m_gradeScales = new HashMap<String, GradeScale>();
    protected Map<String, Collection<GradeScaleGradeDefinition>> m_gradeScaleDefinitions;
    protected Pattern m_illegalNameCharacters;
    protected Pattern m_illegalCrsViewChars;
    protected int m_numOfRecords;
    protected PlainDate m_reportDate;
    protected Map<String, Map<String, Set<PlainDate>>> m_schoolsToCalendars;
    protected ArrayList<String> m_selectedTermCodes;
    protected Map<String, String> m_termCodesMap = new HashMap();
    protected Map<String, Collection<ScheduleTermDate>> m_termDateMap;
    protected Set<String> m_outsideEnrollmentCodes;

    protected StudentHistoryHelper m_helper;

    /**
     * The header shall be in the format of:
     * File Type_Total Number of Records_File Name including extension_File Sent Date_RCDTS (DOE
     * DISTRICT ID).
     *
     * @return String
     */
    @Override
    public String getHeading() {
        StringBuilder heading = new StringBuilder(100);
        heading.append("College Course Assignment");
        heading.append(',');
        heading.append(m_numOfRecords);
        heading.append(',');
        heading.append(getFileName());
        heading.append(',');
        if (m_reportDate != null) {
            heading.append(m_dateFormat.format(m_reportDate));
        }
        heading.append(',');
        if (getOrganization() != null) {
            heading.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        }
        heading.append("\n");
        return heading.toString();
    }

    /**
     * Returns the list of the secondary schools(school objects) the student has for the current
     * year context.
     *
     * @param student SisStudent
     * @param contextYearOid String
     * @return Collection
     */
    public Collection getStudentSchoolsForContextYear(SisStudent student, String contextYearOid) {
        Collection secondarySchoolsForContext = new LinkedList();

        /*
         * TODO: consider to do query directly.
         */
        if (contextYearOid != null) {
            Iterator stdSchoolIt = student.getStudentSchools().iterator();
            while (stdSchoolIt.hasNext()) {
                StudentSchool stduentSchool = (StudentSchool) stdSchoolIt.next();
                /*
                 * Same context year id?
                 */
                if (stduentSchool.getDistrictContextOid().equalsIgnoreCase(contextYearOid)) {
                    secondarySchoolsForContext.add(stduentSchool.getSchool());
                }
            }
        }

        return secondarySchoolsForContext;
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        // Load initialization data
        initializeFields();

        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        m_schoolsToCalendars = new HashMap<String, Map<String, Set<PlainDate>>>();
        m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
        m_illegalCrsViewChars = Pattern.compile(ILLEGAL_CRS_VIEW_CHARACTERS);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            loadTermCodes();

            // Get selected Term Codes
            String rcdOidList = (String) getParameter(INPUT_PARAM_TERM);
            HashSet<String> rcdOids = new HashSet(StringUtils.convertDelimitedStringToList(rcdOidList, CHAR_COMMA));
            m_selectedTermCodes = new ArrayList();

            for (String rcdOid : rcdOids) {
                String termCode = m_termCodesMap.get(rcdOid);
                m_selectedTermCodes.add(termCode);
            }

            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

            // modify the student schedule criteria
            X2Criteria studentScheduleCriteria = m_helper.getStudentScheduleCriteria();
            studentScheduleCriteria.addNotEmpty(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_fieldStateCourseId, getBroker().getPersistenceKey());
            studentScheduleCriteria.addNotEqualTo(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                    Course.COL_CREDIT, new BigDecimal(0));
            studentScheduleCriteria.addNotNull(StudentSchedule.COL_SECTION_OID);
            studentScheduleCriteria.addNotEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                    m_fieldExcludeSkl, BooleanAsStringConverter.TRUE);

            // modify the student schedule change criteria section term started before report
            // date.???????????????????)
            X2Criteria studentScheduleChangeCriteria = m_helper.getStudentScheduleChangeCriteria();
            studentScheduleChangeCriteria.addNotEmpty(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_fieldStateCourseId, getBroker().getPersistenceKey());
            studentScheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                    m_fieldExcludeSkl, BooleanAsStringConverter.TRUE);

            // modify the student transcript criteria
            X2Criteria studentTranscriptCriteria = m_helper.getStudentTranscriptCriteria();
            X2Criteria emptyIdsCriteria = new X2Criteria();
            if (!StringUtils.isEmpty(m_fieldTrnCourseOverride)) {
                emptyIdsCriteria.addNotEmpty(m_fieldTrnCourseOverride, getBroker().getPersistenceKey());
            }
            X2Criteria nonEmptyIdCriteria = new X2Criteria();
            nonEmptyIdCriteria.addNotEmpty(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_fieldStateCourseId, getBroker().getPersistenceKey());
            emptyIdsCriteria.addOrCriteria(nonEmptyIdCriteria);
            studentTranscriptCriteria.addAndCriteria(emptyIdsCriteria);
            if (m_excludeStudentsWoFinalGrades) {
                studentTranscriptCriteria.addNotEmpty(Transcript.COL_FINAL_GRADE, getBroker().getPersistenceKey());
            }

            loadGradeScales();

            /*
             * Get only 'outside' enrollment codes. This will be used to check if students are
             * outside students
             */
            m_outsideEnrollmentCodes = new HashSet<String>();
            DataDictionaryField enrollmentCodeField =
                    getDataDictionaryField(StudentEnrollment.class, StudentEnrollment.COL_ENROLLMENT_CODE);
            if (enrollmentCodeField != null) {
                String enrollmentCodeRefTableOid = enrollmentCodeField.getReferenceTableOid();
                Map<String, ReferenceCode> referenceCodes = getReferenceCodes(enrollmentCodeRefTableOid);
                Collection<ReferenceCode> enrollmentCodes = referenceCodes.values();
                for (ReferenceCode refCode : enrollmentCodes) {
                    if (OUTSIDE_ENROLLMENT_CODE.equals(refCode.getLocalCode())) {
                        m_outsideEnrollmentCodes.add(refCode.getCode());
                    }
                }
            }

            QueryByCriteria studentQuery = m_helper.getStudentQuery(false);
            setQuery(studentQuery);
            setEntityClass(CourseAssignmentEntity.class);

            // Build maps of retriever functions
            Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("CCA-RCDTS", new RetrieveRcdts());
            calcs.put("CCA-COURSE", new RetrieveCourse());
            calcs.put("CCA-GRADE", new RetrieveTranscriptGrade());
            calcs.put("CCA-SECTION", new RetrieveSection());
            calcs.put("CCA-CLEAN", new RetrieveStripNameChar());
            super.addCalcs(calcs);
        }
    }

    /**
     * Returns the days-in-session for the given school and calendar combination.
     *
     * @param school SisSchool
     * @param calendar String
     * @return Set of PlainDate objects
     */
    protected Set<PlainDate> getCalendarDays(SisSchool school, String calendar) {
        Map<String, Set<PlainDate>> calendarData = null;
        Set<PlainDate> calendarDates = new HashSet<PlainDate>();
        if (school != null && !m_schoolsToCalendars.containsKey(school.getOid())) {
            PlainDate startDate = null;
            if (school.getActiveSchedule() != null) {
                startDate = school.getActiveSchedule().getStartDate();
            } else {
                startDate = getOrganization().getCurrentContext().getStartDate();
            }
            calendarData = m_enrollmentManager.getCalendarLookup(school, startDate, m_reportDate);
            m_schoolsToCalendars.put(school.getOid(), calendarData);
        }

        if (school != null) {
            calendarData = m_schoolsToCalendars.get(school.getOid());
            Set<PlainDate> dates = calendarData.get(calendar);
            if (dates != null) {
                calendarDates.addAll(dates);
            }
        }

        return calendarDates;
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
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ScheduleTermDate.COL_SCHEDULE_TERM_OID, scheduleTermOid);
            QueryByCriteria query = new QueryByCriteria(ScheduleTermDate.class, criteria);
            dates = getBroker().getCollectionByQuery(query);
            m_termDateMap.put(scheduleTermOid, dates);
        }

        return m_termDateMap.get(scheduleTermOid);
    }

    /**
     * Generate the filename for the report.
     *
     * @return <district name>_<report date>_001.csv
     */
    private String getFileName() {
        StringBuilder fileName = new StringBuilder();
        if (getOrganization() != null) {
            fileName.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        }
        fileName.append("_");
        if (m_reportDate != null) {
            fileName.append(new SimpleDateFormat("MMddyyyy").format(m_reportDate));
        }
        fileName.append("_");
        fileName.append("001.csv");
        return fileName.toString();
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldExcludeSkl = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldStateCourseId = translateAliasToJavaName(ALIAS_STATE_COURSE_ID, true);
        m_fieldTrnCourseOverride = translateAliasToJavaName(ALIAS_COURSE_OVERRIDE, false);

        m_exportType = (String) getParameter(PARAM_EXPORT_TYPE);
        if (m_exportType == null) {
            m_exportType = EXPORT_TYPE_STUDENT;
        }
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }
        Boolean excludeTranscriptsWithoutFinal = (Boolean) getParameter(PARAM_EXCLUDE_NO_FINAL_GRADES);
        if (excludeTranscriptsWithoutFinal != null) {
            m_excludeStudentsWoFinalGrades = excludeTranscriptsWithoutFinal.booleanValue();
        }

        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);

    }

    /**
     * Load grade scales for transcript grade translation.
     */
    private void loadGradeScales() {
        m_gradeScaleDefinitions = new HashMap<String, Collection<GradeScaleGradeDefinition>>();
        /*
         * Load a map of grade scale definitions by grade scale oid.
         */
        Map<String, Collection<GradeScaleGradeDefinition>> gradeScalesByGsd;
        // transcriptCriteria = new Criteria();
        QueryByCriteria query = new QueryByCriteria(GradeScaleGradeDefinition.class, null);
        gradeScalesByGsd =
                getBroker().getGroupedCollectionByQuery(query, GradeScaleGradeDefinition.COL_GRADE_SCALE_OID, 10);

        /*
         * Remap grade scales by transcript definition Oid for easier retrieval.
         */
        m_gradeScaleDefinitions = new HashMap<String, Collection<GradeScaleGradeDefinition>>();
        X2Criteria transcriptCriteria = new X2Criteria();
        transcriptCriteria.addEqualTo(TranscriptColumnDefinition.COL_COLUMN_TYPE_CODE,
                Integer.valueOf(TranscriptColumnDefinition.COLUMN_TYPE_FINAL));
        query = new QueryByCriteria(TranscriptColumnDefinition.class, transcriptCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                TranscriptColumnDefinition gcd = (TranscriptColumnDefinition) iterator.next();
                Collection<GradeScaleGradeDefinition> gsgs = gradeScalesByGsd.get(gcd.getGradeScaleOid());
                if (gsgs != null) {
                    m_gradeScaleDefinitions.put(gcd.getTranscriptDefinitionOid(), gsgs);
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Load in all the Schedule Term Codes.
     */
    private void loadTermCodes() {
        X2Criteria termCodesCriteria = new X2Criteria();
        termCodesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, REF_CODE_SCHEDULE_TERMS);
        BeanQuery termCodesQuery = new BeanQuery(ReferenceCode.class, termCodesCriteria);
        Collection<ReferenceCode> termCodes = getBroker().getCollectionByQuery(termCodesQuery);

        for (ReferenceCode referenceCode : termCodes) {
            m_termCodesMap.put(referenceCode.getOid(), referenceCode.getCode());
        }
    }
}
