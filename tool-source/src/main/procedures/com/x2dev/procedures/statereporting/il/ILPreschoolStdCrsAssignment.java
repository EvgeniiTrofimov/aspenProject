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
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.il.ILPreschoolStdCrsAssignment.PreschoolCrsAssignmentEntity.GradeData;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Illinois state export procedure for Student Course Assignment.
 *
 * @author Follett Software Company
 */
public class ILPreschoolStdCrsAssignment extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used by Student Course Assignment.
     *
     * @author Follett Software Company
     */
    public static class PreschoolCrsAssignmentEntity extends StateReportEntity {

        /**
         * The Class GradeData.
         */
        public static class GradeData {
            String m_letterGrade = null;
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
            ILPreschoolStdCrsAssignment scData = (ILPreschoolStdCrsAssignment) data;
            BigDecimal numericGrade = null;
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
                                    numericGrade = BigDecimal.valueOf(0);
                                } else {
                                    numericGrade = gsg.getGradeValue();
                                }
                                break;
                            }
                        }
                    }

                    if (gradeData.m_letterGrade == null) {
                        try {
                            // Search for the final grade as a numeric grade.
                            numericGrade = new BigDecimal(finalGrade);
                            if (scales != null) {
                                BigDecimal lastCutoff = BigDecimal.valueOf(-1);
                                // look up the letter grade from the numeric grade.
                                for (GradeScaleGradeDefinition gsg : scales) {
                                    if (!gsg.getNoNumericIndicator() &&
                                            gsg.getGradeCutoffValue().compareTo(numericGrade) <= 0 &&
                                            gsg.getGradeCutoffValue().compareTo(lastCutoff) > 0) {
                                        gradeData.m_letterGrade = (String) gsg.getFieldValueByAlias(ALIAS_GRADE_CODE);
                                        lastCutoff = gsg.getGradeCutoffValue();
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
         * List of the student's absences. Used by the actual attendance field.
         */
        // Map<String, List<StudentPeriodAttendance>> m_absences = new HashMap<String,
        // List<StudentPeriodAttendance>>();
        Map<String, Integer> m_absences = new HashMap<String, Integer>();

        /**
         * Student's most recent Entry enrollment record
         */
        private StudentEnrollment m_enrollment;

        /**
         * Helper class
         */
        private StudentHistoryHelper m_helper;

        /**
         * Cached values
         */
        private ILPreschoolStdCrsAssignment m_scaData = null;

        /**
         * List of courses the student has taken. The bulk of this export.
         */
        private List<StudentScheduleSpan> m_schedules;

        /**
         * Collection of secondary schools oids for the student.
         */
        private Collection<String> m_secondarySchoolOids = null;


        private SisStudent m_student = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public PreschoolCrsAssignmentEntity() {
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
             * If the (1) user turned on "Exclude students without final grades" AND
             * (2) the value for "Final Letter Grade" is empty...
             *
             * I will skip it.
             */
            else if (m_scaData.m_excludeStudentsWoFinalGrades) {
                String finalLetterGrade = getFieldValue("Final Letter Grade");
                if (StringUtils.isEmpty(finalLetterGrade)) {
                    error = new StateReportValidationError("entity", "Final Letter Grade", "Missing final letter grade",
                            "Final letter grades will not appear");
                }
            }

            return error;
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
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.stateexports.StateReportData,
         *      com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_student = (SisStudent) bean;

            String studentOid = m_student.getOid();
            m_scaData = (ILPreschoolStdCrsAssignment) data;
            m_secondarySchoolOids = getSecondarySchoolOids();
            m_helper = m_scaData.m_helper;

            m_enrollment = m_helper.getEnrollmentForDate(studentOid, m_scaData.m_reportDate, "E");

            StudentEnrollment withdrawalEnrollment =
                    m_helper.getEnrollmentForDate(studentOid, m_scaData.m_reportDate, "W");

            boolean withdrawnOnReportDate = false;
            boolean withdrawnInPreviousYear = false;

            // Do not show students that were withdrawn before current school year.
            if (withdrawalEnrollment != null) {
                withdrawnOnReportDate =
                        withdrawalEnrollment.getEnrollmentDate().after(m_enrollment.getEnrollmentDate());
                withdrawnInPreviousYear =
                        withdrawalEnrollment.getEnrollmentDate().before(m_scaData.getCurrentContext().getStartDate());

                if (withdrawnOnReportDate && withdrawnInPreviousYear) {
                    setRowCount(0);
                    return;
                }
            }

            if (m_enrollment != null && m_enrollment.getSchool() != null &&
                    !BooleanAsStringConverter.TRUE
                            .equals(m_enrollment.getSchool().getFieldValueByBeanPath(m_scaData.m_fieldExcludeSkl))) {
                m_schedules = new ArrayList<StudentScheduleSpan>();
                List<StudentScheduleSpan> studentScheduleSpans = m_helper.getStudentScheduleSpans(m_student);

                for (StudentScheduleSpan span : studentScheduleSpans) {
                    // if student is withdrawn on report date and was withdrawn in this year,
                    // include record only if
                    // course has final grade.
                    if (withdrawnOnReportDate && !withdrawnInPreviousYear) {
                        boolean emptyFinalGrade =
                                span.getTranscript() == null
                                        || StringUtils.isEmpty(span.getTranscript().getFinalGrade());

                        if (emptyFinalGrade) {
                            continue;
                        }
                    }

                    // return only student courses with selected terms
                    String scheduleTermCode = null;
                    MasterSchedule section = span.getSection();
                    if (section != null) {
                        ScheduleTerm scheduleTerm = section.getScheduleTerm();
                        if (scheduleTerm != null) {
                            scheduleTermCode = scheduleTerm.getCode();
                        }
                    }

                    if (m_scaData.m_selectedTermCodes == null ||
                            m_scaData.m_selectedTermCodes.isEmpty() ||
                            m_scaData.m_selectedTermCodes.contains(scheduleTermCode)) {
                        if (span.getExitDate().after(span.getEntryDate())) // catch some error spans
                                                                           // from history helper.
                        {
                            // return ONLY student courses records where ssc.[DOE DUAL CREDIT] =
                            // False or empty
                            if (span.getSchedule() == null ||
                                    StringUtils.isEmpty((String) span.getSchedule()
                                            .getFieldValueByAlias(RetrieveCourse.DOE_DUAL_CREDIT))
                                    ||
                                    BooleanAsStringConverter.FALSE.equals(
                                            span.getSchedule().getFieldValueByAlias(RetrieveCourse.DOE_DUAL_CREDIT))) {
                                m_schedules.add(span);
                            }
                        }
                    }
                }

                m_absences = m_scaData.m_absences.get(studentOid);
                setRowCount(m_schedules.size());
            } else {
                setRowCount(0);
            }

            m_scaData.m_numOfRecords = m_scaData.m_numOfRecords + getRowCount();
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
         * returns the secondary schools oids.
         *
         * @return Collection
         */
        private Collection<String> getSecondarySchoolOids() {
            // Get all student's school associations for current year.
            Collection<String> secSchoolsOids = new ArrayList<String>();
            Collection<StudentSchool> studSchools = m_scaData.m_sskMapByStd.get(m_student.getOid());
            if (studSchools != null && !studSchools.isEmpty()) {
                // Determine secondary schools oids.
                for (StudentSchool stdSkl : studSchools) {
                    secSchoolsOids.add(stdSkl.getSchoolOid());
                }
            }
            return secSchoolsOids;
        }
    }

    /**
     * Returns transcript class information from the transcript record associated with this section
     * or class.
     *
     * @author Follett Software Company
     */
    protected class RetrieveAttendance implements FieldRetriever {

        private final String PARAM_ACTUAL_ATTENDANCE = "ACTUAL ATTENDANCE";
        private final String PARAM_TOTAL_ATTENDANCE = "TOTAL ATTENDANCE";

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
            PreschoolCrsAssignmentEntity scae = (PreschoolCrsAssignmentEntity) entity;
            SisStudent student = (SisStudent) scae.getBean();
            Set<PlainDate> calendarDays = getCalendarDays(student.getSchool(), student.getCalendarCode());

            Object result = null;
            String param = (String) field.getParameter();

            MasterSchedule mst = null;
            StudentScheduleSpan scheduleInfo = scae.getScheduleInfo();
            if (scheduleInfo.getTranscript() != null) {
                mst = scheduleInfo.getTranscript().getMasterSchedule();
            } else if (scheduleInfo.getSection() != null) {
                mst = scheduleInfo.getSection();
            }

            Integer totalAttendance = Integer.valueOf(calendarDays.size());
            if (param.equals(PARAM_ACTUAL_ATTENDANCE)) {
                int absences = 0;
                if (scae.m_absences != null) {
                    if (mst != null) {
                        String mstOid = mst.getOid();
                        if (scae.m_absences.get(mstOid) != null) {
                            absences = scae.m_absences.get(mstOid).intValue();
                        }
                    }
                }
                result = Integer.valueOf(totalAttendance.intValue() - absences);
            } else if (param.equals(PARAM_TOTAL_ATTENDANCE)) {
                result = totalAttendance;
            }

            return result;
        }

    }

    /**
     * Returns course information associated with this section.
     *
     * @author Follett Software Company
     */
    protected class RetrieveCourse implements FieldRetriever {
        private static final String DOE_DUAL_CREDIT = "DOE DUAL CREDIT";

        private final String DOE_COURSE_FACILITY_NAME = "DOE COURSE FACILITY NAME";
        private final String DOE_COURSE_FACILITY_TYPE = "DOE COURSE FACILITY TYPE";
        private final String DOE_STATE_COURSE_ID = "DOE STATE COURSE ID";

        private final String PARAM_COURSE_END_DATE = "COURSE END DATE";
        private final String PARAM_COURSE_FACILITY_NAME = "COURSE FACILITY NAME";
        private final String PARAM_COURSE_FACILITY_TYPE = "COURSE FACILITY TYPE";
        private final String PARAM_COURSE_GRADE_LEVEL = "COURSE GRADE LEVEL";
        private final String PARAM_COURSE_SCHOOL_YEAR = "COURSE SCHOOL YEAR";
        private final String PARAM_COURSE_START_DATE = "COURSE START DATE";
        private final String PARAM_LOCAL_COURSE_ID = "LOCAL COURSE ID";
        private final String PARAM_LOCAL_COURSE_TITLE = "LOCAL COURSE TITLE";
        private final String PARAM_STATE_COURSE_CODE = "STATE COURSE CODE";

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
            PreschoolCrsAssignmentEntity scae = (PreschoolCrsAssignmentEntity) entity;
            StudentScheduleSpan info = scae.getScheduleInfo();
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
                    // Get the schedule change exit date if there is one.
                    StudentScheduleChange exitChange = info.getExitChange();
                    if (exitChange != null) {
                        result = exitChange.getDate();
                    }
                    // Otherwise if there is no schedule change exit date or this date
                    // is after the section exit date use the section exit date
                    if (result == null || info.getExitDate().before((PlainDate) result)) {
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
                        GradeData gradeData =
                                PreschoolCrsAssignmentEntity.getGradeData(data, info.getTranscript(), null);

                        if (!StringUtils.isEmpty(gradeData.m_letterGrade)) {
                            result = info.getExitDate();
                        }
                    }

                    /*
                     * The end date cannot be > "current date".
                     */
                    if (result != null && ((PlainDate) result).after(m_reportDate)) {
                        result = null;
                    }
                }
            }

            return result;
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
            PreschoolCrsAssignmentEntity scaEntity = (PreschoolCrsAssignmentEntity) entity;
            ILPreschoolStdCrsAssignment scaData = (ILPreschoolStdCrsAssignment) data;
            String rcdts = null;

            StudentScheduleSpan info = scaEntity.getScheduleInfo();
            SisSchool sklFoCrs = info.getSection().getSchoolCourse().getSchool();

            if (param.equals("H") && sklFoCrs != null) {
                if (scaEntity.getEffectiveEnrollment() != null &&
                        BooleanAsStringConverter.TRUE.equals(sklFoCrs.getFieldValueByBeanPath(m_fieldSklNonCalcFte))) {
                    rcdts = (String) scaEntity.getEffectiveEnrollment().getFieldValueByBeanPath(m_fieldEnrSchoolHome);
                }
                if (StringUtils.isEmpty(rcdts)) {
                    rcdts = (String) sklFoCrs.getFieldValueByBeanPath(scaData.m_fieldSchoolCode);
                }
            } else if (param.equals("S")) {
                if (scaEntity.getEffectiveEnrollment() != null &&
                        BooleanAsStringConverter.TRUE.equals(sklFoCrs.getFieldValueByBeanPath(m_fieldSklNonCalcFte))) {
                    rcdts = (String) scaEntity.getEffectiveEnrollment().getFieldValueByBeanPath(m_fieldEnrSchoolServ);
                    if (StringUtils.isEmpty(rcdts)) {
                        rcdts = (String) sklFoCrs.getFieldValueByBeanPath(scaData.m_fieldSchoolCode);
                    }
                } else {
                    StudentScheduleSpan currentSpan = scaEntity.getScheduleInfo();
                    if (currentSpan != null) {
                        if (currentSpan.getTranscript() != null &&
                                currentSpan.getTranscript().getSchool() != null &&
                                !StringUtils.isEmpty(currentSpan.getTranscript().getFinalGrade())) {
                            // if final grade exists for section/transcript, pull school ID from
                            // transcript
                            rcdts = (String) currentSpan.getTranscript().getSchool()
                                    .getFieldValueByBeanPath(scaData.m_fieldSchoolCode);
                        } else {
                            // if no final grade or transcript exists, pull school ID from the
                            // section/span
                            if (currentSpan.getSection() != null && currentSpan.getSection().getSchoolCourse() != null
                                    && currentSpan.getSection().getSchoolCourse().getSchool() != null) {
                                rcdts = (String) currentSpan.getSection().getSchoolCourse().getSchool()
                                        .getFieldValueByBeanPath(scaData.m_fieldSchoolCode);
                            }
                        }
                    }
                    if (rcdts == null && sklFoCrs != null) {
                        rcdts = (String) sklFoCrs.getFieldValueByBeanPath(scaData.m_fieldSchoolCode);
                    }
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
            PreschoolCrsAssignmentEntity scae = (PreschoolCrsAssignmentEntity) entity;
            StudentScheduleSpan info = scae.getScheduleInfo();
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
     * Returns transcript grade from the transcript record associated with this section or class.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTranscriptGrade implements FieldRetriever {
        private final static String PARAM_FINAL_LETTER_GRADE = "FINAL LETTER GRADE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            ILPreschoolStdCrsAssignment scData = (ILPreschoolStdCrsAssignment) data;
            PreschoolCrsAssignmentEntity scae = (PreschoolCrsAssignmentEntity) entity;
            StudentScheduleSpan info = scae.getScheduleInfo();
            Transcript transcript = info.getTranscript();
            String parameter = (String) field.getParameter();
            GradeData gradeData = PreschoolCrsAssignmentEntity.getGradeData(scData, transcript, parameter);
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
                if (StringUtils.isEmpty((String) returnObject)) {
                    String date = scae.getFieldValue("Course End Date");
                    try {
                        if (!StringUtils.isEmpty(date) &&
                                m_dateFormat.parse(date).before(getEndDate(info.getSection()))) {
                            returnObject = "31";
                        }
                    } catch (ParseException e) {
                        e.printStackTrace();
                    }
                }
                if (StringUtils.isEmpty((String) returnObject)) {
                    StudentSchedule ssc = info.getSchedule();
                    if (ssc != null) {
                        PlainDate sscDate = getEndDate(ssc.getSection());
                        if (sscDate != null && sscDate.before(m_reportDate)) {
                            returnObject = "31";
                        }
                    }
                }
            }
            return returnObject;
        }
    }

    /**
     * Aliases
     */
    protected static final String ALIAS_COURSE_OVERRIDE = "DOE COURSE OVERRIDE TRN";
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_ENR_SKL_HOME = "DOE SCHOOL HOME";
    protected static final String ALIAS_ENR_SKL_SERV = "DOE SCHOOL SERVICE";
    protected static final String ALIAS_EXCLUDE_COURSE = "DOE EXCLUDE CRS";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_EXCLUDE_SECTION = "DOE EXCLUDE MST";
    protected static final String ALIAS_EXCLUDE_STUDENT = "DOE EXCLUDE STD";
    protected static final String ALIAS_GRADE_CODE = "DOE GRADE CODE";
    protected static final String ALIAS_RCDTS_FOR_SERVING_SCHOOL = "DOE OUTPLACEMENT DI";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVICE";
    protected static final String ALIAS_SKL_NON_CALC_FTE = "DOE NON-CALCULATING FTE";
    protected static final String ALIAS_STATE_COURSE_ID = "DOE STATE COURSE ID";
    protected static final char CHAR_COMMA = ',';

    /**
     * Other constants
     */
    protected static final String ILLEGAL_CRS_VIEW_CHARACTERS = "[^-A-z0-9]";
    protected static final String ILLEGAL_NAME_CHARACTERS = "[^-A-z ]";
    protected static final String INPUT_PARAM_TERM = "term";
    protected static final String PARAM_EXCLUDE_NO_FINAL_GRADES = "excludeStudentWoFinalGrade";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String REF_CODE_SCHEDULE_TERMS = "rtbSchTermCode";
    protected static final String REGEX_DOT = "[\\.]";
    protected static final String STRING_HYPHEN = "-";

    /*
     * Supporting instance variables.
     */
    protected Map<String, Map<String, Integer>> m_absences = new HashMap<String, Map<String, Integer>>();
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected EnrollmentManager m_enrollmentManager;
    protected boolean m_excludeStudentsWoFinalGrades = false;
    protected String m_fieldDistrictCode;
    protected String m_fieldEnrSchoolHome;
    protected String m_fieldEnrSchoolServ;
    protected String m_fieldExcludeSkl;
    protected String m_fieldSchoolCode;
    protected String m_fieldSklNonCalcFte;
    protected String m_fieldStateCourseId;
    protected String m_fieldTrnCourseOverride;
    protected Map<String, ReferenceCode> m_gradeCodes = new HashMap<String, ReferenceCode>();
    protected Map<String, Collection<GradeScaleGradeDefinition>> m_gradeScaleDefinitions;
    protected Map<String, GradeScale> m_gradeScales = new HashMap<String, GradeScale>();
    protected StudentHistoryHelper m_helper;
    protected Pattern m_illegalCrsViewChars;
    protected Pattern m_illegalNameCharacters;
    protected int m_numOfRecords;
    protected PlainDate m_reportDate;
    protected Map<String, Map<String, Set<PlainDate>>> m_schoolsToCalendars;
    protected ArrayList<String> m_selectedTermCodes;
    protected Map<String, Collection<StudentSchool>> m_sskMapByStd;
    protected Map<String, String> m_termCodesMap = new HashMap();
    protected Map<String, Collection<ScheduleTermDate>> m_termDateMap;

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
            m_helper.getStudentCriteria().addIn(SisStudent.COL_GRADE_LEVEL, getValidGradeCodes());

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
             * Pre-load absences
             */
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(StudentPeriodAttendance.COL_STUDENT_OID,
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria()));
            criteria.addEqualTo(StudentPeriodAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);
            criteria.addGreaterOrEqualThan(StudentPeriodAttendance.COL_DATE,
                    getOrganization().getCurrentContext().getStartDate());
            BeanQuery query = new BeanQuery(StudentPeriodAttendance.class, criteria);
            query.addOrderBy(StudentPeriodAttendance.COL_STUDENT_OID, true);
            query.addOrderBy(StudentPeriodAttendance.COL_MASTER_SCHEDULE_OID, true);
            query.addOrderBy(StudentPeriodAttendance.COL_DATE, true);
            QueryIterator absenceIterator = getBroker().getIteratorByQuery(query);
            while (absenceIterator.hasNext()) {
                StudentPeriodAttendance attendance = (StudentPeriodAttendance) absenceIterator.next();
                String studentOid = attendance.getStudentOid();
                if (!m_absences.containsKey(studentOid)) {
                    HashMap<String, Integer> sectionMap = new HashMap<String, Integer>();
                    m_absences.put(studentOid, sectionMap);
                }
                Map<String, Integer> sectionMap = m_absences.get(studentOid);

                String masterScheduleOid = attendance.getMasterScheduleOid();
                int count =
                        sectionMap.containsKey(masterScheduleOid) ? sectionMap.get(masterScheduleOid).intValue() : 0;
                sectionMap.put(masterScheduleOid, Integer.valueOf(count + 1));
            }

            QueryByCriteria studentQuery = m_helper.getStudentQuery(false);
            setQuery(studentQuery);
            setEntityClass(PreschoolCrsAssignmentEntity.class);
            initializeStudentSchools();
            // Build maps of retriever functions
            Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("SCA-RCDTS", new RetrieveRcdts());
            calcs.put("SCA-COURSE", new RetrieveCourse());
            calcs.put("SCA-ATTENDANCE", new RetrieveAttendance());
            calcs.put("SCA-GRADE", new RetrieveTranscriptGrade());
            calcs.put("SCA-SECTION", new RetrieveSection());
            calcs.put("SCA-CLEAN", new RetrieveStripNameChar());
            super.addCalcs(calcs);
        }
    }

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
        heading.append("Preschool Student Course Assignment");
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
     * @param section MasterSchedule
     * @return Collection<ScheduleTermDate>
     */
    protected PlainDate getEndDate(MasterSchedule section) {
        Collection<ScheduleTermDate> dates = null;

        String scheduleTermOid = section.getScheduleTermOid();

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

        Collection<ScheduleTermDate> termDates = m_termDateMap.get(scheduleTermOid);

        PlainDate endDate = null;

        for (ScheduleTermDate currendTermDate : termDates) {
            if (endDate == null || endDate.before(currendTermDate.getEndDate())) {
                endDate = currendTermDate.getEndDate();
            }
        }
        // If a term is missing any dates, use school schedule dates or district calendar dates.
        if (endDate == null) {
            endDate = section.getSchedule().getEndDate();
            if (endDate == null) {
                endDate = getCurrentContext().getEndDate();
            }
        }

        return endDate;
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
     * Load grade codes.
     */
    private Collection getValidGradeCodes() {
        Collection<String> validCodes = new ArrayList<>();
        ModelProperty prop =
                new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL, getBroker().getPersistenceKey());
        DataDictionaryField field = getDataDictionary().findDataDictionaryField(prop.getFieldId());
        ReferenceTable referenceTable = field.getReferenceTable();
        Collection<ReferenceCode> refCodes = referenceTable.getReferenceCodes();
        for (ReferenceCode refCode : refCodes) {
            if ("14".equals(refCode.getStateCode())) {
                validCodes.add(refCode.getCode());
            }
        }
        if (validCodes.isEmpty()) {
            validCodes.add("___dummy___");
        }
        return validCodes;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldEnrSchoolHome = translateAliasToJavaName(ALIAS_ENR_SKL_HOME, true);
        m_fieldEnrSchoolServ = translateAliasToJavaName(ALIAS_ENR_SKL_SERV, true);
        m_fieldExcludeSkl = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldSklNonCalcFte = translateAliasToJavaName(ALIAS_SKL_NON_CALC_FTE, true);
        m_fieldStateCourseId = translateAliasToJavaName(ALIAS_STATE_COURSE_ID, true);
        m_fieldTrnCourseOverride = translateAliasToJavaName(ALIAS_COURSE_OVERRIDE, false);

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
     * Initialize map of secondary school keyed on std OID.
     *
     */
    private void initializeStudentSchools() {
        X2Criteria sskCriteria = new X2Criteria();
        sskCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        m_sskMapByStd = getBroker().getGroupedCollectionByQuery(new QueryByCriteria(StudentSchool.class, sskCriteria),
                StudentSchool.COL_STUDENT_OID, 1024);
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