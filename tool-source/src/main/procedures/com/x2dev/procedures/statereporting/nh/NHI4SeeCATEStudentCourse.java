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
package com.x2dev.procedures.statereporting.nh;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class NHI4SeeCATEStudentCourse.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class NHI4SeeCATEStudentCourse extends StateReportData {

    /**
     * The Class I4SeeEntity.
     */
    public static class I4SeeEntity extends StateReportEntity {

        private NHI4SeeCATEStudentCourse m_data;
        private List<StudentScheduleInfo> m_infos;
        String m_pgmError;

        /**
         * Gets the current info.
         *
         * @return Student schedule info
         */
        public StudentScheduleInfo getCurrentInfo() {
            return m_infos.size() > 0 ? m_infos.get(getCurrentRow()) : null;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "]";

            return name;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_data = (NHI4SeeCATEStudentCourse) data;
            String studentOid = bean.getOid();
            NHI4SeeCATEStudentCourse prcData = (NHI4SeeCATEStudentCourse) data;
            m_infos = new LinkedList();
            List<StudentProgramParticipation> programs = prcData.getStudentProgramParticipationByOid(studentOid);
            if (programs != null && !programs.isEmpty()) {
                List<StudentScheduleSpan> spans = prcData.getStudentScheduleSpans((SisStudent) bean);
                if (spans != null && !spans.isEmpty()) {
                    for (StudentScheduleSpan span : spans) {
                        boolean excludeByCrs = false;
                        boolean excludeByAttendances = false;
                        String termId = span.getSection().getScheduleTerm().getCode();
                        String termIdState = null;
                        if (!StringUtils.isEmpty(termId)) {
                            termIdState = m_data.lookupStateValue(ScheduleTerm.class, ScheduleTerm.COL_CODE, termId);
                        }
                        StudentScheduleInfo info = prcData.new StudentScheduleInfo((SisStudent) bean, programs, span);
                        PlainDate entryDate = info.getEntryDate();
                        PlainDate exitDate = info.getExitDate();
                        PlainDate programExitDate = info.getProgramExitDate();
                        PlainDate termStartDate = info.getStartOfTermDate();
                        excludeByAttendances =
                                prcData.m_paramExcludeWithZeros && info.getMeetingDatesAttending().size() < 1;
                        excludeByCrs = !prcData.m_paramAllCrs && ("1".equals(termIdState) || "2".equals(termIdState));
                        if ((entryDate != null && !entryDate.after(prcData.getCurrentContext().getEndDate())) &&
                                (exitDate == null || (entryDate != null && exitDate.after(entryDate))) &&
                                (programExitDate == null || termStartDate == null
                                        || programExitDate.after(termStartDate))
                                && !excludeByAttendances && !excludeByCrs) {
                            m_infos.add(info);
                        }
                    }
                }
            } else {
                m_pgmError = "is enrolled in CATE Course but is not a member of CATE Program.";
            }
            if (m_infos.size() > 1) {
                Collections.sort(m_infos);
                Iterator<StudentScheduleInfo> iterator = m_infos.iterator();
                StudentScheduleInfo prior = iterator.next();
                while (iterator.hasNext()) {
                    StudentScheduleInfo info = iterator.next();
                    if (prior.getReceivingSchoolNumber().equals(info.getReceivingSchoolNumber()) &&
                            prior.getCourseProgramId().equals(info.getCourseProgramId()) &&
                            prior.getTermCode().equals(info.getTermCode()) &&
                            prior.getCourseConcentrator().equals(info.getCourseConcentrator()) &&
                            prior.getExitDate() != null &&
                            prior.getExitDate().equals(info.getEntryDate()) &&
                            EXIT_CODE_CONTINUING_CTE.equals(prior.getExitCode()) &&
                            prior.getEntryCode() != null &&
                            info.getEntryCode() != null &&
                            prior.getEntryCode().equals(info.getEntryCode())) {
                        prior.addStudentScheduleSpans(info.getStudentScheduleSpans());
                        iterator.remove();
                    } else {
                        prior = info;
                    }
                }
            }
            setRowCount(StringUtils.isEmpty(m_pgmError) ? m_infos.size() : 1);
        }

        /**
         * Check enrollment membership count and membership days parameter to determine if the
         * student should be reported.
         *
         * @return StateReportValidationError
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;
            FieldDefinition field = m_data.getFieldDefinition(EXPORT_FIELD_PGM_ID);
            String stdSasid = ((SisStudent) this.getBean()).getStateId();
            if (!StringUtils.isEmpty(m_pgmError)) {
                error = new StateReportValidationError(this, field, "Program existing error",
                        "Student with SASID = [" + stdSasid
                                + "] " + m_pgmError);
            }
            return error;
        }
    }

    /**
     * The Class I4SeeStudentHistoryHelper.
     */
    public class I4SeeStudentHistoryHelper extends StudentHistoryHelper {
        private static final String ALIAS_I4SEE_230_250_DATE = "i4see 230/250 CATE";
        private String m_fieldI4SeeChangeDate;

        /**
         * Instantiates a new i 4 see student history helper.
         *
         * @param data StateReportData
         */
        public I4SeeStudentHistoryHelper(StateReportData data) {
            super(data);
            m_fieldI4SeeChangeDate = translateAliasToJavaName(ALIAS_I4SEE_230_250_DATE, true);
        }

        /**
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#getScheduleChangeDate(com.x2dev.sis.model.beans.StudentScheduleChange)
         */
        @Override
        protected PlainDate getScheduleChangeDate(StudentScheduleChange change) {
            PlainDate effectiveDate = (PlainDate) change.getFieldValueByBeanPath(m_fieldI4SeeChangeDate);
            if (effectiveDate == null) {
                effectiveDate = super.getScheduleChangeDate(change);
            }
            return effectiveDate;
        }
    }

    /**
     * The Class Retrieve1430TermId.
     */
    protected class Retrieve1430TermId implements FieldRetriever {
        protected static final String CALC_ID = "I4SEE1430";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StudentScheduleInfo info = ((I4SeeEntity) entity).getCurrentInfo();

            return info != null ? info.getTermCode() : null;
        }

    }

    /**
     * The Class Retrieve1470LocalClassCode.
     */
    protected class Retrieve1470LocalClassCode implements FieldRetriever {
        protected static final String CALC_ID = "I4SEE1470";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StudentScheduleInfo info = ((I4SeeEntity) entity).getCurrentInfo();
            String classCode = null;
            if (info != null) {
                classCode = info.getLocalClassCode();
                if (classCode.length() > 15) {
                    classCode = classCode.substring(0, 16);
                }
            }
            return classCode;
        }
    }

    /**
     * The Class Retrieve1620SchoolNumber.
     */
    protected class Retrieve1620SchoolNumber implements FieldRetriever {
        protected static final String CALC_ID = "I4SEE1620";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StudentScheduleInfo info = ((I4SeeEntity) entity).getCurrentInfo();

            return info != null ? info.getReceivingSchoolNumber() : "-";
        }

    }

    /**
     * The Class Retrieve1740_1750Course.
     */
    protected class Retrieve1740_1750Course implements FieldRetriever {
        protected static final String CALC_ID = "I4SEERC";

        private static final String CALC_PARAM_1740 = "i4see 1740";
        private static final String CALC_PARAM_1750 = "i4see 1750";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            StudentScheduleInfo info = ((I4SeeEntity) entity).getCurrentInfo();
            if (info != null) {
                String param = (String) field.getParameter();
                if (CALC_PARAM_1740.equals(param)) {
                    value = info.getCourseProgramId();
                } else if (CALC_PARAM_1750.equals(param)) {
                    if ("".equals(info.getCourseConcentrator())) {
                        value = "0";
                    } else {
                        value = info.getCourseConcentrator();
                    }
                }
            }
            return value;
        }
    }

    /**
     * The Class Retrieve1760NumMeetingsInAttendance.
     */
    protected class Retrieve1760NumMeetingsInAttendance implements FieldRetriever {
        protected static final String CALC_ID = "I4SEE1760";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StudentScheduleInfo info = ((I4SeeEntity) entity).getCurrentInfo();
            if (info != null) {
                Set<PlainDate> meetingDays = info.getMeetingDatesAttending();
                return String.valueOf(meetingDays.size());
            }
            return null;
        }
    }

    /**
     * The Class Retrieve230_250_EntryExitDate.
     */
    protected class Retrieve230_250_EntryExitDate implements FieldRetriever {
        protected static final String CALC_ID = "I4SEEEDATE";

        private static final String CALC_PARAM_ENTRY_DATE = "Entry Date";
        private static final String CALC_PARAM_EXIT_DATE = "Exit Date";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String fieldId = field.getFieldId();
            StudentScheduleInfo info = ((I4SeeEntity) entity).getCurrentInfo();
            if (info != null) {
                if (CALC_PARAM_ENTRY_DATE.equals(fieldId)) {
                    value = info.getEntryDate();
                } else if (CALC_PARAM_EXIT_DATE.equals(fieldId)) {
                    value = info.getExitDate();
                }
            }
            return value;
        }
    }

    /**
     * The Class Retrieve240_260_EntryExitCode.
     */
    protected class Retrieve240_260_EntryExitCode implements FieldRetriever {
        protected static final String CALC_ID = "I4SEEECODE";

        private static final String CALC_PARAM_ENTRY_CODE = "Entry Code";
        private static final String CALC_PARAM_EXIT_CODE = "Exit Code";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            String fieldId = field.getFieldId();
            StudentScheduleInfo info = ((I4SeeEntity) entity).getCurrentInfo();
            if (info != null) {
                if (CALC_PARAM_ENTRY_CODE.equals(fieldId)) {
                    value = info.getEntryCode();
                } else if (CALC_PARAM_EXIT_CODE.equals(fieldId)) {
                    value = info.getExitCode();
                }
            }
            return value;
        }
    }

    /**
     * The Class RetrieveLocalClassCode.
     */
    protected class RetrieveLocalClassCode implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StudentScheduleInfo info = ((I4SeeEntity) entity).getCurrentInfo();
            return info != null ? info.getLocalClassCode() : null;
        }
    }

    /**
     * The Class StudentScheduleInfo.
     */
    protected class StudentScheduleInfo implements Comparable<StudentScheduleInfo> {
        private static final String CODE_TERM_ID_FIRST_SEMESTER = "1";
        private static final String CODE_TERM_ID_FULL_YEAR = "30";
        private static final String CODE_TERM_ID_INVALID = "XX";
        private static final String CODE_TERM_ID_SECOND_SEMESTER = "2";

        private boolean m_calcEntryDate = true;
        private boolean m_calcExitDate = true;
        private String m_courseConcentrator;
        private String m_courseProgramId;
        private String m_entryCode;
        private PlainDate m_entryDate;
        private String m_exitCode;
        private PlainDate m_exitDate;
        private String m_localClassCode;
        private Set<PlainDate> m_meetingDates;
        private List<StudentProgramParticipation> m_programs;
        private String m_schoolNumber;
        private List<StudentScheduleSpan> m_spans;
        private SisStudent m_student;
        private String m_termCode;

        /**
         * Instantiates a new student schedule info.
         *
         * @param student SisStudent
         * @param programs List<StudentProgramParticipation>
         * @param span StudentScheduleSpan
         */
        public StudentScheduleInfo(SisStudent student, List<StudentProgramParticipation> programs,
                StudentScheduleSpan span) {
            m_student = student;
            m_spans = new LinkedList();
            m_spans.add(span);
            m_programs = programs;
        }

        /**
         * Compare to.
         *
         * @param o StudentScheduleInfo
         * @return int
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(StudentScheduleInfo o) {
            int result = getReceivingSchoolNumber().compareTo(o.getReceivingSchoolNumber());
            if (result == 0) {
                result = getCourseProgramId().compareTo(o.getCourseProgramId());
            }
            if (result == 0) {
                result = getTermCode().compareTo(o.getTermCode());
            }
            if (result == 0) {
                result = getCourseConcentrator().compareTo(o.getCourseConcentrator());
            }
            if (result == 0) {
                result = getEntryDate().compareTo(o.getEntryDate());
            }
            return result;
        }

        /**
         * Adds the student schedule spans.
         *
         * @param spans List<StudentScheduleSpan>
         */
        public void addStudentScheduleSpans(List<StudentScheduleSpan> spans) {
            m_spans.addAll(spans);
            m_calcEntryDate = true;
            m_calcExitDate = true;
            m_courseConcentrator = null;
            m_courseProgramId = null;
            m_entryCode = null;
            m_exitCode = null;
            m_localClassCode = null;
            m_meetingDates = null;
            m_schoolNumber = null;
            m_termCode = null;
        }

        /**
         * Gets the course.
         *
         * @return Course
         */
        public Course getCourse() {
            SchoolCourse csk = getSchoolCourse();
            return csk == null ? null : csk.getCourse();
        }

        /**
         * Gets the course concentrator.
         *
         * @return String
         */
        public String getCourseConcentrator() {
            if (m_courseConcentrator == null) {
                Course course = getCourse();
                if (course != null) {
                    m_courseConcentrator = (String) course.getFieldValueByBeanPath(m_fieldCourseConcentrator);
                }
                if (m_courseConcentrator == null) {
                    m_courseConcentrator = "";
                }
            }
            return m_courseConcentrator;
        }

        /**
         * Gets the course program id.
         *
         * @return String
         */
        public String getCourseProgramId() {
            if (m_courseProgramId == null) {
                Course course = getCourse();
                if (course != null) {
                    m_courseProgramId = (String) course.getFieldValueByBeanPath(m_fieldCourseProgramId);
                }
                if (m_courseProgramId == null) {
                    m_courseProgramId = "";
                }
            }
            return m_courseProgramId;
        }

        /**
         * Gets the entry code.
         *
         * @return String
         */
        public String getEntryCode() {
            if (m_entryCode == null) {
                StudentScheduleChange change = getStudentScheduleSpan().getEntryChange();
                if (change != null) {
                    String code = (String) change.getFieldValueByBeanPath(m_fieldProgramEntryCodeAlt);
                    m_entryCode = lookupStateValue(StudentScheduleChange.class, m_fieldProgramEntryCodeAlt, code);
                } else {
                    StudentProgramParticipation program = getMatchingProgram(false);
                    if (program != null) {
                        m_entryCode = (String) program.getFieldValueByBeanPath(m_fieldProgramEntryCode);
                    }
                }
                if (StringUtils.isEmpty(m_entryCode)) {
                    m_entryCode =
                            (String) getMatchingProgram(true).getFieldValueByBeanPath(m_fieldProgramEntryCode);
                }
            }
            return m_entryCode;
        }

        /**
         * Gets the entry date.
         *
         * @return Plain date
         */
        public PlainDate getEntryDate() {
            if (m_calcEntryDate) {
                StudentScheduleSpan span = getStudentScheduleSpan();
                StudentScheduleChange change = span.getEntryChange();
                if (change == null) {
                    StudentProgramParticipation program = getMatchingProgram(true);
                    if (program != null) {
                        PlainDate programDate = (PlainDate) program.getFieldValueByBeanPath(m_fieldProgramEntryDate);
                        if (programDate != null && programDate.after(span.getEntryDate())) {
                            m_entryDate = programDate;
                        }
                    }
                }
                if (m_entryDate == null) {
                    m_entryDate = span.getEntryDate();
                }
                m_calcEntryDate = false;
            }
            return m_entryDate;
        }

        /**
         * Gets the exit code.
         *
         * @return String
         */
        public String getExitCode() {
            if (m_exitCode == null) {
                StudentScheduleChange change = getLastStudentScheduleSpan().getExitChange();
                if (change != null) {
                    String code = (String) change.getFieldValueByBeanPath(m_fieldProgramExitCodeAlt);
                    m_exitCode = lookupStateValue(StudentScheduleChange.class, m_fieldProgramExitCodeAlt, code);
                    if (StringUtils.isEmpty(m_exitCode)) {
                        m_exitCode = (String) getMatchingProgram(true).getFieldValueByBeanPath(m_fieldProgramExitCode);
                    }
                } else {
                    StudentProgramParticipation program = getMatchingProgram(false);
                    if (program != null) {
                        m_exitCode = (String) program.getFieldValueByBeanPath(m_fieldProgramExitCode);
                    }
                }
                if (StringUtils.isEmpty(m_exitCode)) {
                    PlainDate exitDate = getExitDate();
                    if (exitDate != null) {
                        PlainDate endOfTermDate = getEndOfTermDate();
                        if (endOfTermDate != null && endOfTermDate.equals(exitDate)) {
                            m_exitCode = EXIT_CODE_CONTINUING_CTE;
                        }
                    }
                }
            }
            return m_exitCode;
        }

        /**
         * Gets the exit date.
         *
         * @return Plain date
         */
        public PlainDate getExitDate() {
            if (m_calcExitDate) {
                StudentScheduleSpan span = getLastStudentScheduleSpan();
                StudentScheduleChange change = span.getExitChange();
                if (change == null) {
                    StudentProgramParticipation program = getMatchingProgram(false);
                    if (program != null) {
                        m_exitDate = (PlainDate) program.getFieldValueByBeanPath(m_fieldProgramExitDate);
                    }
                } else {
                    m_exitDate = change.getEffectiveDate();
                }
                if (m_exitDate == null) {
                    m_exitDate = span.getExitDate();
                }
                PlainDate endOfTermDate = getEndOfTermDate();
                if (endOfTermDate != null && (m_exitDate == null || m_exitDate.after(endOfTermDate))) {
                    m_exitDate = endOfTermDate;
                }
                if (m_exitDate != null && m_exitDate.after(m_paramReportDate)) {
                    m_exitDate = null;
                }
                m_calcExitDate = false;
            }
            return m_exitDate;
        }

        /**
         * Gets the local class code.
         *
         * @return String
         */
        public String getLocalClassCode() {
            if (m_localClassCode == null) {
                MasterSchedule section = getSection();
                if (section != null) {
                    m_localClassCode = (String) section.getFieldValueByBeanPath(m_fieldLocalClassCode);
                } else {
                    m_localClassCode = "";
                }
            }
            return m_localClassCode;
        }

        /**
         * Gets the meeting dates attending.
         *
         * @return Sets the
         */
        public Set<PlainDate> getMeetingDatesAttending() {
            if (m_meetingDates == null) {
                m_meetingDates = new HashSet();
                PlainDate startDateStr = getEntryDate();
                PlainDate endDateStr = getExitDate();
                PlainDate startDate = getStartOfTermDate();
                PlainDate endOfTermDate = getEndOfTermDate();
                PlainDate endDate = m_paramReportDate;

                if (startDateStr != null) {
                    if (startDate.before(startDateStr)) {
                        startDate = startDateStr;
                    }
                }

                if (endDateStr != null) {
                    if (endDate.after(endDateStr)) {
                        endDate = endDateStr;
                    }
                    if (endOfTermDate.before(endDate)) {
                        endDate = endOfTermDate;
                    }
                }

                // make sure the end date is not past the end of the scheduled class
                if (endDate.after(endOfTermDate)) {
                    endDate = endOfTermDate;
                }
                if (endDate.after(startDate)) {

                    Set<PlainDate> tempDates =
                            getCalendarDays(getSchool(), getStudent().getCalendarCode(), startDate, endDate);
                    // Trim to enrollment spans
                    List<StudentEnrollmentSpan> spans = m_helper.getStudentEnrollmentSpans(getStudent(), true);
                    if (!spans.isEmpty()) {
                        for (PlainDate date : tempDates) {
                            for (StudentEnrollmentSpan span : spans) {
                                if (span.getFirstActiveDate() != null && !date.before(span.getFirstActiveDate())) {
                                    if (span.getLastActiveDate() == null || !date.after(span.getLastActiveDate())) {
                                        m_meetingDates.add(date);
                                        break;
                                    }
                                }
                            }
                        }
                    }

                    /*
                     * accumulate the number of off track dates that match the schedule
                     */
                    Set<PlainDate> offTrackDays = new HashSet();
                    if (getOffTrackAttendance(getStudent().getOid()) != null) {
                        for (StudentAttendance studentAttendance : getOffTrackAttendance(getStudent().getOid())) {
                            if (m_meetingDates.contains(studentAttendance.getDate())) {
                                offTrackDays.add(studentAttendance.getDate());
                            }
                        }
                    }

                    /*
                     * accumulate the number of absence dates that match the schedule
                     */
                    Set<PlainDate> absences = new HashSet();
                    if (getStudentAbsences(getStudent().getOid()) != null) {
                        for (StudentAttendance studentAttendance : getStudentAbsences(getStudent().getOid())) {
                            if (m_meetingDates.contains(studentAttendance.getDate())) {
                                absences.add(studentAttendance.getDate());
                            }
                        }
                    }

                    m_meetingDates.removeAll(offTrackDays);
                    m_meetingDates.removeAll(absences);
                }
            }
            return m_meetingDates;
        }

        /**
         * Gets the program exit date.
         *
         * @return Plain date
         */
        public PlainDate getProgramExitDate() {
            StudentProgramParticipation studentProgram = getMatchingProgram(false);
            return studentProgram == null ? null
                    : (PlainDate) studentProgram.getFieldValueByBeanPath(m_fieldProgramExitDate);
        }

        /**
         * Gets the receiving school number.
         *
         * @return String
         */
        public String getReceivingSchoolNumber() {
            if (m_schoolNumber == null) {
                if (StringUtils.isEmpty(m_paramReceivingSchoolNumber)) {
                    SisSchool school = getSchool();
                    if (school != null) {
                        m_schoolNumber = (String) school.getFieldValueByBeanPath(m_fieldSchoolNumber);
                    }
                } else {
                    m_schoolNumber = m_paramReceivingSchoolNumber;
                }
                if (m_schoolNumber == null) {
                    m_schoolNumber = "";
                }
            }
            return m_schoolNumber;
        }

        /**
         * Gets the start of term date.
         *
         * @return Plain date
         */
        public PlainDate getStartOfTermDate() {
            PlainDate startDate = null;
            ScheduleTerm term = getScheduleTerm();
            if (term != null) {
                for (ScheduleTermDate termDate : term.getScheduleTermDates()) {
                    if (startDate == null || startDate.after(termDate.getStartDate())) {
                        startDate = termDate.getStartDate();
                    }
                }
            }
            return startDate;
        }

        /**
         * Gets the student schedule spans.
         *
         * @return List
         */
        public List<StudentScheduleSpan> getStudentScheduleSpans() {
            return m_spans;
        }

        /**
         * Gets the term code.
         *
         * @return String
         */
        public String getTermCode() {
            if (m_termCode == null) {
                ScheduleTerm term = getScheduleTerm();
                String termMap = term == null ? null : term.getBaseTermMap();
                if (!StringUtils.isEmpty(termMap)) {
                    if (!termMap.contains("0")) {
                        m_termCode = CODE_TERM_ID_FULL_YEAR; // full year
                    } else if (termMap.startsWith("1")) {
                        m_termCode = CODE_TERM_ID_FIRST_SEMESTER;
                    } else if (termMap.endsWith("1")) {
                        m_termCode = CODE_TERM_ID_SECOND_SEMESTER;
                    }
                }
                if (StringUtils.isEmpty(m_termCode)) {
                    m_termCode = CODE_TERM_ID_INVALID;
                }
            }
            return m_termCode;
        }

        /**
         * Gets the end of term date.
         *
         * @return Plain date
         */
        private PlainDate getEndOfTermDate() {
            PlainDate endDate = null;
            ScheduleTerm term = getScheduleTerm();
            if (term != null) {
                for (ScheduleTermDate termDate : term.getScheduleTermDates()) {
                    if (endDate == null || endDate.before(termDate.getEndDate())) {
                        endDate = termDate.getEndDate();
                    }
                }
            }
            return endDate;
        }

        /**
         * Gets the last student schedule span.
         *
         * @return Student schedule span
         */
        private StudentScheduleSpan getLastStudentScheduleSpan() {
            return m_spans.get(m_spans.size() - 1);
        }

        /**
         * Gets the matching program.
         *
         * @param matchAny boolean
         * @return Student program participation
         */
        private StudentProgramParticipation getMatchingProgram(boolean matchAny) {
            Course course = getCourse();
            String courseProgramId = (String) course.getFieldValueByBeanPath(m_fieldCourseProgramId);
            StudentProgramParticipation matchingProgram = null;
            if (!StringUtils.isEmpty(courseProgramId)) {
                for (StudentProgramParticipation program : m_programs) {
                    String programId = null;
                    if (m_fieldPgmProgramId != null) {
                        programId = (String) program.getFieldValueByBeanPath(m_fieldPgmProgramId);
                    }
                    if (StringUtils.isEmpty(programId) && m_fieldPgmCipCode != null) {
                        programId = (String) program.getFieldValueByBeanPath(m_fieldPgmCipCode);
                    }
                    if (!StringUtils.isEmpty(programId) && courseProgramId.equals(programId)) {
                        matchingProgram = program;
                        break;
                    }
                }
            }
            if (matchingProgram == null && matchAny) {
                matchingProgram = m_programs.get(0);
            }
            return matchingProgram;
        }

        /**
         * Gets the schedule term.
         *
         * @return Schedule term
         */
        private ScheduleTerm getScheduleTerm() {
            MasterSchedule section = getSection();
            return section == null ? null : section.getScheduleTerm();
        }

        /**
         * Gets the school.
         *
         * @return Sis school
         */
        private SisSchool getSchool() {
            SchoolCourse course = getSchoolCourse();
            return course == null ? null : course.getSchool();
        }

        /**
         * Gets the school course.
         *
         * @return School course
         */
        private SchoolCourse getSchoolCourse() {
            MasterSchedule section = getSection();
            return section == null ? null : section.getSchoolCourse();
        }

        /**
         * Gets the section.
         *
         * @return Master schedule
         */
        private MasterSchedule getSection() {
            StudentScheduleSpan span = getStudentScheduleSpan();

            return span == null ? null : span.getSection();
        }

        /**
         * Gets the student.
         *
         * @return Sis student
         */
        private SisStudent getStudent() {
            return m_student;
        }

        /**
         * Gets the student schedule span.
         *
         * @return Student schedule span
         */
        private StudentScheduleSpan getStudentScheduleSpan() {
            return m_spans.get(0);
        }

    }

    /**
     * The Class Validate1700CATEEnrollmentStatus.
     */
    protected class Validate1700CATEEnrollmentStatus implements FieldValidator {
        protected static final String VAL_ID = "I4SEE1700";

        private final Collection VALID_1700_ENROLLMENT_STATUS_VALUES = Arrays.asList("1", "4", "9", "18", "22");

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
            StateReportValidationError filterError = entity.filterEntity();
            if (filterError != null) {
                errors.add(filterError);
            }
            if (errors.isEmpty() && !VALID_1700_ENROLLMENT_STATUS_VALUES.contains(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "I4SEE 1700 Enrollment Status " + value + " not a valid Enrollment Status",
                        "I4SEE 1700 = " + value));
            }
            return errors;
        }
    }

    /**
     * The Class Validate240EntryCode.
     */
    protected class Validate240EntryCode implements FieldValidator {
        protected static final String EXPORT_FIELD_ENTRY_DATE = "Entry Date";
        protected static final String VAL_ID = "I4SEE240";

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

            String entryDate = entity.getFieldValue(EXPORT_FIELD_ENTRY_DATE);

            if ((!StringUtils.isEmpty(entryDate) && StringUtils.isEmpty(value)) ||
                    (StringUtils.isEmpty(entryDate) && !StringUtils.isEmpty(value))) {
                errors.add(new StateReportValidationError(entity, field,
                        "I4SEE 240 Entry Code and I4See 230 Entry Date must either both be empty, or both be filled in",
                        "I4SEE 230 = " + entryDate + ", I4SEE 240 = " + value));
            }

            return errors;
        }
    }

    protected static final String CODE_I4SEE_CATE_PROGRAM_IDENTIFIER = "CATE";
    protected static final String EXIT_CODE_CONTINUING_CTE = "W31";
    protected static final String EXPORT_FIELD_CLASS_CODE = "Local Class Code";
    protected static final String EXPORT_FIELD_SASID = "SASID";
    protected static final String EXPORT_FIELD_PGM_ID = "Program ID";
    protected static final String OFF_TRACK_CODE = "OFTR";

    private static final String ALIAS_I4SEE_050_SCHOOL_NUMBER = "i4see 050";
    private static final String ALIAS_I4SEE_230_CATE_ENTRY_DATE = "i4see 230 CATE";
    private static final String ALIAS_I4SEE_240_CATE_ENTRY_CODE = "i4see 240 CATE";
    private static final String ALIAS_I4SEE_240_CATE_ENTRY_CODE_ALT = "i4see 240 CATE alt";
    private static final String ALIAS_I4SEE_250_CATE_EXIT_DATE = "i4see 250 CATE";
    private static final String ALIAS_I4SEE_260_CATE_EXIT_CODE = "i4see 260 CATE";
    private static final String ALIAS_I4SEE_260_CATE_EXIT_CODE_ALT = "i4see 260 CATE alt";
    private static final String ALIAS_I4SEE_1470 = "i4see 1470";
    private static final String ALIAS_I4SEE_1740_COURSE_PROGRAM_ID = "i4see 1740";
    private static final String ALIAS_I4SEE_1750_CONCENTRATOR = "i4see 1750";
    private static final String ALIAS_I4SEE_CATE_CONTEXT = "i4see CATE CONTEXT";
    private static final String ALIAS_PGM_I4SEE_1710_PROGRAM_ID = "i4see 1710";
    private static final String ALIAS_PGM_CIP_CODE = "all-pgm-CIPCode";

    private static final String PARAM_FILTER_WITH_ZEROS = "filterWithZeros";
    private static final String PARAM_INCLUDE_ALL_CRS = "includeAllCrs";
    private static final String PARAM_INCLUDE_CLASS_CODE = "includeClassCode";
    private static final String PARAM_INCLUDE_STUDENT_NAMES = "includeStudentName";
    private static final String PARAM_RECEIVING_SCHOOL_NUMBER = "receivingSchoolNumber";
    private static final String PARAM_REPORT_DATE = "reportDate";

    // Alias bean paths
    protected String m_fieldCourseConcentrator;
    protected String m_fieldCourseProgramId;
    protected String m_fieldLocalClassCode;
    protected String m_fieldOrgCateContext;
    protected String m_fieldPgmCipCode;
    protected String m_fieldPgmProgramId;
    protected String m_fieldProgramEntryCode;
    protected String m_fieldProgramEntryCodeAlt;
    protected String m_fieldProgramEntryDate;
    protected String m_fieldProgramExitCode;
    protected String m_fieldProgramExitCodeAlt;
    protected String m_fieldProgramExitDate;
    protected String m_fieldSchoolNumber;

    protected I4SeeStudentHistoryHelper m_helper;

    // Parameter values
    protected boolean m_paramAllCrs;
    protected boolean m_paramExcludeWithZeros;
    protected String m_paramReceivingSchoolNumber;
    protected PlainDate m_paramReportDate;

    private Map<String, Collection<StudentAttendance>> m_absences;
    private EnrollmentManager m_enrollmentManager;
    private Map<String, Collection<StudentAttendance>> m_offTrack;
    private Map<String, Map<String, Set<PlainDate>>> m_schoolsToCalendars = new HashMap();
    private Map<String, LinkedList<StudentProgramParticipation>> m_studentProgramParticipation;

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        /*
         * Load initialization data
         */
        initializeFields();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * get all of the student program participation records for CATE
             */
            loadCATEStudentProgramParticipation();

            m_helper = new I4SeeStudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_paramReportDate);
            m_helper.getStudentScheduleChangeCriteria()
                    .addNotEmpty(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER + Section.REL_SCHOOL_COURSE
                            + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER + m_fieldCourseProgramId,
                            getBroker().getPersistenceKey());
            m_helper.getStudentScheduleCriteria().addNotEmpty(
                    StudentSchedule.REL_SECTION + PATH_DELIMITER + Section.REL_SCHOOL_COURSE + PATH_DELIMITER
                            + SchoolCourse.REL_COURSE + PATH_DELIMITER + m_fieldCourseProgramId,
                    getBroker().getPersistenceKey());

            m_helper.materializeStudentCriteria();

            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(I4SeeEntity.class);

            loadAbsenceDaysMaps();

            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(Retrieve1430TermId.CALC_ID, new Retrieve1430TermId());
            calcs.put(Retrieve1470LocalClassCode.CALC_ID, new Retrieve1470LocalClassCode());
            calcs.put(Retrieve1620SchoolNumber.CALC_ID, new Retrieve1620SchoolNumber());
            calcs.put(Retrieve1760NumMeetingsInAttendance.CALC_ID, new Retrieve1760NumMeetingsInAttendance());
            calcs.put(Retrieve230_250_EntryExitDate.CALC_ID, new Retrieve230_250_EntryExitDate());
            calcs.put(Retrieve240_260_EntryExitCode.CALC_ID, new Retrieve240_260_EntryExitCode());
            calcs.put(Retrieve1740_1750Course.CALC_ID, new Retrieve1740_1750Course());
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldRetriever>();
            validators.put(Validate1700CATEEnrollmentStatus.VAL_ID, new Validate1700CATEEnrollmentStatus());
            validators.put(Validate240EntryCode.VAL_ID, new Validate240EntryCode());
            super.addValidators(validators);
        }
    }

    /**
     * Gets the calendar days.
     *
     * @param school SisSchool
     * @param calendar String
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return Sets the
     */
    protected Set<PlainDate> getCalendarDays(SisSchool school,
                                             String calendar,
                                             PlainDate startDate,
                                             PlainDate endDate) {
        if (startDate == null) {
            startDate = school.getActiveSchedule().getStartDate();
        }
        if (endDate == null) {
            endDate = m_paramReportDate;
        }

        String key = school.getOid() + startDate.toString() + endDate.toString();
        if (!m_schoolsToCalendars.containsKey(key)) {
            Map<String, Set<PlainDate>> calendarData =
                    getEnrollmentManager().getCalendarLookup(school, startDate, endDate);
            m_schoolsToCalendars.put(key, calendarData);
        }

        Map<String, Set<PlainDate>> calendarDatesMap = m_schoolsToCalendars.get(key);
        Set<PlainDate> dates = calendarDatesMap.get(calendar);
        if (dates == null && !calendarDatesMap.isEmpty()) {
            dates = calendarDatesMap.values().iterator().next();
        }

        return dates;
    }

    /**
     * Gets the off track attendance.
     *
     * @param studentOid String
     * @return Collection
     */
    protected Collection<StudentAttendance> getOffTrackAttendance(String studentOid) {
        return m_offTrack.get(studentOid);
    }

    /**
     * Gets the student absences.
     *
     * @param studentOid String
     * @return Collection
     */
    protected Collection<StudentAttendance> getStudentAbsences(String studentOid) {
        return m_absences.get(studentOid);
    }

    /**
     * Gets the student program participation by oid.
     *
     * @param studentOid String
     * @return List
     */
    protected List<StudentProgramParticipation> getStudentProgramParticipationByOid(String studentOid) {
        return m_studentProgramParticipation.get(studentOid);
    }

    /**
     * Gets the student schedule spans.
     *
     * @param student SisStudent
     * @return List
     */
    protected List<StudentScheduleSpan> getStudentScheduleSpans(SisStudent student) {
        return m_helper.getStudentScheduleSpans(student);
    }

    /**
     * Gets the enrollment manager.
     *
     * @return Enrollment manager
     */
    private EnrollmentManager getEnrollmentManager() {
        if (m_enrollmentManager == null) {
            m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        }
        return m_enrollmentManager;
    }

    /**
     * Initialize fields.
     */
    private void initializeFields() {
        // Load Parameters
        if (getParameter(PARAM_INCLUDE_CLASS_CODE) != null
                && ((Boolean) getParameter(PARAM_INCLUDE_CLASS_CODE)).booleanValue()) {
            getFieldDefinitions().add(0,
                    new FieldDefinition("LocalClassCode",
                            null, null, false, 1, 32, null, null,
                            new RetrieveLocalClassCode(), null, null));
        }

        if (getParameter(PARAM_INCLUDE_STUDENT_NAMES) != null
                && ((Boolean) getParameter(PARAM_INCLUDE_STUDENT_NAMES)).booleanValue()) {
            getFieldDefinitions().add(0,
                    new FieldDefinition("Student Name",
                            SisStudent.COL_NAME_VIEW,
                            null, false, 1, 32, null, null, null, null, null));
        }

        m_paramExcludeWithZeros = false;
        if (getParameter(PARAM_FILTER_WITH_ZEROS) != null) {
            m_paramExcludeWithZeros = ((Boolean) getParameter(PARAM_FILTER_WITH_ZEROS)).booleanValue();
        }
        m_paramAllCrs = true;
        if (getParameter(PARAM_INCLUDE_ALL_CRS) != null) {
            m_paramAllCrs = ((Boolean) getParameter(PARAM_INCLUDE_ALL_CRS)).booleanValue();
        }
        m_paramReceivingSchoolNumber = (String) getParameter(PARAM_RECEIVING_SCHOOL_NUMBER);

        m_paramReportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);

        // Load Alias database field Names
        m_fieldCourseConcentrator = translateAliasToJavaName(ALIAS_I4SEE_1750_CONCENTRATOR, true);
        m_fieldCourseProgramId = translateAliasToJavaName(ALIAS_I4SEE_1740_COURSE_PROGRAM_ID, true);
        m_fieldLocalClassCode = translateAliasToJavaName(ALIAS_I4SEE_1470, true);
        m_fieldProgramEntryCode = translateAliasToJavaName(ALIAS_I4SEE_240_CATE_ENTRY_CODE, true);
        m_fieldProgramEntryCodeAlt = translateAliasToJavaName(ALIAS_I4SEE_240_CATE_ENTRY_CODE_ALT, true);
        m_fieldProgramEntryDate = translateAliasToJavaName(ALIAS_I4SEE_230_CATE_ENTRY_DATE, true);
        m_fieldProgramExitCode = translateAliasToJavaName(ALIAS_I4SEE_260_CATE_EXIT_CODE, true);
        m_fieldProgramExitCodeAlt = translateAliasToJavaName(ALIAS_I4SEE_260_CATE_EXIT_CODE_ALT, true);
        m_fieldProgramExitDate = translateAliasToJavaName(ALIAS_I4SEE_250_CATE_EXIT_DATE, true);
        m_fieldOrgCateContext = translateAliasToJavaName(ALIAS_I4SEE_CATE_CONTEXT, true);
        m_fieldSchoolNumber = translateAliasToJavaName(ALIAS_I4SEE_050_SCHOOL_NUMBER, true);
        m_fieldPgmProgramId = translateAliasToJavaName(ALIAS_PGM_I4SEE_1710_PROGRAM_ID, false);
        m_fieldPgmCipCode = translateAliasToJavaName(ALIAS_PGM_CIP_CODE, false);
    }

    /**
     * Load absence days maps.
     */
    private void loadAbsenceDaysMaps() {
        /*
         * Part I. Absence days from attendance.
         */
        // Main report query - students and Absence.
        X2Criteria criteria = new X2Criteria();
        criteria.addGreaterOrEqualThanField(StudentAttendance.COL_DATE,
                StudentAttendance.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.REL_ACTIVE_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_START_DATE);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_paramReportDate);
        criteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);


        QueryByCriteria query =
                m_helper.getStudentSelectionQuery(StudentAttendance.class, criteria, StudentAttendance.COL_STUDENT_OID);
        m_absences = getBroker().getGroupedCollectionByQuery(query, StudentAttendance.COL_STUDENT_OID, 256);

        criteria = new X2Criteria();
        criteria.addEqualTo(StudentAttendance.COL_OTHER_CODE, OFF_TRACK_CODE);
        criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE,
                StudentAttendance.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.REL_ACTIVE_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_START_DATE);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_paramReportDate);
        query = m_helper.getStudentSelectionQuery(StudentAttendance.class, criteria, StudentAttendance.COL_STUDENT_OID);
        m_offTrack = getBroker().getGroupedCollectionByQuery(query, StudentAttendance.COL_STUDENT_OID, 256);

    }

    /**
     * Load CATE student program participation.
     */
    private void loadCATEStudentProgramParticipation() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, CODE_I4SEE_CATE_PROGRAM_IDENTIFIER);
        criteria.addEqualTo(m_fieldOrgCateContext, getOrganization().getCurrentContext().getContextId());
        BeanQuery query = new BeanQuery(StudentProgramParticipation.class, criteria);
        query.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);
        m_studentProgramParticipation = getBroker().getGroupedCollectionByQuery(query,
                StudentProgramParticipation.COL_STUDENT_OID, getBroker().getCount(query));
    }
}
