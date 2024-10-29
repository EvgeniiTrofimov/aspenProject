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

package com.x2dev.procedures.statereporting.ct;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.ct.TCS.TCSEntity.ScheduleInfo;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Massachusetts state report for CT TCS export.
 * This class implements the data export for CT TCS export.
 *
 * @author X2 Development Corporation
 */
public class TCS extends StateReportData {
    private static final String SCHEDULE_DAY_KEY = "scheduleDayOid";
    private static final String SCHEDULE_PERIOD_KEY = "schedulePeriodOid";
    private static final String SCHOOL_KEY = "schoolOid";
    private static final String SCHEDULE_TERM_KEY = "scheduleTermOid";
    private static final String MASTER_SCHEDULE_KEY = "masterScheduleOid";
    private static final String ALL_DAYS_KEY = "ALL";

    /**
     * Implementation of StateReportEntity to be used by the TCS export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class TCSEntity extends StateReportEntity {
        /**
         * A container class for schedule information for one scheduled class.
         * This will contain one of a StudentSchedule or StudentScheduleChange,
         * and optionally one Transcript record.
         *
         * @author X2 Development Corporation
         */
        protected class ScheduleInfo {
            private MasterSchedule m_section;
            private Transcript m_transcript;

            /**
             * @param section
             * @param transcript
             */
            public ScheduleInfo(MasterSchedule section, Transcript transcript) {
                super();
                m_section = section;
                m_transcript = transcript;
            }

            /**
             * @param student
             * @return
             */
            public int getAttendanceSessions(SisStudent student) {
                int value = 0;
                if (m_section != null) {
                    TCS data = getData();
                    String membershipSessionsKey =
                            data.getMembershipSessionsAttendedKey(m_section.getOid(), student.getOid());
                    if (data.m_membershipSessionsAttended.containsKey(membershipSessionsKey)) {
                        value = data.m_membershipSessionsAttended.get(membershipSessionsKey).getValue().intValue();
                    }
                }
                return value;
            }

            /**
             * @return
             */
            public Course getCourse() {
                Course course = null;
                if (m_transcript != null && m_transcript.getSchoolCourse() != null) {
                    course = m_transcript.getSchoolCourse().getCourse();
                } else if (m_section != null && m_section.getSchoolCourse() != null) {
                    course = m_section.getSchoolCourse().getCourse();
                }
                return course;
            }

            /**
             * @return
             */
            public BigDecimal getCourseCredit() {
                BigDecimal value = new BigDecimal(0);
                if (m_transcript != null && m_transcript.getTotalCredit() != null) {
                    value = m_transcript.getTotalCredit();
                }
                if (value != null) {
                    value = value.setScale(2, RoundingMode.HALF_UP);
                }
                return value;
            }

            /**
             * @return
             */
            public String getCourseView() {
                String value = null;
                if (m_section != null) {
                    value = m_section.getCourseView();
                }
                return value;
            }

            public String getCourseSection() {
                String value = null;
                if (m_section != null) {
                    School sectionSchool = m_section.getSchedule().getSchool();
                    String schoolStateId = (String) sectionSchool.getFieldValueByAlias(SCHOOL_STATE_ID_ALIAS);
                    value = schoolStateId + "-" + m_section.getCourseView();
                }
                return value;
            }

            /**
             * @return
             */
            public String getDualEnrollment() {
                String value = EMPTY_STRING;
                if (m_section != null) {
                    TCS data = getData();
                    String dualEnrollmentCode = (String) m_section.getFieldValueByAlias(DUAL_ENROLLMENT_ALIAS);
                    if (data.m_dualEnrollmentCodes.containsKey(dualEnrollmentCode)) {
                        ReferenceCode refCode = data.m_dualEnrollmentCodes.get(dualEnrollmentCode);
                        value = refCode.getStateCode();
                    }
                }
                return value;
            }

            /**
             * @return
             */
            public String getFinalGrade() {
                String value = null;
                if (m_transcript != null) {
                    value = m_transcript.getFinalGrade();
                }
                return value;
            }

            /**
             * @return
             */
            public String getFinalGradeStatus() {
                String value = null;
                if (m_transcript != null) {
                    TCS data = getData();
                    if (m_transcript.getTranscriptDefinition() != null &&
                            m_transcript.getTranscriptDefinition().getFinalColumnDefinition() != null &&
                            m_transcript.getTranscriptDefinition().getFinalColumnDefinition().getGradeScale() != null) {
                        if (data.m_gradesManager == null) {
                            data.m_gradesManager = new GradesManager(getData().getBroker());
                        }
                        GradeScale scale =
                                m_transcript.getTranscriptDefinition().getFinalColumnDefinition().getGradeScale();
                        String grade = m_transcript.getFinalGrade();

                        if (!StringUtils.isEmpty(grade)) {
                            if (StringUtils.isNumeric(grade) && scale != null) {
                                // Try the final grade as a number.
                                BigDecimal gradeAsNumber = null;
                                try {
                                    gradeAsNumber = new BigDecimal(grade);
                                } catch (NumberFormatException nfe) {
                                    // nothing. The grade is not numeric.
                                }

                                if (gradeAsNumber != null) {
                                    grade = data.m_gradesManager.getLetterValue(gradeAsNumber, scale,
                                            m_transcript.getSchool(), m_transcript.getSchoolCourseOid());
                                }
                            }
                        }

                        GradeScaleGradeDefinition gradeScaleDefinition =
                                data.m_gradesManager.getGradeDefinition(grade, scale, m_transcript.getSchoolOid(),
                                        m_transcript.getSchoolCourseOid());

                        if (gradeScaleDefinition != null) {
                            value = StringUtils.coalesce((String) gradeScaleDefinition
                                    .getFieldValueByAlias(GRADE_DEFINITION_OUTCOME_GRADE_ALIAS), EMPTY_STRING);
                            if (data.m_gradeStatusCodes.containsKey(value)) {
                                ReferenceCode refCode = data.m_gradeStatusCodes.get(value);
                                value = refCode.getStateCode();
                            }
                        }
                    }
                }
                return value;
            }

            /**
             * @param student
             * @return
             */
            public int getMembershipSessions(SisStudent student) {
                int value = 0;
                if (m_section != null) {
                    TCS data = getData();
                    String membershipSessionsKey =
                            data.getMembershipSessionsAttendedKey(m_section.getOid(), student.getOid());
                    if (data.m_membershipSessionsAttended.containsKey(membershipSessionsKey)) {
                        value = data.m_membershipSessionsAttended.get(membershipSessionsKey).getKey().intValue();
                    }
                }
                return value;
            }

            /**
             * @return
             */
            public ScheduleTeacher getScheduleTeacher() {
                ScheduleTeacher scheduleTeacher = null;
                if (m_section != null) {
                    Collection<ScheduleTeacher> teachers = getData().m_teacherMap.get(m_section.getOid());
                    if (teachers != null) {
                        for (ScheduleTeacher st : teachers) {
                            if (st.getPrimaryTeacherIndicator()) {
                                scheduleTeacher = st;
                                break;
                            }
                        }
                    }
                }
                return scheduleTeacher;
            }

            public String getSchoolCourseNumber() {
                String value = null;
                if (m_section != null) {
                    value = m_section.getSchoolCourse().getNumber();
                }
                return value;
            }

            /**
             * @param student
             * @return
             */
            public Object getSchoolId(SisStudent student) {
                String schoolId = EMPTY_STRING;
                TCS data = getData();

                if (m_section != null && m_section.getSchedule() != null) {
                    SisSchool school = m_section.getSchedule().getSchool();
                    if (school != null) {
                        schoolId = (String) school.getFieldValueByAlias(STATE_ID_ALIAS);
                    }
                }

                if (student != null && data.m_useOverrideSchool) {
                    SisSchool school = student.getSchool();

                    String temp = (String) school.getFieldValueByAlias(OUTSIDE_PLACEMENT_SCHOOL_ALIAS);
                    if (BooleanAsStringConverter.TRUE.equalsIgnoreCase(temp)) {
                        String facility = (String) student.getFieldValueByAlias(OUTSIDE_PLACEMENT_FACILITY_ALIAS);
                        if (data.m_outplacementFacilities.containsKey(facility)) {
                            ReferenceCode refCode = data.m_outplacementFacilities.get(schoolId);
                            if (refCode != null) {
                                schoolId = refCode.getStateCode();
                            }
                        }
                    }

                    temp = (String) school.getFieldValueByAlias(SPECIAL_EDUCATION_SCHOOL_ALIAS);
                    if (BooleanAsStringConverter.TRUE.equalsIgnoreCase(temp)) {
                        String facility = (String) student.getFieldValueByAlias(SPECIAL_EDUCATION_FACILITY_ALIAS);
                        if (data.m_specialEducationFacilities.containsKey(facility)) {
                            ReferenceCode refCode = data.m_specialEducationFacilities.get(schoolId);
                            if (refCode != null) {
                                schoolId = refCode.getStateCode();
                            }
                        }
                    }
                }

                return schoolId;
            }

            /**
             * @return
             */
            public String getSchoolOid() {
                String value = null;
                if (m_section != null && m_section.getSchedule() != null) {
                    value = m_section.getSchedule().getSchoolOid();
                }
                return value;
            }

            /**
             * @return
             */
            public String getSectionOid() {
                String value = null;
                if (m_section != null) {
                    value = m_section.getOid();
                }
                return value;
            }

            /**
             * @return
             */
            public SisStaff getTeacher() {
                SisStaff teacher = null;
                ScheduleTeacher scheduleTeacher = getScheduleTeacher();
                if (scheduleTeacher != null) {
                    teacher = scheduleTeacher.getStaff();
                }
                if (teacher == null && m_section != null) {
                    teacher = m_section.getPrimaryStaff();
                }
                return teacher;
            }

            /**
             * @return
             */
            public String getTermCode() {
                String value = null;
                if (m_section != null && m_section.getScheduleTerm() != null) {
                    value = m_section.getScheduleTerm().getCode();
                }
                return value;
            }

            public void setTranscript(Transcript transcript) {
                this.m_transcript = transcript;
            }

            private TCS getData() {
                return (TCS) TCSEntity.this.getData();
            }
        }

        /*
         * Cached values for retrievers to share.
         */
        TCS m_tcsData = null;
        List<ScheduleInfo> m_schedules = null;

        /**
         * Instantiates a new TCS entity.
         */
           /*
            * Public no argument constructor for dynamic instantiation.
            */
        public TCSEntity() {
            // public no argument constructor for dynamic instantiation.
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
            name += getScheduleInfo().getCourseView();

            return name;
        }

        /**
         * Returns the schedule info record for the schedule record(s)
         * based in the current row value.
         *
         * @return ScheduleInfo
         */
        public ScheduleInfo getScheduleInfo() {
            ScheduleInfo info = null;
            if (getCurrentRow() < m_schedules.size() && getCurrentRow() >= 0) {
                info = m_schedules.get(getCurrentRow());
            }
            return info;
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
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            Map<String, ScheduleInfo> scheduleMap = new HashMap<String, ScheduleInfo>();
            m_tcsData = (TCS) data;
            SisStudent student = (SisStudent) bean;

            /*
             * Collect all schedule relevant to this student.
             * Then assign transcript records to the student schedule records if possible.
             *
             * schedules = collection of student schedules for this student.
             *
             * transcripts = collection of student transcripts for this student.
             */
            Collection<StudentSchedule> schedules =
                    (m_tcsData.m_scheduleMap == null ? null : m_tcsData.m_scheduleMap.get(student.getOid()));
            Collection<Transcript> transcripts =
                    (m_tcsData.m_transcriptMap == null ? null : m_tcsData.m_transcriptMap.get(student.getOid()));

            // Add current schedule sections to the map.
            if (schedules != null) {
                for (StudentSchedule schedule : schedules) {
                    ScheduleInfo info = new ScheduleInfo(schedule.getSection(), null);
                    scheduleMap.put(schedule.getSectionOid(), info);
                }
            }

            // Add schedules that were deleted after the last day of the class
            List<StudentScheduleSpan> studentScheduleSpans = m_tcsData.m_helper.getStudentScheduleSpans(student);
            for (StudentScheduleSpan span : studentScheduleSpans) {
                MasterSchedule section = span.getSection();
                if (section != null && !scheduleMap.containsKey(section.getOid())) {
                    String termCode = section.getScheduleTerm() != null ? section.getScheduleTerm().getCode() : null;
                    String schoolOid = section.getSchedule().getSchoolOid();

                    PlainDate termEndDate = null;
                    if (!StringUtils.isEmpty(termCode) && !StringUtils.isEmpty(schoolOid) &&
                            m_tcsData.m_termDates.containsKey(schoolOid + termCode)) {
                        Object[] termDates = m_tcsData.m_termDates.get(schoolOid + termCode);

                        if (termDates.length >= SCHEDULE_TERM_END_DATE_POSITION + 1) {
                            termEndDate = (PlainDate) termDates[SCHEDULE_TERM_END_DATE_POSITION];
                        }
                    }
                    if (termEndDate == null) {
                        termEndDate = section.getSchedule().getEndDate();
                        if (termEndDate == null) {
                            termEndDate = getData().getCurrentContext().getEndDate();
                        }
                    }
                    if (!span.getExitDate().before(termEndDate)) {
                        ScheduleInfo info = new ScheduleInfo(section, null);
                        scheduleMap.put(section.getOid(), info);
                    }
                }
            }

            // try to match schedule to transcript on master schedule oid.
            if (transcripts != null) {
                for (Transcript trn : transcripts) {
                    ScheduleInfo info = scheduleMap.get(trn.getMasterScheduleOid());
                    if (info != null) {
                        info.setTranscript(trn);
                    } else {
                        info = new ScheduleInfo(trn.getMasterSchedule(), trn);
                        if (!StringUtils.isEmpty(info.getFinalGrade())
                                && !StringUtils.isEmpty(info.getFinalGradeStatus())) {
                            scheduleMap.put(trn.getMasterScheduleOid(), info);
                        }
                    }
                }
            }

            // Put these into a List for ordered processing.
            m_schedules = new ArrayList<ScheduleInfo>(scheduleMap.size());
            for (ScheduleInfo info : scheduleMap.values()) {
                m_schedules.add(info);
            }

            /*
             * With m_schedules populated, we now know how many schedule records for this student.
             */
            setRowCount(m_schedules.size());
            setCurrentRow(0);
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
    }

    /**
     * The Class ScheduleDayPeriod.
     */
    protected static class ScheduleDayPeriod {
        public PlainDate m_date;
        public String m_perOid;

        /**
         * Instantiates a new schedule day period.
         *
         * @param date PlainDate
         * @param perOid String
         */
        public ScheduleDayPeriod(PlainDate date, String perOid) {
            if (date == null) {
                throw new NullPointerException();
            }
            m_date = date;
            m_perOid = perOid;
        }

        /**
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            ScheduleDayPeriod other = (ScheduleDayPeriod) obj;
            return m_date.equals(other.m_date) && Objects.equals(m_perOid, other.m_perOid);
        }

        /**
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return m_date.hashCode() + (m_perOid == null ? -1 : m_perOid.hashCode());
        }


    }

    /**
     * The Class TCSStudentHistoryHelper.
     */
    protected class TCSStudentHistoryHelper extends StudentHistoryHelper {

        /**
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#buildStudentScheduleChangeCriteria()
         */
        @Override
        protected X2Criteria buildStudentScheduleChangeCriteria() {
            X2Criteria studentScheduleChangeCriteria = new X2Criteria();

            studentScheduleChangeCriteria.addNotNull(StudentScheduleChange.COL_ACTION_DATE);

            if (isSchoolContext()) {
                studentScheduleChangeCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_SCHOOL_OID, getSchool().getOid());
            }

            studentScheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);

            studentScheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_fieldExcludeCrs, BooleanAsStringConverter.TRUE);
            getData().applyInputCriteria(studentScheduleChangeCriteria, false, StudentScheduleChange.REL_STUDENT);

            // From appropriate active Schedule
            if (getOrganization().getCurrentContextOid().equals(m_yearContextOid)) {
                studentScheduleChangeCriteria.addEqualToField(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                        StudentSchedule.COL_SCHEDULE_OID);
            } else {
                studentScheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                        SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                        m_yearContextOid);
            }

            if (!m_includeFuture) {
                // section term started before report date.
                studentScheduleChangeCriteria
                        .addLessOrEqualThan(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                                MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER +
                                ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                                ScheduleTermDate.COL_START_DATE, m_reportDate);
            }

            studentScheduleChangeCriteria.addNotEqualTo(
                    StudentScheduleChange.REL_STUDENT + PATH_DELIMITER + m_fieldExcludeStd,
                    BooleanAsStringConverter.TRUE);
            return studentScheduleChangeCriteria;
        }

        /**
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#buildStudentScheduleCriteria()
         */
        @Override
        protected X2Criteria buildStudentScheduleCriteria() {
            X2Criteria studentScheduleCriteria = new X2Criteria();

            studentScheduleCriteria = new X2Criteria();

            if (isSchoolContext()) {
                studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_SCHOOL_OID, getSchool().getOid());
            }

            studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.COL_MASTER_TYPE, "Class");

            studentScheduleCriteria.addNotEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_fieldExcludeCrs, BooleanAsStringConverter.TRUE);
            getData().applyInputCriteria(studentScheduleCriteria, false, StudentSchedule.REL_STUDENT);

            // From appropriate active Schedule
            if (getOrganization().getCurrentContextOid().equals(m_yearContextOid)) {
                studentScheduleCriteria.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                        StudentSchedule.COL_SCHEDULE_OID);
            } else {
                studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                        SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                        m_yearContextOid);
            }

            if (!m_includeFuture) {
                // section term started before report date.
                studentScheduleCriteria.addLessOrEqualThan(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                        MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER +
                        ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                        ScheduleTermDate.COL_START_DATE, m_reportDate);
            }
            studentScheduleCriteria.addNotEqualTo(
                    StudentSchedule.REL_STUDENT + PATH_DELIMITER + m_fieldExcludeStd,
                    BooleanAsStringConverter.TRUE);
            return studentScheduleCriteria;
        }

        /**
         * Instantiates a new TCS student history helper.
         *
         * @param data StateReportData
         */
        public TCSStudentHistoryHelper(StateReportData data) {
            super(data);
        }

    }

    /**
     * Returns course number from the section in the schedule.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCourse implements FieldRetriever {
        private Set<String> m_schoolOidsToProcess = new HashSet();

        /**
         * @param fieldSklProcessClassAttendance
         */
        public RetrieveCourse(String fieldSklProcessClassAttendance) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(fieldSklProcessClassAttendance, BooleanAsStringConverter.TRUE);
            ColumnQuery query = new ColumnQuery(SisSchool.class, new String[] {X2BaseBean.COL_OID}, criteria);
            try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    m_schoolOidsToProcess.add((String) row[0]);
                }
            }
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String param = (String) field.getParameter();
            SisStudent student = (SisStudent) entity.getBean();
            ScheduleInfo info = ((TCSEntity) entity).getScheduleInfo();

            if (TCS_COURSE_NUMBER.equals(param)) {
                value = info.getSchoolCourseNumber();
            } else if (TCS_COURSE_SECTION.equals(param)) {
                value = info.getCourseSection();
            } else if (TCS_COURSE_GRADE.equals(param)) {
                value = info.getFinalGrade();
            } else if (TCS_COURSE_START_DATE.equals(param)) {
                String termCode = info.getTermCode();
                String schoolOid = info.getSchoolOid();

                if (!StringUtils.isEmpty(termCode) && !StringUtils.isEmpty(schoolOid) &&
                        m_termDates.containsKey(schoolOid + termCode)) {
                    Object[] termDates = m_termDates.get(schoolOid + termCode);

                    if (termDates.length >= 1) {
                        value = termDates[SCHEDULE_TERM_START_DATE_POSITION];
                    }
                }
            } else if (TCS_COURSE_END_DATE.equals(param)) {
                String termCode = info.getTermCode();
                String schoolOid = info.getSchoolOid();

                if (!StringUtils.isEmpty(termCode) && !StringUtils.isEmpty(schoolOid) &&
                        m_termDates.containsKey(schoolOid + termCode)) {
                    Object[] termDates = m_termDates.get(schoolOid + termCode);

                    if (termDates.length >= 2) {
                        value = termDates[SCHEDULE_TERM_END_DATE_POSITION];
                    }
                }
            } else if (TCS_COURSE_NCES_COURSE_CREDIT.equals(param)) {
                value = info.getCourseCredit();
            } else if (TCS_COURSE_GRADE_STATUS.equals(param)) {
                value = info.getFinalGradeStatus();
            } else if (m_includeMemberships && TCS_COURSE_MEMBERSHIP.equals(param)
                    && m_schoolOidsToProcess.contains(info.getSchoolOid())) {
                int totalCourseMembershipSessions = info.getMembershipSessions(student);
                value = Integer.valueOf(totalCourseMembershipSessions);
            } else if (m_includeMemberships && TCS_COURSE_ATTENDANCE.equals(param)
                    && m_schoolOidsToProcess.contains(info.getSchoolOid())) {
                int totalCourseAttendanceSessions = info.getAttendanceSessions(student);
                value = Integer.valueOf(totalCourseAttendanceSessions);
            } else if (TCS_COURSE_DUAL_ENROLL.equals(param)) {
                value = info.getDualEnrollment();
            }

            return value;
        }
    }

    /**
     * Retrieve the NCES Course Code
     *
     * {course description}{course level}{available credit}{sequence}
     * {course description}{course level}{grade span }{sequence}.
     *
     * @author Follett Software Company
     */
    protected class RetrieveCourseCode implements FieldRetriever {

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
            ScheduleInfo info = ((TCSEntity) entity).getScheduleInfo();
            String courseCode = null;

            // get the course
            Course course = info.getCourse();

            // start the calculation!
            if (course != null) {
                // 1. get the sced code (assume 5 char)
                courseCode = (String) course.getFieldValueByAlias(COURSE_CODE_ALIAS);

                // 2. if the course code is not empty, continue to process
                if (!StringUtils.isEmpty(courseCode)) {
                    StringBuilder sb = new StringBuilder(12);

                    // 3. if there is a state code build course code value.
                    // otherwise just use course code as entered. it is presumed to be valid code
                    // already.
                    String stateCode = lookupStateValue(Course.class, m_courseCode, courseCode);
                    if (!StringUtils.isEmpty(stateCode)) {
                        sb.append(stateCode);

                        // 4. get the course level (assume 1 char)
                        String courseLevel = (String) course.getFieldValueByAlias(COURSE_LEVEL);
                        courseLevel = lookupStateValue(Course.class, m_courseLevel, courseLevel);
                        sb.append(courseLevel != null ? courseLevel : "X"); // "X" is default for
                                                                            // "No specific level of
                                                                            // rigor"

                        // 5. get grade-span or course credit (assume 4 char)
                        sb.append(getThirdSection(course));

                        // 6. get sequence (2 char)
                        String courseSequence = (String) course.getFieldValueByAlias(COURSE_SEQUENCE);
                        sb.append(!StringUtils.isEmpty(courseSequence) ? courseSequence : "11"); // Assume
                                                                                                 // 1-1
                                                                                                 // if
                                                                                                 // there's
                                                                                                 // no
                                                                                                 // course
                                                                                                 // sequence

                        // 7. set course code
                        courseCode = sb.toString();
                    }
                }
            }

            return courseCode;
        }


        /**
         * Get the third section of the SCED code depending on what the course description code
         * starts with.
         *
         * If the course description code starts with a digit from 01 - 22, return the credit in
         * 0.00 format.
         * If the course description code starts with a digit from 51 - 74, return the grade span
         * (DOE GRADE SPAN).
         *
         * @param course Course object
         *
         * @return the 4 character long string
         */
        private String getThirdSection(Course course) {
            StringBuilder thirdSection = new StringBuilder(12);

            // get the course code
            String courseCode = (String) course.getFieldValueByAlias(COURSE_CODE_ALIAS);
            courseCode = lookupStateValue(Course.class, m_courseCode, courseCode);

            if (!StringUtils.isEmpty(courseCode)) {

                // get the subject area code (it's the first 2 characters of the sced course code)
                String courseSchoolLevel =
                        lookupStateValue(Course.class, Course.COL_SCHOOL_LEVEL, course.getSchoolLevel());

                if (!StringUtils.isEmpty(courseSchoolLevel) && "Secondary".equals(courseSchoolLevel)) {
                    // get credit #.##
                    BigDecimal credit = course.getCredit();
                    if (credit != null) {
                        credit = credit.setScale(2, RoundingMode.CEILING); // round credit up by 2
                                                                           // digits
                        thirdSection.append(credit.toPlainString());
                    }
                } else if (StringUtils.isEmpty(courseSchoolLevel)) {
                    // get grade span
                    String gradeSpan = (String) course.getFieldValueByAlias(COURSE_GRADE_SPAN);

                    // if grade span start/end is empty or is not length 4, fallback to the course's
                    // grade level
                    if (StringUtils.isEmpty(gradeSpan) || gradeSpan.length() != 4) {
                        String courseGradeLevel = course.getGradeLevel(); // assumes 1-2 char
                                                                          // integer
                        courseGradeLevel = StringUtils.padLeft(courseGradeLevel, 2, '0');
                        thirdSection.append(courseGradeLevel);
                        thirdSection.append(courseGradeLevel); // do it twice, since there's no span
                    } else {
                        thirdSection.append(gradeSpan);
                    }
                }

            }
            return thirdSection.toString();
        }

    }

    /**
     * Returns school from the section in the schedule.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSchool implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SisStudent student = (SisStudent) entity.getBean();
            ScheduleInfo info = ((TCSEntity) entity).getScheduleInfo();
            return info.getSchoolId(student);
        }
    }


    /**
     * Returns course level information from the section in the schedule.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTeacher implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;
            String param = (String) field.getParameter();
            ScheduleInfo info = ((TCSEntity) entity).getScheduleInfo();
            SisStaff teacher = info.getTeacher();
            if (teacher != null) {
                if (TCS_TEACHER_TYPE.equals(param)) {
                    ScheduleTeacher scheduleTeacher = info.getScheduleTeacher();
                    if (scheduleTeacher != null) {
                        value = (String) scheduleTeacher.getFieldValueByAlias(TEACHER_OVERRIDE_TYPE_ALIAS);
                    }
                    if (StringUtils.isEmpty(value) && teacher != null) {
                        value = (String) teacher.getFieldValueByAlias(TEACHER_TYPE_ALIAS);
                    }
                    ReferenceCode refCode = m_teacherTypeCodes.get(value);
                    if (refCode != null) {
                        value = refCode.getStateCode();
                    }
                } else if (TCS_TEACHER_STAFF_EIN.equals(param)) {
                    value = (String) teacher.getFieldValueByAlias(TEACHER_EIN_ALIAS);
                } else if (TCS_TEACHER_LOCAL_ID.equals(param)) {
                    value = teacher.getLocalId();
                }
            }

            return value;
        }
    }

    /**
     * Retrieve the facility 1 code.
     *
     * @author Follett Software Company
     */
    protected class RetrieveFacilityCode implements FieldRetriever {

        private static final String CALC_PARAM_CRS_CODE = "CRS_CODE";

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
            SisStudent student = (SisStudent) entity.getBean();
            String schoolId = EMPTY_STRING;
            ScheduleInfo info = ((TCSEntity) entity).getScheduleInfo();
            Course course = info.getCourse();
            String crsFacility = null;
            String param = (String) field.getParameter();
            if (CALC_PARAM_CRS_CODE.equals(param)) {
                if (course != null && !StringUtils
                        .isEmpty(crsFacility = (String) course.getFieldValueByBeanPath(m_fieldCrsFacility))) {
                    schoolId = data.lookupStateValue(Course.class, m_fieldCrsFacility, crsFacility);
                }
            }
            if (StringUtils.isEmpty(schoolId)) {
                schoolId = getCodeByCommonRule(student);
            }
            return schoolId;
        }

        /**
         * Common rule calculation
         *
         * @param student
         * @return
         */
        private String getCodeByCommonRule(SisStudent student) {
            SisSchool school = student.getSchool();
            String schoolId = EMPTY_STRING;
            if (school != null) {
                schoolId = (String) school.getFieldValueByAlias(SCHOOL_STATE_ID_ALIAS);

                String temp = (String) school.getFieldValueByAlias(OUTSIDE_PLACEMENT_SCHOOL_ALIAS);
                if (BooleanAsStringConverter.TRUE.equalsIgnoreCase(temp)) {
                    String facility = (String) student.getFieldValueByAlias(OUTSIDE_PLACEMENT_FACILITY_ALIAS);
                    if (m_outplacementFacilities.containsKey(facility)) {
                        ReferenceCode refCode = m_outplacementFacilities.get(schoolId);
                        if (refCode != null) {
                            schoolId = refCode.getStateCode();
                        }
                    }
                }

                temp = (String) school.getFieldValueByAlias(SPECIAL_EDUCATION_SCHOOL_ALIAS);
                if (BooleanAsStringConverter.TRUE.equalsIgnoreCase(temp)) {
                    String facility = (String) student.getFieldValueByAlias(SPECIAL_EDUCATION_FACILITY_ALIAS);
                    if (m_specialEducationFacilities.containsKey(facility)) {
                        ReferenceCode refCode = m_specialEducationFacilities.get(schoolId);
                        if (refCode != null) {
                            schoolId = refCode.getStateCode();
                        }
                    }
                }
            }
            return schoolId;
        }
    }

    /**
     * The Class ValidateAttendedDays.
     */
    protected class ValidateAttendedDays implements FieldValidator {

        private static final String FIELD_MEMBERSHIP_SESSIONS = "MEMBERSHIP SESSIONS";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            Integer attendedDays = null;
            String memberShipSession = entity.getFieldValue(FIELD_MEMBERSHIP_SESSIONS);
            Integer sessionDays = null;
            try {
                attendedDays = Integer.valueOf(value);
                sessionDays = Integer.valueOf(memberShipSession);
            } catch (NumberFormatException e) {
                // nothing to do
            }
            if (attendedDays != null && sessionDays != null && attendedDays.intValue() > sessionDays.intValue()) {
                errors.add(new StateReportValidationError(entity, field,
                        "Invalid Membership Sessions Attended",
                        " \"Number of Membership Sessions Attended\" should never exceed \"Number of Membership Sessions\" \n\r"
                                +
                                "MEMBERSHIP SESSIONS " + memberShipSession + "\n\r" +
                                "ATTENDED SESSIONS " + value));
            }

            return errors;
        }

    }



    /**
     * Validate membership date.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateFinalGrade implements FieldValidator {
        private static final String SPACE = " ";
        private static final String NEW_LINE = "\n\r";
        private static final String FIELD_STAFFID = "STAFFID";
        private static final String FIELD_FACILITY_1_CODE = "FACILITY 1 CODE";
        private static final String FIELD_COURSE_CODE = "COURSE CODE";

        private static final String PARAM_GRADE = "GRADE";
        private Map<String, String> m_uniqueFinalGradeForCourseSession = new HashMap<String, String>();

        /**
         * Validation for grade.
         * STUENT + COURSE CODE + STAFF ID + SESSION + FACILITY 1 CODE must contain only one grade
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {

            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            TCSEntity tcsentity = (TCSEntity) entity;
            Student std = (Student) tcsentity.getBean();
            String mstOid = tcsentity.getScheduleInfo().getSectionOid();
            String stdName = std.getNameView();
            String courseName = tcsentity.getScheduleInfo().getCourseView();


            String param = (String) field.getParameter();
            if (PARAM_GRADE.equals(param)) {
                String courseCode = entity.getFieldValue(FIELD_COURSE_CODE);
                String facility1Code = entity.getFieldValue(FIELD_FACILITY_1_CODE);
                String staffId = entity.getFieldValue(FIELD_STAFFID);
                String key = std + courseCode + staffId + mstOid + facility1Code;
                if (m_uniqueFinalGradeForCourseSession.containsKey(key)) {
                    String rows = m_uniqueFinalGradeForCourseSession.get(key);
                    rows = rows + NEW_LINE + courseName + SPACE + stdName;
                    m_uniqueFinalGradeForCourseSession.put(key, rows);
                    errors.add(new StateReportValidationError(entity, field,
                            "GRADE is invalid.",
                            "GRADE must by uniue for STUENT + COURSE CODE + STAFF ID + SESSION + FACILITY 1 CODE\n\r"
                                    + "duplicate records are: \n\r" + rows));
                } else {
                    m_uniqueFinalGradeForCourseSession.put(key, EMPTY_STRING + courseName + SPACE + stdName);
                }
            }
            return errors;
        }
    }

    /**
     * The Class ValidateGradeStatus.
     */
    protected class ValidateGradeStatus implements FieldValidator {
        List<String> m_actualStateCodes = null;

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            List<String> actualStateCodes = getActualGradeStateCodes();

            if (value != null && !actualStateCodes.contains(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Invalid Outcome Grade Status",
                        "GRADE_STATUS (" + value + ") must contain actual state codes: \n\r"
                                + actualStateCodes.toString()));
            }

            return errors;
        }


        /**
         * Gets the actual grade state codes.
         *
         * @return List
         */
        private List<String> getActualGradeStateCodes() {
            if (m_actualStateCodes == null) {
                m_actualStateCodes = new ArrayList<String>();
                for (Entry<String, ReferenceCode> codes : m_gradeStatusCodes.entrySet()) {
                    ReferenceCode code = codes.getValue();
                    if (!code.getDisabledIndicator()) {
                        m_actualStateCodes.add(code.getStateCode());
                    }
                }

            }
            return m_actualStateCodes;
        }
    }

    /**
     * Validate membership date.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateSectionCode implements FieldValidator {
        private static final String DASH = "-";

        private static final String PARAM_SECTION = "SECTION";

        private static final String NEW_LINE = "\n\r";

        private static final String FIELD_TEACHER_TYPE = "TEACHER TYPE";

        private static final String FIELD_FACILITY_1_CODE = "FACILITY 1 CODE";

        private static final String FIELD_STAFFID = "STAFFID";

        private static final String FIELD_COURSE_CODE = "COURSE CODE";

        private static final String SPACE = " ";

        /**
         * list Teacher of Record
         */
        private List<String> m_teacherOfRecorTypeCodes =
                new ArrayList<String>(Arrays.asList("101", "201", "301", "401"));

        /**
         * Used for validation Section Codes.
         * list which contain key: Section Codes + COURSE CODE + Facility 1 Codes+ Teacher Type;
         * Teacher Type must contain only 101,201,301,or 401 value.
         * if this list contain duplicate key during all export - create validation error.
         */
        private Map<String, Pair> m_uniqueCourseDistrictForTeacherRecord = new HashMap<String, Pair>();

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

            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            TCSEntity tcsentity = (TCSEntity) entity;
            String stdName = ((Student) tcsentity.getBean()).getNameView();
            String courseName = tcsentity.getScheduleInfo().getCourseView();
            // 13
            String facility1Code = entity.getFieldValue(FIELD_FACILITY_1_CODE);
            // 9
            String courseCode = entity.getFieldValue(FIELD_COURSE_CODE);

            String key = value + DASH + courseCode + DASH + facility1Code;
            // 6
            String teacherType = entity.getFieldValue(FIELD_TEACHER_TYPE);

            String param = (String) field.getParameter();
            if (PARAM_SECTION.equals(param) && m_teacherOfRecorTypeCodes.contains(teacherType)) {

                String staffId = entity.getFieldValue(FIELD_STAFFID);

                String uniqueValue = staffId + teacherType;
                if (m_uniqueCourseDistrictForTeacherRecord.containsKey(key)) {
                    Pair pair = m_uniqueCourseDistrictForTeacherRecord.get(key);

                    if (!pair.getLeft().toString().equals(uniqueValue)) {

                        errors.add(new StateReportValidationError(entity, field,
                                "SECTION is invalid.",
                                "for SECTION + FACILITY 1 CODE + COURSE CODE cannot be more than one Teacher of Record(STAFFID + TEACHER TYPE (101,201,301,or 401))\n\r"
                                        + "current record has " +
                                        FIELD_STAFFID + SPACE + staffId +
                                        SPACE + FIELD_TEACHER_TYPE + SPACE + teacherType +
                                        SPACE + FIELD_COURSE_CODE + SPACE + courseCode +
                                        SPACE + FIELD_FACILITY_1_CODE + SPACE + facility1Code +
                                        NEW_LINE + "records with another section are: \n\r" + pair.getRight()));
                    } else {
                        Pair newPair = Pair.of(uniqueValue,
                                pair.getRight() + NEW_LINE + stdName + SPACE + courseName);
                        m_uniqueCourseDistrictForTeacherRecord.put(key, newPair);
                    }
                } else {
                    Pair pair = Pair.of(uniqueValue,
                            FIELD_STAFFID + SPACE + staffId +
                                    SPACE + FIELD_TEACHER_TYPE + SPACE + teacherType +
                                    SPACE + FIELD_COURSE_CODE + SPACE + courseCode +
                                    SPACE + FIELD_FACILITY_1_CODE + SPACE + facility1Code +
                                    NEW_LINE + stdName + SPACE + courseName);

                    m_uniqueCourseDistrictForTeacherRecord.put(key, pair);
                }
            }

            return errors;
        }


    }

    /**
     * Validate that the staff id is populated when the teacher type is 101-107 or 301-305.
     *
     * @author Follett Software Company
     */
    protected class ValidateStaffId implements FieldValidator {
        private static final String FIELD_TEACHER_TYPE = "TEACHER TYPE";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String teacherType = entity.getFieldValue(FIELD_TEACHER_TYPE);
            int iType = 0;
            try {
                iType = Integer.parseInt(teacherType);
            } catch (Exception e) {
                // Do nothing
            }
            if (StringUtils.isEmpty(value) && ((iType >= 101 && iType <= 107) || (iType >= 301 && iType <= 305))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Educator Identification Number is required when Teacher Type is between 101 and 107 or between 301 and 305.",
                        value));
            }

            return errors;
        }
    }

    /**
     * Validate that the course credit is > 0 when the course credit extracted from the structured
     * course code contains credit > 0.
     *
     */
    protected class ValidateCourseCredit implements FieldValidator {
        private static final String FIELD_COURSE_CODE = "COURSE CODE";
        private Pattern m_pattern = Pattern.compile("\\d\\.\\d\\d");

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String courseCode = entity.getFieldValue(FIELD_COURSE_CODE);
            if (courseCode != null && courseCode.length() >= 10) {
                String courseCredit = courseCode.substring(6, 10);
                Matcher m = m_pattern.matcher(courseCredit);
                if (m.matches()) {
                    double credit = Double.parseDouble(courseCredit);
                    double dvalue = 0.0;
                    try {
                        dvalue = Double.parseDouble(value);
                    } catch (NumberFormatException e) {
                        // Do nothing
                    }
                    if (credit > 0.0 && dvalue <= 0.0) {
                        errors.add(new StateReportValidationError(entity, field,
                                "Credits Earned is required when course code contains credit.",
                                value));
                    }
                }

            }
            return errors;
        }
    }

    /*
     * Schedule Term Date Array Position
     */
    protected static final int SCHEDULE_TERM_END_DATE_POSITION = 1;
    protected static final int SCHEDULE_TERM_START_DATE_POSITION = 0;
    protected static final int SCHEDULE_TERM_START_DATES_POSITION = 2;
    protected static final int SCHEDULE_TERM_END_DATES_POSITION = 3;

    /**
     * Retriever calc parameters for value type.
     */
    protected static final String TCS_TEACHER_TYPE = "TYPE";
    protected static final String TCS_TEACHER_STAFF_EIN = "STAFF_ID";
    protected static final String TCS_TEACHER_LOCAL_ID = "LOCAL_ID";
    protected static final String TCS_COURSE_ATTENDANCE = "ATTENDANCE";
    protected static final String TCS_COURSE_DUAL_ENROLL = "DUAL_ENROLL";
    protected static final String TCS_COURSE_END_DATE = "END_DATE";
    protected static final String TCS_COURSE_GRADE = "GRADE";
    protected static final String TCS_COURSE_GRADE_STATUS = "GRADE_STATUS";
    protected static final String TCS_COURSE_MEMBERSHIP = "MEMBERSHIP";
    protected static final String TCS_COURSE_NCES_COURSE_CREDIT = "NCES_COURSE_CREDIT";
    protected static final String TCS_COURSE_NUMBER = "NUMBER";
    protected static final String TCS_COURSE_SECTION = "SECTION";
    protected static final String TCS_COURSE_START_DATE = "START_DATE";

    /*
     * Field alias for the adjusted district code. This alias is optional.
     */
    protected static final String ADJUSTED_DISTRICT_CODE_FIELD = "DOE ADJUSTED DISTRICT";
    protected static final String ALIAS_CRS_FACILITY_CODE = "all-crs-CourseFacilityCodeOverride";
    protected static final String ALIAS_EXCLUDE_CRS = "DOE EXCLUDE TCS";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_SKL_PROCESS_CLASS_ATTENDANCE = "all-skl-ClassAttendanceforTCS";
    protected static final String OUTSIDE_PLACEMENT_FACILITY_ALIAS = "Outside Placement Facility";
    protected static final String SPECIAL_EDUCATION_FACILITY_ALIAS = "Special Education Facility";
    protected static final String OUTSIDE_PLACEMENT_SCHOOL_ALIAS = "Outplacement School";
    protected static final String SPECIAL_EDUCATION_SCHOOL_ALIAS = "Special Education School";
    protected static final String STATE_ID_ALIAS = "StateId";

    protected static final String FIELD_FACILITY_CODE = "COURSE FACILITY CODE";

    protected static final String GRADE_DEFINITION_OUTCOME_GRADE_ALIAS = "DOE Outcome Grade";
    protected static final String GRADE_STATUS_ALIAS = "DOE Outcome Grade";
    protected static final String TEACHER_TYPE_ALIAS = "DOE TEACHER TYPE";
    protected static final String TEACHER_OVERRIDE_TYPE_ALIAS = "DOE CLASS TEACHER TYPE";
    protected static final String TEACHER_EIN_ALIAS = "DOE EIN";
    protected static final String COURSE_CODE_ALIAS = "DOE COURSE CODE";
    protected static final String COURSE_GRADE_SPAN = "DOE GRADE SPAN";
    protected static final String COURSE_SEQUENCE = "DOE COURSE SEQUENCE";
    protected static final String COURSE_LEVEL = "DOE COURSE LEVEL";
    protected static final String DUAL_ENROLLMENT_ALIAS = "DOE Dual Enrollment";
    protected static final String SCHOOL_STATE_ID_ALIAS = "StateId";

    /**
     * Name for the report date parameter. The corresponding values is a PlainDate object.
     */
    public static final String REPORT_DATE_PARAM = "reportDate";

    /**
     * Name for the report year OID parameter. The corresponding values is an OID (String) object.
     */
    public static final String REPORT_YEAR_PARAM = "reportYearContext";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /*
     * Field alias constants. These field aliases are all for the STUDENT table.
     */
    private static final String STUDENT_NAME = "name view";

    /**
     * Name for the parameter to include future scheduled classes. The value is a Boolean.
     */
    public static final String INCLUDE_SCHEDULE_PARAM = "includeSchedule";

    /**
     * Name for the "include student names" parameter. The value is a Boolean.
     */
    public static final String INCLUDE_STUDENT_NAMES_PARAM = "includeStudentName";

    /**
     * Name for the parameter to include future scheduled classes. The value is a Boolean.
     */
    public static final String INCLUDE_FUTURE_SCHEDULE_PARAM = "includeFuture";

    /**
     * Name for the parameter to include future scheduled classes. The value is a Boolean.
     */
    public static final String INCLUDE_TRANSCRIPT_PARAM = "includeTranscript";

    /**
     * Name for the parameter to include membership calculations
     */
    public static final String INCLUDE_MEMBERSHIP_PARAM = "includeMemberships";

    /**
     * Name for the parameter to use the students override school instead of the schedule school.
     * The value is a Boolean.
     */
    public static final String USE_OVERRIDE_SCHOOL_PARAM = "overrideSchool";


    private static final String INCLUDE_REPORT_HEADINGS_PARAM = "includeReportHeadings";

    private static final Integer INTEGER_ONE = Integer.valueOf(1);

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected Map<String, ReferenceCode> m_dualEnrollmentCodes;
    protected Map<String, ReferenceCode> m_gradeStatusCodes;
    protected Map<String, ReferenceCode> m_outplacementFacilities;
    protected Map<String, ReferenceCode> m_specialEducationFacilities;
    protected Map<String, ReferenceCode> m_teacherTypeCodes;

    protected String m_courseCode;
    protected String m_courseGradeSpan;
    protected String m_courseSequence;
    protected String m_courseLevel;
    protected String m_fieldCrsFacility;
    protected String m_fieldExcludeCrs;
    protected String m_fieldExcludeStd;
    protected String m_fieldSklProcessClassAttendance;
    protected String m_yearContextOid;

    protected TCSStudentHistoryHelper m_helper;

    protected boolean m_includeFuture;
    protected boolean m_includeMemberships;
    protected boolean m_includeReportHeadings;
    protected boolean m_includeSchedule;
    protected boolean m_includeStudentNames;
    protected boolean m_includeTranscript;
    protected boolean m_useOverrideSchool;

    protected PlainDate m_reportDate;
    protected Map<String, Integer> m_mapDayOidToNumber = new HashMap<String, Integer>();
    /**
     * Key1: schoolOid + termCode + dayNumber <br>
     * Key2: calendarCode <br>
     * Value: set of all dates occurring in key
     */
    protected Map<String, Map<String, Set<PlainDate>>> m_membershipSessions;
    protected Map<String, KeyValuePair<Integer, Integer>> m_membershipSessionsAttended;
    protected Map<String, String> m_mostCommonCalendarCode = new HashMap<String, String>();
    /**
     * Key: mstOid + calendarCode <br>
     * Value: set of all dates for the section and calendar code
     */
    protected Map<String, Set<ScheduleDayPeriod>> m_scheduleDaysMap;
    protected Map<String, Collection<StudentSchedule>> m_scheduleMap;
    protected ScheduleManager m_scheduleMgr = null;
    protected Map<String, ScheduleTerm> m_scheduleTerms = new HashMap<String, ScheduleTerm>();
    protected Map<String, Map<PlainDate, Integer>> m_studentClassAbsences;
    protected Map<String, Collection<ScheduleTeacher>> m_teacherMap;
    /**
     * Key consists of sklOid+trnOid<br>
     * Object array contains four values:<br>
     * 1) PlainDate - earliest start date from schedule terms<br>
     * 2) PlainDate - latest start date from schedule terms<br>
     * 3) List<PlainDate> - List of start dates for schedule terms<br>
     * 4) List<PlainDate> - List of end dates for schedule terms
     */
    protected Map<String, Object[]> m_termDates;
    protected Map<String, Collection<Transcript>> m_transcriptMap;
    protected X2Criteria m_studentScheduleCriteria;
    protected X2Criteria m_studentTranscriptCriteria;
    protected GradesManager m_gradesManager = null;


    /**
     * Flag to include column names as a header row
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return m_includeReportHeadings;
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        // Load initialization data
        initializeFields();


        /*
         * Get core parameters
         */
        m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);
        m_yearContextOid = (String) getParameter(REPORT_YEAR_PARAM);
        m_useOverrideSchool = ((Boolean) getParameter(USE_OVERRIDE_SCHOOL_PARAM)).booleanValue();
        m_includeFuture = ((Boolean) getParameter(INCLUDE_FUTURE_SCHEDULE_PARAM)).booleanValue();
        m_includeSchedule = ((Boolean) getParameter(INCLUDE_SCHEDULE_PARAM)).booleanValue();
        m_includeStudentNames = ((Boolean) getParameter(INCLUDE_STUDENT_NAMES_PARAM)).booleanValue();
        m_includeTranscript = ((Boolean) getParameter(INCLUDE_TRANSCRIPT_PARAM)).booleanValue();
        m_includeMemberships = ((Boolean) getParameter(INCLUDE_MEMBERSHIP_PARAM)).booleanValue();
        m_includeReportHeadings = ((Boolean) getParameter(INCLUDE_REPORT_HEADINGS_PARAM)).booleanValue();

        if (!m_includeSchedule && !m_includeTranscript) {
            addSetupError("Missing input", "Must select 'Include Schedule' or 'Include Transcript'.");
        }
        /*
         * Set the field definition array
         */
        if (m_includeStudentNames) {
            getFieldDefinitions().add(0, getName());
        }
        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build helper object to consider students active on the end current context date.
             */
            m_helper = new TCSStudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_SECONDARY, Boolean.TRUE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_EXCLUDE_FUTURE_SCHEDULES, Boolean.TRUE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE,
                    getOrganization().getCurrentContext().getEndDate());
            // Load a map of student transcript records and student schedules by student.
            // These also prepare criteria that is used in the student query.
            if (m_includeSchedule) {
                loadStudentSchedules();
            }
            if (m_includeTranscript) {
                loadTranscripts();
            }
            loadScheduleTeachers();
            loadScheduleTerms();


            if (m_includeMemberships) {
                X2Criteria criteriaMst = new X2Criteria();
                addMasterScheduleCriteria(criteriaMst, "");

                loadMembershipSessions();
                loadScheduleDayPeriods(criteriaMst);
                loadStudentClassAttendance();
                loadMembershipSessionsAttended();
            }

            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(TCSEntity.class);

            // Build maps of retriever functions and validator functions
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("TCS-SCHOOL", new RetrieveSchool());
            calcs.put("TCS-COURSE", new RetrieveCourse(m_fieldSklProcessClassAttendance));
            calcs.put("TCS-TEACHER", new RetrieveTeacher());
            calcs.put("TCS-NCES", new RetrieveCourseCode());
            calcs.put("TCS-FACILITY", new RetrieveFacilityCode());

            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldValidator>();
            validators.put("SECTION_CODE", new ValidateSectionCode());
            validators.put("FINAL_GRADE", new ValidateFinalGrade());
            validators.put("GRADE_STATUS", new ValidateGradeStatus());
            validators.put("ATTENDED_SESSIONS", new ValidateAttendedDays());
            validators.put("STAFF_ID", new ValidateStaffId());
            validators.put("NCES_COURSE_CREDIT", new ValidateCourseCredit());


            addValidators(validators);
        }
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    protected void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + ModelProperty.PATH_DELIMITER + RecordSet.COL_NAME,
                recordSetName);

        // This is used by both student schedule and transcript.
        // Fortunately both have an identical student OID field so using Transcript.COL_STUDENT_OID
        // will work for both.
        criteria.addIn(Transcript.COL_STUDENT_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Concats the studentOid and mstOid into the classAtendanceKey.
     *
     * @param studentOid String
     * @param mstOid String
     * @return String
     */
    protected String getClassAttendanceKey(String studentOid, String mstOid) {
        return studentOid + mstOid;
    }



    /**
     * Concats the mstOid, studentOid, into the membershipSessionsAttendedKey.
     *
     * @param mstOid String
     * @param studentOid String
     * @return String
     */
    protected String getMembershipSessionsAttendedKey(String mstOid, String studentOid) {
        return mstOid + studentOid;
    }

    /**
     * Concats the schoolOid, termCode, and dayNumber into the membershipSessionsKey.
     *
     * @param schoolOid String
     * @param termCode String
     * @param dayNumber String
     * @return String
     */
    protected String getMembershipSessionsKey(String schoolOid, String termCode, String dayNumber) {
        return schoolOid + termCode + dayNumber;
    }

    /**
     * Gets the most common calendar code.
     *
     * @param school SisSchool
     * @return String
     */
    protected String getMostCommonCalendarCode(SisSchool school) {
        String value = null;
        if (m_scheduleMgr == null) {
            m_scheduleMgr = new ScheduleManager(getBroker());
        }
        if (m_mostCommonCalendarCode.containsKey(school.getOid())) {
            value = m_mostCommonCalendarCode.get(school.getOid());
        } else {
            value = getMostCommonCalendar(school);
            m_mostCommonCalendarCode.put(school.getOid(), value);
        }
        return value;
    }

    /**
     * Modified from
     * com.x2dev.sis.model.business.schedule.ScheduleManager.getMostCommonCalendar(Schedule,
     * Collection<MasterSchedule>)
     *
     * Get the most common calendar id across all students in the section. This calendar will be
     * used, as the current assumption is that assignments apply to all students
     * and thus days only a minority of the students are in the course there should be no
     * assignments.
     *
     * @param schedule Schedule
     * @param sections (can be null)
     * @return calendarId
     */
    protected String getMostCommonCalendar(SisSchool school) {
        String calendarId = null;

        /*
         * Find the calendars associated with the schedule's school and the schedule context.
         */
        Criteria calendarsCriteria = new Criteria();
        calendarsCriteria.addEqualTo(SchoolCalendar.COL_SCHOOL_OID, school.getOid());
        calendarsCriteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, school.getCurrentContextOid());

        Collection<SchoolCalendar> schoolCalendars = getBroker().getCollectionByQuery(new QueryByCriteria(
                SchoolCalendar.class, calendarsCriteria));

        /*
         * If there is more than one calendar then this needs to figure out which is the most common
         * for the section or for the schedule. Otherwise if there is only one then use that
         * calendar
         * since there is no other choices.
         */
        if (schoolCalendars.size() > 1) {
            ColumnQuery calendarQuery = null;
            X2Criteria calendarCriteria = new X2Criteria();

            /*
             * For schedule based search we group students who are active by calendar code.
             */
            calendarCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, school.getOid());
            calendarCriteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(
                    school.getOrganization1(), SisStudent.COL_ENROLLMENT_STATUS));
            String[] columns = new String[] {SisStudent.COL_CALENDAR_CODE};

            calendarQuery = new ColumnQuery(Student.class, columns, calendarCriteria);
            calendarQuery.addGroupBy(SisStudent.COL_CALENDAR_CODE);
            calendarQuery.addOrderByDescending("count(" + SisStudent.COL_CALENDAR_CODE + ")");

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(calendarQuery);
            try {
                if (iterator.hasNext()) {
                    calendarId = (String) ((Object[]) iterator.next())[0];
                }
            } finally {
                iterator.close();
            }
        } else if (schoolCalendars.size() == 1) {
            SchoolCalendar calendar = (SchoolCalendar) schoolCalendars.toArray()[0];
            calendarId = calendar.getCalendarId();
        }

        /*
         * Previous year sections may not have student schedules available. In that case, look for
         * the default
         * calendar. If they custom named all their calendars, use the lowest OID one (assuming it
         * was the first
         * created and is the default one).
         */
        if (calendarId == null) {
            Criteria defaultCalendarCriteria = new Criteria();
            defaultCalendarCriteria.addEqualTo(SchoolCalendar.COL_SCHOOL_OID, school.getOid());
            defaultCalendarCriteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                    school.getCurrentContextOid());

            SubQuery defaultCalendarQuery =
                    new SubQuery(SchoolCalendar.class, SchoolCalendar.COL_CALENDAR_ID, defaultCalendarCriteria);
            defaultCalendarQuery.addOrderByAscending(X2BaseBean.COL_OID);

            Collection<String> calendars = getBroker().getSubQueryCollectionByQuery(defaultCalendarQuery);

            if (!calendars.isEmpty()) {
                String defaultName = LocalizationCache.getMessages(getBroker().getPersistenceKey())
                        .getMessage("label.districtCalendarWizard.2.defaultName");
                if (calendars.contains(defaultName)) {
                    calendarId = defaultName;
                } else {
                    calendarId = calendars.iterator().next();
                }
            }
        }

        return calendarId;
    }

    /**
     * Build Field definition for the student name view.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getName() {
        FieldDefinition field = new FieldDefinition(STUDENT_NAME,
                SisStudent.COL_NAME_VIEW,
                null,
                false,
                1,
                32,
                null,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Concats schoolOid and dayNum into the scheduleDayNumberKey.
     *
     * @param schoolOid String
     * @param dayId String
     * @return String
     */
    protected String getScheduleDayNumberKey(String schoolOid, String dayId) {
        return schoolOid + dayId;
    }

    /**
     * Gets the schedule days key.
     *
     * @param mstOid String
     * @param calendarCode String
     * @return String
     */
    protected String getScheduleDaysKey(String mstOid, String calendarCode) {
        return mstOid + calendarCode;
    }

    /**
     * Get the schedule term.
     *
     * @param oid String
     * @return Schedule term
     */
    protected ScheduleTerm getScheduleTerm(String oid) {
        ScheduleTerm term = null;
        if (m_scheduleTerms.containsKey(oid)) {
            term = m_scheduleTerms.get(oid);
        } else {
            term = (ScheduleTerm) getBroker().getBeanByOid(ScheduleTerm.class, oid);
            m_scheduleTerms.put(oid, term);
        }
        return term;
    }



    /**
     * Function for building custom MasterSchedule criteria.
     *
     * @param criteria X2Criteria
     * @param prefixPath String
     * @return X2Criteria
     */
    private void addMasterScheduleCriteria(X2Criteria criteria, String prefixPath) {

        // Filter by year context
        criteria.addEqualTo(prefixPath + MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.COL_DISTRICT_CONTEXT_OID,
                m_yearContextOid);

        // Include Active Schedules only
        if (getOrganization().getCurrentContextOid().equals(m_yearContextOid)) {
            criteria.addEqualToField(prefixPath + MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                    SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                    StudentSchedule.COL_SCHEDULE_OID);
        } else {
            criteria.addEqualTo(prefixPath + MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                    SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                    m_yearContextOid);
        }

        // Exclude courses with exclude flag
        criteria.addNotEqualTo(
                prefixPath + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE +
                        PATH_DELIMITER + m_fieldExcludeCrs,
                BooleanAsStringConverter.TRUE);

        if (isSchoolContext()) {
            criteria.addEqualTo(prefixPath + MasterSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID,
                    getSchool().getOid());
        }
    }

    /**
     * Find term code into map schoolTermPeriods using day and schoolOid.
     *
     * @param schoolTermPeriods Map<String,Map<String,Object[]>>
     * @param schoolOid String
     * @param date PlainDate
     * @return List
     */
    private List<String> determineTermcode(Map<String, Map<String, Object[]>> schoolTermPeriods,
                                           String schoolOid,
                                           PlainDate date) {
        List<String> value = new ArrayList<String>();
        if (schoolTermPeriods.containsKey(schoolOid)) {
            Map<String, Object[]> termDatesMap = schoolTermPeriods.get(schoolOid);
            for (String term : termDatesMap.keySet()) {
                Object[] termDates = termDatesMap.get(term);
                List<PlainDate> startDates = (List<PlainDate>) termDates[SCHEDULE_TERM_START_DATES_POSITION];
                List<PlainDate> endDates = (List<PlainDate>) termDates[SCHEDULE_TERM_END_DATES_POSITION];

                if (!startDates.isEmpty() && startDates.size() == endDates.size()) {
                    for (int i = 0; i < startDates.size(); i++) {
                        PlainDate startDate = startDates.get(i);
                        PlainDate endDate = endDates.get(i);
                        if (startDate != null && date != null && !date.before(startDate) &&
                                (endDate == null || !endDate.before(date))) {
                            value.add(term);
                            break;
                        }
                    }
                }
            }
        }
        return value;
    }

    /**
     * translate SCHEDULE_DAY_OID to day number.
     *
     * @param dayOid String
     * @return Integer (can return null)
     */
    private Integer getDayNumber(String dayOid) {
        Integer number = null;
        if (m_mapDayOidToNumber.containsKey(dayOid)) {
            number = m_mapDayOidToNumber.get(dayOid);
        } else {
            ScheduleDay scheduleDay = (ScheduleDay) getBroker().getBeanByOid(ScheduleDay.class, dayOid);
            if (scheduleDay != null) {
                number = Integer.valueOf(scheduleDay.getNumber());
            }
        }

        return number;
    }

    /**
     * Gets the num absences.
     *
     * @param key a combination of the student and section, see {@link
     *        #getClassAttendanceKey(String, String)}
     * @param date PlainDate
     * @return int
     */
    private int getNumAbsences(String key, PlainDate date) {
        int value = 0;
        Map<PlainDate, Integer> counts = m_studentClassAbsences.get(key);
        if (counts != null) {
            Integer count = counts.get(date);
            if (count != null) {
                value = count.intValue();
            }
        }

        return value;
    }

    /**
     * Look up a map of reference codes for a field, based on the alias of the field.
     *
     * @param alias String
     * @return Map<String, ReferenceCode>
     */
    private Map<String, ReferenceCode> getReferenceCodesForAlias(String alias) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField dictionaryField =
                dictionary.findDataDictionaryFieldByAlias(alias);

        Map<String, ReferenceCode> refCodes = new HashMap<String, ReferenceCode>();

        if (dictionaryField == null) {
            if (m_useOverrideSchool) {
                addSetupError("data dictionary alias is missing",
                        "alias: " + alias + " is missing from the data dictionary");
            }
        } else {
            ReferenceTable refTable = dictionaryField.getReferenceTable();
            if (refTable == null) {
                addSetupError("missing reference table", "reference table not found for the " + alias + " alias ");
            } else {
                refCodes = refTable.getCodeMap(getBroker());
            }

        }

        return refCodes;
    }

    /**
     * Set up maps and objects for report usage.
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        /*
         * Resolve some support aliases, log an error and stop the export if an alias is not found.
         */
        m_dualEnrollmentCodes = getReferenceCodesForAlias(DUAL_ENROLLMENT_ALIAS);
        m_gradeStatusCodes = getReferenceCodesForAlias(GRADE_STATUS_ALIAS);
        m_outplacementFacilities = getReferenceCodesForAlias(OUTSIDE_PLACEMENT_FACILITY_ALIAS);
        m_specialEducationFacilities = getReferenceCodesForAlias(SPECIAL_EDUCATION_FACILITY_ALIAS);
        m_teacherTypeCodes = getReferenceCodesForAlias(TEACHER_TYPE_ALIAS);
        m_courseCode = translateAliasToJavaName(COURSE_CODE_ALIAS, true);
        m_courseGradeSpan = translateAliasToJavaName(COURSE_GRADE_SPAN, true);
        m_courseSequence = translateAliasToJavaName(COURSE_SEQUENCE, true);
        m_courseLevel = translateAliasToJavaName(COURSE_LEVEL, true);
        m_fieldExcludeCrs = translateAliasToJavaName(ALIAS_EXCLUDE_CRS, true);
        m_fieldCrsFacility = translateAliasToJavaName(ALIAS_CRS_FACILITY_CODE, true);
        m_fieldSklProcessClassAttendance = translateAliasToJavaName(ALIAS_SKL_PROCESS_CLASS_ATTENDANCE, true);
        m_fieldExcludeStd = translateAliasToJavaName(ALIAS_EXCLUDE_STD, true);

    }

    /**
     * returns true if the date is contained in the passed List<StudentEnrollmentSpan>.
     *
     * @param date PlainDate
     * @param enrollmentSpans List<StudentEnrollmentSpan>
     * @return true, if is enrolled day
     */
    private boolean isEnrolledDay(PlainDate date, List<StudentEnrollmentSpan> enrollmentSpans) {
        boolean value = false;
        for (StudentEnrollmentSpan enrollmentSpan : enrollmentSpans) {
            PlainDate startDate = enrollmentSpan.getFirstActiveDate();
            PlainDate endDate = enrollmentSpan.getLastActiveDate();
            if (startDate != null && date != null && (date.after(startDate) || date.equals(startDate)) &&
                    (endDate == null || (endDate != null && (endDate.after(date) || endDate.equals(date))))) {
                value = true;
                break;
            }
        }
        return value;
    }

    /**
     * returns true if the date passed belongs to the passed StudentScheduleSpan.
     *
     * @param date PlainDate
     * @param scheduleSpan StudentScheduleSpan
     * @return true, if is schedule span day
     */
    private boolean isScheduleSpanDay(PlainDate date, StudentScheduleSpan scheduleSpan) {
        boolean value = false;
        PlainDate startDate = scheduleSpan.getEntryDate();
        PlainDate endDate = scheduleSpan.getExitDate();
        if (startDate != null && date != null && (date.after(startDate) || date.equals(startDate)) &&
                (endDate == null || (endDate != null && (endDate.after(date) || endDate.equals(date))))) {
            value = true;
        }
        return value;
    }

    /**
     * Loads a map of the session days for every schedule + calendarCode.
     */
    private void loadMembershipSessions() {
        m_membershipSessions = new HashMap<String, Map<String, Set<PlainDate>>>();

        // Map<sklOid, Map<termCode, termDates>>
        Map<String, Map<String, Object[]>> schoolTermPeriods = new HashMap<String, Map<String, Object[]>>();
        for (String schoolOidTermCode : m_termDates.keySet()) {
            Object[] termDates = m_termDates.get(schoolOidTermCode);

            String schoolOid = schoolOidTermCode.substring(0, 14);
            String termCode = schoolOidTermCode.substring(14);

            Map<String, Object[]> termPeriods;
            if (schoolTermPeriods.containsKey(schoolOid)) {
                termPeriods = schoolTermPeriods.get(schoolOid);
            } else {
                termPeriods = new HashMap<String, Object[]>();
                schoolTermPeriods.put(schoolOid, termPeriods);
            }
            if (!termPeriods.containsKey(termCode)) {
                termPeriods.put(termCode, termDates);
            }
        }

        X2Criteria calendarDaysCriteria = new X2Criteria();
        calendarDaysCriteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_yearContextOid);

        if (isSchoolContext()) {
            calendarDaysCriteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                    SchoolCalendar.COL_SCHOOL_OID, getSchool().getOid());
        }

        calendarDaysCriteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, BooleanAsStringConverter.TRUE);

        calendarDaysCriteria.addEqualTo(
                SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER + SchoolCalendar.REL_SCHOOL
                        + ModelProperty.PATH_DELIMITER + m_fieldSklProcessClassAttendance,
                BooleanAsStringConverter.TRUE);

        QueryByCriteria schoolCalendarDateQuery = new QueryByCriteria(SchoolCalendarDate.class, calendarDaysCriteria);
        schoolCalendarDateQuery.addOrderByDescending(SchoolCalendarDate.COL_DATE);

        QueryIterator schoolCalendarDateIterator = getBroker().getIteratorByQuery(schoolCalendarDateQuery);

        try {
            while (schoolCalendarDateIterator.hasNext()) {
                SchoolCalendarDate calendarDate = (SchoolCalendarDate) schoolCalendarDateIterator.next();

                int dayNumber = calendarDate.getScheduleDayNumber();
                String calendarCode = calendarDate.getSchoolCalendar().getCalendarId();
                String schoolOid = calendarDate.getSchoolCalendar().getSchoolOid();

                List<String> termCodes = determineTermcode(schoolTermPeriods, schoolOid, calendarDate.getDate());
                for (String termCode : termCodes) {
                    String membershipSessionsKey =
                            getMembershipSessionsKey(schoolOid, termCode, Integer.toString(dayNumber));

                    Map<String, Set<PlainDate>> map = m_membershipSessions.get(membershipSessionsKey);
                    if (map == null) {
                        map = new HashMap<String, Set<PlainDate>>();
                        m_membershipSessions.put(membershipSessionsKey, map);
                    }
                    Set<PlainDate> plainDates = map.get(calendarCode);
                    if (plainDates == null) {
                        plainDates = new HashSet<PlainDate>();
                        map.put(calendarCode, plainDates);
                    }
                    plainDates.add(calendarDate.getDate());
                    // create second map for "ALL" day code - used when a day cannot be found on a
                    // section to pull all in session days from calendar
                    String membershipSessionsKeyAllDays = getMembershipSessionsKey(schoolOid, termCode, ALL_DAYS_KEY);
                    Map<String, Set<PlainDate>> allDaysMap = m_membershipSessions.get(membershipSessionsKeyAllDays);
                    if (allDaysMap == null) {
                        allDaysMap = new HashMap<String, Set<PlainDate>>();
                        m_membershipSessions.put(membershipSessionsKeyAllDays, allDaysMap);
                    }
                    Set<PlainDate> plainDatesForAllDays = allDaysMap.get(calendarCode);
                    if (plainDatesForAllDays == null) {
                        plainDatesForAllDays = new HashSet<PlainDate>();
                        allDaysMap.put(calendarCode, plainDatesForAllDays);
                    }
                    plainDatesForAllDays.add(calendarDate.getDate());

                }
            }
        } finally {
            schoolCalendarDateIterator.close();
        }

    }

    /**
     * Loads a map of the attended session days for every schedule + studentOid.
     */
    private void loadMembershipSessionsAttended() {
        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, m_helper.getStudentCriteria());
        Collection<SisStudent> students = getBroker().getCollectionByQuery(studentQuery);

        m_membershipSessionsAttended = new HashMap();

        if (students != null) {
            for (SisStudent student : students) {
                String studentOid = student.getOid();
                String calendarCode = student.getCalendarCode();
                if (calendarCode == null) {
                    calendarCode = getMostCommonCalendarCode(student.getSchool());
                }

                List<StudentScheduleSpan> studentScheduleSpans = m_helper.getStudentScheduleSpans(student);
                List<StudentEnrollmentSpan> enrollmentSpans = m_helper.getStudentEnrollmentSpans(student, false);

                for (StudentScheduleSpan scheduleSpan : studentScheduleSpans) {
                    MasterSchedule mstCandidat = scheduleSpan.getSection();

                    String scheduleDaysKey = getScheduleDaysKey(mstCandidat.getOid(), calendarCode);
                    Set<ScheduleDayPeriod> dayPeriods = new HashSet<ScheduleDayPeriod>();
                    if (m_scheduleDaysMap.containsKey(scheduleDaysKey)) {
                        dayPeriods = m_scheduleDaysMap.get(scheduleDaysKey);
                    }


                    // Transform to sessions per day
                    Map<PlainDate, Integer> sessionCounts = new HashMap();
                    for (ScheduleDayPeriod dayPeriod : dayPeriods) {
                        Integer value = sessionCounts.get(dayPeriod.m_date);
                        if (value == null) {
                            sessionCounts.put(dayPeriod.m_date, INTEGER_ONE);
                        } else {
                            sessionCounts.put(dayPeriod.m_date, Integer.valueOf(value.intValue() + 1));
                        }
                    }
                    int numMember = 0;
                    int numAttended = 0;
                    for (Entry<PlainDate, Integer> entry : sessionCounts.entrySet()) {
                        String classAttendanceKey = getClassAttendanceKey(studentOid, mstCandidat.getOid());
                        if (isScheduleSpanDay(entry.getKey(), scheduleSpan) &&
                                isEnrolledDay(entry.getKey(), enrollmentSpans)) {
                            numMember += entry.getValue().intValue();
                            numAttended +=
                                    entry.getValue().intValue() - getNumAbsences(classAttendanceKey, entry.getKey());
                        }
                    }
                    String membershipSessionsKey =
                            getMembershipSessionsAttendedKey(mstCandidat.getOid(), studentOid);
                    KeyValuePair<Integer, Integer> counts = m_membershipSessionsAttended.get(membershipSessionsKey);
                    if (counts == null) {
                        m_membershipSessionsAttended.put(membershipSessionsKey,
                                new KeyValuePair(Integer.valueOf(numMember), Integer.valueOf(numAttended)));
                    } else {
                        m_membershipSessionsAttended.put(membershipSessionsKey,
                                new KeyValuePair(Integer.valueOf(numMember + counts.getKey().intValue()),
                                        Integer.valueOf(numAttended + counts.getValue().intValue())));
                    }
                }
            }
        }
    }

    /**
     * Loads a map of dates for MasterSchedule OID(section) + calendarCode.
     *
     * @param mstCriteria X2Criteria
     */
    private void loadScheduleDayPeriods(X2Criteria mstCriteria) {

        m_scheduleDaysMap = new HashMap<String, Set<ScheduleDayPeriod>>();
        List<Map<String, String>> results = new ArrayList<>();

        X2Criteria masterScheduleMatrixCriteria = new X2Criteria();
        masterScheduleMatrixCriteria.addIn(
                MasterScheduleMatrix.REL_MASTER_TERM + ModelProperty.PATH_DELIMITER
                        + MasterTerm.COL_MASTER_SCHEDULE_OID,
                new SubQuery(MasterSchedule.class, X2BaseBean.COL_OID, mstCriteria));

        BeanQuery masterScheduleMatrixQuery = new BeanQuery(MasterScheduleMatrix.class, masterScheduleMatrixCriteria);
        QueryIterator masterScheduleMatrixIterator = getBroker().getIteratorByQuery(masterScheduleMatrixQuery);
        try {
            while (masterScheduleMatrixIterator.hasNext()) {
                Map<String, String> row = new HashMap<>();
                MasterScheduleMatrix matrix = (MasterScheduleMatrix) masterScheduleMatrixIterator.next();
                MasterTerm term = matrix.getMasterTerm();
                if (term != null) {
                    row.put(MASTER_SCHEDULE_KEY, term.getMasterScheduleOid());
                    row.put(SCHEDULE_TERM_KEY, term.getScheduleTermOid());

                    MasterSchedule masterSchedule = term.getMasterSchedule();
                    if (masterSchedule != null) {
                        Schedule schedule = masterSchedule.getSchedule();
                        if (schedule != null) {
                            row.put(SCHOOL_KEY, schedule.getSchoolOid());
                        }
                    }
                }
                ScheduleMatrix scheduleMatrix = matrix.getScheduleMatrix();
                if (scheduleMatrix != null) {
                    row.put(SCHEDULE_PERIOD_KEY, scheduleMatrix.getSchedulePeriodOid());
                    row.put(SCHEDULE_DAY_KEY, scheduleMatrix.getScheduleDayOid());
                }

                results.add(row);

            }
        } finally {
            masterScheduleMatrixIterator.close();
        }


        X2Criteria masterScheduleCriteria = new X2Criteria();
        masterScheduleCriteria.addAndCriteria(mstCriteria);
        masterScheduleCriteria.addIsNull(MasterSchedule.COL_SCHEDULE_DISPLAY);

        BeanQuery masterScheduleQuery = new BeanQuery(MasterSchedule.class, masterScheduleCriteria);
        QueryIterator masterScheduleIterator = getBroker().getIteratorByQuery(masterScheduleQuery);
        try {
            while (masterScheduleIterator.hasNext()) {
                Map<String, String> row = new HashMap<>();
                MasterSchedule masterSchedule = (MasterSchedule) masterScheduleIterator.next();
                row.put(MASTER_SCHEDULE_KEY, masterSchedule.getOid());
                row.put(SCHEDULE_TERM_KEY, masterSchedule.getScheduleTermOid());
                Schedule schedule = masterSchedule.getSchedule();
                if (schedule != null) {
                    row.put(SCHOOL_KEY, schedule.getSchoolOid());
                }
                results.add(row);
            }
        } finally {
            masterScheduleIterator.close();
        }

        for (Map<String, String> row : results) {
            String mstOid = row.get(MASTER_SCHEDULE_KEY);
            String trmOid = row.get(SCHEDULE_TERM_KEY);
            String schoolOid = row.get(SCHOOL_KEY);
            String perOid = row.get(SCHEDULE_PERIOD_KEY);
            String dayOid = row.get(SCHEDULE_DAY_KEY);

            Integer dayNumber = getDayNumber(dayOid);

            ScheduleTerm term = getScheduleTerm(trmOid);
            String termCode = term == null ? EMPTY_STRING : term.getCode();
            String dayNumAsString = dayNumber == null ? ALL_DAYS_KEY : dayNumber.toString();
            String membershipSessionsKey = getMembershipSessionsKey(schoolOid, termCode, dayNumAsString);
            Map<String, Set<PlainDate>> mapCalendarCodeDays = new HashMap<String, Set<PlainDate>>();
            if (m_membershipSessions.get(membershipSessionsKey) != null) {
                mapCalendarCodeDays = m_membershipSessions.get(membershipSessionsKey);
            }
            if (mapCalendarCodeDays != null) {
                for (String calendarCode : mapCalendarCodeDays.keySet()) {
                    Set<PlainDate> newPlainDates = mapCalendarCodeDays.get(calendarCode);

                    String scheduleDaysKey = getScheduleDaysKey(mstOid, calendarCode);
                    Set<ScheduleDayPeriod> oldPlainDates = m_scheduleDaysMap.get(scheduleDaysKey);
                    if (oldPlainDates == null) {
                        oldPlainDates = new HashSet<ScheduleDayPeriod>();
                        m_scheduleDaysMap.put(scheduleDaysKey, oldPlainDates);
                    }
                    for (PlainDate date : newPlainDates) {
                        oldPlainDates.add(new ScheduleDayPeriod(date, perOid));
                    }
                }
            }
        }
    }

    /**
     * Load a map of sets of teacher schedule records by master section oid for active schedules.
     */
    private void loadScheduleTeachers() {
        X2Criteria teacherScheduleCriteria = new X2Criteria();
        if (isSchoolContext()) {
            teacherScheduleCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getSchool().getOid());
        }

        teacherScheduleCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, "Class");

        teacherScheduleCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                m_fieldExcludeCrs, BooleanAsStringConverter.TRUE);

        // From appropriate active Schedule
        if (getOrganization().getCurrentContextOid().equals(m_yearContextOid)) {
            // Current year active schedule, identified by School.REL_ACTIVE_SCHOOL_SCHED
            teacherScheduleCriteria.addEqualToField(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                    SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                    ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                            MasterSchedule.COL_SCHEDULE_OID);
        } else {
            // Other year active schedule, identified by SchoolScheduleContext.REL_DISTRICT_CONTEXT
            teacherScheduleCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                    SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                    m_yearContextOid);
        }
        QueryByCriteria query = new QueryByCriteria(ScheduleTeacher.class, teacherScheduleCriteria);
        m_teacherMap = getBroker().getGroupedCollectionByQuery(query, ScheduleTeacher.COL_SECTION_OID, 500);
    }

    /**
     * Loads a map of the selected school years schedule terms.
     */
    private void loadScheduleTerms() {
        X2Criteria scheduleTermCriteria = new X2Criteria();

        if (isSchoolContext()) {
            scheduleTermCriteria.addEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getSchool().getOid());
        }
        if (getOrganization().getCurrentContextOid().equals(m_yearContextOid)) {
            // Current year active schedule, identified by School.REL_ACTIVE_SCHOOL_SCHED
            scheduleTermCriteria.addEqualToField(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                    SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                    ScheduleTerm.COL_SCHEDULE_OID);
        } else {
            // Other year active schedule, identified by SchoolScheduleContext.REL_DISTRICT_CONTEXT
            scheduleTermCriteria.addEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                    SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                    m_yearContextOid);
        }
        QueryByCriteria query = new QueryByCriteria(ScheduleTerm.class, scheduleTermCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        m_termDates = new HashMap<String, Object[]>();
        try {
            while (iterator.hasNext()) {
                ScheduleTerm scheduleTerm = (ScheduleTerm) iterator.next();
                String termCode = scheduleTerm.getCode();
                String schoolOid = scheduleTerm.getSchedule().getSchoolOid();

                if (!StringUtils.isEmpty(termCode) && !StringUtils.isEmpty(schoolOid) &&
                        !m_termDates.containsKey(schoolOid + termCode)) {
                    Collection<ScheduleTermDate> scheduleTermDates = scheduleTerm.getScheduleTermDates(getBroker());
                    List<PlainDate> startDates = new ArrayList<PlainDate>();
                    List<PlainDate> endDates = new ArrayList<PlainDate>();
                    PlainDate startDate = null;
                    PlainDate endDate = null;

                    if (scheduleTermDates != null) {
                        for (ScheduleTermDate scheduleTermDate : scheduleTermDates) {
                            startDates.add(scheduleTermDate.getStartDate());
                            endDates.add(scheduleTermDate.getEndDate());
                            if (startDate == null) {
                                startDate = scheduleTermDate.getStartDate();
                            } else {
                                PlainDate newValue = scheduleTermDate.getStartDate();
                                startDate = (newValue != null && newValue.before(startDate)) ? newValue : startDate;
                            }
                            if (endDate == null) {
                                endDate = scheduleTermDate.getEndDate();
                            } else {
                                PlainDate newValue = scheduleTermDate.getEndDate();
                                endDate = (newValue != null && newValue.after(endDate)) ? newValue : endDate;
                            }
                        }
                    }

                    Object[] termDates = new Object[4];
                    termDates[SCHEDULE_TERM_START_DATE_POSITION] = startDate;
                    termDates[SCHEDULE_TERM_END_DATE_POSITION] = endDate;
                    termDates[SCHEDULE_TERM_START_DATES_POSITION] = startDates;
                    termDates[SCHEDULE_TERM_END_DATES_POSITION] = endDates;

                    if (startDate != null && endDate != null && !StringUtils.isEmpty(schoolOid)) {
                        m_termDates.put(schoolOid + termCode, termDates);
                    }
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads a Map of student absent dates keyed on student and section.
     */
    private void loadStudentClassAttendance() {
        m_studentClassAbsences = new HashMap<String, Map<PlainDate, Integer>>();

        /*
         * Step 1: populate according to StudentPeriodAttendance
         */

        X2Criteria studentClassAttendanceCriteria = new X2Criteria();

        addMasterScheduleCriteria(studentClassAttendanceCriteria,
                StudentPeriodAttendance.REL_MASTER_SCHEDULE + PATH_DELIMITER);

        studentClassAttendanceCriteria.addEqualTo(StudentPeriodAttendance.COL_ABSENT_INDICATOR,
                BooleanAsStringConverter.TRUE);

        if (isSchoolContext()) {
            studentClassAttendanceCriteria.addEqualTo(
                    StudentPeriodAttendance.REL_STUDENT + PATH_DELIMITER + Student.COL_SCHOOL_OID,
                    getSchool().getOid());
        }
        studentClassAttendanceCriteria.addNotEqualTo(StudentPeriodAttendance.REL_STUDENT + PATH_DELIMITER +
                m_fieldExcludeStd,
                BooleanAsStringConverter.TRUE);

        studentClassAttendanceCriteria.addEqualTo(
                StudentPeriodAttendance.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_fieldSklProcessClassAttendance,
                BooleanAsStringConverter.TRUE);

        applyInputCriteria(studentClassAttendanceCriteria, false, StudentPeriodAttendance.REL_STUDENT);

        QueryByCriteria classAttendanceQuery =
                new QueryByCriteria(StudentPeriodAttendance.class, studentClassAttendanceCriteria);
        classAttendanceQuery
                .addOrderByDescending(StudentPeriodAttendance.REL_STUDENT + PATH_DELIMITER + X2BaseBean.COL_OID);
        classAttendanceQuery.addOrderByDescending(StudentPeriodAttendance.COL_MASTER_SCHEDULE_OID);
        classAttendanceQuery.addOrderByDescending(StudentPeriodAttendance.COL_DATE);

        QueryIterator classAttendanceIterator = getBroker().getIteratorByQuery(classAttendanceQuery);

        try {
            while (classAttendanceIterator.hasNext()) {
                StudentPeriodAttendance attendance = (StudentPeriodAttendance) classAttendanceIterator.next();
                if (attendance.getAbsentIndicator()) {
                    String studentOid = attendance.getStudentOid();
                    String mstOid = attendance.getMasterScheduleOid();
                    String classAttendanceKey = getClassAttendanceKey(studentOid, mstOid);
                    Map<PlainDate, Integer> dateCounts = m_studentClassAbsences.get(classAttendanceKey);
                    if (dateCounts == null) {
                        dateCounts = new HashMap();
                        m_studentClassAbsences.put(classAttendanceKey, dateCounts);
                    }
                    Integer count = dateCounts.get(attendance.getDate());
                    if (count == null) {
                        dateCounts.put(attendance.getDate(), INTEGER_ONE);
                    } else {
                        dateCounts.put(attendance.getDate(), Integer.valueOf(count.intValue() + 1));
                    }
                }
            }
        } finally {
            classAttendanceIterator.close();
        }
    }

    /**
     * Load a map of sets of student schedule records by student oid for the students in the export.
     */
    private void loadStudentSchedules() {
        m_studentScheduleCriteria = m_helper.getStudentScheduleCriteria();

        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, m_studentScheduleCriteria);
        m_scheduleMap = getBroker().getGroupedCollectionByQuery(query, StudentSchedule.COL_STUDENT_OID, 500);
    }

    /**
     * Loads a map of sets of transcript records by studentOid for students in the export.
     */
    private void loadTranscripts() {
        m_studentTranscriptCriteria = new X2Criteria();
        m_studentTranscriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, m_yearContextOid);
        if (isSchoolContext()) {
            m_studentTranscriptCriteria.addEqualTo(Transcript.COL_SCHOOL_OID, getSchool().getOid());
        }
        m_studentTranscriptCriteria.addNotEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER +
                m_fieldExcludeStd,
                BooleanAsStringConverter.TRUE);

        applyInputCriteria(m_studentTranscriptCriteria, false, Transcript.REL_STUDENT);

        m_studentTranscriptCriteria.addNotEqualTo(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                m_fieldExcludeCrs, BooleanAsStringConverter.TRUE);

        m_studentTranscriptCriteria.addNotEmpty(Transcript.COL_MASTER_SCHEDULE_OID, getBroker().getPersistenceKey());

        QueryByCriteria query = new QueryByCriteria(Transcript.class, m_studentTranscriptCriteria);
        m_transcriptMap = getBroker().getGroupedCollectionByQuery(query, Transcript.COL_STUDENT_OID, 500);
    }

}
