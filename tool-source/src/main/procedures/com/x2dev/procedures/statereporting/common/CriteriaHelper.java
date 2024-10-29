/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2022 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.common;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.ParameterSelectionHandler;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria.ScheduleType;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanColumn;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.List;

/**
 * The Class CriteriaHelper.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class CriteriaHelper {

    /**
     * The Class EnrollmentSpanCriteria.
     */
    public static class EnrollmentSpanCriteria {

        /**
         * The Enum ScheduleType.
         */
        enum ScheduleType {
            SPANS, ACTIVE
        }

        /*
         * current district context
         */
        private DistrictSchoolYearContext m_context;

        /*
         * end date for date range
         */
        private PlainDate m_endDate;

        /*
         * indicator to exclude future scchedules
         */
        private boolean m_excludeFutureSched = false;

        /*
         * schedule type used to select schedule based student criteria
         */
        private ScheduleType m_scheduleType = ScheduleType.SPANS;

        /*
         * exclude course column defined at section
         */
        private ToolBeanColumn m_excludeCourse = null;

        /*
         * exclude course column defined at transcript
         */
        private ToolBeanColumn m_excludeCourseTrn = null;

        /*
         * exclude section column
         */
        private ToolBeanColumn m_excludeSection = null;

        /*
         * exclude student column
         */
        private ToolBeanColumn m_excludeStudent = null;

        /*
         * Include/exclude secondary (SSK) spans
         */
        private boolean m_includeSecondarySpans = true;

        /*
         * Include/exclude secondary (SSK) spans
         */
        private boolean m_includeAllSecondarySpans = false;

        /*
         * Limit to specific student OID(s)
         */
        private List<String> m_limitingStudentOids = null;

        /*
         * Filter to a School
         */
        private List<String> m_schoolOids = null;

        /*
         * Database-level section limiting
         */
        private X2Criteria m_sectionLimitingCriteria = null;

        /*
         * Database-level student limiting
         */
        private X2Criteria m_studentLimitingCriteria = null;

        /**
         * Gets the current context.
         *
         * @return District school year context
         */
        public DistrictSchoolYearContext getCurrentContext() {
            return m_context;
        }

        /**
         * Gets the end date.
         *
         * @return Plain date
         */
        public PlainDate getEndDate() {
            if (m_endDate == null) {
                m_endDate = getCurrentContext().getEndDate();
            }
            return m_endDate;
        }

        /**
         * Gets the exclude course.
         *
         * @return Tool bean column
         */
        public ToolBeanColumn getExcludeCourse() {
            return m_excludeCourse;
        }

        /**
         * Gets the exclude course trn.
         *
         * @return Tool bean column
         */
        public ToolBeanColumn getExcludeCourseTrn() {
            return m_excludeCourseTrn;
        }

        /**
         * Gets the exclude section.
         *
         * @return Tool bean column
         */
        public ToolBeanColumn getExcludeSection() {
            return m_excludeSection;
        }

        /**
         * Gets the exclude student.
         *
         * @return the excludeStudent
         */
        public ToolBeanColumn getExcludeStudent() {
            return m_excludeStudent;
        }

        /**
         * Gets the limiting student oids.
         *
         * @return the limitingStudentOids
         */
        public List<String> getLimitingStudentOids() {
            return m_limitingStudentOids;
        }

        /**
         * Gets the schedule type.
         *
         * @return Schedule type
         */
        public ScheduleType getScheduleType() {
            return m_scheduleType;
        }

        /**
         * Gets the school oids.
         *
         * @return the schoolOids
         */
        public List<String> getSchoolOids() {
            return m_schoolOids;
        }

        /**
         * Gets the student limiting criteria.
         *
         * @return the studentLimitingCriteria
         */
        public X2Criteria getSectionLimitingCriteria() {
            return m_sectionLimitingCriteria;
        }

        /**
         * Gets the student limiting criteria.
         *
         * @return the studentLimitingCriteria
         */
        public X2Criteria getStudentLimitingCriteria() {
            return m_studentLimitingCriteria;
        }

        /**
         * Checks if is exclude future sched.
         *
         * @return the m_excludeFutureSched
         */
        public boolean isExcludeFutureSched() {
            return m_excludeFutureSched;
        }

        /**
         * Checks if is include secondary spans.
         *
         * @return the includeSecondarySpans
         */
        public boolean isIncludeAllSecondarySpans() {
            return m_includeAllSecondarySpans;
        }

        /**
         * Checks if is include secondary spans.
         *
         * @return the includeSecondarySpans
         */
        public boolean isIncludeSecondarySpans() {
            return m_includeSecondarySpans;
        }

        /**
         * Sets the current context.
         *
         * @param context DistrictSchoolYearContext
         * @return EnrollmentSpanCriteria
         */
        public EnrollmentSpanCriteria setCurrentContext(DistrictSchoolYearContext context) {
            m_context = context;
            return this;
        }

        /**
         * Sets the end date.
         *
         * @param endDate PlainDate
         * @return EnrollmentSpanCriteria
         */
        public EnrollmentSpanCriteria setEndDate(PlainDate endDate) {
            m_endDate = endDate;
            if (getCurrentContext() != null && getCurrentContext().getEndDate().before(endDate)) {
                m_endDate = getCurrentContext().getEndDate();
            }
            return this;
        }

        /**
         * Sets the exclude course.
         *
         * @param excludeCourse ToolBeanColumn
         * @return EnrollmentSpanCriteria
         */
        public EnrollmentSpanCriteria setExcludeCourse(ToolBeanColumn excludeCourse) {
            m_excludeCourse = excludeCourse;
            return this;
        }

        /**
         * Sets the exclude course trn.
         *
         * @param excludeCourse ToolBeanColumn
         * @return EnrollmentSpanCriteria
         */
        public EnrollmentSpanCriteria setExcludeCourseTrn(ToolBeanColumn excludeCourse) {
            m_excludeCourseTrn = excludeCourse;
            return this;
        }

        /**
         * Sets the exclude future sched.
         *
         * @param excludeFutureSched boolean
         * @return EnrollmentSpanCriteria
         */
        public EnrollmentSpanCriteria setExcludeFutureSched(boolean excludeFutureSched) {
            m_excludeFutureSched = excludeFutureSched;
            return this;
        }

        /**
         * Sets the exclude section.
         *
         * @param excludeSection ToolBeanColumn
         * @return EnrollmentSpanCriteria
         */
        public EnrollmentSpanCriteria setExcludeSection(ToolBeanColumn excludeSection) {
            m_excludeSection = excludeSection;
            return this;
        }

        /**
         * Sets the exclude student.
         *
         * @param excludeStudent ToolBeanColumn
         * @return EnrollmentSpanCriteria
         */
        public EnrollmentSpanCriteria setExcludeStudent(ToolBeanColumn excludeStudent) {
            m_excludeStudent = excludeStudent;
            return this;
        }

        /**
         * Sets the include secondary spans.
         *
         * @param includeSecondarySpans boolean
         * @return EnrollmentSpanCriteria
         */
        public EnrollmentSpanCriteria setIncludeAllSecondarySpans(boolean includeAllSecondarySpans) {
            m_includeAllSecondarySpans = includeAllSecondarySpans;
            return this;
        }

        /**
         * Sets the include secondary spans.
         *
         * @param includeSecondarySpans boolean
         * @return EnrollmentSpanCriteria
         */
        public EnrollmentSpanCriteria setIncludeSecondarySpans(boolean includeSecondarySpans) {
            m_includeSecondarySpans = includeSecondarySpans;
            return this;
        }

        /**
         * Sets the limiting student oids.
         *
         * @param limitingStudentOids List<String>
         * @return EnrollmentSpanCriteria
         */
        public EnrollmentSpanCriteria setLimitingStudentOids(List<String> limitingStudentOids) {
            m_limitingStudentOids = limitingStudentOids;
            return this;
        }

        /**
         * Sets the school oids.
         *
         * @param schoolOids List<String>
         * @return EnrollmentSpanCriteria
         */
        public EnrollmentSpanCriteria setSchoolOids(List<String> schoolOids) {
            m_schoolOids = schoolOids;
            return this;
        }

        /**
         * Sets the student limiting criteria.
         *
         * @param studentLimitingCriteria X2Criteria
         * @return EnrollmentSpanCriteria
         */
        public EnrollmentSpanCriteria setSectionLimitingCriteria(X2Criteria sectionLimitingCriteria) {
            m_sectionLimitingCriteria = sectionLimitingCriteria;
            return this;
        }

        /**
         * Sets the student limiting criteria.
         *
         * @param studentLimitingCriteria X2Criteria
         * @return EnrollmentSpanCriteria
         */
        public EnrollmentSpanCriteria setStudentLimitingCriteria(X2Criteria studentLimitingCriteria) {
            m_studentLimitingCriteria = studentLimitingCriteria;
            return this;
        }
    }

    /**
     * Builds the student schedule change criteria.
     *
     * @param spanCriteria EnrollmentSpanCriteria
     * @return X2Criteria
     */
    public static X2Criteria buildStudentScheduleChangeCriteria(EnrollmentSpanCriteria spanCriteria) {
        X2Criteria criteria = new X2Criteria();

        SisBeanPaths.STUDENT_SCHEDULE_CHANGE.schedule().school().archiveIndicator().getPath();
        // Master type Class
        criteria.addEqualTo(
                SisBeanPaths.STUDENT_SCHEDULE_CHANGE.masterSchedule().schoolCourse().masterType().getPath(),
                SchoolCourse.MASTER_TYPE_CLASS);

        // From active Schedule for the selected year.
        criteria.addEqualTo(
                SisBeanPaths.STUDENT_SCHEDULE_CHANGE.schedule().activeSchoolScheduleContexts().districtContextOid()
                        .getPath(),
                spanCriteria.getCurrentContext().getOid());

        // Require section term to start before end/report date.
        if (spanCriteria.isExcludeFutureSched()) {
            criteria.addLessOrEqualThan(
                    SisBeanPaths.STUDENT_SCHEDULE_CHANGE.masterSchedule().scheduleTerm().scheduleTermDates().startDate()
                            .getPath(),
                    spanCriteria.getEndDate());
        }

        // check school or organization selection.
        if (spanCriteria.getSchoolOids() != null) {
            criteria.addIn(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.schedule().schoolOid().getPath(),
                    spanCriteria.getSchoolOids());
        } else {
            criteria.addNotEqualTo(
                    SisBeanPaths.STUDENT_SCHEDULE_CHANGE.schedule().school().inactiveIndicator().getPath(),
                    Boolean.TRUE);
            criteria.addNotEqualTo(
                    SisBeanPaths.STUDENT_SCHEDULE_CHANGE.schedule().school().archiveIndicator().getPath(),
                    Boolean.TRUE);
        }

        // Check exclusion flags for student, section and student schedule.
        if (spanCriteria.getExcludeStudent() != null
                && !StringUtils.isEmpty(spanCriteria.getExcludeStudent().resolve(null))) {
            criteria.addNotEqualTo(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.student().getPath() + PATH_DELIMITER
                    + spanCriteria.getExcludeStudent().resolve(null), BooleanAsStringConverter.TRUE);
        }

        // Check if the section exclusion custom field is present.
        if (spanCriteria.getExcludeSection() != null
                && !StringUtils.isEmpty(spanCriteria.getExcludeSection().resolve(null))) {
            criteria.addNotEqualTo(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.masterSchedule().getPath() + PATH_DELIMITER
                    + spanCriteria.getExcludeSection().resolve(null), BooleanAsStringConverter.TRUE);
        }

        // Check if the course exclusion custom field is present.
        if (spanCriteria.getExcludeCourse() != null
                && !StringUtils.isEmpty(spanCriteria.getExcludeCourse().resolve(null))) {
            criteria.addNotEqualTo(
                    SisBeanPaths.STUDENT_SCHEDULE_CHANGE.masterSchedule().getPath() + PATH_DELIMITER
                            + spanCriteria.getExcludeCourse().resolve(null),
                    BooleanAsStringConverter.TRUE);
        }

        /*
         * Custom caller criteria
         */
        if (spanCriteria.getStudentLimitingCriteria() != null
                && !spanCriteria.getStudentLimitingCriteria().isEmpty()) {
            criteria.addIn(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.studentOid().getPath(),
                    new SubQuery(SisBeanPaths.STUDENT.getBeanType(), SisBeanPaths.STUDENT.oid().getPath(),
                            spanCriteria.getStudentLimitingCriteria()));
        }
        if (spanCriteria.getSectionLimitingCriteria() != null
                && !spanCriteria.getSectionLimitingCriteria().isEmpty()) {
            criteria.addIn(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.masterScheduleOid().getPath(),
                    new SubQuery(SisBeanPaths.SCHEDULE_MASTER.getBeanType(),
                            SisBeanPaths.SCHEDULE_MASTER.oid().getPath(),
                            spanCriteria.getSectionLimitingCriteria()));
        }
        return criteria;
    }

    /**
     * Builds the student schedule criteria.
     *
     * @param spanCriteria EnrollmentSpanCriteria
     * @return X2Criteria
     */
    public static X2Criteria buildStudentScheduleCriteria(EnrollmentSpanCriteria spanCriteria) {
        X2Criteria criteria = new X2Criteria();

        SisBeanPaths.STUDENT_SCHEDULE.schedule().school().archiveIndicator().getPath();
        // Master type Class
        criteria.addEqualTo(
                SisBeanPaths.STUDENT_SCHEDULE.section().schoolCourse().masterType().getPath(),
                SchoolCourse.MASTER_TYPE_CLASS);

        // From active Schedule for the selected year.
        criteria.addEqualTo(
                SisBeanPaths.STUDENT_SCHEDULE.schedule().activeSchoolScheduleContexts().districtContextOid().getPath(),
                spanCriteria.getCurrentContext().getOid());

        // Require section term to start before end/report date.
        if (spanCriteria.isExcludeFutureSched()) {
            criteria.addLessOrEqualThan(
                    SisBeanPaths.STUDENT_SCHEDULE.section().scheduleTerm().scheduleTermDates().startDate().getPath(),
                    spanCriteria.getEndDate());
        }

        // check school or organization selection.
        if (spanCriteria.getSchoolOids() != null) {
            criteria.addIn(SisBeanPaths.STUDENT_SCHEDULE.schedule().schoolOid().getPath(),
                    spanCriteria.getSchoolOids());
        } else {
            criteria.addNotEqualTo(
                    SisBeanPaths.STUDENT_SCHEDULE.schedule().school().inactiveIndicator().getPath(), Boolean.TRUE);
            criteria.addNotEqualTo(
                    SisBeanPaths.STUDENT_SCHEDULE.schedule().school().archiveIndicator().getPath(), Boolean.TRUE);
        }

        // Check exclusion flags for student, section and student schedule.
        if (spanCriteria.getExcludeStudent() != null
                && !StringUtils.isEmpty(spanCriteria.getExcludeStudent().resolve(null))) {
            criteria.addNotEqualTo(SisBeanPaths.STUDENT_SCHEDULE.student().getPath() + PATH_DELIMITER
                    + spanCriteria.getExcludeStudent().resolve(null), BooleanAsStringConverter.TRUE);
        }

        // Check if the section exclusion custom field is present.
        if (spanCriteria.getExcludeSection() != null
                && !StringUtils.isEmpty(spanCriteria.getExcludeSection().resolve(null))) {
            criteria.addNotEqualTo(SisBeanPaths.STUDENT_SCHEDULE.section().getPath() + PATH_DELIMITER
                    + spanCriteria.getExcludeSection().resolve(null), BooleanAsStringConverter.TRUE);
        }

        // Check if the course exclusion custom field is present.
        if (spanCriteria.getExcludeCourse() != null
                && !StringUtils.isEmpty(spanCriteria.getExcludeCourse().resolve(null))) {
            criteria.addNotEqualTo(SisBeanPaths.STUDENT_SCHEDULE.section().getPath() + PATH_DELIMITER
                    + spanCriteria.getExcludeCourse().resolve(null),
                    BooleanAsStringConverter.TRUE);
        }

        /*
         * Custom caller criteria
         */
        if (spanCriteria.getStudentLimitingCriteria() != null
                && !spanCriteria.getStudentLimitingCriteria().isEmpty()) {
            criteria.addIn(SisBeanPaths.STUDENT_SCHEDULE.studentOid().getPath(),
                    new SubQuery(SisBeanPaths.STUDENT.getBeanType(), SisBeanPaths.STUDENT.oid().getPath(),
                            spanCriteria.getStudentLimitingCriteria()));
        }
        if (spanCriteria.getSectionLimitingCriteria() != null
                && !spanCriteria.getSectionLimitingCriteria().isEmpty()) {
            criteria.addIn(SisBeanPaths.STUDENT_SCHEDULE.sectionOid().getPath(),
                    new SubQuery(SisBeanPaths.SCHEDULE_MASTER.getBeanType(),
                            SisBeanPaths.SCHEDULE_MASTER.oid().getPath(),
                            spanCriteria.getSectionLimitingCriteria()));
        }
        return criteria;
    }

    /**
     * Builds the student transcript criteria.
     *
     * @param spanCriteria EnrollmentSpanCriteria
     * @param broker X2Broker
     * @return X2Criteria
     */
    public static X2Criteria buildStudentTranscriptCriteria(EnrollmentSpanCriteria spanCriteria, X2Broker broker) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SisBeanPaths.STUDENT_TRANSCRIPT.districtContextOid().getPath(),
                spanCriteria.getCurrentContext().getOid());
        criteria.addNotEmpty(SisBeanPaths.STUDENT_TRANSCRIPT.masterScheduleOid().getPath(),
                broker.getPersistenceKey());

        // check school or organization selection.
        if (spanCriteria.getSchoolOids() != null) {
            criteria.addIn(SisBeanPaths.STUDENT_TRANSCRIPT.schoolOid().getPath(),
                    spanCriteria.getSchoolOids());
        } else {
            criteria.addNotEqualTo(
                    SisBeanPaths.STUDENT_TRANSCRIPT.school().inactiveIndicator().getPath(), Boolean.TRUE);
            criteria.addNotEqualTo(
                    SisBeanPaths.STUDENT_TRANSCRIPT.school().archiveIndicator().getPath(), Boolean.TRUE);
        }

        // Check exclusion flags for student, section and student schedule.
        if (spanCriteria.getExcludeStudent() != null
                && !StringUtils.isEmpty(spanCriteria.getExcludeStudent().resolve(null))) {
            criteria.addNotEqualTo(SisBeanPaths.STUDENT_TRANSCRIPT.student().getPath() + PATH_DELIMITER
                    + spanCriteria.getExcludeStudent().resolve(null), BooleanAsStringConverter.TRUE);
        }

        // Check if the course exclusion custom field is present.
        if (spanCriteria.getExcludeCourseTrn() != null
                && !StringUtils.isEmpty(spanCriteria.getExcludeCourseTrn().resolve(null))) {
            criteria.addNotEqualTo(
                    SisBeanPaths.STUDENT_TRANSCRIPT.getPath() + PATH_DELIMITER
                            + spanCriteria.getExcludeCourse().resolve(null),
                    BooleanAsStringConverter.TRUE);
        }

        /*
         * Custom caller criteria
         */
        if (spanCriteria.getStudentLimitingCriteria() != null
                && !spanCriteria.getStudentLimitingCriteria().isEmpty()) {
            criteria.addIn(SisBeanPaths.STUDENT_TRANSCRIPT.studentOid().getPath(),
                    new SubQuery(SisBeanPaths.STUDENT.getBeanType(), SisBeanPaths.STUDENT.oid().getPath(),
                            spanCriteria.getStudentLimitingCriteria()));
        }

        return criteria;
    }

    /**
     * Gets the student candidate criteria.
     *
     * @param spanCriteria EnrollmentSpanCriteria
     * @param broker X2Broker
     * @return X 2 criteria
     */
    static public X2Criteria getStudentCandidateCriteria(EnrollmentSpanCriteria spanCriteria, X2Broker broker) {
        X2Criteria studentCriteria = new X2Criteria();

        /*
         * Filter to specific students
         */
        if (spanCriteria.getLimitingStudentOids() != null) {
            ParameterSelectionHandler.addParameterSafeOIDList(studentCriteria, broker,
                    spanCriteria.getLimitingStudentOids(),
                    0, SisBeanPaths.STUDENT.oid().getPath());
        }

        /*
         * Filter by enrollment activity (for specific students/schools/cutoff date)
         */
        X2Criteria enrSubQueryCriteria = buildStudentSubQueryCriteriaForEnrollments(spanCriteria, broker);
        SubQuery enrollmentSubQuery =
                new SubQuery(SisBeanPaths.STUDENT_ENROLLMENT.getBeanType(),
                        SisBeanPaths.STUDENT_ENROLLMENT.studentOid().getPath(), enrSubQueryCriteria);
        X2Criteria enrCriteria = new X2Criteria();
        enrCriteria.addIn(SisBeanPaths.STUDENT.oid().getPath(), enrollmentSubQuery);

        /*
         * Filter by current enrollment and school status
         */
        X2Criteria orCriteria = new X2Criteria();
        X2Criteria activeCriteria = buildStudentCriteriaForActive(spanCriteria, broker);

        /*
         * join the enr, active and ssk criteria in an OR.
         */
        orCriteria.addOrCriteria(enrCriteria);
        orCriteria.addOrCriteria(activeCriteria);

        // Find students who are secondary
        if (spanCriteria.isIncludeSecondarySpans()) {
            X2Criteria sskSubQueryCriteria = buildStudentCriteriaForSecondary(spanCriteria, broker);
            SubQuery secondaryQuery =
                    new SubQuery(StudentSchool.class, StudentSchool.COL_STUDENT_OID, sskSubQueryCriteria);
            X2Criteria sskCriteria = new X2Criteria();
            sskCriteria.addIn(X2BaseBean.COL_OID, secondaryQuery);
            orCriteria.addOrCriteria(sskCriteria);
        }

        // Build the final student criteria, including user criteria and exclude criteria.
        studentCriteria.addAndCriteria(orCriteria);

        /*
         * Custom caller criteria
         */
        if (spanCriteria.getStudentLimitingCriteria() != null
                && !spanCriteria.getStudentLimitingCriteria().isEmpty()) {
            studentCriteria.addAndCriteria(spanCriteria.getStudentLimitingCriteria());
        }

        // Apply exclude criteria.
        if (spanCriteria.getExcludeStudent() != null) {
            studentCriteria.addNotEqualTo(spanCriteria.getExcludeStudent().resolve(null),
                    BooleanAsStringConverter.TRUE);
        }

        return studentCriteria;
    }

    /**
     * Gets the student schedule criteria.
     *
     * @param spanCriteria EnrollmentSpanCriteria
     * @param broker X2Broker
     * @return X 2 criteria
     */
    static public X2Criteria getStudentScheduleCriteria(EnrollmentSpanCriteria spanCriteria, X2Broker broker) {
        X2Criteria criteria = new X2Criteria();
        X2Criteria scheduleCriteria = buildStudentScheduleCriteria(spanCriteria);
        SubQuery scheduleSubquery = new SubQuery(SisBeanPaths.STUDENT_SCHEDULE.getBeanType(),
                SisBeanPaths.STUDENT_SCHEDULE.studentOid().getPath(), scheduleCriteria);
        X2Criteria sub1Criteria = new X2Criteria();
        sub1Criteria.addIn(X2BaseBean.COL_OID, scheduleSubquery);

        // Include schedule changes for schedule spans.
        if (spanCriteria.getScheduleType() == ScheduleType.SPANS) {
            X2Criteria scheduleChangeCriteria = buildStudentScheduleChangeCriteria(spanCriteria);
            SubQuery scheduleChangeSubquery = new SubQuery(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.getBeanType(),
                    SisBeanPaths.STUDENT_SCHEDULE_CHANGE.studentOid().getPath(), scheduleChangeCriteria);
            X2Criteria sub2Criteria = new X2Criteria();
            sub2Criteria.addIn(X2BaseBean.COL_OID, scheduleChangeSubquery);
            sub1Criteria.addOrCriteria(sub2Criteria);
        }
        // Include transcripts for active schedule mode.
        if (spanCriteria.getScheduleType() == ScheduleType.ACTIVE) {
            X2Criteria transcriptCriteria = buildStudentTranscriptCriteria(spanCriteria, broker);
            SubQuery transcriptSubquery = new SubQuery(SisBeanPaths.STUDENT_TRANSCRIPT.getBeanType(),
                    SisBeanPaths.STUDENT_TRANSCRIPT.studentOid().getPath(), transcriptCriteria);
            X2Criteria sub2Criteria = new X2Criteria();
            sub2Criteria.addIn(X2BaseBean.COL_OID, transcriptSubquery);
            sub1Criteria.addOrCriteria(sub2Criteria);
        }

        criteria.addAndCriteria(sub1Criteria);

        // Apply exclude criteria.
        if (spanCriteria.getExcludeStudent() != null) {
            criteria.addNotEqualTo(spanCriteria.getExcludeStudent().resolve(null),
                    BooleanAsStringConverter.TRUE);
        }

        return criteria;

    }

    /**
     * Builds the student criteria for active.
     *
     * @param spanCriteria EnrollmentSpanCriteria
     * @param broker X2Broker
     * @return X2Criteria
     */
    private static X2Criteria buildStudentCriteriaForActive(EnrollmentSpanCriteria spanCriteria, X2Broker broker) {
        X2Criteria activeCriteria = new X2Criteria();

        /*
         * Filter by specific students
         */
        if (spanCriteria.getLimitingStudentOids() != null) {
            ParameterSelectionHandler.addParameterSafeOIDList(activeCriteria, broker,
                    spanCriteria.getLimitingStudentOids(),
                    0, SisBeanPaths.STUDENT.oid().getPath());
        }

        /*
         * Filter by enrollment status
         */
        activeCriteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(ToolBean.DistrictManager.getOrganization(broker),
                        SisBeanPaths.STUDENT.enrollmentStatus().getPath()));

        /*
         * Filter by school
         */
        if (spanCriteria.getSchoolOids() != null) {
            activeCriteria.addIn(SisBeanPaths.STUDENT.schoolOid().getPath(), spanCriteria.getSchoolOids());
        } else {
            activeCriteria.addNotEqualTo(SisBeanPaths.STUDENT.school().inactiveIndicator().getPath(), Boolean.TRUE);
            activeCriteria.addNotEqualTo(SisBeanPaths.STUDENT.school().archiveIndicator().getPath(), Boolean.TRUE);
        }

        /*
         * Custom caller criteria
         */
        if (spanCriteria.getStudentLimitingCriteria() != null
                && !spanCriteria.getStudentLimitingCriteria().isEmpty()) {
            activeCriteria.addAndCriteria(spanCriteria.getStudentLimitingCriteria());
        }

        return activeCriteria;
    }

    /**
     * Builds the student criteria for secondary.
     *
     * @param spanCriteria EnrollmentSpanCriteria
     * @param broker X2Broker
     * @return X2Criteria
     */
    private static X2Criteria buildStudentCriteriaForSecondary(EnrollmentSpanCriteria spanCriteria, X2Broker broker) {
        X2Criteria secondaryCriteria = new X2Criteria();

        /*
         * Filter by specific students
         */
        if (spanCriteria.getLimitingStudentOids() != null) {
            ParameterSelectionHandler.addParameterSafeOIDList(secondaryCriteria, broker,
                    spanCriteria.getLimitingStudentOids(), 0, SisBeanPaths.STUDENT_SCHOOL.studentOid().getPath());
        }

        /*
         * Filter by activity since historical cutoff date
         */
        PlainDate historicalCutoffDate =
                (PlainDate) ToolBean.getPreference(ToolBean.PREFERENCE_HISTORICAL_CUTOFF_DATE);
        PlainDate queryAsOfDate =
                (PlainDate) ToolBean.getPreference(ToolBean.PREFERENCE_QUERY_AS_OF_DATE);
        StudentManager.buildSecondaryStudentDateCriteria(null, secondaryCriteria, historicalCutoffDate,
                queryAsOfDate, broker.getPersistenceKey());

        /*
         * Filter by school
         */
        if (spanCriteria.isIncludeAllSecondarySpans() || spanCriteria.getSchoolOids() == null) {
            secondaryCriteria.addNotEqualTo(SisBeanPaths.STUDENT_SCHOOL.school().inactiveIndicator().getPath(),
                    Boolean.TRUE);
            secondaryCriteria.addNotEqualTo(SisBeanPaths.STUDENT_SCHOOL.school().archiveIndicator().getPath(),
                    Boolean.TRUE);
        } else {
            secondaryCriteria.addIn(SisBeanPaths.STUDENT_SCHOOL.schoolOid().getPath(), spanCriteria.getSchoolOids());
        }

        return secondaryCriteria;
    }



    /**
     * Builds the student sub query criteria for enrollments.
     *
     * @param spanCriteria EnrollmentSpanCriteria
     * @param broker X2Broker
     * @return X2Criteria
     */
    private static X2Criteria buildStudentSubQueryCriteriaForEnrollments(EnrollmentSpanCriteria spanCriteria,
                                                                         X2Broker broker) {
        X2Criteria enrollmentCriteria = new X2Criteria();

        /*
         * Filter by specific students
         */
        if (spanCriteria.getLimitingStudentOids() != null) {
            ParameterSelectionHandler.addParameterSafeOIDList(enrollmentCriteria, broker,
                    spanCriteria.getLimitingStudentOids(), 0, SisBeanPaths.STUDENT_ENROLLMENT.studentOid().getPath());
        }

        /*
         * Filter by activity since historical cutoff date
         */
        PlainDate historicalCutoffDate =
                (PlainDate) ToolBean.getPreference(ToolBean.PREFERENCE_HISTORICAL_CUTOFF_DATE);
        if (historicalCutoffDate != null) {
            enrollmentCriteria.addGreaterOrEqualThan(SisBeanPaths.STUDENT_ENROLLMENT.enrollmentDate().getPath(),
                    historicalCutoffDate);
        }

        /*
         * Filter by school
         */
        if (spanCriteria.getSchoolOids() != null) {
            enrollmentCriteria.addIn(SisBeanPaths.STUDENT_ENROLLMENT.schoolOid().getPath(),
                    spanCriteria.getSchoolOids());
        } else {
            enrollmentCriteria.addNotEqualTo(SisBeanPaths.STUDENT_ENROLLMENT.school().inactiveIndicator().getPath(),
                    Boolean.TRUE);
            enrollmentCriteria.addNotEqualTo(SisBeanPaths.STUDENT_ENROLLMENT.school().archiveIndicator().getPath(),
                    Boolean.TRUE);
        }

        return enrollmentCriteria;
    }


}
