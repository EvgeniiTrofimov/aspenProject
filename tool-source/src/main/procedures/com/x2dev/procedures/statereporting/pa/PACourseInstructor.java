/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.pa;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class PACourseInstructor.
 */
/*
 * The Course Instructor Template is used to identify the instructor(s) of each section of every
 * course reported in
 * the Course template.
 *
 * The Course Instructor template should be submitted in a cumulative fashion so that all
 * instructors associated
 * with all sections and courses are reported for the entire school year.
 *
 * All active teachers and teachers on long term leave should have course instructor records.
 *
 * Long term substitutes are reported only when they are filling a vacancy and become the teacher of
 * record.
 * A teacher on leave does not create a vacancy.
 *
 * One record per LEA / Location / School Year / Course Code / Section / Suppl Course Diff (formerly
 * known as Semester)
 *
 */
public class PACourseInstructor extends StateReportData {
    /**
     * Entity class for PA Course Instructor export.
     *
     */
    public static class PACourseInstructorEntity extends StateReportEntity {

        /**
         * Instantiates a new PA course instructor entity.
         */
        public PACourseInstructorEntity() {

        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            ScheduleTeacher schedule = (ScheduleTeacher) getBean();
            Staff staff = schedule.getStaff();
            String name = staff.getNameView() +
                    " [LASID: " + staff.getLocalId() +
                    ", SASID: " + staff.getStateId() +
                    "] " + schedule.getSection().getCourseView();

            return name;
        }
    }

    /**
     * Constants: Aliases, Fields, IDs, Parameters
     */
    protected static final String ALIAS_EXCLUDE_COURSE = "DOE EXCLUDE CRS";

    /**
     * Instance variables.
     */
    protected String m_fieldExcludeCourse;

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    /*
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     * com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        // registerFieldRetrievers

        initializeFields();

        if (getSetupErrors().size() != 0) {
            return;
        }

        setQuery(getScheduleTeacherQuery());
        setEntityClass(PACourseInstructorEntity.class);

    }

    /**
     * Gets the instructor criteria.
     *
     * @return Criteria
     */
    private Criteria getInstructorCriteria() {
        X2Criteria mtcCriteria = new X2Criteria();

        // Active schedule classes.
        mtcCriteria.addEqualToField(ScheduleTeacher.REL_SECTION + PATH_DELIMITER
                + Section.REL_SCHEDULE + PATH_DELIMITER
                + Schedule.REL_SCHOOL + PATH_DELIMITER
                + SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER
                + SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                ScheduleTeacher.REL_SECTION + PATH_DELIMITER
                        + Section.COL_SCHEDULE_OID);

        // Filter by current year context
        mtcCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER
                + Section.REL_SCHOOL_COURSE + PATH_DELIMITER
                + SchoolCourse.REL_COURSE + PATH_DELIMITER
                + Course.COL_DISTRICT_CONTEXT_OID,
                getOrganization().getCurrentContextOid());

        // Courses with MASTER TYPE = Class
        mtcCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER
                + Section.REL_SCHOOL_COURSE + PATH_DELIMITER
                + SchoolCourse.COL_MASTER_TYPE,
                SchoolCourse.MASTER_TYPE_CLASS);

        mtcCriteria.addEqualTo(ScheduleTeacher.COL_PRIMARY_TEACHER_INDICATOR, Boolean.TRUE);

        // Exclude sections where there is no enrollment, unless there were students earlier in the
        // year
        X2Criteria courseEnrCriteria = new X2Criteria();
        courseEnrCriteria.addGreaterOrEqualThan(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.COL_ENROLLMENT_TOTAL, Integer.valueOf(1));

        X2Criteria courseDroppedCriteria = new X2Criteria();
        courseDroppedCriteria.addIn(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + X2BaseBean.COL_OID,
                new SubQuery(StudentScheduleChange.class,
                        StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER + X2BaseBean.COL_OID,
                        getSccDroppedCriteria()));

        X2Criteria courseEmptyCriteria = new X2Criteria();
        courseEmptyCriteria.addOrCriteria(courseEnrCriteria);
        courseEmptyCriteria.addOrCriteria(courseDroppedCriteria);
        mtcCriteria.addAndCriteria(courseEmptyCriteria);


        // criteria for school selection.
        if (isSchoolContext()) {
            mtcCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER
                    + MasterSchedule.REL_SCHEDULE + PATH_DELIMITER
                    + Schedule.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else {
            mtcCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER
                    + MasterSchedule.REL_SCHEDULE + PATH_DELIMITER
                    + Schedule.REL_SCHOOL + PATH_DELIMITER
                    + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);

            mtcCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER
                    + MasterSchedule.REL_SCHEDULE + PATH_DELIMITER
                    + Schedule.REL_SCHOOL + PATH_DELIMITER
                    + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }

        // ScheduleTeacher.section.schoolCourse.course.[excludeCourse]
        // Exclude courses manually flagged for exclusion by the user
        mtcCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER
                + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER
                + SchoolCourse.REL_COURSE + PATH_DELIMITER
                + m_fieldExcludeCourse,
                BooleanAsStringConverter.TRUE);

        applyInputCriteria(mtcCriteria, false, null);

        return mtcCriteria;
    }

    /**
     * Gets the scc subquery criteria for dropped courses.
     *
     * @return X2Criteria
     */
    private X2Criteria getSccDroppedCriteria() {
        X2Criteria sccCriteria = new X2Criteria();

        sccCriteria.addEqualTo(StudentScheduleChange.COL_CHANGE_TYPE_CODE, StudentScheduleChange.CODE_DROP);

        sccCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER
                + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER
                + Course.COL_DISTRICT_CONTEXT_OID, getOrganization().getCurrentContextOid());

        sccCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER
                + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE,
                SchoolCourse.MASTER_TYPE_CLASS);

        sccCriteria.addEqualToField(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                        MasterSchedule.COL_SCHEDULE_OID);

        sccCriteria.addGreaterThanField(StudentScheduleChange.COL_EFFECTIVE_DATE,
                StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER + MasterSchedule.REL_SCHEDULE_TERM +
                        PATH_DELIMITER + ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER
                        + ScheduleTermDate.COL_START_DATE);


        sccCriteria.addNotEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER
                + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER
                + m_fieldExcludeCourse, BooleanAsStringConverter.TRUE);

        if (isSchoolContext()) {
            sccCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER
                    + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else {
            sccCriteria.addNotEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER
                    + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
        }

        return sccCriteria;
    }


    /**
     * Gets the schedule teacher query.
     *
     * @return Query by criteria
     */
    private QueryByCriteria getScheduleTeacherQuery() {
        QueryByCriteria query = new QueryByCriteria(ScheduleTeacher.class, getInstructorCriteria());
        applyInputSort(query, null);
        return query;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldExcludeCourse = translateAliasToJavaName(ALIAS_EXCLUDE_COURSE, true);
    }
}