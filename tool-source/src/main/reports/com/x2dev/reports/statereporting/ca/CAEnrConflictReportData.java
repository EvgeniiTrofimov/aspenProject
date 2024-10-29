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
package com.x2dev.reports.statereporting.ca;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for "CA Enrollment Conflict/Audit " report.
 *
 * @author X2 Development Corporation
 */
public class CAEnrConflictReportData extends ReportJavaSourceNet {

    /**
     * Helper class contains student helper.
     *
     * @author Follett Software Company
     */
    class EnrollmentData extends StateReportData {

        private StudentHistoryHelper m_helper;

        /**
         * Return student attendances.
         *
         * @param studentOid String
         * @return List
         */
        public List<StudentAttendance> getStudentAttendances(String studentOid) {
            return m_helper.getStudentAttendances(studentOid);
        }

        /**
         * Return the current student criteria.
         *
         * @return Criteria
         */
        public Criteria getStudentCriteria() {
            return m_helper.getStudentCriteria();
        }

        /**
         * Returns a list of student enrollment spans that represent all of the students enrollment
         * activity and
         * segments.
         *
         * @param student Student
         * @param limit boolean
         * @return List
         */
        public List<StudentEnrollmentSpan> getStudentEnrollmentSpans(Student student, boolean limit) {
            return m_helper.getStudentEnrollmentSpans(student, limit);
        }

        /**
         * Returns a list of student enrollments that represent all of the students enrollment
         * activity and
         * segments.
         *
         * @param student Student
         * @return List
         */
        public List<StudentEnrollment> getStudentEnrollments(Student student) {
            return m_helper.getStudentEnrollments(student);
        }

        /**
         * Return the current student criteria.
         *
         * @return Student history helper
         */
        public StudentHistoryHelper getStudentHelper() {
            return m_helper;
        }

        /**
         * Initialize the export. Set up the student history helper.
         */
        @Override
        public void initialize() {
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, getCurrentContext().getStartDate());
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, getCurrentContext().getEndDate());
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_ATTENDANCE, Boolean.TRUE);
        }
    }

    /**
     * Columns
     */
    private static final String COLUMN_ENTRY_DATE = "entryDate";
    private static final String COLUMN_SKL_NAME = "sklName";
    private static final String COLUMN_STD_GRADE = "stdGrade";
    private static final String COLUMN_STD_ID = "stdId";
    private static final String COLUMN_STD_NAME = "stdName";
    private static final String COLUMN_SUBTITLE = "subtitle";
    private static final String COLUMN_WITHDRAWAL_DATE = "withdrawalDate";

    /**
     * Subtitles
     */
    private static final String SUB_TITLE_ACTIVE_NO_ATT =
            "Active Student with no Course Assignment or Teacher association";
    private static final String SUB_TITLE_ENR_OVERLAP = "Student with enrollment data overlap or misalignment";
    private static final String SUB_TITLE_INACTIVE_WITH_ATT =
            "Inactive Students with Course Assignment and/or teacher association";
    /**
     * Class members
     */
    private EnrollmentData m_enrData;
    private Collection<SisSchool> m_schools;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {

        ReportDataGrid grid = new ReportDataGrid();
        for (SisSchool school : m_schools) {
            m_enrData = new EnrollmentData();
            m_enrData.setBroker(getBroker());
            m_enrData.setOrganization(getOrganization());
            m_enrData.setPrivilegeSet(getPrivilegeSet());
            m_enrData.setSchoolContext(true);
            m_enrData.setSchool(school);
            m_enrData.setParameters(getParameters());
            m_enrData.setUser(getUser());
            m_enrData.initializeExport();

            Criteria stdCriteria = m_enrData.getStudentCriteria();
            QueryByCriteria stdQuery = new QueryByCriteria(SisStudent.class, stdCriteria);
            Collection<Student> students = getBroker().getCollectionByQuery(stdQuery);

            for (Student std : students) {
                checkEnrConflict(school.getName(), std, grid);
            }
        }

        grid.sort(Arrays.asList(new String[] {COLUMN_SKL_NAME, COLUMN_SUBTITLE, COLUMN_STD_NAME, COLUMN_STD_GRADE}),
                true);
        grid.beforeTop();
        System.out.println(grid.toString());
        return grid;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        initializeSchools();
    }

    /**
     * Method to determine if student has any Enrollment Date Overlap or Misalignment.
     *
     * @param sklOid String
     * @param std Student
     * @param grid ReportDataGrid
     */
    private void checkEnrConflict(String sklOid, Student std, ReportDataGrid grid) {
        List<StudentEnrollmentSpan> spans = m_enrData.getStudentEnrollmentSpans(std, true);
        List<StudentAttendance> attendances = m_enrData.getStudentAttendances(std.getOid());
        List<StudentAttendance> attWhileInactive = attendances == null ? new LinkedList() : new LinkedList(attendances);
        List<StudentEnrollment> enrollments = m_enrData.getStudentEnrollments(std);

        for (StudentEnrollmentSpan span : spans) {
            Iterator<StudentAttendance> iter = attWhileInactive.iterator();
            while (iter.hasNext()) {
                StudentAttendance att = iter.next();
                if (att.getSchoolOid() != null && att.getSchoolOid().equals(span.getSchool().getOid()) &&
                        att.getDate() != null && !att.getDate().before(span.getFirstActiveDate()) &&
                        (span.getLastActiveDate() == null || !att.getDate().after(span.getLastActiveDate()))) {
                    iter.remove();
                }
            }
            if (span.getFirstActiveEnrollment() != null) {
                PlainDate startDate = span.getFirstActiveDate();
                PlainDate endDate = span.getLastActiveDate();
                if (isActiveWithNoAtt(startDate, endDate, attendances)) {
                    grid.append();
                    grid.set(COLUMN_SUBTITLE, SUB_TITLE_ACTIVE_NO_ATT);
                    grid.set(COLUMN_SKL_NAME, sklOid);
                    grid.set(COLUMN_STD_NAME, std.getNameView());
                    grid.set(COLUMN_STD_ID, std.getStateId());
                    grid.set(COLUMN_STD_GRADE, std.getGradeLevel());
                    grid.set(COLUMN_ENTRY_DATE, startDate);
                    grid.set(COLUMN_WITHDRAWAL_DATE, endDate);
                }
            }
        }
        KeyValuePair<PlainDate, PlainDate> conflict = getEnrConflict(std, enrollments);
        if (conflict != null) {
            grid.append();
            grid.set(COLUMN_SUBTITLE, SUB_TITLE_ENR_OVERLAP);
            grid.set(COLUMN_SKL_NAME, sklOid);
            grid.set(COLUMN_STD_NAME, std.getNameView());
            grid.set(COLUMN_STD_ID, std.getStateId());
            grid.set(COLUMN_STD_GRADE, std.getGradeLevel());
            grid.set(COLUMN_ENTRY_DATE, conflict.getKey());
            grid.set(COLUMN_WITHDRAWAL_DATE, conflict.getValue());

        }
        if (!attWhileInactive.isEmpty()) {
            for (StudentAttendance att : attWhileInactive) {
                grid.append();
                grid.set(COLUMN_SUBTITLE, SUB_TITLE_INACTIVE_WITH_ATT);
                grid.set(COLUMN_SKL_NAME, sklOid);
                grid.set(COLUMN_STD_NAME, std.getNameView());
                grid.set(COLUMN_STD_ID, std.getStateId());
                grid.set(COLUMN_STD_GRADE, std.getGradeLevel());
                grid.set(COLUMN_ENTRY_DATE, att.getDate());
                grid.set(COLUMN_WITHDRAWAL_DATE, att.getDate());
            }
        }
    }

    /**
     * Loads the schools used in the export.
     *
     * @return Collection<SisSchool>
     */
    private void initializeSchools() {
        X2Criteria criteria = new X2Criteria();

        m_schools = new ArrayList<SisSchool>();

        if (isSchoolContext()) {
            criteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());

        } else {
            criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            criteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
        }

        m_schools = getBroker().getCollectionByQuery(new QueryByCriteria(SisSchool.class, criteria));
    }

    /**
     * Check if student has active attendance on the day of.
     *
     * @param student Student
     * @param enrollments List<StudentEnrollment>
     * @return Key value pair
     */
    private KeyValuePair<PlainDate, PlainDate> getEnrConflict(Student student, List<StudentEnrollment> enrollments) {
        KeyValuePair conflict = null;

        // get initial status
        String enrollStatus = null;
        if (enrollments != null && enrollments.size() > 0) {
            StudentEnrollment currentEnrollment = enrollments.iterator().next();
            enrollStatus = currentEnrollment.getStatusCode();
            if (StringUtils.isEmpty(enrollStatus)) {
                enrollStatus = student.getEnrollmentStatus();
            }
        } else {
            enrollStatus = student.getEnrollmentStatus();
        }

        boolean isActive = StudentManager.isActiveStudent(getOrganization(), enrollStatus);
        PlainDate priorDate = null;
        if (enrollments != null) {
            for (StudentEnrollment enrollment : enrollments) {
                if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                    if (isActive && (priorDate == null
                            || !priorDate.before(getOrganization().getCurrentContext().getStartDate()))) {
                        conflict = new KeyValuePair(enrollment.getEnrollmentDate(), priorDate);
                        break;
                    }
                    isActive = true;
                    priorDate = enrollment.getEnrollmentDate();
                } else if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
                    if (!isActive && (priorDate == null
                            || !priorDate.before(getOrganization().getCurrentContext().getStartDate()))) {
                        conflict = new KeyValuePair(enrollment.getEnrollmentDate(), priorDate);
                        break;
                    }
                    isActive = false;
                    priorDate = enrollment.getEnrollmentDate();
                }
            }
        }
        return conflict;
    }

    /**
     * Check if student has no any attendance record for the active enrollment.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @param attendances List<StudentAttendance>
     * @return true, if is active with no att
     */
    private boolean isActiveWithNoAtt(PlainDate startDate, PlainDate endDate, List<StudentAttendance> attendances) {
        boolean match = true;

        if (attendances != null) {
            for (StudentAttendance att : attendances) {
                PlainDate attDate = att.getDate();
                if (att != null && !attDate.before(startDate) && (endDate == null || !attDate.after(endDate))) {
                    match = false;
                    break;
                }
            }
        }

        return match;
    }
}
