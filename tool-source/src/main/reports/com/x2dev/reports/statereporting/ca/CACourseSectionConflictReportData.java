/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2015 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.ca;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for "CA Course-Section Conflict-Audit Report".
 *
 * @author X2 Development Corporation
 */
public class CACourseSectionConflictReportData extends ReportJavaSourceNet {

    /**
     * The Class StudentHistoryData.
     */
    class StudentHistoryData extends StateReportData {
        StudentHistoryHelper m_helper;

        /**
         * Gets the student enrollment spans.
         *
         * @param student SisStudent
         * @return List
         */
        protected List<StudentEnrollmentSpan> getStudentEnrollmentSpans(SisStudent student) {
            return m_helper.getStudentEnrollmentSpans(student, true);
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
         * Gets the students query.
         *
         * @return Query by criteria
         */
        protected QueryByCriteria getStudentsQuery() {
            return m_helper.getStudentQuery(false);
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

            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SCHEDULE_SPANS, Boolean.TRUE);
        }
    }

    /**
     * Class representing record.
     */
    class StudentInfo {
        private String m_course;
        private PlainDate m_courseEntryDate;
        private PlainDate m_courseExitDate;
        private String m_grade;
        private PlainDate m_schoolEntryDate;
        private PlainDate m_schoolExitDate;
        private String m_studentId;
        private String m_studentName;

        /**
         * Gets the course.
         *
         * @return the course
         */
        public String getCourse() {
            return m_course;
        }

        /**
         * Gets the course entry date.
         *
         * @return the courseEntryDate
         */
        public PlainDate getCourseEntryDate() {
            return m_courseEntryDate;
        }

        /**
         * Gets the course exit date.
         *
         * @return the courseExitDate
         */
        public PlainDate getCourseExitDate() {
            return m_courseExitDate;
        }

        /**
         * Gets the grade.
         *
         * @return the grade
         */
        public String getGrade() {
            return m_grade;
        }

        /**
         * Gets the school entry date.
         *
         * @return the schoolEntryDate
         */
        public PlainDate getSchoolEntryDate() {
            return m_schoolEntryDate;
        }

        /**
         * Gets the school exit date.
         *
         * @return the schoolExitDate
         */
        public PlainDate getSchoolExitDate() {
            return m_schoolExitDate;
        }

        /**
         * Gets the student id.
         *
         * @return the m_studentId
         */
        public String getStudentId() {
            return m_studentId;
        }

        /**
         * Gets the student name.
         *
         * @return the studentName
         */
        public String getStudentName() {
            return m_studentName;
        }

        /**
         * Sets the course.
         *
         * @param course the course to set
         */
        public void setCourse(String course) {
            m_course = course;
        }

        /**
         * Sets the course entry date.
         *
         * @param courseEntryDate the courseEntryDate to set
         */
        public void setCourseEntryDate(PlainDate courseEntryDate) {
            m_courseEntryDate = courseEntryDate;
        }

        /**
         * Sets the course exit date.
         *
         * @param courseExitDate the courseExitDate to set
         */
        public void setCourseExitDate(PlainDate courseExitDate) {
            m_courseExitDate = courseExitDate;
        }

        /**
         * Sets the grade.
         *
         * @param grade the grade to set
         */
        public void setGrade(String grade) {
            m_grade = grade;
        }

        /**
         * Sets the school entry date.
         *
         * @param schoolEntryDate the schoolEntryDate to set
         */
        public void setSchoolEntryDate(PlainDate schoolEntryDate) {
            m_schoolEntryDate = schoolEntryDate;
        }

        /**
         * Sets the school exit date.
         *
         * @param schoolExitDate the schoolExitDate to set
         */
        public void setSchoolExitDate(PlainDate schoolExitDate) {
            m_schoolExitDate = schoolExitDate;
        }

        /**
         * Sets the student id.
         *
         * @param studentId the m_studentId to set
         */
        public void setStudentId(String studentId) {
            m_studentId = studentId;
        }

        /**
         * Sets the student name.
         *
         * @param studentName the studentName to set
         */
        public void setStudentName(String studentName) {
            m_studentName = studentName;
        }
    }

    /*
     * Static string constants.
     */
    private static final String FIELD_COURSE = "course";
    private static final String FIELD_COURSE_ENTRY_DATE = "courseEntryDate";
    private static final String FIELD_COURSE_EXIT_DATE = "courseExitDate";
    private static final String FIELD_GRADE = "grade";
    private static final String FIELD_GRID_MINUS = "gridMinus";
    private static final String FIELD_GRID_PLUS = "gridPlus";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_SCHOOL_ENTRY_DATE = "enrolEntryDate";
    private static final String FIELD_SCHOOL_EXIT_DATE = "enrolExitDate";
    private static final String FIELD_STUDENT_ID = "studentId";
    private static final String FIELD_STUDENT_NAME = "studentName";
    private static final String PARAM_FORMAT_MINUS = "formatMinus";
    private static final String PARAM_FORMAT_PLUS = "formatPlus";
    private static final String PARAM_SUBREPORT_ID_MINUS = "subMinusId";
    private static final String PARAM_SUBREPORT_ID_PLUS = "subPlusId";

    private Collection<SisSchool> m_schools;

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        for (SisSchool school : m_schools) {
            StudentHistoryData data = new StudentHistoryData();
            data.setBroker(getBroker());
            data.setOrganization(getOrganization());
            data.setPrivilegeSet(getPrivilegeSet());
            data.setSchoolContext(true);
            data.setSchool(school);
            data.setParameters(getParameters());
            data.setUser(getUser());
            data.initializeExport();

            Collection<SisStudent> students = getBroker().getCollectionByQuery(data.getStudentsQuery());
            ReportDataGrid gridMinusCourses = new ReportDataGrid();
            ReportDataGrid gridPlusCourses = new ReportDataGrid();

            for (SisStudent student : students) {
                Collection<StudentInfo> studentInfos = getStudentInfos(student, data);

                for (StudentInfo info : studentInfos) {
                    if (info.getCourse() == null) {
                        gridMinusCourses.append();
                        gridMinusCourses.set(FIELD_STUDENT_NAME, info.getStudentName());
                        gridMinusCourses.set(FIELD_STUDENT_ID, info.getStudentId());
                        gridMinusCourses.set(FIELD_GRADE, info.getGrade());
                        gridMinusCourses.set(FIELD_SCHOOL_ENTRY_DATE, info.getSchoolEntryDate());
                        gridMinusCourses.set(FIELD_SCHOOL_EXIT_DATE, info.getSchoolExitDate());
                    } else {
                        gridPlusCourses.append();
                        gridPlusCourses.set(FIELD_STUDENT_NAME, info.getStudentName());
                        gridPlusCourses.set(FIELD_STUDENT_ID, info.getStudentId());
                        gridPlusCourses.set(FIELD_GRADE, info.getGrade());
                        gridPlusCourses.set(FIELD_SCHOOL_ENTRY_DATE, info.getSchoolEntryDate());
                        gridPlusCourses.set(FIELD_SCHOOL_EXIT_DATE, info.getSchoolExitDate());
                        gridPlusCourses.set(FIELD_COURSE, info.getCourse());
                        gridPlusCourses.set(FIELD_COURSE_ENTRY_DATE, info.getCourseEntryDate());
                        gridPlusCourses.set(FIELD_COURSE_EXIT_DATE, info.getCourseExitDate());
                    }
                }
            }
            gridMinusCourses.sort(Arrays.asList(FIELD_STUDENT_NAME), true);
            gridPlusCourses.sort(Arrays.asList(FIELD_STUDENT_NAME, FIELD_COURSE), true);

            gridMinusCourses.beforeTop();
            gridPlusCourses.beforeTop();

            grid.append();
            grid.set(FIELD_SCHOOL, school.getName());
            grid.set(FIELD_GRID_MINUS, gridMinusCourses);
            grid.set(FIELD_GRID_PLUS, gridPlusCourses);
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        initializeSchools();

        Report reportMinusCourses = ReportUtils.getReport((String) getParameter(PARAM_SUBREPORT_ID_MINUS), getBroker());
        Report reportPlusCourses = ReportUtils.getReport((String) getParameter(PARAM_SUBREPORT_ID_PLUS), getBroker());

        addParameter(PARAM_FORMAT_MINUS, new ByteArrayInputStream(reportMinusCourses.getCompiledFormat()));
        addParameter(PARAM_FORMAT_PLUS, new ByteArrayInputStream(reportPlusCourses.getCompiledFormat()));
    }

    /**
     * Gets the student infos.
     *
     * @param student SisStudent
     * @param data StudentHistoryData
     * @return records for student.
     */
    private Collection<StudentInfo> getStudentInfos(SisStudent student, StudentHistoryData data) {
        ArrayList<KeyValuePair<StudentEnrollmentSpan, StudentScheduleSpan>> schoolScheduleSpans =
                new ArrayList<KeyValuePair<StudentEnrollmentSpan, StudentScheduleSpan>>();

        ArrayList<StudentEnrollmentSpan> schoolSpans = new ArrayList(data.getStudentEnrollmentSpans(student));

        Collection<StudentScheduleSpan> scheduleSpans = new ArrayList(data.getStudentScheduleSpans(student));

        /*
         * If student hasn't schedules he will be shown in first part of report.
         */
        if (scheduleSpans.size() == 0) {
            for (StudentEnrollmentSpan span : schoolSpans) {
                KeyValuePair<StudentEnrollmentSpan, StudentScheduleSpan> schooltSpan = new KeyValuePair(span, null);
                schoolScheduleSpans.add(schooltSpan);
            }
        }
        /*
         * Else check whether there are Date Overlap or Misalignments.
         * There are three types of conflict, for example:
         * A) Student A has an English Course enrollment date of 8/25/20014 but the student's
         * enrollment
         * entry date is one day later 8/26/2014.
         * B) Student B has a Homeroom Course enrollment record that ends on 6/30/2015 but withdrew
         * from XYZ school on 10/25/2014.
         * C) Student C changed English Courses and Teachers. However, the school personnel created
         * a new
         * enrollment record for the new Course (English 200) and Teacher, but left the student�s
         * initial
         * English 101 Course enrollment that still shows an enrollment date of 8/27/2014 and ending
         * date
         * of 6/30/2015.
         */
        else {
            for (StudentEnrollmentSpan sklSpan : schoolSpans) {
                StudentEnrollment entry = sklSpan.getFirstActiveEnrollment();
                StudentEnrollment withdrawal = sklSpan.getFirstInactiveEnrollment();

                HashMap<String, ArrayList<StudentScheduleSpan>> courses =
                        new HashMap<String, ArrayList<StudentScheduleSpan>>();
                /*
                 * Determine whether there are misalignments between school enrollment dates and
                 * schedule enrollment dates.
                 */
                for (StudentScheduleSpan schSpan : scheduleSpans) {
                    if ( /*
                          * If spans are overlapped.
                          */
                    (withdrawal == null || (!schSpan.getEntryDate().after(withdrawal.getEnrollmentDate())) &&
                            (schSpan.getExitDate() == null || !schSpan.getExitDate().before(entry.getEnrollmentDate())))
                            &&
                            /*
                             * If there is misalignment.
                             */
                            (schSpan.getEntryDate().before(entry.getEnrollmentDate()) ||

                                    (withdrawal != null && schSpan.getExitDate() != null &&
                                            schSpan.getExitDate().after(withdrawal.getEnrollmentDate())))) {
                        KeyValuePair<StudentEnrollmentSpan, StudentScheduleSpan> conflictedSpans =
                                new KeyValuePair(sklSpan, schSpan);
                        schoolScheduleSpans.add(conflictedSpans);
                    }
                    /*
                     * Add schedule spans grouped by course number.
                     */
                    String course = schSpan.getSection().getSchoolCourse().getNumber();

                    ArrayList<StudentScheduleSpan> spansByCourse = courses.get(course);

                    if (spansByCourse == null) {
                        spansByCourse = new ArrayList<StudentScheduleSpan>();
                        courses.put(course, spansByCourse);
                    }

                    spansByCourse.add(schSpan);
                }
                /*
                 * If student has more than one identical courses add these schedule spans (case C).
                 */
                for (ArrayList<StudentScheduleSpan> spans : courses.values()) {
                    if (spans.size() > 1) {
                        for (StudentScheduleSpan schSpan : spans) {
                            KeyValuePair<StudentEnrollmentSpan, StudentScheduleSpan> conflictedSpans =
                                    new KeyValuePair(sklSpan, schSpan);

                            if (!schoolScheduleSpans.contains(conflictedSpans)) {
                                schoolScheduleSpans.add(conflictedSpans);
                            }
                        }
                    }
                }
            }
        }

        ArrayList<StudentInfo> infos = new ArrayList<StudentInfo>();

        for (KeyValuePair<StudentEnrollmentSpan, StudentScheduleSpan> pair : schoolScheduleSpans) {
            StudentEnrollmentSpan enrollmentSpan = pair.getKey();
            StudentScheduleSpan scheduleSpan = pair.getValue();

            StudentEnrollment entry = enrollmentSpan.getFirstActiveEnrollment();
            StudentEnrollment withdrawal = enrollmentSpan.getFirstInactiveEnrollment();

            StudentInfo info = new StudentInfo();

            info.setStudentName(student.getNameView());
            info.setStudentId(student.getLocalId());
            info.setGrade(student.getGradeLevel());
            info.setSchoolEntryDate(entry.getEnrollmentDate());
            if (withdrawal != null) {
                info.setSchoolExitDate(withdrawal.getEnrollmentDate());
            }
            if (scheduleSpan != null) {
                info.setCourse(scheduleSpan.getSection().getCourseView());
                info.setCourseEntryDate(scheduleSpan.getEntryDate());
                info.setCourseExitDate(scheduleSpan.getExitDate());
            }

            infos.add(info);
        }
        return infos;
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
}
