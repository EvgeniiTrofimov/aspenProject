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
package com.x2dev.reports.statereporting.uk;
/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.attendance.uk.UKAttendancePercentageSummary;
import com.x2dev.sis.tools.reports.StudentReportJavaSource;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Collection;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Student Membership" report.
 *
 * @author X2 Development Corporation
 */
public class AttendanceAnalysisData extends StudentReportJavaSource {
    private static final long serialVersionUID = 1L;

    /*
     * Additional input parameters
     */
    private static final String END_DATE_PARAM = "endDate";
    private static final String START_DATE_PARAM = "startDate";

    /*
     * Grid fields
     */
    private static final String COLUMN_ABSENT_EXCUSED = "absentExcused";
    private static final String COLUMN_ABSENT_UNEXCUSED = "absentUnexcused";
    private static final String COLUMN_ATTENDANCE_TOTAL = "attendanceTotal";
    private static final String COLUMN_HOMEROOM = "homeroom";
    private static final String COLUMN_SESSION_COUNT = "sessions";
    private static final String COLUMN_STUDENT_ALL = "allStudents";
    private static final String COLUMN_STUDENT_CURRENT = "currentStudents";
    private static final String COLUMN_STUDENT_EXCUSED = "studentExcused";
    private static final String COLUMN_STUDENT_UNEXCUSED = "studentUnexcused";
    private static final String COLUMN_YEAR_GROUP = "yearGroup";

    private PlainDate m_endDate = null;
    private PlainDate m_startDate = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(1000, 15);

        m_startDate = (PlainDate) getParameter(START_DATE_PARAM);
        m_endDate = (PlainDate) getParameter(END_DATE_PARAM);

        Collection<SisStudent> students = getStudents();

        UKAttendancePercentageSummary ukAttHelper = new UKAttendancePercentageSummary(students, m_startDate, m_endDate,
                (SisSchool) getSchool(), getBroker());

        String lastKey = null;
        SisStudent lastStudent = null;

        int absentExcused = 0;
        int absentTotal = 0;
        int absentUnexcused = 0;
        int sessions = 0;
        int studentCurrent = 0;
        int studentExcused = 0;
        int studentTotal = 0;
        int studentUnexcused = 0;

        for (SisStudent student : students) {
            String studentOid = student.getOid();
            String key = student.getGradeLevel() + (student.getHomeroom() == null ? "" : student.getHomeroom());

            boolean hasExcused = false;
            boolean hasUnexcused = false;

            if (!ObjectUtils.match(key, lastKey)) {
                setGrid(grid,
                        lastStudent,
                        absentExcused,
                        absentTotal,
                        absentUnexcused,
                        sessions,
                        studentCurrent,
                        studentExcused,
                        studentTotal,
                        studentUnexcused);

                grid.append();

                absentExcused = 0;
                absentTotal = 0;
                absentUnexcused = 0;
                sessions = 0;
                studentCurrent = 0;
                studentExcused = 0;
                studentTotal = 0;
                studentUnexcused = 0;
            }

            /*
             * Total attendance data, if any
             */
            absentTotal = absentTotal + ukAttHelper.getTotalAbsent(studentOid);

            int tempAuthorizedAbsent = ukAttHelper.getAbsentTotalAuthorized(studentOid);
            if (tempAuthorizedAbsent > 0) {
                hasExcused = true;
                absentExcused = absentExcused + tempAuthorizedAbsent;
            }

            int tempUnAuthorizedAbsent = ukAttHelper.getAbsentTotalUnauthorized(studentOid);
            if (tempUnAuthorizedAbsent > 0) {
                hasUnexcused = true;
                absentUnexcused = absentUnexcused + tempAuthorizedAbsent;
            }

            sessions += ukAttHelper.getTotalPossible(studentOid);

            studentExcused += (hasExcused ? 1 : 0);
            studentUnexcused += (hasUnexcused ? 1 : 0);

            studentTotal++;

            if (StudentManager.isActiveStudent(getOrganization(), student.getEnrollmentStatus())
                    && student.getSchool().equals(getSchool())) {
                studentCurrent++;
            }

            lastKey = key;
            lastStudent = student;
        }

        /*
         * Append the last row, if necessary
         */
        setGrid(grid,
                lastStudent,
                absentExcused,
                absentTotal,
                absentUnexcused,
                sessions,
                studentCurrent,
                studentExcused,
                studentTotal,
                studentUnexcused);

        grid.beforeTop();
        return grid;
    }

    /**
     * Returns the students to include in this report (in the correct order) based on user input.
     *
     * @return A Collection of Student beans
     */
    private Collection<SisStudent> getStudents() {
        /*
         * Build enrollment criteria
         */
        Criteria enrollmentCriteria = new Criteria();
        enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_startDate);
        enrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_endDate);
        enrollmentCriteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE,
                Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.WITHDRAWAL));

        if (isSchoolContext()) {
            enrollmentCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }

        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        if (!queryBy.contains(SELECTION_SPECIAL_CASE_PREFIX)) {
            queryBy = (StudentAttendance.REL_STUDENT + PATH_DELIMITER + queryBy);
        }
        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        addUserCriteria(enrollmentCriteria, queryBy, queryString, SisStudent.class, StudentEnrollment.COL_STUDENT_OID);

        /*
         * Get students based on school or students with enrollment records to school within the
         * date range
         */
        SubQuery enrollments =
                new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, enrollmentCriteria);
        Criteria studentEnrollment = new Criteria();
        studentEnrollment.addIn(X2BaseBean.COL_OID, enrollments);

        Criteria criteria = buildCriteria();
        criteria.addOrCriteria(studentEnrollment);

        QueryByCriteria query = createQueryByCriteria(SisStudent.class, criteria);

        String sort = (String) getParameter(SORT_PARAM);
        applyUserSort(query, sort);

        return getBroker().getCollectionByQuery(query);
    }

    /**
     * Sets the grid's current row with the current count information.
     *
     * @param grid ReportDataGrid
     * @param student SisStudent
     * @param absentExcused int
     * @param absentTotal int
     * @param absentUnexcused int
     * @param sessions int
     * @param studentCurrent int
     * @param studentExcused int
     * @param studentTotal int
     * @param studentUnexcused int
     */
    private void setGrid(ReportDataGrid grid,
                         SisStudent student,
                         int absentExcused,
                         int absentTotal,
                         int absentUnexcused,
                         int sessions,
                         int studentCurrent,
                         int studentExcused,
                         int studentTotal,
                         int studentUnexcused) {
        if (grid.currentRowNumber() >= 0) {
            grid.set(COLUMN_HOMEROOM, student.getHomeroom());
            grid.set(COLUMN_YEAR_GROUP,
                    student.getGradeLevel() != null ? student.getGradeLevel().replaceAll("YR", "Year ") : "");

            grid.set(COLUMN_ABSENT_EXCUSED, Integer.valueOf(absentExcused));
            grid.set(COLUMN_ABSENT_UNEXCUSED, Integer.valueOf(absentUnexcused));
            grid.set(COLUMN_ATTENDANCE_TOTAL, Integer.valueOf(sessions - absentTotal));

            grid.set(COLUMN_SESSION_COUNT, Integer.valueOf(sessions));

            grid.set(COLUMN_STUDENT_ALL, Integer.valueOf(studentTotal));
            grid.set(COLUMN_STUDENT_CURRENT, Integer.valueOf(studentCurrent));
            grid.set(COLUMN_STUDENT_EXCUSED, Integer.valueOf(studentExcused));
            grid.set(COLUMN_STUDENT_UNEXCUSED, Integer.valueOf(studentUnexcused));
        }
    }
}
