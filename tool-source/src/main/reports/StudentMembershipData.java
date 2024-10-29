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
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.tools.reports.StudentReportJavaSource;
import com.x2dev.utils.types.PlainDate;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Student Membership" report.
 *
 * @author X2 Development Corporation
 */
public class StudentMembershipData extends StudentReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Grid column that contains the number of days absent for the current student.
     */
    public static final String COLUMN_ABSENT_TOTAL = "absentTotal";

    /**
     * Grid column that contains the number of days dismissed for the current student.
     */
    public static final String COLUMN_DISMISSAL_TOTAL = "dismissalTotal";

    /**
     * Grid column that contains the number of member days for the current student.
     */
    public static final String COLUMN_MEMBER_DAYS = "membershipDays";

    /**
     * Grid column that contains the Student bean.
     */
    public static final String COLUMN_STUDENT = "student";

    /**
     * Grid column that contains the number of days tardy for the current student.
     */
    public static final String COLUMN_TARDY_TOTAL = "tardyTotal";

    /**
     * Name for the "end date" report parameter. The value is an Date.
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * Name for the "start date" report parameter. The value is an Date.
     */
    public static final String START_DATE_PARAM = "startDate";

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

        EnrollmentManager enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        Map calendarLookup = enrollmentManager.getCalendarLookup((SisSchool) getSchool(), m_startDate, m_endDate);
        Set firstDayMembers = enrollmentManager.getMembershipAsOf(m_startDate, (SisSchool) getSchool());

        Map attendanceByStudent = getStudentAttendance();
        QueryIterator students = getStudents();
        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();

                double absentTotal = 0.0;
                int tardyCount = 0;
                int dismissalCount = 0;

                /*
                 * Total attendance data, if any
                 */
                if (attendanceByStudent.containsKey(student.getOid())) {
                    Iterator attendanceData =
                            ((List) attendanceByStudent.get(student.getOid())).iterator();
                    while (attendanceData.hasNext()) {
                        StudentAttendance attendance = (StudentAttendance) attendanceData.next();
                        if (attendance.getAbsentIndicator()) {
                            absentTotal += attendance.getPortionAbsent().doubleValue();
                        }
                        if (attendance.getTardyIndicator()) {
                            tardyCount++;
                        }
                        if (attendance.getDismissedIndicator()) {
                            dismissalCount++;
                        }
                    }
                }

                /*
                 * Add the student and attendance counts to the grid
                 */
                grid.append();
                grid.set(COLUMN_STUDENT, student);

                grid.set(COLUMN_ABSENT_TOTAL, Double.valueOf(absentTotal));
                grid.set(COLUMN_DISMISSAL_TOTAL, Integer.valueOf(dismissalCount));
                grid.set(COLUMN_TARDY_TOTAL, Integer.valueOf(tardyCount));

                Set sessionDays = (Set) calendarLookup.get(student.getCalendarCode());
                int membershipTotal =
                        enrollmentManager.getMembershipTotal(student,
                                sessionDays,
                                firstDayMembers.contains(student.getOid()),
                                m_startDate,
                                m_endDate,
                                student.getSchool());
                grid.set(COLUMN_MEMBER_DAYS, Integer.valueOf(membershipTotal));
            }
        } finally {
            if (students != null) {
                students.close();
            }
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Returns the map of attendance data for the students included in this report. Students will
     * perfect attendance will not be included in this report.
     *
     * @return A Map of String objects (Student OIDs) to List objects (containing
     *         StudentAttendance beans)
     */
    private Map getStudentAttendance() {
        Criteria criteria = new Criteria();

        criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_startDate);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_endDate);
        criteria.addEqualTo(StudentAttendance.COL_SCHOOL_OID, getSchool().getOid());

        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        if (!queryBy.contains(SELECTION_SPECIAL_CASE_PREFIX)) {
            queryBy = (StudentAttendance.REL_STUDENT + PATH_DELIMITER + queryBy);
        }

        String queryString = (String) getParameter(QUERY_STRING_PARAM);

        addUserCriteria(criteria, queryBy, queryString, StudentAttendance.class, SisStudent.class,
                StudentAttendance.COL_STUDENT_OID);

        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria);
        return getBroker().getGroupedCollectionByQuery(query, StudentAttendance.COL_STUDENT_OID, 8);
    }

    /**
     * Returns the students to include in this report (in the correct order) based on user input.
     *
     * @return A QueryIterator of Student beans
     */
    private QueryIterator getStudents() {
        Criteria criteria = buildCriteria();
        QueryByCriteria query = createQueryByCriteria(SisStudent.class, criteria);

        String sort = (String) getParameter(SORT_PARAM);
        applyUserSort(query, sort);

        return getBroker().getIteratorByQuery(query);
    }
}
