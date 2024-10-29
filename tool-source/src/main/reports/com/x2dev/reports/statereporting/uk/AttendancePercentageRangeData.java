/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2012 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.uk;

import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.attendance.uk.UKAttendancePercentageSummary;
import com.x2dev.sis.tools.reports.StudentReportJavaSource;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.math.RoundingMode;
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
public class AttendancePercentageRangeData extends StudentReportJavaSource {
    private static final long serialVersionUID = 1L;

    /*
     * Input parameters
     */
    private static final String DATE_END_PARAM = "endDate";
    private static final String DATE_START_PARAM = "startDate";
    private static final String PERCENT_MAX_PARAM = "maxPercent";
    private static final String PERCENT_MIN_PARAM = "minPercent";

    /*
     * Grid columns
     */
    private static final String COLUMN_MEMBER_DAYS = "memberDays";
    private static final String COLUMN_PERCENTAGE = "percentage";
    private static final String COLUMN_PRESENT_DAYS = "presentDays";
    private static final String COLUMN_STUDENT = "student";

    /*
     * Custom percentage sort tag
     */
    private static final String PERCENTAGE_SORT = "##percentage";

    private PlainDate m_endDate = null;
    private PlainDate m_startDate = null;

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_startDate = (PlainDate) getParameter(DATE_START_PARAM);
        m_endDate = (PlainDate) getParameter(DATE_END_PARAM);
    }

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(1000, 15);

        Integer maxPercent = (Integer) getParameter(PERCENT_MAX_PARAM);
        Integer minPercent = (Integer) getParameter(PERCENT_MIN_PARAM);

        Collection<SisStudent> students = getStudents();

        UKAttendancePercentageSummary ukAttHelper = new UKAttendancePercentageSummary(students, m_startDate, m_endDate,
                (SisSchool) getSchool(), getBroker());

        for (SisStudent student : students) {
            String studentOid = student.getOid();
            int totalAbsent = ukAttHelper.getTotalAbsent(studentOid);
            BigDecimal absences = new java.math.BigDecimal(String.valueOf(totalAbsent));

            /*
             * Calculate membership information
             */
            int membershipDays = ukAttHelper.getTotalPossible(studentOid);

            double absent = absences != null ? absences.doubleValue() : 0;

            double present = membershipDays - absent;
            double percent = membershipDays > 0 ? (present / membershipDays) : 0;
            BigDecimal percentDecimal = new BigDecimal(percent * 100.0).setScale(1, RoundingMode.HALF_DOWN);

            /*
             * Add the student and attendance tallies to the grid if they are within the
             * percentage range
             */
            if (percentDecimal.doubleValue() >= minPercent.doubleValue()
                    && percentDecimal.doubleValue() <= maxPercent.doubleValue()) {
                grid.append();
                grid.set(COLUMN_STUDENT, student);
                grid.set(COLUMN_MEMBER_DAYS, Integer.valueOf(membershipDays));
                grid.set(COLUMN_PRESENT_DAYS, Double.valueOf(present));
                grid.set(COLUMN_PERCENTAGE, percentDecimal);
            }
        }

        String sort = (String) getParameter(SORT_PARAM);
        if (PERCENTAGE_SORT.equals(sort)) {
            String[] sortcolumns = new String[] {COLUMN_PERCENTAGE, COLUMN_STUDENT + ".nameView"};
            grid.sort(Arrays.asList(sortcolumns), true);
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Returns the students to include in this report (in the correct order) based on user input.
     *
     * @return A Collection of Student beans
     */
    private Collection<SisStudent> getStudents() {
        Criteria criteria = buildCriteria();
        QueryByCriteria query = createQueryByCriteria(SisStudent.class, criteria);

        String sort = (String) getParameter(SORT_PARAM);
        if (!PERCENTAGE_SORT.equals(sort)) {
            applyUserSort(query, sort);
        }

        return getBroker().getCollectionByQuery(query);
    }

}
