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
package com.x2dev.reports.statereporting.fl;

import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.reports.statereporting.fl.FLStudentAttendanceInfo.AttendanceCalendar;
import com.x2dev.reports.statereporting.fl.FLStudentAttendanceInfo.Daystamp;
import com.x2dev.sis.model.beans.SchedulePeriod;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class FLStudentAttendanceByPeriodRecord.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class FLStudentAttendanceByPeriodRecord extends FLStudentAttendanceReport {

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        m_grid = new ReportDataGrid();

        QueryByCriteria query =
                new QueryByCriteria(SisStudent.class, m_studentAttendanceInfo.getStudentHelper().getStudentCriteria());

        QueryIterator studentIterator = null;
        try {
            studentIterator = getBroker().getIteratorByQuery(query);
            while (studentIterator.hasNext()) {
                SisStudent std = (SisStudent) studentIterator.next();
                List<AttendanceCalendar> all = m_studentAttendanceInfo.getStudentPeriodsCalendar(std);
                for (AttendanceCalendar data : all) {
                    addPage(std, data);
                }
            }
        } finally {
            if (studentIterator != null) {
                studentIterator.close();
            }
        }
        m_grid.beforeTop();
        return m_grid;
    }

    /**
     * Adds the page.
     *
     * @param std SisStudent
     * @param data AttendanceCalendar
     * @throws X2BaseException exception
     */
    private void addPage(SisStudent std, AttendanceCalendar data) throws X2BaseException {
        if (std == null || data == null) {
            return;
        }

        for (List<Daystamp> row : data.getData()) {
            List<SchedulePeriod> periods = m_studentAttendanceInfo.getPeriods(data.getSkl());
            if (periods != null && !periods.isEmpty()) {
                for (int p = 0; p < periods.size(); p++) {
                    String period = periods.get(p).getId();

                    m_grid.append();

                    setSchoolFields(data.getSkl());
                    setStudentFields(std);

                    String[] attendance = new String[row.size() + 1];
                    attendance[0] = p == 0 ? row.get(0).toString() : "";
                    attendance[1] = period;
                    for (int i = 1; i < row.size(); i++) {
                        attendance[i + 1] = row.get(i).toString(period);
                    }
                    m_grid.set(FIELD_STD_ATTENDANCE, attendance);
                }
            }
        }

        setAttendanceHistory(data.getAttendanceHistory());

        m_grid.set(FIELD_FL_EDU_ID, m_studentAttendanceInfo.getFieldValue(std.getPerson(), m_fieldFlEduId));
    }
}
