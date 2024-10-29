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
import com.x2dev.reports.statereporting.fl.FLStudentAttendanceInfo.Daystamp;
import com.x2dev.reports.statereporting.fl.FLStudentAttendanceInfo.MultiDayAttendanceCalendar;
import com.x2dev.sis.model.beans.SchedulePeriod;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class FLStudentMultiDayAttendanceRegister.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class FLStudentMultiDayAttendanceRegister extends FLStudentMultiDayAttendanceReport {

    private static final String FIELD_PERIOD = "period";
    private static final String FIELD_PERIOD_IDX = "periodIdx";

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
                List<MultiDayAttendanceCalendar> all =
                        m_studentAttendanceInfo.getStudentPeriodsCalendarMultiDay(std, m_daysPerRow);
                for (MultiDayAttendanceCalendar data : all) {
                    addPage(std, data);
                }
            }
        } finally {
            if (studentIterator != null) {
                studentIterator.close();
            }
        }
        m_grid.beforeTop();

        sort();
        return m_grid;
    }

    /**
     * Adds the page.
     *
     * @param std SisStudent
     * @param data MultiDayAttendanceCalendar
     * @throws X2BaseException exception
     */
    private void addPage(SisStudent std, MultiDayAttendanceCalendar data) throws X2BaseException {
        if (std == null || data == null) {
            return;
        }

        List<SchedulePeriod> periods = m_studentAttendanceInfo.getPeriods(data.getSkl());
        if (periods != null && !periods.isEmpty()) {
            for (int p = 0; p < periods.size(); p++) {
                String period = periods.get(p).getId();

                m_grid.append();
                setSchoolFields(data.getSkl());
                setStudentFields(std);

                m_grid.set(FIELD_DATE_BEGIN, data.getBegin());
                m_grid.set(FIELD_DATE_END, data.getEnd());
                m_grid.set(FIELD_PERIOD, period);
                m_grid.set(FIELD_PERIOD_IDX, Integer.valueOf(p));

                String[] attendance = new String[m_daysPerRow];
                String[] months = new String[m_daysPerRow];
                String[] days = new String[m_daysPerRow];
                for (int i = 0; i < data.getDays().size(); i++) {
                    Daystamp ds = data.getDays().get(i);
                    attendance[i] = ds.toString(period);
                    months[i] = m_monthFormat.format(ds.getDate());
                    days[i] = m_dayFormat.format(ds.getDate());
                }

                m_grid.set(FIELD_STD_ATTENDANCE, attendance);
                m_grid.set(FIELD_STD_DAYS, days);
                m_grid.set(FIELD_STD_MONTHS, months);

                m_grid.set(FIELD_FL_EDU_ID, m_studentAttendanceInfo.getFieldValue(std.getPerson(), m_fieldFlEduId));
            }
        }
    }

}
