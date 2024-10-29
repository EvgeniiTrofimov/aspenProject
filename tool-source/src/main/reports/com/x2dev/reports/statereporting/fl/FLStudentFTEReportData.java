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
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.fl.FLScheduleHelper;
import com.x2dev.procedures.statereporting.fl.FLStateReportData;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper.StudentScheduleInfo;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import com.x2dev.utils.X2BaseException;
import java.util.Calendar;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * @author Follett Software Company
 * @copyright 2017
 */
public class FLStudentFTEReportData extends ReportJavaSourceNet {

    protected class Statistics extends FLStateReportData {
        private FLScheduleHelper m_scheduleHelper;
        private StudentScheduleHelper m_studentScheduleHelper;

        @Override
        public void initialize() throws X2BaseException {
            super.initialize();

            m_scheduleHelper =
                    new FLScheduleHelper(this, this.getSurveyPeriod().getStartDate(),
                            getSurveyPeriod().getEndDate());
            m_studentScheduleHelper = getStudentHelper().new StudentScheduleHelper(m_scheduleHelper,
                    this.getSurveyPeriod().getStartDate(), this.getSurveyPeriod().getEndDate());
        }

    }

    private static final String FIELD_CLASS_MINUTES = "minutes";
    private static final String FIELD_DAYS_OF_WEEK = "daysOfWeek";
    private static final String FIELD_FEFP_PROGRAM_NUMBER = "fefp";
    private static final String FIELD_FTE_REPORTED = "fte";
    private static final String FIELD_FTE_WEIGHTED = "fteWeighted";
    private static final String FIELD_PERIOD = "period";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_SECTION = "section";
    private static final String FIELD_STUDENT = "student";
    private Statistics m_data;
    private ScheduleReportHelper m_reportHelper;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        if (m_reportHelper.getStudentOid() != null) {
            this.addParameter("queryBy1", "oid");
            this.addParameter("queryString1", m_reportHelper.getStudentOid());
        }
        m_data = new Statistics();
        m_data.setBroker(getBroker());
        m_data.setOrganization(getOrganization());
        m_data.setPrivilegeSet(getPrivilegeSet());
        m_data.setSchoolContext(isSchoolContext());
        m_data.setSchool(getSchool());
        m_data.setParameters(getParameters());
        m_data.setUser(getUser());
        m_data.initializeExport();

        QueryByCriteria query = m_data.getStudentHelper().getStudentQuery(false);
        QueryIterator students = getBroker().getIteratorByQuery(query);
        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();
                if (m_data.getStudentHelper().isStudentEligible(student)) {
                    List<StudentScheduleInfo> records =
                            m_data.m_studentScheduleHelper.getStudentScheduleInfo(student);
                    // remove duplicate sections
                    Set<String> mstOids = new HashSet();
                    Iterator<StudentScheduleInfo> iterator = records.iterator();
                    while (iterator.hasNext()) {
                        StudentScheduleInfo item = iterator.next();
                        String mstOid = item.getSection().getOid();
                        if (mstOids.contains(mstOid)) {
                            iterator.remove();
                        } else {
                            mstOids.add(mstOid);
                        }
                    }
                    iterator = records.iterator();
                    while (iterator.hasNext()) {
                        StudentScheduleInfo item = iterator.next();
                        grid.append();
                        grid.set(FIELD_SCHOOL, item.getSection().getSchedule().getSchool());
                        grid.set(FIELD_STUDENT, student);
                        grid.set(FIELD_SECTION, item.getSection());
                        grid.set(FIELD_DAYS_OF_WEEK, getDaysOfWeek(item));
                        grid.set(FIELD_PERIOD, item.getMasterScheduleInfo().getPeriodNumber());
                        grid.set(FIELD_CLASS_MINUTES, item.getMasterScheduleInfo().getMinutesPerWeek());
                        grid.set(FIELD_FEFP_PROGRAM_NUMBER, item.getStudentInfo().getFefpProgram());
                        grid.set(FIELD_FTE_REPORTED, Double.valueOf(item.getFte().doubleValue() / 10000));
                        grid.set(FIELD_FTE_WEIGHTED, Double.valueOf(item.getFteWeighted().doubleValue() / 10000));
                    }
                }
            }
        } finally {
            students.close();
        }
        grid.beforeTop();
        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_reportHelper = new ScheduleReportHelper(userData);
    }

    private String getDaysOfWeek(StudentScheduleInfo item) throws X2BaseException {
        StringBuilder value = new StringBuilder();
        value.append(item.getMasterScheduleInfo().getScheduledOnDayOfWeek(Calendar.MONDAY).booleanValue() ? "M" : "");
        value.append(item.getMasterScheduleInfo().getScheduledOnDayOfWeek(Calendar.TUESDAY).booleanValue() ? "Tu" : "");
        value.append(
                item.getMasterScheduleInfo().getScheduledOnDayOfWeek(Calendar.WEDNESDAY).booleanValue() ? "W" : "");
        value.append(
                item.getMasterScheduleInfo().getScheduledOnDayOfWeek(Calendar.THURSDAY).booleanValue() ? "Th" : "");
        value.append(item.getMasterScheduleInfo().getScheduledOnDayOfWeek(Calendar.FRIDAY).booleanValue() ? "F" : "");
        return value.toString();
    }


}
