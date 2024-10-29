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
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentInfo;
import com.x2dev.reports.statereporting.fl.FLStudentAttendanceInfo.Daystamp;
import com.x2dev.reports.statereporting.fl.FLStudentAttendanceInfo.MultiDayAttendanceCalendar;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class FLStudentMultiDayAttendanceReport.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class FLStudentMultiDayAttendanceReport extends ReportJavaSourceNet {

    /**
     * Data grid fields.
     */
    protected static final String FIELD_DATE_BEGIN = "beginDate";
    protected static final String FIELD_DATE_END = "endDate";
    protected static final String FIELD_FL_EDU_ID = "flEduId";
    protected static final String FIELD_STD_ATTENDANCE = "stdAttendance";
    protected static final String FIELD_STD_DAYS = "stdDays";
    protected static final String FIELD_STD_MONTHS = "stdMonths";

    protected SimpleDateFormat m_dayFormat = new SimpleDateFormat(FL_DAY_FORMAT);
    protected int m_daysPerRow;

    protected DataDictionaryField m_fieldDistrictNumber;
    protected DataDictionaryField m_fieldFlEduId;
    protected DataDictionaryField m_fieldSchoolNumber;
    protected DataDictionaryField m_fieldStudentNumber;

    protected ReportDataGrid m_grid;
    protected SimpleDateFormat m_monthFormat = new SimpleDateFormat(FL_MONTH_FORMAT);
    protected PlainDate m_startDate;
    protected FLStudentAttendanceInfo m_studentAttendanceInfo;

    private static final String ALIAS_DISTRICT_NUMBER = "all-org-StateId";

    private static final String ALIAS_PERSON_FL_EDU_ID = "all-psn-StateEducationId";

    private static final String ALIAS_SCHOOL_NUMBER = "all-skl-StateId";
    private static final String ALIAS_STUDENT_NUMBER = "all-std-StateId";

    private static final String BOOLEAN_NO = "N";
    private static final String BOOLEAN_YES = "Y";

    /**
     * Data grid fields.
     */

    private static final String FIELD_DST_NAME = "dstName";
    private static final String FIELD_DST_NUMBER = "dstNumber";

    private static final String FIELD_SKL_NAME = "sklName";
    private static final String FIELD_SKL_NUMBER = "sklNumber";
    private static final String FIELD_SKL_YEAR = "sklYear";

    private static final String FIELD_STD_BIRTH_DATE = "stdBirthDate";
    private static final String FIELD_STD_GRADE = "stdGrade";
    private static final String FIELD_STD_LEGAL_NAME = "stdLegalName";
    private static final String FIELD_STD_LOCAL_ID = "stdLocalId";
    private static final String FIELD_STD_NUMBER = "stdNumber";

    private static final String FL_DATE_FORMAT = "MMddyyyy";
    private static final String FL_DAY_FORMAT = "dd";
    private static final String FL_MONTH_FORMAT = "MM";

    private static final String INPUT_PARAM_DAYS_PER_ROW = "daysPerRow";
    private static final String INPUT_PARAM_END_DATE = "endDate";
    private static final String INPUT_PARAM_START_DATE = "startDate";

    private static final String PARAM_FL_DATE_FORMAT = "flDateFormat";
    private static final String PARAM_IS_SCHOOL_CONTEXT = "isSklContext";

    private Calendar m_calendar;

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
                        m_studentAttendanceInfo.getStudentCalendarMultiDay(std, m_daysPerRow);
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
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {

        PlainDate endDate = (PlainDate) getParameters().get(INPUT_PARAM_END_DATE);
        m_startDate = (PlainDate) getParameters().get(INPUT_PARAM_START_DATE);
        m_daysPerRow = Integer.parseInt(getParameters().get(INPUT_PARAM_DAYS_PER_ROW).toString());

        m_calendar = Calendar.getInstance();

        addParameter(PARAM_FL_DATE_FORMAT, new SimpleDateFormat(FL_DATE_FORMAT));
        addParameter(PARAM_IS_SCHOOL_CONTEXT, isSchoolContext() ? BOOLEAN_YES : BOOLEAN_NO);

        FLStudentAttendanceReportData reportData = new FLStudentAttendanceReportData();
        reportData.setParameters(getParameters());
        reportData.setBroker(getBroker());
        reportData.setOrganization(getOrganization());
        reportData.setCurrentContext(getCurrentContext());
        reportData.setSchool(getSchool());
        reportData.setSchoolContext(isSchoolContext());
        reportData.setPrivilegeSet(getPrivilegeSet());

        m_fieldDistrictNumber = reportData.translateAliasToDictionaryField(ALIAS_DISTRICT_NUMBER, true);
        m_fieldSchoolNumber = reportData.translateAliasToDictionaryField(ALIAS_SCHOOL_NUMBER, true);
        m_fieldStudentNumber = reportData.translateAliasToDictionaryField(ALIAS_STUDENT_NUMBER, true);
        m_fieldFlEduId = reportData.translateAliasToDictionaryField(ALIAS_PERSON_FL_EDU_ID, true);

        m_studentAttendanceInfo = new FLStudentAttendanceInfo(reportData, m_startDate, endDate);
    }

    /**
     * Sets the school fields.
     *
     * @param skl void
     * @throws X2BaseException exception
     */
    protected void setSchoolFields(SisSchool skl) throws X2BaseException {
        m_grid.set(FIELD_DST_NUMBER,
                m_studentAttendanceInfo.getFieldValue(skl.getOrganization1(), m_fieldDistrictNumber));
        m_grid.set(FIELD_DST_NAME, skl.getOrganization1().getName());

        m_grid.set(FIELD_SKL_YEAR, Integer.toString(m_calendar.get(Calendar.YEAR)));
        m_grid.set(FIELD_SKL_NAME, skl.getName());
        m_grid.set(FIELD_SKL_NUMBER, m_studentAttendanceInfo.getFieldValue(skl, m_fieldSchoolNumber));
    }

    /**
     * Sets the student fields.
     *
     * @param std void
     * @throws X2BaseException exception
     */
    protected void setStudentFields(SisStudent std) throws X2BaseException {
        StudentInfo info = m_studentAttendanceInfo.getStudentHelper().getStudentInfo(std);
        m_grid.set(FIELD_STD_LOCAL_ID, std.getLocalId());
        m_grid.set(FIELD_STD_NUMBER, m_studentAttendanceInfo.getFieldValue(std, m_fieldStudentNumber));
        m_grid.set(FIELD_STD_LEGAL_NAME, info.formatStudentLegalName());
        m_grid.set(FIELD_STD_BIRTH_DATE, std.getPerson().getDob());
        m_grid.set(FIELD_STD_GRADE, info.getGradeLevel(m_startDate));
    }

    /**
     * Sort.
     */
    protected void sort() {
        List<String> sort =
                new ArrayList<String>(
                        Arrays.asList(new String[] {FIELD_SKL_YEAR, FIELD_SKL_NUMBER, FIELD_DATE_BEGIN,
                                FIELD_STD_LEGAL_NAME}));

        m_grid.sort(sort, true);
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

        m_grid.append();
        setSchoolFields(data.getSkl());
        setStudentFields(std);

        m_grid.set(FIELD_DATE_BEGIN, data.getBegin());
        m_grid.set(FIELD_DATE_END, data.getEnd());

        String[] attendance = new String[m_daysPerRow];
        String[] months = new String[m_daysPerRow];
        String[] days = new String[m_daysPerRow];
        for (int i = 0; i < data.getDays().size(); i++) {
            Daystamp ds = data.getDays().get(i);
            attendance[i] = ds.toString();
            months[i] = m_monthFormat.format(ds.getDate());
            days[i] = m_dayFormat.format(ds.getDate());
        }

        m_grid.set(FIELD_STD_ATTENDANCE, attendance);
        m_grid.set(FIELD_STD_DAYS, days);
        m_grid.set(FIELD_STD_MONTHS, months);

        m_grid.set(FIELD_FL_EDU_ID, m_studentAttendanceInfo.getFieldValue(std.getPerson(), m_fieldFlEduId));
    }
}
