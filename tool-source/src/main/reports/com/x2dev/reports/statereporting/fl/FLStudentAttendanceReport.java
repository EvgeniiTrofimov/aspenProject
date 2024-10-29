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
import com.x2dev.reports.statereporting.fl.FLStudentAttendanceInfo.AttendanceCalendar;
import com.x2dev.reports.statereporting.fl.FLStudentAttendanceInfo.AttendanceHistory;
import com.x2dev.reports.statereporting.fl.FLStudentAttendanceInfo.Daystamp;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class FLStudentAttendanceReport.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class FLStudentAttendanceReport extends ReportJavaSourceNet {

    protected static final String ALIAS_DISTRICT_NUMBER = "all-org-StateId";

    protected static final String ALIAS_PERSON_FL_EDU_ID = "all-psn-StateEducationId";

    protected static final String ALIAS_SCHOOL_NUMBER = "all-skl-StateId";
    protected static final String ALIAS_STUDENT_NUMBER = "all-std-StateId";

    protected static final String BOOLEAN_NO = "N";
    protected static final String BOOLEAN_YES = "Y";

    /**
     * Data grid fields.
     */

    protected static final String FIELD_DST_NAME = "dstName";
    protected static final String FIELD_DST_NUMBER = "dstNumber";

    protected static final String FIELD_ENR_ENTRY = "enrEntry";
    protected static final String FIELD_ENR_SCHOOL = "enrSchool";
    protected static final String FIELD_ENR_WITHDRAWAL = "enrWithdrawal";

    protected static final String FIELD_FL_EDU_ID = "flEduId";

    protected static final String FIELD_IS_SCHOOL_CONTEXT = "isSklContext";

    protected static final String FIELD_SKL_NAME = "sklName";
    protected static final String FIELD_SKL_NUMBER = "sklNumber";
    protected static final String FIELD_SKL_YEAR = "sklYear";

    protected static final String FIELD_STD_ATTENDANCE = "stdAttendance";
    protected static final String FIELD_STD_BIRTH_DATE = "stdBirthDate";
    protected static final String FIELD_STD_ETHNICITY = "stdEthnicity";
    protected static final String FIELD_STD_GENDER = "stdGender";
    protected static final String FIELD_STD_GRADE = "stdGrade";
    protected static final String FIELD_STD_LEGAL_NAME = "stdLegalName";
    protected static final String FIELD_STD_LOCAL_ID = "stdLocalId";
    protected static final String FIELD_STD_NUMBER = "stdNumber";
    protected static final String FIELD_STD_RACE_ASIAN = "stdRaceA";
    protected static final String FIELD_STD_RACE_BLACK = "stdRaceB";
    protected static final String FIELD_STD_RACE_INDIAN = "stdRaceI";
    protected static final String FIELD_STD_RACE_PACIFIC = "stdRaceP";
    protected static final String FIELD_STD_RACE_WHITE = "stdRaceW";
    protected static final String FIELD_STD_SSN = "stdSsn";

    protected static final String FL_DATE_FORMAT = "MMddyyyy";

    protected static final String INPUT_PARAM_END_DATE = "endDate";
    protected static final String INPUT_PARAM_START_DATE = "startDate";

    protected static final String PARAM_FL_DATE_FORMAT = "flDateFormat";

    protected static final String RACE_CODE_ASIAN = "A";
    protected static final String RACE_CODE_BLACK = "B";
    protected static final String RACE_CODE_INDIAN = "I";
    protected static final String RACE_CODE_PACIFIC = "P";
    protected static final String RACE_CODE_WHITE = "W";

    protected DataDictionaryField m_fieldDistrictNumber;
    protected DataDictionaryField m_fieldFlEduId;
    protected DataDictionaryField m_fieldSchoolNumber;
    protected DataDictionaryField m_fieldStudentNumber;

    protected ReportDataGrid m_grid;
    protected PlainDate m_startDate;
    protected FLStudentAttendanceInfo m_studentAttendanceInfo;

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
                List<AttendanceCalendar> all = m_studentAttendanceInfo.getStudentCalendar(std);
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
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {

        addParameter(PARAM_FL_DATE_FORMAT, new SimpleDateFormat(FL_DATE_FORMAT));

        m_startDate = (PlainDate) getParameters().get(INPUT_PARAM_START_DATE);
        m_calendar = Calendar.getInstance();

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

        m_studentAttendanceInfo = new FLStudentAttendanceInfo(reportData, m_startDate,
                (PlainDate) getParameters().get(INPUT_PARAM_END_DATE));

    }

    /**
     * Sets the attendance history.
     *
     * @param attendanceHistory void
     * @throws X2BaseException exception
     */
    protected void setAttendanceHistory(AttendanceHistory attendanceHistory) throws X2BaseException {
        m_grid.set(FIELD_IS_SCHOOL_CONTEXT, isSchoolContext() ? BOOLEAN_YES : BOOLEAN_NO);

        List<String> attSchools = new ArrayList();
        List<PlainDate> attEntryDates = attendanceHistory.getEntryDates();
        List<PlainDate> attWithdrawalDates = attendanceHistory.getWithdrawalDates();

        for (int i = 0; i < attendanceHistory.getSchools().size(); i++) {
            SisSchool skl = attendanceHistory.getSchools().get(i);
            attSchools.add((String) m_studentAttendanceInfo.getFieldValue(skl, m_fieldSchoolNumber));
        }

        m_grid.set(FIELD_ENR_ENTRY, attEntryDates.toArray(new PlainDate[attEntryDates.size()]));
        m_grid.set(FIELD_ENR_SCHOOL, attSchools.toArray(new String[attSchools.size()]));
        m_grid.set(FIELD_ENR_WITHDRAWAL, attWithdrawalDates.toArray(new PlainDate[attWithdrawalDates.size()]));
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
        m_grid.set(FIELD_STD_SSN, info.getSSN());

        m_grid.set(FIELD_STD_ETHNICITY, std.getPerson().getHispanicLatinoIndicator() ? BOOLEAN_YES : BOOLEAN_NO);
        m_grid.set(FIELD_STD_RACE_ASIAN, checkRace(std, RACE_CODE_ASIAN));
        m_grid.set(FIELD_STD_RACE_BLACK, checkRace(std, RACE_CODE_BLACK));
        m_grid.set(FIELD_STD_RACE_INDIAN, checkRace(std, RACE_CODE_INDIAN));
        m_grid.set(FIELD_STD_RACE_PACIFIC, checkRace(std, RACE_CODE_PACIFIC));
        m_grid.set(FIELD_STD_RACE_WHITE, checkRace(std, RACE_CODE_WHITE));

        m_grid.set(FIELD_STD_GENDER, std.getPerson().getGenderCode());
        m_grid.set(FIELD_STD_LEGAL_NAME, info.formatStudentLegalName());
        m_grid.set(FIELD_STD_BIRTH_DATE, std.getPerson().getDob());
        m_grid.set(FIELD_STD_GRADE, info.getGradeLevel(m_startDate));
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
            m_grid.append();
            setSchoolFields(data.getSkl());
            setStudentFields(std);

            String[] attendance = new String[row.size()];
            for (int i = 0; i < row.size(); i++) {
                attendance[i] = row.get(i).toString();
            }

            m_grid.set(FIELD_STD_ATTENDANCE, attendance);
        }

        setAttendanceHistory(data.getAttendanceHistory());

        m_grid.set(FIELD_FL_EDU_ID, m_studentAttendanceInfo.getFieldValue(std.getPerson(), m_fieldFlEduId));
    }

    /**
     * Check race.
     *
     * @param std SisStudent
     * @param raceCode String
     * @return String
     */
    private String checkRace(SisStudent std, String raceCode) {
        return m_studentAttendanceInfo.checkRace(std, raceCode) ? BOOLEAN_YES : BOOLEAN_NO;
    }
}
