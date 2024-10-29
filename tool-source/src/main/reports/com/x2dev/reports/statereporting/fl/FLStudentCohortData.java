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
import com.x2dev.procedures.statereporting.fl.FLScheduleHelper;
import com.x2dev.procedures.statereporting.fl.FLStateReportData;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.IntegerAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;
import java.util.Locale;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class FLStudentCohortData.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class FLStudentCohortData extends ReportJavaSourceNet {

    /**
     * The Class Statistics.
     */
    protected class Statistics extends FLStateReportData {
        private FLScheduleHelper m_scheduleHelper;
        private StudentScheduleHelper m_studentScheduleHelper;

        /**
         * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#initialize()
         */
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

    private static final String ALIAS_NINTH_GRADE = "all-std-NinthGradeYear";

    private static final List<String> CODES_DIPLOMA = Arrays.asList("W06", "W07", "W08", "W09", "W10", "W27", "W8A",
            "W8B", "WD1", "WFT", "WFW", "WGA", "WGD", "WRW", "WXL", "WXT", "WXW");
    private static final List<String> CODES_DROPOUT = Arrays.asList("DNE", "W05", "W13", "W15", "W18", "W21", "W22");
    private static final List<String> CODES_ENROLLED = Arrays.asList("W01", "W02");
    private static final List<String> CODES_TRANSFER = Arrays.asList("W04", "W24", "W3A", "W3B");

    private static final String FIELD_ENROLLMENT = "enrollment";
    private static final String FIELD_SCHOOL_NAME = "schoolName";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_STUDENT_NAME = "studentName";
    private static final String FIELD_TYPE = "type";

    private static final String PARAM_COHORT_YEAR = "ninthGradeYear";
    private static final String PARAM_GRID_SORT = "gridSort";
    private static final String PARAM_INCLUDE_DIPLOMA = "includeDiploma";
    private static final String PARAM_INCLUDE_DROPOUT = "includeDropout";
    private static final String PARAM_INCLUDE_ENROLLED = "includeEnrolled";
    private static final String PARAM_INCLUDE_TRANSFER = "includeTransfer";

    private Statistics m_data;

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

        m_data = new Statistics();
        m_data.setBroker(getBroker());
        m_data.setOrganization(getOrganization());
        m_data.setPrivilegeSet(getPrivilegeSet());
        m_data.setSchoolContext(isSchoolContext());
        m_data.setSchool(getSchool());
        m_data.setParameters(getParameters());
        m_data.setUser(getUser());
        m_data.initializeExport();

        int year = ((Integer) getParameter(PARAM_COHORT_YEAR)).intValue();
        Calendar calendar = Calendar.getInstance();
        calendar.set(year, 1, 1);
        PlainDate beginDate = new PlainDate(calendar.getTime());

        boolean includeDiploma = ((Boolean) getParameter(PARAM_INCLUDE_DIPLOMA)).booleanValue();
        boolean includeDropout = ((Boolean) getParameter(PARAM_INCLUDE_DROPOUT)).booleanValue();
        boolean includeEnrolled = ((Boolean) getParameter(PARAM_INCLUDE_ENROLLED)).booleanValue();
        boolean includeTransfer = ((Boolean) getParameter(PARAM_INCLUDE_TRANSFER)).booleanValue();


        m_data.getStudentHelper().setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, beginDate);

        IntegerAsStringConverter integerConverter = (IntegerAsStringConverter) ConverterFactory
                .getConverterForClass(Converter.INTEGER_CONVERTER, Locale.getDefault(), true);
        String cohortYear = integerConverter.getSystemString(Integer.valueOf(year));
        String aliasField = m_data.translateAliasToJavaName(ALIAS_NINTH_GRADE, false);
        if (aliasField == null) {
            throw new X2RuntimeException(
                    new IllegalStateException("The alias " + ALIAS_NINTH_GRADE + " must be defined."));
        }
        m_data.getStudentHelper().getStudentCriteria().addEqualTo(aliasField, cohortYear);


        QueryByCriteria query = m_data.getStudentHelper().getStudentQuery(false);
        QueryIterator students = getBroker().getIteratorByQuery(query);
        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();
                List<StudentEnrollment> enrollments = m_data.getStudentHelper().getStudentEnrollments(student);
                if (enrollments != null && !enrollments.isEmpty()) {
                    StudentEnrollment enrollment = enrollments.iterator().next();
                    if (enrollment.getSchool() == null) {
                        continue;
                    }
                    boolean append = false;
                    String code = enrollment.getEnrollmentCode();
                    String type = "Undefined";
                    if (CODES_DIPLOMA.contains(code)) {
                        if (includeDiploma) {
                            append = true;
                            type = "Diploma";
                        }
                    } else if (CODES_DROPOUT.contains(code)) {
                        if (includeDropout) {
                            append = true;
                            type = "Dropout";
                        }
                    } else if (CODES_ENROLLED.contains(code)) {
                        if (includeEnrolled) {
                            append = true;
                            type = "Enrolled";
                        }
                    } else if (CODES_TRANSFER.contains(code)) {
                        if (includeTransfer) {
                            append = true;
                            type = "Transfer";
                        }
                    } else {
                        append = true;
                    }

                    if (append) {
                        grid.append();
                        grid.set(FIELD_TYPE, type);
                        grid.set(FIELD_STUDENT, student);
                        grid.set(FIELD_ENROLLMENT, enrollment);
                        grid.set(FIELD_STUDENT_NAME, student.getNameView());
                        grid.set(FIELD_SCHOOL_NAME, enrollment.getSchool().getName());
                        grid.set(FIELD_STUDENT, student);
                    }
                }
            }
        } finally {
            students.close();
        }
        String gridSort = (String) getParameter(PARAM_GRID_SORT);
        if (!StringUtils.isEmpty(gridSort)) {
            grid.sort(Arrays.asList(gridSort.split(",")), false);
        }
        grid.beforeTop();
        return grid;
    }

}
