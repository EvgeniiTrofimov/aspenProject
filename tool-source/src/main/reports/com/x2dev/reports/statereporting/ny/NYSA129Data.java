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
package com.x2dev.reports.statereporting.ny;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.procedures.statereporting.ny.NYEnrollmentHelper;
import com.x2dev.procedures.statereporting.ny.NYEnrollmentHelper.NYStudentEnrollmentSpan;
import com.x2dev.procedures.statereporting.ny.NYEnrollmentHelper.NYStudentHistoryHelper;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "NY SA129" report.
 *
 * @author X2 Development Corporation
 */
public class NYSA129Data extends ReportJavaSourceNet {

    /**
     * General Constants
     */
    private static final String ALIAS_SA129_SKL_EXCLUDE = "all-skl-SA129Exclude";
    private static final Number ZERO = Integer.valueOf(0);

    /**
     * Input definition parameter names.
     */
    public static final String PARAM_BREAK_DOWN = "breakDownBy";
    public static final String PARAM_END_DATE = "endDate";
    public static final String PARAM_START_DATE = "startDate";
    public static final String PARAM_QUERY_BY = "queryBy";
    public static final String PARAM_QUERY_STRING = "queryString";
    public static final String REPORT_PARAM_GROUPING = "grouping";
    /**
     * Report Row titles
     */
    protected static final String ROW_A_TITLE = "Possible Aggregate Attendance";
    protected static final String ROW_B_TITLE = "Aggregate Attendance";
    protected static final String ROW_C_TITLE = "Adjusted Aggregate Attendance";
    protected static final String ROW_D_TITLE = "Calculated Adjusted Aggregate Attendance";
    protected static final String ROW_E_TITLE = "Actual Session";
    protected static final String ROW_F_TITLE = "Net Session";
    protected static final String ROW_G_TITLE = "Average Daily Attendance";
    protected static final String ROW_H_TITLE = "Calculated Average Daily Attendance";
    protected static final String ROW_I_TITLE = "Unadjusted Average Daily Attendance";
    protected static final String ROW_J_TITLE = "Calculated Unadjusted Average Daily Attendance";
    protected static final String ROW_K_TITLE = "Calculated Weighted Average Daily Attendance";
    protected static final String ROW_L_TITLE = "Calculated Average Daily Membership";

    /**
     * Grid column constants
     */
    protected static final String FIELD_DATE_END = "dateEnd";
    protected static final String FIELD_DATE_START = "dateStart";
    protected static final String FIELD_COLUMN_GRADE_00 = "00";
    protected static final String FIELD_COLUMN_GRADE_01 = "01";
    protected static final String FIELD_COLUMN_GRADE_02 = "02";
    protected static final String FIELD_COLUMN_GRADE_03 = "03";
    protected static final String FIELD_COLUMN_GRADE_04 = "04";
    protected static final String FIELD_COLUMN_GRADE_05 = "05";
    protected static final String FIELD_COLUMN_GRADE_06 = "06";
    protected static final String FIELD_COLUMN_GRADE_07 = "07";
    protected static final String FIELD_COLUMN_GRADE_08 = "08";
    protected static final String FIELD_COLUMN_GRADE_09 = "09";
    protected static final String FIELD_COLUMN_GRADE_10 = "10";
    protected static final String FIELD_COLUMN_GRADE_11 = "11";
    protected static final String FIELD_COLUMN_GRADE_12 = "12";
    protected static final String FIELD_COLUMN_GROUP_A = "groupA";
    protected static final String FIELD_COLUMN_GROUP_B = "groupB";
    protected static final String FIELD_COLUMN_GROUP_C = "groupC";
    protected static final String FIELD_COLUMN_GROUP_D = "groupD";
    protected static final String FIELD_COLUMN_GROUP_E = "groupE";
    protected static final String FIELD_COLUMN_GROUP_F = "groupF";
    protected static final String FIELD_COLUMN_GROUP_TOTAL = "total";
    protected static final String FIELD_GROUP_TYPE = "groupType";

    /**
     * Variables
     */
    protected DistrictSchoolYearContext m_context;
    protected PlainDate m_endDate = null;
    protected List<String> m_grades = new ArrayList<>();
    protected int m_gradesNum = 13;
    protected Boolean m_grouping;
    protected PlainDate m_startDate = null;
    private HashSet<String> m_excludedSchools = new HashSet<String>();
    private Map<String, Integer> m_gradeAttendance = new HashMap<String, Integer>();
    private Map<String, Integer> m_gradeEnrollment = new HashMap<String, Integer>();
    private static NumberFormat m_numberFormat = NumberFormat.getNumberInstance();
    private static NumberFormat m_percentFormat = NumberFormat.getPercentInstance();
    private StudentStatistics m_statistics;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        ReportDataGrid grid = null;
        X2Broker broker = getBroker();
        if (!initializeFields()) {
            loadExcludedSchools(broker);

            m_statistics = new StudentStatistics();
            m_statistics.setBroker(broker);
            m_statistics.setPrivilegeSet(getPrivilegeSet());
            m_statistics.setOrganization(getOrganization());
            m_statistics.setSchoolContext(false);
            m_statistics.setParameters(getParameters());
            m_statistics.initializeExport();
            m_statistics.getStudentHistoryHelper().setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE,
                    m_startDate);
            m_statistics.getStudentHistoryHelper().setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE,
                    m_endDate);
            m_grouping = Boolean.valueOf("0".equals(getParameter(PARAM_BREAK_DOWN)));
            addParameter(REPORT_PARAM_GROUPING, m_grouping);
            grid = populateGrid(broker);
        }
        return grid;
    }

    /**
     * Initialize Fields.
     *
     * @return true, if successful
     */
    protected boolean initializeFields() {
        boolean haveErrors = false;
        m_percentFormat.setMaximumFractionDigits(2);
        m_startDate = (PlainDate) getParameter(PARAM_START_DATE);
        m_endDate = (PlainDate) getParameter(PARAM_END_DATE);

        if (m_startDate.before(m_endDate)) {
            X2Criteria ctxCriteria = new X2Criteria();
            ctxCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, m_startDate);
            ctxCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, m_startDate);

            m_context = (DistrictSchoolYearContext) getBroker()
                    .getBeanByQuery(new QueryByCriteria(DistrictSchoolYearContext.class, ctxCriteria));
        } else {
            haveErrors = true;
        }

        return haveErrors;
    }

    /**
     * Loads additional schools to be excluded specific to the NYSA 129 reports.
     *
     * @param broker X2Broker
     */
    private void loadExcludedSchools(X2Broker broker) {
        X2Criteria excludeSchoolCriteria = new X2Criteria();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(broker.getPersistenceKey());
        DataDictionaryField schoolExclusionField = dictionary.findDataDictionaryFieldByAlias(ALIAS_SA129_SKL_EXCLUDE);

        if (schoolExclusionField != null) {
            excludeSchoolCriteria.addEqualTo(schoolExclusionField.getJavaName(), BooleanAsStringConverter.TRUE);

            QueryByCriteria excludedSchoolQuery = new QueryByCriteria(SisSchool.class, excludeSchoolCriteria);
            QueryIterator schools = broker.getIteratorByQuery(excludedSchoolQuery);
            try {
                while (schools.hasNext()) {
                    SisSchool school = (SisSchool) schools.next();
                    m_excludedSchools.add(school.getOid());
                }
            } finally {
                if (schools != null) {
                    schools.close();
                }
            }
        }
    }

    /**
     * Build student report grid.
     *
     * @param broker X2Broker
     * @return ReportDataGrid
     */
    private ReportDataGrid populateGrid(X2Broker broker) {
        ReportDataGrid grid = new ReportDataGrid(10000, 20);
        int schoolYear = m_context.getSchoolYear();
        QueryIterator students =
                broker.getIteratorByQuery(m_statistics.getStudentHistoryHelper().getStudentQuery(false));

        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();
                List<NYStudentEnrollmentSpan> spans =
                        m_statistics.getStudentHistoryHelper().getNYStudentEnrollmentSpans(student, true);
                for (NYStudentEnrollmentSpan span : spans) {
                    SisSchool spanSchool = span.getSchool();
                    if (spanSchool != null && !m_excludedSchools.contains(spanSchool.getOid())) {
                        if (isSchoolContext()) {
                            School selectedSchool = getSchool();
                            if (selectedSchool != null && selectedSchool.equals(spanSchool)) {
                                processSpan(schoolYear, span);
                            }
                        } else {
                            processSpan(schoolYear, span);
                        }
                    }
                }
            }
        } finally {
            if (students != null) {
                students.close();
            }
        }

        // Possible Aggregate Attendance
        grid.append(processGridRow(
                ZERO,
                ZERO,
                Integer.valueOf(calculatePossibleAttendance(FIELD_COLUMN_GRADE_00)),
                Integer.valueOf(calculatePossibleAttendance(FIELD_COLUMN_GRADE_01)),
                Integer.valueOf(calculatePossibleAttendance(FIELD_COLUMN_GRADE_02)),
                Integer.valueOf(calculatePossibleAttendance(FIELD_COLUMN_GRADE_03)),
                Integer.valueOf(calculatePossibleAttendance(FIELD_COLUMN_GRADE_04)),
                Integer.valueOf(calculatePossibleAttendance(FIELD_COLUMN_GRADE_05)),
                Integer.valueOf(calculatePossibleAttendance(FIELD_COLUMN_GRADE_06)),
                Integer.valueOf(calculatePossibleAttendance(FIELD_COLUMN_GRADE_07)),
                Integer.valueOf(calculatePossibleAttendance(FIELD_COLUMN_GRADE_08)),
                Integer.valueOf(calculatePossibleAttendance(FIELD_COLUMN_GRADE_09)),
                Integer.valueOf(calculatePossibleAttendance(FIELD_COLUMN_GRADE_10)),
                Integer.valueOf(calculatePossibleAttendance(FIELD_COLUMN_GRADE_11)),
                Integer.valueOf(calculatePossibleAttendance(FIELD_COLUMN_GRADE_12)),
                ROW_A_TITLE));

        // Aggregate Attendance
        grid.append(processGridRow(
                ZERO,
                ZERO,
                Integer.valueOf(calculateAttendance(FIELD_COLUMN_GRADE_00)),
                Integer.valueOf(calculateAttendance(FIELD_COLUMN_GRADE_01)),
                Integer.valueOf(calculateAttendance(FIELD_COLUMN_GRADE_02)),
                Integer.valueOf(calculateAttendance(FIELD_COLUMN_GRADE_03)),
                Integer.valueOf(calculateAttendance(FIELD_COLUMN_GRADE_04)),
                Integer.valueOf(calculateAttendance(FIELD_COLUMN_GRADE_05)),
                Integer.valueOf(calculateAttendance(FIELD_COLUMN_GRADE_06)),
                Integer.valueOf(calculateAttendance(FIELD_COLUMN_GRADE_07)),
                Integer.valueOf(calculateAttendance(FIELD_COLUMN_GRADE_08)),
                Integer.valueOf(calculateAttendance(FIELD_COLUMN_GRADE_09)),
                Integer.valueOf(calculateAttendance(FIELD_COLUMN_GRADE_10)),
                Integer.valueOf(calculateAttendance(FIELD_COLUMN_GRADE_11)),
                Integer.valueOf(calculateAttendance(FIELD_COLUMN_GRADE_12)),
                ROW_B_TITLE));

        // Adjusted Aggregate Attendance
        grid.append(processGridRow(
                ZERO, // groupA
                ZERO, // groupB
                ZERO, // grade 00
                ZERO, // grade 01
                ZERO, // grade 02
                ZERO, // grade 03
                ZERO, // grade 04
                ZERO, // grade 05
                ZERO, // grade 06
                ZERO, // grade 07
                ZERO, // grade 08
                ZERO, // grade 09
                ZERO, // grade 10
                ZERO, // grade 11
                ZERO, // grade 12
                ROW_C_TITLE));

        // Calculated Adjusted Aggregate Attendance
        grid.append(processGridRow(
                ZERO, // groupA
                ZERO, // groupB
                ZERO, // grade 00
                ZERO, // grade 01
                ZERO, // grade 02
                ZERO, // grade 03
                ZERO, // grade 04
                ZERO, // grade 05
                ZERO, // grade 06
                ZERO, // grade 07
                ZERO, // grade 08
                ZERO, // grade 09
                ZERO, // grade 10
                ZERO, // grade 11
                ZERO, // grade 12
                ROW_D_TITLE));

        // Actual Session
        grid.append(processGridRow(
                ZERO, // groupA
                ZERO, // groupB
                ZERO, // grade 00
                ZERO, // grade 01
                ZERO, // grade 02
                ZERO, // grade 03
                ZERO, // grade 04
                ZERO, // grade 05
                ZERO, // grade 06
                ZERO, // grade 07
                ZERO, // grade 08
                ZERO, // grade 09
                ZERO, // grade 10
                ZERO, // grade 11
                ZERO, // grade 12
                ROW_E_TITLE));

        // Net Session
        grid.append(processGridRow(
                ZERO, // groupA
                ZERO, // groupB
                ZERO, // grade 00
                ZERO, // grade 01
                ZERO, // grade 02
                ZERO, // grade 03
                ZERO, // grade 04
                ZERO, // grade 05
                ZERO, // grade 06
                ZERO, // grade 07
                ZERO, // grade 08
                ZERO, // grade 09
                ZERO, // grade 10
                ZERO, // grade 11
                ZERO, // grade 12
                ROW_F_TITLE));

        double grade00ADA = calculateADA(FIELD_COLUMN_GRADE_00);
        double grade01ADA = calculateADA(FIELD_COLUMN_GRADE_01);
        double grade02ADA = calculateADA(FIELD_COLUMN_GRADE_02);
        double grade03ADA = calculateADA(FIELD_COLUMN_GRADE_03);
        double grade04ADA = calculateADA(FIELD_COLUMN_GRADE_04);
        double grade05ADA = calculateADA(FIELD_COLUMN_GRADE_05);
        double grade06ADA = calculateADA(FIELD_COLUMN_GRADE_06);
        double grade07ADA = calculateADA(FIELD_COLUMN_GRADE_07);
        double grade08ADA = calculateADA(FIELD_COLUMN_GRADE_08);
        double grade09ADA = calculateADA(FIELD_COLUMN_GRADE_09);
        double grade10ADA = calculateADA(FIELD_COLUMN_GRADE_10);
        double grade11ADA = calculateADA(FIELD_COLUMN_GRADE_11);
        double grade12ADA = calculateADA(FIELD_COLUMN_GRADE_12);
        // Average Daily Attendance
        grid.append(processGridRow(
                ZERO, // groupA
                ZERO, // groupB
                Double.valueOf(grade00ADA),
                Double.valueOf(grade01ADA),
                Double.valueOf(grade02ADA),
                Double.valueOf(grade03ADA),
                Double.valueOf(grade04ADA),
                Double.valueOf(grade05ADA),
                Double.valueOf(grade06ADA),
                Double.valueOf(grade07ADA),
                Double.valueOf(grade08ADA),
                Double.valueOf(grade09ADA),
                Double.valueOf(grade10ADA),
                Double.valueOf(grade11ADA),
                Double.valueOf(grade12ADA),
                ROW_G_TITLE));

        // Calculated Average Daily Attendance
        grid.append(processGridRow(
                ZERO, // groupA
                ZERO, // groupB
                ZERO, // grade 00
                ZERO, // grade 01
                ZERO, // grade 02
                ZERO, // grade 03
                ZERO, // grade 04
                ZERO, // grade 05
                ZERO, // grade 06
                ZERO, // grade 07
                ZERO, // grade 08
                ZERO, // grade 09
                ZERO, // grade 10
                ZERO, // grade 11
                ZERO, // grade 12
                ROW_H_TITLE));

        // Unadjusted Average Daily Attendance
        grid.append(processGridRow(
                ZERO, // groupA
                ZERO, // groupB
                ZERO, // grade 00
                ZERO, // grade 01
                ZERO, // grade 02
                ZERO, // grade 03
                ZERO, // grade 04
                ZERO, // grade 05
                ZERO, // grade 06
                ZERO, // grade 07
                ZERO, // grade 08
                ZERO, // grade 09
                ZERO, // grade 10
                ZERO, // grade 11
                ZERO, // grade 12
                ROW_I_TITLE));

        // Calculated Unadjusted Average Daily Attendance
        grid.append(processGridRow(
                ZERO, // groupA
                ZERO, // groupB
                ZERO, // grade 00
                ZERO, // grade 01
                ZERO, // grade 02
                ZERO, // grade 03
                ZERO, // grade 04
                ZERO, // grade 05
                ZERO, // grade 06
                ZERO, // grade 07
                ZERO, // grade 08
                ZERO, // grade 09
                ZERO, // grade 10
                ZERO, // grade 11
                ZERO, // grade 12
                ROW_J_TITLE));

        // Calculated Weighted Average Daily Attendance
        grid.append(processGridRow(
                ZERO, // groupA
                ZERO, // groupB
                ZERO, // grade 00
                ZERO, // grade 01
                ZERO, // grade 02
                ZERO, // grade 03
                ZERO, // grade 04
                ZERO, // grade 05
                ZERO, // grade 06
                ZERO, // grade 07
                ZERO, // grade 08
                ZERO, // grade 09
                ZERO, // grade 10
                ZERO, // grade 11
                ZERO, // grade 12
                ROW_K_TITLE));

        // Calculated Average Daily Membership
        grid.append(processGridRow(
                ZERO, // groupA
                ZERO, // groupB
                ZERO, // grade 00
                ZERO, // grade 01
                ZERO, // grade 02
                ZERO, // grade 03
                ZERO, // grade 04
                ZERO, // grade 05
                ZERO, // grade 06
                ZERO, // grade 07
                ZERO, // grade 08
                ZERO, // grade 09
                ZERO, // grade 10
                ZERO, // grade 11
                ZERO, // grade 12
                ROW_L_TITLE));

        grid.beforeTop();

        return grid;
    }

    /**
     * Process span.
     *
     * @param schoolYear int
     * @param span NYStudentEnrollmentSpan
     */
    private void processSpan(int schoolYear, NYStudentEnrollmentSpan span) {
        int spanGrade = 12 - (span.getYog() - schoolYear);
        String gradeLevel = String.format("%02d", Integer.valueOf(spanGrade));
        Integer membershipDaysByGrade = m_gradeEnrollment.get(gradeLevel);
        if (membershipDaysByGrade == null) {
            membershipDaysByGrade = Integer.valueOf(0);
        }
        m_gradeEnrollment.put(gradeLevel,
                Integer.valueOf(membershipDaysByGrade.intValue() + span.getMembershipDays(m_startDate, m_endDate)));
        m_grades.add(gradeLevel);
        Integer absentDaysByGrade = m_gradeAttendance.get(gradeLevel);
        if (absentDaysByGrade == null) {
            absentDaysByGrade = Integer.valueOf(0);
        }
        m_gradeAttendance.put(gradeLevel,
                Integer.valueOf(absentDaysByGrade.intValue() + span.getAttendanceDays(m_startDate, m_endDate)));
    }

    /**
     * Insert row grids to main grid.
     *
     * @param groupA String
     * @param groupB String
     * @param groupC String
     * @param groupD String
     * @param groupE String
     * @param groupF String
     * @param totals String
     * @param groupType String
     * @return ReportDataGrid
     */
    private ReportDataGrid processGridRow(Number groupA,
                                          Number groupB,
                                          Number grade00,
                                          Number grade01,
                                          Number grade02,
                                          Number grade03,
                                          Number grade04,
                                          Number grade05,
                                          Number grade06,
                                          Number grade07,
                                          Number grade08,
                                          Number grade09,
                                          Number grade10,
                                          Number grade11,
                                          Number grade12,
                                          String groupType) {
        ReportDataGrid grid = new ReportDataGrid(10000, 20);
        /*
         * Add the student and attendance counts to the grid
         */
        grid.append();
        grid.set(FIELD_DATE_END, m_endDate);
        grid.set(FIELD_DATE_START, m_startDate);
        grid.set(FIELD_GROUP_TYPE, groupType);
        grid.set(FIELD_COLUMN_GROUP_A, m_numberFormat.format(groupA));
        grid.set(FIELD_COLUMN_GROUP_B, m_numberFormat.format(groupB));
        NumberFormat numFormat = !ROW_G_TITLE.equals(groupType) ? m_numberFormat : m_percentFormat;
        if (m_grouping.booleanValue()) {
            if (!ROW_G_TITLE.equals(groupType)) {
                grid.set(FIELD_COLUMN_GROUP_C,
                        m_numberFormat.format(
                                grade00.intValue() + grade01.intValue() + grade02.intValue() + grade03.intValue()));
                grid.set(FIELD_COLUMN_GROUP_D,
                        m_numberFormat.format(grade04.intValue() + grade05.intValue() + grade06.intValue()));
                grid.set(FIELD_COLUMN_GROUP_E,
                        m_numberFormat.format(grade07.intValue() + grade08.intValue()));
                grid.set(FIELD_COLUMN_GROUP_F,
                        m_numberFormat.format(
                                grade09.intValue() + grade10.intValue() + grade11.intValue() + grade12.intValue()));
            } else {
                grid.set(FIELD_COLUMN_GROUP_C,
                        m_percentFormat.format((grade00.doubleValue() + grade01.doubleValue() + grade02.doubleValue()
                                + grade03.doubleValue()) / 4.0));
                grid.set(FIELD_COLUMN_GROUP_D,
                        m_percentFormat
                                .format((grade04.doubleValue() + grade05.doubleValue() + grade06.doubleValue()) / 3.0));
                grid.set(FIELD_COLUMN_GROUP_E,
                        m_percentFormat.format((grade07.doubleValue() + grade08.doubleValue()) / 2.0));
                grid.set(FIELD_COLUMN_GROUP_F,
                        m_percentFormat.format((grade09.doubleValue() + grade10.doubleValue() + grade11.doubleValue()
                                + grade12.doubleValue()) / 4.0));

            }
        } else {
            grid.set(FIELD_COLUMN_GRADE_00, numFormat.format(grade00));
            grid.set(FIELD_COLUMN_GRADE_01, numFormat.format(grade01));
            grid.set(FIELD_COLUMN_GRADE_02, numFormat.format(grade02));
            grid.set(FIELD_COLUMN_GRADE_03, numFormat.format(grade03));
            grid.set(FIELD_COLUMN_GRADE_04, numFormat.format(grade04));
            grid.set(FIELD_COLUMN_GRADE_05, numFormat.format(grade05));
            grid.set(FIELD_COLUMN_GRADE_06, numFormat.format(grade06));
            grid.set(FIELD_COLUMN_GRADE_07, numFormat.format(grade07));
            grid.set(FIELD_COLUMN_GRADE_08, numFormat.format(grade08));
            grid.set(FIELD_COLUMN_GRADE_09, numFormat.format(grade09));
            grid.set(FIELD_COLUMN_GRADE_10, numFormat.format(grade10));
            grid.set(FIELD_COLUMN_GRADE_11, numFormat.format(grade11));
            grid.set(FIELD_COLUMN_GRADE_12, numFormat.format(grade12));
        }
        if (!ROW_G_TITLE.equals(groupType)) {
            grid.set(FIELD_COLUMN_GROUP_TOTAL,
                    m_numberFormat.format(grade00.intValue() + grade01.intValue() + grade02.intValue()
                            + grade03.intValue()
                            + grade04.intValue() + grade05.intValue() + grade06.intValue() + grade07.intValue()
                            + grade08.intValue() + grade09.intValue() + grade10.intValue() + grade11.intValue()
                            + grade12.intValue()));
        } else {
            grid.set(FIELD_COLUMN_GROUP_TOTAL,
                    m_percentFormat.format((grade00.doubleValue() + grade01.doubleValue() + grade02.doubleValue()
                            + grade03.doubleValue() + grade04.doubleValue() + grade05.doubleValue()
                            + grade06.doubleValue() + grade07.doubleValue() + grade08.doubleValue()
                            + grade09.doubleValue() + grade10.doubleValue() + grade11.doubleValue()
                            + grade12.doubleValue()) / m_gradesNum));
        }
        return grid;
    }

    /**
     * Calculate Average daily attendance for the given grade.
     *
     * @param gradeColumn
     * @return
     */
    private double calculateADA(String gradeColumn) {
        if (m_gradeEnrollment.get(gradeColumn) != null && m_gradeEnrollment.get(gradeColumn).intValue() != 0) {
            return (m_gradeEnrollment.get(gradeColumn).doubleValue()
                    - m_gradeAttendance.get(gradeColumn).doubleValue())
                    / m_gradeEnrollment.get(gradeColumn).doubleValue();
        }
        m_gradesNum -= 1;
        return 0.0;
    }

    /**
     * Calculate Attendance for the given grade.
     *
     * @param gradeColumn
     * @return
     */
    private int calculateAttendance(String gradeColumn) {
        int value = 0;
        if (m_gradeEnrollment.containsKey(gradeColumn) && m_gradeAttendance.containsKey(gradeColumn)) {
            value += (m_gradeEnrollment.get(gradeColumn).intValue() - m_gradeAttendance.get(gradeColumn).intValue());
        }

        return value;
    }

    /**
     * Calculate possible attendance for the given grade.
     *
     * @param gradeColumn
     * @return
     */
    private int calculatePossibleAttendance(String gradeColumn) {
        int value = 0;
        if (m_gradeEnrollment.containsKey(gradeColumn)) {
            value += (m_gradeEnrollment.get(gradeColumn).intValue());
        }
        return value;
    }

    /**
     * State Report Data class for gathering data, using NYEnrollmentHelper, calculating enrollment
     * history.
     * Days not in membership should be reported as empty.
     *
     * @author Follett School Solutions
     */
    class StudentStatistics extends StateReportData {
        /**
         * Instance variables.
         */
        private NYEnrollmentHelper m_enrollmentHelper;

        /**
         * Initialize the export.
         * Set up the student history helper.
         */
        @Override
        public void initialize() {

            m_enrollmentHelper = new NYEnrollmentHelper(m_startDate, m_endDate, this);
            setCurrentContext(m_context);

            setQuery(m_enrollmentHelper.getStudentHistoryHelper().getStudentQuery(false));
        }

        /**
         * Gets the student history helper.
         *
         * @return NY student history helper
         */
        public NYStudentHistoryHelper getStudentHistoryHelper() {
            return m_enrollmentHelper.getStudentHistoryHelper();
        }
    }

}
