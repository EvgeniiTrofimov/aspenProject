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

import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ReferenceDescriptionLookup;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.business.attendance.uk.UKAttendancePercentageSummary;
import com.x2dev.sis.tools.reports.StudentReportJavaSource;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
/*
 * ====================================================================
 * Copyright (c) 2002-2012 Follett Software Corporation. All rights reserved. Redistribution and use
 * in source and binary forms, with or without modification, is not permitted without express
 * written agreement from Follett Software Corporation.
 * ====================================================================
 */
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Attendance Summary" report.
 *
 * @author X2 Development Corporation
 */
public class AttendanceSummaryBySessionsData extends StudentReportJavaSource {

    /**
     * Helper class containing the counts for a particular grouping of attendance information.
     */
    class GroupSummary {
        public String grouping;
        public int present = 0;
        public int authorizedAbsences = 0;
        public int unauthorizedAbsences = 0;
        public int possible = 0;
        public Map<String, ValueHolder> detail = new HashMap<String, ValueHolder>();

        /**
         * Calculate percent.
         *
         * @param numerator int
         * @param denominator int
         * @return BigDecimal
         */
        private BigDecimal calculatePercent(int numerator, int denominator) {
            return denominator == 0 ? BigDecimal.ZERO
                    : new BigDecimal((((double) numerator) / ((double) denominator)) * 100);
        }

        /**
         * Gets the present percent.
         *
         * @return Big decimal
         */
        public BigDecimal getPresentPercent() {
            return calculatePercent(present, possible);
        }

        /**
         * Gets the authorized absences percent.
         *
         * @return Big decimal
         */
        public BigDecimal getAuthorizedAbsencesPercent() {
            return calculatePercent(authorizedAbsences, possible);
        }

        /**
         * Gets the unauthorized absences percent.
         *
         * @return Big decimal
         */
        public BigDecimal getUnauthorizedAbsencesPercent() {
            return calculatePercent(unauthorizedAbsences, possible);
        }

        /**
         * Gets the detail percent.
         *
         * @param reason String
         * @return Big decimal
         */
        public BigDecimal getDetailPercent(String reason) {
            return calculatePercent(detail.get(reason).value, possible);
        }
    }

    /**
     * Helper class for int value.
     */
    class ValueHolder {
        public int value;
    }

    private static final String CODE_NOT_COUNTED_ATTENDANCES = "XYZ#";

    private static final long serialVersionUID = 1L;

    /**
     * Group by choices
     */
    private static final String GROUP_BY_REG = "reg";
    private static final String GROUP_BY_YEAR = "year";
    private static final String GROUP_BY_HOUSE = "house";

    /*
     * Input parameters
     */
    private static final String DATE_END_PARAM = "endDate";
    private static final String DATE_START_PARAM = "startDate";
    private static final String GROUP_BY_PARAM = "groupBy";

    /*
     * grid columns
     */
    private static final String COLUMN_PERCENTAGE = "percentage";
    private static final String COLUMN_STUDENT = "student";
    private static final String COLUMN_GROUP_BY = "groupBy";
    private static final String COLUMN_PRESENT = "present";
    private static final String COLUMN_PRESENT_PERCENT = "presentPercent";
    private static final String COLUMN_AUTHORIZED = "authorized";
    private static final String COLUMN_AUTHORIZED_PERCENT = "authorizedPercent";
    private static final String COLUMN_UNAUTHORIZED = "unauthorized";
    private static final String COLUMN_UNAUTHORIZED_PERCENT = "unauthorizedPercent";
    private static final String COLUMN_POSSIBLE = "possible";
    private static final String COLUMN_REASON_CODE = "reasonCode";
    private static final String COLUMN_REASON_CODE_COUNT = "reasonCodeCount";
    private static final String COLUMN_REASON_CODE_PERCENT = "reasonCodePercent";

    /*
     * Alias names
     */
    private static final String ALIAS_HOUSE = "HOUSE";
    private static final String ALIAS_YEAR = "key-stage";

    /*
     * Reference table oids
     */
    private static final String RTB_REASON_OID = "rtbAttReason";

    private PlainDate m_endDate = null;
    private PlainDate m_startDate = null;
    private ReferenceDescriptionLookup m_reasonLookup;
    private String m_houseBeanPath;
    private String m_yearBeanPath;
    private UKAttendancePercentageSummary m_ukAttHelper;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(1000, 15);

        Collection<SisStudent> students = getStudents();
        Map<String, GroupSummary> mapSummaries = new HashMap<String, GroupSummary>();
        m_ukAttHelper = new UKAttendancePercentageSummary(students, m_startDate, m_endDate, (SisSchool) getSchool(),
                getBroker());

        for (SisStudent student : students) {
            String studentOid = student.getOid();

            String groupBy = getGroupBy(student);
            GroupSummary summary = mapSummaries.get(groupBy);
            if (summary == null) {
                summary = new GroupSummary();
                summary.grouping = groupBy;
                mapSummaries.put(groupBy, summary);
            }

            /*
             * Calculate membership information
             */
            int membershipDays = m_ukAttHelper.getTotalPossible(studentOid);

            summarizeStudent(summary, studentOid, m_ukAttHelper.getStudentAttendance(studentOid), membershipDays);
        }
        for (GroupSummary summary : mapSummaries.values()) {
            appendToGrid(grid, summary);
        }


        String[] sortcolumns = new String[] {COLUMN_GROUP_BY, COLUMN_PERCENTAGE, COLUMN_STUDENT + ".nameView"};
        Boolean[] sortOrder = new Boolean[] {Boolean.valueOf(true), Boolean.valueOf(false), Boolean.valueOf(true)};
        grid.sort(Arrays.asList(sortcolumns), Arrays.asList(sortOrder), true);

        grid.beforeTop();
        return grid;
    }

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

        m_reasonLookup = new ReferenceDescriptionLookup(getBroker(), getOrganization());

        /*
         * get bean paths
         */
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());
        DataDictionaryField dictionaryField = dictionary.findDataDictionaryFieldByAlias(ALIAS_HOUSE);
        if (dictionaryField != null) {
            m_houseBeanPath = dictionaryField.getJavaName();
        }

        dictionaryField = dictionary.findDataDictionaryFieldByAlias(ALIAS_YEAR);
        if (dictionaryField != null) {
            m_yearBeanPath = dictionaryField.getJavaName();
        }
    }

    /**
     * Appends the information contained in the group summary to the report grid.
     *
     * @param grid ReportDataGrid
     * @param summary GroupSummary
     */
    private void appendToGrid(ReportDataGrid grid, GroupSummary summary) {
        if (summary != null) {
            for (String code : summary.detail.keySet()) {
                grid.append();
                grid.set(COLUMN_GROUP_BY, summary.grouping);
                grid.set(COLUMN_PRESENT, Integer.valueOf(summary.present));
                grid.set(COLUMN_PRESENT_PERCENT, summary.getPresentPercent());
                grid.set(COLUMN_AUTHORIZED, Integer.valueOf(summary.authorizedAbsences));
                grid.set(COLUMN_AUTHORIZED_PERCENT, summary.getAuthorizedAbsencesPercent());
                grid.set(COLUMN_UNAUTHORIZED, Integer.valueOf(summary.unauthorizedAbsences));
                grid.set(COLUMN_UNAUTHORIZED_PERCENT, summary.getUnauthorizedAbsencesPercent());
                grid.set(COLUMN_POSSIBLE, Integer.valueOf(summary.possible));
                grid.set(COLUMN_REASON_CODE, code);
                grid.set(COLUMN_REASON_CODE_COUNT, Integer.valueOf(summary.detail.get(code).value));
                grid.set(COLUMN_REASON_CODE_PERCENT, summary.getDetailPercent(code));
            }
        }
    }

    /**
     * Checks if an absence code belongs to predefined set of codes.
     *
     * @param code The absence code that is to be checked.
     * @param codeSet The predefined set of codes.
     * @return A boolean of true if the code presented in the set.
     */
    private static boolean codeMatches(String code, String codeSet) {
        return !StringUtils.isEmpty(code) && codeSet.indexOf(code.toUpperCase()) >= 0;
    }

    /**
     * Gets the value of the group by column based on the input selected by the user.
     *
     * @param student SisStudent
     * @return String
     */
    private String getGroupBy(SisStudent student) {
        String value = null;

        String groupBy = (String) getParameter(GROUP_BY_PARAM);
        if (GROUP_BY_HOUSE.equals(groupBy)) {
            value = "House " + student.getFieldValueByBeanPath(m_houseBeanPath);
        } else if (GROUP_BY_YEAR.equals(groupBy)) {
            value = "Year " + student.getFieldValueByBeanPath(m_yearBeanPath);
        } else if (GROUP_BY_REG.equals(groupBy)) {
            value = "Reg Group " + student.getHomeroom();
        } else {
            value = "Whole School";
        }

        return value;
    }

    /**
     * Returns the students to include in this report based on user input.
     *
     * @return A Collection of Student beans
     */
    private Collection<SisStudent> getStudents() {
        Criteria criteria = buildCriteria();
        QueryByCriteria query = createQueryByCriteria(SisStudent.class, criteria);
        return getBroker().getCollectionByQuery(query);
    }

    /**
     * Increments the count for a particular reason code in a group summary.
     *
     * @param summary GroupSummary
     * @param code String
     */
    private void incrementReasonCode(GroupSummary summary, String code) {
        String reason = m_reasonLookup.getDescription(RTB_REASON_OID, code);
        if (!summary.detail.containsKey(reason)) {
            summary.detail.put(reason, new ValueHolder());
        }

        summary.detail.get(reason).value += 1;
    }

    /**
     * Increments the reason code count for a particular group summary
     * based on the AM content of the attendance record.
     *
     * @param summary GroupSummary
     * @param attendance StudentAttendance
     */
    private void summarizeAmReasonCodes(GroupSummary summary, StudentAttendance attendance) {
        if (!codeMatches(attendance.getOtherCode(), CODE_NOT_COUNTED_ATTENDANCES) &&
                (m_ukAttHelper.isAbsent(attendance.getOtherCode())
                        || m_ukAttHelper.isTardy(attendance.getOtherCode()))) {
            String code = attendance.getReasonCode();
            incrementReasonCode(summary, code);
        }
    }

    /**
     * Increments the reason code count for a particular group summary
     * based on the PM content of the attendance record.
     *
     * @param summary GroupSummary
     * @param attendance StudentAttendance
     */
    private void summarizePmReasonCodes(GroupSummary summary, StudentAttendance attendance) {
        if (!codeMatches(attendance.getOtherCode02(), CODE_NOT_COUNTED_ATTENDANCES) &&
                (m_ukAttHelper.isAbsent(attendance.getOtherCode02())
                        || m_ukAttHelper.isTardy(attendance.getOtherCode02()))) {
            String code = attendance.getReasonCode02();
            incrementReasonCode(summary, code);
        }
    }

    /**
     * Updates the overall summary statistics with membership and attendance information for a
     * student.
     *
     * @param summary GroupSummary
     * @param studentOid String
     * @param attendances List<StudentAttendance>
     * @param membershipDays int
     */
    private void summarizeStudent(GroupSummary summary,
                                  String studentOid,
                                  List<StudentAttendance> attendances,
                                  int membershipDays) {
        summary.possible += membershipDays;
        int totalAbsent = 0;
        if (attendances != null) {
            totalAbsent += summarizeStudentAttendances(summary, studentOid, attendances);
        }
        summary.present += (membershipDays - totalAbsent);
    }

    /**
     * Adds summary information for a list of attendance records.
     *
     * @param summary GroupSummary
     * @param studentOid String
     * @param attendances List<StudentAttendance>
     * @return int
     */
    private int summarizeStudentAttendances(GroupSummary summary,
                                            String studentOid,
                                            List<StudentAttendance> attendances) {
        int totalAbsent = m_ukAttHelper.getTotalAbsent(studentOid);
        summary.authorizedAbsences += m_ukAttHelper.getAbsentTotalAuthorized(studentOid);
        summary.unauthorizedAbsences += m_ukAttHelper.getAbsentTotalUnauthorized(studentOid);

        for (StudentAttendance attendance : attendances) {
            summarizeAmReasonCodes(summary, attendance);
            summarizePmReasonCodes(summary, attendance);
        }
        return totalAbsent;
    }
}
