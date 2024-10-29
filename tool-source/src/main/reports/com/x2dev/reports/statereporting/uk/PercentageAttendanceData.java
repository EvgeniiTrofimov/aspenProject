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
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.attendance.uk.UKAttendancePercentageSummary;
import com.x2dev.sis.tools.reports.StudentReportJavaSource;
import com.x2dev.utils.StringUtils;
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
public class PercentageAttendanceData extends StudentReportJavaSource {
    private static final long serialVersionUID = 1L;

    /*
     * Input parameters
     */
    private static final String COUNT_MODE_PARAM = "countMode";
    private static final String DATE_END_PARAM = "endDate";
    private static final String DATE_START_PARAM = "startDate";
    private static final String GROUP_BY_PARAM = "groupBy";
    private static final String PERCENT_MAX_PARAM = "maxPercent";

    // Output parameters
    private static final String MESSAGE_PARAM = "message";
    private static final String PERCENT_MAX_OUT_PARAM = "maxPercentOut";

    /*
     * Grid columns
     */
    private static final String COLUMN_MEMBER_DAYS = "memberDays";
    private static final String COLUMN_PERCENTAGE = "percentage";
    private static final String COLUMN_PRESENT_DAYS = "presentDays";
    private static final String COLUMN_STUDENT = "student";
    private static final String COLUMN_GROUP_BY = "groupBy";

    /*
     * Group by selections
     */
    private static final String GROUP_BY_HOUSE = "house";
    private static final String GROUP_BY_REG = "reg";
    private static final String GROUP_BY_YEAR = "year";
    /*
     * Alias names
     */
    private static final String ALIAS_HOUSE = "HOUSE";
    private static final String ALIAS_YEAR = "key-stage";

    private static final String MODE_STATISTICAL = "statistical";

    private PlainDate m_endDate;
    private PlainDate m_startDate;
    private String m_houseBeanPath;
    private String m_yearBeanPath;

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

        /*
         * get bean path for house field
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
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(1000, 15);

        String maxPercentParam = (String) getParameter(PERCENT_MAX_PARAM);
        String countMode = (String) getParameter(COUNT_MODE_PARAM);
        maxPercentParam = maxPercentParam.replace("%", "");

        try {
            double maxPercent = Double.parseDouble(maxPercentParam);
            addParameter(PERCENT_MAX_OUT_PARAM, Double.valueOf(maxPercent));

            Collection<SisStudent> students = getStudents();
            UKAttendancePercentageSummary ukAttHelper = new UKAttendancePercentageSummary(students, m_startDate,
                    m_endDate, (SisSchool) getSchool(), getBroker());

            for (SisStudent student : students) {
                String studentOid = student.getOid();
                BigDecimal absences = BigDecimal.ZERO;

                if (countMode != null && MODE_STATISTICAL.equals(countMode)) {
                    int statAbsences = ukAttHelper.getAbsentTotalStatistical(studentOid);
                    absences = new java.math.BigDecimal(String.valueOf(statAbsences));
                } else {
                    int totalAbsences = ukAttHelper.getTotalAbsent(studentOid);
                    absences = new java.math.BigDecimal(String.valueOf(totalAbsences));
                }

                /*
                 * Calculate membership information
                 */
                int membershipDays = ukAttHelper.getTotalPossible(studentOid);

                double absent = absences != null ? absences.doubleValue() : 0;

                double present = membershipDays - absent;
                double percent = membershipDays > 0 ? (present / membershipDays) : 0;
                BigDecimal percentDecimal = new BigDecimal(percent * 100.0).setScale(1, RoundingMode.HALF_DOWN);

                // Add the student and attendance tallies to the grid if they are within the
                // percentage range
                if (percentDecimal.doubleValue() <= maxPercent) {
                    grid.append();
                    grid.set(COLUMN_STUDENT, student);
                    grid.set(COLUMN_MEMBER_DAYS, Integer.valueOf(membershipDays));
                    grid.set(COLUMN_PRESENT_DAYS, Double.valueOf(present));
                    grid.set(COLUMN_PERCENTAGE, percentDecimal);
                    grid.set(COLUMN_GROUP_BY, getGroupBy(student));
                }
            }

            String[] sortcolumns = new String[] {COLUMN_GROUP_BY, COLUMN_PERCENTAGE, COLUMN_STUDENT + ".nameView"};
            Boolean[] sortOrder = new Boolean[] {Boolean.valueOf(true), Boolean.valueOf(false), Boolean.valueOf(true)};
            grid.sort(Arrays.asList(sortcolumns), Arrays.asList(sortOrder), true);
        } catch (NumberFormatException nfe) {
            addParameter(MESSAGE_PARAM, "Could not convert max percent input to a number: '" + maxPercentParam + "'");
        }

        grid.beforeTop();
        return grid;
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
        if (GROUP_BY_HOUSE.equals(groupBy) && !StringUtils.isEmpty(m_houseBeanPath)) {
            value = "House " + student.getFieldValueByBeanPath(m_houseBeanPath);
        } else if (GROUP_BY_YEAR.equals(groupBy) && !StringUtils.isEmpty(m_yearBeanPath)) {
            value = "Year " + student.getFieldValueByBeanPath(m_yearBeanPath);
        } else if (GROUP_BY_REG.equals(groupBy)) {
            value = "Reg Group " + student.getHomeroom();
        } else {
            value = "Individual Students";
        }

        return value;
    }

    /**
     * Returns the students to include in this report (in the correct order) based on user input.
     *
     * @return A Collection of Student beans
     */
    private Collection<SisStudent> getStudents() {
        Criteria criteria = buildCriteria();
        QueryByCriteria query = createQueryByCriteria(SisStudent.class, criteria);

        return getBroker().getCollectionByQuery(query);
    }
}
