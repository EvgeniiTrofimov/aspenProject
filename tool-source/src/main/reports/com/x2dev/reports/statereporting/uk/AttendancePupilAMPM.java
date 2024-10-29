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
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Student Membership" report.
 *
 * @author X2 Development Corporation
 */
public class AttendancePupilAMPM extends StudentReportJavaSource {
    private static final long serialVersionUID = 1L;

    /*
     * Parameters
     */
    private static final String END_DATE_PARAM = "endDate";
    private static final String GROUP_BY_PARAM = "groupBy";
    private static final String MAX_SESSION_COUNT_PARAM = "mapMaxSessionCount";
    private static final String START_DATE_PARAM = "startDate";

    /*
     * Grid field names
     */
    private static final String AEA_AM_FIELD = "aeaAM";
    private static final String AEA_PM_FIELD = "aeaPM";
    private static final String AM_PERCENTAGE_FIELD = "amPercent";
    private static final String AUTHORISED_AM_FIELD = "authorisedAM";
    private static final String AUTHORISED_PM_FIELD = "authorisedPM";
    private static final String GROUP_BY_FIELD = "groupBy";
    private static final String PM_PERCENTAGE_FIELD = "pmPercent";
    private static final String PRESENT_AM_FIELD = "presentAM";
    private static final String PRESENT_PM_FIELD = "presentPM";
    private static final String REG_FIELD = "reg";
    private static final String SESSION_COUNT_FIELD = "sessionCount";
    private static final String STUDENT_NAME_FIELD = "studentName";
    private static final String UNAUTHORISED_AM_FIELD = "unauthorisedAM";
    private static final String UNAUTHORISED_PM_FIELD = "unauthorisedPM";

    /*
     * Alias names
     */
    private static final String ALIAS_HOUSE = "HOUSE";
    private static final String ALIAS_YEAR = "key-stage";
    /*
     * Group by selections
     */
    private static final String GROUP_BY_HOUSE = "house";
    private static final String GROUP_BY_REG = "reg";
    private static final String GROUP_BY_YEAR = "year";

    private PlainDate m_endDate = null;
    private PlainDate m_startDate = null;

    private String m_houseBeanPath;
    private String m_yearBeanPath;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(1000, 15);

        Map<String, Integer> mapMaxSessionCount = new HashMap<String, Integer>();
        Collection<SisStudent> students = getStudents();

        UKAttendancePercentageSummary ukAttHelper = new UKAttendancePercentageSummary(students, m_startDate, m_endDate,
                (SisSchool) getSchool(), getBroker());

        for (SisStudent student : students) {
            grid.append();
            double amPercentage = 0.0;
            double pmPercentage = 0.0;

            String studentOid = student.getOid();

            /*
             * Total attendance data, if any
             */
            int membershipTotal = ukAttHelper.getTotalPossible(studentOid);
            int membershipPerSession = ukAttHelper.getTotalDaysInSession(studentOid); // total
                                                                                      // possible
                                                                                      // per session

            int amAuthorizedCount = ukAttHelper.getAbsentAMAuthorized(studentOid);
            int amUnauthorizedCount = ukAttHelper.getAbsentAMUnauthorized(studentOid);
            int amAbsentCount = ukAttHelper.getTotalAbsentAM(studentOid);
            int amAEACount = ukAttHelper.getAeaAM(studentOid);

            int pmAuthorizedCount = ukAttHelper.getAbsentPMAuthorized(studentOid);
            int pmUnauthorizedCount = ukAttHelper.getAbsentPMUnauthorized(studentOid);
            int pmAbsentCount = ukAttHelper.getTotalAbsentPM(studentOid);
            int pmAEACount = ukAttHelper.getAeaPM(studentOid);

            String groupByField = getGroupBy(student);
            grid.set(STUDENT_NAME_FIELD, student.getNameView());
            grid.set(REG_FIELD, student.getHomeroom());
            grid.set(PRESENT_AM_FIELD, Integer.valueOf(membershipPerSession - amAbsentCount));
            grid.set(PRESENT_PM_FIELD, Integer.valueOf(membershipPerSession - pmAbsentCount));
            grid.set(AEA_AM_FIELD, Integer.valueOf(amAEACount));
            grid.set(AEA_PM_FIELD, Integer.valueOf(pmAEACount));
            grid.set(AUTHORISED_AM_FIELD, Integer.valueOf(amAuthorizedCount));
            grid.set(AUTHORISED_PM_FIELD, Integer.valueOf(pmAuthorizedCount));
            grid.set(UNAUTHORISED_AM_FIELD, Integer.valueOf(amUnauthorizedCount));
            grid.set(UNAUTHORISED_PM_FIELD, Integer.valueOf(pmUnauthorizedCount));
            grid.set(GROUP_BY_FIELD, getGroupBy(student));

            if (membershipTotal != 0) {
                double totalDays = membershipPerSession;
                double amAbsCount = amAbsentCount;
                amPercentage = ((((totalDays - amAbsCount)) / totalDays) * 100);
                amPercentage = (double) Math.round(amPercentage * 100) / 100;
                grid.set(AM_PERCENTAGE_FIELD, Double.valueOf(amPercentage));

                pmPercentage = (((totalDays - pmAbsentCount) / totalDays) * 100);
                pmPercentage = (double) Math.round(pmPercentage * 100) / 100;
                grid.set(PM_PERCENTAGE_FIELD, Double.valueOf(pmPercentage));
            }

            // the UK tracks by session, 2 sessions = 1 day. Membership total is based on 2 session
            // per day.
            grid.set(SESSION_COUNT_FIELD, Integer.valueOf(membershipPerSession));

            /*
             * Accumulate max value for report header
             */
            Integer maxCount = mapMaxSessionCount.get(groupByField);
            if (maxCount == null) {
                mapMaxSessionCount.put(groupByField, Integer.valueOf(membershipTotal));
            } else if (maxCount.intValue() < membershipTotal) {
                mapMaxSessionCount.put(groupByField, Integer.valueOf(membershipTotal));
            }
            // reset counts
            amAbsentCount = 0;
            pmAbsentCount = 0;
        }

        this.addParameter(MAX_SESSION_COUNT_PARAM, mapMaxSessionCount);
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

        m_startDate = (PlainDate) getParameter(START_DATE_PARAM);
        m_endDate = (PlainDate) getParameter(END_DATE_PARAM);

        /*
         * Get bean paths
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
            value = "Individual Students";
        }

        return value;
    }

    /**
     * Returns the students to include in this report (in the correct order) based on user input.
     *
     * @return A QueryIterator of Student beans
     */
    private Collection<SisStudent> getStudents() {
        Criteria studentCriteria = buildCriteria();

        com.follett.fsc.core.k12.web.AppGlobals.getLog().log(java.util.logging.Level.INFO, "");

        QueryByCriteria query = createQueryByCriteria(SisStudent.class, studentCriteria);

        String groupBy = (String) getParameter(GROUP_BY_PARAM);
        if (groupBy.endsWith(GROUP_BY_HOUSE) && !StringUtils.isEmpty(m_houseBeanPath)) {
            query.addOrderByAscending(m_houseBeanPath);
        } else if (groupBy.endsWith(GROUP_BY_YEAR) && !StringUtils.isEmpty(m_yearBeanPath)) {
            query.addOrderByAscending(m_yearBeanPath);
        } else if (groupBy.endsWith(GROUP_BY_REG)) {
            query.addOrderByAscending(SisStudent.COL_HOMEROOM);
        }

        String sort = (String) getParameter(SORT_PARAM);
        applyUserSort(query, sort);

        return getBroker().getCollectionByQuery(query);
    }
}
