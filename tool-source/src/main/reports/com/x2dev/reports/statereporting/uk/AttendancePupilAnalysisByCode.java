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
public class AttendancePupilAnalysisByCode extends StudentReportJavaSource {

    private static final long serialVersionUID = 1L;

    /**
     * Parameters
     */
    private static final String END_DATE_PARAM = "endDate";
    private static final String GROUP_BY_PARAM = "groupBy";
    private static final String MAX_SESSION_COUNT_PARAM = "mapMaxSessionCount";
    private static final String START_DATE_PARAM = "startDate";

    /*
     * grid field names
     */
    private static final String FSlASH_ENTRY_FIELD = "entryCode_FSlash";
    private static final String BSLASH_ENTRY_FIELD = "entryCode_BSlash";
    private static final String GROUP_BY_FIELD = "groupBy";
    private static final String POSSIBLE_DAYS_FIELD = "possibleDays";
    private static final String REG_FIELD = "reg";
    private static final String SESSION_COUNT_FIELD = "sessionCount";
    private static final String STUDENT_FIELD = "student";
    private static final String STUDENT_NAME_FIELD = "studentName";

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

    /**
     * The report is expected "entryCode_" + the attendance code as the key.
     */
    private static final String ENTRY_CODE = "entryCode_";

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

        boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
        UKAttendancePercentageSummary ukAttHelper = new UKAttendancePercentageSummary(students, m_startDate, m_endDate,
                (SisSchool) getSchool(), getBroker(), activeOnly);

        for (SisStudent student : students) {
            String studentOid = student.getOid();

            int membershipTotal = ukAttHelper.getTotalPossible(studentOid);
            int membershipPerSession = ukAttHelper.getTotalDaysInSession(studentOid);

            if (membershipTotal > 0) {
                Map<String, Map<String, Integer>> codeCounts = ukAttHelper.getAttendanceCodeMap(studentOid);
                grid.append();

                int totalAmCodeCount = 0;
                int totalPmCodeCount = 0;

                if (codeCounts != null) {
                    /*
                     * Add the student and attendance counts to the grid
                     */
                    for (String key : codeCounts.keySet()) {
                        Integer amCount = codeCounts.get(key).get(UKAttendancePercentageSummary.AM_KEY);
                        totalAmCodeCount = totalAmCodeCount + (amCount != null ? amCount.intValue() : 0);
                        Integer pmCount = codeCounts.get(key).get(UKAttendancePercentageSummary.PM_KEY);
                        totalPmCodeCount = totalPmCodeCount + (pmCount != null ? pmCount.intValue() : 0);

                        Integer totalCount =
                                Integer.valueOf((amCount != null ? amCount.intValue() : 0) +
                                        (pmCount != null ? pmCount.intValue() : 0));
                        grid.set(ENTRY_CODE + key, totalCount);
                    }
                    codeCounts.clear();
                }

                String groupByField = getGroupBy(student);
                grid.set(STUDENT_FIELD, student);
                grid.set(GROUP_BY_FIELD, groupByField);
                grid.set(STUDENT_NAME_FIELD, student.getNameView());
                grid.set(REG_FIELD, student.getHomeroom());

                int fslashTotal = (membershipPerSession - totalAmCodeCount);
                grid.set(FSlASH_ENTRY_FIELD, Integer.valueOf(fslashTotal));
                grid.set(SESSION_COUNT_FIELD, Integer.valueOf(membershipTotal));

                /*
                 * Accumulate max value for report header
                 */
                Integer maxCount = mapMaxSessionCount.get(groupByField);
                if (maxCount == null) {
                    mapMaxSessionCount.put(groupByField, Integer.valueOf(membershipTotal));
                } else if (maxCount.intValue() < membershipTotal) {
                    mapMaxSessionCount.put(groupByField, Integer.valueOf(membershipTotal));
                }

                int bslashTotal = (membershipPerSession - totalPmCodeCount);
                grid.set(BSLASH_ENTRY_FIELD, Integer.valueOf(bslashTotal));

                // the UK tracks by session, 2 sessions = 1 day.
                grid.set(POSSIBLE_DAYS_FIELD, Integer.valueOf(membershipTotal));
            }
        }

        addParameter(MAX_SESSION_COUNT_PARAM, mapMaxSessionCount);

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

        /**
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
     * @return A QueryIterator of Student beans
     */
    private Collection<SisStudent> getStudents() {
        Criteria studentCriteria = buildCriteria();
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
