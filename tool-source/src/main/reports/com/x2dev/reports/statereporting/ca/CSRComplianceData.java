/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.ca;

import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.ca.CSRDataHelper;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Set;

/**
 * Data source for "CA Class Size Reduction (CSR) Compliance" report .
 *
 * @author X2 Development Corporation
 */
public class CSRComplianceData extends ReportJavaSourceNet {
    private static final String ELEMENTARY = "ELEMENTARY";

    private static final String FIELD_DAY_AVERAGE = "dayAverage";
    private static final String FIELD_GRADE = "grade";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_SECTION = "section";
    private static final String FIELD_TOTAL_DAYS = "totalDays";
    private static final String FIELD_TOTAL_ENROLLMENT = "totalEnrollment";
    private static final String FIELD_YTD_AVERAGE = "ytdAverage";

    private static final String HOMEROOM_ONLY = "HOMEROOM";

    private static final String PARAM_DAY_END = "dayEnd";
    private static final String PARAM_DAY_START = "dayStart";
    private static final String PARAM_HOMEROOM_ONLY = "applyFilter";
    private static final String PARAM_GENERATED_BY = "generatedBy";
    private static final String PARAM_GRADE = "grade";

    private PlainDate m_endDate = null;
    private String m_grade = null;
    private boolean m_homeroomOnly = false;
    private PlainDate m_startDate = null;

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

        CSRDataHelper data = null;
        data = new CSRDataHelper();
        data.setBroker(getBroker());
        data.setOrganization(getOrganization());
        data.setPrivilegeSet(getPrivilegeSet());
        data.setSchoolContext(isSchoolContext());

        if (isSchoolContext()) {
            data.setSchool(getSchool());
        }

        data.setParameters(getParameters());
        data.setUser(getUser());
        data.initializeExport();

        Set<School> schools = data.getSchools();
        for (School school : schools) {
            if ((m_homeroomOnly && school.getSchoolLevelCode() != null
                    && school.getSchoolLevelCode().toUpperCase().indexOf(ELEMENTARY) != -1)
                    || !m_homeroomOnly) {
                Set<MasterSchedule> sections = data.getSections(school);
                for (MasterSchedule section : sections) {
                    if (((m_homeroomOnly && section.getSchoolCourse().getDescription() != null &&
                            section.getSchoolCourse().getDescription().toUpperCase().indexOf(HOMEROOM_ONLY) != -1)
                            || !m_homeroomOnly)
                            && ((!"all".equals(m_grade)
                                    && data.getSectionGradeList().get(section.getOid()).contains(m_grade)) &&
                                    data.getSectionGradeList().get(section.getOid()).toString().replace("[", "")
                                            .replace("]", "").contains(m_grade)
                                    ||
                                    "all".equals(m_grade))) {
                        grid.append();
                        // header
                        grid.set(FIELD_SCHOOL, school);
                        // detail
                        grid.set(FIELD_SECTION, section);
                        grid.set(FIELD_GRADE, data.getSectionGradeList().get(section.getOid()).toString()
                                .replace("[", "").replace("]", ""));

                        // Determine the number of students enrolled for a particular
                        // section and date
                        Integer totalEnrollment = data.getDayRangeCount(section,
                                getOrganization().getCurrentContext().getStartDate(),
                                getOrganization().getCurrentContext().getEndDate(), m_grade);
                        grid.set(FIELD_TOTAL_ENROLLMENT, totalEnrollment);

                        Integer totalDays = Integer.valueOf(data.getSectionDatesForRange(section,
                                getOrganization().getCurrentContext().getStartDate(),
                                getOrganization().getCurrentContext().getEndDate()).size());
                        grid.set(FIELD_TOTAL_DAYS, totalDays);

                        grid.set(FIELD_DAY_AVERAGE,
                                Double.valueOf(totalEnrollment.doubleValue() / totalDays.doubleValue()));

                        Integer ytdEnrollment = data.getDayRangeCount(section,
                                getOrganization().getCurrentContext().getStartDate(), m_endDate, m_grade);
                        Integer ytdDays =
                                Integer.valueOf(data
                                        .getSectionDatesForRange(section,
                                                getOrganization().getCurrentContext().getStartDate(), m_endDate)
                                        .size());
                        grid.set(FIELD_YTD_AVERAGE, Double.valueOf(ytdEnrollment.doubleValue() / ytdDays.doubleValue()));
                    }
                }
            }
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        Boolean applyFilter = (Boolean) getParameters().get(PARAM_HOMEROOM_ONLY);
        if (applyFilter != null) {
            m_homeroomOnly = applyFilter.booleanValue();
        }
        m_grade = (String) getParameters().get(PARAM_GRADE);
        m_startDate = getOrganization().getCurrentContext().getStartDate();
        addParameter(PARAM_DAY_START, m_startDate);
        m_endDate = (PlainDate) getParameter(PARAM_DAY_END);
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
        addParameter(PARAM_GENERATED_BY, userData.getUser().getNameView());
    }

}
