/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2015 X2 Development Corporation.
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
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Export Data Module for CA Daily.
 *
 * @author X2 Development Corporation
 */
public class CSRDailyData extends ReportJavaSourceNet {
    /**
     * Data class for CA Daily export.
     *
     */
    private static final String ELEMENTARY = "ELEMENTARY";

    private static final String FIELD_DATE = "date";
    private static final String FIELD_DAY_ENROLLMENT = "enrollment";
    private static final String FIELD_DAY_NUMBER = "numberOfDay";
    private static final String FIELD_DAY_PREVIOUS = "previous";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_SECTION = "section";

    private static final String HOMEROOM_ONLY = "HOMEROOM";

    private static final String PARAM_HOMEROOM_ONLY = "applyFilter";
    private static final String PARAM_GRADE = "grade";
    private static final String PARAM_GRADE_LIST_BY_SECTION = "gradesListBySection";

    private static final String REPORT_PARAM_GENERATED_BY = "generatedBy";

    private String m_grade = null;
    private boolean m_homeroomOnly = false;

    /**
     * Gather data.
     *
     * @return Object
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws X2BaseException {
        CSRDataHelper data;
        data = new CSRDataHelper();
        data.setBroker(getBroker());
        data.setOrganization(getOrganization());
        data.setPrivilegeSet(getPrivilegeSet());
        data.setSchoolContext(isSchoolContext());

        Map<String, Set<String>> gradeList = new HashMap<String, Set<String>>();

        Boolean applyFilter = (Boolean) getParameters().get(PARAM_HOMEROOM_ONLY);
        if (applyFilter != null) {
            m_homeroomOnly = applyFilter.booleanValue();
        }
        m_grade = (String) getParameters().get(PARAM_GRADE);

        if (isSchoolContext()) {
            data.setSchool(getSchool());
        }
        data.setParameters(getParameters());
        data.setUser(getUser());
        data.initializeExport();

        ReportDataGrid grid = new ReportDataGrid();

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
                        if (!"all".equals(m_grade) && data.getSectionGradeList().get(section.getOid()).contains(m_grade)
                                &&
                                data.getSectionGradeList().get(section.getOid()).toString().replace("[", "")
                                        .replace("]", "").contains(m_grade)) {
                            gradeList.put(section.getOid(), data.getSectionGradeList().get(section.getOid()));
                        }
                        Set<PlainDate> sectionDates = data.getSectionDates(section);
                        int totalCount = 0;
                        int dayNumber = 0;
                        for (PlainDate date : sectionDates) {
                            Integer count = data.getDayCount(section, date, m_grade);
                            grid.append();
                            grid.set(FIELD_SCHOOL, school);
                            grid.set(FIELD_SECTION, section);
                            grid.set(FIELD_DATE, date);
                            grid.set(FIELD_DAY_PREVIOUS, Integer.valueOf(totalCount));
                            grid.set(FIELD_DAY_ENROLLMENT, count);
                            grid.set(FIELD_DAY_NUMBER, Integer.valueOf(++dayNumber));
                            totalCount += count.intValue();
                        }
                    }
                }
            }
        }
        if ("all".equals(m_grade)) {
            addParameter(PARAM_GRADE_LIST_BY_SECTION, data.getSectionGradeList());
        } else {
            addParameter(PARAM_GRADE_LIST_BY_SECTION, gradeList);
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
        addParameter(REPORT_PARAM_GENERATED_BY, userData.getUser().getNameView());
    }
}
