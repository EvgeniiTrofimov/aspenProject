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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.ca.CSRDataHelper;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for CA Class Size Reduction (CSR) Compliance report.
 *
 * @author X2 Development Corporation
 */
public class CSRSummaryData extends ReportJavaSourceNet {
    private static final String ELEMENTARY = "ELEMENTARY";

    private static final String FIELD_AVERAGE = "d_avg";
    private static final String FIELD_ENR_I_CODE = "enr_1";
    private static final String FIELD_ENR_II_CODE = "enr_2";
    private static final String FIELD_ENR_III_CODE = "enr_3";
    private static final String FIELD_ENR_IV_CODE = "enr_4";
    private static final String FIELD_ENR_V_CODE = "enr_5";
    private static final String FIELD_ENR_VI_CODE = "enr_6";
    private static final String FIELD_ENR_VII_CODE = "enr_7";
    private static final String FIELD_ENR_VIII_CODE = "enr_8";
    private static final String FIELD_ENR_K_CODE = "enr_k";
    private static final String FIELD_ENR_TK_CODE = "enr_tk";
    private static final String FIELD_ENROLLED_DAYS = "total_enr";
    private static final String FIELD_GRADE = "grade";
    private static final String FIELD_POSSIBLE_DAYS = "days";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_SECTION = "section";

    private static final String GRADE_CODE_1 = "01";
    private static final String GRADE_CODE_2 = "02";
    private static final String GRADE_CODE_3 = "03";
    private static final String GRADE_CODE_4 = "04";
    private static final String GRADE_CODE_5 = "05";
    private static final String GRADE_CODE_6 = "06";
    private static final String GRADE_CODE_7 = "07";
    private static final String GRADE_CODE_8 = "08";
    private static final String GRADE_CODE_K = "00";
    private static final String GRADE_CODE_TK = "TK";

    private static final String HOMEROOM_ONLY = "HOMEROOM";

    private static final String PARAM_ALL_GRADES = "allGrades";
    private static final String PARAM_CYCLE = "cycle";
    private static final String PARAM_CYCLES = "cycles";
    private static final String PARAM_GENERATED_BY = "generatedBy";
    private static final String PARAM_GRADES = "gradeList";
    private static final String PARAM_HOMEROOM_ONLY = "homeroomOnly";

    private boolean m_homeroomOnly;
    private boolean m_allGrades;
    private Collection<String> m_grades = null;

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
        CSRDataHelper m_data = null;

        m_data = new CSRDataHelper();
        m_data.setBroker(getBroker());
        m_data.setOrganization(getOrganization());
        m_data.setPrivilegeSet(getPrivilegeSet());
        m_data.setSchoolContext(isSchoolContext());
        if (isSchoolContext()) {
            m_data.setSchool(getSchool());
        }
        m_data.setParameters(getParameters());
        m_data.setUser(getUser());
        m_data.initializeExport();

        Set<School> schools = m_data.getSchools();
        Map<String, Set<String>> sectionGradeList = m_data.getSectionGradeList();
        for (School school : schools) {
            if ((m_homeroomOnly && school.getSchoolLevelCode() != null
                    && school.getSchoolLevelCode().toUpperCase().indexOf(ELEMENTARY) != -1)
                    || !m_homeroomOnly) {
                Set<MasterSchedule> sections = m_data.getSections(school);
                for (MasterSchedule section : sections) {
                    if (((m_homeroomOnly && section.getSchoolCourse().getDescription() != null &&
                            section.getSchoolCourse().getDescription().toUpperCase().indexOf(HOMEROOM_ONLY) != -1)
                            || !m_homeroomOnly)
                            // Show only sections that contains at least one selected grade.
                            && ((!m_allGrades
                                    && !Collections.disjoint(sectionGradeList.get(section.getOid()), m_grades)) ||
                                    m_allGrades)) {
                        grid.append();
                        grid.set(FIELD_SCHOOL, school);
                        grid.set(FIELD_SECTION, section);
                        grid.set(FIELD_GRADE, getGradesView(sectionGradeList.get(section.getOid())));

                        if (m_allGrades) {
                            grid.set(FIELD_ENR_I_CODE,
                                    String.valueOf(m_data.getCountOfStudent(section.getOid(), GRADE_CODE_1)));
                            grid.set(FIELD_ENR_II_CODE,
                                    String.valueOf(m_data.getCountOfStudent(section.getOid(), GRADE_CODE_2)));
                            grid.set(FIELD_ENR_III_CODE,
                                    String.valueOf(m_data.getCountOfStudent(section.getOid(), GRADE_CODE_3)));
                            grid.set(FIELD_ENR_IV_CODE,
                                    String.valueOf(m_data.getCountOfStudent(section.getOid(), GRADE_CODE_4)));
                            grid.set(FIELD_ENR_V_CODE,
                                    String.valueOf(m_data.getCountOfStudent(section.getOid(), GRADE_CODE_5)));
                            grid.set(FIELD_ENR_VI_CODE,
                                    String.valueOf(m_data.getCountOfStudent(section.getOid(), GRADE_CODE_6)));
                            grid.set(FIELD_ENR_VII_CODE,
                                    String.valueOf(m_data.getCountOfStudent(section.getOid(), GRADE_CODE_7)));
                            grid.set(FIELD_ENR_VIII_CODE,
                                    String.valueOf(m_data.getCountOfStudent(section.getOid(), GRADE_CODE_8)));
                            grid.set(FIELD_ENR_K_CODE,
                                    String.valueOf(m_data.getCountOfStudent(section.getOid(), GRADE_CODE_K)));
                            grid.set(FIELD_ENR_TK_CODE,
                                    String.valueOf(m_data.getCountOfStudent(section.getOid(), GRADE_CODE_TK)));
                        } else {
                            for (String grade : m_grades) {
                                switch (grade) {
                                    case GRADE_CODE_1:
                                        grid.set(FIELD_ENR_I_CODE, String
                                                .valueOf(m_data.getCountOfStudent(section.getOid(), GRADE_CODE_1)));
                                        break;
                                    case GRADE_CODE_2:
                                        grid.set(FIELD_ENR_II_CODE, String
                                                .valueOf(m_data.getCountOfStudent(section.getOid(), GRADE_CODE_2)));
                                        break;
                                    case GRADE_CODE_3:
                                        grid.set(FIELD_ENR_III_CODE, String
                                                .valueOf(m_data.getCountOfStudent(section.getOid(), GRADE_CODE_3)));
                                        break;
                                    case GRADE_CODE_4:
                                        grid.set(FIELD_ENR_IV_CODE, String
                                                .valueOf(m_data.getCountOfStudent(section.getOid(), GRADE_CODE_4)));
                                        break;
                                    case GRADE_CODE_5:
                                        grid.set(FIELD_ENR_V_CODE, String
                                                .valueOf(m_data.getCountOfStudent(section.getOid(), GRADE_CODE_5)));
                                        break;
                                    case GRADE_CODE_6:
                                        grid.set(FIELD_ENR_VI_CODE, String
                                                .valueOf(m_data.getCountOfStudent(section.getOid(), GRADE_CODE_6)));
                                        break;
                                    case GRADE_CODE_7:
                                        grid.set(FIELD_ENR_VII_CODE, String
                                                .valueOf(m_data.getCountOfStudent(section.getOid(), GRADE_CODE_7)));
                                        break;
                                    case GRADE_CODE_8:
                                        grid.set(FIELD_ENR_VIII_CODE, String
                                                .valueOf(m_data.getCountOfStudent(section.getOid(), GRADE_CODE_8)));
                                        break;
                                    case GRADE_CODE_K:
                                        grid.set(FIELD_ENR_K_CODE, String
                                                .valueOf(m_data.getCountOfStudent(section.getOid(), GRADE_CODE_K)));
                                        break;
                                    case GRADE_CODE_TK:
                                        grid.set(FIELD_ENR_TK_CODE, String
                                                .valueOf(m_data.getCountOfStudent(section.getOid(), GRADE_CODE_TK)));
                                        break;
                                }
                            }
                        }
                        section.getStudentSections();

                        Integer totalEnrollment = Integer.valueOf(0);
                        if (m_allGrades == true) {
                            totalEnrollment = m_data.getDayRangeCount(section, null, null, "all");
                        } else {
                            for (String grade : m_grades) {
                                totalEnrollment = Integer.valueOf(totalEnrollment.intValue() +
                                        m_data.getDayRangeCount(section, null, null, grade).intValue());
                            }
                        }
                        grid.set(FIELD_ENROLLED_DAYS, totalEnrollment);
                        Integer totalDays = Integer.valueOf(m_data.getSectionDatesForRange(section, null, null).size());
                        grid.set(FIELD_POSSIBLE_DAYS, totalDays);
                        grid.set(FIELD_AVERAGE, String.format("%.2f",
                                Double.valueOf(totalEnrollment.doubleValue() / totalDays.doubleValue())));
                    }
                }

            }
        }
        grid.beforeTop();
        return grid;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    protected void initialize() throws X2BaseException {
        ArrayList<String> cycles = new ArrayList<String>();
        String cyclesOids = (String) getParameter(PARAM_CYCLE);
        if (!StringUtils.isEmpty(cyclesOids)) {
            cycles.addAll(StringUtils.convertDelimitedStringToList(cyclesOids, ","));
        }
        X2Criteria cycleCodesCriteria = new X2Criteria();
        cycleCodesCriteria.addIn(X2BaseBean.COL_OID, cycles);
        QueryByCriteria cycleCodesQuery = new QueryByCriteria(ReferenceCode.class, cycleCodesCriteria);
        cycleCodesQuery.addOrderBy(ReferenceCode.COL_CODE, true);
        ArrayList<String> codes = new ArrayList<String>();
        codes.addAll(getBroker().getGroupedCollectionByQuery(cycleCodesQuery, ReferenceCode.COL_CODE, 1024).keySet());
        Collections.sort(codes,
                new Comparator<String>() {
                    @Override
                    public int compare(String str1, String str2) {
                        return str1.toString().compareTo(str2.toString());
                    }
                });
        String cyclesString = "";

        for (String cycleCode : codes) {
            cyclesString = cyclesString + (cyclesString.isEmpty() ? "" : ", ") + cycleCode;
        }
        addParameter(PARAM_CYCLES, cyclesString);

        m_homeroomOnly = getParameter(PARAM_HOMEROOM_ONLY) != null
                ? ((Boolean) getParameter(PARAM_HOMEROOM_ONLY)).booleanValue() : false;

        m_allGrades = (getParameter(PARAM_ALL_GRADES) != null
                ? ((Boolean) getParameter(PARAM_ALL_GRADES)).booleanValue() : false);

        if (m_allGrades == false) {
            String grades = (String) getParameter(PARAM_GRADES);
            getGradesCodes(grades);
        }
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

    /**
     * Gets the grades codes.
     *
     * @param grades String
     * @return list of selected grade levels.
     */
    private void getGradesCodes(String grades) {
        m_grades = new ArrayList<String>();

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(X2BaseBean.COL_OID, Arrays.asList((grades.split(","))));
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                ReferenceCode code = (ReferenceCode) iterator.next();
                m_grades.add(code.getCode());
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Return view of grades.
     *
     * @param grades Set<String>
     * @return view of grades
     */
    private String getGradesView(Set<String> grades) {
        StringBuilder result = new StringBuilder();
        for (String string : grades) {
            result.append(string);
            result.append(",");
        }
        return result.length() > 0 ? result.substring(0, result.length() - 1).replace("[", "").replace("]", "") : "";
    }
}
