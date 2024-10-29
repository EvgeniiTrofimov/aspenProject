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
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.FteCalculator;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper.StudentScheduleInfo;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class FLStudentFTESummaryData.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class FLStudentFTESummaryData extends ReportJavaSourceNet {

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

    /**
     * The Class Accumulator.
     */
    class Accumulator {
        private String m_fefpProgram;
        private double m_fte = 0;
        private double m_fteTotalStudent;

        private SisSchool m_school;

        /**
         * Instantiates a new accumulator.
         *
         * @param school SisSchool
         * @param fefpProgram String
         */
        public Accumulator(SisSchool school, String fefpProgram) {
            super();
            this.m_school = school;
            this.m_fefpProgram = fefpProgram;
        }

        /**
         * Adds the fte.
         *
         * @param value double
         */
        public void addFte(double value) {
            m_fte += value;
        }

        /**
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            Accumulator other = (Accumulator) obj;
            if (m_fefpProgram == null) {
                if (other.m_fefpProgram != null) {
                    return false;
                }
            } else if (!m_fefpProgram.equals(other.m_fefpProgram)) {
                return false;
            }
            if (m_school == null) {
                if (other.m_school != null) {
                    return false;
                }
            } else if (!m_school.equals(other.m_school)) {
                return false;
            }
            return true;
        }

        /**
         * Gets the fefp program.
         *
         * @return String
         */
        public String getFefpProgram() {
            return m_fefpProgram;
        }

        /**
         * Gets the fte.
         *
         * @return double
         */
        public double getFte() {
            return m_fte;
        }

        /**
         * Gets the fte adjusted.
         *
         * @return double
         */
        public double getFteAdjusted() {
            return m_fteTotalStudent > 0.5 ? m_fte / m_fteTotalStudent * 0.5 : m_fte;
        }

        /**
         * Gets the school.
         *
         * @return Sis school
         */
        public SisSchool getSchool() {
            return m_school;
        }

        /**
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((m_fefpProgram == null) ? 0 : m_fefpProgram.hashCode());
            result = prime * result + ((m_school == null) ? 0 : m_school.hashCode());
            return result;
        }

        /**
         * Sets the fte total student.
         *
         * @param fteTotalStudent void
         */
        public void setFteTotalStudent(double fteTotalStudent) {
            this.m_fteTotalStudent = fteTotalStudent;
        }
    }

    private static final KeyValuePair[] FEFP_VALUES = {
            new KeyValuePair("101", "K-3 Basic"),
            new KeyValuePair("102", "4-8 Basic"),
            new KeyValuePair("103", "9-12 Basic"),
            new KeyValuePair("111", "K-3 Basic with ESE Services"),
            new KeyValuePair("112", "4-8 Basic with ESE Services"),
            new KeyValuePair("113", "9-12 Basic with ESE Services"),
            new KeyValuePair("254", "ESE Support Level 4"),
            new KeyValuePair("255", "ESE Support Level 5"),
            new KeyValuePair("130", "ESOL"),
            new KeyValuePair("300", "Career Education 9-12")
    };
    private static final String FIELD_FEFP_PROGRAM_NUMBER = "fefp";
    private static final String FIELD_FTE_ADJUSTED = "fteAdjusted";
    private static final String FIELD_FTE_REPORTED = "fte";
    private static final String FIELD_FTE_WEIGHTED = "fteWeighted";
    private static final String FIELD_GRADE_LEVEL = "gradeLevel";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_SCHOOL_NAME = "schoolName";
    private static final String FIELD_SCHOOL_OID = "schoolOid";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_STUDENT_NAME = "studentName";
    private static final String FIELD_STUDENT_OID = "studentOId";

    private static final String OUTPUT_PARAMETER_FEFP_MAP = "fefpMap";
    private static final String PARAM_GRID_SORT = "gridSort";

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
        Map<String, String> fefpNames = new HashMap();
        for (KeyValuePair<String, String> item : FEFP_VALUES) {
            fefpNames.put(item.getKey(), item.getValue());
        }
        addParameter(OUTPUT_PARAMETER_FEFP_MAP, fefpNames);

        m_data = new Statistics();
        m_data.setBroker(getBroker());
        m_data.setOrganization(getOrganization());
        m_data.setPrivilegeSet(getPrivilegeSet());
        m_data.setSchoolContext(isSchoolContext());
        m_data.setSchool(getSchool());
        m_data.setParameters(getParameters());
        m_data.setUser(getUser());
        m_data.initializeExport();

        if (isSchoolContext()) {
            m_data.getStudentHelper().setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
            m_data.getStudentHelper().setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);
        }
        FteCalculator fteCalculator = m_data.getStudentHelper().getFteCalculator();
        QueryByCriteria query = m_data.getStudentHelper().getStudentQuery(false);
        QueryIterator students = getBroker().getIteratorByQuery(query);
        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();
                if (m_data.getStudentHelper().isStudentEligible(student)) {
                    List<StudentScheduleInfo> records =
                            m_data.m_studentScheduleHelper.getStudentScheduleInfo(student);
                    // remove duplicate sections
                    Set<String> mstOids = new HashSet();
                    Iterator<StudentScheduleInfo> iterator = records.iterator();
                    while (iterator.hasNext()) {
                        StudentScheduleInfo item = iterator.next();
                        String mstOid = item.getSection().getOid();
                        if (mstOids.contains(mstOid)) {
                            iterator.remove();
                        } else {
                            mstOids.add(mstOid);
                        }
                    }
                    Map<Accumulator, Accumulator> accumulators = new HashMap();
                    iterator = records.iterator();
                    while (iterator.hasNext()) {
                        StudentScheduleInfo item = iterator.next();
                        if (!isSchoolContext() || item.getMasterScheduleInfo().getSection().getSchedule()
                                .getSchool().getOid().equals(getSchool().getOid())) {
                            Accumulator accumulator = new Accumulator(item.getSection().getSchedule().getSchool(),
                                    item.getStudentInfo().getFefpProgram());
                            if (accumulators.containsKey(accumulator)) {
                                accumulator = accumulators.get(accumulator);
                            } else {
                                accumulators.put(accumulator, accumulator);
                            }
                            accumulator.addFte(item.getFte().doubleValue() / 10000);
                        }
                    }

                    double totalFte = 0;
                    for (Accumulator item : accumulators.values()) {
                        totalFte += item.getFte();
                    }

                    for (Accumulator item : accumulators.values()) {
                        item.setFteTotalStudent(totalFte);
                    }

                    for (Accumulator item : accumulators.values()) {
                        if (fefpNames.containsKey(item.getFefpProgram())) {
                            grid.append();
                            grid.set(FIELD_SCHOOL, item.getSchool());
                            grid.set(FIELD_STUDENT, student);
                            grid.set(FIELD_SCHOOL_NAME, item.getSchool().getName());
                            grid.set(FIELD_STUDENT_NAME, student.getNameView());
                            grid.set(FIELD_SCHOOL_OID, item.getSchool().getOid());
                            grid.set(FIELD_STUDENT_OID, student.getOid());
                            grid.set(FIELD_GRADE_LEVEL, student.getGradeLevel());
                            grid.set(FIELD_FEFP_PROGRAM_NUMBER, item.getFefpProgram());
                            grid.set(FIELD_FTE_REPORTED, Double.valueOf(item.getFte()));
                            grid.set(FIELD_FTE_ADJUSTED, Double.valueOf(item.getFteAdjusted()));
                            grid.set(FIELD_FTE_WEIGHTED, Double.valueOf(
                                    fteCalculator.getWeightedFteValue(item.getFefpProgram(), item.getFteAdjusted())
                                            / 10000.0));
                        }
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
