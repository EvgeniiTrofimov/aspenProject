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
import com.x2dev.procedures.statereporting.fl.FLScheduleHelper.MasterScheduleInfo;
import com.x2dev.procedures.statereporting.fl.FLStaffHelper;
import com.x2dev.procedures.statereporting.fl.FLStaffHelper.ScheduleTeacherInfo;
import com.x2dev.procedures.statereporting.fl.FLStateReportData;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper.StudentScheduleInfo;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class FLClassCountDetail.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class FLClassCountDetail extends ReportJavaSourceNet {

    /**
     * The Class Accumulator.
     */
    private class Accumulator {

        /**
         * The Class FishData.
         */
        private class FishData {
            private String m_countDetail;
            private String m_fishNumber;
            private String m_gradeLevel;
            private String m_period;
            private MasterScheduleInfo m_mainSectionInfo;
            private Map<String, Set<StudentScheduleInfo>> m_mapSectionStudents;
            private List<StudentScheduleInfo> m_studentInfos = new LinkedList();
            private List<ScheduleTeacherInfo> m_teacherInfos = new LinkedList();

            /**
             * Instantiates a new fish data.
             *
             * @param fishNumber String
             * @param period String
             */
            public FishData(String fishNumber, String period) {
                super();
                m_fishNumber = fishNumber;
                m_period = period;
            }

            /**
             * Adds the.
             *
             * @param info ScheduleTeacherInfo
             */
            public void add(ScheduleTeacherInfo info) {
                m_teacherInfos.add(info);
            }

            /**
             * Adds the.
             *
             * @param info StudentScheduleInfo
             */
            public void add(StudentScheduleInfo info) {
                m_studentInfos.add(info);
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
                FishData other = (FishData) obj;
                if (!getOuterType().equals(other.getOuterType())) {
                    return false;
                }
                if (m_fishNumber == null) {
                    if (other.m_fishNumber != null) {
                        return false;
                    }
                } else if (!m_fishNumber.equals(other.m_fishNumber)) {
                    return false;
                }
                if (m_period == null) {
                    if (other.m_period != null) {
                        return false;
                    }
                } else if (!m_period.equals(other.m_period)) {
                    return false;
                }
                return true;
            }

            /**
             * Gets the count detail.
             *
             * @param studentDetail boolean
             * @return String
             */
            public String getCountDetail(boolean studentDetail) {
                if (m_countDetail == null) {
                    StringBuilder countDetail = new StringBuilder();
                    for (Entry<String, Set<StudentScheduleInfo>> entry : getSectionStudentMap().entrySet()) {
                        if (countDetail.length() > 0) {
                            countDetail.append("\n");
                        }
                        MasterScheduleInfo info =
                                getStatistics().getScheduleHelper().getMasterScheduleInfo(entry.getKey());
                        countDetail.append(info.getSection().getPrimaryStaff() == null ? "No Primary Teacher"
                                : info.getSection().getPrimaryStaff().getNameView());
                        countDetail.append(" - ");
                        countDetail.append(info.getSection().getCourseView());
                        countDetail.append(" [");
                        countDetail.append(entry.getValue().size());
                        countDetail.append("]");
                        if (studentDetail) {
                            for (StudentScheduleInfo item : entry.getValue()) {
                                String gradeLevel = item.getStudentInfo()
                                        .getGradeLevel(getStatistics().getSurveyPeriod().getDateCertain());
                                String name = item.getStudentInfo().formatStudentLegalName();

                                countDetail.append("\n");
                                countDetail.append("    ");
                                countDetail.append(gradeLevel);
                                countDetail.append(" - ");
                                countDetail.append(name);
                            }
                        }

                    }
                    m_countDetail = countDetail.toString();
                }
                return m_countDetail;
            }

            /**
             * Gets the fish number.
             *
             * @return String
             */
            public String getFishNumber() {
                return m_fishNumber;
            }

            /**
             * Gets the grade level.
             *
             * @return Object
             */
            public Object getGradeLevel() {
                if (m_gradeLevel == null) {
                    MasterScheduleInfo info = getMainSection();
                    if (info != null) {
                        Map<String, Set<StudentScheduleInfo>> map = getSectionStudentMap();
                        if (map != null) {
                            Set<StudentScheduleInfo> set = map.get(info.getSection().getOid());
                            if (set != null) {
                                Map<String, Set<StudentScheduleInfo>> gradeMap = new HashMap();
                                for (StudentScheduleInfo item : set) {
                                    String gradeLevel = item.getStudentInfo()
                                            .getGradeLevel(getStatistics().getSurveyPeriod().getDateCertain());
                                    Set<StudentScheduleInfo> grades = gradeMap.get(gradeLevel);
                                    if (grades == null) {
                                        grades = new HashSet();
                                        gradeMap.put(gradeLevel, grades);
                                    }
                                    grades.add(item);
                                }

                                int count = 0;
                                for (Entry<String, Set<StudentScheduleInfo>> item : gradeMap.entrySet()) {
                                    if (item.getValue().size() > count) {
                                        count = item.getValue().size();
                                        m_gradeLevel = item.getKey();
                                    }
                                }
                            }
                        }
                    }
                }
                return m_gradeLevel;
            }

            /**
             * Gets the main section.
             *
             * @return Master schedule info
             */
            public MasterScheduleInfo getMainSection() {
                MasterScheduleInfo info = null;
                if (!m_studentInfos.isEmpty()) {
                    if (m_mainSectionInfo == null) {
                        // count students per section
                        Map<String, Set<StudentScheduleInfo>> map = getSectionStudentMap();
                        if (map != null) {
                            int mostCount = 0;
                            for (Entry<String, Set<StudentScheduleInfo>> item : map.entrySet()) {
                                if (item.getValue().size() > mostCount) {
                                    mostCount = item.getValue().size();
                                    m_mainSectionInfo =
                                            getStatistics().getScheduleHelper()
                                                    .getMasterScheduleInfo(item.getKey());
                                }
                            }
                        }
                    }
                    info = m_mainSectionInfo;
                }
                return info;
            }

            /**
             * Gets the period.
             *
             * @return String
             */
            public String getPeriod() {
                return m_period;
            }

            /**
             * Gets the school.
             *
             * @return Sis school
             */
            public SisSchool getSchool() {
                SisSchool value = null;
                MasterScheduleInfo info = getMainSection();
                if (info != null) {
                    value = info.getSection().getSchedule().getSchool();
                }
                return value;
            }

            /**
             * Gets the total student count.
             *
             * @return int
             */
            public int getTotalStudentCount() {
                int value = 0;
                Map<String, Set<StudentScheduleInfo>> map = getSectionStudentMap();
                if (map != null && !map.values().isEmpty()) {
                    if (map.values().size() == 1) {
                        value = map.values().iterator().next().size();
                    } else {
                        Set<StudentScheduleInfo> totalSet = new HashSet();
                        for (Set<StudentScheduleInfo> set : map.values()) {
                            totalSet.addAll(set);
                        }
                        value = totalSet.size();
                    }
                }
                return value;
            }

            /**
             * @see java.lang.Object#hashCode()
             */
            @Override
            public int hashCode() {
                final int prime = 31;
                int result = 1;
                result = prime * result + getOuterType().hashCode();
                result = prime * result + ((m_fishNumber == null) ? 0 : m_fishNumber.hashCode());
                result = prime * result + ((m_period == null) ? 0 : m_period.hashCode());
                return result;
            }

            /**
             * Gets the outer type.
             *
             * @return Accumulator
             */
            private Accumulator getOuterType() {
                return Accumulator.this;
            }

            /**
             * Gets the section student map.
             *
             * @return Map
             */
            private Map<String, Set<StudentScheduleInfo>> getSectionStudentMap() {
                if (m_mapSectionStudents == null && !m_studentInfos.isEmpty()) {
                    m_mapSectionStudents = new HashMap();
                    for (StudentScheduleInfo info : m_studentInfos) {
                        String sectionOid = info.getMasterScheduleInfo().getSection().getOid();
                        Set<StudentScheduleInfo> set = m_mapSectionStudents.get(sectionOid);
                        if (set == null) {
                            set = new HashSet();
                            m_mapSectionStudents.put(sectionOid, set);
                        }
                        set.add(info);
                    }
                }
                return m_mapSectionStudents;
            }

        }

        private Map<String, FishData> m_mapFishNumber = new HashMap();

        /**
         * Adds the.
         *
         * @param info ScheduleTeacherInfo
         * @throws X2BaseException exception
         */
        public void add(ScheduleTeacherInfo info) throws X2BaseException {
            String fishNumber = info.getMasterScheduleInfo().getClassroomIdentificationNo();
            String period = info.getMasterScheduleInfo().getPeriodNumber();
            FishData fishData = getFishData(fishNumber, period);
            fishData.add(info);
        }

        /**
         * Adds the.
         *
         * @param info StudentScheduleInfo
         * @throws X2BaseException exception
         */
        public void add(StudentScheduleInfo info) throws X2BaseException {
            String fishNumber = info.getMasterScheduleInfo().getClassroomIdentificationNo();
            String period = info.getMasterScheduleInfo().getPeriodNumber();
            FishData fishData = getFishData(fishNumber, period);
            fishData.add(info);
        }

        /**
         * Gets the room data.
         *
         * @return Collection
         */
        public Collection<FishData> getRoomData() {
            return m_mapFishNumber.values();
        }

        /**
         * Gets the fish data.
         *
         * @param fishNumber String
         * @param period String
         * @return Fish data
         */
        private FishData getFishData(String fishNumber, String period) {
            FishData fishData = m_mapFishNumber.get(fishNumber + period);
            if (fishData == null) {
                fishData = new FishData(fishNumber, period);
                m_mapFishNumber.put(fishNumber + period, fishData);
            }
            return fishData;
        }
    }

    /**
     * The Class Statistics.
     */
    protected class Statistics extends FLStateReportData {
        private FLScheduleHelper m_scheduleHelper;
        private FLStaffHelper m_staffHelper;
        private StudentScheduleHelper m_studentScheduleHelper;

        /**
         * Gets the schedule helper.
         *
         * @return FL schedule helper
         */
        public FLScheduleHelper getScheduleHelper() {
            return m_scheduleHelper;
        }

        /**
         * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#getStaffHelper()
         */
        @Override
        public FLStaffHelper getStaffHelper() {
            return m_staffHelper;
        }

        /**
         * Gets the student schedule helper.
         *
         * @return Student schedule helper
         */
        public StudentScheduleHelper getStudentScheduleHelper() {
            return m_studentScheduleHelper;
        }

        /**
         * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#initialize()
         */
        @Override
        public void initialize() throws X2BaseException {
            super.initialize();

            m_staffHelper = new FLStaffHelper(this);
            m_staffHelper.setStaffSelectionMode(FLStaffHelper.MODE_SCHEDULED);
            m_staffHelper.setSelectionProperty(FLStaffHelper.PROPERTY_EXCLUDE_FUTURE_SCHEDULES, Boolean.TRUE);
            m_staffHelper.setSelectionProperty(FLStaffHelper.PROPERTY_END_DATE, getSurveyPeriod().getEndDate());

            m_scheduleHelper =
                    new FLScheduleHelper(this, this.getSurveyPeriod().getStartDate(),
                            getSurveyPeriod().getEndDate());
            m_studentScheduleHelper = getStudentHelper().new StudentScheduleHelper(m_scheduleHelper,
                    this.getSurveyPeriod().getStartDate(), this.getSurveyPeriod().getEndDate());
        }

    }

    private static final String FIELD_COUNT_DETAIL = "countDetail";
    private static final String FIELD_FISH_NUMBER = "fishNumber";
    private static final String FIELD_GRADE_LEVEL = "gradeLevel";
    private static final String FIELD_MAIN_SECTION = "mainSection";
    private static final String FIELD_MAIN_TEACHER = "mainStaff";
    private static final String FIELD_PERIOD = "period";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_SCHOOL_NAME = "schoolName";
    private static final String FIELD_STUDENT_COUNT = "studentCount";
    private static final String PARAMETER_INCLUDE_STUDENT_DETAIL = "studentDetail";

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

        boolean includeStudentDetail = getParameter(PARAMETER_INCLUDE_STUDENT_DETAIL) != null &&
                getParameter(PARAMETER_INCLUDE_STUDENT_DETAIL) instanceof Boolean
                        ? ((Boolean) getParameter(PARAMETER_INCLUDE_STUDENT_DETAIL)).booleanValue()
                        : false;

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

        Accumulator accumulator = new Accumulator();

        QueryByCriteria query = m_data.getStaffHelper().getStaffQuery(false);
        QueryIterator teachers = getBroker().getIteratorByQuery(query);
        try {
            while (teachers.hasNext()) {
                SisStaff teacher = (SisStaff) teachers.next();
                List<ScheduleTeacher> schedules = m_data.getStaffHelper().getTeacherSchedules(teacher.getOid());
                if (schedules != null && !schedules.isEmpty()) {
                    for (ScheduleTeacher schedule : schedules) {
                        ScheduleTeacherInfo infoTeacher = m_data.getStaffHelper().new ScheduleTeacherInfo(schedule);
                        if (!isSchoolContext() || infoTeacher.getMasterScheduleInfo().getSection().getSchedule()
                                .getSchool().getOid().equals(getSchool().getOid())) {
                            accumulator.add(infoTeacher);
                        }
                    }
                }
            }
        } finally {
            teachers.close();
        }

        query = m_data.getStudentHelper().getStudentQuery(false);
        QueryIterator students = getBroker().getIteratorByQuery(query);
        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();
                if (m_data.getStudentHelper().isStudentEligible(student)) {
                    List<StudentScheduleInfo> records = m_data.m_studentScheduleHelper.getStudentScheduleInfo(student);
                    if (records != null && !records.isEmpty()) {
                        for (StudentScheduleInfo info : records) {
                            if (!isSchoolContext() || info.getMasterScheduleInfo().getSection().getSchedule()
                                    .getSchool().getOid().equals(getSchool().getOid())) {
                                if (info.getMasterScheduleInfo().getScheduledOnDayOfWeek(Calendar.FRIDAY)
                                        .booleanValue()) {
                                    accumulator.add(info);
                                }
                            }
                        }
                    }
                }
            }
        } finally {
            teachers.close();
        }

        for (com.x2dev.reports.statereporting.fl.FLClassCountDetail.Accumulator.FishData data : accumulator
                .getRoomData()) {
            if (data.getMainSection() != null) {
                grid.append();
                grid.set(FIELD_SCHOOL, data.getSchool());
                grid.set(FIELD_SCHOOL_NAME, data.getSchool() == null ? null : data.getSchool().getName());
                grid.set(FIELD_FISH_NUMBER, data.getFishNumber());
                grid.set(FIELD_PERIOD, data.getPeriod());
                grid.set(FIELD_GRADE_LEVEL, data.getGradeLevel());
                grid.set(FIELD_MAIN_TEACHER, data.getMainSection().getSection().getPrimaryStaff());
                grid.set(FIELD_COUNT_DETAIL, data.getCountDetail(includeStudentDetail));
                grid.set(FIELD_MAIN_SECTION, data.getMainSection().getSection());
                grid.set(FIELD_STUDENT_COUNT, Integer.valueOf(data.getTotalStudentCount()));
            }
        }

        // String gridSort = (String) getParameter(PARAM_GRID_SORT);
        String gridSort = "schoolName,fishNumber,period";
        if (!StringUtils.isEmpty(gridSort)) {
            grid.sort(Arrays.asList(gridSort.split(",")), false);
        }
        grid.beforeTop();
        return grid;

    }

    /**
     * Gets the statistics.
     *
     * @return Statistics
     */
    protected Statistics getStatistics() {
        return m_data;
    }

}
