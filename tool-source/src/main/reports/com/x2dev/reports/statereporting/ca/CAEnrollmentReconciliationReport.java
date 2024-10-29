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

import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.ca.ADADataHelper;
import com.x2dev.procedures.statereporting.ca.ADADataHelper.ADADetailItem;
import com.x2dev.procedures.statereporting.ca.ADADataHelper.ADA_ENROLLMENT_TYPE;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Data source for "CA Average Daily Attendance (ADA) Detail" report .
 *
 * @author X2 Development Corporation
 */
public class CAEnrollmentReconciliationReport extends ReportJavaSourceNet {

    protected int m_date = -1;
    protected int m_type = -1;
    protected int m_studentName = -1;
    protected int m_studentLASID = -1;
    protected int m_gradeLvl = -1;
    protected int m_yog = -1;
    protected int m_code = -1;
    protected int m_reason = -1;
    protected int m_sex = -1;

    /**
     * Helper class to calculate necessary subtotals.
     *
     * @author Follett Software Company
     */
    class SubTotal implements Comparable<SubTotal> {
        private String m_gradesInSubTotal;
        private Set<PlainDate> m_scheduledDays;
        private int m_numAbsentST;
        private int m_numApportionmentST;
        private int m_numExcusedST;
        private int m_numUnExcusedST;
        private int m_numScheduledST;
        private int m_numNotEnrolledST;
        private int m_numFirstST;
        private int m_numLastST;
        private int m_beginST;
        private int m_gainST;
        private int m_lossST;
        private int m_endST;

        /**
         * Construct object of SubTotal class.
         *
         * @param gradesInSubTotal String
         */
        public SubTotal(String gradesInSubTotal) {
            this.m_gradesInSubTotal = gradesInSubTotal;
            m_scheduledDays = new HashSet<PlainDate>();
        }

        /**
         * Compare to.
         *
         * @param other SubTotal
         * @return int
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(SubTotal other) {
            return m_gradesInSubTotal.compareTo(other.m_gradesInSubTotal);
        }

        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            return compareTo((SubTotal) obj) == 0 ? true : false;
        }

        /**
         * Gets the begin ST.
         *
         * @return the m_beginST
         */
        public int getBeginST() {
            return m_beginST;
        }

        /**
         * Gets the end ST.
         *
         * @return the m_endST
         */
        public int getEndST() {
            return m_endST;
        }

        /**
         * Gets the gain ST.
         *
         * @return the m_gainST
         */
        public int getGainST() {
            return m_gainST;
        }

        /**
         * Gets the grades in sub total.
         *
         * @return the m_gradesInSubTotal
         */
        public String getGradesInSubTotal() {
            return m_gradesInSubTotal;
        }

        /**
         * Gets the loss ST.
         *
         * @return the m_lossST
         */
        public int getLossST() {
            return m_lossST;
        }

        /**
         * Gets the num absent ST.
         *
         * @return the m_numAbsentST
         */
        public int getNumAbsentST() {
            return m_numAbsentST;
        }

        /**
         * Gets the num apportionment ST.
         *
         * @return the m_numApportionmentST
         */
        public int getNumApportionmentST() {
            return m_numApportionmentST;
        }

        /**
         * Gets the num excused ST.
         *
         * @return the m_numExcusedST
         */
        public int getNumExcusedST() {
            return m_numExcusedST;
        }

        /**
         * Gets the num first ST.
         *
         * @return the m_numFirstST
         */
        public int getNumFirstST() {
            return m_numFirstST;
        }

        /**
         * Gets the num last ST.
         *
         * @return the m_numLastST
         */
        public int getNumLastST() {
            return m_numLastST;
        }

        /**
         * Gets the num not enrolled ST.
         *
         * @return the m_numNotEnrolledST
         */
        public int getNumNotEnrolledST() {
            return m_numNotEnrolledST;
        }

        /**
         * Gets the num scheduled ST.
         *
         * @return the m_numScheduledST
         */
        public int getNumScheduledST() {
            return m_numScheduledST;
        }

        /**
         * Gets the num un excused ST.
         *
         * @return the m_numUnExcusedST
         */
        public int getNumUnExcusedST() {
            return m_numUnExcusedST;
        }

        /**
         * Gets the scheduled days.
         *
         * @return the scheduled days
         */
        public Set<PlainDate> getScheduledDays() {
            return m_scheduledDays;
        }


        /**
         * Increment member variables on the values of DetailItem.
         *
         * @param item DetailItem
         */
        public void increment(DetailItem item) {
            m_numNotEnrolledST += item.getNumNotEnrolled();
            m_numApportionmentST += item.getNumApportionment();
            m_numAbsentST += item.getNumAbsent();
            m_numExcusedST += item.getNumExcused();
            m_numUnExcusedST += item.getNumUnExcused();
            m_numScheduledST += item.getNumScheduled();
            m_numFirstST += item.getNumFirst();
            m_numLastST += item.getNumLast();
            m_beginST += item.getBegin();
            m_gainST += item.getGain();
            m_lossST += item.getLoss();
            m_endST += item.getEnd();
            m_scheduledDays.addAll(item.getScheduledDays());
        }

        /**
         * Increment member variables of the Subtotal item.
         *
         * @param item SubTotal
         */
        public void increment(SubTotal item) {
            this.m_numNotEnrolledST += item.m_numNotEnrolledST;
            this.m_numApportionmentST += item.m_numApportionmentST;
            this.m_numAbsentST += item.m_numAbsentST;
            this.m_numExcusedST += item.m_numExcusedST;
            this.m_numUnExcusedST += item.m_numUnExcusedST;
            this.m_numScheduledST += item.m_numScheduledST;
            this.m_numFirstST += item.m_numFirstST;
            this.m_numLastST += item.m_numLastST;
            this.m_beginST += item.m_beginST;
            this.m_gainST += item.m_gainST;
            this.m_lossST += item.m_lossST;
            this.m_endST += item.m_endST;
            m_scheduledDays.addAll(item.getScheduledDays());
        }
    }

    /**
     * Helper class to calculate necessary subtotals.
     *
     * @author Follett Software Company
     */
    class DetailItem {

        /**
         * The Class StudentDetail.
         */
        class StudentDetail {
            boolean isEnrolledFirstDay;
            boolean isEnrolledIntermediateDay;
            boolean isEnrolledLastDay;
            boolean lossRecorded;
        }

        /**
         * Constants
         */
        private static final String NOT_ENROLLED_CODE = "NE";

        /**
         * Member variables of DetailItem
         */
        private int m_begin;
        private int m_end;
        private int m_gain;
        private int m_loss;
        private int m_numAbsent;
        private int m_numApportionment;
        private int m_numExcused;
        private int m_numFirst;
        private int m_numLast;
        private int m_numNotEnrolled;
        private int m_numONA;
        private int m_numScheduled;
        private int m_numSuspended;
        private int m_numTruant;
        private int m_numUnExcused;
        private Map<String, StudentDetail> m_students;
        private Set<PlainDate> m_scheduledDays;

        /**
         * Construct object of DetailItem class.
         */
        public DetailItem() {
            m_students = new HashMap<String, StudentDetail>();
            m_scheduledDays = new HashSet<PlainDate>();
        }

        /**
         * Gets the begin.
         *
         * @return the m_begin
         */
        public int getBegin() {
            return m_begin;
        }

        /**
         * Gets the end.
         *
         * @return the m_end
         */
        public int getEnd() {
            return m_end;
        }

        /**
         * Gets the gain.
         *
         * @return the m_gain
         */
        public int getGain() {
            return m_gain;
        }

        /**
         * Gets the loss.
         *
         * @return the m_loss
         */
        public int getLoss() {
            return m_loss;
        }

        /**
         * Gets the num absent.
         *
         * @return the m_numAbsent
         */
        public int getNumAbsent() {
            return m_numAbsent;
        }

        /**
         * Gets the num apportionment.
         *
         * @return the m_numApportionment
         */
        public int getNumApportionment() {
            return m_numApportionment;
        }

        /**
         * Gets the num excused.
         *
         * @return the m_numExcused
         */
        public int getNumExcused() {
            return m_numExcused;
        }

        /**
         * Gets the num first.
         *
         * @return the m_numFirst
         */
        public int getNumFirst() {
            return m_numFirst;
        }

        /**
         * Gets the num last.
         *
         * @return the m_numLast
         */
        public int getNumLast() {
            return m_numLast;
        }

        /**
         * Gets the num not enrolled.
         *
         * @return the m_numNotEnrolled
         */
        public int getNumNotEnrolled() {
            return m_numNotEnrolled;
        }

        /**
         * Gets the num ONA.
         *
         * @return the m_numONA
         */
        public int getNumONA() {
            return m_numONA;
        }

        /**
         * Gets the num scheduled.
         *
         * @return the m_numScheduled
         */
        public int getNumScheduled() {
            return m_numScheduled;
        }

        /**
         * Gets the num suspended.
         *
         * @return the m_numSuspended
         */
        public int getNumSuspended() {
            return m_numSuspended;
        }

        /**
         * Gets the num truant.
         *
         * @return the m_numTruant
         */
        public int getNumTruant() {
            return m_numTruant;
        }

        /**
         * Gets the num un excused.
         *
         * @return the m_numUnExcused
         */
        public int getNumUnExcused() {
            return m_numUnExcused;
        }

        /**
         * Gets the scheduled days.
         *
         * @return the scheduled days
         */
        public Set<PlainDate> getScheduledDays() {
            return m_scheduledDays;
        }

        /**
         * Gets the students.
         *
         * @return the m_students
         */
        public Map<String, StudentDetail> getStudents() {
            return m_students;
        }

        /**
         * Increment DetailItem's variables on the given values.
         *
         * @param item ADADetailItem
         */
        public void increment(ADADetailItem item) {
            StudentDetail studentDetail = getStudentDetail(item.getStudent());
            if (item.getAttendanceDate().equals(getStartDate())) {
                if (!NOT_ENROLLED_CODE.equals(item.getAttendanceCode())) {
                    ++m_begin;
                    studentDetail.isEnrolledFirstDay = true;
                }
            } else if (item.getAttendanceDate().equals(getEndDate())) {
                if (NOT_ENROLLED_CODE.equals(item.getAttendanceCode())) {
                    if ((studentDetail.isEnrolledIntermediateDay || studentDetail.isEnrolledFirstDay) &&
                            !studentDetail.lossRecorded) {
                        ++m_loss;
                        studentDetail.lossRecorded = true;
                    }
                } else {
                    ++m_end;
                    studentDetail.isEnrolledLastDay = true;
                }
            } else {
                if (NOT_ENROLLED_CODE.equals(item.getAttendanceCode())) {
                    if ((studentDetail.isEnrolledIntermediateDay || studentDetail.isEnrolledFirstDay) &&
                            !studentDetail.lossRecorded) {
                        ++m_loss;
                        studentDetail.lossRecorded = true;
                    }
                } else {
                    if (!studentDetail.isEnrolledFirstDay && !studentDetail.isEnrolledIntermediateDay) {

                        ++m_gain;
                        studentDetail.isEnrolledIntermediateDay = true;
                    }
                }
            }

            if (item.isScheduled()) {
                m_scheduledDays.add(item.getAttendanceDate());
            }

            String attCode = item.getAttendanceCode();
            m_numNotEnrolled += NOT_ENROLLED_CODE.equals(attCode) ? 1 : 0;
            m_numApportionment += item.isApportionment() ? 1 : 0;
            m_numAbsent += item.isAbsent() ? 1 : 0;
            m_numExcused += item.isAbsent() && item.isExcused() ? 1 : 0;
            m_numUnExcused += item.isAbsent() && !item.isExcused() ? 1 : 0;
            m_numScheduled += item.isScheduled() ? 1 : 0;
            m_numSuspended += item.isSuspended() ? 1 : 0;
            m_numTruant += item.isTruant() ? 1 : 0;
            m_numFirst += item.getAttendanceDate().equals(getStartDate()) &&
                    ADA_ENROLLMENT_TYPE.FIRST.equals(item.getEnrollmentType()) ? 1 : 0;
            m_numLast += item.getAttendanceDate().equals(getEndDate()) &&
                    ADA_ENROLLMENT_TYPE.LAST.equals(item.getEnrollmentType()) ? 1 : 0;
        }

        /**
         * Returns Student detail information.
         *
         * @param student SisStudent
         * @return StudentDetail
         */
        private StudentDetail getStudentDetail(SisStudent student) {
            StudentDetail value = m_students.get(student.getOid());
            if (value == null) {
                value = new StudentDetail();
                m_students.put(student.getOid(), value);
            }
            return value;
        }
    }

    /**
     * This class is key for the data map .
     *
     * @author Follett Software Company
     */
    class GroupItem implements Comparable<GroupItem> {
        private String m_fund;
        private String m_grade;
        private SisSchool m_school;

        /**
         * Construct object of GroupItem (key for the data map).
         *
         * @param school SisSchool
         * @param fund String
         * @param grade String
         */
        public GroupItem(SisSchool school, String fund, String grade) {
            this.m_school = school;
            this.m_fund = fund;
            this.m_grade = grade;
        }

        /**
         * Compare to.
         *
         * @param other GroupItem
         * @return int
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(GroupItem other) {
            int value = m_school.getName().compareTo(other.m_school.getName());
            if (value == 0) {
                value = m_school.getOid().compareTo(other.m_school.getOid());
            }
            if (value == 0) {
                value = m_fund.compareTo(other.m_fund);
            }
            if (value == 0) {
                value = m_grade.compareTo(other.m_grade);
            }
            return value;
        }

        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            return compareTo((GroupItem) obj) == 0 ? true : false;
        }

        /**
         * Hash code.
         *
         * @return int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return m_school.getOid().hashCode() + m_fund.hashCode() + m_grade.hashCode();
        }

        /**
         * Gets the fund.
         *
         * @return the m_fund
         */
        public String getFund() {
            return m_fund;
        }

        /**
         * Gets the grade.
         *
         * @return the m_grade
         */
        public String getGrade() {
            return m_grade;
        }

        /**
         * Gets the school.
         *
         * @return the m_school
         */
        public SisSchool getSchool() {
            return m_school;
        }
    }


    private static final String INPUT_PARAM_PROCEDURE_ID = "procedureIdHelper";
    private static final String INPUT_PARAM_INCLUDE_LEGEND = "includeLegend";

    private static final int NUM_FIELDS = 9;

    private static final String PROCEDURE_ID = "procedureId";

    private static final String REPORT_PARAM_END_DATE = "endDate";
    private static final String REPORT_PARAM_START_DATE = "startDate";
    private static final String REPORT_PARAM_INCLUDE_LEGEND = "includeLegend";

    private ADADataHelper m_dataHelper;
    private String m_endCycle;
    private PlainDate m_endDate = null;
    private Collection<StateReportValidationError> m_initErrors = null;
    private StateReportData m_reportData = null;
    private String m_startCycle;
    private PlainDate m_startDate = null;

    /**
     * Gets the end date.
     *
     * @return the m_endDate
     */
    public PlainDate getEndDate() {
        return m_endDate;
    }

    /**
     * Gets the start date.
     *
     * @return the m_startDate
     */
    public PlainDate getStartDate() {
        return m_startDate;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected ReportDataGrid gatherData() throws Exception {
        String procedureId = (String) getParameter(PROCEDURE_ID);
        m_initErrors = new ArrayList<StateReportValidationError>();

        // Lookup State report source data procedure
        m_reportData = StateReportData.getReportDataFromProcedure(procedureId, getBroker(), m_initErrors);

        if (m_reportData != null && m_initErrors.size() == 0) {
            try {
                // Initialize the report data object.
                m_reportData.setBroker(getBroker());
                m_reportData.setOrganization(getOrganization());
                m_reportData.setPrivilegeSet(getPrivilegeSet());
                m_reportData.setSchoolContext(isSchoolContext());
                m_reportData.setSchool(getSchool());
                getParameters().put("organizationCriteria", getOrganizationCriteria(StudentEnrollment.class));
                m_reportData.setParameters(getParameters());
                m_reportData.setUser(getUser());
                m_reportData.initializeExport();

            } catch (X2BaseException x2be) {
                String init_msg = "Failure initializing data structure in CASchoolImmunizationRecord";
                m_initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));

                throw x2be;
            }

            m_initErrors.addAll(m_reportData.getSetupErrors());
        }
        ReportDataGrid dataGrid = new ReportDataGrid(NUM_FIELDS);

        if (m_reportData != null && m_reportData.open()) {
            initializeFieldPositions();
            try {
                StateReportEntity entity = null;
                while ((entity = m_reportData.next()) != null) {
                    entity.preProcess();
                    dataGrid.append();
                    dataGrid.set("enrollmentDate", entity.getFieldValue(m_date));
                    dataGrid.set("enrollmentType", entity.getFieldValue(m_type));
                    dataGrid.set("studentName", entity.getFieldValue(m_studentName));
                    dataGrid.set("studentLASID", entity.getFieldValue(m_studentLASID));
                    dataGrid.set("gradeLevel", entity.getFieldValue(m_gradeLvl));
                    dataGrid.set("yog", entity.getFieldValue(m_yog));
                    dataGrid.set("enrollmentCode", entity.getFieldValue(m_code));
                    dataGrid.set("enrollmentReason", entity.getFieldValue(m_reason));
                    dataGrid.set("sex", entity.getFieldValue(m_sex));

                    entity.postProcess();
                }
            } finally {
                m_reportData.close();
            }
        }
        dataGrid.beforeTop();
        return dataGrid;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        m_dataHelper = new ADADataHelper(getBroker(), getOrganization());
        /*
         * The mechanism used to test the report populated the days report using
         * four weeks starting with the first monday after the input date.
         * The mechanism used to set the report days for production selects dates
         * from the calendar based on the cycle field
         * Only one of these should be used.
         */
        m_startCycle = (String) getParameter(ADADataHelper.INPUT_PARAM_START_CYCLE);
        m_endCycle = (String) getParameter(ADADataHelper.INPUT_PARAM_END_CYCLE);

        if (Integer.parseInt(m_startCycle) > Integer.parseInt(m_endCycle)) {
            m_endCycle = m_startCycle;
        }

        m_dataHelper.setCycle(null, m_startCycle, m_endCycle, null, false);
        PlainDate[] days = m_dataHelper.getDays();

        m_startDate = days[0];
        m_endDate = days[days.length - 1];

        addParameter(REPORT_PARAM_INCLUDE_LEGEND, getParameter(INPUT_PARAM_INCLUDE_LEGEND));

        this.addParameter(REPORT_PARAM_START_DATE, m_startDate);
        this.addParameter(REPORT_PARAM_END_DATE, m_endDate);

        // Lookup State report source data procedure
        String procedureIdHelper = (String) getParameter(INPUT_PARAM_PROCEDURE_ID);
        m_dataHelper.initialize(getPrivilegeSet(), isSchoolContext(), null, getParameters(), getUser(),
                procedureIdHelper);
    }

    /**
     *
     * Sets the field position array for the input data.
     */
    private void initializeFieldPositions() {
        for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
            FieldDefinition field = m_reportData.getFieldDefinition(pos);
            String fieldName = field.getFieldId();

            if ("Date".equals(fieldName)) {
                m_date = pos;
            } else if ("Type".equals(fieldName)) {
                m_type = pos;
            } else if ("Student Name".equals(fieldName)) {
                m_studentName = pos;
            } else if ("Student LASID".equals(fieldName)) {
                m_studentLASID = pos;
            } else if ("Grade Level".equals(fieldName)) {
                m_gradeLvl = pos;
            } else if ("YOG".equals(fieldName)) {
                m_yog = pos;
            } else if ("Code".equals(fieldName)) {
                m_code = pos;
            } else if ("Reason".equals(fieldName)) {
                m_reason = pos;
            } else if ("Sex".equals(fieldName)) {
                m_sex = pos;
            }
        }
    }
}
