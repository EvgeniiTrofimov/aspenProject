/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.ca;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.x2dev.procedures.statereporting.ca.ADADataHelper;
import com.x2dev.procedures.statereporting.ca.ADADataHelper.ADADetailItem;
import com.x2dev.procedures.statereporting.ca.ADADataHelper.ADADetailIterator;
import com.x2dev.procedures.statereporting.ca.ADADataHelper.ADA_ENROLLMENT_TYPE;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for "CA Average Daily Attendance (ADA) Detail" report .
 *
 * @author X2 Development Corporation
 */
public class ADASummaryReportData extends ReportJavaSourceNet {

    /**
     * Helper class to calculate necessary subtotals.
     *
     * @author Follett Software Company
     */
    class SubTotal implements Comparable<SubTotal> {
        private Map<String, Integer> m_beginST = new HashMap<String, Integer>();
        private int m_endST;
        private Map<String, Integer> m_gainST = new HashMap<String, Integer>();
        private String m_gradesInSubTotal;
        private int m_lossST;
        private int m_numAbsentST;
        private int m_numApportionmentST;
        private int m_numExcusedST;
        private int m_numFirstST;
        private int m_numLastST;
        private int m_numNotEnrolledST;
        private int m_numScheduledST;
        private int m_numSuspendedST;
        private int m_numUnExcusedST;
        private int m_numWithMultEntriesST;
        private int m_numWithMultExitsST;
        private Set<PlainDate> m_scheduledDays;

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
            int schoolsBeginST = 0;
            for (Integer schoolBeginST : m_beginST.values()) {
                schoolsBeginST += schoolBeginST.intValue();
            }
            return schoolsBeginST;
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
            int schoolsGainST = 0;
            for (Integer schoolGainST : m_gainST.values()) {
                schoolsGainST += schoolGainST.intValue();
            }
            return schoolsGainST;
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
         * Gets the num suspended ST.
         *
         * @return the m_numSuspendedST
         */
        public int getNumSuspendedST() {
            return m_numSuspendedST;
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
         * Gets the num with mult entries ST.
         *
         * @return int
         */
        public int getNumWithMultEntriesST() {
            return m_numWithMultEntriesST;
        }

        /**
         * Gets the num with mult exits ST.
         *
         * @return int
         */
        public int getNumWithMultExitsST() {
            return m_numWithMultExitsST;
        }

        /**
         * Gets the possible days ST.
         *
         * @return int
         */
        public int getPossibleDaysST() {
            int possibleDaysST = 0;
            Set<String> schoolOids = new HashSet<String>();
            schoolOids.addAll(m_beginST.keySet());
            schoolOids.addAll(m_gainST.keySet());
            for (String schoolOid : schoolOids) {
                Integer beginST = m_beginST.get(schoolOid);
                Integer gainST = m_gainST.get(schoolOid);

                int gross = (beginST == null ? 0 : beginST.intValue()) + (gainST == null ? 0 : gainST.intValue());

                possibleDaysST += gross * m_inSessionMap.get(schoolOid).intValue();
            }

            return possibleDaysST;
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
         * @param schoolOid String
         */
        public void increment(DetailItem item, String schoolOid) {
            m_numNotEnrolledST += item.getNumNotEnrolled();
            m_numAbsentST += item.getNumAbsent();
            m_numExcusedST += item.getNumExcused();
            m_numSuspendedST += item.getNumSuspended();
            m_numUnExcusedST += item.getNumUnExcused();
            m_numFirstST += item.getNumFirst();
            m_numLastST += item.getNumLast();
            m_numWithMultEntriesST += item.getNumWithMultEntries();
            m_numWithMultExitsST += item.getNumWithMultExits();

            Integer beginCounter = this.m_beginST.get(schoolOid);
            if (beginCounter == null) {
                beginCounter = Integer.valueOf(0);
                this.m_beginST.put(schoolOid, beginCounter);
            }
            int currentBeginVal = beginCounter.intValue();
            this.m_beginST.put(schoolOid, Integer.valueOf(currentBeginVal + item.getBegin()));

            Integer gainCounter = this.m_gainST.get(schoolOid);
            if (gainCounter == null) {
                gainCounter = Integer.valueOf(0);
                this.m_gainST.put(schoolOid, gainCounter);
            }
            int currentGainVal = gainCounter.intValue();
            this.m_gainST.put(schoolOid, Integer.valueOf(currentGainVal + item.getGain()));

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
            this.m_numSuspendedST += item.m_numSuspendedST;
            this.m_numUnExcusedST += item.m_numUnExcusedST;
            this.m_numScheduledST += item.m_numScheduledST;
            this.m_numFirstST += item.m_numFirstST;
            this.m_numLastST += item.m_numLastST;

            Set<String> allSchoolOids = new HashSet<String>();

            allSchoolOids.addAll(this.m_beginST.keySet());
            allSchoolOids.addAll(item.m_beginST.keySet());

            for (String schoolOid : allSchoolOids) {
                Integer beginCounter = this.m_beginST.get(schoolOid);
                if (beginCounter == null) {
                    beginCounter = Integer.valueOf(0);
                    this.m_beginST.put(schoolOid, beginCounter);
                }
                int currentValue = beginCounter.intValue();
                this.m_beginST.put(schoolOid, Integer.valueOf(currentValue +
                        (item.m_beginST.get(schoolOid) == null ? 0 : item.m_beginST.get(schoolOid).intValue())));
            }

            allSchoolOids = new HashSet<String>();

            allSchoolOids.addAll(this.m_gainST.keySet());
            allSchoolOids.addAll(item.m_gainST.keySet());

            for (String schoolOid : allSchoolOids) {
                Integer gainCounter = this.m_gainST.get(schoolOid);
                if (gainCounter == null) {
                    gainCounter = Integer.valueOf(0);
                    this.m_gainST.put(schoolOid, gainCounter);
                }
                int currentValue = gainCounter.intValue();
                this.m_gainST.put(schoolOid, Integer.valueOf(currentValue +
                        (item.m_beginST.get(schoolOid) == null ? 0 : item.m_gainST.get(schoolOid).intValue())));
            }

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
            boolean addedAsMultEntries;
            boolean addedAsMultExits;
            boolean isEnrolled;
            boolean isEverEnrolled;
            boolean wasAsGain;
            boolean wasAsLoss;
        }

        /**
         * Member variables of DetailItem
         */
        public boolean hasMultipleEntries;
        public boolean hasMultipleExits;
        private Map<String, StudentDetail> m_students;
        private Set<PlainDate> m_scheduledDays;
        private int m_begin;
        private int m_gain;
        private int m_loss;
        private int m_end;
        private int m_numExcused;
        private int m_numUnExcused;
        private int m_numSuspended;
        private int m_numTruant;
        private int m_numNotEnrolled;
        private int m_numONA;
        private int m_numFirst;
        private int m_numLast;
        private int m_numWithMultEntries;
        private int m_numWithMultExits;

        /**
         * Constants
         */
        private static final String NOT_ENROLLED_CODE = "NE";

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
         * Gets the num with mult entries.
         *
         * @return int
         */
        public int getNumWithMultEntries() {
            return m_numWithMultEntries;
        }

        /**
         * Gets the num with mult exits.
         *
         * @return int
         */
        public int getNumWithMultExits() {
            return m_numWithMultExits;
        }

        /**
         * Gets the num absent.
         *
         * @return the m_numAbsent
         */
        public int getNumAbsent() {
            return m_numExcused + m_numUnExcused + m_numSuspended;
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
         * @param exceptionGrid ReportDataGrid
         * @param schools Collection<SisSchool>
         */
        public void increment(ADADetailItem item, ReportDataGrid exceptionGrid, Collection<SisSchool> schools) {
            PlainDate endDate = getEndDate(item.getSchool(), item.getStudent().getCalendarCode());
            StudentDetail studentDetail = getStudentDetail(item.getStudent());
            String schoolOid = item.getSchool().getOid();

            // ignore dates in cycle after the last in session date
            if (!item.getAttendanceDate().after(endDate)) {
                if (item.getAttendanceDate().equals(getStartDate(schoolOid, item.getStudent().getCalendarCode()))) {
                    if (item.isEnrolled()) {
                        ++m_begin;
                        studentDetail.isEnrolled = true;
                        studentDetail.isEverEnrolled = true;
                    } else {
                        studentDetail.isEnrolled = false;
                    }
                } else if (item.getAttendanceDate().equals(endDate)) {
                    if (!item.isEnrolled()) {
                        if (studentDetail.isEnrolled) {
                            ++m_loss;
                            calcMultExits(studentDetail);

                            studentDetail.isEnrolled = false;
                        }
                    } else {
                        ++m_end;
                        if (!studentDetail.isEnrolled) {
                            ++m_gain;
                            calcMultEntries(studentDetail);

                            studentDetail.isEnrolled = true;
                            studentDetail.isEverEnrolled = true;

                            if (schools.contains(item.getSchool()) && !exceptionGrid.getColumn(FIELD_SUB_STD_NAME)
                                    .contains(item.getStudent().getNameView())) {
                                exceptionGrid.append();
                                exceptionGrid.set(FIELD_SUB_SKL_NAME, item.getSchool().getName());
                                exceptionGrid.set(FIELD_FUND, item.getFundingCategory());
                                exceptionGrid.set(FIELD_GRADE, item.getGradeLevel());
                                exceptionGrid.set(FIELD_SUB_STD_LOCAL_ID, item.getStudent().getLocalId());
                                exceptionGrid.set(FIELD_SUB_STD_NAME, item.getStudent().getNameView());
                                exceptionGrid.set(FIELD_SUB_DATE, item.getAttendanceDate());
                            }
                        }
                    }
                } else {
                    if (!item.isEnrolled()) {
                        if (studentDetail.isEnrolled) {
                            ++m_loss;
                            calcMultExits(studentDetail);

                            studentDetail.isEnrolled = false;
                        }
                    } else {
                        if (!studentDetail.isEnrolled) {

                            ++m_gain;
                            calcMultEntries(studentDetail);

                            studentDetail.isEnrolled = true;
                            studentDetail.isEverEnrolled = true;

                            if (schools.contains(item.getSchool()) && !exceptionGrid.getColumn(FIELD_SUB_STD_NAME)
                                    .contains(item.getStudent().getNameView())) {
                                exceptionGrid.append();
                                exceptionGrid.set(FIELD_SUB_SKL_NAME, item.getSchool().getName());
                                exceptionGrid.set(FIELD_FUND, item.getFundingCategory());
                                exceptionGrid.set(FIELD_GRADE, item.getGradeLevel());
                                exceptionGrid.set(FIELD_SUB_STD_LOCAL_ID, item.getStudent().getLocalId());
                                exceptionGrid.set(FIELD_SUB_STD_NAME, item.getStudent().getNameView());
                                exceptionGrid.set(FIELD_SUB_DATE, item.getAttendanceDate());
                            }
                        }
                    }
                }

                if (item.isScheduled()) {
                    m_scheduledDays.add(item.getAttendanceDate());
                }

                if (item.isScheduled()) {
                    String attCode = item.getAttendanceCode();
                    m_numNotEnrolled += NOT_ENROLLED_CODE.equals(attCode) ? 1 : 0;
                    if (!NOT_ENROLLED_CODE.equals(attCode)) {
                        m_numExcused += item.isAbsent() && item.isExcused() ? 1 : 0;
                        m_numUnExcused += item.isAbsent() && !item.isExcused() ? 1 : 0;
                        m_numSuspended +=
                                item.isSuspended() && !"IS".equals(item.getAttendance().getReasonCode()) ? 1 : 0;
                        m_numTruant += item.isTruant() ? 1 : 0;
                    }
                }
                m_numFirst +=
                        item.getAttendanceDate().equals(getStartDate(schoolOid, item.getStudent().getCalendarCode()))
                                && ADA_ENROLLMENT_TYPE.FIRST.equals(item.getEnrollmentType()) ? 1 : 0;
                m_numLast += item.getAttendanceDate().equals(endDate)
                        && ADA_ENROLLMENT_TYPE.LAST.equals(item.getEnrollmentType()) ? 1 : 0;
            }
        }

        /**
         * Calc mult entries.
         *
         * @param studentDetail StudentDetail
         */
        private void calcMultEntries(StudentDetail studentDetail) {
            if (!studentDetail.wasAsGain) {
                studentDetail.wasAsGain = true;
            } else if (!studentDetail.addedAsMultEntries) {
                ++m_numWithMultEntries;
                studentDetail.addedAsMultEntries = true;
            }
        }

        /**
         * Calc mult exits.
         *
         * @param studentDetail StudentDetail
         */
        private void calcMultExits(StudentDetail studentDetail) {
            if (!studentDetail.wasAsLoss) {
                studentDetail.wasAsLoss = true;
            } else if (!studentDetail.addedAsMultExits) {
                ++m_numWithMultExits;
                studentDetail.addedAsMultExits = true;
            }
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

        /**
         * Returns Student detail information.
         *
         * @param student SisStudent
         * @return StudentDetail
         */
        StudentDetail getStudentDetailFromMap(SisStudent student) {
            return m_students.get(student.getOid());
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

        /*
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
    }

    private static final String FIELD_ABS = "absent";
    private static final String FIELD_APPORT = "apport";
    private static final String FIELD_BEGIN = "begin";
    private static final String FIELD_DAYS = "numDays";
    private static final String FIELD_END = "end";
    private static final String FIELD_ENTRIES = "multEntries";
    private static final String FIELD_EXC_ABS = "exc_abs";
    private static final String FIELD_EXITS = "multExits";
    private static final String FIELD_EXCEPTION_GRID = "exceptionReport";
    private static final String FIELD_FIRST_ENTRY = "firstDays";
    private static final String FIELD_FUND = "fund";
    private static final String FIELD_GAIN = "gain";
    private static final String FIELD_GRADE = "grade";
    private static final String FIELD_LAST_ENTRY = "lastDays";
    private static final String FIELD_LOSS = "loss";
    private static final String FIELD_NOT_ENROLLED = "notEnrolled";
    private static final String FIELD_POSSIBLE_DAYS = "possibleDays";
    private static final String FIELD_REPORT_FORMAT = "reportFormat";
    private static final String FIELD_SCHEDULED = "scheduled";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_SCHOOL_NAME = "schoolName";
    private static final String FIELD_SUB_DATE = "date";
    private static final String FIELD_SUB_SKL_NAME = "schoolName";
    private static final String FIELD_SUB_STD_LOCAL_ID = "stdLocalId";
    private static final String FIELD_SUB_STD_NAME = "stdNameView";
    private static final String FIELD_SUSPENDED = "suspended";

    private static final String FIELD_UNEX_ABS = "unex_abs";

    private static final String INPUT_PARAM_PROCEDURE_ID = "procedureId";
    private static final String INPUT_PARAM_SCHOOL_OIDS = "schoolOids";
    private static final String INPUT_PARAM_FUND_TOTALS_TK = "fund-subtotals-TK-K";
    private static final String INPUT_PARAM_FUND_TOTALS_1_3 = "fund-subtotals-1-3";
    private static final String INPUT_PARAM_FUND_TOTALS_4_6 = "fund-subtotals-4-6";
    private static final String INPUT_PARAM_FUND_TOTALS_7_8 = "fund-subtotals-7-8";
    private static final String INPUT_PARAM_FUND_TOTALS_ALL = "fund-subtotals-All";
    private static final String INPUT_PARAM_INCLUDE_EXP_REP = "inclExpReport";
    private static final String INPUT_PARAM_INCLUDE_TOTALS = "includeTotals";
    private static final String INPUT_PARAM_SUBREPORT_ID = "exceptionReport";

    private static final String INPUT_PARAM_ALL_TOTALS_TK = "all-subtotals-TK-K";
    private static final String INPUT_PARAM_ALL_TOTALS_1_3 = "all-subtotals-1-3";
    private static final String INPUT_PARAM_ALL_TOTALS_4_6 = "all-subtotals-4-6";
    private static final String INPUT_PARAM_ALL_TOTALS_7_8 = "all-subtotals-7-8";
    private static final String INPUT_PARAM_ALL_TOTALS_ALL = "all-subtotals-All";

    private static final String INPUT_PARAM_EACH_GRADE = "showEachGL";
    private static final String INPUT_PARAM_INCLUDE_LEGEND = "includeLegend";

    private static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";

    private static final String REPORT_PARAM_CURRENT_DATE = "currentTime";
    private static final String REPORT_PARAM_CYCLE_FROM = "cycleFrom";
    private static final String REPORT_PARAM_CYCLE_TO = "cycleTo";
    private static final String REPORT_PARAM_END_DATE = "endDate";
    private static final String REPORT_PARAM_INCLUDE_EXCEPTION = "includeExcReport";
    private static final String REPORT_PARAM_ORGANIZATION = "organization";
    private static final String REPORT_PARAM_START_DATE = "startDate";

    private static final String REPORT_PARAM_INCLUDE_LEGEND = "includeLegend";
    private static final String REPORT_PARAM_USER_NAME = "generatedBy";
    private static final String POSTFIX_TK_K = "TK-K";
    private static final String POSTFIX_1_3 = "1-3";
    private static final String POSTFIX_4_6 = "4-6";
    private static final String POSTFIX_7_8 = "7-8";
    private static final String POSTFIX_ALL = "All";
    private static final String PREFIX_ALL = "All";
    private static final String TOTAL_PAGE_SCHOOL_OID = "Total School Page Oid";

    private static final String GRADE_00 = "00";
    private static final String GRADE_01 = "01";
    private static final String GRADE_02 = "02";
    private static final String GRADE_03 = "03";
    private static final String GRADE_04 = "04";
    private static final String GRADE_05 = "05";
    private static final String GRADE_06 = "06";
    private static final String GRADE_07 = "07";
    private static final String GRADE_08 = "08";
    private static final String GRADE_TK = "TK";

    protected Map<String, Integer> m_inSessionMap;
    protected ADADataHelper m_dataHelper;
    private EnrollmentManager m_enrollmentManager;

    private TreeMap<GroupItem, DetailItem> m_data = new TreeMap<GroupItem, DetailItem>();
    private PlainDate m_endDate = null;
    private Collection<SisSchool> m_schools = null;
    private Map<String, Map<String, PlainDate>> m_schoolCalendarEndDate = new HashMap();
    private PlainDate m_startDate = null;

    private Map<String, Map<String, List<SubTotal>>> m_dataMap = null;
    private String m_startCycle;
    private String m_endCycle;

    private boolean m_isFund_TK_K;
    private boolean m_isFund_1_3;
    private boolean m_isFund_4_6;
    private boolean m_isFund_7_8;
    private boolean m_isFund_All;

    private boolean m_isAll_TK_K;
    private boolean m_isAll_1_3;
    private boolean m_isAll_4_6;
    private boolean m_isAll_7_8;
    private boolean m_isAll_All;
    private boolean m_isTotal;
    private boolean m_includeEachGrade;
    private Report m_subreport;

    /**
     * Gets the end date.
     *
     * @return the m_endDate
     */
    public PlainDate getEndDate() {
        return m_endDate;
    }

    /**
     * Gets the end date.
     *
     * @param school SisSchool
     * @param calendarCode String
     * @return Plain date
     */
    public PlainDate getEndDate(SisSchool school, String calendarCode) {
        PlainDate endDate = m_endDate;
        if (school != null && !m_schoolCalendarEndDate.containsKey(school.getOid())) {
            if (m_enrollmentManager == null) {
                m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
            }
            Map<String, Set<PlainDate>> calendarData =
                    m_enrollmentManager.getCalendarLookup(school, m_startDate, m_endDate);
            Map<String, PlainDate> endDateData = new HashMap();
            for (Entry<String, Set<PlainDate>> entry : calendarData.entrySet()) {
                String calendar = entry.getKey();
                Set<PlainDate> dates = entry.getValue();
                PlainDate lastDate = null;
                for (PlainDate date : dates) {
                    if (lastDate == null || lastDate.before(date)) {
                        lastDate = date;
                    }
                }
                if (lastDate != null) {
                    endDateData.put(calendar, lastDate);
                }
            }
            m_schoolCalendarEndDate.put(school.getOid(), endDateData);
        }

        if (school != null) {
            Map<String, PlainDate> endDateData = m_schoolCalendarEndDate.get(school.getOid());
            if (endDateData.containsKey(calendarCode)) {
                endDate = endDateData.get(calendarCode);
            } else if (!endDateData.isEmpty()) {
                endDate = endDateData.values().iterator().next();
            }
        }
        return endDate;
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
     * Gets the start date.
     *
     * @param schoolOid String
     * @param calendarCode String
     * @return the m_startDate
     */
    public PlainDate getStartDate(String schoolOid, String calendarCode) {
        PlainDate startDate = m_startDate;
        PlainDate firstInSessionDate = m_dataHelper.getFirstInSessionDate(schoolOid, calendarCode);
        if (firstInSessionDate != null && firstInSessionDate.after(startDate)) {
            startDate = firstInSessionDate;
        }
        return startDate;
    }


    /**
     * Gather data.
     *
     * @return ReportDataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected ReportDataGrid gatherData() throws Exception {
        addParameter(REPORT_PARAM_ORGANIZATION, getOrganization());
        addParameter(REPORT_PARAM_START_DATE, m_startDate);
        addParameter(REPORT_PARAM_END_DATE, m_endDate);

        TimeZone tz = OrganizationManager.getTimeZone(getOrganization());
        SimpleDateFormat formatFull = new SimpleDateFormat("MMMMM dd, yyyy, h:mm a");
        formatFull.setTimeZone(tz);
        addParameter(REPORT_PARAM_CURRENT_DATE, formatFull.format(new Date()));

        addParameter(REPORT_PARAM_USER_NAME, getUser().getNameView());
        addParameter(REPORT_PARAM_CYCLE_FROM, m_startCycle);
        addParameter(REPORT_PARAM_CYCLE_TO, m_endCycle);
        ReportDataGrid grid = new ReportDataGrid();
        ReportDataGrid expReport = new ReportDataGrid();

        initializeInputParams();

        loadInputData(expReport);

        populateDataMap();

        populateGrid(grid);

        if (((Boolean) getParameter(INPUT_PARAM_INCLUDE_EXP_REP)).booleanValue()) {
            expReport.sort(
                    Arrays.asList(new String[] {FIELD_SUB_SKL_NAME, FIELD_FUND, FIELD_GRADE, FIELD_SUB_STD_NAME}),
                    true);
            expReport.beforeTop();
            grid.set(FIELD_EXCEPTION_GRID, expReport);
            grid.set(FIELD_REPORT_FORMAT, new ByteArrayInputStream(m_subreport.getCompiledFormat()));
        }
        grid.sort(FIELD_SUB_SKL_NAME, false);
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

        addParameter(REPORT_PARAM_INCLUDE_LEGEND, getParameter(INPUT_PARAM_INCLUDE_LEGEND));

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

        m_dataHelper.setCycle(null, m_startCycle, m_endCycle, null, true);

        PlainDate[] days = m_dataHelper.getDays();

        m_startDate = days[0];
        m_endDate = days[days.length - 1];

        initializeHelper();

        m_schools = getSchools();

        initializeInSessMap();

        m_dataMap = new HashMap<String, Map<String, List<SubTotal>>>();

        m_subreport = ReportUtils.getReport((String) getParameter(INPUT_PARAM_SUBREPORT_ID), getBroker());
    }

    /**
     * Add detail item to the data map and increment it.
     *
     * @param item ADADetailItem
     * @param exceptionGrid ReportDataGrid
     */
    private void addDetailItem(ADADetailItem item, ReportDataGrid exceptionGrid) {
        if (m_schools.contains(item.getSchool())) {
            GroupItem groupItem = new GroupItem(item.getSchool(), item.getFundingCategory(), item.getGradeLevel());
            DetailItem detailItem = m_data.get(groupItem);
            if (detailItem == null) {
                detailItem = new DetailItem();
                m_data.put(groupItem, detailItem);
            }
            detailItem.increment(item, exceptionGrid, m_schools);
        }
    }

    /**
     * Append additional (if needed) row to the grid and populate it with values.
     * Additional row is exist if additional input parameter is selected.
     *
     * @param grid ReportDataGrid
     * @param subTotals List<SubTotal>
     * @param school SisSchool
     * @param fund String
     * @param grade String
     */
    private void appendAdditionalRow(ReportDataGrid grid,
                                     List<SubTotal> subTotals,
                                     SisSchool school,
                                     String fund,
                                     String grade) {
        if (subTotals != null && !subTotals.isEmpty()) {
            SubTotal st = subTotals.get(0);

            grid.append();
            grid.set(FIELD_SCHOOL, school);
            grid.set(FIELD_SCHOOL_NAME, school != null ? school.getName() : "~");
            grid.set(FIELD_GRADE, grade);
            grid.set(FIELD_FUND, fund);

            grid.set(FIELD_BEGIN, Integer.valueOf(st.getBeginST()));
            grid.set(FIELD_GAIN, Integer.valueOf(st.getGainST()));
            grid.set(FIELD_ENTRIES, Integer.valueOf(st.getNumWithMultEntriesST()));
            grid.set(FIELD_LOSS, Integer.valueOf(st.getLossST()));
            grid.set(FIELD_EXITS, Integer.valueOf(st.getNumWithMultExitsST()));
            grid.set(FIELD_END, Integer.valueOf(st.getEndST()));

            grid.set(FIELD_APPORT, Integer.valueOf(st.getNumApportionmentST()));
            grid.set(FIELD_ABS, Integer.valueOf(st.getNumAbsentST()));
            grid.set(FIELD_EXC_ABS, Integer.valueOf(st.getNumExcusedST()));
            grid.set(FIELD_UNEX_ABS, Integer.valueOf(st.getNumUnExcusedST()));
            grid.set(FIELD_SUSPENDED, Integer.valueOf(st.getNumSuspendedST()));
            grid.set(FIELD_SCHEDULED, Integer.valueOf(st.getNumScheduledST()));
            grid.set(FIELD_FIRST_ENTRY, Integer.valueOf(st.getBeginST() + st.getGainST()));

            grid.set(FIELD_POSSIBLE_DAYS, Integer.valueOf(st.getPossibleDaysST()));

            grid.set(FIELD_LAST_ENTRY, Integer.valueOf(st.getNumLastST()));
            grid.set(FIELD_DAYS, Integer.valueOf(st.getScheduledDays().size()));
            grid.set(FIELD_NOT_ENROLLED, Integer.valueOf(st.getNumNotEnrolledST()));
        }
    }

    /**
     * Loads the schools used in the export.
     *
     * @return Collection<SisSchool>
     */
    private Collection<SisSchool> getSchools() {
        X2Criteria criteria = new X2Criteria();

        String exclSchool = null;
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getOrganization().getExtendedDataDictionary(),
                getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_EXCLUDE_SCHOOL);
        if (field != null) {
            exclSchool = field.getJavaName();

            if (exclSchool != null) {
                criteria.addNotEqualTo(exclSchool, BooleanAsStringConverter.TRUE);
            }
        }


        if (getSchool() != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        } else {
            String oids = (String) getParameter(INPUT_PARAM_SCHOOL_OIDS);
            if (!StringUtils.isEmpty(oids)) {
                Collection<String> oidList = StringUtils.convertDelimitedStringToList(oids, ',', true);
                criteria.addIn(X2BaseBean.COL_OID, oidList);
            } else {
                criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
                criteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
            }
        }
        QueryByCriteria sklQuery = new QueryByCriteria(SisSchool.class, criteria);
        sklQuery.addOrderBy(SisSchool.COL_NAME, true);
        return getBroker().getCollectionByQuery(sklQuery);
    }

    /**
     * Initialize helper.
     *
     * @throws X2BaseException exception
     */
    private void initializeHelper() throws X2BaseException {
        this.addParameter(REPORT_PARAM_START_DATE, m_startDate);
        this.addParameter(REPORT_PARAM_END_DATE, m_endDate);
        this.addParameter(REPORT_PARAM_INCLUDE_EXCEPTION, getParameter(INPUT_PARAM_INCLUDE_EXP_REP));

        // Lookup State report source data procedure
        String procedureId = (String) getParameter(INPUT_PARAM_PROCEDURE_ID);

        // This report has schools selector, so the school context must be false in all cases
        m_dataHelper.initialize(getPrivilegeSet(), false, null, getParameters(), getUser(), procedureId);
    }

    /**
     * Initialize input parameters.
     */
    private void initializeInputParams() {
        m_isFund_TK_K = ((Boolean) getParameter(INPUT_PARAM_FUND_TOTALS_TK)).booleanValue();
        m_isFund_1_3 = ((Boolean) getParameter(INPUT_PARAM_FUND_TOTALS_1_3)).booleanValue();
        m_isFund_4_6 = ((Boolean) getParameter(INPUT_PARAM_FUND_TOTALS_4_6)).booleanValue();
        m_isFund_7_8 = ((Boolean) getParameter(INPUT_PARAM_FUND_TOTALS_7_8)).booleanValue();
        m_isFund_All = ((Boolean) getParameter(INPUT_PARAM_FUND_TOTALS_ALL)).booleanValue();

        m_isAll_TK_K = ((Boolean) getParameter(INPUT_PARAM_ALL_TOTALS_TK)).booleanValue();
        m_isAll_1_3 = ((Boolean) getParameter(INPUT_PARAM_ALL_TOTALS_1_3)).booleanValue();
        m_isAll_4_6 = ((Boolean) getParameter(INPUT_PARAM_ALL_TOTALS_4_6)).booleanValue();
        m_isAll_7_8 = ((Boolean) getParameter(INPUT_PARAM_ALL_TOTALS_7_8)).booleanValue();
        m_isAll_All = ((Boolean) getParameter(INPUT_PARAM_ALL_TOTALS_ALL)).booleanValue();
        m_isTotal = ((Boolean) getParameter(INPUT_PARAM_INCLUDE_TOTALS)).booleanValue();

        m_includeEachGrade = ((Boolean) getParameter(INPUT_PARAM_EACH_GRADE)).booleanValue();
    }

    /**
     * Initialize in sess map.
     */
    private void initializeInSessMap() {
        m_inSessionMap = new HashMap<String, Integer>();
        for (School school : m_schools) {
            Integer inSessionDays = Integer.valueOf(0);
            Collection<String> cycles = m_dataHelper.getCycles();
            for (String cycle : cycles) {
                Collection<SchoolCalendarDate> dates = m_dataHelper.getInSessionDaysForCycle(cycle, school.getOid());
                inSessionDays = Integer.valueOf(inSessionDays.intValue() + dates.size());
            }
            m_inSessionMap.put(school.getOid(), inSessionDays);
        }
    }

    /**
     * Load data and populate data map with necessary values.
     *
     * @param expReport ReportDataGrid
     * @throws X2BaseException exception
     */
    private void loadInputData(ReportDataGrid expReport) throws X2BaseException {
        ADADetailIterator iterator = m_dataHelper.iterator();
        if (iterator != null) {
            List<PlainDate> days = Arrays.asList(m_dataHelper.getDays());
            try {
                ADADetailItem item = null;
                while ((item = iterator.next()) != null) {
                    if (days.contains(item.getAttendanceDate())) {
                        addDetailItem(item, expReport);
                    }
                }
            } finally {
                iterator.close();
            }
        }
    }

    /**
     * Populate data grid.
     *
     * @param grid ReportDataGrid
     */
    private void populateGrid(ReportDataGrid grid) {

        for (String sklOid : m_dataMap.keySet()) {
            if (!TOTAL_PAGE_SCHOOL_OID.equals(sklOid)) {
                populateSchoolGrid(grid, sklOid);
            }
        }

        if (m_isTotal) {
            populateSchoolGrid(grid, TOTAL_PAGE_SCHOOL_OID);
        }
    }

    /**
     * Populate school page in the report.
     *
     * @param grid ReportDataGrid
     * @param sklOid String
     */
    private void populateSchoolGrid(ReportDataGrid grid, String sklOid) {
        SisSchool school = (SisSchool) getBroker().getBeanByOid(SisSchool.class, sklOid);
        Map<String, List<SubTotal>> stMap = m_dataMap.get(sklOid);
        for (String fund : stMap.keySet()) {
            if (!fund.contains(POSTFIX_TK_K) && !fund.contains(POSTFIX_1_3) && !fund.contains(POSTFIX_4_6)
                    && !fund.contains(POSTFIX_7_8) &&
                    !fund.contains(POSTFIX_ALL)) {
                List<SubTotal> stList = stMap.get(fund);

                if (m_includeEachGrade) {
                    for (SubTotal st : stList) {
                        if (!POSTFIX_TK_K.equals(st.getGradesInSubTotal()) &&
                                !POSTFIX_1_3.equals(st.getGradesInSubTotal()) &&
                                !POSTFIX_4_6.equals(st.getGradesInSubTotal()) &&
                                !POSTFIX_7_8.equals(st.getGradesInSubTotal()) &&
                                !POSTFIX_ALL.equals(st.getGradesInSubTotal())) {
                            grid.append();
                            grid.set(FIELD_SCHOOL, school);
                            grid.set(FIELD_SCHOOL_NAME, school != null ? school.getName() : "~");
                            grid.set(FIELD_GRADE, st.getGradesInSubTotal());
                            grid.set(FIELD_FUND, fund);

                            grid.set(FIELD_BEGIN, Integer.valueOf(st.getBeginST()));
                            grid.set(FIELD_GAIN, Integer.valueOf(st.getGainST()));
                            grid.set(FIELD_ENTRIES, Integer.valueOf(st.getNumWithMultEntriesST()));
                            grid.set(FIELD_LOSS, Integer.valueOf(st.getLossST()));
                            grid.set(FIELD_EXITS, Integer.valueOf(st.getNumWithMultExitsST()));
                            grid.set(FIELD_END, Integer.valueOf(st.getEndST()));

                            grid.set(FIELD_APPORT, Integer.valueOf(st.getNumApportionmentST()));
                            grid.set(FIELD_ABS, Integer.valueOf(st.getNumAbsentST()));
                            grid.set(FIELD_EXC_ABS, Integer.valueOf(st.getNumExcusedST()));
                            grid.set(FIELD_UNEX_ABS, Integer.valueOf(st.getNumUnExcusedST()));
                            grid.set(FIELD_SUSPENDED, Integer.valueOf(st.getNumSuspendedST()));
                            grid.set(FIELD_SCHEDULED, Integer.valueOf(st.getNumScheduledST()));
                            grid.set(FIELD_FIRST_ENTRY, Integer.valueOf(st.getBeginST() + st.getGainST()));

                            grid.set(FIELD_POSSIBLE_DAYS, Integer.valueOf(st.getPossibleDaysST()));

                            grid.set(FIELD_LAST_ENTRY, Integer.valueOf(st.getNumLastST()));
                            grid.set(FIELD_DAYS, Integer.valueOf(st.getScheduledDays().size()));
                            grid.set(FIELD_NOT_ENROLLED, Integer.valueOf(st.getNumNotEnrolledST()));
                        }
                    }
                }

                if (m_isFund_TK_K) {
                    List<SubTotal> subTotals = stMap.get(fund + POSTFIX_TK_K);
                    appendAdditionalRow(grid, subTotals, school, fund, POSTFIX_TK_K);
                }

                if (m_isFund_1_3) {
                    List<SubTotal> subTotals = stMap.get(fund + POSTFIX_1_3);
                    appendAdditionalRow(grid, subTotals, school, fund, POSTFIX_1_3);
                }
                if (m_isFund_4_6) {
                    List<SubTotal> subTotals = stMap.get(fund + POSTFIX_4_6);
                    appendAdditionalRow(grid, subTotals, school, fund, POSTFIX_4_6);
                }
                if (m_isFund_7_8) {
                    List<SubTotal> subTotals = stMap.get(fund + POSTFIX_7_8);
                    appendAdditionalRow(grid, subTotals, school, fund, POSTFIX_7_8);
                }
                if (m_isFund_All) {
                    List<SubTotal> subTotals = stMap.get(fund + POSTFIX_ALL);
                    appendAdditionalRow(grid, subTotals, school, fund, POSTFIX_ALL);
                }

            }
        }
        if (m_isAll_TK_K) {
            List<SubTotal> subTotals = stMap.get(PREFIX_ALL + POSTFIX_TK_K);
            appendAdditionalRow(grid, subTotals, school, PREFIX_ALL, POSTFIX_TK_K);
        }
        if (m_isAll_1_3) {
            List<SubTotal> subTotals = stMap.get(PREFIX_ALL + POSTFIX_1_3);
            appendAdditionalRow(grid, subTotals, school, PREFIX_ALL, POSTFIX_1_3);
        }
        if (m_isAll_4_6) {
            List<SubTotal> subTotals = stMap.get(PREFIX_ALL + POSTFIX_4_6);
            appendAdditionalRow(grid, subTotals, school, PREFIX_ALL, POSTFIX_4_6);
        }
        if (m_isAll_7_8) {
            List<SubTotal> subTotals = stMap.get(PREFIX_ALL + POSTFIX_7_8);
            appendAdditionalRow(grid, subTotals, school, PREFIX_ALL, POSTFIX_7_8);
        }
        if (m_isAll_All) {
            List<SubTotal> subTotals = stMap.get(PREFIX_ALL + POSTFIX_ALL);
            appendAdditionalRow(grid, subTotals, school, PREFIX_ALL, POSTFIX_ALL);
        }

    }

    /**
     * Populate data map.
     */
    private void populateDataMap() {
        Map<String, List<SubTotal>> fundsSTMapTotal = new HashMap<String, List<SubTotal>>();
        List<SubTotal> subTotalListTotal = null;
        m_dataMap.put(TOTAL_PAGE_SCHOOL_OID, fundsSTMapTotal);
        for (Entry<GroupItem, DetailItem> groupEntry : m_data.entrySet()) {
            DetailItem detail = groupEntry.getValue();
            SisSchool school = groupEntry.getKey().getSchool();

            if (school != null && m_schools.contains(school)) {

                String gradesInSubTotal = groupEntry.getKey().getGrade();

                Map<String, List<SubTotal>> fundsSTMap = null;
                List<SubTotal> subTotalList = null;

                if (!m_dataMap.containsKey(school.getOid())) {
                    fundsSTMap = new HashMap<String, List<SubTotal>>();
                    m_dataMap.put(school.getOid(), fundsSTMap);
                } else {
                    fundsSTMap = m_dataMap.get(school.getOid());
                }

                if (!fundsSTMap.containsKey(groupEntry.getKey().getFund())) {
                    subTotalList = new ArrayList<SubTotal>();
                    fundsSTMap.put(groupEntry.getKey().getFund(), subTotalList);
                } else {
                    subTotalList = fundsSTMap.get(groupEntry.getKey().getFund());
                }

                if (!fundsSTMapTotal.containsKey(groupEntry.getKey().getFund())) {
                    subTotalListTotal = new ArrayList<SubTotal>();
                    fundsSTMapTotal.put(groupEntry.getKey().getFund(), subTotalListTotal);
                } else {
                    subTotalListTotal = fundsSTMapTotal.get(groupEntry.getKey().getFund());
                }

                SubTotal subTotalToAdd = new SubTotal(gradesInSubTotal);
                SubTotal subTotalToAddTotal = new SubTotal(gradesInSubTotal);

                if (subTotalList.contains(subTotalToAdd)) {
                    subTotalList.get(subTotalList.indexOf(subTotalToAdd)).increment(detail, school.getOid());
                } else {
                    subTotalToAdd.increment(detail, school.getOid());
                    subTotalList.add(subTotalToAdd);
                }

                if (subTotalListTotal.contains(subTotalToAddTotal)) {
                    subTotalListTotal.get(subTotalListTotal.indexOf(subTotalToAddTotal)).increment(detail,
                            school.getOid());
                } else {
                    subTotalToAddTotal.increment(detail, school.getOid());
                    subTotalListTotal.add(subTotalToAddTotal);
                }

            }
        }

        String allFund_TK_K = PREFIX_ALL + POSTFIX_TK_K;
        String allFund_1_3 = PREFIX_ALL + POSTFIX_1_3;
        String allFund_4_6 = PREFIX_ALL + POSTFIX_4_6;
        String allFund_7_8 = PREFIX_ALL + POSTFIX_7_8;
        String allFund_All = PREFIX_ALL + POSTFIX_ALL;

        if (!m_dataMap.isEmpty()) {
            for (String sklOid : m_dataMap.keySet()) {
                SubTotal subTotal_All_TK_K = new SubTotal(PREFIX_ALL + POSTFIX_TK_K);
                SubTotal subTotal_All_1_3 = new SubTotal(PREFIX_ALL + POSTFIX_1_3);
                SubTotal subTotal_All_4_6 = new SubTotal(PREFIX_ALL + POSTFIX_4_6);
                SubTotal subTotal_All_7_8 = new SubTotal(PREFIX_ALL + POSTFIX_7_8);
                SubTotal subTotal_All_All = new SubTotal(PREFIX_ALL + POSTFIX_ALL);

                Map<String, List<SubTotal>> stMap = m_dataMap.get(sklOid);
                List<String> funds = new ArrayList(stMap.keySet().size());
                funds.addAll(stMap.keySet());

                for (String fund : funds) {
                    String fund_TK_K = fund + POSTFIX_TK_K;
                    String fund_1_3 = fund + POSTFIX_1_3;
                    String fund_4_6 = fund + POSTFIX_4_6;
                    String fund_7_8 = fund + POSTFIX_7_8;
                    String fund_All = fund + POSTFIX_ALL;

                    SubTotal subTotal_TK_K = new SubTotal(POSTFIX_TK_K);
                    SubTotal subTotal_1_3 = new SubTotal(POSTFIX_1_3);
                    SubTotal subTotal_4_6 = new SubTotal(POSTFIX_4_6);
                    SubTotal subTotal_7_8 = new SubTotal(POSTFIX_7_8);
                    SubTotal subTotal_All = new SubTotal(POSTFIX_ALL);

                    List<SubTotal> stList = stMap.get(fund);
                    for (SubTotal st : stList) {
                        if (GRADE_00.equals(st.getGradesInSubTotal()) || GRADE_TK.equals(st.getGradesInSubTotal())) {
                            subTotal_TK_K.increment(st);
                            subTotal_All_TK_K.increment(st);
                        }

                        if (GRADE_01.equals(st.getGradesInSubTotal()) || GRADE_02.equals(st.getGradesInSubTotal()) ||
                                GRADE_03.equals(st.getGradesInSubTotal())) {
                            subTotal_1_3.increment(st);
                            subTotal_All_1_3.increment(st);
                        }
                        if (GRADE_04.equals(st.getGradesInSubTotal()) || GRADE_05.equals(st.getGradesInSubTotal()) ||
                                GRADE_06.equals(st.getGradesInSubTotal())) {
                            subTotal_4_6.increment(st);
                            subTotal_All_4_6.increment(st);
                        }
                        if (GRADE_07.equals(st.getGradesInSubTotal()) || GRADE_08.equals(st.getGradesInSubTotal())) {
                            subTotal_7_8.increment(st);
                            subTotal_All_7_8.increment(st);
                        }


                        subTotal_All.increment(st);
                        subTotal_All_All.increment(st);
                    }
                    List<SubTotal> st_TK_K_List = new ArrayList<SubTotal>();
                    st_TK_K_List.add(subTotal_TK_K);

                    List<SubTotal> st_1_3_List = new ArrayList<SubTotal>();
                    st_1_3_List.add(subTotal_1_3);

                    List<SubTotal> st_4_6_List = new ArrayList<SubTotal>();
                    st_4_6_List.add(subTotal_4_6);

                    List<SubTotal> st_7_8_List = new ArrayList<SubTotal>();
                    st_7_8_List.add(subTotal_7_8);

                    List<SubTotal> st_All_List = new ArrayList<SubTotal>();
                    st_All_List.add(subTotal_All);

                    stMap.put(fund_TK_K, st_TK_K_List);
                    stMap.put(fund_1_3, st_1_3_List);
                    stMap.put(fund_4_6, st_4_6_List);
                    stMap.put(fund_7_8, st_7_8_List);
                    stMap.put(fund_All, st_All_List);
                }

                List<SubTotal> st_All_TK_K_List = new ArrayList<SubTotal>();
                st_All_TK_K_List.add(subTotal_All_TK_K);

                List<SubTotal> st_All_1_3_List = new ArrayList<SubTotal>();
                st_All_1_3_List.add(subTotal_All_1_3);

                List<SubTotal> st_All_4_6_List = new ArrayList<SubTotal>();
                st_All_4_6_List.add(subTotal_All_4_6);

                List<SubTotal> st_All_7_8_List = new ArrayList<SubTotal>();
                st_All_7_8_List.add(subTotal_All_7_8);

                List<SubTotal> st_All_All_List = new ArrayList<SubTotal>();
                st_All_All_List.add(subTotal_All_All);

                stMap.put(allFund_TK_K, st_All_TK_K_List);
                stMap.put(allFund_1_3, st_All_1_3_List);
                stMap.put(allFund_4_6, st_All_4_6_List);
                stMap.put(allFund_7_8, st_All_7_8_List);
                stMap.put(allFund_All, st_All_All_List);

            }
        }
    }
}
