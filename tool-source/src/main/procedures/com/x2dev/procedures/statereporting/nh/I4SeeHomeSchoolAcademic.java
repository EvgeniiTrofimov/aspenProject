/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2009 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.nh;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * This class implements the data export for Home School Academic export.
 *
 * @author X2 Development Corporation
 */
public class I4SeeHomeSchoolAcademic extends I4SeeEnrollment {
    /**
     * Implementation of StateReportEntity to be used by the Home School Academic export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class I4SeeHomeSchoolAcademicEntity extends I4SeeEntity {
        /**
         * Helper class to aggregate needed data.
         *
         * @author Follett Software Company
         */
        private class EnrollmentsCourse {
            private KeyValuePair<StudentEnrollment, StudentEnrollment> m_pair = null;
            private Float m_prcntOfComplete = null;
            private SchoolCourse m_schoolCourse = null;
            private StudentScheduleSpan m_span = null;

            /**
             * Instantiates a new enrollments course.
             *
             * @param pair KeyValuePair<StudentEnrollment,StudentEnrollment>
             * @param span StudentScheduleSpan
             */
            public EnrollmentsCourse(KeyValuePair<StudentEnrollment, StudentEnrollment> pair,
                    StudentScheduleSpan span) {
                m_span = span;
                m_pair = pair;
            }

            /**
             * Gets the entry date.
             *
             * @return Plain date
             */
            public PlainDate getEntryDate() {
                return m_span.getEntryDate();
            }

            /**
             * Gets the exit date.
             *
             * @return Plain date
             */
            public PlainDate getExitDate() {
                return m_span.getExitDate();
            }

            /**
             * Gets the pair.
             *
             * @return Key value pair
             */
            public KeyValuePair<StudentEnrollment, StudentEnrollment> getPair() {
                return m_pair;
            }

            /**
             * Gets the school course.
             *
             * @return School course
             */
            public SchoolCourse getSchoolCourse() {
                if (m_schoolCourse == null) {
                    m_schoolCourse = m_span.getSection().getSchoolCourse();
                }
                return m_schoolCourse;
            }

            /**
             * Gets the percent complete.
             *
             * @return Float
             */
            public Float getPercentComplete() {
                if (m_prcntOfComplete == null) {
                    calcPrcntOfComplete();
                }
                return m_prcntOfComplete;
            }

            /**
             * Calc prcnt of complete.
             */
            private void calcPrcntOfComplete() {
                Collection<MasterTerm> masterTerms = m_span.getSection().getMasterTerms();
                ArrayList<String> termOids = new ArrayList<String>();
                ArrayList<ScheduleTerm> scheduleTerms = new ArrayList<ScheduleTerm>();
                for (MasterTerm masterTerm : masterTerms) {
                    termOids.add(masterTerm.getOid());
                    scheduleTerms.add(masterTerm.getScheduleTerm());
                }

                Map<Integer, Integer> periodsDaysNumbers = getDaysNumWithNumPeriods(termOids);

                Collection<SchoolCalendarDate> daysWithPeriods =
                        getAllDatesWithPeriods(m_span.getSection().getSchoolCourse().getSchoolOid(),
                                m_student.getCalendarCode(), scheduleTerms, periodsDaysNumbers);

                int totalNumOfPeriods = getTotalNumOfPeriods(daysWithPeriods, periodsDaysNumbers);
                float percentPerPeriod = totalNumOfPeriods == 0 ? 0 : 100f / totalNumOfPeriods;

                int studentNumOfPeriods = getStudentsNumOfPeriods(periodsDaysNumbers, daysWithPeriods);

                m_prcntOfComplete = Float.valueOf(percentPerPeriod * studentNumOfPeriods);
            }

            /**
             * Gets the all dates with periods.
             *
             * @param schoolOid String
             * @param calendarId String
             * @param terms Collection<ScheduleTerm>
             * @param periodsDaysNumbers Map<Integer,Integer>
             * @return Collection
             */
            private Collection<SchoolCalendarDate> getAllDatesWithPeriods(String schoolOid,
                                                                          String calendarId,
                                                                          Collection<ScheduleTerm> terms,
                                                                          Map<Integer, Integer> periodsDaysNumbers) {
                ArrayList<SchoolCalendarDate> datesWithPeriods = new ArrayList<SchoolCalendarDate>();

                Map<String, Collection<SchoolCalendarDate>> schoolCalendarDates =
                        m_data.m_schoolCalDatesMap.get(schoolOid);
                if (schoolCalendarDates != null) {
                    Collection<SchoolCalendarDate> dates = schoolCalendarDates.get(calendarId);
                    if (dates != null) {
                        for (SchoolCalendarDate schoolCalendarDate : dates) {
                            for (ScheduleTerm term : terms) {
                                ScheduleTermDate scheduleTermDate = term.getScheduleTermDates().iterator().next();
                                PlainDate startDate = scheduleTermDate.getStartDate();
                                PlainDate endDate = scheduleTermDate.getEndDate();
                                if (endDate == null || (getExitDate() != null && endDate.after(getExitDate()))) {
                                    endDate = getExitDate();
                                }

                                if (!schoolCalendarDate.getDate().before(startDate) &&
                                        !schoolCalendarDate.getDate().after(endDate) &&
                                        periodsDaysNumbers.keySet()
                                                .contains(Integer.valueOf(schoolCalendarDate.getScheduleDayNumber()))) {
                                    datesWithPeriods.add(schoolCalendarDate);
                                }
                            }
                        }
                    }
                }

                return datesWithPeriods;
            }

            /**
             * Gets the days num with num periods.
             *
             * @param termOids ArrayList<String>
             * @return Map
             */
            private Map<Integer, Integer> getDaysNumWithNumPeriods(ArrayList<String> termOids) {
                Map<Integer, Integer> periodsDaysNumbers = new HashMap<Integer, Integer>();
                X2Criteria schedMatrixCriteria = new X2Criteria();
                schedMatrixCriteria.addIn(ScheduleMatrix.REL_MASTER_SCHEDULE_MATRICES + ModelProperty.PATH_DELIMITER +
                        MasterScheduleMatrix.REL_MASTER_TERM + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                        termOids);
                SubQuery schedMatrixSubQuery =
                        new SubQuery(ScheduleMatrix.class, ScheduleMatrix.COL_SCHEDULE_DAY_OID, schedMatrixCriteria);

                X2Criteria scheduleDaysCriteria = new X2Criteria();
                scheduleDaysCriteria.addIn(X2BaseBean.COL_OID, schedMatrixSubQuery);

                String[] columns = {ScheduleDay.COL_NUMBER};

                ReportQueryByCriteria query =
                        new ReportQueryByCriteria(ScheduleDay.class, columns, scheduleDaysCriteria);
                ReportQueryIterator iterator = m_data.getBroker().getReportQueryIteratorByQuery(query);

                while (iterator.hasNext()) {
                    try {
                        Object[] item = (Object[]) iterator.next();
                        BigDecimal number = (BigDecimal) item[0];
                        Integer numberOfPeriodsForDate = periodsDaysNumbers.get(Integer.valueOf(number.intValue()));
                        if (numberOfPeriodsForDate == null) {
                            numberOfPeriodsForDate = Integer.valueOf(0);
                            periodsDaysNumbers.put(Integer.valueOf(number.intValue()), numberOfPeriodsForDate);
                        }
                        periodsDaysNumbers.put(Integer.valueOf(number.intValue()),
                                Integer.valueOf(numberOfPeriodsForDate.intValue() + 1));
                    } finally {
                        iterator.close();
                    }
                }
                return periodsDaysNumbers;
            }

            /**
             * Gets the students num of periods.
             *
             * @param periodsDaysNumbers Map<Integer,Integer>
             * @param daysWithPeriods Collection<SchoolCalendarDate>
             * @return int
             */
            private int getStudentsNumOfPeriods(Map<Integer, Integer> periodsDaysNumbers,
                                                Collection<SchoolCalendarDate> daysWithPeriods) {
                int studentNumOfPeriods = 0;

                // Use Home school intervals to determine percentage
                PlainDate homeSchoolStartDate = getPair().getKey().getEnrollmentDate();
                PlainDate homeSchoolEndDate =
                        getPair().getValue() == null ? null : getPair().getValue().getEnrollmentDate();

                PlainDate startDate =
                        m_span.getEntryDate().before(homeSchoolStartDate) ? homeSchoolStartDate : m_span.getEntryDate();
                PlainDate endDate = (m_span.getExitDate() == null ||
                        (homeSchoolEndDate != null && m_span.getExitDate().after(homeSchoolEndDate)))
                                ? homeSchoolEndDate
                                : m_span.getExitDate();

                for (SchoolCalendarDate date : daysWithPeriods) {
                    if (!date.getDate().before(startDate) &&
                            (endDate == null || !date.getDate().after(endDate))) {
                        studentNumOfPeriods = studentNumOfPeriods +
                                periodsDaysNumbers.get(Integer.valueOf(date.getScheduleDayNumber())).intValue();
                    }
                }
                return studentNumOfPeriods;
            }

            /**
             * Gets the total num of periods.
             *
             * @param daysWithPeriods Collection<SchoolCalendarDate>
             * @param numOfPeriodsPerDayMap Map<Integer,Integer>
             * @return int
             */
            private int getTotalNumOfPeriods(Collection<SchoolCalendarDate> daysWithPeriods,
                                             Map<Integer, Integer> numOfPeriodsPerDayMap) {
                int totalNumOfPeriods = 0;
                for (SchoolCalendarDate date : daysWithPeriods) {
                    int dayNumber = date.getScheduleDayNumber();
                    Integer numOfPeriodsForCurrentDate = numOfPeriodsPerDayMap.get(Integer.valueOf(dayNumber));
                    totalNumOfPeriods = totalNumOfPeriods + numOfPeriodsForCurrentDate.intValue();
                }

                return totalNumOfPeriods;
            }
        }

        /*
         * Enrollment record constants
         */
        private static final String ENROLLMENT_H1_CODE = "H1";

        protected I4SeeHomeSchoolAcademic m_data = null;
        protected ArrayList<EnrollmentsCourse> m_enrollmentsCourses = new ArrayList<EnrollmentsCourse>();
        protected SisStudent m_student = null;

        private static BigDecimal m_zero = new BigDecimal(0);

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public I4SeeHomeSchoolAcademicEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * @see com.x2dev.procedures.statereporting.nh.I4SeeEnrollment.I4SeeEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;

            String enrollmentStatus = null;
            FieldRetriever enrStatusRetriever = m_data.new Retrieve210EnrollmentStatus();
            FieldDefinition field = m_data.getFieldDefinition("Enrollment Status");
            try {
                enrollmentStatus = (String) enrStatusRetriever.getFieldValue(m_data, this, field);
            } catch (X2BaseException e) {
                e.printStackTrace();
            }

            if (StringUtils.isEmpty(enrollmentStatus) ||
                    !enrollmentStatus.equals("4")) {
                error = new StateReportValidationError(this, field,
                        "Only include students with Enrollment Status where state code = \"4\" ", "");
            }

            return error;
        }

        /**
         * Gets the current record.
         *
         * @return Enrollments course
         */
        public EnrollmentsCourse getCurrentRecord() {
            return m_enrollmentsCourses.get(getCurrentRow());
        }

        /**
         * @see com.x2dev.procedures.statereporting.nh.I4SeeEnrollment.I4SeeEntity#getEntry()
         */
        @Override
        public StudentEnrollment getEntry() {
            return getCurrentRecord().getPair().getKey();
        }

        /**
         * @see com.x2dev.procedures.statereporting.nh.I4SeeEnrollment.I4SeeEntity#getWithdrawal()
         */
        @Override
        public StudentEnrollment getWithdrawal() {
            return getCurrentRecord().getPair().getValue();
        }

        /**
         * @see com.x2dev.procedures.statereporting.nh.I4SeeEnrollment.I4SeeEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_data = (I4SeeHomeSchoolAcademic) data;

            m_student = (SisStudent) bean;

            /*
             * A student can appear multiple times on the report if they withdrew and re-entered
             * the district
             */
            m_enrollments = new LinkedList<KeyValuePair<StudentEnrollment, StudentEnrollment>>();

            Collection<StudentEnrollment> enrollments = getEnrollments(m_student, false);

            StudentEnrollment entry = null;
            StudentEnrollment withdrawal = null;

            for (StudentEnrollment enrollment : enrollments) {
                if (enrollment.getEnrollmentCode() != null && enrollment.getEnrollmentCode().startsWith("X2")) {
                    continue;
                }
                if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType()) &&
                        entry == null) {
                    entry = enrollment;
                }

                if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType()) &&
                        withdrawal == null) {
                    withdrawal = enrollment;
                }

                /*
                 * Special case! For students with certain status changes, there need to be
                 * faux E and W records for the state report.
                 *
                 * A type=='S' with a Reason containing "W12R12", two rows of data need to
                 * be added to the report:
                 * "E1", "last E date", "W12", "date"
                 * "R12", "date", "", ""
                 *
                 * A type=='Y' with a Reason containing "W1R1", two rows of data need to
                 * be added to the report:
                 * "E1", "last E date", "W1", "date"
                 * "R1", "date", "", ""
                 *
                 * The "last E date" should be the 'startDate' or the student's last Entry date.
                 */
                boolean statusChangeWasCounted = false;

                if (StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType()) ||
                        StudentEnrollment.YOG_CHANGE.equals(enrollment.getEnrollmentType())) {
                    String enrollmentCode = null;
                    String withdrawalCode = null;
                    String reentryCode = null;

                    if (!StringUtils.isEmpty(enrollment.getReasonCode())) {
                        if (enrollment.getReasonCode().startsWith(W1R1_KEY)) {
                            enrollmentCode = "E1";
                            withdrawalCode = "W1";
                            reentryCode = "R1";
                        } else if (enrollment.getReasonCode().startsWith(W12R12_KEY)) {
                            enrollmentCode = "E1";
                            withdrawalCode = "W12";
                            reentryCode = "R12";
                        }
                    }
                    /*
                     * if this status change is a special case (e.g. Reason contains W12R12 or W1R1)
                     * create the entry/withdrawal records needed
                     */
                    if (enrollmentCode != null) {
                        /*
                         * if the first record is a withdrawal, create an entry record
                         */
                        if (entry == null) {
                            entry = getMostRecentEnrollment();
                        }

                        /*
                         * use the current entry as the entry record, or create a dummy one
                         */
                        if (entry == null) {
                            entry = enrollment.clone();
                            // entry.setEnrollmentCode(enrollmentCode);
                            entry.setEnrollmentDate(data.getCurrentContext().getStartDate());
                            entry.setEnrollmentType(StudentEnrollment.ENTRY);
                        }
                        withdrawal = enrollment.clone();
                        withdrawal.setEnrollmentCode(withdrawalCode);
                        withdrawal.setEnrollmentType(StudentEnrollment.WITHDRAWAL);

                        addEnrollmentPair(entry, withdrawal);

                        /*
                         * set up the next entry record for the next pair (or final entry)
                         */
                        withdrawal = null;
                        entry = enrollment.clone();
                        entry.setEnrollmentCode(reentryCode);

                        if (StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType())) {
                            statusChangeWasCounted = true;
                        }
                    }
                }

                /*
                 * Finish span if Enrollment status was changed to not "4" (not Home school).
                 */
                if (StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType()) && !statusChangeWasCounted) {
                    String status = (String) enrollment.getFieldValueByBeanPath(m_data.m_enr210Field);
                    if (!StringUtils.isEmpty(status)) {
                        if (!status.equals("4")) {
                            String enrollmentCode = "dummy";

                            if (entry == null) {
                                entry = getMostRecentEnrollment();
                            }
                            /*
                             * use the current entry as the entry record, or create a dummy one
                             */
                            if (entry == null) {
                                entry = enrollment.clone();
                                // entry.setEnrollmentCode(enrollmentCode);
                                entry.setEnrollmentDate(data.getCurrentContext().getStartDate());
                                entry.setEnrollmentType(StudentEnrollment.ENTRY);
                            }
                            withdrawal = enrollment.clone();
                            withdrawal.setEnrollmentType(StudentEnrollment.WITHDRAWAL);
                            withdrawal.setEnrollmentCode(enrollmentCode);
                            withdrawal.setFieldValueByBeanPath(m_data.m_enr210Field,
                                    entry.getFieldValueByBeanPath(m_data.m_enr210Field));
                            addEnrollmentPair(entry, withdrawal);

                            /*
                             * set up the next entry record for the next pair (or final entry) with
                             * new Enrollment status.
                             */
                            withdrawal = null;
                            entry = enrollment.clone();
                            entry.setEnrollmentType(StudentEnrollment.ENTRY);
                            entry.setEnrollmentCode(enrollmentCode);
                        }
                    }
                }

                /*
                 * Add the entry, withdrawal pair
                 */
                if (withdrawal != null) {
                    addEnrollmentPair(entry, withdrawal);

                    entry = null;
                    withdrawal = null;
                }
            }

            /*
             * If enrollments is empty, student may be a summer withdrawal
             */
            Boolean includeSummerWithdrawals = (Boolean) data.getParameter(INCLUDE_SUMMER_WITHDRAWALS_PARAM);

            if (enrollments.isEmpty() && includeSummerWithdrawals != null && includeSummerWithdrawals.booleanValue()) {
                enrollments = getEnrollments(m_student, true);
                for (StudentEnrollment enrollment : enrollments) {
                    if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                        withdrawal = enrollment;
                    }
                }

                if (withdrawal != null) {
                    addEnrollmentPair(entry, withdrawal);
                }

                entry = null;
                withdrawal = null;
            }

            /*
             * This call covers the case if the student does not have any enrollment records or if
             * the student has a withdrawal followed by an entry
             */
            if (enrollments.isEmpty() ||
                    (!enrollments.isEmpty() && withdrawal == null && entry != null)) {
                addEnrollmentPair(entry, withdrawal);
            }

            if (m_enrollments.size() == 0 || m_enrollments.get(0).getKey() == null) {
                addMostRecentEnrollment(m_enrollments);
            }

            // Filter enrollments.
            Iterator<KeyValuePair<StudentEnrollment, StudentEnrollment>> iterator = m_enrollments.iterator();

            while (iterator.hasNext()) {
                KeyValuePair<StudentEnrollment, StudentEnrollment> enrollmentsPair = iterator.next();

                String enrollmentStatus = null;

                StudentEnrollment entryOfPair = enrollmentsPair.getKey();
                StudentEnrollment withdrawalOfPair = enrollmentsPair.getValue();
                StudentEnrollment enrollment = entryOfPair;
                /*
                 * Check to see if the enrollment status should be taken from the entry record or
                 * the withdrawal record (which ever is the latest). W12 and W1 withdrawals
                 * cause us to use the entry's enrollment status, not the withdrawals
                 */

                if (entryOfPair != null && withdrawalOfPair != null
                        && withdrawalOfPair.getEnrollmentDate().after(entryOfPair.getEnrollmentDate())
                        && !withdrawalOfPair.getEnrollmentCode().equals("W12")
                        && !withdrawalOfPair.getEnrollmentCode().equals("W1")) {
                    enrollment = withdrawalOfPair;
                }

                if (enrollment != null) {
                    enrollmentStatus = (String) WebUtils.getProperty(enrollment, m_data.m_enr210Field);
                }

                /*
                 * If the enrollmentStatus is empty, see if the status is present on the paired
                 * enrollment record
                 */
                if (StringUtils.isEmpty(enrollmentStatus)) {
                    /*
                     * status empty, try the 'other' paired record
                     */
                    if (enrollment == withdrawalOfPair && entryOfPair != null) {
                        enrollmentStatus = (String) WebUtils.getProperty(entryOfPair, m_data.m_enr210Field);
                    } else if (withdrawalOfPair != null) {
                        enrollmentStatus = (String) WebUtils.getProperty(withdrawalOfPair, m_data.m_enr210Field);
                    }
                }

                if (StringUtils.isEmpty(enrollmentStatus) || !enrollmentStatus.equals("4")) {
                    iterator.remove();
                }
            }

            for (KeyValuePair<StudentEnrollment, StudentEnrollment> enrollmentsPair : m_enrollments) {
                PlainDate entryDate = enrollmentsPair.getKey().getEnrollmentDate();
                PlainDate exitDate =
                        enrollmentsPair.getValue() == null ? null : enrollmentsPair.getValue().getEnrollmentDate();

                // Initialize school courses by overlapping enrollments pairs.
                List<StudentScheduleSpan> spans = m_data.m_helper.getStudentScheduleSpans(m_student);
                for (StudentScheduleSpan span : spans) {
                    if ((span.getExitDate() != null && exitDate != null &&
                            !span.getEntryDate().after(exitDate) && !span.getExitDate().before(entryDate)) ||
                            (span.getExitDate() != null && exitDate == null && !span.getExitDate().before(entryDate)) ||
                            (span.getExitDate() == null && exitDate != null) && !span.getEntryDate().after(entryDate) ||
                            span.getExitDate() == null && exitDate == null) {
                        SchoolCourse schoolCourse = span.getSection().getSchoolCourse();
                        Course course = schoolCourse.getCourse();

                        if (!HIGH_SCHOOL_GRADES.contains(m_student.getGradeLevel())
                                || (HIGH_SCHOOL_GRADES.contains(m_student.getGradeLevel())
                                        && course.getCredit().compareTo(m_zero) > 0)) {
                            EnrollmentsCourse enrCourse = new EnrollmentsCourse(enrollmentsPair, span);
                            m_enrollmentsCourses.add(enrCourse);
                        }
                    }
                }
            }

            setRowCount(m_enrollmentsCourses.size());
        }

        /**
         * Adds an entry/withdrawal pair of StudentEnrollment records to the m_enrollments list.
         *
         * @param entry StudentEnrollment
         * @param withdrawal StudentEnrollment
         */
        private void addEnrollmentPair(StudentEnrollment entry, StudentEnrollment withdrawal) {
            KeyValuePair<StudentEnrollment, StudentEnrollment> enrollmentPair =
                    new KeyValuePair<StudentEnrollment, StudentEnrollment>(entry, withdrawal);

            m_enrollments.add(enrollmentPair);
        }

        /**
         * Adds the most recent enrollment prior to the start of the school year.
         *
         * @param enrollments List<KeyValuePair<StudentEnrollment,StudentEnrollment>>
         */
        private void addMostRecentEnrollment(List<KeyValuePair<StudentEnrollment, StudentEnrollment>> enrollments) {
            List<StudentEnrollment> orderedEnrollments = getOrderedEnrollment((SisStudent) getBean(), null,
                    DateUtils.add(getData().getCurrentContext().getStartDate(), -1), null, false);
            Object value = null;
            while (orderedEnrollments.size() > 0) {
                if ((orderedEnrollments.get(0).getEnrollmentType().equals(StudentEnrollment.ENTRY)
                        && (orderedEnrollments.get(0).getEnrollmentCode() == null
                                || !orderedEnrollments.get(0).getEnrollmentCode().startsWith("X2")))
                        || (orderedEnrollments.get(0).getEnrollmentType().equals(StudentEnrollment.STATUS_CHANGE)
                                && (orderedEnrollments.get(0).getEnrollmentCode() == null
                                        || !orderedEnrollments.get(0).getEnrollmentCode().startsWith("W1")))) {
                    if (enrollments.size() > 0) {
                        value = enrollments.get(0).getValue();
                        enrollments.remove(0);
                    }
                    enrollments.add(0, new KeyValuePair(orderedEnrollments.get(0), value));
                    break;
                }
                orderedEnrollments.remove(0);
            }
        }

        /**
         * Returns a Collection of Student Enrollment beans for the passed student since the first
         * day
         * of the school year.
         *
         * @param student SisStudent
         * @param summerDates - true if collecting summer information
         *        NOT USED for B-O-Y report.
         * @return Collection of StudentEnrollment records for any date on or after the start of
         *         school
         */
        private Collection<StudentEnrollment> getEnrollments(SisStudent student, boolean summerDates) {
            Collection<StudentEnrollment> enrollments = new LinkedList<StudentEnrollment>();

            PlainDate startDate = null;
            PlainDate endDate = null;

            /*
             * B-O-Y report includes all dates from start of summer up to the report date
             */

            if (REPORT_TYPE_BOY.equals(((I4SeeEnrollment) getData()).m_reportType)) {
                startDate = (PlainDate) getData().getParameter(SUMMER_START_DATE_PARAM);
                endDate = (PlainDate) getData().getParameter(REPORT_DATE_PARAM);
            } else if (summerDates) {
                startDate = (PlainDate) getData().getParameter(SUMMER_START_DATE_PARAM);
                endDate = (PlainDate) getData().getParameter(SUMMER_END_DATE_PARAM);
            } else {
                startDate = ((I4SeeEnrollment) getData()).m_firstDayDate;
                endDate = ((I4SeeEnrollment) getData()).m_reportDate;
            }

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());
            criteria.addNotEqualTo(StudentEnrollment.COL_ENROLLMENT_CODE, ENROLLMENT_H1_CODE);
            criteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
            criteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, endDate);

            /*
             * Only include schools that are not CATE (Career And Technical Education)
             */

            String javaName = getData().translateAliasToJavaName(ALIAS_SKL_CATE_INDICATOR, false);
            if (!StringUtils.isEmpty(javaName)) {
                criteria.addNotEqualTo(StudentEnrollment.REL_SCHOOL + "." + javaName, Boolean.valueOf(true));
            }

            QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
            query.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);
            query.addOrderByAscending(StudentEnrollment.COL_TIMESTAMP);

            enrollments = getData().getBroker().getCollectionByQuery(query);

            if (!summerDates) {
                /*
                 * Go through records and remove any that should not be reported
                 *
                 * If a student has and E1 and a W2 on 8/26/08 along with an R2 at another
                 * school on 8/26/08, then the E1 and W2 should be ignored and only the
                 * active R2 (which will have to be corrected to an E1) should be reported
                 *
                 * If a student has an E1 and a W-anything on 8/26/08, and is NOT active at
                 * any other school, then only the withdrawal should be reported ? not the
                 * entry code.
                 *
                 * Translation: Do not report on any E records on the first date. A lone withdrawal
                 * record will generate a E record in the export for the first date. If a W is
                 * encountered, only report it if it is not followed by an E record on the same
                 * date.
                 */
                Collection<StudentEnrollment> recordsToRemove = new LinkedList<StudentEnrollment>();
                StudentEnrollment lastEnrollment = null;

                for (StudentEnrollment enrollment : enrollments) {
                    if (enrollment.getEnrollmentDate().equals(startDate)) {
                        if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                            lastEnrollment = enrollment;
                        } else {
                            // Check to see if the last record was an enrollment
                            if (lastEnrollment != null &&
                                    StudentEnrollment.WITHDRAWAL.equals(lastEnrollment.getEnrollmentType())) {
                                // Ignore the last withdrawal
                                recordsToRemove.add(lastEnrollment);
                                lastEnrollment = null;
                            }
                        }
                    }
                }

                enrollments.removeAll(recordsToRemove);
            }

            return enrollments;
        }

        /**
         * Gets the most recent enrollment prior to the start of the school year.
         *
         * @return Student enrollment
         */
        private StudentEnrollment getMostRecentEnrollment() {
            StudentEnrollment entry = null;
            List<StudentEnrollment> orderedEnrollments = getOrderedEnrollment((SisStudent) getBean(), null,
                    DateUtils.add(getData().getCurrentContext().getStartDate(), -1), null, false);
            while (orderedEnrollments.size() > 0) {
                if ((orderedEnrollments.get(0).getEnrollmentType().equals(StudentEnrollment.ENTRY)
                        && (orderedEnrollments.get(0).getEnrollmentCode() == null
                                || !orderedEnrollments.get(0).getEnrollmentCode().startsWith("X2")))
                        || (orderedEnrollments.get(0).getEnrollmentType().equals(StudentEnrollment.STATUS_CHANGE)
                                && (orderedEnrollments.get(0).getEnrollmentCode() == null
                                        || !orderedEnrollments.get(0).getEnrollmentCode().startsWith("W1")))) {
                    entry = orderedEnrollments.get(0);
                    break;
                }
                orderedEnrollments.remove(0);
            }
            return entry;
        }

        /**
         * Returns the list of ENTRY, STATUS CHANGE and WITHDRAWAL enrollment beans, ordered by date
         * and timestamp, for
         * the given student from the start date to the end date, inclusive. If either the start or
         * end
         * date parameters is null then there will be no lower or upper date boundary, respectively.
         * If
         * the school parameter is not null then the results will be filtered by that school.
         *
         * @param student the student whose enrollment records will be queried
         * @param startDate if not null then all records returned will be on or after this date
         * @param endDate if not null then all records returned will be on or before this date
         * @param school if not null then all records returned will be for this school
         * @param ascending if true then the beans will be listed in ascending order (oldest to
         *        newest),
         *        otherwise the beans will be reverse ordered (newest to oldest)
         *
         * @return A List of StudentEnrollment beans, this list may be empty if no enrollment
         *         records
         *         meet the specified criteria
         */
        private List getOrderedEnrollment(Student student,
                                          PlainDate startDate,
                                          PlainDate endDate,
                                          School school,
                                          boolean ascending) {
            ArrayList typeCodes = new ArrayList(2);
            typeCodes.add(StudentEnrollment.ENTRY);
            typeCodes.add(StudentEnrollment.WITHDRAWAL);
            typeCodes.add(StudentEnrollment.STATUS_CHANGE);

            Criteria criteria = new Criteria();
            criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());
            criteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, typeCodes);

            if (startDate != null) {
                criteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
            }

            if (endDate != null) {
                criteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, endDate);
            }

            if (school != null) {
                criteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, school.getOid());
            }

            QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
            query.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, ascending);
            query.addOrderBy(StudentEnrollment.COL_TIMESTAMP, ascending);

            ArrayList enrollments = new ArrayList();
            enrollments.addAll(getData().getBroker().getCollectionByQuery(query));

            return enrollments;
        }
    }

    /**
     * Returns value from SchoolCourse by field parameter that is alias.
     */
    protected class RetrieveFromSchoolCourse implements FieldRetriever {
        final String CREDITS = "i4see 1440";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            I4SeeHomeSchoolAcademicEntity hsaEntity = (I4SeeHomeSchoolAcademicEntity) entity;
            Student student = (Student) hsaEntity.getBean();
            String param = (String) field.getParameter();
            Object value = null;

            if (!CREDITS.equals(param) || HIGH_SCHOOL_GRADES.contains(student.getGradeLevel())) {
                value = hsaEntity.getCurrentRecord().getSchoolCourse().getFieldValueByAlias(param);
            }
            return value;
        }
    }

    /**
     * Returns entry/exit dates.
     * The entry date for Home School should be the date that the student entered into the course
     * and the exit date
     * should be the date that the student ended the course.
     * In most cases these dates will be in-line with the Term or Semester start / end dates but in
     * some cases where
     * the student enrolls late or withdraws the Student Schedule History should be used to
     * determine the correct
     * dates for Entry / Exit on the export.
     */
    protected class RetrieveHSADates implements FieldRetriever {
        private final String PARAM_ENTRY_DATE = "ENTRY-DATE";
        private final String PARAM_EXIT_DATE = "EXIT-DATE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            I4SeeHomeSchoolAcademicEntity hsaEntity = (I4SeeHomeSchoolAcademicEntity) entity;

            Object value = null;

            if (PARAM_ENTRY_DATE.equals(field.getParameter())) {
                value = hsaEntity.getCurrentRecord().getEntryDate();
            } else if (PARAM_EXIT_DATE.equals(field.getParameter())) {
                value = hsaEntity.getCurrentRecord().getExitDate();
            }

            return value;
        }
    }

    /**
     * Returns percent complete value.
     *
     * @author Follett Software Company
     */
    protected class RetrievePercentComplete implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            I4SeeHomeSchoolAcademicEntity hsaEntity = (I4SeeHomeSchoolAcademicEntity) entity;

            return hsaEntity.getCurrentRecord().getPercentComplete();
        }
    }

    /**
     * Returns SCED Common Course Code.
     * SCED Common Course Code Format is a concatenation of the following values: Prefix "SCED" +
     * SCED Course Code [i4see 1800] + Academic Level State Value of B, G, H, E.
     * Academic Level State Code value is on the Course Table using system field Academic Level -
     * CRS_ACADEMIC_LEVEL.
     */
    protected class RetrieveSCEDCommonCourseCode implements FieldRetriever {
        private static final String SCED_PREFIX = "SCED";
        private static final int SCED_LENGTH = 5;

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String code = null;

            I4SeeHomeSchoolAcademicEntity hsaEntity = (I4SeeHomeSchoolAcademicEntity) entity;
            Course crs = hsaEntity.getCurrentRecord().getSchoolCourse().getCourse();

            code = StringUtils
                    .unNullify(data.lookupStateValue(Course.class, m_fieldI4see1800,
                            (String) crs.getFieldValueByBeanPath(m_fieldI4see1800)));
            if (!StringUtils.isEmpty(code)) {
                if (code.length() < SCED_LENGTH) {
                    code = StringUtils.padLeft(code, SCED_LENGTH, '0');
                }
                code = SCED_PREFIX + code + StringUtils.unNullify(
                        data.lookupStateValue(Course.class, Course.COL_ACADEMIC_LEVEL, crs.getAcademicLevel()));
            }

            return code;
        }
    }

    protected String m_fieldI4see1440 = null;
    protected String m_fieldI4see1480 = null;
    protected String m_fieldI4see1800 = null;

    protected StudentHistoryHelper m_helper = null;

    protected Map<String, Map<String, Collection<SchoolCalendarDate>>> m_schoolCalDatesMap = null;

    private static final String ALIAS_CSED_COMMON_COURSE_CODE = "i4see 1800";
    private static final String ALIAS_LOCAL_CLASS_NAME = "i4see 1480";
    private static final String ALIAS_LOCAL_CLASS_CREDITS = "i4see 1440";
    private static final String ALIAS_SKL_CATE_INDICATOR = "i4see CATE";

    private static final String CALC_ID_CRS_PRCNT_COMPLETE = "CrsPrcntComplete";
    private static final String CALC_ID_HSA_DATES = "HSA-DATES";
    private static final String CALC_ID_I4SEE_FROM_SKL_CRS = "I4seeFromSklCrs";
    private static final String CALC_ID_SCED_COMMON_COURSE_CODE = "SCEDCommonCourseCode";

    /**
     * String for keying off special Reason Code
     */
    private final static String W12R12_KEY = "W12/R12";
    /**
     * String for keying off special Reason Code
     */
    private final static String W1R1_KEY = "W1/R1";

    protected final static Collection<String> HIGH_SCHOOL_GRADES = Arrays.asList("09", "10", "11", "12");

    /**
     * @see com.x2dev.procedures.statereporting.nh.I4SeeEnrollment#initialize()
     */
    @Override
    public void initialize() {
        super.initialize();

        m_fieldI4see1440 = translateAliasToJavaName(ALIAS_LOCAL_CLASS_CREDITS, true);
        m_fieldI4see1480 = translateAliasToJavaName(ALIAS_LOCAL_CLASS_NAME, true);
        m_fieldI4see1800 = translateAliasToJavaName(ALIAS_CSED_COMMON_COURSE_CODE, true);

        X2Criteria schoolCalendarDatesCriteria = new X2Criteria();
        schoolCalendarDatesCriteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR +
                ModelProperty.PATH_DELIMITER + SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        QueryByCriteria queryByCriteria = new QueryByCriteria(SchoolCalendarDate.class, schoolCalendarDatesCriteria);
        String[] columns = {SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                SchoolCalendar.COL_SCHOOL_OID,
                SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                        SchoolCalendar.COL_CALENDAR_ID};
        int[] sizes = {1, 180};

        m_schoolCalDatesMap = getBroker().getGroupedCollectionByQuery(queryByCriteria, columns, sizes);

        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
        // The export is cumulative End Of Year report so it would include any student that was
        // enrolled during the current school year
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, getCurrentContext().getEndDate());

        Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(CALC_ID_SCED_COMMON_COURSE_CODE, new RetrieveSCEDCommonCourseCode());
        calcs.put(CALC_ID_I4SEE_FROM_SKL_CRS, new RetrieveFromSchoolCourse());
        calcs.put(CALC_ID_CRS_PRCNT_COMPLETE, new RetrievePercentComplete());
        calcs.put(CALC_ID_HSA_DATES, new RetrieveHSADates());
        addCalcs(calcs);

        setEntityClass(I4SeeHomeSchoolAcademicEntity.class);
    }
}