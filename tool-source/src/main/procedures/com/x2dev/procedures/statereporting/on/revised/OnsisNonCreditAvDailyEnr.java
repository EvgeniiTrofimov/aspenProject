/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2022 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnsisStudentSchoolEnrollment.OnsisStudentEnrollmentEntity;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.util.Comparator;
import java.util.Date;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisNonCreditAvDailyEnr extends OnsisStudentScheduleCommon {

    public static class OnsisNonCreditAvDailyEnrEntity extends OnsisStudentScheduleCommonEntity {

        private OnsisNonCreditAvDailyEnr m_reportData;

        /**
         * Gets the ade september june.
         *
         * @return String
         */
        public String getAdeSeptemberJune() {
            final StringBuilder debugOutput = getGlobalData().getDebugDetail() ? new StringBuilder() : null;
            OnSection section = (OnSection) getSpan().getSection();
            Range<Date> range = getGlobalData().getDateRange().intersection(getSpan().getDateRange());
            PlainDate startDate = new PlainDate(range.getStart());
            PlainDate endDate = new PlainDate(range.getEnd());
            boolean isSummerDate = startDate != null && !startDate.before(getGlobalData().getSummerStartDate());
            Double ade = 0.0;
            Boolean isSummer = false;
            if ((isSummer && isSummerDate) || (!isSummer && !isSummerDate)) {
                ade = section.getAde(getGlobalData().getDateRange(), getStudent(), startDate, endDate,
                        debugOutput);
            }
            if (debugOutput != null) {
                m_reportData.log(debugOutput.toString());
            }

            return OnsisConstants.DECIMAL_FORMAT_ADE.format(ade);
        }

        /**
         * Gets the ade summer.
         *
         * @return String
         */
        public String getAdeSummer() {
            final StringBuilder debugOutput = getGlobalData().getDebugDetail() ? new StringBuilder() : null;
            OnSection section = (OnSection) getSpan().getSection();
            Range<Date> range = getGlobalData().getDateRange().intersection(getSpan().getDateRange());
            PlainDate startDate = new PlainDate(range.getStart());
            PlainDate endDate = new PlainDate(range.getEnd());
            boolean isSummerDate = startDate != null && !startDate.before(getGlobalData().getSummerStartDate());
            Double ade = 0.0;
            Boolean isSummer = true;
            if ((isSummer && isSummerDate) || (!isSummer && !isSummerDate)) {
                ade = section.getAde(getGlobalData().getDateRange(), getStudent(), startDate, endDate,
                        debugOutput);
            }
            if (debugOutput != null) {
                m_reportData.log(debugOutput.toString());
            }

            return OnsisConstants.DECIMAL_FORMAT_ADE.format(ade);
        }

        /**
         * Gets the class entity.
         *
         * @return Class wrapper
         */
        public String getClassCode() {
            String classCode = ((OnSection) getSpan().getSection()).getClassCode(getBroker());
            if (classCode != null) {
                classCode = fixupClassCode(getGlobalData(), classCode, getBroker());
            }
            return classCode;
        }

        /**
         * Gets the course offering type.
         *
         * @return String
         */
        public String getCourseOfferingType() {
            OnSection section = (OnSection) getSpan().getSection();
            return section == null ? null : section.getCourseOfferingType();
        }

        /**
         * Gets the program end date.
         *
         * @return Plain date
         */
        public PlainDate getProgramEndDate() {
            PlainDate endDate = getSpan().getExitDate();
            getReportData().log("getProgramEndDate-endDate = " + endDate);
            if (getSpan().getSection() != null) {
                OnSection section = (OnSection) getSpan().getSection();
                PlainDate termEndDate = section.getTermEndDate(getBroker(), getGlobalData().getDateRange());
                getReportData().log("getProgramEndDate-termEndDate = " + termEndDate);
                if (termEndDate != null && termEndDate.before(endDate)) {
                    endDate = termEndDate;
                }
            }
            return endDate;
        }

        /**
         * Gets the program end time.
         *
         * @return Plain time
         * @throws X2BaseException exception
         */
        public PlainTime getProgramEndTime() throws X2BaseException {
            OnSection section = (OnSection) getSpan().getSection();
            return section == null ? null : section.getConEdEndTime();
        }

        /**
         * Gets the program start date.
         *
         * @return Plain date
         */
        public PlainDate getProgramStartDate() {
            StudentScheduleSpan span = getEntrySpan();
            PlainDate spanEntryDate = span.getEntryDate();
            getReportData().log("getProgramStartDate-spanEntryDate = " + spanEntryDate);
            PlainDate enrStartDate =
                    m_reportData.getParentEntity().getOnsisEnrollmentStartDate();
            getReportData().log("getProgramStartDate-enrStartDate = " + enrStartDate);
            if (enrStartDate != null && enrStartDate.after(spanEntryDate)) {
                spanEntryDate = enrStartDate;
            }
            if (span.getSection() != null) {
                OnSection section = (OnSection) getSpan().getSection();
                PlainDate termStartDate = section.getStartDate(getBroker(), getGlobalData().getDateRange());
                getReportData().log("getProgramStartDate-termStartDate = " + termStartDate);
                if (termStartDate != null && termStartDate.after(spanEntryDate)) {
                    spanEntryDate = termStartDate;
                }
            }

            return spanEntryDate;
        }

        /**
         * Gets the program start time.
         *
         * @return Plain time
         * @throws X2BaseException exception
         */
        public PlainTime getProgramStartTime() throws X2BaseException {
            OnSection section = (OnSection) getSpan().getSection();
            return section == null ? null : section.getConEdStartTime();
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            /*
             * Primary query returns a Student.
             * Entity rows are one per enrollment span.
             */
            super.intitialize(data, bean);
            m_reportData = (OnsisNonCreditAvDailyEnr) data;


            Set<String> nonCreditCodes = m_reportData.getNonCreditProgramCodes();
            OnsisStudentEnrollmentEntity parentEntity = m_reportData.getParentEntity();
            PlainDate enrStartDate = parentEntity.getOnsisEnrollmentStartDate();
            Range<Date> limitingDateRange = Range.of(enrStartDate, parentEntity.getOnsisEnrollmentEndDate());

            m_studentScheduleSpans = m_student.getStudentScheduleSpans(getBroker()).stream()
                    .map(scheduleSpan -> (OnsisStudentScheduleSpan) scheduleSpan)
                    .filter(scheduleSpan -> getGlobalData().getCurrentSchoolOids()
                            .contains(scheduleSpan.getSection().getSchedule(getBroker()).getSchoolOid()))
                    .filter(scheduleSpan -> limitingDateRange == null
                            || scheduleSpan.getDateRange().isOverlap(limitingDateRange))
                    .filter(scheduleSpan -> scheduleSpan.getDateRange().isOverlap(getGlobalData().getDateRange()))
                    .filter(scheduleSpan -> !scheduleSpan.getExitDate()
                            .before(scheduleSpan.calculateCourseStartDate(getGlobalData(), enrStartDate)))
                    .filter(scheduleSpan -> nonCreditCodes
                            .contains(((OnSection) scheduleSpan.getSection()).getConedProgType()))
                    .collect(Collectors.groupingBy(span -> getKey(span)))
                    .values()
                    .stream()
                    .map(spanList -> new StudentScheduleSpanSet(spanList))
                    .sorted(new Comparator<StudentScheduleSpanSet>() {
                        @Override
                        public int compare(StudentScheduleSpanSet o1List, StudentScheduleSpanSet o2List) {
                            StudentScheduleSpan o1 = o1List.getSubmissionSpan();
                            StudentScheduleSpan o2 = o2List.getSubmissionSpan();

                            // CourseView
                            int compare = o1.getSection().getCourseView().compareTo(o2.getSection().getCourseView());

                            // Entry Date
                            if (compare == 0) {
                                compare = o1.getEntryDate().compareTo(o2.getEntryDate());
                            }

                            return compare;
                        }
                    })
                    .collect(Collectors.toList());
            setRowCount(m_studentScheduleSpans.size());
        }
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisNonCreditAvDailyEnrEntity.class);
    }
}
