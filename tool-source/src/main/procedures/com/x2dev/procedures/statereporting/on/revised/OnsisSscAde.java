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
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchedule;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentScheduleChange;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.FteMonthly;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisStudentClassEnrolment.OnsisStudentClassEnrollmentEntity;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisSscAde extends OnsisStateReportData {
    /**
     * The Class OnsisSscAdeEntity.
     */
    public static class OnsisSscAdeEntity extends OnsisStateReportEntity {
        private static final String FIELD_ID_COURSE_OFFERING_TYPE = "CourseOfferingType";

        private static final Collection<String> VALUE_EXEMPT_COURSE_OFFERING_TYPES =
                Arrays.asList("02", "10", "11", "12");

        private StudentScheduleSpan m_span;
        private OnsisStudent m_student;

        /**
         * Gets the ade september june.
         *
         * @return String
         */
        public String getAdeSeptemberJune() {
            final StringBuilder debugOutput = getGlobalData().getDebugDetail() ? new StringBuilder() : null;

            Double ade = 0.0;

            FteMonthly octoberFte = m_student.getFteMonthlyRecords(getBroker()).stream()
                    .filter(item -> OnsisConstants.SUBMISSION_SCHOOL_TYPE_PUBLIC_SECONDARY_OCTOBER
                            .contains(item.getMonth())
                            && OnsisConstants.VALUE_ATTENDANCE_TYPE_FULL_TIME.equals(item.getAttendanceType()))
                    .findAny()
                    .orElse(null);
            boolean isFullTimeOctober = octoberFte != null;

            FteMonthly marchFte = m_student.getFteMonthlyRecords(getBroker()).stream()
                    .filter(item -> OnsisConstants.SUBMISSION_SCHOOL_TYPE_PUBLIC_SECONDARY_MARCH
                            .contains(item.getMonth())
                            && OnsisConstants.VALUE_ATTENDANCE_TYPE_FULL_TIME.equals(item.getAttendanceType()))
                    .findAny()
                    .orElse(null);
            boolean isFullTimeMarch = marchFte != null;

            boolean isExemptCourseOffering =
                    VALUE_EXEMPT_COURSE_OFFERING_TYPES.contains(getFieldValue(FIELD_ID_COURSE_OFFERING_TYPE));

            getReportData().log("OnsisSscAdeEntity.getAdeSeptemberJune - isFullTimeOctober:" + isFullTimeOctober
                    + " isFullTimeMarch:" + isFullTimeMarch + " isExemptCourseOffering:" + isExemptCourseOffering);

            if (isExemptCourseOffering && isFullTimeOctober && isFullTimeMarch) {
                ade = 0.0;
            } else if (isDualCredit()) {
                ade = 0.0;
            } else {
                OnsisStudentScheduleCommon data = getOnsisStudentScheduleCommon(getReportData());
                if (data != null) {
                    OnSection section = (OnSection) m_span.getSection();

                    PlainDate startDate = m_span.getEntryDate();
                    PlainDate endDate = m_span.getLastMembershipDate();
                    if (getReportData().getParentEntity() instanceof OnsisStudentClassEnrollmentEntity) {
                        OnsisStudentClassEnrollmentEntity entity =
                                (OnsisStudentClassEnrollmentEntity) getReportData().getParentEntity();
                        startDate = entity.getCourseStartDate();
                        PlainDate testDate = entity.getCourseEndDate();
                        if (testDate != null && endDate.after(testDate)) {
                            endDate = testDate;
                        }
                    }
                    boolean isSummerDate = startDate != null && !startDate.before(getGlobalData().getSummerStartDate());
                    Boolean isSummer = false;
                    if ((isSummer && isSummerDate) || (!isSummer && !isSummerDate)) {
                        if (OnSection.CONED_SELF_STUDY.equals(section.getConedProgType())) {
                            // remove lessons performed when student is full time
                            startDate = null;
                            endDate = null;
                            if (isFullTimeOctober) {
                                startDate = getGlobalData().getDateFebruary1();
                            }
                            if (isFullTimeMarch) {
                                endDate = getGlobalData().getDateJanuary31();
                            }
                        }
                        getReportData().log("OnsisSscAdeEntity.getAdeSeptemberJune - Course date range : [" + startDate
                                + ", " + endDate + "]" + "\n");
                        List<Range<Date>> dateRanges = getStudentSscRanges(section.getSchool(getBroker()).getOid(),
                                Range.of(startDate, endDate));
                        getReportData().log("OnsisSscAdeEntity.getAdeSeptemberJune - Course-enrollment intersection : ["
                                + dateRanges.toString() + "]" + "\n");
                        ade = section.getAde(getGlobalData().getDateRange(), m_student, dateRanges, debugOutput);
                    }
                } else {
                    ade = 0.0;
                }
            }
            if (debugOutput != null) {
                getReportData().log(debugOutput.toString());
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

            Double ade = 0.0;
            OnSection section = (OnSection) m_span.getSection();
            Range<Date> range = getGlobalData().getDateRange().intersection(m_span.getDateRange());

            PlainDate startDate = m_span.getEntryDate();
            PlainDate endDate = m_span.getLastMembershipDate();
            if (getReportData().getParentEntity() instanceof OnsisStudentClassEnrollmentEntity) {
                OnsisStudentClassEnrollmentEntity entity =
                        (OnsisStudentClassEnrollmentEntity) getReportData().getParentEntity();
                startDate = entity.getCourseStartDate();
                PlainDate testDate = entity.getCourseEndDate();
                if (testDate != null && endDate.after(testDate)) {
                    endDate = testDate;
                }
            }

            boolean isSummerDate = startDate != null && !startDate.before(getGlobalData().getSummerStartDate());
            Boolean isSummer = true;
            getReportData()
                    .log("OnsisSscAdeEntity.getAdeSummer - isSummer:" + isSummer + " isSummerDate:" + isSummerDate
                            + " startDate:" + startDate + " summerStartDate:" + getGlobalData().getSummerStartDate());

            if ((isSummer && isSummerDate) || (!isSummer && !isSummerDate)) {
                List<Range<Date>> dateRanges =
                        getStudentSscRanges(section.getSchool(getBroker()).getOid(), Range.of(startDate, endDate));
                ade = section.getAde(getGlobalData().getDateRange(), m_student, dateRanges, debugOutput);
            }

            if (debugOutput != null) {
                getReportData().log(debugOutput.toString());
            }
            return OnsisConstants.DECIMAL_FORMAT_ADE.format(ade);
        }

        /**
         * Gets the course offering type.
         *
         * @return String
         */
        public String getCourseOfferingType() {
            OnSection section = (OnSection) m_span.getSection();
            return section == null ? null : section.getCourseOfferingType();
        }

        /**
         * }
         * Gets the report data.
         *
         * @return Onsis student class enrolment
         * @see com.follett.fsc.aspensif.framework.OnsisStateReportData.OnsisStateReportEntity#getReportData()
         */
        @Override
        public OnsisSscAde getReportData() {
            return (OnsisSscAde) super.getReportData();
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
            super.intitialize(data, bean);
            m_student = (OnsisStudent) bean;

            if (getReportData().getParentEntity() instanceof OnsisStudentClassEnrollmentEntity) {
                OnsisStudentClassEnrollmentEntity entity =
                        (OnsisStudentClassEnrollmentEntity) getReportData().getParentEntity();
                m_span = entity.getSpan();
            } else {
                setRowCount(0);
            }
        }

        /**
         * @param reportData
         * @return
         */
        private List<Range<Date>> getStudentSscRanges(String schoolOid, Range<Date> courseRange) {
            OnSection section = (OnSection) m_span.getSection();
            List<ToolStudentScheduleChange> sccList = m_student.getStudentScheduleChanges(getBroker())
                    .stream()
                    .filter(sscTest -> sscTest.getSectionOid().equals(section.getOid()))
                    .collect(Collectors.toList());
            ToolStudentSchedule ssc = m_student.getStudentSchedules(getBroker())
                    .stream()
                    .filter(sscTest -> sscTest.getSectionOid().equals(section.getOid()))
                    .findFirst()
                    .orElse(null);
            return m_student.getStudentScheduleSpans(getBroker(), ssc, sccList)
                    .stream()
                    .map(span -> courseRange.intersection(span.getDateRange()))
                    .collect(Collectors.toList());
        }

        /**
         * @param reportData
         * @return
         */
        private OnsisStudentScheduleCommon getOnsisStudentScheduleCommon(OnsisStateReportData initial) {
            if (initial instanceof OnsisStudentScheduleCommon) {
                return (OnsisStudentScheduleCommon) initial;
            }
            initial = initial.getParentReportData();
            return initial == null ? null : getOnsisStudentScheduleCommon(initial);
        }

        /**
         * @return
         */
        private boolean isDualCredit() {
            OnSection section = (OnSection) m_span.getSection();
            return OnSection.COURSE_CODE_TYPE_DCC.equals(section.getCourseCodeTypeState());
        }
    }

    /**
     * Builds the beans.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans()
     */
    @Override
    public void buildBeans() throws X2BaseException {
        ToolBean parentEntityBean = getParentEntity().getBean();
        List<ToolBean> beans = new ArrayList();
        beans.add(parentEntityBean);
        setBeans(beans);
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisSscAdeEntity.class);
    }
}
