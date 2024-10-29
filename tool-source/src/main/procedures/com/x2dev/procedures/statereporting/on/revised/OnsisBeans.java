/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.business.X2Broker;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchool;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSection;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpanFactory;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchedule;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnScheduleTeacher;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.GlobalData;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * The Class OnsisBeans.
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class OnsisBeans {

    public static class OnsisScheduleTeacher extends OnScheduleTeacher {

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = OnScheduleTeacher.FULL_DEFINITION
                .expandFilters(new Predicate<ToolBean>() {
                    @Override
                    public boolean test(ToolBean bean) {
                        OnsisScheduleTeacher mtc = (OnsisScheduleTeacher) bean;
                        GlobalData globalData =
                                (GlobalData) ToolBean.getPreference(OnsisStateReportData.PREFERENCE_GLOBAL_DATA);
                        return mtc.getDateIntervals(globalData.getBroker()).stream().filter(Objects::nonNull)
                                .map(range -> globalData.getDateRange().isOverlap(range))
                                .reduce(false, (value, element) -> value || element);
                    }
                });


        /**
         * @param columns
         * @param data
         */
        public OnsisScheduleTeacher(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
            // TODO Auto-generated constructor stub
        }
    }

    /**
     * The Class OnsisStudent.
     */
    public static class OnsisStudent extends OnStudent {

        /**
         * Instantiates a new onsis student.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnsisStudent(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        private Map<String, List<AnnualSpan>> m_enrollmentSpans;

        /**
         * Gets the student schedule spans.
         *
         * @param broker the broker
         * @param globalData the global data
         * @param limitingDateRange the limiting date range
         * @return the student schedule spans
         */
        public List<OnsisStudentScheduleSpan> getStudentScheduleSpans(X2Broker broker,
                                                                      GlobalData globalData,
                                                                      Range<Date> limitingDateRange) {
            return super.getStudentScheduleSpans(broker,
                    globalData.getCurrentSchoolOids(),
                    globalData.isSectionIncluded(),
                    globalData.getDateRange(),
                    limitingDateRange).stream()
                            .map(span -> (OnsisStudentScheduleSpan) span)
                            .collect(Collectors.toList());
        }

        public String getClassCode(X2Broker broker, GlobalData globalData) {
            String classCode = null;
            PlainDate endDate = globalData.getEndDate();
            PlainDate lastScheduledDate = globalData.getCurrentSchool().stream()
                    .map(skl -> skl.getActiveSchedule(broker))
                    .filter(Objects::nonNull)
                    .map(sch -> (OnSchedule) sch)
                    .filter(Objects::nonNull)
                    .map(sch -> sch.getLastScheduledDate(broker))
                    .filter(Objects::nonNull).max(Comparator.naturalOrder()).orElse(null);



            Optional<OnsisStudentScheduleSpan> homeroomSection =
                    getStudentScheduleSpans(broker).stream()
                            .map(scheduleSpan -> (OnsisStudentScheduleSpan) scheduleSpan)
                            .filter(scheduleSpan -> globalData.getCurrentSchoolOids()
                                    .contains(scheduleSpan.getSection().getSchedule(broker).getSchoolOid()))
                            .filter(scheduleSpan -> scheduleSpan.getDateRange().isOverlap(globalData.getDateRange()))
                            .filter(span -> {
                                OnSection mst = (OnSection) span.getSection();
                                Object courseCodeType = mst.getCourseCodeType();
                                if (StringUtils.isBlank((String) courseCodeType)) {
                                    return false;
                                }

                                /*
                                 * S-59233: CLASS_CODE (homeroom) must overlap count date after 2020
                                 */
                                DistrictSchoolYearContext currentContext = globalData.getCurrentContext();
                                if (currentContext.getSchoolYear() > 2020) {
                                    PlainDate exitDate = span.getExitDate();

                                    return OnSection.COURSE_CODE_TYPE_HOMEROOM.equals(courseCodeType)
                                            && (span.getDateRange().contains(endDate) ||
                                                    (lastScheduledDate != null && endDate.after(lastScheduledDate)
                                                            && !exitDate.before(lastScheduledDate)));
                                }

                                return OnSection.COURSE_CODE_TYPE_HOMEROOM.equals(courseCodeType);
                            })
                            .sorted(new Comparator<OnsisStudentScheduleSpan>() {
                                @Override
                                public int compare(OnsisStudentScheduleSpan ssc1, OnsisStudentScheduleSpan ssc2) {
                                    return ssc2.getExitDate().compareTo(ssc1.getExitDate());
                                }
                            }).findFirst();
            if (homeroomSection.isPresent()) {
                OnSection mst = (OnSection) homeroomSection.get().getSection();
                classCode = mst.getClassCode(broker);
            }
            if (classCode != null) {
                classCode = OnsisStateReportData.fixupClassCode(globalData, classCode, broker);
            }
            return classCode;
        }

        /**
         * Gets the enrollment spans.
         *
         * @param broker X2Broker
         * @param queryAsOfDate PlainDate
         * @param isBreakOnYog boolean
         * @param isBreakOnStatus boolean
         * @return List
         * @see com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent#getEnrollmentSpans(com.follett.fsc.core.k12.business.X2Broker,
         *      com.x2dev.utils.types.PlainDate, boolean, boolean)
         */
        @Override
        public List<AnnualSpan> getEnrollmentSpans(X2Broker broker,
                                                   PlainDate queryAsOfDate,
                                                   boolean isBreakOnYog,
                                                   boolean isBreakOnStatus) {
            String key = Boolean.toString(isBreakOnYog) + Boolean.toString(isBreakOnStatus) + queryAsOfDate;
            if (m_enrollmentSpans == null) {
                m_enrollmentSpans = new HashMap();
            }

            List<AnnualSpan> spans = m_enrollmentSpans.get(key);

            if (spans == null) {
                spans = getParentSpans(broker, isBreakOnYog, isBreakOnStatus).stream()
                        .flatMap(parent -> parent.getAnnualSpans().stream())
                        .collect(Collectors.toList());

                Collections.sort(spans);

                if (queryAsOfDate != null) {
                    spans = spans.stream()
                            .filter(span -> !span.getSpanStartDate().after(queryAsOfDate))
                            .collect(Collectors.toList());
                    spans.forEach(span -> span.setQueryAsOfDate(queryAsOfDate));
                }
                m_enrollmentSpans.put(key, spans);
            }
            return spans;
        }

    }

    /**
     * The Class OnsisStudentScheduleSpan.
     */
    public static class OnsisStudentScheduleSpan extends OnStudentScheduleSpan {

        private PlainDate m_retirementDate;

        /**
         * Instantiates a new onsis student schedule span.
         *
         * @param section ToolSection
         */
        public OnsisStudentScheduleSpan(ToolSection section) {
            super(section);
        }


        /**
         * Calculate course start date.
         *
         * @param globalData the global data
         * @param enrStartDate the enr start date
         * @return the plain date
         */
        public PlainDate calculateCourseStartDate(GlobalData globalData, PlainDate enrStartDate) {
            return calculateCourseStartDate(globalData.getBroker(), globalData.getDateRange(), enrStartDate);
        }

        /**
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpan#getExitDate()
         */
        @Override
        public PlainDate getExitDate() {
            PlainDate candidateDate = getRetirementDate(ToolBean.getBroker(true));
            return candidateDate == null ? super.getExitDate() : candidateDate;
        }

        /**
         * Gets the retirement date.
         *
         * @param broker the broker
         * @return the retirement date
         */
        public PlainDate getRetirementDate(X2Broker broker) {
            PlainDate value = null;
            if (getExitChange() != null) {
                if (m_retirementDate == null) {
                    Range<Date> sectionDateRange = getExitChange().getSection(broker).getSectionDateRange(broker);
                    PlainDate termEnd = (PlainDate) sectionDateRange.getEnd();
                    ToolSchool school = getExitChange().getSchedule(broker).getSchool(broker);
                    ToolStudent student = getExitChange().getStudent(broker);
                    boolean after = true;
                    PlainDate candidateDate = school.findSessionDate(broker, getExitChange().getDistrictContextOid(),
                            student.getCalendarCode(), super.getExitDate(), after);
                    m_retirementDate = termEnd == null || termEnd.after(candidateDate) ? candidateDate : termEnd;
                }
                value = m_retirementDate;
            }
            return value;
        }


    }

    /**
     * A factory for creating OnsisStudentScheduleSpan objects.
     */
    public static class OnsisStudentScheduleSpanFactory extends StudentScheduleSpanFactory {

        /**
         * Instantiate span.
         *
         * @param section ToolSection
         * @return StudentScheduleSpan
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpanFactory#instantiateSpan(com.x2dev.procedures.statereporting.common.ToolBean.ToolSection)
         */
        @Override
        public StudentScheduleSpan instantiateSpan(ToolSection section) {
            return new OnsisStudentScheduleSpan(section);
        }

    }

}
