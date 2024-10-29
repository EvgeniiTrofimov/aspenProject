/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2023 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.on.register;

import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer;
import com.x2dev.procedures.statereporting.on.register.original.OntarioAlias;
import com.x2dev.procedures.statereporting.on.revised.OnBeans;
import com.x2dev.procedures.statereporting.on.revised.OnsisConstants;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.GradesHelper;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.*;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.*;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection.ConedRegisterInfo;

import java.io.*;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.text.DateFormatSymbols;
import java.text.ParseException;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import java.util.stream.Collectors;

import static com.x2dev.reports.statereporting.on.register.EnrRegByCourseDataConstants.*;

/**
 * The Class EnrRegByCourseDataHelper.
 *
 * @author Follett Software Company
 * @copyright 2023
 */
public class EnrRegByCourseDataHelper extends OnRegisterReportBase {
    private static final long serialVersionUID = 1L;

    protected DataDictionary m_dictionary;

    protected Calendar m_calendar;

    protected PlainDate m_calHalf1EndDt;

    protected PlainDate m_calSummerEnrStartDt;

    protected PlainDate m_calSummerAssignAfterDt;

    protected OrganizationLocale m_locForLang;

    protected boolean m_byAssign;

    protected boolean m_byDate;

    protected Staff m_staffCurrent;

    protected Selection m_selectionMst;

    protected Map<String, MasterSchedule> m_mstToMstOid;

    protected Map<String, PlainDate> m_classStartDateToMstOid;

    protected Map<String, PlainDate> m_classEndDateToMstOid;

    protected Map<String, Boolean> m_classSummerCourseIndToMstOid;

    protected Map<String, List<String>> m_stdOidListToMstOid;

    protected List<String> m_stdOids;

    protected Map<String, String> m_stdCalIdToMstOid;

    protected Map<String, String> m_enrInfoToStdOidTypeMap;

    protected Map<String, Map<String, Map<Integer, String>>> m_sskOpIndToSklMthToSklOidToStdOidMap;

    protected Map<String, ScheduleTermDate> m_courseTrmDtToTrmOidMap;

    protected Map<String, List<String>> m_courseDeliveryTypeListByOnsisCodeMap;

    protected List<String> m_courseDeliveryTypeListForDualCredit;

    protected Map<String, Map<String, Integer>> m_assignCtRegToRegTypeToMstOidStdOid;

    protected Map<String, Integer> m_assignCtSummerToMstOidStdOid;

    protected Map<String, Integer> m_assignTotToMstOidMap;

    protected Map<String, Integer> m_assignCtNonOpRegToMstOidMap;

    protected Map<String, Integer> m_assignCtNonOpSummerToMstOidMap;

    protected Map<String, Integer> m_assignCtNonOpFtToMstOidMap;

    protected Map<String, Integer> m_pupilCtMaleOctToMstOidMap;

    protected Map<String, Integer> m_pupilCtFemaleOctToMstOidMap;

    protected Map<String, Integer> m_pupilCtMaleJunToMstOidMap;

    protected Map<String, Integer> m_pupilCtFemaleJunToMstOidMap;

    protected Map<String, Integer> m_pupilCtMaleAugToMstOidMap;

    protected Map<String, Integer> m_pupilCtFemaleAugToMstOidMap;

    protected Map<String, List<PlainDate>> m_classDtListToMstOidMap;

    protected Map<String, List<String>> m_classDayListToMstOidMap;

    protected Map<String, String> m_classStartEndTimeToMstOidMap;

    protected Map<String, EnrRegStudentStartEndDatesForSection> m_stdStartEndDatesToMstOidMap;

    protected Map<String, Integer> m_attFundedCtNonOpToMstOidMap;

    protected Map<String, List<String>> m_attListToMstOidStdOidMap;

    protected Map<String, Integer> m_attAbsentUnfundedCtToMstOidStdOid;

    protected Map<String, ScheduleBellPeriod> m_bpeTimeToBelOidPeriodNmMap;

    protected Map<String, String> m_mstReasons = new HashMap<>();

    protected static Calendar s_calendar = Calendar.getInstance();

    /**
     * The Class RptScheduleSpan.
     */
    public static class RptScheduleSpan extends OnStudentScheduleSpan {
        private List<ToolsSharedContainer.StudentScheduleSpan> m_spans;

        /**
         * Instantiates a new rpt schedule span.
         *
         * @param section the section
         */
        public RptScheduleSpan(ToolBean.ToolSection section) {
            super(section);
        }

        /**
         * Contains date.
         *
         * @param date the date
         * @return true, if successful
         */
        public boolean containsDate(Date date) {
            if (m_spans == null || m_spans.isEmpty()) {
                this.getDateRange().contains(date);
            }

            return m_spans.stream().anyMatch(span -> span.getDateRange().contains(date));
        }

        /**
         * Gets the date ranges.
         *
         * @param spans the spans
         * @return the date ranges
         */
        public List<Range<Date>> getDateRanges(List<OnAnnualSpan> spans) {
            Range<Date> scheduleRange = getDateRange();
            return spans.stream()
                    .map(enrSpan -> enrSpan.getDateRange().intersection(scheduleRange))
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        }

    }

    /**
     * The Class RptSection.
     */
    public static class RptSection extends OnSection {
        public static final ToolBeanColumn FIELD_CSK_DESCRIPTION =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().description());
        public static final ToolBeanColumn FIELD_CSK_NUMBER =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().number());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = OnSection.FULL_DEFINITION
                .expand(FIELD_CSK_DESCRIPTION,
                        FIELD_CSK_NUMBER);

        /**
         * Instantiates a new rpt section.
         *
         * @param columns the columns
         * @param data the data
         */
        public RptSection(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        private String m_calendarId;
        private List<PlainDate> m_classDateList;
        private List<String> m_classDayList;
        private String m_classStartEndTime;
        private Double m_durationHours;
        private String m_endDateString;
        private List<ConedRegisterInfo> m_infoConedRegisters = new ArrayList<>();
        private Integer m_numberOfSessionsSeptJune;
        private Integer m_numberOfSessionsSummer;
        private Double m_sessionHoursSeptJune;
        private Double m_sessionHoursSummer;
        private Integer m_sessionsRecognizedSeptJune;
        private Integer m_sessionsRecognizedSummer;
        private String m_startDateString;
        private PlainDate m_summerStartDate;
        private Integer m_totalDaysFundedReg;
        private Integer m_totalDaysFundedSummer;

        /**
         * Adds the coned register info.
         *
         * @param info the info
         */
        public void addConedRegisterInfo(ConedRegisterInfo info) {
            m_infoConedRegisters.add(info);
        }

        /**
         * Gets the calendar id.
         *
         * @return the calendar id
         */
        public String getCalendarId() {
            if (m_calendarId == null) {
                Optional<String> calendarId = m_infoConedRegisters.stream()
                        .map(ConedRegisterInfo::getStudent)
                        .collect(Collectors.groupingBy(ToolStudent::getCalendarCode))
                        .entrySet().stream().sorted(Comparator.comparing(entry -> entry.getValue().size()))
                        .map(Entry::getKey)
                        .reduce((first, second) -> second);

                m_calendarId = calendarId.orElse("");
            }

            return m_calendarId;
        }

        /**
         * Gets the class date list.
         *
         * @return the class date list
         */
        public List<PlainDate> getClassDateList() {
            if (m_classDateList == null) {
                m_classDateList = m_infoConedRegisters.stream()
                        .map(ConedRegisterInfo::getClassSessions)
                        .distinct()
                        .flatMap(Collection::stream)
                        .map(ClassSession::getPlainDate)
                        .distinct()
                        .sorted()
                        .collect(Collectors.toList());
            }

            return m_classDateList;
        }

        /**
         * Gets the class day list.
         *
         * @return the class day list
         */
        public List<String> getClassDayList() {
            if (m_classDayList == null) {
                m_classDayList = m_infoConedRegisters.stream()
                        .map(ConedRegisterInfo::getClassSessions)
                        .distinct()
                        .flatMap(Collection::stream)
                        .map(session -> {
                            s_calendar.setTime(session.getPlainDate());
                            int csdDateDayOfWeek = s_calendar.get(Calendar.DAY_OF_WEEK);
                            return Integer.valueOf(csdDateDayOfWeek).toString();
                        })
                        .distinct()
                        .collect(Collectors.toList());
            }

            return m_classDayList;
        }

        /**
         * Gets the class start end time.
         *
         * @return the class start end time
         */
        public String getClassStartEndTime() {
            if (m_classStartEndTime == null) {
                PlainTime startTimeMst = getStartTime(getCalendarId(), ToolBean.getBroker(true));
                PlainTime endTimeMst = getEndTime(getCalendarId(), ToolBean.getBroker(true));
                int durationMst = getDuration(getCalendarId(), ToolBean.getBroker(true));

                String classStartTime = CONST_EMPTY + CONST_COMMA + CONST_EMPTY;

                if (startTimeMst != null) {
                    s_calendar.setTime(startTimeMst);

                    int classStartHourInt = s_calendar.get(Calendar.HOUR_OF_DAY);
                    int classStartMinInt = s_calendar.get(Calendar.MINUTE);
                    String classStartAmPm = CONST_AM;

                    if (classStartHourInt == CONST_ZERO) {
                        classStartHourInt = CONST_PM_HOUR;
                        classStartAmPm = CONST_AM;
                    } else if (classStartHourInt > CONST_PM_HOUR) {
                        classStartHourInt = classStartHourInt - CONST_PM_HOUR;
                        classStartAmPm = CONST_PM;
                    } else if (classStartHourInt == CONST_PM_HOUR) {
                        classStartAmPm = CONST_PM;
                    }

                    classStartTime = CONST_FORMAT_INT_2.format(classStartHourInt) + CONST_COLON
                            + CONST_FORMAT_INT_2.format(classStartMinInt) + CONST_COMMA + classStartAmPm;
                }
                String classEndTime = CONST_EMPTY + CONST_COMMA + CONST_EMPTY;
                if (endTimeMst != null) {
                    s_calendar.setTime(endTimeMst);

                    int classEndHourInt = s_calendar.get(Calendar.HOUR_OF_DAY);
                    int classEndMinInt = s_calendar.get(Calendar.MINUTE);
                    String classEndAmPm = CONST_AM;

                    if (classEndHourInt == CONST_ZERO) {
                        classEndHourInt = CONST_PM_HOUR;
                        classEndAmPm = CONST_AM;
                    } else if (classEndHourInt > CONST_PM_HOUR) {
                        classEndHourInt = classEndHourInt - CONST_PM_HOUR;
                        classEndAmPm = CONST_PM;
                    } else if (classEndHourInt == CONST_PM_HOUR) {
                        classEndAmPm = CONST_PM;
                    }

                    classEndTime = CONST_FORMAT_INT_2.format(classEndHourInt) + CONST_COLON
                            + CONST_FORMAT_INT_2.format(classEndMinInt) + CONST_COMMA + classEndAmPm;
                }

                String classDuration =
                        CONST_FORMAT_DBL_2_CALC_ONLY.format(Double.valueOf(durationMst) / CONST_HOUR_TO_MINS_DBL);

                m_classStartEndTime =
                        classStartTime + CONST_COMMA + classEndTime + CONST_COMMA + classDuration;
            }
            return m_classStartEndTime;
        }

        /**
         * Gets the count october.
         *
         * @return the count october
         */
        public Integer getCountOctober() {
            return m_infoConedRegisters.stream()
                    .filter(ConedRegisterInfo::isOctober)
                    .mapToInt(info -> 1)
                    .sum();
        }

        /**
         * Gets the count sept june.
         *
         * @return the count sept june
         */
        public Integer getCountSeptJune() {
            return m_infoConedRegisters.stream()
                    .filter(info -> !info.isSummer())
                    .mapToInt(info -> 1)
                    .sum();
        }

        /**
         * Gets the count summer.
         *
         * @return the count summer
         */
        public Integer getCountSummer() {
            return m_infoConedRegisters.stream()
                    .filter(ConedRegisterInfo::isSummer)
                    .mapToInt(info -> 1)
                    .sum();
        }

        /**
         * Gets the duration hours.
         *
         * @return the duration hours
         */
        public Double getDurationHours() {
            if (m_durationHours == null) {
                m_durationHours = Double.valueOf(getDuration(getCalendarId(),
                        ToolBean.getBroker(true))) / CONST_HOUR_TO_MINS_DBL;
            }

            return m_durationHours;
        }

        /**
         * Gets the end date string.
         *
         * @return the end date string
         */
        public String getEndDateString() {
            if (m_endDateString == null) {
                ToolsSharedContainer.Range<Date> dateRange = getDistrictContext(ToolBean.getBroker(true)).getDateRange();
                PlainDate endDate = getTermEndDate(ToolBean.getBroker(true), dateRange);
                m_endDateString = String.format("%10s", endDate.toString().replaceAll("[^0-9]", ""));
            }

            return m_endDateString;
        }

        /**
         * Gets the number of sessions sept june.
         *
         * @return the number of sessions sept june
         */
        public Integer getNumberOfSessionsSeptJune() {
            if (m_numberOfSessionsSeptJune == null) {
                m_numberOfSessionsSeptJune = 0;
                List<PlainDate> dateList = getClassDateList();

                if (dateList != null && !dateList.isEmpty()
                        && dateList.iterator().next().before(getSummerStartDate())) {
                    m_numberOfSessionsSeptJune = dateList.size();
                }
            }

            return m_numberOfSessionsSeptJune;
        }

        /**
         * Gets the number of sessions summer.
         *
         * @return the number of sessions summer
         */
        public Integer getNumberOfSessionsSummer() {
            if (m_numberOfSessionsSummer == null) {
                m_numberOfSessionsSummer = 0;
                List<PlainDate> dateList = getClassDateList();

                if (dateList != null && !dateList.isEmpty()
                        && !dateList.iterator().next().before(getSummerStartDate())) {
                    m_numberOfSessionsSummer = dateList.size();
                }
            }

            return m_numberOfSessionsSummer;
        }

        /**
         * Gets the session hours pupil sept june.
         *
         * @return the session hours pupil sept june
         */
        public Double getSessionHoursPupilSeptJune() {
            int pupilDays = m_infoConedRegisters.stream()
                    .filter(info -> !(((RptStudent) info.getStudent()).isOtherPupil() && info.isSummer()))
                    .mapToInt(ConedRegisterInfo::getTotalDays)
                    .sum();

            return getSessionHoursSeptJune() * pupilDays;
        }

        /**
         * Gets the session hours pupil summer.
         *
         * @return the session hours pupil summer
         */
        public Double getSessionHoursPupilSummer() {
            int pupilDays = m_infoConedRegisters.stream()
                    .filter(info -> !((RptStudent) info.getStudent()).isOtherPupil() && info.isSummer())
                    .mapToInt(ConedRegisterInfo::getTotalDays)
                    .sum();

            return getSessionHoursSummer() * pupilDays;
        }

        /**
         * Gets the session hours sept june.
         *
         * @return the session hours sept june
         */
        public Double getSessionHoursSeptJune() {
            if (m_sessionHoursSeptJune == null) {
                m_sessionHoursSeptJune = Double.valueOf(0);

                if (getNumberOfSessionsSeptJune() > 0) {
                    m_sessionHoursSeptJune = getDurationHours();
                }
            }
            return m_sessionHoursSeptJune;
        }

        /**
         * Gets the session hours summer.
         *
         * @return the session hours summer
         */
        public Double getSessionHoursSummer() {
            if (m_sessionHoursSummer == null) {
                m_sessionHoursSummer = Double.valueOf(0);

                if (getNumberOfSessionsSummer() > 0) {
                    m_sessionHoursSummer = getDurationHours();
                }
            }

            return m_sessionHoursSummer;
        }

        /**
         * Gets the sessions recognized sept june.
         *
         * @return the sessions recognized sept june
         */
        public Integer getSessionsRecognizedSeptJune() {
            if (m_sessionsRecognizedSeptJune == null) {
                m_sessionsRecognizedSeptJune = m_infoConedRegisters.stream()
                        .filter(info -> !((RptStudent) info.getStudent()).isOtherPupil() && !info.isSummer())
                        .mapToInt(ConedRegisterInfo::getTotalDays)
                        .sum();
            }

            return m_sessionsRecognizedSeptJune;
        }

        /**
         * Gets the sessions recognized summer.
         *
         * @return the sessions recognized summer
         */
        public Integer getSessionsRecognizedSummer() {
            if (m_sessionsRecognizedSummer == null) {
                m_sessionsRecognizedSummer = m_infoConedRegisters.stream()
                        .filter(info -> !((RptStudent) info.getStudent()).isOtherPupil())
                        .filter(ConedRegisterInfo::isSummer)
                        .mapToInt(ConedRegisterInfo::getTotalDays).sum();
            }

            return m_sessionsRecognizedSummer;
        }

        /**
         * Gets the school course description.
         *
         * @return the school course description
         */
        public String getSchoolCourseDescription() {
            return getValueString(FIELD_CSK_DESCRIPTION);
        }

        /**
         * Gets the school course description.
         *
         * @return the school course description
         */
        public String getSchoolCourseNumber() {
            return getValueString(FIELD_CSK_NUMBER);
        }

        /**
         * Gets the start date string.
         *
         * @return the start date string
         */
        public String getStartDateString() {
            if (m_startDateString == null) {
                ToolsSharedContainer.Range<Date> dateRange = getDistrictContext(ToolBean.getBroker(true)).getDateRange();
                PlainDate startDate = getStartDate(ToolBean.getBroker(true), dateRange);
                m_startDateString = String.format("%10s", startDate.toString().replaceAll("[^0-9]", ""));
            }

            return m_startDateString;
        }

        /**
         * Gets the summer start date.
         *
         * @return the summer start date
         */
        public PlainDate getSummerStartDate() {
            if (m_summerStartDate == null) {
                int year = getSchedule(ToolBean.getBroker(true))
                        .getDistrictContext(ToolBean.getBroker(true)).getSchoolYear();
                m_summerStartDate = ToolBean.getPlainDateValue(year, Calendar.JUNE, 25);

            }

            return m_summerStartDate;
        }

        /**
         * Gets the total days funded reg.
         *
         * @return the total days funded reg
         */
        public Integer getTotalDaysFundedReg() {
            if (m_totalDaysFundedReg == null) {
                m_totalDaysFundedReg = m_infoConedRegisters.stream()
                        .filter(info -> !info.isSummer() && !((RptStudent) info.getStudent()).isOtherPupil())
                        .map(ConedRegisterInfo::getTotalDays)
                        .reduce(0, Integer::sum);
            }

            return m_totalDaysFundedReg;
        }

        /**
         * Gets the total days funded summer.
         *
         * @return the total days funded summer
         */
        public Integer getTotalDaysFundedSummer() {
            if (m_totalDaysFundedSummer == null) {
                m_totalDaysFundedSummer = m_infoConedRegisters.stream()
                        .filter(info -> info.isSummer() && !((RptStudent) info.getStudent()).isOtherPupil())
                        .map(ConedRegisterInfo::getTotalDays)
                        .reduce(0, Integer::sum);
            }

            return m_totalDaysFundedSummer;
        }
    }

    /**
     * The Class RptStudent.
     */
    public static class RptStudent extends OnStudent {
        private List<OnAnnualSpan> m_annualSpans;
        private String m_boardResidentStatus;
        private String m_fteMar;
        private String m_fteOct;
        private ConedRegisterInfo m_infoConedRegister;
        private Map<String, List<OnAnnualSpan>> m_spansLookup;
        private Integer m_totalDaysFundedReg;
        private Integer m_totalDaysFundedSummer;

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = OnStudent.FULL_DEFINITION;

        /**
         * Instantiates a new rpt student.
         *
         * @param columns the columns
         * @param data the data
         */
        public RptStudent(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the board resident status.
         *
         * @param broker the broker
         * @param dictionaryExtractor the dictionary extractor
         * @return the board resident status
         */
        public String getBoardResidentStatus(X2Broker broker, DictionaryExtractor dictionaryExtractor) {
            if (m_boardResidentStatus == null) {
                List<OnAnnualSpan> spans = getEnrollmentSpans();
                if (!spans.isEmpty()) {
                    OnAnnualSpan annualSpan = getEnrollmentSpans().iterator().next();
                    m_boardResidentStatus = annualSpan.getBoardResidentStatus(dictionaryExtractor);
                    if (StringUtils.isEmpty(m_boardResidentStatus)) {
                        OnAnnualSpan primarySpan = annualSpan.getBestPrimarySpanFor(broker, null);
                        if (primarySpan == null) {
                            OnStudent student = annualSpan.getStudent();
                            String errorMessage = "Couldn't find primary span for student: "
                                    + "\n Student name: "
                                    + student.getNameView()
                                    + ", \n OEN: "
                                    + student.getOenRaw()
                                    + ", \n School Name: "
                                    + student.getSchool(getBroker(false)).getName();

                            throw new NoSuchElementException(errorMessage);
                        }

                        m_boardResidentStatus = primarySpan.getBoardResidentStatus(dictionaryExtractor);
                    }
                }
                if (m_boardResidentStatus == null) {
                    m_boardResidentStatus = "";
                }
            }

            return m_boardResidentStatus;
        }

        /**
         * Gets the enrollment spans.
         *
         * @return the enrollment spans
         */
        public List<OnAnnualSpan> getEnrollmentSpans() {
            if (m_annualSpans == null) {
                throw new IllegalStateException("Spans must be initialized");
            }

            return m_annualSpans;
        }

        /**
         * Gets the enrollment spans.
         *
         * @param broker the broker
         * @param ctxOid the ctx oid
         * @param schoolOids the school oids
         * @param dateRange the date range
         * @return the enrollment spans
         */
        public List<OnAnnualSpan> getEnrollmentSpans(X2Broker broker,
                                                     String ctxOid,
                                                     Collection<String> schoolOids,
                                                     Range<Date> dateRange) {
            String key = ctxOid + schoolOids + dateRange;

            if (m_spansLookup == null) {
                m_spansLookup = new HashMap<>();
            }

            if (!m_spansLookup.containsKey(key)) {
                m_spansLookup.put(key, getEnrollmentSpans(broker, false, false).stream()
                        .filter(span -> {
                            boolean isSchool = schoolOids.contains(span.getSchool().getOid());
                            boolean isDate = dateRange.isOverlap(span.getDateRange());
                            boolean isContext = span.getContext().getOid().equals(ctxOid);
                            return isSchool && isDate && isContext;
                        })
                        .map(span -> (OnBeans.OnAnnualSpan) span)
                        .collect(Collectors.toList()));
            }

            return m_spansLookup.get(key);
        }

        /**
         * Gets the enrollment spans.
         *
         * @param broker the broker
         * @param ctxOid the ctx oid
         * @param schoolOids the school oids
         * @param date the date
         * @return the enrollment spans
         */
        public List<OnAnnualSpan> getEnrollmentSpans(X2Broker broker,
                                                     String ctxOid,
                                                     Collection<String> schoolOids,
                                                     PlainDate date) {
            String key = ctxOid + schoolOids + date;

            if (m_spansLookup == null) {
                m_spansLookup = new HashMap<>();
            }

            if (!m_spansLookup.containsKey(key)) {
                m_spansLookup.put(key, getEnrollmentSpans(broker, false, false).stream()
                        .filter(span -> {
                            boolean isSchool = schoolOids.contains(span.getSchool().getOid());
                            boolean isDate = span.getDateRange().contains(date);
                            boolean isContext = span.getContext().getOid().equals(ctxOid);
                            return isSchool && isDate && isContext;
                        })
                        .map(span -> (OnAnnualSpan) span)
                        .collect(Collectors.toList()));
            }

            return m_spansLookup.get(key);
        }

        /**
         * Gets the fte mar.
         *
         * @param section the section
         * @param gradesHelper the grades helper
         * @param broker the broker
         * @return the fte mar
         */
        public String getFteMar(RptSection section, GradesHelper gradesHelper, X2Broker broker) {

            if (m_fteMar == null) {
                ToolSchedule sch = section.getSchedule(ToolBean.getBroker(true));
                ToolDistrictContext ctx = sch.getDistrictContext(ToolBean.getBroker(true));
                String gradeLevel =
                        getEnrollmentSpans().get(0).getGradeType(broker, ToolBean.getDictionaryExtractor(true),
                                gradesHelper);
                if (OnsisConstants.VALUE_GRADES_ELEMENTARY.contains(gradeLevel)) {
                    m_fteMar = "";
                    int year = ctx.getSchoolYear();
                    PlainDate mar31 = ToolBean.getPlainDateValue(year, Calendar.MARCH, 31);
                    ToolEnrollment enrollment = this.getEnrollmentForDate(mar31, "ES", broker);
                    if (enrollment != null) {
                        m_fteMar = getDictionaryExtractor().getStateValue(enrollment,
                                OnBeans.OnEnrollment.FIELD_ENROLMENT_REGISTER);
                    }

                } else {
                    Optional<OnBeans.FteMonthly> fteMar = getFteMonthlyRecords(broker).stream()
                            .filter(monthly -> monthly.getSchoolYear().equals(section.getDistrictContext(broker).getContextId()))
                            .filter(monthly -> monthly.getMonth().contains("MAR"))
                            .findAny();

                    m_fteMar = fteMar.isPresent() ? fteMar.get().getRegister() : getFteOct(section, gradesHelper, broker);
                }
            }

            return m_fteMar;
        }

        /**
         * Gets the fte oct.
         *
         * @param section the section
         * @param gradesHelper the grades helper
         * @param broker the broker
         * @return the fte oct
         */
        public String getFteOct(RptSection section, OnHelpersContainer.GradesHelper gradesHelper, X2Broker broker) {
            if (m_fteOct == null) {
                ToolSchedule sch = section.getSchedule(ToolBean.getBroker(true));
                ToolDistrictContext ctx = sch.getDistrictContext(ToolBean.getBroker(true));
                StringBuilder debugOutput = new StringBuilder();
                debugOutput.append("getEnrollmentSpans().get(0)" + getEnrollmentSpans().get(0) + "\n");
                String gradeLevel =
                        getEnrollmentSpans().get(0).getGradeType(broker, ToolBean.getDictionaryExtractor(true),
                                gradesHelper);
                debugOutput.append("gradeLevel" + gradeLevel + "\n");
                if (OnsisConstants.VALUE_GRADES_ELEMENTARY.contains(gradeLevel)) {
                    m_fteOct = "";
                    int year = ctx.getSchoolYear() - 1;
                    PlainDate oct31 = ToolBean.getPlainDateValue(year, Calendar.OCTOBER, 31);
                    debugOutput.append("oct31" + oct31 + "\n");
                    ToolEnrollment enrollment = this.getEnrollmentForDate(oct31, "ES", broker);
                    debugOutput.append("enrollment" + enrollment + "\n");
                    if (enrollment != null) {
                        m_fteOct = getDictionaryExtractor().getStateValue(enrollment,
                                OnEnrollment.FIELD_ENROLMENT_REGISTER);
                    }

                } else {
                    Optional<OnBeans.FteMonthly> fteOct = getFteMonthlyRecords(broker).stream()
                            .filter(monthly -> monthly.getSchoolYear()
                                    .equals(section.getDistrictContext(broker).getContextId()))
                            .filter(monthly -> monthly.getMonth().contains("OCT"))
                            .findAny();
                    m_fteOct = fteOct.isPresent() ? fteOct.get().getRegister() : "";
                }
            }

            return m_fteOct;
        }

        /**
         * Gets the grade level.
         *
         * @param enr the enr
         * @return the grade level
         */
        String getGradeLevel(ToolEnrollment enr) {
            return getGradeLevel();
        }

        /**
         * Gets the total days funded reg.
         *
         * @return the total days funded reg
         */
        public Integer getTotalDaysFundedReg() {
            if (m_totalDaysFundedReg == null) {
                m_totalDaysFundedReg =
                        isOtherPupil() || m_infoConedRegister.isSummer() ? 0
                                : m_infoConedRegister.getTotalDays();
            }

            return m_totalDaysFundedReg;
        }

        /**
         * Gets the total days funded summer.
         *
         * @return the total days funded summer
         */
        public Integer getTotalDaysFundedSummer() {
            if (m_totalDaysFundedSummer == null) {
                m_totalDaysFundedSummer =
                        !isOtherPupil() && m_infoConedRegister.isSummer() ? m_infoConedRegister.getTotalDays() : 0;
            }

            return m_totalDaysFundedSummer;
        }

        /**
         * Checks if is coned school.
         *
         * @param broker the broker
         * @param dictionaryExtractor the dictionary extractor
         * @return true, if is coned school
         */
        public boolean isConedSchool(X2Broker broker, DictionaryExtractor dictionaryExtractor) {
            boolean value = false;
            List<OnAnnualSpan> spans = getEnrollmentSpans();
            if (!spans.isEmpty()) {
                OnSchool school = null;
                OnAnnualSpan annualSpan = getEnrollmentSpans().iterator().next();

                if (annualSpan.isSecondary()) {
                    OnAnnualSpan primarySpan = annualSpan.getBestPrimarySpanFor(broker, null);
                    if (primarySpan != null) {
                        school = (OnBeans.OnSchool) primarySpan.getSchool();
                    }
                } else {
                    school = (OnBeans.OnSchool) annualSpan.getSchool();
                }
                if (school != null && OnBeans.OnSchool.SPECIAL_CONDITION_CON_ED.contains(school.getSpecialCondition())) {
                    value = true;
                }
                m_boardResidentStatus = annualSpan.getBoardResidentStatus(dictionaryExtractor);
                if (StringUtils.isEmpty(m_boardResidentStatus)) {
                    OnAnnualSpan primarySpan = annualSpan.getBestPrimarySpanFor(broker, null);
                    m_boardResidentStatus = primarySpan.getBoardResidentStatus(getDictionaryExtractor());
                }
            }

            return value;
        }

        /**
         * Checks if is other pupil.
         *
         * @return true, if is other pupil
         */
        public boolean isOtherPupil() {
            String boardResidentStatus =
                    getBoardResidentStatus(ToolBean.getBroker(true), ToolBean.getDictionaryExtractor(true));
            return !StringUtils.isEmpty(boardResidentStatus) && ENR_OTHER_PUPIL_CODES.contains(boardResidentStatus);
        }

        /**
         * Sets the annual spans.
         *
         * @param spans the new annual spans
         */
        public void setAnnualSpans(List<OnBeans.OnAnnualSpan> spans) {
            m_annualSpans = spans;
            m_boardResidentStatus = null;
            m_fteMar = null;
            m_fteOct = null;
            m_infoConedRegister = null;
            m_totalDaysFundedReg = null;
            m_totalDaysFundedSummer = null;
        }

        /**
         * Sets the coned register info.
         *
         * @param info the new coned register info
         */
        public void setConedRegisterInfo(OnBeans.OnSection.ConedRegisterInfo info) {
            m_infoConedRegister = info;
        }
    }


    /**
     * Gets report type based on input parameters
     */
    public void getReportType() {
        String studentInfoBy = (String) getParameter(PARAM_STUDENT_INFO_BY);
        m_byAssign = studentInfoBy.equals(CONST_STD_INFO_BY_ASSIGN);
        m_byDate = studentInfoBy.equals(CONST_STD_INFO_BY_DATE);
    }

    /**
     * Loads the schedule term dates to schedule term oid map for selected year
     *
     * @param ctxOid    The context OID for filtering ScheduleTermDate instance.
     * @param fromDate  The start date for filtering ScheduleTermDate instance.
     * @param asOfDate  The end date for filtering ScheduleTermDate instance.
     */
    public void loadScheduleTermDatesMap(String ctxOid, PlainDate fromDate, PlainDate asOfDate) {
        m_courseTrmDtToTrmOidMap = new HashMap<>();

        Criteria tmdCriteria = new Criteria();
        tmdCriteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + CONST_DELIMITER + ScheduleTerm.REL_SCHEDULE
                + CONST_DELIMITER + Schedule.COL_DISTRICT_CONTEXT_OID, ctxOid);
        tmdCriteria.addGreaterOrEqualThan(ScheduleTermDate.COL_START_DATE, fromDate);
        tmdCriteria.addLessOrEqualThan(ScheduleTermDate.COL_END_DATE, asOfDate);

        QueryByCriteria tmdQuery = new QueryByCriteria(ScheduleTermDate.class, tmdCriteria);

        try (QueryIterator tmdIterator = getBroker().getIteratorByQuery(tmdQuery)) {
            while (tmdIterator.hasNext()) {
                ScheduleTermDate tmd = (ScheduleTermDate) tmdIterator.next();
                String trmOid = tmd.getScheduleTermOid();
                m_courseTrmDtToTrmOidMap.put(trmOid, tmd);
            }
        }
    }

    /**
     * Loads the course delivery type reference table codes to Onsis code maps
     */
    public void loadCourseDeliveryTypeListToOnsisCodeMap() {
        m_courseDeliveryTypeListByOnsisCodeMap = new HashMap<>();
        m_courseDeliveryTypeListForDualCredit = new ArrayList<>();

        String[] requestColumns = {ReferenceCode.COL_CODE, ReferenceCode.COL_STATE_CODE};
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + CONST_DELIMITER + X2BaseBean.COL_OID,
                OntarioAlias.REF_OID_ON_SIS_COURSE_DELIVERY_TYPE);

        ColumnQuery query = new ColumnQuery(ReferenceCode.class, requestColumns, criteria);

        try (ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                String code = (String) row[0];
                String onsisCode = (String) row[1];

                List<String> courseDeliveryTypeListForOnsisCode = m_courseDeliveryTypeListByOnsisCodeMap.get(onsisCode);
                if (courseDeliveryTypeListForOnsisCode == null) {
                    courseDeliveryTypeListForOnsisCode = new ArrayList<>();
                }

                if (!courseDeliveryTypeListForOnsisCode.contains(code)) {
                    courseDeliveryTypeListForOnsisCode.add(code);
                }

                m_courseDeliveryTypeListByOnsisCodeMap.put(onsisCode, courseDeliveryTypeListForOnsisCode);

                if (CONST_CRS_DELIVERY_TYPE_DUAL_CRED_ONSIS_CODE_LIST.contains(onsisCode)) {
                    m_courseDeliveryTypeListForDualCredit.add(code);
                }
            }
        }

        addParameter(REPORT_COURSE_DELIVERY_TYPE_LIST_BY_ONSIS_CODE_MAP, m_courseDeliveryTypeListByOnsisCodeMap);
    }

    /**
     * Loads the master schedule selection objects based on input selection
     */
    public void loadMstSelection() {
        /*
         * General set up of input parameters, master schedule selection objects,
         * initialization
         */
        // Get master schedule oids based on input selection
        Collection<String> mstOidsInput = new ArrayList<>();
        if (getParameter(PARAM_MST_OIDS) != null) {
            mstOidsInput.addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_MST_OIDS), ","));
        }
        if (getParameter(PARAM_MST_OIDS_STAFF_VIEW) != null) {
            mstOidsInput.addAll(
                    StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_MST_OIDS_STAFF_VIEW), ","));
        }

        // initialize selection
        m_selectionMst = X2BaseBean.newInstance(Selection.class, getBroker().getPersistenceKey());
        m_selectionMst.setTimestamp(System.currentTimeMillis());

        // initialize other variables
        m_mstToMstOid = new HashMap<>();
        m_classStartDateToMstOid = new HashMap<>();
        m_classEndDateToMstOid = new HashMap<>();
        m_classSummerCourseIndToMstOid = new HashMap<>();

        /*
         * Add to selection if sections have been selected
         */
        // loop through, save selection objects if sections have been selected
        try {
            for (String mstOid : mstOidsInput) {
                // create SelectionObject for each mst oid
                SelectionObject selectionObj = X2BaseBean.newInstance(
                        SelectionObject.class, getBroker().getPersistenceKey());
                selectionObj.setObjectOid(mstOid);
                m_selectionMst.addToSelectionObjects(selectionObj);

                // get master schedule for mstOid
                MasterSchedule mst = getBroker().getBeanByOid(MasterSchedule.class, mstOid);
                m_mstToMstOid.put(mstOid, mst);

                // save master schedule start and end dates
                saveMstStartEndDates(mst, mstOid);

                // save master schedule summer class indicator
                saveMstSummerClassInd(mst, mstOid, m_classStartDateToMstOid.get(mstOid));
            }
        } catch (Exception e) {
            // Catch the exception into the report output message.
            logToolMessage(Level.INFO, e.getMessage(), false);
            throw e;
        }

        /*
         * Add to selection if all sections that meet other criteria
         */
        // select all sections based on school/staff if particular sections not selected
        if (mstOidsInput.isEmpty()) {
            Collection<String> cskConEdProgramTypesInput = new ArrayList<>();
            if (getParameter(PARAM_CSK_CON_ED_PROGRAM_TYPES) != null) {
                cskConEdProgramTypesInput.addAll(StringUtils
                        .convertDelimitedStringToList((String) getParameter(PARAM_CSK_CON_ED_PROGRAM_TYPES), ";"));
            }

            // create master schedule criteria and add sections to selection object
            X2Criteria mstCriteria = new X2Criteria();

            // add school condition
            mstCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + CONST_DELIMITER + SchoolCourse.COL_SCHOOL_OID,
                    getSchool().getOid());

            // add school year condition
            mstCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + CONST_DELIMITER + SchoolCourse.REL_COURSE
                    + CONST_DELIMITER + Course.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

            // add staff condition (would be from staff view)
            if (m_staffCurrent != null) {
                mstCriteria.addEqualTo(
                        MasterSchedule.REL_TEACHER_SECTIONS + CONST_DELIMITER + TeacherSection.COL_STAFF_OID,
                        m_staffCurrent.getOid());
            }

            // add course con ed program condition
            String fieldCourseConEdProgramType = getBeanPathFromAlias(OntarioAlias.ALIAS_CSK_CON_ED_PROGRAM_TYPE,
                    false);
            mstCriteria.addIn(MasterSchedule.REL_SCHOOL_COURSE + CONST_DELIMITER + fieldCourseConEdProgramType,
                    cskConEdProgramTypesInput);

            // create query for master schedule
            QueryByCriteria mstQuery = new QueryByCriteria(MasterSchedule.class, mstCriteria);

            // load master schedules
            try (QueryIterator mstIterator = getBroker().getIteratorByQuery(mstQuery)) {
                // iterates through and saves
                while (mstIterator.hasNext()) {
                    MasterSchedule mst = (MasterSchedule) mstIterator.next();
                    String mstOid = mst.getOid();

                    // create SelectionObject for each mst oid
                    SelectionObject selectionObj = X2BaseBean.newInstance(SelectionObject.class,
                            getBroker().getPersistenceKey());
                    selectionObj.setObjectOid(mstOid);
                    m_selectionMst.addToSelectionObjects(selectionObj);

                    // save master schedule for mstOid
                    m_mstToMstOid.put(mstOid, mst);

                    // save master schedule start and end dates
                    saveMstStartEndDates(mst, mstOid);

                    // save master schedule summer class indicator
                    saveMstSummerClassInd(mst, mstOid, m_classStartDateToMstOid.get(mstOid));
                }
            }
        }

        // Save the selection as criteria for use on other queries or show errors
        Collection<ValidationError> errors = getBroker().saveBean(m_selectionMst);
        if (errors.size() > 0) {
            StringBuilder errorCause = new StringBuilder();
            for (ValidationError err : errors) {
                errorCause.append(WebUtils.getMessage(err, getBroker().getPersistenceKey()));
            }
            throw new RuntimeException(errorCause.toString());
        }

        // Save course start/end dates
        addParameter(REPORT_COURSE_START_DATE_TO_MST_MAP, m_classStartDateToMstOid);
        addParameter(REPORT_COURSE_END_DATE_TO_MST_MAP, m_classEndDateToMstOid);
        addParameter(REPORT_COURSE_SUMMER_COURSE_iND_TO_MST_MAP, m_classSummerCourseIndToMstOid);
    }

    /**
     * Save the master schedule start end dates in global maps
     *
     * @param mst - MasterSchedule
     * @param mstOid
     */
    public void saveMstStartEndDates(MasterSchedule mst, String mstOid) {
        // get school year start/end date in case not found (data issue)
        PlainDate classStartDate = getCurrentContext().getStartDate();
        PlainDate classEndDate = getCurrentContext().getEndDate();

        // get master schedule start/end dates based on section (if overriding values
        // saved)
        String classStartDateMstStr = (String) mst.getFieldValueByAlias(OntarioAlias.ALIAS_MST_CON_ED_START_DATE);
        String classEndDateMstStr = (String) mst.getFieldValueByAlias(OntarioAlias.ALIAS_MST_CON_ED_END_DATE);

        // get master schedule start and end dates from schedule term if not on section
        String trmOid = mst.getScheduleTermOid();
        ScheduleTermDate tmd = m_courseTrmDtToTrmOidMap.get(trmOid);
        if (!StringUtils.isEmpty(classStartDateMstStr)) {
            classStartDate = getDate(mst, OntarioAlias.ALIAS_MST_CON_ED_START_DATE);
        } else if (tmd != null) {
            classStartDate = tmd.getStartDate();
        }
        if (!StringUtils.isEmpty(classEndDateMstStr)) {
            classEndDate = getDate(mst, OntarioAlias.ALIAS_MST_CON_ED_END_DATE);
        } else if (tmd != null) {
            classEndDate = tmd.getEndDate();
        }

        m_classStartDateToMstOid.put(mstOid, classStartDate);
        m_classEndDateToMstOid.put(mstOid, classEndDate);
    }

    /**
     * Save the master schedule summer class indicator in global map
     *
     * @param mst - MasterSchedule
     * @param mstOid - oid identifier for the MasterSchedule.
     * @param classStartDate - The start date of the class as a PlainDate.
     */
    public void saveMstSummerClassInd(MasterSchedule mst, String mstOid, PlainDate classStartDate) {
        // get master schedule con ed offering type
        String mstConEdOfferingType = (String) mst.getFieldValueByAlias(OntarioAlias.ALIAS_MST_CON_ED_OFFERING_TYPE);
        String mstTimeOfDayCode = (String) mst.getFieldValueByAlias(OntarioAlias.ALIAS_MST_TIME_OF_DAY_CODE);

        boolean isCourseSummer = isSummerCourseType(mstConEdOfferingType)
                || isSummerTimeOfDay(mstTimeOfDayCode)
                || classStartDate.after(m_calSummerEnrStartDt);

        m_classSummerCourseIndToMstOid.put(mstOid, isCourseSummer);
    }

    /**
     * Checks if the provided continuing education offering type
     * corresponds to a summer course OnSIS code.
     *
     * @param conEdOfferingType The continuing education offering type to be checked.
     * @return true if the offering type is associated with a summer course, false otherwise.
     */
    private boolean isSummerCourseType(String conEdOfferingType) {
        DictionaryExtractor extractor = ToolBean.getDictionaryExtractor();
        DataDictionaryField field = new ToolBean.ToolBeanColumn(
                SisBeanPaths.SCHEDULE_MASTER, OntarioAlias.ALIAS_MST_CON_ED_OFFERING_TYPE).getField(extractor);

        return extractor
                .getReferenceCodes(field.getReferenceTableOid())
                .entrySet()
                .stream()
                .filter(type -> !StringUtils.isEmpty(conEdOfferingType))
                .filter(type -> type.getKey().equals(conEdOfferingType))
                .map(type -> type.getValue().getStateCode())
                .anyMatch(code -> {
                    return OntarioAlias.CONST_MST_CON_ED_OFFERING_SUMMER
                            .stream()
                            .anyMatch(os -> os.contains(code));
                });
    }

    /**
     * Checks if the provided time of day corresponds to
     * a summer time of day OnSIS code.
     *
     * @param timeOfDay The time of day to be checked.
     * @return true if the time of day is associated with summer, false otherwise.
     */
    private boolean isSummerTimeOfDay(String timeOfDay) {
        DictionaryExtractor extractor = ToolBean.getDictionaryExtractor();
        DataDictionaryField field = new ToolBean.ToolBeanColumn(
                SisBeanPaths.SCHEDULE_MASTER, OntarioAlias.ALIAS_MST_TIME_OF_DAY_CODE).getField(extractor);

        return extractor
                .getReferenceCodes(field.getReferenceTableOid())
                .entrySet()
                .stream()
                .filter(td -> !StringUtils.isEmpty(timeOfDay))
                .filter(td -> td.getKey().equals(timeOfDay))
                .map(td -> td.getValue().getStateCode())
                .anyMatch(code -> {
                    return OntarioAlias.CONST_MST_TIME_OF_DAY_SUMMER
                            .stream()
                            .anyMatch(ds -> ds.contains(code));
                });
    }

    /**
     * Loads the student oids who are taking the master schedule selection courses
     */
    public void loadStdOidsToMstOid() {
        // create student schedule criteria
        X2Criteria sccCriteria = new X2Criteria();

        // based on selection of master schedule oids
        SubQuery subQuery = getSubQueryFromMstSelection(StudentScheduleChange.COL_MASTER_SCHEDULE_OID);
        sccCriteria.addExists(subQuery);

        // get sort parameter
        String courseSort = (String) getParameter(PARAM_COURSE_SORT);

        // create query for load students to master schedule
        // not doing column query in case date is returned as time stamp
        QueryByCriteria sccQuery = new QueryByCriteria(StudentScheduleChange.class, sccCriteria);
        if (courseSort.equals(CONST_SORT_CRS_NUM_SECTION)) {
            sccQuery.addOrderByAscending(
                    StudentScheduleChange.REL_MASTER_SCHEDULE + CONST_DELIMITER + MasterSchedule.COL_COURSE_VIEW);
        } else if (courseSort.equals(CONST_SORT_CRS_DESC_SECTION)) {
            sccQuery.addOrderByAscending(
                    StudentScheduleChange.REL_MASTER_SCHEDULE + CONST_DELIMITER + MasterSchedule.COL_DESCRIPTION);
            sccQuery.addOrderByAscending(
                    StudentScheduleChange.REL_MASTER_SCHEDULE + CONST_DELIMITER + MasterSchedule.COL_SECTION_NUMBER);
        }
        sccQuery.addOrderByAscending(StudentScheduleChange.REL_STUDENT + CONST_DELIMITER + SisStudent.COL_NAME_VIEW);
        sccQuery.addOrderByAscending(StudentScheduleChange.COL_TIMESTAMP);

        // load map of student oid to mst oid, map should be ordered by
        // entry (LinkedHashMap) and list of student oids
        m_stdOidListToMstOid = new LinkedHashMap<>();
        m_stdCalIdToMstOid = new HashMap<>();
        m_stdOids = new ArrayList<>();
        m_stdStartEndDatesToMstOidMap = new HashMap<>();
        String mstOidPrev = CONST_EMPTY;
        String stdOidPrev = CONST_EMPTY;
        List<String> stdOidListForMstOid = new ArrayList<>();
        EnrRegStudentStartEndDatesForSection stdEnrStartEndDatesForMst = null;
        try (QueryIterator sccIterator = getBroker().getIteratorByQuery(sccQuery)) {
            while (sccIterator.hasNext()) {
                // get new values
                StudentScheduleChange scc = (StudentScheduleChange) sccIterator.next();
                String mstOid = scc.getMasterScheduleOid();
                String stdOid = scc.getStudentOid();
                PlainDate sccEffDate = scc.getEffectiveDate();
                String sccChgType = scc.getChangeTypeCode();
                String stdCalId = scc.getStudent().getCalendarCode();

                if (!hasScoresForMasterScheduleSelection(stdOid, mstOid)) {
                    continue;
                }

                // if new mstOid, write stdOids for previous mstOid including last stdOid
                if ((!StringUtils.isEmpty(mstOidPrev) && (!mstOidPrev.equals(mstOid)))) {
                    // add previous stdOid to list for previous mstOid
                    if (!StringUtils.isEmpty(stdOidPrev)) {
                        // if student was enrolled after class started add to lists for mst
                        if ((stdEnrStartEndDatesForMst != null)
                                && (stdEnrStartEndDatesForMst.isStdEnrolledWithinClassDates(stdOidPrev))) {
                            stdOidListForMstOid.add(stdOidPrev);

                            // add previous stdOid to overall list
                            if (!m_stdOids.contains(stdOidPrev)) {
                                m_stdOids.add(stdOidPrev);
                            }
                        }
                    }

                    // save student enrolment dates for previous mstOid
                    m_stdStartEndDatesToMstOidMap.put(mstOidPrev, stdEnrStartEndDatesForMst);

                    // add list of stdOids for previous mstOid
                    m_stdOidListToMstOid.put(mstOidPrev, stdOidListForMstOid);

                    // reinitialize variables (mst and std)
                    stdOidListForMstOid = new ArrayList<String>();
                }
                // if new stdOid (but not new mstOid), add previous stdOid to list
                else if ((!StringUtils.isEmpty(stdOidPrev)) && (!stdOidPrev.equals(stdOid))) {
                    // if student was enrolled after class started add to lists for mst
                    if ((stdEnrStartEndDatesForMst != null)
                            && (stdEnrStartEndDatesForMst.isStdEnrolledWithinClassDates(stdOidPrev))) {
                        stdOidListForMstOid.add(stdOidPrev);
                    }

                    // add previous stdOid to overall list
                    if (!m_stdOids.contains(stdOidPrev)) {
                        m_stdOids.add(stdOidPrev);
                    }
                }

                // if new mst oid fetch variables saved for new mst oid
                if (!mstOidPrev.equals(mstOid)) {
                    PlainDate classStartDate = m_classStartDateToMstOid.get(mstOid);
                    PlainDate classEndDate = m_classEndDateToMstOid.get(mstOid);
                    stdEnrStartEndDatesForMst =
                            new EnrRegStudentStartEndDatesForSection(mstOid, classStartDate, classEndDate);
                }

                // set mst/std previous to current values
                mstOidPrev = mstOid;
                stdOidPrev = stdOid;

                // check if student was enrolled when class started, changes come in order so
                // check date/type
                if (!sccChgType.equals(StudentScheduleChange.CODE_DROP)) {
                    // add student start date
                    stdEnrStartEndDatesForMst.addStdMstStartDate(stdOid, sccEffDate);

                    // save calendar id if not saved for section
                    m_stdCalIdToMstOid.putIfAbsent(mstOid, stdCalId);
                } else {
                    // add student drop date
                    m_calendar.setTime(sccEffDate);
                    PlainDate sccEffDateLastClassDate = addDays(m_calendar, -1);
                    stdEnrStartEndDatesForMst.addStdMstEndDate(stdOid, sccEffDateLastClassDate);
                }
            }
        } catch (Exception e) {
            // Catch the exception into the report output message.
            logToolMessage(Level.INFO, e.getMessage(), false);
            throw e;
        }

        // write stdOids/drop dates for last section including last std oid
        if (!StringUtils.isEmpty(mstOidPrev)) {
            // add last student
            if (!StringUtils.isEmpty(stdOidPrev)) {
                // if student was enrolled after class started add to lists for mst
                if (stdEnrStartEndDatesForMst.isStdEnrolledWithinClassDates(stdOidPrev)) {
                    stdOidListForMstOid.add(stdOidPrev);

                    // add previous stdOid to overall list
                    if (!m_stdOids.contains(stdOidPrev)) {
                        m_stdOids.add(stdOidPrev);
                    }
                }

                // save student enrolment dates for previous mstOid
                m_stdStartEndDatesToMstOidMap.put(mstOidPrev, stdEnrStartEndDatesForMst);

                // add list of stdOids for previous mstOid
                m_stdOidListToMstOid.put(mstOidPrev, stdOidListForMstOid);
            }
        }
    }

    /**
     * Loads assignment total count to master schedule oid
     */
    public void loadAssignTotalCtToMstOid() {
        // create student schedule criteria
        X2Criteria gbcCriteria = new X2Criteria();

        // based on selection of master schedule oids
        SubQuery subQuery = getSubQueryFromMstSelection(GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID);
        gbcCriteria.addExists(subQuery);

        // add condition based on system indicator
        gbcCriteria.addEqualTo(GradebookColumnDefinition.COL_SYSTEM_ONLY_INDICATOR, Boolean.FALSE);

        // add condition based on indicator to include in these registers
        String fieldAssignConEdIncl = getBeanPathFromAlias(OntarioAlias.ALIAS_GCD_CON_ED_REGISTER_INCL_ON, false);
        if (fieldAssignConEdIncl != null) {
            gbcCriteria.addEqualTo(fieldAssignConEdIncl, CONST_TRUE);
        }

        // load assignments to master schedule/student
        String[] columns = new String[] {GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID, X2BaseBean.COL_OID};
        ColumnQuery gbcQuery = new ColumnQuery(GradebookColumnDefinition.class, columns, gbcCriteria);
        gbcQuery.addOrderByAscending(GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID);

        // load map of assignment counts to master schedule oid/student oid
        m_assignTotToMstOidMap = new HashMap<>();
        String mstOidPrev = CONST_EMPTY;
        int assignTotCt = CONST_ZERO;
        // loop through assignments
        try (ReportQueryIterator gbcIterator = getBroker().getReportQueryIteratorByQuery(gbcQuery)) {
            while (gbcIterator.hasNext()) {
                // get new values
                Object[] data = (Object[]) gbcIterator.next();
                String mstOid = data[0].toString();

                // if new section, write assignment total count for prev
                if ((!StringUtils.isEmpty(mstOidPrev) && (!mstOidPrev.equals(mstOid)))) {
                    m_assignTotToMstOidMap.put(mstOidPrev, assignTotCt);
                    assignTotCt = CONST_ZERO;
                }
                mstOidPrev = mstOid;

                // accumulate assignments
                assignTotCt++;
            }
        } catch (Exception e) {
            // Catch the exception into the report output message.
            logToolMessage(Level.INFO, e.getMessage(), false);
            throw e;
        }

        // write assignment total count for last section
        if (!StringUtils.isEmpty(mstOidPrev)) {
            m_assignTotToMstOidMap.put(mstOidPrev, assignTotCt);
        }

        addParameter(REPORT_ASSIGN_TOT_TO_MST_MAP, m_assignTotToMstOidMap);
    }

    /**
     * Loads assignment count to master schedule oid/student oid
     */
    public void loadAssignCtToMstOidStdOid() {
        // create student schedule criteria
        X2Criteria gbsCriteria = new X2Criteria();

        // based on selection of master schedule oids
        SubQuery subQuery = getSubQueryFromMstSelection(GradebookScore.REL_COLUMN_DEFINITION + CONST_DELIMITER
                + GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID);
        gbsCriteria.addExists(subQuery);

        // add condition based on system indicator
        gbsCriteria.addEqualTo(GradebookScore.REL_COLUMN_DEFINITION + CONST_DELIMITER
                + GradebookColumnDefinition.COL_SYSTEM_ONLY_INDICATOR, Boolean.FALSE);

        // add condition to only include scores with a completion date (to bypass excluded assignments)
        gbsCriteria.addNotNull(GradebookScore.COL_COMPLETED_DATE);

        // add condition based on indicator to include in these registers
        String fieldAssignConEdIncl = getBeanPathFromAlias(OntarioAlias.ALIAS_GCD_CON_ED_REGISTER_INCL_ON, false);
        if (fieldAssignConEdIncl != null) {
            gbsCriteria.addEqualTo(GradebookScore.REL_COLUMN_DEFINITION + CONST_DELIMITER + fieldAssignConEdIncl,
                    CONST_TRUE);
        }

        // load assignments to master schedule/student
        String[] columns = new String[] {
                GradebookScore.REL_COLUMN_DEFINITION + CONST_DELIMITER
                        + GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID,
                GradebookScore.COL_STUDENT_OID, GradebookScore.COL_SCORE, GradebookScore.COL_COMPLETED_DATE};
        ColumnQuery gbsQuery = new ColumnQuery(GradebookScore.class, columns, gbsCriteria);
        gbsQuery.addOrderByAscending(GradebookScore.REL_COLUMN_DEFINITION + CONST_DELIMITER
                + GradebookColumnDefinition.REL_MASTER_SCHEDULE + CONST_DELIMITER + MasterSchedule.COL_COURSE_VIEW);
        gbsQuery.addOrderByAscending(GradebookScore.REL_COLUMN_DEFINITION + CONST_DELIMITER
                + GradebookColumnDefinition.REL_MASTER_SCHEDULE + CONST_DELIMITER + MasterSchedule.COL_DESCRIPTION);
        gbsQuery.addOrderByAscending(GradebookScore.REL_STUDENT + CONST_DELIMITER + SisStudent.COL_NAME_VIEW);

        // load map of assignment counts to master schedule oid/student oid
        m_assignCtRegToRegTypeToMstOidStdOid = new HashMap<>();
        m_assignCtSummerToMstOidStdOid = new HashMap<>();
        String mstOidStdOidPrev = CONST_EMPTY;
        int assignCtRegFt = CONST_ZERO;
        int assignCtRegPt = CONST_ZERO;
        int assignCtSummer = CONST_ZERO;
        String regTypeOct = CONST_EMPTY;
        String regTypeMar = CONST_EMPTY;
        // loop through scores
        try (ReportQueryIterator gbsIterator = getBroker().getReportQueryIteratorByQuery(gbsQuery)) {
            while (gbsIterator.hasNext()) {
                // get new values
                Object[] data = (Object[]) gbsIterator.next();
                String mstOid = data[0].toString();
                String stdOid = data[1].toString();
                String score = data[2].toString();
                Timestamp scoreDateTs = (Timestamp) data[3];
                PlainDate scoreDate = null;
                if (scoreDateTs != null) {
                    scoreDate = PlainDate.fromString(scoreDateTs.toString().substring(0, 10));
                }
                String mstOidStdOid = mstOid + CONST_HYPHEN + stdOid;

                // process only if student is in list of std oids with enrolment info saved
                // one student had assignments but was not on roster so is necessary
                if (!m_stdOids.contains(stdOid)) {
                    continue;
                }

                // if new section/student, write assignment count for prev
                if ((!StringUtils.isEmpty(mstOidStdOidPrev) && (!mstOidStdOidPrev.equals(mstOidStdOid)))) {
                    Map<String, Integer> assignCtRegByRegType = new HashMap<>();
                    assignCtRegByRegType.put(OntarioAlias.ENR_REG_TYPE_FT, assignCtRegFt);
                    assignCtRegByRegType.put(OntarioAlias.ENR_REG_TYPE_PT, assignCtRegPt);
                    m_assignCtRegToRegTypeToMstOidStdOid.put(mstOidStdOidPrev, assignCtRegByRegType);
                    m_assignCtSummerToMstOidStdOid.put(mstOidStdOidPrev, assignCtSummer);
                    assignCtRegFt = CONST_ZERO;
                    assignCtRegPt = CONST_ZERO;
                    assignCtSummer = CONST_ZERO;
                    regTypeOct = CONST_EMPTY;
                    regTypeMar = CONST_EMPTY;
                }
                mstOidStdOidPrev = mstOidStdOid;

                // accumulate assignment if score is populated
                if (!StringUtils.isEmpty(score)) {
                    if (StringUtils.isEmpty(regTypeOct)) {
                        regTypeOct = m_enrInfoToStdOidTypeMap.get(
                                stdOid + OntarioAlias.CONST_ENR_REG_TYP_SUFFIX + OntarioAlias.CONST_STD_FTE_REG_OCT);
                        regTypeMar = m_enrInfoToStdOidTypeMap.get(
                                stdOid + OntarioAlias.CONST_ENR_REG_TYP_SUFFIX + OntarioAlias.CONST_STD_FTE_REG_MAR);
                    }

                    // if score date != null then check if summer otherwise check if PT
                    // assignments are FT if not summer and not PT
                    if (scoreDate != null) {
                        // check if summer assignment
                        if (scoreDate.after(m_calSummerAssignAfterDt)) {
                            assignCtSummer++;
                        }
                        // assignment counts if PT
                        // first half yr assignment and PT in Oct
                        else if (((regTypeOct != null) && (regTypeOct.equals(OntarioAlias.ENR_REG_TYPE_PT))
                                && (!scoreDate.after(m_calHalf1EndDt)))
                                // second half yr assignment and PT in Mar
                                || ((regTypeMar != null) && (regTypeMar.equals(OntarioAlias.ENR_REG_TYPE_PT))
                                && (scoreDate.after(m_calHalf1EndDt)))) {
                            assignCtRegPt++;
                        } else {
                            assignCtRegFt++;
                        }
                    } else {
                        assignCtRegFt++;
                    }
                }
            }
        } catch (Exception e) {
            // Catch the exception into the report output message.
            logToolMessage(Level.INFO, e.getMessage(), false);
            throw e;
        }

        // write assignment count for last section/student
        if (!StringUtils.isEmpty(mstOidStdOidPrev)) {
            Map<String, Integer> assignCtRegByRegType = new HashMap<>();
            assignCtRegByRegType.put(OntarioAlias.ENR_REG_TYPE_FT, assignCtRegFt);
            assignCtRegByRegType.put(OntarioAlias.ENR_REG_TYPE_PT, assignCtRegPt);
            m_assignCtRegToRegTypeToMstOidStdOid.put(mstOidStdOidPrev, assignCtRegByRegType);
            m_assignCtSummerToMstOidStdOid.put(mstOidStdOidPrev, assignCtSummer);
        }
    }

    /**
     * Loads bell periods to bel oid, period name map
     */
    public void loadBpeToBelOidPeriodNm() {
        m_bpeTimeToBelOidPeriodNmMap = new HashMap<>();

        // create criteria/query to get bell schedule information - fetches for all
        // schools though
        // may not be needed
        X2Criteria bpeCriteria = new X2Criteria();

        QueryByCriteria bpeQuery = new QueryByCriteria(ScheduleBellPeriod.class, bpeCriteria);
        bpeQuery.addOrderByAscending(ScheduleBellPeriod.COL_BELL_SCHEDULE_OID);
        bpeQuery.addOrderByAscending(ScheduleBellPeriod.REL_SCHEDULE_PERIOD + CONST_DELIMITER + SchedulePeriod.COL_NAME);

        // load map of schedule bell period to schedule bell oid/period name
        try (QueryIterator bpeIterator = getBroker().getIteratorByQuery(bpeQuery)) {
            // iterates through and saves into map
            while (bpeIterator.hasNext()) {
                ScheduleBellPeriod bpe = (ScheduleBellPeriod) bpeIterator.next();

                // get new values
                String belOid = bpe.getBellScheduleOid();
                String bpePeriodNm = bpe.getSchedulePeriod().getName();

                // load into map
                String keyField = belOid + CONST_HYPHEN + bpePeriodNm;
                m_bpeTimeToBelOidPeriodNmMap.put(keyField, bpe);
            }
        }
    }

    /**
     * Loads class dates/days/time lists in order of class number to master schedule
     * map
     *
     * @param ctxOid
     * @param courseIncludeDays
     * @param courseIncludeTime
     */
    public void loadClassDateDayTimeListsToMstOid(String ctxOid,
                                                  boolean courseIncludeDays,
                                                  boolean courseIncludeTime) {
        m_classDtListToMstOidMap = new HashMap<>();
        m_classDayListToMstOidMap = new HashMap<>();
        m_classStartEndTimeToMstOidMap = new HashMap<>();

        /*
         * get the master schedules to ScheduleDays that selected sections meet also
         * lists of school oids, day numbers to use in next query
         */
        Map<String, List<String>> mstOidsToSklOidDayNumMap = new HashMap<>();
        Map<String, List<String>> periodNmsToMstOidMap = new HashMap<>();
        List<String> mstSklOidsToRestrictSelection = new ArrayList<>();
        List<Integer> mstDayNumsToRestrictSelection = new ArrayList<>();

        // create criteria/query to get schedule days for mstOids
        X2Criteria mtxCriteria = new X2Criteria();

        // based on selection of master schedule oids
        SubQuery subQuery = getSubQueryFromMstSelection(
                MasterScheduleMatrix.REL_MASTER_TERM + CONST_DELIMITER + MasterTerm.COL_MASTER_SCHEDULE_OID);
        mtxCriteria.addExists(subQuery);

        String[] mtxColumns = new String[] {
                // school oid
                MasterScheduleMatrix.REL_MASTER_TERM + CONST_DELIMITER + MasterTerm.REL_MASTER_SCHEDULE + CONST_DELIMITER
                        + MasterSchedule.REL_SCHOOL_COURSE + CONST_DELIMITER + SchoolCourse.COL_SCHOOL_OID,
                // schedule day number
                MasterScheduleMatrix.REL_SCHEDULE_MATRIX + CONST_DELIMITER + ScheduleMatrix.REL_SCHEDULE_DAY
                        + CONST_DELIMITER + ScheduleDay.COL_NUMBER,
                // schedule period name
                MasterScheduleMatrix.REL_SCHEDULE_MATRIX + CONST_DELIMITER + ScheduleMatrix.REL_SCHEDULE_PERIOD
                        + CONST_DELIMITER + SchedulePeriod.COL_NAME,
                // master schedule oid
                MasterScheduleMatrix.REL_MASTER_TERM + CONST_DELIMITER + MasterTerm.COL_MASTER_SCHEDULE_OID};
        ColumnQuery mtxQuery = new ColumnQuery(MasterScheduleMatrix.class, mtxColumns, mtxCriteria);
        mtxQuery.addOrderByAscending(
                MasterScheduleMatrix.REL_MASTER_TERM + CONST_DELIMITER + MasterTerm.REL_MASTER_SCHEDULE + CONST_DELIMITER
                        + MasterSchedule.REL_SCHOOL_COURSE + CONST_DELIMITER + SchoolCourse.COL_SCHOOL_OID);
        mtxQuery.addOrderByAscending(MasterScheduleMatrix.REL_SCHEDULE_MATRIX + CONST_DELIMITER
                + ScheduleMatrix.REL_SCHEDULE_DAY + CONST_DELIMITER + ScheduleDay.COL_NUMBER);
        mtxQuery.addOrderByAscending(MasterScheduleMatrix.REL_SCHEDULE_MATRIX + CONST_DELIMITER
                + ScheduleMatrix.REL_SCHEDULE_PERIOD + CONST_DELIMITER + SchedulePeriod.COL_NAME);
        mtxQuery.addOrderByAscending(
                MasterScheduleMatrix.REL_MASTER_TERM + CONST_DELIMITER + MasterTerm.COL_MASTER_SCHEDULE_OID);

        // load map of schedule days to mstOids
        String mtxQuerySklOidDayNumPrev = null;
        List<String> mstOidsForSklOidDayNum = new ArrayList<>();
        try (ReportQueryIterator mtxIterator = getBroker().getReportQueryIteratorByQuery(mtxQuery)) {
            // iterates through using sort key which is school/day/period/mstOid
            while (mtxIterator.hasNext()) {
                // get new values
                Object[] data = (Object[]) mtxIterator.next();
                String sklOid = data[0].toString();
                Integer dayNum = ((BigDecimal) data[1]).intValue();
                String periodNm = ((String) data[2]);
                String mstOid = data[3].toString();
                String sklOidDayNum = sklOid + CONST_SPLIT_STR + dayNum;

                // if new key school/day, write section list for prev key
                if ((!(mtxQuerySklOidDayNumPrev == null)) && (!sklOidDayNum.equals(mtxQuerySklOidDayNumPrev))) {
                    mstOidsToSklOidDayNumMap.put(mtxQuerySklOidDayNumPrev, mstOidsForSklOidDayNum);

                    // initialize list
                    mstOidsForSklOidDayNum = new ArrayList<>();
                }
                mtxQuerySklOidDayNumPrev = sklOidDayNum;

                // save sklOids, day numbers in list
                if (!mstSklOidsToRestrictSelection.contains(sklOid)) {
                    mstSklOidsToRestrictSelection.add(sklOid);
                }
                if (!mstDayNumsToRestrictSelection.contains(dayNum)) {
                    mstDayNumsToRestrictSelection.add(dayNum);
                }

                // add section to list for school/day
                // may be duplicates including if multiple periods so saved only once
                if (!mstOidsForSklOidDayNum.contains(mstOid)) {
                    mstOidsForSklOidDayNum.add(mstOid);
                }

                // add period names by mstOid
                List<String> periodNmsForMst = periodNmsToMstOidMap.get(mstOid);
                if (periodNmsForMst == null) {
                    periodNmsForMst = new ArrayList<>();
                }
                if (!periodNmsForMst.contains(periodNm)) {
                    periodNmsForMst.add(periodNm);
                }
                periodNmsToMstOidMap.put(mstOid, periodNmsForMst);
            }
        } catch (Exception e) {
            // Catch the exception into the report output message.
            logToolMessage(Level.INFO, e.getMessage(), false);
            throw e;
        }

        // write section list for last school/day
        if (!(mtxQuerySklOidDayNumPrev == null)) {
            mstOidsToSklOidDayNumMap.put(mtxQuerySklOidDayNumPrev, mstOidsForSklOidDayNum);
        }

        /*
         * get SchoolCalendarDates(full year)/days-of-week lists to selected sections
         * also get bell schedule by school/day number (assuming 1 to 1 relation)
         */
        Map<String, List<PlainDate>> classDtFullYrToMstOidMap = new HashMap<>();
        Map<String, String> belOidToMstOidMap = new HashMap<>();

        // create criteria/query to get school calendar dates for mstOids
        X2Criteria csdCriteria = new X2Criteria();

        // condition for school year/schools/day numbers for selected sections
        csdCriteria.addEqualTo(
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + CONST_DELIMITER + SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                ctxOid);
        csdCriteria.addIn(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + CONST_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                mstSklOidsToRestrictSelection);
        csdCriteria.addIn(SisSchoolCalendarDate.COL_SCHEDULE_DAY_NUMBER, mstDayNumsToRestrictSelection);

        String[] csdColumns = new String[] {
                // school oid
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + CONST_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                // calendar id
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + CONST_DELIMITER + SchoolCalendar.COL_CALENDAR_ID,
                // schedule day number
                SisSchoolCalendarDate.COL_SCHEDULE_DAY_NUMBER,
                // date
                SisSchoolCalendarDate.COL_DATE,
                // schedule bell oid
                SisSchoolCalendarDate.COL_BELL_SCHEDULE_OID};
        ColumnQuery csdQuery = new ColumnQuery(SisSchoolCalendarDate.class, csdColumns, csdCriteria);
        csdQuery.addOrderByAscending(
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + CONST_DELIMITER + SchoolCalendar.COL_SCHOOL_OID);
        csdQuery.addOrderByAscending(
                SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + CONST_DELIMITER + SchoolCalendar.COL_CALENDAR_ID);
        csdQuery.addOrderByAscending(SisSchoolCalendarDate.COL_SCHEDULE_DAY_NUMBER);
        csdQuery.addOrderByAscending(SisSchoolCalendarDate.COL_DATE);

        // load map of schedule days to mstOids
        String csdQuerySklOidCalIdDayNumPrev = null;
        Collection<PlainDate> csdDatesForSklOidCalIdDayNum = new ArrayList<>();
        Collection<String> csdDaysForSklOidCalIdDayNum = new ArrayList<>();
        String belOidForSklOidCalIdDayNum = CONST_EMPTY;
        try (ReportQueryIterator csdIterator = getBroker().getReportQueryIteratorByQuery(csdQuery)) {
            while (csdIterator.hasNext()) {
                // get new values
                Object[] data = (Object[]) csdIterator.next();
                String sklOid = data[0].toString();
                String calId = CONST_EMPTY;
                if (data[1] != null) {
                    calId = data[1].toString();
                }
                int dayNum = ((BigDecimal) data[2]).intValue();
                PlainDate csdDate = new PlainDate((Timestamp) data[3]);
                String sklOidCalIdDayNum = sklOid + CONST_SPLIT_STR + calId + CONST_SPLIT_STR + dayNum;
                String belOid = CONST_EMPTY;
                if (data[4] != null) {
                    belOid = data[3].toString();
                }

                // if new key school/cal id/day, write class date list for prev key
                if ((!(csdQuerySklOidCalIdDayNumPrev == null))
                        && (!sklOidCalIdDayNum.equals(csdQuerySklOidCalIdDayNumPrev))) {
                    // add school dates/days to lists to mstOid
                    // get calendar id and key without calendar id
                    String calIdPrev = csdQuerySklOidCalIdDayNumPrev.split(CONST_SPLIT_STR)[1];
                    String csdQuerySklOidDayNumPrev = csdQuerySklOidCalIdDayNumPrev.split(CONST_SPLIT_STR)[0]
                            + CONST_SPLIT_STR + csdQuerySklOidCalIdDayNumPrev.split(CONST_SPLIT_STR)[2];
                    List<String> mstOidsForSklOidDayNumLoop = mstOidsToSklOidDayNumMap.get(csdQuerySklOidDayNumPrev);
                    for (String mstOid : mstOidsForSklOidDayNumLoop) {
                        // check if calendar Id is one for section or null if no students
                        String calIdMst = null;
                        if (m_stdCalIdToMstOid.containsKey(mstOid)) {
                            calIdMst = m_stdCalIdToMstOid.get(mstOid);
                        }
                        if ((calIdMst == null) || (calIdPrev.equals(calIdMst))) {
                            // add school dates (for full year)
                            List<PlainDate> csdDates = classDtFullYrToMstOidMap.get(mstOid);
                            if (csdDates == null) {
                                csdDates = new ArrayList<>();
                            }
                            csdDates.addAll(csdDatesForSklOidCalIdDayNum);
                            classDtFullYrToMstOidMap.put(mstOid, csdDates);

                            // add days of week
                            if (courseIncludeDays) {
                                List<String> csdDays = m_classDayListToMstOidMap.get(mstOid);
                                if (csdDays == null) {
                                    csdDays = new ArrayList<>();
                                }
                                csdDays.addAll(csdDaysForSklOidCalIdDayNum);
                                m_classDayListToMstOidMap.put(mstOid, csdDays);
                            }

                            // add bell schedule oid if not populated already
                            // - taking first, only one set of times on report
                            if ((courseIncludeTime) && (belOidToMstOidMap.get(mstOid) == null)) {
                                belOidToMstOidMap.put(mstOid, belOidForSklOidCalIdDayNum);
                            }
                        }
                    }

                    // initialize lists
                    csdDatesForSklOidCalIdDayNum = new ArrayList<>();
                    csdDaysForSklOidCalIdDayNum = new ArrayList<>();
                    belOidForSklOidCalIdDayNum = CONST_EMPTY;
                }
                csdQuerySklOidCalIdDayNumPrev = sklOidCalIdDayNum;

                // add class date to list for school/day
                if (!csdDatesForSklOidCalIdDayNum.contains(csdDate)) {
                    csdDatesForSklOidCalIdDayNum.add(csdDate);

                    // save the day-of-week if several have not been been saved for the day number
                    if ((courseIncludeDays)
                            && (csdDatesForSklOidCalIdDayNum.size() < CONST_MIN_DATES_SAVED_FOR_DAY_LOOKUP)) {
                        m_calendar.setTime(csdDate);
                        int csdDateDayOfWeek = m_calendar.get(Calendar.DAY_OF_WEEK);
                        csdDaysForSklOidCalIdDayNum.add(Integer.valueOf(csdDateDayOfWeek).toString());
                    }
                }

                // save bell schedule oid for school/day
                if ((courseIncludeTime) && (!StringUtils.isEmpty(belOid))) {
                    belOidForSklOidCalIdDayNum = belOid;
                }
            }
        } catch (Exception e) {
            // Catch the exception into the report output message.
            logToolMessage(Level.INFO, e.getMessage(), false);
            throw e;
        }

        // write class date list for last school/day
        if (!(csdQuerySklOidCalIdDayNumPrev == null)) {
            // add school dates/days to lists to mstOid
            // get calendar id and key without calendar id
            String calIdPrev = csdQuerySklOidCalIdDayNumPrev.split(CONST_SPLIT_STR)[1];
            String csdQuerySklOidDayNumPrev = csdQuerySklOidCalIdDayNumPrev.split(CONST_SPLIT_STR)[0] + CONST_SPLIT_STR
                    + csdQuerySklOidCalIdDayNumPrev.split(CONST_SPLIT_STR)[2];
            List<String> mstOidsForSklOidDayNumLoop = mstOidsToSklOidDayNumMap.get(csdQuerySklOidDayNumPrev);
            for (String mstOid : mstOidsForSklOidDayNumLoop) {
                // check if calendar Id is one for section or null if no students
                String calIdMst = null;
                if (m_stdCalIdToMstOid.containsKey(mstOid)) {
                    calIdMst = m_stdCalIdToMstOid.get(mstOid);
                }
                if ((calIdMst == null) || (calIdPrev.equals(calIdMst))) {
                    // add school dates (for full year)
                    List<PlainDate> csdDates = classDtFullYrToMstOidMap.get(mstOid);
                    if (csdDates == null) {
                        csdDates = new ArrayList<>();
                    }
                    csdDates.addAll(csdDatesForSklOidCalIdDayNum);
                    classDtFullYrToMstOidMap.put(mstOid, csdDates);

                    // add days of week
                    if (courseIncludeDays) {
                        List<String> csdDays = m_classDayListToMstOidMap.get(mstOid);
                        if (csdDays == null) {
                            csdDays = new ArrayList<>();
                        }
                        csdDays.addAll(csdDaysForSklOidCalIdDayNum);
                        m_classDayListToMstOidMap.put(mstOid, csdDays);
                    }

                    // add bell schedule oid if not populated already
                    // - taking first, only one set of times on report
                    if ((courseIncludeTime) && (belOidToMstOidMap.get(mstOid) == null)) {
                        belOidToMstOidMap.put(mstOid, belOidForSklOidCalIdDayNum);
                    }
                }
            }
        }

        /*
         * clean class date lists to only leave for dates within schedule term sort
         * class date lists saved to mstOids save class time (start/end) by mstOids
         */
        // get sections
        Set<String> mstOids = classDtFullYrToMstOidMap.keySet();

        // get class date list
        // get unsorted list, sort for each section and add to output map
        // for class time
        // get period names for mst, get bell schedule times
        for (String mstOid : mstOids) {
            // get class date list
            List<PlainDate> classDtsFullYr = classDtFullYrToMstOidMap.get(mstOid);
            PlainDate classStartDate = m_classStartDateToMstOid.get(mstOid);
            PlainDate classEndDate = m_classEndDateToMstOid.get(mstOid);

            // process class date list for each section
            List<PlainDate> classDts = new ArrayList<>();
            if (classDtsFullYr != null) {
                // create class date list for section's schedule term
                for (PlainDate classDt : classDtsFullYr) {
                    if ((classDt.compareTo(classStartDate) >= 0) && (classDt.compareTo(classEndDate) <= 0)) {
                        classDts.add(classDt);
                    }
                }

                // sort list of dates
                classDts.sort(new Comparator<PlainDate>() {
                    @Override
                    public int compare(PlainDate o1, PlainDate o2) {
                        return o1.compareTo(o2);
                    }
                });
            }

            // save sorted list
            m_classDtListToMstOidMap.put(mstOid, classDts);

            if (courseIncludeTime) {
                // initialize section start/end times
                PlainTime startTimeMst = null;
                PlainTime endTimeMst = null;
                Double durationMst = null;

                // get start/end time and class length if overriding values saved for section
                MasterSchedule mst = m_mstToMstOid.get(mstOid);
                String startTimeMstStr = (String) mst.getFieldValueByAlias(OntarioAlias.ALIAS_MST_CON_ED_START_TIME);
                if (startTimeMstStr != null) {
                    String[] startTimeMstStrSplit = startTimeMstStr.split(CONST_COLON);
                    if (startTimeMstStrSplit.length >= 2) {
                        startTimeMst = new PlainTime();
                        int startTimeHourInt = Integer.parseInt(startTimeMstStrSplit[0]);
                        int startTimeMinInt = Integer.parseInt(startTimeMstStrSplit[1]);
                        m_calendar.setTime(startTimeMst);
                        m_calendar.set(Calendar.HOUR_OF_DAY, startTimeHourInt);
                        m_calendar.set(Calendar.MINUTE, startTimeMinInt);
                        startTimeMst.setTime(m_calendar.getTimeInMillis());
                    }
                }
                String endTimeMstStr = (String) mst.getFieldValueByAlias(OntarioAlias.ALIAS_MST_CON_ED_END_TIME);
                if (endTimeMstStr != null) {
                    String[] endTimeMstStrSplit = endTimeMstStr.split(CONST_COLON);
                    if (endTimeMstStrSplit.length >= 2) {
                        endTimeMst = new PlainTime();
                        int endTimeHourInt = Integer.parseInt(endTimeMstStrSplit[0]);
                        int endTimeMinInt = Integer.parseInt(endTimeMstStrSplit[1].split(CONST_SPACE)[0]);
                        m_calendar.setTime(endTimeMst);
                        m_calendar.set(Calendar.HOUR_OF_DAY, endTimeHourInt);
                        m_calendar.set(Calendar.MINUTE, endTimeMinInt);
                        endTimeMst.setTime(m_calendar.getTimeInMillis());
                    }
                }
                String durationMstStr = (String) mst.getFieldValueByAlias(OntarioAlias.ALIAS_MST_CON_ED_LENGTH_MINS);
                if (durationMstStr != null) {
                    durationMst = Integer.valueOf(durationMstStr).doubleValue();
                }

                // get start/end times from periods if overrides do not exist for either
                if ((startTimeMst == null) || (endTimeMst == null)) {
                    // get period names list
                    List<String> periodNms = periodNmsToMstOidMap.get(mstOid);

                    // sort period names
                    Collections.sort(periodNms);

                    // get class start/end time
                    if (belOidToMstOidMap.get(mstOid) != null) {
                        String belOid = belOidToMstOidMap.get(mstOid);
                        // look for lowest start time and highest end time for class periods
                        // -(if bell schedule is found)
                        if (!StringUtils.isEmpty(belOid)) {
                            for (String periodNm : periodNms) {
                                String keyField = belOid + CONST_HYPHEN + periodNm;
                                PlainTime startTimeBpe = null;
                                PlainTime endTimeBpe = null;

                                ScheduleBellPeriod bpe = m_bpeTimeToBelOidPeriodNmMap.get(keyField);
                                if (bpe != null) {
                                    startTimeBpe = bpe.getStartTime();
                                    endTimeBpe = bpe.getEndTime();
                                }

                                if ((startTimeBpe != null)
                                        && ((startTimeMst == null) || (startTimeBpe.before(startTimeMst)))) {
                                    startTimeMst = startTimeBpe;
                                }
                                if ((endTimeBpe != null) && ((endTimeMst == null) || (endTimeBpe.after(endTimeMst)))) {
                                    endTimeMst = endTimeBpe;
                                }
                            }
                        }
                    }
                }

                // format resulting times
                String classStartTime = CONST_EMPTY + CONST_COMMA + CONST_EMPTY;
                if (startTimeMst != null) {
                    m_calendar.setTime(startTimeMst);
                    int classStartHourInt = m_calendar.get(Calendar.HOUR_OF_DAY);
                    int classStartMinInt = m_calendar.get(Calendar.MINUTE);
                    String classStartAmPm = CONST_AM;
                    if (classStartHourInt == CONST_ZERO) {
                        classStartHourInt = CONST_PM_HOUR;
                        classStartAmPm = CONST_AM;
                    } else if (classStartHourInt > CONST_PM_HOUR) {
                        classStartHourInt = classStartHourInt - CONST_PM_HOUR;
                        classStartAmPm = CONST_PM;
                    } else if (classStartHourInt == CONST_PM_HOUR) {
                        classStartAmPm = CONST_PM;
                    }
                    classStartTime = CONST_FORMAT_INT_2.format(classStartHourInt) + CONST_COLON
                            + CONST_FORMAT_INT_2.format(classStartMinInt) + CONST_COMMA + classStartAmPm;
                }
                String classEndTime = CONST_EMPTY + CONST_COMMA + CONST_EMPTY;
                if (endTimeMst != null) {
                    m_calendar.setTime(endTimeMst);
                    int classEndHourInt = m_calendar.get(Calendar.HOUR_OF_DAY);
                    int classEndMinInt = m_calendar.get(Calendar.MINUTE);
                    String classEndAmPm = CONST_AM;
                    if (classEndHourInt == CONST_ZERO) {
                        classEndHourInt = CONST_PM_HOUR;
                        classEndAmPm = CONST_AM;
                    } else if (classEndHourInt > CONST_PM_HOUR) {
                        classEndHourInt = classEndHourInt - CONST_PM_HOUR;
                        classEndAmPm = CONST_PM;
                    } else if (classEndHourInt == CONST_PM_HOUR) {
                        classEndAmPm = CONST_PM;
                    }
                    classEndTime = CONST_FORMAT_INT_2.format(classEndHourInt) + CONST_COLON
                            + CONST_FORMAT_INT_2.format(classEndMinInt) + CONST_COMMA + classEndAmPm;
                }
                // get class duration
                String classDuration = CONST_FORMAT_DBL_2_CALC_ONLY.format(CONST_ZERO_DBL);
                if (durationMst != null) {
                    double classDurationDbl = durationMst / CONST_HOUR_TO_MINS_DBL;
                    classDuration = CONST_FORMAT_DBL_2_CALC_ONLY.format(classDurationDbl);
                } else {
                    if ((startTimeMst != null) && (endTimeMst != null)) {
                        LocalDateTime startTimeMstLocal = new java.sql.Timestamp(startTimeMst.getTime()).toLocalDateTime();
                        LocalDateTime endTimeMstLocal = new java.sql.Timestamp(endTimeMst.getTime()).toLocalDateTime();
                        Duration classDurationObject = Duration.between(startTimeMstLocal, endTimeMstLocal);
                        long classDurationMins = classDurationObject.toMinutes();
                        double classDurationDbl = Double.valueOf(classDurationMins)
                                / CONST_HOUR_TO_MINS_DBL;
                        classDuration = CONST_FORMAT_DBL_2_CALC_ONLY.format(classDurationDbl);
                    }
                }

                // save result by mstOid
                m_classStartEndTimeToMstOidMap.put(mstOid,
                        classStartTime + CONST_COMMA + classEndTime + CONST_COMMA + classDuration);
            }
        }
    }

    /**
     * Loads class attendance list in order of class number to student to master
     * schedule map
     */
    public void loadClassAttListToStdOidToMstOid() {
        // create criteria/query to get class attendance for selected sections
        X2Criteria patCriteria = new X2Criteria();

        // based on selection of master schedule oids
        SubQuery subQuery = getSubQueryFromMstSelection(StudentPeriodAttendance.COL_MASTER_SCHEDULE_OID);
        patCriteria.addExists(subQuery);

        String[] patColumns = new String[] {StudentPeriodAttendance.COL_MASTER_SCHEDULE_OID,
                StudentPeriodAttendance.COL_STUDENT_OID, StudentPeriodAttendance.COL_DATE,
                StudentPeriodAttendance.COL_CODE_VIEW, StudentPeriodAttendance.COL_OTHER_CODE,
                StudentPeriodAttendance.COL_OTHER_CODE02, StudentPeriodAttendance.COL_REASON_CODE};

        ColumnQuery patQuery = new ColumnQuery(StudentPeriodAttendance.class, patColumns, patCriteria);

        patQuery.addOrderByAscending(StudentPeriodAttendance.COL_MASTER_SCHEDULE_OID);
        patQuery.addOrderByAscending(StudentPeriodAttendance.COL_STUDENT_OID);
        patQuery.addOrderByAscending(StudentPeriodAttendance.COL_DATE);

        // load map of class attendance to master schedule/student
        m_attListToMstOidStdOidMap = new HashMap<>();
        m_attAbsentUnfundedCtToMstOidStdOid = new HashMap<>();

        String mstOidCurrent = CONST_EMPTY;
        String mstOidPrev = CONST_EMPTY;
        int mstCount = 0;

        String mstOidStdOidPrev = CONST_EMPTY;
        List<String> attList = null;

        List<String> reasons = new ArrayList<>();

        int attAbsentConsecCtAbs = CONST_ZERO;
        int attAbsentUnfundedCt = CONST_ZERO;
        // these will be initialized based on class dates for section
        List<PlainDate> classDtListForMstOid = null;
        int classDtListSize = CONST_ZERO;
        // loop through class attendance
        try (ReportQueryIterator patIterator = getBroker().getReportQueryIteratorByQuery(patQuery)) {
            while (patIterator.hasNext()) {
                // get new values
                Object[] data = (Object[]) patIterator.next();
                mstOidCurrent = data[0].toString();
                String stdOid = data[1].toString();
                String mstOidStdOid = mstOidCurrent + CONST_HYPHEN + stdOid;
                PlainDate patDate = new PlainDate((Timestamp) data[2]);
                String patCodeIn = data[3].toString();
                String patOtherCode = OntarioAlias.CONST_EMPTY;
                if (data[4] != null) {
                    patOtherCode = data[4].toString();
                }
                String patOtherCode02 = OntarioAlias.CONST_EMPTY;
                if (data[5] != null) {
                    patOtherCode02 = data[5].toString();
                }

                String reasonCode = data[6] == null ? "" : data[6].toString();
                StringBuilder reasonBuilder = new StringBuilder();

                // continue to next iteration if patDate is not between start and end date for
                // std for mst
                EnrRegStudentStartEndDatesForSection stdEnrStartEndDatesForMst = m_stdStartEndDatesToMstOidMap
                        .get(mstOidCurrent);
                if ((stdEnrStartEndDatesForMst == null)
                        || (!stdEnrStartEndDatesForMst.isDateInStdMstEnrDates(stdOid, patDate))) {
                    continue;
                }

                // if new section/student, write attendance count for prev
                if ((!StringUtils.isEmpty(mstOidStdOidPrev) && (!mstOidStdOidPrev.equals(mstOidStdOid)))) {
                    if (attAbsentConsecCtAbs >= CONST_ABS_CONSEC_CT_MIN) {
                        attAbsentUnfundedCt += attAbsentConsecCtAbs;
                    }

                    if (!m_locForLang.getSystemLocale().toString().equals(OntarioAlias.CONST_LOCALE_ENGLISH)) {
                        attList = setAttCodeListToFrench(attList);
                    }
                    m_attListToMstOidStdOidMap.put(mstOidStdOidPrev, attList);
                    m_attAbsentUnfundedCtToMstOidStdOid.put(mstOidStdOidPrev, attAbsentUnfundedCt);
                    attList = null;
                    attAbsentConsecCtAbs = CONST_ZERO;
                    attAbsentUnfundedCt = CONST_ZERO;
                }
                mstOidStdOidPrev = mstOidStdOid;

                // if new section get class dates and size of class dates list
                if (!mstOidCurrent.equals(mstOidPrev)) {
                    classDtListForMstOid = m_classDtListToMstOidMap.get(mstOidCurrent);
                    if (classDtListForMstOid != null) {
                        classDtListSize = classDtListForMstOid.size();
                    } else {
                        classDtListSize = CONST_ZERO;
                    }
                }

                if (!mstOidCurrent.equals(mstOidPrev)) {
                    if (reasons.size() != 0) {
                        m_mstReasons.put(mstOidPrev, reasons.stream()
                                .distinct()
                                .collect(Collectors.joining("\n")));

                        reasons.clear();
                        mstCount++;
                    }
                }

                mstOidPrev = mstOidCurrent;

                // get index of att date in class date list
                int indexAttDt = CONST_LIST_NOT_FOUND;
                if ((classDtListForMstOid != null) && (classDtListForMstOid.contains(patDate))) {
                    indexAttDt = classDtListForMstOid.indexOf(patDate);
                }

                // save class attendance if att date is found in class date list
                if (!(indexAttDt == CONST_LIST_NOT_FOUND)) {
                    // initialize class attendance list for student/section if first attendance for
                    // them
                    if (attList == null) {
                        attList = new ArrayList<>(classDtListSize);
                        for (int i = 0; i < classDtListSize; i++) {
                            attList.add(CONST_EMPTY);
                        }
                    }

                    // save class attendance
                    String patCodeEnlish = patCodeIn;
                    if (!m_locForLang.getSystemLocale().toString().equals(OntarioAlias.CONST_LOCALE_ENGLISH)) {
                        if (ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.get(patCodeIn) != null) {
                            patCodeEnlish = ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.get(patCodeIn);
                        }
                    }

                    if (patCodeEnlish.contains(OntarioAlias.ATT_CODE_GENERAL_ABSENCE)) {
                        patCodeEnlish = OntarioAlias.ATT_CODE_GENERAL_ABSENCE;
                    } else if (
                        // if other codes show cancelled unfunded move corresponding report code
                        // other code
                            ((patOtherCode != null) && (patOtherCode.equals(OntarioAlias.ATT_CODE_CON_ED_CANCELLED_UNFUNDED)))
                                    // other code 02
                                    || ((patOtherCode02 != null)
                                    && (patOtherCode02.equals(OntarioAlias.ATT_CODE_CON_ED_CANCELLED_UNFUNDED)))) {
                        patCodeEnlish = OntarioAlias.ATT_CODE_CON_ED_CANCELLED_UNFUNDED_RPT;
                        reasons.add(collectReason(reasonBuilder, reasonCode, patDate));
                    } else if (
                        // if other codes show cancelled funded move corresponding report code
                        // other code
                            ((patOtherCode != null) && (patOtherCode.equals(OntarioAlias.ATT_CODE_CON_ED_CANCELLED_FUNDED)))
                                    // other code 02
                                    || ((patOtherCode02 != null)
                                    && (patOtherCode02.equals(OntarioAlias.ATT_CODE_CON_ED_CANCELLED_FUNDED)))) {
                        patCodeEnlish = OntarioAlias.ATT_CODE_CON_ED_CANCELLED_FUNDED_RPT;
                        reasons.add(collectReason(reasonBuilder, reasonCode, patDate));
                    }

                    // set pat code in output list
                    attList.set(indexAttDt, patCodeEnlish.trim());

                    // check if attendance code is not present or tardy
                    if ((!StringUtils.isEmpty(patCodeEnlish))
                            && (!patCodeEnlish.contains(OntarioAlias.ATT_CODE_TARDY))) {
                        // accumulate/reset consecutive count of absences
                        // if previous value counts as consecutive
                        if ((indexAttDt > CONST_ZERO) && (OntarioAlias.ATT_CODE_CON_ED_CONSECUTIVE_INCL
                                .contains(attList.get(indexAttDt - 1)))) {
                            if (OntarioAlias.ATT_CODE_ABSENT.equals(attList.get(indexAttDt))) {
                                attAbsentConsecCtAbs++;
                            }
                        } else
                        // if previous value does not count as consecutive
                        {
                            if (attAbsentConsecCtAbs >= CONST_ABS_CONSEC_CT_MIN) {
                                attAbsentUnfundedCt += attAbsentConsecCtAbs;
                            }
                            attAbsentConsecCtAbs = CONST_ZERO;
                            if (OntarioAlias.ATT_CODE_ABSENT.equals(attList.get(indexAttDt))) {
                                attAbsentConsecCtAbs++;
                            }
                        }

                        // if consecutive absences (including unfunded) are greater than 3 and
                        // attendance code is not funded add to days absent unfunded
                        if (patCodeEnlish.equals(OntarioAlias.ATT_CODE_CON_ED_CANCELLED_UNFUNDED_RPT)) {
                            attAbsentUnfundedCt++;
                        }
                    }
                }
            }
        } catch (Exception e) {
            // Catch the exception into the report output message.
            logToolMessage(Level.INFO, e.getMessage(), false);
            throw e;
        }

        // write class attendance for students for last section/student
        if (!StringUtils.isEmpty(mstOidStdOidPrev)) {
            // write class attendance for students for previous section
            if (attAbsentConsecCtAbs >= CONST_ABS_CONSEC_CT_MIN) {
                attAbsentUnfundedCt += attAbsentConsecCtAbs;
            }

            if (!m_locForLang.getSystemLocale().toString().equals(OntarioAlias.CONST_LOCALE_ENGLISH)) {
                attList = setAttCodeListToFrench(attList);
            }
            m_attListToMstOidStdOidMap.put(mstOidStdOidPrev, attList);
            m_attAbsentUnfundedCtToMstOidStdOid.put(mstOidStdOidPrev, attAbsentUnfundedCt);
        }

        if (mstCount == 0) {
            m_mstReasons.put(mstOidPrev, reasons.stream()
                    .distinct()
                    .collect(Collectors.joining("\n")));
        }
    }


    /**
     * Loads reference code description by code for reference table
     *
     * @param rtbOid
     *
     * @return Map<String, ReferenceCode>
     */
    public Map<String, String> loadReferenceCodeDescMap(String rtbOid) {
        Map<String, String> rcdDescByCodeMap = new HashMap<>();

        // create criteria for student contact
        X2Criteria rcdCriteria = new X2Criteria();

        // for students in student criteria
        rcdCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, rtbOid);

        // select query
        QueryByCriteria rcdQuery = new QueryByCriteria(ReferenceCode.class, rcdCriteria);

        // load map of reference code description to reference code
        try (QueryIterator rcdIterator = getBroker().getIteratorByQuery(rcdQuery)) {
            // iterates through and saves into map
            while (rcdIterator.hasNext()) {
                ReferenceCode rcd = (ReferenceCode) rcdIterator.next();
                String rcdCode = rcd.getCode();
                String rcdDesc = rcd.getDescription();

                rcdDescByCodeMap.put(rcdCode, rcdDesc);
            }
        }

        // return map
        return rcdDescByCodeMap;
    }

    /**
     * Sets each att code value in input string to french
     *
     * @param attListEng
     * @return List<String> - attListFr
     */
    public List<String> setAttCodeListToFrench(List<String> attListEng) {
        List<String> attListFr = new ArrayList<>();

        for (String attCodeEng : attListEng) {
            String attCodeFr = attCodeEng;
            if ((!StringUtils.isEmpty(attCodeEng))
                    && (OntarioAlias.ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.get(attCodeEng) != null)) {
                attCodeFr = OntarioAlias.ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.get(attCodeEng);
            }

            attListFr.add(attCodeFr);
        }

        return attListFr;
    }

    /**
     * Release resources.
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#releaseResources()
     */
    @Override
    public void releaseResources() {
        super.releaseResources();
        logToolMessage(Level.INFO, "Sample variable size BEFORE clearing it, m_dataDictionary: " + m_dictionary + " "
                + com.x2dev.utils.MemoryUtils.formatMemoryUsed(getObjectSize(m_dictionary, false)), false);
        clearLocallyDeclaredVariables(this.getClass(), true);
        logToolMessage(Level.INFO, "Sample variable size AFTER clearing it, m_dataDictionary: " + m_dictionary + " "
                + com.x2dev.utils.MemoryUtils.formatMemoryUsed(getObjectSize(m_dictionary, false)), false);
    }

    /**
     * Method to sift through all your declared member variables and sets them to
     * null for proper garbage collection.
     *
     * @param classToClear Class
     * @param recordOnToolLog boolean
     */
    public void clearLocallyDeclaredVariables(Class classToClear, boolean recordOnToolLog) {
        if (recordOnToolLog) {
            logToolMessage(Level.INFO, "Memory used by tool: " + getTotalMemoryUsedByClass(classToClear), false);
        }
        for (java.lang.reflect.Field field : classToClear.getDeclaredFields()) {
            if (field.getModifiers() == java.lang.reflect.Modifier.PRIVATE && !field.getType().isPrimitive()) {
                field.setAccessible(true);
            }
            if (field.isAccessible()) {
                try {
                    field.set(this, null);
                } catch (IllegalArgumentException e) {
                    if (recordOnToolLog) {
                        logToolMessage(Level.INFO,
                                "Unable to clear " + field.getName() + " due to IllegalArgumentException" + "\n" + e,
                                false);
                    }
                } catch (IllegalAccessException e) {
                    if (recordOnToolLog) {
                        logToolMessage(Level.INFO,
                                "Unable to clear " + field.getName() + " due to IllegalAccessException" + "\n" + e,
                                false);
                    }
                }
            }
        }
        if (recordOnToolLog) {
            logToolMessage(Level.INFO, "Memory used after clearing: " + getTotalMemoryUsedByClass(classToClear), false);
        }
    }

    /**
     * Method that relies on reflection to gather the size of all locally declared
     * variables.
     *
     * @param classToMeasure
     * @return Easy to read memory size as a string
     */
    public String getTotalMemoryUsedByClass(Class classToMeasure) {
        long totalMemoryUsed = 0;
        for (java.lang.reflect.Field field : classToMeasure.getDeclaredFields()) {
            if (field.getModifiers() == java.lang.reflect.Modifier.PRIVATE) {
                field.setAccessible(true);
            }
            try {
                totalMemoryUsed = totalMemoryUsed + getObjectSize(field.get(this), false);
            } catch (IllegalArgumentException e) {
                // TODO You could log exception to learn more if needed.
                e.printStackTrace();
            } catch (IllegalAccessException e) {
                // TODO You could log expection to learn more if needed.
                e.printStackTrace();
            }
        }
        return com.x2dev.utils.MemoryUtils.formatMemoryUsed(totalMemoryUsed);
    }

    /**
     * Returns approximate footprint of an object in memory.
     *
     * @param obj Object
     * @param recordOnToolLog boolean
     * @return long
     */
    public long getObjectSize(Object obj, boolean recordOnToolLog) {
        long memmoryUsed = 0;
        try {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            ObjectOutputStream oos = new ObjectOutputStream(baos);
            oos.writeObject(obj);
            oos.close();
            if (recordOnToolLog) {
                logToolMessage(Level.INFO,
                        obj + " Object Data Size: " + com.x2dev.utils.MemoryUtils.formatMemoryUsed(baos.size()), false);
            }
            memmoryUsed += baos.size();
        } catch (IOException e) {
            e.printStackTrace();
        }

        return memmoryUsed;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     */
    @Override
    public void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);

        // check if staff view
        m_staffCurrent = null;
        if (userData.getSessionNavConfig().getApplicationContext().contextDisplayName(getBroker().getPersistenceKey())
                .equalsIgnoreCase(ApplicationContext.STAFF.toString())) {
            m_staffCurrent = userData.getStaff();
        }
    }

    /**
     * Returns sub query of mst oids
     *
     * @param beanPath target mstoid column path
     * @return sub query
     */
    public SubQuery getSubQueryFromMstSelection(String beanPath) {
        SubQuery mstSubQuery;

        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, m_selectionMst.getOid());
        subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID, Criteria.PARENT_QUERY_PREFIX + beanPath);

        mstSubQuery = new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria);

        return mstSubQuery;
    }

    /**
     * Takes an alias of field and returns either the java name or the database name
     * that field depending on the status of returnBbName.
     *
     * @param alias - String,
     * @param returnJavaName - boolean
     *
     * @return String the java name or field name of the object that is aliased
     */
    public String getBeanPathFromAlias(String alias, boolean returnJavaName) {
        String foundField = null;

        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(alias);

        if (field != null) {
            if (returnJavaName) {
                foundField = field.getJavaName();
            } else {
                foundField = field.getDatabaseName();
            }
        }

        return foundField;
    }

    /**
     * Takes a bean and an alias (for a udf) and returns the PlainDate in that udf.
     *
     * @param bean - X2BaseBean
     * @param alias - String
     *
     * @return PlainDate - value for alias in bean
     */
    public PlainDate getDate(X2BaseBean bean, String alias) {
        PlainDate date = null;
        if (bean != null) {
            String value = (String) bean.getFieldValueByAlias(alias, m_dictionary);
            date = getDate(value);
        }

        return date;
    }

    /**
     * Takes a date as string and returns the PlainDate.
     *
     * @param dateStr - String in "yyyy-MM-dd" format
     *
     * @return PlainDate - value for alias in bean
     */
    public PlainDate getDate(String dateStr) {
        PlainDate date = null;
        try {
            date = new PlainDate(CONST_FORMAT_DATE_MST_IN.parse(dateStr));
        } catch (ParseException parseExp) {
            logToolMessage(Level.WARNING, String.format("%s %s ", " Problem with date parsing", parseExp.getMessage()),
                    false);
        }

        return date;
    }

    /**
     * A utility method that adds a number of days to a calendar date and returns a
     * date value representing this new date.
     *
     * @param cal the calendar date to add days to
     * @param days the number of days to add
     *
     * @return the newly calculated date
     */
    public PlainDate addDays(Calendar cal, int days) {
        cal.add(Calendar.DATE, days);
        return new PlainDate(cal.getTime().getTime());
    }

    /**
     * Returns the Jasper format based on report id
     *
     * @param reportId
     *
     * @return InputStream An InputStream containing the compile Jasper format
     */
    public InputStream getFormatRptId(String reportId) {
        InputStream format = null;

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(Report.COL_ID, reportId);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        Report alternateFormat = (Report) getBroker().getBeanByQuery(query);
        if (alternateFormat != null) {
            format = new ByteArrayInputStream(alternateFormat.getCompiledFormat());
        }

        /*
         * If a custom format was not specified (or it was invalid) then use the
         * original report's format.
         */
        if (format == null) {
            Report report = (Report) getJob().getTool();
            format = new ByteArrayInputStream(report.getCompiledFormat());
        }

        return format;
    }

    /**
     * Gets the data break column.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.reports.Publishable#getDataBreakColumn()
     */
    @Override
    public String getDataBreakColumn() {
        return FIELD_MASTER_SCHEDULE;
    }

    /**
     * Gets the description.
     *
     * @param bean X2BaseBean
     * @return String
     */
    @Override
    public String getDescription(X2BaseBean bean) {
        return "Enrolment Register for " + ((MasterSchedule) bean).getCourseView();
    }

    /**
     * Gets the email address. Will never be published so returning null.
     *
     * @param person Person
     * @return String
     */
    @Override
    public String getEmailAddress(Person person) {
        return null;
    }

    /**
     * Gets the email recipients. Will never be published so returning null.
     *
     * @param bean X2BaseBean
     * @return Collection
     * @see com.follett.fsc.core.k12.tools.reports.Publishable#getEmailRecipients(X2BaseBean)
     */
    @Override
    public Collection<Person> getEmailRecipients(X2BaseBean bean) {
        return null;
    }

    /**
     * Returns the value of a boolean parameter or false if the parameter does not
     * exist in the input definition.
     *
     * @param key String
     * @return boolean
     */
    public boolean getBooleanParameter(String key) {
        return getParameter(key) != null && (Boolean) getParameter(key);
    }

    /**
     * Inner class for saving and getting dates for student enrolment in one section
     *
     * @author Follett Software Company
     */
    public class EnrRegStudentStartEndDatesForSection {
        // Variables - mst oid
        public String m_mstOid;
        public PlainDate m_mstStartDate;
        public PlainDate m_mstEndDate;

        // Variables - effective start and end dates for each student
        public Map<String, List<PlainDate>> m_stdStartDatesListToStdOid;
        public Map<String, List<PlainDate>> m_stdEndDatesListToStdOid;

        /**
         * Constructor; initializes for the mst
         *
         * @param mstOid
         * @param mstStartDt
         * @param mstEndDt
         */
        public EnrRegStudentStartEndDatesForSection(String mstOid, PlainDate mstStartDt, PlainDate mstEndDt) {
            m_mstOid = mstOid;
            m_mstStartDate = mstStartDt;
            m_mstEndDate = mstEndDt;

            m_stdStartDatesListToStdOid = new HashMap<>();
            m_stdEndDatesListToStdOid = new HashMap<>();
        }

        /**
         * Add start date for mst/std
         *
         * @param stdOid
         * @param stdMstStartDt
         */
        public void addStdMstStartDate(String stdOid, PlainDate stdMstStartDt) {
            List<PlainDate> stdStartDatesList;
            List<PlainDate> stdEndDatesList;
            if (m_stdStartDatesListToStdOid.get(stdOid) != null) {
                stdStartDatesList = m_stdStartDatesListToStdOid.get(stdOid);
                stdEndDatesList = m_stdEndDatesListToStdOid.get(stdOid);
            } else {
                stdStartDatesList = new ArrayList<>();
                stdEndDatesList = new ArrayList<>();
            }

            stdStartDatesList.add(stdMstStartDt);
            stdEndDatesList.add(m_mstEndDate);

            m_stdStartDatesListToStdOid.put(stdOid, stdStartDatesList);
            m_stdEndDatesListToStdOid.put(stdOid, stdEndDatesList);
        }

        /**
         * Add end date for mst/std
         *
         * @param stdOid
         * @param stdMstEndDt
         */
        public void addStdMstEndDate(String stdOid, PlainDate stdMstEndDt) {
            List<PlainDate> stdStartDatesList;
            List<PlainDate> stdEndDatesList;
            if (m_stdStartDatesListToStdOid.get(stdOid) != null) {
                stdStartDatesList = m_stdStartDatesListToStdOid.get(stdOid);
                stdEndDatesList = m_stdEndDatesListToStdOid.get(stdOid);
            } else {
                stdStartDatesList = new ArrayList<>();
                stdEndDatesList = new ArrayList<>();
            }

            if (!stdEndDatesList.isEmpty()) {
                stdEndDatesList.set(stdEndDatesList.size() - 1, stdMstEndDt);

                m_stdStartDatesListToStdOid.put(stdOid, stdStartDatesList);
                m_stdEndDatesListToStdOid.put(stdOid, stdEndDatesList);
            }
        }

        /**
         * Check if student is enrolled within class dates
         *
         * @param stdOid
         *
         * @return boolean
         */
        public boolean isStdEnrolledWithinClassDates(String stdOid) {
            List<PlainDate> stdStartDatesList = null;
            List<PlainDate> stdEndDatesList = null;
            if (m_stdStartDatesListToStdOid.get(stdOid) != null) {
                stdStartDatesList = m_stdStartDatesListToStdOid.get(stdOid);
                stdEndDatesList = m_stdEndDatesListToStdOid.get(stdOid);
            }

            boolean stdEnrolledWithinClassDates = false;

            // return false if student dates not found
            if (stdStartDatesList == null) {
                return stdEnrolledWithinClassDates;
            }

            // loop thru date ranges and return true if pat record is valid
            int indexDates = 0;
            while ((indexDates < stdStartDatesList.size()) && (!stdEnrolledWithinClassDates)) {
                PlainDate stdStartDate = stdStartDatesList.get(indexDates);
                PlainDate stdEndDate = stdEndDatesList.get(indexDates);

                // verify student start and end date including that end date is after start (not
                // just equal)
                if ((stdEndDate.after(stdStartDate)) &&
                        ((!stdEndDate.before(m_mstStartDate)) || (!stdStartDate.after(m_mstEndDate)))) {
                    stdEnrolledWithinClassDates = true;
                }

                indexDates++;
            }

            return stdEnrolledWithinClassDates;
        }

        /**
         * Check if date is for when student enrolled in class
         *
         * @param stdOid - student oid
         * @param dateToCheck - input date to method
         *
         * @return boolean
         */
        public boolean isDateInStdMstEnrDates(String stdOid, PlainDate dateToCheck) {
            List<PlainDate> stdStartDatesList = null;
            List<PlainDate> stdEndDatesList = null;
            if (m_stdStartDatesListToStdOid.get(stdOid) != null) {
                stdStartDatesList = m_stdStartDatesListToStdOid.get(stdOid);
                stdEndDatesList = m_stdEndDatesListToStdOid.get(stdOid);
            }

            boolean dateToCheckInStdMstEnrDates = false;

            // return false if student dates not found
            if (stdStartDatesList == null) {
                return dateToCheckInStdMstEnrDates;
            }

            // loop through date ranges and return true if pat record is valid
            int indexDates = 0;
            while ((indexDates < stdStartDatesList.size()) && (!dateToCheckInStdMstEnrDates)) {
                PlainDate stdStartDate = stdStartDatesList.get(indexDates);
                PlainDate stdEndDate = stdEndDatesList.get(indexDates);

                if ((!dateToCheck.before(stdStartDate)) && (!dateToCheck.after(stdEndDate))) {
                    dateToCheckInStdMstEnrDates = true;
                }

                indexDates++;
            }

            return dateToCheckInStdMstEnrDates;
        }

        /**
         * Check if date is for when student enrolled in class
         *
         * @param stdOid - student oid
         * @param dateToCheck - input date to method
         * @param notArrivedAsOfDate - a specific date indicating the student has not arrived as of that date
         *
         * @return boolean
         */
        public boolean isDateInStdMstEnrDates(String stdOid, PlainDate dateToCheck, PlainDate notArrivedAsOfDate) {
            List<PlainDate> stdStartDatesList = null;
            List<PlainDate> stdEndDatesList = null;

            if (m_stdStartDatesListToStdOid.get(stdOid) != null) {
                stdStartDatesList = m_stdStartDatesListToStdOid.get(stdOid);
                stdEndDatesList = m_stdEndDatesListToStdOid.get(stdOid);
            }

            boolean dateToCheckInStdMstEnrDates = false;

            if (stdStartDatesList == null) {
                return dateToCheckInStdMstEnrDates;
            }

            int indexDates = 0;

            while (indexDates < stdStartDatesList.size() && !dateToCheckInStdMstEnrDates) {
                PlainDate stdStartDate = stdStartDatesList.get(indexDates);
                PlainDate stdEndDate = stdEndDatesList.get(indexDates);

                if (!dateToCheck.before(stdStartDate) && !dateToCheck.after(stdEndDate)) {
                    dateToCheckInStdMstEnrDates = true;
                }

                indexDates++;
            }

            if (notArrivedAsOfDate != null && notArrivedAsOfDate.equals(dateToCheck)) {
                dateToCheckInStdMstEnrDates = false;
            }

            return dateToCheckInStdMstEnrDates;
        }

        /**
         * Get first (effective) start date for student for class -defaults to class
         * start date
         *
         * @param stdOid - student oid
         *
         * @return PlainDate
         */
        public PlainDate getEffStartDateForStdForMst(String stdOid) {
            List<PlainDate> stdStartDatesList = null;
            List<PlainDate> stdEndDatesList = null;

            if (m_stdStartDatesListToStdOid.get(stdOid) != null) {
                stdStartDatesList = m_stdStartDatesListToStdOid.get(stdOid);
                stdEndDatesList = m_stdEndDatesListToStdOid.get(stdOid);
            }

            PlainDate effStartDateStdMst = null;

            // return null if student dates not found
            if (stdStartDatesList == null) {
                return m_mstStartDate;
            }

            // loop through date ranges and return true if pat record is valid
            int indexDates = 0;
            while ((indexDates < stdStartDatesList.size()) && (effStartDateStdMst == null)) {
                // verify that student not dropped before it starts
                PlainDate indexStdEndDate = stdEndDatesList.get(indexDates);
                if (indexStdEndDate.before(m_mstStartDate)) {
                    indexDates++;
                    continue;
                }

                effStartDateStdMst = stdStartDatesList.get(indexDates);

                indexDates++;
            }

            // if the effective start date is before class starts set to class start date
            if ((effStartDateStdMst == null) || (effStartDateStdMst.before(m_mstStartDate))) {
                effStartDateStdMst = m_mstStartDate;
            }

            // if the effective start date is before student enrolls change it to date of enrollment
            PlainDate dtFirstStd = getSchool().getCurrentContext().getStartDate();
            if (m_enrInfoToStdOidTypeMap.containsKey(stdOid + OntarioAlias.CONST_ENR_DT_FIRST)) {
                String dtFirstStdStr = m_enrInfoToStdOidTypeMap.get(stdOid + OntarioAlias.CONST_ENR_DT_FIRST);
                dtFirstStd = getDate(dtFirstStdStr);
            }
            if (dtFirstStd.after(effStartDateStdMst)) {
                effStartDateStdMst = dtFirstStd;
            }

            return effStartDateStdMst;
        }
    }

    /**
     * Retrieves the arrival status of a student for a specific school year.
     *
     * @param student The student for whom the arrival status is to be determined.
     * @param selectedSchoolYear The school year for which wants to retrieve the arrival status.
     * @param schoolName The school for which wants to retrieve the arrival status.
     * @return An optional containing the arrival status if found, or an empty optional if not found.
     */
    public Optional<String> getArrivalStatus(SisStudent student, int selectedSchoolYear, String schoolName) {
        return student.getStudentSchools().stream()
                .filter(studentSchool -> studentSchool.getDistrictContext().getSchoolYear() == selectedSchoolYear)
                .filter(studentSchool -> studentSchool.getSchool().getName().equals(schoolName))
                .map(studentSchool -> (String) studentSchool.getFieldValueByAlias(OntarioAlias.ALIAS_SSK_ARRIVAL_STATUS))
                .filter(Objects::nonNull)
                .findFirst();
    }

    /**
     * Retrieves grade level OnSIS code for a given student.
     *
     * @param std The SisStudent instance for which the grade level code is to be retrieved.
     * @return An Optional containing grade level code if found.
     */
    public Optional<String> getStdGradeLevelOnSisCode(SisStudent std) {
        String stdGradeLevel = std.getGradeLevel();
        DictionaryExtractor extractor = ToolBean.getDictionaryExtractor();
        DataDictionaryField field = new ToolBean.ToolBeanColumn(SisBeanPaths.STUDENT.gradeLevel()).getField(extractor);

        return extractor
                .getReferenceCodes(field.getReferenceTableOid())
                .entrySet()
                .stream()
                .filter(gl -> !StringUtils.isEmpty(stdGradeLevel))
                .filter(gl -> gl.getKey().equals(stdGradeLevel))
                .map(type -> type.getValue().getStateCode())
                .findFirst();
    }

    /**
     * Updates the pupil count based on the student gender and increments the corresponding count.
     *
     * @param stdGender   The gender of the student.
     * @param femaleCount The count of female students to be updated.
     * @param maleCount   The count of male students to be updated.
     */
    protected void processGenderCount(String stdGender, AtomicInteger femaleCount, AtomicInteger maleCount) {
        if (stdGender.equals(OntarioAlias.CONST_GENDER_FEMALE)) {
            femaleCount.incrementAndGet();
        }

        if (stdGender.equals(OntarioAlias.CONST_GENDER_MALE)) {
            maleCount.incrementAndGet();
        }
    }

    /**
     * Checks if there are scores available for the specified student and the selected master schedule.
     *
     * @param stdOid The OID of the student to check for scores.
     * @param mstOid The OID of the master schedule to check for scores.
     * @return true if there are scores for the specified student and master schedule selection, false otherwise.
     */
    protected boolean hasScoresForMasterScheduleSelection(String stdOid, String mstOid) {
        X2Criteria gbsCriteria = new X2Criteria();

        // based on selection of master schedule oids
        SubQuery subQuery = getSubQueryFromMstSelection(GradebookScore.REL_COLUMN_DEFINITION + CONST_DELIMITER
                + GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID);

        gbsCriteria.addExists(subQuery);

        // add condition based on system indicator
        gbsCriteria.addEqualTo(GradebookScore.REL_STUDENT + CONST_DELIMITER
                + SisStudent.COL_OID, stdOid);
        gbsCriteria.addEqualTo(GradebookScore.REL_COLUMN_DEFINITION + CONST_DELIMITER
                + GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID, mstOid);

        // add condition to only include scores with a completion date (to bypass excluded assignments)
        gbsCriteria.addNotNull(GradebookScore.COL_COMPLETED_DATE);

        // add condition based on indicator to include in these registers
        String fieldAssignConEdIncl = getBeanPathFromAlias(OntarioAlias.ALIAS_GCD_CON_ED_REGISTER_INCL_ON, false);

        if (fieldAssignConEdIncl != null) {
            gbsCriteria.addEqualTo(GradebookScore.REL_COLUMN_DEFINITION
                    + CONST_DELIMITER + fieldAssignConEdIncl, CONST_TRUE);
        }

        return getBroker().getCount(new QueryByCriteria(GradebookScore.class, gbsCriteria)) > CONST_ZERO;
    }

    /**
     * Collects and formats a reason based on the provided reason code and date.
     *
     * @param reasonBuilder A StringBuilder to construct the formatted reason
     * @param reasonCode    The reason code
     * @param date          The date
     * @return A formatted string describing the reason, or an empty string if the reason code is empty
     */
    private String collectReason(StringBuilder reasonBuilder, String reasonCode, PlainDate date) {
        if (reasonCode.isEmpty()) {
            return "";

        } else {
            DictionaryExtractor extractor = ToolBean.getDictionaryExtractor();
            DataDictionaryField field = new ToolBean.ToolBeanColumn(
                    SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.reasonCode()).getField(extractor);

            Optional<String> reasonDescription = extractor
                    .getReferenceCodes(field.getReferenceTableOid())
                    .entrySet()
                    .stream()
                    .filter(rc -> rc.getKey().equals(reasonCode))
                    .map(rc -> rc.getValue().getDescription())
                    .findFirst();

            LocalDate currentAttOfSessionDay = LocalDate.parse(String.valueOf(date));
            String formattedMonthName = new DateFormatSymbols().getMonths()[currentAttOfSessionDay.getMonthValue() - 1];
            int dayOfMonth = currentAttOfSessionDay.getDayOfMonth();

            reasonBuilder.append(formattedMonthName)
                    .append(" ")
                    .append(dayOfMonth)
                    .append(" - ");

            if (reasonDescription.isPresent()) {
                reasonBuilder.append(reasonDescription.get());
            } else {
                reasonBuilder.append(reasonCode);
            }

            return reasonBuilder.toString();
        }
    }
}
