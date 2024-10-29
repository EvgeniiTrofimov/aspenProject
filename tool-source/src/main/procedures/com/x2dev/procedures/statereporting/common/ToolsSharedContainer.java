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
package com.x2dev.procedures.statereporting.common;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.beans.BeanManager.PersistenceKey;
import com.follett.fsc.core.k12.beans.DataTableConfig.OrganizationAccess;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ToolManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryRelationship;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.script.ScriptManager;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean.*;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentScheduleChange;
import com.x2dev.utils.ByteArrayClassLoader;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.LoggerUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.FieldPosition;
import java.text.Format;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.script.ScriptException;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * The Class ToolsSharedContainer.
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class ToolsSharedContainer {

    /**
     * The Class AnnualSpan.
     *
     * @author Follett Software Company
     * @copyright 2020
     */
    public static class AnnualSpan implements Comparable {
        protected List<? extends ToolEnrollment> m_enrollments = null;
        protected List<? extends ToolEnrollment> m_spanEnrollments;
        private List<? extends ToolEnrollment> m_allEnrollmentsDesc = null;
        private ToolEnrollment m_firstActiveEnrollment;
        private ToolDistrictContext m_context;
        private ToolEnrollment m_dataStorageEnrollment = null;
        private PlainDate m_dateAfterLastActiveInSessionDate;
        private Range<Date> m_dateRange;
        private PlainDate m_firstActiveInSessionDate;
        private PlainDate m_firstInactiveInSessionDate;
        private PlainDate m_lastActiveInSessionDate;
        private PlainDate m_queryAsOfDate = null;
        private ParentSpan m_parentSpan = null;
        private ToolSchool m_school;
        private ToolStudent m_student;
        private ToolSchoolCalendar m_schoolCalendar;
        private ToolStudentSchool m_studentSchool;
        private ToolEnrollment m_terminatingEnrollment;
        private PlainDate m_withdrawalMemberDate;
        private Function<AnnualSpan, PlainDate> m_spanStartDateFn = null;
        private Function<AnnualSpan, PlainDate> m_spanEndDateFn = null;

        private PlainDate m_spanEndDate = null;
        private PlainDate m_spanStartDate = null;

        /**
         * Instantiates a new annual span.
         *
         * @param enrollments List<? extends ToolEnrollment>
         * @param spanEnrollments List<? extends ToolEnrollment>
         * @param spanContext ToolSchoolYearContext
         */
        protected AnnualSpan(List<? extends ToolEnrollment> enrollments,
                List<? extends ToolEnrollment> spanEnrollments,
                ToolDistrictContext spanContext) {
            m_context = spanContext;
            m_enrollments = enrollments;
            m_spanEnrollments = spanEnrollments;

            ToolEnrollment firstEnrollment = getAllEnrollmentsAscend().get(0);
            m_school = firstEnrollment.getSchool(null);
            m_student = firstEnrollment.getStudent(null);
        }

        /**
         * Instantiates a new annual span.
         *
         * @param studentSchool ToolStudentSchool
         * @param broker X2Broker
         */
        protected AnnualSpan(ToolStudentSchool studentSchool, X2Broker broker) {
            this.m_school = studentSchool.getSchool(broker);
            this.m_student = studentSchool.getStudent(broker);
            this.m_context = studentSchool.getDistrictContext(broker);
            this.m_studentSchool = studentSchool;

            this.m_enrollments = null;
            this.m_spanEnrollments = null;
            this.m_firstActiveEnrollment = null;
            this.m_terminatingEnrollment = null;
            this.m_dataStorageEnrollment = null;
        }

        /*
         * *************************************
         * PUBLIC INTERFACE:
         * Basic span info
         * *************************************
         */

        /**
         * Gets the first active in session date. <br />
         * This the Entry Date <br />
         * - Adjusted by member on entry preference <br />
         * - Advanced to first session date of the span context <br />
         *
         * @return Plain date
         */
        public PlainDate getFirstActiveInSessionDate() {
            return m_firstActiveInSessionDate;
        }

        /**
         * Gets the first inactive in-session date.
         *
         * This is the first in-session date after the span's last active date.
         * I.e., it's the first in-session day after the student withdraws.
         *
         * If there are no more in-session days in the calendar,
         * this will return the span's last active date + 1.
         *
         * @return Plain date
         */
        public PlainDate getFirstInactiveInSessionDate() {
            return m_firstInactiveInSessionDate;
        }

        /**
         * Gets the last active in session date.
         *
         * Case W record exists inside the span context:
         * This is the Withdrawal Date
         * - Adjusted by member on withdrawal preference
         * - Advanced to last session date on/before adjusted date
         *
         * Case no W record inside the span context:
         * This is the last session date for the school calendar
         *
         * @return Plain date
         */
        public PlainDate getLastActiveInSessionDate() {
            return m_lastActiveInSessionDate;
        }

        /**
         * Gets the school.
         *
         * @return Sis school
         */
        public ToolSchool getSchool() {
            return m_school;
        }

        /**
         * Gets the student.
         *
         * @return std
         */
        /*
         * @return Sis student
         */
        public ToolStudent getStudent() {
            return m_student;
        }

        /**
         * Checks if is secondary.
         *
         * @return true, if is secondary
         */
        public boolean isSecondary() {
            return m_studentSchool != null;
        }

        /*
         * *********************************************
         * PUBLIC INTERFACE:
         * Supporting detail and raw underlying records
         * *********************************************.
         */

        /**
         * Examples of spans
         *
         * Same year: E --- W
         * Normal across years: (Y1) E --- (Y2) --- W
         *
         * All StudentEnrollment records from original entry enrollment thru terminatingEnrollment
         * including non-span records such as PASI P records and Onsis FTE S records.
         *
         * ASCENDING date-timestamp order.
         *
         * Note that in the case of spans broken by an S record:
         * 1. In the first span, the S record will be returned here as the last record
         * (and also returned by getTerminatingEnrollment).
         *
         * 2. In the second span, the S record will be returned here as the first record
         * (and also returned by getFirstActiveEnrollment).
         *
         * @return List
         */
        public List<? extends ToolEnrollment> getAllEnrollmentsAscend() {
            return m_enrollments;
        }

        /**
         * All StudentEnrollment records from original entry enrollment thru terminatingEnrollment
         * including non-span records such as PASI P records and Onsis FTE S records.
         *
         * DESCENDING date-timestamp order.
         *
         * @return List
         */
        public List<? extends ToolEnrollment> getAllEnrollmentsDescend() {
            if (m_allEnrollmentsDesc == null) {
                List<? extends ToolEnrollment> descendingEnrollments = getAllEnrollmentsAscend();
                if (descendingEnrollments == null || descendingEnrollments.isEmpty()) {
                    m_allEnrollmentsDesc = Collections.EMPTY_LIST;
                } else {
                    m_allEnrollmentsDesc = reverse(descendingEnrollments.stream()).collect(Collectors.toList());
                }
            }
            return m_allEnrollmentsDesc;
        }

        /**
         * Gets the context.
         *
         * @return District school year context
         */
        public ToolDistrictContext getContext() {
            return m_context;
        }

        /**
         * Get Data Storage record.
         *
         * This is the record where Student values are copied to
         * during the withdrawal process. It should be used
         * to look up historical span values such as Calendar ID.
         *
         * At this time this is always the W record
         * even when a span has been terminated by a break on S record
         * years earlier.
         *
         * @return Student enrollment
         */
        public ToolEnrollment getDataStorageEnrollment() {
            if (m_dataStorageEnrollment == null) {
                m_dataStorageEnrollment = findFirst(StudentEnrollment.WITHDRAWAL);
            }
            return m_dataStorageEnrollment;
        }

        /**
         * Gets the first date after last active in session date.
         *
         * @return Plain date
         */
        public PlainDate getDateAfterLastActiveInSessionDate() {
            return m_dateAfterLastActiveInSessionDate;
        }

        /**
         * All StudentEnrollment records that fall within this span.
         * Unlike getEnrollments(), getSpanEnrollments will not return records from prior years
         * e.g. the original Entry record from last year.
         *
         * This includes any Pre-Reg E/S record before the first active enrollment.
         * This includes the Withdrawal record if the W record terminates this span.
         *
         * ASCENDING date-timestamp order.
         *
         * @return List
         */
        public List<? extends ToolEnrollment> getEnrollmentsInsideSpanAscend() {
            return m_spanEnrollments;
        }

        /**
         * First Active record. Could be E or S.
         *
         * @return Student enrollment
         */
        public ToolEnrollment getFirstActiveEnrollment() {
            return m_firstActiveEnrollment;
        }

        /**
         * Gets the parent span.
         *
         * @return Parent span
         */
        public ParentSpan getParentSpan() {
            return m_parentSpan;
        }

        /**
         * Gets the recent enrollment.
         *
         * @return Student enrollment
         */
        public ToolEnrollment getRecentEnrollment() {
            return getRecentEnrollment(null);
        }

        /**
         * Gets the recent enrollment.
         *
         * @param types List<String>
         * @return Student enrollment
         */
        public ToolEnrollment getRecentEnrollment(List<String> types) {
            return getRecentEnrollment(getAllEnrollmentsDescend(), types, null);
        }

        /**
         * Gets the query as of date.
         *
         * @return Plain date
         */
        public PlainDate getQueryAsOfDate() {
            return m_queryAsOfDate;
        }

        /**
         * Gets the recent enrollment.
         *
         * @param descendingEnrollments Collection<? extends ToolEnrollment>
         * @param recordTypes Collection<String>
         * @param includeCallback Predicate<? extends ToolEnrollment>
         * @return enr
         */
        public ToolEnrollment getRecentEnrollment(Collection<? extends ToolEnrollment> descendingEnrollments,
                                                  Collection<String> recordTypes,
                                                  Predicate<ToolEnrollment> includeCallback) {
            Optional<? extends ToolEnrollment> recent = (descendingEnrollments == null)
                    ? null
                    : descendingEnrollments.stream().filter(enr -> {
                        if (m_queryAsOfDate != null && enr.getEnrollmentDate().after(m_queryAsOfDate)) {
                            return false;
                        }
                        // First filter by record type
                        if (recordTypes != null && !recordTypes.contains(enr.getEnrollmentType())) {
                            return false;
                        }

                        // optional additional caller filter
                        if (includeCallback != null && !includeCallback.test(enr)) {
                            return false;
                        }
                        return true;
                    }).findFirst();
            return recent.isPresent() ? recent.get() : null;
        }

        /**
         * Gets the recent enrollment E.
         *
         * @return Student enrollment
         */
        public ToolEnrollment getRecentEnrollmentE() {
            return getRecentEnrollment(Arrays.asList(StudentEnrollment.ENTRY));
        }

        /**
         * Gets the recent enrollment ES.
         *
         * @return Student enrollment
         */
        public ToolEnrollment getRecentEnrollmentES() {
            return getRecentEnrollment(Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.STATUS_CHANGE));
        }

        public ToolEnrollment getRecentEnrollmentESY() {
            return getRecentEnrollment(Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.STATUS_CHANGE,
                    StudentEnrollment.YOG_CHANGE));
        }

        /**
         * Gets the recent enrollment EW.
         *
         * @return Student enrollment
         */
        public ToolEnrollment getRecentEnrollmentEW() {
            return getRecentEnrollment(Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.WITHDRAWAL));
        }

        /**
         * Gets the recent enrollment W.
         *
         * @return Student enrollment
         */
        public ToolEnrollment getRecentEnrollmentW() {
            return getRecentEnrollment(Arrays.asList(StudentEnrollment.WITHDRAWAL));
        }

        /**
         * Gets the school calendar.
         *
         * @return School calendar
         */
        public ToolSchoolCalendar getSchoolCalendar() {
            return m_schoolCalendar;
        }

        /**
         * Gets the secondary.
         *
         * @return Student school
         */
        public ToolStudentSchool getSecondary() {
            return m_studentSchool;
        }

        /*
         * *********************************************
         * INTERNAL SUPPORT METHODS
         * *********************************************.
         */

        /**
         * Get the record that ends the span.
         * This is normally the W record.
         *
         * Note that in the case of spans broken by an S record:
         * 1. The S record will be returned by getTerminatingEnrollment for the first span.
         * 2. The S record will be returned by getFirstActiveEnrollment for the second span.
         *
         * In the case of queryAsOfDate:
         * If the LastActiveDate <= queryAsOfDate,
         * this WILL return the Withdrawal record,
         * even if the W record date > queryAsOfDate.
         *
         * @return Student enrollment
         */
        public ToolEnrollment getTerminatingEnrollment() {
            return m_terminatingEnrollment;
        }

        /**
         * The Withdrawal date for this span, if it exists,
         * adjusted by memberOnWithdrawal.
         *
         * Not advanced to in-session date.
         *
         * @return Plain date
         */
        public PlainDate getWithdrawalMemberDate() {
            return m_withdrawalMemberDate;
        }

        /**
         * The WithdrawalMember date, if it exists,
         * else the last active date, if it exists.
         *
         * @return Plain date
         */
        public PlainDate getWithdrawalMemberOrLastActiveDate() {
            return (getWithdrawalMemberDate() != null && (getLastActiveInSessionDate() != null
                    && getWithdrawalMemberDate().before(getLastActiveInSessionDate())))
                            ? getWithdrawalMemberDate()
                            : getLastActiveInSessionDate();
        }

        /**
         * Checks if is entry.
         *
         * @return true, if is entry
         */
        public boolean isEntry() {
            return findFirstInSpan(StudentEnrollment.ENTRY) != null;
        }

        /**
         * Is this span terminated by a Withdrawal?
         * (false if the WithdrawalMember date falls after queryAsOfDate).
         *
         * @return true, if is withdrawal
         */
        public boolean isWithdrawal() {
            return m_terminatingEnrollment != null
                    && StudentEnrollment.WITHDRAWAL.equals(m_terminatingEnrollment.getEnrollmentType());
        }



        /*
         * *********************************************
         * INTERNAL SUPPORT METHODS
         * *********************************************.
         */

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            String schoolYearStr = getContext() == null ? "<null context>" : "" + getContext().getSchoolYear();
            String studentName = getStudent() == null ? "<null student>" : getStudent().getNameView();
            String stateId = getStudent() == null ? "" : getStudent().getStateId();
            String schoolName = getSchool() == null ? "<null school>" : getSchool().getName();


            return schoolYearStr + ": [" + studentName + "] [" + stateId
                    + "] " + schoolName
                    + " [" + m_spanStartDate + " - " + m_spanEndDate + "]"
                    + (isSecondary() ? " CONCURRENT" : "");
        }

        /**
         * Find first Enrollment record of this type
         * from original Entry through end of this span.
         *
         * @param type String
         * @return ToolEnrollment
         */
        protected ToolEnrollment findFirst(String type) {
            List<? extends ToolEnrollment> allEnrollmentsAscend = getAllEnrollmentsAscend();
            if (allEnrollmentsAscend == null) {
                return null;
            }

            Optional<? extends ToolEnrollment> findFirst =
                    allEnrollmentsAscend.stream().filter(enr -> type.equals(enr.getEnrollmentType())).findFirst();

            return findFirst.isPresent() ? findFirst.get() : null;
        }

        /**
         * Find first Enrollment record of this type
         * within the date range of this span.
         *
         * @param type String
         * @return ToolEnrollment
         */
        protected ToolEnrollment findFirstInSpan(String type) {
            Optional<? extends ToolEnrollment> findFirst =
                    getEnrollmentsInsideSpanAscend().stream().filter(enr -> type.equals(enr.getEnrollmentType()))
                            .findFirst();

            return findFirst.isPresent() ? findFirst.get() : null;
        }

        /**
         * Allow span builder to store future W record in the span object
         * separate from m_enrollments
         * so that values stored on the W can be accessed for the span.
         *
         * @param dataStorageEnrollment void
         */
        protected void setDataStorageEnrollment(ToolEnrollment dataStorageEnrollment) {
            m_dataStorageEnrollment = dataStorageEnrollment;
        }

        /**
         * Sets the date after last active date.
         *
         * @param date PlainDate
         */
        protected void setDateAfterLastActiveInSessionDate(PlainDate date) {
            m_dateAfterLastActiveInSessionDate = date;
        }

        /**
         * Sets the first active enrollment.
         *
         * @param enr void
         */
        protected void setFirstActiveEnrollment(ToolEnrollment enr) {
            m_firstActiveEnrollment = enr;
        }

        /**
         * Sets the first active in session date.
         *
         * @param firstActiveInSessionDate void
         */
        protected void setFirstActiveInSessionDate(PlainDate firstActiveInSessionDate) {
            m_firstActiveInSessionDate = firstActiveInSessionDate;
        }

        /**
         * Sets the first inactive in session date.
         *
         * @param firstInactiveInSessionDate void
         */
        protected void setFirstInactiveInSessionDate(PlainDate firstInactiveInSessionDate) {
            m_firstInactiveInSessionDate = firstInactiveInSessionDate;
        }

        /**
         * Sets the last active in session date.
         *
         * @param date void
         */
        protected void setLastActiveInSessionDate(PlainDate date) {
            m_lastActiveInSessionDate = date;
        }

        /**
         * Sets the parent span.
         *
         * @param parentSpan void
         */
        protected void setParentSpan(ParentSpan parentSpan) {
            this.m_parentSpan = parentSpan;
        }

        /**
         * Sets the school calendar.
         *
         * @param schoolCalendar void
         */
        protected void setSchoolCalendar(ToolSchoolCalendar schoolCalendar) {
            m_schoolCalendar = schoolCalendar;
        }

        /**
         * Sets the terminating enrollment.
         *
         * @param terminatingEnrollment void
         */
        protected void setTerminatingEnrollment(ToolEnrollment terminatingEnrollment) {
            this.m_terminatingEnrollment = terminatingEnrollment;
        }

        /**
         * Sets the withdrawal member date.
         *
         * @param withdrawalMemberDate void
         */
        protected void setWithdrawalMemberDate(PlainDate withdrawalMemberDate) {
            this.m_withdrawalMemberDate = withdrawalMemberDate;
        }

        /**
         * Gets the span end date.
         *
         * @return Plain date
         */
        public PlainDate getSpanEndDate() {
            if (m_spanEndDate == null) {
                m_spanEndDate = m_spanEndDateFn.apply(this);
            }
            return m_spanEndDate;
        }

        /**
         * Gets the span start date.
         *
         * @return Plain date
         */
        public PlainDate getSpanStartDate() {
            if (m_spanStartDate == null) {
                m_spanStartDate = m_spanStartDateFn.apply(this);
            }
            return m_spanStartDate;
        }

        /**
         * Sets the query as of date.
         *
         * @param date void
         */
        public void setQueryAsOfDate(PlainDate date) {
            m_queryAsOfDate = date;
        }

        /**
         * Sets the span end date fn.
         *
         * @param spanEndDateFn Function<AnnualSpan,PlainDate>
         */
        public void setSpanEndDateFn(Function<AnnualSpan, PlainDate> spanEndDateFn) {
            m_spanEndDateFn = spanEndDateFn;
        }

        /**
         * Sets the span start date fn.
         *
         * @param spanStartDateFn Function<AnnualSpan,PlainDate>
         */
        public void setSpanStartDateFn(Function<AnnualSpan, PlainDate> spanStartDateFn) {
            m_spanStartDateFn = spanStartDateFn;
        }

        /**
         * Gets the date range.
         *
         * @return Range
         */
        public Range<Date> getDateRange() {
            if (m_dateRange == null) {
                try {
                    m_dateRange = Range.of(getSpanStartDate(), getSpanEndDate());
                } catch (Exception e) {
                    String msg = "Annual Span: " + toString();
                    throw (new RuntimeException(msg, e));
                }
            }
            return m_dateRange;
        }

        /**
         * Gets the date range safe. This eliminates exceptions resulting from poor date range data
         * by returning a future date range unlikely to match with anything
         *
         * @return the date range safe
         */
        public Range<Date> getDateRangeSafe() {
            try {
                return getDateRange();
            } catch (Exception e) {
                return JAN_1_2200_Range;
            }
        }

        /**
         * Compare to.
         *
         * @param span2 Object
         * @return int
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(Object span2) {
            if (!(span2 instanceof AnnualSpan)) {
                return 0;
            }
            PlainDate a = getFirstActiveInSessionDate();
            PlainDate b = ((AnnualSpan) span2).getFirstActiveInSessionDate();
            if (a == null && b == null) {
                return 0;
            }

            if (a == null && b != null) {
                return -1;
            }

            if (a != null && b == null) {
                return 1;
            }

            return a.compareTo(b);
        }

    }

    /**
     * A factory for creating AnnualSpan objects.
     */
    public static class AnnualSpanFactory {
        /*
         * The business layer (caller) must set
         * the definition of "span start date" and "start end date"
         */
        private Function<AnnualSpan, PlainDate> m_annualSpanStartDateFn = new Function<AnnualSpan, PlainDate>() {
            @Override
            public PlainDate apply(AnnualSpan t) {
                return t.getFirstActiveInSessionDate();
            }
        };

        private Predicate<ToolEnrollment> m_annualSpanIgnoreEnrollmentFn = new Predicate<ToolEnrollment>() {

            @Override
            public boolean test(ToolEnrollment t) {
                return false;
            }

        };

        private Function<AnnualSpan, PlainDate> m_annualSpanEndDateFn = new Function<AnnualSpan, PlainDate>() {
            @Override
            public PlainDate apply(AnnualSpan t) {
                return t.getWithdrawalMemberOrLastActiveDate();
            }
        };

        /**
         * Gets the annual span end date fn.
         *
         * @return Function
         */
        public Function<AnnualSpan, PlainDate> getAnnualSpanEndDateFn() {
            return m_annualSpanEndDateFn;
        }

        /**
         * Gets the annual span ignore enrollment fn.
         *
         * @return Predicate
         */
        public Predicate<ToolEnrollment> getAnnualSpanIgnoreEnrollmentFn() {
            return m_annualSpanIgnoreEnrollmentFn;
        }

        /**
         * Gets the annual span start date fn.
         *
         * @return Function
         */
        public Function<AnnualSpan, PlainDate> getAnnualSpanStartDateFn() {
            return m_annualSpanStartDateFn;
        }

        /**
         * Instantiate annual span.
         *
         * @param broker X2Broker
         * @param ssk ToolStudentSchool
         * @return AnnualSpan
         */
        public AnnualSpan instantiateAnnualSpan(X2Broker broker, ToolStudentSchool ssk) {
            AnnualSpan span = new AnnualSpan(ssk, broker);
            initialize(span);
            return span;
        }

        /**
         * Instantiate annual span.
         *
         * @param enrollmentsUpToYear List<? extends ToolEnrollment>
         * @param enrollmentsInThisYear List<? extends ToolEnrollment>
         * @param spanContext ToolDistrictContext
         * @return AnnualSpan
         */
        public AnnualSpan instantiateAnnualSpan(List<? extends ToolEnrollment> enrollmentsUpToYear,
                                                List<? extends ToolEnrollment> enrollmentsInThisYear,
                                                ToolDistrictContext spanContext) {
            AnnualSpan span = new AnnualSpan(enrollmentsUpToYear, enrollmentsInThisYear, spanContext);
            initialize(span);
            return span;
        }

        /**
         * Gets the annual span end date fn.
         *
         * @param fn Function<AnnualSpan,PlainDate>
         * @return Function
         */
        public AnnualSpanFactory setAnnualSpanEndDateFn(Function<AnnualSpan, PlainDate> fn) {
            m_annualSpanEndDateFn = fn;
            return this;
        }

        /**
         * Sets the annual span ignore enrollment fn.
         *
         * @param fn void
         * @return AnnualSpanFactory
         */
        public AnnualSpanFactory setAnnualSpanIgnoreEnrollmentFn(Predicate<ToolEnrollment> fn) {
            m_annualSpanIgnoreEnrollmentFn = fn;
            return this;
        }

        /**
         * Sets the annual span start date fn.
         *
         * @param fn Function<AnnualSpan,PlainDate>
         * @return AnnualSpanFactory
         */
        public AnnualSpanFactory setAnnualSpanStartDateFn(Function<AnnualSpan, PlainDate> fn) {
            m_annualSpanStartDateFn = fn;
            return this;
        }

        /**
         * Initialize.
         *
         * @param span AnnualSpan
         */
        protected void initialize(AnnualSpan span) {
            span.setSpanEndDateFn(getAnnualSpanEndDateFn());
            span.setSpanStartDateFn(getAnnualSpanStartDateFn());
        }

    }

    /**
     * Temporary object used when splitting spans.
     *
     * This is a multi-year span potentially split on S/Y records,
     * similar to those returned by StudentHistoryHelper.
     *
     * @author Follett Software Company
     * @copyright 2020
     */
    public static class AspenSpan implements Comparable {
        /*
         * Multi-year list of enrollments backing this span.
         *
         * Unlike StudentHistoryHelper, this can end with a W record
         * but will NOT include the terminatingEnrollment
         * when the terminatingEnrollment starts the next span (e.g. S/Y/E).
         */
        private List<ToolEnrollment> ascendingEnrollments;

        /*
         * The W record has to be accessible for its data content
         * to every fractional span when breaking spans on S or Y
         */
        private ToolEnrollment dataStorageEnrollment = null;

        private ToolStudentSchool studentSchool;

        /*
         * The StudentEnrollment record that causes this span to terminate.
         * Could actually be the starting record for the following span.
         *
         * This can be any of:
         * - A W record withdrawing this span
         *
         * - An S record with inactive status
         *
         * - An S record that starts a new span
         * (i.e. S record with Active status if statusBreak==true
         *
         * - A Y record that starts a new span
         * (i.e. SY record with Active status if yogBreak==true
         *
         * - An E record for a new school
         */
        private ToolEnrollment terminatingEnrollment = null;

        /**
         * Instantiates a new aspen span.
         *
         * @param enrollments List<? extends ToolEnrollment>
         */
        public AspenSpan(List<ToolEnrollment> enrollments) {
            this.ascendingEnrollments = enrollments;
        }

        /**
         * Instantiates a new aspen span.
         *
         * @param enrollments List<? extends ToolEnrollment>
         * @param terminatingEnrollment ToolEnrollment
         * @param dataStorageEnrollment ToolEnrollment
         */
        public AspenSpan(List<ToolEnrollment> enrollments, ToolEnrollment terminatingEnrollment,
                ToolEnrollment dataStorageEnrollment) {
            this(enrollments);
            this.terminatingEnrollment = terminatingEnrollment;
            this.dataStorageEnrollment = dataStorageEnrollment;
        }

        /**
         * Compare to.
         *
         * @param span2obj Object
         * @return int
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(Object span2obj) {
            AspenSpan span2 = (AspenSpan) span2obj;

            boolean nullIsOld = true;
            PlainDate myStartDate = (getAscendingEnrollments().isEmpty())
                    ? null
                    : getAscendingEnrollments().get(0).getEnrollmentDate();
            PlainDate span2StartDate = (span2.getAscendingEnrollments().isEmpty())
                    ? null
                    : span2.getAscendingEnrollments().get(0).getEnrollmentDate();
            int compareTo = compareDates(myStartDate, span2StartDate, nullIsOld);

            if (compareTo == 0) {
                nullIsOld = false;
                PlainDate myEndDate = (getAscendingEnrollments().isEmpty())
                        ? null
                        : getAscendingEnrollments().get(getAscendingEnrollments().size() - 1).getEnrollmentDate();
                PlainDate span2EndDate = (span2.getAscendingEnrollments().isEmpty())
                        ? null
                        : span2.getAscendingEnrollments().get(span2.getAscendingEnrollments().size() - 1)
                                .getEnrollmentDate();
                compareTo = compareDates(myEndDate, span2EndDate, nullIsOld);
            }
            return compareTo;
        }

        /**
         * Adds the ascending enrollment.
         *
         * @param enrollment ToolEnrollment
         */
        public void addAscendingEnrollment(ToolEnrollment enrollment) {
            ascendingEnrollments.add(0, enrollment);
        }

        /**
         * Gets the ascending enrollments.
         *
         * @return the enrollments
         */
        public List<? extends ToolEnrollment> getAscendingEnrollments() {
            return ascendingEnrollments;
        }

        /**
         * Gets the data storage enrollment.
         *
         * @return the dataStorageEnrollment
         */
        public ToolEnrollment getDataStorageEnrollment() {
            return dataStorageEnrollment;
        }

        /**
         * Gets the secondary.
         *
         * @return ssk
         */
        public ToolStudentSchool getSecondary() {
            return studentSchool;
        }

        /**
         * Gets the student.
         *
         * @param broker X2Broker
         * @return std
         */
        public ToolStudent getStudent(X2Broker broker) {
            if (!ascendingEnrollments.isEmpty()) {
                return ascendingEnrollments.get(0).getStudent(broker);
            }
            if (getSecondary() != null) {
                return getSecondary().getStudent(broker);
            }
            return null;
        }

        /**
         * Gets the school.
         *
         * @param broker X2Broker
         * @return skl
         */
        public ToolSchool getSchool(X2Broker broker) {
            if (!ascendingEnrollments.isEmpty()) {
                return ascendingEnrollments.get(0).getSchool(broker);
            }
            if (getSecondary() != null) {
                return getSecondary().getSchool(broker);
            }
            return null;
        }

        /**
         * Gets the terminating enrollment.
         *
         * @return the terminatingEnrollment
         */
        public ToolEnrollment getTerminatingEnrollment() {
            return terminatingEnrollment;
        }

        /**
         * Checks if is secondary.
         *
         * @return true, if is secondary
         */
        public boolean isSecondary() {
            return studentSchool != null;
        }

        /**
         * Sets the data storage enrollment.
         *
         * @param dataStorageEnrollment void
         */
        public void setDataStorageEnrollment(ToolEnrollment dataStorageEnrollment) {
            this.dataStorageEnrollment = dataStorageEnrollment;
        }

        /**
         * Sets the enrollments.
         *
         * @param enrollments void
         */
        public void setEnrollments(List<ToolEnrollment> enrollments) {
            this.ascendingEnrollments = enrollments;
        }

        /**
         * Sets the secondary.
         *
         * @param studentSchool void
         */
        public void setSecondary(ToolStudentSchool studentSchool) {
            this.studentSchool = studentSchool;
        }

        /**
         * Sets the terminating enrollment.
         *
         * @param terminatingEnrollment void
         */
        public void setTerminatingEnrollment(ToolEnrollment terminatingEnrollment) {
            this.terminatingEnrollment = terminatingEnrollment;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            ToolEnrollment firstEnrollment = null;
            if (ascendingEnrollments != null && ascendingEnrollments.size() > 0) {
                firstEnrollment = ascendingEnrollments.get(0);
            }

            String schoolName = "";
            if (firstEnrollment != null && firstEnrollment.getSchool(null) != null) {
                schoolName = firstEnrollment.getSchool(null).getName();
            } else if (getSecondary() != null && getSecondary().getSchool(null) != null) {
                schoolName = getSecondary().getSchool(null).getName();
            }

            ToolStudent student = null;
            if (firstEnrollment != null) {
                student = firstEnrollment.getStudent(null);
            } else if (getSecondary() != null) {
                student = getSecondary().getStudent(null);
            }
            String studentName = student == null ? "" : student.getNameView();

            PlainDate startDateForPrint = null;
            if (firstEnrollment != null) {
                startDateForPrint = firstEnrollment.getEnrollmentDate();
            } else if (getSecondary() != null) {
                startDateForPrint = getSecondary().getStartDate();
            }

            return schoolName + " " + studentName + " " + startDateForPrint;
        }
    }

    /**
     * A manager class for state reporting and custom export functionality.
     *
     * @author X2 Development Corporation
     */
    public static class ExportFormatManager {
        /**
         * The left square bracket ([) prefix character for identifying aliases in bean paths.
         * An Alias must be enclosed in square brackets.
         * <br>
         * [<i>alias</i>]
         */
        public static final String ALIAS_PREFIX_CHAR = "[";

        /**
         * The right square bracket (]) postfix character for identifying aliases in bean paths.
         * An Alias must be enclosed in square brackets.
         * <br>
         * [<i>alias</i>]
         */
        public static final String ALIAS_SUFFIX_CHAR = "]";

        public static final String MESSAGE_DEFINITON = "message.customexp.definition";

        private static final String ERROR_ALIAS_INVALID = "error.custexp.aliasInvalid";
        private static final String ERROR_END_OF_PATH1 = "error.custexp.endOfPath1";
        private static final String ERROR_END_OF_PATH2 = "error.custexp.endOfPath2";
        private static final String ERROR_NO_ALIAS = "error.custexp.noAlias";
        private static final String ERROR_NO_FIELD = "error.custexp.noField";
        private static final String ERROR_NO_TABLE = "error.custexp.noTable";
        private static final String ERROR_RELATION_INVALID = "error.custexp.relationInvalid";
        private static final String ERROR_EXT_FIELD_MISSING = "error.custexp.extField.missing";

        private static final char PAD_CHAR = ' ';

        /**
         * The logical formatter should allow input of a representation pattern in its constructor
         * and allow multiple types of input in its format method.
         * <p>
         * The constructor should take a two character string.
         * <p>
         * Ex: "YN"
         * <p>
         * or a string in two parts separated by a comma ",".
         * <p>
         * Ex: "Yes,No"
         * <p>
         * The first character or part before the colon represents the output value if the format
         * value
         * is "true".
         * <br>
         * The second character or part after the colon represents the output value if the format
         * value
         * is "false".
         * <p>
         * The format method should allow Boolean, String or numeric input types.
         * <br>
         * Booleans will be interpreted directly as "true" or "false".
         * <br>
         * String values will be interpreted:
         * <ul>
         * <li>starts with "Y","T", any string representation of a number other than zero ( != 0) =
         * "true"</li>
         * <li>starts with "N", "F", a string representation of zero (0) = "false"</li>
         * <li>otherwise "false"</li>
         * </ul>
         * <br>
         * numeric values will be interpreted:
         * <ul>
         * <li>Any numeric value of zero = "false"
         * <li>Any numeric value other than zero = "true"
         * </ul>
         * <br>
         * other values will be interpreted:
         * <ul>
         * <li>null value = "false"
         * <li>any other type = "false"
         * </ul>
         *
         * @author X2 Development Corporation
         */
        public static class X2LogicalFormat extends Format {
            private final String SEPARATOR_CHAR = ",";
            /**
             *
             */
            private static final long serialVersionUID = 1L;
            private String m_pattern = null;
            private String m_truePattern = null;
            private String m_falsePattern = null;

            /**
             * Constructor accepts either:
             * A two character input string pattern (YN)
             * A string in two parts separated by a colon (Yes,No)
             * The first character or part is the return value for a true indicator.
             * The second character or part is the return value for a false indicator.
             *
             * @param pattern String
             */
            public X2LogicalFormat(String pattern) {
                m_pattern = pattern;
                if (pattern != null) {
                    int idx = pattern.indexOf(SEPARATOR_CHAR);
                    if (idx > 0) {
                        m_truePattern = m_pattern.substring(0, idx);
                        m_falsePattern = m_pattern.substring(idx + 1);
                    } else if (pattern.length() > 1) {
                        m_truePattern = m_pattern.substring(0, 1);
                        m_falsePattern = m_pattern.substring(1, 2);
                    }
                }
            }

            /**
             * Format the source object base on the rules of the formatter.
             * The format method should allow Boolean, String or numeric input types.
             * <br>
             * Booleans will be interpreted directly as "true" or "false".
             * <br>
             * String values will be interpreted:
             * <ul>
             * <li>starts with "Y","T", any string representation of a number other than zero ( !=
             * 0) =
             * "true"</li>
             * <li>starts with "N", "F", a string representation of zero (0) = "false"</li>
             * <li>otherwise "false"</li>
             * </ul>
             * <br>
             * numeric values will be interpreted:
             * <ul>
             * <li>Any numeric value of zero = "false"
             * <li>Any numeric value other than zero = "true"
             * </ul>
             * <br>
             * other values will be interpreted:
             * <ul>
             * <li>null value = "false"
             * <li>any other type = "false"
             * </ul>
             *
             * @param parseValue Object
             * @param toAppendTo StringBuffer
             * @param pos FieldPosition
             * @return StringBuffer
             * @see java.text.Format#format(java.lang.Object, java.lang.StringBuffer,
             *      java.text.FieldPosition)
             */
            @Override
            public StringBuffer format(Object parseValue, StringBuffer toAppendTo, FieldPosition pos) {
                boolean boolValue = false;
                // Check boolean type.
                if (parseValue instanceof Boolean) {
                    boolValue = ((Boolean) parseValue).booleanValue();
                }
                // Check string type.
                else if (parseValue instanceof String) {
                    String parseString = ((String) parseValue).toUpperCase();
                    // Y, N, T, F
                    if (parseString.startsWith("Y") ||
                            parseString.startsWith("T")) {
                        boolValue = true;
                    }
                    // Numeric as string.
                    else if (parseString.matches("[0-9]+")) {
                        long numValue = Long.parseLong(parseString);
                        if (numValue != 0) {
                            boolValue = true;
                        }
                    } else if (parseString.matches("[0-9]*.[0-9]*") && parseString.length() > 1) {
                        // Don't want "."
                        double numValue = Double.parseDouble(parseString);
                        if (numValue != 0.0) {
                            boolValue = true;
                        }
                    }
                }
                // Check numeric type.
                else if (parseValue instanceof Number) {
                    Number numValue = (Number) parseValue;
                    if (numValue.doubleValue() != 0.0) {
                        boolValue = true;
                    }
                }

                // Generate the output character based on the boolean value.
                String result = null;
                if (boolValue) {
                    result = m_truePattern;
                } else {
                    result = m_falsePattern;
                }

                // return the result string in the string buffer.
                return toAppendTo.append(result);
            }

            /**
             * Do not need a parse function for this implementation.
             *
             * @param source String
             * @param pos ParsePosition
             * @return Object
             * @see java.text.Format#parseObject(java.lang.String, java.text.ParsePosition)
             */
            @Override
            public Object parseObject(String source, ParsePosition pos) {
                return null;
            }
        }

        /**
         * This method translates and validates one bean path, with optional alias,
         * into a true bean path.
         * If the bean path is invalid, or the alias is not found, error messages are returned
         * in the errors collection.
         *
         * @param table DataTable
         * @param beanPath String
         * @param dictionary DataDictionary
         * @param strictAlias boolean
         * @param errors Collection<StateReportValidationError>
         * @param errText1 String
         * @param errText2 String
         * @return String
         */
        public static String decodeBeanPath(DataTable table,
                                            String beanPath,
                                            DataDictionary dictionary,
                                            boolean strictAlias,
                                            Collection<StateReportValidationError> errors,
                                            String errText1,
                                            String errText2) {
            StateReportValidationError error = null;
            MessageResources resources = LocalizationCache.getMessages(dictionary.getPersistenceKey());
            boolean last = false;
            StringBuilder resultPath = new StringBuilder();

            if (table != null) {
                if (!StringUtils.isEmpty(beanPath)) {
                    List<String> pathElements =
                            StringUtils.convertDelimitedStringToList(beanPath, ModelProperty.PATH_DELIMITER, true);
                    for (String element : pathElements) {
                        if (last) {
                            // Elements past end of path. The previous element was a field.
                            String errField = resources.getMessage(ERROR_END_OF_PATH1);

                            error = new StateReportValidationError(errText1, errText2, errField, element);
                            errors.add(error);
                            break;
                        } else if (element.startsWith(ALIAS_PREFIX_CHAR) && element.endsWith(ALIAS_SUFFIX_CHAR)) {
                            // Alias lookup.
                            String alias = element.substring(1, element.length() - 1);
                            DataDictionaryField dataField = dictionary.findDataDictionaryFieldByAlias(alias);
                            if (strictAlias && dataField == null) {
                                // no alias with this name.
                                String errAlias = resources.getMessage(ERROR_NO_ALIAS);

                                error = new StateReportValidationError(errText1, errText2, errAlias, element);
                                break;
                            } else if (dataField != null && !dataField.getDataTable().equals(table)) {
                                // field not on this table.
                                String errAlias = resources.getMessage(ERROR_ALIAS_INVALID);

                                error = new StateReportValidationError(errText1, errText2, errAlias, element);
                                break;
                            } else if (dataField != null) {
                                // Field element is valid (or aliases not strict). Add it and mark
                                // element as last.
                                if (resultPath.length() > 0) {
                                    resultPath.append(ModelProperty.PATH_DELIMITER);
                                }
                                resultPath.append(dataField.getJavaName());
                                last = true;
                            } else {
                                last = true;
                            }
                        } else {
                            boolean found = false;
                            List<DataDictionaryField> dFields = dictionary.getFieldsForContext(table);
                            for (DataDictionaryField dField : dFields) {
                                if (dField.getJavaName().equals(element)) {
                                    // Field element is valid. Add it and mark element as last.
                                    if (resultPath.length() > 0) {
                                        resultPath.append(ModelProperty.PATH_DELIMITER);
                                    }
                                    resultPath.append(element);
                                    last = true;
                                    found = true;
                                    break;
                                }
                            }
                            if (!found) {
                                List<DataDictionaryRelationship> dRelations =
                                        dictionary.getRelationshipsForContext(table);
                                for (DataDictionaryRelationship ddRelation : dRelations) {
                                    if (ddRelation.getRelatedJavaName().equals(element)) {

                                        if (!ddRelation.getRelatedRelationType()
                                                .equals(DataDictionaryRelationship.ONE_TYPE_CODE)) {
                                            // Relationship element path not n to 1 or 1 to 1.
                                            String errRelation = resources.getMessage(ERROR_RELATION_INVALID);

                                            error = new StateReportValidationError(errText1, errText2, errRelation,
                                                    element);
                                        } else {
                                            // relationship Ok. continue path. set next table in
                                            // path.
                                            if (resultPath.length() > 0) {
                                                resultPath.append(ModelProperty.PATH_DELIMITER);
                                            }
                                            resultPath.append(element);
                                            table = ddRelation.getRelatedDataTable();
                                            found = true;
                                        }
                                        break;
                                    }
                                }
                            }
                            if (!found && error == null) {
                                String errRelation = resources.getMessage(ERROR_NO_FIELD);

                                error = new StateReportValidationError(errText1, errText2, errRelation, element);
                                break;
                            }
                        }
                    }
                    // Check the path ends in a field, not a relation.
                    if (!last && error == null) {
                        String errRelation = resources.getMessage(ERROR_END_OF_PATH2);

                        error = new StateReportValidationError(errText1, errText2, errRelation, beanPath);
                    }
                    if (errors.size() > 0) {
                        resultPath = null;
                    }
                }
            } else {
                // No table defined in the definition.
                String msgField = resources.getMessage(MESSAGE_DEFINITON);
                String errNoTable = resources.getMessage(ERROR_NO_TABLE);

                error = new StateReportValidationError(errText1, msgField, errNoTable, null);
            }

            if (error != null && errors != null) {
                errors.add(error);
            }
            return (resultPath == null ? null : resultPath.toString());
        }

        /**
         * This method takes a field value and pads it according the the padding rules in the
         * field definition.
         *
         * @param fieldValue String
         * @param paddingDirection int
         * @param paddingChar String
         * @param paddingLength int
         * @return String
         */
        public static String doPadding(String fieldValue, int paddingDirection, String paddingChar, int paddingLength) {
            // Identify pad character
            char padChar = PAD_CHAR;
            if (!StringUtils.isEmpty(paddingChar)) {
                padChar = paddingChar.charAt(0);
            }
            // Identify pad type and direction.
            if (paddingDirection != 0) {
                if (paddingDirection == ExportFormatField.PaddingDirectionCode.PAD_LEFT.ordinal()) {
                    fieldValue = StringUtils.padLeft(
                            (fieldValue.length() > paddingLength ? fieldValue.substring(0, paddingLength) : fieldValue),
                            paddingLength, padChar);
                } else if (paddingDirection == ExportFormatField.PaddingDirectionCode.PAD_RIGHT.ordinal()) {
                    fieldValue = StringUtils.padRight(
                            (fieldValue.length() > paddingLength ? fieldValue.substring(0, paddingLength) : fieldValue),
                            paddingLength, padChar);
                } else if (paddingDirection == ExportFormatField.PaddingDirectionCode.TRUNCATE_ONLY.ordinal()) {
                    fieldValue =
                            (fieldValue.length() > paddingLength ? fieldValue.substring(0, paddingLength) : fieldValue);
                }
            }
            return fieldValue;
        }

        /**
         * This method will check an export definition and validate
         * elements of the definition, lookup aliases and present
         * validation errors for invalid conditions.
         *
         * @param definition ExportFormatDefinition
         * @param broker X2Broker
         * @param saveFields boolean
         * @return List of errors.
         */
        public static List<StateReportValidationError> validateDefinition(ExportFormatDefinition definition,
                                                                          X2Broker broker,
                                                                          boolean saveFields) {
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            // Check all fields on the definition.
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ExportFormatField.COL_DEFINITION_OID, definition.getOid());
            QueryByCriteria query = new QueryByCriteria(ExportFormatField.class, criteria);
            query.addOrderBy(ExportFormatField.COL_POSITION, true);
            QueryIterator iterator = broker.getIteratorByQuery(query);

            try {
                while (iterator.hasNext()) {
                    ExportFormatField field = (ExportFormatField) iterator.next();
                    StateReportValidationError error = validateField(field, true, true, broker);
                    // Save result path.
                    if (saveFields && error == null) {
                        broker.saveBeanForced(field);
                    } else if (error != null) {
                        errors.add(error);
                    }
                }
            } finally {
                iterator.close();
            }

            return errors;
        }

        /**
         * This method validates one field object, validating the bean path and aliases.
         *
         * @param field ExportFormatField
         * @param strictAlias boolean
         * @param findSaveField boolean
         * @param broker X2Broker
         * @return StateReportValidationError
         */
        public static StateReportValidationError validateField(ExportFormatField field,
                                                               boolean strictAlias,
                                                               boolean findSaveField,
                                                               X2Broker broker) {
            ExportFormatDefinition definition = field.getDefinition();

            DataDictionary dictionary = DataDictionary.getDistrictDictionary(definition.getExtendedDataDictionary(),
                    broker.getPersistenceKey());
            MessageResources resources = LocalizationCache.getMessages(broker.getPersistenceKey());

            String beanPath = field.getFieldPath();
            DataTable table = definition.getSourceTable().getDataTable();

            StateReportValidationError error = null;
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String resultPath =
                    decodeBeanPath(table, beanPath, dictionary, strictAlias, errors, definition.getName(),
                            field.getName());

            if (errors.size() == 0) {
                field.setValFieldPath(resultPath);
                if (findSaveField) {
                    error = findSaveField(field, resources, broker);
                    if (error != null) {
                        errors.add(error);
                    }
                }
            } else {
                field.setValFieldPath(null);
            }

            // Get the first error from the errors list.
            error = null;
            if (errors.size() > 0) {
                error = errors.iterator().next();
            }
            return error;
        }

        /**
         * This method will check the bean for a valid DataFieldConfig value.
         *
         * If none is found, it will search the list of available fields in the extended dictionary,
         * and used rows in the fields to find a new appropriate unused FDD for this field
         * definition.
         *
         * @param field ExportFormatField
         * @param resources MessageResources
         * @param broker X2Broker
         * @return StateReportValidationError
         */
        private static StateReportValidationError findSaveField(ExportFormatField field,
                                                                MessageResources resources,
                                                                X2Broker broker) {
            StateReportValidationError error = null;

            if (StringUtils.isEmpty(field.getDataFieldConfigOid())) {
                /*
                 * Find a new data field for saving this field value.
                 * Error if one is not found.
                 */
                DataFieldConfig dataField = DataDictionary.getAvailableField(ExportFormatRow.DICTIONARY_ID,
                        field.getMaximumLength(),
                        field.getDefinitionOid(),
                        broker);
                if (dataField != null) {
                    field.setDataFieldConfigOid(dataField.getOid());
                } else {
                    // No data dictionary field available.
                    String msgField = resources.getMessage(MESSAGE_DEFINITON);
                    String errNoTable = resources.getMessage(ERROR_EXT_FIELD_MISSING);

                    error = new StateReportValidationError(field.getName(), msgField, errNoTable, null);
                }
            }

            return error;
        }
    }

    /**
     * This class defines one field of the export. It defines
     * the value location, some validation constraints, and
     * some interfaces for extended functionality.
     */
    public static class FieldDefinition {
        /*
         * Constants for javascript signature creation
         */
        public static String DATA_PARAM = "data";
        public static String ENTITY_PARAM = "entity";
        public static String FIELD_PARAM = "field";

        /**
         * Field definition values.
         */
        private String m_beanPath;
        private String m_calcId;
        private SystemStringConverter m_converter;
        private String m_defaultValue;
        private int m_depth;
        private String m_effOid;
        private int m_exportLength;
        private String m_fieldId;
        private Format m_formatter;
        private boolean m_keyIndicator;
        private int m_lengthMax;
        private int m_lengthMin;
        private int m_mappedLookup;
        private String m_paddingChar;
        private Object m_parameter;
        private String m_pattern;
        private ExportFormatField.PaddingDirectionCode m_resizeMode;
        private FieldRetriever m_retriever;
        private String m_saveBeanPath;
        private String m_sifPath;
        private String m_userName;
        private String m_validatorId;
        private FieldValidator m_validator;
        private String m_scriptFunction;
        private String m_scriptFunctionName;

        /**
         * Constructs one field definition for one field in the export.
         *
         * @param formatField ExportFormatField
         * @param beanClass Class
         * @param broker X2Broker
         * @throws X2BaseException exception
         */
        public FieldDefinition(ExportFormatField formatField, Class beanClass, X2Broker broker) throws X2BaseException {
            /*
             * Calculate/Validate the bean path in the bean.
             * If there is an error, generate an initialization error.
             * If there is not an error, and the validated bean path is new/changed, save the bean.
             */
            String beanPath = formatField.getFieldPath();
            if (!StringUtils.isEmpty(beanPath)) {
                String oldValPath = formatField.getValFieldPath();
                StateReportValidationError valError =
                        ExportFormatManager.validateField(formatField, true, false, broker);
                if (valError != null) {
                    throw new X2BaseException((String) null, valError.getErrorMessage());
                }

                if (!StringUtils.isEmpty(formatField.getValFieldPath())
                        && !formatField.getValFieldPath().equals(oldValPath)) {
                    broker.saveBeanForced(formatField);
                }
                beanPath = formatField.getValFieldPath();
            }
            if (StringUtils.isEmpty(beanPath)) {
                beanPath = StateReportData.LABEL_PREFIX_CHAR + formatField.getName();
            }

            /*
             * Get the formatter for the field.
             * If a formatter is required, see if a SystemStringConverteris also required to
             * convert the saved field value to a formattable object.
             * (i.e. fieldA date "01-01-2009" to java Date for formatting)
             */
            Format formatter = null;
            SystemStringConverter converter = null;

            // Build display formatter if necessary.
            if (ExportFormatField.FormatTypeCode.NONE.ordinal() != formatField.getFormatType()) {
                if (beanPath.charAt(0) != StateReportData.LABEL_PREFIX_CHAR) {
                    DataDictionary dictionary = DataDictionary.getDistrictDictionary(
                            formatField.getDefinition().getExtendedDataDictionary(), broker.getPersistenceKey());
                    ModelProperty property = new ModelProperty(beanClass.getName(), beanPath, dictionary);
                    if (property != null) {
                        /*
                         * Fields that use formatters may require a converter to take the
                         * valueAsString in FieldA,B,C,D values into java objects for
                         * formatting. This requires a SystemStringConverter.
                         *
                         * Ex: FieldA as Date contains "10-01-2009" needs a
                         * SystemStringConverter to turn that into a
                         * java Date so it can be reformatted with the
                         * specified format.
                         */
                        DataDictionaryField dictField = property.getField();
                        if (dictField.isString()) {
                            Converter baseConverter = ConverterFactory.getConverterForClass(
                                    dictField.getEffectiveJavaType(),
                                    LocalizationCache.getPrimarySystemLocale(broker.getPersistenceKey()),
                                    dictField.isString());
                            if (baseConverter instanceof SystemStringConverter) {
                                converter = (SystemStringConverter) baseConverter;
                            }
                        }
                    }
                }


                /*
                 * Find and assign the format object.
                 */
                try {
                    if (ExportFormatField.FormatTypeCode.DATE.ordinal() == formatField.getFormatType()) {
                        formatter = new SimpleDateFormat(formatField.getFormatExpression());
                    } else if (ExportFormatField.FormatTypeCode.NUMBER.ordinal() == formatField.getFormatType()) {
                        formatter = new DecimalFormat(formatField.getFormatExpression());
                    } else if (ExportFormatField.FormatTypeCode.LOGICAL.ordinal() == formatField.getFormatType()) {
                        formatter = new ExportFormatManager.X2LogicalFormat(formatField.getFormatExpression());
                    }
                } catch (Exception e) {
                    throw new X2BaseException((String) null,
                            formatField.getName() + " - Invalid Formatter or expression");
                }
            }

            /*
             * Find padding type if necessary.
             */
            ExportFormatField.PaddingDirectionCode resizeMode = ExportFormatField.PaddingDirectionCode.NONE;
            if (formatField.getPaddingDirection() == ExportFormatField.PaddingDirectionCode.PAD_LEFT.ordinal()) {
                resizeMode = ExportFormatField.PaddingDirectionCode.PAD_LEFT;
            } else if (formatField.getPaddingDirection() == ExportFormatField.PaddingDirectionCode.PAD_RIGHT
                    .ordinal()) {
                resizeMode = ExportFormatField.PaddingDirectionCode.PAD_RIGHT;
            } else if (formatField.getPaddingDirection() == ExportFormatField.PaddingDirectionCode.TRUNCATE_ONLY
                    .ordinal()) {
                resizeMode = ExportFormatField.PaddingDirectionCode.TRUNCATE_ONLY;
            }

            m_fieldId = formatField.getName();
            m_beanPath = beanPath;
            m_defaultValue = formatField.getDefaultValue();
            m_depth = formatField.getDepth();
            m_mappedLookup = formatField.getReferenceMap();
            m_lengthMin = formatField.getMinimumLength();
            m_lengthMax = formatField.getMaximumLength();
            m_keyIndicator = formatField.getKeyInd();
            m_pattern = formatField.getValidationPattern();
            m_formatter = formatter;
            m_converter = converter;
            m_calcId = formatField.getCalculationId();
            m_validatorId = formatField.getValidationId();
            m_parameter = formatField.getCalcParam();
            m_exportLength = formatField.getMaximumLength();
            m_resizeMode = resizeMode;
            m_paddingChar = formatField.getPaddingChar();
            m_effOid = formatField.getOid();
            if (formatField.getDataFieldConfig() != null) {
                m_saveBeanPath = formatField.getDataFieldConfig().getDataField().getJavaName();
            }
            m_sifPath = formatField.getSifPath();

            setScript(formatField);
        }

        /**
         * Constructs one field definition for one field in the export.
         *
         * @param fieldId - The state name for this field, used for display, field
         *        lookup. Every field should have a unique name. (required)
         *
         * @param beanPath - The bean path on the X2BaseBean contained in the entity
         *        to use to retrieve the value for the field. If the field
         *        value is not retrieved through a bean path, this can be
         *        the bean path of the field to use to retrieve the readable
         *        name from its property. If no field property contains the
         *        proper display name for the field, this can be the text
         *        display name to use for the field and should be prepended
         *        with LABEL_PREFIX_CHAR ($). (required)
         *
         * @param defaultValue - The default value to use in this field if no other
         *        value is retrieved. (optional)
         *
         * @param mappedLookup - an indicator that the value retrieved from the bean
         *        property must be translated through its reference code
         *        to a local, state, federal or system value. (required)
         *
         * @param min - Validation: the minimum string length for the field value.
         *        (required)
         * @param max - Validation: the maximum string length for the field value.
         *        (required)
         * @param pattern - Validation: a regular expression used to validate the
         *        value. (optional)
         * @param formatter - A text Format object that can be used to properly
         *        format this field value, such as a DateFormat. A null
         *        indicates no formatting necessary. (optional)
         *
         * @param retriever - an implementation of the FieldRetriever interface
         *        that implements the lookup of the proper value for
         *        this field. (optional)
         *
         * @param validator - an implementation of the FieldValidator interface
         *        that implements the validation of the value for this
         *        field. (optional)
         *
         * @param parameter - a place for the data export implementation to put
         *        whatever data it needs. This will not be used by the
         *        export, but may be used by retrievers or validators.
         *        (optional)
         */
        public FieldDefinition(String fieldId, String beanPath, String defaultValue, int mappedLookup, int min, int max,
                String pattern, Format formatter, FieldRetriever retriever, FieldValidator validator,
                Object parameter) {
            m_fieldId = fieldId;
            m_beanPath = beanPath;
            m_defaultValue = defaultValue;
            m_mappedLookup = mappedLookup;
            m_lengthMin = min;
            m_lengthMax = max;
            m_pattern = pattern;
            m_formatter = formatter;
            m_retriever = retriever;
            m_validator = validator;
            m_parameter = parameter;
        }

        /**
         * Constructs one field definition for one field in the export.
         *
         * @param fieldId - The state name for this field, used for display, field
         *        lookup. Every field should have a unique name. (required)
         *
         * @param beanPath - The bean path on the X2BaseBean contained in the entity
         *        to use to retrieve the value for the field. If the field
         *        value is not retrieved through a bean path, this can be
         *        the bean path of the field to use to retrieve the readable
         *        name from its property. If no field property contains the
         *        proper display name for the field, this can be the text
         *        display name to use for the field and should be prepended
         *        with LABEL_PREFIX_CHAR ($). (required)
         *
         * @param defaultValue - The default value to use in this field if no other
         *        value is retrieved. (optional)
         *
         * @param mappedLookup - an indicator that the value retrieved from the bean
         *        property must be translated through its reference code
         *        to a local, state, federal or system value. (required)
         *
         * @param min - Validation: the minimum string length for the field value.
         *        (required)
         * @param max - Validation: the maximum string length for the field value.
         *        (required)
         * @param pattern - Validation: a regular expression used to validate the
         *        value. (optional)
         * @param exportLength - formatting: the string length for the field value
         *        padding/trimming.
         * @param resizeMode - An enumeration defining the field padding/trimming
         *        behavior on output.
         *
         */
        public FieldDefinition(String fieldId, String beanPath, String defaultValue, int mappedLookup, int min, int max,
                String pattern, int exportLength, ExportFormatField.PaddingDirectionCode resizeMode) {
            m_fieldId = fieldId;
            m_beanPath = beanPath;
            m_defaultValue = defaultValue;
            m_exportLength = exportLength;
            m_resizeMode = resizeMode;
            m_mappedLookup = mappedLookup;
            m_lengthMin = min;
            m_lengthMax = max;
            m_pattern = pattern;
        }

        /**
         * Constructs one field definition for one field in the export.
         *
         * @param fieldId - The state name for this field, used for display, field
         *        lookup. Every field should have a unique name. (required)
         *
         * @param beanPath - The bean path on the X2BaseBean contained in the entity
         *        to use to retrieve the value for the field. If the field
         *        value is not retrieved through a bean path, this can be
         *        the bean path of the field to use to retrieve the readable
         *        name from its property. If no field property contains the
         *        proper display name for the field, this can be the text
         *        display name to use for the field and should be prepended
         *        with LABEL_PREFIX_CHAR ($). (required)
         *
         * @param defaultValue - The default value to use in this field if no other
         *        value is retrieved. (optional)
         *
         * @param mappedLookup - an indicator that the value retrieved from the bean
         *        property must be translated through its reference code
         *        state value. (required)
         *
         * @param min - Validation: the minimum string length for the field value.
         *        (required)
         * @param max - Validation: the maximum string length for the field value.
         *        (required)
         * @param pattern - Validation: a regular expression used to validate the
         *        value. (optional)
         * @param formatter - A text Format object that can be used to properly
         *        format this field value, such as a DateFormat. A null
         *        indicates no formatting necessary. (optional)
         *
         * @param retriever - an implementation of the FieldRetriever interface
         *        that implements the lookup of the proper value for
         *        this field. (optional)
         *
         * @param validator - an implementation of the FieldValidator interface
         *        that implements the validation of the value for this
         *        field. (optional)
         *
         * @param parameter - a place for the data export implementation to put
         *        whatever data it needs. This will not be used by the
         *        export, but may be used by retrievers or validators.
         *        (optional)
         */
        public FieldDefinition(String fieldId, String beanPath, String defaultValue, boolean mappedLookup, int min,
                int max,
                String pattern, Format formatter, FieldRetriever retriever, FieldValidator validator,
                Object parameter) {
            m_fieldId = fieldId;
            m_beanPath = beanPath;
            m_defaultValue = defaultValue;
            // Mapped lookup indicates state code
            m_mappedLookup = (mappedLookup ? ExportFormatField.ReferenceMapTypeCode.STATE.ordinal() : 0);
            m_lengthMin = min;
            m_lengthMax = max;
            m_pattern = pattern;
            m_formatter = formatter;
            m_retriever = retriever;
            m_validator = validator;
            m_parameter = parameter;
        }

        /**
         * Constructs one field definition for one field in the export.
         *
         * @param fieldId - The state name for this field, used for display, field
         *        lookup. Every field should have a unique name. (required)
         *
         * @param beanPath - The bean path on the X2BaseBean contained in the entity
         *        to use to retrieve the value for the field. If the field
         *        value is not retrieved through a bean path, this can be
         *        the bean path of the field to use to retrieve the readable
         *        name from its property. If no field property contains the
         *        proper display name for the field, this can be the text
         *        display name to use for the field and should be prepended
         *        with LABEL_PREFIX_CHAR ($). (required)
         *
         * @param defaultValue - The default value to use in this field if no other
         *        value is retrieved. (optional)
         *
         * @param mappedLookup - an indicator that the value retrieved from the bean
         *        property must be translated through its reference code
         *        state value. (required)
         *
         * @param min - Validation: the minimum string length for the field value.
         *        (required)
         * @param max - Validation: the maximum string length for the field value.
         *        (required)
         * @param pattern - Validation: a regular expression used to validate the
         *        value. (optional)
         * @param exportLength - formatting: the string length for the field value
         *        padding/trimming.
         * @param resizeMode - An enumeration defining the field padding/trimming
         *        behavior on output.
         *
         */
        public FieldDefinition(String fieldId, String beanPath, String defaultValue, boolean mappedLookup, int min,
                int max,
                String pattern, int exportLength, ExportFormatField.PaddingDirectionCode resizeMode) {
            m_fieldId = fieldId;
            m_beanPath = beanPath;
            m_defaultValue = defaultValue;
            m_exportLength = exportLength;
            m_resizeMode = resizeMode;
            // Mapped lookup indicates state code
            m_mappedLookup = (mappedLookup ? ExportFormatField.ReferenceMapTypeCode.STATE.ordinal() : 0);
            m_lengthMin = min;
            m_lengthMax = max;
            m_pattern = pattern;
        }

        /**
         * Returns the bean path on the X2BaseBean wrapped in the entity to use to
         * retrieve the value for this field.
         *
         * @return String
         */
        public String getBeanPath() {
            return m_beanPath;
        }

        /**
         * Returns the calculation Id specificed for theis field.
         *
         * @return String
         */
        public String getCalcId() {
            return m_calcId;
        }

        /**
         * Returns a Converter for the data/field type on the bean path.
         * For fields that use formatters, may need a converter to
         * change the stored data into the propert java object for the
         * formatter.
         *
         * @return SystemStringConverter
         */
        public SystemStringConverter getConverter() {
            return m_converter;
        }

        /**
         * Returns the default value to use if the property and/or retriever
         * return an empty value.
         *
         * @return a String
         */
        public String getDefaultValue() {
            return m_defaultValue;
        }

        /**
         * Returns the nesting depth for XML elements.
         *
         * @return int
         */
        public int getDepth() {
            return m_depth;
        }

        /**
         * Returns the saved EfdOid for the EFF record that defined this field, if any.
         *
         * @return String
         */
        public String getEffOid() {
            return m_effOid;
        }


        /**
         * returns the state name value for this field.
         *
         * @return the state name for the field.
         */
        public String getFieldId() {
            return m_fieldId;
        }


        /**
         * Returns the formatter object that will properly format this field value
         * (such as a DateFormat).
         *
         * @return Format
         */
        public Format getFormatter() {
            return m_formatter;
        }

        /**
         * Returns the key indicator flag from the export format field.
         *
         * @return boolean
         */
        public boolean getKeyindicator() {
            return m_keyIndicator;
        }

        /**
         * Returns an type of lookup mapped through a lookup
         * reference table.
         *
         * @return int
         */
        public int getMappedLookup() {
            return m_mappedLookup;
        }

        /**
         * Returns the field maximum length for validation.
         *
         * @return int
         */
        public int getMaxLength() {
            return m_lengthMax;
        }

        /**
         * Returns the resizeMode.
         *
         * @return a PaddingDirectionCode
         */
        public ExportFormatField.PaddingDirectionCode getResizeMode() {
            return m_resizeMode;
        }

        /**
         * Returns the exportLength.
         *
         * @return int
         */
        public int getExportLength() {
            return m_exportLength;
        }

        /**
         * Returns the field value minimum length for validation.
         *
         * @return int
         */
        public int getMinLength() {
            return m_lengthMin;
        }

        /**
         * Returns the padding character used to pad values for display.
         *
         * @return String. The first character is significant
         */
        public String getPaddingChar() {
            return m_paddingChar;
        }

        /**
         * Returns the implementation defined extra object.
         *
         * @return Object
         */
        public Object getParameter() {
            return m_parameter;
        }

        /**
         * Returns the regular expression pattern to be used to validate this field.
         *
         * @return a String regex pattern
         */
        public String getPattern() {
            return m_pattern;
        }

        /**
         * Returns the data retriever for this field.
         *
         * @return a FieldRetriever
         */
        public FieldRetriever getRetriever() {
            return m_retriever;
        }

        /**
         * Returns the bean field on the save record (CustomExportRow) which will
         * hold the save value for this field. This will be a "field@###" value.
         * If this value is empty, the field value cannot be saved, (such as filler fields).
         *
         * @return String
         */
        public String getSaveBeanPath() {
            return m_saveBeanPath;
        }

        /**
         * Gets the calculating function for this field definition.
         *
         * @return String
         */
        public String getScriptFunction() {
            return m_scriptFunction;
        }

        /**
         * Gets the name of the calculating function for this field definition.
         *
         * @return String
         */
        public String getScriptFunctionName() {
            return m_scriptFunctionName;
        }

        /**
         * Returns the Sif path for this value.
         * For definitions that implement sif topics, the sif path is used
         * to identify where in the sif xml this value is to be set.
         *
         * @return String
         */
        public String getSifPath() {
            return m_sifPath;
        }

        /**
         * Returns the user name of the field. this could be a lookup
         * based on the property (bean path) or it could be a constant
         * passed into the bean path field with a '$' prefix.
         *
         * @param beanClass Class
         * @param persistenceKey PersistenceKey
         * @return String
         */
        public String getUserName(Class beanClass, PersistenceKey persistenceKey) {
            if (m_userName == null) {
                if (m_beanPath.charAt(0) == StateReportData.LABEL_PREFIX_CHAR) {
                    m_userName = m_beanPath.substring(1);
                } else {
                    m_userName =
                            WebUtils.getLabel(new ModelProperty(beanClass.getName(), m_beanPath, persistenceKey), true,
                                    null, false);
                }
            }
            return m_userName;
        }

        /**
         * Returns the validator for this field.
         *
         * @return a FieldValidator
         */
        public FieldValidator getValidator() {
            return m_validator;
        }

        /**
         * Returns the validator ID for the validator to use for this field.
         *
         * @return String
         */
        public String getValidatorId() {
            return m_validatorId;
        }

        /**
         * Allow the bean path to be changed. This is used by some field retrievers that
         * must deal with
         *
         *
         * @param beanPath void
         */
        public void setBeanPath(String beanPath) {
            m_beanPath = beanPath;
        }

        /**
         * Sets the SystemStringConverter used to convert the field pointed to
         * by the bean path into a proper java object for use in
         * formatters.
         *
         * @param converter void
         */
        public void setConverter(SystemStringConverter converter) {
            m_converter = converter;
        }

        /**
         * Sets the default value for the field.
         *
         * @param defaultValue void
         */
        public void setDefaultValue(String defaultValue) {
            m_defaultValue = defaultValue;
        }

        /**
         * Sets the efdOid for the EFF record that defined this field, if any.
         *
         * @param effOid void
         */
        public void setEffOid(String effOid) {
            m_effOid = effOid;
        }


        /**
         * Sets the retriever for this field. This is used to assign a retriever based on the
         * calcId.
         *
         *
         * @param retriever void
         */
        public void setRetriever(FieldRetriever retriever) {
            m_retriever = retriever;
        }

        /**
         * Sets the bean field name on the save record (CustomExportRow) where the value for this
         * field
         * will be saved.
         *
         * @param saveBeanPath void
         */
        public void setSaveBeanPath(String saveBeanPath) {
            m_saveBeanPath = saveBeanPath;
        }

        /**
         * Sets member variables for a javascript retrieval function from a given ExportFormatField.
         *
         * @param formatField void
         */
        private void setScript(ExportFormatField formatField) {
            if (!StringUtils.isEmpty(formatField.getScript())) {
                StringBuilder javascript = new StringBuilder();

                javascript.append(ScriptManager.FUNCTION_CONSTANT);

                StringBuilder functionName = new StringBuilder();
                functionName.append(formatField.getDefinition().getOid());
                functionName.append(ScriptManager.FUNCTION_DELIMETER_CONSTANT);
                functionName.append(formatField.getOid());
                functionName.append(ScriptManager.FUNCTION_DELIMETER_CONSTANT);
                functionName.append(formatField.getPosition());

                javascript.append(functionName.toString());
                javascript.append(ScriptManager.LEFT_PARENTHESIS);
                javascript.append(DATA_PARAM);
                javascript.append(ScriptManager.PARAMETER_DELIMETER);
                javascript.append(ENTITY_PARAM);
                javascript.append(ScriptManager.PARAMETER_DELIMETER);
                javascript.append(FIELD_PARAM);
                javascript.append(ScriptManager.RIGHT_PARENTHESIS);
                javascript.append(ScriptManager.LEFT_BRACKET);
                javascript.append(formatField.getScript());
                javascript.append(ScriptManager.RIGHT_BRACKET);

                m_scriptFunctionName = functionName.toString();
                m_scriptFunction = javascript.toString();
            }
        }

        /**
         * Sets the calculating function for this field definition.
         *
         * @param scriptFunction void
         * @return String
         */
        public void setScriptFunction(String scriptFunction) {
            this.m_scriptFunction = scriptFunction;
        }

        /**
         * Sets the name of the calculating function for this field definition.
         *
         * @param m_scriptFunctionName void
         * @return String
         */
        public void setScriptFunctionName(String m_scriptFunctionName) {
            this.m_scriptFunctionName = m_scriptFunctionName;
        }

        /**
         * Sets the validator for this field. This is used to assign a validator based on the
         * calcId.
         *
         * @param validator void
         */
        public void setValidator(FieldValidator validator) {
            m_validator = validator;
        }
    }

    /**
     * The Class FieldPerformanceMonitor.
     */
    public static class PerformanceMonitor {
        private static final DecimalFormat TIME_FORMATTER = new DecimalFormat("######0.00000");
        private static final String HEADING =
                "Item, Count, ElapsedTime, RunTime, minRunTime, maxRunTime, minElapsedTime, maxElapsedTime";

        /**
         * The Class StackElement.
         */
        class StackElement {
            String path;
            private long m_endTime;
            private long m_initialStartTime;
            private long m_intervalTime;
            private long m_runTime;
            private long m_totalTime;

            /**
             * Instantiates a new stack element.
             *
             * @param path String
             * @param pushTime long
             */
            public StackElement(String path, long pushTime) {
                this.path = path;
                m_initialStartTime = pushTime;
                m_intervalTime = pushTime;
            }

            /**
             * Gets the field definition.
             *
             * @return Field definition
             */
            public String getPath() {
                return path;
            }

            /**
             * End interval.
             *
             * @param time long
             */
            public void endInterval(long time) {
                m_endTime = time;
                m_runTime += m_endTime - m_intervalTime;
            }

            /**
             * Start interval.
             *
             * @param time long
             */
            public void startInterval(long time) {
                m_intervalTime = time;
            }

            /**
             * Gets the run time.
             *
             * @return long
             */
            public long getRunTime() {
                return m_runTime;
            }

            /**
             * Gets the elapsed time.
             *
             * @return long
             */
            public long getElapsedTime() {
                return m_endTime - m_initialStartTime;
            }

        }

        /**
         * The Class Accumulator.
         */
        class Accumulator {
            int cnt;
            long totalRunTime;
            long maxRunTime;
            long minRunTime = Long.MAX_VALUE;
            long totalElapsedTime;
            long maxElapsedTime;
            long minElapsedTime = Long.MAX_VALUE;

            /**
             * To string.
             *
             * @return String
             * @see java.lang.Object#toString()
             */
            @Override
            public String toString() {
                StringBuilder output = new StringBuilder();
                output.append(Integer.toString(cnt));
                output.append(",");
                output.append(time(totalElapsedTime));
                output.append(",");
                output.append(time(totalRunTime));
                output.append(",");
                output.append(time(minRunTime));
                output.append(",");
                output.append(time(maxRunTime));
                output.append(",");
                output.append(time(minElapsedTime));
                output.append(",");
                output.append(time(maxElapsedTime));
                return output.toString();
            }

            /**
             * Time.
             *
             * @param t long
             * @return String
             */
            private String time(long t) {
                String value = "";
                if (!Double.isNaN(t)) {
                    value = TIME_FORMATTER.format((double) t / 1_000_000_000);
                }
                return value;
            }
        }

        Stack<StackElement> stack = new Stack();
        Map<String, Accumulator> statistics = new LinkedHashMap();

        /**
         * Push field definition.
         *
         * @param path String
         */
        public void pushFieldDefinition(String path) {
            long pushTime = System.nanoTime();
            StackElement topElement = stack.isEmpty() ? null : stack.peek();
            if (topElement != null) {
                topElement.endInterval(pushTime);
            }
            stack.push(new StackElement(path, pushTime));
            String key = getCurrentKey();
            if (!statistics.containsKey(key)) {
                statistics.put(key, new Accumulator());
            }
        }

        /**
         * Pop field definition.
         */
        public void popFieldDefinition() {
            long popTime = System.nanoTime();
            String key = getCurrentKey();
            StackElement element = stack.pop();
            element.endInterval(popTime);
            StackElement topElement = stack.isEmpty() ? null : stack.peek();
            if (topElement != null) {
                topElement.startInterval(popTime);
            }
            addStatistics(key, element);
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            StringBuilder output = new StringBuilder();
            output.append(HEADING);
            output.append(System.getProperty("line.separator"));
            for (Entry<String, Accumulator> item : statistics.entrySet()) {
                output.append(item.getKey());
                output.append(",");
                output.append(item.getValue().toString());
                output.append(System.getProperty("line.separator"));
            }
            return output.toString();
        }

        /**
         * Adds the statistics.
         *
         * @param key String
         * @param element StackElement
         */
        private void addStatistics(String key, StackElement element) {
            Accumulator accumulator = statistics.get(key);
            if (accumulator == null) {
                throw new IllegalStateException("Accummulator not found " + key);
            }
            ++accumulator.cnt;
            long runTime = element.getRunTime();
            accumulator.totalRunTime += runTime;
            if (accumulator.maxRunTime < runTime) {
                accumulator.maxRunTime = runTime;
            }
            if (accumulator.minRunTime > runTime) {
                accumulator.minRunTime = runTime;
            }
            long elapsedTime = element.getElapsedTime();
            accumulator.totalElapsedTime += elapsedTime;
            if (accumulator.maxElapsedTime < elapsedTime) {
                accumulator.maxElapsedTime = elapsedTime;
            }
            if (accumulator.minElapsedTime > elapsedTime) {
                accumulator.minElapsedTime = elapsedTime;
            }
        }

        /**
         * Gets the current key.
         *
         * @return String
         */
        private String getCurrentKey() {
            StringBuilder key = new StringBuilder();
            Iterator<StackElement> iterator = stack.iterator();
            while (iterator.hasNext()) {
                if (key.length() != 0) {
                    key.append("/");
                }
                StackElement element = iterator.next();
                key.append(element.getPath());
            }
            return key.toString();
        }
    }

    /**
     * An interface for use in fields that need to extract custom data in a more complex way.
     * Instances of this interface will be used to lookup complex field values.
     */
    public interface FieldRetriever {

        /**
         * Implementing method will look up the field value for a field in an entity.
         *
         * @param data The owning StateReportData object.
         * @param entity the reporting Entity object.
         * @param field The field to retrieve.
         * @return Object
         * @throws X2BaseException exception
         */
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException;
    }

    /**
     * An interface for use in fields that need extra validation.
     * Instances of this interface will be used to perform complex, multi-field validation.
     */
    public interface FieldValidator {

        /**
         * Implementing method will check field value validity within
         * the context of the reporting entity.
         *
         * @param data the owning data module.
         * @param entity The reporting entity.
         * @param field The field definition of the field.
         * @param value The field value to check
         * @return Collection
         */
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value);
    }

    /**
     * The Class Pair.
     *
     * @param <L> the generic type
     * @param <R> the generic type
     */
    public static class Pair<L, R> {
        /**
         * Of.
         *
         * @param <L> the generic type
         * @param <R> the generic type
         * @param left L
         * @param right R
         * @return Pair
         */
        public static <L, R> Pair<L, R> of(L left, R right) {
            return new Pair(left, right);
        }

        private final L m_left;
        private final R m_right;

        /**
         * Instantiates a new pair.
         *
         * @param left L
         * @param right R
         */
        private Pair(L left, R right) {
            m_left = left;
            m_right = right;
        }

        /**
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {

            return getLeft().equals(((Pair) obj).getLeft()) && getRight().equals(((Pair) obj).getRight());
        }

        /**
         * Gets the left.
         *
         * @return l
         */
        public L getLeft() {
            return m_left;
        }

        /**
         * Gets the right.
         *
         * @return r
         */
        public R getRight() {
            return m_right;
        }

        /**
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return getLeft().hashCode() + getRight().hashCode();
        }

        /**
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "Pair(" + m_left + "," + m_right + ")";
        }
    }

    /**
     * Continuous potentially multi-year parent span
     * representing the entire student-school enrollment
     * from entry (including prereg) through withdrawal.
     *
     * Not split on S or Y records.
     * Can return single year child spans
     * which can also optionally split on S/Y records.
     */
    public static class ParentSpan implements Comparable {
        private List<AnnualSpan> m_annualSpans;
        private List<? extends ToolEnrollment> m_enrollments;
        private ToolSchool m_school;
        private ToolStudent m_student;

        /**
         * *********************
         * getters and setters
         * *********************.
         *
         * @return List
         */

        /**
         * @return the enrollments
         */
        public List<? extends ToolEnrollment> getEnrollments() {
            return m_enrollments;
        }

        /**
         * *********************
         * Most basic span info
         * *********************.
         *
         * @param span2 Object
         * @return int
         */

        /**
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(Object span2) {
            if (!(span2 instanceof ParentSpan)) {
                return 0;
            }

            boolean nullIsOld = true;
            return compareDates(getFirstActiveInSessionDate(),
                    ((ParentSpan) span2).getFirstActiveInSessionDate(), nullIsOld);
        }

        /**
         * Gets the annual spans.
         *
         * @return List
         */
        public List<AnnualSpan> getAnnualSpans() {
            return m_annualSpans;
        }

        /**
         * Gets the first active enrollment.
         *
         * @return enr
         */
        public ToolEnrollment getFirstActiveEnrollment() {
            return (m_annualSpans.isEmpty() ? null : m_annualSpans.get(0).getFirstActiveEnrollment());
        }

        /**
         * Gets the first active in session date. <br />
         * This the Entry Date <br />
         * - Adjusted by member on entry preference <br />
         * - Advanced to first in-session date <br />
         *
         * @return Plain date
         */
        public PlainDate getFirstActiveInSessionDate() {
            return (m_annualSpans.isEmpty() ? null : m_annualSpans.get(0).getFirstActiveInSessionDate());
        }

        /**
         * Gets the last active in session date.
         *
         * This is:
         * - null if the parent span is not terminated
         * ---- i.e. has no W record and isn't a secondary enrollment
         * - else lastActiveDateInSessionDate of the last (child) AnnualSpan
         *
         * @return Plain date
         */
        public PlainDate getLastActiveInSessionDate() {
            AnnualSpan lastAnnualSpan = m_annualSpans.isEmpty()
                    ? null
                    : m_annualSpans.get(m_annualSpans.size() - 1);
            if (lastAnnualSpan != null
                    && (lastAnnualSpan.isWithdrawal() || lastAnnualSpan.isSecondary())) {
                return lastAnnualSpan.getLastActiveInSessionDate();
            }
            return null;
        }

        /**
         * Gets the school.
         *
         * @return Sis school
         */
        public ToolSchool getSchool() {
            return m_school;
        }

        /**
         * Gets the secondary.
         *
         * @return ssk
         */
        public ToolStudentSchool getSecondary() {
            return m_annualSpans.isEmpty()
                    ? null
                    : m_annualSpans.get(0).getSecondary();
        }


        /**
         * Gets the student.
         *
         * @return std
         */
        /*
         * @return SisStudent student
         */
        public ToolStudent getStudent() {
            return m_student;
        }

        /**
         * Checks if is secondary.
         *
         * @return true, if is secondary
         */
        public boolean isSecondary() {
            return m_annualSpans.isEmpty()
                    ? false
                    : m_annualSpans.get(0).isSecondary();
        }

        /**
         * Sets the enrollments.
         *
         * @param enrollments void
         */
        public void setAnnualSpans(List<AnnualSpan> enrollments) {
            m_annualSpans = enrollments;
        }

        /**
         * Sets the enrollments.
         *
         * @param enrollments void
         */
        public void setEnrollments(List<? extends ToolEnrollment> enrollments) {
            this.m_enrollments = enrollments;
        }

        /**
         * Sets the school.
         *
         * @param school void
         */
        public void setSchool(ToolSchool school) {
            m_school = school;
        }

        /**
         * Sets the student.
         *
         * @param student void
         */
        public void setStudent(ToolStudent student) {
            m_student = student;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            String schoolName = getSchool().getName();
            String studentName = getStudent().getNameView();

            PlainDate startDateForPrint = null;
            ToolEnrollment firstEnrollment = getFirstActiveEnrollment();
            if (firstEnrollment != null) {
                startDateForPrint = firstEnrollment.getEnrollmentDate();
            } else if (isSecondary()) {
                startDateForPrint = getSecondary().getStartDate();
            }

            String toString = schoolName + " " + studentName + " " + startDateForPrint;

            for (AnnualSpan annualSpan : m_annualSpans) {
                toString += "; " + annualSpan.toString();
            }

            return toString;
        }
    }

    /**
     * The Class Range.
     *
     * @param <T> the generic type
     */
    public static class Range<T extends Comparable<T>> {
        private static final String INFINITE = "INFINITE";

        /**
         * Of.
         *
         * @param <T> the generic type
         * @param left T
         * @param right T
         * @return Range
         */
        public static <T extends Comparable<T>> Range of(T left, T right) {
            if (left != null && right != null) {
                if (left.compareTo(right) > 0) {
                    throw new RuntimeException(
                            "Range cannot be with left side (" + left + ") greater than right side (" + right + ")");
                }
            }
            return new Range(left, right);
        }

        private T m_start;
        private T m_end;

        /**
         * Instantiates a new range.
         *
         * @param start T
         * @param end T
         */
        private Range(T start, T end) {
            m_start = start;
            m_end = end;
        }

        /**
         * Contains.
         *
         * @param item T
         * @return true, if successful
         */
        public boolean contains(T item) {
            return (isStartInfinite() || m_start.compareTo(item) <= 0)
                    && (isEndInfinite() || m_end.compareTo(item) >= 0);
        }

        /**
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            Range<T> item = (Range<T>) obj;
            boolean beginMatch = false;
            if (m_end == null) {
                if (item.m_end == null) {
                    beginMatch = true;
                }
            } else if (item.m_end != null) {
                beginMatch = m_end.equals(item.m_end);
            }
            boolean endMatch = false;
            if (m_end == null) {
                if (item.m_end == null) {
                    endMatch = true;
                }
            } else if (item.m_end != null) {
                endMatch = m_end.equals(item.m_end);
            }
            return beginMatch && endMatch;
        }

        /**
         * Gets the end.
         *
         * @return t
         */
        public T getEnd() {
            return m_end;
        }

        /**
         * Gets the start.
         *
         * @return t
         */
        public T getStart() {
            return m_start;
        }

        /**
         * Creates a range that is the intersection of the two ranges.
         *
         * @param b Range<T>
         * @return Range
         */
        public Range<T> intersection(Range<T> b) {
            T start = null;
            if (isStartInfinite()) {
                start = b.m_start;
            } else if (b.isStartInfinite() || m_start.compareTo(b.m_start) >= 0) {
                start = m_start;
            } else {
                start = b.m_start;
            }

            T end = null;
            if (isEndInfinite()) {
                end = b.m_end;
            } else if (b.isEndInfinite() || m_end.compareTo(b.m_end) <= 0) {
                end = m_end;
            } else {
                end = b.m_end;
            }

            if (start != null && end != null) {
                if (start.compareTo(end) > 0) {
                    return null;
                }
            }
            return Range.of(start, end);
        }

        /**
         * Checks if is overlap.
         *
         * Let ConditionA Mean that DateRange A Completely After DateRange B
         * (True if StartA > EndB)
         *
         * Let ConditionB Mean that DateRange A is Completely Before DateRange B
         * (True if EndA < StartB)
         *
         * Then Overlap exists if Neither A Nor B is true -
         * (If one range is neither completely after the other,
         * nor completely before the other, then they must overlap.)
         *
         * Now one of De Morgan's laws says that:
         *
         * Not (A Or B) <=> Not A And Not B
         *
         * Which translates to: (StartA <= EndB) and (EndA >= StartB)
         *
         * @param dateRange Range<T>
         * @return true, if is overlap
         */
        public boolean isOverlap(Range<T> dateRange) {
            // (StartA <= EndB) and (EndA >= StartB), proof:
            // https://stackoverflow.com/a/325964/2382239

            Range A = this;
            Range B = dateRange;
            // StartA <= EndB
            boolean leftCondition = A.isStartInfinite() || B.isEndInfinite() || A.m_start.compareTo(B.m_end) != 1;

            // EndA >= StartB
            boolean rightCondition = A.isEndInfinite() || B.isStartInfinite() || A.m_end.compareTo(B.m_start) != -1;

            return leftCondition && rightCondition;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return (m_start == null ? INFINITE : m_start.toString())
                    + " - " +
                    (m_end == null ? INFINITE : m_end.toString());
        }

        /**
         * Checks if is start infinite.
         *
         * @return true, if is start infinite
         */
        private boolean isStartInfinite() {
            return m_start == null;
        }

        /**
         * Checks if is end infinite.
         *
         * @return true, if is end infinite
         */
        private boolean isEndInfinite() {
            return m_end == null;
        }
    }

    /**
     * The Class StateReportData.
     *
     * @author Follett Software Company
     * @copyright 2021
     */
    public static class StateReportData {
        protected static final String BAR_SEPARATOR = "|";
        protected static final String EMPTY_STRING = "";
        protected static final char LABEL_PREFIX_CHAR = '$';

        private static final String INITIALIZE_KEY = "label.state.report.initialize";

        /*
         * iReport style constants.
         */
        protected static final String STYLE_BOLD = "<style isBold=\"true\" pdfFontName=\"Helvetica-Bold\">";
        protected static final String STYLE_END = "</style>";

        /*
         * Constants for query criteria parameters from user input template.
         */
        protected static final String PARAM_QUERY_BY_FIELD = "queryBy";
        protected static final String PARAM_QUERY_BY_CRITERIA = "queryString";
        protected static final String PARAM_SORT_BY_FIELDS = "sortBy";

        /**
         * Returns a StateReportData object from a procedure Id.
         *
         * Custom implementations of StateReportData are stored in Procedures. The
         * StateReport reports and exports look up the procedure to get the custom class.
         *
         * This method looks up the procedure, gets the java object from that procedure
         * and loads it as a StateReportData object.
         * <p>
         * Note: if the tool class is being loaded from a JAR plugin, and the class calling this
         * method
         * was itself loaded from
         * the same JAR, the other version of <code>getReportDataFromProcedure</code> must be used
         * and a
         * classloader provided; see javadoc
         * on the <code>classLoader</code> argument.
         *
         * @param procedureId The procedure ID.
         * @param broker A broker to use to lookup the procedure.
         * @param errors A collection to store any errors encountered during retrieval.
         *
         * @return StateReportData
         */
        public static StateReportData getReportDataFromProcedure(String procedureId,
                                                                 X2Broker broker,
                                                                 Collection<StateReportValidationError> errors) {
            return getReportDataFromProcedure(procedureId, broker, errors, StateReportData.class.getClassLoader());
        }

        /**
         * Returns a StateReportData object from a procedure Id.
         *
         * Custom implementations of StateReportData are stored in Procedures. The
         * StateReport reports and exports look up the procedure to get the custom class.
         *
         * This method looks up the procedure, gets the java object from that procedure
         * and loads it as a StateReportData object.
         *
         * @param procedureId The procedure ID.
         * @param broker A broker to use to lookup the procedure.
         * @param errors A collection to store any errors encountered during retrieval.
         * @param classLoader May be required if the class is being loaded from a JAR plugin. If the
         *        class calling this
         *        method was itself loaded from a JAR plugin, and the code being loaded is also
         *        coming
         *        from the same
         *        JAR plugin, the same class loader must be used to avoid different versions of the
         *        same
         *        Class
         *        object from being used. In such cases, passing "getClass().getClassLoader()" is
         *        required; otherwise
         *        null maybe passed in which case a new class loader will be created internally
         *
         * @return StateReportData
         */
        public static StateReportData getReportDataFromProcedure(String procedureId,
                                                                 X2Broker broker,
                                                                 Collection<StateReportValidationError> errors,
                                                                 ClassLoader classLoader) {
            return getReportDataFromProcedure(procedureId, broker, errors, classLoader, null);
        }

        /**
         * Returns a StateReportData object from a procedure Id.
         *
         * Custom implementations of StateReportData are stored in Procedures. The
         * StateReport reports and exports look up the procedure to get the custom class.
         *
         * This method looks up the procedure, gets the java object from that procedure
         * and loads it as a StateReportData object.
         *
         * @param procedureId String
         * @param broker X2Broker
         * @param errors Collection<StateReportValidationError>
         * @param parentClassLoader ClassLoader
         * @param userTempPath String
         * @return StateReportData
         */
        public static StateReportData getReportDataFromProcedure(String procedureId,
                                                                 X2Broker broker,
                                                                 Collection<StateReportValidationError> errors,
                                                                 ClassLoader parentClassLoader,
                                                                 String userTempPath) {
            StateReportData reportData = null;

            try {
                Procedure procedure = (Procedure) ToolManager.getToolForId(Tool.TYPE_PROCEDURE, procedureId, broker);

                File userTempFolder = null;
                if (!StringUtils.isEmpty(userTempPath)) {
                    userTempFolder = new File(userTempPath);
                }

                ToolSourceCode sourceCode = procedure.getSourceCode();

                ByteArrayClassLoader classLoader = new ByteArrayClassLoader(parentClassLoader,
                        sourceCode.getCompiledCode(),
                        procedure.getPersistenceKey().getDeploymentId(),
                        userTempFolder != null ? userTempFolder.getAbsolutePath() : null);

                String className = getToolClass(sourceCode.getSourceCode());
                String packageName = getToolPackage(sourceCode.getSourceCode());
                if (StringUtils.isEmpty(className)) {
                    throw new IllegalStateException("Tool package/class cannot be determined for procedure id "
                            + procedureId + " class [" + className + "] package [" + packageName + "]");
                }
                if (!StringUtils.isEmpty(packageName)) {
                    className = packageName + "." + className;
                }

                Class toolObjectClass = classLoader.loadClass(className);

                reportData = (StateReportData) toolObjectClass.getDeclaredConstructor().newInstance();

                if (reportData != null) {
                    reportData.setClassLoader(classLoader);
                    reportData.setProcedure(procedure);
                    if (sourceCode != null) {
                        reportData.setInputDefinition(sourceCode.getInputDefinition());
                    }
                }
            } catch (Exception e) {
                errors.add(
                        new StateReportValidationError(procedureId, procedureId, e.getClass().getName(),
                                LoggerUtils.convertThrowableToString(e)));
            }

            if (reportData == null) {
                reportData = new StateReportData();
            }
            reportData.setProcedureId(procedureId);
            errors.addAll(reportData.loadDefinitions(procedureId, broker));

            return reportData;
        }

        /**
         * Gets the tool class.
         *
         * @param sourceCode String
         * @return String
         */
        private static String getToolClass(String sourceCode) {
            Pattern pattern = Pattern.compile("public class (\\S*)");
            Matcher matcher = pattern.matcher(sourceCode);
            if (matcher.find()) {
                return matcher.group(1);
            }
            return null;
        }

        /**
         * Gets the tool package.
         *
         * @param sourceCode String
         * @return String
         */
        private static String getToolPackage(String sourceCode) {
            Pattern pattern = Pattern.compile("package (\\S*);");
            Matcher matcher = pattern.matcher(sourceCode);
            if (matcher.find()) {
                return matcher.group(1);
            }
            return null;
        }

        /**
         * No argument constructor, necessary for instantiation from the
         * calling report and export definition.
         */
        public StateReportData() {
            super();
            m_escapedChars = new HashMap<String, String>();
            m_escapedChars.put("\\t", "\t");
            m_escapedChars.put("\\b", "\b");
            m_escapedChars.put("\\n", "\n");
            m_escapedChars.put("\\r", "\r");
            m_escapedChars.put("\\f", "\f");
        }

        /**
         * A class defining the root bean of the report query.
         */
        protected Class<? extends X2BaseBean> m_x2BeanClass;

        /**
         * An X2Broker for performing initialization and the data query.
         */
        protected X2Broker m_broker = null;

        /**
         * A map of field retrievers specified by calc name.
         */
        protected HashMap<String, FieldRetriever> m_calcMap;

        /**
         * The class loader that loaded the compiled code
         */
        protected ClassLoader m_classLoader;

        /**
         * Current school year context.
         */
        protected DistrictSchoolYearContext m_currentContext;

        /**
         * The current entity from the iterator, that may be iterating its own row count.
         */
        protected StateReportEntity m_currentEntity;

        /**
         * A list of the field definition objects for this export data.
         */
        protected List<FieldDefinition> m_currentFieldDefinitions;

        /**
         * Custom export definition, from state reporting definition table.
         */
        protected ExportFormatDefinition m_definition;

        /**
         * Delimiter character to use in delimited output, such as comma (,) for CSV.
         */
        protected Character m_delimiterChar = Character.valueOf(',');

        /**
         * A local copy of the data dictionary for use by various lookup utilities.
         */
        protected DataDictionary m_dictionary;

        protected DictionaryExtractor m_dictionaryExtractor;

        /**
         * The OID of the ExportFormatDefinition that defines this export, if any.
         */
        protected String m_efdOid;

        /**
         * A StateReportEntity object. This will contain the class object for StateReportEntity or
         * an implementing subclass of StateReportEntity.
         * It should be assigned by the implementing data class in the initialize() method if an
         * overriding entity subclass is present.
         */
        protected Class<StateReportEntity> m_entityClass = StateReportEntity.class;

        /**
         * Escape character to use in delimited/wrapped output for special chars.
         */
        protected Character m_escapeChar = null;

        /**
         * Map of user enterable escaped character codes and their translated non-printable values.
         */
        protected Map<String, String> m_escapedChars = null;

        /**
         * The name of the export.
         */
        protected String m_exportName = null;

        protected Filterable<? extends ToolBean> m_filterable;

        /**
         * String for heading text to appear in the first line of the export
         */
        protected String m_heading;

        /**
         * The procedure's input definition
         */
        protected String m_inputDefinition;

        /**
         * Context information from the calling report/export.
         */
        protected boolean m_isSchoolContext;

        /**
         * A map of all sets of field definitions loaded for all
         * export format definitions for this export.
         * The key is the suffix of the export format definition beyond the procedure id
         * used for the export and procedure.
         */
        protected Map<String, List<FieldDefinition>> m_loadedFieldDefinitions =
                new HashMap<String, List<FieldDefinition>>();


        protected Iterator<? extends ToolBean> m_iterator;
        /**
         * A organization from the calling report/export
         */
        protected Organization m_organization;

        /**
         * A collection of user input parameters from the report or export.
         */
        protected Map<String, Object> m_parameters;

        /**
         * A privilege set from the report or export.
         */
        protected PrivilegeSet m_privSet;

        /**
         * The state report's procedure ID
         */
        protected String m_procedureId;

        /**
         * The state report's procedure
         */
        protected Procedure m_procedure;

        /**
         * The selected school from the report or export.
         */
        protected School m_school;

        /**
         * Script manager for saving and later executing calculations for fields.
         */
        protected ScriptManager m_scriptManager;

        /**
         * A list of errors encountered during initialization. Accessible through getSetupErrors().
         */
        protected ArrayList<StateReportValidationError> m_setupErrors = new ArrayList<StateReportValidationError>();

        /**
         * String for trailer text to appear in the last line of the export
         */
        protected String m_trailer;

        // Initial report formatting values.
        protected boolean m_useDelimiter = true;
        protected boolean m_useEscape = true;
        protected boolean m_useWrapper = true;

        /**
         * A User object from the enclosing report or export.
         */
        protected User m_user;

        /**
         * A map of field retrievers specified by calc name.
         *
         */
        protected HashMap<String, FieldValidator> m_validatorMap;

        /**
         * Wrapper character for column values in wrapped output. Such as a quote (").
         */
        protected Character m_wrapperChar = Character.valueOf('"');

        /**
         * Adds one validation error with no entity or field.
         *
         *
         * @param errorType String
         * @param errorMessage String
         */
        public void addSetupError(String errorType, String errorMessage) {
            String initMsg = LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(INITIALIZE_KEY);
            m_setupErrors.add(new StateReportValidationError(initMsg, initMsg, errorType, errorMessage));
        }

        /**
         * Apply the standard user input criteria from input template to a Criteria object.
         *
         * <p>
         * Multiple criteria supported.
         * <br>
         * Iterate through enumerated values of PARAM_QUERY_BY_FIELD + "#" as "#" increments from 1
         * to n.
         *
         * <p>
         * The prefix path is the relationship bean path from to apply to the current bean to take
         * it
         * to the expected base bean of the criteria.
         * <br>
         * EX: If the user input selection criteria is for the Student table, but the criteria is
         * for a related table StudentEnrollment, the prefix path would be "student" (or
         * StudentEnrollment.REL_STUDENT) to get from the StudentEnrollment to the Student table.
         *
         * @param criteria Criteria
         * @param applySchool boolean
         * @param prefixPath String
         */
        public void applyInputCriteria(Criteria criteria, boolean applySchool, String prefixPath) {
            int queryCount = 1;
            String fullPrefixPath = EMPTY_STRING;
            if (!StringUtils.isEmpty(prefixPath)) {
                fullPrefixPath = prefixPath + ModelProperty.PATH_DELIMITER;
            }

            String queryBy = (String) getParameter(PARAM_QUERY_BY_FIELD + Integer.toString(queryCount));
            String queryString = (String) getParameter(PARAM_QUERY_BY_CRITERIA + Integer.toString(queryCount));
            while (!StringUtils.isEmpty(queryBy) && !StringUtils.isEmpty(queryString)) {
                if (queryBy.equals("##all")) {
                    // Do nothing, select all.
                } else if (queryBy.equals("##snapshot")) {
                    SubQuery recordSetSubquery = ReportUtils.getRecordSetSubQuery(queryString, getUser(), getSchool());
                    Collection<String> objectOids = getBroker().getSubQueryCollectionByQuery(recordSetSubquery);
                    criteria.addIn(fullPrefixPath + X2BaseBean.COL_OID, objectOids);
                } else if (queryBy.startsWith("a:")) {
                    String resolvedAliasBeanPath = getResolvedAliasBeanPath(queryBy);
                    if (resolvedAliasBeanPath != null) {
                        criteria.addEqualTo(fullPrefixPath + resolvedAliasBeanPath, queryString);
                    }
                } else {
                    criteria.addEqualTo(fullPrefixPath + queryBy, queryString);
                }

                queryCount++;
                queryBy = (String) getParameter(PARAM_QUERY_BY_FIELD + Integer.toString(queryCount));
                queryString = (String) getParameter(PARAM_QUERY_BY_CRITERIA + Integer.toString(queryCount));
            }

            if (applySchool) {
                /*
                 * Add School criteria.
                 */
                if (isSchoolContext()) {
                    DataDictionaryTable table =
                            getDataDictionary().findDataDictionaryTableByClass(getBeanClassX2().getName());
                    if (table != null && !StringUtils.isEmpty(table.getDataTableConfig().getSchoolPath())) {
                        ModelProperty property = new ModelProperty(table.getDataTableConfig().getSchoolPath(),
                                getBroker().getPersistenceKey());
                        criteria.addEqualTo(fullPrefixPath + property.getBeanPath(), getSchool().getOid());
                    }
                }

                /*
                 * Add Organization criteria.
                 */
                DataDictionaryTable table =
                        getDataDictionary().findDataDictionaryTableByClass(getBeanClassX2().getName());
                if (table != null && !StringUtils.isEmpty(table.getDataTableConfig().getOrganizationPath())) {
                    int level = getOrganization().getOrganizationDefinition().getLevel();
                    DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
                    Collection<ModelProperty> properties =
                            OrganizationManager.getOrganizationPaths(table.getBeanClass(), dictionary, level);

                    if (!CollectionUtils.isEmpty(properties)) {
                        for (ModelProperty property : properties) {
                            criteria.addAndCriteria(OrganizationManager.getOrganizationAccessCriteria(getOrganization(),
                                    property, OrganizationAccess.NONE, OrganizationAccess.READ_WRITE));
                        }
                    }
                }
            }
        }

        /**
         * Apply the standard user input sort from input template to a Query object.
         * The input value PARAM_SORT_BY_FIELDS can contain multiple sort options.
         *
         * The prefix path is the relationship bean path from to apply to the current bean to take
         * it to
         * the
         * expected base bean of the criteria.
         * EX: If the user input selection criteria is for the Student table, but the criteria is
         * for a
         * related
         * table StudentEnrollment, the prefix path would be "student" (or
         * StudentEnrollment.REL_STUDENT)
         * to get from the StudentEnrollment to the Student table.
         *
         * @param prefixPath String
         * @return ToolBeanColumns
         */
        public List<ToolBeanColumn> applyInputSort(String prefixPath) {
            List<ToolBeanColumn> columns = new ArrayList();
            String sortBy = (String) getParameter(PARAM_SORT_BY_FIELDS);
            String fullPrefixPath = EMPTY_STRING;
            if (!StringUtils.isEmpty(prefixPath)) {
                fullPrefixPath = prefixPath + ModelProperty.PATH_DELIMITER;
            }

            if (sortBy != null) {
                Collection<String> sortParameters = StringUtils.convertDelimitedStringToList(sortBy, ',', true);
                for (String sortParameter : sortParameters) {
                    String sortOrder = null;
                    String sortByField = sortParameter;

                    if (sortParameter.contains(BAR_SEPARATOR)) {
                        int sortOrderKeyPosition = sortParameter.indexOf(BAR_SEPARATOR);
                        sortOrder = sortParameter.substring(sortOrderKeyPosition + 1).trim();
                        sortByField = sortParameter.substring(0, sortOrderKeyPosition).trim();
                    }
                    if (sortByField.startsWith("a:")) {
                        sortByField = getResolvedAliasBeanPath(sortByField);
                    }
                    if (sortByField != null) {
                        columns.add(
                                new ToolBeanColumn(fullPrefixPath + sortByField, !"DESC".equalsIgnoreCase(sortOrder)));
                    }
                }
            }
            return columns;
        }

        /**
         * Clear all setup errors.
         */
        public void clearSetupErrors() {
            if (m_setupErrors != null) {
                m_setupErrors.clear();
            }
        }

        /**
         * Closes the query iterator used for retrieving the data elements of the export.
         */
        public void close() {
            // no default action
        }

        /**
         * Returns the class of the base bean used for this export.
         *
         * @return Class
         */
        public Class<? extends ToolBean> getBeanClassTool() {
            return ToolBean.getRegisteredClass(getBeanClassX2());
        }

        /**
         * Returns the class of the base bean used for this export.
         *
         * @return Class
         */
        public Class<? extends X2BaseBean> getBeanClassX2() {
            return m_x2BeanClass;
        }

        /**
         * Return the broker provided in initialization.
         * This is useful for local and inner subclass use.
         *
         * @return X2Broker
         */
        public X2Broker getBroker() {
            return m_broker;
        }

        /**
         * Gets the class loader.
         *
         * @return Class loader
         */
        public ClassLoader getClassLoader() {
            return m_classLoader;
        }

        /**
         * Gets the current context.
         *
         * @return District school year context
         */
        public DistrictSchoolYearContext getCurrentContext() {
            return m_currentContext;
        }

        /**
         * Returns a local instance of a district data dictionary.
         *
         * @return DataDictionary.
         */
        public DataDictionary getDataDictionary() {
            if (m_dictionary == null) {
                String ddxId = null;
                if (m_definition != null && m_definition.getExtendedDataDictionary() != null) {
                    ddxId = m_definition.getExtendedDataDictionary().getId();
                }
                m_dictionary = getDictionaryExtractor().getDictionary(ddxId);
            }

            return m_dictionary;
        }

        /**
         * Gets the dictionary extractor.
         *
         * @return Dictionary extractor
         */
        public DictionaryExtractor getDictionaryExtractor() {
            if (m_dictionaryExtractor == null) {
                m_dictionaryExtractor = new DictionaryExtractor(getBroker());
            }
            return m_dictionaryExtractor;
        }

        /**
         * Returns the oid of the EFD record that defines this export, if any.
         *
         * @return String
         */
        public String getEfdOid() {
            return m_efdOid;
        }

        /**
         * Returns the name of the entity for the report.
         * (EX: "Student" for SIMS)
         *
         * @return String - title
         */
        public String getEntityTitle() {
            DataDictionaryTable table = getDataDictionary().findDataDictionaryTableByClass(getBeanClassX2().getName());

            return table.getUserName();
        }

        /**
         * Returns a character for a specific purpose:
         * This is the escape character.
         *
         * @return Character
         */
        public Character getEscapeCharacter() {
            return m_escapeChar;
        }

        /**
         * Returns the title of the export/report for report heading.
         *
         * @return String - title
         */
        public String getExportTitle() {
            return m_exportName;
        }


        /**
         * Returns a count of fields implemented by this data object.
         * Fields will be valid for index values 0 through one less than count.
         *
         * @return int - count of fields.
         */
        public int getFieldCount() {
            int count = 0;
            if (m_currentFieldDefinitions != null) {
                count = m_currentFieldDefinitions.size();
            }

            return count;
        }

        /**
         * Returns a count of fields implemented by this data object.
         * Fields will be valid for index values 0 through one less than count.
         * fieldsKey selects any field set in the loaded fields. This allows
         * count of any field set, not just the currently active one.
         *
         * @param fieldsKey String
         * @return int - count of fields.
         */
        public int getFieldCount(String fieldsKey) {
            int count = 0;
            List<FieldDefinition> list = m_loadedFieldDefinitions.get(fieldsKey);
            if (list != null) {
                count = list.size();
            }

            return count;
        }

        /**
         * Returns the field definition for a field.
         *
         * @param index - the field to return. This must be from zero to one less than field count.
         *
         * @return FieldDefinition
         */
        public FieldDefinition getFieldDefinition(int index) {
            FieldDefinition field = null;

            if (m_currentFieldDefinitions != null) {
                if (index < m_currentFieldDefinitions.size() && index >= 0) {
                    field = m_currentFieldDefinitions.get(index);
                }
            }

            return field;
        }

        /**
         * Returns the field definition for a field.
         *
         * @param stateName - name of the field to return.
         *
         * @return FieldDefinition
         */
        public FieldDefinition getFieldDefinition(String stateName) {
            FieldDefinition field = null;

            if (m_currentFieldDefinitions != null) {
                for (FieldDefinition fd : m_currentFieldDefinitions) {
                    if (fd.getFieldId().equals(stateName)) {
                        field = fd;
                        break;
                    }
                }
            }

            return field;
        }

        /**
         * Returns the list of field definitions.
         *
         * @return List<FieldDefinition>
         */
        public List<FieldDefinition> getFieldDefinitions() {
            return m_currentFieldDefinitions;
        }

        /**
         * Gets the filterable.
         *
         * @return the filterable
         */
        public Filterable<? extends ToolBean> getFilterable() {
            if (m_filterable == null) {
                Class<? extends ToolBean> toolBeanClass = getBeanClassTool();
                X2Criteria criteria = new X2Criteria();

                // Apply criteria from tool input.
                applyInputCriteria(criteria, true, null);
                m_filterable = FilterableFactory.create(getBroker(), getDictionaryExtractor(), toolBeanClass, criteria,
                        applyInputSort(null));
            }
            return m_filterable;
        }

        /**
         * Gets the format definition.
         *
         * @return Export format definition
         */
        public ExportFormatDefinition getFormatDefinition() {
            return m_definition;
        }

        /**
         * Returns a string heading to include at the top of an export file.
         * Implementing classes may choose to override this method to provide
         * a custom heading string.
         * The default value is the heading value from ExportFormatDefinition.
         *
         * @return String - heading text
         */
        public String getHeading() {
            String heading = EMPTY_STRING;
            if (!StringUtils.isEmpty(m_heading)) {
                heading = m_heading;
                if (!m_heading.endsWith(ExportJavaSource.FORMAT_EOL_UNIX) &&
                        !m_heading.endsWith(ExportJavaSource.FORMAT_EOL_WINDOWS)) {
                    heading += ExportJavaSource.FORMAT_EOL_WINDOWS;
                }
            }

            return heading;
        }

        /**
         * Returns an indicator for the export formatter indicating:
         * This export should generate a heading row from field names.
         *
         * We typically do not want this and should use the heading text
         * from the ExportFormatDefinition or getHeading() to indicate if
         * a heading is needed and what the heading should be.
         *
         * @return boolean
         */
        public boolean getIncludeHeaderRow() {
            return false;
        }

        /**
         * Returns the organization set in the report or export parameters.
         *
         * @return Organization
         */
        public Organization getOrganization() {
            return m_organization;
        }

        /**
         * Returns a parameter as specified in the input from the user for this report run.
         *
         * @param key String
         * @return Object - value for passed key
         */
        public Object getParameter(String key) {
            return m_parameters.get(key);
        }

        /**
         * Returns the Procedure of the export.
         *
         * @return String - the Procedure ID
         */
        public Procedure getProcedure() {
            return m_procedure;
        }

        /**
         * Returns the Procedure ID of the export.
         *
         * @return String - the Procedure ID
         */
        public String getProcedureId() {
            return m_procedureId;
        }

        /**
         * Gets the property.
         *
         * @param bean ToolBean
         * @param beanPath String
         * @return Object
         * @throws X2BaseException exception
         */
        public Object getProperty(ToolBean bean, String beanPath) throws X2BaseException {
            Object value = null;
            if (beanPath.charAt(0) != LABEL_PREFIX_CHAR) {
                try {
                    value = bean.getFieldValueByColumnName(beanPath, true);
                } catch (X2BaseException e) {
                    value = WebUtils.getProperty(bean, beanPath);
                }
            }
            return value;
        }

        /**
         * Returns the resolved bean path of the passed alias field.
         * <p>
         * Detect if the path contains an alias request. And alias request can be either:
         * <ul>
         * <li>prefix a:</li>
         * <li>last field in square brackets []</li>
         * </ul>
         * <p>
         * Returns the bean path if there is not an alias requested.
         * <p>
         * Returns the translated bean path (with a: and [] removed) with the alias translated to
         * the
         * proper bean path.
         * <p>
         * Returns NULL if there is an alias request and the alias cannot be resolved.
         *
         * @param beanPath - sort by field for an alias
         *
         * @return String
         */
        public String getResolvedAliasBeanPath(String beanPath) {
            String resolvedAliasBeanPath = null;
            boolean isAliasRequest = false;

            if (beanPath.startsWith("a:")) {
                beanPath = beanPath.substring(2).trim();
                isAliasRequest = true;
            }

            String[] relationshipPaths = beanPath.split("\\.");

            String lastField = relationshipPaths[(relationshipPaths.length) - 1];
            int position = beanPath.indexOf(lastField);
            if (lastField.startsWith("[") && lastField.endsWith("]")) {
                lastField = lastField.substring(0, lastField.length() - 1).substring(1);
                isAliasRequest = true;
            }

            if (isAliasRequest) {
                DataDictionaryField aliasField =
                        getDataDictionary().findDataDictionaryFieldByAlias(lastField);

                if (aliasField != null) {
                    if (position == 0) {
                        resolvedAliasBeanPath = aliasField.getJavaName();
                    } else {
                        resolvedAliasBeanPath = beanPath.substring(0, position) + aliasField.getJavaName();
                    }
                }
            } else {
                resolvedAliasBeanPath = beanPath;
            }

            return resolvedAliasBeanPath;
        }

        /**
         * Returns the School object.
         * As specified in the input template and context.
         *
         * @return School
         */
        public School getSchool() {
            return m_school;
        }

        /**
         * Gets the script manager.
         *
         * @return ScriptManager
         */
        public ScriptManager getScriptManager() {
            return m_scriptManager;
        }

        /**
         * Return a collection of errors encountered during initialization.
         * These may indicate improper parameters or missing system configuration.
         *
         * @return Collection<StateReportValidationError>
         */
        public Collection<StateReportValidationError> getSetupErrors() {
            return m_setupErrors;
        }

        /**
         * Returns a string trailer to include at the bottom of an export file.
         * Implementing classes may choose to override this method to provide
         * a custom trailer string.
         *
         * @return String - trailer text
         */
        public String getTrailer() {
            String trailer = EMPTY_STRING;
            if (!StringUtils.isEmpty(m_trailer)) {
                trailer = m_trailer;
                if (!m_trailer.endsWith(ExportJavaSource.FORMAT_EOL_UNIX) &&
                        !m_trailer.endsWith(ExportJavaSource.FORMAT_EOL_WINDOWS)) {
                    trailer += ExportJavaSource.FORMAT_EOL_WINDOWS;
                }
            }

            return trailer;
        }

        /**
         * Returns an indicator for the export formatter indicating:
         * This export should use an escape character.
         *
         * @return boolean
         */
        public boolean getUseEscapes() {
            return m_useEscape;
        }

        /**
         * Returns the user specified in the export run properties.
         *
         * @return User
         */
        public User getUser() {
            return m_user;
        }

        /**
         * Returns an indicator for the export formatter indicating:
         * This export should use a value delimiter character.
         *
         * @return boolean
         */
        public boolean getUseValueDelimiters() {
            return m_useDelimiter;
        }

        /**
         * Returns an indicator for the export formatter indicating:
         * This export should use value wrappers.
         *
         * @return boolean
         */
        public boolean getUseValueWrappers() {
            return m_useWrapper;
        }

        /**
         * Returns a character for a specific purpose:
         * This is the value delimiter character (usually ,).
         *
         * @return Character
         */
        public Character getValueDelimiter() {
            return m_delimiterChar;
        }

        /**
         * Returns a character for a specific purpose:
         * This is the value wrapper character (usually ").
         *
         * @return Character
         */
        public Character getValueWrapper() {
            return m_wrapperChar;
        }

        /**
         * Superclass initialize method. This performs superclass initialization, then
         * calls initialize() to allow subclasses to initialize.
         *
         * @throws X2BaseException exception
         */
        public final void initializeExport() throws X2BaseException {
            m_calcMap = new HashMap<String, FieldRetriever>();
            m_validatorMap = new HashMap<String, FieldValidator>();

            // TODO: Replace input definition parsing
            // TODO: Replace pre-defined retrievers


            // Call initialization for extending classes.
            initialize();

            // verify ToolBeanClass
            Class<? extends ToolBean> toolBeanClass = getBeanClassTool();
            if (toolBeanClass == null) {
                addSetupError("Class Exception",
                        "A ToolBean class must be registered for " + getBeanClassX2().getName());
            }

            // After initialization, apply calculation map to all fields list.
            for (List<FieldDefinition> fields : m_loadedFieldDefinitions.values()) {
                for (FieldDefinition field : fields) {
                    if (!StringUtils.isEmpty(field.getCalcId())) {
                        field.setRetriever(m_calcMap.get(field.getCalcId()));
                    }
                    if (!StringUtils.isEmpty(field.getValidatorId())) {
                        field.setValidator(m_validatorMap.get(field.getValidatorId()));
                    }
                    if (field.getRetriever() == null && StringUtils.isEmpty(field.getScriptFunctionName())
                            && !StringUtils.isEmpty(field.getBeanPath())) {

                        if (field.getBeanPath().charAt(0) != StateReportData.LABEL_PREFIX_CHAR
                                && !ToolBean.hasColumn(toolBeanClass, field.getBeanPath(), getDictionaryExtractor())) {
                            addSetupError("Bean path exception",
                                    "The column in " + toolBeanClass.getName() + " for column " + field.getBeanPath()
                                            + " cannot be found");
                        }
                    }
                }
            }
        }

        /**
         * Return an indicator of whether the export is running in a school context.
         *
         * @return boolean
         */
        public boolean isSchoolContext() {
            return m_isSchoolContext;
        }

        /**
         * This method will cause the data class to load report and field definitions
         * from the CustomExportDefinitions tables.
         *
         * @param procedureId String
         * @param broker X2Broker
         * @return Collection<StateReportValidationError>
         */
        public Collection<StateReportValidationError> loadDefinitions(String procedureId, X2Broker broker) {
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            try {
                m_scriptManager = new ScriptManager(ScriptManager.ENGINE_JAVASCRIPT);
            } catch (ScriptException e) {
                addSetupError("ScriptException", "Invalid construction of script manager");
            }

            X2Criteria criteria = new X2Criteria();
            criteria.addLike(ExportFormatDefinition.COL_PROCEDURE_ID, procedureId + "%");
            QueryByCriteria query = new QueryByCriteria(ExportFormatDefinition.class, criteria);
            query.addOrderBy(ExportFormatDefinition.COL_PROCEDURE_ID, true);
            QueryIterator definitionIterator = broker.getIteratorByQuery(query);
            try {
                while (definitionIterator.hasNext()) {
                    ExportFormatDefinition definition = (ExportFormatDefinition) definitionIterator.next();

                    // Set all global attributes based on the first definition record.
                    if (m_definition == null) {
                        m_definition = definition;
                        setEfdOid(m_definition.getOid());
                        m_exportName = m_definition.getName();

                        // Load definition based values: class, formatting parameters.
                        if (m_definition.getSourceTable() != null) {
                            m_x2BeanClass = m_definition.getSourceTable().getDataTable().getDataClass();
                        }

                        if (!StringUtils.isEmpty(m_definition.getEscapeChar())) {
                            // Converted escaped characters into the true non-printable character as
                            // a
                            // delimiter.
                            String formatedChar = m_definition.getEscapeChar();
                            if (m_escapedChars.containsKey(formatedChar)) {
                                formatedChar = m_escapedChars.get(formatedChar);
                            }
                            m_escapeChar = Character.valueOf(formatedChar.charAt(0));
                            m_useEscape = true;
                        } else {
                            m_useEscape = false;
                        }

                        if (!StringUtils.isEmpty(m_definition.getWrapperChar())) {
                            // Converted escaped characters into the true non-printable character as
                            // a
                            // delimiter.
                            String formatedChar = m_definition.getWrapperChar();
                            if (m_escapedChars.containsKey(formatedChar)) {
                                formatedChar = m_escapedChars.get(formatedChar);
                            }
                            m_wrapperChar = Character.valueOf(formatedChar.charAt(0));
                            m_useWrapper = true;
                        } else {
                            m_useWrapper = false;
                        }

                        if (!StringUtils.isEmpty(m_definition.getDelimiterChar())) {
                            // Converted escaped characters into the true non-printable character as
                            // a
                            // delimiter.
                            String formatedChar = m_definition.getDelimiterChar();
                            if (m_escapedChars.containsKey(formatedChar)) {
                                formatedChar = m_escapedChars.get(formatedChar);
                            }
                            m_delimiterChar = Character.valueOf(formatedChar.charAt(0));
                            m_useDelimiter = true;
                        } else {
                            m_useDelimiter = false;
                        }

                        if (!StringUtils.isEmpty(m_definition.getHeading())) {
                            m_heading = m_definition.getHeading();
                        }

                        if (!StringUtils.isEmpty(m_definition.getTrailer())) {
                            m_trailer = m_definition.getTrailer();
                        }
                    }

                    /*
                     * Working bean class is the class defined in the current definition as opposed
                     * to
                     * the first.
                     * this allows the entity to provide different bean classes for different row
                     * types.
                     */
                    Class workingBeanClass = definition.getSourceTable().getDataTable().getDataClass();

                    /*
                     * Prepare field key. This is the portion of the individual
                     * format definitions procedureId after the base procedureId.
                     * procedureId + "-" + fieldKey
                     * or just
                     * procedureId => fieldKey is null.
                     */
                    String fieldKey = definition.getProcedureId();
                    fieldKey = fieldKey.substring(procedureId.length());
                    if (fieldKey.length() == 0) {
                        fieldKey = null;
                    } else if (fieldKey.startsWith("-")) {
                        fieldKey = fieldKey.substring(1);
                    }

                    // Prepare fields list.
                    ArrayList<FieldDefinition> fields = new ArrayList<FieldDefinition>();

                    // Load fields in order of position.
                    criteria = new X2Criteria();
                    criteria.addEqualTo(ExportFormatField.COL_DEFINITION_OID, definition.getOid());
                    query = new QueryByCriteria(ExportFormatField.class, criteria);
                    query.addOrderBy(ExportFormatField.COL_POSITION, true);
                    QueryIterator fieldIterator = broker.getIteratorByQuery(query);
                    try {
                        // Load fields list and definitions.
                        while (fieldIterator.hasNext()) {
                            ExportFormatField field = (ExportFormatField) fieldIterator.next();
                            FieldDefinition newField = null;

                            try {
                                newField = new FieldDefinition(field, workingBeanClass, broker);
                                // If this field has a script, initialize it
                                if (!StringUtils.isEmpty(field.getScript())) {
                                    m_scriptManager.initializeScript(newField.getScriptFunction());
                                }
                                fields.add(newField);
                            } catch (X2BaseException x2be) {
                                errors.add(new StateReportValidationError(definition.getName(), field.getName(),
                                        x2be.getMessage(), field.getFieldPath()));
                            }
                        }
                    } finally {
                        fieldIterator.close();
                    }

                    setFieldDefinitions(fieldKey, fields);
                }
            } finally {
                definitionIterator.close();
            }

            return errors;
        }

        /**
         * Returns the next entity in the export list, wrapped in a subclass of StateReportEntity.
         * The appropriate StateReportEntity subclass must have been defined in initialize method
         * by calling setEntityClass.
         *
         * @return StateReportEntity - null if the iterator is empty
         * @throws X2BaseException exception
         */
        public StateReportEntity next() throws X2BaseException {
            StateReportEntity entity = null;

            // See if the current entity can iterate more. If not, abandon it.
            if (m_currentEntity != null) {
                if (m_currentEntity.getCurrentRow() + 1 < m_currentEntity.getRowCount()) {
                    m_currentEntity.setCurrentRow(m_currentEntity.getCurrentRow() + 1);
                    entity = m_currentEntity;
                } else {
                    m_currentEntity = null;
                }
            }

            /*
             * If multiple queries are used, and the current query/iterator is exhausted,
             * open the next available query/iterator.
             */
            boolean iteratorHasNext = false;
            if (m_iterator != null) {
                iteratorHasNext = m_iterator.hasNext();
            }
            /*
             * If the last entity was exhausted, get another from the iterator.
             * Entities may generate zero rows, if so skip them and get the
             * next from the iterator until it is exhausted too.
             */
            while (entity == null && getEntityClass() != null && m_iterator != null && iteratorHasNext) {
                ToolBean bean = m_iterator.next();
                if (bean != null) {
                    try {
                        entity = (StateReportEntity) getEntityClass().getDeclaredConstructor().newInstance();
                    } catch (ClassCastException | InstantiationException | IllegalAccessException
                            | IllegalArgumentException
                            | InvocationTargetException | NoSuchMethodException | SecurityException e) {
                        throw new X2BaseException(e);
                    }

                    if (entity != null) {
                        entity.intitialize(this, bean);
                        if (entity.getRowCount() > 0) {
                            entity.setCurrentRow(0);
                            m_currentEntity = entity;
                        } else {
                            entity = null;
                        }
                    }
                }

                iteratorHasNext = m_iterator.hasNext();
            }

            // Get the correct fields set for this entity/iteration based on the entity requested
            // definition Id.
            if (entity != null) {
                String fieldKey = entity.getCurrentFormatDefinitionId();
                m_currentFieldDefinitions = m_loadedFieldDefinitions.get(fieldKey);
                if (m_currentFieldDefinitions == null) {
                    m_currentFieldDefinitions = new ArrayList<FieldDefinition>();
                }
            }

            return entity;
        }

        /**
         * Open the query iterator based on the query provided
         * by the implementing class.<br>
         * <code>close()</code> must be called after an open.
         * Otherwise, resources will be left open.
         *
         * @return boolean - true if the open was successful, false otherwise.
         */
        public boolean open() {
            Filterable<? extends ToolBean> filterable = getFilterable();
            if (filterable != null) {
                m_iterator = filterable.extract().iterator();
            }
            return (m_iterator != null);
        }

        /**
         * This is called at the end of the validation loop.
         * Give the data module a chance to perform final totals validation and return totals data
         * set
         * result errors.
         *
         * @return Collection<StateReportValidationError>
         */
        public Collection<StateReportValidationError> postProcess() {
            return new ArrayList<StateReportValidationError>();
        }

        /**
         * Sets the X2Broker for this data retrieval process.
         * This should be set by the calling process (report or export).
         *
         * @param broker void
         */
        public void setBroker(X2Broker broker) {
            m_broker = broker;
        }

        /**
         * Sets the DistrictSchoolYearContext for this data retrieval process. THis should be set by
         * the calling process (report or export).
         *
         * @param currentContext void
         */
        public void setCurrentContext(DistrictSchoolYearContext currentContext) {
            m_currentContext = currentContext;
        }

        /**
         * Sets the oid of the EFD record that defines this export, if any.
         *
         * @param efdOid void
         */
        public void setEfdOid(String efdOid) {
            m_efdOid = efdOid;
        }

        /**
         * Sets the filterable.
         *
         * @param filterable void
         */
        public void setFilterable(Filterable<? extends ToolBean> filterable) {
            m_filterable = filterable;
        }

        /**
         * Sets the list of field definitions for this export.
         *
         * @param fieldDefinitions void
         */
        protected void setFieldDefinitions(List<FieldDefinition> fieldDefinitions) {
            m_loadedFieldDefinitions.put(null, fieldDefinitions);
            m_currentFieldDefinitions = fieldDefinitions;
        }

        /**
         * Set the input definition of the export.
         *
         * @param inputDefinition void
         */
        public void setInputDefinition(String inputDefinition) {
            this.m_inputDefinition = inputDefinition;
        }

        /**
         * Sets the district object for this data retrieval process.
         * This should be set by the calling process (report or export).
         *
         *
         * @param organization void
         */
        public void setOrganization(Organization organization) {
            m_organization = organization;
        }

        /**
         * Sets the user input parameters for this data retrieval process.
         * This should be set by the calling process (report or export).
         *
         *
         * @param parameters Map<String,Object>
         */
        public void setParameters(Map<String, Object> parameters) {
            m_parameters = parameters;
        }

        /**
         * Sets the user privilege set for this data retrieval process.
         * This should be set by the calling process (report or export).
         *
         *
         * @param privSet void
         */
        public void setPrivilegeSet(PrivilegeSet privSet) {
            m_privSet = privSet;
        }

        /**
         * Sets the procedure of the export.
         *
         * @param procedure void
         */
        public void setProcedure(Procedure procedure) {
            m_procedure = procedure;
        }

        /**
         * Sets the procedure ID of the export.
         *
         * @param procedureId void
         */
        public void setProcedureId(String procedureId) {
            m_procedureId = procedureId;
        }

        /**
         * Sets the user selected school for this data retrieval process.
         * This should be set by the calling process (report or export).
         *
         *
         * @param school void
         */
        public void setSchool(School school) {
            m_school = school;
        }

        /**
         * Sets the school input context flag for this data retrieval process.
         * This should be set by the calling process (report or export).
         *
         *
         * @param context void
         */
        public void setSchoolContext(boolean context) {
            m_isSchoolContext = context;
        }

        /**
         * Sets the user object from the export runtime.
         *
         * @param user void
         */
        public void setUser(User user) {
            m_user = user;
        }

        /**
         * Adds custom calculation map to the built-in calculation map.
         * This allows extending implementations to add their own
         * calculation FieldRetrievers.
         *
         * @param calcMap Map<String,FieldRetriever>
         */
        protected void addCalcs(Map<String, FieldRetriever> calcMap) {
            m_calcMap.putAll(calcMap);
        }

        /**
         * Adds custom validator map to the built-in validator map.
         * This allows extending implementations to add their own
         * FieldValidators.
         *
         * @param validatorMap Map<String,FieldValidator>
         */
        protected void addValidators(Map<String, FieldValidator> validatorMap) {
            m_validatorMap.putAll(validatorMap);
        }

        /**
         * Returns the defined StateReportEntity subclass type.
         *
         * @return Class
         */
        protected Class getEntityClass() {
            return m_entityClass;
        }

        /**
         * Sets the StateReportEntity subclass to use for this export.
         *
         * @param entityClass void
         */
        protected void setEntityClass(Class entityClass) {
            m_entityClass = entityClass;
        }

        /**
         * Initialize the data set.
         * The parameters map comes from the export tool input template.
         * The implementing class should:
         * 1. Implement any abstract method of this class.
         * 2. Build a list of field definitions
         * 3. Build a query for the bean class of the export.
         * 4. Define and specify an StateReportEntity class to be used in the export.
         *
         *
         * @throws X2BaseException exception
         */
        protected void initialize() throws X2BaseException {
            // Empty block. Subclasses may implement.
        }

        /**
         * Sets the list of field definitions for this export.
         *
         * @param id String
         * @param fieldDefinitions List<FieldDefinition>
         */
        protected void setFieldDefinitions(String id, List<FieldDefinition> fieldDefinitions) {
            m_loadedFieldDefinitions.put(id, fieldDefinitions);
            if (id == null) {
                m_currentFieldDefinitions = fieldDefinitions;
            }
        }

        /**
         * Sets the class loader.
         *
         * @param classLoader void
         */
        private void setClassLoader(ClassLoader classLoader) {
            m_classLoader = classLoader;
        }
    }

    /**
     * The Class StateReportEntity.
     *
     * @author Follett Software Company
     * @copyright 2021
     */
    public static class StateReportEntity {
        /**
         * lookup keys for standard error messages.
         */
        private static final String ERROR_EXCEPTION = "error.state.report.exception";
        private static final String ERROR_INVALID_VALUE_KEY = "error.state.report.invalidvalue";
        private static final String ERROR_LONG_VALUE_KEY = "error.state.report.longvalue";
        private static final String ERROR_MISSING_VALUE_KEY = "error.state.report.missingvalue";
        private static final String ERROR_SHORT_VALUE_KEY = "error.state.report.shortvalue";


        /**
         * Instance variables.
         */
        private ToolBean m_bean = null;
        private int m_currentRow = 0;
        private StateReportData m_data;
        private String[] m_fieldValues = null;
        private Collection<StateReportValidationError>[] m_fieldRetrievealErrors = null;
        private int m_rowCount = 1;
        private String m_schoolOid;
        private ScriptManager m_scriptManager;

        /**
         * Add a retrieval error to the list based on field ID of the field definition.
         * This should only be called from retrieval code for
         * retrieval related errors.
         *
         * @param fieldId String
         * @param error StateReportValidationError
         */
        public void addRetrievalError(String fieldId, StateReportValidationError error) {
            for (int pos = 0; pos < m_data.getFieldCount(); pos++) {
                if (m_data.getFieldDefinition(pos).getFieldId().equals(fieldId)) {
                    addRetrievalError(pos, error);
                    break;
                }
            }
        }

        /**
         * Add an error to the retrieval errors list for a field based
         * of the index position of the field in data.getFieldDefinition().
         * This should only be called from retrieval code for
         * retrieval related errors.
         *
         * @param index field index number for the error.
         * @param error StateReportValidationError
         */
        public void addRetrievalError(int index, StateReportValidationError error) {
            if (index >= 0 && index < m_fieldRetrievealErrors.length) {
                if (m_fieldRetrievealErrors[index] == null) {
                    m_fieldRetrievealErrors[index] = new ArrayList<StateReportValidationError>();
                }
                m_fieldRetrievealErrors[index].add(error);
            }
        }

        /**
         * Returns an error message or null indicating if this entity should be filtered out of the
         * export or included.
         * Implementing classes can perform run time filtering on entities.
         *
         * @return a validation error if this entity should not be exported.
         *         a null indicates the entity should be exported.
         */
        public StateReportValidationError filterEntity() {
            return null;
        }

        /**
         * Returns the enclosed X2BaseBean for this entity.
         *
         * @return an x2bean
         */
        public ToolBean getBean() {
            return m_bean;
        }

        /**
         * For multi-format reports, this returns a suffix for identifying which set of format
         * fields
         * to use to display the current row.
         * For standard or single format reports, null should select the default format definition.
         *
         * The selection of the definition Id to return should be based on the getCurrentRow()
         * value.
         *
         * @return String
         */
        public String getCurrentFormatDefinitionId() {
            return null;
        }

        /**
         * Returns the current data row the entity is prepared to produce.
         * When an entity can product more than one row of export data, this
         * indicates which row.
         *
         * @return the current row from 0 to (row count - 1)
         */
        public int getCurrentRow() {
            return m_currentRow;
        }

        /**
         * Returns a reference to the StateReportData that this entity belongs to.
         *
         * @return State report data
         */
        public StateReportData getData() {
            return m_data;
        }

        /**
         * Returns a string display name representing the data element in this X2BaseBean.
         *
         * The default behavior is to get the identifying fields in the data dictionary
         * for the table, and assemble those fields as the name of the bean.
         *
         * Overriding classes can produce a more representative display name,
         *
         * @return String
         */
        public String getEntityName() {
            return getBean().getClass().getSimpleName() + " " + getBean().getOid();
        }

        /**
         * Returns any field validations that occurred for a given field.
         * Also include retrieval errors, errors that occur trying to
         * look up the value from the database.
         *
         * @param index int
         * @return a collection of validation errors
         */
        public Collection<StateReportValidationError> getFieldValidation(int index) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            // Retrieve the value. If it has not been retrieved already,
            // this will retrieve it and set any retrieval errors.
            String value = getFieldValue(index);

            // Pickup any errors that occured during data retrieval.
            errors.addAll(m_fieldRetrievealErrors[index]);

            // Get the definition and check all validation parameters there.
            FieldDefinition field = m_data.getFieldDefinition(index);
            if (field != null) {
                // Check value existence and size.
                if (value == null) {
                    String message = LocalizationCache.getMessages(getData().getBroker().getPersistenceKey())
                            .getMessage(ERROR_MISSING_VALUE_KEY);
                    StateReportValidationError error = new StateReportValidationError(this, field, message, value);
                    errors.add(error);
                } else if (value.length() < field.getMinLength()) {
                    String message = null;
                    if (StringUtils.isEmpty(value)) {
                        message = LocalizationCache.getMessages(getData().getBroker().getPersistenceKey())
                                .getMessage(ERROR_MISSING_VALUE_KEY);
                    } else {
                        message = LocalizationCache.getMessages(getData().getBroker().getPersistenceKey())
                                .getMessage(ERROR_SHORT_VALUE_KEY);
                    }
                    StateReportValidationError error = new StateReportValidationError(this, field, message, value);
                    errors.add(error);
                } else if (value.length() > field.getMaxLength()) {
                    String message = LocalizationCache.getMessages(getData().getBroker().getPersistenceKey())
                            .getMessage(ERROR_LONG_VALUE_KEY);
                    StateReportValidationError error = new StateReportValidationError(this, field, message, value);
                    errors.add(error);
                } else {
                    // Check regex pattern match.
                    if (field.getPattern() != null && !value.matches(field.getPattern())) {
                        String message = LocalizationCache.getMessages(getData().getBroker().getPersistenceKey())
                                .getMessage(ERROR_INVALID_VALUE_KEY);
                        StateReportValidationError error = new StateReportValidationError(this, field, message, value);
                        errors.add(error);
                    }
                    if (field.getValidator() != null) {
                        // Check custom validator.
                        errors.addAll(field.getValidator().getFieldValidation(m_data, this, field, value));
                    }
                }
            }
            return errors;
        }

        /**
         * Returns all field validation errors for all fields.
         *
         * @return a Collection of field validation errors.
         */
        public Collection<StateReportValidationError> getFieldValidations() {
            int count = m_data.getFieldCount();
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            for (int pos = 0; pos < count; pos++) {
                errors.addAll(getFieldValidation(pos));
            }
            return errors;
        }

        /**
         * Retrieve the value of one field. This can include several steps.
         * Check the cache to see if the value is already retrieved.
         * Check FieldDefinition for field source.
         *
         * @param index int
         * @return the field value
         */
        public String getFieldValue(int index) {
            // Check the field cache to see if this value has already been retrieved.
            String value = getCachedFieldValue(index);

            // Set on exception
            Exception exception = null;

            if (value == null) {
                if (m_fieldRetrievealErrors[index] == null) {
                    m_fieldRetrievealErrors[index] = new ArrayList<StateReportValidationError>();
                }
                FieldDefinition field = m_data.getFieldDefinition(index);
                if (field != null) {
                    Object baseValue = null;
                    // Check if there is a custom retriever object for this value.
                    if (field.getRetriever() != null) {
                        try {
                            baseValue = field.getRetriever().getFieldValue(m_data, this, field);
                        } catch (X2BaseException x2be) {
                            exception = x2be;
                        }
                    } else if (!StringUtils.isEmpty(field.getScriptFunctionName())) {
                        try {
                            baseValue = m_scriptManager.invokeFunction(field.getScriptFunctionName(),
                                    new Object[] {m_data, this, field});
                        } catch (NoSuchMethodException | ScriptException ex) {
                            exception = ex;
                        }
                    } else {
                        // Look up the value through the bean.
                        try {
                            if (field.getBeanPath().charAt(0) != StateReportData.LABEL_PREFIX_CHAR) {
                                baseValue = getBean().getFieldValueByColumnName(field.getBeanPath(), true);
                            }
                        } catch (X2BaseException x2be) {
                            exception = x2be;
                        }
                    }
                    // Check default value.
                    if (baseValue == null || ((baseValue instanceof String) && ((String) baseValue).length() == 0)) {
                        baseValue = field.getDefaultValue();
                    } else {
                        // See if the value needs to be mapped to a reference code lookup value.
                        if (field.getMappedLookup() != ExportFormatField.ReferenceMapTypeCode.NONE.ordinal()) {
                            baseValue = getData().getDictionaryExtractor().lookupReferenceCodeByBeanPath(
                                    getBean().getClass(), field.getBeanPath(),
                                    (String) baseValue, field.getMappedLookup());
                        }

                        // See if the value needs a formatter for display.
                        if (field.getFormatter() != null) {
                            try {
                                if (baseValue instanceof String && field.getConverter() != null) {
                                    baseValue = field.getConverter().parseSystemString((String) baseValue);
                                }
                                if (baseValue != null) {
                                    baseValue = field.getFormatter().format(baseValue);
                                }
                            } catch (RuntimeException re) {
                                StateReportValidationError error =
                                        new StateReportValidationError(this, field, re.getMessage(),
                                                baseValue != null ? baseValue.toString()
                                                        : StateReportData.EMPTY_STRING);
                                this.addRetrievalError(index, error);
                            }
                        }
                    }
                    if (baseValue != null) {
                        value = baseValue.toString();
                    } else {
                        value = StateReportData.EMPTY_STRING;
                    }
                    setCachedFieldValue(index, value);
                }

                if (exception != null) {
                    String errorId = LocalizationCache.getMessages(getData().getBroker().getPersistenceKey())
                            .getMessage(ERROR_EXCEPTION);
                    addRetrievalError(index,
                            new StateReportValidationError(getEntityName(), field.getBeanPath(), errorId,
                                    exception.getMessage()));
                }
            }
            return value;
        }

        /**
         * Returns the value of a field based on the state name of the field.
         *
         * @param aliasName String
         * @return the field value
         */
        public String getFieldValue(String aliasName) {
            String value = null;
            for (int pos = 0; pos < m_data.getFieldCount(); pos++) {
                if (m_data.getFieldDefinition(pos).getFieldId().equals(aliasName)) {
                    value = getFieldValue(pos);
                    break;
                }
            }
            return value;
        }

        /**
         * Returns the number of rows of data this entity can produce.
         *
         * @return the count of rows this entity can produce
         */
        public int getRowCount() {
            return m_rowCount;
        }

        /**
         * Method that needs data to be set in order for the result rows to be shown on the results
         * set
         * page.
         *
         * @return a school oid set during the entity creation or null
         */
        public String getSchoolOid() {
            String schoolOid = null;
            if (!StringUtils.isEmpty(m_schoolOid)) {
                schoolOid = m_schoolOid;
            } else if (m_data.getSchool() != null) {
                schoolOid = m_data.getSchool().getOid();
            }
            return schoolOid;
        }

        /**
         * Set parent and bean values for the entity.
         * NOTE: This method name cannot be changed and a new identical method named initialize()
         * cannot be successfully introduced. The existing method with spelling error is called by
         * StateReportData and refactoring all existing code and customizations is impractical.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            m_bean = bean;
            m_data = data;
            m_fieldValues = new String[data.getFieldCount()];
            m_fieldRetrievealErrors = new Collection[data.getFieldCount()];
        }

        /**
         * Pre processing for an entity.
         *
         * This is called before each entity is processed. It gives the entity an opportunity
         * to do extra processing on the bean.
         */
        public void preProcess() {
            // No default action.
        }

        /**
         * Post processing for an entity.
         *
         * This is called after each entity is processed. It gives the entity an opportunity
         * to do extra processing on the bean.
         */
        public void postProcess() {
            // No default action.
        }

        /**
         * Sets the current data row the entity is prepared to produce.
         * When an entity can product more than one row of export data, this
         * indicates which row. The value must be >= 0 and < rowcount.
         *
         * Reset values cache for the new row.
         *
         * @param currentRow void
         */
        public void setCurrentRow(int currentRow) {
            if (currentRow >= 0 && currentRow < m_rowCount) {
                m_currentRow = currentRow;
                m_fieldValues = new String[getData().getFieldCount(getCurrentFormatDefinitionId())];
                m_fieldRetrievealErrors = new Collection[getData().getFieldCount(getCurrentFormatDefinitionId())];
            }
        }

        /**
         * Set the school oid for this entity.
         *
         * @param schoolOid void
         */
        public void setSchoolOid(String schoolOid) {
            m_schoolOid = schoolOid;
        }

        /**
         * Sets the script manager.
         *
         * @param scriptManager void
         */
        public void setScriptManager(ScriptManager scriptManager) {
            m_scriptManager = scriptManager;
        }

        /**
         * Sets the number of rows of data this entity can produce.
         *
         * @param rowCount void
         */
        protected void setRowCount(int rowCount) {
            m_rowCount = rowCount;
        }

        /**
         * Get the cached value of the field, if it has been retrieved.
         *
         * @param index the field index of the field to retrieve.
         *
         * @return a String value
         */
        private String getCachedFieldValue(int index) {
            String value = null;
            if (m_fieldValues != null && index >= 0 && index < m_fieldValues.length) {
                value = m_fieldValues[index];
            }
            return value;
        }

        /**
         * Sets the cached value for a field index.
         *
         * @param index int
         * @param value string value to set.
         */
        private void setCachedFieldValue(int index, String value) {
            if (m_fieldValues != null && index >= 0 && index < m_fieldValues.length) {
                m_fieldValues[index] = value;
            }
        }

    }

    /**
     * The Class StateReportExport.
     */
    public static class StateReportExport extends ExportJavaSource {

        /**
         *
         */
        private static final long serialVersionUID = 1L;

        private static final String INITIALIZE_KEY = "label.state.report.initialize";

        private static final String PROCEDURE_ID = "procedureId";

        private static final String SAVE_RESULTS = "saveResults";

        private Collection<StateReportValidationError> m_initErrors = null;
        private StateReportData m_reportData = null;

        /**
         * A parameter that indicates the export results should be saved in the CustomExportResults
         * table.
         */
        private boolean m_saveResults = false;
        boolean m_useErrorColumns = false;

        /**
         * Gather data.
         *
         * @return DataGrid
         * @throws Exception exception
         * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
         */
        @Override
        protected DataGrid gatherData() throws Exception {
            ExportFormatResult saveResult = null;
            ExportFormatRow saveRow = null;
            long rowNumber = 0;
            int rowcount = 1;
            if (m_reportData != null) {
                rowcount = m_reportData.getFieldCount();
            }

            DataGrid dataGrid = new DataGrid(rowcount);
            if (m_initErrors.size() == 0) {
                if (m_saveResults) {
                    saveResult = X2BaseBean.newInstance(ExportFormatResult.class, getBroker().getPersistenceKey());
                    OrganizationManager.setOrganizationOids(saveResult, m_reportData.getOrganization());
                    saveResult.setRunDate(System.currentTimeMillis());
                    saveResult.setName(m_reportData.getExportTitle());
                    saveResult.setDefinitionOid(m_reportData.getEfdOid());
                    getBroker().saveBeanForced(saveResult);
                }

                if (m_reportData.open()) {
                    try {
                        StateReportEntity entity = null;
                        while ((entity = m_reportData.next()) != null) {
                            StateReportValidationError err = entity.filterEntity();
                            if (err == null) {
                                entity.preProcess();
                                entity.setScriptManager(m_reportData.getScriptManager());
                                dataGrid.append();

                                if (m_saveResults) {
                                    rowNumber++;
                                    saveRow =
                                            X2BaseBean.newInstance(ExportFormatRow.class,
                                                    getBroker().getPersistenceKey());
                                    if (!StringUtils.isEmpty(entity.getSchoolOid())) {
                                        saveRow.setSchoolOid(entity.getSchoolOid());
                                    }
                                    saveRow.setResultOid(saveResult.getOid());
                                    saveRow.setDefinitionOid(m_reportData.getEfdOid());
                                    saveRow.setSortOrder(new BigDecimal(rowNumber));
                                    saveRow.setSourceOid(entity.getBean().getOid());
                                    String rowName = entity.getEntityName();
                                    if (rowName != null && rowName.length() > 50) {
                                        rowName = rowName.substring(0, 50);
                                    }
                                    saveRow.setDescription(rowName);
                                }

                                /*
                                 * Add all fields
                                 */
                                for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
                                    FieldDefinition field = m_reportData.getFieldDefinition(pos);
                                    String fieldValue = entity.getFieldValue(pos);

                                    if (m_saveResults) {
                                        /*
                                         * If a value has a specified maximum length, then the field
                                         * that it is
                                         * being saved into also has the specified maximum length,
                                         * So we must trim the value to that maximum length before
                                         * saving.
                                         *
                                         * Ex: Middle name is specified as 10 chars and is assigned
                                         * to a
                                         * FieldA.
                                         * The value is 12 chars.
                                         * Must trim to 10 prior to saving so it will fit into the
                                         * field.
                                         *
                                         * The case that this might lose data would be in a CSV
                                         * where
                                         * the length is not
                                         * absolute as it would be in a column width report. The
                                         * export
                                         * might still
                                         * contain the excessive length but the saved value would
                                         * not.
                                         *
                                         * In those cases, the field would generate a validation
                                         * error
                                         * anyway.
                                         *
                                         * Save happens before padding so pad values do not also get
                                         * saved.
                                         */
                                        String saveFieldValue = ExportFormatManager.doPadding(fieldValue,
                                                ExportFormatField.PaddingDirectionCode.TRUNCATE_ONLY.ordinal(), null,
                                                field.getExportLength());

                                        /*
                                         * Save field value into CustomExportRow before
                                         * padding/trimming.
                                         */
                                        String saveField = field.getSaveBeanPath();
                                        if (!StringUtils.isEmpty(saveField)) {
                                            try {
                                                WebUtils.setProperty(saveRow, saveField, saveFieldValue);
                                            } catch (RuntimeException re) {
                                                // Ignore: the value was not saved, probably an
                                                // invalid
                                                // field name.
                                            }
                                        }
                                    }

                                    /*
                                     * If the value requires padding, pad it and trim it to field
                                     * max
                                     * length.
                                     */
                                    fieldValue = ExportFormatManager.doPadding(fieldValue,
                                            (field.getResizeMode() == null
                                                    ? ExportFormatField.PaddingDirectionCode.NONE.ordinal()
                                                    : field.getResizeMode().ordinal()),
                                            field.getPaddingChar(),
                                            field.getExportLength());

                                    // Set the final value.
                                    dataGrid.set(field.getFieldId(), fieldValue);

                                }
                                entity.postProcess();

                                if (m_saveResults) {
                                    getBroker().saveBean(saveRow);
                                }

                            } else {
                                m_initErrors.add(err);
                            }
                        }
                    } finally {
                        m_reportData.close();
                    }
                }

                // If the report has a heading or trailer, save it to the parent record.
                if (m_saveResults && (!StringUtils.isEmpty(m_reportData.getHeading())
                        || !StringUtils.isEmpty(m_reportData.getTrailer()))) {
                    saveResult.setHeading(m_reportData.getHeading());
                    saveResult.setTrailer(m_reportData.getTrailer());
                    getBroker().saveBeanForced(saveResult);
                }

            } else {
                m_useErrorColumns = true;
                for (StateReportValidationError error : m_initErrors) {
                    dataGrid.append();
                    dataGrid.set("Entity name", error.getEntityName());
                    dataGrid.set("Error ID", error.getErrorId());
                    dataGrid.set("Field Name", error.getFieldName());
                    dataGrid.set("Error message", error.getErrorMessage());
                }
            }
            dataGrid.beforeTop();
            System.out.println(ToolBean.getCachedCounts());
            return dataGrid;
        }

        /**
         * Returns a list of export field names.
         *
         * @return List
         * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
         */
        @Override
        protected List getColumnNames() {
            List fields = null;
            if (m_reportData != null) {
                if (m_useErrorColumns) {
                    setIncludeHeaderRow(true);

                    fields = new ArrayList(4);

                    fields.add("Entity name");
                    fields.add("Error ID");
                    fields.add("Field Name");
                    fields.add("Error message");
                } else {
                    fields = new ArrayList(m_reportData.getFieldCount());
                    for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
                        fields.add(m_reportData.getFieldDefinition(pos) == null ? ""
                                : m_reportData.getFieldDefinition(pos).getFieldId());
                    }
                }
            }
            return fields;
        }

        /**
         * Gets the column user names.
         *
         * @return List
         * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
         */
        @Override
        protected List getColumnUserNames() {
            return getColumnNames();
        }

        /**
         * Gets the comment.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
         */
        @Override
        protected String getComment() {
            StringBuilder comment = new StringBuilder();
            String lastName = "";
            if (m_initErrors != null && m_initErrors.size() > 0) {
                for (StateReportValidationError err : m_initErrors) {
                    String thisName = err.getEntityName();
                    if (!lastName.equals(thisName)) {
                        comment.append(err.getEntityName());
                        comment.append("\n");
                        lastName = thisName;
                    }
                    comment.append("    ");
                    comment.append(err.getFieldName());
                    comment.append("   ");
                    comment.append(err.getErrorId());
                    comment.append("   ");
                    comment.append(err.getErrorMessage());
                    comment.append("\n");
                }
            }
            return comment.toString();
        }

        /**
         * Gets the header.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
         */
        @Override
        protected String getHeader() {
            String header = null;
            if (m_reportData != null) {
                header = m_reportData.getHeading();
            }
            return header;
        }

        /**
         * Gets the trailer.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getTrailer()
         */
        @Override
        protected String getTrailer() {
            String trailer = null;
            if (m_reportData != null) {
                trailer = m_reportData.getTrailer();
            }
            return trailer;
        }

        /**
         * Initialize.
         *
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
         */
        @Override
        protected void initialize() throws X2BaseException {
            // Set exports to use MS/windows end of line character for all exports.
            setLineSeparator(FORMAT_EOL_WINDOWS);

            // Determine if the results should be saved in the StateReport results tables.
            Boolean saveResults = (Boolean) getParameter(SAVE_RESULTS);
            if (saveResults != null) {
                m_saveResults = saveResults.booleanValue();
            }

            String procedureId = (String) getParameter(PROCEDURE_ID);
            m_initErrors = new ArrayList<StateReportValidationError>();

            // Lookup State report source data procedure
            m_reportData = StateReportData.getReportDataFromProcedure(procedureId, getBroker(), m_initErrors);
            if (m_reportData != null && m_initErrors.size() == 0) {
                try {
                    // Initialize the report data object.
                    m_reportData.setBroker(getBroker());
                    m_reportData.setCurrentContext(getCurrentContext());
                    m_reportData.setOrganization(getOrganization());
                    m_reportData.setPrivilegeSet(getPrivilegeSet());
                    m_reportData.setSchoolContext(isSchoolContext());
                    m_reportData.setSchool(getSchool());
                    m_reportData.setParameters(getParameters());
                    m_reportData.setUser(getUser());
                    m_reportData.initializeExport();

                    // Set export parameters from the report data object.
                    setEscapeCharacter(m_reportData.getEscapeCharacter());
                    setIncludeHeaderRow(m_reportData.getIncludeHeaderRow());
                    setUseEscapes(m_reportData.getUseEscapes());
                    setUseValueDelimiters(m_reportData.getUseValueDelimiters());
                    setUseValueWrappers(m_reportData.getUseValueWrappers());
                    setValueDelimiter(m_reportData.getValueDelimiter());
                    setValueWrapper(m_reportData.getValueWrapper());
                } catch (X2BaseException x2be) {
                    String init_msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                            .getMessage(INITIALIZE_KEY);
                    m_initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));

                    throw x2be;
                }

                m_initErrors.addAll(m_reportData.getSetupErrors());
            }
        }
    }

    /**
     * This is a report class that performs standardized error and validation reporting for
     * the state report infrastructure.
     * this class will identify a procedure that contains a state report definition.
     * It will use that definition to final all configuration errors, data load errors
     * and validation errors.
     *
     * @author X2 Development Corporation
     */
    public static class StateReportValidateData extends ReportJavaSourceNet {
        /**
         *
         */
        private static final long serialVersionUID = 1L;
        /**
         * Keys for resource messages
         */
        private static final String TOTAL_KEY = "label.state.report.total";
        private static final String INITIALIZE_KEY = "label.state.report.initialize";
        private static final String GLOBAL_VALIDATION_KEY = "label.state.report.global";
        private static final String PROBLEM_AREA_KEY = "label.state.report.problemarea";

        /**
         * Format parameter keys.
         */
        private static final String PARAM_REPORT_TITLE = "reportTitle";

        /**
         * Format grid field keys.
         */
        private static final String FIELD_COUNT = "count";
        private static final String FIELD_ENTITY = "entity";
        private static final String FIELD_ERROR = "error";
        private static final String FIELD_ERROR_SECTION = "section";
        private static final String FIELD_FIELD = "field";
        private static final String FIELD_MESSAGE = "message";
        private static final String FIELD_SECTION_TITLE = "secttitle";
        private final static List<String> CSV_COLUMNS =
                Arrays.asList(FIELD_ENTITY, FIELD_FIELD, FIELD_ERROR, FIELD_MESSAGE);
        private final static String CSV_HEADING = "Entity, Field, Error, Message\r\n";

        /**
         * Input parameters for the procedure with the report data object.
         */
        public static String PROCEDURE_ID = "procedureId";
        public static String SHOW_SUMMARY = "summary";

        /**
         * Gather data.
         *
         * @return JRDataSource
         * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
         */
        @Override
        protected JRDataSource gatherData() {
            Collection<StateReportValidationError> initErrors = new ArrayList<StateReportValidationError>();
            Map<String, StateReportValidationError> summaryErrors = new TreeMap<String, StateReportValidationError>();

            ReportDataGrid grid = new ReportDataGrid(300, 4);

            String procedureId = (String) getParameter(PROCEDURE_ID);
            Boolean summaryOnly = (Boolean) getParameter(SHOW_SUMMARY);
            if (summaryOnly == null) {
                summaryOnly = Boolean.FALSE;
            }

            // Lookup State report source data procedure
            StateReportData reportData =
                    StateReportData.getReportDataFromProcedure(procedureId, getBroker(), initErrors);
            if (reportData != null) {
                // Set report parameters.
                addParameter(PARAM_REPORT_TITLE, reportData.getExportTitle());

                if (initErrors.size() == 0) {
                    try {
                        // Initialize the report data object.
                        reportData.setBroker(getBroker());
                        reportData.setCurrentContext(getCurrentContext());
                        reportData.setOrganization(getOrganization());
                        reportData.setPrivilegeSet(getPrivilegeSet());
                        reportData.setSchoolContext(isSchoolContext());
                        reportData.setSchool(getSchool());
                        reportData.setParameters(getParameters());
                        reportData.setUser(getUser());
                        reportData.initializeExport();
                    } catch (X2BaseException x2be) {
                        String initMsg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                                .getMessage(INITIALIZE_KEY);
                        initErrors.add(new StateReportValidationError(initMsg, initMsg, initMsg, x2be.getMessage()));
                    }

                    initErrors.addAll(reportData.getSetupErrors());
                    reportData.clearSetupErrors();

                    // Only go detail errors if there are no initialization errors.
                    if (initErrors.size() == 0) {
                        // Continue if no setup errors occur.

                        String totalLabel = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                                .getMessage(TOTAL_KEY);

                        // Iterate over source data, build up errors list.
                        if (reportData.open()) {
                            try {
                                StateReportEntity entity = null;
                                while ((entity = reportData.next()) != null) {
                                    entity.preProcess();
                                    Collection<StateReportValidationError> fieldErrors = entity.getFieldValidations();

                                    for (StateReportValidationError fve : fieldErrors) {
                                        // Accumulate totals for summary area.
                                        String summaryKey = fve.getFieldName() + '\t' + fve.getErrorId();
                                        StateReportValidationError summaryErr = summaryErrors.get(summaryKey);
                                        if (summaryErr == null) {
                                            summaryErr = new StateReportValidationError(totalLabel, fve.getFieldName(),
                                                    fve.getErrorId(), null);
                                            summaryErr.increment();
                                            summaryErrors.put(summaryKey, summaryErr);
                                        } else {
                                            summaryErr.increment();
                                        }

                                        // Write the individual error out, if not in summary mode.
                                        if (!summaryOnly.booleanValue()) {
                                            grid.append();
                                            grid.set(FIELD_SECTION_TITLE, reportData.getEntityTitle());
                                            grid.set(FIELD_ERROR_SECTION, reportData.getEntityTitle());
                                            grid.set(FIELD_ENTITY, fve.getEntityName());
                                            grid.set(FIELD_FIELD, fve.getFieldName());
                                            grid.set(FIELD_ERROR, fve.getErrorId());
                                            grid.set(FIELD_MESSAGE, fve.getErrorMessage());
                                        }
                                    }
                                    entity.postProcess();
                                }
                            } catch (X2BaseException x2be) {
                                reportData.addSetupError("Exception", x2be.getMessage());
                            } finally {
                                reportData.close();
                            }

                            // Now include error summary.
                            for (StateReportValidationError sfve : summaryErrors.values()) {
                                grid.append();
                                grid.set(FIELD_SECTION_TITLE, reportData.getEntityTitle());
                                grid.set(FIELD_ERROR_SECTION, reportData.getEntityTitle());
                                grid.set(FIELD_ENTITY, sfve.getEntityName());
                                grid.set(FIELD_FIELD, sfve.getFieldName());
                                grid.set(FIELD_ERROR, sfve.getErrorId());
                                grid.set(FIELD_COUNT, Integer.valueOf(sfve.getCount()));
                            }
                        }
                    }
                    /*
                     * postProcess performs total statistical checks on the
                     * entire content of the data.
                     */
                    initErrors.addAll(reportData.postProcess());
                    /*
                     * Check for processing errors that occured in the entity.
                     */
                    initErrors.addAll(reportData.getSetupErrors());
                }
            }
            if (initErrors.size() > 0) {
                // display initialization errors and global errors, if there are any.
                String section = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                        .getMessage(GLOBAL_VALIDATION_KEY);
                String caption = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                        .getMessage(PROBLEM_AREA_KEY);
                for (StateReportValidationError ifve : initErrors) {
                    grid.append();
                    grid.set(FIELD_SECTION_TITLE, section);
                    grid.set(FIELD_ERROR_SECTION, caption);
                    grid.set(FIELD_ENTITY, ifve.getEntityName());
                    grid.set(FIELD_FIELD, ifve.getFieldName());
                    grid.set(FIELD_ERROR, ifve.getErrorId());
                    grid.set(FIELD_MESSAGE, ifve.getErrorMessage());
                }
            }
            // If the grid is empty, add one empty row with title and section to populate heading
            // information
            // in the printed report. This avoids the display of the word "null".
            if (grid.getRows().size() == 0) {
                grid.append();
                grid.set(FIELD_SECTION_TITLE, reportData.getEntityTitle());
                grid.set(FIELD_ERROR_SECTION, reportData.getEntityTitle());
                grid.set(FIELD_ENTITY, "");
                grid.set(FIELD_FIELD, "");
                grid.set(FIELD_ERROR, "");
                grid.set(FIELD_MESSAGE, "");
            }
            grid.beforeTop();

            return grid;
        }

        /**
         * Publish results.
         *
         * @throws Exception exception
         * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#publishResults()
         */
        @Override
        protected void publishResults() throws Exception {
            ToolJob job = this.getJob();
            if (ToolInput.CSV_FORMAT == job.getInput().getFormat()) {
                writeResults();
            } else {
                super.publishResults();
            }
        }

        /**
         * Gets the column names.
         *
         * @return List
         */
        private List<String> getColumnNames() {
            return CSV_COLUMNS;
        }

        /**
         * Gets the header.
         *
         * @return String
         */
        private String getHeader() {
            return CSV_HEADING;
        }

        /**
         * Write results. Similar to ExportJavaSource.WriteResults()
         *
         * @throws IOException Signals that an I/O exception has occurred.
         */
        private void writeResults() throws IOException {
            String name = getJob().getTool().getName();
            String jobId = String.valueOf(getJob().getJobId());

            String[] logParameters = new String[] {
                    name, jobId
            };
            AppGlobals.getLog().log(Level.INFO, LOG_DATA_PREPARED, logParameters);

            /*
             * If the job has been aborted then don't bother formatting the results.
             */
            if (getJob().getStatus() != ToolJob.STATUS_ABORT) {
                DataGrid dataGrid = null;
                Object data = getDataSource();
                if (data != null && data instanceof DataGrid) {
                    dataGrid = (DataGrid) data;
                }

                if (dataGrid != null && dataGrid.rowCount() > 0) {
                    // delete total rows
                    String totalLabel = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                            .getMessage(TOTAL_KEY);
                    dataGrid.beforeTop();
                    while (dataGrid.next()) {
                        String value = (String) dataGrid.get(FIELD_ENTITY);
                        if (totalLabel.equals(value)) {
                            dataGrid.deleteRow();
                        }
                        // remove html markup
                        for (String col : dataGrid.getColumns()) {
                            if (dataGrid.get(col) instanceof String) {
                                String gridValue = (String) dataGrid.get(col);
                                gridValue = gridValue.replaceAll("<[^>]*>", "");
                                dataGrid.set(col, gridValue);
                            }
                        }
                    }

                    // output detail rows
                    OutputStream outputStream = getResultHandler().getOutputStream();
                    ExportJavaSource.writeData(outputStream,
                            dataGrid,
                            getColumnNames(),
                            getColumnNames(),
                            getCharacterEncoding(),
                            "\r\n", // LineSeparator
                            getHeader(),
                            "", // trailer
                            null, // escapeCharacter
                            ',', // valueDelimiter
                            '\"', // valueWrapper
                            false, // includeHeaderRow
                            false, // useEscapes
                            true, // useValueDelimiters
                            true // useValueWrappers
                    );
                } else {
                    getResultHandler().setEmpty(true);
                }
            }
        }
    }
    /**
     * This class holds information related to one field validation error.
     * The entity, field and error condition are stored.
     * The count field is used when summarizing for totals (on field, id).
     */
    public static class StateReportValidationError {
        /**
         * Instance variables
         */
        private int m_count;
        private String m_entityName;
        private String m_errorId;
        private String m_errorMessage;
        private String m_fieldName;

        /**
         * Constructor with all field values.
         *
         * @param entity String
         * @param field String
         * @param error String
         * @param message String
         */
        public StateReportValidationError(String entity, String field, String error, String message) {
            m_entityName = entity;
            m_fieldName = field;
            m_errorId = error;
            m_errorMessage = message;
            m_count = 0;
        }

        /**
         * Constructor with all field values.
         *
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param error String
         * @param message String
         */
        public StateReportValidationError(StateReportEntity entity, FieldDefinition field, String error,
                String message) {
            m_entityName = entity.getEntityName();
            m_fieldName = "(" + field.getFieldId() + ") ";
            m_errorId = error;
            m_errorMessage = message;
            m_count = 0;
        }

        /**
         * Returns the count of how many instances of this error were counted.
         *
         * @return int
         */
        public int getCount() {
            return m_count;
        }

        /**
         * Returns the entity name the error is reported for.
         *
         * @return String
         */
        public String getEntityName() {
            return m_entityName;
        }

        /**
         * Returns the error id for the error.
         *
         * @return String
         */
        public String getErrorId() {
            return m_errorId;
        }

        /**
         * Returns an informative error message for the specific entity, field and error.
         *
         * @return String
         */
        public String getErrorMessage() {
            return m_errorMessage;
        }

        /**
         * Returns the field name the error is reported for.
         *
         * @return String
         */
        public String getFieldName() {
            return m_fieldName;
        }

        /**
         * Increment the count field. Used for generating totals.
         */
        public void increment() {
            m_count++;
        }
    }

    /**
     * The Class StudentScheduleSpan.
     */
    public static class StudentScheduleSpan {

        /**
         * Constructor.
         *
         * @param broker X2Broker
         * @param schedule ToolStudentSchedule
         * @param changes List<ToolStudentScheduleChange>
         * @return List
         */
        public static List<StudentScheduleSpan> getStudentScheduleSpans(X2Broker broker,
                                                                        ToolStudentSchedule schedule,
                                                                        List<ToolStudentScheduleChange> changes) {
            List<StudentScheduleSpan> spans = new LinkedList();
            ToolStudentScheduleChange lastChange = null;
            ToolSection section = schedule != null ? schedule.getSection(broker)
                    : (changes != null && !changes.isEmpty() ? changes.get(0).getSection(broker) : null);
            Range<Date> sectionDateRange = section.getSectionDateRange(broker);
            PlainDate termStart = (PlainDate) sectionDateRange.getStart();
            PlainDate termEnd = (PlainDate) sectionDateRange.getEnd();


            for (ToolStudentScheduleChange change : changes) {

                // For a section, see if its dates compare with report dates or term dates.
                if (StudentScheduleChange.CODE_DROP.equals(change.getChangeTypeCode())) {
                    lastChange = change;
                } else if (StudentScheduleChange.CODE_ADD.equals(change.getChangeTypeCode())) {
                    PlainDate changeDate = change.getScheduleChangeDate(broker);
                    if (lastChange == null) {
                        if (schedule != null) {
                            StudentScheduleSpan info = DistrictManager.getStudentScheduleSpanFactory()
                                    .instantiateSpan(schedule.getSection(broker));
                            info.setSchedule(schedule);
                            info.setEntryDate(changeDate);
                            info.setEntryChange(change);
                            if (info.getEntryDate().before(termStart)) {
                                info.setEntryDate(termStart);
                            }
                            spans.add(info);
                        }
                    } else {
                        StudentScheduleSpan info = DistrictManager.getStudentScheduleSpanFactory()
                                .instantiateSpan(change.getSection(broker));
                        info.setEntryDate(changeDate);
                        info.setEntryChange(change);
                        if (info.getEntryDate().before(termStart)) {
                            info.setEntryDate(termStart);
                        }
                        PlainDate lastChangeDate = lastChange.getScheduleChangeDate(broker);
                        info.setLastMembershipDate(lastChangeDate);
                        // Avoid entering a change date that is after the term end date
                        if (info.getLastMembershipDate().after(termEnd)) {
                            info.setLastMembershipDate(termEnd);
                        }
                        info.setExitChange(lastChange);
                        // Avoid recording sections scheduled out entirely
                        // before the start of it's term. This is just scheduling activity.
                        if (!info.getLastMembershipDate().before(termStart)) {
                            spans.add(info);
                        }
                    }
                    lastChange = null;
                }
            }
            if (lastChange != null) {
                // The last change record for this section (in reverse chronological order)
                // was a drop. Assume the section was scheduled from the beginning of the
                // term/year.
                StudentScheduleSpan info = DistrictManager.getStudentScheduleSpanFactory().instantiateSpan(section);
                info.setEntryDate(termStart);
                PlainDate lastChangeDate = lastChange.getScheduleChangeDate(broker);
                if (lastChangeDate.after(termEnd)) {
                    info.setLastMembershipDate(termEnd);
                } else {
                    info.setLastMembershipDate(lastChangeDate);
                }
                info.setExitChange(lastChange);
                // Avoid recording sections scheduled out entirely
                // before the start of it's term. This is just scheduling activity.
                if (!info.getLastMembershipDate().before(termStart)) {
                    spans.add(info);
                }
            }
            return spans;
        }

        /*
         * Instance variables
         */
        private ToolSection m_section;
        private ToolStudentScheduleChange m_entryChange;
        private PlainDate m_entryDate;
        private ToolStudentScheduleChange m_exitChange;
        private PlainDate m_lastMembershipDate;
        private ToolTranscript m_transcript;
        private ToolStudentSchedule m_schedule;

        /**
         * Constructor.
         *
         * @param section MasterSchedule
         */
        public StudentScheduleSpan(ToolSection section) {
            m_section = section;
        }

        /**
         * Gets the date range.
         *
         * @return Range
         */
        public Range<Date> getDateRange() {
            return Range.of(getEntryDate(), getExitDate());
        }

        /**
         * Gets the entry change.
         *
         * @return the m_entryChange
         */
        public ToolStudentScheduleChange getEntryChange() {
            return m_entryChange;
        }

        /**
         * Returns the schedule begin date. This may be the term begin date, or other
         * date if a schedule change occurred during the term.
         *
         * @return PlainDate
         */
        public PlainDate getEntryDate() {
            return m_entryDate;
        }

        /**
         * Gets the exit change.
         *
         * @return the m_exitChange
         */
        public ToolStudentScheduleChange getExitChange() {
            return m_exitChange;
        }

        /**
         * Returns the schedule end date. This may be the term end date, or other
         * date if a schedule change occurred during the term.
         *
         * @return PlainDate
         */
        public PlainDate getExitDate() {
            return getLastMembershipDate();
        }

        /**
         * Returns the schedule end date. This may be the term end date, or other
         * date if a schedule change occurred during the term.
         *
         * @return PlainDate
         */
        public final PlainDate getLastMembershipDate() {
            return m_lastMembershipDate;
        }

        /**
         * Return the student schedule record if present.
         *
         * @return StudentSchedule
         */
        public ToolStudentSchedule getSchedule() {
            return m_schedule;
        }

        /**
         * Returns the master section for this schedule span.
         *
         * @return MasterSchedule
         */
        public ToolSection getSection() {
            return m_section;
        }

        /**
         * Returns the student transcript record for this class if there is one.
         *
         * @return Transcript
         */
        public ToolTranscript getTranscript() {
            return m_transcript;
        }

        /**
         * Set the student schedule change for add.
         *
         * @param entryChange void
         */
        public void setEntryChange(ToolStudentScheduleChange entryChange) {
            this.m_entryChange = entryChange;
        }

        /**
         * Sets the entry date for this student in this class.
         *
         * @param entryDate void
         */
        public void setEntryDate(PlainDate entryDate) {
            m_entryDate = entryDate;
        }

        /**
         * Set the student schedule change for drop.
         *
         * @param exitChange void
         */
        public void setExitChange(ToolStudentScheduleChange exitChange) {
            this.m_exitChange = exitChange;
        }

        /**
         * Sets the exit date for this student in this class.
         *
         * @param exitDate void
         */
        public void setLastMembershipDate(PlainDate exitDate) {
            m_lastMembershipDate = exitDate;
        }

        /**
         * Set the student schedule record.
         *
         * @param schedule void
         */
        public void setSchedule(ToolStudentSchedule schedule) {
            this.m_schedule = schedule;
        }

        /**
         * Sets the student transcript record for this class.
         *
         * @param transcript void
         */
        public void setTranscript(ToolTranscript transcript) {
            m_transcript = transcript;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            String output = m_section.getCourseView();

            output += " [" + m_section.getTermView() + "]: ";
            output += " " + m_entryDate + " to " + m_lastMembershipDate;
            output += " EntryChange: " + (m_entryChange != null ? "Yes" : "No");
            output += " ExitChange: " + (m_exitChange != null ? "Yes" : "No");

            return output;
        }
    }

    /**
     * A factory for creating StudentScheduleSpan objects.
     */
    public static class StudentScheduleSpanFactory {

        /**
         * Instantiate span.
         *
         * @param section ToolSection
         * @return StudentScheduleSpan
         */
        public StudentScheduleSpan instantiateSpan(ToolSection section) {
            return new StudentScheduleSpan(section);
        }

    }

    /**
     * The Class ToolUserDataContainer.
     */
    public static class ToolUserDataContainer implements Serializable {
        private static final long serialVersionUID = 1L;

        /**
         * Instantiates a new tool user data container.
         *
         * @param privilegeSet PrivilegeSet
         * @param userOid String
         */
        public ToolUserDataContainer(PrivilegeSet privilegeSet, String userOid) {
            m_privilegeSet = privilegeSet;
            m_userOid = userOid;
        }

        private PrivilegeSet m_privilegeSet = null;
        private String m_userOid = null;

        /**
         * Gets the privilege set.
         *
         * @return Privilege set
         */
        public PrivilegeSet getPrivilegeSet() {
            return m_privilegeSet;
        }

        /**
         * Gets the user.
         *
         * @return User
         */
        public User getUser() {
            ModelBroker broker = new ModelBroker(m_privilegeSet);
            return (User) broker.getBeanByOid(User.class, m_userOid);
        }
    }

    /*
     * This block of code creates a future date and date range to generate a date range that will
     * match nothing
     */
    private static PlainDate JAN_1_2200_Date;

    static {
        Calendar year2200 = Calendar.getInstance();

        year2200.set(Calendar.YEAR, 2200);
        year2200.set(Calendar.MONTH, Calendar.JANUARY);
        year2200.set(Calendar.DAY_OF_MONTH, 1);

        JAN_1_2200_Date = new PlainDate(year2200.getTime());
    }

    private static Range<Date> JAN_1_2200_Range = Range.of(JAN_1_2200_Date, JAN_1_2200_Date);

    /**
     * Return negative if a < b, 0 if equal, and positive if a > b.
     *
     * @param a PlainDate
     * @param b PlainDate
     * @param nullIsOld Treat a null date as infinitely old
     * @return int
     */
    public static int compareDates(PlainDate a, PlainDate b, boolean nullIsOld) {
        if (a == null && b == null) {
            return 0;
        }

        if (a == null && b != null) {
            if (nullIsOld) {
                return -1;
            }

            return 1;
        }

        if (a != null && b == null) {
            if (nullIsOld) {
                return 1;
            }

            return -1;
        }

        return a.compareTo(b);
    }

    /**
     * Reverse.
     *
     * @param <T> the generic type
     * @param stream Stream<T>
     * @return Stream
     */
    public static <T> Stream<T> reverse(Stream<T> stream) {
        LinkedList<T> stack = new LinkedList<>();
        stream.forEach(stack::push);
        return stack.stream();
    }


}
