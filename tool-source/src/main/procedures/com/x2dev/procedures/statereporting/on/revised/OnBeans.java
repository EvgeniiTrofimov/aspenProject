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

import static com.x2dev.procedures.statereporting.on.revised.OnsisConstants.SUBMISSION_SCHOOL_TYPE_OCTOBER;
import com.follett.fsc.core.framework.persistence.CollectionCriteriaHelper;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.framework.persistence.adjusters.JoinAdjuster.JoinType;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.InvalidDictionaryIdException;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.*;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanDefinition.JoinAdjusterPattern;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpanFactory;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Pair;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpanFactory;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.GradesHelper;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.text.DateFormatSymbols;
import java.text.DecimalFormat;
import java.text.Format;
import java.time.LocalDate;
import java.util.*;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.Attribute;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

/**
 * The Class OnBeans.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnBeans {
    /**
     * The Class CommunityInvolvementAssessment.
     */
    public static class CommunityInvolvementAssessment extends OnStudentAssessment {
        private static final String ALIAS_ASM_HOURS = "asm-cih-hours";

        private static final String ASD_ID = "CommInvolve";

        public static final ToolBeanColumn FIELD_HOURS =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT,
                        new ToolBeanColumn.AliasDefinition(ALIAS_ASM_HOURS, ASD_ID, false));

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = OnStudentAssessment.FULL_DEFINITION
                .expand(FIELD_HOURS)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(FIELD_ASSESSMENT_ID.resolve(getDictionaryExtractor()), ASD_ID);
                        return criteria;
                    }
                });

        /**
         * Instantiates a new community involvement assessment.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public CommunityInvolvementAssessment(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the hours.
         *
         * @return Big decimal
         */
        public BigDecimal getHours() {
            return this.getValueBigDecimal(FIELD_HOURS);
        }
    }

    /**
     * The Class FteMonthly.
     */
    public static class FteMonthly extends ToolBean {
        private static final String ALIAS_UDD_FTE = "udd-fte-fte";
        private static final String ALIAS_UDD_FTE_HIGH_CREDIT = "udd-fte-fte-hc";
        private static final String ALIAS_UDD_FTE_MINUTES = "udd-fte-minutes";
        private static final String ALIAS_UDD_FTE_MINUTES_HIGH_CREDIT = "udd-fte-minutes-hc";
        private static final String ALIAS_UDD_FTE_MONTH = "udd-fte-month";
        private static final String ALIAS_UDD_FTE_REGISTER = "udd-fte-register";
        private static final String ALIAS_UDD_FTE_REPORT_DATE = "udd-fte-report-date";
        private static final String ALIAS_UDD_FTE_SB_RESIDENT_STATUS = "udd-fte-SBResidentStatus";
        private static final String ALIAS_UDD_FTE_SCHEDULE_MODE = "udd-fte-schedule-mode";
        private static final String ALIAS_UDD_FTE_SCHOOL_YEAR = "udd-fte-school-year";

        public static final String DDX_ID = "STD-FTE";

        public static final ToolBeanColumn FIELD_EXTENDED_DATA_DICTIONARY_OID =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_D.extendedDataDictionaryOid());
        public static final ToolBeanColumn FIELD_FTE =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_D, ALIAS_UDD_FTE, DDX_ID);
        public static final ToolBeanColumn FIELD_FTE_HIGH_CREDIT =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_D, ALIAS_UDD_FTE_HIGH_CREDIT, DDX_ID);
        public static final ToolBeanColumn FIELD_MINUTES =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_D, ALIAS_UDD_FTE_MINUTES, DDX_ID);
        public static final ToolBeanColumn FIELD_MINUTES_HIGH_CREDIT =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_D, ALIAS_UDD_FTE_MINUTES_HIGH_CREDIT, DDX_ID);
        public static final ToolBeanColumn FIELD_MONTH =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_D, ALIAS_UDD_FTE_MONTH, DDX_ID);
        public static final ToolBeanColumn FIELD_REGISTER =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_D, ALIAS_UDD_FTE_REGISTER, DDX_ID);
        public static final ToolBeanColumn FIELD_REPORT_DATE =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_D, ALIAS_UDD_FTE_REPORT_DATE, DDX_ID);
        public static final ToolBeanColumn FIELD_SB_RESIDENT_STATUS =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_D,
                        new ToolBeanColumn.AliasDefinition(ALIAS_UDD_FTE_SB_RESIDENT_STATUS, DDX_ID, false));
        public static final ToolBeanColumn FIELD_SCHEDULE_MODE =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_D,
                        new ToolBeanColumn.AliasDefinition(ALIAS_UDD_FTE_SCHEDULE_MODE, DDX_ID, false));
        public static final ToolBeanColumn FIELD_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_D.schoolOid());
        public static final ToolBeanColumn FIELD_SCHOOL_YEAR =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_D, ALIAS_UDD_FTE_SCHOOL_YEAR, DDX_ID);
        public static final ToolBeanColumn FIELD_STUDENT_OID =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_D.studentOid());

        public static ToolBeanRelationship PARENT_STUDENT =
                new ToolBeanRelationship(SisBeanPaths.USER_DEFINED_TABLE_D.student().getBeanType(),
                        SisBeanPaths.USER_DEFINED_TABLE_D.student().getValueType(),
                        SisBeanPaths.USER_DEFINED_TABLE_D.studentOid().getPath(),
                        SisBeanPaths.STUDENT.userDefinedRecordsD().getPath(),
                        SisBeanPaths.USER_DEFINED_TABLE_D.student().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_EXTENDED_DATA_DICTIONARY_OID,
                        FIELD_FTE,
                        FIELD_FTE_HIGH_CREDIT,
                        FIELD_MINUTES,
                        FIELD_MINUTES_HIGH_CREDIT,
                        FIELD_MONTH,
                        FIELD_REGISTER,
                        FIELD_REPORT_DATE,
                        FIELD_SB_RESIDENT_STATUS,
                        FIELD_SCHEDULE_MODE,
                        FIELD_SCHOOL_OID,
                        FIELD_SCHOOL_YEAR,
                        FIELD_STUDENT_OID)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {

                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        DictionaryExtractor extractor = ToolBean.getDictionaryExtractor();
                        DataDictionary dictionary = extractor.getDictionary(DDX_ID);
                        String dictionaryOid =
                                dictionary == null ? "__NO_MATCH__" : dictionary.getExtendedDictionaryOid();
                        criteria.addEqualTo(FIELD_EXTENDED_DATA_DICTIONARY_OID.resolve(extractor), dictionaryOid);
                        return criteria;
                    }
                });

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.USER_DEFINED_TABLE_D.getBeanType();
        }

        /**
         * Instantiates a new fte monthly.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public FteMonthly(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the attendance type.
         *
         * @return String
         */
        public String getAttendanceType() {
            return this.getValueReferenceState(FIELD_REGISTER);
        }

        /**
         * Gets the board resident status.
         *
         * @return the board resident status
         */
        public String getBoardResidentStatus() {
            return getValueString(FIELD_SB_RESIDENT_STATUS);
        }

        /**
         * Gets the fte.
         *
         * @return Big decimal
         */
        public BigDecimal getFte() {
            return this.getValueBigDecimal(FIELD_FTE);
        }

        /**
         * Gets the month.
         *
         * @return String
         */
        public String getMonth() {
            return this.getValueString(FIELD_MONTH);
        }

        /**
         * Gets the high credit fte.
         *
         * @return Big decimal
         */
        public BigDecimal getFteHc() {
            return this.getValueBigDecimal(FIELD_FTE_HIGH_CREDIT);
        }

        /**
         * Gets the minutes.
         *
         * @return the minutes
         */
        public Integer getMinutes() {
            return getValueInt(FIELD_MINUTES);
        }

        /**
         * Gets the minutes hc.
         *
         * @return the minutes hc
         */
        public Integer getMinutesHc() {
            return getValueInt(FIELD_MINUTES_HIGH_CREDIT);
        }

        /**
         * Gets the register.
         *
         * @return String
         */
        public String getRegister() {
            return this.getValueString(FIELD_REGISTER);
        }

        /**
         * Gets the date.
         *
         * @return Plain date
         */
        public PlainDate getReportDate() {
            return getValueDate(FIELD_REPORT_DATE);
        }

        /**
         * Gets the school oid.
         *
         * @return String
         */
        public Object getSchoolOid() {
            return getValueString(FIELD_SCHOOL_OID);
        }

        /**
         * Gets the school year.
         *
         * @return the school year
         */
        public String getSchoolYear() {
            return getValueString(FIELD_SCHOOL_YEAR);
        }

        /**
         * Gets the student.
         *
         * @param broker X2Broker
         * @return Student
         */
        public ToolStudent getStudent(X2Broker broker) {
            String stdOid = getValueString(FIELD_STUDENT_OID);
            return getBeanByOid(broker, ToolStudent.class, stdOid, true);
        }

        /**
         * Gets the student oid.
         *
         * @return String
         */
        public String getStudentOid() {
            return getValueString(FIELD_STUDENT_OID);
        }

        /**
         * Checks if is changed.
         *
         * @param studentMonthlyValue the student monthly value
         * @return true, if is changed
         */
        public boolean isChanged(Map<String, Object> studentMonthlyValue) {
            for (Entry<String, Object> entry : studentMonthlyValue.entrySet()) {
                String alias = entry.getKey();
                ToolBeanColumn column = FULL_DEFINITION.getColumnByAlias(alias);
                if (column == null) {
                    throw new IllegalStateException("Column for alias " + alias + " is not found.");
                }
                if (column.getField(ToolBean.getDictionaryExtractor()) != null) {
                    Comparable newFieldValue = (Comparable) entry.getValue();
                    Comparable oldFieldValue = (Comparable) getValueAsJavaType(column);
                    boolean isSame = (newFieldValue == null)
                            ? (oldFieldValue == null)
                            : (oldFieldValue == null ? false : newFieldValue.compareTo(oldFieldValue) == 0);
                    if (!isSame) {
                        return true;
                    }
                }
            }
            return false;
        }

    }

    /**
     * The Class FteMonthly.
     */
    public static class FteRecord extends ToolBean {
        private static final String ALIAS_UDC_FTE = "udc-fte-fte";
        private static final String ALIAS_UDC_FTE_DATE = "udc-fte-date";
        private static final String ALIAS_UDC_FTE_HIGH_CREDIT = "udc-fte-fte-hc";
        private static final String ALIAS_UDC_FTE_MINUTES = "udc-fte-minutes";
        private static final String ALIAS_UDC_FTE_MINUTES_HIGH_CREDIT = "udc-fte-minutes-hc";
        private static final String ALIAS_UDC_FTE_REGISTER = "udc-fte-register";
        private static final String ALIAS_UDC_FTE_SCHOOL_YEAR = "udc-fte-school-year";

        public static final String DDX_ID = "STD-FTE";

        public static final ToolBeanColumn FIELD_EXTENDED_DATA_DICTIONARY_OID =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_C.extendedDataDictionaryOid());
        public static final ToolBeanColumn FIELD_FTE =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_C, ALIAS_UDC_FTE, DDX_ID);
        public static final ToolBeanColumn FIELD_FTE_DATE =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_C, ALIAS_UDC_FTE_DATE, DDX_ID);
        public static final ToolBeanColumn FIELD_FTE_HIGH_CREDIT =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_C, ALIAS_UDC_FTE_HIGH_CREDIT, DDX_ID);
        public static final ToolBeanColumn FIELD_MINUTES =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_C, ALIAS_UDC_FTE_MINUTES, DDX_ID);
        public static final ToolBeanColumn FIELD_MINUTES_HIGH_CREDIT =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_C, ALIAS_UDC_FTE_MINUTES_HIGH_CREDIT, DDX_ID);
        public static final ToolBeanColumn FIELD_REGISTER =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_C, ALIAS_UDC_FTE_REGISTER, DDX_ID);
        public static final ToolBeanColumn FIELD_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_C.schoolOid());
        public static final ToolBeanColumn FIELD_SCHOOL_YEAR =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_C, ALIAS_UDC_FTE_SCHOOL_YEAR, DDX_ID);
        public static final ToolBeanColumn FIELD_STUDENT_OID =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_C.studentOid());

        public static ToolBeanRelationship PARENT_STUDENT =
                new ToolBeanRelationship(SisBeanPaths.USER_DEFINED_TABLE_C.student().getBeanType(),
                        SisBeanPaths.USER_DEFINED_TABLE_C.student().getValueType(),
                        SisBeanPaths.USER_DEFINED_TABLE_C.studentOid().getPath(),
                        SisBeanPaths.STUDENT.userDefinedRecordsC().getPath(),
                        SisBeanPaths.USER_DEFINED_TABLE_C.student().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_EXTENDED_DATA_DICTIONARY_OID,
                        FIELD_FTE,
                        FIELD_FTE_DATE,
                        FIELD_FTE_HIGH_CREDIT,
                        FIELD_MINUTES,
                        FIELD_MINUTES_HIGH_CREDIT,
                        FIELD_REGISTER,
                        FIELD_SCHOOL_OID,
                        FIELD_SCHOOL_YEAR,
                        FIELD_STUDENT_OID)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {

                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        DictionaryExtractor extractor = ToolBean.getDictionaryExtractor();
                        DataDictionary dictionary = extractor.getDictionary(DDX_ID);
                        String dictionaryOid =
                                dictionary == null ? "__NO_MATCH__" : dictionary.getExtendedDictionaryOid();
                        criteria.addEqualTo(FIELD_EXTENDED_DATA_DICTIONARY_OID.resolve(extractor), dictionaryOid);
                        return criteria;
                    }
                });

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.USER_DEFINED_TABLE_C.getBeanType();
        }

        /**
         * Instantiates a new fte monthly.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public FteRecord(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the attendance type.
         *
         * @return String
         */
        public String getAttendanceType() {
            return this.getValueReferenceState(FIELD_REGISTER);
        }

        /**
         * Gets the fte.
         *
         * @return Big decimal
         */
        public BigDecimal getFte() {
            return this.getValueBigDecimal(FIELD_FTE);
        }

        /**
         * Gets the date.
         *
         * @return Plain date
         */
        public PlainDate getFteDate() {
            return getValueDate(FIELD_FTE_DATE);
        }

        /**
         * Gets the high credit fte.
         *
         * @return Big decimal
         */
        public BigDecimal getFteHc() {
            return getValueBigDecimal(FIELD_FTE_HIGH_CREDIT);
        }

        /**
         * Gets the minutes.
         *
         * @return the minutes
         */
        public Integer getMinutes() {
            return getValueInt(FIELD_MINUTES);
        }

        /**
         * Gets the minutes hc.
         *
         * @return the minutes hc
         */
        public Integer getMinutesHc() {
            return getValueInt(FIELD_MINUTES_HIGH_CREDIT);
        }

        /**
         * Gets the student.
         *
         * @param broker X2Broker
         * @return Student
         */
        public ToolSchool getSchool(X2Broker broker) {
            String stdOid = getValueString(FIELD_STUDENT_OID);
            return getBeanByOid(broker, ToolSchool.class, stdOid, true);
        }

        /**
         * Gets the school oid.
         *
         * @return String
         */
        public String getSchoolOid() {
            return getValueString(FIELD_SCHOOL_OID);
        }

        /**
         * Gets the school year.
         *
         * @return the school year
         */
        public String getSchoolYear() {
            return getValueString(FIELD_SCHOOL_YEAR);
        }

        /**
         * Gets the student.
         *
         * @param broker X2Broker
         * @return Student
         */
        public ToolStudent getStudent(X2Broker broker) {
            String stdOid = getValueString(FIELD_STUDENT_OID);
            return getBeanByOid(broker, ToolStudent.class, stdOid, true);
        }

        /**
         * Gets the student oid.
         *
         * @return String
         */
        public String getStudentOid() {
            return getValueString(FIELD_STUDENT_OID);
        }

    }

    /**
     * The Class OnsisAddress.
     */
    public static class OnAddress extends ToolAddress {
        public static final ToolBeanColumn FIELD_COUNTRY = new ToolBeanColumn(SisBeanPaths.PERSON_ADDRESS.country());
        public static final ToolBeanColumn FIELD_PROVINCE = new ToolBeanColumn(SisBeanPaths.PERSON_ADDRESS.state());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolAddress.FULL_DEFINITION
                .expand(FIELD_COUNTRY,
                        FIELD_PROVINCE);

        /**
         * Instantiates a new onsis course request.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnAddress(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the country.
         *
         * @return String
         */
        public String getCountry() {
            return getValueReferenceState(FIELD_COUNTRY);
        }

        /**
         * Gets the country.
         *
         * @return String
         */
        public String getProvince() {
            return getValueReferenceState(FIELD_PROVINCE);
        }

    }

    /**
     * The Class OnAnnualSpan.
     */
    public static class OnAnnualSpan extends AnnualSpan {
        private OnAnnualSpan m_bestPrimarySpan;
        private Range<Date> m_dateRange;

        /**
         * Instantiates a new On annual span.
         *
         * @param enrollments List<? extends ToolEnrollment>
         * @param spanEnrollments List<? extends ToolEnrollment>
         * @param spanContext ToolDistrictContext
         */
        protected OnAnnualSpan(List<? extends ToolEnrollment> enrollments,
                List<? extends ToolEnrollment> spanEnrollments, ToolDistrictContext spanContext) {
            super(enrollments, spanEnrollments, spanContext);
        }

        /**
         * Instantiates a new annual span.
         *
         * @param studentSchool ToolStudentSchool
         * @param broker X2Broker
         */
        protected OnAnnualSpan(ToolStudentSchool studentSchool, X2Broker broker) {
            super(studentSchool, broker);
        }

        /**
         * Checks if is inclusion span included.
         *
         * Normally an On span calculates the Span End Date as the first date of non-attendance,
         * aka firstInactiveInSessionDate.
         *
         * There is one exception:
         *
         * A span with a W record whose date falls between the last in session date of the context
         * year,
         * and the first in-session date of the next year (eg a summer withdrawal),
         * should calculate the span end date as the W date adjusted by MbrOnWithd pref.
         *
         * Note that June submissions have additional logic for span end date
         * in OnStudentSchoolEnrolment.getOnEnrollmentEndDate
         *
         * This exception can be determined when:
         *
         * 1. this is a withdrawn span
         * i.e. W record is non-null and lastActiveDate is non-null
         *
         * 2. AND W-1 date falls after the last in-session date of the span context year
         *
         * Note, there’s no need to check if W is before 1st in-session date of next year
         * because lastActiveDate has already done that.
         *
         * @param broker X2Broker
         * @return PlainDate
         */
        public PlainDate calculateSpanEndDate(X2Broker broker) {
            PlainDate endDate = getFirstInactiveInSessionDate();

            boolean isWithdrawal = isWithdrawal();
            PlainDate lastActiveDate = getLastActiveInSessionDate();
            PlainDate withdrawalMemberDate = getWithdrawalMemberDate();

            PlainDate scanStartDate = null;
            boolean falseForward = false;
            ToolSchoolCalendar calendar = getSchoolCalendar();
            PlainDate lastInSessionDate =
                    calendar == null ? null : calendar.findFirstInSessionDate(broker, scanStartDate, falseForward);

            /*
             * 1. is this is a withdrawn span
             * i.e. W record is non-null and lastActiveDate is non-null
             */
            boolean isWithdrawn = (isWithdrawal && withdrawalMemberDate != null && lastActiveDate != null);

            /*
             * 2. Does W-1 date fall after the last in-session date of the span context year
             */

            boolean isAfterLastInSession =
                    isWithdrawn && lastInSessionDate != null && withdrawalMemberDate.after(lastInSessionDate);

            if (isAfterLastInSession) {
                endDate = withdrawalMemberDate;
            } else if (endDate == null) {
                endDate = lastActiveDate;
            }

            return endDate;
        }

        /**
         * Span Calendar is the School Calendar with the following criteria
         * - Belongs to span context
         * -Belongs to span school
         * -Calendar ID matches:
         * 1. W record Calendar Code if found
         * 2. else Student Calendar Code
         * 3. else Default Calendar Code(s)
         * 4. else Wild Card (*) Calendar Code
         *
         * @param broker the broker
         * @return the tool school calendar
         */
        public ToolSchoolCalendar findBestCalendar(X2Broker broker) {
            return ToolSchoolCalendar.findBestCalendar(broker, getStudent(), getSchool(),
                    getDataStorageEnrollment(), getContext());
        }

        /**
         * Gets the arrival date.
         *
         * @return the arrival date
         */
        public PlainDate getArrivalDate() {
            if (isSecondary()) {
                return getSpanStartDate();
            }

            List<OnEnrollment> enrollments = (List<OnEnrollment>) getAllEnrollmentsAscend();
            if (enrollments == null) {
                return null;
            }

            Optional<OnEnrollment> findFirst = enrollments.stream()
                    .filter(enr -> !enr.getEnrollmentDate().before(getContext().getStartDate())
                            && !StudentEnrollment.WITHDRAWAL.equals(enr.getEnrollmentType())
                            && "Arrived".equals(enr.getArrivalStatus()))
                    .findFirst();

            if (!findFirst.isPresent()) {
                return null;
            }

            OnEnrollment arrivedStudentEnrollment = findFirst.get();

            PlainDate arrivedDate = arrivedStudentEnrollment.getEnrollmentDate();
            PlainDate earliestDate = getFirstActiveInSessionDate();
            if (earliestDate != null & earliestDate.after(arrivedDate)) {
                arrivedDate = earliestDate;
            }
            return arrivedDate;
        }

        /**
         * Gets the best primary span for.
         *
         * @param broker X2Broker
         * @return On annual span
         */
        public OnAnnualSpan getBestPrimarySpanFor(X2Broker broker) {
            return getBestPrimarySpanFor(broker, null);
        }

        /**
         * Gets the best primary span for.
         *
         * @param broker X2Broker
         * @param logPredicate Predicate<String>
         * @return On annual span
         */
        public OnAnnualSpan getBestPrimarySpanFor(X2Broker broker, Predicate<String> logPredicate) {
            /*
             * If the lookup span is already the primary, return it
             */
            if (!isSecondary()) {
                return this;
            }
            if (m_bestPrimarySpan != null) {
                return m_bestPrimarySpan;
            }

            /*
             * Determine date to start searching for most recent enrollment span
             */
            PlainDate scanFromTemp = getSpanEndDate();
            if (scanFromTemp == null) {
                scanFromTemp = getContext().getEndDate();
            }

            PlainDate spanHelperAsOfDate =
                    (PlainDate) ToolBean.getPreference(ToolBean.PREFERENCE_QUERY_AS_OF_DATE);

            if (scanFromTemp == null || (spanHelperAsOfDate != null && scanFromTemp.after(spanHelperAsOfDate))) {
                scanFromTemp = spanHelperAsOfDate;
            }

            if (logPredicate != null) {
                logPredicate.test("getBestPrimarySpanFor-scanFromTemp:" + scanFromTemp);
            }

            /*
             * Get all primary enrollment spans for this student
             */
            List<AnnualSpan> allSpans = getStudent().getEnrollmentSpans(broker, false, false);
            List<AnnualSpan> primarySpansDescending =
                    allSpans.stream().filter(span -> !span.isSecondary())
                            .sorted(new Comparator<AnnualSpan>() {
                                @Override
                                public int compare(AnnualSpan span1, AnnualSpan span2) {
                                    // Compare LastActiveDate. No date means infinite future
                                    boolean nullIsOld = false;
                                    int compare = ToolsSharedContainer.compareDates(span1.getSpanEndDate(),
                                            span2.getSpanEndDate(), nullIsOld);

                                    // Compare FirstActiveDate. No date means infinite past
                                    if (compare == 0) {
                                        nullIsOld = true;
                                        compare = ToolsSharedContainer.compareDates(span1.getSpanStartDate(),
                                                span2.getSpanStartDate(), nullIsOld);
                                    }
                                    return -compare;
                                }
                            })
                            .collect(Collectors.toList());
            if (logPredicate != null) {
                logPredicate.test("getBestPrimarySpanFor-primarySpansDescending:" + primarySpansDescending);
            }


            /*
             * Walk descending spans from the scanFromLatest date to find the latest primary span
             */
            PlainDate scanFromLatest = scanFromTemp;
            PlainDate scanUntilEarliest = getSpanStartDate();
            Optional<AnnualSpan> mostRecentSpanOpt =
                    primarySpansDescending.stream().filter(span -> (span.getSpanStartDate() != null
                            && !span.getSpanStartDate().after(scanFromLatest))
                            && (span.getSpanEndDate() == null ||
                                    !span.getSpanEndDate().before(scanUntilEarliest)))
                            .findFirst();
            if (logPredicate != null) {
                logPredicate.test("getBestPrimarySpanFor-mostRecentSpanOpt:" + mostRecentSpanOpt);
            }

            if (!mostRecentSpanOpt.isPresent()) {
                // Search for the most recent span that has ended since there is no overlapping span
                // This is needed for summer school where secondary span often starts after primary
                // ends
                mostRecentSpanOpt =
                        primarySpansDescending.stream().filter(span -> (span.getSpanStartDate() != null
                                && !span.getSpanStartDate().after(scanFromLatest)))
                                .findFirst();
            }
            if (logPredicate != null) {
                logPredicate.test("getBestPrimarySpanFor-mostRecentSpanOpt2:" + mostRecentSpanOpt);
            }

            m_bestPrimarySpan = mostRecentSpanOpt.isPresent() ? (OnAnnualSpan) mostRecentSpanOpt.get() : null;

            return m_bestPrimarySpan;
        }

        /**
         * Gets the board resident status.
         *
         * @param dictionaryExtractor the dictionary extractor
         * @return String
         */
        public String getBoardResidentStatus(DictionaryExtractor dictionaryExtractor) {
            String status = null;
            if (!isSecondary()) {
                OnEnrollment enrollment = (OnEnrollment) getRecentEnrollmentES();
                if (enrollment != null) {
                    status = enrollment.getBoardResidentStatus();
                }
            } else {
                OnStudentSchool secondarySchool = (OnStudentSchool) getSecondary();
                if (secondarySchool != null && !StringUtils.isBlank(
                        OnStudentSchool.FIELD_BRD_RES_STAT_TYPE.resolve(dictionaryExtractor))) {
                    status = dictionaryExtractor.getStateValue(secondarySchool,
                            OnStudentSchool.FIELD_BRD_RES_STAT_TYPE);
                }
            }

            return status;
        }

        /**
         * Gets the canadian residence status.
         *
         * @param dictionaryExtractor the dictionary extractor
         * @return String
         */
        public String getCanadianResidenceStatus(DictionaryExtractor dictionaryExtractor) {
            String result = null;
            OnEnrollment enrollment = (OnEnrollment) getRecentEnrollment();
            if (enrollment != null) {
                result = dictionaryExtractor.getStateValue(enrollment,
                        OnEnrollment.FIELD_CANADIAN_RES_STATUS);
            }
            if (result == null && StringUtils.isEmpty(result)) {
                OnStudent student = getStudent();
                result = dictionaryExtractor.getStateValue(student,
                        OnStudent.FIELD_CANADIAN_RES_STATUS);
            }
            return result;
        }

        /**
         * Gets the entry demit country entry.
         *
         * @return String
         */
        public String getEntryDemitCountryEntry() {
            String result = null;
            OnEnrollment recentEnrollment = (OnEnrollment) getRecentEnrollmentE();
            if (recentEnrollment != null) {
                result = recentEnrollment.getCountry();
            }
            result = OnsisConstants.VALUE_CANADA.equals(result) ? null : result;
            return result;
        }

        /**
         * Gets the entry demit country entry.
         *
         * @return String
         */
        public String getEntryDemitCountryExit() {
            String result = null;
            OnEnrollment recentEnrollment = (OnEnrollment) getRecentEnrollmentEW();
            if (recentEnrollment != null && StudentEnrollment.WITHDRAWAL.equals(recentEnrollment.getEnrollmentType())) {
                result = recentEnrollment.getCountry();
            }
            result = OnsisConstants.VALUE_CANADA.equals(result) ? null : result;
            return result;
        }

        /**
         * Gets the entry demit country entry.
         *
         * @return String
         */
        public String getEntryDemitProvinceEntry() {
            String result = null;
            OnEnrollment recentEnrollment = (OnEnrollment) getRecentEnrollmentE();
            if (recentEnrollment != null) {
                result = recentEnrollment.getProvince();
            }
            return result;
        }

        /**
         * Gets the entry demit country entry.
         *
         * @return String
         */
        public String getEntryDemitProvinceExit() {
            String result = null;
            OnEnrollment recentEnrollment = (OnEnrollment) getRecentEnrollmentEW();
            if (recentEnrollment != null) {
                if (StudentEnrollment.WITHDRAWAL.equals(recentEnrollment.getEnrollmentType())) {
                    result = recentEnrollment.getProvince();
                }
            }
            result = OnsisConstants.VALUE_CANADA.equals(result) ? null : result;
            return result;
        }

        /**
         * Gets the fte.
         *
         * @return String
         */
        public String getFte() {
            OnEnrollment recentEnrolment = (OnEnrollment) getRecentEnrollmentES();
            return recentEnrolment == null ? OnsisConstants.DECIMAL_FORMAT_FTE.format(BigDecimal.ZERO)
                    : OnsisConstants.DECIMAL_FORMAT_FTE.format(recentEnrolment.getFte());
        }

        /**
         * Gets the high credit fte.
         *
         * @return String
         */
        public String getFteHc() {
            OnEnrollment recentEnrolment = (OnEnrollment) getRecentEnrollmentES();
            return recentEnrolment == null ? OnsisConstants.DECIMAL_FORMAT_FTE.format(BigDecimal.ZERO)
                    : OnsisConstants.DECIMAL_FORMAT_FTE.format(recentEnrolment.getFteHc());
        }

        /**
         * Gets the grade type.
         *
         * @param broker the broker
         * @param dictionaryExtractor the dictionary extractor
         * @param gradesHelper the grades helper
         * @return String
         */
        public String getGradeType(X2Broker broker,
                                   DictionaryExtractor dictionaryExtractor,
                                   GradesHelper gradesHelper) {
            String gradeLevel = getStudent().getGradeLevel();
            OnEnrollment enrollment = (OnEnrollment) getRecentEnrollment();

            /*
             * If this is a secondary enrollment,
             * get a primary enrollment span that overlaps this span
             * and get the value from it.
             */
            if (enrollment == null && isSecondary()) {
                OnAnnualSpan primarySpan = getBestPrimarySpanFor(broker, null);
                if (primarySpan != null) {
                    enrollment = (OnEnrollment) primarySpan.getRecentEnrollment();
                }
            }

            if (enrollment != null && !StringUtils.isBlank(enrollment.getGradeLevel())) {
                gradeLevel = enrollment.getGradeLevel();
            }

            if (!StringUtils.isBlank(gradeLevel)) {
                gradeLevel =
                        dictionaryExtractor.lookupStateValue(ToolStudent.FIELD_GRADE_LEVEL, gradeLevel);
            }

            if (StringUtils.isBlank(gradeLevel)) {
                int yog = getStudent().getYog();
                if (enrollment != null) {
                    int enrYog = enrollment.getYog();
                    if (enrYog > 0) {
                        yog = enrYog;
                    }
                }
                gradeLevel = gradesHelper.getGradeLevel(yog);
            }

            return gradeLevel;
        }

        /**
         * Gets the language prev school.
         *
         * @param dictionaryExtractor the dictionary extractor
         * @return String
         */
        public String getLanguagePrevSchool(DictionaryExtractor dictionaryExtractor) {
            String result = null;
            ToolEnrollment enrollment = getRecentEnrollmentE();
            if (enrollment != null) {
                result = dictionaryExtractor.getStateValue(enrollment,
                        OnEnrollment.FIELD_LANGUAGE_PREV_SKL);
            }
            return result;
        }

        /**
         * Gets the mature flag.
         *
         * @return Boolean
         */
        public Boolean getMatureFlag() {
            OnEnrollment enrollment = (OnEnrollment) getRecentEnrollmentES();
            return enrollment != null && enrollment.getMatureFlag() ? Boolean.TRUE : Boolean.FALSE;
        }

        /**
         * Gets the mobility type entry.
         *
         * @param dictionaryExtractor the dictionary extractor
         * @return String
         */
        public String getMobilityTypeEntry(DictionaryExtractor dictionaryExtractor) {
            String result = null;
            ToolEnrollment enr = getRecentEnrollmentE();
            if (enr != null) {
                result = dictionaryExtractor.getStateValue(enr, ToolEnrollment.FIELD_ENROLLMENT_CODE);
            }
            return result;
        }

        /**
         * Gets the mobility type exit.
         *
         * @param dictionaryExtractor the dictionary extractor
         * @return String
         */
        public String getMobilityTypeExit(DictionaryExtractor dictionaryExtractor) {
            String result = null;
            OnEnrollment recentEnrollment = (OnEnrollment) getRecentEnrollmentEW();
            if (recentEnrollment != null) {
                if (StudentEnrollment.WITHDRAWAL.equals(recentEnrollment.getEnrollmentType())) {
                    result = dictionaryExtractor.getStateValue(recentEnrollment,
                            ToolEnrollment.FIELD_ENROLLMENT_CODE);
                }
            }
            return result;
        }

        /**
         * Gets the mobility type from primary.
         *
         * @param dictionaryExtractor the dictionary extractor
         * @param isEntry boolean
         * @return String
         */
        public String getMobilityTypeFromPrimary(DictionaryExtractor dictionaryExtractor, boolean isEntry) {
            String mobilityType = null;
            OnSchool primarySchool = (OnSchool) getSchool();

            if (primarySchool != null && !StringUtils.isBlank(primarySchool.getBsid())) {
                String sklType = dictionaryExtractor.getStateValue(primarySchool,
                        OnSchool.FIELD_SCHOOL_TYPE_CODE);
                if (StringUtils.isEmpty(sklType)) {
                    throw new IllegalStateException(
                            "The school type On code for school " + primarySchool.getName() + " must be set.");
                }
                if (OnsisConstants.VALUE_SCHOOL_TYPES_SEPARATE.contains(sklType)) {
                    if (OnsisConstants.VALUE_SCHOOL_TYPES_SECONDARY.contains(primarySchool.getSchoolLevelCode())) {
                        mobilityType = OnsisConstants.VALUE_MOBILITY_TYPE_SAME_SEPARATE_SECONDARY;
                    } else {
                        mobilityType = OnsisConstants.VALUE_MOBILITY_TYPE_SAME_SEPARATE_ELEMENTARY;
                    }
                } else {
                    if (OnsisConstants.VALUE_SCHOOL_TYPES_SECONDARY.contains(primarySchool.getSchoolLevelCode())) {
                        mobilityType = OnsisConstants.VALUE_MOBILITY_TYPE_SAME_PUBLIC_SECONDARY;
                    } else {
                        mobilityType = OnsisConstants.VALUE_MOBILITY_TYPE_SAME_PUBLIC_ELEMENTARY;
                    }
                }
            } else {
                // for out of board school use enrollment
                mobilityType = isEntry
                        ? getMobilityTypeEntry(dictionaryExtractor)
                        : getMobilityTypeExit(dictionaryExtractor);
            }
            return mobilityType;
        }

        /**
         * Gets the recent enrollment EW.
         *
         * @return the recent enrollment EW
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan#getRecentEnrollmentEW()
         */
        @Override
        public ToolEnrollment getRecentEnrollmentEW() {
            // make sure to include any trailing withdrawal
            PlainDate saveDate = getQueryAsOfDate();
            setQueryAsOfDate(null);
            ToolEnrollment enr = super.getRecentEnrollmentEW();
            setQueryAsOfDate(saveDate);
            return enr;
        }

        /**
         * Gets the student.
         *
         * @return the student
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan#getStudent()
         */
        @Override
        public OnStudent getStudent() {
            return (OnStudent) super.getStudent();
        }

    }


    /**
     * A factory for creating OnAnnualSpan objects.
     */
    public static class OnAnnualSpanFactory extends AnnualSpanFactory {

        /**
         * Instantiates a new on annual span factory.
         *
         * @param broker the broker
         */
        public OnAnnualSpanFactory(X2Broker broker) {
            super();

            setAnnualSpanStartDateFn(new Function<AnnualSpan, PlainDate>() {
                @Override
                public PlainDate apply(AnnualSpan t) {
                    return t.getFirstActiveInSessionDate();
                }
            });
            setAnnualSpanEndDateFn(new Function<AnnualSpan, PlainDate>() {
                @Override
                public PlainDate apply(AnnualSpan t) {
                    OnAnnualSpan span = (OnAnnualSpan) t;
                    return span.calculateSpanEndDate(broker);
                }
            });
        }

        /**
         * Instantiate annual span.
         *
         * @param broker X2Broker
         * @param ssk ToolStudentSchool
         * @return AnnualSpan
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpanFactory#instantiateAnnualSpan(com.follett.fsc.core.k12.business.X2Broker,
         *      com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchool)
         */
        @Override
        public AnnualSpan instantiateAnnualSpan(X2Broker broker, ToolStudentSchool ssk) {
            OnAnnualSpan span = new OnAnnualSpan(ssk, broker);
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
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpanFactory#instantiateAnnualSpan(java.util.List,
         *      java.util.List,
         *      com.x2dev.procedures.statereporting.common.ToolBean.ToolDistrictContext)
         */
        @Override
        public AnnualSpan instantiateAnnualSpan(List<? extends ToolEnrollment> enrollmentsUpToYear,
                                                List<? extends ToolEnrollment> enrollmentsInThisYear,
                                                ToolDistrictContext spanContext) {
            OnAnnualSpan span = new OnAnnualSpan(enrollmentsUpToYear, enrollmentsInThisYear, spanContext);
            initialize(span);
            return span;
        }
    }

    /**
     * The Class OnConductAction.
     */
    public static class OnConductAction extends ToolConductAction {

        public static final String ALIAS_ACT_NON_ACADEMIC_PROGRAM = "all-act-NonAcademicProgram";

        private static final String ALIAS_ACT_APPEAL_DECISION_DATE = "all-act-AppealDecisionDate";
        private static final String ALIAS_ACT_APPEAL_START_DATE = "all-act-AppealStartDate";
        private static final String ALIAS_ACT_APPEAL_STATUS = "all-act-AppealStatus";
        private static final String ALIAS_ACT_AUTHORITY_TYPE = "all-act-AuthorityType";
        private static final String ALIAS_ACT_NEW_SUSPENSION_LENGTH = "all-act-NewSuspensionLength";
        private static final String ALIAS_ACT_PROGRAM_STATUS = "all-act-ProgramStatus";
        private static final String ALIAS_ACT_SUSPENSION_PROGRAM = "all-act-SuspensionProgram";

        public static final ToolBeanColumn FIELD_APPEAL_DECISION_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_ACTION, ALIAS_ACT_APPEAL_DECISION_DATE);
        public static final ToolBeanColumn FIELD_APPEAL_START_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_ACTION, ALIAS_ACT_APPEAL_START_DATE);
        public static final ToolBeanColumn FIELD_APPEAL_STATUS =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_ACTION, ALIAS_ACT_APPEAL_STATUS);
        public static final ToolBeanColumn FIELD_AUTHORITY_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_ACTION, ALIAS_ACT_AUTHORITY_TYPE);
        public static final ToolBeanColumn FIELD_NEW_SUSPENSION_LENGTH =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_ACTION, ALIAS_ACT_NEW_SUSPENSION_LENGTH);
        public static final ToolBeanColumn FIELD_NON_ACADEMIC_PROGRAM =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_ACTION, ALIAS_ACT_NON_ACADEMIC_PROGRAM);
        public static final ToolBeanColumn FIELD_PROGRAM_STATUS =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_ACTION, ALIAS_ACT_PROGRAM_STATUS);
        public static final ToolBeanColumn FIELD_SUSPENSION_PROGRAM =
                new ToolBeanColumn(SisBeanPaths.STUDENT_CONDUCT_ACTION, ALIAS_ACT_SUSPENSION_PROGRAM);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolConductAction.FULL_DEFINITION
                .expand(FIELD_APPEAL_DECISION_DATE,
                        FIELD_APPEAL_START_DATE,
                        FIELD_APPEAL_STATUS,
                        FIELD_AUTHORITY_TYPE,
                        FIELD_NEW_SUSPENSION_LENGTH,
                        FIELD_NON_ACADEMIC_PROGRAM,
                        FIELD_PROGRAM_STATUS,
                        FIELD_SUSPENSION_PROGRAM);

        /**
         * Instantiates a new onsis conduct action.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnConductAction(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the appeal decision date.
         *
         * @return Plain date
         */
        public PlainDate getAppealDecisionDate() {
            return getValueDate(FIELD_APPEAL_DECISION_DATE);
        }

        /**
         * Gets the appeal start date.
         *
         * @return Plain date
         */
        public PlainDate getAppealStartDate() {
            return getValueDate(FIELD_APPEAL_START_DATE);
        }

        /**
         * Gets the appeal status.
         *
         * @return String
         */
        public String getAppealStatus() {
            return getValueString(FIELD_APPEAL_STATUS);
        }

        /**
         * Returns the authority type.
         *
         * @return String
         */
        public String getAuthorityType() {
            return this.getValueString(FIELD_AUTHORITY_TYPE);
        }

        /**
         * Returns the new suspension length.
         *
         * @return String
         */
        public String getNewSuspensionLength() {
            return this.getValueString(FIELD_NEW_SUSPENSION_LENGTH);
        }

        /**
         * Returns the non academic program code.
         *
         * @return String
         */
        public String getNonAcademicProgram() {
            return this.getValueString(FIELD_NON_ACADEMIC_PROGRAM);
        }

        /**
         * Returns the non academic program data dictionary field.
         *
         * @return DataDictionaryField
         */
        public DataDictionaryField getNonAcademicProgramField() {
            return FIELD_NON_ACADEMIC_PROGRAM.getField(getDictionaryExtractor());
        }

        /**
         * Returns the non academic program state code.
         *
         * @return String
         */
        public List<String> getNonAcademicProgramState() {
            return this.getValuesReferenceState(FIELD_NON_ACADEMIC_PROGRAM);
        }

        /**
         * Returns the suspension program.
         *
         * @return String
         */
        public String getSuspensionProgram() {
            return this.getValueString(FIELD_SUSPENSION_PROGRAM);
        }

    }

    /**
     * The Class OnsisCourseRequest.
     */
    public static class OnCourseRequest extends ToolBean {

        // Nonquery Fields
        public static final ToolBeanColumn FIELD_COURSE_CODE_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_COURSE_REQUEST.schoolCourse().course(),
                        ALIAS_CRS_COURSE_CODE_TYPE);
        public static final ToolBeanColumn FIELD_CRS_NUMBER =
                new ToolBeanColumn(SisBeanPaths.STUDENT_COURSE_REQUEST.schoolCourse().course().number());
        public static final ToolBeanColumn FIELD_ELEMENTARY_SUBJECT_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_COURSE_REQUEST.schoolCourse().course(),
                        ALIAS_CRS_ELEMENTARY_SUBJECT_TYPE);
        public static final ToolBeanColumn FIELD_MINISTRY_COURSE_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_COURSE_REQUEST.schoolCourse().course(),
                        ALIAS_CRS_MINISTRY_COURSE_CODE);
        public static final ToolBeanColumn FIELD_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_COURSE_REQUEST.schoolOid());
        public static final ToolBeanColumn FIELD_SCHOOL_YEAR =
                new ToolBeanColumn(SisBeanPaths.STUDENT_COURSE_REQUEST.districtContext().schoolYear());
        public static final ToolBeanColumn FIELD_STUDENT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_COURSE_REQUEST.studentOid());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_COURSE_CODE_TYPE,
                        FIELD_CRS_NUMBER,
                        FIELD_ELEMENTARY_SUBJECT_TYPE,
                        FIELD_MINISTRY_COURSE_CODE,
                        FIELD_SCHOOL_OID,
                        FIELD_SCHOOL_YEAR,
                        FIELD_STUDENT_OID);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT_COURSE_REQUEST.getBeanType();
        }

        /**
         * Instantiates a new onsis course request.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnCourseRequest(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

    }

    /**
     * The Class OnsisEnrollment.
     */
    public static class OnEnrollment extends ToolEnrollment {
        private static final String ALIAS_ENR_ARRIVAL_STATUS = "all-enr-ArrivalStatus";
        private static final String ALIAS_ENR_BRD_RES_STAT_TYPE = "all-enr-BoardResidentStatus";
        private static final String ALIAS_ENR_CANADIAN_RES_STATUS = "all-enr-CanadianResidenceStatus";
        private static final String ALIAS_ENR_ELEM_ALT_REPORT_CARD_FLAG = "all-enr-ElementaryAlternateReportCard";
        private static final String ALIAS_ENR_ENROLMENT_REGISTER = "all-enr-EnrolmentRegister";
        private static final String ALIAS_ENR_ENTRY_DEMIT_COUNTRY = "all-enr-EntryDemitCountry";
        private static final String ALIAS_ENR_ENTRY_DEMIT_PROVINCE = "all-enr-EntryDemitProvince";
        private static final String ALIAS_ENR_FTE = "all-enr-Fte";
        private static final String ALIAS_ENR_FTE_HIGH_CREDIT = "all-enr-FteHc";
        private static final String ALIAS_ENR_GRADE_LEVEL = "all-enr-GradeLevel";
        private static final String ALIAS_ENR_LANGUAGE_PREV_SKL = "all-enr-LanguagePreviousSchool";
        private static final String ALIAS_ENR_MATURE_FLAG = "all-enr-MatureStudentFlag";

        public static final String FTE_ENROLLMENT_CODE = "IT";

        public static final ToolBeanColumn FIELD_ARRIVAL_STATUS =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_ENR_ARRIVAL_STATUS);
        public static final ToolBeanColumn FIELD_BRD_RES_STAT_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_ENR_BRD_RES_STAT_TYPE);
        public static final ToolBeanColumn FIELD_CANADIAN_RES_STATUS =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_ENR_CANADIAN_RES_STATUS);
        public static final ToolBeanColumn FIELD_ELEM_ALT_REPORT_CARD_FLAG =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_ENR_ELEM_ALT_REPORT_CARD_FLAG);
        public static final ToolBeanColumn FIELD_ENROLMENT_REGISTER =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_ENR_ENROLMENT_REGISTER);
        public static final ToolBeanColumn FIELD_ENTRY_DEMIT_COUNTRY =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_ENR_ENTRY_DEMIT_COUNTRY);
        public static final ToolBeanColumn FIELD_ENTRY_DEMIT_PROVINCE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_ENR_ENTRY_DEMIT_PROVINCE);
        public static final ToolBeanColumn FIELD_FTE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_ENR_FTE);
        public static final ToolBeanColumn FIELD_FTE_HIGH_CREDIT =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_ENR_FTE_HIGH_CREDIT);
        public static final ToolBeanColumn FIELD_GRADE_LEVEL =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_ENR_GRADE_LEVEL);
        public static final ToolBeanColumn FIELD_LANGUAGE_PREV_SKL =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT, ALIAS_ENR_LANGUAGE_PREV_SKL);
        public static final ToolBeanColumn FIELD_MATURE_FLAG =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT,
                        new ToolBeanColumn.AliasDefinition(ALIAS_ENR_MATURE_FLAG, null, false));
        public static final ToolBeanColumn FIELD_ORIGINATING_CLASS =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ENROLLMENT.originatingClass());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolEnrollment.FULL_DEFINITION
                .expand(FIELD_ARRIVAL_STATUS,
                        FIELD_BRD_RES_STAT_TYPE,
                        FIELD_CANADIAN_RES_STATUS,
                        FIELD_ELEM_ALT_REPORT_CARD_FLAG,
                        FIELD_ENROLMENT_REGISTER,
                        FIELD_ENTRY_DEMIT_COUNTRY,
                        FIELD_ENTRY_DEMIT_PROVINCE,
                        FIELD_FTE,
                        FIELD_FTE_HIGH_CREDIT,
                        FIELD_GRADE_LEVEL,
                        FIELD_LANGUAGE_PREV_SKL,
                        FIELD_MATURE_FLAG,
                        FIELD_ORIGINATING_CLASS);

        /**
         * Instantiates a new onsis enrollment.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnEnrollment(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the arrival status.
         *
         * @return String
         */
        public String getArrivalStatus() {
            return this.getValueString(FIELD_ARRIVAL_STATUS);
        }

        /**
         * Gets the board resident status.
         *
         * @return the board resident status
         */
        public String getBoardResidentStatus() {
            return this.getValueReferenceState(FIELD_BRD_RES_STAT_TYPE);
        }

        /**
         * Gets the board resident status plain value.
         *
         * @return the board resident status
         */
        public String getBoardResidentStatusPlain() {
            return this.getValueString(FIELD_BRD_RES_STAT_TYPE);
        }

        /**
         * Gets the country.
         *
         * @return String
         */
        public String getCountry() {
            return getValueReferenceState(FIELD_ENTRY_DEMIT_COUNTRY);
        }

        /**
         * Gets the elementary alternate report card flag.
         *
         * @return the elementary alternate report card flag
         */
        public String getElementaryAlternateReportCardFlag() {
            return getValueReferenceState(FIELD_ELEM_ALT_REPORT_CARD_FLAG);
        }

        /**
         * Gets the fte.
         *
         * @return Big decimal
         */
        public BigDecimal getFte() {
            BigDecimal value = this.getValueBigDecimal(FIELD_FTE);
            return value == null ? BigDecimal.ZERO : value;
        }

        /**
         * Gets the high credit fte.
         *
         * @return Big decimal
         */
        public BigDecimal getFteHc() {
            BigDecimal value = this.getValueBigDecimal(FIELD_FTE_HIGH_CREDIT);
            return value == null ? BigDecimal.ZERO : value;
        }

        /**
         * Gets the grade level.
         *
         * @return String
         */
        public String getGradeLevel() {
            return getValueString(FIELD_GRADE_LEVEL);
        }

        /**
         * Gets the languange prev school.
         *
         * @return String
         */
        public String getLanguagePrevSchool() {
            return getValueReferenceState(FIELD_LANGUAGE_PREV_SKL);
        }

        /**
         * Gets the mature flag.
         *
         * @return boolean
         */
        public boolean getMatureFlag() {
            return getValueLogical(FIELD_MATURE_FLAG);
        }

        /**
         * Gets the originating class.
         *
         * @return String
         */
        public String getOriginatingClass() {
            return getValueString(FIELD_ORIGINATING_CLASS);
        }

        /**
         * Gets the province.
         *
         * @return String
         */
        public String getProvince() {
            return getValueReferenceState(FIELD_ENTRY_DEMIT_PROVINCE);
        }

        /**
         * Gets register status.
         *
         * @return the register status
         */
        public String getRegister() {
            return this.getValueReferenceState(FIELD_ENROLMENT_REGISTER);
        }

        /**
         * Checks if is fte record.
         *
         * @return true, if is fte record
         */
        public boolean isFteRecord() {
            return (getOriginatingClass() != null
                    && getOriginatingClass().startsWith("com.x2dev.procedures.statereporting.on.OnsisDmFte"))
                    && StudentEnrollment.STATUS_CHANGE.equals(getEnrollmentType())
                    && FTE_ENROLLMENT_CODE.equals(getEnrollmentCode());
        }

    }

    /**
     * The Class OnsisGradebookColumnDefinition.
     */
    public static class OnGradebookColumnDefinition extends ToolBean {
        // Query only field
        public static final ToolBeanColumn FIELD_INCLUDE_ON_CONED_REGISTER =
                new ToolBeanColumn(SisBeanPaths.GRADEBOOK_COLUMN_DEFINITION,
                        new ToolBeanColumn.AliasDefinition(ALIAS_GCD_INCLUDE_ON_CONED_REGISTER, null, false));
        public static final ToolBeanColumn FIELD_SECTION_OID =
                new ToolBeanColumn(SisBeanPaths.GRADEBOOK_COLUMN_DEFINITION.masterScheduleOid());
        public static final ToolBeanColumn FIELD_SYSTEM_ONLY_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.GRADEBOOK_COLUMN_DEFINITION.systemOnlyIndicator());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_INCLUDE_ON_CONED_REGISTER,
                        FIELD_SECTION_OID,
                        FIELD_SYSTEM_ONLY_INDICATOR)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(FIELD_SYSTEM_ONLY_INDICATOR.resolve(getDictionaryExtractor()),
                                Boolean.FALSE);
                        if (!FIELD_INCLUDE_ON_CONED_REGISTER.isUndefined()) {
                            criteria.addEqualTo(FIELD_INCLUDE_ON_CONED_REGISTER.resolve(getDictionaryExtractor()),
                                    BooleanAsStringConverter.TRUE);
                        }
                        return criteria;
                    }
                });

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.GRADEBOOK_COLUMN_DEFINITION.getBeanType();
        }

        /**
         * Instantiates a new onsis gradebook column definition.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnGradebookColumnDefinition(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the include on con ed register.
         *
         * @return the include on con ed register
         */
        public boolean getIncludeOnConEdRegister() {
            return FIELD_INCLUDE_ON_CONED_REGISTER.isUndefined() ? true
                    : this.getValueLogical(FIELD_INCLUDE_ON_CONED_REGISTER);
        }

    }

    /**
     * The Class OnsisGradebookScore.
     */
    public static class OnGradebookScore extends ToolBean {

        public static final ToolBeanColumn FIELD_COMPLETED_DATE =
                new ToolBeanColumn(SisBeanPaths.GRADEBOOK_SCORE.completedDate());
        public static final ToolBeanColumn FIELD_INCLUDE_ON_CONED_REGISTER =
                new ToolBeanColumn(SisBeanPaths.GRADEBOOK_SCORE.columnDefinition(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_GCD_INCLUDE_ON_CONED_REGISTER, null, false));
        public static final ToolBeanColumn FIELD_SCORE =
                new ToolBeanColumn(SisBeanPaths.GRADEBOOK_SCORE.score());
        public static final ToolBeanColumn FIELD_SECTION_OID =
                new ToolBeanColumn(SisBeanPaths.GRADEBOOK_SCORE.columnDefinition().masterScheduleOid());
        public static final ToolBeanColumn FIELD_STUDENT_OID =
                new ToolBeanColumn(SisBeanPaths.GRADEBOOK_SCORE.studentOid());

        // Query only field
        public static final ToolBeanColumn FIELD_SYSTEM_ONLY_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.GRADEBOOK_SCORE.columnDefinition().systemOnlyIndicator());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_COMPLETED_DATE,
                        FIELD_INCLUDE_ON_CONED_REGISTER,
                        FIELD_SCORE,
                        FIELD_SECTION_OID,
                        FIELD_STUDENT_OID)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(FIELD_SYSTEM_ONLY_INDICATOR.resolve(getDictionaryExtractor()),
                                Boolean.FALSE);
                        if (!FIELD_INCLUDE_ON_CONED_REGISTER.isUndefined()) {
                            criteria.addEqualTo(FIELD_INCLUDE_ON_CONED_REGISTER.resolve(getDictionaryExtractor()),
                                    BooleanAsStringConverter.TRUE);
                        }
                        return criteria;
                    }
                });

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.GRADEBOOK_SCORE.getBeanType();
        }

        /**
         * Instantiates a new onsis gradebook column definition.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnGradebookScore(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Returns the Completed Date.
         * <p>
         * The data dictionary ID for this property is <code>gscComplDate</code>.
         *
         * @return PlainDate
         */
        public PlainDate getCompletedDate() {
            return this.getValueDate(FIELD_COMPLETED_DATE);
        }

        /**
         * Gets the include on con ed register.
         *
         * @return the include on con ed register
         */
        public boolean getIncludeOnConEdRegister() {
            return FIELD_INCLUDE_ON_CONED_REGISTER.isUndefined() ? true
                    : this.getValueLogical(FIELD_INCLUDE_ON_CONED_REGISTER);
        }

        /**
         * Returns the Score.
         * <p>
         * The data dictionary ID for this property is <code>gscScore</code>.
         *
         * @return String
         */
        public String getScore() {
            return this.getValueString(FIELD_SCORE);
        }

        /**
         * Returns the Score as double.
         * <p>
         * The data dictionary ID for this property is <code>gscScore</code>.
         *
         * @return String
         */
        public double getScoreAsDouble() {
            return this.getValueDouble(FIELD_SCORE);
        }


    }

    /**
     * The Class OnsisGraduationRequirement.
     */
    public static class OnGraduationRequirement extends ToolBean {
        /**
         * The Class NavFilter.
         */
        public static class NavFilter extends com.follett.fsc.core.k12.web.nav.Filter {

            /**
             * Instantiates a new nav filter.
             *
             * @param element Element
             * @param dictionary DataDictionary
             * @throws InvalidDictionaryIdException exception
             */
            public NavFilter(Element element, DataDictionary dictionary) throws InvalidDictionaryIdException {
                super(element, dictionary);
            }

        }

        public static final String CERT_REQ_COMPULSORY = "Compulsory";
        public static final String CERT_REQ_ELECTIVE = "Elective";

        public static final ToolBeanColumn FIELD_CODE =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_REQUIREMENT.code());
        public static final ToolBeanColumn FIELD_EVALUATION_DEFINITION =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_REQUIREMENT.evaluationDefinition());
        public static final ToolBeanColumn FIELD_PROGRAM_STUDIES_OID =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_REQUIREMENT.programStudiesOid());
        public static final ToolBeanColumn FIELD_TYPE =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_REQUIREMENT.type(), PredefinedConverter.INTEGER);
        public static final ToolBeanColumn FIELD_SUB_PROGRAM_STUDIES_OID =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_REQUIREMENT.subProgramStudiesOid());

        private static final String ATTRIBUTE_FIELD = "field";
        private static final String RELATION_STD_ADM = "relStdAsmOid";

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_CODE,
                        FIELD_EVALUATION_DEFINITION,
                        FIELD_PROGRAM_STUDIES_OID,
                        FIELD_SUB_PROGRAM_STUDIES_OID,
                        FIELD_TYPE);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.GRADUATION_REQUIREMENT.getBeanType();
        }

        /**
         * Instantiates a new onsis graduation requirement.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnGraduationRequirement(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        private boolean m_isFoldedFilterLoaded = false;
        private NavFilter m_foldedFilter;
        private Set<String> m_assessmentOids = null;

        /**
         * Clear assessment oids.
         */
        public void clearAssessmentOids() {
            m_assessmentOids = null;
        }

        /**
         * Gets the assessment oids.
         *
         * @param broker the broker
         * @return the assessment oids
         */
        public Set<String> getAssessmentOids(X2Broker broker) {
            CollectionCriteriaHelper helper = null;
            if (m_assessmentOids == null) {
                m_assessmentOids = new HashSet();
                if (getFoldedFilter(broker) != null) {
                    try {
                        Set<String> asmOids = ToolBean.getCachedToolBeanOids(ShsmAssessment.class);
                        X2Criteria criteria = new X2Criteria();
                        if (asmOids.size() > ToolBean.MAX_SAFE_PARAMETERS) {
                            helper = new CollectionCriteriaHelper(asmOids, broker);
                            helper.applyToCriteria(SisBeanPaths.STUDENT_ASSESSMENT.oid().getPath(), criteria);
                        } else {
                            criteria.addIn(SisBeanPaths.STUDENT_ASSESSMENT.oid().getPath(), asmOids);
                        }
                        getFoldedFilter(broker).applyTo(criteria, null);
                        String[] columns = new String[] {X2BaseBean.COL_OID};
                        QueryByCriteria query = new ColumnQuery(StudentAssessment.class, columns, criteria);
                        try (QueryIterator iterator = broker.getReportQueryIteratorByQuery(query)) {
                            while (iterator.hasNext()) {
                                Object[] row = (Object[]) iterator.next();
                                m_assessmentOids.add((String) row[0]);
                            }
                        }
                    } catch (Exception e) {
                        StringBuilder output = new StringBuilder();
                        output.append("Evaluation fails for " + toString());
                        output.append(System.getProperty("line.separator"));
                        StringWriter sw = new StringWriter();
                        PrintWriter pw = new PrintWriter(sw);
                        e.printStackTrace(pw);
                        output.append(sw.toString());
                        throw new IllegalStateException(output.toString());
                    } finally {
                        if (helper != null) {
                            helper.cleanup();
                        }
                    }
                }
            }
            return m_assessmentOids;
        }

        /**
         * Returns the Code.
         * <p>
         * The data dictionary ID for this property is <code>grqCode</code>.
         *
         * @return String
         */
        public String getCode() {
            return getValueString(FIELD_CODE);
        }

        /**
         * Returns the Evaluation defintion.
         * <p>
         * The data dictionary ID for this property is <code>grqEvalDef</code>.
         *
         * @return String
         */
        public String getEvaluationDefinition() {
            return getValueString(FIELD_EVALUATION_DEFINITION);
        }

        /**
         * Gets the folded filter.
         *
         * @param broker the broker
         * @return the folded filter
         */
        public NavFilter getFoldedFilter(X2Broker broker) {
            if (!m_isFoldedFilterLoaded) {
                m_isFoldedFilterLoaded = true;
                m_foldedFilter = null;
                List<NavFilter> filters = parseEvaluationDefinitionAsmFilters(broker);
                for (NavFilter filter : filters) {
                    if (m_foldedFilter == null) {
                        m_foldedFilter = filter;
                    } else {
                        m_foldedFilter.addFilter(filter);
                    }
                }
            }
            return m_foldedFilter;
        }

        /**
         * Returns the Related program study object identifier.
         * <p>
         * The data dictionary ID for this property is <code>grqGprOID</code>.
         *
         * @return String
         */
        public String getProgramStudiesOid() {
            return getValueString(FIELD_PROGRAM_STUDIES_OID);
        }

        /**
         * Returns the Related sub program object identifier.
         * <p>
         * The data dictionary ID for this property is <code>grqGprOIDSubPg</code>.
         *
         * @return String
         */
        public String getSubProgramStudiesOid() {
            return getValueString(FIELD_SUB_PROGRAM_STUDIES_OID);
        }

        /**
         * Matches assessment.
         *
         * @param broker the broker
         * @param asm the asm
         * @return true, if successful
         */
        public boolean matchesAssessment(X2Broker broker, OnStudentAssessment asm) {
            return getAssessmentOids(broker).contains(asm.getOid());
        }

        /**
         * Parses the evaluation definition asm filters.
         *
         * @param broker the broker
         * @return List
         */
        private List<NavFilter> parseEvaluationDefinitionAsmFilters(X2Broker broker) {
            String evalXml = getEvaluationDefinition();
            List<NavFilter> filters = new LinkedList<>();
            if (!StringUtils.isEmpty(evalXml)) {
                SAXBuilder builder = new SAXBuilder();
                org.jdom.Document document = null;
                try {
                    document = builder.build(new ByteArrayInputStream(evalXml.getBytes()));
                } catch (JDOMException e) {
                    e.printStackTrace();
                } catch (IOException e) {
                    e.printStackTrace();
                }

                if (document != null) {
                    Element root = document.getRootElement();
                    DataDictionary dictionary =
                            DataDictionary.getDistrictDictionary(broker.getPersistenceKey());
                    List filterElements = root.getChildren(com.follett.fsc.core.k12.web.nav.Filter.FILTER_ELEMENT);
                    if (filterElements != null) {
                        Iterator filterIterator = filterElements.iterator();
                        while (filterIterator.hasNext()) {
                            Element filterElement = (Element) filterIterator.next();
                            Attribute fieldAttribute = filterElement.getAttribute(ATTRIBUTE_FIELD);
                            String fieldValue = fieldAttribute.getValue();
                            if (fieldValue.startsWith(RELATION_STD_ADM)) {
                                String updatedFieldValue =
                                        fieldValue.substring(RELATION_STD_ADM.length() + 1, fieldValue.length());
                                filterElement.setAttribute(ATTRIBUTE_FIELD, updatedFieldValue);
                                NavFilter filter =
                                        new NavFilter(filterElement, dictionary);
                                filters.add(filter);
                            }
                        }
                    }
                }
            }
            return filters;
        }
    }

    /**
     * The Class OnsisGraduationStudentProgram.
     */
    public static class OnGraduationStudentProgram extends ToolBean {
        public static final String GPR_NAME_OS = "OS";

        public static final String DIPLOMA_TYPE_COLLEGE_PREP = "1";
        public static final String DIPLOMA_TYPE_HONORS_COLLEGE_PREP = "2";
        public static final String DIPLOMA_TYPE_OSSD_SHSM = "4";
        public static final String DIPLOMA_TYPE_SSGD = "3";
        public static final String ONLINE_GRADUATION_REQUIREMENT_COMPLETED = "1";
        public static final String ONLINE_GRADUATION_REQUIREMENT_NOT_APPLICABLE = "2";

        private static final String ALIAS_GPR_DIPLOMA_TYPE = "all-gpr-DiplomaType";
        private static final String ALIAS_GPR_SHSM_PROGRAM_TYPE = "all-gpr-SHSMProgramType";
        private static final String ALIAS_GSR_BSID_DIPLOMA_EARNED = "all-gsr-BsidDiplomaEarned";
        private static final String ALIAS_GSR_END_DATE = "all-gsr-EndDate";
        private static final String ALIAS_GSR_ISSUED_DATE = "all-gsr-IssuedDate";
        private static final String ALIAS_GSR_ONTARIO_SKILLS_PASSPORT = "all-gsr-OntarioSkillsPassport";
        private static final String ALIAS_GSR_SPC_CODING = "all-gsr-SpcCoding";
        private static final String ALIAS_GSR_SPC_ICE = "all-gsr-SpcIce";
        private static final String ALIAS_GSR_SPC_MATH_LITERACY = "all-gsr-SpcMathLiteracy";
        private static final String ALIAS_GSR_START_DATE = "all-gsr-StartDate";

        public static final ToolBeanColumn FIELD_BSID_DIPLOMA_EARNED =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_STUDENT_PROGRAM, ALIAS_GSR_BSID_DIPLOMA_EARNED);
        public static final ToolBeanColumn FIELD_DIPLOMA_TYPE =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_STUDENT_PROGRAM.programStudies(), ALIAS_GPR_DIPLOMA_TYPE);
        public static final ToolBeanColumn FIELD_END_DATE =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_STUDENT_PROGRAM, ALIAS_GSR_END_DATE);
        public static final ToolBeanColumn FIELD_ISSUED_DATE =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_STUDENT_PROGRAM, ALIAS_GSR_ISSUED_DATE);
        public static final ToolBeanColumn FIELD_ONTARIO_SKILLS_PASSPORT =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_STUDENT_PROGRAM, ALIAS_GSR_ONTARIO_SKILLS_PASSPORT);
        public static final ToolBeanColumn FIELD_PRIMARY_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_STUDENT_PROGRAM.primaryIndicator());
        public static final ToolBeanColumn FIELD_PROGRAM_NAME =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_STUDENT_PROGRAM.programStudies().name());
        public static final ToolBeanColumn FIELD_PROGRAM_STUDIES_OID =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_STUDENT_PROGRAM.programStudies().oid());
        public static final ToolBeanColumn FIELD_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_STUDENT_PROGRAM.student().schoolOid());
        public static final ToolBeanColumn FIELD_SHSM_PROGRAM_TYPE =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_STUDENT_PROGRAM.programStudies(),
                        ALIAS_GPR_SHSM_PROGRAM_TYPE);
        public static final ToolBeanColumn FIELD_SPC_CODING =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_STUDENT_PROGRAM, ALIAS_GSR_SPC_CODING);
        public static final ToolBeanColumn FIELD_SPC_ICE =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_STUDENT_PROGRAM, ALIAS_GSR_SPC_ICE);
        public static final ToolBeanColumn FIELD_SPC_MATH_LITERACY =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_STUDENT_PROGRAM, ALIAS_GSR_SPC_MATH_LITERACY);
        public static final ToolBeanColumn FIELD_START_DATE =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_STUDENT_PROGRAM, ALIAS_GSR_START_DATE);
        public static final ToolBeanColumn FIELD_STUDENT_OID =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_STUDENT_PROGRAM.studentOid());

        public static ToolBeanRelationship CHILD_WAIVERS =
                new ToolBeanRelationship(SisBeanPaths.GRADUATION_STUDENT_PROGRAM.studentWaivers().getBeanType(),
                        SisBeanPaths.GRADUATION_STUDENT_PROGRAM.studentWaivers().getValueType(),
                        SisBeanPaths.GRADUATION_STUDENT_PROGRAM.studentWaivers().getPath(),
                        SisBeanPaths.GRADUATION_STUDENT_WAIVER.studentProgramOid().getPath(),
                        SisBeanPaths.GRADUATION_STUDENT_PROGRAM.studentWaivers().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_BSID_DIPLOMA_EARNED,
                        FIELD_DIPLOMA_TYPE,
                        FIELD_END_DATE,
                        FIELD_ISSUED_DATE,
                        FIELD_ONTARIO_SKILLS_PASSPORT,
                        FIELD_PRIMARY_INDICATOR,
                        FIELD_PROGRAM_NAME,
                        FIELD_PROGRAM_STUDIES_OID,
                        FIELD_SCHOOL_OID,
                        FIELD_SHSM_PROGRAM_TYPE,
                        FIELD_SPC_CODING,
                        FIELD_SPC_ICE,
                        FIELD_SPC_MATH_LITERACY,
                        FIELD_START_DATE,
                        FIELD_STUDENT_OID)
                .expandRelationships(CHILD_WAIVERS);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.GRADUATION_STUDENT_PROGRAM.getBeanType();
        }

        /**
         * Instantiates a new onsis graduation student program.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnGraduationStudentProgram(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the bsid diploma earned.
         *
         * @return String
         */
        public String getBsidDiplomaEarned() {
            return getValueString(FIELD_BSID_DIPLOMA_EARNED);
        }

        /**
         * Gets the diploma type.
         *
         * @return String
         */
        public String getDiplomaType() {
            return getValueReferenceState(FIELD_DIPLOMA_TYPE);
        }

        /**
         * Gets the end date.
         *
         * @return Plain date
         */
        public PlainDate getEndDate() {
            return getValueDate(FIELD_END_DATE);
        }

        /**
         * Gets the issued date.
         *
         * @return Plain date
         */
        public PlainDate getIssuedDate() {
            return getValueDate(FIELD_ISSUED_DATE);
        }

        /**
         * Gets the ontario skills passport.
         *
         * @return String
         */
        public String getOntarioSkillsPassport() {
            return getValueString(FIELD_ONTARIO_SKILLS_PASSPORT);
        }

        /**
         * Gets the primary indicator.
         *
         * @return boolean
         */
        public boolean getPrimaryIndicator() {
            return getValueLogical(FIELD_PRIMARY_INDICATOR);
        }

        /**
         * Gets the program studies oid.
         *
         * @return String
         */
        public String getProgramStudiesOid() {
            return getValueString(FIELD_PROGRAM_STUDIES_OID);
        }

        /**
         * Gets the shsm program type.
         *
         * @return String
         */
        public String getShsmProgramType() {
            return getValueReferenceState(FIELD_SHSM_PROGRAM_TYPE);
        }

        /**
         * Gets the spc coding indicator.
         *
         * @return boolean
         */
        public boolean getSpcCodingIndicator() {
            return getValueLogical(FIELD_SPC_CODING);
        }

        /**
         * Gets the spc ice indicator.
         *
         * @return boolean
         */
        public boolean getSpcIceIndicator() {
            return getValueLogical(FIELD_SPC_ICE);
        }

        /**
         * Gets the spc math literacy indicator.
         *
         * @return boolean
         */
        public boolean getSpcMathLiteracyIndicator() {
            return getValueLogical(FIELD_SPC_MATH_LITERACY);
        }

        /**
         * Gets the start date.
         *
         * @return Plain date
         */
        public PlainDate getStartDate() {
            return getValueDate(FIELD_START_DATE);
        }

        /**
         * Returns the student.
         *
         * @param broker X2Broker
         * @return SisStudent
         */
        public ToolStudent getStudent(X2Broker broker) {
            String stdOid = getValueString(FIELD_STUDENT_OID);
            return getBeanByOid(broker, ToolStudent.class, stdOid, true);
        }

        /**
         * Gets the program waivers.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<OnGraduationStudentWaiver> getWaivers(X2Broker broker) {
            return (List<OnGraduationStudentWaiver>) getChildren(broker, CHILD_WAIVERS);
        }
    }

    /**
     * The Class OnsisGraduationStudentWaiver.
     */
    public static class OnGraduationStudentWaiver extends ToolBean {
        private static final String ALIAS_GSW_WAIVER_REASON = "all-gsw-WaiverReason";

        public static final ToolBeanColumn FIELD_REQUIREMENT_CODE =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_STUDENT_WAIVER.requirement().code());
        public static final ToolBeanColumn FIELD_STUDENT_PROGRAM_OID =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_STUDENT_WAIVER.studentProgramOid());
        public static final ToolBeanColumn FIELD_WAIVER_REASON =
                new ToolBeanColumn(SisBeanPaths.GRADUATION_STUDENT_WAIVER,
                        new ToolBeanColumn.AliasDefinition(ALIAS_GSW_WAIVER_REASON, null, false));

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_REQUIREMENT_CODE,
                        FIELD_STUDENT_PROGRAM_OID,
                        FIELD_WAIVER_REASON);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.GRADUATION_STUDENT_WAIVER.getBeanType();
        }

        /**
         * Instantiates a new onsis graduation student program.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnGraduationStudentWaiver(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the waiver reason.
         *
         * @return the waiver reason
         */
        public String getWaiverReason() {
            return getValueReferenceState(FIELD_WAIVER_REASON);
        }

    }

    /**
     * The Class OnOrganization.
     */
    public static class OnOrganization extends ToolOrganization {
        private static final String ALIAS_BOARD_TYPE = "rcd-bsid-board-type";
        private static final String ALIAS_ORG_LANGUAGE = "all-org-BoardLanguage";
        private static final String RTB_OID_BSID_BOARDS = "rtbOnBsidBoard";
        private static final String BOARD_TYPE_CATHOLIC = "CD";

        public static final ToolBeanColumn FIELD_ORG_LANGUAGE =
                new ToolBeanColumn(SisBeanPaths.ORGANIZATION,
                        new ToolBeanColumn.AliasDefinition(ALIAS_ORG_LANGUAGE, null, false));

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolOrganization.FULL_DEFINITION
                .expand(FIELD_ORG_LANGUAGE);

        /**
         * Instantiates a new on organization.
         *
         * @param columns the columns
         * @param data the data
         */
        public OnOrganization(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        Boolean m_isCatholic;

        /**
         * Gets the language
         *
         * @return String
         */
        public String getLanguage() {
            return getValueString(FIELD_ORG_LANGUAGE);
        }

        /**
         * Gets the language
         *
         * @return String
         */
        public String getLanguageStateValue() {
            return getValueReferenceState(FIELD_ORG_LANGUAGE);
        }

        /**
         * Checks if is catholic.
         *
         * @param broker the broker
         * @return true, if is catholic
         */
        public boolean isCatholic(X2Broker broker) {
            if (m_isCatholic == null) {
                m_isCatholic = Boolean.FALSE;
                ReferenceTable rtb = broker.getBeanByOid(ReferenceTable.class, RTB_OID_BSID_BOARDS);
                String id = getId();
                if (rtb != null && !StringUtils.isEmpty(id)) {
                    ReferenceCode rcd = rtb.getCodeMap(broker).get(id);
                    if (rcd != null) {
                        String beanPath = Stream.of(rtb)
                                .map(item -> item.getExtendedDataDictionary())
                                .filter(Objects::nonNull)
                                .map(dictAttrib -> DataDictionary.getDistrictDictionary(dictAttrib,
                                        broker.getPersistenceKey()))
                                .filter(Objects::nonNull)
                                .map(dict -> dict.findDataDictionaryFieldByAlias(ALIAS_BOARD_TYPE))
                                .filter(Objects::nonNull)
                                .map(field -> field.getJavaName())
                                .findFirst()
                                .orElse(null);
                        m_isCatholic =
                                Boolean.valueOf(BOARD_TYPE_CATHOLIC.equals(rcd.getFieldValueByBeanPath(beanPath)));
                    }
                }
            }
            return m_isCatholic.booleanValue();
        }

    }

    /**
     * The Class OnsisTranscriptRubric.
     */
    public static class OnRubricAssessmentPerformance extends ToolRubricAssessmentPerformance {
        public static final ToolBeanColumn FIELD_TRANSCRIPT_GRADE_LEVEL =
                new ToolBeanColumn(SisBeanPaths.RUBRIC_ASSESSMENT_PERFORMANCE.rubricAssessment().transcriptRubrics()
                        .transcript().gradeLevel());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolRubricAssessmentPerformance.FULL_DEFINITION
                .expand(FIELD_TRANSCRIPT_GRADE_LEVEL)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        DistrictSchoolYearContext context =
                                (DistrictSchoolYearContext) ToolBean.getPreference(ToolBean.PREFERENCE_CURRENT_CONTEXT);
                        if (context == null) {
                            throw new IllegalStateException("ToolBean preference value "
                                    + ToolBean.PREFERENCE_CURRENT_CONTEXT + " must be set.");
                        }
                        criteria.addEqualTo(ToolRubricAssessmentPerformance.FIELD_DISTRICT_CONTEXT_OID.resolve(null),
                                context.getOid());
                        criteria.addNotEmpty(ToolRubricAssessmentPerformance.FIELD_ID.resolve(null),
                                broker.getPersistenceKey());
                        return criteria;
                    }
                });

        /**
         * Instantiates a new onsis transcript rubric.
         *
         * @param columns the columns
         * @param data the data
         */
        public OnRubricAssessmentPerformance(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the transcript grade level.
         *
         * @return the transcript grade level
         */
        public String getTranscriptGradeLevel() {
            return getValueString(FIELD_TRANSCRIPT_GRADE_LEVEL);
        }

    }

    /**
     * The Class OnsisRubricCriterion.
     */
    public static class OnRubricCriterion extends ToolRubricCriterion {
        private static final String ALIAS_RBC_LEARNING_SKILLS = "all-rbc-LearningSkill";
        private static final String ALIAS_RBC_SUBJECT_STRAND = "all-rbc-SubjectStrand";

        public static final ToolBeanColumn FIELD_LEARNING_SKILLS =
                new ToolBeanColumn(SisBeanPaths.RUBRIC_CRITERION, ALIAS_RBC_LEARNING_SKILLS);
        public static final ToolBeanColumn FIELD_SUBJECT_STRAND =
                new ToolBeanColumn(SisBeanPaths.RUBRIC_CRITERION, ALIAS_RBC_SUBJECT_STRAND);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolRubricCriterion.FULL_DEFINITION
                .expand(FIELD_LEARNING_SKILLS,
                        FIELD_SUBJECT_STRAND);

        /**
         * Instantiates a new onsis rubric criterion.
         *
         * @param columns the columns
         * @param data the data
         */
        public OnRubricCriterion(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the subject strand.
         *
         * @return the subject strand
         */
        public String getSubjectStrand() {
            return this.getValueReferenceState(FIELD_SUBJECT_STRAND);
        }

        /**
         * Gets the learning skills.
         *
         * @return the learning skills
         */
        public String getLearningSkills() {
            return this.getValueReferenceState(FIELD_LEARNING_SKILLS);
        }

    }

    /**
     * The Class OnsisSchedule.
     */
    public static class OnSchedule extends ToolSchedule {

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolSchedule.FULL_DEFINITION;

        /**
         * Instantiates a new onsis schedule.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnSchedule(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        private PlainDate m_lastScheduledDate;

        /**
         * Gets the schedule bell period.
         *
         * @param broker the broker
         * @param perId the per id
         * @param countDate the count date
         * @param bellScheduleOids the bell schedule oids
         * @return the schedule bell period
         */
        public OnScheduleBellPeriod getScheduleBellPeriod(X2Broker broker,
                                                          String perId,
                                                          PlainDate countDate,
                                                          Set<String> bellScheduleOids) {
            return getScheduleBellPeriod(broker, perId, countDate, bellScheduleOids, null);
        }

        /**
         * Gets the schedule bell period.
         *
         * @param broker the broker
         * @param perId the per id
         * @param countDate the count date
         * @param bellScheduleOids the bell schedule oids
         * @param calendar the calendar
         * @return the schedule bell period
         */
        public OnScheduleBellPeriod getScheduleBellPeriod(X2Broker broker,
                                                          String perId,
                                                          PlainDate countDate,
                                                          Collection<String> bellScheduleOids,
                                                          ToolSchoolCalendar calendar) {
            return getScheduleBellPeriods(broker).stream()
                    .map(bpe -> (OnScheduleBellPeriod) bpe)
                    .filter(bpe -> perId.equals(bpe.getId()))
                    .filter(bpe -> {
                        if (bellScheduleOids.isEmpty()) {
                            return true;
                        }
                        if (!bellScheduleOids.contains(bpe.getBellScheduleOid())) {
                            return false;
                        }

                        if (countDate == null) {
                            return true;
                        }
                        return bpe.getDateRange().contains(countDate);
                    })
                    .collect(Collectors.maxBy(new Comparator<ToolScheduleBellPeriod>() {

                        @Override
                        public int compare(ToolScheduleBellPeriod bpe1, ToolScheduleBellPeriod bpe2) {
                            int compare = 0;
                            if (calendar != null) {
                                Optional<ToolSchoolCalendarDate> calDate = calendar.getCalendarDates(broker).stream()
                                        .filter(csd -> countDate.equals(csd.getDate()))
                                        .findAny();
                                if (calDate.isPresent()) {
                                    if (bpe1.getBellScheduleOid().equals(calDate.get().getBellScheduleOid())) {
                                        if (!bpe2.getBellScheduleOid().equals(calDate.get().getBellScheduleOid())) {
                                            compare = 1;
                                        }
                                    } else if (bpe2.getBellScheduleOid().equals(calDate.get().getBellScheduleOid())) {
                                        compare = -1;
                                    }
                                }
                                if (compare == 0) {
                                    int count1 = (int) calendar.getCalendarDates(broker).stream()
                                            .filter(csd -> bpe1.getBellScheduleOid().equals(csd.getBellScheduleOid()))
                                            .count();
                                    int count2 = (int) calendar.getCalendarDates(broker).stream()
                                            .filter(csd -> bpe2.getBellScheduleOid().equals(csd.getBellScheduleOid()))
                                            .count();
                                    compare = count1 - count2;
                                }
                            }
                            if (compare == 0) {
                                compare = ((int) (bpe1.getEndTime().getTimeInMinutes()
                                        - bpe1.getStartTime().getTimeInMinutes()))
                                        - ((int) (bpe2.getEndTime().getTimeInMinutes()
                                                - bpe2.getStartTime().getTimeInMinutes()));
                            }
                            return compare;
                        }
                    })).orElse(null);
        }

        /**
         * Gets the last scheduled date.
         *
         * @param broker X2Broker
         * @return Plain date
         */
        PlainDate getLastScheduledDate(X2Broker broker) {
            if (m_lastScheduledDate == null) {
                m_lastScheduledDate = getScheduleTerms(broker).stream()
                        .map(trm -> trm.getScheduleTermDates(broker))
                        .flatMap(Collection::stream)
                        .map(tmd -> tmd.getEndDate())
                        .max(Comparator.naturalOrder()).orElse(null);
            }
            return m_lastScheduledDate;
        }
    }

    /**
     * The Class OnsisScheduleBellPeriod.
     */
    public static class OnScheduleBell extends ToolScheduleBell {
        public static final ToolBeanColumn FIELD_DEFAULT_BELL_TIME_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_BELL, ALIAS_BEL_DEFAULT_BELL_TIME);
        public static final ToolBeanColumn FIELD_END_DATE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_BELL, ALIAS_BEL_END_DATE);
        public static final ToolBeanColumn FIELD_START_DATE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_BELL, ALIAS_BEL_START_DATE);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolScheduleBell.FULL_DEFINITION
                .expand(FIELD_DEFAULT_BELL_TIME_INDICATOR,
                        FIELD_END_DATE,
                        FIELD_START_DATE)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(FIELD_DEFAULT_BELL_TIME_INDICATOR.resolve(getDictionaryExtractor()),
                                BooleanAsStringConverter.TRUE);
                        return criteria;
                    }
                });

        /**
         * Instantiates a new onsis schedule bell period.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnScheduleBell(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        private Range<Date> m_dateRange;

        /**
         * Gets the range.
         *
         * @return Range
         */
        public Range<Date> getDateRange() {
            if (m_dateRange == null) {
                m_dateRange = Range.of(getStartDate(), getEndDate());
            }
            return m_dateRange;
        }

        /**
         * Gets the default bell time indicator.
         *
         * @return the default bell time indicator
         */
        public boolean getDefaultBellTimeIndicator() {
            return getValueLogical(FIELD_DEFAULT_BELL_TIME_INDICATOR);
        }

        /**
         * Gets the end date.
         *
         * @return the end date
         */
        public PlainDate getEndDate() {
            return this.getValueDate(FIELD_END_DATE);
        }

        /**
         * Gets the start date.
         *
         * @return the start date
         */
        public PlainDate getStartDate() {
            return this.getValueDate(FIELD_START_DATE);
        }

    }
    /**
     * The Class OnsisScheduleBellPeriod.
     */
    public static class OnScheduleBellPeriod extends ToolScheduleBellPeriod {

        public static final ToolBeanColumn FIELD_DEFAULT_BELL_TIME_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_BELL_PERIOD.bellSchedule(), ALIAS_BEL_DEFAULT_BELL_TIME);
        public static final ToolBeanColumn FIELD_END_DATE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_BELL, ALIAS_BEL_END_DATE);
        public static final ToolBeanColumn FIELD_START_DATE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_BELL, ALIAS_BEL_START_DATE);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolScheduleBellPeriod.FULL_DEFINITION
                .expand(FIELD_DEFAULT_BELL_TIME_INDICATOR,
                        FIELD_END_DATE,
                        FIELD_START_DATE)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(FIELD_DEFAULT_BELL_TIME_INDICATOR.resolve(getDictionaryExtractor()),
                                BooleanAsStringConverter.TRUE);
                        return criteria;
                    }
                });

        /**
         * Instantiates a new onsis schedule bell period.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnScheduleBellPeriod(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        private Range<Date> m_dateRange;

        /**
         * Gets the range.
         *
         * @return Range
         */
        public Range<Date> getDateRange() {
            if (m_dateRange == null) {
                m_dateRange = Range.of(getStartDate(), getEndDate());
            }
            return m_dateRange;
        }

        /**
         * Gets the end date.
         *
         * @return the end date
         */
        public PlainDate getEndDate() {
            return this.getValueDate(FIELD_END_DATE);
        }

        /**
         * Gets the start date.
         *
         * @return the start date
         */
        public PlainDate getStartDate() {
            return this.getValueDate(FIELD_START_DATE);
        }

    }

    /**
     * The Class OnScheduleTeacher.
     */
    public static class OnScheduleTeacher extends ToolScheduleTeacher {

        /**
         * The Enum CoreScheduledMode.
         */
        public enum CoreScheduledMode {
            SCHEDULED_ON_DATE, SCHEDULED_BEFORE_DATE
        }

        public static final String MTC_TEACHING_ROLE_CO_TEACH = "Co-Teach";
        public static final String MTC_TEACHING_ROLE_EXCLUDE = "Exclude";
        public static final String MTC_TEACHING_ROLE_PRIMARY = "Primary";

        private static final String ALIAS_MTC_END_DATE = "all-mtc-EndDate";
        private static final String ALIAS_MTC_END_DATE_2 = "all-mtc-EndDate2";
        private static final String ALIAS_MTC_POSITION = "all-mtc-Position";
        private static final String ALIAS_MTC_POSITION_2 = "all-mtc-Position2";
        private static final String ALIAS_MTC_REPORT_SPECIALTY_SUBJECT = "all-mtc-ReportSpecialtySubject";
        private static final String ALIAS_MTC_REPORT_SPECIALTY_SUBJECT_2 = "all-mtc-ReportSpecialtySubject2";
        private static final String ALIAS_MTC_ROLE_2 = "all-mtc-Role2";
        private static final String ALIAS_MTC_START_DATE = "all-mtc-StartDate";
        private static final String ALIAS_MTC_START_DATE_2 = "all-mtc-StartDate2";

        public static final ToolBeanColumn FIELD_END_DATE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER_TEACHER, ALIAS_MTC_END_DATE);
        public static final ToolBeanColumn FIELD_END_DATE_2 =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER_TEACHER, ALIAS_MTC_END_DATE_2);
        public static final ToolBeanColumn FIELD_GRADE_LEVEL =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER_TEACHER.section().schoolCourse().course().gradeLevel());
        public static final ToolBeanColumn FIELD_POSITION =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER_TEACHER, ALIAS_MTC_POSITION);
        public static final ToolBeanColumn FIELD_POSITION_2 =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER_TEACHER, ALIAS_MTC_POSITION_2);
        public static final ToolBeanColumn FIELD_REPORT_SPECIALTY_SUBJECT =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER_TEACHER, ALIAS_MTC_REPORT_SPECIALTY_SUBJECT);
        public static final ToolBeanColumn FIELD_REPORT_SPECIALTY_SUBJECT_2 =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER_TEACHER, ALIAS_MTC_REPORT_SPECIALTY_SUBJECT_2);
        public static final ToolBeanColumn FIELD_ROLE_2 =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER_TEACHER, ALIAS_MTC_ROLE_2);
        public static final ToolBeanColumn FIELD_START_DATE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER_TEACHER, ALIAS_MTC_START_DATE);
        public static final ToolBeanColumn FIELD_START_DATE_2 =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER_TEACHER, ALIAS_MTC_START_DATE_2);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolScheduleTeacher.FULL_DEFINITION
                .expand(FIELD_END_DATE,
                        FIELD_END_DATE_2,
                        FIELD_GRADE_LEVEL,
                        FIELD_POSITION,
                        FIELD_POSITION_2,
                        FIELD_REPORT_SPECIALTY_SUBJECT,
                        FIELD_REPORT_SPECIALTY_SUBJECT_2,
                        FIELD_ROLE_2,
                        FIELD_START_DATE,
                        FIELD_START_DATE_2);

        private List<Range<Date>> m_dateIntervals;
        private Map<PlainDate, Boolean> m_isCoreScheduleMap;

        /**
         * Instantiates a new onsis schedule teacher.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnScheduleTeacher(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the date intervals.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<Range<Date>> getDateIntervals(X2Broker broker) {
            if (m_dateIntervals == null) {
                m_dateIntervals = Arrays.asList(null, null);
                ToolScheduleTerm trm = getScheduleTerm(broker);
                if (trm == null) {
                    ToolSection section = getSection(broker);
                    if (section != null) {
                        trm = section.getScheduleTerm(broker);
                    }
                }
                PlainDate startDate =
                        getStartDate() == null ? (trm == null ? null : trm.getStartDate(broker)) : getStartDate();
                PlainDate endDate = getEndDate() == null ? (trm == null ? null : trm.getEndDate(broker)) : getEndDate();
                if (startDate == null || endDate == null || !startDate.after(endDate)) {
                    m_dateIntervals.set(0, endDate != null || startDate != null ? Range.of(startDate, endDate) : null);
                }

                startDate = getStartDate2();
                endDate = getEndDate2();
                if (startDate == null || endDate == null || !startDate.after(endDate)) {
                    m_dateIntervals.set(1, endDate != null || startDate != null ? Range.of(startDate, endDate) : null);
                }
            }
            return m_dateIntervals;
        }

        /**
         * Gets the end date.
         *
         * @return Plain date
         */
        public PlainDate getEndDate() {
            return getValueDate(FIELD_END_DATE);
        }

        /**
         * Gets the end date 2.
         *
         * @return Plain date
         */
        public PlainDate getEndDate2() {
            return getValueDate(FIELD_END_DATE_2);
        }

        /**
         * Gets the grade level.
         *
         * @return String
         */
        public String getGradeLevel() {
            return this.getValueReferenceState(FIELD_GRADE_LEVEL);
        }

        /**
         * Gets the position.
         *
         * @return String
         */
        public String getPosition() {
            return getValueReferenceState(FIELD_POSITION);
        }

        /**
         * Gets the position 2.
         *
         * @return String
         */
        public String getPosition2() {
            return getValueReferenceState(FIELD_POSITION_2);
        }

        /**
         * Gets the report specialty subject.
         *
         * @return boolean
         */
        public boolean getReportSpecialtySubject() {
            return getValueLogical(FIELD_REPORT_SPECIALTY_SUBJECT);
        }

        /**
         * Gets the report specialty subject 2.
         *
         * @return boolean
         */
        public boolean getReportSpecialtySubject2() {
            return getValueLogical(FIELD_REPORT_SPECIALTY_SUBJECT_2);
        }

        /**
         * Gets the role 2.
         *
         * @return String
         */
        public String getRole2() {
            return getValueString(FIELD_ROLE_2);
        }

        /**
         * Gets the schedule position code.
         *
         * @param broker X2Broker
         * @param dateRange Range<Date>
         * @return String
         */
        public String getSchedulePositionCode(X2Broker broker, Range<Date> dateRange) {
            String value = null;
            if (getStartDate() == null) {
                value = getPosition();
            } else {
                List<Range<Date>> intervals = getDateIntervals(broker);
                if (intervals.get(0) != null && intervals.get(0).isOverlap(dateRange)) {
                    value = getPosition();
                } else if (intervals.get(1) != null && intervals.get(1).isOverlap(dateRange)) {
                    value = getPosition2();
                }
            }
            return value;
        }

        /**
         * Gets the start date.
         *
         * @return Plain date
         */
        public PlainDate getStartDate() {
            return getValueDate(FIELD_START_DATE);
        }

        /**
         * Gets the start date 2.
         *
         * @return Plain date
         */
        public PlainDate getStartDate2() {
            return getValueDate(FIELD_START_DATE_2);
        }

        /**
         * Checks if is core schedule.
         * when test date is null test means teacher is core on at lease one day
         *
         * @param broker the broker
         * @param testDate PlainDate
         * @param mode the mode
         * @return true, if is core schedule
         */
        public boolean isCoreSchedule(X2Broker broker, PlainDate testDate, CoreScheduledMode mode) {
            if (m_isCoreScheduleMap == null) {
                m_isCoreScheduleMap = new HashMap();
            }
            Boolean value = m_isCoreScheduleMap.get(testDate);
            if (value == null) {
                value = isCoreScheduleRaw(broker, testDate, mode) ? Boolean.TRUE : Boolean.FALSE;
                m_isCoreScheduleMap.put(testDate, value);
            }
            return value.booleanValue();
        }

        /**
         * Checks if is core schedule raw.
         *
         * @param broker the broker
         * @param testDate PlainDate
         * @param mode the mode
         * @return true, if is core schedule raw
         */
        private boolean isCoreScheduleRaw(X2Broker broker, PlainDate testDate, CoreScheduledMode mode) {
            PlainDate span1Start = getStartDate();
            PlainDate span1End = getEndDate();
            PlainDate span2Start = getStartDate2();
            PlainDate span2End = getEndDate2();


            String translatedRole = getDictionaryExtractor().getStateValue(this, ToolScheduleTeacher.FIELD_ROLE);
            if (StringUtils.isBlank(translatedRole)) {
                translatedRole = getRole();
            }

            /*
             * If (there are no span dates), return core mtcPrimaryTchr
             */
            if (span1Start == null && span1End == null && span2Start == null && span2End == null
                    && StringUtils.isEmpty(translatedRole)) {
                return getPrimaryTeacherIndicator();
            }

            /*
             * If (span 1 overlaps submission AND ([all-mtc-Role] ==
             * "Primary"),
             * return true
             */
            if (span1Start != null || span1End != null || !StringUtils.isEmpty(translatedRole)) {
                boolean span1OverlapsCountDate = false;
                Range<Date> interval = Range.of(span1Start, span1End);
                switch (mode) {
                    case SCHEDULED_ON_DATE:
                        span1OverlapsCountDate = interval.contains(testDate);
                        break;
                    case SCHEDULED_BEFORE_DATE:
                        Range<Date> mstRange = getSection(broker).getSectionDateRange(broker);
                        span1OverlapsCountDate = interval.isOverlap(mstRange)
                                && (mstRange.getStart() == null || !mstRange.getStart().after(testDate))
                                && (interval.getStart() == null || !interval.getStart().after(testDate));
                        break;
                    default:
                        break;
                }
                boolean isCoreSpan1 = false;
                if (span1OverlapsCountDate) {
                    isCoreSpan1 = MTC_TEACHING_ROLE_PRIMARY.equals(translatedRole)
                            || MTC_TEACHING_ROLE_CO_TEACH.equals(translatedRole);
                }
                if (isCoreSpan1) {
                    return true;
                }
            }

            /*
             * If (span 2 overlaps submission AND ([all-mtc-Role2] ==
             * "Primary"),
             * return true
             */
            if (span2Start != null) {
                boolean span2OverlapsCountDate = false;
                Range<Date> interval = Range.of(span2Start, span2End);
                switch (mode) {
                    case SCHEDULED_ON_DATE:
                        span2OverlapsCountDate = interval.contains(testDate);
                        break;
                    case SCHEDULED_BEFORE_DATE:
                        Range<Date> mstRange = getSection(broker).getSectionDateRange(broker);
                        span2OverlapsCountDate = interval.isOverlap(mstRange)
                                && (mstRange.getStart() == null || !mstRange.getStart().after(testDate))
                                && (interval.getStart() == null || !interval.getStart().after(testDate));
                        break;
                    default:
                        break;
                }
                boolean isCoreSpan2 = false;
                if (span2OverlapsCountDate) {
                    String translatedRole2 =
                            getDictionaryExtractor().getStateValue(this, OnScheduleTeacher.FIELD_ROLE_2);
                    if (StringUtils.isBlank(translatedRole2)) {
                        translatedRole2 = getRole2();
                    }
                    isCoreSpan2 = MTC_TEACHING_ROLE_PRIMARY.equals(translatedRole2)
                            || MTC_TEACHING_ROLE_CO_TEACH.equals(translatedRole2);
                }
                if (isCoreSpan2) {
                    return true;
                }
            }
            return false;
        }

    }

    /**
     * The Class OnsisSchool.
     */
    public static class OnSchool extends ToolSchool {
        public static final List<String> SPECIAL_CONDITION_CON_ED = Arrays.asList("N");
        public static final List<String> SPECIAL_CONDITION_EXCLUDE_FROM_TRANSCRIPT = Arrays.asList("J", "T");
        public static final List<String> SPECIAL_CONDITION_SUMMER_SCHOOL = Arrays.asList("8");

        private static final String ALIAS_RCD_BOARD_NUMBER = "rcd-bsid-board-number";
        private static final String ALIAS_SKL_BSID = "all-skl-BSID";
        private static final String ALIAS_SKL_DESIGNATED_BSID_ELEM = "all-skl-DesignatedElementaryBSID";
        private static final String ALIAS_SKL_DESIGNATED_BSID_SEC = "all-skl-DesignatedSecondaryBSID";
        private static final String ALIAS_SKL_ECPP_PROGRAM_TYPE = "all-skl-ECPPProgramType";
        private static final String ALIAS_SKL_EY_LEAD = "all-skl-EYLeadNameEmail";
        private static final String ALIAS_SKL_INCLUDE_IN_FTE = "all-skl-IncludeInFTECalcs";
        private static final String ALIAS_SKL_SPECIAL_CONDITION = "all-skl-SpecialCondition";

        public static final ToolBeanColumn FIELD_BSID = new ToolBeanColumn(SisBeanPaths.SCHOOL, ALIAS_SKL_BSID);
        public static final ToolBeanColumn FIELD_DESIGNATED_BSID_ELEM =
                new ToolBeanColumn(SisBeanPaths.SCHOOL, ALIAS_SKL_DESIGNATED_BSID_ELEM);
        public static final ToolBeanColumn FIELD_DESIGNATED_BSID_SEC =
                new ToolBeanColumn(SisBeanPaths.SCHOOL, ALIAS_SKL_DESIGNATED_BSID_SEC);
        public static final ToolBeanColumn FIELD_ECPP_PROGRAM_TYPE =
                new ToolBeanColumn(SisBeanPaths.SCHOOL, ALIAS_SKL_ECPP_PROGRAM_TYPE);
        public static final ToolBeanColumn FIELD_EY_LEAD =
                new ToolBeanColumn(SisBeanPaths.SCHOOL, ALIAS_SKL_EY_LEAD);
        public static final ToolBeanColumn FIELD_INCLUDE_IN_FTE =
                new ToolBeanColumn(SisBeanPaths.SCHOOL, ALIAS_SKL_INCLUDE_IN_FTE);
        public static final ToolBeanColumn FIELD_LANGUAGE_TYPE =
                new ToolBeanColumn(SisBeanPaths.SCHOOL, ALIAS_SKL_LANGUAGE_TYPE);
        public static final ToolBeanColumn FIELD_SCHOOL_LEVEL_CODE =
                new ToolBeanColumn(SisBeanPaths.SCHOOL.schoolLevelCode());
        public static final ToolBeanColumn FIELD_SCHOOL_TYPE_CODE =
                new ToolBeanColumn(SisBeanPaths.SCHOOL.schoolTypeCode());
        public static final ToolBeanColumn FIELD_SPECIAL_CONDITION =
                new ToolBeanColumn(SisBeanPaths.SCHOOL, ALIAS_SKL_SPECIAL_CONDITION);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolSchool.FULL_DEFINITION
                .expand(FIELD_BSID,
                        FIELD_DESIGNATED_BSID_ELEM,
                        FIELD_DESIGNATED_BSID_SEC,
                        FIELD_ECPP_PROGRAM_TYPE,
                        FIELD_EY_LEAD,
                        FIELD_INCLUDE_IN_FTE,
                        FIELD_LANGUAGE_TYPE,
                        FIELD_SCHOOL_LEVEL_CODE,
                        FIELD_SCHOOL_TYPE_CODE,
                        FIELD_SPECIAL_CONDITION);

        String m_boardBsid;
        String m_boardName;

        /**
         * Instantiates a new onsis school.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnSchool(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the board bsid.
         *
         * @return the board bsid
         */
        public String getBoardBsid() {
            if (m_boardBsid == null) {
                initBoardFields();
            }
            return m_boardBsid;
        }

        /**
         * Gets the board name.
         *
         * @return the board name
         */
        public String getBoardName() {
            if (m_boardName == null) {
                initBoardFields();
            }
            return m_boardName;
        }

        /**
         * Gets the bsid.
         *
         * @return String
         */
        public String getBsid() {
            return this.getValueString(FIELD_BSID);
        }

        /**
         * Gets the designated bsid for elemnetary.
         *
         * @return String
         */
        public String getDesignatedBsidElementary() {
            return this.getValueReferenceState(FIELD_DESIGNATED_BSID_ELEM);
        }

        /**
         * Gets the designated bsid for secondary.
         *
         * @return String
         */
        public String getDesignatedBsidSecondary() {
            return this.getValueReferenceState(FIELD_DESIGNATED_BSID_SEC);
        }

        /**
         * Gets the ecpp program type.
         *
         * @return the ecpp program type
         */
        public String getEcppProgramType() {
            return this.getValueReferenceState(FIELD_ECPP_PROGRAM_TYPE);
        }

        /**
         * Gets the EY lead email.
         *
         * @return the EY lead email
         */
        public String getEYLeadEmail() {
            String email = "";
            String value = getValueString(FIELD_EY_LEAD);
            if (value != null) {
                String lines[] = value.split("\\r?\\n");
                if (lines.length > 1) {
                    email = lines[1];
                }
            }
            return email;
        }

        /**
         * Gets the EY lead name.
         *
         * @return the EY lead name
         */
        public String getEYLeadName() {
            String name = "";
            String value = getValueString(FIELD_EY_LEAD);
            if (value != null) {
                String lines[] = value.split("\\r?\\n");
                if (lines.length > 0) {
                    name = lines[0];
                }
            }
            return name;
        }

        /**
         * Gets the checks if is include in FTE indicator.
         *
         * @return the checks if is include in FTE indicator
         */
        public boolean getIsIncludeInFTEIndicator() {
            return getValueLogical(FIELD_INCLUDE_IN_FTE);
        }

        /**
         * Gets the language type.
         *
         * @return String
         */
        public String getLanguageType() {
            return this.getValueReferenceState(FIELD_LANGUAGE_TYPE);
        }

        /**
         * Gets the school level code.
         *
         * @return String
         */
        public String getSchoolLevelCode() {
            return this.getValueString(FIELD_SCHOOL_LEVEL_CODE);
        }

        /**
         * Gets the school level code.
         *
         * @return String
         */
        public String getSchoolLevelCodeState() {
            return this.getValueReferenceState(FIELD_SCHOOL_LEVEL_CODE);
        }

        /**
         * Gets the special condition.
         *
         * @return String
         */
        public String getSpecialCondition() {
            return this.getValueReferenceState(FIELD_SPECIAL_CONDITION);
        }

        /**
         * Inits the board fields.
         */
        private void initBoardFields() {
            m_boardBsid = "";
            m_boardName = "";
            DictionaryExtractor extractor = ToolBean.getDictionaryExtractor();
            DataDictionaryField dictionaryField = FIELD_BSID.getField(extractor);
            String dictionaryId = extractor.getDictionaryId(dictionaryField);
            if (dictionaryId != null) {
                DataDictionaryField fieldBoardNumber =
                        extractor.getFieldByAlias(ALIAS_RCD_BOARD_NUMBER, dictionaryId, false);
                ReferenceCode code = extractor.getRefCodeByAlias(this, FIELD_BSID);
                ReferenceCode boardCode = extractor.getRefCodeByAlias(code, ALIAS_RCD_BOARD_NUMBER, dictionaryId);
                m_boardBsid = boardCode.getCode();
                m_boardName = boardCode.getDescription();
            }
        }

    }

    /**
     * The Class OnSchoolCalendar.
     */
    public static class OnSchoolCalendar extends ToolSchoolCalendar {
        private static final String ALIAS_CAS_SCHEDULE_MODE = "all-cas-ScheduleMode";
        private static final String ALIAS_CAS_SCHEDULE_MODE_2 = "all-cas-ScheduleMode2";

        public static final ToolBeanColumn FIELD_SCHEDULE_MODE =
                new ToolBeanColumn(SisBeanPaths.CALENDAR_SCHOOL,
                        new ToolBeanColumn.AliasDefinition(ALIAS_CAS_SCHEDULE_MODE, null, false));
        public static final ToolBeanColumn FIELD_SCHEDULE_MODE_2 =
                new ToolBeanColumn(SisBeanPaths.CALENDAR_SCHOOL,
                        new ToolBeanColumn.AliasDefinition(ALIAS_CAS_SCHEDULE_MODE_2, null, false));

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolSchoolCalendar.FULL_DEFINITION
                .expand(FIELD_SCHEDULE_MODE,
                        FIELD_SCHEDULE_MODE_2);

        /**
         * Instantiates a new on school calendar.
         *
         * @param columns the columns
         * @param data the data
         */
        public OnSchoolCalendar(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the schedule mode.
         *
         * @return the schedule mode
         */
        public String getScheduleMode() {
            return this.getValueString(FIELD_SCHEDULE_MODE);
        }

        /**
         * Gets the schedule mode 2.
         *
         * @return the schedule mode 2
         */
        public String getScheduleMode2() {
            return this.getValueString(FIELD_SCHEDULE_MODE_2);
        }
    }

    /**
     * The Class OnSchoolIncident.
     */
    public static class OnSchoolIncident extends ToolBean {
        private static final String ALIAS_UDE_INCIDENT_DATE = "inc-ude-incident-date";
        private static final String ALIAS_UDE_INCIDENT_ID = "inc-ude-incident-id";
        private static final String ALIAS_UDE_INCIDENT_LOCATION = "inc-ude-incident-location";
        private static final String ALIAS_UDE_INCIDENT_TIME = "inc-ude-incident-time";
        private static final String ALIAS_UDE_POLICE = "inc-ude-police";
        private static final String ALIAS_UDE_SCHOOL_OID = "inc-ude-school-oid";
        private static final String DDX_ID = "ON-INCIDENT";

        public static final ToolBeanColumn FIELD_DDX_OID =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_E.extendedDataDictionaryOid());
        public static final ToolBeanColumn FIELD_INCIDENT_DATE =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_E, ALIAS_UDE_INCIDENT_DATE, DDX_ID);
        public static final ToolBeanColumn FIELD_INCIDENT_ID =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_E, ALIAS_UDE_INCIDENT_ID, DDX_ID);
        public static final ToolBeanColumn FIELD_INCIDENT_LOCATION =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_E, ALIAS_UDE_INCIDENT_LOCATION, DDX_ID);
        public static final ToolBeanColumn FIELD_INCIDENT_TIME =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_E, ALIAS_UDE_INCIDENT_TIME, DDX_ID);
        public static final ToolBeanColumn FIELD_POLICE =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_E, ALIAS_UDE_POLICE, DDX_ID);
        public static final ToolBeanColumn FIELD_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_E, ALIAS_UDE_SCHOOL_OID, DDX_ID);

        public static ToolBeanRelationship CHILD_CONDUCT_INCIDENTS =
                new ToolBeanRelationship(
                        SisBeanPaths.USER_DEFINED_TABLE_E.conductIncidents().getBeanType(),
                        SisBeanPaths.USER_DEFINED_TABLE_E.conductIncidents().getValueType(),
                        SisBeanPaths.USER_DEFINED_TABLE_E.conductIncidents().getPath(),
                        SisBeanPaths.STUDENT_CONDUCT_INCIDENT.userDefinedTableEOid().getPath(),
                        SisBeanPaths.USER_DEFINED_TABLE_E.conductIncidents().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_DDX_OID,
                        FIELD_INCIDENT_DATE,
                        FIELD_INCIDENT_ID,
                        FIELD_INCIDENT_LOCATION,
                        FIELD_INCIDENT_TIME,
                        FIELD_POLICE,
                        FIELD_SCHOOL_OID)
                .expandRelationships(CHILD_CONDUCT_INCIDENTS)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {

                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        DictionaryExtractor extractor = ToolBean.getDictionaryExtractor();
                        DataDictionary dictionary = extractor.getDictionary(DDX_ID);
                        String dictionaryOid =
                                dictionary == null ? "__NO_MATCH__" : dictionary.getExtendedDictionaryOid();
                        criteria.addEqualTo(FIELD_DDX_OID.resolve(extractor), dictionaryOid);
                        return criteria;
                    }
                });

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.USER_DEFINED_TABLE_E.getBeanType();
        }

        /**
         * Instantiates a new onsis school incident.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnSchoolIncident(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the conduct incidents.
         *
         * @param broker the broker
         * @return the conduct incident
         */
        public List<ToolConductIncident> getConductIncidents(X2Broker broker) {
            return (List<ToolConductIncident>) getChildren(broker, CHILD_CONDUCT_INCIDENTS);
        }

        /**
         * Gets the incident date.
         *
         * @return Plain date
         */
        public PlainDate getIncidentDate() {
            return getValueDate(FIELD_INCIDENT_DATE);
        }

        /**
         * Gets the incident location state code.
         *
         * @return String
         */
        public String getIncidentLocationState() {
            return getValueReferenceState(FIELD_INCIDENT_LOCATION);
        }

        /**
         * Gets the incident time.
         *
         * @return Plain time
         */
        public PlainTime getIncidentTime() {
            return getValueTime(FIELD_INCIDENT_TIME);
        }
    }

    /**
     * The Class OnsisSection.
     */
    public static class OnSection extends ToolSection {
        public static final Map<String, String> ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP = new HashMap<String, String>();
        static {
            ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("A", "A");
            ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("B", "C");
            ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("C", "EC");
            ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("CEC", "CEC");
            ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("CED", "CED");
            ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("E", "E");
            ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("G", "G");
            ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("H", "F");
            ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("N", "SE");
            ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("P", "P");
            ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("PA", "JP");
            ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("L", "R");
            ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.put("S", "S");
        }

        /**
         * The Class ConedRegisterInfo.
         */
        public class ConedRegisterInfo {
            public static final String ATT_CODE_GENERAL_ABSENCE = "G";
            public static final String ATT_CODE_CON_ED_CANCELLED_FUNDED = "CEC";
            public static final String ATT_CODE_CON_ED_CANCELLED_FUNDED_RPT = "C";
            public static final String ATT_CODE_CON_ED_CANCELLED_UNFUNDED = "CED";
            public static final String ATT_CODE_CON_ED_CANCELLED_UNFUNDED_RPT = "D";


            private ArrayList<String> m_attList;
            private List<Range<Date>> m_dateRanges;
            private PlainDate m_firstDate;
            private Boolean m_isOctober;
            private Boolean m_isSummer;
            private ClassSessionsList m_sessions;
            private OnStudent m_student;
            private int m_totalDays = 0;
            private int m_totalMinutes = 0;
            private List<String> m_reasons;

            /**
             * Instantiates a new coned register info.
             *
             * @param student the student
             * @param dateRanges the date ranges
             */
            public ConedRegisterInfo(OnStudent student, List<Range<Date>> dateRanges) {
                m_student = student;
                m_dateRanges = dateRanges;
                init();
            }

            /**
             * Contains date.
             *
             * @param date the date
             * @return true, if successful
             */
            public boolean containsDate(PlainDate date) {
                if (m_dateRanges != null) {
                    return getDateRanges().stream().anyMatch(range -> range.contains(date));
                }
                return true;
            }

            /**
             * Gets the attendance list.
             *
             * @param isFrenchLanguage the is french language
             * @return the attendance list
             */
            public List<String> getAttendanceList(boolean isFrenchLanguage) {
                return isFrenchLanguage ? m_attList.stream()
                        .map(code -> isFrenchLanguage && ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.containsKey(code)
                                ? ATT_CODES_LOCALE_FRENCH_TO_ENGLISH_MAP.get(code)
                                : code)
                        .collect(Collectors.toList()) : m_attList;
            }

            /**
             * Gets the list of Reasons.
             *
             * @return the list of reasons
             */
            public List<String> getReasons() {
                return m_reasons;
            }

            /**
             * Gets the class sessions.
             *
             * @return the class sessions
             */
            public ClassSessionsList getClassSessions() {
                return m_sessions;
            }

            /**
             * Gets the date ranges.
             *
             * @return the m_dateRanges
             */
            public List<Range<Date>> getDateRanges() {
                return m_dateRanges;
            }

            /**
             * Gets the student.
             *
             * @return the m_student
             */
            public OnStudent getStudent() {
                return m_student;
            }

            /**
             * Ge total days.
             *
             * @return the int
             */
            public int getTotalDays() {
                return m_totalDays;
            }

            /**
             * Gets the total minutes.
             *
             * @return the total minutes
             */
            public int getTotalMinutes() {
                return m_totalMinutes;
            }

            /**
             * Checks if is summer.
             *
             * @return true, if is summer
             */
            public boolean isOctober() {
                if (m_isOctober == null) {
                    m_isOctober = Boolean.FALSE;
                    if (m_firstDate != null) {
                        int year = OnSection.this.getSchedule(ToolBean.getBroker(true))
                                .getDistrictContext(ToolBean.getBroker(true)).getSchoolYear() - 1;
                        PlainDate octoberStartDate = ToolBean.getPlainDateValue(year, Calendar.OCTOBER, 31);
                        m_isOctober = containsDate(octoberStartDate);
                    }
                }
                return m_isOctober.booleanValue();
            }

            /**
             * Checks if is summer.
             *
             * @return true, if is summer
             */
            public boolean isSummer() {
                if (m_isSummer == null) {
                    m_isSummer = Boolean.FALSE;
                    if (m_firstDate != null) {
                        int year = OnSection.this.getSchedule(ToolBean.getBroker(true))
                                .getDistrictContext(ToolBean.getBroker(true)).getSchoolYear();
                        PlainDate summerStartDate = ToolBean.getPlainDateValue(year, Calendar.JUNE, 25);
                        m_isSummer = Boolean.valueOf(!m_firstDate.before(summerStartDate));
                    }
                }
                return m_isSummer.booleanValue();
            }

            /**
             * Inits the.
             */
            private void init() {
                int overrideMinutes = getConEdDuration();
                if (overrideMinutes == 0) {
                    overrideMinutes = getOverrideMinutes();
                }
                int absentDays = 0;
                int absentMinutes = 0;

                m_sessions = OnSection.this.getClassSessions(getStudent().getCalendarCode(), ToolBean.getBroker(true));
                m_attList = new ArrayList<>(m_sessions.size());
                m_reasons = new ArrayList<>();
                StringBuilder reasonBuilder = new StringBuilder();
                for (ClassSession session : m_sessions) {
                    String dateValue = "";
                    if (containsDate(session.getPlainDate())) {
                        if (m_firstDate == null) {
                            m_firstDate = session.getPlainDate();
                        }
                        Optional<ToolStudentPeriodAttendance> pat =
                                getStudentPeriodAttendances(ToolBean.getBroker(true))
                                        .filter(new ToolBean.ToolBeanDefinition(
                                                ToolStudentPeriodAttendance.FIELD_STUDENT_OID,
                                                ToolStudentPeriodAttendance.FIELD_DATE),
                                                Arrays.asList(getStudent().getOid(), session.getPlainDate()))
                                        .extract().stream().findFirst();
                        if (pat.isPresent()) {
                            if (isCanceledUnfunded(pat.get())) {
                                dateValue = ATT_CODE_CON_ED_CANCELLED_UNFUNDED_RPT;
                                collectReason(reasonBuilder, pat.get());
                            } else if (isCancelledFunded(pat.get())) {
                                dateValue = ATT_CODE_CON_ED_CANCELLED_FUNDED_RPT;
                                collectReason(reasonBuilder, pat.get());
                            } else if (pat.get().getAbsentIndicator()) {
                                dateValue = "A";
                            }
                        }

                        List<String> absentPeriods =
                                isAbsent(pat) ? Arrays.asList(pat.get().getPeriodView().split("\\s*,\\s*"))
                                        : Collections.EMPTY_LIST;
                        boolean isAbsent = true;
                        for (ClassSessionPeriod period : session.getPeriods()) {
                            if (absentPeriods.contains(period.getPeriodId())) {
                                if (overrideMinutes == 0) {
                                    absentMinutes += period.getDuration();
                                }
                            } else {
                                isAbsent = false;
                                if (absentDays <= 2) {
                                    m_totalDays += absentDays;
                                    m_totalMinutes += absentMinutes;
                                }
                                absentMinutes = 0;
                                absentDays = 0;
                                if (overrideMinutes == 0) {
                                    m_totalMinutes += period.getDuration();
                                }
                            }
                        }
                        if (isAbsent) {
                            ++absentDays;
                            if (overrideMinutes > 0) {
                                absentMinutes += overrideMinutes;
                            }
                        } else {
                            m_totalDays++;
                            if (overrideMinutes > 0) {
                                m_totalMinutes += overrideMinutes;
                            }
                        }

                    } else {
                        dateValue = "-";
                    }
                    m_attList.add(dateValue);
                }
                m_reasons.add(reasonBuilder.toString());

                if (absentDays <= 2) {
                    m_totalDays += absentDays;
                    m_totalMinutes += absentMinutes;
                }
            }

            /**
             * Collects the reason.
             *
             * @param reasonBuilder The StringBuilder to which the formatted reason information will
             *        be appended.
             * @param pat The ToolStudentPeriodAttendance instance containing the reason code and
             *        other attendance details.
             */
            private void collectReason(StringBuilder reasonBuilder, ToolStudentPeriodAttendance pat) {
                String reasonCode = pat.getReasonCode();

                DictionaryExtractor extractor = ToolBean.getDictionaryExtractor();
                DataDictionaryField field = FIELD_REASON_CODE.getField(extractor);

                Optional<String> reasonDescription = extractor
                        .getReferenceCodes(field.getReferenceTableOid())
                        .entrySet()
                        .stream()
                        .filter(rc -> rc.getKey().equals(reasonCode))
                        .map(rc -> rc.getValue().getDescription())
                        .findFirst();

                LocalDate currentAttOfSessionDay = LocalDate.parse(String.valueOf(pat.getDate()));
                String formattedMonthName =
                        new DateFormatSymbols().getMonths()[currentAttOfSessionDay.getMonthValue() - 1];
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
            }

            /**
             * Checks if is cancelled funded.
             *
             * @param pat the pat
             * @return true, if is cancelled funded
             */
            private boolean isCancelledFunded(ToolStudentPeriodAttendance pat) {
                String patOtherCode = pat.getOtherCode();
                String patOtherCode02 = pat.getOtherCode02();
                return ((patOtherCode != null) && (patOtherCode.equals(ATT_CODE_CON_ED_CANCELLED_FUNDED))) ||
                        ((patOtherCode02 != null) && (patOtherCode02.equals(ATT_CODE_CON_ED_CANCELLED_FUNDED)));
            }

            /**
             * Checks if is canceled unfunded.
             *
             * @param pat the pat
             * @return true, if is canceled unfunded
             */
            private boolean isCanceledUnfunded(ToolStudentPeriodAttendance pat) {
                String patOtherCode = pat.getOtherCode();
                String patOtherCode02 = pat.getOtherCode02();
                return ((patOtherCode != null) && (patOtherCode.equals(ATT_CODE_CON_ED_CANCELLED_UNFUNDED))) ||
                        ((patOtherCode02 != null) && (patOtherCode02.equals(ATT_CODE_CON_ED_CANCELLED_UNFUNDED)));
            }

            /**
             * Checks if is absent.
             *
             * @param pat the pat
             * @return true, if is absent
             */
            private boolean isAbsent(Optional<ToolStudentPeriodAttendance> pat) {
                if (pat.isPresent() && (pat.get().getAbsentIndicator() || isCanceledUnfunded(pat.get()))) {
                    return true;
                }
                return false;
            }
        }

        public static final String CLASS_TYPE_EXTERNAL_EDUCATOR = "NE";

        public static final String CONED_CREDIT_DAY = "2";
        public static final String CONED_CREDIT_NIGHT = "1";
        public static final String CONED_DEVELOPMENTALLY_DISABLED = "7";
        public static final String CONED_IILE = "9";
        public static final String CONED_INDIGENOUS_LANGUAGE = "6";
        public static final String CONED_LITERACY = "5";
        public static final String CONED_SELF_STUDY = "3";
        public static final String CONED_SUMMER_CREDIT = "8";

        public static final String COURSE_DELIVERY_TYPE_CORRESPONDENCE = "16";
        public static final String COURSE_DELIVERY_TYPE_DC_COLLEGE_CRS = "12";
        public static final String COURSE_DELIVERY_TYPE_DC_COLLEGE_APPRENTICE_CRS = "13";
        public static final String COURSE_DELIVERY_TYPE_DC_ON_LINE = "21";
        public static final String COURSE_DELIVERY_TYPE_E_LEARNING_LMS = "18";
        public static final String COURSE_DELIVERY_TYPE_E_LEARNING_PROVINCE_LMS = "19";
        public static final String COURSE_DELIVERY_TYPE_INDEPENDENT_STUDY = "2";
        public static final String COURSE_DELIVERY_TYPE_SUMMER = "8";
        public static final List<String> COURSE_DELIVERY_TYPES_DC = Arrays.asList(COURSE_DELIVERY_TYPE_DC_COLLEGE_CRS,
                COURSE_DELIVERY_TYPE_DC_COLLEGE_APPRENTICE_CRS, COURSE_DELIVERY_TYPE_DC_ON_LINE);


        public static final String COURSE_CODE_TYPE_DCC = "DCC";
        public static final String COURSE_CODE_TYPE_LDC = "LDC";
        public static final String COURSE_CODE_TYPE_MDC = "MDC";
        public static final String COURSE_CODE_TYPE_HOMEROOM = "Homeroom";
        public static final String COURSE_CODE_TYPE_PLACEHOLDER = "Place holder";
        public static final String COURSE_CODE_TYPE_PLE = "PLE";
        public static final List<String> COURSE_CODE_TYPES_MDC_DCC_LDC =
                Arrays.asList(COURSE_CODE_TYPE_DCC, COURSE_CODE_TYPE_LDC, COURSE_CODE_TYPE_MDC);

        public static final String COURSE_OFFERING_TYPE_ADDITIONAL_PREP = "05";
        public static final String COURSE_OFFERING_TYPE_AFTER_SCHOOL = "03";
        public static final String COURSE_OFFERING_TYPE_CREDIT_ON_MATH = "04";
        public static final String COURSE_OFFERING_TYPE_DAY = "02";
        public static final String COURSE_OFFERING_TYPE_NIGHT = "01";

        public static final String TIME_OF_DAY_SUMMER = "2";

        public static final String VALUE_FRENCH = "F";

        private static final String ALIAS_CRS_CLASS_TYPE = "all-crs-ClassType";
        private static final String ALIAS_CRS_COURSE_DELIVERY_TYPE = "all-crs-CourseDeliveryType";

        private static final String ALIAS_CSK_CLASS_TYPE = "all-csk-ClassType";
        private static final String ALIAS_CSK_CONED_PROG_TYPE = "all-csk-ConEdProgramType";
        private static final String ALIAS_CSK_COURSE_DELIVERY_TYPE = "all-csk-CourseDeliveryType";
        private static final String ALIAS_CSK_INTERNATIONAL_LANGUAGE = "all-csk-IntlLanguage";
        private static final String ALIAS_CSK_LESSONS = "all-csk-Lessons";

        private static final String ALIAS_MST_CLASS_TYPE = "all-mst-ClassType";
        private static final String ALIAS_MST_CONED_DURATION = "all-mst-ContinuingEducationMinutes";
        private static final String ALIAS_MST_CONED_END_DATE = "all-mst-ContinuingEducationEndDate";
        private static final String ALIAS_MST_CONED_END_TIME = "all-mst-ContinuingEducationEndTime";
        private static final String ALIAS_MST_CONED_OFFERING_TYPE = "all-mst-ContinuingEducationOfferingType";
        private static final String ALIAS_MST_CONED_START_DATE = "all-mst-ContinuingEducationStartDate";
        private static final String ALIAS_MST_CONED_START_TIME = "all-mst-ContinuingEducationStartTime";
        private static final String ALIAS_MST_COURSE_CONTINUED = "all-mst-ContinuedSection";
        private static final String ALIAS_MST_LANGUAGE_OF_INSTRUCTION = "all-mst-LanguageOfInstruction";
        private static final String ALIAS_MST_LANGUAGE_PROGRAM = "all-mst-LanguageProgram";
        private static final String ALIAS_MST_TIME_OF_DAY = "all-mst-TimeOfDay";

        private static final String ALIAS_RCD_INSTITUTION_CODE = "rcd-crs-institution-code";

        private static final String DDX_ID_REF_CRS_CODES = "REF-CRS-CODES";

        // Query Fields
        public static final ToolBeanColumn FIELD_BUILDING_CODE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.primaryRoom().buildingCode());
        public static final ToolBeanColumn FIELD_CLASS_TYPE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER, ALIAS_MST_CLASS_TYPE);
        public static final ToolBeanColumn FIELD_CLASS_TYPE_CRS =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_CRS_CLASS_TYPE, null, false));
        public static final ToolBeanColumn FIELD_CLASS_TYPE_CSK =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_CSK_CLASS_TYPE, null, false));
        public static final ToolBeanColumn FIELD_COURSE_CONTINUED =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER, ALIAS_MST_COURSE_CONTINUED);
        public static final ToolBeanColumn FIELD_CONED_DURATION =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER, ALIAS_MST_CONED_DURATION);
        public static final ToolBeanColumn FIELD_CONED_END_DATE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER, ALIAS_MST_CONED_END_DATE);
        public static final ToolBeanColumn FIELD_CONED_END_TIME =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER, ALIAS_MST_CONED_END_TIME);
        public static final ToolBeanColumn FIELD_CONED_OFFERING_TYPE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER, ALIAS_MST_CONED_OFFERING_TYPE);
        public static final ToolBeanColumn FIELD_CONED_PROG_TYPE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse(), ALIAS_CSK_CONED_PROG_TYPE);
        public static final ToolBeanColumn FIELD_CONED_START_DATE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER, ALIAS_MST_CONED_START_DATE);
        public static final ToolBeanColumn FIELD_CONED_START_TIME =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER, ALIAS_MST_CONED_START_TIME);
        public static final ToolBeanColumn FIELD_COURSE_CODE_TYPE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course(), ALIAS_CRS_COURSE_CODE_TYPE);
        public static final ToolBeanColumn FIELD_COURSE_DELIVERY_TYPE_CRS =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_CRS_COURSE_DELIVERY_TYPE, null, false));
        public static final ToolBeanColumn FIELD_COURSE_DELIVERY_TYPE_CSK =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_CSK_COURSE_DELIVERY_TYPE, null, false));
        public static final ToolBeanColumn FIELD_COURSE_OFFERING_TYPE_CSK =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_CSK_COURSE_OFFERING_TYPE, null, false));
        public static final ToolBeanColumn FIELD_ELEMENTARY_SUBJECT_TYPE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course(),
                        ALIAS_CRS_ELEMENTARY_SUBJECT_TYPE);
        public static final ToolBeanColumn FIELD_GRADE_LEVEL =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course().gradeLevel());
        public static final ToolBeanColumn FIELD_INTERNATIONAL_LANGUAGE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse(), ALIAS_CSK_INTERNATIONAL_LANGUAGE);
        public static final ToolBeanColumn FIELD_LANGUAGE_OF_INSTRUCTION =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER, ALIAS_MST_LANGUAGE_OF_INSTRUCTION);
        public static final ToolBeanColumn FIELD_LANGUAGE_OF_INSTRUCTION_CRS =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_CRS_LANGUAGE_OF_INSTRUCTION, null, false));
        public static final ToolBeanColumn FIELD_LANGUAGE_OF_INSTRUCTION_CSK =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_CSK_LANGUAGE_OF_INSTRUCTION, null, false));
        public static final ToolBeanColumn FIELD_LANGUAGE_TYPE_SKL =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schedule().school(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_SKL_LANGUAGE_TYPE, null, false));
        public static final ToolBeanColumn FIELD_LANGUAGE_PROGRAM =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER, ALIAS_MST_LANGUAGE_PROGRAM);
        public static final ToolBeanColumn FIELD_LANGUAGE_PROGRAM_CRS =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course(), ALIAS_CRS_LANGUAGE_PROGRAM);
        public static final ToolBeanColumn FIELD_LANGUAGE_PROGRAM_CSK =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_CSK_LANGUAGE_PROGRAM, null, false));
        public static final ToolBeanColumn FIELD_MST_EXCLUDE_FROM_ONSIS =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER, ALIAS_MST_EXCLUDE_FROM_ONSIS);
        public static final ToolBeanColumn FIELD_NUM_ASSIGNMENTS_OVERRIDE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_CSK_LESSONS, null, false));
        public static final ToolBeanColumn FIELD_MINISTRY_COURSE_CODE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course(),
                        ALIAS_CRS_MINISTRY_COURSE_CODE);
        public static final ToolBeanColumn FIELD_OTHER_COURSE_INFO_TYPE_CRS =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_CRS_OTHER_COURSE_INFO, null, false));
        public static final ToolBeanColumn FIELD_OTHER_COURSE_INFO_TYPE_CSK =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_CSK_OTHER_COURSE_INFO, null, false));
        public static final ToolBeanColumn FIELD_ROOM_TYPE_CODE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.primaryRoom().roomTypeCode());
        public static final ToolBeanColumn FIELD_SCHOOL_LEVEL =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course().schoolLevel());
        public static final ToolBeanColumn FIELD_TIME_OF_DAY =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER, ALIAS_MST_TIME_OF_DAY);


        // Nonquery Fields
        public static final ToolBeanColumn FIELD_CRS_EXCLUDE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course(), ALIAS_CRS_EXCLUDE);
        public static final ToolBeanColumn FIELD_CSK_MASTER_TYPE_CLASS =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().masterType());

        public static final ToolBeanColumn FIELD_REASON_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PERIOD_ATTENDANCE.reasonCode());

        public static ToolBeanRelationship CHILD_ASSIGNMENTS =
                new ToolBeanRelationship(SisBeanPaths.SCHEDULE_MASTER.gradebookColumnDefinitions().getBeanType(),
                        SisBeanPaths.SCHEDULE_MASTER.gradebookColumnDefinitions().getValueType(),
                        SisBeanPaths.SCHEDULE_MASTER.gradebookColumnDefinitions().getPath(),
                        SisBeanPaths.GRADEBOOK_COLUMN_DEFINITION.masterScheduleOid().getPath(),
                        SisBeanPaths.SCHEDULE_MASTER.gradebookColumnDefinitions().getRelationshipType());

        public static ToolBeanRelationship CHILD_SCORES =
                new ToolBeanRelationship(
                        SisBeanPaths.SCHEDULE_MASTER.gradebookColumnDefinitions().gradebookScores().getBeanType(),
                        SisBeanPaths.SCHEDULE_MASTER.gradebookColumnDefinitions().gradebookScores().getValueType(),
                        SisBeanPaths.SCHEDULE_MASTER.gradebookColumnDefinitions().gradebookScores().getPath(),
                        SisBeanPaths.GRADEBOOK_SCORE.columnDefinition().masterScheduleOid().getPath(),
                        SisBeanPaths.SCHEDULE_MASTER.gradebookColumnDefinitions().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolSection.FULL_DEFINITION
                .expand(FIELD_BUILDING_CODE,
                        FIELD_CLASS_TYPE,
                        FIELD_CLASS_TYPE_CRS,
                        FIELD_CLASS_TYPE_CSK,
                        FIELD_COURSE_CONTINUED,
                        FIELD_CONED_DURATION,
                        FIELD_CONED_END_DATE,
                        FIELD_CONED_END_TIME,
                        FIELD_CONED_OFFERING_TYPE,
                        FIELD_CONED_PROG_TYPE,
                        FIELD_CONED_START_DATE,
                        FIELD_CONED_START_TIME,
                        FIELD_COURSE_CODE_TYPE,
                        FIELD_COURSE_DELIVERY_TYPE_CRS,
                        FIELD_COURSE_DELIVERY_TYPE_CSK,
                        FIELD_COURSE_OFFERING_TYPE_CSK,
                        FIELD_ELEMENTARY_SUBJECT_TYPE,
                        FIELD_GRADE_LEVEL,
                        FIELD_INTERNATIONAL_LANGUAGE,
                        FIELD_LANGUAGE_OF_INSTRUCTION,
                        FIELD_LANGUAGE_OF_INSTRUCTION_CRS,
                        FIELD_LANGUAGE_OF_INSTRUCTION_CSK,
                        FIELD_LANGUAGE_TYPE_SKL,
                        FIELD_LANGUAGE_PROGRAM,
                        FIELD_LANGUAGE_PROGRAM_CRS,
                        FIELD_LANGUAGE_PROGRAM_CSK,
                        FIELD_MINISTRY_COURSE_CODE,
                        FIELD_MST_EXCLUDE_FROM_ONSIS,
                        FIELD_NUM_ASSIGNMENTS_OVERRIDE,
                        FIELD_OTHER_COURSE_INFO_TYPE_CRS,
                        FIELD_OTHER_COURSE_INFO_TYPE_CSK,
                        FIELD_ROOM_TYPE_CODE,
                        FIELD_SCHOOL_LEVEL,
                        FIELD_TIME_OF_DAY)
                .expandRelationships(CHILD_ASSIGNMENTS,
                        CHILD_SCORES)
                .expandSort(FIELD_COURSE_CODE_TYPE, FIELD_OID)
                .expandJoinAdjusters(
                        new JoinAdjusterPattern(JoinType.LEFT_OUTER, SisBeanPaths.SCHOOL_ROOM.getDatabaseName()))
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addNotEqualTo(FIELD_CRS_EXCLUDE.resolve(getDictionaryExtractor()),
                                BooleanAsStringConverter.TRUE);
                        criteria.addNotEqualTo(FIELD_MST_EXCLUDE_FROM_ONSIS.resolve(getDictionaryExtractor()),
                                BooleanAsStringConverter.TRUE);
                        criteria.addEqualTo(FIELD_CSK_MASTER_TYPE_CLASS.resolve(getDictionaryExtractor()),
                                SchoolCourse.MASTER_TYPE_CLASS);
                        return criteria;
                    }
                });

        /**
         * Instantiates a new onsis section.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnSection(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        private Range<Date> m_dateConEdRange;
        Integer m_duration;
        PlainTime m_endTime;
        PlainTime m_startTime;

        /**
         * @see com.x2dev.procedures.statereporting.common.ToolBean.ToolSection#containsDate(com.x2dev.utils.types.PlainDate,
         *      com.follett.fsc.core.k12.business.X2Broker)
         */
        @Override
        public boolean containsDate(PlainDate date, X2Broker broker) {
            return getConEdDateRange().contains(date);
        }

        /**
         * Gets the ade.
         *
         * @param reportRange the report range
         * @param student OnsisStudent
         * @param courseStartDate PlainDate
         * @param courseEndDate PlainDate
         * @param debugOutput the debug output
         * @return String
         */
        public Double getAde(Range<Date> reportRange,
                             OnStudent student,
                             PlainDate courseStartDate,
                             PlainDate courseEndDate,
                             StringBuilder debugOutput) {
            return getAde(reportRange, student, Arrays.asList(Range.of(courseStartDate, courseEndDate)), debugOutput);
        }

        /**
         * Gets the ade.
         *
         * @param reportRange the report range
         * @param student OnsisStudent
         * @param courseDateRanges course dates
         * @param debugOutput the debug output
         * @return String
         */
        public Double getAde(Range<Date> reportRange,
                             OnStudent student,
                             List<Range<Date>> courseDateRanges,
                             StringBuilder debugOutput) {
            Double ade = 0.0;
            String conedProgType = getConedProgType();
            if (COURSE_DELIVERY_TYPES_DC.contains(getCourseDeliveryType())) {
                // S-60381 MADE0012 - ADE must be 0 for Dual Credit courses
                return (ade);
            } else if (isIndependentStudy()) {
                double credit = 0.0;
                try {
                    credit = getCredit().doubleValue();
                } catch (Exception e1) {
                    // use 0 value
                }
                Integer numAssignmentsOverride = getOverrideNumAssignments();
                int numAssignments = numAssignmentsOverride == null || numAssignmentsOverride.intValue() == 0
                        ? getAssignments(ToolBean.getBroker(true)).size()
                        : numAssignmentsOverride.intValue();
                long numScores = getScores(ToolBean.getBroker(true))
                        .getGroup(OnGradebookScore.FIELD_STUDENT_OID, student.getOid()).stream()
                        .filter(score -> score.getCompletedDate() != null &&
                                courseDateRanges.stream().anyMatch(range -> range.contains(score.getCompletedDate())))
                        .count();
                if (numAssignments > 0) {
                    ade = 0.1158 * credit * Double.valueOf(numScores) / Double.valueOf(numAssignments);
                }
                if (debugOutput != null) {
                    debugOutput.append("OnSection.getAde: " + ade + " - INDEPENDENT STUDY " + this + "\n");
                    debugOutput.append("numAssignmentsOverride:" + numAssignmentsOverride + "\n");
                    debugOutput.append(
                            "getAssignments size:" + getAssignments(ToolBean.getBroker(true)).size() + "\n");
                    getScores(ToolBean.getBroker(true))
                            .getGroup(OnGradebookScore.FIELD_STUDENT_OID, student.getOid()).stream()
                            .forEach(score -> {
                                debugOutput.append("Included: "
                                        + (score.getCompletedDate() != null && courseDateRanges.stream()
                                                .anyMatch(range -> range.contains(score.getCompletedDate())))
                                        + " Score: " + score + "\n");
                            });
                }
            } else {
                switch (conedProgType) {
                    case CONED_SELF_STUDY:
                        double credit = 0.0;
                        try {
                            credit = getCredit().doubleValue();
                        } catch (Exception e1) {
                            // use 0 value
                        }
                        Integer numAssignmentsOverride = getOverrideNumAssignments();
                        int numAssignments = numAssignmentsOverride == null || numAssignmentsOverride.intValue() == 0
                                ? getAssignments(ToolBean.getBroker(true)).size()
                                : numAssignmentsOverride.intValue();
                        long numScores = getScores(ToolBean.getBroker(true))
                                .getGroup(OnGradebookScore.FIELD_STUDENT_OID, student.getOid()).stream()
                                .filter(score -> score.getCompletedDate() != null)
                                .count();
                        if (numAssignments > 0) {
                            ade = 0.1158 * credit * Double.valueOf(numScores) / Double.valueOf(numAssignments);
                        }
                        if (debugOutput != null) {
                            debugOutput.append("OnSection.getAde: " + ade + " - CONED_SELF_STUDY " + this + "\n");
                            debugOutput.append("Date ranges: " + courseDateRanges + "\n");
                            debugOutput.append("numAssignmentsOverride:" + numAssignmentsOverride + "\n");
                            debugOutput.append(
                                    "getAssignments size:" + getAssignments(ToolBean.getBroker(true)).size() + "\n");
                            getScores(ToolBean.getBroker(true))
                                    .getGroup(OnGradebookScore.FIELD_STUDENT_OID, student.getOid()).stream()
                                    .forEach(score -> {
                                        debugOutput.append("Included: " + (score.getCompletedDate() != null)
                                                + " Score: " + score + "\n");
                                    });
                        }
                        break;
                    case CONED_CREDIT_DAY:
                    case CONED_CREDIT_NIGHT:
                    case CONED_INDIGENOUS_LANGUAGE:
                    case CONED_LITERACY:
                    case CONED_DEVELOPMENTALLY_DISABLED:
                    case CONED_SUMMER_CREDIT:
                        List<ClassSession> sessions =
                                getClassSessions(student.getCalendarCode(), ToolBean.getBroker(true));
                        if (debugOutput != null) {
                            debugOutput.append(
                                    "Calendar code for " + getCourseView() + ": " + student.getCalendarCode() + "\n");
                            debugOutput.append("Course date ranges : [" + courseDateRanges.toString() + "]" + "\n");
                            debugOutput.append("Term date range : ["
                                    + getTermStartDate(ToolBean.getBroker(true), reportRange) + ", "
                                    + getTermEndDate(ToolBean.getBroker(true), reportRange) + "]" + "\n");
                            PlainDate courseStartDate = (PlainDate) courseDateRanges.get(0).getStart();
                            PlainDate courseEndDate =
                                    (PlainDate) courseDateRanges.get(courseDateRanges.size() - 1).getEnd();
                            debugOutput.append("Dates with periods for " + getCourseView() + ": "
                                    + getCalendarDates(student.getCalendarCode(), courseStartDate, courseEndDate,
                                            ToolBean.getBroker(true))
                                                    .stream()
                                                    .filter(csd -> csd.getInSessionIndicator())
                                                    .map(csd -> {
                                                        StringBuilder out = new StringBuilder();
                                                        out.append(csd.getDate().toString());
                                                        Set<ToolSchedulePeriod> periods =
                                                                getSectionPeriods(ToolBean.getBroker(true), csd);
                                                        if (periods != null && !periods.isEmpty()) {
                                                            out.append(periods.stream().map(ToolSchedulePeriod::getId)
                                                                    .collect(Collectors.toList()).toString());
                                                        }
                                                        return out.toString();
                                                    })
                                                    .collect(Collectors.toList())
                                    + "\n");
                            debugOutput.append("Sessions for " + getCourseView() + ": "
                                    + sessions.stream().map(session -> session.getPlainDate())
                                            .collect(Collectors.toList())
                                    + "\n");
                            debugOutput.append("Override minutes: " + getOverrideMinutes() + "\n");
                        }
                        ConedRegisterInfo info = getConedRegisterInfo(student, courseDateRanges);
                        ade = Double.valueOf(info.getTotalMinutes()) / 60.0 / 950.0;
                        break;
                    default:
                        break;
                }
            }
            return ade;
        }

        /**
         * Gets the assignments.
         *
         * @param broker X2Broker
         * @return Filterable
         */
        public List<OnGradebookColumnDefinition> getAssignments(X2Broker broker) {
            return (List<OnGradebookColumnDefinition>) getChildren(broker, CHILD_ASSIGNMENTS);

        }

        /**
         * Calculate Average Daily Minutes from the bell schedule period duration.
         *
         * @param broker the broker
         * @param dateOfInterest PlainDate
         * @param bellScheduleOids Set<String>
         * @return double
         */
        public double getAverageDailyCycleMinutes(X2Broker broker,
                                                  PlainDate dateOfInterest,
                                                  Set<String> bellScheduleOids) {
            return getAverageDailyCycleMinutes(broker, dateOfInterest, bellScheduleOids, null, null);
        }

        /**
         * Calculate Average Daily Minutes from the bell schedule period duration.
         *
         * @param broker the broker
         * @param dateOfInterest PlainDate
         * @param bellScheduleOids Set<String>
         * @param calendar the calendar
         * @param debugOutput the debug output
         * @return double
         */
        public double getAverageDailyCycleMinutes(X2Broker broker,
                                                  PlainDate dateOfInterest,
                                                  Set<String> bellScheduleOids,
                                                  ToolSchoolCalendar calendar,
                                                  StringBuilder debugOutput) {
            ToolScheduleTermDate scheduleTermDate = getScheduleTerms(broker).stream()
                    .flatMap(trm -> trm.getScheduleTermDates(broker).stream())
                    .filter(tmd -> tmd.getRange().contains(dateOfInterest))
                    .min(Comparator.nullsLast(Comparator.comparing(ToolScheduleTermDate::getStartDate)))
                    .orElse(null);
            if (scheduleTermDate == null) {
                if (debugOutput != null) {
                    debugOutput.append("No schedule term date found\n");
                }
                return 0.0d;
            }

            return getAverageDailyCycleMinutes(broker, scheduleTermDate, dateOfInterest, bellScheduleOids, calendar,
                    debugOutput);
        }

        /**
         * Calculate Average Daily Minutes from the bell schedule period duration.
         *
         * @param broker the broker
         * @param tmd ScheduleTermDate
         * @param dateOfInterest PlainDate
         * @param bellScheduleOids Set<String>
         * @return double
         */
        public double getAverageDailyCycleMinutes(X2Broker broker,
                                                  ToolScheduleTermDate tmd,
                                                  PlainDate dateOfInterest,
                                                  Set<String> bellScheduleOids) {
            return getAverageDailyCycleMinutes(broker, tmd, dateOfInterest, bellScheduleOids, null, null);
        }

        /**
         * Calculate Average Daily Minutes from the bell schedule period duration.
         *
         * @param broker the broker
         * @param tmd ScheduleTermDate
         * @param date PlainDate
         * @param bellScheduleOids Set<String>
         * @param calendar the calendar
         * @param debugOutput the debug output
         * @return double
         */
        public double getAverageDailyCycleMinutes(X2Broker broker,
                                                  ToolScheduleTermDate tmd,
                                                  PlainDate date,
                                                  Set<String> bellScheduleOids,
                                                  ToolSchoolCalendar calendar,
                                                  StringBuilder debugOutput) {
            double totalMinutes = 0;
            int totalDays = 1;
            OnSchedule schedule = (OnSchedule) getSchedule(broker);
            if (debugOutput != null) {
                debugOutput.append("Schedule term: " + tmd + "\n");
            }

            List<Pair<ToolScheduleDay, ToolSchoolCalendarDate>> pairs = schedule.getScheduleDays(broker).stream()
                    .map(day -> {
                        ToolSchoolCalendarDate csdToUse =
                                calendar == null || bellScheduleOids.isEmpty() || bellScheduleOids.size() == 1 ? null
                                        : calendar.getCalendarDates(broker).stream()
                                                .filter(ToolSchoolCalendarDate::getInSessionIndicator)
                                                .filter(csd -> day.getNumber() == csd.getScheduleDayNumber())
                                                .filter(csd -> bellScheduleOids == null || bellScheduleOids.isEmpty()
                                                        || bellScheduleOids.contains(csd.getBellScheduleOid()))
                                                .filter(csd -> tmd == null || tmd.getRange().contains(csd.getDate()))
                                                .sorted(new Comparator<ToolSchoolCalendarDate>() {
                                                    @Override
                                                    public int compare(ToolSchoolCalendarDate csd1,
                                                                       ToolSchoolCalendarDate csd2) {
                                                        int days1 = date.before(csd1.getDate())
                                                                ? DateUtils.countWeekdays(date, csd1.getDate())
                                                                : DateUtils.countWeekdays(csd1.getDate(), date);
                                                        int days2 = date.before(csd2.getDate())
                                                                ? DateUtils.countWeekdays(date, csd2.getDate())
                                                                : DateUtils.countWeekdays(csd2.getDate(), date);
                                                        return days1 - days2;
                                                    }
                                                })
                                                .findFirst().orElse(null);
                        return Pair.of(day, csdToUse);
                    })
                    .collect(Collectors.toList());


            for (Pair<ToolScheduleDay, ToolSchoolCalendarDate> pair : pairs) {
                if (debugOutput != null) {
                    debugOutput.append("Pair: " + pair + "\n");
                }
                ToolScheduleDay day = pair.getLeft();
                ToolSchoolCalendarDate csd = pair.getRight();
                Collection<ToolSchedulePeriod> periods = getSectionPeriods(broker, tmd, day);
                if (periods != null && !periods.isEmpty()) {
                    if (debugOutput != null) {

                        debugOutput.append("Candidate bell schedule periods: \n");
                        schedule.getScheduleBellPeriods(broker).stream()
                                .sorted(Comparator.comparing(ToolScheduleBellPeriod::getId))
                                .forEach(belPeriod -> debugOutput.append(belPeriod.toString() + "\n"));
                    }
                    for (ToolSchedulePeriod period : periods) {
                        OnScheduleBellPeriod bpe = schedule.getScheduleBellPeriod(broker, period.getId(), date,
                                csd == null ? bellScheduleOids : Arrays.asList(csd.getBellScheduleOid()), calendar);
                        if (debugOutput != null) {
                            debugOutput.append("Candidate schedule period: " + period + "\n");
                            debugOutput.append("Bell period: " + bpe + "\n");
                        }
                        if (bpe != null) {
                            if (debugOutput != null) {
                                debugOutput.append("Start time is " + bpe.getStartTime() + ", " +
                                        "End time is " + bpe.getEndTime() + ", " +
                                        "Days is " + bpe.getBellSchedule(broker).getDays() + "\n");
                            }
                            totalMinutes += (int) (bpe.getEndTime().getTimeInMinutes()
                                    - bpe.getStartTime().getTimeInMinutes());

                            ToolScheduleBell bellSchedule = bpe.getBellSchedule(broker);
                            totalDays = bellSchedule.getDays();
                        }
                    }
                }
            }
            return Double.valueOf(totalMinutes / totalDays);
        }

        /**
         * Gets the building code.
         *
         * @return the building code
         */
        public String getBuildingCode() {
            return getValueString(FIELD_BUILDING_CODE);
        }

        /**
         * Gets the building code description.
         *
         * @return the building code description
         */
        public String getBuildingCodeDescription() {
            String key = FIELD_BUILDING_CODE.resolve(null);
            String value = (String) (StringUtils.isEmpty(key) ? null : getFieldValueByColumnName(key));
            if (!StringUtils.isEmpty(value)) {
                DictionaryExtractor extractor = ToolBean.getDictionaryExtractor();
                DataDictionaryField field = FIELD_BUILDING_CODE.getField(extractor);
                if (field != null && field.hasReferenceTable()) {
                    Map<String, ReferenceCode> refCodes = extractor.getReferenceCodes(field.getReferenceTableOid());
                    ReferenceCode code = refCodes.get(value);
                    if (code != null) {
                        value = code.getDescription();
                    }
                }
            }
            return value;
        }

        /**
         * Gets the class code.
         *
         * @param broker X2Broker
         * @return String
         */
        public String getClassCode(X2Broker broker) {
            ToolScheduleClass scheduleClass = getSectionClass(broker);
            String classCode = null;
            if (scheduleClass != null) {
                classCode = scheduleClass.getId();
            } else {
                classCode = getCourseView();
            }
            return classCode;
        }

        /**
         * Gets the class type.
         *
         * @return String
         */
        public String getClassType() {
            String classType = getValueReferenceState(FIELD_CLASS_TYPE);
            if (StringUtils.isEmpty(classType)) {
                classType = getValueReferenceState(FIELD_CLASS_TYPE_CSK);
            }
            if (StringUtils.isEmpty(classType)) {
                classType = getValueReferenceState(FIELD_CLASS_TYPE_CRS);
            }
            return classType;
        }

        /**
         * Gets the ConEd date range.
         *
         * @return Range
         */
        public Range<Date> getConEdDateRange() {
            if (m_dateConEdRange == null) {
                m_dateConEdRange = Range.of(getConEdStartDate(), getConEdEndDate());
            }
            return m_dateConEdRange;
        }

        /**
         * Gets the con ed duration.
         *
         * @return the con ed duration
         */
        public int getConEdDuration() {
            return getValueInt(FIELD_CONED_DURATION);
        }

        /**
         * Gets the con ed end date.
         *
         * @return Plain date
         */
        public PlainDate getConEdEndDate() {
            return getValueDate(FIELD_CONED_END_DATE);
        }

        /**
         * Gets the con ed end time.
         *
         * @return Plain date
         */
        public PlainTime getConEdEndTime() {
            return getValueTime(FIELD_CONED_END_TIME);
        }

        /**
         * Gets the coned prog type.
         *
         * @return String
         */
        public String getConedProgType() {
            return getValueString(FIELD_CONED_PROG_TYPE);
        }

        /**
         * Gets the coned register info.
         *
         * @param student the student
         * @param range the range
         * @return the coned register info
         */
        public ConedRegisterInfo getConedRegisterInfo(OnStudent student, List<Range<Date>> range) {
            return new ConedRegisterInfo(student, range);
        }

        /**
         * Gets the con ed start date.
         *
         * @return Plain date
         */
        public PlainDate getConEdStartDate() {
            return getValueDate(FIELD_CONED_START_DATE);
        }

        /**
         * Gets the con ed start date.
         *
         * @return Plain date
         */
        public PlainTime getConEdStartTime() {
            return getValueTime(FIELD_CONED_START_TIME);
        }

        /**
         * Gets the course code type.
         *
         * @return String
         */
        public String getCourseCodeType() {
            return getValueString(FIELD_COURSE_CODE_TYPE);
        }

        /**
         * Gets the state reference course code type.
         *
         * @return String
         */
        public String getCourseCodeTypeState() {
            return this.getValueReferenceState(FIELD_COURSE_CODE_TYPE);
        }

        /**
         * Gets the course continued indicator.
         *
         * @return boolean
         */
        public boolean getCourseContinuedIndicator() {
            return getValueLogical(FIELD_COURSE_CONTINUED);
        }

        /**
         * Gets the course delivery type.
         *
         * @return String
         */
        public String getCourseDeliveryType() {
            String value = this.getValueReferenceState(FIELD_COURSE_DELIVERY_TYPE_CSK);
            if (StringUtils.isEmpty(value)) {
                value = this.getValueReferenceState(FIELD_COURSE_DELIVERY_TYPE_CRS);
            }
            return value;
        }

        /**
         * Gets the course offering type.
         *
         * @return String
         */
        public String getCourseOfferingType() {
            String value = getValueReferenceState(FIELD_CONED_OFFERING_TYPE);
            if (StringUtils.isEmpty(value)) {
                value = getValueReferenceState(FIELD_COURSE_OFFERING_TYPE_CSK);
            }
            return value;
        }

        /**
         * Gets the duration.
         *
         * @param calendarId the calendar id
         * @param broker the broker
         * @return the duration
         */
        public int getDuration(String calendarId, X2Broker broker) {
            if (m_duration == null) {
                m_duration = Integer.valueOf(getConEdDuration());
                if (m_duration.intValue() == 0) {
                    ClassSessionsList sessionsList = getClassSessions(calendarId, broker);
                    m_duration = sessionsList.getDuration();
                }
            }
            return m_duration.intValue();
        }


        /**
         * Gets the elementary subject type.
         *
         * @return String
         */
        public String getElementarySubjectType() {
            return getValueReferenceState(FIELD_ELEMENTARY_SUBJECT_TYPE);
        }

        /**
         * Gets the elementary subject type.
         *
         * @return String
         */
        public String getElementarySubjectTypeRaw() {
            return getValueString(FIELD_ELEMENTARY_SUBJECT_TYPE);
        }

        /**
         * Gets the end time.
         *
         * @param calendarId the calendar id
         * @param broker the broker
         * @return the end time
         */
        public PlainTime getEndTime(String calendarId, X2Broker broker) {
            if (m_endTime == null) {
                m_endTime = getConEdEndTime();
                if (m_endTime == null) {
                    ClassSessionsList sessionsList = getClassSessions(calendarId, broker);
                    m_endTime = sessionsList.getEndTime();
                }
            }
            return m_endTime;
        }

        /**
         * Gets the exclude from onsis indicator.
         *
         * @return boolean
         */
        public boolean getExcludeFromOnsisIndicator() {
            return getValueLogical(FIELD_MST_EXCLUDE_FROM_ONSIS);
        }

        /**
         * Gets the grade level.
         *
         * @return String
         */
        public String getGradeLevel() {
            return this.getValueReferenceState(FIELD_GRADE_LEVEL);
        }

        /**
         * Gets the institution type.
         *
         * @return String
         */
        public String getInstitutionType() {
            String value = null;
            if (COURSE_CODE_TYPE_DCC.equals(getCourseCodeType())) {
                DataDictionaryField institutionCodeField =
                        getDictionaryExtractor().getFieldByAlias(ALIAS_RCD_INSTITUTION_CODE, DDX_ID_REF_CRS_CODES,
                                false);
                if (institutionCodeField != null && !StringUtils.isEmpty(institutionCodeField.getJavaName())) {
                    ReferenceCode refCode = getMinistryDefinedCourseReferenceCode();
                    if (refCode != null) {
                        value = (String) refCode.getFieldValueByBeanPath(institutionCodeField.getJavaName());
                    }
                }
            }
            return value;
        }

        /**
         * Gets the international language.
         *
         * @return the international language
         */
        public String getInternationalLanguage() {
            return this.getValueString(FIELD_INTERNATIONAL_LANGUAGE);
        }

        /**
         * Find language type for section.
         *
         * @return String
         */
        public String getLanguageOfInstruction() {
            String languageType = getValueReferenceState(FIELD_LANGUAGE_OF_INSTRUCTION);
            if (StringUtils.isEmpty(languageType)) {
                languageType = getValueReferenceState(FIELD_LANGUAGE_OF_INSTRUCTION_CSK);
            }
            if (StringUtils.isEmpty(languageType)) {
                languageType = getValueReferenceState(FIELD_LANGUAGE_OF_INSTRUCTION_CRS);
            }
            if (StringUtils.isEmpty(languageType)) {
                languageType = getValueReferenceState(FIELD_LANGUAGE_TYPE_SKL);
            }
            return languageType;
        }

        /**
         * Gets the language program.
         *
         * @return String
         */
        public String getLanguageProgram() {
            String languageProgram = getValueReferenceState(FIELD_LANGUAGE_PROGRAM);
            if (StringUtils.isEmpty(languageProgram)) {
                languageProgram = getValueReferenceState(FIELD_LANGUAGE_PROGRAM_CSK);
            }
            if (StringUtils.isEmpty(languageProgram)) {
                languageProgram = getValueReferenceState(FIELD_LANGUAGE_PROGRAM_CRS);
            }
            return languageProgram;
        }

        /**
         * Gets the locally developed course.
         *
         * @return String
         */
        public String getLocallyDevelopedCourse() {
            String value = null;
            if (COURSE_CODE_TYPE_LDC.equals(getCourseCodeType())) {
                value = this.getValueReferenceState(FIELD_MINISTRY_COURSE_CODE);
            }
            return value;
        }

        /**
         * Gets the ministry course code.
         *
         * @return String
         */
        public String getMinistryDefinedCourse() {
            String value = null;
            if (COURSE_CODE_TYPE_MDC.equals(getCourseCodeTypeState())
                    || COURSE_CODE_TYPE_DCC.equals(getCourseCodeTypeState())) {
                value = this.getValueReferenceState(FIELD_MINISTRY_COURSE_CODE);
            }
            return value;
        }

        /**
         * Gets the ministry course code.
         *
         * @return String
         */
        public ReferenceCode getMinistryDefinedCourseReferenceCode() {
            ReferenceCode code = null;
            if (COURSE_CODE_TYPE_MDC.equals(getCourseCodeTypeState())
                    || COURSE_CODE_TYPE_DCC.equals(getCourseCodeTypeState())) {
                DictionaryExtractor extractor = ToolBean.getDictionaryExtractor();
                DataDictionaryField field = FIELD_MINISTRY_COURSE_CODE.getField(extractor);
                Map<String, ReferenceCode> refCodes = extractor.getReferenceCodes(field.getReferenceTableOid());
                code = refCodes.get(getValueString(FIELD_MINISTRY_COURSE_CODE));
            }
            return code;
        }

        /**
         * Gets the mst exclude from onsis indicator.
         *
         * @return boolean
         */
        public boolean getMstExcludeFromOnsisIndicator() {
            return this.getValueLogical(FIELD_CLASS_TYPE);
        }

        /**
         * Gets the num assignments.
         *
         * @param broker the broker
         * @return the num assignments
         */
        public int getNumAssignments(X2Broker broker) {
            int numAssignmentsOverride = getOverrideNumAssignments();
            int numAssignments = numAssignmentsOverride == 0
                    ? getAssignments(ToolBean.getBroker(true)).size()
                    : numAssignmentsOverride;
            return numAssignments;
        }

        /**
         * Gets the other course info types.
         *
         * @return List
         */
        public List<String> getOtherCourseInfoTypes() {
            List<String> values = getValuesReferenceState(FIELD_OTHER_COURSE_INFO_TYPE_CSK);
            if (values == null || values.isEmpty()) {
                values = getValuesReferenceState(FIELD_OTHER_COURSE_INFO_TYPE_CRS);
            }
            return values;
        }

        /**
         * Gets the override minutes.
         *
         * @return int
         */
        public int getOverrideMinutes() {
            PlainTime startTime = getConEdStartTime();
            PlainTime endTime = getConEdEndTime();
            return startTime == null || endTime == null ? 0
                    : (int) (endTime.getTimeInMinutes() - startTime.getTimeInMinutes());
        }

        /**
         * Gets the num assignments.
         *
         * @return Integer
         */
        public Integer getOverrideNumAssignments() {
            return this.getValueInt(FIELD_NUM_ASSIGNMENTS_OVERRIDE);
        }

        /**
         * Gets the room type code.
         *
         * @return String
         */
        public String getRoomTypeCode() {
            return this.getValueReferenceState(FIELD_ROOM_TYPE_CODE);
        }

        /**
         * Gets the assignments.
         *
         * @param broker X2Broker
         * @return Filterable
         */
        public Filterable<OnGradebookScore> getScores(X2Broker broker) {
            return (Filterable<OnGradebookScore>) getChildrenFilterable(broker, CHILD_SCORES);
        }

        /**
         * Gets the start time.
         *
         * @param calendarId the calendar id
         * @param broker the broker
         * @return the start time
         */
        public PlainTime getStartTime(String calendarId, X2Broker broker) {
            if (m_startTime == null) {
                m_startTime = getConEdStartTime();
                if (m_startTime == null) {
                    ClassSessionsList sessionsList = getClassSessions(calendarId, broker);
                    m_startTime = sessionsList.getStartTime();
                }
            }
            return m_startTime;
        }

        /**
         * Gets the term end date.
         *
         * @param broker X2Broker
         * @param dateRange Range<Date>
         * @return Plain date
         * @see com.x2dev.procedures.statereporting.common.ToolBean.ToolSection#getFirstTermDate(com.follett.fsc.core.k12.business.X2Broker,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range)
         */
        @Override
        public PlainDate getTermEndDate(X2Broker broker) {
            PlainDate endDate = super.getTermEndDate(broker);
            PlainDate conEdEndDate = getConEdEndDate();
            if (conEdEndDate != null && (endDate == null || conEdEndDate.before(endDate))) {
                endDate = conEdEndDate;
            }
            return endDate;
        }

        /**
         * Gets the term end date.
         *
         * @param broker X2Broker
         * @param dateRange Range<Date>
         * @return Plain date
         * @see com.x2dev.procedures.statereporting.common.ToolBean.ToolSection#getFirstTermDate(com.follett.fsc.core.k12.business.X2Broker,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range)
         */
        @Override
        public PlainDate getTermEndDate(X2Broker broker, Range<Date> dateRange) {
            PlainDate endDate = super.getTermEndDate(broker, dateRange);
            PlainDate conEdEndDate = getConEdEndDate();
            if (conEdEndDate != null && (endDate == null || conEdEndDate.before(endDate))) {
                endDate = conEdEndDate;
            }
            return endDate;
        }

        /**
         * Gets the term start date.
         *
         * @param broker the broker
         * @return the term start date
         */
        @Override
        public PlainDate getTermStartDate(X2Broker broker) {
            PlainDate startDate = super.getTermStartDate(broker);
            PlainDate conEdStartDate = getConEdStartDate();
            if (conEdStartDate != null && (startDate == null || conEdStartDate.after(startDate))) {
                startDate = conEdStartDate;
            }
            return startDate;
        }

        /**
         * Gets the term start date.
         *
         * @param broker X2Broker
         * @param dateRange Range<Date>
         * @return Plain date
         * @see com.x2dev.procedures.statereporting.common.ToolBean.ToolSection#getFirstTermDate(com.follett.fsc.core.k12.business.X2Broker,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range)
         */
        @Override
        public PlainDate getTermStartDate(X2Broker broker, Range<Date> dateRange) {
            PlainDate startDate = super.getTermStartDate(broker, dateRange);
            PlainDate conEdStartDate = getConEdStartDate();
            if (conEdStartDate != null && (startDate == null || conEdStartDate.after(startDate))) {
                startDate = conEdStartDate;
            }
            return startDate;
        }

        /**
         * Gets the time of day.
         *
         * @return the time of day
         */
        public String getTimeOfDay() {
            return this.getValueReferenceState(FIELD_TIME_OF_DAY);
        }

        /**
         * Checks if is correspondence.
         *
         * @return true, if is correspondence
         */
        public boolean isCorrespondence() {
            String courseDeliveryType = getCourseDeliveryType();
            return !getExcludeFromOnsisIndicator()
                    && COURSE_DELIVERY_TYPE_CORRESPONDENCE.equals(courseDeliveryType);
        }

        /**
         * Checks if is course offering additional prep.
         *
         * @return true, if is course offering additional prep
         */
        public boolean isCourseOfferingAdditionalPrep() {
            return StringUtils.isEmpty(getCourseOfferingType()) ? false
                    : COURSE_OFFERING_TYPE_ADDITIONAL_PREP.equals(getCourseOfferingType());
        }

        /**
         * Checks if is course offering after school.
         *
         * @return true, if is course offering after school
         */
        public boolean isCourseOfferingAfterSchool() {
            return StringUtils.isEmpty(getCourseOfferingType()) ? false
                    : COURSE_OFFERING_TYPE_AFTER_SCHOOL.equals(getCourseOfferingType());
        }

        /**
         * Checks if is course offering credit on math.
         *
         * @return true, if is course offering credit on math
         */
        public boolean isCourseOfferingCreditOnMath() {
            return StringUtils.isEmpty(getCourseOfferingType()) ? false
                    : COURSE_OFFERING_TYPE_CREDIT_ON_MATH.equals(getCourseOfferingType());
        }

        /**
         * Checks if is course offering day.
         *
         * @return true, if is course offering day
         */
        public boolean isCourseOfferingDay() {
            return StringUtils.isEmpty(getCourseOfferingType()) ? false
                    : COURSE_OFFERING_TYPE_DAY.equals(getCourseOfferingType());
        }

        /**
         * Checks if is course offering night.
         *
         * @return true, if is course offering night
         */
        public boolean isCourseOfferingNight() {
            return StringUtils.isEmpty(getCourseOfferingType()) ? false
                    : COURSE_OFFERING_TYPE_NIGHT.equals(getCourseOfferingType());
        }

        /**
         * Checks if is e learning.
         *
         * @return true, if is e learning
         */
        public boolean isELearning() {
            String courseDeliveryType = getCourseDeliveryType();
            return !getExcludeFromOnsisIndicator()
                    && (COURSE_DELIVERY_TYPE_E_LEARNING_LMS.equals(courseDeliveryType)
                            || COURSE_DELIVERY_TYPE_E_LEARNING_PROVINCE_LMS.equals(courseDeliveryType));
        }

        /**
         * Checks if is french.
         *
         * @return true, if is french
         */
        public boolean isFrench() {
            return VALUE_FRENCH.equals(getLanguageOfInstruction());

        }

        /**
         * Checks if is independent study.
         *
         * @return true, if is independent study
         */
        public boolean isIndependentStudy() {
            return !getExcludeFromOnsisIndicator()
                    && COURSE_DELIVERY_TYPE_INDEPENDENT_STUDY.equals(getCourseDeliveryType());
        }

        /**
         * Checks if is summer.
         *
         * @return true, if is summer
         */
        public boolean isSummer() {
            return TIME_OF_DAY_SUMMER.equals(getTimeOfDay());
        }
    }

    /**
     * The Class OnsisStaff.
     */
    public static class OnStaff extends ToolStaff {
        public static final ToolBeanColumn FIELD_GENDER_CODE =
                new ToolBeanColumn(SisBeanPaths.STAFF.person().genderCode());
        public static final ToolBeanColumn FIELD_GENDER_SPECIFY =
                new ToolBeanColumn(SisBeanPaths.STAFF.person(), ALIAS_PSN_GENDER_SPECIFY);
        public static final ToolBeanColumn FIELD_LEGAL_FIRST_NAME =
                new ToolBeanColumn(SisBeanPaths.STAFF.person(), ALIAS_PSN_LEGAL_FIRST_NAME);
        public static final ToolBeanColumn FIELD_LEGAL_LAST_NAME =
                new ToolBeanColumn(SisBeanPaths.STAFF.person(), ALIAS_PSN_LEGAL_LAST_NAME);
        public static final ToolBeanColumn FIELD_LEGAL_MIDDLE_NAME =
                new ToolBeanColumn(SisBeanPaths.STAFF.person(), ALIAS_PSN_LEGAL_MIDDLE_NAME);
        public static final ToolBeanColumn FIELD_MEN =
                new ToolBeanColumn(SisBeanPaths.STAFF, ALIAS_STF_MEN);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStaff.FULL_DEFINITION
                .expand(FIELD_GENDER_CODE,
                        FIELD_GENDER_SPECIFY,
                        FIELD_LEGAL_FIRST_NAME,
                        FIELD_LEGAL_LAST_NAME,
                        FIELD_LEGAL_MIDDLE_NAME,
                        FIELD_MEN);

        /**
         * Instantiates a new onsis staff.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnStaff(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the gender specify.
         *
         * @return String
         */
        public String getGenderSpecify() {
            return getValueString(FIELD_GENDER_SPECIFY);
        }

        /**
         * Gets the student legal first name.
         *
         * @return String
         */
        public String getLegalFirstName() {
            return getValueString(FIELD_LEGAL_FIRST_NAME);
        }

        /**
         * Gets the student legal last name.
         *
         * @return String
         */
        public String getLegalLastName() {
            return getValueString(FIELD_LEGAL_LAST_NAME);
        }

        /**
         * Gets the student legal middle name.
         *
         * @return String
         */
        public String getLegalMiddleName() {
            return getValueString(FIELD_LEGAL_MIDDLE_NAME);
        }

        /**
         * Gets the men.
         *
         * @return String
         */
        public String getMEN() {
            String men = getValueString(FIELD_MEN);
            if (!StringUtils.isEmpty(men)) {
                men = men.replaceAll(PATTERN_DASH, EMPTY_STRING);
            }
            return men;
        }
    }

    /**
     * The Class OnsisStaffPosition.
     */
    public static class OnStaffPosition extends ToolStaffPosition {
        public final List<String> VALID_TEACHING_TYPES = Arrays.asList("B", "T");

        private static final String ALIAS_SFP_DEPARTMENT = "all-sfp-DepartmentCode";
        private static final String ALIAS_SFP_INSTRUCTIONAL_TIME = "all-sfp-InstructionalTime";
        private static final String ALIAS_SFP_LEAVE_END_DATE = "all-sfp-LeaveEndDate";
        private static final String ALIAS_SFP_LEAVE_START_DATE = "all-sfp-LeaveStartDate";
        private static final String ALIAS_SFP_LEAVE_TYPE = "all-sfp-LeaveType";
        private static final String ALIAS_SFP_LETTER_OF_PERMISSION = "all-sfp-LetterPermission";
        private static final String ALIAS_SFP_NTIP_STATUS = "all-sfp-NTIPStatusType";
        private static final String ALIAS_SFP_TEACHING_TYPE = "all-sfp-TeachingType";
        private static final String ALIAS_SFP_TEMPORARY_LETTER_OF_APPROVAL = "all-sfp-TemporaryLetterApprovalID";
        private static final String ALIAS_SFP_WITHDRAWAL_TYPE = "all-sfp-WithdrawalType";

        private Format s_fteFormatter = new DecimalFormat("0.00");

        // Query Fields
        public static final ToolBeanColumn FIELD_DEPARTMENT =
                new ToolBeanColumn(SisBeanPaths.STAFF_POSITION, ALIAS_SFP_DEPARTMENT);
        public static final ToolBeanColumn FIELD_INSTRUCTIONAL_TIME =
                new ToolBeanColumn(SisBeanPaths.STAFF_POSITION, ALIAS_SFP_INSTRUCTIONAL_TIME);
        public static final ToolBeanColumn FIELD_LEAVE_END_DATE =
                new ToolBeanColumn(SisBeanPaths.STAFF_POSITION, ALIAS_SFP_LEAVE_END_DATE);
        public static final ToolBeanColumn FIELD_LEAVE_START_DATE =
                new ToolBeanColumn(SisBeanPaths.STAFF_POSITION, ALIAS_SFP_LEAVE_START_DATE);
        public static final ToolBeanColumn FIELD_LEAVE_TYPE =
                new ToolBeanColumn(SisBeanPaths.STAFF_POSITION, ALIAS_SFP_LEAVE_TYPE);
        public static final ToolBeanColumn FIELD_LETTER_OF_PERMISSION =
                new ToolBeanColumn(SisBeanPaths.STAFF_POSITION, ALIAS_SFP_LETTER_OF_PERMISSION);
        public static final ToolBeanColumn FIELD_NTIP_STATUS =
                new ToolBeanColumn(SisBeanPaths.STAFF_POSITION, ALIAS_SFP_NTIP_STATUS);
        public static final ToolBeanColumn FIELD_TEACHING_TYPE =
                new ToolBeanColumn(SisBeanPaths.STAFF_POSITION, ALIAS_SFP_TEACHING_TYPE);
        public static final ToolBeanColumn FIELD_TEMPORARY_LETTER_OF_APPROVAL =
                new ToolBeanColumn(SisBeanPaths.STAFF_POSITION, ALIAS_SFP_TEMPORARY_LETTER_OF_APPROVAL);
        public static final ToolBeanColumn FIELD_WITHDRAWAL_TYPE =
                new ToolBeanColumn(SisBeanPaths.STAFF_POSITION, ALIAS_SFP_WITHDRAWAL_TYPE);

        // Nonquery Fields
        public static final ToolBeanColumn FIELD_MEN =
                new ToolBeanColumn(SisBeanPaths.STAFF_POSITION.staff(), ALIAS_STF_MEN);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStaffPosition.FULL_DEFINITION
                .expand(FIELD_DEPARTMENT,
                        FIELD_INSTRUCTIONAL_TIME,
                        FIELD_LEAVE_END_DATE,
                        FIELD_LEAVE_START_DATE,
                        FIELD_LEAVE_TYPE,
                        FIELD_LETTER_OF_PERMISSION,
                        FIELD_NTIP_STATUS,
                        FIELD_TEACHING_TYPE,
                        FIELD_TEMPORARY_LETTER_OF_APPROVAL,
                        FIELD_WITHDRAWAL_TYPE);

        /**
         * Instantiates a new onsis staff position.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnStaffPosition(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the fte string.
         *
         * @return String
         */
        public String getFteString() {
            String fteString = "0.00";
            if (getFte() != null) {
                try {
                    fteString = s_fteFormatter.format(getFte());
                } catch (Exception e) {
                    // Do nothing
                }
            }
            return fteString;
        }

        /**
         * Gets the instructional time codes.
         *
         * @return List
         */
        public List<String> getInstructionalTimeCodes() {
            String value = getValueString(FIELD_INSTRUCTIONAL_TIME);
            return StringUtils.isEmpty(value) ? Collections.EMPTY_LIST
                    : Arrays.stream(value.split("\\s*,\\s*"))
                            .map(str -> {
                                String finalValue = null;
                                DictionaryExtractor extractor = ToolBean.getDictionaryExtractor();
                                DataDictionaryField dictionaryField = FIELD_INSTRUCTIONAL_TIME.getField(extractor);
                                if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
                                    finalValue =
                                            extractor.lookupReferenceCodeByRefTbl(
                                                    dictionaryField.getReferenceTableOid(), str,
                                                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                                }
                                return finalValue;
                            })
                            .filter(Objects::nonNull)
                            .distinct()
                            .collect(Collectors.toList());
        }

        /**
         * Gets the leave end date.
         *
         * @return Plain date
         */
        public PlainDate getLeaveEndDate() {
            return getValueDate(FIELD_LEAVE_END_DATE);
        }

        /**
         * Gets the leave start date.
         *
         * @return Plain date
         */
        public PlainDate getLeaveStartDate() {
            return getValueDate(FIELD_LEAVE_START_DATE);
        }

        /**
         * Gets the leave type.
         *
         * @return String
         */
        public String getLeaveType() {
            return getValueString(FIELD_LEAVE_TYPE);
        }

        /**
         * Gets the letter of permission.
         *
         * @return String
         */
        public String getLetterOfPermission() {
            return getValueString(FIELD_LETTER_OF_PERMISSION);
        }

        /**
         * Gets the NTIP status.
         *
         * @return boolean
         */
        public boolean getNTIPStatus() {
            return getValueLogical(FIELD_NTIP_STATUS);
        }

        /**
         * Gets the position type.
         *
         * @return String
         */
        public String getPositionType() {
            return StringUtils.emptyIfNull(getValueReferenceState(FIELD_JOB_CODE));
        }

        /**
         * Gets the temporary letter of approval.
         *
         * @return String
         */
        public String getTemporaryLetterOfApproval() {
            return getValueString(FIELD_TEMPORARY_LETTER_OF_APPROVAL);
        }

        /**
         * Checks if is active on.
         *
         * @param date the date
         * @return the boolean
         */
        public Boolean isActiveOn(PlainDate date) {
            boolean value = Range.of(getStartDate(), getEndDate()).contains(date);
            if (value && !StringUtils.isEmpty(getLeaveType()) && getLeaveStartDate() != null
                    && Range.of(getLeaveStartDate(), getLeaveEndDate()).contains(date)) {
                value = false;
            }
            return value;
        }

        /**
         * Checks if is teacher.
         *
         * @return true, if is teacher
         */
        public boolean isTeacher() {
            return VALID_TEACHING_TYPES.contains(this.getValueReferenceState(FIELD_TEACHING_TYPE));
        }

    }

    /**
     * The Class OnsisStudent.
     */
    public static class OnStudent extends ToolStudent {
        public static final String GENDER_TYPE_FEMALE = "F";
        public static final String GENDER_TYPE_MALE = "M";
        public static final String GENDER_TYPE_NOT_DISCLOSED = "N";
        public static final String GENDER_TYPE_SPECIFY = "S";

        private static final String ALIAS_PSN_ARRIVAL_DATE_CANADA = "all-psn-ArrivalDateCanada";
        private static final String ALIAS_PSN_BIRTH_COUNTRY = "all-psn-BirthCountry";
        private static final String ALIAS_PSN_BIRTH_PROVINCE = "all-psn-BirthProvince";
        private static final String ALIAS_PSN_CANADIAN_RES_STATUS = "all-psn-CanadaStatusCode";
        private static final String ALIAS_PSN_SHELTERED_STUDENT = "all-psn-ShelteredStudent";

        private static final String ALIAS_SKL_BSID = "all-skl-BSID";

        private static final String ALIAS_STD_AGE_VERIFICATION = "all-std-AgeVerification";
        private static final String ALIAS_STD_EDI_ID = "all-std-EdiId";
        private static final String ALIAS_STD_ELEM_ALT_REPORT_CARD_FLAG = "all-std-ElementaryAlternateReportCard";
        private static final String ALIAS_STD_EOY_DEMIT_BSID = "all-std-EoyrDemitBsid";
        private static final String ALIAS_STD_EOY_DEMIT_CODE = "all-std-EoyrDemitCode";
        private static final String ALIAS_STD_EOY_DEMIT_COUNTRY = "all-std-EoyrDemitCountry";
        private static final String ALIAS_STD_EOY_DEMIT_PROVINCE = "all-std-EoyrDemitProvince";
        private static final String ALIAS_STD_EXCLUDE_FROM_REPORTING = "all-std-ExcludeFromReporting";
        private static final String ALIAS_STD_FIRST_LANGUAGE_SPOKEN = "all-std-FirstLanguageSpoken";
        private static final String ALIAS_STD_FRENCH_ADMISSION_COMMITTEE_APPROVAL_DATE =
                "all-std-FrenchAdmissionCommitteeApprovalDate";
        private static final String ALIAS_STD_GRADE_9_COHORT = "all-std-Grade9Cohort";
        private static final String ALIAS_STD_GRADE_DESIGNATION = "all-std-GradeDesignationType";
        private static final String ALIAS_STD_INDIGENOUS = "all-std-Indigenous";
        private static final String ALIAS_STD_IPRC_REVIEW_DATE_HISTORY = "all-std-IPRCReviewDateHistory";
        private static final String ALIAS_STD_LOCAL_ID = "all-std-stdNumber";
        private static final String ALIAS_STD_MATURE_STUDENT = "all-std-MatureStudent";
        private static final String ALIAS_STD_OEN = "all-std-Oen";
        private static final String ALIAS_STD_RETAINED = "all-std-Retained";
        private static final String ALIAS_STD_SCHOLAR_AWARDED_DATE = "all-std-OntarioScholarAwardedDate";
        private static final String ALIAS_STD_OSSLT_ADJUDICATION = "all-std-OssltAdjudication";

        public static final String CHILD_KEY_COMMUNITY_INVOLVEMENT_ASSESSMENT = "communityInvolvementAssessments";
        public static final String CHILD_KEY_ECPP_PROGRAM = "ecppPrograms";
        public static final String CHILD_KEY_ELL_PROGRAM = "ellPrograms";
        public static final String CHILD_KEY_EXTENDED_DAY_PROGRAM = "extendedDayPrograms";
        public static final String CHILD_KEY_OSSLT_ASSESSMENT = "ossltAssessments";
        public static final String CHILD_KEY_OYAP_PROGRAM = "oyapPrograms";
        public static final String CHILD_KEY_SALEP_PROGRAM = "salepPrograms";
        public static final String CHILD_KEY_SHSM_ASSESSMENT = "shsmAssessments";
        public static final String CHILD_KEY_SLP_PROGRAM = "slpPrograms";
        public static final String CHILD_KEY_SLP_FR_PROGRAM = "slpFrPrograms";
        public static final String CHILD_KEY_SPED_PROGRAM = "spedPrograms";


        public static final ToolBeanColumn FIELD_AGE_VERIFICATION =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_AGE_VERIFICATION);
        public static final ToolBeanColumn FIELD_ARRIVAL_DATE_CANADA =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person(), ALIAS_PSN_ARRIVAL_DATE_CANADA);
        public static final ToolBeanColumn FIELD_BIRTH_COUNTRY =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person(), ALIAS_PSN_BIRTH_COUNTRY);
        public static final ToolBeanColumn FIELD_BIRTH_PROVINCE =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person(), ALIAS_PSN_BIRTH_PROVINCE);
        public static final ToolBeanColumn FIELD_CANADIAN_RES_STATUS =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person(), ALIAS_PSN_CANADIAN_RES_STATUS);
        public static final ToolBeanColumn FIELD_EDI_ID =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_EDI_ID);
        public static final ToolBeanColumn FIELD_ELEM_ALT_REPORT_CARD_FLAG =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_ELEM_ALT_REPORT_CARD_FLAG);
        public static final ToolBeanColumn FIELD_EOY_DEMIT_BSID =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_EOY_DEMIT_BSID);
        public static final ToolBeanColumn FIELD_EOY_DEMIT_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_EOY_DEMIT_CODE);
        public static final ToolBeanColumn FIELD_EOY_DEMIT_COUNTRY =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_EOY_DEMIT_COUNTRY);
        public static final ToolBeanColumn FIELD_EOY_DEMIT_PROVINCE =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_EOY_DEMIT_PROVINCE);
        public static final ToolBeanColumn FIELD_EXCLUDE_FROM_REPORTING =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_EXCLUDE_FROM_REPORTING);
        public static final ToolBeanColumn FIELD_FIRST_LANGUAGE_SPOKEN =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_FIRST_LANGUAGE_SPOKEN);
        public static final ToolBeanColumn FIELD_FRENCH_ADMISSION_COMMITTEE_APPROVAL_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT, new ToolBeanColumn.AliasDefinition(
                        ALIAS_STD_FRENCH_ADMISSION_COMMITTEE_APPROVAL_DATE, null, false));
        public static final ToolBeanColumn FIELD_GENDER_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person().genderCode());
        public static final ToolBeanColumn FIELD_GENDER_SPECIFY =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person(), ALIAS_PSN_GENDER_SPECIFY);
        public static final ToolBeanColumn FIELD_GRADE_DESIGNATION =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_GRADE_DESIGNATION);
        public static final ToolBeanColumn FIELD_GRADE_9_COHORT =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_GRADE_9_COHORT);
        public static final ToolBeanColumn FIELD_INDIGENOUS =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_INDIGENOUS);
        public static final ToolBeanColumn FIELD_IPRC_REVIEW_DATE_HISTORY =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_IPRC_REVIEW_DATE_HISTORY);
        public static final ToolBeanColumn FIELD_LEGAL_FIRST_NAME =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person(), ALIAS_PSN_LEGAL_FIRST_NAME);
        public static final ToolBeanColumn FIELD_LEGAL_LAST_NAME =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person(), ALIAS_PSN_LEGAL_LAST_NAME);
        public static final ToolBeanColumn FIELD_LEGAL_MIDDLE_NAME =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person(), ALIAS_PSN_LEGAL_MIDDLE_NAME);
        public static final ToolBeanColumn FIELD_LOCAL_ID_OVERRIDE =
                new ToolBeanColumn(SisBeanPaths.STUDENT,
                        new ToolBeanColumn.AliasDefinition(ALIAS_STD_LOCAL_ID, null, false));
        public static final ToolBeanColumn FIELD_MATURE_STUDENT =
                new ToolBeanColumn(SisBeanPaths.STUDENT,
                        new ToolBeanColumn.AliasDefinition(ALIAS_STD_MATURE_STUDENT, null, false));
        public static final ToolBeanColumn FIELD_NEXT_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT.nextSchoolOid());
        public static final ToolBeanColumn FIELD_OEN =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_OEN);
        public static final ToolBeanColumn FIELD_RETAINED =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_RETAINED);
        public static final ToolBeanColumn FIELD_OSSLT_ADJUDICATION =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_OSSLT_ADJUDICATION);
        public static final ToolBeanColumn FIELD_SCHOLAR_AWARDED_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_SCHOLAR_AWARDED_DATE);
        public static final ToolBeanColumn FIELD_SHELTERED_STUDENT =
                new ToolBeanColumn(SisBeanPaths.STUDENT.person(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_PSN_SHELTERED_STUDENT, null, false));
        public static final ToolBeanColumn FIELD_SCHOOL_BSID =
                new ToolBeanColumn(SisBeanPaths.STUDENT.school(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_SKL_BSID, null, false));

        // Nonquery Fields
        public static ToolBeanRelationship CHILD_COMMUNITY_INVOLVEMENT_ASSESSMENTS =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.getBeanType(),
                        CommunityInvolvementAssessment.class,
                        CHILD_KEY_COMMUNITY_INVOLVEMENT_ASSESSMENT,
                        SisBeanPaths.STUDENT_ASSESSMENT.studentOid().toString(),
                        SisBeanPaths.STUDENT.studentAssessments().getRelationshipType());

        public static ToolBeanRelationship CHILD_ECPP_PROGRAMS =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.getBeanType(),
                        OnStudentECPPProgram.class,
                        CHILD_KEY_ECPP_PROGRAM,
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid().toString(),
                        SisBeanPaths.STUDENT.programParticipation().getRelationshipType());

        public static ToolBeanRelationship CHILD_ELL_PROGRAMS =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.getBeanType(),
                        OnStudentELLProgram.class,
                        CHILD_KEY_ELL_PROGRAM,
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid().toString(),
                        SisBeanPaths.STUDENT.programParticipation().getRelationshipType());

        public static ToolBeanRelationship CHILD_EXTENDED_DAY_PROGRAMS =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.getBeanType(),
                        OnStudentExtendedDay.class,
                        CHILD_KEY_EXTENDED_DAY_PROGRAM,
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid().toString(),
                        SisBeanPaths.STUDENT.programParticipation().getRelationshipType());

        public static ToolBeanRelationship CHILD_FTE_MONTHLY_RECORDS =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.userDefinedRecordsD().getBeanType(),
                        SisBeanPaths.STUDENT.userDefinedRecordsD().getValueType(),
                        SisBeanPaths.STUDENT.userDefinedRecordsD().getPath(),
                        SisBeanPaths.USER_DEFINED_TABLE_D.studentOid().getPath(),
                        SisBeanPaths.STUDENT.userDefinedRecordsD().getRelationshipType());

        public static ToolBeanRelationship CHILD_FTE_RECORDS =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.userDefinedRecordsC().getBeanType(),
                        SisBeanPaths.STUDENT.userDefinedRecordsC().getValueType(),
                        SisBeanPaths.STUDENT.userDefinedRecordsC().getPath(),
                        SisBeanPaths.USER_DEFINED_TABLE_C.studentOid().getPath(),
                        SisBeanPaths.STUDENT.userDefinedRecordsC().getRelationshipType());

        public static ToolBeanRelationship CHILD_GRADUATION_STUDENT_PROGRAMS =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.programStudies().getBeanType(),
                        SisBeanPaths.STUDENT.programStudies().getValueType(),
                        SisBeanPaths.STUDENT.programStudies().getPath(),
                        SisBeanPaths.GRADUATION_STUDENT_PROGRAM.studentOid().getPath(),
                        SisBeanPaths.STUDENT.programStudies().getRelationshipType());

        public static ToolBeanRelationship CHILD_OSSLT_ASSESSMENTS =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.getBeanType(),
                        OssltAssessment.class,
                        CHILD_KEY_OSSLT_ASSESSMENT,
                        SisBeanPaths.STUDENT_ASSESSMENT.studentOid().toString(),
                        SisBeanPaths.STUDENT.studentAssessments().getRelationshipType());

        public static ToolBeanRelationship CHILD_OYAP_PROGRAMS =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.getBeanType(),
                        OnStudentOyap.class,
                        CHILD_KEY_OYAP_PROGRAM,
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid().toString(),
                        SisBeanPaths.STUDENT.programParticipation().getRelationshipType());

        public static ToolBeanRelationship CHILD_SALEP_PROGRAMS =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.getBeanType(),
                        OnStudentSalep.class,
                        CHILD_KEY_SALEP_PROGRAM,
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid().toString(),
                        SisBeanPaths.STUDENT.programParticipation().getRelationshipType());

        public static ToolBeanRelationship CHILD_SHSM_ASSESSMENTS =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.getBeanType(),
                        ShsmAssessment.class,
                        CHILD_KEY_SHSM_ASSESSMENT,
                        SisBeanPaths.STUDENT_ASSESSMENT.studentOid().toString(),
                        SisBeanPaths.STUDENT.studentAssessments().getRelationshipType());

        public static ToolBeanRelationship CHILD_SLP_PROGRAMS =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.getBeanType(),
                        OnStudentSLPProgram.class,
                        CHILD_KEY_SLP_PROGRAM,
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid().toString(),
                        SisBeanPaths.STUDENT.programParticipation().getRelationshipType());

        public static ToolBeanRelationship CHILD_SLP_FR_PROGRAMS =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.getBeanType(),
                        OnStudentSLPProgramFrench.class,
                        CHILD_KEY_SLP_FR_PROGRAM,
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid().toString(),
                        SisBeanPaths.STUDENT.programParticipation().getRelationshipType());

        public static ToolBeanRelationship CHILD_SPED_PROGRAMS =
                new ToolBeanRelationship(SisBeanPaths.STUDENT.getBeanType(),
                        OnStudentSped.class,
                        CHILD_KEY_SPED_PROGRAM,
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid().toString(),
                        SisBeanPaths.STUDENT.programParticipation().getRelationshipType())
                                .addComparator(new Comparator<ToolBean>() {
                                    @Override
                                    public int compare(ToolBean o1, ToolBean o2) {
                                        OnStudentSped sped1, sped2;
                                        sped1 = (OnStudentSped) o1;
                                        sped2 = (OnStudentSped) o2;
                                        return sped1.getStartDate().compareTo(sped2.getStartDate());
                                    }
                                });

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStudent.FULL_DEFINITION
                .expand(FIELD_AGE_VERIFICATION,
                        FIELD_ARRIVAL_DATE_CANADA,
                        FIELD_BIRTH_COUNTRY,
                        FIELD_BIRTH_PROVINCE,
                        FIELD_CANADIAN_RES_STATUS,
                        FIELD_EDI_ID,
                        FIELD_ELEM_ALT_REPORT_CARD_FLAG,
                        FIELD_EOY_DEMIT_BSID,
                        FIELD_EOY_DEMIT_CODE,
                        FIELD_EOY_DEMIT_COUNTRY,
                        FIELD_EOY_DEMIT_PROVINCE,
                        FIELD_EXCLUDE_FROM_REPORTING,
                        FIELD_FIRST_LANGUAGE_SPOKEN,
                        FIELD_FRENCH_ADMISSION_COMMITTEE_APPROVAL_DATE,
                        FIELD_GENDER_CODE,
                        FIELD_GENDER_SPECIFY,
                        FIELD_GRADE_9_COHORT,
                        FIELD_GRADE_DESIGNATION,
                        FIELD_INDIGENOUS,
                        FIELD_IPRC_REVIEW_DATE_HISTORY,
                        FIELD_LEGAL_FIRST_NAME,
                        FIELD_LEGAL_LAST_NAME,
                        FIELD_LEGAL_MIDDLE_NAME,
                        FIELD_LOCAL_ID_OVERRIDE,
                        FIELD_MATURE_STUDENT,
                        FIELD_NEXT_SCHOOL_OID,
                        FIELD_OEN,
                        FIELD_OSSLT_ADJUDICATION,
                        FIELD_RETAINED,
                        FIELD_SCHOLAR_AWARDED_DATE,
                        FIELD_SHELTERED_STUDENT,
                        FIELD_SCHOOL_BSID)
                .expandRelationships(
                        CHILD_COMMUNITY_INVOLVEMENT_ASSESSMENTS,
                        CHILD_ECPP_PROGRAMS,
                        CHILD_ELL_PROGRAMS,
                        CHILD_EXTENDED_DAY_PROGRAMS,
                        CHILD_FTE_MONTHLY_RECORDS,
                        CHILD_GRADUATION_STUDENT_PROGRAMS,
                        CHILD_OSSLT_ASSESSMENTS,
                        CHILD_OYAP_PROGRAMS,
                        CHILD_SALEP_PROGRAMS,
                        CHILD_SHSM_ASSESSMENTS,
                        CHILD_SLP_PROGRAMS,
                        CHILD_SLP_FR_PROGRAMS,
                        CHILD_SPED_PROGRAMS,
                        CHILD_STUDENT_ATTENDANCE);

        /**
         * Instantiates a new onsis student.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnStudent(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the age on a particular date in years.
         *
         * @param asOfDate PlainDate
         * @return int age
         */
        public int getAgeAsOfDate(PlainDate asOfDate) {
            return OnBeans.getAgeAsOfDate(getDob(), asOfDate);
        }

        /**
         * Gets the value "all-psn-ArrivalDateCanada" converted to date.
         *
         * @return PlainDate
         */
        public PlainDate getArrivalDateCanada() {
            return this.getValueDate(FIELD_ARRIVAL_DATE_CANADA);
        }

        /**
         * Gets the Onsis reference code value for the "all-psn-BirthCountry".
         *
         * @return String state reference code value for birth country
         */
        public String getBirthCountry() {
            return this.getValueReferenceState(FIELD_BIRTH_COUNTRY);
        }

        /**
         * Gets value for the "all-psn-BirthCountry".
         *
         * @return String for birth country
         */
        public String getBirthCountryPlain() {
            return this.getValueString(FIELD_BIRTH_COUNTRY);
        }

        /**
         * Gets the Onsis reference code value for the "all-psn-BirthProvince".
         *
         * @return String state reference code value for birth province
         */
        public String getBirthProvince() {
            return this.getValueReferenceState(FIELD_BIRTH_PROVINCE);
        }

        /**
         * Gets the board resident status.
         *
         * @param broker the broker
         * @param school the school
         * @param date the date
         * @param isBreakOnYog the is break on yog
         * @param isBreakOnStatus the is break on status
         * @return the board resident status
         */
        public String getBoardResidentStatus(X2Broker broker,
                                             OnSchool school,
                                             PlainDate date,
                                             boolean isBreakOnYog,
                                             boolean isBreakOnStatus) {
            List<OnAnnualSpan> candidates = getEnrollmentSpans(broker, isBreakOnYog, isBreakOnStatus).stream()
                    .filter(span -> span.getSchool().equals(school))
                    .filter(span -> !span.isSecondary())
                    .filter(span -> !span.getFirstActiveInSessionDate().after(date))
                    .map(span -> (OnAnnualSpan) span)
                    .collect(Collectors.toList());

            List<OnAnnualSpan> enrollmentSpans = candidates.stream()
                    .filter(span -> span.getLastActiveInSessionDate() == null
                            || !span.getLastActiveInSessionDate().before(date))
                    .collect(Collectors.toList());

            if (enrollmentSpans.isEmpty() && !candidates.isEmpty()) {
                Optional<OnAnnualSpan> maxSpan = candidates.stream().max(new Comparator<OnAnnualSpan>() {

                    @Override
                    public int compare(OnAnnualSpan span0, OnAnnualSpan span1) {
                        return span0.getLastActiveInSessionDate().compareTo(span1.getLastActiveInSessionDate());
                    }
                });
                if (maxSpan.isPresent()) {
                    enrollmentSpans = Arrays.asList(maxSpan.get());
                }
            }
            enrollmentSpans.stream()
                    .map(span -> span.getRecentEnrollmentES())
                    .filter(Objects::nonNull)
                    .map(enr -> ((OnEnrollment) enr).getBoardResidentStatus());
            Set<String> residentStatusSet = enrollmentSpans.stream()
                    .map(span -> span.getRecentEnrollmentES())
                    .filter(Objects::nonNull)
                    .map(enr -> ((OnEnrollment) enr).getBoardResidentStatus())
                    .collect(Collectors.toSet());

            if (residentStatusSet.contains(OnsisConstants.VALUE_STU_BRD_RES_STAT_PUPIL_OF_BOARD)) {
                return OnsisConstants.VALUE_STU_BRD_RES_STAT_PUPIL_OF_BOARD;
            }

            return residentStatusSet.stream().filter(Objects::nonNull).reduce((first, second) -> second).orElse(null);
        }

        /**
         * Gets the community involvement assessments.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<CommunityInvolvementAssessment> getCommunityInvolvementAssessments(X2Broker broker) {
            return (List<CommunityInvolvementAssessment>) getChildren(broker, CHILD_COMMUNITY_INVOLVEMENT_ASSESSMENTS);
        }

        /**
         * Gets the ecpp programs.
         *
         * @param broker the broker
         * @return the ecpp programs
         */
        public List<OnStudentECPPProgram> getEcppPrograms(X2Broker broker) {
            return (List<OnStudentECPPProgram>) getChildren(broker, CHILD_ECPP_PROGRAMS);
        }

        /**
         * Gets the EDI ID.
         *
         * @return String
         */
        public String getEdiID() {
            return getValueString(FIELD_EDI_ID);
        }

        /**
         * Gets the elementary alternate report card flag.
         *
         * @return the elementary alternate report card flag
         */
        public String getElementaryAlternateReportCardFlag() {
            return getValueReferenceState(FIELD_ELEM_ALT_REPORT_CARD_FLAG);
        }

        /**
         * Gets the sped programs.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<OnStudentELLProgram> getEllPrograms(X2Broker broker) {
            return (List<OnStudentELLProgram>) getChildren(broker, CHILD_ELL_PROGRAMS);
        }

        /**
         * Gets the enrollment for date.
         *
         * @param date the date
         * @param types the types
         * @param broker the broker
         * @return the enrollment for date
         */
        public ToolEnrollment getEnrollmentForDate(PlainDate date, String types, X2Broker broker) {
            List<ToolEnrollment> enrollments = getEnrollments(broker);
            ToolEnrollment lastEnrollment = null;
            if (enrollments != null) {
                for (ToolEnrollment enrollment : enrollments) {
                    if (enrollment.getEnrollmentDate() != null && !enrollment.getEnrollmentDate().after(date)) {
                        if (types.contains(enrollment.getEnrollmentType())) {
                            lastEnrollment = enrollment;
                            break;
                        }
                    }
                }
            }
            return lastEnrollment;
        }

        /**
         * Gets the primary enrollment spans.
         *
         * @param school the school
         * @param broker the broker
         * @return the primary enrollment spans
         */
        public List<AnnualSpan> getPrimaryEnrollmentSpans(ToolSchool school, X2Broker broker) {
            return getEnrollmentSpans(broker, false, false).stream()
                    .filter(span -> !span.isSecondary())
                    .filter(span -> span.getSchool().equals(school))
                    .collect(Collectors.toList());
        }

        /**
         * Gets the exclude from reporting indicator.
         *
         * @return boolean
         */
        public boolean getExcludeFromReporting() {
            return getValueLogical(FIELD_EXCLUDE_FROM_REPORTING);
        }

        /**
         * Gets the extended day programs.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<OnStudentExtendedDay> getExtendedDayPrograms(X2Broker broker) {
            return (List<OnStudentExtendedDay>) getChildren(broker, CHILD_EXTENDED_DAY_PROGRAMS);
        }

        /**
         * Gets the french admission committee approval date.
         *
         * @return PlainDate
         */
        public PlainDate getFrenchAdmissionCommitteeApprovalDate() {
            return this.getValueDate(FIELD_FRENCH_ADMISSION_COMMITTEE_APPROVAL_DATE);
        }

        /**
         * Gets the fte monthly record.
         *
         * @param broker X2Broker
         * @param schoolOids the school oids
         * @param submissionPeriodCode the submission period code
         * @return Fte monthly
         */
        public FteMonthly getFteMonthlyRecord(X2Broker broker, List<String> schoolOids, String submissionPeriodCode) {
            return getFteMonthlyRecords(broker, schoolOids, submissionPeriodCode).stream()
                    .sorted(new Comparator<FteMonthly>() {

                        @Override
                        public int compare(FteMonthly o1, FteMonthly o2) {
                            return o1.getReportDate().compareTo(o2.getReportDate());
                        }
                    }).findFirst().orElse(null);
        }

        /**
         * Gets the fte monthly records.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<FteMonthly> getFteMonthlyRecords(X2Broker broker) {
            return (List<FteMonthly>) getChildren(broker, CHILD_FTE_MONTHLY_RECORDS);
        }

        /**
         * Gets the fte monthly records.
         *
         * @param broker X2Broker
         * @param schoolOids the school oids
         * @param submissionPeriodCode the submission period code
         * @return List
         */
        public List<FteMonthly> getFteMonthlyRecords(X2Broker broker,
                                                     List<String> schoolOids,
                                                     String submissionPeriodCode) {
            return getFteMonthlyRecords(broker).stream()
                    .filter(fteMonthly -> schoolOids.contains(fteMonthly.getSchoolOid())
                            && submissionPeriodCode.equals(fteMonthly.getMonth()))
                    .collect(Collectors.toList());
        }

        /**
         * Gets the fte record for date.
         *
         * @param broker the broker
         * @param school the school
         * @param date the date
         * @return the fte record for date
         */
        public FteRecord getFteRecordForDate(X2Broker broker, OnSchool school, PlainDate date) {
            return getFteRecords(broker).stream()
                    .filter(fteRecord -> school == null || school.getOid().equals(fteRecord.getSchoolOid()))
                    .filter(fteRecord -> fteRecord.getFteDate().compareTo(date) <= 0)
                    .sorted(Comparator.comparing(FteRecord::getFteDate))
                    .reduce((first, second) -> second)
                    .orElse(null);
        }

        /**
         * Gets the fte monthly records.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<FteRecord> getFteRecords(X2Broker broker) {
            return (List<FteRecord>) getChildren(broker, CHILD_FTE_RECORDS);
        }

        /**
         * Gets the gender specify.
         *
         * @return String
         */
        public String getGenderSpecify() {
            return getValueString(FIELD_GENDER_SPECIFY);
        }

        /**
         * Gets the gender type.
         *
         * @return String
         */
        public String getGenderType() {
            return this.getValueReferenceState(FIELD_GENDER_CODE);
        }

        /**
         * Gets the grade 9 cohort.
         *
         * @return the grade 9 cohort
         */
        public int getGrade9Cohort() {
            String rawValue = getValueString(FIELD_GRADE_9_COHORT);
            if (StringUtils.isEmpty(rawValue)) {
                return 0;
            }
            if (rawValue.indexOf("-") > 0) {
                rawValue = rawValue.substring(0, rawValue.indexOf("-"));
            }
            Integer value = (Integer) PredefinedConverter.INTEGER.convertedValue(rawValue);
            return value == null ? 0 : value.intValue();
        }

        /**
         * Gets the grade designation.
         *
         * @return String
         */
        public String getGradeDesignation() {
            return getValueString(FIELD_GRADE_DESIGNATION);
        }

        /**
         * Gets the iprc review date history.
         *
         * @return Object
         */
        public Object getIprcReviewDateHistory() {
            return getValue(FIELD_IPRC_REVIEW_DATE_HISTORY);
        }

        /**
         * Gets the student legal first name.
         *
         * @return String
         */
        public String getLegalFirstName() {
            return getValueString(FIELD_LEGAL_FIRST_NAME);
        }

        /**
         * Gets the student legal last name.
         *
         * @return String
         */
        public String getLegalLastName() {
            return getValueString(FIELD_LEGAL_LAST_NAME);
        }

        /**
         * Gets the student legal middle name.
         *
         * @return String
         */
        public String getLegalMiddleName() {
            return getValueString(FIELD_LEGAL_MIDDLE_NAME);
        }

        /**
         * Gets the student local id override.
         *
         * @return String
         */
        public String getLocalIdOverride() {
            return getValueString(FIELD_LOCAL_ID_OVERRIDE);
        }

        /**
         * Gets the mature student.
         *
         * @return Plain date
         */
        public PlainDate getMatureStudent() {
            return getValueDate(FIELD_MATURE_STUDENT);
        }

        /**
         * Gets the next school oid.
         *
         * @return String
         */
        public String getNextSchoolOid() {
            return getValueString(FIELD_NEXT_SCHOOL_OID);
        }

        /**
         * Gets the osslt assessments.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<OssltAssessment> getOssltAssessments(X2Broker broker) {
            return (List<OssltAssessment>) getChildren(broker, CHILD_OSSLT_ASSESSMENTS);
        }

        /**
         * Gets the oen.
         *
         * @return String
         */
        public String getOen() {
            String oen = getValueString(FIELD_OEN);
            if (!StringUtils.isEmpty(oen)) {
                oen = oen.replaceAll(PATTERN_DASH, EMPTY_STRING);
            }
            return oen;
        }

        /**
         * Gets the oen.
         *
         * @return String
         */
        public String getOenRaw() {
            return getValueString(FIELD_OEN);
        }

        /**
         * Gets the ontario scholar award date.
         *
         * @return PlainDate
         */
        public PlainDate getOntarioScholarAwardDate() {
            return this.getValueDate(FIELD_SCHOLAR_AWARDED_DATE);
        }

        /**
         * Gets the osslt ajudication.
         *
         * @return Object
         */
        public Object getOssltAdjudication() {
            return getValueAsJavaType(FIELD_OSSLT_ADJUDICATION);
        }

        /**
         * Gets the oyap programs.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<OnStudentOyap> getOyapPrograms(X2Broker broker) {
            return (List<OnStudentOyap>) getChildren(broker, CHILD_OYAP_PROGRAMS);
        }

        /**
         * Gets the program studies.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<OnGraduationStudentProgram> getProgramStudies(X2Broker broker) {
            return (List<OnGraduationStudentProgram>) getChildren(broker, CHILD_GRADUATION_STUDENT_PROGRAMS);
        }

        /**
         * Gets the program studies.
         *
         * @param issuedDateRange Range<Date> The range of issue dates to return.
         * @param bsid String The matching BSID for returned values. Returned values will have this
         *        BSID or no BSID.
         * @param isUnderEnrolment boolean Used to indicate whether diplomas are for an enrollment
         *        span or for when no enrollment spans.
         * @param enrollmentRange Range<Date> The range used to filter records to this enrollment
         *        span. This is not used unless isUnderEnrollment is true.
         * @return List
         */
        public List<OnGraduationStudentProgram> getProgramStudies(Range<Date> issuedDateRange,
                                                                  String bsid,
                                                                  boolean isUnderEnrolment,
                                                                  Range<Date> enrollmentRange) {
            List<OnGraduationStudentProgram> diplomasFilterable = getProgramStudies(ToolBean.getBroker(true)).stream()
                    .filter(gsr -> gsr.getIssuedDate() != null
                            && issuedDateRange.contains(gsr.getIssuedDate()))
                    .filter(gsr -> StringUtils.isBlank(gsr.getBsidDiplomaEarned())
                            || StringUtils.isEqual(gsr.getBsidDiplomaEarned(), bsid))
                    .collect(Collectors.toList());

            /*
             * Determine DIPLOMA_TYPE_COLLEGE_PREP issue date to filter subsequent SHSM
             */
            Optional<PlainDate> graduationDate = getProgramStudies(ToolBean.getBroker(true)).stream()
                    .filter(gsr -> OnGraduationStudentProgram.DIPLOMA_TYPE_COLLEGE_PREP
                            .equals(ToolBean.getDictionaryExtractor(true).getStateValue(gsr,
                                    OnGraduationStudentProgram.FIELD_DIPLOMA_TYPE)))
                    .map(OnGraduationStudentProgram::getIssuedDate)
                    .filter(Objects::nonNull).reduce(BinaryOperator.maxBy(Comparator.naturalOrder()));

            /*
             * An Enrolment span (as opposed to a non-enrolment)
             * should only export Diploma within its date range
             */
            if (isUnderEnrolment) {
                diplomasFilterable = diplomasFilterable.stream()
                        .filter(gsr -> {
                            if (enrollmentRange.getStart() == null) {
                                return false;
                            }

                            PlainDate issuedDate = gsr.getIssuedDate();
                            if (issuedDate == null) {
                                return false;
                            }
                            return enrollmentRange.contains(issuedDate);
                        }).collect(Collectors.toList());
            }

            diplomasFilterable = diplomasFilterable.stream()
                    .filter(gsr -> {
                        PlainDate issuedDate = gsr.getIssuedDate();
                        if (issuedDate == null) {
                            return true;
                        }

                        if (OnGraduationStudentProgram.DIPLOMA_TYPE_OSSD_SHSM
                                .equals(ToolBean.getDictionaryExtractor(true).getStateValue(gsr,
                                        OnGraduationStudentProgram.FIELD_DIPLOMA_TYPE))
                                && graduationDate.isPresent()
                                && issuedDate.after(graduationDate.get())) {
                            return false;
                        }

                        /*
                         * Report only primary diploma
                         */
                        return gsr.getPrimaryIndicator();
                    }).collect(Collectors.toList());

            /*
             * A NonEnrolment should only export Diploma
             * that aren't in any Enrolment span date range
             */
            if (!isUnderEnrolment) {
                diplomasFilterable = diplomasFilterable.stream()
                        .filter(gsr -> {
                            PlainDate issuedDate = gsr.getIssuedDate();
                            if (issuedDate == null) {
                                return true;
                            }

                            String issuedByBSID = gsr.getBsidDiplomaEarned();
                            List<String> limitingSchoolOids = DistrictManager.getSchoolsFilterable()
                                    .getGroup(OnSchool.FIELD_BSID, issuedByBSID)
                                    .stream()
                                    .map(ToolBean::getOid).collect(Collectors.toList());
                            return !getEnrollmentSpans(ToolBean.getBroker(true), false, false).stream()
                                    .filter(span -> (StringUtils.isEmpty(issuedByBSID)
                                            || limitingSchoolOids.contains(span.getSchool().getOid()))
                                            && span.getDateRange().contains(issuedDate)
                                            && !span.isSecondary())
                                    .findAny().isPresent();
                        }).collect(Collectors.toList());
            }

            return diplomasFilterable;
        }

        /**
         * Gets the retained indicator.
         *
         * @return boolean
         */
        public boolean getRetainedIndicator() {
            return getValueLogical(FIELD_RETAINED);
        }

        /**
         * Gets the salep programs.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<OnStudentSalep> getSalepPrograms(X2Broker broker) {
            return (List<OnStudentSalep>) getChildren(broker, CHILD_SALEP_PROGRAMS);
        }

        /**
         * Gets the scholar awarded date.
         *
         * @return PlainDate
         */
        public PlainDate getScholarAwardedDate() {
            return this.getValueDate(FIELD_SCHOLAR_AWARDED_DATE);
        }

        /**
         * Gets the sheltered student indicator.
         *
         * @return the sheltered student indicator
         */
        public boolean getShelteredStudentIndicator() {
            return getValueLogical(FIELD_SHELTERED_STUDENT);
        }

        /**
         * Gets the shsm assessments.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<ShsmAssessment> getShsmAssessments(X2Broker broker) {
            return (List<ShsmAssessment>) getChildren(broker, CHILD_SHSM_ASSESSMENTS);
        }

        /**
         * Gets the sped programs.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<OnStudentSLPProgramFrench> getSlpFrenchPrograms(X2Broker broker) {
            return (List<OnStudentSLPProgramFrench>) getChildren(broker, CHILD_SLP_FR_PROGRAMS);
        }

        /**
         * Gets the sped programs.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<OnStudentSLPProgram> getSlpPrograms(X2Broker broker) {
            return (List<OnStudentSLPProgram>) getChildren(broker, CHILD_SLP_PROGRAMS);
        }

        /**
         * Gets the sped programs.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<OnStudentSped> getSpedPrograms(X2Broker broker) {
            return (List<OnStudentSped>) getChildren(broker, CHILD_SPED_PROGRAMS);
        }


        /**
         * Gets the student schedule spans.
         *
         * @param broker the broker
         * @param schoolOids the school oids
         * @param isSectionIncluded the is section included
         * @param reportDateRange the report date range
         * @param enrollmentDateRange the limiting date range is intersection of global data date
         *        range
         *        and limiting date range
         * @return the student schedule spans
         */
        public List<OnStudentScheduleSpan> getStudentScheduleSpans(X2Broker broker,
                                                                   List<String> schoolOids,
                                                                   Predicate<OnSection> isSectionIncluded,
                                                                   Range<Date> reportDateRange,
                                                                   Range<Date> enrollmentDateRange) {
            PlainDate enrStartDate = enrollmentDateRange == null ? null : (PlainDate) enrollmentDateRange.getStart();
            return super.getStudentScheduleSpans(broker).stream()
                    .map(scheduleSpan -> (OnStudentScheduleSpan) scheduleSpan)
                    .filter(scheduleSpan -> schoolOids
                            .contains(scheduleSpan.getSection().getSchedule(broker).getSchoolOid()))
                    .filter(scheduleSpan -> enrollmentDateRange == null
                            || scheduleSpan.getDateRange().isOverlap(enrollmentDateRange))
                    .filter(scheduleSpan -> scheduleSpan.getDateRange().isOverlap(reportDateRange))
                    .filter(scheduleSpan -> isSectionIncluded.test((OnSection) scheduleSpan.getSection()))
                    .filter(scheduleSpan -> !scheduleSpan.getExitDate()
                            .before(scheduleSpan.calculateCourseStartDate(broker, reportDateRange, enrStartDate)))
                    .collect(Collectors.toList());
        }

        /**
         * Checks if is changing school.
         *
         * @return true, if is changing school
         */
        public boolean isChangingSchool() {
            return !StringUtils.isEmpty(getNextSchoolOid()) && !getNextSchoolOid().equals(getSchoolOid());
        }

        /**
         * Checks if is mature student.
         *
         * @param broker X2Broker
         * @param testDate PlainDate
         * @param schoolOids List<String>
         * @return true, if is mature student
         */
        public boolean isMatureStudent(X2Broker broker, PlainDate testDate, List<String> schoolOids) {
            PlainDate matureDate = getMatureStudent();
            if (matureDate != null && !matureDate.after(testDate)) {
                return true;
            }
            OnEnrollment record = (OnEnrollment) getEnrollments(broker).stream()
                    .filter(enr -> StudentEnrollment.ENTRY.equals(enr.getEnrollmentType())
                            || StudentEnrollment.STATUS_CHANGE.equals(enr.getEnrollmentType()))
                    .filter(enr -> schoolOids.contains(enr.getSchoolOid()))
                    .findFirst().orElse(null);
            if (record != null && record.getMatureFlag()) {
                return true;
            }
            return false;

        }

    }

    /**
     * The Class OnsisStudentAttendance.
     */
    public static class OnStudentAttendance extends ToolStudentAttendance {
        public static final String STD_ATT_G_DAY_CODE = "G";

        public static final ToolBeanColumn FIELD_REASON_CODE_02 =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ATTENDANCE.reasonCode02());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStudentAttendance.FULL_DEFINITION
                .expand(FIELD_REASON_CODE_02);

        /**
         * Instantiates a new onsis student attendance.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnStudentAttendance(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the reason code 02.
         *
         * @return String
         */
        public String getReasonCode02() {
            return getValueString(FIELD_REASON_CODE_02);
        }

    }

    /**
     * The Class OnsisStudentAssessment.
     */
    public static class OnStudentAssessment extends ToolBean {
        public static final ToolBeanColumn FIELD_ASSESSMENT_ID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT.assessmentDefinition().id());
        public static final ToolBeanColumn FIELD_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT.date());
        public static final ToolBeanColumn FIELD_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT.schoolOid());
        public static final ToolBeanColumn FIELD_STUDENT_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT.studentOid());


        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_ASSESSMENT_ID,
                        FIELD_DATE,
                        FIELD_SCHOOL_OID,
                        FIELD_STUDENT_OID);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT_ASSESSMENT.getBeanType();
        }

        /**
         * Instantiates a new onsis student assessment.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnStudentAssessment(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the date.
         *
         * @return Plain date
         */
        public PlainDate getDate() {
            return getValueDate(FIELD_DATE);
        }

        /**
         * Gets the student oid.
         *
         * @return String
         */
        public String getStudentOid() {
            return getValueString(FIELD_STUDENT_OID);
        }
    }

    /**
     * The Class OnsisStudentExtendedDay.
     */
    public static class OnStudentExtendedDay extends OnStudentProgramParticipation {
        private static final String DDX_ID = "STD-PGM-EXTDAY";

        private static final String ALIAS_PGM_EXT_DAY_PROGRAM_TYPE = "pgm-extday-program-type";

        public static final ToolBeanColumn FIELD_PROGRAM_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION, ALIAS_PGM_EXT_DAY_PROGRAM_TYPE, DDX_ID);

        // Nonquery Fields
        public static final ToolBeanColumn FIELD_DDX_ID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.extendedDataDictionary().id());

        public static ToolBeanRelationship PARENT_STUDENT =
                new ToolBeanRelationship(OnStudentExtendedDay.class,
                        SisBeanPaths.STUDENT.getBeanType(),
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid().getPath(),
                        OnStudent.CHILD_KEY_EXTENDED_DAY_PROGRAM,
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.student().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = OnStudentProgramParticipation.FULL_DEFINITION
                .expand(FIELD_PROGRAM_TYPE)
                .expandRelationships(PARENT_STUDENT)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(FIELD_DDX_ID.resolve(getDictionaryExtractor()), DDX_ID);
                        return criteria;
                    }
                });

        /**
         * Instantiates a new onsis student extended day.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnStudentExtendedDay(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the program type.
         *
         * @return String
         */
        public String getProgramType() {
            return getValueString(FIELD_PROGRAM_TYPE);
        }
    }

    /**
     * The Class OnsisStudentOyap.
     */
    public static class OnStudentOyap extends OnStudentProgramParticipation {
        private static final String DDX_ID = "STD-PGM-OYAP";

        // Nonquery Fields
        public static final ToolBeanColumn FIELD_DDX_ID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.extendedDataDictionary().id());

        public static ToolBeanRelationship PARENT_STUDENT =
                new ToolBeanRelationship(OnStudentOyap.class,
                        SisBeanPaths.STUDENT.getBeanType(),
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid().getPath(),
                        OnStudent.CHILD_KEY_OYAP_PROGRAM,
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.student().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = OnStudentProgramParticipation.FULL_DEFINITION
                .expandRelationships(PARENT_STUDENT)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(FIELD_DDX_ID.resolve(getDictionaryExtractor()), DDX_ID);
                        return criteria;
                    }
                });

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public OnStudentOyap(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }
    }

    /**
     * The Class OnStudentProgramParticipation.
     */
    public static class OnStudentProgramParticipation extends ToolStudentProgramParticipation {
        private static final String ALIAS_PGM_SCHOOL_OID = "all-pgm-SchoolOid";

        public static final ToolBeanColumn FIELD_SCHOOL_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION, ALIAS_PGM_SCHOOL_OID);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStudentProgramParticipation.FULL_DEFINITION
                .expand(FIELD_SCHOOL_OID);

        /**
         * Instantiates a new program.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public OnStudentProgramParticipation(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the school.
         *
         * @param broker X2Broker
         * @return School
         */
        public ToolSchool getSchool(X2Broker broker) {
            String sklOid = getValueString(FIELD_SCHOOL_OID);
            return getBeanByOid(broker, ToolSchool.class, sklOid, true);
        }

        /**
         * Gets the school oid.
         *
         * @return String
         */
        public String getSchoolOid() {
            return getValueString(FIELD_SCHOOL_OID);
        }

    }

    /**
     * The Class OnsisStudentSalep.
     */
    public static class OnStudentSalep extends OnStudentProgramParticipation {
        public static final String SAL_ATT_FULL_TIME = "002";

        private static final String ALIAS_PGM_SAL_ATTENDANCE_TYPE = "pgm-sal-AtdType";
        private static final String ALIAS_PGM_SAL_EXIT_TYPE = "pgm-sal-exit-type";
        private static final String ALIAS_PGM_SAL_STATUS = "pgm-sal-status";

        private static final String DDX_ID = "STD-PGM-SAL";

        public static final ToolBeanColumn FIELD_ATTENDANCE_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION, new ToolBeanColumn.AliasDefinition(
                        ALIAS_PGM_SAL_ATTENDANCE_TYPE, OnStudentSalep.DDX_ID, true));
        public static final ToolBeanColumn FIELD_EXIT_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGM_SAL_EXIT_TYPE, OnStudentSalep.DDX_ID, true));
        public static final ToolBeanColumn FIELD_STATUS =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGM_SAL_STATUS, OnStudentSalep.DDX_ID, true));

        // Nonquery Fields
        public static final ToolBeanColumn FIELD_DDX_ID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.extendedDataDictionary().id());

        public static ToolBeanRelationship CHILD_PROGRAM_DETAILS =
                new ToolBeanRelationship(OnStudentSalep.class,
                        OnStudentSalepDetail.class,
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.programDetails().getPath(),
                        SisBeanPaths.STUDENT_PROGRAM_DETAIL.programOid().getPath(),
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.programDetails().getRelationshipType(),
                        false);

        public static ToolBeanRelationship PARENT_STUDENT =
                new ToolBeanRelationship(OnStudentSalep.class,
                        SisBeanPaths.STUDENT.getBeanType(),
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid().getPath(),
                        OnStudent.CHILD_KEY_SALEP_PROGRAM,
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.student().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = OnStudentProgramParticipation.FULL_DEFINITION
                .expand(FIELD_ATTENDANCE_TYPE,
                        FIELD_EXIT_TYPE,
                        FIELD_STATUS)
                .expandRelationships(CHILD_PROGRAM_DETAILS,
                        PARENT_STUDENT)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(FIELD_DDX_ID.resolve(getDictionaryExtractor()), DDX_ID);
                        return criteria;
                    }
                });

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public OnStudentSalep(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the attendance type.
         *
         * @return String
         */
        public String getAttendanceType() {
            return getValueReferenceState(FIELD_ATTENDANCE_TYPE);
        }

        /**
         * Gets the exit type.
         *
         * @return String
         */
        public String getExitType() {
            return getValueReferenceState(FIELD_EXIT_TYPE);
        }

        /**
         * Gets the program details.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<OnStudentSalepDetail> getProgramDetails(X2Broker broker) {
            return (List<OnStudentSalepDetail>) getChildren(broker, CHILD_PROGRAM_DETAILS);
        }

        /**
         * Gets the status.
         *
         * @return String
         */
        public String getStatus() {
            return getValueReferenceState(FIELD_STATUS);
        }
    }

    /**
     * The Class OnsisStudentSalepDetail.
     */
    public static class OnStudentSalepDetail extends ToolBean {
        private static final String ALIAS_PGD_SAL_COMPONENT = "pgd-sal-component";
        private static final String ALIAS_PGD_SAL_COMPONENT_START = "pgd-sal-component-start";
        private static final String ALIAS_PGD_SAL_COMPONENT_END = "pgd-sal-component-end";

        public static final ToolBeanColumn FIELD_COMPONENT =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_DETAIL,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGD_SAL_COMPONENT, OnStudentSalep.DDX_ID, true));
        public static final ToolBeanColumn FIELD_END_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_DETAIL,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGD_SAL_COMPONENT_END, OnStudentSalep.DDX_ID,
                                true));
        public static final ToolBeanColumn FIELD_PROGRAM_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_DETAIL.programOid());
        public static final ToolBeanColumn FIELD_START_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_DETAIL,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGD_SAL_COMPONENT_START, OnStudentSalep.DDX_ID,
                                true));

        public static ToolBeanRelationship PARENT_PROGRAM =
                new ToolBeanRelationship(OnStudentSalepDetail.class,
                        OnStudentSalep.class,
                        SisBeanPaths.STUDENT_PROGRAM_DETAIL.programOid().getPath(),
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.programDetails().getPath(),
                        SisBeanPaths.STUDENT_PROGRAM_DETAIL.program().getRelationshipType());


        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_COMPONENT,
                        FIELD_END_DATE,
                        FIELD_PROGRAM_OID,
                        FIELD_START_DATE)
                .expandRelationships(PARENT_PROGRAM);

        /**
         * Instantiates a new onsis student salep detail.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnStudentSalepDetail(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT_PROGRAM_DETAIL.getBeanType();
        }

        private Range<Date> m_dateRange;

        /**
         * Gets the component.
         *
         * @return String
         */
        public String getComponent() {
            return getValueReferenceState(FIELD_COMPONENT);
        }

        /**
         * Gets the date range.
         *
         * @return Range
         */
        public Range<Date> getDateRange() {
            if (m_dateRange == null) {
                m_dateRange = Range.of(getStartDate(), getEndDate());
            }
            return m_dateRange;
        }

        /**
         * Gets the end date.
         *
         * @return Plain date
         */
        public PlainDate getEndDate() {
            return getValueDate(FIELD_END_DATE);
        }

        /**
         * Gets the start date.
         *
         * @return Plain date
         */
        public PlainDate getStartDate() {
            return getValueDate(FIELD_START_DATE);
        }

    }

    /**
     * The Class OnStudentScheduleSpan.
     */
    public static class OnStudentScheduleSpan extends StudentScheduleSpan {

        /**
         * Instantiates a new On student schedule span.
         *
         * @param section ToolSection
         */
        public OnStudentScheduleSpan(ToolSection section) {
            super(section);
        }


        /**
         * Calculate course start date.
         *
         * @param broker the broker
         * @param reportDateRange the report date range
         * @param enrStartDate the enr start date
         * @return the plain date
         */
        public PlainDate calculateCourseStartDate(X2Broker broker,
                                                  Range<Date> reportDateRange,
                                                  PlainDate enrStartDate) {
            PlainDate spanEntryDate = getEntryDate();
            if (enrStartDate != null && enrStartDate.after(spanEntryDate)) {
                spanEntryDate = enrStartDate;
            }
            if (getSection() != null) {
                PlainDate termStartDate = getSection().getStartDate(broker, reportDateRange);
                if (termStartDate != null && termStartDate.after(spanEntryDate)) {
                    spanEntryDate = termStartDate;
                }
            }

            return spanEntryDate;
        }

        /**
         * Gets the grade term date.
         *
         * @param broker the broker
         * @param submissionDateRange the submission date range
         * @return the grade term date
         */
        public ToolGradeTermDate getGradeTermDate(X2Broker broker, Range<Date> submissionDateRange) {
            ToolTranscript transcript = getTranscript();
            if (transcript == null) {
                return null;
            }
            return transcript.getGradeTermDates(broker).stream()
                    .filter(gta -> gta.getRange()
                            .isOverlap(transcript.getSection(broker).getDateRange(broker))
                            && gta.getRange().isOverlap(this.getDateRange())
                            && gta.getRange().isOverlap(submissionDateRange))
                    .sorted(new Comparator<ToolGradeTermDate>() {
                        @Override
                        public int compare(ToolGradeTermDate date1, ToolGradeTermDate date2) {
                            return date1.getStartDate().compareTo(date2.getStartDate());
                        }
                    })
                    .findFirst().orElse(null);
        }

        /**
         * Checks if is independent study.
         *
         * @return true, if is independent study
         */
        public boolean isIndependentStudy() {
            OnSection section = (OnSection) getSection();
            return section != null && !section.getExcludeFromOnsisIndicator() && section.isIndependentStudy();
        }
    }

    /**
     * A factory for creating OnStudentScheduleSpan objects.
     */
    public static class OnStudentScheduleSpanFactory extends StudentScheduleSpanFactory {

        /**
         * Instantiate span.
         *
         * @param section ToolSection
         * @return StudentScheduleSpan
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpanFactory#instantiateSpan(com.x2dev.procedures.statereporting.common.ToolBean.ToolSection)
         */
        @Override
        public StudentScheduleSpan instantiateSpan(ToolSection section) {
            return new OnStudentScheduleSpan(section);
        }

    }

    /**
     * The Class OnsisStudentSchool.
     */
    public static class OnStudentSchool extends ToolStudentSchool {
        private static final String ALIAS_SSK_ARRIVAL_STATUS = "all-ssk-ArrivalStatus";
        private static final String ALIAS_SSK_BRD_RES_STAT_TYPE = "all-ssk-BoardResidentStatus";
        private static final String ALIAS_SSK_MAIN_SCHOOL_FLAG_OVERRIDE = "all-ssk-MainSchoolFlagOverride";

        public static final ToolBeanColumn FIELD_ARRIVAL_STATUS =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHOOL, ALIAS_SSK_ARRIVAL_STATUS);
        public static final ToolBeanColumn FIELD_BRD_RES_STAT_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHOOL,
                        new ToolBeanColumn.AliasDefinition(ALIAS_SSK_BRD_RES_STAT_TYPE, null, false));
        public static final ToolBeanColumn FIELD_MAIN_SCHOOL_FLAG_OVERRIDE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHOOL,
                        new ToolBeanColumn.AliasDefinition(ALIAS_SSK_MAIN_SCHOOL_FLAG_OVERRIDE, null, false));

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStudentSchool.FULL_DEFINITION
                .expand(FIELD_ARRIVAL_STATUS,
                        FIELD_BRD_RES_STAT_TYPE,
                        FIELD_MAIN_SCHOOL_FLAG_OVERRIDE)
                .expandFilters(new Predicate<ToolBean>() {
                    @Override
                    public boolean test(ToolBean bean) {
                        OnStudentSchool ssk = (OnStudentSchool) bean;
                        String status = ssk.getArrivalStatus();
                        return StringUtils.isEmpty(status) || status.toLowerCase().startsWith("a");
                    }
                });

        /**
         * Instantiates a new onsis student school.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnStudentSchool(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the arrival status.
         *
         * @return String
         */
        public String getArrivalStatus() {
            return this.getValueString(FIELD_ARRIVAL_STATUS);
        }

        /**
         * Gets the arrival status.
         *
         * @return String
         */
        public String getBoardResidenceStatus() {
            return this.getValueString(FIELD_BRD_RES_STAT_TYPE);
        }

        /**
         * Returns the Board Residence Status reference code.
         *
         * @return ReferenceCode
         */
        public ReferenceCode getBoardResidenceStatusReferenceCode() {
            DictionaryExtractor extractor = ToolBean.getDictionaryExtractor();
            DataDictionaryField field = FIELD_BRD_RES_STAT_TYPE.getField(extractor);
            Map<String, ReferenceCode> refCodes = extractor.getReferenceCodes(field.getReferenceTableOid());
            return refCodes.get(getBoardResidenceStatus());
        }

        /**
         * Gets the main school override.
         *
         * @return boolean
         */
        public boolean getMainSchoolOverride() {
            return this.getValueLogical(FIELD_MAIN_SCHOOL_FLAG_OVERRIDE);
        }

    }


    /**
     * The Class OnStudentECPPProgram.
     */
    public static class OnStudentECPPProgram extends OnStudentProgramParticipation {
        public static final String PROGRAM_TYPE_DAY_TREATMENT = "2";
        private static final String ALIAS_PGM_ECPP_NUMBER_OF_DAYS = "pgm-ecpp-number-of-days";
        private static final String ALIAS_PGM_ECPP_REPORT_DAYS_0 = "pgm-ecpp-report-0-days";

        private static final String DDX_ID = "STD-PGM-ECPP";

        public static final ToolBeanColumn FIELD_ECPP_REPORT_DAYS_0 =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGM_ECPP_REPORT_DAYS_0, DDX_ID, true));
        public static final ToolBeanColumn FIELD_NUMBER_OF_DAYS =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGM_ECPP_NUMBER_OF_DAYS, DDX_ID, true));

        // Nonquery Fields
        public static final ToolBeanColumn FIELD_DDX_ID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.extendedDataDictionary().id());

        public static ToolBeanRelationship PARENT_STUDENT =
                new ToolBeanRelationship(OnStudentECPPProgram.class,
                        SisBeanPaths.STUDENT.getBeanType(),
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid().getPath(),
                        OnStudent.CHILD_KEY_ECPP_PROGRAM,
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.student().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = OnStudentProgramParticipation.FULL_DEFINITION
                .expand(FIELD_ECPP_REPORT_DAYS_0,
                        FIELD_NUMBER_OF_DAYS)
                .expandRelationships(PARENT_STUDENT)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(FIELD_DDX_ID.resolve(getDictionaryExtractor()), DDX_ID);
                        return criteria;
                    }
                });

        /**
         * Instantiates a new on student ECPP program.
         *
         * @param columns the columns
         * @param data the data
         */
        public OnStudentECPPProgram(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Checks if is ECPP days zero.
         *
         * @return true, if is ECPP days zero
         */
        public boolean isECPPDaysZero() {
            return getValueLogical(FIELD_ECPP_REPORT_DAYS_0);
        }

        /**
         * Gets the number of days.
         *
         * @return the number of days
         */
        public int getNumberOfDays() {
            return getValueInt(FIELD_NUMBER_OF_DAYS);
        }

    }
    /**
     * The Class OnsisStudentELLProgram.
     */
    public static class OnStudentELLProgram extends OnStudentProgramParticipation {
        private static final String ALIAS_ELL_PROGRAM_CODE = "pgm-ell-program";

        private static final String DDX_ID = "STD-PGM-ELL";

        public static final ToolBeanColumn FIELD_PROGRAM_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION,
                        new ToolBeanColumn.AliasDefinition(ALIAS_ELL_PROGRAM_CODE, DDX_ID, true));

        // Nonquery Fields
        public static final ToolBeanColumn FIELD_DDX_ID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.extendedDataDictionary().id());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = OnStudentProgramParticipation.FULL_DEFINITION
                .expand(FIELD_PROGRAM_CODE)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(FIELD_DDX_ID.resolve(getDictionaryExtractor()), DDX_ID);
                        return criteria;
                    }
                });

        /**
         * Instantiates a new onsis student ELL program.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnStudentELLProgram(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the program code.
         *
         * @return String
         */
        @Override
        public String getProgramCode() {
            return getValueReferenceState(FIELD_PROGRAM_CODE);
        }

    }

    /**
     * The Class OnsisStudentSLPProgram.
     */
    public static class OnStudentSLPProgram extends OnStudentProgramParticipation {
        public static final String TYPE_CORE = "001";
        public static final String TYPE_EXTENDED = "002";
        public static final String TYPE_IMMERSION = "003";
        private static final String ALIAS_PGM_MINUTES_OF_INSTRUCTION = "pgm-slp-minutes-of-instruction";
        private static final String ALIAS_PGM_SECOND_LANGUAGE_PROGRAM_CODE = "pgm-slp-code";
        private static final String ALIAS_PGM_SECOND_LANGUAGE_PROGRAM_DO_NOT_REPORT = "pgm-slp-do-not-report";
        private static final String ALIAS_PGM_SECOND_LANGUAGE_PROGRAM_TYPE = "pgm-slp-type";

        private static final String DDX_ID = "STD-PGM-SLP";

        public static final ToolBeanColumn FIELD_DO_NOT_REPORT =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGM_SECOND_LANGUAGE_PROGRAM_DO_NOT_REPORT, DDX_ID,
                                false));
        public static final ToolBeanColumn FIELD_MINUTES_OF_INSTRUCTION =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGM_MINUTES_OF_INSTRUCTION, DDX_ID, true));
        public static final ToolBeanColumn FIELD_PROGRAM_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGM_SECOND_LANGUAGE_PROGRAM_CODE, DDX_ID, true));
        public static final ToolBeanColumn FIELD_PROGRAM_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGM_SECOND_LANGUAGE_PROGRAM_TYPE, DDX_ID, true));
        public static final ToolBeanColumn FIELD_PROGRAM_CODE_BEAN_VALUE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.programCode());

        // Nonquery Fields
        public static final ToolBeanColumn FIELD_DATE_DESC =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.startDate(),
                        PredefinedConverter.PLAINDATE,
                        false);
        public static final ToolBeanColumn FIELD_DDX_ID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.extendedDataDictionary().id());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = OnStudentProgramParticipation.FULL_DEFINITION
                .expand(FIELD_DO_NOT_REPORT,
                        FIELD_MINUTES_OF_INSTRUCTION,
                        FIELD_PROGRAM_CODE,
                        FIELD_PROGRAM_TYPE,
                        FIELD_PROGRAM_CODE_BEAN_VALUE)
                .expandSort(FIELD_STUDENT_OID, FIELD_DATE_DESC)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        //
                        // Removing this to reverse implementation of S-64036 for now
                        //
                        // String doNotReportField =
                        // FIELD_DO_NOT_REPORT.resolve(getDictionaryExtractor());
                        // if (!StringUtils.isEmpty(doNotReportField)) {
                        // criteria.addNotEqualTo(doNotReportField, BooleanAsStringConverter.TRUE);
                        // }
                        criteria.addEqualTo(FIELD_DDX_ID.resolve(getDictionaryExtractor()), DDX_ID);
                        return criteria;
                    }
                });

        /**
         * Instantiates a new onsis student SLP program.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnStudentSLPProgram(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the minutes of instruction.
         *
         * @return Big decimal
         */
        public BigDecimal getMinutesOfInstruction() {
            return this.getValueBigDecimal(FIELD_MINUTES_OF_INSTRUCTION);
        }

        /**
         * Gets the program code SLP.
         *
         * @return String
         */
        public String getProgramCodeSLP() {
            return getValueString(FIELD_PROGRAM_CODE);
        }

        /**
         * Gets the program type.
         *
         * @return String
         */
        @Override
        public String getProgramCode() {
            return getValueReferenceState(FIELD_PROGRAM_CODE);
        }

        /**
         * Gets the program code.
         *
         * @return String
         */
        public String getProgramCodeBeanValue() {
            return getValueString(FIELD_PROGRAM_CODE_BEAN_VALUE);
        }

        /**
         * Gets the program type.
         *
         * @return the program type
         */
        public String getProgramType() {
            return getValueReferenceState(FIELD_PROGRAM_TYPE);
        }
    }

    /**
     * The Class OnsisStudentSLPProgramFrench.
     */
    public static class OnStudentSLPProgramFrench extends OnStudentProgramParticipation {
        private static final String ALIAS_PGM_SECOND_LANGUAGE_PROGRAM_DO_NOT_REPORT = "pgm-slp-fr-do-not-report";
        private static final String ALIAS_PGM_SECOND_LANGUAGE_PROGRAM_TYPE = "pgm-slp-type";
        private static final String ALIAS_PGM_MINUTES_OF_INSTRUCTION = "pgm-slp-fr-mins-of-instruction";

        private static final String DDX_ID = "STD-PGM-SLP-FR";

        public static final ToolBeanColumn FIELD_DO_NOT_REPORT =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGM_SECOND_LANGUAGE_PROGRAM_DO_NOT_REPORT, DDX_ID,
                                false));
        public static final ToolBeanColumn FIELD_MINUTES_OF_INSTRUCTION =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGM_MINUTES_OF_INSTRUCTION, DDX_ID, false));
        public static final ToolBeanColumn FIELD_PROGRAM_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGM_SECOND_LANGUAGE_PROGRAM_TYPE, DDX_ID, false));

        // Nonquery Fields
        public static final ToolBeanColumn FIELD_DDX_ID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.extendedDataDictionary().id());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = OnStudentProgramParticipation.FULL_DEFINITION
                .expand(FIELD_DO_NOT_REPORT,
                        FIELD_MINUTES_OF_INSTRUCTION,
                        FIELD_PROGRAM_TYPE)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        String doNotReportField = FIELD_DO_NOT_REPORT.resolve(getDictionaryExtractor());
                        if (!StringUtils.isEmpty(doNotReportField)) {
                            criteria.addNotEqualTo(doNotReportField, BooleanAsStringConverter.TRUE);
                        }
                        criteria.addEqualTo(FIELD_DDX_ID.resolve(getDictionaryExtractor()), DDX_ID);
                        return criteria;
                    }
                });

        /**
         * Instantiates a new onsis student SLP program french.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnStudentSLPProgramFrench(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the minutes of instruction.
         *
         * @return Big decimal
         */
        public BigDecimal getMinutesOfInstruction() {
            return this.getValueBigDecimal(FIELD_MINUTES_OF_INSTRUCTION);
        }

        /**
         * Gets the program type.
         *
         * @return String
         */
        public String getProgramType() {
            return getValueReferenceState(FIELD_PROGRAM_TYPE);
        }

    }

    /**
     * The Class OnsisStudentSped.
     */
    public static class OnStudentSped extends OnStudentProgramParticipation {
        public static List<String> NOT_EXCEPTIONAL_CODES = Arrays.asList("NONEXC", "NONIDE");

        private static final String ALIAS_PGM_SPED_EXCEPTIONALITY = "pgm-speced-pri-exceptionality";
        private static final String ALIAS_PGM_SPECED_EXCEPTIONALITY = "pgm-speced-exceptionality";
        private static final String ALIAS_PGM_SPED_REPORT_INDICATOR = "pgm-speced-report-ind";

        private static final String DDX_ID = "STD-PGM-SPECED";

        public static final ToolBeanColumn FIELD_EXCEPTIONALITY =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGM_SPECED_EXCEPTIONALITY, DDX_ID, true));
        public static final ToolBeanColumn FIELD_EXCEPTIONALITY_FLAG =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGM_SPED_EXCEPTIONALITY, DDX_ID, true));
        public static final ToolBeanColumn FIELD_SPED_REPORT_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGM_SPED_REPORT_INDICATOR, DDX_ID, true));

        // Nonquery Fields
        public static final ToolBeanColumn FIELD_DDX_ID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.extendedDataDictionary().id());

        public static ToolBeanRelationship CHILD_PROGRAM_DETAILS =
                new ToolBeanRelationship(OnStudentSped.class,
                        OnStudentSpedDetail.class,
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.programDetails().getPath(),
                        SisBeanPaths.STUDENT_PROGRAM_DETAIL.programOid().getPath(),
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.programDetails().getRelationshipType(),
                        false);

        public static ToolBeanRelationship PARENT_STUDENT =
                new ToolBeanRelationship(OnStudentSped.class,
                        SisBeanPaths.STUDENT.getBeanType(),
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.studentOid().getPath(),
                        OnStudent.CHILD_KEY_SPED_PROGRAM,
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.student().getRelationshipType());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = OnStudentProgramParticipation.FULL_DEFINITION
                .expand(FIELD_EXCEPTIONALITY,
                        FIELD_EXCEPTIONALITY_FLAG,
                        FIELD_SPED_REPORT_INDICATOR)
                .expandRelationships(CHILD_PROGRAM_DETAILS, PARENT_STUDENT)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(FIELD_DDX_ID.resolve(getDictionaryExtractor()), DDX_ID);
                        return criteria;
                    }
                });

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public OnStudentSped(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the exceptionality.
         *
         * @return String
         */
        public String getExceptionality() {
            String exceptionality = getValueReferenceState(FIELD_EXCEPTIONALITY);
            if (NOT_EXCEPTIONAL_CODES.contains(exceptionality)) {
                exceptionality = "";
            }
            return exceptionality;
        }

        /**
         * Gets the exceptionality.
         *
         * @return String
         */
        public String getExceptionalityPlain() {
            return getValueString(FIELD_EXCEPTIONALITY);
        }

        /**
         * Gets the iep flag.
         *
         * @param broker X2Broker
         * @param evaluationDate PlainDate
         * @return Boolean
         */
        public Boolean getIepFlag(X2Broker broker, PlainDate evaluationDate) {
            OnStudentSpedDetail detail = getProgramDetails(broker).stream()
                    .filter(pgd -> OnStudentSpedDetail.TYPE_PLACEMENT.equals(pgd.getType())
                            && pgd.getStartDate() != null
                            && pgd.getDateRange().contains(evaluationDate))
                    .collect(Collectors.maxBy(new Comparator<OnStudentSpedDetail>() {
                        // Latest Start Date
                        @Override
                        public int compare(OnStudentSpedDetail pgd1, OnStudentSpedDetail pgd2) {
                            return pgd1.getStartDate().compareTo(pgd2.getStartDate());
                        }
                    })).orElse(null);
            return detail == null ? null : detail.getIepRequiredIndicator();
        }

        /**
         * Gets the iprc student flag.
         *
         * @param broker X2Broker
         * @param evaluationDate PlainDate
         * @return Boolean
         */
        public Boolean getIprcStudentFlag(X2Broker broker, PlainDate evaluationDate) {
            return Boolean.valueOf(
                    getReviewDate(broker, evaluationDate) == null ? false : !StringUtils.isBlank(getExceptionality()));
        }

        /**
         * Gets the main except flag.
         *
         * @return Boolean
         */
        public Boolean getMainExceptFlag() {
            Boolean value = Boolean.FALSE;
            if (!StringUtils.isEmpty(getExceptionality())) {
                value = getValueLogical(FIELD_EXCEPTIONALITY_FLAG);
            }
            return value;
        }

        /**
         * Gets the main except flag plain value.
         *
         * @return Boolean
         */
        public Boolean getMainExceptFlagPlain() {
            return getValueLogical(FIELD_EXCEPTIONALITY_FLAG);
        }

        /**
         * Gets the program details.
         *
         * @param broker X2Broker
         * @return List
         */
        public List<OnStudentSpedDetail> getProgramDetails(X2Broker broker) {
            return (List<OnStudentSpedDetail>) getChildren(broker, CHILD_PROGRAM_DETAILS);
        }

        /**
         * Gets the review date.
         *
         * @param broker X2Broker
         * @param evaluationDate PlainDate
         * @return Plain date
         */
        public PlainDate getReviewDate(X2Broker broker, PlainDate evaluationDate) {
            PlainDate date = getStudent(broker).getSpedLastEvaluationDate();
            if (date != null && date.after(evaluationDate)) {
                Object value = ((OnStudent) getStudent(broker)).getIprcReviewDateHistory();
                if (value != null) {
                    String valueString =
                            value.getClass().equals(byte[].class) ? new String((byte[]) value) : value.toString();
                    Set<String> dates = StringUtils.isEmpty(valueString) ? new HashSet()
                            : new HashSet(Arrays.asList(valueString.split("\\s*,\\s*")));
                    SystemStringConverter converter = (SystemStringConverter) ConverterFactory
                            .getConverterForClass(PlainDate.class.getName(), Locale.getDefault(), true);
                    Optional<PlainDate> maxDate =
                            dates.stream().map(str -> (PlainDate) converter.parseSystemString(str))
                                    .filter(dt -> !dt.after(evaluationDate)).max(Comparator.naturalOrder());
                    if (maxDate.isPresent()) {
                        date = maxDate.get();
                    }
                }

            }
            return date;
        }

        /**
         * Gets the sped report indicator.
         *
         * @return boolean
         */
        public boolean getSpedReportIndicator() {
            return getValueLogical(FIELD_SPED_REPORT_INDICATOR);
        }

        /**
         * Gets the placement type.
         *
         * @param broker X2Broker
         * @param evaluationDate PlainDate
         * @return String
         */
        public String getPlacementType(X2Broker broker, PlainDate evaluationDate) {
            OnStudentSpedDetail detail = getProgramDetails(broker).stream()
                    .filter(pgd -> OnStudentSpedDetail.TYPE_PLACEMENT.equals(pgd.getType())
                            && pgd.getStartDate() != null
                            && pgd.getDateRange().contains(evaluationDate))
                    .collect(Collectors.maxBy(new Comparator<OnStudentSpedDetail>() {
                        // Latest Start Date
                        @Override
                        public int compare(OnStudentSpedDetail pgd1, OnStudentSpedDetail pgd2) {
                            return pgd1.getStartDate().compareTo(pgd2.getStartDate());
                        }
                    })).orElse(null);
            return detail == null ? null : detail.getPlacementTypeState();
        }
    }

    /**
     * The Class OnsisStudentSpedDetail.
     */
    public static class OnStudentSpedDetail extends ToolBean {
        public static final String TYPE_PLACEMENT = "Placement";

        private static final String ALIAS_PGD_END_DATE = "pgd-speced-end-date";
        private static final String ALIAS_PGD_IEP_REQUIRED = "pgd-speced-iep-required";
        private static final String ALIAS_PGD_SPECED_PLACEMENT_TYPE = "pgd-speced-placement-type";
        private static final String ALIAS_PGD_SPECED_PROGRAM_NAME = "pgd-speced-program-name";
        private static final String ALIAS_PGD_SPECED_TYPE = "pgd-speced-type";
        private static final String ALIAS_PGD_START_DATE = "pgd-speced-start-date";

        public static ToolBeanRelationship PARENT_PROGRAM =
                new ToolBeanRelationship(OnStudentSpedDetail.class,
                        OnStudentSped.class,
                        SisBeanPaths.STUDENT_PROGRAM_DETAIL.programOid().getPath(),
                        SisBeanPaths.STUDENT_PROGRAM_PARTICIPATION.programDetails().getPath(),
                        SisBeanPaths.STUDENT_PROGRAM_DETAIL.program().getRelationshipType());


        public static final ToolBeanColumn FIELD_END_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_DETAIL,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGD_END_DATE, OnStudentSped.DDX_ID, true));
        public static final ToolBeanColumn FIELD_IEP_REQUIRED =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_DETAIL,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGD_IEP_REQUIRED, OnStudentSped.DDX_ID, true));
        public static final ToolBeanColumn FIELD_PROGRAM_OID =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_DETAIL.programOid());
        public static final ToolBeanColumn FIELD_SPECED_PLACEMENT_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_DETAIL,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGD_SPECED_PLACEMENT_TYPE, OnStudentSped.DDX_ID,
                                true));
        public static final ToolBeanColumn FIELD_SPECED_PROGRAM_NAME =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_DETAIL,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGD_SPECED_PROGRAM_NAME, OnStudentSped.DDX_ID,
                                true));
        public static final ToolBeanColumn FIELD_SPECED_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_DETAIL,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGD_SPECED_TYPE, OnStudentSped.DDX_ID, true));
        public static final ToolBeanColumn FIELD_START_DATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_DETAIL,
                        new ToolBeanColumn.AliasDefinition(ALIAS_PGD_START_DATE, OnStudentSped.DDX_ID, true));
        public static final ToolBeanColumn FIELD_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_PROGRAM_DETAIL.type());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_END_DATE,
                        FIELD_IEP_REQUIRED,
                        FIELD_PROGRAM_OID,
                        FIELD_SPECED_PLACEMENT_TYPE,
                        FIELD_SPECED_PROGRAM_NAME,
                        FIELD_SPECED_TYPE,
                        FIELD_START_DATE,
                        FIELD_TYPE)
                .expandRelationships(PARENT_PROGRAM);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.STUDENT_PROGRAM_DETAIL.getBeanType();
        }

        /**
         * Instantiates a new student.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public OnStudentSpedDetail(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        private Range<Date> m_dateRange;

        /**
         * Gets the date range.
         *
         * @return Range
         */
        public Range<Date> getDateRange() {
            if (m_dateRange == null) {
                m_dateRange = Range.of(getStartDate(), getEndDate());
            }
            return m_dateRange;
        }

        /**
         * Gets the end date.
         *
         * @return Plain date
         */
        public PlainDate getEndDate() {
            return getValueDate(FIELD_END_DATE);
        }

        /**
         * Gets the iep required indicator.
         *
         * @return boolean
         */
        public boolean getIepRequiredIndicator() {
            return getValueLogical(FIELD_IEP_REQUIRED);
        }

        /**
         * Gets the placement type.
         *
         * @return String
         */
        public String getPlacementTypeLocal() {
            return getValueReference(FIELD_SPECED_PLACEMENT_TYPE,
                    ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
        }

        /**
         * Gets the placement type.
         *
         * @return String
         */
        public String getPlacementTypeState() {
            return getValueReferenceState(FIELD_SPECED_PLACEMENT_TYPE);
        }

        /**
         * Gets the start date.
         *
         * @return Plain date
         */
        public PlainDate getStartDate() {
            return getValueDate(FIELD_START_DATE);
        }

        /**
         * Gets the type.
         *
         * @return String
         */
        public String getType() {
            return getValueString(FIELD_TYPE);
        }
    }

    /**
     * The Class OnsisTranscript.
     */
    public static class OnTranscript extends ToolTranscript {
        public static final String FLAG_REPEATED = "R";
        public static final String FLAG_WITHDRAWN = "W";
        public static final String PLAR_TYPE_GRADE_CHA_10 = "6";
        public static final String PLAR_TYPE_GRADE_CHA_11_12 = "8";
        public static final String PLAR_TYPE_GRADE_EQU_11_12 = "7";
        public static final String PLAR_TYPE_GRADE_IND_9_10 = "5";

        public static final String VALUE_FRENCH = "French";

        public static final String WITHDRAWAL_TYPE_D = "D";
        public static final String WITHDRAWAL_TYPE_W = "W";

        private static final String ALIAS_CRS_CREDIT_TYPE = "all-crs-CreditType";
        private static final String ALIAS_TRN_BSID_CREDIT_EARNED = "all-trn-BsidCreditEarned";
        private static final String ALIAS_TRN_COMPULSORY_CREDIT_APPLIED = "all-trn-CompulsoryCreditApplied";
        private static final String ALIAS_TRN_COURSE_DELIVERY_TYPE = "all-trn-CourseDeliveryTypeOverride";
        private static final String ALIAS_TRN_COURSE_REPEATED = "all-trn-CourseRepeated";
        private static final String ALIAS_TRN_CREDIT_BY_REQ_DETAIL = "all-trn-CreditByReqDetail";
        private static final String ALIAS_TRN_CREDIT_EXEMPT = "all-trn-CreditExemptFromHighCreditThreshold";
        private static final String ALIAS_TRN_CREDIT_TYPE = "all-trn-CreditType";
        private static final String ALIAS_TRN_DATE_COMPLETED = "all-trn-DateCompleted";
        private static final String ALIAS_TRN_LANGUAGE_OF_INSTRUCTION_OVERRIDE =
                "all-trn-LanguageOfInstructionOverride";
        private static final String ALIAS_TRN_ONLINE_LEARNING_CREDIT = "all-trn-OnlineLearningCredit";
        private static final String ALIAS_TRN_OTHER_COURSE_INFO_TYPE_OVERRIDE =
                "all-trn-OtherCourseInformationTypeOverride";
        public static final String ALIAS_TRN_PLAR_STATUS = "all-trn-PLARStatus";
        public static final String ALIAS_TRN_PLAR_TYPE = "all-trn-PLARType";

        public static final ToolBeanColumn FIELD_BSID_CREDIT_EARNED =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT,
                        new ToolBeanColumn.AliasDefinition(ALIAS_TRN_BSID_CREDIT_EARNED, null, false));
        public static final ToolBeanColumn FIELD_COMPULSORY_CREDIT_APPLIED =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT, ALIAS_TRN_COMPULSORY_CREDIT_APPLIED);
        public static final ToolBeanColumn FIELD_CREDIT_BY_REQUIREMENT_DETAIL =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT, ALIAS_TRN_CREDIT_BY_REQ_DETAIL);
        public static final ToolBeanColumn FIELD_CREDIT_EXEMPT =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT,
                        new ToolBeanColumn.AliasDefinition(ALIAS_TRN_CREDIT_EXEMPT, null, false));
        public static final ToolBeanColumn FIELD_CREDIT_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT,
                        new ToolBeanColumn.AliasDefinition(ALIAS_TRN_CREDIT_TYPE, null, false));
        public static final ToolBeanColumn FIELD_CREDIT_TYPE_CRS =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.schoolCourse().course(), ALIAS_CRS_CREDIT_TYPE);
        public static final ToolBeanColumn FIELD_COURSE_CODE_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.schoolCourse().course(),
                        ALIAS_CRS_COURSE_CODE_TYPE);
        public static final ToolBeanColumn FIELD_COURSE_DELIVERY_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT, ALIAS_TRN_COURSE_DELIVERY_TYPE);
        public static final ToolBeanColumn FIELD_CRS_EXCLUDE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.schoolCourse().course(), ALIAS_CRS_EXCLUDE);
        public static final ToolBeanColumn FIELD_CRS_NUMBER =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.schoolCourse().course().number());
        public static final ToolBeanColumn FIELD_COURSE_OFFERING_TYPE_CSK =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.schoolCourse(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_CSK_COURSE_OFFERING_TYPE, null, false));
        public static final ToolBeanColumn FIELD_COURSE_REPEATED =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT, ALIAS_TRN_COURSE_REPEATED);
        public static final ToolBeanColumn FIELD_DATE_COMPLETED =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT, ALIAS_TRN_DATE_COMPLETED);
        public static final ToolBeanColumn FIELD_LANGUAGE_OF_INSTRUCTION_CRS =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.schoolCourse().course(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_CRS_LANGUAGE_OF_INSTRUCTION, null, false));
        public static final ToolBeanColumn FIELD_LANGUAGE_OF_INSTRUCTION_CSK =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.schoolCourse(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_CSK_LANGUAGE_OF_INSTRUCTION, null, false));
        public static final ToolBeanColumn FIELD_LANGUAGE_PROGRAM_CSK =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.schoolCourse(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_CSK_LANGUAGE_PROGRAM, null, false));
        public static final ToolBeanColumn FIELD_LANGUAGE_OF_INSTRUCTION_OVERRIDE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT, ALIAS_TRN_LANGUAGE_OF_INSTRUCTION_OVERRIDE);
        public static final ToolBeanColumn FIELD_LANGUAGE_PROGRAM_CRS =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.schoolCourse().course(), ALIAS_CRS_LANGUAGE_PROGRAM);
        public static final ToolBeanColumn FIELD_MINISTRY_COURSE_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.schoolCourse().course(),
                        ALIAS_CRS_MINISTRY_COURSE_CODE);
        public static final ToolBeanColumn FIELD_MST_EXCLUDE_FROM_ONSIS =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.masterSchedule(), ALIAS_MST_EXCLUDE_FROM_ONSIS);
        public static final ToolBeanColumn FIELD_ONLINE_LEARNING_CREDIT =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT,
                        new ToolBeanColumn.AliasDefinition(ALIAS_TRN_ONLINE_LEARNING_CREDIT, null, false));
        public static final ToolBeanColumn FIELD_OTHER_COURSE_INFO_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT,
                        new ToolBeanColumn.AliasDefinition(ALIAS_TRN_OTHER_COURSE_INFO_TYPE_OVERRIDE, null, false));
        public static final ToolBeanColumn FIELD_OTHER_COURSE_INFO_TYPE_CRS =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.schoolCourse().course(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_CRS_OTHER_COURSE_INFO, null, false));
        public static final ToolBeanColumn FIELD_OTHER_COURSE_INFO_TYPE_CSK =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.schoolCourse(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_CSK_OTHER_COURSE_INFO, null, false));
        public static final ToolBeanColumn FIELD_PLAR_STATUS =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT, ALIAS_TRN_PLAR_STATUS);
        public static final ToolBeanColumn FIELD_PLAR_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT, ALIAS_TRN_PLAR_TYPE);
        public static final ToolBeanColumn FIELD_SCHOOL_YEAR =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT.districtContext().schoolYear());

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolTranscript.FULL_DEFINITION
                .expand(FIELD_BSID_CREDIT_EARNED,
                        FIELD_CREDIT_BY_REQUIREMENT_DETAIL,
                        FIELD_COMPULSORY_CREDIT_APPLIED,
                        FIELD_COURSE_CODE_TYPE,
                        FIELD_COURSE_DELIVERY_TYPE,
                        FIELD_CRS_EXCLUDE,
                        FIELD_CRS_NUMBER,
                        FIELD_COURSE_OFFERING_TYPE_CSK,
                        FIELD_COURSE_REPEATED,
                        FIELD_CREDIT_EXEMPT,
                        FIELD_CREDIT_TYPE,
                        FIELD_CREDIT_TYPE_CRS,
                        FIELD_LANGUAGE_OF_INSTRUCTION_CSK,
                        FIELD_DATE_COMPLETED,
                        FIELD_LANGUAGE_OF_INSTRUCTION_CRS,
                        FIELD_LANGUAGE_OF_INSTRUCTION_OVERRIDE,
                        FIELD_LANGUAGE_PROGRAM_CRS,
                        FIELD_LANGUAGE_PROGRAM_CSK,
                        FIELD_MINISTRY_COURSE_CODE,
                        FIELD_MST_EXCLUDE_FROM_ONSIS,
                        FIELD_ONLINE_LEARNING_CREDIT,
                        FIELD_OTHER_COURSE_INFO_TYPE,
                        FIELD_OTHER_COURSE_INFO_TYPE_CRS,
                        FIELD_OTHER_COURSE_INFO_TYPE_CSK,
                        FIELD_PLAR_STATUS,
                        FIELD_PLAR_TYPE,
                        FIELD_SCHOOL_YEAR)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addNotEqualTo(FIELD_CRS_EXCLUDE.resolve(getDictionaryExtractor()),
                                BooleanAsStringConverter.TRUE);
                        criteria.addNotEqualTo(FIELD_MST_EXCLUDE_FROM_ONSIS.resolve(getDictionaryExtractor()),
                                BooleanAsStringConverter.TRUE);
                        return criteria;
                    }
                })
                .expandExtraColumnsFunctions(new BiFunction<X2Broker, X2Criteria, List<String>>() {

                    @Override
                    public List<String> apply(X2Broker broker, X2Criteria criteria) {
                        String[] columns =
                                new String[] {SisBeanPaths.STUDENT_TRANSCRIPT.transcriptDefinitionOid().getPath()};
                        Stream<String> dataColumns = Stream.empty();
                        ColumnQuery query = new ColumnQuery(getX2BaseClass(), columns, criteria);
                        query.setDistinct(true);
                        QueryIterator iterator = broker.getReportQueryIteratorByQuery(query);
                        while (iterator.hasNext()) {
                            Object[] row = (Object[]) iterator.next();
                            String gtdOid = (String) row[0];
                            ToolTranscriptDefinition gtd =
                                    ToolBean.getBeanByOid(broker, null, ToolTranscriptDefinition.class, gtdOid, false);
                            if (gtd != null) {
                                Stream<String> additionalColumns =
                                        ToolBean.getBeanByOid(broker, null, ToolTranscriptDefinition.class, gtdOid,
                                                false)
                                                .getTranscriptColumnDefinitions(broker).stream()
                                                .map(gtc -> (OnTranscriptColumnDefinition) gtc)
                                                .filter(gtc -> "IEP".equals(gtc.getReferenceEntryType()))
                                                .map(gtc -> gtc.getJavaName());
                                dataColumns = Stream.concat(dataColumns, additionalColumns);
                            }
                        }
                        return dataColumns.distinct().collect(Collectors.toList());
                    }

                });

        /**
         * Filter by school.
         *
         * @param trn OnsisTranscript
         * @param schoolOids the school oids
         * @param bsid the bsid
         * @param excludedSpecialConditions the excluded special conditions
         * @return true, if successful
         */
        public static boolean filterBySchool(OnTranscript trn,
                                             List<String> schoolOids,
                                             String bsid,
                                             List<String> excludedSpecialConditions) {
            DataDictionaryField field =
                    OnTranscript.FIELD_BSID_CREDIT_EARNED.getField(getDictionaryExtractor());
            if (field == null) {
                if (!schoolOids.contains(trn.getSchoolOid())) {
                    return false;
                }
            } else {
                boolean value = false;
                if (schoolOids.contains(trn.getSchoolOid())
                        && StringUtils.isEmpty(trn.getBsidCreditEarned())) {
                    value = true;
                }
                if (!value) {
                    DataDictionaryField specialConditionField =
                            OnSchool.FIELD_SPECIAL_CONDITION.getField(getDictionaryExtractor());
                    if (bsid.equals(trn.getBsidCreditEarned())) {
                        if (specialConditionField == null) {
                            value = true;
                        } else {
                            if (excludedSpecialConditions.isEmpty()) {
                                value = true;
                            } else {
                                List<String> codes = getDictionaryExtractor()
                                        .getRefCodesWithStateValue(specialConditionField, excludedSpecialConditions)
                                        .stream().map(ReferenceCode::getCode).collect(Collectors.toList());
                                if (!codes.isEmpty()) {
                                    String specialCondition =
                                            ((OnSchool) trn.getSchool(ToolBean.getBroker(true))).getSpecialCondition();
                                    if (StringUtils.isEmpty(specialCondition)) {
                                        value = true;
                                    } else if (!codes.contains(specialCondition)) {
                                        value = true;
                                    }
                                }
                            }
                        }
                    }
                }
                return value;
            }
            return true;
        }

        private Boolean m_isStudentScheduled;

        /**
         * Instantiates a new onsis transcript.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnTranscript(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the alias value.
         *
         * @param alias String
         * @param dictionaryExtractor DictionaryExtractor
         * @return Object
         */
        public Object getAliasValue(String alias, DictionaryExtractor dictionaryExtractor) {
            DataDictionaryField field = dictionaryExtractor.getFieldByAlias(alias, getTranscriptDefinitionOid(), false);
            if (field == null) {
                throw new IllegalStateException("Alias " + alias + " not found for bean " + this.toString());
            }
            return getFieldValueByColumnName(field.getJavaName());
        }

        /**
         * Gets the bsid credit earned.
         *
         * @return String
         */
        public String getBsidCreditEarned() {
            return getValueString(FIELD_BSID_CREDIT_EARNED);
        }

        /**
         * Gets the compulsory credit applied.
         *
         * @return Big decimal
         */
        public BigDecimal getCompulsoryCreditApplied() {
            return getValueBigDecimal(FIELD_COMPULSORY_CREDIT_APPLIED);
        }

        /**
         * Gets the course code type.
         *
         * @return String
         */
        public String getCourseCodeType() {
            return getValueString(FIELD_COURSE_CODE_TYPE);
        }


        /**
         * Gets the state reference course code type.
         *
         * @return String
         */
        public String getCourseCodeTypeState() {
            return this.getValueReferenceState(FIELD_COURSE_CODE_TYPE);
        }

        /**
         * Gets the course delivery type.
         *
         * @return String
         */
        public String getCourseDeliveryType() {
            return getValueReferenceState(FIELD_COURSE_DELIVERY_TYPE);
        }

        /**
         * Gets the course number.
         *
         * @return String
         */
        public String getCourseNumber() {
            return getValueString(FIELD_CRS_NUMBER);
        }

        /**
         * Gets the course offering type.
         *
         * @return String
         */
        public String getCourseOfferingType() {
            return getValueReferenceState(FIELD_COURSE_OFFERING_TYPE_CSK);
        }

        /**
         * Gets the course repeated.
         *
         * @return String
         */
        public String getCourseRepeated() {
            return FIELD_COURSE_REPEATED.getField(ToolBean.getDictionaryExtractor()).hasReferenceTable()
                    ? this.getValueReferenceState(FIELD_COURSE_REPEATED)
                    : this.getValueString(FIELD_COURSE_REPEATED);
        }

        /**
         * Gets the credit by requirement detail.
         *
         * @return String
         */
        public String getCreditByRequirementDetail() {
            return getValueString(FIELD_CREDIT_BY_REQUIREMENT_DETAIL);
        }

        /**
         * Gets the credit exempt.
         *
         * @return the credit exempt
         */
        public BigDecimal getCreditExempt() {
            return getValueBigDecimal(FIELD_CREDIT_EXEMPT);
        }

        /**
         * Gets the credit type.
         *
         * @return String
         */
        public String getCreditType() {
            String value = this.getValueReferenceState(FIELD_CREDIT_TYPE);
            if (StringUtils.isEmpty(value)) {
                value = this.getValueReferenceState(FIELD_CREDIT_TYPE_CRS);
            }
            return value;
        }

        /**
         * Gets the date completed.
         *
         * @return Plain date
         */
        public PlainDate getDateCompleted() {
            return this.getValueDate(FIELD_DATE_COMPLETED);
        }

        /**
         * Gets the institution type.
         *
         * @return String
         */
        public String getInstitutionType() {
            String value = null;
            if (OnSection.COURSE_CODE_TYPE_DCC.equals(getCourseCodeType())) {
                ReferenceCode refCode = getMinistryDefinedCourseReferenceCode();

                /*
                 * S-59233 2021-06-08
                 * Return characters after "-" or "_" of state code for [all-crs-MinistryCourseCode]
                 * e.g. "ADE4T-ALGO" --> "ALGO"
                 */
                if (refCode != null) {
                    String onsisCourseCode = refCode.getCode();
                    if (onsisCourseCode != null) {
                        int underscorePos = onsisCourseCode.lastIndexOf('-');
                        if (underscorePos < 0) {
                            underscorePos = onsisCourseCode.lastIndexOf('_');
                        }

                        if (underscorePos >= 0 && onsisCourseCode.length() > underscorePos + 1) {
                            value = onsisCourseCode.substring(underscorePos + 1);
                        } else {
                            value = onsisCourseCode;
                        }
                    }
                }
            }

            return value;
        }

        /**
         * Gets the language program.
         *
         * @return String
         */
        public String getLanguageProgram() {
            String languageProgram = getValueReferenceState(FIELD_LANGUAGE_PROGRAM_CSK);

            if (StringUtils.isEmpty(languageProgram)) {
                languageProgram = getValueReferenceState(FIELD_LANGUAGE_PROGRAM_CRS);
            }
            return languageProgram;
        }

        /**
         * Gets the locally developed course.
         *
         * @return String
         */
        public String getLocallyDevelopedCourse() {
            String value = null;
            if (OnSection.COURSE_CODE_TYPE_LDC.equals(getCourseCodeType())) {
                value = this.getValueReferenceState(FIELD_MINISTRY_COURSE_CODE);
            }
            return value;
        }

        /**
         * Gets the ministry course code.
         *
         * @return String
         */
        public String getMinistryCourseCode() {
            return this.getValueString(FIELD_MINISTRY_COURSE_CODE);
        }

        /**
         * Gets the ministry course code.
         *
         * @return String
         */
        public String getMinistryDefinedCourse() {
            String value = null;
            if (OnSection.COURSE_CODE_TYPE_MDC.equals(getCourseCodeTypeState())
                    || OnSection.COURSE_CODE_TYPE_DCC.equals(getCourseCodeTypeState())) {
                value = getValueReferenceState(FIELD_MINISTRY_COURSE_CODE);
                if (StringUtils.isEmpty(value)) {
                    value = getValueString(FIELD_MINISTRY_COURSE_CODE);
                }
            } else if (OnSection.COURSE_CODE_TYPE_PLE.equals(getCourseCodeType())) {
                value = getValueString(FIELD_MINISTRY_COURSE_CODE);
            }
            return value;
        }

        /**
         * Gets the ministry course code.
         *
         * @return String
         */
        public ReferenceCode getMinistryDefinedCourseReferenceCode() {
            ReferenceCode code = null;
            if (OnSection.COURSE_CODE_TYPE_MDC.equals(getCourseCodeTypeState())
                    || OnSection.COURSE_CODE_TYPE_DCC.equals(getCourseCodeTypeState())) {
                DictionaryExtractor extractor = ToolBean.getDictionaryExtractor();
                DataDictionaryField field = FIELD_MINISTRY_COURSE_CODE.getField(extractor);
                Map<String, ReferenceCode> refCodes = extractor.getReferenceCodes(field.getReferenceTableOid());
                code = refCodes.get(getValueString(FIELD_MINISTRY_COURSE_CODE));
            }
            return code;
        }

        /**
         * Gets the online learning credit indicator.
         *
         * @return the online learning credit indicator
         */
        public boolean getOnlineLearningCreditIndicator() {
            return getValueLogical(FIELD_ONLINE_LEARNING_CREDIT);
        }

        /**
         * Gets the other course info types.
         *
         * @return List
         */
        public List<String> getOtherCourseInfoTypes() {
            List<String> values = getValuesReferenceState(FIELD_OTHER_COURSE_INFO_TYPE);
            if (values == null || values.isEmpty()) {
                values = getValuesReferenceState(FIELD_OTHER_COURSE_INFO_TYPE_CSK);
            }
            if (values == null || values.isEmpty()) {
                values = getValuesReferenceState(FIELD_OTHER_COURSE_INFO_TYPE_CRS);
            }
            return values;
        }

        /**
         * Gets the plar status.
         *
         * @return String
         */
        public String getPlarStatus() {
            return this.getValueReferenceState(FIELD_PLAR_STATUS);
        }

        /**
         * Gets the plar type.
         *
         * @return String
         */
        public String getPlarType() {
            return this.getValueReferenceState(FIELD_PLAR_TYPE);
        }

        /**
         * Returns the School year.
         * <p>
         * The data dictionary ID for this property is <code>ctxSchoolYear</code>.
         *
         * @return int
         */
        public int getSchoolYear() {
            return this.getValueInt(FIELD_SCHOOL_YEAR);
        }

        /**
         * Checks if is french.
         *
         * @param broker X2Broker
         * @return true, if is french
         */
        public boolean isFrench(X2Broker broker) {
            String value = this.getValueString(FIELD_LANGUAGE_OF_INSTRUCTION_OVERRIDE);
            if (StringUtils.isEmpty(value)) {
                OnSection section = (OnSection) this.getSection(broker);
                if (section != null) {
                    return section.isFrench();
                }

            }
            if (StringUtils.isEmpty(value)) {
                value = this.getValueString(FIELD_LANGUAGE_OF_INSTRUCTION_CSK);
            }
            if (StringUtils.isEmpty(value)) {
                value = this.getValueString(FIELD_LANGUAGE_OF_INSTRUCTION_CRS);
            }
            return VALUE_FRENCH.equals(value);

        }

        /**
         * Checks if is student scheduled.
         *
         * @param broker X2Broker
         * @param dateRange the date range
         * @return true, if is student scheduled
         */
        public boolean isStudentScheduled(X2Broker broker, Range<Date> dateRange) {
            if (m_isStudentScheduled == null) {
                m_isStudentScheduled = Boolean.FALSE;
                if (!StringUtils.isEmpty(getSectionOid()) && !StringUtils.isEmpty(getStudentOid())) {
                    OnStudent student = (OnStudent) getStudent(broker);
                    if (student.getStudentSchedules(broker).stream()
                            .anyMatch(ssc -> ssc.getSectionOid().equals(getSectionOid()))) {
                        m_isStudentScheduled = Boolean.TRUE;
                    }
                    if (!m_isStudentScheduled.booleanValue()) {
                        if (student.getStudentScheduleChanges(broker).stream()
                                .anyMatch(scc -> scc.getSectionOid().equals(getSectionOid())
                                        && scc.getEffectiveDate() != null
                                        && dateRange.contains(scc.getEffectiveDate()))) {
                            m_isStudentScheduled = Boolean.TRUE;
                        }
                    }
                }
            }
            return m_isStudentScheduled.booleanValue();
        }
    }

    /**
     * The Class OnsisTranscriptColumnDefinition.
     */
    public static class OnTranscriptColumnDefinition extends ToolTranscriptColumnDefinition {
        private static final String ALIAS_GTC_REFERENCE_ENTRY_TYPE = "all-gtc-ReferenceEntryType";

        public static final ToolBeanColumn FIELD_REFERENCE_ENTRY_TYPE =
                new ToolBeanColumn(SisBeanPaths.GRADE_TRANS_COLUMN_DEFINITION, ALIAS_GTC_REFERENCE_ENTRY_TYPE);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolTranscriptColumnDefinition.FULL_DEFINITION
                .expand(FIELD_REFERENCE_ENTRY_TYPE);

        /**
         * Instantiates a new onsis staff.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OnTranscriptColumnDefinition(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the reference entry type.
         *
         * @return String
         */
        public String getReferenceEntryType() {
            return getValueString(FIELD_REFERENCE_ENTRY_TYPE);
        }
    }

    /**
     * The Class OnsisTranscriptRubric.
     */
    public static class OnTranscriptRubric extends ToolTranscriptRubric {
        public static final ToolBeanColumn FIELD_CRS_EXCLUDE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT_RUBRIC.transcript().schoolCourse().course(),
                        ALIAS_CRS_EXCLUDE);
        public static final ToolBeanColumn FIELD_MST_EXCLUDE_FROM_ONSIS =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT_RUBRIC.transcript().masterSchedule(),
                        ALIAS_MST_EXCLUDE_FROM_ONSIS);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolTranscriptRubric.FULL_DEFINITION
                .expand(FIELD_CRS_EXCLUDE,
                        FIELD_MST_EXCLUDE_FROM_ONSIS)
                .expandJoinAdjusters(
                        new JoinAdjusterPattern(JoinType.LEFT_OUTER, SisBeanPaths.SCHEDULE_MASTER.getDatabaseName()))
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        DistrictSchoolYearContext context =
                                (DistrictSchoolYearContext) ToolBean.getPreference(ToolBean.PREFERENCE_CURRENT_CONTEXT);
                        if (context == null) {
                            throw new IllegalStateException("ToolBean preference value "
                                    + ToolBean.PREFERENCE_CURRENT_CONTEXT + " must be set.");
                        }
                        criteria.addEqualTo(ToolTranscriptRubric.FIELD_DISTRICT_CONTEXT_OID.resolve(null),
                                context.getOid());
                        criteria.addEqualTo(ToolTranscriptRubric.FIELD_GTC_REPORT_TYPE.resolve(null),
                                TranscriptColumnDefinition.GRADE_TYPE_TERM);
                        criteria.addNotEqualTo(FIELD_CRS_EXCLUDE.resolve(getDictionaryExtractor()),
                                BooleanAsStringConverter.TRUE);
                        criteria.addNotEqualTo(FIELD_MST_EXCLUDE_FROM_ONSIS.resolve(getDictionaryExtractor()),
                                BooleanAsStringConverter.TRUE);
                        return criteria;
                    }
                });

        /**
         * Instantiates a new onsis transcript rubric.
         *
         * @param columns the columns
         * @param data the data
         */
        public OnTranscriptRubric(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }
    }

    /**
     * The Class OssltAssessment.
     */
    public static class OssltAssessment extends OnStudentAssessment {
        public static final Set<String> PRELOAD_SET = new HashSet();

        private static final String ALIAS_ASM_OSSLT_LANGUAGE = "asm-osslt-language";
        private static final String ALIAS_ASM_OSSLT_OUTCOME = "asm-osslt-outcome";

        private static final String ASD_ID = "OSSLT";

        public static final ToolBeanColumn FIELD_OSSLT_LANGUAGE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT, ALIAS_ASM_OSSLT_LANGUAGE, ASD_ID);
        public static final ToolBeanColumn FIELD_OSSLT_OUTCOME =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT, ALIAS_ASM_OSSLT_OUTCOME, ASD_ID);
        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = OnStudentAssessment.FULL_DEFINITION
                .expand(FIELD_OSSLT_LANGUAGE,
                        FIELD_OSSLT_OUTCOME)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(FIELD_ASSESSMENT_ID.resolve(getDictionaryExtractor()), ASD_ID);
                        return criteria;
                    }
                }).expandFilters(new Predicate<ToolBean>() {
                    // Limit to first iccurence per student
                    @Override
                    public boolean test(ToolBean bean) {
                        OssltAssessment asm = (OssltAssessment) bean;
                        if (PRELOAD_SET.contains(asm.getStudentOid())) {
                            return false;
                        }
                        PRELOAD_SET.add(asm.getStudentOid());
                        return true;
                    }
                });

        /**
         * Instantiates a new osslt assessment.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public OssltAssessment(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the osslt language.
         *
         * @return String
         */
        public String getLanguage() {
            return getValueString(FIELD_OSSLT_LANGUAGE);
        }

        /**
         * Gets the osslt outcome.
         *
         * @return String
         */
        public String getOutcome() {
            return getValueString(FIELD_OSSLT_OUTCOME);
        }
    }

    /**
     * The Class ShsmAssessment.
     */
    public static class ShsmAssessment extends OnStudentAssessment {
        private static final String ALIAS_ASM_SHSM_ATTAINED = "asm-shsm-attained";
        private static final String ALIAS_ASM_SHSM_CERTIFICATE = "asm-shsm-certificate";
        private static final String ALIAS_ASM_SHSM_HOURS = "asm-shsm-hours";

        private static final String ASD_ID = "SHSM";

        public static final ToolBeanColumn FIELD_ATTAINED =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT,
                        new ToolBeanColumn.AliasDefinition(ALIAS_ASM_SHSM_ATTAINED, ASD_ID, true));
        public static final ToolBeanColumn FIELD_CERTIFICATE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT,
                        new ToolBeanColumn.AliasDefinition(ALIAS_ASM_SHSM_CERTIFICATE, ASD_ID, true));
        public static final ToolBeanColumn FIELD_HOURS =
                new ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT,
                        new ToolBeanColumn.AliasDefinition(ALIAS_ASM_SHSM_HOURS, ASD_ID, true));

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = OnStudentAssessment.FULL_DEFINITION
                .expand(FIELD_ATTAINED,
                        FIELD_CERTIFICATE,
                        FIELD_HOURS)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {
                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        criteria.addEqualTo(FIELD_ASSESSMENT_ID.resolve(getDictionaryExtractor()), ASD_ID);
                        return criteria;
                    }
                });

        /**
         * Instantiates a new shsm assessment.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public ShsmAssessment(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the attained indicator.
         *
         * @return boolean
         */
        public boolean getAttainedIndicator() {
            return getValueLogical(FIELD_ATTAINED);
        }

        /**
         * Gets the certificate.
         *
         * @return String
         */
        public String getCertificate() {
            return getValueReferenceState(FIELD_CERTIFICATE);
        }

        /**
         * Gets the hours.
         *
         * @return Big decimal
         */
        public BigDecimal getHours() {
            return getValueBigDecimal(FIELD_HOURS);
        }

    }

    /**
     * The Class SubmissionType.
     */
    public static class SubmissionType extends ToolBean {
        private static final String ALIAS_BATCH_TYPE = "batch-type";
        private static final String ALIAS_CLEAR_FIELDS = "clear-fields";
        private static final String ALIAS_FIELDS = "fields";
        private static final String ALIAS_PERIOD_END_DATE = "period-end-date";
        private static final String ALIAS_PERIOD_START_DATE = "period-start-date";
        private static final String ALIAS_SUBMISSION_PERIOD_CODE = "submission-period-code";
        private static final String ALIAS_SUBMISSION_TYPE = "submission-type";
        private static final String ALIAS_VALIDATE_ADD_UPDATE = "validate-add-update";

        private static final String ALIAS_QUADMESTER_A_DAY_16 = "quadmester-a-day-16";
        private static final String ALIAS_QUADMESTER_B_DAY_16 = "quadmester-b-day-16";
        private static final String ALIAS_OCTOMESTER_A_DAY_16 = "octomester-a-day-16";
        private static final String ALIAS_OCTOMESTER_B_DAY_16 = "octomester-b-day-16";
        private static final String ALIAS_OCTOMESTER_C_DAY_16 = "octomester-c-day-16";
        private static final String ALIAS_OCTOMESTER_D_DAY_16 = "octomester-d-day-16";

        public static final String DDX_ID = "ON-SIS-SUB-FLD";

        public static final List<String> SUBMISSION_SCHOOL_TYPE_CONTINUING_EDUCATION = Arrays.asList("JUNNS");
        public static final List<String> SUBMISSION_SCHOOL_TYPE_CTCC_SECONDARY = Arrays.asList("CTCCSEC");
        public static final List<String> SUBMISSION_SCHOOL_TYPE_ECPP = Arrays.asList("ECPP");
        public static final List<String> SUBMISSION_SCHOOL_TYPE_MARCH =
                Arrays.asList("MARELEM2", "MARELEM3", "MARSEC1", "MARSEC2");
        public static final List<String> SUBMISSION_SCHOOL_TYPE_JUNE =
                Arrays.asList("JUNELEM3", "JUNELEM4", "JUNELEM5", "JUNSEC1", "JUNSECSUS");
        public static final List<String> SUBMISSION_SCHOOL_TYPE_JUNE_SECONDARY = Arrays.asList("JUNSEC1");
        public static final List<String> SUBMISSION_SCHOOL_TYPE_PUBLIC_ELEMENTARY =
                Arrays.asList("JUNELEM3", "JUNELEM4", "JUNELEM5", "JUNELEMSUS", "MARELEM2", "MARELEM3", "OCTELEM2",
                        "OCTELEM3");
        public static final List<String> SUBMISSION_SCHOOL_TYPE_PUBLIC_SECONDARY =
                Arrays.asList("JUNSEC1", "JUNSECSUS", "MARSEC1", "MARSEC2", "OCTSEC1");
        public static final List<String> SUBMISSION_SCHOOL_TYPE_PUBLIC_SECONDARY_MARCH =
                Arrays.asList("MARSEC1", "MARSEC2");
        public static final List<String> SUBMISSION_SCHOOL_TYPE_PUBLIC_SECONDARY_OCTOBER =
                Arrays.asList("OCTSEC1");
        public static final List<String> SUBMISSION_SCHOOL_TYPE_SUMMER_SCHOOL = Arrays.asList("SUMSEC");

        public static final ToolBeanColumn FIELD_BATCH_TYPE =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_A, ALIAS_BATCH_TYPE, DDX_ID);
        public static final ToolBeanColumn FIELD_CLEAR_FIELDS =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_A, ALIAS_CLEAR_FIELDS, DDX_ID);
        public static final ToolBeanColumn FIELD_EXTENDED_DATA_DICTIONARY_OID =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_A.extendedDataDictionaryOid());
        public static final ToolBeanColumn FIELD_FIELDS =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_A, ALIAS_FIELDS, DDX_ID);
        public static final ToolBeanColumn FIELD_OCTOMESTER_A_DAY_16 =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_A, ALIAS_OCTOMESTER_A_DAY_16, DDX_ID);
        public static final ToolBeanColumn FIELD_OCTOMESTER_B_DAY_16 =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_A, ALIAS_OCTOMESTER_B_DAY_16, DDX_ID);
        public static final ToolBeanColumn FIELD_OCTOMESTER_C_DAY_16 =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_A, ALIAS_OCTOMESTER_C_DAY_16, DDX_ID);
        public static final ToolBeanColumn FIELD_OCTOMESTER_D_DAY_16 =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_A, ALIAS_OCTOMESTER_D_DAY_16, DDX_ID);
        public static final ToolBeanColumn FIELD_PERIOD_END_DATE =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_A, ALIAS_PERIOD_END_DATE, DDX_ID);
        public static final ToolBeanColumn FIELD_PERIOD_START_DATE =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_A, ALIAS_PERIOD_START_DATE, DDX_ID);
        public static final ToolBeanColumn FIELD_QUADMESTER_A_DAY_16 =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_A, ALIAS_QUADMESTER_A_DAY_16, DDX_ID);
        public static final ToolBeanColumn FIELD_QUADMESTER_B_DAY_16 =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_A, ALIAS_QUADMESTER_B_DAY_16, DDX_ID);
        public static final ToolBeanColumn FIELD_SUBMISSION_PERIOD_CODE =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_A, ALIAS_SUBMISSION_PERIOD_CODE, DDX_ID);
        public static final ToolBeanColumn FIELD_SUBMISSION_TYPE =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_A, ALIAS_SUBMISSION_TYPE, DDX_ID);
        public static final ToolBeanColumn FIELD_VALIDATE_ADD_UPDATE =
                new ToolBeanColumn(SisBeanPaths.USER_DEFINED_TABLE_A, ALIAS_VALIDATE_ADD_UPDATE, DDX_ID);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolBean.FULL_DEFINITION
                .expand(FIELD_BATCH_TYPE,
                        FIELD_CLEAR_FIELDS,
                        FIELD_EXTENDED_DATA_DICTIONARY_OID,
                        FIELD_FIELDS,
                        FIELD_OCTOMESTER_A_DAY_16,
                        FIELD_OCTOMESTER_B_DAY_16,
                        FIELD_OCTOMESTER_C_DAY_16,
                        FIELD_OCTOMESTER_D_DAY_16,
                        FIELD_PERIOD_END_DATE,
                        FIELD_PERIOD_START_DATE,
                        FIELD_QUADMESTER_A_DAY_16,
                        FIELD_QUADMESTER_B_DAY_16,
                        FIELD_SUBMISSION_PERIOD_CODE,
                        FIELD_SUBMISSION_TYPE,
                        FIELD_VALIDATE_ADD_UPDATE)
                .expandCriteriaFunctions(new BiFunction<X2Broker, X2Criteria, X2Criteria>() {

                    @Override
                    public X2Criteria apply(X2Broker broker, X2Criteria criteria) {
                        DictionaryExtractor extractor = ToolBean.getDictionaryExtractor();
                        DataDictionary dictionary = extractor.getDictionary(DDX_ID);
                        String dictionaryOid =
                                dictionary == null ? "__NO_MATCH__" : dictionary.getExtendedDictionaryOid();
                        criteria.addEqualTo(FIELD_EXTENDED_DATA_DICTIONARY_OID.resolve(extractor), dictionaryOid);
                        return criteria;
                    }
                });

        private static List<ToolBeanColumn> s_quadmester_columns =
                Arrays.asList(FIELD_QUADMESTER_A_DAY_16, FIELD_QUADMESTER_B_DAY_16);
        private static List<ToolBeanColumn> s_octomester_columns =
                Arrays.asList(FIELD_OCTOMESTER_A_DAY_16, FIELD_OCTOMESTER_B_DAY_16,
                        FIELD_OCTOMESTER_C_DAY_16, FIELD_OCTOMESTER_D_DAY_16);

        /**
         * Gets the X2 base class.
         *
         * @return Class
         */
        public static final Class getX2BaseClass() {
            return SisBeanPaths.USER_DEFINED_TABLE_A.getBeanType();
        }

        /**
         * Instantiates a new fte monthly.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public SubmissionType(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        private List<PlainDate> m_countDates;
        private Range<Date> m_dateRange;

        /**
         * Do validate sea add update.
         *
         * @return true, if successful
         */
        public boolean doValidateSeaAddUpdate() {
            return getValueLogical(FIELD_VALIDATE_ADD_UPDATE);
        }

        /**
         * Gets the batch type.
         *
         * @return String
         */
        public String getBatchType() {
            return getValueString(FIELD_BATCH_TYPE);
        }

        /**
         * Gets the clear fields.
         *
         * @return List
         */
        public List<String> getClearFields() {
            String clearFields = getValueString(FIELD_CLEAR_FIELDS);
            return parseMultiCheckboxValue(clearFields);
        }

        /**
         * Gets the count date.
         *
         * @return Plain date
         */
        public PlainDate getCountDate() {
            return getPeriodEndDate();
        }

        /**
         * Gets the count dates.
         *
         * @return List
         */
        public List<PlainDate> getCountDates() {
            if (m_countDates == null) {
                Collection<PlainDate> countDates = new ArrayList<>();

                countDates.addAll(getQuadmesterDates());
                countDates.addAll(getOctomesterDates());

                if (countDates.isEmpty()) {
                    countDates.add(getPeriodEndDate());
                }

                m_countDates = countDates.stream().filter(Objects::nonNull).distinct().collect(Collectors.toList());
            }
            return m_countDates;
        }

        /**
         * Gets the count dates.
         *
         * @param scheduleMode the schedule mode
         * @return the count dates
         */
        public Collection<PlainDate> getCountDates(int scheduleMode) {
            if (!isJuneSubmission()) {
                if (scheduleMode == 4) {
                    return getQuadmesterDates();
                } else if (scheduleMode == 8) {
                    return getOctomesterDates();
                }
            } else {
                return Arrays.asList(getCountDate());
            }

            return Collections.EMPTY_LIST;
        }

        /**
         * Gets the date range.
         *
         * @return the date range
         */
        public Range<Date> getDateRange() {
            if (m_dateRange == null) {
                Collection<PlainDate> countDates = getCountDates();
                if (countDates.size() < 2) {
                    countDates.add(getPeriodStartDate());
                    countDates.add(getPeriodEndDate());
                }

                m_dateRange = Range.of(countDates.stream().min(Date::compareTo).get(),
                        countDates.stream().max(Date::compareTo).get());
            }
            return m_dateRange;
        }

        /**
         * Gets the excluded special condition codes.
         *
         * @return the excluded special condition codes
         */
        public List<String> getExcludedSpecialConditionCodes() {
            if (isECPPSubmission()) {
                return Collections.EMPTY_LIST;
            }
            return OnSchool.SPECIAL_CONDITION_EXCLUDE_FROM_TRANSCRIPT;
        }

        /**
         * Gets the fields repository.
         *
         * @return String[]
         */
        public String getFields() {
            return getValueString(FIELD_FIELDS);
        }

        /**
         * Gets the name.
         *
         * @return String
         */
        public String getName() {
            return getValueString(FIELD_SUBMISSION_TYPE);
        }

        /**
         * Gets the octomester dates.
         *
         * @return Collection
         */
        public Collection<PlainDate> getOctomesterDates() {
            return s_octomester_columns.stream()
                    .map(column -> getValueDate(column))
                    .filter(Objects::nonNull)
                    .collect(Collectors.toSet());
        }

        /**
         * Gets the quadmester dates.
         *
         * @return Collection
         */
        public Collection<PlainDate> getQuadmesterDates() {
            return s_quadmester_columns.stream()
                    .map(column -> getValueDate(column))
                    .filter(Objects::nonNull)
                    .collect(Collectors.toSet());
        }

        /**
         * Gets the period description.
         *
         * @return String
         */
        public String getPeriodDescription() {
            ReferenceCode code = getDictionaryExtractor().getRefCodeByAlias(this, FIELD_SUBMISSION_PERIOD_CODE);
            return code == null ? null : code.getDescription();
        }

        /**
         * Gets the period end date.
         *
         * @return Plain date
         */
        public PlainDate getPeriodEndDate() {
            return getValueDate(FIELD_PERIOD_END_DATE);
        }

        /**
         * Gets the period start date.
         *
         * @return Plain date
         */
        public PlainDate getPeriodStartDate() {
            return getValueDate(FIELD_PERIOD_START_DATE);
        }

        /**
         * Gets the submission period code.
         *
         * @return String
         */
        public String getSubmissionPeriodCode() {
            return getValueString(FIELD_SUBMISSION_PERIOD_CODE);
        }

        /**
         * Checks if is continuing education submission.
         *
         * @return true, if is continuing education submission
         */
        public boolean isContinuingEducationSubmission() {
            return SUBMISSION_SCHOOL_TYPE_CONTINUING_EDUCATION.contains(getSubmissionPeriodCode());
        }

        /**
         * Checks if is ECPP submission.
         *
         * @return true, if is ECPP submission
         */
        public boolean isECPPSubmission() {
            return SUBMISSION_SCHOOL_TYPE_ECPP.contains(getSubmissionPeriodCode());
        }

        /**
         * Checks if is elementary submission.
         *
         * @return true, if is elementary submission
         */
        public boolean isElementarySubmission() {
            return SUBMISSION_SCHOOL_TYPE_PUBLIC_ELEMENTARY.contains(getSubmissionPeriodCode());
        }

        /**
         * Checks if is june submission.
         *
         * @return true, if is march submission
         */
        public boolean isMarchSubmission() {
            return SUBMISSION_SCHOOL_TYPE_MARCH.contains(getSubmissionPeriodCode());
        }

        /**
         * Checks if is june submission.
         *
         * @return true, if is june submission
         */
        public boolean isJuneSubmission() {
            return SUBMISSION_SCHOOL_TYPE_JUNE.contains(getSubmissionPeriodCode());
        }

        /**
         * Checks if is october submission.
         *
         * @return true, if is june submission
         */
        public boolean isOctoberSubmission() {
            return SUBMISSION_SCHOOL_TYPE_OCTOBER.contains(getSubmissionPeriodCode());
        }

        /**
         * Checks if is secondary submission.
         *
         * @return true, if is secondary submission
         */
        public boolean isSecondarySubmission() {
            return SUBMISSION_SCHOOL_TYPE_PUBLIC_SECONDARY.contains(getSubmissionPeriodCode());
        }

        /**
         * Checks if is summer submission.
         *
         * @return true, if is summer submission
         */
        public boolean isSummerSubmission() {
            return SUBMISSION_SCHOOL_TYPE_SUMMER_SCHOOL.contains(getSubmissionPeriodCode());
        }

        /**
         * Parses the multi checkbox value.
         *
         * @param value String
         * @return List
         */
        private static List<String> parseMultiCheckboxValue(String value) {
            if (StringUtils.isEmpty(value)) {
                return Collections.EMPTY_LIST;
            }
            String[] valuesArray = value.split(",");
            ArrayList<String> valuesList = new ArrayList<>();
            for (String valueFromArray : valuesArray) {
                valuesList.add(valueFromArray.trim());
            }
            return valuesList;
        }
    }

    static final String ALIAS_BEL_DEFAULT_BELL_TIME = "all-bel-DefaultFteBellTime";
    static final String ALIAS_BEL_END_DATE = "all-bel-EndDate";
    static final String ALIAS_BEL_START_DATE = "all-bel-StartDate";
    static final String ALIAS_CRS_COURSE_CODE_TYPE = "all-crs-CourseCodeType";
    static final String ALIAS_CRS_ELEMENTARY_SUBJECT_TYPE = "all-crs-ElementarySubjectType";
    static final String ALIAS_CRS_EXCLUDE = "all-crs-ExcludeFromReporting";
    static final String ALIAS_CRS_LANGUAGE_PROGRAM = "all-crs-LanguageProgram";
    static final String ALIAS_CRS_MINISTRY_COURSE_CODE = "all-crs-MinistryCourseCode";
    static final String ALIAS_CRS_OTHER_COURSE_INFO = "all-crs-OtherCourseInformationType";
    static final String ALIAS_CSK_COURSE_OFFERING_TYPE = "all-csk-CourseOfferingType";
    static final String ALIAS_CSK_LANGUAGE_OF_INSTRUCTION = "all-csk-LanguageOfInstruction";
    static final String ALIAS_CSK_LANGUAGE_PROGRAM = "all-csk-LanguageProgram";
    static final String ALIAS_CSK_OTHER_COURSE_INFO = "all-csk-OtherCourseInformationType";
    static final String ALIAS_CRS_LANGUAGE_OF_INSTRUCTION = "all-crs-LanguageOfInstruction";
    static final String ALIAS_MST_EXCLUDE_FROM_ONSIS = "all-mst-ExcludeFromOnSIS";
    static final String ALIAS_GCD_INCLUDE_ON_CONED_REGISTER = "all-gcd-IncludeonConEdRegister";
    static final String ALIAS_PSN_GENDER_SPECIFY = "all-psn-GenderSpecify";
    static final String ALIAS_PSN_LEGAL_FIRST_NAME = "all-psn-LegalFirstName";
    static final String ALIAS_PSN_LEGAL_LAST_NAME = "all-psn-LegalLastName";
    static final String ALIAS_PSN_LEGAL_MIDDLE_NAME = "all-psn-LegalMiddleName";
    static final String ALIAS_SKL_LANGUAGE_TYPE = "all-skl-LanguageType";
    static final String ALIAS_STF_MEN = "all-stf-MinistryEducatorNumber";

    static final String EMPTY_STRING = "";
    static final String PATTERN_DASH = "[-]";

    /**
     * Calculates the age based on the given date of birth on the given date.
     *
     * @param dateOfBirth PlainDate
     * @param calendarDate PlainDate
     * @return int
     */
    public static int getAgeAsOfDate(PlainDate dateOfBirth, PlainDate calendarDate) {
        return getAgeAsOfDateWithMonths(dateOfBirth, calendarDate)[0];
    }

    /**
     * Calculates the age based on the given date of birth on the given date.
     *
     * @param dateOfBirth if null then 0 is returned
     * @param calendarDate PlainDate
     * @return int
     */
    public static int[] getAgeAsOfDateWithMonths(PlainDate dateOfBirth, PlainDate calendarDate) {
        int years = 0;
        int months = 0;

        if (dateOfBirth != null) {
            Calendar calendar = Calendar.getInstance();

            calendar.setTime(dateOfBirth);
            int birthYear = calendar.get(Calendar.YEAR);
            int birthMonth = calendar.get(Calendar.MONTH);
            int birthDay = calendar.get(Calendar.DAY_OF_MONTH);

            calendar.setTime(calendarDate);
            int currentYear = calendar.get(Calendar.YEAR);
            int currentMonth = calendar.get(Calendar.MONTH);
            int currentDay = calendar.get(Calendar.DAY_OF_MONTH);

            if (currentMonth < birthMonth) {
                months = 12 - (birthMonth - currentMonth);
            } else {
                months = currentMonth - birthMonth;
            }

            if (currentDay < birthDay) {
                if (months > 0) {
                    months--;
                } else {
                    months = 11;
                }
            }

            years = currentYear - birthYear;
            if (currentMonth < birthMonth || (currentMonth == birthMonth && currentDay < birthDay)) {
                years--;
            }
        }

        return new int[] {years, months};
    }


}
