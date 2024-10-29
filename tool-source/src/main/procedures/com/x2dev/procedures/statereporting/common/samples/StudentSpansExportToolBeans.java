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
package com.x2dev.procedures.statereporting.common.samples;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.DistrictManager;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanColumn;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolEnrollment;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchool;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpanFactory;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.Criteria;

/**
 * The Class StudentSpansExport.
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class StudentSpansExportToolBeans extends ExportJavaSource {

    /**
     * The Class MyEnrollment.
     */
    public static class MyEnrollment extends ToolEnrollment {
        // Query Fields
        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolEnrollment.FULL_DEFINITION;

        /**
         * Instantiates a new my enrollment.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public MyEnrollment(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

    }

    /**
     * The Class MyEnrollment.
     */
    public static class MyStudent extends ToolStudent {
        // Query Fields
        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStudent.FULL_DEFINITION;

        /**
         * Instantiates a new my enrollment.
         *
         * @param columns RptBeanColumns
         * @param data Object[]
         */
        public MyStudent(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }
    }

    /**
     * Name for the "active only" report parameter. The value is an Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Name for the "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    private static final String FIELD_ENRSPAN_ACTIVE_ENR_CODE = "enrSpanActiveEnrCode";
    private static final String FIELD_ENRSPAN_ACTIVE_ENR_DATE = "enrSpanActiveEnrDate";
    private static final String FIELD_ENRSPAN_ACTIVE_ENR_TYPE = "enrSpanActiveEnrType";
    private static final String FIELD_ENRSPAN_ACTIVE_ENR_YOG = "enrSpanActiveEnrYog";
    private static final String FIELD_ENRSPAN_FIRST_ACTIVE_DATE = "enrSpanFirstActiveDate";
    private static final String FIELD_ENRSPAN_INACTIVE_ENR_CODE = "enrSpanInactiveEnrCode";
    private static final String FIELD_ENRSPAN_INACTIVE_ENR_DATE = "enrSpanInactiveEnrDate";
    private static final String FIELD_ENRSPAN_INACTIVE_ENR_TYPE = "enrSpanInactiveEnrType";
    private static final String FIELD_ENRSPAN_INACTIVE_ENR_YOG = "enrSpanInactiveEnrYog";
    private static final String FIELD_ENRSPAN_LAST_ACTIVE_DATE = "enrSpanLastActiveDate";
    private static final String FIELD_ENRSPAN_SCHOOL = "enrSpanSchool";
    private static final String FIELD_ENRSPAN_SECONDARY_SCHOOL = "enrSecondarySchool";
    private static final String FIELD_SKL_NAME = "schoolName";
    private static final String FIELD_STD_NAME = "studentName";

    private SisStudent m_currentStudent;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {

        ToolBean.registerClass(MyStudent.class);
        ToolBean.registerClass(MyEnrollment.class);
        DataGrid grid = new DataGrid();
        X2Broker broker = getBroker();
        broker.beginSession();

        try {
            DictionaryExtractor dictionaryExtractor = new DictionaryExtractor(broker);
            initializeHelpers();

            loadStudents(broker, dictionaryExtractor);

            for (MyStudent student : loadStudents(broker, dictionaryExtractor).stream()
                    .sorted(new Comparator<ToolStudent>() {

                        @Override
                        public int compare(ToolStudent o1, ToolStudent o2) {
                            return o1.getOid().compareTo(o2.getOid());
                        }
                    }).collect(Collectors.toList())) {
                List<AnnualSpan> spans = student.getEnrollmentSpans(broker, true, false);
                for (AnnualSpan span : spans) {
                    grid.append();
                    grid.set(FIELD_SKL_NAME, student.getSchool(getBroker()).getName());
                    grid.set(FIELD_STD_NAME, student.getNameView());
                    grid.set(FIELD_ENRSPAN_SCHOOL, span.getSchool() == null ? null : span.getSchool().getName());
                    grid.set(FIELD_ENRSPAN_SECONDARY_SCHOOL,
                            span.getSecondary() == null ? null : span.getSecondary().getSchool(getBroker()).getName());
                    grid.set(FIELD_ENRSPAN_FIRST_ACTIVE_DATE, getDateString(span.getFirstActiveInSessionDate()));
                    grid.set(FIELD_ENRSPAN_LAST_ACTIVE_DATE, getDateString(span.getLastActiveInSessionDate()));
                    MyEnrollment enr = (MyEnrollment) span.getFirstActiveEnrollment();
                    if (enr != null) {
                        grid.set(FIELD_ENRSPAN_ACTIVE_ENR_DATE, getDateString(enr.getEnrollmentDate()));
                        grid.set(FIELD_ENRSPAN_ACTIVE_ENR_TYPE, enr.getEnrollmentType());
                        grid.set(FIELD_ENRSPAN_ACTIVE_ENR_CODE, enr.getEnrollmentCode());
                        grid.set(FIELD_ENRSPAN_ACTIVE_ENR_YOG, Integer.toString(enr.getYog()));
                    }
                    enr = (MyEnrollment) span.getTerminatingEnrollment();
                    if (enr != null) {
                        grid.set(FIELD_ENRSPAN_INACTIVE_ENR_DATE, getDateString(enr.getEnrollmentDate()));
                        grid.set(FIELD_ENRSPAN_INACTIVE_ENR_TYPE, enr.getEnrollmentType());
                        grid.set(FIELD_ENRSPAN_INACTIVE_ENR_CODE, enr.getEnrollmentCode());
                        grid.set(FIELD_ENRSPAN_INACTIVE_ENR_YOG, Integer.toString(enr.getYog()));
                    }
                }
            }
        } finally {
            broker.endSession();
        }
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return Arrays.asList(FIELD_SKL_NAME,
                FIELD_STD_NAME,
                FIELD_ENRSPAN_SCHOOL,
                FIELD_ENRSPAN_SECONDARY_SCHOOL,
                FIELD_ENRSPAN_FIRST_ACTIVE_DATE,
                FIELD_ENRSPAN_LAST_ACTIVE_DATE,
                FIELD_ENRSPAN_ACTIVE_ENR_DATE,
                FIELD_ENRSPAN_ACTIVE_ENR_TYPE,
                FIELD_ENRSPAN_ACTIVE_ENR_CODE,
                FIELD_ENRSPAN_ACTIVE_ENR_YOG,
                FIELD_ENRSPAN_INACTIVE_ENR_DATE,
                FIELD_ENRSPAN_INACTIVE_ENR_TYPE,
                FIELD_ENRSPAN_INACTIVE_ENR_YOG,
                FIELD_ENRSPAN_INACTIVE_ENR_YOG);
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
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        /*
         * If we're in the context of a single student, print the report for just that student
         */
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }

    /**
     * Normally an OnSIS span calculates the Span End Date as the first date of non-attendance,
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
     * in OnsisStudentSchoolEnrolment.getOnsisEnrollmentEndDate
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
     * @param span AnnualSpan
     * @return PlainDate
     */
    private PlainDate calculateSpanEndDate(AnnualSpan span) {
        PlainDate endDate = span.getFirstInactiveInSessionDate();

        boolean isWithdrawal = span.isWithdrawal();
        PlainDate lastActiveDate = span.getLastActiveInSessionDate();
        PlainDate withdrawalMemberDate = span.getWithdrawalMemberDate();

        PlainDate scanStartDate = null;
        boolean falseForward = false;
        PlainDate lastInSessionDate = span.getSchoolCalendar() == null ? null
                : span.getSchoolCalendar().findFirstInSessionDate(getBroker(), scanStartDate, falseForward);

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


    private X2Criteria getActiveStudentCriteria() {
        X2Criteria activeCriteria = new X2Criteria();

        /*
         * Filter by specific students
         */
        List<String> limitingStudentOids =
                (List<String>) ToolBean.getPreference(ToolBean.PREFERENCE_LIMITING_STUDENT_OIDS);
        if (limitingStudentOids != null && !limitingStudentOids.isEmpty()) {
            activeCriteria.addIn(ToolBean.FIELD_OID.resolve(null), limitingStudentOids);
        }

        /*
         * Filter by enrollment status
         */
        activeCriteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                        ToolStudent.FIELD_ENROLLMENT_STATUS.resolve(null)));

        /*
         * Filter by school
         */
        List<String> limitingSchoolOids =
                (List<String>) ToolBean.getPreference(ToolBean.PREFERENCE_LIMITING_SCHOOL_OIDS);
        if (limitingSchoolOids != null && !limitingSchoolOids.isEmpty()) {
            activeCriteria.addIn(ToolStudent.FIELD_SCHOOL_OID.resolve(null), limitingSchoolOids);
        } else {
            activeCriteria.addNotEqualTo(ToolStudent.FIELD_SCHOOL_INACTIVE_INDICATOR.resolve(null), Boolean.TRUE);
            activeCriteria.addNotEqualTo(ToolStudent.FIELD_SCHOOL_ARCHIVE_INDICATOR.resolve(null), Boolean.TRUE);
        }

        /*
         * Custom caller criteria
         */
        Criteria studentLimitingCriteria =
                (Criteria) ToolBean.getPreference(ToolBean.PREFERENCE_LIMITING_STUDENT_CRITERIA);
        if (studentLimitingCriteria != null && !studentLimitingCriteria.isEmpty()) {
            activeCriteria.addAndCriteria(studentLimitingCriteria);
        }

        return activeCriteria;
    }

    /**
     * Gets the date string.
     *
     * @param date PlainDate
     * @return String
     */
    private String getDateString(PlainDate date) {
        return date == null ? null : date.toString();
    }


    /**
     * @return
     */
    private X2Criteria getEnrollmentCriteria() {
        X2Criteria enrollmentCriteria = new X2Criteria();

        /*
         * Filter by specific students
         */
        List<String> limitingStudentOids =
                (List<String>) ToolBean.getPreference(ToolBean.PREFERENCE_LIMITING_STUDENT_OIDS);
        if (limitingStudentOids != null && !limitingStudentOids.isEmpty()) {
            enrollmentCriteria.addIn(ToolEnrollment.FIELD_STUDENT_OID.resolve(null), limitingStudentOids);
        }

        /*
         * Filter by activity since historical cutoff date
         */
        PlainDate historicalCutoffDate = (PlainDate) ToolBean.getPreference(ToolBean.PREFERENCE_HISTORICAL_CUTOFF_DATE);
        if (historicalCutoffDate != null) {
            enrollmentCriteria.addGreaterOrEqualThan(ToolEnrollment.FIELD_DATE_DESC.resolve(null),
                    historicalCutoffDate);
        }

        /*
         * Filter by school
         */
        List<String> limitingSchoolOids =
                (List<String>) ToolBean.getPreference(ToolBean.PREFERENCE_LIMITING_SCHOOL_OIDS);
        if (limitingSchoolOids != null && !limitingSchoolOids.isEmpty()) {
            enrollmentCriteria.addIn(ToolEnrollment.FIELD_SCHOOL_OID.resolve(null), limitingSchoolOids);
        } else {
            enrollmentCriteria.addNotEqualTo(ToolEnrollment.FIELD_SCHOOL_INACTIVE_INDICATOR.resolve(null),
                    Boolean.TRUE);
            enrollmentCriteria.addNotEqualTo(ToolEnrollment.FIELD_SCHOOL_ARCHIVE_INDICATOR.resolve(null), Boolean.TRUE);
        }

        return enrollmentCriteria;
    }

    private X2Criteria getSecondaryStudentCriteria(X2Broker broker) {
        X2Criteria secondaryCriteria = new X2Criteria();

        /*
         * Filter by specific students
         */
        List<String> limitingStudentOids =
                (List<String>) ToolBean.getPreference(ToolBean.PREFERENCE_LIMITING_STUDENT_OIDS);
        if (limitingStudentOids != null && !limitingStudentOids.isEmpty()) {
            secondaryCriteria.addIn(ToolStudentSchool.FIELD_STUDENT_OID.resolve(null), limitingStudentOids);
        }

        /*
         * Filter by activity since historical cutoff date
         */
        PlainDate historicalCutoffDate = (PlainDate) ToolBean.getPreference(ToolBean.PREFERENCE_HISTORICAL_CUTOFF_DATE);
        PlainDate asOfDate = (PlainDate) ToolBean.getPreference(ToolBean.PREFERENCE_AS_OF_DATE);
        StudentManager.buildSecondaryStudentDateCriteria(null, secondaryCriteria, historicalCutoffDate,
                asOfDate, broker.getPersistenceKey());

        /*
         * Filter by school
         */
        List<String> limitingSchoolOids =
                (List<String>) ToolBean.getPreference(ToolBean.PREFERENCE_LIMITING_SCHOOL_OIDS);
        if (limitingSchoolOids != null && !limitingSchoolOids.isEmpty()) {
            secondaryCriteria.addIn(ToolStudentSchool.FIELD_SCHOOL_OID.resolve(null), limitingSchoolOids);
        } else {
            secondaryCriteria.addNotEqualTo(
                    ToolStudentSchool.FIELD_SCHOOL_INACTIVE_INDICATOR.resolve(null), Boolean.TRUE);
            secondaryCriteria.addNotEqualTo(
                    ToolStudentSchool.FIELD_SCHOOL_ARCHIVE_INDICATOR.resolve(null), Boolean.TRUE);
        }

        return secondaryCriteria;
    }

    private X2Criteria getStudentCriteria(X2Broker broker) {
        X2Criteria studentCriteria = new X2Criteria();

        /*
         * Filter to specific students
         */
        List<String> limitingStudentOids =
                (List<String>) ToolBean.getPreference(ToolBean.PREFERENCE_LIMITING_STUDENT_OIDS);
        if (limitingStudentOids != null && !limitingStudentOids.isEmpty()) {
            studentCriteria.addIn(ToolBean.FIELD_OID.resolve(null), limitingStudentOids);
        }

        /*
         * Filter by enrollment activity (for specific students/schools/cutoff date)
         */
        X2Criteria enrSubQueryCriteria = getEnrollmentCriteria();
        SubQuery enrollmentSubQuery =
                new SubQuery(StudentEnrollment.class, ToolEnrollment.FIELD_STUDENT_OID.resolve(null),
                        enrSubQueryCriteria);
        X2Criteria enrCriteria = new X2Criteria();
        enrCriteria.addIn(ToolBean.FIELD_OID.resolve(null), enrollmentSubQuery);

        /*
         * Filter by current enrollment and school status
         */
        X2Criteria orCriteria = new X2Criteria();
        Criteria activeCriteria = getActiveStudentCriteria();

        /*
         * join the enr, active and ssk criteria in an OR.
         */
        orCriteria.addOrCriteria(enrCriteria);
        orCriteria.addOrCriteria(activeCriteria);

        // Find students who are secondary
        Boolean includeSecondarySpans = (Boolean) ToolBean.getPreference(ToolBean.PREFERENCE_INCLUDE_SECONDARY_SPANS);
        if (includeSecondarySpans) {
            Criteria sskSubQueryCriteria = getSecondaryStudentCriteria(broker);
            SubQuery secondaryQuery =
                    new SubQuery(StudentSchool.class, ToolStudentSchool.FIELD_STUDENT_OID.resolve(null),
                            sskSubQueryCriteria);
            Criteria sskCriteria = new Criteria();
            sskCriteria.addIn(ToolBean.FIELD_OID.resolve(null), secondaryQuery);
            orCriteria.addOrCriteria(sskCriteria);
        }

        // Build the final student criteria, including user criteria and exclude criteria.
        studentCriteria.addAndCriteria(orCriteria);

        /*
         * Custom caller criteria
         */
        Criteria studentLimitingCriteria =
                (Criteria) ToolBean.getPreference(ToolBean.PREFERENCE_LIMITING_STUDENT_CRITERIA);
        if (studentLimitingCriteria != null && !studentLimitingCriteria.isEmpty()) {
            studentCriteria.addAndCriteria(studentLimitingCriteria);
        }

        // Apply exclude criteria.
        Boolean applyExcludeStudent = (Boolean) ToolBean.getPreference(ToolBean.PREFERENCE_EXCLUDE_STUDENT);
        ToolBeanColumn excludeStudentColumn =
                (ToolBeanColumn) ToolBean.getPreference(ToolBean.PREFERENCE_EXCLUDE_STUDENT_COLUMN);
        if (applyExcludeStudent != null && applyExcludeStudent.booleanValue() && excludeStudentColumn != null) {
            studentCriteria.addNotEqualTo(excludeStudentColumn.resolve(null), BooleanAsStringConverter.TRUE);
        }

        return studentCriteria;
    }

    /**
     * Initialize helpers.
     */
    private void initializeHelpers() {
        /*
         * The business layer (caller) must set
         * the definition of "span start date" and "start end date"
         */
        DistrictManager.setAnnualSpanFactory(new AnnualSpanFactory()
                .setAnnualSpanStartDateFn(new Function<AnnualSpan, PlainDate>() {
                    @Override
                    public PlainDate apply(AnnualSpan span) {
                        return span.getFirstActiveInSessionDate();
                    }
                }).setAnnualSpanEndDateFn(new Function<AnnualSpan, PlainDate>() {
                    @Override
                    public PlainDate apply(AnnualSpan span) {
                        return calculateSpanEndDate(span);
                    }
                }));

        String activeStatus = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        X2Criteria studentLimitingCriteria = new X2Criteria();
        if (m_currentStudent != null) {
            studentLimitingCriteria.addEqualTo(X2BaseBean.COL_OID, m_currentStudent.getOid());
        } else {
            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            addUserCriteria(studentLimitingCriteria, queryBy, (String) getParameter(QUERY_STRING_PARAM), null,
                    null);

            boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
            if (activeOnly) {
                studentLimitingCriteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, activeStatus);
            }
        }
        ToolBean.setPreference(ToolBean.PREFERENCE_LIMITING_STUDENT_CRITERIA, studentLimitingCriteria);
        if (getSchool() != null) {
            ToolBean.setPreference(ToolBean.PREFERENCE_LIMITING_SCHOOL_OIDS, Arrays.asList(getSchool().getOid()));
        }

        ToolBean.setPreference(ToolBean.PREFERENCE_INCLUDE_SECONDARY_SPANS, Boolean.TRUE);
    }

    /**
     * @return
     *
     */
    private Collection<MyStudent> loadStudents(X2Broker broker, DictionaryExtractor dictionaryExtractor) {
        X2Criteria studentCriteria = getStudentCriteria(broker);

        // Perform ToolBean Load
        FilterableFactory.create(broker, MyStudent.class, studentCriteria, null);

        ToolBean.preload(getBroker(), dictionaryExtractor,
                Arrays.asList(ToolEnrollment.FIELD_DATE_DESC, ToolEnrollment.FIELD_TIMESTAMP_DESC),
                ToolStudent.CHILD_STUDENT_ENROLLMENTS);
        ToolBean.preload(getBroker(), dictionaryExtractor,
                Arrays.asList(ToolStudentSchool.FIELD_START_DATE_DESC, ToolBean.FIELD_OID),
                ToolStudent.CHILD_STUDENT_SCHOOLS);

        Range<Date> range = Range.of(getCurrentContext().getStartDate(), getCurrentContext().getEndDate());
        return ToolBean.filterCachedToolBeans(MyStudent.class, new Predicate<MyStudent>() {

            @Override
            public boolean test(MyStudent student) {
                List<AnnualSpan> spans = student.getEnrollmentSpans(broker, true, false);
                boolean included = spans.stream().filter(span -> {
                    return range.isOverlap(span.getDateRange());
                }).findAny().isPresent();
                return included;
            }
        });
    }
}
