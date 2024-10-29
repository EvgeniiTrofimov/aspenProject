/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2020 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.on;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.OrganizationLocale;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.common.CriteriaHelper;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.DistrictManager;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolEnrollment;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchedule;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentScheduleChange;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchool;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolTranscript;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.ParentSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnAnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnAnnualSpanFactory;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnEnrollment;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudentScheduleSpanFactory;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.LoggerUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import net.sf.jasperreports5.engine.JRRewindableDataSource;
import net.sf.jasperreports5.engine.data.JRBeanCollectionDataSource;
import org.apache.struts.util.MessageResources;

/**
 * The Class OnsisStudentSpansData.
 *
 * @author Follett Software Company
 * @copyright 2020
 */
public class OnsisStudentSpansData extends ReportJavaSourceNet {
    /**
     * The Class RewindableReportDataGrid.
     */
    class RewindableJRBeanDataGrid extends JRBeanCollectionDataSource implements JRRewindableDataSource {
        /**
         * @param beanCollection
         */
        public RewindableJRBeanDataGrid(Collection<?> beanCollection) {
            super(beanCollection);
        }

        /**
         * @see net.sf.jasperreports5.engine.JRRewindableDataSource#moveFirst()
         */
        @Override
        public void moveFirst() {
            super.moveFirst();
        }
    }

    /**
     * Name for the "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

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
    private static final String FIELD_GRID_SORT_HOMEROOM = "gridSortHomeroom";
    private static final String FIELD_GRID_SORT_YOG = "gridSortYog";
    private static final String FIELD_ROW_TYPE = "rowType";
    private static final String FIELD_SCHSPAN_ENTRY_CHANGE = "schSpanEntryChange";
    private static final String FIELD_SCHSPAN_EXIT_CHANGE = "schSpanExitChange";
    private static final String FIELD_SCHSPAN_EXIT_DATE = "schSpanExitDate";
    private static final String FIELD_SCHSPAN_SCHEDULE = "schSpanSchedule";
    private static final String FIELD_SCHSPAN_SCHOOL = "schSpanSchool";
    private static final String FIELD_SCHSPAN_SECTION = "schSpanSection";
    private static final String FIELD_SCHSPAN_START_DATE = "schSpanStartDate";
    private static final String FIELD_SCHSPAN_TERM = "schSpanTerm";
    private static final String FIELD_SKL_NAME = "schoolName";
    private static final String FIELD_STD_LOCAL_ID = "studentLocalId";
    private static final String FIELD_STD_NAME = "studentName";
    private static final String FIELD_STD_OEN = "studentOEN";
    private static final String FIELD_STD_SHARED = "studentShared";

    private static final String INPUT_PARAM_DEBUG_DETAIL_ENR = "debugDetailEnr";
    private static final String INPUT_PARAM_DEBUG_DETAIL_SSC = "debugDetailSsc";
    private static final String INPUT_PARAM_ENR_STATUS = "enrStatus";
    private static final String INPUT_PARAM_ENR_STATUS_CTX = "currentCtx";
    private static final String INPUT_PARAM_ENR_STATUS_TODAY = "activeToday";
    private static final String INPUT_SORT_HOMEROOM = "homeroom,nameView";
    private static final String INPUT_SORT_YOG = "yog,nameView";

    private static final String OUTPUT_PARAM_DEBUG_MESSAGES = "debugMessages";

    private static final String REPORT_PARAM_VERSION = "version";
    private static final String ROW_TYPE_ENROLLMENT_SPAN = "enrollmentSpan";
    private static final String ROW_TYPE_SCHEDULE_SPAN = "scheduleSpan";

    private SisStudent m_currentStudent;
    private Boolean m_debugDetailEnr;
    private Boolean m_debugDetailSsc;
    private DictionaryExtractor m_dictionaryExtractor;
    private List<String> m_messages = new ArrayList();
    private OnSchool m_school;

    // Localization
    private Map<String, String> m_validLocales;
    private String m_defaultLocale; // Usually English
    private MessageResources m_default_message_resource;
    private Locale m_user_locale;
    private static final String PARAM_PREFIX = "prefix";
    private static final String PARAM_REPORT_LOCALE = "reportLocale";
    private static final String PARAM_LOCALES = "locales";
    private static final String PARAM_DEFAULT_LOCALE = "default_locale";
    private static final String PARAM_DEFAULT_LANGUAGE = "default_language";
    private static final String CONST_TOOLS_FOR_PREFIX = "tools.";
    private static final String CONST_AMERICAN_ENGLISH_LOCALE = "en_US";
    private static final String CONST_AMERICAN_ENGLISH_LANGUAGE = "English";

    /**
     * Gets the user message key prefix.
     *
     * @return the user message key prefix
     */
    public String getUserMessageKeyPrefix() {
        return CONST_TOOLS_FOR_PREFIX + getJob().getTool().getOid() + ".";
    }

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() {
        X2Broker broker = getBroker();
        broker.beginSession();

        ReportDataGrid grid = new ReportDataGrid();
        m_school = ToolBean.getBeanByOid(getBroker(), getDictionaryExtractor(), OnSchool.class, getSchool().getOid(),
                true);

        try {

            initializeHelpers();

            if (isSchoolContext()) {
                List<OnStudent> students = loadStudents(broker, getDictionaryExtractor()).stream()
                        .sorted(new Comparator<ToolStudent>() {
                            @Override
                            public int compare(ToolStudent o1, ToolStudent o2) {
                                return o1.getOid().compareTo(o2.getOid());
                            }
                        }).collect(Collectors.toList());

                EnrollmentSpanCriteria spanCriteria = new EnrollmentSpanCriteria()
                        .setCurrentContext(getCurrentContext())
                        .setExcludeSection(OnSection.FIELD_MST_EXCLUDE_FROM_ONSIS);
                // preload schedules
                ToolBean.addAndCriteria(getBroker(), ToolStudentSchedule.class,
                        CriteriaHelper.buildStudentScheduleCriteria(spanCriteria));
                ToolBean.addAndCriteria(getBroker(), ToolStudentScheduleChange.class,
                        CriteriaHelper.buildStudentScheduleChangeCriteria(spanCriteria));
                ToolBean.addAndCriteria(getBroker(), ToolTranscript.class,
                        CriteriaHelper.buildStudentTranscriptCriteria(spanCriteria, getBroker()));

                ToolBean.preload(getBroker(), getDictionaryExtractor(), null, ToolStudentSchedule.PARENT_STUDENT);
                ToolBean.preload(getBroker(), getDictionaryExtractor(), null, ToolStudentScheduleChange.PARENT_STUDENT);
                ToolBean.preload(getBroker(), getDictionaryExtractor(), null, ToolTranscript.PARENT_STUDENT);

                Set<String> sectionOids = Stream.concat(Stream.concat(
                        ToolBean.getCachedToolBeans(ToolStudentSchedule.class).stream().map(ssc -> ssc.getSectionOid()),
                        ToolBean.getCachedToolBeans(ToolStudentScheduleChange.class).stream()
                                .map(scc -> scc.getSectionOid())),
                        ToolBean.getCachedToolBeans(ToolTranscript.class).stream().map(trn -> trn.getSectionOid()))
                        .collect(Collectors.toSet());
                ToolBean.loadByOid(getBroker(), getDictionaryExtractor(), OnSection.class, sectionOids);

                for (OnStudent student : students) {
                    if (getDebugDetailEnr() || getDebugDetailSsc()) {
                        m_messages.add("Student: " + student.getNameView());
                    }
                    for (OnAnnualSpan span : student.getEnrollmentSpans(getBroker(), true, false).stream()
                            .map(span -> (OnAnnualSpan) span)
                            .sorted(new Comparator<AnnualSpan>() {
                                @Override
                                public int compare(AnnualSpan span1, AnnualSpan span2) {
                                    int result = 0;
                                    if (span1.isSecondary() && !span2.isSecondary()) {
                                        result = 1;
                                    } else if (span2.isSecondary() && !span1.isSecondary()) {
                                        result = -1;
                                    }
                                    if (result == 0) {
                                        result = span1.getSpanStartDate()
                                                .compareTo(span2.getSpanStartDate());
                                    }
                                    return result;
                                }
                            }).collect(Collectors.toList())) {
                        grid.append();
                        grid.set(FIELD_ROW_TYPE, ROW_TYPE_ENROLLMENT_SPAN);
                        grid.set(FIELD_SKL_NAME, m_school.getName());
                        grid.set(FIELD_STD_NAME, student.getNameView());
                        grid.set(FIELD_STD_OEN, student.getValueString(OnStudent.FIELD_OEN));
                        String stdLasid = student.getLocalIdOverride() != null ? (String) student.getLocalIdOverride()
                                : student.getLocalId();
                        grid.set(FIELD_STD_LOCAL_ID, stdLasid);
                        grid.set(FIELD_ENRSPAN_SCHOOL,
                                span.getSchool() == null ? null
                                        : (span.getArrivalDate() == null ? span.getSchool().getName() + " - No Show"
                                                : span.getSchool().getName()));
                        grid.set(FIELD_ENRSPAN_FIRST_ACTIVE_DATE, getDateString(span.getFirstActiveInSessionDate()));
                        grid.set(FIELD_ENRSPAN_LAST_ACTIVE_DATE, getDateString(span.getSpanEndDate()));
                        ToolEnrollment enr = span.getFirstActiveEnrollment();
                        if (enr != null) {
                            grid.set(FIELD_ENRSPAN_ACTIVE_ENR_DATE, getDateString(enr.getEnrollmentDate()));
                            grid.set(FIELD_ENRSPAN_ACTIVE_ENR_TYPE, enr.getEnrollmentType());
                            grid.set(FIELD_ENRSPAN_ACTIVE_ENR_CODE, enr.getEnrollmentCode());
                            grid.set(FIELD_ENRSPAN_ACTIVE_ENR_YOG, Integer.toString(enr.getYog()));
                        }
                        enr = span.getTerminatingEnrollment();
                        if (enr != null) {
                            grid.set(FIELD_ENRSPAN_INACTIVE_ENR_DATE, getDateString(enr.getEnrollmentDate()));
                            grid.set(FIELD_ENRSPAN_INACTIVE_ENR_TYPE, enr.getEnrollmentType());
                            grid.set(FIELD_ENRSPAN_INACTIVE_ENR_CODE, enr.getEnrollmentCode());
                            grid.set(FIELD_ENRSPAN_INACTIVE_ENR_YOG, Integer.toString(enr.getYog()));
                        }
                        grid.set(FIELD_STD_SHARED, span.isSecondary() ? "Yes" : null);
                    }
                    for (OnsisStudentScheduleSpan span : student
                            .getStudentScheduleSpans(getBroker()).stream()
                            .map(span -> (OnsisStudentScheduleSpan) span)
                            .sorted(new Comparator<StudentScheduleSpan>() {
                                @Override
                                public int compare(StudentScheduleSpan span1, StudentScheduleSpan span2) {
                                    int result =
                                            span1.getSection().getSchedule(getBroker()).getSchool(getBroker()).getName()
                                                    .compareTo(span2.getSection().getSchedule(getBroker())
                                                            .getSchool(getBroker()).getName());
                                    if (result == 0) {
                                        result = span1.getEntryDate().compareTo(span2.getEntryDate());
                                    }
                                    if (result == 0) {
                                        result = span1.getSection().getCourseView()
                                                .compareTo(span2.getSection().getCourseView());
                                    }
                                    return result;
                                }
                            }).collect(Collectors.toList())) {
                        OnAnnualSpan enrollmentSpan = student.getEnrollmentSpans(getBroker(), true, false).stream()
                                .map(enrSpan -> (OnAnnualSpan) enrSpan)
                                .filter(testSpan -> {
                                    if (!span.getSection().getSchedule(getBroker()).getSchoolOid()
                                            .equals(testSpan.getSchool().getOid())) {
                                        return false;
                                    }
                                    Range<Date> enrollDateRange =
                                            Range.of(testSpan.getSpanStartDate(),
                                                    testSpan.getSpanEndDate());
                                    return enrollDateRange.isOverlap(span.getDateRange());
                                })
                                .sorted(new Comparator<AnnualSpan>() {
                                    @Override
                                    public int compare(AnnualSpan span1, AnnualSpan span2) {
                                        int result = 0;
                                        if (span1.isSecondary() && !span2.isSecondary()) {
                                            result = 1;
                                        } else if (span2.isSecondary() && !span1.isSecondary()) {
                                            result = -1;
                                        }
                                        if (result == 0) {
                                            result = span1.getSpanStartDate()
                                                    .compareTo(span2.getSpanStartDate());
                                        }
                                        return result;
                                    }
                                }).findFirst().orElse(null);
                        grid.append();
                        grid.set(FIELD_ROW_TYPE, ROW_TYPE_SCHEDULE_SPAN);
                        grid.set(FIELD_SKL_NAME, m_school.getName());
                        grid.set(FIELD_STD_NAME, student.getNameView());
                        grid.set(FIELD_STD_OEN, student.getValueString(OnStudent.FIELD_OEN));
                        grid.set(FIELD_STD_LOCAL_ID, student.getLocalIdOverride());
                        grid.set(FIELD_SCHSPAN_SCHOOL,
                                span.getSection().getSchedule(getBroker()).getSchool(getBroker()).getName());
                        grid.set(FIELD_SCHSPAN_SECTION, span.getSection().getCourseView());
                        grid.set(FIELD_SCHSPAN_TERM, span.getSection().getTermView());
                        grid.set(FIELD_SCHSPAN_SCHEDULE, span.getSection().getScheduleDisplay());
                        PlainDate spanTermStartDAte = span.getEntryDate();
                        grid.set(FIELD_SCHSPAN_START_DATE, getDateString(getSchSpanStartDate(span, enrollmentSpan)));
                        PlainDate spanTermEndDAte = span.getExitDate();
                        grid.set(FIELD_SCHSPAN_EXIT_DATE,
                                getDateString(span.getExitChange() != null
                                        && !span.getExitChange().getEffectiveDate().after(spanTermEndDAte)
                                                ? span.getExitChange().getEffectiveDate()
                                                : spanTermEndDAte));
                        grid.set(FIELD_SCHSPAN_ENTRY_CHANGE, span.getEntryChange() != null ? "Yes" : "No");
                        grid.set(FIELD_SCHSPAN_EXIT_CHANGE, span.getExitChange() != null ? "Yes" : "No");
                    }
                    grid.set(FIELD_GRID_SORT_YOG, Integer.valueOf(student.getYog()));
                    grid.set(FIELD_GRID_SORT_HOMEROOM, student.getHomeroom());
                }
                if (getDebugDetailEnr() || getDebugDetailSsc()) {
                    if (grid.getRows().size() == 0) {
                        grid.append();
                        grid.set(FIELD_ROW_TYPE, ROW_TYPE_ENROLLMENT_SPAN);
                        grid.set(FIELD_SKL_NAME, m_school.getName());
                    }
                    addParameter(OUTPUT_PARAM_DEBUG_MESSAGES, new RewindableJRBeanDataGrid(m_messages));
                }
                if (getParameter(SORT_PARAM) != null) {
                    String sortValue = (String) getParameter(SORT_PARAM);
                    if (INPUT_SORT_HOMEROOM.equals(sortValue)) {
                        grid.sort(Arrays.asList(FIELD_SKL_NAME, FIELD_STD_NAME, FIELD_STD_OEN, FIELD_GRID_SORT_HOMEROOM,
                                FIELD_SCHSPAN_TERM, FIELD_SCHSPAN_START_DATE), true);
                    } else if (INPUT_SORT_YOG.equals(sortValue)) {
                        grid.sort(Arrays.asList(FIELD_SKL_NAME, FIELD_STD_NAME, FIELD_STD_OEN, FIELD_GRID_SORT_YOG,
                                FIELD_SCHSPAN_TERM,
                                FIELD_SCHSPAN_START_DATE), true);
                    } else {
                        grid.sort(
                                Arrays.asList(FIELD_SKL_NAME, FIELD_STD_NAME, FIELD_STD_OEN, FIELD_SCHSPAN_TERM,
                                        FIELD_SCHSPAN_START_DATE),
                                true);
                    }
                }
                grid.beforeTop();
            }
        } finally {
            broker.endSession();
        }
        addParameter(REPORT_PARAM_VERSION, getJob().getTool().getComment());
        return grid;
    }

    /**
     * Initialize the report data.
     *
     */
    @Override
    protected void initialize() throws X2BaseException {
        ToolBean.setDictionaryExtractor(getDictionaryExtractor());
        ToolBean.registerClass(OnEnrollment.class);
        ToolBean.registerClass(OnSchool.class);
        ToolBean.registerClass(OnStudent.class);
        ToolBean.registerClass(OnSection.class);
        super.initialize();
        initializeLocalized();
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

        try {
            m_default_message_resource =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey(), userData.getLocale());
            m_user_locale = userData.getLocale();
        } catch (Exception e) {
            m_default_message_resource = LocalizationCache.getMessages(getBroker().getPersistenceKey(),
                    LocalizationCache.getCurrentLocale());
            m_user_locale = Locale.US;
        }
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
     * Gets the debug detail.
     *
     * @return boolean
     */
    private boolean getDebugDetailEnr() {
        if (m_debugDetailEnr == null) {
            m_debugDetailEnr = getParameter(INPUT_PARAM_DEBUG_DETAIL_ENR) != null
                    && getParameter(INPUT_PARAM_DEBUG_DETAIL_ENR) instanceof Boolean
                    && ((Boolean) getParameter(INPUT_PARAM_DEBUG_DETAIL_ENR));
        }
        return m_debugDetailEnr.booleanValue() ? true : false;
    }

    /**
     * Gets the debug detail.
     *
     * @return boolean
     */
    private boolean getDebugDetailSsc() {
        if (m_debugDetailSsc == null) {
            m_debugDetailSsc = getParameter(INPUT_PARAM_DEBUG_DETAIL_SSC) != null
                    && getParameter(INPUT_PARAM_DEBUG_DETAIL_SSC) instanceof Boolean
                    && ((Boolean) getParameter(INPUT_PARAM_DEBUG_DETAIL_SSC));
        }
        return m_debugDetailSsc.booleanValue() ? true : false;
    }

    /**
     * Gets the Dictionary Extractor
     *
     * @return DictionaryExtractor
     */
    private DictionaryExtractor getDictionaryExtractor() {

        if (m_dictionaryExtractor == null) {
            m_dictionaryExtractor = new DictionaryExtractor(getBroker());
        }
        return m_dictionaryExtractor;
    }

    /**
     * Gets the schedule span start date.
     *
     * @param span
     * @param enrollmentSpan
     * @return PlainDate
     */
    private PlainDate getSchSpanStartDate(OnsisStudentScheduleSpan span, OnAnnualSpan enrollmentSpan) {
        PlainDate spanTermStartDAte = span.getEntryDate();
        PlainDate startDate = span.getEntryChange() != null
                && !span.getEntryChange().getEffectiveDate().before(spanTermStartDAte)
                        ? span.getEntryChange().getEffectiveDate()
                        : spanTermStartDAte;
        if (enrollmentSpan != null && enrollmentSpan.getFirstActiveInSessionDate().after(startDate)) {
            startDate = enrollmentSpan.getFirstActiveInSessionDate();
        }
        return startDate;
    }

    /**
     * Initialize helpers.
     */
    private void initializeHelpers() {
        /*
         * The business layer (caller) must set
         * the definition of "span start date" and "start end date"
         */
        DistrictManager.setAnnualSpanFactory(new OnAnnualSpanFactory(getBroker()));
        DistrictManager.setStudentScheduleSpanFactory(new OnsisStudentScheduleSpanFactory());
        ToolBean.setBroker(getBroker());

        X2Criteria studentLimitingCriteria = new X2Criteria();
        if (m_currentStudent != null) {
            studentLimitingCriteria.addEqualTo(X2BaseBean.COL_OID, m_currentStudent.getOid());
        } else {
            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            addUserCriteria(studentLimitingCriteria, queryBy, (String) getParameter(QUERY_STRING_PARAM), null,
                    null);
        }
        ToolBean.setPreference(ToolBean.PREFERENCE_LIMITING_STUDENT_CRITERIA, studentLimitingCriteria);
        if (getSchool() != null) {
            ToolBean.setPreference(ToolBean.PREFERENCE_LIMITING_SCHOOL_OIDS, Arrays.asList(getSchool().getOid()));
        }

        ToolBean.setPreference(ToolBean.PREFERENCE_INCLUDE_SECONDARY_SPANS, Boolean.TRUE);
    }

    /**
     * Initializes for localization.
     *
     * Adds the localization parameters
     * Populates the Valid Locales map
     */
    private void initializeLocalized() {
        Collection<OrganizationLocale> locales = getOrganization().getRootOrganization().getLocales();
        Map<String, MessageResources> resources = new HashMap<String, MessageResources>();
        m_validLocales = new HashMap<String, String>();

        for (OrganizationLocale loc : locales) {
            if (loc.getEnabledIndicator()) {

                MessageResources messages = LocalizationCache.getMessages(getBroker().getPersistenceKey(),
                        loc.getLocale());
                // save the messages for that language
                resources.put(loc.getLocale(), messages);

                // populate the map of valid locales
                m_validLocales.put(loc.getName(), loc.getLocale());
                if (loc.getPrimaryIndicator()) {
                    m_defaultLocale = loc.getLocale();
                }
            }
        }

        if (m_defaultLocale == null) {
            m_defaultLocale = CONST_AMERICAN_ENGLISH_LOCALE;
        }
        addParameter(PARAM_PREFIX, getUserMessageKeyPrefix());
        addParameter(PARAM_REPORT_LOCALE, m_default_message_resource);
        // Additional hooks for enhanced implementations
        addParameter(PARAM_LOCALES, resources);
        addParameter(PARAM_DEFAULT_LOCALE, CONST_AMERICAN_ENGLISH_LOCALE);
        addParameter(PARAM_DEFAULT_LANGUAGE, CONST_AMERICAN_ENGLISH_LANGUAGE);
        // Comment line below if your numeric notation, currencies and others don't display as
        // expected
        // Only tested for jasper version 5
        addParameter(net.sf.jasperreports5.engine.JRParameter.REPORT_LOCALE, m_user_locale);
    }

    /**
     * @return
     *
     */
    private Collection<OnStudent> loadStudents(X2Broker broker, DictionaryExtractor dictionaryExtractor) {
        X2Criteria studentLimitingCriteria = new X2Criteria();
        if (m_currentStudent != null) {
            studentLimitingCriteria.addEqualTo(X2BaseBean.COL_OID, m_currentStudent.getOid());
        } else {
            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            addUserCriteria(studentLimitingCriteria, queryBy, (String) getParameter(QUERY_STRING_PARAM), null,
                    null);
        }
        EnrollmentSpanCriteria spanCriteria = new EnrollmentSpanCriteria()
                .setStudentLimitingCriteria(studentLimitingCriteria)
                .setIncludeSecondarySpans(true)
                .setSchoolOids(Arrays.asList(m_school.getOid()));
        X2Criteria candidateCriteria = CriteriaHelper.getStudentCandidateCriteria(spanCriteria, broker);

        FilterableFactory.create(broker, getDictionaryExtractor(), OnStudent.class, candidateCriteria, null);

        ToolBean.preload(getBroker(), dictionaryExtractor,
                Arrays.asList(ToolEnrollment.FIELD_DATE_DESC, ToolEnrollment.FIELD_TIMESTAMP_DESC),
                ToolStudent.CHILD_STUDENT_ENROLLMENTS);
        ToolBean.preload(getBroker(), dictionaryExtractor,
                Arrays.asList(ToolStudentSchool.FIELD_START_DATE_DESC, ToolBean.FIELD_OID),
                ToolStudent.CHILD_STUDENT_SCHOOLS);

        String enrStatus = (String) getParameter(INPUT_PARAM_ENR_STATUS);
        StringBuilder exceptionList = new StringBuilder();
        Collection<OnStudent> students =
                ToolBean.filterCachedToolBeans(OnStudent.class, new Predicate<OnStudent>() {

                    @Override
                    public boolean test(OnStudent student) {
                        List<ParentSpan> parentSpans = Collections.EMPTY_LIST;
                        try {
                            parentSpans = student.getParentSpans(broker, true, false);
                        } catch (Exception e) {
                            exceptionList.append("Student: ");
                            exceptionList.append(student.getNameView());
                            exceptionList.append(" Oid: ");
                            exceptionList.append(student.getOid());
                            exceptionList.append("\n");
                            exceptionList.append(LoggerUtils.convertThrowableToString(e));
                            exceptionList.append("\n\n");
                        }
                        List<AnnualSpan> spans = parentSpans.stream()
                                .filter(parent -> {
                                    if (m_school != null) {
                                        if (!m_school.getOid().equals(parent.getSchool().getOid())) {
                                            return false;
                                        }
                                    }
                                    return true;
                                })
                                .map(parent -> parent.getAnnualSpans().stream())
                                .flatMap(annualSpans -> annualSpans.filter(span -> {
                                    switch (enrStatus) {
                                        case INPUT_PARAM_ENR_STATUS_CTX:
                                            return Range
                                                    .of(getCurrentContext().getStartDate(),
                                                            getCurrentContext().getEndDate())
                                                    .isOverlap(span.getDateRange());
                                        case INPUT_PARAM_ENR_STATUS_TODAY:
                                            PlainDate reportDate =
                                                    new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
                                            return Range.of(reportDate, reportDate).isOverlap(span.getDateRange());
                                        default:
                                            return true;
                                    }
                                })).collect(Collectors.toList());
                        return !spans.isEmpty();
                    }
                });
        if (exceptionList.length() != 0) {
            throw new RuntimeException("Critical Student Processing Errors\n" + exceptionList.toString());
        }
        return students;
    }
}
