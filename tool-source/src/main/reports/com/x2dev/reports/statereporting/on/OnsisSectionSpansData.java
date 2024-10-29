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
package com.x2dev.reports.statereporting.on;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.OrganizationLocale;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.DistrictManager;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolEnrollment;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchedule;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentScheduleChange;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchool;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Pair;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnAnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnAnnualSpanFactory;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnEnrollment;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentSchool;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudentScheduleSpanFactory;
import com.x2dev.procedures.statereporting.on.revised.OnsisConstants;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.struts.util.MessageResources;

/**
 * @author Follett Software Company
 * @copyright 2023
 */
public class OnsisSectionSpansData extends ReportJavaSourceNet {
    /**
     * Name for the "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    private static final String FIELD_ENRSPAN_FIRST_ACTIVE_DATE = "enrSpanFirstActiveDate";
    private static final String FIELD_ENRSPAN_LAST_ACTIVE_DATE = "enrSpanLastActiveDate";
    private static final String FIELD_SCHSPAN_DESCRIPTION = "schSpanDescription";
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

    private static final String INPUT_SORT_DESCRIPTION = "description";
    private static final String INPUT_SORT_NUMBER = "number";

    private static final String REPORT_PARAM_VERSION = "version";

    private MasterSchedule m_currentSection;
    private DictionaryExtractor m_dictionaryExtractor;
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
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();
        X2Broker broker = getBroker();
        broker.beginSession();
        ToolBean.setBroker(broker);

        try {
            m_school =
                    ToolBean.getBeanByOid(getBroker(), getDictionaryExtractor(), OnSchool.class, getSchool().getOid(),
                            true);
            X2Criteria sectionCriteria = new X2Criteria();
            sectionCriteria.addEqualTo(SisBeanPaths.SCHEDULE_MASTER.schedule().schoolOid().getPath(),
                    m_school.getOid());
            if (m_currentSection != null) {
                sectionCriteria.addEqualTo(X2BaseBean.COL_OID, m_currentSection.getOid());
            } else {
                String queryBy = (String) getParameter(QUERY_BY_PARAM);
                addUserCriteria(sectionCriteria, queryBy, "", null, null);
            }

            if (isSchoolContext()) {
                List<OnSection> sections = loadSections(sectionCriteria, broker, getDictionaryExtractor()).stream()
                        .sorted(new Comparator<OnSection>() {
                            @Override
                            public int compare(OnSection o1, OnSection o2) {
                                return o1.getOid().compareTo(o2.getOid());
                            }
                        }).collect(Collectors.toList());
                Date infiniteDate = null;
                Range<Date> infiniteRange = Range.of(infiniteDate, infiniteDate);

                for (OnSection section : sections) {
                    for (Pair<ToolStudent, List<StudentScheduleSpan>> pair : section
                            .getStudentsWithScheduleSpans(broker, infiniteRange, false)) {
                        OnStudent student = (OnStudent) pair.getLeft();
                        for (StudentScheduleSpan scheduleSpan : pair.getRight()) {
                            OnsisStudentScheduleSpan span = (OnsisStudentScheduleSpan) scheduleSpan;
                            OnStudentSchool studentSchool = student.getStudentSchools(getBroker()).stream()
                                    .map(ssk -> (OnStudentSchool) ssk)
                                    .filter(ssk -> ssk.getSchool(getBroker()).getOid().equals(m_school.getOid()))
                                    .filter(ssk -> Range.of(ssk.getStartDate(), ssk.getEndDate())
                                            .isOverlap(scheduleSpan.getDateRange()))
                                    .sorted(new Comparator<OnStudentSchool>() {
                                        @Override
                                        public int compare(OnStudentSchool ssk1, OnStudentSchool ssk2) {
                                            return ssk2.getStartDate().compareTo(ssk1.getStartDate());
                                        }
                                    }).findFirst().orElse(null);
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
                            if (isSpanReportable(studentSchool, enrollmentSpan)) {
                                grid.append();
                                grid.set(FIELD_SKL_NAME, m_school.getName());
                                grid.set(FIELD_STD_NAME, student.getNameView());
                                grid.set(FIELD_STD_OEN, student.getValueString(OnStudent.FIELD_OEN));
                                grid.set(FIELD_STD_LOCAL_ID, student.getLocalIdOverride());
                                grid.set(FIELD_SCHSPAN_SCHOOL,
                                        span.getSection().getSchedule(getBroker()).getSchool(getBroker()).getName());
                                grid.set(FIELD_SCHSPAN_DESCRIPTION, span.getSection().getDescription());
                                grid.set(FIELD_SCHSPAN_SECTION, span.getSection().getCourseView() + " - "
                                        + span.getSection().getTermView() + " - "
                                        + StringUtils.coalesce(span.getSection().getScheduleDisplay(), "None")
                                        + " - " + span.getSection().getDateRange(getBroker()));

                                if (enrollmentSpan != null) {
                                    grid.set(FIELD_STD_SHARED, enrollmentSpan.isSecondary() ? "Yes" : null);
                                    grid.set(FIELD_ENRSPAN_FIRST_ACTIVE_DATE,
                                            getDateString(enrollmentSpan.getFirstActiveInSessionDate()));
                                    grid.set(FIELD_ENRSPAN_LAST_ACTIVE_DATE,
                                            getDateString(enrollmentSpan.getSpanEndDate()));
                                }
                                grid.set(FIELD_SCHSPAN_START_DATE,
                                        getDateString(getSchSpanStartDate(span, enrollmentSpan)));
                                PlainDate spanTermEndDAte = span.getExitDate();
                                grid.set(FIELD_SCHSPAN_EXIT_DATE,
                                        getDateString(span.getExitChange() != null
                                                && !span.getExitChange().getEffectiveDate().after(spanTermEndDAte)
                                                        ? span.getExitChange().getEffectiveDate()
                                                        : spanTermEndDAte));
                                grid.set(FIELD_SCHSPAN_ENTRY_CHANGE, span.getEntryChange() != null ? "Yes" : "No");
                                grid.set(FIELD_SCHSPAN_EXIT_CHANGE, span.getExitChange() != null ? "Yes" : "No");
                            }
                        }
                    }
                }
                if (getParameter(SORT_PARAM) != null) {
                    String sortValue = (String) getParameter(SORT_PARAM);
                    if (INPUT_SORT_NUMBER.equals(sortValue)) {
                        grid.sort(Arrays.asList(FIELD_SKL_NAME, FIELD_SCHSPAN_SECTION, FIELD_STD_NAME, FIELD_STD_OEN,
                                FIELD_SCHSPAN_START_DATE), true);
                    } else if (INPUT_SORT_DESCRIPTION.equals(sortValue)) {
                        grid.sort(Arrays.asList(FIELD_SKL_NAME, FIELD_SCHSPAN_DESCRIPTION, FIELD_SCHSPAN_SECTION,
                                FIELD_STD_NAME, FIELD_STD_OEN, FIELD_SCHSPAN_START_DATE), true);
                    } else {
                        grid.sort(Arrays.asList(FIELD_SKL_NAME, FIELD_SCHSPAN_SECTION, FIELD_STD_NAME, FIELD_STD_OEN,
                                FIELD_SCHSPAN_START_DATE), true);
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
        DistrictManager.setAnnualSpanFactory(new OnAnnualSpanFactory(getBroker()));
        DistrictManager.setStudentScheduleSpanFactory(new OnsisStudentScheduleSpanFactory());
        ToolBean.setDictionaryExtractor(getDictionaryExtractor());
        ToolBean.registerClass(OnEnrollment.class);
        ToolBean.registerClass(OnSchool.class);
        ToolBean.registerClass(OnStudent.class);
        ToolBean.registerClass(OnStudentSchool.class);
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
        m_currentSection = userData.getCurrentRecord(MasterSchedule.class);

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
     * Checks if span should be reported.
     *
     * @param ssk OnStudentSchool
     * @param span OnAnnualSpan
     * @return boolean
     */
    private boolean isSpanReportable(OnStudentSchool ssk, OnAnnualSpan span) {
        boolean arrived = false;
        if (ssk != null) {
            String arrivalStatus = ssk.getArrivalStatus();
            arrived = StringUtils.isEmpty(arrivalStatus)
                    || OnsisConstants.ARRIVAL_STATUSES_ARRIVED.contains(arrivalStatus);
        }
        if (arrived && span != null) {
            OnEnrollment enrollment = (OnEnrollment) span.getFirstActiveEnrollment();
            if (enrollment != null) {
                String arrivalStatus = enrollment.getArrivalStatus();
                arrived = arrived && (StringUtils.isEmpty(arrivalStatus)
                        || OnsisConstants.ARRIVAL_STATUSES_ARRIVED.contains(arrivalStatus));
            }
        }
        return arrived;
    }

    /**
     * @param sectionCriteria
     * @param broker
     * @param dictionaryExtractor
     * @return
     */
    private Collection<OnSection> loadSections(X2Criteria mstCriteria,
                                               X2Broker broker,
                                               DictionaryExtractor dictionaryExtractor) {
        FilterableFactory.create(getBroker(), getDictionaryExtractor(), OnSection.class, mstCriteria, null);
        ToolBean.preload(getBroker(), getDictionaryExtractor(), null,
                ToolStudentSchedule.PARENT_SECTION);

        ToolBean.preload(getBroker(), getDictionaryExtractor(), null,
                ToolStudentScheduleChange.PARENT_SECTION);

        Set<String> studentOids = Stream.concat(
                ToolBean.getCachedToolBeans(ToolStudentSchedule.class).stream().map(ssc -> ssc.getStudentOid()),
                ToolBean.getCachedToolBeans(ToolStudentScheduleChange.class).stream().map(scc -> scc.getStudentOid()))
                .collect(Collectors.toSet());
        ToolBean.loadByOid(getBroker(), getDictionaryExtractor(), OnStudent.class, studentOids);

        // load enrollments and student school
        ToolBean.preload(getBroker(), getDictionaryExtractor(),
                Arrays.asList(ToolEnrollment.FIELD_DATE_DESC, ToolEnrollment.FIELD_TIMESTAMP_DESC),
                ToolStudent.CHILD_STUDENT_ENROLLMENTS);
        ToolBean.preload(getBroker(), getDictionaryExtractor(),
                Arrays.asList(ToolStudentSchool.FIELD_START_DATE_DESC, ToolBean.FIELD_OID),
                ToolStudent.CHILD_STUDENT_SCHOOLS);

        return ToolBean.getCachedToolBeans(OnSection.class);
    }


}
