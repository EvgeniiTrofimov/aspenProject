/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on;

import com.follett.fsc.core.framework.persistence.CollectionCriteriaHelper;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationMessageResources;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.microsoft.sqlserver.jdbc.StringUtils;
import com.x2dev.procedures.statereporting.common.CriteriaHelper;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filter;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.*;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanRelationship.LoaderMode;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.*;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class OnsisSecondaryClassSizeExport.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class OnsisSecondaryClassSizeExportRevised extends ExportJavaSource {
    private static final long serialVersionUID = 1L;

    private static final BigDecimal BIG_DECIMAL_ONE_HALF = new BigDecimal(0.5);
    private static final String CLASS_TYPE_ONLINE = "online";
    private static final String CONSTANT_RESOURCE_KEY_ONLINE = "label.EXP-ON-SEC-CLASS-SIZ.Online";
    private static final String CONSTANT_RESOURCE_KEY_NON_ONLINE = "label.EXP-ON-SEC-CLASS-SIZ.Non-Online";
    private static final String CONST_TOOLS_FOR_PREFIX = "ied.EXP-ON-SEC-CLASS-SIZ.";
    private static final String FIELD_CLASS = "Class";
    private static final String FIELD_CLASS_START_DATE = "Class Start Date";
    private static final String FIELD_CLASS_TYPE = "Class Type";
    private static final String FIELD_CLASSROOM_CREDIT = "Classroom Credit";
    private static final String FIELD_CLASSROOM_CREDIT_FY = "Full Year Classroom Credit";
    private static final String FIELD_CLASSROOM_CREDIT_SEM = "Semester Classroom Credit";
    private static final String FIELD_CREDIT = "Credit";
    private static final String FIELD_OEN = "OEN";
    private static final String FIELD_PUPIL_CREDIT = "Pupil Credit";
    private static final String FIELD_PUPIL_CREDIT_FY = "Full Year Pupil Credit";
    private static final String FIELD_PUPIL_CREDIT_SEM = "Semester Pupil Credit";
    private static final String FIELD_SECTION_NAME = "Section";
    private static final String FIELD_SCHEDULE_TERM = "Schedule Term";
    private static final String FIELD_SCHOOL_BSID = "School Bsid";
    private static final String FIELD_SCHOOL_NAME = "School Name";
    private static final String FIELD_STUDENT_NAME = "Student Name";
    private static final String FIELD_TOTAL_CLASS_STUDENTS = "Total Class Students";
    private static final String FIELD_TOTAL_CREDIT = "Total Credit";
    private static final String INPUT_PARAM_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_PARAM_CLASS_TYPE = "classType";
    private static final String INPUT_PARAM_DETAIL_LEVEL = "detail";
    private static final String INPUT_PARAM_FILE_NAME = "fileName";
    private static final String INPUT_PARAM_REPORT_DATE = "reportDate";
    private static final String INPUT_PARAM_SCHOOL_OIDS = "schoolOids";
    private static final List<String> ONSIS_CODES_HIGH_SCHOOL = Arrays.asList("02", "03");

    /**
     * The Enum DetailLevel.
     */
    enum DetailLevel {
        SCHOOL("school") {

            @Override
            protected List<String> getSortColumns() {
                return Arrays.asList(FIELD_SCHOOL_NAME);
            }

            @Override
            protected List<Boolean> getSortOrders() {
                return Arrays.asList(Boolean.TRUE);
            }
        },
        CLASS("class") {

            @Override
            protected List<String> getSortColumns() {
                return Arrays.asList(FIELD_SCHOOL_NAME, FIELD_CLASS);
            }

            @Override
            protected List<Boolean> getSortOrders() {
                return Arrays.asList(Boolean.TRUE, Boolean.TRUE);
            }
        },
        STUDENT("student") {

            @Override
            protected List<String> getSortColumns() {
                return Arrays.asList(FIELD_SCHOOL_NAME, FIELD_CLASS, FIELD_SECTION_NAME, FIELD_STUDENT_NAME);
            }

            @Override
            protected List<Boolean> getSortOrders() {
                return Arrays.asList(Boolean.TRUE, Boolean.TRUE, Boolean.TRUE, Boolean.TRUE);
            }
        };

        private String m_type;

        /**
         * Instantiates a new detail level.
         *
         * @param type String
         */
        DetailLevel(String type) {
            m_type = type;
        }

        /**
         * Gets the type.
         *
         * @return String
         */
        String getType() {
            return m_type;
        }

        /**
         * Sort grid.
         *
         * @param grid the grid
         */
        public void sortGrid(DataGrid grid) {
            grid.sort(getSortColumns(), getSortOrders(), true);

        }

        /**
         * Gets the sort columns.
         *
         * @return the sort columns
         */
        protected List<String> getSortColumns() {
            return null;
        }

        /**
         * Gets the sort orders.
         *
         * @return the sort orders
         */
        protected List<Boolean> getSortOrders() {
            return null;
        }
    }

    /**
     * The Class StudentScheduleSpanInfo.
     */
    private static class StudentScheduleSpanInfo {
        private X2Broker m_broker;
        private BigDecimal m_credit;
        private boolean m_isFullYear;
        private OnSchool m_school;
        private OnSection m_section; // may be MST or CLS
        private OnStudent m_student;

        /**
         * Instantiates a new student schedule span info.
         *
         * @param school SisSchool
         * @param section MasterSchedule
         * @param student SisStudent
         * @param broker X2Broker
         * @param dictionaryExtractor the dictionary extractor
         */
        public StudentScheduleSpanInfo(OnSchool school, OnSection section, OnStudent student, X2Broker broker,
                DictionaryExtractor dictionaryExtractor) {
            m_school = school;
            m_section = section;
            m_student = student;
            m_broker = broker;

            ToolScheduleTerm term = section.getScheduleTerm(broker);
            String termCode = dictionaryExtractor.getStateValue(term, ToolScheduleTerm.FIELD_CODE);
            m_isFullYear = "3".equals(termCode);
            m_credit = m_isFullYear ? section.getCredit().multiply(BIG_DECIMAL_ONE_HALF) : section.getCredit();
        }

        /**
         * Gets the class name.
         *
         * @return String
         */
        protected String getClassName() {
            if (m_section.getSectionClass(m_broker) != null) {
                return m_section.getSectionClass(m_broker).getId();
            }
            return m_section.getCourseView();
        }

        /**
         * Gets the class name.
         *
         * @return String
         */
        protected String getClassStartDate() {
            String dateString = "";
            Collection<ToolScheduleTermDate> termdate = getSection().getScheduleTermDates(m_broker);
            if (!termdate.isEmpty()) {
                dateString = termdate.iterator().next().getStartDate().toString();
            }
            return dateString;
        }

        /**
         * Gets the credit.
         *
         * @return Big decimal
         */
        protected BigDecimal getCredit() {
            return m_credit;
        }

        /**
         * Gets the schedule term.
         *
         * @return the schedule term
         */
        protected String getScheduleTerm() {
            String termCode = "";
            ToolScheduleTerm term = getSection().getScheduleTerm(m_broker);
            if (term != null) {
                termCode = term.getCode();
            }
            return termCode;
        }

        /**
         * Gets the school name.
         *
         * @return String
         */
        protected String getSchoolBsid() {
            return m_school.getBsid();
        }

        /**
         * Gets the school name.
         *
         * @return String
         */
        protected String getSchoolName() {
            return m_school.getName();
        }

        /**
         * Gets the school class.
         *
         * @return X 2 base bean
         */
        protected OnSection getSection() {
            return m_section;
        }

        /**
         * Gets the student.
         *
         * @return Sis student
         */
        protected OnStudent getStudent() {
            return m_student;
        }

        protected boolean isFullYear() {
            return m_isFullYear;
        }
    }

    /**
     * Class members.
     */
    private String m_classType;
    private DistrictSchoolYearContext m_ctxByDate;
    private DetailLevel m_detailLevel = DetailLevel.SCHOOL;
    private DictionaryExtractor m_dictExtractor;
    private boolean m_isAllSchools = true;
    private Boolean m_isOnline;
    private transient LocalizationMessageResources m_defaultMessageResource;
    private PlainDate m_reportDate;
    private List<String> m_schoolOids;
    private Filterable<OnSchool> m_schools;
    private Locale m_userLocale;

    /**
     * Gets the context.
     *
     * @return District school year context
     */
    public DistrictSchoolYearContext getContext() {
        return getContextByDate(getReportDate());
    }

    /**
     * Gets the context by date.
     *
     * @param date PlainDate
     * @return District school year context
     */
    public DistrictSchoolYearContext getContextByDate(PlainDate date) {
        if (m_ctxByDate == null) {
            QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class);
            query.addOrderByDescending(DistrictSchoolYearContext.COL_START_DATE);
            Collection<DistrictSchoolYearContext> contexts = getBroker().getCollectionByQuery(query);
            /*
             * Walk context years in descending order until one of them starts on/before date
             */
            DistrictSchoolYearContext found = null;
            Iterator<DistrictSchoolYearContext> iter = contexts.iterator();
            while (found == null && iter.hasNext()) {
                DistrictSchoolYearContext ctx = iter.next();

                // Check that context starts before date.
                if (date.before(ctx.getStartDate())) {
                    continue;
                }

                /*
                 * Check date is within 370 days of startDate
                 * to detect if we've gone too far (ie no context contains the date)
                 *
                 * Don't compare vs Context End Date
                 * because contexts might not extend until next context start date
                 */
                PlainDate extendedContextEnd = DateUtils.add(ctx.getStartDate(), 370);
                if (date.after(extendedContextEnd)) {
                    break;
                }

                found = ctx;
            }
            m_ctxByDate = found;
        }

        return m_ctxByDate;
    }

    /**
     * Gets the context by year.
     *
     * @param schoolYear int
     * @return Tool district school year context
     */
    public ToolDistrictContext getContextByYear(int schoolYear) {
        return FilterableFactory
                .create(getBroker(), getDictExtractor(), ToolDistrictContext.class, new X2Criteria(), null)
                .extractFirst(ToolDistrictContext.FIELD_SCHOOL_YEAR, schoolYear);
    }

    /**
     * Gets the custom file name.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getCustomFileName()
     */
    @Override
    public String getCustomFileName() {
        String value = super.getCustomFileName();
        if (getParameter(INPUT_PARAM_FILE_NAME) != null) {
            value = getParameter(INPUT_PARAM_FILE_NAME).toString();
        }

        return value;
    }

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        Range<Date> reportDateRange = Range.of(getReportDate(), getReportDate());
        DataGrid grid = new DataGrid();
        new DecimalFormat("######0.00");
        for (OnSchool school : getSchools().extract()) {
            Map<String, StudentScheduleSpanInfo> infos = new HashMap();
            loadStudentsFromSections(school).stream()
                    .filter(student -> student.getEnrollmentSpans(getBroker(), false, false).stream()
                            .anyMatch(span -> !span.isSecondary() && span.getDateRange().contains(getReportDate())))
                    .filter(student -> student.getEnrollmentSpans(getBroker(), false, false).stream()
                            .anyMatch(span -> {
                                // test school
                                if (!school.getOid().equals(span.getSchool().getOid())) {
                                    return false;
                                }

                                // test date range
                                if (!span.getDateRange().contains(getReportDate())) {
                                    return false;
                                }

                                return true;
                            }))
                    .filter(student -> student.getConductActions(getBroker()).isEmpty())
                    .forEach(student -> {
                        List<StudentScheduleSpan> studentSpans = student
                                .getStudentScheduleSpans(getBroker())
                                .stream()
                                .filter(span -> reportDateRange.isOverlap(span.getDateRange()))
                                .sorted((s1, s2) -> s1.getEntryDate().compareTo(s2.getEntryDate()))
                                .collect(Collectors.toList());

                        if (studentSpans != null) {
                            for (StudentScheduleSpan span : studentSpans) {
                                OnSection section = (OnSection) span.getSection();
                                if (span.getSection().getSchedule(getBroker()).getSchoolOid()
                                        .equals(school.getOid())) {
                                    StudentScheduleSpanInfo info = new StudentScheduleSpanInfo(school, section, student,
                                            getBroker(), getDictExtractor());
                                    String key = info.getStudent().getOid() + info.getSection().getOid();
                                    infos.put(key, info);
                                }
                            }
                        }
                    });
            List<List<StudentScheduleSpanInfo>> groupings = infos.values().stream()
                    .collect(Collectors
                            .groupingBy(span -> span.getSchoolName() + span.getClassName() + span.getCredit()))
                    .values().stream().collect(Collectors.toList());
            processGrouping(grid, groupings);
            // .forEach(grouping -> processGrouping(grid, grouping));
        }

        m_detailLevel.sortGrid(grid);
        grid.beforeTop();
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
        List<String> columns = new LinkedList();
        if (m_detailLevel.equals(DetailLevel.SCHOOL)) {
            columns.add(FIELD_SCHOOL_NAME);
            columns.add(FIELD_SCHOOL_BSID);
            columns.add(FIELD_CLASSROOM_CREDIT_SEM);
            columns.add(FIELD_PUPIL_CREDIT_SEM);
            columns.add(FIELD_CLASSROOM_CREDIT_FY);
            columns.add(FIELD_PUPIL_CREDIT_FY);
        } else if (m_detailLevel.equals(DetailLevel.CLASS)) {
            columns.add(FIELD_SCHOOL_NAME);
            columns.add(FIELD_SCHOOL_BSID);
            columns.add(FIELD_CLASS);
            columns.add(FIELD_CLASS_START_DATE);
            columns.add(FIELD_CLASS_TYPE);
            columns.add(FIELD_SCHEDULE_TERM);
            columns.add(FIELD_TOTAL_CLASS_STUDENTS);
            columns.add(FIELD_TOTAL_CREDIT);
            columns.add(FIELD_CLASSROOM_CREDIT);
            columns.add(FIELD_PUPIL_CREDIT);
        } else if (m_detailLevel.equals(DetailLevel.STUDENT)) {
            columns.add(FIELD_SCHOOL_NAME);
            columns.add(FIELD_SCHOOL_BSID);
            columns.add(FIELD_CLASS);
            columns.add(FIELD_CLASS_TYPE);
            columns.add(FIELD_SECTION_NAME);
            columns.add(FIELD_OEN);
            columns.add(FIELD_STUDENT_NAME);
            columns.add(FIELD_CREDIT);
        }
        return columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        List<String> columns = new LinkedList();
        LocalizationMessageResources messageResource = getMessageResource();
        if (m_detailLevel.equals(DetailLevel.SCHOOL)) {
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_SCHOOL_NAME));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_SCHOOL_BSID));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_CLASSROOM_CREDIT_SEM));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_PUPIL_CREDIT_SEM));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_CLASSROOM_CREDIT_FY));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_PUPIL_CREDIT_FY));
        } else if (m_detailLevel.equals(DetailLevel.CLASS)) {
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_SCHOOL_NAME));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_SCHOOL_BSID));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_CLASS));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_CLASS_START_DATE));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_CLASS_TYPE));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_SCHEDULE_TERM));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_TOTAL_CLASS_STUDENTS));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_TOTAL_CREDIT));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_CLASSROOM_CREDIT));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_PUPIL_CREDIT));
        } else if (m_detailLevel.equals(DetailLevel.STUDENT)) {
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_SCHOOL_NAME));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_SCHOOL_BSID));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_CLASS));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_CLASS_TYPE));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_SECTION_NAME));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_OEN));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_STUDENT_NAME));
            columns.add(messageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_CREDIT));
        }
        return columns;
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
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        ToolBean.setBroker(getBroker());
        ToolBean.setDictionaryExtractor(getDictExtractor());

        ToolBean.setPreference(ToolBean.PREFERENCE_HISTORICAL_CUTOFF_DATE,
                getContextByYear(getContext().getSchoolYear() - 1).getStartDate());
        ToolBean.setPreference(ToolBean.PREFERENCE_QUERY_AS_OF_DATE, getReportDate());

        ToolBean.registerClass(OnSchedule.class);
        ToolBean.registerClass(OnSchool.class);
        ToolBean.registerClass(OnSection.class);
        ToolBean.registerClass(OnStudent.class);
        ToolBean.registerClass(OnEnrollment.class);
        ToolBean.registerClass(OnStudentSchool.class);
        ToolBean.registerClass(OnTranscript.class);
        ToolBean.registerClass(OnConductAction.class);
        ToolBean.registerClass(OnTranscriptColumnDefinition.class);

        Boolean isAllSchools = (Boolean) getParameter(INPUT_PARAM_ALL_SCHOOLS);
        if (isAllSchools != null) {
            m_isAllSchools = isAllSchools;
        }
        String detailLevel = (String) getParameter(INPUT_PARAM_DETAIL_LEVEL);
        if (detailLevel != null) {
            for (DetailLevel level : DetailLevel.values()) {
                if (level.getType().equals(detailLevel)) {
                    m_detailLevel = level;
                    break;
                }
            }
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_userLocale = userData.getLocale();
    }

    /**
     * Double format.
     *
     * @param formatter the formatter
     * @param d the d
     * @return the string
     */
    private String doubleFormat(DecimalFormat formatter, double d) {
        String value = "";
        if (!Double.isNaN(d)) {
            value = formatter.format(d);
        }
        return value;
    }

    /**
     * Gets the class type.
     *
     * @return the class type
     */
    private String getClassType() {
        if (m_classType == null) {
            LocalizationMessageResources messageResource = getMessageResource();
            m_classType = isOnline() ? messageResource.getMessage(CONSTANT_RESOURCE_KEY_ONLINE)
                    : messageResource.getMessage(CONSTANT_RESOURCE_KEY_NON_ONLINE);
        }
        return m_classType;
    }

    /**
     * Gets the dict extractor.
     *
     * @return Dictionary extractor
     */
    private DictionaryExtractor getDictExtractor() {
        if (m_dictExtractor == null) {
            m_dictExtractor = new DictionaryExtractor(getBroker());
        }
        return m_dictExtractor;
    }

    /**
     * Gets the message resource.
     *
     * @return the message resource
     */
    private LocalizationMessageResources getMessageResource() {
        if (m_defaultMessageResource == null) {
            try {
                m_defaultMessageResource =
                        LocalizationCache.getMessages(getBroker().getPersistenceKey(), m_userLocale);
            } catch (Exception e) {
                m_defaultMessageResource = LocalizationCache.getMessages(getBroker().getPersistenceKey(),
                        LocalizationCache.getCurrentLocale());
            }
        }
        return m_defaultMessageResource;
    }

    /**
     * Gets the report date.
     *
     * @return Plain date
     */
    private PlainDate getReportDate() {
        if (m_reportDate == null) {
            m_reportDate = getParameter(INPUT_PARAM_REPORT_DATE) == null
                    ? new PlainDate(OrganizationManager.getTimeZone(getOrganization()))
                    : (PlainDate) getParameter(INPUT_PARAM_REPORT_DATE);
        }
        return m_reportDate;
    }

    /**
     * Gets the school oids.
     *
     * @return List
     */
    private List<String> getSchoolOids() {
        if (m_schoolOids == null) {
            String schoolOids = (String) getParameter(INPUT_PARAM_SCHOOL_OIDS);
            m_schoolOids = Arrays.asList(schoolOids.split(","));
        }
        return m_schoolOids;
    }

    /**
     * Gets the schools.
     *
     * @return Filterable
     */
    private Filterable<OnSchool> getSchools() {
        if (m_schools == null) {
            X2Criteria schoolCriteria = new X2Criteria();
            schoolCriteria.addNotEqualTo(ToolSchool.FIELD_INACTIVE_INDICATOR.resolve(null), Boolean.TRUE);
            schoolCriteria.addNotEqualTo(ToolSchool.FIELD_ARCHIVE_INDICATOR.resolve(null), Boolean.TRUE);
            m_schools = FilterableFactory
                    .create(getBroker(), OnSchool.class, schoolCriteria,
                            Arrays.asList(OnSchool.FIELD_NAME, OnSchool.FIELD_OID))
                    .filter(new Filter() {
                        @Override
                        public boolean isFiltered(Object toFilter) {
                            OnSchool school = (OnSchool) toFilter;
                            return (m_isAllSchools || getSchoolOids().contains(school.getOid()))
                                    && isHighSchool(school);
                        }
                    });
        }
        return m_schools;
    }


    /**
     * Load students from sections.
     *
     * @param school the school
     * @return the collection
     */
    private Collection<OnStudent> loadStudentsFromSections(OnSchool school) {
        ToolBean.clearAllCachedToolBeans(OnEnrollment.class);
        ToolBean.clearAllCachedToolBeans(OnSection.class);
        ToolBean.clearAllCachedToolBeans(OnStudent.class);
        ToolBean.clearAllCachedToolBeans(OnStudentSchool.class);
        ToolBean.clearAllCachedToolBeans(OnTranscript.class);
        ToolBean.clearAllCachedToolBeans(OnConductAction.class);
        ToolBean.clearAllCachedToolBeans(ToolMasterTerm.class);
        ToolBean.clearAllCachedToolBeans(ToolScheduleClass.class);
        ToolBean.clearAllCachedToolBeans(ToolStudentSchedule.class);
        ToolBean.clearAllCachedToolBeans(ToolStudentScheduleChange.class);

        ToolBean.resetCriteria(getBroker(), ToolStudentSchedule.class);
        ToolBean.resetCriteria(getBroker(), ToolStudentScheduleChange.class);
        ToolBean.resetCriteria(getBroker(), OnTranscript.class);
        ToolBean.resetCriteria(getBroker(), OnConductAction.class);

        X2Criteria sectionCriteria = ToolBean.getCriteria(getBroker(), OnSection.class).copy();

        // From active Schedule for the selected year.
        sectionCriteria.addEqualTo(SisBeanPaths.SCHEDULE_MASTER.schedule().activeSchoolScheduleContexts()
                .districtContextOid().getPath(), getCurrentContext().getOid());

        // Require section term to start before report date.
        sectionCriteria.addLessOrEqualThan(
                SisBeanPaths.SCHEDULE_MASTER.scheduleTerm().scheduleTermDates().startDate().getPath(),
                getReportDate());

        // Require section term to end after submission period start date.
        sectionCriteria.addGreaterOrEqualThan(
                SisBeanPaths.SCHEDULE_MASTER.scheduleTerm().scheduleTermDates().endDate().getPath(),
                getReportDate());

        // Limit to selected schools
        sectionCriteria.addEqualTo(SisBeanPaths.SCHEDULE_MASTER.schedule().schoolOid().getPath(), school.getOid());

        X2Criteria inclusionCriteria = new X2Criteria();

        // exclude cskCourseDelivery 2 and 21
        List<String> courseDeliveryCodes = getDictExtractor().getRefCodesWithStateValue(
                OnSection.FIELD_COURSE_DELIVERY_TYPE_CSK.getField(getDictExtractor()),
                Arrays.asList(OnSection.COURSE_DELIVERY_TYPE_INDEPENDENT_STUDY,
                        OnSection.COURSE_DELIVERY_TYPE_DC_ON_LINE))
                .stream()
                .map(code -> code.getCode())
                .collect(Collectors.toList());
        inclusionCriteria.addNotIn(OnSection.FIELD_COURSE_DELIVERY_TYPE_CSK.resolve(getDictExtractor()),
                courseDeliveryCodes);

        // add class type selection
        List<String> onsisClassTypeCodes = isOnline() ? Arrays.asList("E") : Arrays.asList("OTH", "R", "RCR");
        X2Criteria classTypeCriteria = new X2Criteria();
        List<String> classTypeCodes = getDictExtractor()
                .getRefCodesWithStateValue(
                        OnSection.FIELD_CLASS_TYPE_CSK.getField(getDictExtractor()),
                        onsisClassTypeCodes)
                .stream()
                .map(code -> code.getCode())
                .collect(Collectors.toList());

        classTypeCriteria.addIn(OnSection.FIELD_CLASS_TYPE.resolve(getDictExtractor()), classTypeCodes);
        X2Criteria classTypeCriteriaOr = new X2Criteria();
        classTypeCriteriaOr.addEmpty(OnSection.FIELD_CLASS_TYPE.resolve(getDictExtractor()),
                getBroker().getPersistenceKey());
        classTypeCriteriaOr.addIn(OnSection.FIELD_CLASS_TYPE_CSK.resolve(getDictExtractor()), classTypeCodes);
        classTypeCriteria.addOrCriteria(classTypeCriteriaOr);
        inclusionCriteria.addAndCriteria(classTypeCriteria);

        // add school level selection
        List<String> schoolLevelCodes = getDictExtractor().getRefCodesWithStateValue(
                OnSection.FIELD_SCHOOL_LEVEL.getField(getDictExtractor()),
                Arrays.asList("02"))
                .stream()
                .map(code -> code.getCode())
                .collect(Collectors.toList());
        inclusionCriteria.addIn(OnSection.FIELD_SCHOOL_LEVEL.resolve(getDictExtractor()), schoolLevelCodes);

        // all-crs-CourseCodeType
        List<String> courseCodeTypeCodes = getDictExtractor()
                .getRefCodesWithStateValue(
                        OnSection.FIELD_COURSE_CODE_TYPE.getField(getDictExtractor()),
                        OnSection.COURSE_CODE_TYPES_MDC_DCC_LDC)
                .stream()
                .map(code -> code.getCode()).collect(Collectors.toList());
        inclusionCriteria.addIn(OnSection.FIELD_COURSE_CODE_TYPE.resolve(getDictExtractor()), courseCodeTypeCodes);

        // course code populated and not begins with K
        inclusionCriteria.addNotEmpty(OnSection.FIELD_MINISTRY_COURSE_CODE.resolve(getDictExtractor()),
                getBroker().getPersistenceKey());
        inclusionCriteria.addNotLike(OnSection.FIELD_MINISTRY_COURSE_CODE.resolve(getDictExtractor()), "K%");

        // credit must be > 0
        inclusionCriteria.addGreaterThan(OnSection.FIELD_CSK_CREDIT.resolve(getDictExtractor()), BigDecimal.ZERO);

        // onsis exclude flag
        inclusionCriteria.addNotEqualTo(OnSection.FIELD_MST_EXCLUDE_FROM_ONSIS.resolve(getDictExtractor()),
                BooleanAsStringConverter.TRUE);

        // add term code inclusion
        List<String> termCodes = getDictExtractor()
                .getRefCodesWithStateValue(
                        ToolScheduleTerm.FIELD_CODE.getField(getDictExtractor()),
                        Arrays.asList("1", "2", "3", "4"))
                .stream()
                .map(code -> code.getCode()).collect(Collectors.toList());
        inclusionCriteria.addIn(SisBeanPaths.SCHEDULE_MASTER.scheduleTerm().code().getPath(), termCodes);

        sectionCriteria.addAndCriteria(inclusionCriteria);

        FilterableFactory.create(getBroker(), getDictExtractor(), OnSection.class, sectionCriteria,
                Arrays.asList(ToolSection.FIELD_CRS_NUMBER, ToolBean.FIELD_OID));

        ToolBean.preload(getBroker(), getDictExtractor(), null, ToolMasterTerm.PARENT_SECTION);

        Set<String> scheduleClassOids =
                ToolBean.getCachedToolBeans(OnSection.class).stream()
                        .map(section -> section.getSectionClassOid()).collect(Collectors.toSet());
        if (!scheduleClassOids.isEmpty()) {
            X2Criteria classCriteria = new X2Criteria();
            CollectionCriteriaHelper helper = null;
            if (scheduleClassOids.size() > ToolBean.MAX_SAFE_PARAMETERS) {
                helper = new CollectionCriteriaHelper(scheduleClassOids, getBroker());
                helper.applyToCriteria(SisBeanPaths.SCHEDULE_CLASS.oid().getPath(), classCriteria);
            } else {
                classCriteria.addIn(SisBeanPaths.SCHEDULE_CLASS.oid().getPath(), scheduleClassOids);
            }
            FilterableFactory.create(getBroker(), getDictExtractor(), ToolScheduleClass.class,
                    classCriteria, null);
            if (helper != null) {
                helper.cleanup();
            }
        }

        List<String> schoolOids = Arrays.asList(school.getOid());
        X2Criteria studentLimitingCriteria = new X2Criteria();
        studentLimitingCriteria.addNotEmpty(OnStudent.FIELD_OEN.resolve(getDictExtractor()),
                getBroker().getPersistenceKey());
        EnrollmentSpanCriteria spanCriteria = new EnrollmentSpanCriteria()
                .setSchoolOids(schoolOids)
                .setExcludeStudent(OnStudent.FIELD_EXCLUDE_FROM_REPORTING)
                .setExcludeSection(OnSection.FIELD_MST_EXCLUDE_FROM_ONSIS)
                .setCurrentContext(getCurrentContext())
                .setIncludeSecondarySpans(true)
                .setStudentLimitingCriteria(studentLimitingCriteria)
                .setSectionLimitingCriteria(inclusionCriteria);

        ToolBean.addAndCriteria(getBroker(), ToolStudentSchedule.class,
                CriteriaHelper.buildStudentScheduleCriteria(spanCriteria));
        ToolBean.addAndCriteria(getBroker(), ToolStudentScheduleChange.class,
                CriteriaHelper.buildStudentScheduleChangeCriteria(spanCriteria));
        ToolBean.preload(getBroker(), getDictExtractor(), null, ToolStudentSchedule.PARENT_SECTION);
        ToolBean.preload(getBroker(), getDictExtractor(), null, ToolStudentScheduleChange.PARENT_SECTION);

        List<String> studentOids = Stream
                .concat(ToolBean.getCachedToolBeans(ToolStudentSchedule.class).stream().map(ssc -> ssc.getStudentOid()),
                        ToolBean.getCachedToolBeans(ToolStudentScheduleChange.class).stream()
                                .map(scc -> scc.getStudentOid()))
                .distinct()
                .collect(Collectors.toList());
        if (!studentOids.isEmpty()) {
            X2Criteria classCriteria = new X2Criteria();
            CollectionCriteriaHelper helper = null;
            if (studentOids.size() > ToolBean.MAX_SAFE_PARAMETERS) {
                helper = new CollectionCriteriaHelper(studentOids, getBroker());
                helper.applyToCriteria(SisBeanPaths.STUDENT.oid().getPath(), classCriteria);
            } else {
                classCriteria.addIn(SisBeanPaths.STUDENT.oid().getPath(), studentOids);
            }
            FilterableFactory.create(getBroker(), getDictExtractor(), OnStudent.class,
                    classCriteria, null);
            if (helper != null) {
                helper.cleanup();
            }
        }
        ToolBean.reload(getBroker(), getDictExtractor(), null,
                ToolStudentScheduleChange.PARENT_STUDENT.setLoaderMode(LoaderMode.CLEAR));
        ToolBean.reload(getBroker(), getDictExtractor(), null,
                ToolStudentSchedule.PARENT_STUDENT.setLoaderMode(LoaderMode.CLEAR));

        ToolBean.preload(getBroker(), getDictExtractor(),
                Arrays.asList(ToolEnrollment.FIELD_DATE_DESC, ToolEnrollment.FIELD_TIMESTAMP_DESC),
                ToolStudent.CHILD_STUDENT_ENROLLMENTS);
        ToolBean.preload(getBroker(), getDictExtractor(),
                Arrays.asList(ToolStudentSchool.FIELD_START_DATE_DESC, ToolBean.FIELD_OID),
                ToolStudent.CHILD_STUDENT_SCHOOLS);

        // preload conduct action matching criteria for restriction
        X2Criteria actionCriteria = new X2Criteria();
        List<String> actionCodes = getDictExtractor()
                .getRefCodesWithStateValue(
                        OnConductAction.FIELD_ACTION_CODE.getField(getDictExtractor()),
                        Arrays.asList("1", "4", "5"))
                .stream()
                .map(code -> code.getCode()).collect(Collectors.toList());
        actionCriteria.addIn(OnConductAction.FIELD_ACTION_CODE.resolve(getDictExtractor()), actionCodes);

        List<String> suspensionProgramCodes = getDictExtractor()
                .getRefCodesWithStateValue(
                        OnConductAction.FIELD_SUSPENSION_PROGRAM.getField(getDictExtractor()),
                        Arrays.asList("01", "02"))
                .stream()
                .map(code -> code.getCode()).collect(Collectors.toList());
        actionCriteria.addIn(OnConductAction.FIELD_SUSPENSION_PROGRAM.resolve(getDictExtractor()),
                suspensionProgramCodes);
        actionCriteria.addLessOrEqualThan(OnConductAction.FIELD_ACTION_START_DATE.resolve(getDictExtractor()),
                getReportDate());
        actionCriteria.addGreaterOrEqualThan(OnConductAction.FIELD_ACTION_END_DATE.resolve(getDictExtractor()),
                getReportDate());
        actionCriteria.addGreaterThan(ToolConductAction.FIELD_ACTION_PENALTY_TIME.resolve(getDictExtractor()),
                BigDecimal.valueOf(5.0));
        ToolBean.addAndCriteria(getBroker(), OnConductAction.class, actionCriteria);
        ToolBean.preload(getBroker(), getDictExtractor(), null, ToolConductAction.PARENT_STUDENT);


        // preload transcripts with no records
        X2Criteria noTranscripts = new X2Criteria();
        noTranscripts.addIsNull(ToolBean.FIELD_OID.resolve(getDictExtractor()));
        ToolBean.addAndCriteria(getBroker(), OnTranscript.class, noTranscripts);
        ToolBean.preload(getBroker(), getDictExtractor(), null, ToolTranscript.PARENT_STUDENT);

        DistrictManager.setAnnualSpanFactory(new OnAnnualSpanFactory(getBroker()));
        DistrictManager.setStudentScheduleSpanFactory(new OnStudentScheduleSpanFactory());

        return ToolBean.getCachedToolBeans(OnStudent.class);
    }

    /**
     * Checks if is day school.
     *
     * @param school SisSchool
     * @return true, if is day school
     */
    private boolean isHighSchool(OnSchool school) {
        return ONSIS_CODES_HIGH_SCHOOL.contains(school.getSchoolLevelCodeState());
    }

    /**
     * Checks if is online.
     *
     * @return true, if is online
     */
    private boolean isOnline() {
        if (m_isOnline == null) {
            String classType = (String) this.getParameter(INPUT_PARAM_CLASS_TYPE);
            m_isOnline = Boolean.valueOf(!StringUtils.isEmpty(classType) && classType.equals(CLASS_TYPE_ONLINE));
        }
        return m_isOnline.booleanValue();
    }

    /**
     * Process grouping.
     *
     * @param grid the grid
     * @param groupings the groupings
     */
    private void processGrouping(DataGrid grid, List<List<StudentScheduleSpanInfo>> groupings) {
        DecimalFormat formatter = new DecimalFormat("######0.00");
        double totalFYClassroomCredit = 0.0;
        double totalFYPupilCredit = 0.0;
        double totalSemClassroomCredit = 0.0;
        double totalSemPupilCredit = 0.0;
        StudentScheduleSpanInfo firstElement = null;

        for (List<StudentScheduleSpanInfo> grouping : groupings) {
            BigDecimal totalCredit = BigDecimal.ZERO;
            int totalStudents = 0;
            firstElement = grouping.iterator().next();
            for (StudentScheduleSpanInfo info : grouping) {
                ++totalStudents;
                totalCredit = totalCredit.add(info.getCredit());
                if (m_detailLevel.equals(DetailLevel.STUDENT)) {
                    grid.append();
                    grid.set(FIELD_SCHOOL_NAME, firstElement.getSchoolName());
                    grid.set(FIELD_SCHOOL_BSID, firstElement.getSchoolBsid());
                    grid.set(FIELD_CLASS, firstElement.getClassName());
                    grid.set(FIELD_CLASS_TYPE, getClassType());
                    grid.set(FIELD_SECTION_NAME, info.getSection().getCourseView());
                    grid.set(FIELD_OEN, info.getStudent().getOen());
                    grid.set(FIELD_STUDENT_NAME,
                            info.getStudent().getNameView());
                    grid.set(FIELD_CREDIT, doubleFormat(formatter, info.getCredit().doubleValue()));
                }
            }
            double classroomCredit = totalCredit.doubleValue() / (totalStudents);
            double pupilCredit = classroomCredit * totalStudents;
            if (m_detailLevel.equals(DetailLevel.CLASS)) {
                grid.append();
                grid.set(FIELD_SCHOOL_NAME, firstElement.getSchoolName());
                grid.set(FIELD_SCHOOL_BSID, firstElement.getSchoolBsid());
                grid.set(FIELD_CLASS, firstElement.getClassName());
                grid.set(FIELD_CLASS_START_DATE, firstElement.getClassStartDate());
                grid.set(FIELD_CLASS_TYPE, getClassType());
                grid.set(FIELD_SCHEDULE_TERM, firstElement.getScheduleTerm());
                grid.set(FIELD_TOTAL_CLASS_STUDENTS, Integer.toString(totalStudents));
                grid.set(FIELD_TOTAL_CREDIT, doubleFormat(formatter, totalCredit.doubleValue()));
                grid.set(FIELD_CLASSROOM_CREDIT, doubleFormat(formatter, classroomCredit));
                grid.set(FIELD_PUPIL_CREDIT, doubleFormat(formatter, pupilCredit));
            }

            if (firstElement.isFullYear()) {
                totalFYClassroomCredit += classroomCredit;
                totalFYPupilCredit += pupilCredit;
            } else {
                totalSemClassroomCredit += classroomCredit;
                totalSemPupilCredit += pupilCredit;
            }
        }

        if (firstElement != null) {
            if (m_detailLevel.equals(DetailLevel.SCHOOL)) {
                grid.append();
                grid.set(FIELD_SCHOOL_NAME, firstElement.getSchoolName());
                grid.set(FIELD_SCHOOL_BSID, firstElement.getSchoolBsid());
                grid.set(FIELD_CLASSROOM_CREDIT_SEM, doubleFormat(formatter, totalSemClassroomCredit));
                grid.set(FIELD_PUPIL_CREDIT_SEM, doubleFormat(formatter, totalSemPupilCredit));
                grid.set(FIELD_CLASSROOM_CREDIT_FY, doubleFormat(formatter, totalFYClassroomCredit));
                grid.set(FIELD_PUPIL_CREDIT_FY, doubleFormat(formatter, totalFYPupilCredit));
            }
        }
    }

}
