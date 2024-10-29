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
package com.x2dev.reports.statereporting.on.revised;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.OrganizationLocale;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filter;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnEnrollment;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.SubmissionType;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * The Class OnReportJavaSource.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class OnReportJavaSource extends ReportJavaSourceNet {

    /**
     * The Interface FieldInterface.
     */
    public interface FieldInterface {
        //
    }

    /**
     * The Interface RecordInterface.
     */
    public interface RecordInterface {

        /**
         * Gets the.
         *
         * @param field FieldInterface
         * @return Object
         */
        public Object get(FieldInterface field);

        /**
         * Sets the.
         *
         * @param field FieldInterface
         * @param value Object
         */
        public void set(FieldInterface field, Object value);
    }

    /**
     * The Class DataErrorException.
     */
    public static class DataErrorException extends RuntimeException {

        /**
         * Instantiates a new data error exception.
         *
         * @param errorMessage the error message
         */
        public DataErrorException(String errorMessage) {
            super(errorMessage);
        }
    }

    public static final String ALIAS_DDX_NUM_GRADE = "NumericGradeLevel";
    public static final String ALIAS_RCD_BOARD_NUMBER = "rcd-bsid-board-number";
    public static final String BOARD_RES_STATUS_ELEARNING = "09";
    public static final String BOARD_RES_STATUS_INDEPENDENT = "10";
    public static final String BOARD_RES_STATUS_INDEPENDENT_STUDY = "06";
    public static final String BOARD_RES_STATUS_OTHER_CANADA_GOV = "03";
    public static final String BOARD_RES_STATUS_PUPIL_OF_THE_BOARD = "01";
    public static final String BOARD_RES_STATUS_SHARED = "08";
    public static final String DDX_ID_GRADES = "REF-GRADE-LEVELS";
    public static final String DDX_ID_REF_BSID_SCHOOL = "REF-School-BSID";
    public static final String EMPTY_REPORT_ID = "ON-EMPTY-RPT";
    public static final Float FTE_FT_FINAL_THRESHOLD = 0.7F;
    public static final String INPUT_PARAM_ALL_SCHOOLS = "allSchools";
    public static final String INPUT_PARAM_REPORT_DATE = "reportDate";
    public static final String INPUT_PARAM_SCHOOL_OIDS = "schoolOids";
    public static final String INPUT_PARAM_SUBMISSION_TYPE = "submissionType";
    public static final String PARAMETER_NAME_ERRORS_LOG = "errorsLog";
    public static final String REF_TABLE_NAME_BSID_BOARD = "BSID - Boards";
    public static final String REF_TABLE_NAME_BSID_SCHOOL = "BSID - Schools";
    public static final String REPORT_PARAM_AS_OF_DATE = "asOfDate";
    public static final String REPORT_PARAM_VERSION = "version";
    public static final String TOPIC_SCHOOL_STUDENT_SUBMISSION = "SchoolStudent";
    public static final String ZERO = "0.00";

    public static final SimpleDateFormat s_aliasDateFormat = new SimpleDateFormat("yyyy-MM-dd");
    public static final SimpleDateFormat s_asOfDateFormat = new SimpleDateFormat("yyyy/MM/dd");


    // Localization
    private Map<String, String> m_validLocales;
    private String m_defaultLocale; // Usually English
    private transient MessageResources m_default_message_resource;
    private Locale m_user_locale;
    private static final String PARAM_PREFIX = "prefix";
    private static final String PARAM_REPORT_LOCALE = "reportLocale";
    private static final String PARAM_LOCALES = "locales";
    private static final String PARAM_DEFAULT_LOCALE = "default_locale";
    private static final String PARAM_DEFAULT_LANGUAGE = "default_language";
    private static final String CONST_TOOLS_FOR_PREFIX = "tools.";
    private static final String CONST_AMERICAN_ENGLISH_LOCALE = "en_US";
    private static final String CONST_AMERICAN_ENGLISH_LANGUAGE = "English";


    private PlainDate m_december31;
    private DictionaryExtractor m_dictExtractor;
    private StringBuilder m_errorsLog = new StringBuilder();
    private boolean m_isAllSchools = false;
    private Map<String, Filterable<ReferenceCode>> m_refCodesFilterables = new HashMap<>();
    private PlainDate m_reportDate;
    private List<String> m_schoolOids;
    private Filterable<OnSchool> m_schools;
    private SubmissionType m_submissionType;


    /**
     * Filter by fields.
     *
     * @param <T> the generic type
     * @param <E> the element type
     * @param fields List<FteRecord.Field>
     * @param record FteRecord
     * @param filterable Filterable<FteRecord>
     * @return Filterable
     */
    public <T extends RecordInterface, E extends FieldInterface> Filterable<T> filterByFields(List<E> fields,
                                                                                              T record,
                                                                                              Filterable<T> filterable) {
        return filterable.filter(
                fields.stream().map(FieldInterface::toString).collect(Collectors.toList()),
                fields.stream().map(field -> record.get(field)).collect(Collectors.toList()));
    }

    /**
     * Gets the board by school number.
     *
     * @param schoolNumber String
     * @return Reference code
     */
    public ReferenceCode getBoardBySchoolNumber(String schoolNumber) {
        ReferenceCode boardRefCode = null;
        ReferenceCode schoolRefCode =
                getRefCodesFilterable(REF_TABLE_NAME_BSID_SCHOOL).extractFirst(ReferenceCode.COL_CODE, schoolNumber);
        if (schoolRefCode != null) {
            String boardNumber = (String) schoolRefCode.getFieldValueByAlias(ALIAS_RCD_BOARD_NUMBER,
                    getDictExtractor().getDictionary(DDX_ID_REF_BSID_SCHOOL));
            boardRefCode =
                    getRefCodesFilterable(REF_TABLE_NAME_BSID_BOARD).extractFirst(ReferenceCode.COL_CODE, boardNumber);
        }
        return boardRefCode;
    }

    /**
     * Gets the december 31.
     *
     * @return Plain date
     */
    public PlainDate getDecember31() {
        if (m_december31 == null) {
            Calendar calendar = Calendar.getInstance();
            calendar.set(Calendar.MONTH, Calendar.DECEMBER);
            calendar.set(Calendar.DAY_OF_MONTH, 31);
            calendar.set(Calendar.YEAR, getCurrentContext().getSchoolYear() - 1);
            m_december31 = new PlainDate(calendar.getTime());
        }
        return m_december31;
    }

    /**
     * Gets the dict extractor.
     *
     * @return Dictionary extractor
     */
    public DictionaryExtractor getDictExtractor() {
        if (m_dictExtractor == null) {
            m_dictExtractor = new DictionaryExtractor(getBroker());
        }
        return m_dictExtractor;
    }

    /**
     * Gets the dictionary.
     *
     * @return Data dictionary
     */
    public DataDictionary getDictionary() {
        return DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
    }

    /**
     * Gets the empty report id.
     *
     * @return the empty report id
     */
    public String getEmptyReportId() {
        return EMPTY_REPORT_ID;
    }

    /**
     * Gets the errors log.
     *
     * @return String builder
     */
    public StringBuilder getErrorsLog() {
        return m_errorsLog;
    }

    /**
     * Gets the field by alias.
     *
     * @param alias String
     * @return Data dictionary field
     */
    public DataDictionaryField getFieldByAlias(String alias) {
        return getDictionary().findDataDictionaryFieldByAlias(alias);
    }

    /**
     * Gets the historical cutoff date.
     *
     * @return Plain date
     */
    public PlainDate getHistoricalCutoffDate() {
        PlainDate reportDate = getReportDate();

        if (reportDate != null) {
            DistrictSchoolYearContext dateContext =
                    FilterableFactory.create(getBroker(), DistrictSchoolYearContext.class)
                            .extractFirst(new Filter<DistrictSchoolYearContext>() {

                                @Override
                                public boolean isFiltered(DistrictSchoolYearContext ctx) {
                                    return !reportDate.before(ctx.getStartDate())
                                            && !reportDate.after(ctx.getEndDate());
                                }
                            });
            if (dateContext != null) {
                return dateContext.getStartDate();
            }
        } else {
            return getCurrentContext().getStartDate();
        }
        return reportDate;
    }

    /**
     * Gets the ref code.
     *
     * @param bean X2BaseBean
     * @param field DataDictionaryField
     * @return Reference code
     */
    public ReferenceCode getRefCode(X2BaseBean bean, DataDictionaryField field) {
        ReferenceTable refTable = field.getReferenceTable();
        if (refTable == null) {
            throw new RuntimeException("Cannot find reference table for field [" + field.getJavaName() + "]");
        }
        Filterable<ReferenceCode> codes = getRefCodesFilterable(refTable.getUserName());
        Object value = bean.getFieldValueByBeanPath(field.getJavaName());
        if (value == null) {
            return null;
        }
        return codes.extractFirst(ReferenceCode.COL_CODE, value);
    }

    /**
     * Gets the report date.
     *
     * @return Plain date
     */
    public PlainDate getReportDate() {
        if (m_reportDate == null) {
            m_reportDate = (PlainDate) getParameter(INPUT_PARAM_REPORT_DATE);
        }
        return m_reportDate;
    }

    /**
     * Gets the school oids.
     *
     * @return List
     */
    public List<String> getSchoolOids() {
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
    public Filterable<OnSchool> getSchools() {
        if (m_schools == null) {
            m_schools = FilterableFactory.create(ToolBean.getBroker(true), ToolBean.getDictionaryExtractor(),
                    OnSchool.class, new X2Criteria(), null)
                    .filter(new Filter<OnSchool>() {
                        @Override
                        public boolean isFiltered(OnSchool school) {
                            return !StringUtils.isEmpty(school.getBsid())
                                    && (m_isAllSchools || getSchoolOids().contains(school.getOid()));
                        }
                    });
        }
        return m_schools;
    }

    /**
     * Gets the span configuration.
     *
     * @param asOfDate PlainDate
     * @return Span configuration
     */
    public EnrollmentSpanCriteria getSpanConfiguration(PlainDate asOfDate) {
        ToolBean.setPreference(ToolBean.PREFERENCE_HISTORICAL_CUTOFF_DATE, getHistoricalCutoffDate());
        ToolBean.setPreference(ToolBean.PREFERENCE_QUERY_AS_OF_DATE, asOfDate);
        EnrollmentSpanCriteria spanConfig = new EnrollmentSpanCriteria()
                .setExcludeStudent(OnStudent.FIELD_EXCLUDE_FROM_REPORTING);
        return spanConfig;
    }

    /**
     * Gets the full student name (First Middle Last).
     *
     * @param student the student
     * @return String
     */
    public String getStudentNameFirstMiddleLast(OnStudent student) {
        return String.join(" ", Arrays.asList(
                student.getLegalFirstName(),
                student.getLegalMiddleName(),
                student.getLegalLastName())
                .stream()
                .filter(it -> !StringUtils.isBlank(it))
                .collect(Collectors.toList()));
    }

    /**
     * Gets the student last name.
     *
     * @param student the student
     * @return String
     */
    public String getStudentNameLast(OnStudent student) {
        return student.getLegalLastName();
    }

    /**
     * Gets the full student name (Last, First Middle).
     *
     * @param student the student
     * @return String
     */
    public String getStudentNameLastFirstMiddle(OnStudent student) {
        return String.join(" ", Arrays.asList(
                student.getLegalLastName() + ",",
                student.getLegalFirstName(),
                student.getLegalMiddleName())
                .stream()
                .filter(it -> !StringUtils.isBlank(it))
                .collect(Collectors.toList()));
    }

    /**
     * Gets the submission type.
     *
     * @return Submission type
     */
    public SubmissionType getSubmissionType() {
        if (m_submissionType == null) {
            String submissionOid = (String) getParameter(INPUT_PARAM_SUBMISSION_TYPE);
            m_submissionType =
                    ToolBean.getBeanByOid(getBroker(), m_dictExtractor, SubmissionType.class, submissionOid, false);
        }
        return m_submissionType;
    }

    /**
     * Gets the user locale.
     *
     * @return the user locale
     */
    public Locale getUserLocale() {
        return m_user_locale;
    }

    /**
     * Gets the user message key prefix.
     *
     * @return the user message key prefix
     */
    public String getUserMessageKeyPrefix() {
        return CONST_TOOLS_FOR_PREFIX + getJob().getTool().getOid() + ".";
    }

    /**
     * Gets the user message resources.
     *
     * @return the user message resources
     */
    public MessageResources getUserMessageResources() {
        return m_default_message_resource;
    }

    /**
     * Gets the yog.
     *
     * @param student SisStudent
     * @param enrollment StudentEnrollment
     * @return int
     */
    public int getYog(OnStudent student, OnEnrollment enrollment) {
        int yog = student.getYog();
        if (enrollment != null) {
            yog = enrollment.getYog();
        }
        return yog;
    }

    /**
     * Checks if is 21 plus.
     *
     * @param student the student
     * @return true, if is 21 plus
     */
    public boolean is21plus(OnStudent student) {
        return student.getAgeAsOfDate(getDecember31()) >= 21;
    }

    /**
     * Log error.
     *
     * @param error String
     */
    public void logError(String error) {
        m_errorsLog.append(error);
        m_errorsLog.append("\n");
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#afterGatherData(java.lang.Object)
     */
    @Override
    protected void afterGatherData(Object data) {
        if (data instanceof ReportDataGrid) {
            ReportDataGrid grid = (ReportDataGrid) data;
            if (grid.isEmpty()) {
                grid.append();
                grid.set("DUMMY", "DUMMY");
                grid.beforeTop();
                setFormatId(getEmptyReportId());
            }
        }
        super.afterGatherData(data);
    }

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
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

        Boolean isAllSchools = (Boolean) getParameter(INPUT_PARAM_ALL_SCHOOLS);
        if (isAllSchools != null) {
            m_isAllSchools = isAllSchools;
        }

        initializeLocalized();
    }

    /**
     * Initializes for localization.
     *
     * Adds the localization parameters
     * Populates the Valid Locales map
     */
    protected void initializeLocalized() {
        try {
            m_default_message_resource =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey(), m_user_locale);
        } catch (Exception e) {
            m_default_message_resource = LocalizationCache.getMessages(getBroker().getPersistenceKey(),
                    LocalizationCache.getCurrentLocale());
            m_user_locale = Locale.US;
        }


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
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_user_locale = userData.getLocale();
    }

    /**
     * Gets the ref codes filterable.
     *
     * @param refTableName String
     * @return Filterable
     */
    private Filterable<ReferenceCode> getRefCodesFilterable(String refTableName) {
        Filterable<ReferenceCode> refCodesFilterable = m_refCodesFilterables.get(refTableName);
        if (refCodesFilterable == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceTable.COL_USER_NAME, refTableName);
            ReferenceTable referenceTable =
                    getBroker().getBeanByQuery(new QueryByCriteria(ReferenceTable.class, criteria));
            if (referenceTable == null) {
                throw new RuntimeException("Cannot find reference table " + refTableName);
            }
            refCodesFilterable = FilterableFactory.create(referenceTable.getCodeMap().values(),
                    Arrays.asList(X2BaseBean.COL_OID),
                    FilterableFactory.Filterable.PredefinedResolver.X2BASE_BEAN);
            m_refCodesFilterables.put(refTableName, refCodesFilterable);
        }
        return refCodesFilterable;
    }

}
