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
package com.x2dev.reports.statereporting.on;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.OrganizationLocale;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.microsoft.sqlserver.jdbc.StringUtils;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnOrganization;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.utils.X2BaseException;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnLocalizedReport extends ReportJavaSourceNet {
    public static final String ALIAS_ORG_LANGUAGE = "all-org-BoardLanguage";
    public static final String ALIAS_SKL_LANGUAGE = "all-skl-LanguageType";

    public static final String CONST_COMMA = ",";
    public static final String CONST_EMPTY = "";
    protected static final String CONST_COLON = ":";
    protected static final Integer CONST_ZERO = Integer.valueOf(0);
    protected static final String CONST_AM = "AM";
    protected static final String CONST_PM = "PM";
    protected static final int CONST_PM_HOUR = 12;
    protected static final double CONST_HOUR_TO_MINS_DBL = 60.00;
    protected static final DecimalFormat CONST_FORMAT_INT_2 = new DecimalFormat("00");
    protected static final Double CONST_ZERO_DBL = Double.valueOf(0.0);
    protected static final DecimalFormat CONST_FORMAT_DBL_2_CALC_ONLY = new DecimalFormat("#0.00");

    protected static final String CONST_DATE_FMT_STR_ENG_MMM_DD = "MMM d";
    protected static final String CONST_DATE_FMT_STR_FR_DD_MMM = "d MMM";
    protected static final String CONST_NUM_FMT_STR_DEC = "#,##0.00";
    protected static final String CONST_NUM_FMT_STR_DEC4 = "#,##0.0000";
    protected static final char CONST_NUM_FMT_FR_CHAR_DECIMAL = ',';

    public static final String CONST_LOCALE_ENGLISH = "en_US";
    public static final String CONST_LANG_FRENCH = "French";

    protected static final String REPORT_SCHOOL_LOCALE = "schoolLocale";
    protected static final String REPORT_DATE_FMT_OUTPUT_MMM_YY = "formatDateMmmYy";
    protected static final String REPORT_DEC_FMT_OUTPUT = "decFmtOutput";
    protected static final String REPORT_DEC4_FMT_OUTPUT = "dec4FmtOutput";
    protected static final String REPORT_VERSION = "version";

    // Localization
    protected OnSchool m_school = null;

    private DictionaryExtractor m_dictExtractor;
    private String m_defaultLocale; // Usually English
    private transient MessageResources m_default_message_resource;
    private OrganizationLocale m_schoolLocale = null;
    private Locale m_user_locale;
    private Map<String, String> m_validLocales;

    private static final String PARAM_PREFIX = "prefix";
    private static final String PARAM_REPORT_LOCALE = "reportLocale";
    private static final String PARAM_LOCALES = "locales";
    private static final String PARAM_DEFAULT_LOCALE = "default_locale";
    private static final String PARAM_DEFAULT_LANGUAGE = "default_language";
    private static final String CONST_TOOLS_FOR_PREFIX = "tools.";
    private static final String CONST_AMERICAN_ENGLISH_LOCALE = "en_US";
    private static final String CONST_AMERICAN_ENGLISH_LANGUAGE = "English";

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
     * Gets the organization tool bean.
     *
     * @return Tool organization
     */
    public OnOrganization getOrganizationToolBean() {
        return (OnOrganization) ToolBean.DistrictManager.getOrganizationToolBean(getBroker());
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
        return getUserMessageKeyPrefix(getJob().getTool().getOid());
    }

    /**
     * Gets the user message key prefix.
     *
     * @param toolOid the tool oid
     * @return the user message key prefix
     */
    public String getUserMessageKeyPrefix(String toolOid) {
        return CONST_TOOLS_FOR_PREFIX + toolOid + ".";
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
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        return null;
    }

    /**
     * Returns the report oid based on report id
     *
     * @param reportId
     *
     * @return String - report oid
     */
    protected String getReportOidRptId(String reportId) {
        X2Criteria rptCriteria = new X2Criteria();
        rptCriteria.addEqualTo(Report.COL_ID, reportId);
        QueryByCriteria rptQuery = new QueryByCriteria(Report.class, rptCriteria);

        Report rpt = getBroker().getBeanByQuery(rptQuery);


        return rpt.getOid();
    }
    /**
     * Gets the school tool bean.
     *
     * @return tool school
     */
    protected OnSchool getOnSchool() {
        if (m_school == null && getSchool() != null) {
            m_school = ToolBean.getBeanByOid(getBroker(), OnSchool.class, getSchool().getOid());
        }
        return m_school;
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

        addParameter(REPORT_VERSION, getJob().getTool().getComment());
        ToolBean.setBroker(getBroker());
        ToolBean.setDictionaryExtractor(getDictExtractor());

        ToolBean.registerClass(OnOrganization.class);
        ToolBean.registerClass(OnSchool.class);
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

        // Determine school locale
        String sklLang = CONST_EMPTY;
        if (getOnSchool() != null) {
            sklLang = getOnSchool().getLanguageType();
        }
        if (StringUtils.isEmpty(sklLang)) {
            sklLang = getOrganizationToolBean().getLanguage();
        }
        if (!StringUtils.isEmpty(sklLang) && sklLang.equals(CONST_LANG_FRENCH)) {
            m_schoolLocale = locales.stream()
                    .filter(locale -> locale.getEnabledIndicator())
                    .filter(locale -> locale.getLocale().contains("fr"))
                    .findFirst()
                    .orElse(null);
        }
        if (m_schoolLocale == null) {
            m_schoolLocale = locales.stream()
                    .filter(locale -> locale.getEnabledIndicator())
                    .filter(locale -> locale.getLocale().equals(CONST_LOCALE_ENGLISH))
                    .findFirst()
                    .orElse(null);
        }
        if (m_schoolLocale != null) {
            addParameter(REPORT_SCHOOL_LOCALE, resources.get(m_schoolLocale.getLocale()));
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

        DecimalFormat decNumFormatOut = new DecimalFormat(CONST_NUM_FMT_STR_DEC);
        decNumFormatOut.setGroupingUsed(Boolean.FALSE.booleanValue());
        DecimalFormat dec4NumFormatOut = new DecimalFormat(CONST_NUM_FMT_STR_DEC4);
        dec4NumFormatOut.setGroupingUsed(Boolean.FALSE.booleanValue());

        if (m_schoolLocale.getSystemLocale().toString().equals(CONST_LOCALE_ENGLISH)) {
            SimpleDateFormat dateFormatOut = new SimpleDateFormat(CONST_DATE_FMT_STR_ENG_MMM_DD,
                    m_schoolLocale.getSystemLocale());
            addParameter(REPORT_DATE_FMT_OUTPUT_MMM_YY, dateFormatOut);
        } else {
            SimpleDateFormat dateFormatOut = new SimpleDateFormat(CONST_DATE_FMT_STR_FR_DD_MMM,
                    m_schoolLocale.getSystemLocale());
            addParameter(REPORT_DATE_FMT_OUTPUT_MMM_YY, dateFormatOut);
            DecimalFormatSymbols decNumFormatOutDecimalFormatSymbols = decNumFormatOut.getDecimalFormatSymbols();
            decNumFormatOutDecimalFormatSymbols.setDecimalSeparator(CONST_NUM_FMT_FR_CHAR_DECIMAL);
            decNumFormatOut.setDecimalFormatSymbols(decNumFormatOutDecimalFormatSymbols);
            DecimalFormatSymbols dec4NumFormatOutDecimalFormatSymbols = dec4NumFormatOut.getDecimalFormatSymbols();
            dec4NumFormatOutDecimalFormatSymbols.setDecimalSeparator(CONST_NUM_FMT_FR_CHAR_DECIMAL);
            dec4NumFormatOut.setDecimalFormatSymbols(dec4NumFormatOutDecimalFormatSymbols);
        }
        addParameter(REPORT_DEC_FMT_OUTPUT, decNumFormatOut);
        addParameter(REPORT_DEC4_FMT_OUTPUT, dec4NumFormatOut);
    }

    protected boolean isFrenchLanguage() {
        return m_schoolLocale != null && m_schoolLocale.getLocale().contains("fr");
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
}
