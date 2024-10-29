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
package com.x2dev.reports.sys.sped.on;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.UserDefinedTableB;
import com.x2dev.sis.model.beans.UserDefinedTableC;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import java.util.Locale;
import org.apache.ojb.broker.query.QueryByCriteria;

import com.follett.fsc.core.k12.beans.OrganizationLocale;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.web.UserDataContainer;
import java.util.HashMap;
import java.util.Map;
import org.apache.struts.util.MessageResources;

/**
 * @author Follett Software Company
 * @copyright 2020
 */
public class SpecialEquipmentInventoryItemData extends ReportJavaSourceNet {

    private static final String ALIAS_CATEGORY = "uda-spedinv-category";
    private static final String ALIAS_ITEM = "uda-spedinv-description";
    private static final String ALIAS_SERIAL_NUMBER = "udc-spedinv-serial-no";
    private static final String ALIAS_RCD_IMAGE_BASE64 = "all-rcd-ImageBase64";
    private static final String ALIAS_RETURN_DATE = "udb-spedinv-return-date";
    private static final String LOGO_CODE_ON_BOARD = "OnBoardLogo";
    private static final String PARAM_LOGO = "logoOntario";
    private static final String PARAM_TITLE = "title";
    private static final String RTB_OID_ON_SIS_IMAGES = "rtbOnImage    ";// OnSIS Images

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
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ExtendedDataDictionary ddx = getExtendedDictionary();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());

        addParameter(PARAM_TITLE, "Inventory");

        addParameter(PARAM_LOGO, getBase64ImageString(LOGO_CODE_ON_BOARD, getBroker()));

        QueryByCriteria query = getInventoryItemQuery(dictionary, ddx.getOid());
        InventoryStudentDataSource source =
                new InventoryStudentDataSource(getBroker().getIteratorByQuery(query), dictionary, false, getLocale());
        return source;
    }

    /**
     * Build a query for outstanding student inventory records
     * (where return date is empty)
     *
     * @param dictionary
     * @param extensionOid
     *
     * @return QueryByCriteria
     */
    private QueryByCriteria getInventoryItemQuery(DataDictionary dictionary, String extensionOid) {
        DataDictionaryField categoryField = dictionary.findDataDictionaryFieldByAlias(ALIAS_CATEGORY);
        DataDictionaryField itemField = dictionary.findDataDictionaryFieldByAlias(ALIAS_ITEM);
        DataDictionaryField numberField = dictionary.findDataDictionaryFieldByAlias(ALIAS_SERIAL_NUMBER);

        QueryByCriteria query = null;
        X2Criteria criteria = new X2Criteria();
        criteria = new X2Criteria();
        criteria.addEqualTo(UserDefinedTableC.COL_EXTENDED_DATA_DICTIONARY_OID, extensionOid);
        query = new QueryByCriteria(UserDefinedTableC.class, criteria);
        query.addOrderByAscending(UserDefinedTableC.REL_USER_DEFINED_TABLE_A + ModelProperty.PATH_DELIMITER +
                categoryField.getJavaName());
        query.addOrderByAscending(UserDefinedTableC.REL_USER_DEFINED_TABLE_A + ModelProperty.PATH_DELIMITER +
                itemField.getJavaName());
        query.addOrderByAscending(numberField.getJavaName());
        return query;
    }

    /**
     * Gets the base 64 image string.
     *
     * @param imageCode String
     * @param broker X2Broker
     * @return String
     */
    public String getBase64ImageString(String imageCode, X2Broker broker) {
        String base64Image = "";

        X2Criteria imageCriteria = new X2Criteria();
        imageCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, RTB_OID_ON_SIS_IMAGES);
        imageCriteria.addEqualTo(ReferenceCode.COL_CODE, imageCode);
        BeanQuery imageQuery = new BeanQuery(ReferenceCode.class, imageCriteria);
        ReferenceCode rcdBean = broker.getBeanByQuery(imageQuery);
        if (rcdBean != null) {
            base64Image = (String) rcdBean.getFieldValueByAlias(ALIAS_RCD_IMAGE_BASE64);
        }
        return base64Image;
    }

    /**
     * A subclass of the data source that can interpret other source beans into a value retrieval.
     *
     * @author Follett Software Company
     * @copyright 2020
     */
    static class InventoryStudentDataSource extends QueryIteratorDataSource {

        X2BaseBean m_overrideCurrentBean;

        /**
         * @param iterator
         * @param dictionary
         * @param autoConvert
         * @param locale
         */
        public InventoryStudentDataSource(QueryIterator iterator, DataDictionary dictionary, boolean autoConvert,
                Locale locale) {
            super(iterator, dictionary, autoConvert, locale);
        }

        /**
         * Check incoming field requests for a "x:" prefix and retrieve an alternate bean source.
         * Search UDBs for the active student transaction bean, if there is one.
         *
         * @see com.follett.fsc.core.k12.tools.reports.BeanDataSource#getRawFieldValue(java.lang.String)
         */
        @Override
        protected Object getRawFieldValue(String baseFieldName) throws X2BaseException {
            if (baseFieldName.startsWith("x:")) {
                UserDefinedTableC bean = (UserDefinedTableC) super.getCurrentBean();
                Collection<UserDefinedTableB> students = bean.getUserDefinedRecordsB();
                String trimmedFieldName = baseFieldName.substring(2); // peal off the alias
                DataDictionaryField returnDateField = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_RETURN_DATE);
                for (UserDefinedTableB student : students) {
                    String returnDateValue =
                            (String) WebUtils.getProperty(getCurrentBean(), returnDateField.getJavaName());
                    if (StringUtils.isEmpty(returnDateValue)) {
                        m_overrideCurrentBean = student;
                        Object value = super.getRawFieldValue(trimmedFieldName);
                        m_overrideCurrentBean = null;
                        return value;
                    }
                }
                return null;
            }
            return super.getRawFieldValue(baseFieldName);
        }

        /**
         * override current bean so we can retrieve a value from an alternate bean source.
         *
         * @see com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource#getCurrentBean()
         */
        @Override
        protected X2BaseBean getCurrentBean() {
            if (m_overrideCurrentBean != null) {
                return m_overrideCurrentBean;
            }
            return super.getCurrentBean();
        }
    }

    /**
    * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
    */
   @Override
   protected void saveState(UserDataContainer userData) throws X2BaseException {
       super.saveState(userData);
       try {
           m_default_message_resource = LocalizationCache.getMessages(getBroker().getPersistenceKey(), userData.getLocale());
           m_user_locale = userData.getLocale();
       } catch (Exception e) {
           m_default_message_resource = LocalizationCache.getMessages(getBroker().getPersistenceKey(), LocalizationCache.getCurrentLocale());
           m_user_locale = Locale.US;
       }
    }

    /**
    * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
    */
   @Override
   protected void initialize() throws X2BaseException {
       super.initialize();
       // Enabling localization
       initializeLocalized();
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
    
            MessageResources messages = LocalizationCache.getMessages(getBroker().getPersistenceKey(),loc.getLocale());
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
    addParameter(PARAM_PREFIX, CONST_TOOLS_FOR_PREFIX + getJob().getTool().getOid() + ".");
    addParameter(PARAM_REPORT_LOCALE, m_default_message_resource);
    // Additional hooks for enhanced implementations
    addParameter(PARAM_LOCALES, resources);
    addParameter(PARAM_DEFAULT_LOCALE, CONST_AMERICAN_ENGLISH_LOCALE);
    addParameter(PARAM_DEFAULT_LANGUAGE, CONST_AMERICAN_ENGLISH_LANGUAGE);
    // Comment line below if your numeric notation, currencies and others don't display as expected
    addParameter(net.sf.jasperreports5.engine.JRParameter.REPORT_LOCALE, m_user_locale); // Only tested for JasperReports engine 5 
    }
}