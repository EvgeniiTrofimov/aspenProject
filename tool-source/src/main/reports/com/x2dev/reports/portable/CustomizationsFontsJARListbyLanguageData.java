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
package com.x2dev.reports.portable;

import com.follett.fsc.core.k12.beans.OrganizationLocale;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.struts.util.MessageResources;

public class CustomizationsFontsJARListbyLanguageData extends ReportJavaSourceNet {


    private Map<String, String> m_validLocales;
    private String m_defaultLocale; // Usually English
    // Report Parameters
    private static final String PARAM_PREFIX = "prefix";
    private static final String PARAM_LOCALES = "locales";
    private static final String PARAM_DEFAULT_LOCALE = "default_locale";
    private static final String PARAM_DEFAULT_LANGUAGE = "default_language";


    // Aliases/Constants - Grid field identifiers
    private static final String CONST_TOOLS_FOR_PREFIX = "tools.";


    private static final String CONST_AMERICAN_ENGLISH_LOCALE = "en_US";
    private static final String CONST_AMERICAN_ENGLISH_LANGUAGE = "English";

    @Override
    protected Object gatherData() throws Exception {

        ReportDataGrid grid = new ReportDataGrid();
        initializeLocalized();


        grid.append();

        grid.set("studentLanguage", "en_US");
        grid.set("districtStudentLanguage", "English");
        grid.beforeTop();
        return grid;
    }



    /**
     * Initializes for localization, this is a custom version for Hudson, MA to ensure two extra
     * variations of Chinese are properly mapped to print a Chinese based report card
     *
     * Adds the localization parameters
     * Populates the Valid Locales map
     */
    private void initializeLocalized() {
        Collection<OrganizationLocale> locales = getOrganization().getRootOrganization().getLocales();
        Map<String, MessageResources> resources = new HashMap<String, MessageResources>();
        m_validLocales = new HashMap<String, String>();
        m_defaultLocale = null;
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
        addParameter(PARAM_PREFIX, CONST_TOOLS_FOR_PREFIX + getJob().getTool().getOid() + ".");
        addParameter(PARAM_LOCALES, resources);
        addParameter(PARAM_DEFAULT_LOCALE, CONST_AMERICAN_ENGLISH_LOCALE);
        addParameter(PARAM_DEFAULT_LANGUAGE, CONST_AMERICAN_ENGLISH_LANGUAGE);
    }

}
