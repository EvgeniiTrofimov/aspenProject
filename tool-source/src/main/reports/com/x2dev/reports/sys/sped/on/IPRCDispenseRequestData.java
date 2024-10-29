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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.OrganizationLocale;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.utils.StringUtils;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * @author Follett Software Company
 * @copyright 2020
 */
public class IPRCDispenseRequestData extends OnBaseFormReportJavaSource {

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private Map<String, String> m_validLocales;
    private String m_defaultLocale; // Usually English
    private MessageResources m_default_message_resource;
    private Locale m_user_locale;
    private static final String PARAM_PREFIX = "prefix";
    private static final String PARAM_REPORT_LOCALE = "reportLocale";
    private static final String PARAM_LOCALES = "locales";
    private static final String PARAM_DEFAULT_LOCALE = "default_locale";
    private static final String PARAM_DEFAULT_LANGUAGE = "default_language";
    private static final String PARAM_EXCEPTIONALITIES = "exceptionalities";
    private static final String PARAM_EXCEPTIONALITIES_DESC = "exceptionalitiesDesc";
    private static final String PARAM_PLACEMENT = "prevPlacement";

    private static final String CONST_TOOLS_FOR_PREFIX = "tools.";
    private static final String CONST_AMERICAN_ENGLISH_LOCALE = "en_US";
    private static final String CONST_AMERICAN_ENGLISH_LANGUAGE = "English";

    private static final String ALIAS_IPRC_PLACEMENT = "iep-iprc-placement-decision";
    private static final String ALIAS_IPRC_PREV_PLACEMENT = "iep-iprc-prv-placement";

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        super.gatherData();

        // addParameter(PARAM_PLACEMENT, getPrevPlacement());
        loadExceptionalities();
        loadPlacementReference();
        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * @see com.x2dev.reports.sys.sped.on.OnBaseFormReportJavaSource#getTitle()
     */
    @Override
    protected String getTitle() {
        return "Identification, Placement and Review Committee\nMeeting Dispense Request";
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);
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
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();
        // Enabling localization
        initializeLocalized();
    }

    /**
     * Retrieve the previous placement field and convert it to the reference code description.
     * The data field is previous placement, but that field does not have the reference table.
     * the reference table is on the iprc placement field.
     *
     * @return String
     */
    private String getPrevPlacement() {
        DataDictionaryField placementField = getDictionary().findDataDictionaryFieldByAlias(ALIAS_IPRC_PLACEMENT);
        DataDictionaryField prevPlacementField =
                getDictionary().findDataDictionaryFieldByAlias(ALIAS_IPRC_PREV_PLACEMENT);
        String value = null;
        if (getIep() != null) {
            value = (String) getIep().getFieldValueByBeanPath(prevPlacementField.getJavaName());
            ReferenceTable table = placementField.getReferenceTable();
            if (table != null) {
                Collection<ReferenceCode> codes = table.getReferenceCodes(getBroker());
                for (ReferenceCode code : codes) {
                    if (code.getCode().equals(value)) {
                        value = code.getDescription();
                        break;
                    }
                }
            }
        }
        return value;
    }

    /**
     * look up the placement code, get the description.
     */
    private void loadPlacementReference() {
        String placement = (String) getIep().getFieldValueByAlias(ALIAS_IPRC_PLACEMENT, getDictionary());
        DataDictionaryField field = getDictionary().findDataDictionaryFieldByAlias(ALIAS_IPRC_PLACEMENT);
        if (!StringUtils.isEmpty(placement) && field != null && field.hasReferenceTable()) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
            criteria.addEqualTo(ReferenceCode.COL_CODE, placement);
            ReferenceCode rcd = getBroker().getBeanByQuery(new QueryByCriteria(ReferenceCode.class, criteria));
            if (rcd != null && !StringUtils.isEmpty(rcd.getDescription())) {
                placement = rcd.getDescription();
            }
        }
        addParameter(PARAM_PLACEMENT, placement);
    }

    /**
     * Gets the exceptionalities.
     *
     * @return String
     */
    private void loadExceptionalities() {
        Map<String, String> map = new HashMap();
        Collection<IepDisability> disabilities = getIepDisability();
        DataDictionaryField field =
                getDictionary().findDataDictionaryField(IepDisability.class.getName(),
                        IepDisability.COL_DISABILITY_CODE);
        List<ReferenceCode> disabCodes = null;
        if (field != null && field.hasReferenceTable()) {
            disabCodes = (List<ReferenceCode>) field.getReferenceTable().getReferenceCodes();
        }
        StringBuilder allExcepDescriptons = new StringBuilder();
        if (disabilities != null && !disabilities.isEmpty()) {
            // When building a concatinated list of disabilities,
            // show the primary disability first, then the rest.
            DataDictionaryField rcdCodeField = getDictionary()
                    .findDataDictionaryField(ReferenceCode.class.getCanonicalName(), ReferenceCode.COL_CODE);
            DataDictionaryField rcdDescField = getDictionary()
                    .findDataDictionaryField(ReferenceCode.class.getCanonicalName(), ReferenceCode.COL_DESCRIPTION);
            MessageResources resources = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale());
            for (IepDisability disability : disabilities) {
                String disabCode = disability.getDisabilityCode();
                ReferenceCode foundCode = null;
                if (disabCode != null) {
                    for (ReferenceCode code : disabCodes) {
                        String resourceKey = LocalizationUtils.generateKey(code.getOid(), rcdCodeField.getId());
                        String codeTranslation = resources.getMessage(getLocale(), resourceKey);
                        if (disabCode.equals(code.getCode()) || disabCode.equals(codeTranslation)) {
                            foundCode = code;
                            break;
                        }
                    }
                    if (foundCode != null) {
                        String stateCode = foundCode.getStateCode();
                        String resourceKey = LocalizationUtils.generateKey(foundCode.getOid(), rcdDescField.getId());
                        String descTranslation = resources.getMessage(getLocale(), resourceKey);
                        if (descTranslation == null || descTranslation.startsWith("??")) {
                            descTranslation = foundCode.getDescription();
                        }
                        map.put(stateCode, descTranslation);
                        if (allExcepDescriptons.length() > 0) {
                            allExcepDescriptons.append("\n");
                        }
                        allExcepDescriptons.append(descTranslation);
                    } else {
                        if (allExcepDescriptons.length() > 0) {
                            allExcepDescriptons.append("\n");
                        }
                        allExcepDescriptons.append(disabCode);
                    }
                }
            }
        }
        addParameter(PARAM_EXCEPTIONALITIES, map);
        addParameter(PARAM_EXCEPTIONALITIES_DESC, allExcepDescriptons.toString());
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
                MessageResources messages =
                        LocalizationCache.getMessages(getBroker().getPersistenceKey(), loc.getLocale());

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
        // Comment line below if your numeric notation, currencies and others don't display as
        // expected
        addParameter(net.sf.jasperreports5.engine.JRParameter.REPORT_LOCALE, m_user_locale); // Only
                                                                                             // tested
                                                                                             // for
                                                                                             // JasperReports
                                                                                             // engine
                                                                                             // 5
    }

}
