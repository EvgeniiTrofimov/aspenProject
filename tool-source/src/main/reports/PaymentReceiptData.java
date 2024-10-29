/*
 * ====================================================================
 *
 * X2 Development Corporation
 * A wholly owned subsidiary of Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

import com.follett.fsc.core.k12.beans.OrganizationLocale;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.ChildDetailSet;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.CashiersJournalCreditItem;
import com.x2dev.sis.model.beans.CashiersJournalEntry;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.web.cashier.CashiersOfficeDataEntryAction;
import com.x2dev.sis.web.cashier.CashiersOfficeManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import org.apache.struts.action.ActionErrors;
import org.apache.struts.util.MessageResources;

import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

/**
 * Prepares data for Payment Receipt reports.
 *
 * @author Follett Software Company
 */
public class PaymentReceiptData extends ReportJavaSourceNet {

    private CashiersJournalEntry m_cjoCredit;
    private Collection<CashiersJournalCreditItem> m_cjcs;
    private Locale m_userLocale;
    private String m_userLocaleLanguage;
    private Map<String, String> m_validLocales;
    private MessageResources m_default_message_resource;
    private String m_defaultLocale; // Usually English
    private static final String PARAM_PREFIX = "prefix";
    private static final String PARAM_REPORT_LOCALE = "reportLocale";
    private static final String PARAM_LOCALES = "locales";
    private static final String PARAM_DEFAULT_LOCALE = "default_locale";
    private static final String PARAM_DEFAULT_LANGUAGE = "default_language";
    private static final String CONST_TOOLS_FOR_PREFIX = "tools.";
    private static final String CONST_AMERICAN_ENGLISH_LOCALE = "en_US";
    private static final String CONST_AMERICAN_ENGLISH_LANGUAGE = "English";

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        if (m_cjoCredit != null) {
            addParameter("credit", m_cjoCredit);

            SisPerson person = m_cjoCredit.getPerson();
            addParameter("person", person);

            SisStudent student = m_cjoCredit.getStudent();
            addParameter("student", student);

            addParameter("staff", m_cjoCredit.getStaff());

            m_cjcs = m_cjoCredit.getCashiersJournalPaymentItems();
            if (m_cjcs != null && !m_cjcs.isEmpty()) {
                for (CashiersJournalCreditItem cjc : m_cjcs) {
                    grid.append();
                    grid.set("Date", cjc.getDate());
                    grid.set("Fee Type", cjc.getCashiersJournalEntryDebit().getFeeType());
                    grid.set("Name", cjc.getCashiersJournalEntryDebit().getDescription());
                    grid.set("Comment", cjc.getComment() == null ? "" : cjc.getComment());
                    grid.set("Amount Due", cjc.getCashiersJournalEntryDebit().getAmount());
                    grid.set("Amount Paid", cjc.getAmount());
                    grid.set("Balance Due", cjc.getCashiersJournalEntryDebit().getAmountDue());
                }
            } else {
                grid.append();
                grid.set("Date", m_cjoCredit.getDate());
                grid.set("Fee Type", "");
                grid.set("Name", "");
                grid.set("Comment", m_cjoCredit.getComment() == null ? "" : m_cjoCredit.getComment());
                grid.set("Amount Due", BigDecimal.ZERO);
                grid.set("Amount Paid", m_cjoCredit.getAmount());
                grid.set("Balance Due", BigDecimal.ZERO.subtract(m_cjoCredit.getAmount()));
            }
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        m_cjoCredit = userData.getCurrentRecord(CashiersJournalEntry.class);

        /*
         * Not in a detail or list ... the print may have been triggered from the
         * CashiersOfficeDataEntryAction class
         */
        if (m_cjoCredit == null) {
            try {
                ActionErrors actionErrors = new ActionErrors();
                ModelBroker broker = new ModelBroker(userData.getPrivilegeSet());

                GenericDetail detail = userData.getCurrentDetail();
                if (detail != null) {
                    ChildDetailSet childSet = detail.getChildDetailSet(CashiersOfficeDataEntryAction.PAY_EMBEDDED_LIST);

                    // If this wasn't the DataEntry detail, get the data from the cashier's office
                    // detail
                    if (childSet == null) {
                        childSet = detail.getChildDetailSet(CashiersOfficeManager.PAYMENT_ITEMIZATIONS_DETAIL_SET);
                        if (childSet != null && childSet.getChildDetails() != null
                                && !childSet.getChildDetails().isEmpty()) {
                            GenericDetail childDetail = childSet.getChildDetails().iterator().next();
                            CashiersJournalCreditItem cjc =
                                    (CashiersJournalCreditItem) childDetail.materializeBean(broker, actionErrors, null,
                                            CashiersJournalCreditItem.class, GenericDetail.ROOT_RELATION);
                            m_cjoCredit = cjc.getCashiersJournalEntry();
                        }
                    } else if (childSet != null && childSet.getChildDetails() != null
                            && !childSet.getChildDetails().isEmpty()) {
                        GenericDetail childDetail = childSet.getChildDetails().iterator().next();
                        m_cjoCredit = (CashiersJournalEntry) childDetail.materializeBean(broker, actionErrors, null,
                                CashiersJournalEntry.class, GenericDetail.ROOT_RELATION);
                    }
                }
            } catch (InstantiationException e) {
                e.printStackTrace();
            } catch (IllegalAccessException e) {
                e.printStackTrace();
            } catch (NoSuchMethodException e) {
                e.printStackTrace();
            } catch (InvocationTargetException e) {
                e.printStackTrace();
            }
        }
    }

    @Override
    protected void initialize() {
        initializeLocalized();
    }

    private void initializeLocalized() {
        Collection<OrganizationLocale> locales = getOrganization().getRootOrganization().getLocales();
        Map<String, MessageResources> resources = new HashMap<>();
        m_validLocales = new HashMap<>();

        if (m_userLocale != null) {
            m_default_message_resource =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey(), m_userLocale);
        } else {
            m_default_message_resource = LocalizationCache.getMessages(getBroker().getPersistenceKey(), LocalizationCache.getCurrentLocale());
        }

        if (StringUtils.isBlank(m_userLocaleLanguage)) {
            m_userLocaleLanguage = LocalizationCache.getCurrentLocale().getDisplayLanguage();
        }
        for (OrganizationLocale loc : locales) {
            if (loc.getEnabledIndicator()) {
                MessageResources messages = LocalizationCache.getMessages(getBroker().getPersistenceKey(), loc.getLocale());
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
        addParameter(PARAM_LOCALES, resources);
        addParameter(PARAM_DEFAULT_LOCALE, CONST_AMERICAN_ENGLISH_LOCALE);
        addParameter(PARAM_DEFAULT_LANGUAGE, CONST_AMERICAN_ENGLISH_LANGUAGE);
        // Comment line below if your numeric notation, currencies and others don't display as expected
        addParameter(net.sf.jasperreports5.engine.JRParameter.REPORT_LOCALE, m_userLocale); // Only tested for JasperReports engine 5
    }
}
