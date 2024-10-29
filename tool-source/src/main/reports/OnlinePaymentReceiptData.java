/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

import com.follett.fsc.core.k12.beans.OnlinePayment;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.CashiersJournalCreditItem;
import com.x2dev.sis.model.beans.CashiersJournalEntry;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import java.util.Iterator;

/**
 * Prepares data for Payment Receipt reports.
 *
 * @author Follett Software Company
 */
public class OnlinePaymentReceiptData extends ReportJavaSourceNet {

    private CashiersJournalEntry m_cjoCredit;
    private OnlinePayment m_onlinePayment;

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

        addParameter("receiptView", m_onlinePayment.getReceiptView());
        addParameter("applicableSchool", m_cjoCredit.getSchoolApplied());

        grid.append();
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


        if (m_cjoCredit != null) {
            ModelBroker broker = new ModelBroker(userData.getPrivilegeSet());

            Collection<CashiersJournalCreditItem> cashiersJournalPaymentItems =
                    m_cjoCredit.getCashiersJournalPaymentItems(broker);

            Iterator<CashiersJournalCreditItem> itr = cashiersJournalPaymentItems.iterator();
            while (itr.hasNext()) {
                CashiersJournalCreditItem creditItem = itr.next();
                if (creditItem.getOnlinePayment() != null) {
                    m_onlinePayment = creditItem.getOnlinePayment();
                }
            }
        }
    }
}
