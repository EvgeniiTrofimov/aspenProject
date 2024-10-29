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
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.CashiersJournalCreditItem;
import com.x2dev.sis.model.beans.CashiersJournalEntry;
import com.x2dev.sis.model.beans.CashiersJournalEntry.EntryType;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.web.cashier.CashiersOfficeConstants;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Prepares data for the "Transaction Detail" report.
 *
 * @author X2 Development Corporation
 */
public class TransactionDetailData extends ReportJavaSourceNet {
    /**
     * Dummy string used for empty OID lists in queries
     */
    private static final String DUMMY_OID = "xxxxxxxxxxxxxx";

    private static final String EMPTY_ACCOUNT_NUMBER_STRING = "(EMPTY ACCOUNT NUMBER)";
    private static final String UNKNOWN_FEE_STRING = "Unknown";
    private static final String GRID_ACCOUNT = "Account";
    private static final String GRID_AMOUNT = "Amount";
    private static final String GRID_DATE = "Date";
    private static final String GRID_DUE = "Due";
    private static final String GRID_FEE_TYPE = "Fee type";
    private static final String GRID_JED_OID = "jedOid";
    private static final String GRID_STUDENT_NAME = "Student Name";
    private static final String PARAM_ACCOUNT = "account";
    private static final String PARAM_END_DATE = "endDate";
    private static final String PARAM_START_DATE = "startDate";

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

        School school = getSchool();
        PlainDate startDate = (PlainDate) getParameter(PARAM_START_DATE);
        PlainDate endDate = (PlainDate) getParameter(PARAM_END_DATE);

        // get the Fees & Fine stuff
        ReferenceTable feesFinesRefTable = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class,
                ReferenceTable.REF_TABLE_OID_FEES_AND_FINES);
        ExtendedDataDictionary feesFinesDdx = feesFinesRefTable.getExtendedDataDictionary();
        DataDictionary feesFinesDictionary =
                DataDictionary.getDistrictDictionary(feesFinesDdx, getBroker().getPersistenceKey());
        Map<String, ReferenceCode> feeRefMap = feesFinesRefTable.getCodeMap(school, getBroker());

        // get the General Ledger Account Type stuff
        ReferenceTable glRefTable = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class,
                ReferenceTable.REF_TABLE_OID_GENERAL_LEDGER_ACCT_TYPE);
        ExtendedDataDictionary glDdx = glRefTable.getExtendedDataDictionary();
        DataDictionary glDictionary = DataDictionary.getDistrictDictionary(glDdx, getBroker().getPersistenceKey());
        Map<String, ReferenceCode> glRefMap = glRefTable.getCodeMap(school, getBroker());

        // get the accounts I should filter and put them into a set
        String accounts = (String) getParameter(PARAM_ACCOUNT); // comes in as <blank>, or comma
                                                                // separated rcds
        Set<String> accountsSet = new HashSet<String>();
        if (!StringUtils.isEmpty(accounts)) {
            ArrayList<String> glRcdOids = StringUtils.convertDelimitedStringToList(accounts, ',');
            X2Criteria rcdCriteria = new X2Criteria();
            rcdCriteria.addIn(X2BaseBean.COL_OID, glRcdOids);
            BeanQuery rcdQuery = new BeanQuery(ReferenceCode.class, rcdCriteria);
            Collection<ReferenceCode> codes = getBroker().getCollectionByQuery(rcdQuery);
            if (codes != null) {
                for (ReferenceCode code : codes) {
                    accountsSet.add(code.getCode());
                }
            }
        }

        // get all the payments within the school year
        X2Criteria cjoCreditCriteria = new X2Criteria();
        cjoCreditCriteria.addGreaterOrEqualThan(CashiersJournalEntry.COL_DATE, startDate);
        cjoCreditCriteria.addLessOrEqualThan(CashiersJournalEntry.COL_DATE, endDate);

        if (school != null) {
            cjoCreditCriteria.addEqualTo(CashiersJournalEntry.COL_SCHOOL_OID, school.getOid());
        } else {
            cjoCreditCriteria.addAndCriteria(getOrganizationCriteria(CashiersJournalEntry.class));
        }

        cjoCreditCriteria.addEqualTo(CashiersJournalEntry.COL_ENTRY_TYPE, EntryType.CREDIT.toString());

        BeanQuery cjoCreditQuery = new BeanQuery(CashiersJournalEntry.class, cjoCreditCriteria);
        Collection<CashiersJournalEntry> cjoCredits = getBroker().getCollectionByQuery(cjoCreditQuery);
        Collection<String> cjoCreditOids = CollectionUtils.getPropertyCollection(cjoCredits, X2BaseBean.COL_OID);

        X2Criteria cjcCriteria = new X2Criteria();
        cjcCriteria.addIn(CashiersJournalCreditItem.COL_CASHIERS_JOURNAL_ENTRY_OID,
                getQuerySafeCollection(cjoCreditOids));
        BeanQuery jciQuery = new BeanQuery(CashiersJournalCreditItem.class, cjcCriteria);
        Map<String, Collection<CashiersJournalCreditItem>> cjcMap = getBroker().getGroupedCollectionByQuery(jciQuery,
                CashiersJournalCreditItem.COL_CASHIERS_JOURNAL_ENTRY_OID, 1);

        List<String> cjoDebitOids = new ArrayList<String>(cjcMap.values().size());
        for (Collection<CashiersJournalCreditItem> cjcCollection : cjcMap.values()) {
            List<String> oids = CollectionUtils.getPropertyCollection(cjcCollection,
                    CashiersJournalCreditItem.COL_CASHIERS_JOURNAL_ENTRY_DEBIT_OID);
            cjoDebitOids.addAll(oids);
        }

        X2Criteria cjoDebitCriteria = new X2Criteria();
        cjoDebitCriteria.addIn(X2BaseBean.COL_OID, getQuerySafeCollection(cjoDebitOids));
        cjoDebitCriteria.addEqualTo(CashiersJournalEntry.COL_ENTRY_TYPE, EntryType.DEBIT.toString());

        BeanQuery cjoDebitQuery = new BeanQuery(CashiersJournalEntry.class, cjoDebitCriteria);
        Map<String, CashiersJournalEntry> cjoDebitMap = getBroker().getMapByQuery(cjoDebitQuery, X2BaseBean.COL_OID, 1);

        if (cjoCredits != null) {
            for (CashiersJournalEntry cjoCredit : cjoCredits) {
                SisPerson person = cjoCredit.getPerson();

                Collection<CashiersJournalCreditItem> cjcs = cjcMap.get(cjoCredit.getOid());
                if (cjcs != null) {
                    for (CashiersJournalCreditItem cjc : cjcs) {
                        CashiersJournalEntry cjoDebit = cjoDebitMap.get(cjc.getCashiersJournalEntryDebitOid());

                        if (cjoDebit != null) {
                            String feeType = cjoDebit.getFeeType();
                            ReferenceCode feeRefCode = feeRefMap.get(feeType);
                            String glAccount = feeRefCode == null ? UNKNOWN_FEE_STRING
                                    : (String) feeRefCode.getFieldValueByAlias(CashiersOfficeConstants.ALIAS_GL_TYPE,
                                            feesFinesDictionary);
                            if (accountsSet.isEmpty() || accountsSet.contains(glAccount)) {
                                ReferenceCode glRefCode = glRefMap.get(glAccount);

                                String accountNumber = EMPTY_ACCOUNT_NUMBER_STRING;
                                if (glRefCode != null) {
                                    accountNumber = (String) glRefCode.getFieldValueByAlias(
                                            CashiersOfficeConstants.ALIAS_GL_NUMBER, glDictionary);
                                }

                                StringBuilder sb = new StringBuilder(glAccount);
                                sb.append(" - ");
                                sb.append(accountNumber);
                                String account = sb.toString();

                                grid.append();
                                grid.set(GRID_JED_OID, cjoDebit.getOid());
                                grid.set(GRID_DATE, cjoDebit.getDate());
                                grid.set(GRID_ACCOUNT, account);
                                grid.set(GRID_STUDENT_NAME, person.getNameView());
                                grid.set(GRID_FEE_TYPE, cjoDebit.getFeeType());
                                grid.set(GRID_AMOUNT, cjc.getAmount());
                                grid.set(GRID_DUE, cjoDebit.getAmountDue());
                            }
                        }
                    }
                }
            }
        }

        grid.sort(Arrays.asList(new String[] {GRID_ACCOUNT, GRID_JED_OID, GRID_STUDENT_NAME}), true);

        grid.beforeTop();
        return grid;
    }

    /**
     * Makes a collection of OIDs safe for use by queries.
     * The Criteria.addIn(field, collection) method is not safe when the collection is empty, as
     * an exception will be thrown regarding a misplaced right parenthesis. This method solves that
     * syntax problem by adding a bogus OID to the list if the list is empty.
     *
     * @param oidCollection Collection<String>
     * @return Collection<String>
     */
    private Collection<String> getQuerySafeCollection(Collection<String> oidCollection) {
        if (oidCollection.isEmpty()) {
            oidCollection.add(DUMMY_OID);
        }
        return oidCollection;
    }
}
