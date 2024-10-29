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
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.CashiersJournalCreditItem;
import com.x2dev.sis.model.beans.CashiersJournalEntry;
import com.x2dev.sis.model.beans.CashiersJournalEntry.EntryType;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.web.cashier.CashiersOfficeConstants;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;


/**
 * Prepares data for Payment Distribution Details
 *
 * NOTE: this report is STUDENT based. Non-students are not included
 *
 * @author X2 Development Corporation
 */
public class PaymentDistributionData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final String MISSING_FEE = "(MISSING)";
    private static final String EMPTY_ACCOUNT_NUMBER_STRING = "(EMPTY ACCOUNT NUMBER)";
    private static final String EMPTY_SUMMARY_STRING = "(EMPTY SUMMARY)";
    private static final String EMPTY_VALUE = "";

    private static final String GRID_AMOUNT = "Amount";
    private static final String GRID_DATE = "Date";
    private static final String GRID_DESCRIPTION = "Description";
    private static final String GRID_FEE_TYPE = "Fee Type";
    private static final String GRID_GROUP_1 = "Group 1";
    private static final String GRID_GROUP_1_HEADER = "Group 1 Header";
    private static final String GRID_GROUP_2 = "Group 2";
    private static final String GRID_GROUP_2_HEADER = "Group 2 Header";
    private static final String GRID_GROUP_3 = "Group 3";
    private static final String GRID_GROUP_3_HEADER = "Group 3 Header";
    private static final String GRID_PAYMENT_METHOD = "Payment Method";
    private static final String GRID_REF_NUMBER = "Ref Number";
    private static final String GRID_STUDENT_NAME = "Student Name";
    private static final String GRID_SUMMARY = "Summary";
    private static final String GRID_VOIDED = "Voided";

    private static final String PARAM_END_DATE = "endDate";
    private static final String PARAM_GROUP_BY = "groupBy";
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

        // get the start and end date
        PlainDate startDate = (PlainDate) getParameter(PARAM_START_DATE);
        addParameter(PARAM_START_DATE, startDate);
        PlainDate endDate = (PlainDate) getParameter(PARAM_END_DATE);
        addParameter(PARAM_END_DATE, endDate);

        // get the Fees & Fine stuff
        ReferenceTable feesFineRefTable = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class,
                ReferenceTable.REF_TABLE_OID_FEES_AND_FINES);
        ExtendedDataDictionary feesFinesDdx = feesFineRefTable.getExtendedDataDictionary();
        DataDictionary feesFinesDictionary =
                DataDictionary.getDistrictDictionary(feesFinesDdx, getBroker().getPersistenceKey());

        /*
         * Get the code map for each school
         */
        Map<String, Map<String, ReferenceCode>> feeRefMap = new HashMap();
        Organization rootOrg = OrganizationManager.getRootOrganization(getOrganization());
        for (School school : rootOrg.getSchools()) {
            feeRefMap.put(school.getOid(), feesFineRefTable.getCodeMap(school, getBroker()));
        }

        // get the General Ledger Account Type stuff for each school
        ReferenceTable glRefTable = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class,
                ReferenceTable.REF_TABLE_OID_GENERAL_LEDGER_ACCT_TYPE);
        ExtendedDataDictionary glDdx = glRefTable.getExtendedDataDictionary();
        DataDictionary glDictionary = DataDictionary.getDistrictDictionary(glDdx, getBroker().getPersistenceKey());
        Map<String, Map<String, ReferenceCode>> glRefMap = new HashMap();
        for (School school : rootOrg.getSchools()) {
            glRefMap.put(school.getOid(), glRefTable.getCodeMap(school, getBroker()));
        }

        /*
         * Prepare journal credit item criteria - fetch ALL payments made in the date range
         * NOTE: this is based on the payment date, not the itemized payment date
         */
        X2Criteria cjoCreditCriteria = new X2Criteria();
        cjoCreditCriteria.addEqualTo(CashiersJournalEntry.COL_SCHOOL_OID, getSchool().getOid());
        cjoCreditCriteria.addGreaterOrEqualThan(CashiersJournalEntry.COL_DATE, startDate);
        cjoCreditCriteria.addLessOrEqualThan(CashiersJournalEntry.COL_DATE, endDate);
        cjoCreditCriteria.addNotEqualTo(CashiersJournalEntry.COL_DELETED_INDICATOR, "1");
        cjoCreditCriteria.addEqualTo(CashiersJournalEntry.COL_ENTRY_TYPE, EntryType.CREDIT.toString());

        BeanQuery cjoCreditQuery = new BeanQuery(CashiersJournalEntry.class, cjoCreditCriteria);

        /*
         * Get all of the data and related data
         */
        Collection<CashiersJournalEntry> cjoCredits = getBroker().getCollectionByQuery(cjoCreditQuery);
        Collection<String> cjoCreditOids = CollectionUtils.getPropertyCollection(cjoCredits, X2BaseBean.COL_OID);

        if (cjoCreditOids != null && cjoCreditOids.size() > 0) {
            // find all credit items associated with our list of credit entry oids
            X2Criteria cjcCriteria = new X2Criteria();
            cjcCriteria.addIn(CashiersJournalCreditItem.COL_CASHIERS_JOURNAL_ENTRY_OID, cjoCreditOids);
            BeanQuery cjcQuery = new BeanQuery(CashiersJournalCreditItem.class, cjcCriteria);
            Map<String, Collection<CashiersJournalCreditItem>> cjcMap = getBroker().getGroupedCollectionByQuery(
                    cjcQuery, CashiersJournalCreditItem.COL_CASHIERS_JOURNAL_ENTRY_OID, cjoCreditOids.size());

            Collection<String> cjoDebitOids = new HashSet<String>();
            for (Collection<CashiersJournalCreditItem> cjcs : cjcMap.values()) {
                for (CashiersJournalCreditItem cjc : cjcs) {
                    cjoDebitOids.add(cjc.getCashiersJournalEntryDebitOid());
                }
            }

            X2Criteria cjoDebitCriteria = new X2Criteria();
            cjoDebitCriteria.addIn(X2BaseBean.COL_OID, cjoDebitOids);
            cjoDebitCriteria.addEqualTo(CashiersJournalEntry.COL_ENTRY_TYPE, EntryType.DEBIT.toString());
            BeanQuery cjoDebitQuery = new BeanQuery(CashiersJournalEntry.class, cjoDebitCriteria);
            Map<String, CashiersJournalEntry> cjoDebitMap =
                    getBroker().getMapByQuery(cjoDebitQuery, X2BaseBean.COL_OID, cjoDebitOids.size());

            Collection<String> psnOids =
                    CollectionUtils.getPropertyCollection(cjoCredits, CashiersJournalEntry.COL_PERSON_OID);

            X2Criteria psnCriteria = new X2Criteria();
            psnCriteria.addIn(X2BaseBean.COL_OID, psnOids);
            BeanQuery psnQuery = new BeanQuery(SisPerson.class, psnCriteria);
            Map<String, SisPerson> psnMap = getBroker().getMapByQuery(psnQuery, X2BaseBean.COL_OID, psnOids.size());

            X2Criteria stdCriteria = new X2Criteria();
            stdCriteria.addIn(SisStudent.COL_PERSON_OID, psnOids);
            BeanQuery stdQuery = new BeanQuery(SisStudent.class, stdCriteria);
            Map<String, SisStudent> stdMap =
                    getBroker().getMapByQuery(stdQuery, SisStudent.COL_PERSON_OID, psnOids.size());

            /*
             * Set up the group headers for the grid
             */
            Integer groupBy = (Integer) getParameter(PARAM_GROUP_BY);
            if (groupBy == null) {
                groupBy = Integer.valueOf(1);
            }

            switch (groupBy.intValue()) {
                case 1:
                    addParameter(GRID_GROUP_1_HEADER, GRID_FEE_TYPE);
                    addParameter(GRID_GROUP_2_HEADER, EMPTY_VALUE);
                    addParameter(GRID_GROUP_3_HEADER, GRID_PAYMENT_METHOD);
                    break;
                case 2:
                    addParameter(GRID_GROUP_1_HEADER, GRID_FEE_TYPE);
                    addParameter(GRID_GROUP_2_HEADER, EMPTY_VALUE);
                    addParameter(GRID_GROUP_3_HEADER, GRID_STUDENT_NAME);
                    break;
                case 3:
                    addParameter(GRID_GROUP_1_HEADER, GRID_FEE_TYPE);
                    addParameter(GRID_GROUP_2_HEADER, GRID_PAYMENT_METHOD);
                    addParameter(GRID_GROUP_3_HEADER, GRID_STUDENT_NAME);
                    break;
                case 4:
                    addParameter(GRID_GROUP_1_HEADER, GRID_PAYMENT_METHOD);
                    addParameter(GRID_GROUP_2_HEADER, GRID_FEE_TYPE);
                    addParameter(GRID_GROUP_3_HEADER, GRID_STUDENT_NAME);
                    break;
                case 5:
                    addParameter(GRID_GROUP_1_HEADER, GRID_SUMMARY);
                    addParameter(GRID_GROUP_2_HEADER, GRID_PAYMENT_METHOD);
                    addParameter(GRID_GROUP_3_HEADER, GRID_STUDENT_NAME);
                    break;
                case 6:
                    addParameter(GRID_GROUP_1_HEADER, GRID_SUMMARY);
                    addParameter(GRID_GROUP_2_HEADER, GRID_FEE_TYPE);
                    addParameter(GRID_GROUP_3_HEADER, GRID_STUDENT_NAME);
                    break;
                default:
                    throw new Exception("Bad input");
            }

            /*
             * Prepare the report data grid
             */
            Iterator iterator = cjoCredits.iterator();
            while (iterator.hasNext()) {
                CashiersJournalEntry cjoCredit = (CashiersJournalEntry) iterator.next();

                SisPerson person = psnMap.get(cjoCredit.getPersonOid());
                SisStudent student = stdMap.get(cjoCredit.getPersonOid());

                Collection<CashiersJournalCreditItem> cjcs = cjcMap.get(cjoCredit.getOid());

                if (cjcs != null && !cjcs.isEmpty()) {
                    for (CashiersJournalCreditItem cjc : cjcs) {
                        CashiersJournalEntry cjoDebit = cjoDebitMap.get(cjc.getCashiersJournalEntryDebitOid());

                        String type = cjoDebit == null ? MISSING_FEE : cjoDebit.getFeeType();
                        String accountNumber = null;
                        String summary = null;
                        ReferenceCode feeCode =
                                cjoDebit == null ? null : feeRefMap.get(cjoDebit.getSchoolOid()).get(type);
                        if (feeCode != null) {
                            String glAcctType = (String) feeCode
                                    .getFieldValueByAlias(CashiersOfficeConstants.ALIAS_GL_TYPE, feesFinesDictionary);
                            ReferenceCode glCode =
                                    cjoDebit == null ? null : glRefMap.get(cjoDebit.getSchoolOid()).get(glAcctType);
                            if (glCode != null) {
                                accountNumber = (String) glCode
                                        .getFieldValueByAlias(CashiersOfficeConstants.ALIAS_GL_NUMBER, glDictionary);
                                summary = (String) glCode.getFieldValueByAlias(CashiersOfficeConstants.ALIAS_GL_SUMMARY,
                                        glDictionary);
                            }
                        }
                        if (StringUtils.isEmpty(accountNumber)) {
                            accountNumber = EMPTY_ACCOUNT_NUMBER_STRING;
                        }
                        if (StringUtils.isEmpty(summary)) {
                            summary = EMPTY_SUMMARY_STRING;
                        }

                        grid.append();
                        if (groupBy.intValue() == 1 || groupBy.intValue() == 2 || groupBy.intValue() == 3) {
                            grid.set(GRID_FEE_TYPE, type + " - " + accountNumber);
                        } else {
                            grid.set(GRID_FEE_TYPE, type);
                        }
                        grid.set(GRID_SUMMARY, summary);
                        grid.set(GRID_PAYMENT_METHOD, cjoCredit.getMethod());
                        grid.set(GRID_DATE, cjoCredit.getDate());
                        grid.set(GRID_REF_NUMBER, cjoDebit == null ? "" : cjoDebit.getReferenceNumber());
                        grid.set(GRID_DESCRIPTION, cjoDebit == null ? "" : cjoDebit.getDescription());
                        grid.set(GRID_AMOUNT, cjc.getAmount());
                        grid.set(GRID_STUDENT_NAME, String.format("%s (%s)", person.getNameView(),
                                student == null ? "" : student.getLocalId()));
                        grid.set(GRID_VOIDED,
                                cjoDebit != null && cjoDebit.getVoidedIndicator() ? Boolean.TRUE : Boolean.FALSE);

                        setGridGroups(grid, groupBy);
                    }
                }

                /*
                 * If there is a credit balance, then create a line for the unallocated balance
                 */
                if (cjoCredit.getAmountCredit() != null &&
                        cjoCredit.getAmountCredit().compareTo(BigDecimal.ZERO) > 0) {
                    grid.append();
                    grid.set(GRID_FEE_TYPE, EMPTY_ACCOUNT_NUMBER_STRING);
                    grid.set(GRID_SUMMARY, EMPTY_SUMMARY_STRING);
                    grid.set(GRID_PAYMENT_METHOD, cjoCredit.getMethod());
                    grid.set(GRID_DATE, cjoCredit.getDate());
                    grid.set(GRID_REF_NUMBER, cjoCredit.getReferenceNumber());
                    grid.set(GRID_DESCRIPTION, cjoCredit.getDescription());
                    grid.set(GRID_AMOUNT, cjoCredit.getAmountCredit());
                    grid.set(GRID_STUDENT_NAME, String.format("%s (%s)", person.getNameView(),
                            student == null ? "" : student.getLocalId()));
                    grid.set(GRID_VOIDED, Boolean.FALSE);

                    setGridGroups(grid, groupBy);
                }
            }

            /*
             * Sort the grid by the groups
             */
            grid.sort(Arrays.asList(new String[] {GRID_GROUP_1, GRID_GROUP_2, GRID_GROUP_3}),
                    Arrays.asList(new Boolean[] {Boolean.TRUE, Boolean.TRUE, Boolean.TRUE}),
                    true);

            grid.beforeTop();
        }

        return grid;
    }

    /**
     * Set Grid Groups.
     *
     * @param grid ReportDataGrid
     * @param groupBy Integer
     */
    private void setGridGroups(ReportDataGrid grid, Integer groupBy) {
        /*
         * Set the groups
         */
        switch (groupBy.intValue()) {
            case 1:
                grid.set(GRID_GROUP_1, grid.get(GRID_FEE_TYPE));
                grid.set(GRID_GROUP_2, EMPTY_VALUE);
                grid.set(GRID_GROUP_3, grid.get(GRID_PAYMENT_METHOD));
                break;
            case 2:
                grid.set(GRID_GROUP_1, grid.get(GRID_FEE_TYPE));
                grid.set(GRID_GROUP_2, EMPTY_VALUE);
                grid.set(GRID_GROUP_3, grid.get(GRID_STUDENT_NAME));
                break;
            case 3:
                grid.set(GRID_GROUP_1, grid.get(GRID_FEE_TYPE));
                grid.set(GRID_GROUP_2, grid.get(GRID_PAYMENT_METHOD));
                grid.set(GRID_GROUP_3, grid.get(GRID_STUDENT_NAME));
                break;
            case 4:
                grid.set(GRID_GROUP_1, grid.get(GRID_PAYMENT_METHOD));
                grid.set(GRID_GROUP_2, grid.get(GRID_FEE_TYPE));
                grid.set(GRID_GROUP_3, grid.get(GRID_STUDENT_NAME));
                break;
            case 5:
                grid.set(GRID_GROUP_1, grid.get(GRID_SUMMARY));
                grid.set(GRID_GROUP_2, grid.get(GRID_PAYMENT_METHOD));
                grid.set(GRID_GROUP_3, grid.get(GRID_STUDENT_NAME));
                break;
            case 6:
            default:
                grid.set(GRID_GROUP_1, grid.get(GRID_SUMMARY));
                grid.set(GRID_GROUP_2, grid.get(GRID_FEE_TYPE));
                grid.set(GRID_GROUP_3, grid.get(GRID_STUDENT_NAME));
        }
    }
}
