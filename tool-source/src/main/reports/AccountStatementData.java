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
import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Contact;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.CashiersJournalCreditItem;
import com.x2dev.sis.model.beans.CashiersJournalDebitItem;
import com.x2dev.sis.model.beans.CashiersJournalEntry;
import com.x2dev.sis.model.beans.CashiersJournalEntry.EntryType;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.PersistenceBrokerException;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares data for the "Account Statement" report.
 *
 * @author X2 Development Corporation
 */
public class AccountStatementData extends ReportJavaSourceNet {
    private static final String EMPTY_STRING = "";

    private static final String GRID_AMOUNT_DUE = "Amount Due";
    private static final String GRID_AMOUNT_PAID = "Amount Paid";
    private static final String GRID_BALANCE_DUE = "Balance Due";
    private static final String GRID_COMMENT = "Comment";
    private static final String GRID_DATE = "Date";
    private static final String GRID_FEE_TYPE = "Fee Type";
    private static final String GRID_GROUP = "Group";
    private static final String GRID_NAME = "Name";
    private static final String GRID_PAYMENT_METHOD = "Payment Method";
    private static final String GRID_PERSON_NAME = "Person Name";
    private static final String GRID_PERSON_OBJ = "Person";
    private static final String GRID_STUDENT_OBJ = "Student";
    private static final String GRID_VOIDED = "Voided";
    private static final String PARAM_ACTIVE_ONLY = "activeOnly";

    private static final String PARAM_CONTACT_INFO = "contactInfo";
    private static final String PARAM_INCLUDE_VOIDED_FEES = "includeVoidedFees";
    private static final String PARAM_QUERY_BY = "queryBy";
    private static final String PARAM_QUERY_STRING = "queryString";
    private static final String QUERY_BY_ALL = "##all";

    private static final String QUERY_BY_HOMEROOM = "homeroom";
    private static final String QUERY_BY_SNAPSHOT = "##snapshot";
    private static final String QUERY_BY_YOG = "yog";
    private static final String REPORT_PARAM_CONTACT_INFO = "Contact Info";

    /**
     * Map of fees and the current amount due. Used for keeping a history
     * of the amount due for multiple itemized payments.
     */
    private Map<String, BigDecimal> historyMap;

    /**
     * Map of fees and their itemized fees
     */
    private Map<String, Collection<CashiersJournalDebitItem>> m_cjoCjdsMap;

    /**
     * Collection of reporting person(s)
     */
    private Collection<Person> m_persons;

    /**
     * Map of people and their itemized payments
     */
    private Map<String, Collection<CashiersJournalCreditItem>> m_psnCjcsMap;

    private Map<String, Collection<CashiersJournalEntry>> m_psnCjoCreditsMap;

    /**
     * Map of people and their fees/payments
     */
    private Map<String, Collection<CashiersJournalEntry>> m_psnCjoDebitsMap;

    private String m_queryBy;

    private Map<String, SisStudent> m_studentMap;

    /**
     * List of classes that can resolve to a person
     */
    private Collection<Class> personClasses = new ArrayList();
    {
        personClasses.add(CashiersJournalEntry.class);
        personClasses.add(SisStudent.class);
        personClasses.add(SisStaff.class);
        personClasses.add(Contact.class);
        personClasses.add(StudentContact.class);
    }

    /**
     * getAccountBalance.
     *
     * @param oid String
     * @return BigDecimal
     */
    private BigDecimal getAccountBalance(String oid) {
        BigDecimal balance = BigDecimal.ZERO;

        // iterate through cjo debits
        Collection<CashiersJournalEntry> cjoDebits = m_psnCjoDebitsMap.get(oid);
        if (cjoDebits != null) {
            for (CashiersJournalEntry cjo : cjoDebits) {
                if (cjo.getAmountDue() != null) {
                    balance = balance.add(cjo.getAmountDue());
                }
            }
        }

        // iterate through cjo credits
        Collection<CashiersJournalEntry> cjoCredits = m_psnCjoCreditsMap.get(oid);
        if (cjoCredits != null) {
            for (CashiersJournalEntry cjo : cjoCredits) {
                if (cjo.getAmountCredit() != null) {
                    balance = balance.subtract(cjo.getAmountCredit());
                }
            }
        }

        return balance;
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
        ReportDataGrid grid = new ReportDataGrid();
        boolean includeVoidedFees = ((Boolean) getParameter(PARAM_INCLUDE_VOIDED_FEES)).booleanValue();
        String UNKNOWN =
                LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage("label.contentType.unknown");

        // iterate through each person
        for (Person person : m_persons) {
            Student student = m_studentMap.get(person.getOid());

            BigDecimal accountBalance = getAccountBalance(person.getOid());

            if ((!m_queryBy.equals(QUERY_BY_ALL)) || m_persons.size() == 1 || accountBalance.signum() != 0) {
                // iterate through jeds
                Collection<CashiersJournalEntry> cjoDebits = m_psnCjoDebitsMap.get(person.getOid());
                if (cjoDebits != null) {
                    for (CashiersJournalEntry cjo : cjoDebits) {
                        String cjoOid = cjo.getOid();

                        // If the Fee is Voided and there are no refunds... we want to skip this
                        // result.
                        if (cjo.getVoidedIndicator() && !includeVoidedFees) {
                            /*
                             * we DON'T want to skip if there were refunds (i.e., there were
                             * payments)
                             *
                             * In order for a Fee to be voided, the fee must have amount paid =
                             * 0.00.
                             * Therefore, if there weren't any credit items attached to this voided
                             * Fee to begin with,
                             * no refund was made and we can skip this.
                             */
                            continue;
                        }

                        /*
                         * Iterate through the fee's itemized fees to get the total amount
                         */
                        Collection<CashiersJournalDebitItem> cjds = m_cjoCjdsMap.get(cjoOid);
                        BigDecimal amountDue = BigDecimal.ZERO;
                        if (cjds != null) {
                            for (CashiersJournalDebitItem cjd : cjds) {
                                BigDecimal amount = cjd.getAmount();
                                if (amount.doubleValue() > 0.00) {
                                    // we only want the positive amounts,
                                    // we assume negative fee item amount were from voiding the fee
                                    amountDue = amountDue.add(cjd.getAmount());
                                }
                            }
                        }

                        // initialize this jed's amount once payments are processed below
                        historyMap.put(cjoOid, amountDue);

                        BigDecimal amountPaid = cjo.getAmountPaid();
                        if (amountPaid == null) {
                            amountPaid = BigDecimal.ZERO;
                        }

                        grid.append();
                        grid.set(GRID_PERSON_OBJ, person);
                        grid.set(GRID_PERSON_NAME, person.getNameView());
                        grid.set(GRID_STUDENT_OBJ, student);
                        grid.set(GRID_DATE, cjo.getDate());
                        grid.set(GRID_FEE_TYPE, cjo.getFeeType());
                        grid.set(GRID_NAME, cjo.getDescription());
                        grid.set(GRID_COMMENT, cjo.getComment());
                        grid.set(GRID_AMOUNT_DUE, amountDue);
                        grid.set(GRID_AMOUNT_PAID, amountPaid);
                        grid.set(GRID_BALANCE_DUE, cjo.getAmountDue());
                        grid.set(GRID_VOIDED, Boolean.valueOf(cjo.getVoidedIndicator()));

                        if (cjo.getVoidedIndicator()) {
                            grid.set(GRID_GROUP, "Voided Fees");
                        } else {
                            grid.set(GRID_GROUP, "Fees");
                        }
                    }
                }

                // iterate through jcis
                Collection<CashiersJournalCreditItem> cjcs = m_psnCjcsMap.get(person.getOid());
                if (cjcs != null) {
                    for (CashiersJournalCreditItem cjc : cjcs) {
                        CashiersJournalEntry cjoDebit = cjc.getCashiersJournalEntryDebit();
                        if (cjoDebit == null) {
                            cjoDebit =
                                    X2BaseBean.newInstance(CashiersJournalEntry.class, getBroker().getPersistenceKey());
                            cjoDebit.setFeeType(UNKNOWN);
                            cjoDebit.setDescription(UNKNOWN);
                        }

                        String cjoDebitOid = cjoDebit.getOid();

                        BigDecimal previousAmount = historyMap.get(cjoDebitOid);
                        BigDecimal amountPaid = cjc.getAmount();

                        grid.append();
                        grid.set(GRID_PERSON_OBJ, person);
                        grid.set(GRID_PERSON_NAME, person.getNameView());
                        grid.set(GRID_STUDENT_OBJ, student);
                        grid.set(GRID_DATE, cjc.getCashiersJournalEntry().getDate());
                        grid.set(GRID_PAYMENT_METHOD, cjc.getMethod());
                        grid.set(GRID_NAME, cjc.getCashiersJournalEntryDebit().getFeeType());
                        grid.set(GRID_COMMENT, cjc.getComment());
                        grid.set(GRID_AMOUNT_DUE, previousAmount);
                        grid.set(GRID_AMOUNT_PAID, amountPaid);
                        grid.set(GRID_VOIDED, Boolean.valueOf(cjoDebit.getVoidedIndicator()));

                        BigDecimal newAmount = BigDecimal.ZERO;
                        if (previousAmount != null && !cjoDebit.getVoidedIndicator()) {
                            newAmount = previousAmount.subtract(amountPaid);
                        }

                        /*
                         * Note, this is a credit ITEM, so the balance will always be ZERO. There
                         * is nothing left to apply to anything else. BELOW, are the credits that
                         * are still not applied to any fees and are added into the total balance
                         */
                        grid.set(GRID_BALANCE_DUE, BigDecimal.ZERO);

                        historyMap.put(cjoDebitOid, newAmount);

                        if (cjc.getCashiersJournalEntry().getRefundIndicator()) {
                            grid.set(GRID_GROUP, "Refunds");
                        } else {
                            grid.set(GRID_GROUP, "Payments");
                        }
                    }
                }

                // iterate through the outstanding credits
                Collection<CashiersJournalEntry> cjoCredits = m_psnCjoCreditsMap.get(person.getOid());
                if (cjoCredits != null) {
                    CashiersJournalEntry cjoDebit =
                            X2BaseBean.newInstance(CashiersJournalEntry.class, getBroker().getPersistenceKey());
                    cjoDebit.setFeeType(UNKNOWN);
                    cjoDebit.setDescription(UNKNOWN);

                    for (CashiersJournalEntry cjoCredit : cjoCredits) {
                        grid.append();
                        grid.set(GRID_PERSON_OBJ, person);
                        grid.set(GRID_PERSON_NAME, person.getNameView());
                        grid.set(GRID_STUDENT_OBJ, student);
                        grid.set(GRID_DATE, cjoCredit.getDate());
                        grid.set(GRID_PAYMENT_METHOD, cjoCredit.getMethod());
                        grid.set(GRID_NAME, cjoCredit.getDescription());
                        grid.set(GRID_COMMENT, cjoCredit.getComment());
                        grid.set(GRID_AMOUNT_DUE, BigDecimal.ZERO);
                        grid.set(GRID_AMOUNT_PAID, BigDecimal.ZERO);

                        if (cjoCredit.getAmountCredit() == null) {
                            grid.set(GRID_BALANCE_DUE, BigDecimal.ZERO);
                        } else {
                            grid.set(GRID_BALANCE_DUE, BigDecimal.ZERO.subtract(cjoCredit.getAmountCredit()));
                        }

                        grid.set(GRID_GROUP, "Payments");
                    }
                }
            }
        }

        /*
         * Sort it out by "Name", "Group", and "Date."
         *
         * The JRXML does a group-by with "Name" and "Group" so this is needed.
         */
        grid.sort(Arrays.asList(new String[] {GRID_PERSON_NAME, GRID_GROUP, GRID_DATE}), false);
        grid.beforeTop();

        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        String contactInfo = (String) getParameter(PARAM_CONTACT_INFO);
        if (contactInfo == null) {
            contactInfo = EMPTY_STRING;
        }
        addParameter(REPORT_PARAM_CONTACT_INFO, contactInfo);

        m_queryBy = (String) getParameter(PARAM_QUERY_BY);

        /*
         * See if we're on a person related bean
         * CashiersJournalEntry, SisStudent, SisStaff, Contact, and StudentContact
         */
        X2Criteria cjoDebitCriteria = new X2Criteria();
        X2Criteria psnCriteria = new X2Criteria();
        X2Criteria stdCriteria = new X2Criteria();

        Person person = null;
        for (Class beanClass : personClasses) {
            X2BaseBean bean = (X2BaseBean) userData.getCurrentRecord(beanClass);
            if (bean != null) {
                try {
                    Method method = bean.getClass().getMethod("getPerson", (Class[]) null);
                    try {
                        person = (Person) method.invoke(bean, (Object[]) null);
                        break;
                    } catch (Exception ex) {
                        throw new PersistenceBrokerException("Unable to invoke initialization method:"
                                + method.getName() + " for class:" + bean.getClass(), ex);
                    }
                } catch (Exception ex) {
                    throw new PersistenceBrokerException(
                            "Unable to invoke initialization method: getPerson()" + " for class:" + bean.getClass(),
                            ex);
                }
            }
        }

        if (person != null) {
            cjoDebitCriteria.addEqualTo(CashiersJournalEntry.COL_PERSON_OID, person.getOid());
            psnCriteria.addEqualTo(X2BaseBean.COL_OID, person.getOid());
        } else {
            /*
             * If a student-specific option was picked, then only query JED's with a related student
             */
            String queryString = (String) getParameter(PARAM_QUERY_STRING);
            if (!StringUtils.isEmpty(m_queryBy) && !StringUtils.isEmpty(queryString)) {
                if (m_queryBy.equals(QUERY_BY_ALL)) {
                    // do nothing, select all
                } else if (m_queryBy.equals(QUERY_BY_SNAPSHOT)) {
                    SubQuery recordSetSubquery = ReportUtils.getRecordSetSubQuery(queryString, getUser(), getSchool());
                    Collection<String> objectOids = getBroker().getSubQueryCollectionByQuery(recordSetSubquery);
                    cjoDebitCriteria.addIn(CashiersJournalEntry.COL_STUDENT_OID, objectOids);
                    stdCriteria.addIn(X2BaseBean.COL_OID, objectOids);
                } else if (m_queryBy.equals(QUERY_BY_YOG)) {
                    cjoDebitCriteria.addEqualTo(CashiersJournalEntry.REL_STUDENT + "." + SisStudent.COL_YOG,
                            queryString);
                    stdCriteria.addEqualTo(SisStudent.COL_YOG, queryString);
                } else if (m_queryBy.equals(QUERY_BY_HOMEROOM)) {
                    cjoDebitCriteria.addEqualTo(CashiersJournalEntry.REL_STUDENT + "." + SisStudent.COL_HOMEROOM,
                            queryString);
                    stdCriteria.addEqualTo(SisStudent.COL_HOMEROOM, queryString);
                }
            }

            boolean activeOnly = ((Boolean) getParameter(PARAM_ACTIVE_ONLY)).booleanValue();
            if (activeOnly) {
                cjoDebitCriteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                        CashiersJournalEntry.REL_STUDENT + "." + SisStudent.COL_ENROLLMENT_STATUS));
                stdCriteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                        SisStudent.COL_ENROLLMENT_STATUS));
            }

            if (getSchool() != null) {
                stdCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
            }
        }

        /*
         * Query the persons and store them in a Collection
         */
        if (!stdCriteria.isEmpty()) {
            SubQuery psnSubQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, stdCriteria);
            psnCriteria.addIn(X2BaseBean.COL_OID, psnSubQuery);
        }
        QueryByCriteria personQuery = new QueryByCriteria(Person.class, psnCriteria);
        m_persons = getBroker().getCollectionByQuery(personQuery);

        /*
         * Query the students and store them in a map
         */
        SubQuery personSubQuery = new SubQuery(SisPerson.class, X2BaseBean.COL_OID, psnCriteria);
        if (!m_persons.isEmpty()) {
            stdCriteria = new X2Criteria();
            stdCriteria.addIn(SisStudent.COL_PERSON_OID, personSubQuery);
            QueryByCriteria stdQuery = new QueryByCriteria(SisStudent.class, stdCriteria);
            m_studentMap = getBroker().getMapByQuery(stdQuery, SisStudent.COL_PERSON_OID, m_persons.size());

            // only get fees for people specified (don't be redundant if using a snapshot)
            if (!m_queryBy.equals(QUERY_BY_SNAPSHOT)) {
                cjoDebitCriteria.addIn(CashiersJournalEntry.COL_PERSON_OID, personSubQuery);
            }
        }

        /*
         * Query each person(s)' JEDs and store them in a map <psn_oid, collection of jeds>
         */
        X2Criteria dateCriteria = new X2Criteria();
        X2Criteria balanceCriteria = new X2Criteria();
        dateCriteria.addGreaterOrEqualThan(CashiersJournalEntry.COL_DATE, getCurrentContext().getStartDate());
        dateCriteria.addLessOrEqualThan(CashiersJournalEntry.COL_DATE, getCurrentContext().getEndDate());
        balanceCriteria.addNotEqualTo(CashiersJournalEntry.COL_AMOUNT_DUE, BigDecimal.ZERO);
        dateCriteria.addOrCriteria(balanceCriteria);

        cjoDebitCriteria.addAndCriteria(dateCriteria);
        cjoDebitCriteria.addEqualTo(CashiersJournalEntry.COL_ENTRY_TYPE, EntryType.DEBIT.toString());

        BeanQuery cjoDebitQuery = new BeanQuery(CashiersJournalEntry.class, cjoDebitCriteria);
        m_psnCjoDebitsMap =
                getBroker().getGroupedCollectionByQuery(cjoDebitQuery, CashiersJournalEntry.COL_PERSON_OID, 32);

        SubQuery cjoSubQuery = new SubQuery(CashiersJournalEntry.class, X2BaseBean.COL_OID, cjoDebitCriteria);

        /*
         * Query the fee items from the fees collected from above and store them
         * in a Map<jed_oid, collection of fee items>
         */
        X2Criteria cjdCriteria = new X2Criteria();
        cjdCriteria.addIn(CashiersJournalDebitItem.COL_CASHIERS_JOURNAL_ENTRY_OID, cjoSubQuery);
        BeanQuery cjdQuery = new BeanQuery(CashiersJournalDebitItem.class, cjdCriteria);
        m_cjoCjdsMap = getBroker().getGroupedCollectionByQuery(cjdQuery,
                CashiersJournalDebitItem.COL_CASHIERS_JOURNAL_ENTRY_OID, 32);

        /*
         * Query the payment items and then store them into two maps:
         *
         * 1. Map<PSN_OID, Collection of payment items> - this will be used for knowing what the
         * person had payed
         * 2. Map<JED_OID, Collection of payment items> - this will be used for calculating the fee
         * amount for historical purposes
         *
         * Why not just use jci.getJournalEntryDebit().getAmount() to get the amount?
         *
         * When a fee becomes void, the amount due, amount paid, and balance due becomes zero. To
         * get the 'original' fee amount for a voided fee, we
         * iterate the fee items and sum the amount there.
         */
        SubQuery cashiersJournalEntrySubQuery =
                new SubQuery(CashiersJournalEntry.class, X2BaseBean.COL_OID, cjoDebitCriteria);
        X2Criteria cjcCriteria = new X2Criteria();

        // only get the payments made towards fees included in this report
        cjcCriteria.addIn(CashiersJournalCreditItem.COL_CASHIERS_JOURNAL_ENTRY_DEBIT_OID, cashiersJournalEntrySubQuery);

        BeanQuery cjcQuery = new BeanQuery(CashiersJournalCreditItem.class, cjcCriteria);
        cjcQuery.addOrderBy(CashiersJournalCreditItem.COL_DATE, true);
        cjcQuery.addOrderBy(X2BaseBean.COL_LAST_MODIFIED_TIME, true);
        m_psnCjcsMap =
                getBroker().getGroupedCollectionByQuery(cjcQuery, CashiersJournalCreditItem.REL_CASHIERS_JOURNAL_ENTRY
                        + PATH_DELIMITER + CashiersJournalEntry.COL_PERSON_OID, 32);

        /*
         * Get any payments that have not been applied to any fees
         */
        X2Criteria cjoCreditCriteria = new X2Criteria();
        cjoCreditCriteria.addIn(CashiersJournalEntry.COL_PERSON_OID, personSubQuery);
        cjoCreditCriteria.addNotEqualTo(CashiersJournalEntry.COL_DELETED_INDICATOR, "1");
        cjoCreditCriteria.addNotEqualTo(CashiersJournalEntry.COL_AMOUNT_CREDIT, BigDecimal.ZERO);
        cjoCreditCriteria.addEqualTo(CashiersJournalEntry.COL_ENTRY_TYPE, EntryType.CREDIT.toString());
        BeanQuery cjoCreditQuery = new BeanQuery(CashiersJournalEntry.class, cjoCreditCriteria);
        cjoCreditQuery.addOrderBy(CashiersJournalEntry.COL_DATE, true);
        cjoCreditQuery.addOrderBy(X2BaseBean.COL_LAST_MODIFIED_TIME, true);
        m_psnCjoCreditsMap =
                getBroker().getGroupedCollectionByQuery(cjoCreditQuery, CashiersJournalEntry.COL_PERSON_OID, 32);

        historyMap = new HashMap<String, BigDecimal>();
    }
}
