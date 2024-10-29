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
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.CashiersJournalEntry;
import com.x2dev.sis.model.beans.CashiersJournalEntry.EntryType;
import com.x2dev.sis.model.beans.SisStudent;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;

/**
 * Prepares data for Outstanding Balance reports.
 *
 * @author Follett Software Company
 */
public class OutstandingBalanceData extends ReportJavaSourceNet {

    private static final String GRID_AMOUNT_DUE = "Amount Due";
    private static final String GRID_AMOUNT_PAID = "Amount Paid";
    private static final String GRID_AMOUNT = "Amount";
    private static final String GRID_COMMENT = "Comment";
    private static final String GRID_DESCRIPTION = "Description";
    private static final String GRID_FEE_TYPE = "Fee Type";
    private static final String GRID_DATE = "Date";
    private static final String GRID_STUDENT_NAME = "Student Name";

    private SisStudent m_currentStudent;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        /*
         * Create criteria to find CashiersJournalEntry debits where amount due != 0.00
         */
        X2Criteria cjoDebitCriteria = new X2Criteria();
        cjoDebitCriteria.addNotEqualTo(CashiersJournalEntry.COL_AMOUNT_DUE, BigDecimal.ZERO);
        if (isSchoolContext()) {
            cjoDebitCriteria.addEqualTo(CashiersJournalEntry.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            X2Criteria organizationCriteria = getOrganizationCriteria(CashiersJournalEntry.class);

            X2Criteria districtCriteria = new X2Criteria();
            districtCriteria.addIsNull(CashiersJournalEntry.COL_SCHOOL_OID);
            districtCriteria.addOrCriteria(organizationCriteria);

            cjoDebitCriteria.addAndCriteria(districtCriteria);
        }

        if (m_currentStudent != null) {
            cjoDebitCriteria.addEqualTo(CashiersJournalEntry.COL_STUDENT_OID, m_currentStudent.getOid());
        }

        cjoDebitCriteria.addEqualTo(CashiersJournalEntry.COL_ENTRY_TYPE, EntryType.DEBIT.toString());

        BeanQuery cjoDebitQuery = new BeanQuery(CashiersJournalEntry.class, cjoDebitCriteria);
        Collection<CashiersJournalEntry> cjoDebits = getBroker().getCollectionByQuery(cjoDebitQuery);
        if (cjoDebits != null && !cjoDebits.isEmpty()) {
            for (CashiersJournalEntry cjo : cjoDebits) {
                grid.append();
                grid.set(GRID_STUDENT_NAME, cjo.getNameView());
                grid.set(GRID_DATE, cjo.getDate());
                grid.set(GRID_FEE_TYPE, cjo.getFeeType());
                grid.set(GRID_DESCRIPTION, cjo.getDescription());
                grid.set(GRID_COMMENT, cjo.getComment());
                grid.set(GRID_AMOUNT, cjo.getAmount());
                grid.set(GRID_AMOUNT_PAID, cjo.getAmountPaid());
                grid.set(GRID_AMOUNT_DUE, cjo.getAmountDue());
            }
        }

        /*
         * Create criteria to find CashiersJournalEntry credits where amount due != 0.00
         */
        X2Criteria cjoCreditCriteria = new X2Criteria();
        cjoCreditCriteria.addNotEqualTo(CashiersJournalEntry.COL_AMOUNT_CREDIT, BigDecimal.ZERO);
        cjoCreditCriteria.addNotNull(CashiersJournalEntry.COL_AMOUNT_CREDIT);
        if (isSchoolContext()) {
            cjoCreditCriteria.addEqualTo(CashiersJournalEntry.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            cjoCreditCriteria.addAndCriteria(getOrganizationCriteria(CashiersJournalEntry.class));
        }

        if (m_currentStudent != null) {
            cjoCreditCriteria.addEqualTo(CashiersJournalEntry.COL_STUDENT_OID, m_currentStudent.getOid());
        }

        cjoCreditCriteria.addEqualTo(CashiersJournalEntry.COL_ENTRY_TYPE, EntryType.CREDIT.toString());

        BeanQuery cjoCreditQuery = new BeanQuery(CashiersJournalEntry.class, cjoCreditCriteria);
        Collection<CashiersJournalEntry> cjosCredit = getBroker().getCollectionByQuery(cjoCreditQuery);
        if (cjosCredit != null && !cjosCredit.isEmpty()) {
            for (CashiersJournalEntry cjo : cjosCredit) {
                grid.append();
                grid.set(GRID_STUDENT_NAME, cjo.getNameView());
                grid.set(GRID_DATE, cjo.getDate());
                grid.set(GRID_FEE_TYPE, cjo.getMethod());
                grid.set(GRID_DESCRIPTION, cjo.getDescription());
                grid.set(GRID_COMMENT, cjo.getComment());
                grid.set(GRID_AMOUNT, cjo.getAmount().negate());
                grid.set(GRID_AMOUNT_PAID, cjo.getAmountApplied().negate());
                grid.set(GRID_AMOUNT_DUE, cjo.getAmountCredit().negate());
            }
        }

        /*
         * Sort it out by "Name", "Group", and "Date."
         *
         * The JRXML does a group-by with "Name" and "Group" so this is needed.
         */
        grid.sort(Arrays.asList(new String[] {GRID_STUDENT_NAME, GRID_DATE}), false);
        grid.beforeTop();
        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see @see
     *      com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        /*
         * If we're in the context of a single student, print the report for just that student
         */
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }

}
