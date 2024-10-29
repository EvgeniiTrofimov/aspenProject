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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.CashiersJournalEntry;
import com.x2dev.sis.model.beans.CashiersJournalEntry.EntryType;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * Prepares data for the "Deleted Payments" report.
 *
 * @author X2 Development Corporation
 */
public class DeletedPaymentsData extends ReportJavaSourceNet {
    private static final String GRID_COMMENT = "Comment";
    private static final String GRID_DATE = "Date";
    private static final String GRID_DESCRIPTION = "Description";
    private static final String GRID_PAYMENT_METHOD = "Payment Method";
    private static final String GRID_PERSON_NAME = "Person Name";
    private static final String GRID_PERSON_OBJ = "Person";
    private static final String GRID_REFERENCE_NUM = "Reference";
    private static final String GRID_DELETED_BY = "Deleted By";
    private static final String GRID_DELETED_AMOUNT = "Deleted Amount";

    private static final String PARAM_START_DATE = "startDate";
    private static final String PARAM_END_DATE = "endDate";

    private PlainDate m_startDate;
    private PlainDate m_endDate;
    private Collection<CashiersJournalEntry> m_cjoCredits;
    private Map<String, SisPerson> m_psnMap;

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

        // iterate through each person
        for (CashiersJournalEntry cjoCredit : m_cjoCredits) {
            SisPerson person = m_psnMap.get(cjoCredit.getPersonOid());

            grid.append();
            grid.set(GRID_PERSON_OBJ, person);
            grid.set(GRID_PERSON_NAME, person.getNameView());
            grid.set(GRID_REFERENCE_NUM, cjoCredit.getReferenceNumber());
            grid.set(GRID_DATE, cjoCredit.getDate());
            grid.set(GRID_DESCRIPTION, cjoCredit.getDescription());
            grid.set(GRID_COMMENT, cjoCredit.getComment());
            grid.set(GRID_PAYMENT_METHOD, cjoCredit.getMethod());
            grid.set(GRID_DELETED_BY, cjoCredit.getDeletedByName());
            grid.set(GRID_DELETED_AMOUNT, cjoCredit.getDeletedAmount());
        }

        /*
         * Sort it out by "Name" and "Date."
         */
        grid.sort(Arrays.asList(new String[] {GRID_PERSON_NAME, GRID_DATE}), false);
        grid.beforeTop();

        return grid;
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

        m_startDate = (PlainDate) getParameter(PARAM_START_DATE);
        m_endDate = (PlainDate) getParameter(PARAM_END_DATE);

        // midnight on startDate
        long startDate = m_startDate.getTime();

        // next day at midnight
        long endDate = m_endDate.getTime() + TimeUnit.MILLISECONDS.convert(1, TimeUnit.DAYS);

        X2Criteria cjoCreditCriteria = new X2Criteria();
        cjoCreditCriteria.addEqualTo(CashiersJournalEntry.COL_SCHOOL_OID, getSchool().getOid());
        cjoCreditCriteria.addEqualTo(CashiersJournalEntry.COL_DELETED_INDICATOR, Boolean.TRUE);
        cjoCreditCriteria.addGreaterOrEqualThan(CashiersJournalEntry.COL_DELETED_TIMESTAMP, Long.valueOf(startDate));
        cjoCreditCriteria.addLessThan(CashiersJournalEntry.COL_DELETED_TIMESTAMP, Long.valueOf(endDate));
        cjoCreditCriteria.addEqualTo(CashiersJournalEntry.COL_ENTRY_TYPE, EntryType.CREDIT.toString());

        BeanQuery cjoCreditQuery = new BeanQuery(CashiersJournalEntry.class, cjoCreditCriteria);
        m_cjoCredits = getBroker().getCollectionByQuery(cjoCreditQuery);

        Collection<String> psnOids =
                CollectionUtils.getPropertyCollection(m_cjoCredits, CashiersJournalEntry.COL_PERSON_OID);

        X2Criteria psnCriteria = new X2Criteria();
        psnCriteria.addIn(X2BaseBean.COL_OID, psnOids);
        BeanQuery psnQuery = new BeanQuery(SisPerson.class, psnCriteria);

        if (psnOids.size() > 0) {
            m_psnMap = getBroker().getMapByQuery(psnQuery, X2BaseBean.COL_OID, psnOids.size());
        }
    }
}
