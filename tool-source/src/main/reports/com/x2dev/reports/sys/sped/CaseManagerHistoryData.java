/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped;

import com.follett.fsc.core.k12.beans.DataAudit;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.business.AuditXmlManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.utils.X2BaseException;
import java.util.Date;
import java.util.HashMap;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Report that uses audits on the IEP Data table to create a history of case managers for an IEP.
 *
 * @author Follett Software Company
 */
public class CaseManagerHistoryData extends ReportJavaSourceNet {
    private static final String IEP_STAFF_AUDIT_KEY = "iepStfOID";

    private static final String REPORT_FIELD_CASE_MANAGER = "caseManager";
    private static final String REPORT_FIELD_CHANGED_BY = "changedBy";
    private static final String REPORT_FIELD_END_DATE = "endDate";
    private static final String REPORT_FIELD_START_DATE = "startDate";

    private static final String REPORT_PARAMETER_IEP_DATA = "iepData";

    private IepData m_currentIep;

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
        m_currentIep = userData.getCurrentRecord(IepData.class);
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

        Criteria iepCriteria = new Criteria();
        iepCriteria.addEqualTo(DataAudit.COL_TABLE_OID, IepData.DICTIONARY_ID);
        iepCriteria.addEqualTo(DataAudit.COL_OBJECT_OID, m_currentIep.getOid());
        QueryByCriteria iepQuery = createQueryByCriteria(DataAudit.class, iepCriteria);
        iepQuery.addOrderByDescending(DataAudit.COL_TIMESTAMP);

        QueryIterator iepQueryIterator = getBroker().getIteratorByQuery(iepQuery);
        try {
            while (iepQueryIterator.hasNext()) {
                DataAudit audit = (DataAudit) iepQueryIterator.next();

                HashMap<String, String> changedValues = new HashMap<String, String>();
                HashMap<String, String> previousValues = new HashMap<String, String>();

                AuditXmlManager.parseAuditChangeDefinition(audit, previousValues, changedValues);

                String previousManagerOid = previousValues.get(IEP_STAFF_AUDIT_KEY);
                Date auditDate = new Date(audit.getTimestamp());

                Staff previousManager = (Staff) getBroker().getBeanByOid(Staff.class, previousManagerOid);

                /*
                 * Set the start date of the 'more recent' case manager as the date of the current
                 * audit
                 * before starting a new row and setting the end date and name for the current audit
                 * case manager
                 */
                grid.set(REPORT_FIELD_START_DATE, Long.valueOf(auditDate.getTime()));

                grid.append();

                if (previousManager != null) {
                    grid.set(REPORT_FIELD_CASE_MANAGER, previousManager.getNameView());
                }

                grid.set(REPORT_FIELD_END_DATE, Long.valueOf(auditDate.getTime()));
                grid.set(REPORT_FIELD_CHANGED_BY, audit.getUser().getNameView());
            }
        } finally {
            iepQueryIterator.close();
        }

        grid.beforeTop();

        addParameter(REPORT_PARAMETER_IEP_DATA, m_currentIep);

        return grid;
    }
}
