/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.sys.sped.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepGoal;
import com.x2dev.sis.model.beans.IepGoalObjective;
import com.x2dev.sis.model.beans.IepGoalProgress;
import com.x2dev.utils.StringUtils;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.metadata.FieldHelper;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for BC's IEP progress report. This report is a basic list of IEP Goal Progress
 * records.
 *
 * @author X2 Development Corporation
 */
public class IepProgressReportData extends ReportJavaSourceNet {
    private static final long serialVersionUID = 1L;

    /**
     * End date in the progress report date window. Only progress reports dated prior to and
     * including this date will be included.
     */
    public static final String END_DATE = "endDate";

    /**
     * boolean in the input window that when checked will include unposted reports in this report
     */
    public static final String INCLUDE_UNPOSTED = "includeUnposted";

    /**
     * Report parameter containing the reporting period to filter on. This parameter is optional.
     */
    public static final String REPORT_PERIOD = "reportPeriod";

    /**
     * OID of a specific staff member to filter by. Chosen on input screen.
     */
    public static final String STAFF_OID = "staffOid";

    /**
     * Start date in the progress report date window. Only progress reports dated after and
     * including this date will be included.
     */
    public static final String START_DATE = "startDate";

    private IepData m_iep = null;
    private IepGoalProgress m_progress = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        Criteria criteria = new Criteria();

        Object startDate = getParameter(START_DATE);
        if (startDate != null) {
            criteria.addGreaterOrEqualThan(IepGoalProgress.COL_DATE, startDate);
        }

        Object endDate = getParameter(END_DATE);
        if (endDate != null) {
            criteria.addLessOrEqualThan(IepGoalProgress.COL_DATE, endDate);
        }

        String staffOid = (String) getParameter(STAFF_OID);
        if (!StringUtils.isEmpty(staffOid)) {
            criteria.addEqualTo(IepGoalProgress.COL_STAFF_OID, staffOid);
        }

        Object includeUnposted = getParameter(INCLUDE_UNPOSTED);
        if (includeUnposted != null && !((Boolean) includeUnposted).booleanValue()) {
            criteria.addEqualTo(IepGoalProgress.COL_POSTED_INDICATOR, Boolean.TRUE);
        }

        String postingPeriod = (String) getParameter(REPORT_PERIOD);
        if (!StringUtils.isEmpty(postingPeriod)) {
            criteria.addEqualTo(IepGoalProgress.COL_REPORT_PERIOD, postingPeriod);
        }

        if (m_progress != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, m_progress.getOid());
        } else if (m_iep != null) {
            criteria.addEqualTo(IepGoalProgress.COL_IEP_DATA_OID, m_iep.getOid());
        } else {
            criteria.addIn(IepGoalProgress.COL_IEP_DATA_OID,
                    new SubQuery(IepData.class, X2BaseBean.COL_OID, getCurrentCriteria()));
        }

        QueryByCriteria query = new QueryByCriteria(IepGoalProgress.class, criteria);

        if (m_progress == null && m_iep == null) {
            applyCurrentSort(query);
        }

        for (Object orderItem : query.getOrderBy()) {
            if (!(orderItem instanceof FieldHelper)) {
                continue;
            }

            FieldHelper orderItemFieldHelper = (FieldHelper) orderItem;
            orderItemFieldHelper.name = IepGoalProgress.REL_IEP_DATA + "." + orderItemFieldHelper.name;
        }

        query.addOrderByAscending(IepGoalProgress.REL_IEP_GOAL + PATH_DELIMITER + IepGoal.COL_ID);
        query.addOrderByAscending(
                IepGoalProgress.REL_IEP_GOAL_OBJECTIVE + PATH_DELIMITER + IepGoalObjective.COL_SEQUENCE_NUMBER);
        query.addOrderByAscending(IepGoalProgress.COL_DATE);

        query.setPathOuterJoin(IepGoalProgress.REL_IEP_GOAL_OBJECTIVE);

        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query),
                DataDictionary.getDistrictDictionary(getExtendedDictionary(), getBroker().getPersistenceKey()),
                false,
                getLocale());
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
        if (!userData.getCurrentList().getDataClass().equals(IepData.class)) {
            m_iep = userData.getCurrentRecord(IepData.class);
        }

        m_progress = userData.getCurrentRecord(IepGoalProgress.class);
    }
}
