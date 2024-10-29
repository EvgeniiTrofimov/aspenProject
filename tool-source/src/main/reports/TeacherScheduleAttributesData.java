/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2004 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.StaffScheduleAttributes;
import com.x2dev.sis.tools.reports.ScheduleReportHelper;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Teacher Schedule Attributes" report.
 *
 * @author X2 Development Corporation
 */
public class TeacherScheduleAttributesData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "selection" report parameter. The value is an String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "sort" report parameter. The value is an String.
     */
    public static final String SORT_PARAM = "sort";

    private ScheduleReportHelper m_scheduleReportHelper;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Criteria criteria = new Criteria();

        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        addUserCriteria(criteria, queryBy, "", null, null);

        criteria.addEqualTo(StaffScheduleAttributes.COL_SCHEDULE_OID, m_scheduleReportHelper.getScheduleOid());

        QueryByCriteria query = createQueryByCriteria(StaffScheduleAttributes.class, criteria);

        String sortBy = (String) getParameter(SORT_PARAM);
        applyUserSort(query, sortBy);

        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
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
        m_scheduleReportHelper = new ScheduleReportHelper(userData);
    }
}
