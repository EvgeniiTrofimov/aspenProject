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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.BuildMasterSchedule;
import com.x2dev.sis.web.SisUserDataContainer;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares data for the "Prescheduled sections" report.
 *
 * @author X2 Development Corporation
 */
public class PrescheduledSectionsData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    private String m_scheduleOid;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        X2Criteria criteria = new X2Criteria();

        /*
         * Build the schedule criteria based on the school's current schedule and the user's input.
         */
        addUserCriteria(criteria, (String) getParameter(QUERY_BY_PARAM), null, null, null);
        criteria.addEqualTo(BuildMasterSchedule.COL_SCHEDULE_OID, m_scheduleOid);

        /*
         * Prescheduled criteria
         */
        X2Criteria preschedTermCriteria = new X2Criteria();
        preschedTermCriteria.addEqualTo(BuildMasterSchedule.COL_PRESCHED_TERM_INDICATOR, Boolean.TRUE);

        X2Criteria preschedScheduleCriteria = new X2Criteria();
        preschedScheduleCriteria.addEqualTo(BuildMasterSchedule.COL_PRESCHED_SCHEDULE_INDICATOR, Boolean.TRUE);

        X2Criteria preschedRoomCriteria = new X2Criteria();
        preschedRoomCriteria.addEqualTo(BuildMasterSchedule.COL_PRESCHED_ROOM_INDICATOR, Boolean.TRUE);

        X2Criteria preschedCriteria = new X2Criteria();
        preschedCriteria.addOrCriteria(preschedTermCriteria);
        preschedCriteria.addOrCriteria(preschedScheduleCriteria);
        preschedCriteria.addOrCriteria(preschedRoomCriteria);

        criteria.addAndCriteria(preschedCriteria);

        /*
         * Build and sort the query
         */
        QueryByCriteria query = createQueryByCriteria(BuildMasterSchedule.class, criteria);
        applyUserSort(query, (String) getParameter(SORT_PARAM));

        /*
         * Execute the query
         */
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
        m_scheduleOid = ((SisUserDataContainer) userData).getBuildScheduleOid();
    }
}
