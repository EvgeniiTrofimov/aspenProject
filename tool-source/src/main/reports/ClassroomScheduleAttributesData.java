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
import com.x2dev.sis.model.beans.RoomScheduleAttributes;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.utils.StringUtils;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares data for the "Classroom Schedule Attributes" report.
 *
 * @author X2 Development Corporation
 */
public class ClassroomScheduleAttributesData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private static final String SCENARIO_PARAM = "scenario";
    private static final String SCHEDULE_OID_PARAM = "scheduleOid";
    protected static final String COL_ROOM_NUMBER = "roomNumber";

    /**
     * Name for the enumerated "selection" report parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Criteria criteria = new Criteria();
        addUserCriteria(criteria,
                (String) getParameter(QUERY_BY_PARAM),
                null,
                null,
                null);
        Schedule scenario = null;
        addParameter(SCENARIO_PARAM, scenario);
        String scheduleOid = (String) getParameter(SCHEDULE_OID_PARAM);
        QueryByCriteria query = null;
        if (!StringUtils.isEmpty(scheduleOid)) {
            scenario = (Schedule) getBroker().getBeanByOid(Schedule.class, scheduleOid);
            criteria.addEqualTo(RoomScheduleAttributes.COL_SCHEDULE_OID, scenario.getRoomScheduleOid());
            query = createQueryByCriteria(RoomScheduleAttributes.class, criteria);
            applyUserSort(query, ((String) getParameter(SORT_PARAM)));
        }
        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
    }
}
