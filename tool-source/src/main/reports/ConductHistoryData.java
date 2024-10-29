/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Conduct History" report.
 *
 * @author X2 Development Corporation
 */
public class ConductHistoryData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    public static final String FIELD_ACTION = "action";
    public static final String FIELD_CONTAINS_ACTIONS = "containsActions";
    public static final String FIELD_INCIDENT = "incident";

    public static final String PARAM_TIME_FORMAT = "timeFormat";

    /**
     * Report parameter name for the end of the date range. This value is a PlainDate object.
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * Report parameter name for the start of the date range. This value is a PlainDate object.
     */
    public static final String START_DATE_PARAM = "startDate";

    private SisStudent m_student;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(100, 5);

        if (m_student != null) {
            PlainDate startDate = (PlainDate) getParameter(START_DATE_PARAM);
            PlainDate endDate = (PlainDate) getParameter(END_DATE_PARAM);

            Criteria incidentCriteria = new Criteria();
            incidentCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, startDate);
            incidentCriteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, endDate);
            incidentCriteria.addEqualTo(ConductIncident.COL_STUDENT_OID, m_student.getOid());

            Criteria actionCriteria = new Criteria();
            actionCriteria.addEqualTo(ConductAction.COL_STUDENT_OID, m_student.getOid()); // Not
                                                                                          // required
                                                                                          // - but
                                                                                          // greatly
                                                                                          // improves
                                                                                          // query
                                                                                          // performance
            actionCriteria.addIn(ConductAction.COL_INCIDENT_OID,
                    new SubQuery(ConductIncident.class, X2BaseBean.COL_OID, incidentCriteria));

            QueryByCriteria actionQuery = new QueryByCriteria(ConductAction.class, actionCriteria);
            actionQuery.addOrderByAscending(ConductAction.COL_ACTION_START_DATE);
            actionQuery.addOrderByAscending(ConductAction.COL_ACTION_CODE);
            Map actions =
                    getBroker().getGroupedCollectionByQuery(actionQuery,
                            ConductAction.COL_INCIDENT_OID, 100);

            QueryByCriteria incidentQuery =
                    new QueryByCriteria(ConductIncident.class, incidentCriteria);
            incidentQuery.addOrderByAscending(ConductIncident.COL_INCIDENT_DATE);
            incidentQuery.addOrderByAscending(ConductIncident.COL_INCIDENT_TIME);
            incidentQuery.addPrefetchedRelationship(ConductIncident.REL_REFERRAL_STAFF);
            incidentQuery.addPrefetchedRelationship(ConductIncident.REL_OWNER);

            QueryIterator incidents = null;
            try {
                incidents = getBroker().getIteratorByQuery(incidentQuery);
                while (incidents.hasNext()) {
                    ConductIncident incident = (ConductIncident) incidents.next();

                    List actionList = (List) actions.get(incident.getOid());
                    if (actionList != null) {
                        Iterator actionIterator = actionList.iterator();
                        while (actionIterator.hasNext()) {
                            ConductAction action = (ConductAction) actionIterator.next();
                            grid.append();
                            grid.set(FIELD_INCIDENT, incident);
                            grid.set(FIELD_ACTION, action);
                            grid.set(FIELD_CONTAINS_ACTIONS, Boolean.TRUE);
                        }
                    } else {
                        grid.append();
                        grid.set(FIELD_INCIDENT, incident);
                        grid.set(FIELD_CONTAINS_ACTIONS, Boolean.FALSE);
                    }
                }
            } finally {
                if (incidents != null) {
                    incidents.close();
                }
            }
        }

        // Add the time formatter
        SimpleDateFormat timeFormat = new SimpleDateFormat("h:mm a");
        addParameter(PARAM_TIME_FORMAT, timeFormat);

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
        m_student = userData.getCurrentRecord(SisStudent.class);
    }
}
