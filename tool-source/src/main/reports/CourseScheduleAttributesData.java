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
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SchoolCourseSchedule;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.web.SisUserDataContainer;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares data for the "Course Schedule Attributes" report.
 *
 * @author X2 Development Corporation
 */
public class CourseScheduleAttributesData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the enumerated "selection" report parameter. The value is a String.
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
        Criteria criteria = new Criteria();

        addUserCriteria(criteria, (String) getParameter(QUERY_BY_PARAM), null, null, null);
        criteria.addEqualTo(SchoolCourseSchedule.COL_SCHEDULE_OID, m_scheduleOid);

        QueryByCriteria query = createQueryByCriteria(SchoolCourseSchedule.class, criteria);

        applyUserSort(query, (String) getParameter(SORT_PARAM));
        query.addOrderByAscending(SchoolCourseSchedule.COL_SCHOOL_COURSE_OID);

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
        if (userData.getSessionNavConfig().getApplicationContext() == ApplicationContext.BUILD) {
            m_scheduleOid = ((SisUserDataContainer) userData).getBuildSchedule().getCourseScheduleOid();
        } else {
            m_scheduleOid = ((SisSchool) getSchool()).getActiveScheduleOid();
        }
    }
}
