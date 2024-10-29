/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolCourseSchedule;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Course Catalog report.
 *
 * @author X2 Development Corporation
 */
public class CourseCatalogData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the enumerated "selection" report parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    private ApplicationContext m_context;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        /*
         * Build the criteria based on user input
         */
        Criteria criteria = new Criteria();

        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 0: // Current selection
                if (m_context == ApplicationContext.BUILD || m_context == ApplicationContext.ELEMENTARY) {
                    Criteria buildCriteria = getCurrentCriteria();
                    SubQuery buildSubQuery = new SubQuery(SchoolCourseSchedule.class,
                            SchoolCourseSchedule.COL_SCHOOL_COURSE_OID, buildCriteria);
                    criteria.addIn(X2BaseBean.COL_OID, buildSubQuery);
                } else {
                    criteria = getCurrentCriteria();
                }
                break;

            case 2: // Year
                criteria.addEqualTo(SchoolCourse.REL_COURSE + "." + Course.REL_DISTRICT_CONTEXT + "." +
                        DistrictSchoolYearContext.COL_SCHOOL_YEAR, getParameter(QUERY_STRING_PARAM));
                break;

            default:
                // No additional criteria (this is the case for "All")
                break;
        }

        criteria.addEqualTo(SchoolCourse.COL_SCHOOL_OID, getSchool().getOid());

        QueryByCriteria query = new QueryByCriteria(SchoolCourse.class, criteria);

        /*
         * Build the sort based on user input
         */
        applyUserSort(query, ((String) getParameter(SORT_PARAM)));

        /*
         * Execute the query and return the results
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
        m_context = userData.getSessionNavConfig().getApplicationContext();
    }
}
