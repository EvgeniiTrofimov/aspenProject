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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.CourseRequestAdjustment;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.web.gradebook.LimitedColumnScoreGrid;
import com.x2dev.sis.web.schedule.ScheduleUtils;
import java.util.HashMap;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Prepares the data for the "Student Request" report.
 *
 * @author X2 Development Corporation
 */
public class CourseRequestRecommendationData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for source look up map parameter.
     */
    public static final String SOURCE_LOOKUP_MAP_PARAM = "sourceLookupMap";

    /**
     * Name for the enumerated "student sort" report parameter. The value is a String.
     */
    public static final String STUDENT_SORT_PARAM = "studentSort";

    private MasterSchedule m_currentSection;
    private SisSchool m_currentSchool;
    private SisStaff m_currentStaff;
    private Schedule m_schedule;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Criteria criteria = new Criteria();

        if (m_currentSection != null) {
            criteria.addEqualTo(CourseRequestAdjustment.COL_SOURCE, m_currentSection.getOid());
            criteria.addEqualTo(CourseRequestAdjustment.COL_DISTRICT_CONTEXT_OID, m_currentSchool.getBuildContextOid());
        } else {
            criteria.addEqualTo(CourseRequestAdjustment.COL_DISTRICT_CONTEXT_OID, m_currentSchool.getBuildContextOid());
            criteria.addEqualTo(CourseRequestAdjustment.COL_SCHOOL_OID, m_currentSchool.getOid());

            X2Criteria sectionCriteria = new X2Criteria();
            sectionCriteria.addEqualTo(MasterSchedule.COL_SCHEDULE_OID, m_schedule.getOid());
            sectionCriteria.addEqualTo(MasterSchedule.COL_PRIMARY_STAFF_OID, m_currentStaff.getOid());

            SubQuery sectionSubQuery = new SubQuery(MasterSchedule.class, X2BaseBean.COL_OID, sectionCriteria);
            criteria.addIn(CourseRequestAdjustment.COL_SOURCE, sectionSubQuery);
        }

        criteria.addEqualTo(CourseRequestAdjustment.COL_TYPE,
                Integer.valueOf(CourseRequestAdjustment.TypeCode.Recommended.ordinal()));

        QueryByCriteria query = new QueryByCriteria(CourseRequestAdjustment.class, criteria);
        query.addOrderByAscending(CourseRequestAdjustment.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_NUMBER);
        query.addOrderByAscending(CourseRequestAdjustment.COL_SCHOOL_COURSE_OID);

        // Add student sort
        applyUserSort(query, (String) getParameter(STUDENT_SORT_PARAM));
        query.addOrderByAscending(CourseRequestAdjustment.COL_STUDENT_OID);

        /*
         * Source section map
         */
        SubQuery sourceOidQuery =
                new SubQuery(CourseRequestAdjustment.class, CourseRequestAdjustment.COL_SOURCE, criteria);

        X2Criteria sourceCriteria = new X2Criteria();
        sourceCriteria.addIn(X2BaseBean.COL_OID, sourceOidQuery);

        String[] columns =
                new String[] {X2BaseBean.COL_OID, MasterSchedule.COL_COURSE_VIEW, MasterSchedule.COL_DESCRIPTION};
        ReportQueryByCriteria sourceQuery = new ReportQueryByCriteria(MasterSchedule.class, columns, sourceCriteria);

        Map<String, String> sourceMap = new HashMap<String, String>();
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(sourceQuery);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                sourceMap.put((String) row[0], (String) row[1] + " " + (String) row[2]);
            }
        } finally {
            iterator.close();
        }


        addParameter(SOURCE_LOOKUP_MAP_PARAM, sourceMap);

        /*
         * Execute the query
         */
        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see
     *      com.x2dev.sis.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        m_currentSchool = (SisSchool) getSchool();

        if (userData.getSessionNavConfig().getApplicationContext() == ApplicationContext.STAFF &&
                !userData.getCurrentNode().isList()) {
            m_currentSection = ((LimitedColumnScoreGrid) userData.getCurrentGrid()).getSection();
        }

        if (userData.getSessionNavConfig().getApplicationContext() == ApplicationContext.STAFF) {
            m_currentStaff = (SisStaff) userData.getStaff();
        }

        m_schedule = ScheduleUtils.getSchedule(userData);
    }
}
