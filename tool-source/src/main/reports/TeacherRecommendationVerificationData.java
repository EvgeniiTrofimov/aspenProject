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
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.CourseRequestAdjustment;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.List;
import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.data.JRBeanCollectionDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for "Teacher Recommendation Verification" report.
 * It finds all sections whether teachers have or have not make the recommendation.
 * 
 * @author X2 Development Corporation
 */
public class TeacherRecommendationVerificationData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Report parameter name for the map of section OIDs to CourseRequestAdjustment beans. The value
     * is an
     * Map.
     */
    public static final String MASTER_TO_RECOMMEND_MAP_PARAM = "masterOidToRecommendBeans";

    /**
     * Name for "query string" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Report parameter name for preference to show "Not recommend" only. This value is
     * a boolean
     */
    private static final String SHOW_NO_RECOMMEND_ONLY_INPUT = "nonRecommendationOnly";

    /**
     * Parameter used to obtain which column to sort by. This value is a String.
     */
    private static final String SORT_INPUT = "sort";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        // Grab input parameters
        boolean showNoPostsOnly = ((Boolean) getParameter(SHOW_NO_RECOMMEND_ONLY_INPUT)).booleanValue();
        String department = (String) getParameter(QUERY_STRING_PARAM);

        /*
         * Retrieve the list of school courses that teachers are allowed to make recommendation for.
         */
        X2Criteria schoolCourseCriteria = new X2Criteria();
        schoolCourseCriteria.addEqualTo(SchoolCourse.COL_SCHOOL_OID, getSchool().getOid());
        schoolCourseCriteria.addEqualTo(SchoolCourse.REL_COURSE + "." + Course.COL_DISTRICT_CONTEXT_OID,
                getCurrentContext().getOid());
        schoolCourseCriteria.addEqualTo(SchoolCourse.COL_ALLOW_RECOMMENDATION_INDICATOR, Boolean.TRUE);

        if (StringUtils.isEmpty(department)) {
            schoolCourseCriteria.addEqualTo(SchoolCourse.COL_DEPARTMENT_CODE, department.trim());
        }

        SubQuery schoolCourseQuery = new SubQuery(SchoolCourse.class, X2BaseBean.COL_OID, schoolCourseCriteria);

        X2Criteria sectionCriteria = new X2Criteria();
        sectionCriteria.addEqualTo(MasterSchedule.COL_SCHEDULE_OID, ((SisSchool) getSchool()).getActiveScheduleOid());
        sectionCriteria.addIn(MasterSchedule.COL_SCHOOL_COURSE_OID, schoolCourseQuery);

        /*
         * Retreive the list of request adjustment coming from teacher recommendation.
         */
        X2Criteria recommendedCriteria = new X2Criteria();
        recommendedCriteria.addEqualTo(CourseRequestAdjustment.COL_DISTRICT_CONTEXT_OID,
                getSchool().getBuildContextOid());
        recommendedCriteria.addNotEmpty(CourseRequestAdjustment.COL_SOURCE, getBroker().getPersistenceKey());

        QueryByCriteria recommendedQuery = new QueryByCriteria(CourseRequestAdjustment.class, recommendedCriteria);
        addParameter(MASTER_TO_RECOMMEND_MAP_PARAM,
                getBroker().getGroupedCollectionByQuery(recommendedQuery, CourseRequestAdjustment.COL_SOURCE, 100));

        if (showNoPostsOnly) {
            SubQuery recommendedSubQuery = new SubQuery(CourseRequestAdjustment.class,
                    CourseRequestAdjustment.COL_SOURCE, recommendedCriteria);
            sectionCriteria.addNotIn(X2BaseBean.COL_OID, recommendedSubQuery);
        }

        QueryByCriteria sectionQuery = createQueryByCriteria(MasterSchedule.class, sectionCriteria);
        applyUserSort(sectionQuery, (String) getParameter(SORT_INPUT));

        List<MasterSchedule> sortedSections =
                new ArrayList<MasterSchedule>(getBroker().getCollectionByQuery(sectionQuery));

        return new JRBeanCollectionDataSource(sortedSections);
    }
}
