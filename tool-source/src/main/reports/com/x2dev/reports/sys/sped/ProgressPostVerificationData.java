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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.IepGoal;
import com.x2dev.sis.model.beans.IepGoalProgress;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Collects data needed for the progress report post verification report.
 *
 * @author X2 Development Corporation
 */
public class ProgressPostVerificationData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    public static final String CONTEXT_OID = "contextOid";

    /**
     * Report parameter name for the end date. This value is a PlainDate object.
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * Report column name which contains the IepGoalProgress bean.
     */
    public static final String FIELD_PROGRESS = "Progress";

    /**
     * Report parameter containing the reporting period to filter on. This parameter is optional.
     */
    public static final String REPORT_PERIOD = "reportPeriod";

    /**
     * Report column name which contains the school
     */
    public static final String SCHOOL_PARAM = "school";

    /**
     * Report parameter name for preference to show "No Posts" only. This value is
     * a boolean.
     */
    public static final String SHOW_NO_POSTS_ONLY_PARAM = "onlyNoPosts";

    /**
     * Report parameter name for the start date. This value is a PlainDate object.
     */
    public static final String START_DATE_PARAM = "startDate";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Object endDate = getParameter(END_DATE_PARAM);
        Object startDate = getParameter(START_DATE_PARAM);
        Object noPostsOnly = getParameter(SHOW_NO_POSTS_ONLY_PARAM);
        String postingPeriod = (String) getParameter(REPORT_PERIOD);
        String contextOid = (String) getParameter(CONTEXT_OID);

        X2Criteria criteria = new X2Criteria();

        if (startDate != null) {
            criteria.addGreaterOrEqualThan(IepGoalProgress.COL_DATE, startDate);
        }

        if (endDate != null) {
            criteria.addLessOrEqualThan(IepGoalProgress.COL_DATE, endDate);
        }

        if (noPostsOnly != null && ((Boolean) noPostsOnly).booleanValue()) {
            criteria.addNotEqualTo(IepGoalProgress.COL_POSTED_INDICATOR, Boolean.TRUE);
        }

        if (!StringUtils.isEmpty(postingPeriod)) {
            criteria.addEqualTo(IepGoalProgress.COL_REPORT_PERIOD, postingPeriod);
        }

        if (!StringUtils.isEmpty(contextOid)) {
            criteria.addEqualTo(IepGoalProgress.COL_DISTRICT_CONTEXT_OID, contextOid);
        }

        if (isSchoolContext()) {
            criteria.addEqualTo(IepGoalProgress.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else {
            X2Criteria studentOrgCriteria = getOrganizationCriteria(SisStudent.class);
            criteria.addIn(IepGoalProgress.COL_STUDENT_OID,
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentOrgCriteria));
        }

        ReportQueryByCriteria query = new ReportQueryByCriteria(IepGoalProgress.class, criteria);
        query.addOrderBy(IepGoalProgress.REL_STUDENT + "." + SisStudent.COL_NAME_VIEW, true);
        query.addOrderBy(IepGoalProgress.REL_IEP_GOAL + "." + IepGoal.COL_ID, true);

        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
    }
}
