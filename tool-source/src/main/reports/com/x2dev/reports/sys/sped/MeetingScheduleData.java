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

package com.x2dev.reports.sys.sped;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Report that displays a list of students along with their next scheduled meeting date. The
 * students can be printed for all or a selected case manager. The student's case manager is the
 * staff member identified on their active IEP.
 * <p>
 * The report can be sorted by student, next review, or next re-evaluation.
 *
 * @author X2 Development Corporation
 */
public class MeetingScheduleData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Input parameter containing the OID of the case manager to display. If null, all students are
     * displayed.
     */
    public static final String PARAM_CASE_MANAGER = "caseManagerOid";

    /**
     * Input parameter containing the end date in the date range to display. If null, an end date is
     * not applied.
     */
    public static final String PARAM_END_DATE = "endDate";

    /**
     * Input parameter containing the integer that indentifies the query by option.
     */
    public static final String PARAM_QUERY_BY = "queryBy";

    /**
     * Input parameter containing the string that indentifies the query search value.
     */
    public static final String PARAM_QUERY_STRING = "queryString";

    /**
     * Input parameter containing the integer that indentifies the sort order.
     */
    public static final String PARAM_SORT = "sort";

    /**
     * Input parameter containing the start date in the date range to display. If null, start date
     * is not applied.
     */
    public static final String PARAM_START_DATE = "startDate";

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
        criteria.addEqualTo(SisStudent.COL_ORGANIZATION1_OID, getOrganization().getOid());

        addUserCriteria(criteria, (String) getParameter(PARAM_QUERY_BY), (String) getParameter(PARAM_QUERY_STRING),
                null, null);

        PlainDate startDate = (PlainDate) getParameter(PARAM_START_DATE);
        PlainDate endDate = (PlainDate) getParameter(PARAM_END_DATE);

        if (startDate != null || endDate != null) {
            Criteria reviewCriteria = new Criteria();
            Criteria evaluationCriteria = new Criteria();

            /*
             * Start date
             */
            if (startDate != null) {
                reviewCriteria.addGreaterOrEqualThan(SisStudent.COL_SPED_NEXT_REVIEW_DATE, startDate);
                evaluationCriteria.addGreaterOrEqualThan(SisStudent.COL_SPED_NEXT_EVALUATION_DATE, startDate);
            }

            /*
             * End date
             */
            if (endDate != null) {
                reviewCriteria.addLessOrEqualThan(SisStudent.COL_SPED_NEXT_REVIEW_DATE, endDate);
                evaluationCriteria.addLessOrEqualThan(SisStudent.COL_SPED_NEXT_EVALUATION_DATE, endDate);
            }

            Criteria dateCriteria = new Criteria();
            dateCriteria.addOrCriteria(reviewCriteria);
            dateCriteria.addOrCriteria(evaluationCriteria);

            criteria.addAndCriteria(dateCriteria);
        }

        /*
         * Case manager
         */
        String staffOid = (String) getParameter(PARAM_CASE_MANAGER);
        if (!StringUtils.isEmpty(staffOid)) {
            Criteria iepCriteria = new Criteria();
            iepCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()));
            iepCriteria.addEqualTo(IepData.COL_STAFF_OID, staffOid);
            iepCriteria.addEqualToField(Criteria.PARENT_QUERY_PREFIX + X2BaseBean.COL_OID,
                    IepData.COL_STUDENT_OID);

            SubQuery iepQuery = new SubQuery(IepData.class, X2BaseBean.COL_OID, iepCriteria);
            criteria.addExists(iepQuery);
        }

        QueryByCriteria query = createQueryByCriteria(SisStudent.class, criteria);
        applyUserSort(query, (String) getParameter(PARAM_SORT));

        QueryIterator students = getBroker().getIteratorByQuery(query);

        return new QueryIteratorDataSource(students,
                DataDictionary.getDistrictDictionary(getUser().getPersistenceKey()), true, getLocale());
    }
}
