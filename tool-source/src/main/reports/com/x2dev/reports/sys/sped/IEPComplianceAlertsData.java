package com.x2dev.reports.sys.sped;

import com.follett.fsc.core.framework.persistence.BeanQuery;
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
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.List;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Report that displays workflow compliance information across owners. The compliance information
 * is for a specific workflow phase that has one or more compliance rules attached.
 *
 * @author X2 Development Corporation
 */
public class IEPComplianceAlertsData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Grid field containing the IepData record.
     */
    public static final String GRID_IEP = "iep";

    /**
     * Grid field containing the draft or pending Iep for the student.
     */
    public static final String GRID_PENDING = "pending";

    /**
     * The days left until the review and evaluation requirement date.
     */
    public static final String GRID_REVIEW_DAYS = "reviewDays";
    public static final String GRID_EVAL_DAYS = "evalDays";

    /**
     * Input parameter containing the number of days until the evaluation date threshold for
     * warning.
     */
    public static final String PARAM_EVAL_DAYS = "evalDays";

    /**
     * Input parameter containing the number of days until the review date threshold for warning.
     */
    public static final String PARAM_REVIEW_DAYS = "reviewDays";

    /**
     * Input parameter that specifies if the results should be limited to warnings and violations.
     */
    public static final String PARAM_WARNINGS_AND_VIOLATIONS = "warningsAndViolations";

    /**
     * Input parameter that specifies if the results should include review date, eval date or both.
     */
    public static final String PARAM_REVIEW_AND_EVAL = "reviewAndEval";

    /**
     * Input parameter for the report sort option.
     */
    public static final String PARAM_SORT = "sort";


    // Days in milliseconds.
    private static final long DAYS = (1000 * 60 * 60 * 24);
    public static final String VALUE_REVIEW = "review";
    public static final String VALUE_EVAL = "eval";
    public static final String VALUE_WARNING = "warnings";
    public static final String VALUE_VIOLATIONS = "violations";
    public static final String VALUE_BOTH = "both";

    private Class m_currentClass = null;
    private long m_today = 0;
    private boolean m_warnings = false;
    private boolean m_violations = false;
    private boolean m_reviews = false;
    private boolean m_evals = false;
    private List<Integer> m_pendingStatus = null;

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();
        m_today = (new PlainDate()).getTime();
        m_pendingStatus = new ArrayList<Integer>(2);
        m_pendingStatus.add(Integer.valueOf(IepData.StatusCode.DRAFT.ordinal()));
        m_pendingStatus.add(Integer.valueOf(IepData.StatusCode.PENDING_APPROVAL.ordinal()));

        /*
         * Gather input parameter values
         */
        Integer evalDays = (Integer) getParameter(PARAM_EVAL_DAYS);
        Integer reviewDays = (Integer) getParameter(PARAM_REVIEW_DAYS);
        String sort = (String) getParameter(PARAM_SORT);

        String warningsAndViolations = (String) getParameter(PARAM_WARNINGS_AND_VIOLATIONS);
        if (VALUE_BOTH.contentEquals(warningsAndViolations)) {
            m_warnings = true;
            m_violations = true;
        } else if (VALUE_WARNING.contentEquals(warningsAndViolations)) {
            m_warnings = true;
        } else if (VALUE_VIOLATIONS.contentEquals(warningsAndViolations)) {
            m_violations = true;
        }

        String reviewAndEvals = (String) getParameter(PARAM_REVIEW_AND_EVAL);
        if (VALUE_BOTH.contentEquals(reviewAndEvals)) {
            m_reviews = true;
            m_evals = true;
        } else if (VALUE_REVIEW.contentEquals(reviewAndEvals)) {
            m_reviews = true;
        } else if (VALUE_EVAL.contentEquals(reviewAndEvals)) {
            m_evals = true;
        }

        /*
         * Build a query that finds the workflows to analyze.
         */
        Criteria criteria = new Criteria();
        if (m_currentClass != null) {
            SubQuery listCriteria = new SubQuery(m_currentClass, X2BaseBean.COL_OID, getCurrentCriteria());
            if (IepData.class.isAssignableFrom(m_currentClass)) {
                criteria.addIn(X2BaseBean.COL_OID, listCriteria);
            } else if (Student.class.isAssignableFrom(m_currentClass)) {
                criteria.addIn(IepData.COL_STUDENT_OID, listCriteria);
            }
        }
        criteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()));
        QueryByCriteria query = new QueryByCriteria(IepData.class, criteria);

        if ("startDate".equals(sort)) {
            query.addOrderBy(IepData.COL_START_DATE, false);
            query.addOrderBy(IepData.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                    SisStudent.COL_NAME_VIEW, true);
        } else {
            query.addOrderBy(IepData.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                    SisStudent.COL_NAME_VIEW, true);
        }

        QueryIterator<IepData> iterator = getBroker().getIteratorByQuery(query);
        try {
            /*
             * Iterate over the results of the query and populate the grid.
             */
            while (iterator.hasNext()) {
                IepData iep = iterator.next();
                checkIEP(iep, grid, evalDays, reviewDays);
            }
        } finally {
            iterator.close();
        }
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
        if (userData.getCurrentNode().isList()) {
            m_currentClass = userData.getCurrentList().getDataClass();
        }
    }

    /**
     * Check an IEP for next review date and next evaluation date.
     *
     * @param iep
     * @param grid
     * @param evalDays
     * @param reviewDays
     */
    private void checkIEP(IepData iep, ReportDataGrid grid, Integer evalDays, Integer reviewDays) {
        PlainDate nextReviewDate = iep.getNextReviewDate();
        PlainDate nextEvalDate = iep.getNextEvaluationDate();

        long nextReview = 0;
        boolean reviewViolation = false;
        boolean reviewWarning = false;
        if (nextReviewDate != null && m_reviews) {
            long nrd = nextReviewDate.getTime();
            nextReview = (nrd - m_today) / DAYS;
            if (nextReview < 0) {
                reviewViolation = m_violations;
            } else if (nextReview < reviewDays.intValue()) {
                reviewWarning = m_warnings;
            }
        }

        long nextEval = 0;
        boolean evalViolation = false;
        boolean evalWarning = false;
        if (nextEvalDate != null && m_evals) {
            long ned = nextEvalDate.getTime();
            nextEval = (ned - m_today) / DAYS;
            if (nextEval < 0) {
                evalViolation = m_violations;
            } else if (nextEval < evalDays.intValue()) {
                evalWarning = m_warnings;
            }
        }
        if (reviewWarning || evalWarning || reviewViolation || evalViolation) {
            grid.append();
            grid.set(GRID_IEP, iep);
            SisStudent student = iep.getStudent();
            if (student != null) {
                IepData pendingIep = getPendingIep(student);
                if (pendingIep != null) {
                    grid.set(GRID_PENDING, pendingIep);
                }
            }
            if (reviewWarning || reviewViolation) {
                grid.set(GRID_REVIEW_DAYS, Long.valueOf(nextReview));
            }
            if (evalWarning || evalViolation) {
                grid.set(GRID_EVAL_DAYS, Long.valueOf(nextEval));
            }
        }
    }

    /**
     * Find any draft or pending IEP for this student.
     *
     * @param student
     *
     * @return IepData
     */
    private IepData getPendingIep(SisStudent student) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(IepData.COL_STUDENT_OID, student.getOid());
        criteria.addIn(IepData.COL_STATUS_CODE, m_pendingStatus);
        BeanQuery query = new BeanQuery(IepData.class, criteria);
        return (IepData) getBroker().getBeanByQuery(query);
    }
}
