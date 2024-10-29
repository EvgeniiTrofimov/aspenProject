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
package com.x2dev.reports.sys.pd;


import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.PdGoal;
import com.x2dev.sis.model.beans.StaffPdActivity;
import com.x2dev.sis.model.beans.StaffPdPlan;
import com.x2dev.sis.model.beans.StaffPdPlanGoal;
import com.x2dev.sis.model.beans.StaffPdPlanGoalAlignment;
import com.x2dev.utils.CollectionUtils;
import java.util.Collection;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the professional development plan report. This is a multi-page report comprised
 * of 4 subreports:
 * <ul>
 * <li>Page 1: A ReportDataGrid containing personal and aligned district goals
 * <li>Page 2: A BeanCollectionDataSource containing action plans
 * <li>Page 3: A BeanCollectionDataSource containing activities
 * <li>Page 4: A BeanCollectionDataSource containing plan reviews
 * </ul>
 *
 * @author X2 Development Corporation
 */
public class PdPlanData extends MultiPageFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    // Column constants for the Page 1 ReportDataGrid
    private static final String COL_DISTRICT_GOAL = "districtGoal";
    private static final String COL_STAFF_GOAL = "staffGoal";
    private static final String COL_GOAL_DESCRIPTION = "goalDescription";
    private static final String COL_GOAL_ID = "goalId";
    private static final String COL_GOAL_TEXT = "goalText";
    private static final String COL_GOAL_TYPE = "goalType";
    private static final String COL_PLAN = "plan";
    private static final String COL_STAFF = "staff";

    private StaffPdPlan m_pdPlan = null;

    /**
     * Gets the format ids.
     *
     * @return String[]
     * @see com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#getFormatIds()
     */
    @Override
    protected String[] getFormatIds() {
        return new String[] {"SYS-PD-002-1", "SYS-PD-002-2", "SYS-PD-002-3", "SYS-PD-002-4"};
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        // Suppress initialization - there is currently no form behind this report

        setDictionary(DataDictionary.getDistrictDictionary(getUser().getPersistenceKey()));
    }

    /**
     * Prepare page.
     *
     * @param grid ReportDataGrid
     * @see
     *      com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#preparePage(com.follett.
     *      fsc.core.k12.tools.reports.ReportDataGrid)
     */
    @Override
    protected void preparePage(ReportDataGrid grid) {
        switch (getCurrentPageNumber()) {
            case 1:
                prepagePage1(grid);
                break;

            case 2:
                preparePage2(grid);
                break;

            case 3:
                preparePage3(grid);
                break;

            case 4:
                preparePage4(grid);
                break;

            default:
                super.preparePage(grid);
        }
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
        m_pdPlan = userData.getCurrentRecord(StaffPdPlan.class);
    }

    /**
     * Prepares the first page - a ReportDataGrid containing personal and aligned district goals.
     * The grid is populated with the staff member's personal goals, followed by the unique set
     * of aligned district goals. The COL_GOAL_TYPE column is set to "staff" or "district",
     * respectively.
     *
     * @param grid ReportDataGrid
     */
    private void prepagePage1(ReportDataGrid grid) {
        ReportDataGrid currentPage = new ReportDataGrid();

        // Goals
        for (StaffPdPlanGoal staffGoal : m_pdPlan.getPdpGoals(getBroker())) {
            currentPage.append();
            currentPage.set(COL_GOAL_TYPE, "staff");
            currentPage.set(COL_STAFF_GOAL, staffGoal);
            currentPage.set(COL_GOAL_TEXT, staffGoal.getGoal());
            currentPage.set(COL_GOAL_DESCRIPTION, staffGoal.getDescription());
            currentPage.set(COL_GOAL_ID, staffGoal.getId());
            currentPage.set(COL_STAFF, m_pdPlan.getStaff());
            currentPage.set(COL_PLAN, m_pdPlan);
        }

        // Organization alignments
        Criteria districtAlignmentCriteria = new Criteria();
        districtAlignmentCriteria.addEqualTo(StaffPdPlanGoalAlignment.REL_STAFF_PD_PLAN_GOAL + "." +
                StaffPdPlanGoal.COL_PD_PLAN_OID, m_pdPlan.getOid());

        SubQuery districtAlignmentQuery = new SubQuery(StaffPdPlanGoalAlignment.class,
                StaffPdPlanGoalAlignment.COL_PD_GOAL_OID, districtAlignmentCriteria);

        Criteria districtGoalCriteria = new Criteria();
        districtGoalCriteria.addIn(X2BaseBean.COL_OID, districtAlignmentQuery);

        QueryByCriteria districtGoalQuery = new QueryByCriteria(PdGoal.class, districtGoalCriteria);
        districtGoalQuery.addOrderByAscending(PdGoal.COL_ID);

        QueryIterator districtGoals = getBroker().getIteratorByQuery(districtGoalQuery);
        try {
            while (districtGoals.hasNext()) {
                PdGoal districtGoal = (PdGoal) districtGoals.next();

                currentPage.append();
                currentPage.set(COL_GOAL_TYPE, "district");
                currentPage.set(COL_DISTRICT_GOAL, districtGoal);
                currentPage.set(COL_GOAL_TEXT, districtGoal.getGoal());
                currentPage.set(COL_GOAL_DESCRIPTION, districtGoal.getDescription());
                currentPage.set(COL_GOAL_ID, districtGoal.getId());
                currentPage.set(COL_STAFF, m_pdPlan.getStaff());
                currentPage.set(COL_PLAN, m_pdPlan);
            }
        } finally {
            districtGoals.close();
        }

        currentPage.beforeTop();

        prepareCurrentPage(grid, currentPage);
    }

    /**
     * Prepares the second page data source.
     *
     * @param grid ReportDataGrid
     */
    private void preparePage2(ReportDataGrid grid) {
        BeanCollectionDataSource dataSource =
                new BeanCollectionDataSource(m_pdPlan.getStaffPdActionPlans(getBroker()), getDictionary(), getLocale());
        prepareCurrentPage(grid, dataSource);
    }

    /**
     * Prepares the third page data source.
     *
     * @param grid ReportDataGrid
     */
    private void preparePage3(ReportDataGrid grid) {
        Collection<StaffPdActivity> activities = m_pdPlan.getStaffPdActivities(getBroker());
        activities = CollectionUtils.sortBeans(activities, StaffPdActivity.COL_PRIMARY_INDICATOR, true, false);
        BeanCollectionDataSource dataSource = new BeanCollectionDataSource(activities, getDictionary(), getLocale());
        prepareCurrentPage(grid, dataSource);
    }

    /**
     * Prepares the fourth page data source.
     *
     * @param grid ReportDataGrid
     */
    private void preparePage4(ReportDataGrid grid) {
        BeanCollectionDataSource dataSource =
                new BeanCollectionDataSource(m_pdPlan.getPdpReviews(getBroker()), getDictionary(), getLocale());
        prepareCurrentPage(grid, dataSource);
    }
}
