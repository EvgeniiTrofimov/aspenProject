package com.x2dev.reports.sys.sped.dodea;

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
import com.follett.fsc.core.k12.beans.Workflow;
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.workflow.Checklist;
import com.follett.fsc.core.k12.business.workflow.ChecklistEntry;
import com.follett.fsc.core.k12.business.workflow.ComplianceManager.ComplianceInfo;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Report that displays workflow compliance information across owners. The compliance information
 * is for a specific workflow phase that has one or more compliance rules attached.
 *
 * @author X2 Development Corporation
 */
public class DodeaWorkflowComplianceAlertsData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Grid field containing the case manager's name view.
     */
    public static final String GRID_CASE_MANAGER = "caseManager";

    /**
     * Grid field containing the ComplianceInfo object, which wraps compliance a compliance rule
     * calculation.
     */
    public static final String GRID_COMPLIANCE_INFO = "complianceInfo";

    /**
     * Grid field containing the number of days left to complete a task. This can contain a positive
     * or negative integer.
     */
    public static final String GRID_DAYS_LEFT = "daysLeft";

    /**
     * Grid field containing the date that a task is due.
     */
    public static final String GRID_DUE_DATE = "dueDate";

    /**
     * Grid field containing the school placement.
     */
    public static final String GRID_PLACEMENT = "placement";

    /**
     * Grid field containing the type of compliance information "Violation" or "Warning".
     */
    public static final String GRID_TYPE = "type";

    /**
     * Grid field containing the workflow owner name view.
     */
    public static final String GRID_OWNER = "owner";

    /**
     * Grid field containing the workflow owner local ID.
     */
    public static final String GRID_OWNER_ID = "ownerId";

    /**
     * Grid field containing the school name.
     */
    public static final String GRID_SCHOOL = "school";

    /**
     * Grid field for the workflow and phase name.
     */
    public static final String GRID_WORKFLOW_NAME = "workflowName";

    /**
     * Grid field containing the workflow progress instance. This may be null if the phase has not
     * been completed, or the phase is not currently available for completion.
     */
    public static final String GRID_WORKFLOW_PROGRESS = "progress";

    /**
     * Input parameter containing the initiated end date. This is an optional parameter used to
     * narrow down the results to workflows initiated on or before a specific date.
     */
    public static final String PARAM_INITIATED_END_DATE = "initiatedEndDate";

    /**
     * Input parameter containing the initiated start date. This is an optional parameter used to
     * narrow down the results to workflows initiated on or after a specific date.
     */
    public static final String PARAM_INITIATED_START_DATE = "initiatedStartDate";

    /**
     * Input parameter that specifies if the results should be limited to warnings and violations.
     */
    public static final String PARAM_WARNINGS_AND_VIOLATIONS = "warningsAndViolations";
    public static final String PARAM_SORT = "sort";

    public static final String VALUE_WARNING = "warnings";
    public static final String VALUE_VIOLATIONS = "violations";
    public static final String VALUE_BOTH = "both";

    private Class m_currentClass = null;

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

        /*
         * Gather input parameter values
         */
        PlainDate startDate = (PlainDate) getParameter(PARAM_INITIATED_START_DATE);
        PlainDate endDate = (PlainDate) getParameter(PARAM_INITIATED_END_DATE);
        String warningsAndViolations = (String) getParameter(PARAM_WARNINGS_AND_VIOLATIONS);
        String sort = (String) getParameter(PARAM_SORT);

        boolean warnings = false;
        boolean violations = false;
        if (VALUE_BOTH.contentEquals(warningsAndViolations)) {
            warnings = true;
            violations = true;
        } else if (VALUE_WARNING.contentEquals(warningsAndViolations)) {
            warnings = true;
        } else if (VALUE_VIOLATIONS.contentEquals(warningsAndViolations)) {
            violations = true;
        }

        /*
         * Build a query that finds the workflows to analyze.
         */
        Criteria criteria = new Criteria();
        if (m_currentClass != null) {
            criteria.addIn(Workflow.COL_OWNER_OID,
                    new SubQuery(m_currentClass, X2BaseBean.COL_OID, getCurrentCriteria()));
        }
        if (!OrganizationManager.ROOT_ORGANIZATION.contentEquals(getOrganization().getOid()) && 
                m_currentClass.equals(IepData.class)) {
            X2Criteria orgCriteria = OrganizationManager.getOrganizationCriteria(getOrganization().getOid(), IepData.REL_STUDENT);
            SubQuery orgQuery = new SubQuery(IepData.class, X2BaseBean.COL_OID, orgCriteria);
            criteria.addIn(Workflow.COL_OWNER_OID, orgQuery);
        }
        if (isSchoolContext() && m_currentClass.equals(IepData.class)) {
            X2Criteria schoolCriteria = new X2Criteria();
            schoolCriteria.addEqualTo(IepData.REL_STUDENT + "." + 
                   SisStudent.COL_SCHOOL_OID, getSchool().getOid());
            SubQuery schoolQuery = new SubQuery(IepData.class, X2BaseBean.COL_OID, schoolCriteria);
            criteria.addIn(Workflow.COL_OWNER_OID, schoolQuery);
        }
        if (startDate != null) {
            criteria.addGreaterOrEqualThan(Workflow.COL_DATE_INITIATED, startDate);
        }

        if (endDate != null) {
            criteria.addLessOrEqualThan(Workflow.COL_DATE_INITIATED, endDate);
        }

        QueryByCriteria query = new QueryByCriteria(Workflow.class, criteria);
        if ("workflow".equals(sort)) {
            query.addOrderByAscending(Workflow.REL_WORKFLOW_DEFINITION +
                    ModelProperty.PATH_DELIMITER + WorkflowDefinition.COL_NAME);
            query.addOrderByAscending(Workflow.COL_OWNER_VIEW);
            query.addOrderByAscending(Workflow.COL_OWNER_OID);
        } else {
            query.addOrderByAscending(Workflow.COL_OWNER_VIEW);
            query.addOrderByAscending(Workflow.COL_OWNER_OID);
        }

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            /*
             * Iterate over the results of the query and populate the grid.
             */
            while (iterator.hasNext()) {
                Workflow workflow = (Workflow) iterator.next();
                WorkflowDefinition workflowDefinition = workflow.getWorkflowDefinition();

                SisStaff caseManager = null;
                String placementProgram = null;
                String ownerView = workflow.getOwnerView();
                String ownerId = null;
                String schoolName = null;
                X2BaseBean owner = workflow.getOwner();
                if (owner instanceof IepData) {
                    IepData iep = (IepData) owner;
                    caseManager = iep.getStaff();
                    placementProgram = iep.getPlacementsView();
                    if (StringUtils.isEmpty(ownerView)) {
                        ownerView = iep.getStudent().getNameView();
                    }
                    ownerId = iep.getStudent().getLocalId();
                    schoolName = iep.getStudent().getSchool().getName();
                }

                Collection<ChecklistEntry> checklistData = null;
                try {
                    Checklist checklist =
                            new Checklist(workflow, getUser().getOid(), null, getPrivilegeSet(), getBroker());
                    checklistData = checklist.getChecklistData();
                } catch (NullPointerException e) {
                    // A bug in 6.4 and earlier can cause a null pointer.
                    // we can ignore it and skip the workflow.
                }

                if (checklistData != null) {
                    ChecklistEntry lastEntry = null;
                    /*
                     * Find the first progress entry that is not complete.
                     */
                    for (ChecklistEntry entry : checklistData) {
                        if (!entry.isComplete()) {
                            lastEntry = entry;
                            break;
                        }
                    }

                    if (lastEntry != null) {
                        /*
                         * Calculate compliance information from that phase.
                         */
                        ComplianceInfo complianceInfo = lastEntry.getComplianceInfo();
                        if (complianceInfo != null &&
                                ((complianceInfo.isViolation() && violations) ||
                                        (complianceInfo.isWarning(lastEntry.getProgressEntry()) && warnings))) {

                            String workflowName = workflowDefinition.getName() + "\n" + lastEntry.getPhase().getName();

                            grid.append();
                            grid.set(GRID_COMPLIANCE_INFO, complianceInfo);
                            grid.set(GRID_DUE_DATE, complianceInfo.dueDate);
                            grid.set(GRID_DAYS_LEFT, Integer.valueOf(complianceInfo.daysLeft));
                            grid.set(GRID_OWNER, ownerView);
                            grid.set(GRID_OWNER_ID, ownerId);
                            grid.set(GRID_SCHOOL, schoolName);
                            grid.set(GRID_WORKFLOW_NAME, workflowName);
                            grid.set(GRID_WORKFLOW_PROGRESS, lastEntry.getProgressEntry());
                            grid.set(GRID_CASE_MANAGER, caseManager);
                            grid.set(GRID_PLACEMENT, placementProgram);

                            String type = null;
                            if (complianceInfo.isViolation()) {
                                type = "Violation";
                            } else if (complianceInfo.isWarning(lastEntry.getProgressEntry())) {
                                type = "Warning";
                            }

                            grid.set(GRID_TYPE, type);
                        }
                    }
                }
            }
        } finally {
            iterator.close();
        }

        grid.sort(GRID_DUE_DATE, false);
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
}
