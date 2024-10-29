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
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Workflow;
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.beans.WorkflowPhase;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.workflow.Checklist;
import com.follett.fsc.core.k12.business.workflow.ChecklistEntry;
import com.follett.fsc.core.k12.business.workflow.ComplianceManager.ComplianceInfo;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Report that displays workflow compliance information across owners. The compliance information
 * is for a specific workflow phase that has one or more compliance rules attached.
 *
 * @author X2 Development Corporation
 */
public class WorkflowComplianceData extends ReportJavaSourceNet {
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
     * Grid field containing the workflow instance.
     */
    public static final String GRID_WORKFLOW = "workflow";

    /**
     * Grid field containing the workflow progress instance. This may be null if the phase has not
     * been completed, or the phase is not currently available for completion.
     */
    public static final String GRID_WORKFLOW_PROGRESS = "progress";

    /**
     * Input parameter containing whether or not completed phases should be included.
     */
    public static final String PARAM_INCLUDE_COMPLETED = "includeCompleted";

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
     * Input parameter that specifies if the results should be limited to warnings and violations
     * only.
     */
    public static final String PARAM_WARNINGS_AND_VIOLATIONS_ONLY = "warningsAndViolationsOnly";

    /**
     * Input parameter containing the workflow definition to report compliance for.
     */
    public static final String PARAM_WORKFLOW_DEFINITION = "workflowDefinition";

    /**
     * Parameter containing the workflow phase to report compliance for. This parameter is set
     * by this class.
     */
    public static final String PARAM_WORKFLOW_PHASE = "workflowPhase";

    /**
     * Input parameter containing the OID of the workflow phase to report compliance for.
     */
    public static final String PARAM_WORKFLOW_PHASE_OID = "workflowPhaseOid";

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
        Boolean warningsAndViolationsOnly = (Boolean) getParameter(PARAM_WARNINGS_AND_VIOLATIONS_ONLY);
        String workflowPhaseOid = (String) getParameter(PARAM_WORKFLOW_PHASE_OID);
        PlainDate startDate = (PlainDate) getParameter(PARAM_INITIATED_START_DATE);
        PlainDate endDate = (PlainDate) getParameter(PARAM_INITIATED_END_DATE);
        Boolean includeCompleted = (Boolean) getParameter(PARAM_INCLUDE_COMPLETED);

        WorkflowPhase workflowPhase =
                (WorkflowPhase) getBroker().getBeanByOid(WorkflowPhase.class, workflowPhaseOid);

        addParameter(PARAM_WORKFLOW_DEFINITION, workflowPhase.getWorkflowDefinition());
        addParameter(PARAM_WORKFLOW_PHASE, workflowPhase);

        /*
         * Build a query that finds the workflows to analyze.
         */
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Workflow.REL_WORKFLOW_DEFINITION + "." +
                WorkflowDefinition.REL_WORKFLOW_PHASES + "." +
                X2BaseBean.COL_OID, workflowPhaseOid);

        if (m_currentClass != null) {
            criteria.addIn(Workflow.COL_OWNER_OID,
                    new SubQuery(m_currentClass, X2BaseBean.COL_OID, getCurrentCriteria()));
        }

        if (startDate != null) {
            criteria.addGreaterOrEqualThan(Workflow.COL_DATE_INITIATED, startDate);
        }

        if (endDate != null) {
            criteria.addLessOrEqualThan(Workflow.COL_DATE_INITIATED, endDate);
        }

        QueryByCriteria query = new QueryByCriteria(Workflow.class, criteria);
        query.addOrderByAscending(Workflow.COL_OWNER_VIEW);
        query.addOrderByAscending(Workflow.COL_OWNER_OID);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            /*
             * Iterate over the results of the query and populate the grid.
             */
            while (iterator.hasNext()) {
                Workflow workflow = (Workflow) iterator.next();

                SisStaff caseManager = null;
                String placementProgram = null;

                if (workflow.getOwner() instanceof IepData) {
                    IepData iep = (IepData) workflow.getOwner();
                    caseManager = iep.getStaff();
                    placementProgram = iep.getPlacementsView();
                }

                Checklist checklist = new Checklist(workflow, getUser().getOid(), null, getPrivilegeSet(), getBroker());

                Collection<ChecklistEntry> checklistData = checklist.getChecklistData();

                ChecklistEntry lastEntry = null;

                /*
                 * Find the last progress entry for the phase selected by the user.
                 */
                for (ChecklistEntry entry : checklistData) {
                    if (entry.getPhase().equals(workflowPhase)) {
                        lastEntry = entry;
                    }
                }

                if (lastEntry != null) {
                    /*
                     * Calculate compliance information from that phase.
                     */
                    ComplianceInfo complianceInfo = lastEntry.getComplianceInfo();
                    if (complianceInfo != null) {
                        /*
                         * Only include the information requested by the user
                         */
                        if ((!warningsAndViolationsOnly.booleanValue() ||
                                complianceInfo.isViolation() ||
                                complianceInfo.isWarning(lastEntry.getProgressEntry())) &&

                                (includeCompleted.booleanValue() ||
                                        (lastEntry.getProgressEntry() == null
                                                || !lastEntry.getProgressEntry().isOutcomeSelected()))) {
                            grid.append();
                            grid.set(GRID_COMPLIANCE_INFO, complianceInfo);
                            grid.set(GRID_DUE_DATE, complianceInfo.dueDate);
                            grid.set(GRID_DAYS_LEFT, Integer.valueOf(complianceInfo.daysLeft));
                            grid.set(GRID_WORKFLOW, workflow);
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
