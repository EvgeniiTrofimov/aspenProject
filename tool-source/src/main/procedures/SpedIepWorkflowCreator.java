/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
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
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.utils.types.PlainDate;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SpedIepWorkflowCreator.
 *
 * @author X2 Development Corporation
 */
public class SpedIepWorkflowCreator extends ProcedureJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private boolean debug = false;

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        // Initial referrals
        createWorkflowForIeps(IepMeeting.TypeCode.INITIAL,
                IepData.StatusCode.DRAFT,
                "wfdX2IepRefer",
                "wphX2RefCreate",
                "wphX2RefSendCf",
                "wpoX2RefCreate");

        // Reviews
        createWorkflowForIeps(IepMeeting.TypeCode.REVIEW,
                IepData.StatusCode.DRAFT,
                "WFD000000471xd",
                "WPH000000471xi",
                "WPH000000471xx",
                "WPO000000471y0");

        // Amendments
        createWorkflowForIeps(IepMeeting.TypeCode.AMENDMENT,
                IepData.StatusCode.DRAFT,
                "WFD0000004607M",
                "WPH000000474BY",
                "WPH000000462RK",
                "WPO000000474Be");

        // Re-evaluations
        createWorkflowForIeps(IepMeeting.TypeCode.REEVAL,
                IepData.StatusCode.DRAFT,
                "WFD00000046038",
                "WPH0000004603a",
                "WPH0000004603l",
                "WPO0000004603g");
    }

    /**
     * Creates the workflow for ieps.
     *
     * @param meetingType TypeCode
     * @param statusCode StatusCode
     * @param workflowDefOid String
     * @param wphPhase1Oid String
     * @param wphPhase2Oid String
     * @param wpoOutcome1Oid String
     */
    private void createWorkflowForIeps(IepMeeting.TypeCode meetingType,
                                       IepData.StatusCode statusCode,
                                       String workflowDefOid,
                                       String wphPhase1Oid,
                                       String wphPhase2Oid,
                                       String wpoOutcome1Oid) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepData.COL_MEETING_TYPE_CODE, Integer.valueOf(meetingType.ordinal()));
        criteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(statusCode.ordinal()));
        // criteria.addLike(X2BaseBean.COL_OID, "iep%");

        Criteria workflowCriteria = new Criteria();
        workflowCriteria.addEqualTo(Workflow.COL_WORKFLOW_DEFINITION_OID, workflowDefOid);
        SubQuery workflowSubQuery = new SubQuery(Workflow.class, Workflow.COL_OWNER_OID, workflowCriteria);
        criteria.addNotIn(X2BaseBean.COL_OID, workflowSubQuery);

        QueryByCriteria query = new QueryByCriteria(IepData.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                IepData iep = (IepData) iterator.next();
                createWorkflow(iep, workflowDefOid, wphPhase1Oid, wphPhase2Oid, wpoOutcome1Oid);
                logMessage("Created workflow for IEP : " + iep.getOid());
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Creates the workflow.
     *
     * @param iep IepData
     * @param workflowDefOid String
     * @param wphPhase1Oid String
     * @param wphPhase2Oid String
     * @param wpoOutcome1Oid String
     */
    private void createWorkflow(IepData iep,
                                String workflowDefOid,
                                String wphPhase1Oid,
                                String wphPhase2Oid,
                                String wpoOutcome1Oid) {
        Workflow workflow = X2BaseBean.newInstance(Workflow.class, getBroker().getPersistenceKey());
        workflow.setWorkflowDefinitionOid(workflowDefOid);
        workflow.setOwnerOid(iep.getOid());
        if (iep.getStudent() != null) {
            workflow.setOwnerView(iep.getStudent().getNameView());
        }
        workflow.setDateInitiated(new PlainDate());
        if (!debug) {
            getBroker().saveBeanForced(workflow);
        }

        WorkflowProgress progress = X2BaseBean.newInstance(WorkflowProgress.class, getBroker().getPersistenceKey());
        progress.setWorkflowOid(workflow.getOid());
        progress.setWorkflowPhaseOid(wphPhase1Oid);
        progress.setWorkflowPhaseOutcomeOid(wpoOutcome1Oid);
        progress.setDate(new PlainDate());
        if (!debug) {
            getBroker().saveBeanForced(progress);
        }

        WorkflowProgress progress2 = X2BaseBean.newInstance(WorkflowProgress.class, getBroker().getPersistenceKey());
        progress2.setWorkflowOid(workflow.getOid());
        progress2.setWorkflowPhaseOid(wphPhase2Oid);
        if (!debug) {
            getBroker().saveBeanForced(progress2);
        }
    }
}
