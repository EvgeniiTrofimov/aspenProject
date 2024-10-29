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
package com.x2dev.procedures.sys.sped.ma;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehavior;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehaviorFactory;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class checkSpedExitDate.
 */
public class checkSpedExitDate extends ProcedureJavaSource {
    private static final String EMPTY = "";
    private static final String LABEL_APPLY_EXIT = "apply exit ";
    private static final String MESSAGE_EXIT_ON = " exit on ";
    private static final String PHASE_OID_WPH_X2_REA_EXIT = "wphX2ReaExit";

    private Collection<WorkflowProgress> m_progresses = new ArrayList<WorkflowProgress>();
    private SpedWorkflowBehavior m_workflowBehavior =
            SpedWorkflowBehaviorFactory.getWorkflowBehavior(getOrganization());

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        for (WorkflowProgress progress : m_progresses) {
            m_workflowBehavior.executeExit(progress, getOrganization(), getBroker());
            progress.setComment(EMPTY);
            getBroker().saveBean(progress);
            IepData iepData = (IepData) progress.getWorkflow().getOwner();
            logMessage(iepData.getStudent().getNameView() + MESSAGE_EXIT_ON + progress.getDate());
        }

    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        Criteria criteria = new X2Criteria();
        PlainDate currentDate = new PlainDate();
        criteria.addEqualTo(WorkflowProgress.COL_WORKFLOW_PHASE_OID, PHASE_OID_WPH_X2_REA_EXIT);
        criteria.addLike(WorkflowProgress.COL_COMMENT, LABEL_APPLY_EXIT + "%");
        criteria.addLessOrEqualThan(WorkflowProgress.COL_COMMENT, LABEL_APPLY_EXIT + currentDate);
        m_progresses = getBroker().getCollectionByQuery(new QueryByCriteria(WorkflowProgress.class, criteria));

    }

}
