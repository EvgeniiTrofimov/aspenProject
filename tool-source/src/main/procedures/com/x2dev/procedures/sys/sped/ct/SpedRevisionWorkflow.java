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
package com.x2dev.procedures.sys.sped.ct;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.beans.WorkflowPhaseOutcome;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.workflow.WorkflowManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.workflow.SessionAccessManager;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.logging.Level;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SpedRevisionWorkflow.
 */
public class SpedRevisionWorkflow extends ProcedureJavaSource {

    // member variables
    private UserDataContainer m_userData;

    // constants used by procedure.
    private final String WORKFLOW_DEFINITION_OID_IEP_REVISION = "wfdX2IepRevis";
    private final String PARAM_STUDENT_OID = "studentOid";
    private final String PARAM_REVISION_DATE_OID = "revisionDate";

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {

        // Get the studentOid and revision date needed for the workflow.
        String studentOid = (String) getParameter(PARAM_STUDENT_OID);
        PlainDate revisionDate = (PlainDate) getParameter(PARAM_REVISION_DATE_OID);
        SisStudent student = null;

        // Ensure no other draft iep exists.
        X2Criteria draftIepCriteria = new X2Criteria();
        draftIepCriteria.addEqualTo(IepData.COL_STUDENT_OID, studentOid);
        draftIepCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(StatusCode.DRAFT.ordinal()));
        QueryByCriteria draftIepQuery = new QueryByCriteria(IepData.class, draftIepCriteria);
        Collection<IepData> draftIeps = getBroker().getCollectionByQuery(draftIepQuery);

        // Ensure no amendment draft iep exists.
        X2Criteria amendmentDraftCriteria = new X2Criteria();
        amendmentDraftCriteria.addEqualTo(IepData.COL_STUDENT_OID, studentOid);
        amendmentDraftCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(StatusCode.AMENDMENT_DRAFT.ordinal()));
        QueryByCriteria amendmentDraftQuery = new QueryByCriteria(IepData.class, amendmentDraftCriteria);
        Collection<IepData> amendmentDrafts = getBroker().getCollectionByQuery(amendmentDraftQuery);

        if (draftIeps.size() > 0 && amendmentDrafts.size() > 0) {
            logMessage(
                    "Draft IEP(s) and Amendment IEP(s) currently exist.  They must be either completed or discarded" +
                            " to create a Revision IEP.");
        }

        else if (draftIeps.size() > 0) {
            logMessage("Draft IEP currently exits.  It must be either completed or discarded to create another one.");
        }

        else if (amendmentDrafts.size() > 0) {
            logMessage(
                    "Amendment Draft IEP currently exits.  It must be either completed or discarded to create a Revision IEP.");
        }

        else {
            // Get student bean corresponding to studentOid.
            X2Criteria studentCriteria = new X2Criteria();
            studentCriteria.addEqualTo(X2BaseBean.COL_OID, studentOid);
            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
            student = (SisStudent) getBroker().getBeanByQuery(studentQuery);

            initiateWorkflow(student, revisionDate);
        }

    }

    /**
     * Initiate workflow.
     *
     * @param student Student
     * @param revisionDate PlainDate
     */
    private void initiateWorkflow(Student student, PlainDate revisionDate) {
        logMessage("Created a SPED Revision Workflow Instance for student : " + student.getNameView());

        WorkflowManager workflowManager = new WorkflowManager(getBroker());

        // Get SPED Revision WorkflowDefinition
        Criteria workflowDefinitionCriteria = new Criteria();
        workflowDefinitionCriteria.addEqualTo(X2BaseBean.COL_OID, WORKFLOW_DEFINITION_OID_IEP_REVISION);
        QueryByCriteria workflowDefinitionQuery =
                new QueryByCriteria(WorkflowDefinition.class, workflowDefinitionCriteria);
        WorkflowDefinition revisionWorkFlowDef =
                (WorkflowDefinition) getBroker().getBeanByQuery(workflowDefinitionQuery);
        WorkflowPhaseOutcome revisionWorkFlowFirstPhaseOutcome =
                revisionWorkFlowDef.getFirstWorkflowPhase().getStandardPhaseOutcome();

        try {
            workflowManager.initiateWorkflow(student.getOid(), revisionWorkFlowFirstPhaseOutcome, null, m_userData,
                    revisionDate, false, getLocale(), new SessionAccessManager(m_userData, getBroker()));

            if (!workflowManager.getValidationErrors().isEmpty()) {
                AppGlobals.getLog().log(Level.SEVERE,
                        "Procedure: SpedRevisionProcedure failed. Validations errors occuring during creation of a Workflow. ");
                logMessage(
                        "Error: SpedRevisionProcedure failed. Validations errors occuring during creation of a Workflow. ");
            }
        } catch (X2BaseException e) {
            AppGlobals.getLog().log(Level.SEVERE, e.getMessage(), e);
        }

        logMessage("For new workflow to appear, you must refresh page by pressing \"IEP\" tab");
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    public void saveState(UserDataContainer userData) {
        m_userData = userData;
    }

}
