/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2014 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped.bc;

import static com.x2dev.sis.model.beans.SisPreferenceConstants.SPED_WORKFLOW_REFERRAL;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.Workflow;
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.beans.WorkflowPhase;
import com.follett.fsc.core.k12.beans.WorkflowPhaseOutcome;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.workflow.WorkflowManager;
import com.follett.fsc.core.k12.tools.ToolBroker;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.workflow.SessionAccessManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure for BC that enrolls multiple students in special education.
 *
 * @author X2 Development Corporation
 */
public class MassEnrollInSpecialEd extends ProcedureJavaSource {
    /*
     * Input parameters
     */
    private static final String DATE_PARAM = "date";
    private static final String STUDENT_OIDS_PARAM = "studentOids";

    private UserDataContainer m_userData;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        // Enable tool broker to audit
        ((ToolBroker) getBroker()).turnAuditOn(getUser());

        String studentOidsParam = (String) getParameter(STUDENT_OIDS_PARAM);
        if (StringUtils.isEmpty(studentOidsParam)) {
            logMessage("Invalid student selection!");
        } else {
            WorkflowDefinition definition = getInitialReferralWorkflowDefinition();
            if (definition == null) {
                logMessage(
                        "Ministry Identification workflow definition not found. Please contact your system administrator.");
            } else {
                WorkflowManager workflowManager = new WorkflowManager(getBroker());
                SessionAccessManager accessManager = new SessionAccessManager(m_userData, getBroker());
                WorkflowPhase firstPhase = definition.getFirstWorkflowPhase();
                WorkflowPhaseOutcome standardOutcome =
                        ((firstPhase != null) ? firstPhase.getStandardPhaseOutcome() : null);
                PlainDate date = (PlainDate) getParameter(DATE_PARAM);

                boolean exception = false;
                getBroker().beginTransaction();
                try {
                    StringBuilder initiatedWorkflowOwners = new StringBuilder();

                    List<String> studentOids = StringUtils.convertDelimitedStringToList(studentOidsParam, ",", true);
                    for (String studentOid : studentOids) {
                        Workflow workflow = workflowManager.initiateWorkflow(studentOid,
                                standardOutcome,
                                new HashMap<String, FormInstance>(),
                                m_userData,
                                date,
                                false,
                                getLocale(),
                                accessManager);
                        if (workflow != null) {
                            if (initiatedWorkflowOwners.toString().length() > 0) {
                                initiatedWorkflowOwners.append("\n");
                            }

                            initiatedWorkflowOwners.append(workflow.getOwnerView());
                        }
                    }

                    logMessage(
                            "Enrolled following students in special ed:" + "\n" + initiatedWorkflowOwners.toString());
                } catch (X2BaseException xbe) {
                    exception = true;
                    throw xbe;
                } finally {
                    if (workflowManager.getValidationErrors().isEmpty() && !exception) {
                        getBroker().commitTransaction();
                    } else {
                        getBroker().rollbackTransaction();

                        List<ValidationError> validationErrors = workflowManager.getValidationErrors();
                        if (!validationErrors.isEmpty()) {
                            for (ValidationError validationError : validationErrors) {
                                logMessage(validationError.toString());
                            }
                        }
                    }
                }
            }
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
        m_userData = userData;
    }

    /**
     * Returns the workflow definition bean for initial referral workflow.
     *
     * @return WorkflowDefinition
     */
    private WorkflowDefinition getInitialReferralWorkflowDefinition() {
        String wfdId = m_userData.getPreferenceSet().getPreferenceValue(SPED_WORKFLOW_REFERRAL);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(WorkflowDefinition.COL_ID, wfdId);

        QueryByCriteria query = new QueryByCriteria(WorkflowDefinition.class, criteria);

        return (WorkflowDefinition) getBroker().getBeanByQuery(query);
    }
}
