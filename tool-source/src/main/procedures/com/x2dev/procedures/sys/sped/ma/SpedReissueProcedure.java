/* #PROCEDURE-ID [LPS-IEP-001] */
/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped.ma;

import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.business.X2Broker;
import com.x2dev.sis.model.beans.IepData;
import java.util.Locale;

/**
 * Custom sped workflow procedure for "Reissue Draft" workflow for Lexington, MA.
 *
 * @author X2 Development Corporation
 */
public class SpedReissueProcedure extends MaSpedWorkflowProcedure {
    /*
     * Partial status constant
     */
    private static final String STATUS_PARTIAL = "Partial";

    /*
     * Rejection Draft Workflow ID
     */
    private static final String REJECTION_DRAFT_WORKFLOW_ID = "LEX-SPED-REJ";

    /*
     * MA IEP Form definition OID
     */
    private static final String MA_IEP_FORM_OID = "fmdMaIep";

    /*
     * User-defined field on "IEP Data" that tracks the partial status
     */
    private static final String FIELD_PARTIAL_STATUS = IepData.COL_FIELD_A048;

    /*
     * Parent Approval aliases
     */
    private static final String PARENT_APPROVAL_ALIAS = "parent-approval";
    private static final String PARENT_REJECTED_PORTIONS_ALIAS = "parent-rejected-portions";

    /**
     * Constructs a new SpedReferralProcedure.
     *
     * @param definition WorkflowDefinition
     * @param district Organization
     * @param user User
     * @param broker X2Broker
     * @param locale Locale
     */
    public SpedReissueProcedure(WorkflowDefinition definition, Organization district,
            User user, X2Broker broker, Locale locale) {
        super(definition, district, user, broker, locale);
    }

    /**
     * Schedules an IEP meeting using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeHoldMeeting(WorkflowProgress progress) throws Exception {
        m_behavior.executeHoldMeeting(progress, getOrganization(), getBroker());
    }

    /**
     * Performs tasks associated with a successful IEP approval using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeIepApproved(WorkflowProgress progress) {
        m_behavior.executeIepApproved(progress, getOrganization(), getBroker());
    }

    /**
     * Performs tasks associated with an IEP rejection using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeIepReissueRejected(WorkflowProgress progress) {
        m_behavior.executeIepReissueRejected(progress);
    }

    /**
     * Execute iep rejected reissue.
     *
     * @param progress WorkflowProgress
     */
    public void executeIepRejectedReissue(WorkflowProgress progress) {
        addValidationErrors(m_behavior.executeIepRejectedReissue(progress));
    }

    /**
     * Implements the draft IEP (with email notification) using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeImplementNotifyReissueIep(WorkflowProgress progress) throws Exception {
        addValidationErrors(
                m_behavior.executeImplementNotifyReissueIep(progress, getOrganization(), getLocale(), getBroker()));
    }

    /**
     * Implements the draft IEP using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeImplementReissueIep(WorkflowProgress progress) throws Exception {
        addValidationErrors(
                m_behavior.executeImplementReissueIep(progress, getOrganization(), getLocale(), getBroker()));
    }

    /**
     * Rolls back changes made by the submit for approval method using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     */
    public void executeSubmitForApproval(WorkflowProgress progress) {
        m_behavior.executeSubmitForApproval(progress, getOrganization(), getLocale(), getBroker());
    }

    /**
     * Owner exists before initiation.
     *
     * @return true, if successful
     * @see com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#ownerExistsBeforeInitiation()
     */
    @Override
    public boolean ownerExistsBeforeInitiation() {
        return false;
    }

    /**
     * Rolls back changes made by the schedule meeting method using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackHoldMeeting(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackHoldMeeting(progress, getOrganization(), getBroker());
    }

    /**
     * Rolls back changes made when an IEP is approved using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackIepApproved(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackIepApproved(progress, getOrganization(), getBroker());
    }

    /**
     * Rolls back changes made when an IEP is rejected using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackIepReissueRejected(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackIepReissueRejected(progress);
    }

    /**
     * Rolls back changes made when an IEP is rejected using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackIepRejectedReissue(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackIepRejectedReissue(progress);
    }

    /**
     * Rolls back changes made by the implement IEP method using the default sped workflow behavior.
     * During rollback, no notification is sent to the user.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackImplementNotifyReissueIep(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackImplementReissueIep(progress, getOrganization(), getLocale(), getBroker());
    }

    /**
     * Rolls back changes made by the implement IEP method using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackImplementReissueIep(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackImplementReissueIep(progress, getOrganization(), getLocale(), getBroker());
    }

    /**
     * Rolls back changes made by the submit for approval method using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     */
    public void rollbackSubmitForApproval(WorkflowProgress progress) {
        addValidationErrors(m_behavior.rollbackSubmitForApproval(progress, getBroker()));
    }
}
