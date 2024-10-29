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

package com.x2dev.procedures.sys.sped.ri;

import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.Workflow;
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.procedures.sys.sped.ri.SpedRIWorkflowCommonProcedure.ExtendedSpedNotificationManager;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehavior;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Calendar;
import java.util.Locale;
import java.util.Map;
import org.apache.struts.action.ActionErrors;

/**
 * Defines special behavior that occurs on the special education initial referral workflow.
 * <p>
 * By default, all behavior is defined in the business layer. Custom workflow behavior can be
 * defined directly in this procedure. When adding behavior beyond what is defined in the business
 * layer, provide a detailed description of the changes made.
 *
 * @author X2 Development Corporation
 */
public class SpedReferralProcedure extends WorkflowProcedure {



    /**
     *
     */
    private static final String ALIAS_PARENTAL_CONSENT = "iep-parental-consent";
    private static final String DDX_ID_SPED_RI_IEP = "SPED-RI-IEP";

    private static final long serialVersionUID = 1L;
    private SpedWorkflowBehavior m_behavior = null;
    private SpedRIWorkflowCommonProcedure m_riWorkflowHelper = null;

    /**
     * Constructs a new SpedReferralProcedure.
     *
     * @param definition WorkflowDefinition
     * @param district Organization
     * @param user User
     * @param broker X2Broker
     * @param locale Locale
     */
    public SpedReferralProcedure(WorkflowDefinition definition, Organization district,
            User user, X2Broker broker, Locale locale) {
        super(definition, district, user, broker, locale);
        m_riWorkflowHelper = new SpedRIWorkflowCommonProcedure(this, definition, district, user, broker, locale);
        m_behavior = m_riWorkflowHelper.new ExtendedRISpedWorkflowBehavior();
    }

    /**
     * Performs tasks associated with finding a student eligible using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeEligible(WorkflowProgress progress) {

        m_behavior.executeEligible(progress, getOrganization(), getBroker());
        IepData iep = (IepData) progress.getWorkflow().getOwner();

        Calendar calendar = Calendar.getInstance();
        calendar.setTime(progress.getDate());
        calendar.add(Calendar.YEAR, 3);
        iep.setLastEvaluationDate(progress.getDate());
        iep.setNextEvaluationDate(new PlainDate(calendar.getTimeInMillis()));
        getBroker().saveBeanForced(iep);

    }

    /**
     * Schedules an IEP meeting using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeHoldMeeting(WorkflowProgress progress) throws Exception {
        // S-30759
        // Last IEP Evaluation Date should be pulling from the "Hold Eligibility Meeting" phase of
        // the initial IEP Workflow, NOT the "Hold Referral Meeting" phase of the Initial IEP
        // Workflow.
        // Next IEP Evaluation Date should be three years after the "Hold Eligibility Meeting" date
        // for now Hold Referral Meeting used executeHoldMeeting we should revert just Last IEP
        // Evaluation Date and Next IEP Evaluation Date
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        PlainDate lastEvaluationDate = iep.getLastEvaluationDate();
        PlainDate nextEvaluationDate = iep.getNextEvaluationDate();
        // try execute standard behavior
        m_behavior.executeHoldMeeting(progress, getOrganization(), getBroker());
        // revert unnecessary changes
        iep.setLastEvaluationDate(lastEvaluationDate);
        iep.setNextEvaluationDate(nextEvaluationDate);
        getBroker().saveBeanForced(iep);
    }

    /**
     * Performs tasks associated with a successful IEP appeal using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeIepAppeal(WorkflowProgress progress) {
        m_behavior.executeIepAppeal(progress, getBroker());
    }

    /**
     * Performs tasks associated with a successful IEP approval using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeIepApproved(WorkflowProgress progress) throws X2BaseException {
        ActionErrors errors = new ActionErrors();
        IepData iep = (IepData) progress.getWorkflow().getOwner();

        ModelProperty modelProperty =
                new ModelProperty(WorkflowProgress.class, WorkflowProgress.COL_DATE, getBroker().getPersistenceKey());

        DataDictionary ddx = m_riWorkflowHelper.getDictionaryByExtendedDictionaryId(DDX_ID_SPED_RI_IEP);
        DataDictionaryField field = ddx.findDataDictionaryFieldByAlias(ALIAS_PARENTAL_CONSENT);

        // we need convert date into format for target field
        // first step get string represent value from bean
        String date = WebUtils.getPropertyAsString(progress, modelProperty, getLocale());
        // second state convert into appropriate format
        date = (String) GenericDetail.getTypedValue(field, date, getLocale(), errors);

        iep.setFieldValueByAlias(ALIAS_PARENTAL_CONSENT, date, ddx);
        getBroker().saveBeanForced(iep);
    }

    /**
     * Performs tasks associated with an IEP rejection using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeIepRejected(WorkflowProgress progress) {
        m_behavior.executeIepRejected(progress, getBroker());
    }

    /**
     * Implements the draft IEP using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeImplementIep(WorkflowProgress progress) throws Exception {
        addValidationErrors(m_behavior.executeImplementIep(progress, getOrganization(), getLocale(), getBroker()));
    }

    /**
     * Implements the draft IEP (with email notification) using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeImplementNotifyIep(WorkflowProgress progress) throws Exception {
        addValidationErrors(m_behavior.executeImplementIep(progress, getOrganization(), getLocale(), getBroker()));

        IepData implementedIep = (IepData) progress.getWorkflow().getOwner();
        ExtendedSpedNotificationManager notificationManager =
                m_riWorkflowHelper.new ExtendedSpedNotificationManager(implementedIep);
        notificationManager.sendEmailNotification();
    }

    /**
     * Changes a student's status to ineligible using default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeIneligible(WorkflowProgress progress) throws Exception {
        m_behavior.executeIneligible(progress, getOrganization(), getBroker());
    }

    /**
     * Change IEP meeting type on workflow converting.
     *
     * @param workflow Workflow
     * @param broker X2Broker
     */
    @Override
    public void executeOnConvert(Workflow workflow, X2Broker broker) {
        X2BaseBean owner = workflow.getOwner();
        if (owner instanceof IepData) {
            ((IepData) owner).setMeetingTypeCode(0);
            broker.saveBeanForced(owner);
        }
    }

    /**
     * Peforms tasks associated with sending the assessment consent form using the default sped
     * workflow behavior.
     *
     * @param progress WorkflowProgress
     */
    public void executeSendEvalConsent(WorkflowProgress progress) {
        m_behavior.executeReceiveConsent(progress, getOrganization(), getLocale(), getBroker());
    }

    /**
     * Rolls back changes made by the submit for approval method using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     */
    public void executeSubmitForApproval(WorkflowProgress progress) {
        m_behavior.executeSubmitForApproval(progress, getBroker());
    }

    /**
     * Initialize owner.
     *
     * @param selectionBean X2BaseBean
     * @param formInstances Map<String,FormInstance>
     * @param date PlainDate
     * @param workflowDefinition WorkflowDefinition
     * @return X2BaseBean
     * @see com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#initializeOwner(com.follett.fsc.
     *      core.k12.beans.X2BaseBean, java.util.Map, PlainDate,
     *      com.follett.fsc.core.k12.beans.WorkflowDefinition)
     */
    @Override
    public X2BaseBean initializeOwner(X2BaseBean selectionBean,
                                      Map<String, FormInstance> formInstances,
                                      PlainDate date,
                                      WorkflowDefinition workflowDefinition) {
        return m_behavior.executeReferral((SisStudent) selectionBean, workflowDefinition, formInstances, date,
                getBroker());
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
     * Rolls back changes made by the eligible method using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackEligible(WorkflowProgress progress) throws Exception {
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        m_behavior.rollbackEligible(progress, getOrganization(), getBroker());

        // Last IEP Evaluation Date should be pulling from the "Hold Eligibility Meeting" phase of
        // the initial IEP Workflow, NOT the "Hold Referral Meeting" phase of the Initial IEP
        // Workflow.
        // Next IEP Evaluation Date should be three years after the "Hold Eligibility Meeting" date
        // revert Last IEP Evaluation Date and Next IEP Evaluation Date
        iep.setNextEvaluationDate(iep.getLastEvaluationDate());
        iep.setLastEvaluationDate(null);
        getBroker().saveBeanForced(iep);
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
        // S-30759
        // Last IEP Evaluation Date should be pulling from the "Hold Eligibility Meeting" phase of
        // the initial IEP Workflow, NOT the "Hold Referral Meeting" phase of the Initial IEP
        // Workflow.
        // Next IEP Evaluation Date should be three years after the "Hold Eligibility Meeting" date
        // for now Hold Referral Meeting used rollbackHoldMeeting we should revert just Last IEP
        // Evaluation Date and Next IEP Evaluation Date
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        PlainDate lastEvaluationDate = iep.getLastEvaluationDate();
        PlainDate nextEvaluationDate = iep.getNextEvaluationDate();
        // try execute standard behavior
        m_behavior.rollbackHoldMeeting(progress, getOrganization(), getBroker());
        // revert unnecessary rollback
        iep.setLastEvaluationDate(lastEvaluationDate);
        iep.setNextEvaluationDate(nextEvaluationDate);
        getBroker().saveBeanForced(iep);
    }

    /**
     * Rolls back changes made when an IEP is appealed using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackIepAppeal(WorkflowProgress progress) {
        m_behavior.rollbackIepAppeal(progress, getBroker());
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
    public void rollbackIepRejected(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackIepRejected(progress, getBroker());
    }

    /**
     * Rolls back changes made by the implement IEP method using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackImplementIep(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackImplementIep(progress, getOrganization(), getLocale(), getBroker());
    }

    /**
     * Rolls back changes made by the implement IEP method using the default sped workflow behavior.
     * During rollback, no notification is sent to the user.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackImplementNotifyIep(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackImplementIep(progress, getOrganization(), getLocale(), getBroker());
    }

    /**
     * Rolls back changes made by the ineligible method using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackIneligible(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackIneligible(progress, getOrganization(), getBroker());
    }

    /**
     * Rolls back changes made by the send eval consent method using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackSendEvalConsent(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackReceiveConsent(progress, getOrganization(), getBroker());
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
