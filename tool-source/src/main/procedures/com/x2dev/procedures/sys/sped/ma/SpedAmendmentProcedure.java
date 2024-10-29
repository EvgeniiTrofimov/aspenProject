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
package com.x2dev.procedures.sys.sped.ma;

import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.Workflow;
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.x2dev.procedures.sys.sped.ma.SpedMAWorkflowCommonProcedure.ExtendedSpedNotificationManager;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.LinkedList;
import java.util.Locale;
import java.util.Map;

/**
 * Defines special behavior that occurs on the special education IEP amendment workflow.
 * <p>
 * By default, all behavior is defined in the business layer. Custom workflow behavior can be
 * defined directly in this procedure. When adding behavior beyond what is defined in the business
 * layer, provide a detailed description of the changes made.
 *
 * @author X2 Development Corporation
 */
public class SpedAmendmentProcedure extends MaSpedWorkflowProcedure {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Constructs a new SpedAmendmentProcedure.
     *
     * @param definition WorkflowDefinition
     * @param district Organization
     * @param user User
     * @param broker X2Broker
     * @param locale Locale
     */
    public SpedAmendmentProcedure(WorkflowDefinition definition, Organization district,
            User user, X2Broker broker, Locale locale) {
        super(definition, district, user, broker, locale);
    }

    /**
     * Holds an amendment meeting using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeHoldAmendmentMeeting(WorkflowProgress progress) throws Exception {
        m_behavior.executeHoldAmendmentMeeting(progress, getBroker());
    }

    /**
     * Performs tasks associated with a successful IEP appeal using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeIepAmendmentAppeal(WorkflowProgress progress) {
        m_behavior.executeIepAmendmentAppeal(progress, getBroker());
    }

    /**
     * Performs tasks associated with an IEP rejection using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeIepAmendmentRejected(WorkflowProgress progress) {
        m_behavior.executeIepAmendmentRejected(progress, getBroker());
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
     * Implements the draft amendment IEP using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeImplementAmendedIep(WorkflowProgress progress) throws Exception {
        addValidationErrors(m_behavior.executeImplementAmendedIep(progress, getOrganization(), getBroker()));
    }

    /**
     * Implements the draft amendment IEP (with email notification) using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeImplementNotifyAmendedIep(WorkflowProgress progress) throws Exception {
        addValidationErrors(m_behavior.executeImplementAmendedIep(progress, getOrganization(), getBroker()));

        IepData implementedIep = (IepData) progress.getWorkflow().getOwner();
        ExtendedSpedNotificationManager notificationManager =
                m_riWorkflowHelper.new ExtendedSpedNotificationManager(implementedIep);
        notificationManager.sendEmailNotification();
    }

    /**
     * Execute iep rejected reissue.
     *
     * @param progress WorkflowProgress
     */
    public void executeIepRejectedReissue(WorkflowProgress progress) {
        // Set the reissue type if it has not been set yet.
        ExtendedDataDictionary iepExtendedDictionary = SpedUtils.getIepDictionary(getOrganization(), getBroker());
        DataDictionary iepDictionary =
                DataDictionary.getDistrictDictionary(iepExtendedDictionary, getBroker().getPersistenceKey());
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        if (!"Amendment".equals(iep.getFieldValueByAlias("reissue-type", iepDictionary))) {
            iep.setFieldValueByAlias("reissue-type", "Amendment", iepDictionary);
            getBroker().saveBean(iep);
        }

        addValidationErrors(m_behavior.executeIepRejectedReissue(progress));
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
            IepData iep = (IepData) owner;
            iep.setMeetingTypeCode(IepMeeting.TypeCode.AMENDMENT.ordinal());
            int statusCode = iep.getStatusCode();
            if (statusCode == IepData.StatusCode.DRAFT.ordinal()) {
                iep.setStatusCode(IepData.StatusCode.AMENDMENT_DRAFT.ordinal());
            }
            broker.saveBeanForced(owner);
        }
    }

    /**
     * Rolls back changes made by the submit for approval method using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeSubmitForApproval(WorkflowProgress progress) {
        m_behavior.executeSubmitForApproval(progress, getOrganization(), getLocale(), getBroker());
    }

    /**
     * Initialize owner.
     *
     * @param selectionBean X2BaseBean
     * @param formInstances Map<String,FormInstance>
     * @param date PlainDate
     * @param workflowDefinition WorkflowDefinition
     * @return X2BaseBean
     * @see com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#initializeOwner(com.follett.fsc.core.k12.beans.X2BaseBean,
     *      java.util.Map, PlainDate, com.follett.fsc.core.k12.beans.WorkflowDefinition)
     */
    @Override
    public X2BaseBean initializeOwner(X2BaseBean selectionBean,
                                      Map<String, FormInstance> formInstances,
                                      PlainDate date,
                                      WorkflowDefinition workflowDefinition) {
        LinkedList<ValidationError> errors = new LinkedList<ValidationError>();

        IepData owner =
                m_behavior.executeCreateAmendmentDraft((SisStudent) selectionBean, new PlainDate(getTimeZone()),
                        errors, getBroker());

        addValidationErrors(errors);

        return owner;
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
     * Rolls back changes made by the hold amendment meeting method using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackHoldAmendmentMeeting(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackHoldAmendmentMeeting(progress, getBroker());
    }

    /**
     * Rolls back changes made when an IEP is appealed using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackIepAmendmentAppeal(WorkflowProgress progress) {
        m_behavior.rollbackIepAmendmentAppeal(progress, getBroker());
    }

    /**
     * Rolls back changes made when an IEP is rejected using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackIepAmendmentRejected(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackIepAmendmentRejected(progress, getBroker());
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
     * Rolls back implementation of a draft amendment IEP using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackImplementAmendedIep(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackImplementAmendedIep(progress, getOrganization(), getBroker());
    }

    /**
     * Rolls back implementation of a draft amendment IEP using the default sped workflow behavior.
     * During rollback, no notification is sent to the user.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackImplementNotifyAmendedIep(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackImplementAmendedIep(progress, getOrganization(), getBroker());
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
     * Rolls back changes made by the submit for approval method using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     */
    public void rollbackSubmitForApproval(WorkflowProgress progress) {
        addValidationErrors(m_behavior.rollbackSubmitForApproval(progress, getBroker()));
    }
}
