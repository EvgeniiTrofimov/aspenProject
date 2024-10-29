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
package com.x2dev.procedures.sys.sped.ga;

import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.Workflow;
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.x2dev.sis.model.beans.IepAmendment;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehavior;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehaviorFactory;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
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
import java.util.Collection;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Defines special behavior that occurs on the special education IEP amendment workflow.
 * <p>
 * By default, all behavior is defined in the business layer. Custom workflow behavior can be
 * defined directly in this procedure. When adding behavior beyond what is defined in the business
 * layer, provide a detailed description of the changes made.
 *
 * @author X2 Development Corporation
 */
public class SpedAmendmentProcedure extends WorkflowProcedure {
    private static final long serialVersionUID = 1L;
    private static final String AMENDMENT_WORKFLOW_OUTCOME_WITH_MEETING_OID = "wpoX2AmeDetrm1";
    private static final String FORM_DEFINITION_TRANSITION = "fmdGaTrans";

    private SpedWorkflowBehavior m_behavior = null;

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
        m_behavior = SpedWorkflowBehaviorFactory.getWorkflowBehavior(getOrganization());
    }

    /**
     * Set the start date and IEP Amendment Date to be the workflow progress date for the finalize
     * IEP step.
     * Also set the iep meeting date if it's Amendment with the meeting.
     *
     * @param progress WorkflowProgress
     */
    public void executeFinalizeIep(WorkflowProgress progress) {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        iep.getIepAmendment().setDate(progress.getDate());

        // If it's "Amend with meeting", then sets the meeting date based on the "Completed On" date
        // (progress date).
        if (isWithMeeting(progress.getWorkflow().getWorkflowProgress())) {
            iep.setMeetingDate(progress.getDate());
        }

        executeSubmitForApproval(progress);
        broker.saveBeanForced(iep);
    }

    /**
     * Holds an amendment meeting using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeHoldAmendmentMeeting(WorkflowProgress progress) throws Exception {
        // Not currently used
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
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeIepApproved(WorkflowProgress progress) throws Exception {
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
        addValidationErrors(m_behavior.executeImplementNotifyAmendedIep(progress, getOrganization(), getBroker()));
    }

    /**
     * Finds the earliest initial referral date, if there is one, and sets this on current iep.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeInitialReferral(WorkflowProgress progress) throws Exception {

        X2Broker broker = getBroker();
        IepData currentIep = (IepData) progress.getWorkflow().getOwner();
        SisStudent student = currentIep.getStudent();

        Optional<PlainDate> initialReferralDate =
                student.getIepData().stream()
                        .filter(iep -> iep.getReferralDate() != null)
                        .map(iep -> iep.getReferralDate())
                        .min(new Comparator<PlainDate>() {
                            @Override
                            public int compare(PlainDate o1, PlainDate o2) {
                                // TODO Auto-generated method stub
                                return o1.compareTo(o2);
                            }
                        });
        currentIep.setReferralDate(initialReferralDate.orElse(null));
        broker.saveBeanForced(currentIep);
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
            String iepAmendmentOid = ((IepData) owner).getIepAmendmentOid();

            if (StringUtils.isEmpty(iepAmendmentOid)) {
                IepAmendment iepAmendment = new IepAmendment(broker.getPersistenceKey());
                iepAmendment.setDate(new PlainDate(System.currentTimeMillis()));
                iepAmendment.setIepDataOid(owner.getOid());
                iepAmendment.setStudentOid(((IepData) owner).getStudentOid());
                broker.saveBeanForced(iepAmendment);

                // Create IEP Amendment form instance
                FormDefinition amendmentFormDefinition =
                        getFormDefinition(m_behavior.getAmendmentFormDefinitionId(), broker);
                if (amendmentFormDefinition != null) {
                    FormInstance amendmentForm = X2BaseBean.newInstance(FormInstance.class, broker.getPersistenceKey());
                    amendmentForm.setFormDefinitionOid(amendmentFormDefinition.getOid());
                    amendmentForm.setCreatedTime(System.currentTimeMillis());
                    amendmentForm.setOwnerObjectOid(owner.getOid());
                    amendmentForm.setStorageObjectOid(iepAmendment.getOid());
                    broker.saveBeanForced(amendmentForm);
                }

                ((IepData) owner).setIepAmendmentOid(iepAmendment.getOid());
            }

            ((IepData) owner).setAmendedIndicator(true);
            ((IepData) owner).setStatusCodeEnum(StatusCode.AMENDMENT_DRAFT);
            ((IepData) owner).setMeetingTypeCodeEnum(IepMeeting.TypeCode.AMENDMENT);
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
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#initializeOwner(com.follett.fsc.
     *      core.k12.beans.X2BaseBean, java.util.Map, PlainDate,
     *      com.follett.fsc.core.k12.beans.WorkflowDefinition)
     *
     *      Copies Transition Service Plan form, if there is one.
     */
    @Override
    public X2BaseBean initializeOwner(X2BaseBean selectionBean,
                                      Map<String, FormInstance> formInstances,
                                      PlainDate date,
                                      WorkflowDefinition workflowDefinition) {
        LinkedList<ValidationError> errors = new LinkedList<ValidationError>();

        SisStudent student = (SisStudent) selectionBean;
        X2Broker broker = getBroker();
        IepData previouslyActiveIep = student.getActiveIep(broker);

        IepData owner = m_behavior.executeCreateAmendmentDraft((SisStudent) selectionBean, new PlainDate(getTimeZone()),
                errors, getBroker());

        if (owner != null) // Note previously active iep was checked for null in
                           // executedCreateAmendmentDraft.
        {
            // Get the "Transition" form instance for the previously active iep
            Criteria criteria = new Criteria();
            criteria.addEqualTo(FormInstance.COL_FORM_DEFINITION_OID, FORM_DEFINITION_TRANSITION);
            criteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, previouslyActiveIep.getOid());
            QueryByCriteria query = new QueryByCriteria(FormInstance.class, criteria);
            FormInstance formInstance = (FormInstance) broker.getBeanByQuery(query);

            // The amendment date was set to "today" in executeCreateAmendmentDraft. Set it to
            // "null" for now. It will be set later.
            owner.getIepAmendment().setDate(null);
            owner.setMeetingTypeCodeEnum(IepMeeting.TypeCode.AMENDMENT);
            owner.setEndDate(previouslyActiveIep.getEndDate());

            if (formInstance != null) {
                // copy previous generic form data, creating new gfd without oid.
                // save to new generic form data, creating new oid.
                GenericFormData originalGfd = (GenericFormData) broker.getBeanByOid(GenericFormData.class,
                        formInstance.getStorageObjectOid());
                GenericFormData newGfd = (GenericFormData) originalGfd.copyBean();
                getBroker().saveBeanForced(newGfd);

                // create + save new form instance.
                FormInstance newFormInstance =
                        X2BaseBean.newInstance(FormInstance.class, getBroker().getPersistenceKey());
                newFormInstance.setFormDefinitionOid(FORM_DEFINITION_TRANSITION);
                newFormInstance.setOwnerObjectOid(owner.getOid());
                newFormInstance.setStorageObjectOid(newGfd.getOid());
                newFormInstance.setCreatedTime(formInstance.getCreatedTime());
                getBroker().saveBeanForced(newFormInstance);
            }
        }

        addValidationErrors(errors);

        return owner;
    }

    /**
     * Owner exists before initiation.
     *
     * @return true, if successful
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#ownerExistsBeforeInitiation()
     */
    @Override
    public boolean ownerExistsBeforeInitiation() {
        return false;
    }

    /**
     * Rolls back change made by the finalize iep method using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     */
    public void rollbackFinalizeIep(WorkflowProgress progress) {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        if (null != iep.getIepMeeting()) {
            iep.setMeetingDate(progress.getDate());
        }
        rollbackSubmitForApproval(progress);
        broker.saveBeanForced(iep);
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
        // Not currently used
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
        this.rollbackStartIep(progress);
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
     * Rolls back the initial referral date to null.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackInitialReferral(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        iep.setReferralDate(null);
        broker.saveBeanForced(iep);
    }

    /**
     * Rolls back "Start Date" field of IEP to null.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackStartIep(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        iep.setStartDate(null);
        broker.saveBeanForced(iep);
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

    /**
     * Returns the form definition with the passed ID.
     *
     * @param id String
     * @param broker X2Broker
     * @return FormDefinition
     */
    private FormDefinition getFormDefinition(String id, X2Broker broker) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(FormDefinition.COL_ID, id);

        QueryByCriteria query = new QueryByCriteria(FormDefinition.class, criteria);

        return (FormDefinition) broker.getBeanByQuery(query);
    }

    /**
     * Check if the given workflow progress is with meeting.
     *
     * @param workflowProgress Collection<WorkflowProgress>
     * @return boolean
     */
    private boolean isWithMeeting(Collection<WorkflowProgress> workflowProgress) {
        boolean isWithMeeting = false;

        for (WorkflowProgress progress : workflowProgress) {
            if (AMENDMENT_WORKFLOW_OUTCOME_WITH_MEETING_OID.equals(progress.getWorkflowPhaseOutcomeOid())) {
                isWithMeeting = true;
                break;
            }
        }

        return isWithMeeting;
    }
}
