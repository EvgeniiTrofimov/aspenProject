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

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.k12.beans.BeanManager;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.Workflow;
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehavior;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehaviorFactory;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
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
import java.util.Calendar;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Locale;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;

/**
 * Defines special behavior that occurs on the special education IEP renewal workflow.
 * <p>
 * By default, all behavior is defined in the business layer. Custom workflow behavior can be
 * defined directly in this procedure. When adding behavior beyond what is defined in the business
 * layer, provide a detailed description of the changes made.
 *
 * @author X2 Development Corporation
 */
public class SpedRenewalProcedure extends WorkflowProcedure {
    /**
     * Constants
     */
    private static final long serialVersionUID = 1L;
    private static final String OID_MEETING_FORM = "MTG";
    private static final String ALIAS_CONSECUTIVE_REEVAL_REVIEW = "img-consecutive-reeval-review";
    private static final String ALIAS_IEP_MOST_RECENT_ELIGIBILITY = "iep-most-recent-elig";
    private static final String ALIAS_SPED_EXIT_REASON = "DOE SPED EXIT REASON";
    private static final String PHASE_HOLD_REEVAL_MEETING = "wphX2ReaHoldR";
    private static final String PHASE_SELECT_EVAL_DECISION = "wphX2ReaNeeds";
    private static final String PHASE_SEND_EVAL_FORM = "wphX2RenSend";
    private static final String OUTCOME_COMPLETE_REEVAL_DISC = "wpoX2ReaHoldR";
    private static final String OUTCOME_CONDUCT_ANNUAL_REVIEW = "wpoX2ReaHoldR1";
    private static final String OUTCOME_EVAL_NEEDED_EDU_PLAN = "wpoX2ReaNeeds2";
    private static final String OUTCOME_EVAL_NEEDED_NEW_ADDIT_ELIG = "wpoX2ReaNeeds3";
    private static final String OUTCOME_EVAL_NOT_APPLICABLE = "wpoX2RenSend1";
    private static final String SPED_EXIT_REASON_09 = "09";

    /**
     * Member variables
     */
    private SpedWorkflowBehavior m_behavior = null;

    /**
     * Constructs a new SpedRenewalProcedure.
     *
     * @param definition WorkflowDefinition
     * @param district Organization
     * @param user User
     * @param broker X2Broker
     * @param locale Locale
     */
    public SpedRenewalProcedure(WorkflowDefinition definition, Organization district,
            User user, X2Broker broker, Locale locale) {
        super(definition, district, user, broker, locale);
        m_behavior = SpedWorkflowBehaviorFactory.getWorkflowBehavior(getOrganization());
    }

    /**
     * Sets ALIAS_CONSECUTIVE_REEVAL_REVIEW logical field to true and then performs
     * method executeHoldMeeting.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeConsecutiveReevalReview(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();

        FormInstance formInstance = progress.getFormInstanceById(OID_MEETING_FORM, broker);
        if (formInstance != null) {
            IepMeeting meeting = (IepMeeting) formInstance.getStorageObject(broker);
            if (meeting != null) {
                ExtendedDictionaryAttributes extendedDataDictionary = meeting.getIepData().getExtendedDataDictionary();
                DataDictionary dictionary =
                        DataDictionary.getDistrictDictionary(extendedDataDictionary, broker.getPersistenceKey());
                meeting.setFieldValueByAlias(ALIAS_CONSECUTIVE_REEVAL_REVIEW, BooleanAsStringConverter.TRUE,
                        dictionary);
                broker.saveBeanForced(meeting);
            }
        }

        executeHoldMeeting(progress);
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
    }

    /**
     * Changes a student's status to exited using default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeExit(WorkflowProgress progress) throws Exception {
        m_behavior.executeExit(progress, getOrganization(), getBroker());
    }

    /**
     * Sets start date of IEP. Calculates + sets end date of IEP.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeFinalizeIep(WorkflowProgress progress) throws Exception {
        // get + set start date
        PlainDate startDate = progress.getDate();
        updateFieldsForFinalizeIep(startDate, progress);

        // set the iep status to "Pending Approval"
        this.executeSubmitForApproval(progress);
    }

    /**
     * Calls executeLastReview().
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeFinalizeReviewIep(WorkflowProgress progress) throws Exception {
        this.executeLastReview(progress);

        // get + set start date
        PlainDate startDate = progress.getDate();

        boolean isAnnualReviewOnReEval = false;
        Collection<WorkflowProgress> workFlowProgresses = progress.getWorkflow().getWorkflowProgress();
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        Organization organization = getOrganization();
        WorkflowDefinition workflowDef = getWorkflowDefinition();
        X2Broker broker = getBroker();


        // Based on the outcome selection on specific phases in the workflows, setting boolean
        // values to identify which fields should be updated.
        // Note: We can use the break statement for both of the current conditions because they
        // occur within separate workflows and would not ever be true simultaneously
        // If adding additional phase/outcome conditions, this will need to be modified to allow the
        // for each loop to complete the iteration.
        for (WorkflowProgress workflowProgress : workFlowProgresses) {
            // Getting phase oid - Hold re-evaluation meeting
            if (isOidMatch(PHASE_HOLD_REEVAL_MEETING, workflowProgress.getWorkflowPhaseOid())) {
                // Getting the WF phase outcome oid- Conduct Annual review.
                if (isOidMatch(OUTCOME_CONDUCT_ANNUAL_REVIEW, workflowProgress.getWorkflowPhaseOutcomeOid())) {
                    isAnnualReviewOnReEval = true;
                    break;
                }
            }

        }

        // [07] IEP Annual Review date is set for all IEP review workflows and when the Reeval
        // Meeting is an Annual Review on IEP Reeval workflows
        if (isAnnualReviewOnReEval || isReviewWorkflow(workflowDef, organization)) {
            iep.setLastReviewDate(startDate);
        }

        iep.setStartDate(startDate);

        // set meeting date to start date
        iep.setMeetingDate(startDate);


        // calculate + set end date
        Calendar endDate = Calendar.getInstance();
        endDate.setTime(startDate);
        endDate.add(Calendar.YEAR, 1);
        endDate.add(Calendar.DATE, -1);
        PlainDate endDateValue = new PlainDate(endDate.getTime());

        iep.setEndDate(endDateValue);

        // Set Next IEP Review Date
        iep.setNextReviewDate(endDateValue);

        broker.saveBeanForced(iep);


        // set the iep status to "Pending Approval"
        this.executeSubmitForApproval(progress);
    }

    /**
     * Update fields for finalize iep.
     *
     * @param startDate PlainDate
     * @param progress WorkflowProgress
     */
    private void updateFieldsForFinalizeIep(PlainDate startDate, WorkflowProgress progress) {

        boolean isAnnualReviewOnReEval = false;
        boolean isCompleteReEvalDiscus = false;
        boolean isEvalPhaseNotApplicable = false;
        Collection<WorkflowProgress> workFlowProgresses = progress.getWorkflow().getWorkflowProgress();
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        Organization organization = getOrganization();
        WorkflowDefinition workflowDef = getWorkflowDefinition();
        X2Broker broker = getBroker();


        // Calculate next eligibility determination date to be one day less than three year from the
        // initial meeting.
        Calendar nextEligibilityDate = Calendar.getInstance();
        nextEligibilityDate.setTime(startDate);
        nextEligibilityDate.add(Calendar.YEAR, 3);
        nextEligibilityDate.add(Calendar.DATE, -1);
        PlainDate nextEligiblityValue = new PlainDate(nextEligibilityDate.getTime());


        // Based on the outcome selection on specific phases in the workflows, setting boolean
        // values to identify which fields should be updated.
        // Note: We can use the break statement for both of the current conditions because they
        // occur within separate workflows and would not ever be true simultaneously
        // If adding additional phase/outcome conditions, this will need to be modified to allow the
        // for each loop to complete the iteration.
        for (WorkflowProgress workflowProgress : workFlowProgresses) {
            // Getting phase oid - Hold re-evaluation meeting
            if (isOidMatch(PHASE_HOLD_REEVAL_MEETING, workflowProgress.getWorkflowPhaseOid())) {
                // Getting the WF phase outcome oid- Conduct Annual review.
                if (isOidMatch(OUTCOME_CONDUCT_ANNUAL_REVIEW, workflowProgress.getWorkflowPhaseOutcomeOid())) {
                    isAnnualReviewOnReEval = true;
                    break;
                }
                // Getting the WF phase outcome oid- Complete re-evaluation discussion.
                else if (isOidMatch(OUTCOME_COMPLETE_REEVAL_DISC, workflowProgress.getWorkflowPhaseOutcomeOid())) {
                    isCompleteReEvalDiscus = true;
                    break;
                }
            }

            // Getting phase oid - Send Evaluation Form
            if (isOidMatch(PHASE_SEND_EVAL_FORM, workflowProgress.getWorkflowPhaseOid())) {
                // Getting the phase outcome OID - Not Applicable
                if (isOidMatch(OUTCOME_EVAL_NOT_APPLICABLE, workflowProgress.getWorkflowPhaseOutcomeOid())) {
                    isEvalPhaseNotApplicable = true;
                    break;
                }

            }
        }
        // Altered to set the Next Evaluation Date for all ReEval workflows
        if (isEvalWorkflow(workflowDef, organization)) {
            iep.setNextEvaluationDate(nextEligiblityValue);
        }
        // Set the Next Evaluation Date for Review workflows where the Send Evaluation Form Phase
        // outcome is anything other than Not Applicable
        if (!isEvalPhaseNotApplicable && isReviewWorkflow(workflowDef, organization)) {
            iep.setNextEvaluationDate(nextEligiblityValue);
        }
        // Set the Most Recent Eligibility and Eligibility Redetermination dates for ReEval
        // workflows and any Review workflow where the Send Evaluation Form phase outcome
        // is anything other than Not Applicable
        if (!isEvalPhaseNotApplicable) {
            // set 'Most Recent Eligibility Date' date to start date
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(
                    SpedUtils.getIepDictionary(getOrganization(), broker), broker.getPersistenceKey());
            DateAsStringConverter converter = (DateAsStringConverter) ConverterFactory
                    .getConverterForClass(Converter.DATE_CONVERTER, getLocale(), true);
            iep.setFieldValueByAlias(ALIAS_IEP_MOST_RECENT_ELIGIBILITY, converter.getSystemString(startDate),
                    dictionary);

            // set '[08] Eligibility redetermination' date to start date
            iep.setLastEligibilityDate(startDate);
        }

        // [07] IEP Annual Review date is set for all IEP review workflows and when the Reeval
        // Meeting is an Annual Review on IEP Reeval workflows
        if (isAnnualReviewOnReEval || isReviewWorkflow(workflowDef, organization)) {
            iep.setLastReviewDate(startDate);
        }
        // IF phase "Hold re-evaluation meeting" = "Complete re-evaluation discussion", then fields
        // "Start Dates",
        // "End Date" and "Next IEP Review Date" shouldn't be updated at the phase "Finalize IEP"
        if (!isCompleteReEvalDiscus) {
            iep.setStartDate(startDate);

            // calculate + set end date
            Calendar endDate = Calendar.getInstance();
            endDate.setTime(startDate);
            endDate.add(Calendar.YEAR, 1);
            endDate.add(Calendar.DATE, -1);
            PlainDate endDateValue = new PlainDate(endDate.getTime());

            iep.setEndDate(endDateValue);

            // Set Next IEP Review Date
            iep.setNextReviewDate(endDateValue);
        }

        // set meeting date to start date
        iep.setMeetingDate(startDate);

        broker.saveBeanForced(iep);
    }

    /**
     * Holds an IEP meeting using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeHoldMeeting(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        PlainDate meetingDate = null;

        FormInstance formInstance = progress.getFormInstanceById(OID_MEETING_FORM, broker);
        if (formInstance != null) {
            IepMeeting meeting = (IepMeeting) formInstance.getStorageObject(broker);
            if (meeting != null) {
                meetingDate = meeting.getDate();
            }
        }

        if (meetingDate != null && !isEvalWorkflow(progress.getWorkflow().getWorkflowDefinition(), getOrganization())) {
            /*
             * TODO check these three dates: IEP Meeting date, IEP Last Review Date (IEP Annual
             * Review date),
             * and IEP Next Review Date (Next IEP review date); whether we want to modify it in IEP
             * Review workflow.
             *
             * NOTE: right now, it's only used in the IEP Review workflow (Hold IEP meeting phase),
             * but not in IEP Re-evaluation workflow.
             */

            // iep.setMeetingDate(meetingDate);
            //
            // Calendar calendar = Calendar.getInstance();
            // calendar.setTime(meetingDate);
            // calendar.add(Calendar.YEAR, 1);
            // calendar.add(Calendar.DATE, -1);
            //
            // iep.setLastReviewDate(meetingDate);
            // iep.setNextReviewDate(new PlainDate(calendar.getTimeInMillis()));

            broker.saveBeanForced(iep);
        }
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
        addValidationErrors(
                m_behavior.executeImplementNotifyIep(progress, getOrganization(), getLocale(), getBroker()));
    }

    /**
     * Changes a student's status to ineligible using default sped workflow behavior.
     * If the workflow is of type Review, set the iep status to "DISCARDED"
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeIneligible(WorkflowProgress progress) throws Exception {
        m_behavior.executeIneligible(progress, getOrganization(), getBroker());

        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);

        WorkflowDefinition workflowDefinition = progress.getWorkflow().getWorkflowDefinition();
        Organization org = getOrganization();

        if (isReviewWorkflow(workflowDefinition, org)) {
            iep.setStatusCodeEnum(IepData.StatusCode.DISCARDED);
        } else if (isEvalWorkflow(workflowDefinition, org)) {
            SisStudent student = iep.getStudent();

            // Set the SPED (Special Education) status to "Exited" and SPED exit date to be
            // "Completed On" date
            String exitedCode = PreferenceManager.getPreferenceValue(org, SisPreferenceConstants.SPED_EXITED_CODE);
            student.setSpedStatusCode(exitedCode);
            student.setSpedExitDate(progress.getDate());

            // Set the SPED exit reason to "09" State Code from "Special Education Exit Reason"
            // reference table
            Criteria exitReasonCriteria = new Criteria();
            DataDictionaryField reasonField = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                    .findDataDictionaryFieldByAlias(ALIAS_SPED_EXIT_REASON);
            exitReasonCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, reasonField.getReferenceTableOid());
            exitReasonCriteria.addEqualTo(ReferenceCode.COL_STATE_CODE, SPED_EXIT_REASON_09);
            BeanQuery exitReasonQuery = new BeanQuery(ReferenceCode.class, exitReasonCriteria);
            ReferenceCode exitReason09 = (ReferenceCode) getBroker().getBeanByQuery(exitReasonQuery);

            if (exitReason09 != null) {
                String exitCode09 = exitReason09.getCode();
                if (!StringUtils.isEmpty(exitCode09)) {
                    ExtendedDictionaryAttributes extendedDataDictionary = iep.getExtendedDataDictionary();
                    DataDictionary dictionary =
                            DataDictionary.getDistrictDictionary(extendedDataDictionary, broker.getPersistenceKey());
                    student.setFieldValueByAlias(ALIAS_SPED_EXIT_REASON, exitCode09, dictionary);
                }
            }
        }

        broker.saveBeanForced(iep);
    }

    /**
     * Hold a meeting, set IEP Status to "Discarded," set the SPED status
     * to "ineligible" and set the SPED exit date to the progress date.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeIneligibleHoldMeeting(WorkflowProgress progress) throws Exception {
        executeIneligible(progress);
        executeHoldMeeting(progress);
    }

    /**
     * Finds the earliest initial referral date and sets this on current iep.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeInitialReferral(WorkflowProgress progress) throws Exception {
        PlainDate initialReferralDate = null;

        X2Broker broker = getBroker();
        IepData currentIep = (IepData) progress.getWorkflow().getOwner();
        SisStudent student = currentIep.getStudent();

        Collection<IepData> ieps = student.getIepData();
        for (IepData iep : ieps) {
            if (initialReferralDate == null || iep.getReferralDate().before(initialReferralDate)) {
                initialReferralDate = iep.getReferralDate();
            }
        }

        currentIep.setReferralDate(initialReferralDate);
        broker.saveBeanForced(currentIep);
    }

    /**
     * Sets an IEP's last review date using default sped workflow behavior.
     *
     * NOTE: this isn't executed in the standard IEP Re-evaluation workflows
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeLastReview(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        iep.setLastReviewDate(progress.getDate());
        broker.saveBeanForced(iep);
    }

    /**
     * Calculate and set the next review date.
     *
     * NOTE: this isn't executed in the standard IEP Re-evaluation workflow
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeNextReview(WorkflowProgress progress) throws Exception {
        Calendar nextReviewDate = Calendar.getInstance();

        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);

        // get + set last review date
        PlainDate lastReviewDate = progress.getDate();
        iep.setLastReviewDate(lastReviewDate);

        // calculate next review date to be one day less than one year from the initial meeting.
        nextReviewDate.setTime(lastReviewDate);
        nextReviewDate.add(Calendar.YEAR, 1);
        nextReviewDate.add(Calendar.DATE, -1);

        iep.setNextReviewDate(new PlainDate(nextReviewDate.getTime()));

        broker.saveBeanForced(iep);
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
            if (isReviewWorkflow(workflow.getWorkflowDefinition(), getOrganization())) {
                ((IepData) owner).setMeetingTypeCodeEnum(IepMeeting.TypeCode.REVIEW);
            } else if (isEvalWorkflow(workflow.getWorkflowDefinition(), getOrganization())) {
                ((IepData) owner).setMeetingTypeCodeEnum(IepMeeting.TypeCode.REEVAL);
            }
            broker.saveBeanForced(owner);
        }
    }

    /**
     * Gets the IEP consent date.
     * Sets the IEP last evaluation date with the 'Completed On' date.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeParentalConsent(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);
        PlainDate completedOnDate = progress.getDate();
        iep.setLastEvaluationDate(completedOnDate);
        broker.saveBeanForced(iep);
    }

    /**
     * Sets next initial eligibility date to be that of initial eligibility date.
     * Sets next evaluation date to be one day less than one year from the initial eligibility date.
     * Calls the executeEligibile method.
     *
     * @param progress WorkflowProgress
     */
    public void executeReevaluationEligibile(WorkflowProgress progress) {
        Calendar nextEligibilityDate = Calendar.getInstance();

        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);
        PlainDate currentDate = progress.getDate();
        // calculate next eligibility date to be one day less than one year from the initial
        // meeting.
        nextEligibilityDate.setTime(currentDate);
        nextEligibilityDate.add(Calendar.YEAR, 3);
        nextEligibilityDate.add(Calendar.DATE, -1);

        broker.saveBeanForced(iep);

        this.executeEligible(progress);
        // [08] next eligibility reevaluation - setting it to null as it is getting set in
        // SpedDefault WorkflowBehaviour
        iep.setLastEligibilityDate(null);
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
     * Sets last evaluation date on iep.
     *
     * @param progress WorkflowProgress
     */
    public void executeSetLastEvaluationDate(WorkflowProgress progress) {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);
        iep.setLastEvaluationDate(progress.getDate());
        broker.saveBeanForced(iep);
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
     * Find the most recent meeting, the update just IEP meeting date field.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeUpdateMeeting(WorkflowProgress progress) throws Exception {
        // Not currently used
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
     */
    @Override
    public X2BaseBean initializeOwner(X2BaseBean selectionBean,
                                      Map<String, FormInstance> formInstances,
                                      PlainDate date,
                                      WorkflowDefinition workflowDefinition) {
        LinkedList<ValidationError> errors = new LinkedList<ValidationError>();

        IepData owner = m_behavior.executeRenewal((SisStudent) selectionBean, workflowDefinition, formInstances,
                getLocale(), errors, getBroker());

        // Pull Start and End Dates from Prior IEP if it is Re-Evaluation Workflow
        if (isEvalWorkflow(workflowDefinition, getOrganization())) {
            IepData iep = ((SisStudent) selectionBean).getActiveIep();
            PlainDate startDate = iep.getStartDate();
            PlainDate endDate = iep.getEndDate();
            owner.setStartDate(startDate);
            owner.setEndDate(endDate);
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
     * Rolls back changes made by executedConsecutiveReevalReview method.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackConsecutiveReevalReview(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();

        FormInstance formInstance = progress.getFormInstanceById(OID_MEETING_FORM, broker);
        if (formInstance != null) {
            IepMeeting meeting = (IepMeeting) formInstance.getStorageObject(broker);
            if (meeting != null) {
                ExtendedDictionaryAttributes extendedDataDictionary = meeting.getIepData().getExtendedDataDictionary();
                DataDictionary dictionary =
                        DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());
                meeting.setFieldValueByAlias(ALIAS_CONSECUTIVE_REEVAL_REVIEW, BooleanAsStringConverter.FALSE,
                        dictionary);
                broker.saveBeanForced(meeting);
            }
        }

        rollbackHoldMeeting(progress);
    }

    /**
     * Rolls back changes made by the eligible method using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackEligible(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackEligible(progress, getOrganization(), getBroker());
    }

    /**
     * Rolls back changes made by the exit method using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackExit(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackExit(progress, getOrganization(), getBroker());
    }

    /**
     * Rolls back "Start Date" + "End Date" fields of IEP to null.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackFinalizeIep(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner();

        boolean isCompleteReEvalDiscus = false;

        Collection<WorkflowProgress> workFlowProgresses = progress.getWorkflow().getWorkflowProgress();
        for (WorkflowProgress workflowProgress : workFlowProgresses) {
            // Getting phase oid - Hold re-evaluation meeting
            if (isOidMatch(PHASE_HOLD_REEVAL_MEETING, workflowProgress.getWorkflowPhaseOid())) {
                // Getting the WF phase outcome oid- Complete re-evaluation discussion.
                if (isOidMatch(OUTCOME_COMPLETE_REEVAL_DISC, workflowProgress.getWorkflowPhaseOutcomeOid())) {
                    isCompleteReEvalDiscus = true;
                }
            }
        }
        // IF phase "Hold re-evaluation meeting" = "Complete re-evaluation discussion", then fields
        // "Start Dates", "End Date"
        // and "Next IEP Review Date" shouldn't be updated at the phase "Finalize IEP"
        if (!isCompleteReEvalDiscus) {
            iep.setStartDate(null);
            iep.setEndDate(null);
        }

        broker.saveBeanForced(iep);

        this.rollbackSubmitForApproval(progress);
    }

    /**
     * Rolls back effects of executeFinalizeReviewIep() by calling.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackFinalizeReviewIep(WorkflowProgress progress) throws Exception {
        this.rollbackLastReview(progress);
        this.rollbackFinalizeIep(progress);
    }

    /**
     * Rolls back changes made by the hold meeting method.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackHoldMeeting(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner();

        boolean isCompleteReEvalDiscus = false;
        boolean isEduPlanOrNewAdditElig = false;

        Collection<WorkflowProgress> workFlowProgresses = progress.getWorkflow().getWorkflowProgress();

        for (WorkflowProgress workflowProgress : workFlowProgresses) {
            // Getting phase oid - Hold re-evaluation meeting
            if (isOidMatch(PHASE_HOLD_REEVAL_MEETING, workflowProgress.getWorkflowPhaseOid())) {
                // Getting the WF phase outcome oid- Complete re-evaluation discussion.
                if (isOidMatch(OUTCOME_COMPLETE_REEVAL_DISC, workflowProgress.getWorkflowPhaseOutcomeOid())) {
                    isCompleteReEvalDiscus = true;
                }
            }

            // Getting phase oid - Select evaluation decision
            if (isOidMatch(PHASE_SELECT_EVAL_DECISION, workflowProgress.getWorkflowPhaseOid())) {
                if (isOidMatch(OUTCOME_EVAL_NEEDED_EDU_PLAN, workflowProgress.getWorkflowPhaseOutcomeOid()) ||
                        isOidMatch(OUTCOME_EVAL_NEEDED_NEW_ADDIT_ELIG, workflowProgress.getWorkflowPhaseOutcomeOid())) {
                    isEduPlanOrNewAdditElig = true;
                }
            }
        }
        // IF phase "Hold re-evaluation meeting" = "Complete re-evaluation discussion",
        // AND IF the phase "Select Evaluation Decision" = "Evaluation needed for educational
        // planning" OR "Evaluation needed for new/additional elig.",
        // then field "[07] IEP Annual Review date" should not be removed in case when phase "Hold
        // IEP Meeting date" was unchecked
        if (!isCompleteReEvalDiscus && !isEduPlanOrNewAdditElig) {
            iep.setLastReviewDate(null);
        }

        iep.setMeetingDate(null);

        iep.setNextReviewDate(null);

        broker.saveBeanForced(iep);
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
     * Sets an IEP's last review date using default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackLastReview(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        iep.setLastReviewDate(null);
        broker.saveBeanForced(iep);
    }

    /**
     * Rolls back the changes made by the executeNextReview() by setting
     * next review date to null.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackNextReview(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);
        iep.setNextReviewDate(null);
        broker.saveBeanForced(iep);
    }

    /**
     * Rolls back the changes made by the parental consent method by setting
     * last evaluation date to null.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackParentalConsent(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);
        iep.setLastEvaluationDate(null);
        broker.saveBeanForced(iep);
    }

    /**
     * Rolls back changes made by the set eligibility method by setting the
     * last eligibility date and next evaluation date to null.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackReevaluationEligibile(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);
        iep.setLastEligibilityDate(null);
        broker.saveBeanForced(iep);

        this.rollbackEligible(progress);
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
     * Rolls back changes made by executeSetLastEvaluationDate.
     *
     * @param progress WorkflowProgress
     */
    public void rollbackSetLastEvaluationDate(WorkflowProgress progress) {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);
        iep.setLastEvaluationDate(null);
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
     * Rolls back changes made by the update meeting method by setting meeting .
     *
     * TODO check if we can remove this as well, when we can remove executeUpdateMeeting (if it's no
     * longer used).
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackUpdateMeeting(WorkflowProgress progress) throws Exception {
        // Not currently used
    }

    /**
     * Returns true if the passed workflow is an IEP evaluation workflow. Evaluation occurs during
     * the referral and re-evaluation workflows.
     *
     * @param workflowDefinition WorkflowDefinition
     * @param district Organization
     * @return boolean
     */
    private boolean isEvalWorkflow(WorkflowDefinition workflowDefinition, Organization district) {
        String reevalId = PreferenceManager.getPreferenceValue(district, SisPreferenceConstants.SPED_WORKFLOW_REEVAL);
        boolean isEval = reevalId.equals(workflowDefinition.getId());

        return isEval;
    }

    /**
     * Checks if oids match.
     *
     * @param oid1 String
     * @param oid2 String
     * @return true, if oids match
     */
    private boolean isOidMatch(String oid1, String oid2) {
        oid1 = BeanManager.getFullOid(oid1, getBroker().getPersistenceKey());
        oid2 = BeanManager.getFullOid(oid2, getBroker().getPersistenceKey());
        return oid1.equals(oid2);
    }

    /**
     * Returns true if the passed workflow is an IEP review workflow.
     *
     * @param workflowDefinition WorkflowDefinition
     * @param organization Organization
     * @return boolean
     */
    private boolean isReviewWorkflow(WorkflowDefinition workflowDefinition, Organization organization) {
        String reviewId =
                PreferenceManager.getPreferenceValue(organization, SisPreferenceConstants.SPED_WORKFLOW_RENEWAL);
        boolean isReview = reviewId.equals(workflowDefinition.getId());

        return isReview;
    }
}
