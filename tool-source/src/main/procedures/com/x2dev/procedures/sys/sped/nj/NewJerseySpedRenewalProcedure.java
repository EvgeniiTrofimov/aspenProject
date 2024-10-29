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
package com.x2dev.procedures.sys.sped.nj;

import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.NewJerseySpedWorkflowBehavior;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehaviorFactory;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2RuntimeException;
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
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Defines NJ specific special behavior that occurs on the special education IEP renewal workflow.
 * <p>
 * By default, all behavior is defined in the business layer. Custom workflow behavior can be
 * defined directly in this procedure. When adding behavior beyond what is defined in the business
 * layer, provide a detailed description of the changes made.
 *
 * @author X2 Development Corporation
 */
public class NewJerseySpedRenewalProcedure extends WorkflowProcedure {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private static final String ALIAS_CURRENT_EVAL_DATE = "iep-sped-current-eval-date";
    private static final String ALIAS_CASE_MANAGER = "Case Manager";
    private NewJerseySpedWorkflowBehavior m_behavior = null;

    private SimpleDateFormat inputFormat = new SimpleDateFormat("yyyy-MM-dd");

    /**
     * Constructs a new SpedRenewalProcedure.
     *
     * @param definition WorkflowDefinition
     * @param district Organization
     * @param user User
     * @param broker X2Broker
     * @param locale Locale
     */
    public NewJerseySpedRenewalProcedure(WorkflowDefinition definition, Organization district,
            User user, X2Broker broker, Locale locale) {
        super(definition, district, user, broker, locale);
        m_behavior = (NewJerseySpedWorkflowBehavior) SpedWorkflowBehaviorFactory.getWorkflowBehavior(getOrganization());
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
     * Peforms tasks associated with evaluation plan meeting using the NJ sped
     * workflow behavior.
     *
     * @param progress WorkflowProgress
     */
    public void executeEvalPlanMeeting(WorkflowProgress progress) {
        m_behavior.executeEvalPlanMeeting(progress, getOrganization(), getBroker());
    }

    /**
     * Rolls back tasks associated with evaluation plan meeting using the NJ sped
     * workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackEvalPlanMeeting(WorkflowProgress progress) throws Exception {
        addValidationErrors(m_behavior.rollbackEvalPlanMeeting(progress, getOrganization(), getBroker()));
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
     * Performs tasks associated with an IEP rejection using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeIepRejected(WorkflowProgress progress) {
        m_behavior.executeIepRejected(progress, getBroker());
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
     * Implements the draft IEP using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeImplementIep(WorkflowProgress progress) throws Exception {
        List<ValidationError> errors =
                m_behavior.executeImplementIep(progress, getOrganization(), getLocale(), getBroker());
        if (errors.size() > 0) {
            addValidationErrors(errors);
        } else {
            // Update the Case Manager Name on the Student when the IEP gets saved.
            IepData iep = (IepData) progress.getWorkflow().getOwner();
            String staffName = iep.getStaff().getNameView();
            Student student = iep.getStudent();
            student.setFieldValueByAlias(ALIAS_CASE_MANAGER, staffName);
            getBroker().saveBeanForced(student);
        }
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

        IepData iep = (IepData) progress.getWorkflow().getOwner();
        iep.setStatusCodeEnum(StatusCode.DRAFT);
        getBroker().saveBeanForced(iep);
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
     * Peforms tasks associated with sending the assessment consent form using the default sped
     * workflow behavior.
     *
     * @param progress WorkflowProgress
     */
    public void executeReceiveConsent(WorkflowProgress progress) {
        m_behavior.executeReceiveConsent(progress, getOrganization(), getLocale(), getBroker());
    }

    /**
     * Rolls back changes made by the send eval consent method using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackReceiveConsent(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackReceiveConsent(progress, getOrganization(), getBroker());
    }

    /*
     * IMPORTANT:
     * 
     * The empty methods below exist because there are workflow phase outcomes containing
     * the corresponding method IDs. Although no behavior is implemented here, the method IDs
     * are needed to drive custom NJ field defaults in MeetingDetail.java. Therefore these
     * methods must remain along with the method IDs.
     */

    /**
     * Execute schedule reeval meeting.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeScheduleReevalMeeting(WorkflowProgress progress) throws Exception {
        // no behavior necessary
    }

    /**
     * Rollback schedule reeval meeting.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackScheduleReevalMeeting(WorkflowProgress progress) throws Exception {
        // no behavior necessary
    }

    /**
     * Execute schedule det iep meeting.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeScheduleDetIepMeeting(WorkflowProgress progress) throws Exception {
        // no behavior necessary
    }

    /**
     * Rollback schedule det meeting.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackScheduleDetMeeting(WorkflowProgress progress) throws Exception {
        // no behavior necessary
    }

    /**
     * Execute schedule review meeting.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeScheduleReviewMeeting(WorkflowProgress progress) throws Exception {
        // no behavior necessary
    }

    /**
     * Rollback schedule review meeting.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackScheduleReviewMeeting(WorkflowProgress progress) throws Exception {
        // no behavior necessary
    }

    /**
     * Performs changes made by the submit for approval method using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeSubmitForApproval(WorkflowProgress progress) {
        m_behavior.executeSubmitForApproval(progress, getBroker());
    }

    /**
     * Rolls back changes made by the submit for approval method using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     */
    public void rollbackSubmitForApproval(WorkflowProgress progress) {
        List<ValidationError> errors = m_behavior.rollbackSubmitForApproval(progress, getBroker());
        if (errors.size() > 0) {
            addValidationErrors(errors);
        }
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

        SisStudent student = (SisStudent) selectionBean;

        IepData owner =
                m_behavior.executeRenewal(student, workflowDefinition, formInstances, getLocale(), errors, getBroker());

        // update either the last review or last evaluation date
        X2Broker broker = getBroker();

        // if this is a reevaluation workflow update the last evaluation date with the current eval
        // date,
        // then
        if (isEvalWorkflow(workflowDefinition, getOrganization())) {
            DataDictionary dictionary =
                    DataDictionary.getDistrictDictionary(SpedUtils.getIepDictionary(getOrganization(), broker),
                            broker.getPersistenceKey());

            PlainDate currentEvalDate = null;
            String currentEvalDateString = (String) owner.getFieldValueByAlias(ALIAS_CURRENT_EVAL_DATE, dictionary);
            if (!StringUtils.isEmpty(currentEvalDateString)) {
                try {
                    currentEvalDate = new PlainDate(inputFormat.parse(currentEvalDateString));

                    owner.setLastEvaluationDate(currentEvalDate);
                    owner.setFieldValueByAlias(ALIAS_CURRENT_EVAL_DATE, null, dictionary);
                    owner.setNextEvaluationDate(null);
                } catch (ParseException pe) {
                    throw new X2RuntimeException(pe);
                }
            }
        }

        PlainDate meetingDate = student.getActiveIep().getMeetingDate();
        owner.setLastReviewDate(meetingDate);
        owner.setNextReviewDate(null);
        owner.setMeetingDate(null);

        broker.saveBeanForced(owner);

        addValidationErrors(errors);

        return owner;
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
        String referralId =
                PreferenceManager.getPreferenceValue(district, SisPreferenceConstants.SPED_WORKFLOW_REFERRAL);
        boolean isEval = referralId.equals(workflowDefinition.getId());

        if (!isEval) {
            String reevalId =
                    PreferenceManager.getPreferenceValue(district, SisPreferenceConstants.SPED_WORKFLOW_REEVAL);
            isEval = reevalId.equals(workflowDefinition.getId());
        }

        return isEval;
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
}
