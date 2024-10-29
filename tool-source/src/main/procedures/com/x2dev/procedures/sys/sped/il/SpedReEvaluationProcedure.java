/* #PROCEDURE-ID [SYS-SPED-REEVAL] */
/*
 *
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2016 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.procedures.sys.sped.il;

import static com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure.EXTENDED_DICTIOANRY_ID_SPED_IL_IEP;
import static com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure.NUMBER_OF_PHASE_FIRST;
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
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure.MeetingTypes;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehavior;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehaviorFactory;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Defines special behavior that occurs on the special education IEP renewal workflow.
 * <p>
 * By default, all behavior is defined in the business layer. Custom workflow behavior can be
 * defined directly in this procedure. When adding behavior beyond what is defined in the business
 * layer, provide a detailed description of the changes made.
 *
 * @author X2 Development Corporation
 */
public class SpedReEvaluationProcedure extends WorkflowProcedure {

    private static final String ALIAS_COPY_CURRENT_IEP = "copy-current-iep";
    private static final String ALIGN_NOTES = "Notes";
    private static final String ALIGN_H_MEET = "HMeet";
    private static final String FMD_ID_SPED_REN = "SPED-REN";



    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private SpedWorkflowBehavior m_behavior = null;
    private SpedIlWorkflowCommonProcedure m_ilWorkflowHelper = null;


    /**
     * Constructs a new SpedRenewalProcedure.
     *
     * @param definition WorkflowDefinition
     * @param district Organization
     * @param user User
     * @param broker X2Broker
     * @param locale Locale
     */
    public SpedReEvaluationProcedure(WorkflowDefinition definition, Organization district,
            User user, X2Broker broker, Locale locale) {
        super(definition, district, user, broker, locale);
        m_behavior = SpedWorkflowBehaviorFactory.getWorkflowBehavior(getOrganization());
        m_ilWorkflowHelper = new SpedIlWorkflowCommonProcedure(this, definition, district, user, broker, locale);
    }


    /**
     * Create new Form Instance when Re-Evaluation Workflow is created and fill it by data from Old
     * FormInstance.
     *
     * @param owner IepData
     * @param student SisStudent
     */
    public void addFormInstances(IepData owner, SisStudent student) {
        m_ilWorkflowHelper.add3457CFormInstances(owner, student);
        m_ilWorkflowHelper.addInd13FormInstances(owner, student);
    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#completePhase(com.follett.fsc.core.k12.beans.WorkflowProgress)
     */
    @Override
    /**
     *
     * @see com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#completePhase(com.follett.fsc.core.k12.beans.WorkflowProgress)
     */
    public void completePhase(WorkflowProgress progress) {
        IepData iepData = (IepData) progress.getWorkflow().getOwner();
        prepareFormInstance(iepData, progress.getWorkflowPhase().getSequenceNumber());
        super.completePhase(progress);
    }

    /**
     * Save selected contact nameView in GenericFormData by alias field "al-pg-name".
     *
     * @param progress WorkflowProgress
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeDetermine(WorkflowProgress progress) {
        m_behavior.executeEligible(progress, getOrganization(), getBroker());
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
     * Holds an IEP meeting using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeHoldMeeting(WorkflowProgress progress) throws Exception {
        m_behavior.executeHoldMeeting(progress, getOrganization(), getBroker());
        m_ilWorkflowHelper.setMeeitngType(progress, MeetingTypes.RE_EVALUATION_ELIGIBILITY.toString());
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
    // * this is feature for b-5-5 and higher version
    public void executeOnConvert(Workflow workflow, X2Broker broker) {
        X2BaseBean owner = workflow.getOwner();
        if (owner instanceof IepData) {
            IepData iepData = (IepData) owner;
            m_ilWorkflowHelper.setTypeToIep(iepData, IepMeeting.TypeCode.REEVAL);
            List<IepMeeting> meetings = setReevaluationTypeToMeeting(workflow);
            m_ilWorkflowHelper.deleteOtherMeeting(iepData, meetings);
            m_ilWorkflowHelper.alignDifFormDefinition(workflow,
                    new ArrayList(Arrays.asList(ALIGN_NOTES, ALIGN_NOTES + 2, ALIGN_NOTES + 3)));

        }
    }


    /**
     * Execute review meeting.
     *
     * @param progress WorkflowProgress
     * @see com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure#setMeeitngType(
     *      WorkflowProgress, String)
     */
    public void executeReviewMeeting(WorkflowProgress progress) {
        m_ilWorkflowHelper.setMeeitngType(progress, MeetingTypes.RE_ANNUAL_REVIEW.toString());
    }


    /**
     * Execute re eligibility meeting.
     *
     * @param progress WorkflowProgress
     * @see com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure#setMeeitngType(
     *      WorkflowProgress, String)
     */
    public void executeReEligibilityMeeting(WorkflowProgress progress) {
        m_ilWorkflowHelper.setMeeitngType(progress, MeetingTypes.RE_EVALUATION_ELIGIBILITY.toString());
    }

    /**
     * Execute re domain meeting.
     *
     * @param progress WorkflowProgress
     * @see com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure#setMeeitngType(
     *      WorkflowProgress, String)
     */
    public void executeReDomainMeeting(WorkflowProgress progress) {
        m_ilWorkflowHelper.setMeeitngType(progress, MeetingTypes.RE_EVALUATION_DOMAIN.toString());
    }

    /**
     * Execute re hold DM.
     *
     * @param progress WorkflowProgress
     * @see com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure#setMeeitngType(
     *      WorkflowProgress, String)
     */
    public void executeReHoldDM(WorkflowProgress progress) {
        m_ilWorkflowHelper.setMeeitngType(progress, MeetingTypes.RE_EVALUATION_DOMAIN.toString());
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
     * Peforms tasks associated with sending the approval phase using the default sped
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
     */
    @Override
    public X2BaseBean initializeOwner(X2BaseBean selectionBean,
                                      Map<String, FormInstance> formInstances,
                                      PlainDate date,
                                      WorkflowDefinition workflowDefinition) {
        LinkedList<ValidationError> errors = new LinkedList<ValidationError>();

        IepData owner = m_behavior.executeRenewal((SisStudent) selectionBean, workflowDefinition, formInstances,
                getLocale(), errors, getBroker());

        addFormInstances(owner, (SisStudent) selectionBean);

        addValidationErrors(errors);

        return owner;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#initializeForm(com.follett.fsc.core.k12.beans.FormInstance,
     *      com.follett.fsc.core.k12.beans.X2BaseBean,
     *      com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    /**
     *
     * @see com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#initializeForm(com.follett.fsc.core.k12.beans.FormInstance,
     *      com.follett.fsc.core.k12.beans.X2BaseBean,
     *      com.follett.fsc.core.k12.web.UserDataContainer)
     */
    public void initializeForm(FormInstance formInstance, X2BaseBean formStorage, UserDataContainer userData)
            throws X2BaseException {
        super.initializeForm(formInstance, formStorage, userData);
        FormDefinition formDefinition = formInstance.getFormDefinition();
        String formId = formDefinition.getId();
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(formInstance.getFormDefinition().getExtendedDataDictionary(),
                        getBroker().getPersistenceKey());

        if (formId != null && formId.equals(FMD_ID_SPED_REN)) {
            GenericFormData formData = (GenericFormData) formStorage;
            formData.setFieldValueByAlias(ALIAS_COPY_CURRENT_IEP, BooleanAsStringConverter.TRUE, dictionary);
        }
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
     * Rollback determine.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @See {@link
     *      com.x2dev.sis.model.business.sped.SpedWorkflowBehavior#rollbackEligible(WorkflowProgress,
     *      Organization, X2Broker)}
     */
    public void rollbackDetermine(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackEligible(progress, getOrganization(), getBroker());
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
     * Rolls back changes made by the hold meeting method using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackHoldMeeting(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackHoldMeeting(progress, getOrganization(), getBroker());
        m_ilWorkflowHelper.rollbackMeeitngType(progress, null);
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
     * Rollback review meeting.
     *
     * @param progress WorkflowProgress
     * @see com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure#rollbackMeeitngType(
     *      WorkflowProgress, String)
     */
    public void rollbackReviewMeeting(WorkflowProgress progress) {
        m_ilWorkflowHelper.rollbackMeeitngType(progress, MeetingTypes.RE_ANNUAL_REVIEW.toString());
    }

    /**
     * Rollback re eligibility meeting.
     *
     * @param progress WorkflowProgress
     * @see com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure#rollbackMeeitngType(
     *      WorkflowProgress, String)
     */
    public void rollbackReEligibilityMeeting(WorkflowProgress progress) {
        m_ilWorkflowHelper.rollbackMeeitngType(progress, MeetingTypes.RE_EVALUATION_ELIGIBILITY.toString());
    }

    /**
     * Rollback re hold DM.
     *
     * @param progress WorkflowProgress
     * @see com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure#rollbackMeeitngType(
     *      WorkflowProgress, String)
     */
    public void rollbackReHoldDM(WorkflowProgress progress) {
        m_ilWorkflowHelper.rollbackMeeitngType(progress, null);
    }


    /**
     * Rollback re domain meeting.
     *
     * @param progress WorkflowProgress
     * @see com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure#rollbackMeeitngType(
     *      WorkflowProgress, String)
     */
    public void rollbackReDomainMeeting(WorkflowProgress progress) {
        m_ilWorkflowHelper.rollbackMeeitngType(progress, MeetingTypes.RE_EVALUATION_DOMAIN.toString());
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

    /**
     * try prepopulate formInstance when it's created.
     *
     * @param iepData IepData
     * @param numberOfPhase int
     */
    private void prepareFormInstance(IepData iepData, int numberOfPhase) {
        if (numberOfPhase == NUMBER_OF_PHASE_FIRST) {
            m_ilWorkflowHelper.fill3457BC(iepData);
            m_ilWorkflowHelper.fill3457C(iepData);
            m_ilWorkflowHelper.fillSpAppr(iepData);
            m_ilWorkflowHelper.fill3454I(iepData);
            m_ilWorkflowHelper.fillIndicator(iepData);
            m_ilWorkflowHelper.fillSumPref(iepData);
            m_ilWorkflowHelper.fill3454V(iepData);
        }
    }

    /**
     * start using from b-5-5 version
     * set ReAnnualReview meeting type to meeting which belong to forminstance where outcome has
     * HMeet3 align
     * and save this meeting.
     *
     * @param workflow Workflow
     * @return List
     */
    private List<IepMeeting> setReevaluationTypeToMeeting(Workflow workflow) {
        List<IepMeeting> iepMeetings = new ArrayList<IepMeeting>();
        DataDictionary ddx = m_ilWorkflowHelper.getDictionaryByExtendedDictionaryId(EXTENDED_DICTIOANRY_ID_SPED_IL_IEP);
        List<String> meetingCustomTypes =
                new ArrayList<String>(Arrays.asList(MeetingTypes.RE_EVALUATION_DOMAIN.toString(),
                        MeetingTypes.RE_EVALUATION_ELIGIBILITY.toString(),
                        MeetingTypes.RE_ANNUAL_REVIEW.toString()));
        int index = 0;
        for (String meetingTypes : meetingCustomTypes) {
            index++;
            String align = ALIGN_H_MEET + (index > 1 ? "" + index : "");
            IepMeeting iepMeeting = m_ilWorkflowHelper.setMeetingTypeIntoMeeting(workflow, ddx, align,
                    IepMeeting.TypeCode.REEVAL, meetingTypes);
            if (iepMeeting != null) {
                iepMeetings.add(iepMeeting);
                if (iepMeeting.isDirty()) {
                    getBroker().saveBeanForced(iepMeeting);
                }
            }
        }
        return iepMeetings;
    }



}
