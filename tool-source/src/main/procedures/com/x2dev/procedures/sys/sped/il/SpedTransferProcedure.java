/* #PROCEDURE-ID [SYS-SPED-TRANS] */
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
import static com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure.FMD_ID_TRANSFER;
import static com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure.NUMBER_OF_PHASE_FIRST;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.Workflow;
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure.MeetingTypes;
import com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure.TransferContainerFabric;
import com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure.TransferToX2BaseBeanContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehavior;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehaviorFactory;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Defines special behavior that occurs on the special education IEP transfer workflow.
 * <p>
 * By default, all behavior is defined in the business layer. Custom workflow behavior can be
 * defined directly in this procedure. When adding behavior beyond what is defined in the business
 * layer, provide a detailed description of the changes made.
 *
 * @author X2 Development Corporation
 */
public class SpedTransferProcedure extends WorkflowProcedure {
    /**
     *
     */
    private static final String ALIGN_NOTES3 = "Notes3";
    private static final String ALIGN_H_MEET3 = "HMeet3";
    private static final long serialVersionUID = 1L;
    private SpedWorkflowBehavior m_behavior = null;
    private SpedIlWorkflowCommonProcedure m_ilWorkflowHelper = null;

    /**
     * Constructs a new SpedIepTransferProcedure.
     *
     * @param definition WorkflowDefinition
     * @param district Organization
     * @param user User
     * @param broker X2Broker
     * @param locale Locale
     */
    public SpedTransferProcedure(WorkflowDefinition definition, Organization district,
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
        m_ilWorkflowHelper.addInd13FormInstances(owner, student);
    }

    /**
     * Complete phase.
     *
     * @param progress WorkflowProgress
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#completePhase(com.follett.fsc.
     *      core.k12.beans.WorkflowProgress)
     */
    @Override
    public void completePhase(WorkflowProgress progress) {
        IepData iepData = (IepData) progress.getWorkflow().getOwner();
        prepareFormInstance(iepData, progress);
        super.completePhase(progress);
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
     * Performs tasks associated with a successful IEP approval using the default sped workflow.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeIepApproved(WorkflowProgress progress) throws Exception {
        addValidationErrors(m_behavior.executeIepApproved(progress, getOrganization(), getBroker()));
    }

    /**
     * execute transfer meeting phase.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeTrMeeting(WorkflowProgress progress) throws Exception {
        m_ilWorkflowHelper.setMeeitngType(progress, MeetingTypes.TRANSFER_IN.toString());
        // addValidationErrors();
    }

    /**
     * Peforms tasks associated with sending the approval phase using the default sped.
     *
     * @param progress WorkflowProgress
     */
    public void executeSubmitForApproval(WorkflowProgress progress) {
        m_behavior.executeSubmitForApproval(progress, getBroker());
    }

    /**
     * execute transfer hold meeting phase.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeTrHoldMeeting(WorkflowProgress progress) throws Exception {
        m_behavior.executeHoldMeeting(progress, getOrganization(), getBroker());
        m_ilWorkflowHelper.setMeeitngType(progress, MeetingTypes.TRANSFER_IN.toString());
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
     * Change IEP meeting type on workflow converting.
     *
     * @param workflow Workflow
     * @param broker X2Broker
     */
    @Override
    // * this is feature for b-5-5 and biggest version
    public void executeOnConvert(Workflow workflow, X2Broker broker) {
        X2BaseBean owner = workflow.getOwner();
        if (owner instanceof IepData) {
            IepData iepData = (IepData) owner;
            m_ilWorkflowHelper.setTypeToIep(iepData, IepMeeting.TypeCode.OTHER);
            List<IepMeeting> meetings = setTransferTypeToMeeting(workflow);
            m_ilWorkflowHelper.deleteOtherMeeting(iepData, meetings);
            m_ilWorkflowHelper.alignDifFormDefinition(workflow, new ArrayList(Arrays.asList(ALIGN_NOTES3)));

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
        IepData owner = m_behavior.executeTransfer((SisStudent) selectionBean, formInstances, getLocale(), getBroker());
        addFormInstances(owner, (SisStudent) selectionBean);

        return owner;
    }

    /**
     * Initialize form.
     *
     * @param formInstance FormInstance
     * @param formStorage X2BaseBean
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#initializeForm(com.follett.fsc.
     *      core.k12.beans.FormInstance, com.follett.fsc.core.k12.beans.X2BaseBean,
     *      com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    public void initializeForm(FormInstance formInstance, X2BaseBean formStorage, UserDataContainer userData)
            throws X2BaseException {
        super.initializeForm(formInstance, formStorage, userData);
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
     * roll back transfer meeting phase.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackTrMeeting(WorkflowProgress progress) throws Exception {
        m_ilWorkflowHelper.rollbackMeeitngType(progress, MeetingTypes.TRANSFER_IN.toString());
    }

    /**
     * Rolls back changes made when an IEP is approved using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackIepApproved(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackIepApproved(progress, getOrganization(), getBroker());
    }

    /**
     * Rollback submit for approval.
     *
     * @param progress WorkflowProgress
     */
    public void rollbackSubmitForApproval(WorkflowProgress progress) {
        m_behavior.rollbackSubmitForApproval(progress, getBroker());
    }

    /**
     * roll back hold transfer meeting phase .
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackTrHoldMeeting(WorkflowProgress progress) throws Exception {
        m_ilWorkflowHelper.rollbackMeeitngType(progress, null);
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
     * transfer data from "SPED-XFER form" storage table to Iep data.
     *
     * @param iepData IepData
     * @param progress WorkflowProgress
     */
    private void copyTransferDataToIep(IepData iepData, WorkflowProgress progress) {
        FormInstance transferFormInstance = m_ilWorkflowHelper.getFormInstanceByFmdId(FMD_ID_TRANSFER, progress);
        X2BaseBean beanFrom = transferFormInstance.getStorageObject();
        DataDictionary ddxFrom = m_ilWorkflowHelper.getDataDictionaryByFormInstance(transferFormInstance);
        TransferContainerFabric transferContainerFabric =
                m_ilWorkflowHelper.getTransferContainerFabric(progress, ddxFrom);
        DataDictionary ddxTo =
                m_ilWorkflowHelper.getDictionaryByExtendedDictionaryId(EXTENDED_DICTIOANRY_ID_SPED_IL_IEP);
        TransferToX2BaseBeanContainer container =
                transferContainerFabric.createTransferContainer(beanFrom, iepData, ddxFrom, ddxTo);
        container.transferValues(true);
    }



    /**
     * try prepopulate formInstance when it's created.
     *
     * @param iepData IepData
     * @param progress WorkflowProgress
     */
    private void prepareFormInstance(IepData iepData, WorkflowProgress progress) {
        int numberOfPhase = progress.getWorkflowPhase().getSequenceNumber();
        if (numberOfPhase == NUMBER_OF_PHASE_FIRST) {
            m_ilWorkflowHelper.fill3457BC(iepData);
            m_ilWorkflowHelper.fill3457C(iepData);
            m_ilWorkflowHelper.fillSpAppr(iepData);
            m_ilWorkflowHelper.fill3454I(iepData);
            m_ilWorkflowHelper.fillIndicator(iepData);
            m_ilWorkflowHelper.fillSumPref(iepData);
            m_ilWorkflowHelper.fill3454V(iepData);
            copyTransferDataToIep(iepData, progress);
        }
    }


    /**
     * start using from b-5-5 version
     * set AnnualReview meeting type to meeting which belong to forminstance where outcome has
     * HMeet3 align.
     *
     * @param workflow Workflow
     * @return List
     */
    private List<IepMeeting> setTransferTypeToMeeting(Workflow workflow) {
        List<IepMeeting> iepMeetings = new ArrayList<IepMeeting>();
        DataDictionary ddx = m_ilWorkflowHelper.getDictionaryByExtendedDictionaryId(EXTENDED_DICTIOANRY_ID_SPED_IL_IEP);
        IepMeeting iepMeeting = m_ilWorkflowHelper.setMeetingTypeIntoMeeting(workflow, ddx, ALIGN_H_MEET3,
                IepMeeting.TypeCode.OTHER, MeetingTypes.TRANSFER_IN.toString());
        if (iepMeeting != null) {
            iepMeetings.add(iepMeeting);
            if (iepMeeting.isDirty()) {
                getBroker().saveBeanForced(iepMeeting);
            }
        }

        return iepMeetings;
    }

}
