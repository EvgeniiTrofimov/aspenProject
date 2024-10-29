/* #PROCEDURE-ID [SYS-SPED-REFER] */
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

import static com.follett.fsc.core.k12.business.BusinessRules.IMPLEMENT_IEP_DRAFT_REQUIRED;
import static com.follett.fsc.core.k12.business.ValidationConstants.BUSINESS_RULE_VIOLATION;
import static com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure.EXTENDED_DICTIOANRY_ID_SPED_IL_IEP;
/* import static com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure.FMD_ID_3457D; */
import static com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure.NUMBER_OF_PHASE_FIRST;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.Workflow;
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.forms.FormDetail;
import com.follett.fsc.core.k12.web.template.Template;
import com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure.MeetingTypes;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehavior;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehaviorFactory;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

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

    private static final String ALIGN_H_MEET = "HMeet";
    private static final String ALIGN_NOTES = "Notes";
    private static final String CONTEXT_3457B_C = "form.sped-il-3457b/c";
    private static final String CONTEXT_3454H = "form.sped-il-3454h";
    private static final String CONTEXT_3454I = "form.sped-il-3454i";
    private static final String TEMPLATE_NAME_INDICATOR_13 = "Indicator 13";

    private static final long serialVersionUID = 1L;
    private SpedWorkflowBehavior m_behavior = null;
    private Map<String, DataDictionary> m_dictionaryMap = new HashMap<String, DataDictionary>();
    private Map<String, String> m_formOidMethodNameMap = new HashMap<String, String>();
    private SpedIlWorkflowCommonProcedure m_ilWorkflowHelper = null;

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
        m_behavior = SpedWorkflowBehaviorFactory.getWorkflowBehavior(getOrganization());
        m_ilWorkflowHelper = new SpedIlWorkflowCommonProcedure(this, definition, district, user, broker, locale);
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
     * Schedule Domain Meeting phase .
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeDomainMeeting(WorkflowProgress progress) throws Exception {
        m_ilWorkflowHelper.setMeeitngType(progress, MeetingTypes.INITIAL_DOMAIN.toString());
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
     * Schedule Eligibility Meeting phase.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeEligibilityMeeting(WorkflowProgress progress) throws Exception {
        m_ilWorkflowHelper.setMeeitngType(progress, MeetingTypes.INITIAl_ELIGIBILITY.toString());
    }

    /**
     * Execute hold DM.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeHoldDM(WorkflowProgress progress) throws Exception {
        m_ilWorkflowHelper.setMeeitngType(progress, MeetingTypes.INITIAL_DOMAIN.toString());
    }

    /**
     * Execute hold EM.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeHoldEM(WorkflowProgress progress) throws Exception {
        m_ilWorkflowHelper.setMeeitngType(progress, MeetingTypes.INITIAl_ELIGIBILITY.toString());
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
        m_ilWorkflowHelper.setMeeitngType(progress, MeetingTypes.INITIAL_IEP.toString());
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
     * Schedule IEP Meeting phase.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeIepMeeting(WorkflowProgress progress) throws Exception {
        m_ilWorkflowHelper.setMeeitngType(progress, MeetingTypes.INITIAL_IEP.toString());
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
            m_ilWorkflowHelper.setTypeToIep(iepData, IepMeeting.TypeCode.INITIAL);
            List<IepMeeting> meetings = setInitialTypeToMeeting(workflow);
            m_ilWorkflowHelper.deleteOtherMeeting(iepData, meetings);
            m_ilWorkflowHelper.alignDifFormDefinition(workflow,
                    new ArrayList(Arrays.asList(ALIGN_NOTES, ALIGN_NOTES + 2, ALIGN_NOTES + 3)));

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
     * Changes Student Status to Active
     * behavior.
     *
     * @param progress WorkflowProgress
     * @return List
     */
    public List<ValidationError> executeTemporaryActive(WorkflowProgress progress) {
        List<ValidationError> errors = new ArrayList<ValidationError>(0);

        IepData iep = (IepData) progress.getWorkflow().getOwner();
        SisStudent student = iep.getStudent();
        String activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.SPED_ACTIVE_CODE);


        student.setSpedStatusCode(activeCode);
        getBroker().saveBeanForced(student);
        IepData draftIep = (IepData) progress.getWorkflow().getOwner();
        if (draftIep == null) {
            errors.add(new ValidationError(BUSINESS_RULE_VIOLATION, Integer.valueOf(IMPLEMENT_IEP_DRAFT_REQUIRED)));
        } else {
            draftIep.setStatusCodeEnum(StatusCode.ACTIVE);
            getBroker().saveBeanForced(draftIep);
        }


        errors.addAll(m_behavior.executeEligible(progress, getOrganization(), getBroker()));


        return errors;
    }

    /**
     * Gets the dictionary by extended dictionary id.
     *
     * @param extendedDataDictionaryID String
     * @return Data dictionary
     */
    @Deprecated
    /**
     * use the same method on <code>SpedIlWorkflowCommonProcedure</code>
     * 
     * @param extendedDataDictionaryID
     * @return
     */
    public DataDictionary getDictionaryByExtendedDictionaryId(String extendedDataDictionaryID) {
        DataDictionary returnValue = m_dictionaryMap.get(extendedDataDictionaryID);
        if (returnValue == null) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ExtendedDataDictionary.COL_ID, extendedDataDictionaryID);
            QueryByCriteria byCriteria = new QueryByCriteria(ExtendedDataDictionary.class, criteria);
            ExtendedDataDictionary extendedDataDictionary =
                    (ExtendedDataDictionary) getBroker().getBeanByQuery(byCriteria);
            returnValue = DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());
            m_dictionaryMap.put(extendedDataDictionaryID, returnValue);
        }
        return returnValue;
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
        return m_behavior.executeReferral((SisStudent) selectionBean, workflowDefinition, formInstances, date,
                getBroker());
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
     * Populate form.
     *
     * @param workflow Workflow
     * @param formDetail FormDetail
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#populateForm(com.follett.fsc.core
     *      .k12.beans.Workflow, com.follett.fsc.core.k12.web.forms.FormDetail,
     *      com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    public void populateForm(Workflow workflow, FormDetail formDetail, UserDataContainer userData)
            throws X2BaseException {

        if (formDetail != null && formDetail.getTemplates() != null) {
            for (Template template : formDetail.getTemplates()) {
                template.getProperties();
                String nameTemplate = template.getName();
                if (nameTemplate != null) {
                    if (nameTemplate.equals(m_ilWorkflowHelper.getTemplateNameByContext(CONTEXT_3457B_C))) {
                        m_ilWorkflowHelper.populateConsentEvaluation(formDetail, userData);
                    } else if (nameTemplate.equals(m_ilWorkflowHelper.getTemplateNameByContext(CONTEXT_3454H))) {
                        m_ilWorkflowHelper.populateSecondaryTransition(formDetail, userData);
                    }
                    /*
                     * 3454i form
                     * Form need pre-populating embedded lists, but for now form hasn't any
                     * workflow.
                     * This code was create for testing
                     */

                    else if (nameTemplate.equals(m_ilWorkflowHelper.getTemplateNameByContext(CONTEXT_3454I))) {
                        m_ilWorkflowHelper.populateTransitionServices(formDetail, userData);
                    } else if (nameTemplate.equals(TEMPLATE_NAME_INDICATOR_13)) {
                        m_ilWorkflowHelper.populateIndicator13(formDetail, userData);
                    }

                }
            }
        }
        super.populateForm(workflow, formDetail, userData);
    }

    /**
     * Rolls back changes made by the Determine method using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackDetermine(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackEligible(progress, getOrganization(), getBroker());
    }

    /**
     * rollback Schedule Domain Meeting phase.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */

    public void rollbackDomainMeeting(WorkflowProgress progress) throws Exception {
        m_ilWorkflowHelper.rollbackMeeitngType(progress, MeetingTypes.INITIAL_DOMAIN.toString());
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
     * roll back Schedule Eligibility Meeting phase.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackEligibilityMeeting(WorkflowProgress progress) throws Exception {
        m_ilWorkflowHelper.rollbackMeeitngType(progress, MeetingTypes.INITIAl_ELIGIBILITY.toString());
    }

    /**
     * roll back hold domain meeting pahse.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackHoldDM(WorkflowProgress progress) throws Exception {
        m_ilWorkflowHelper.rollbackMeeitngType(progress, null);
    }

    /**
     * roll back hold eligibility meeting.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackHoldEM(WorkflowProgress progress) throws Exception {
        m_ilWorkflowHelper.rollbackMeeitngType(progress, null);
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
     * rollback Schedule IEP Meeting phase.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackIepMeeting(WorkflowProgress progress) throws Exception {
        m_ilWorkflowHelper.rollbackMeeitngType(progress, MeetingTypes.INITIAL_IEP.toString());
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

    /**
     * Changes Student Status to Active
     * behavior.
     *
     * @param progress WorkflowProgress
     * @return List
     * @throws Exception exception
     */
    public List<ValidationError> rollbackTemporaryActive(WorkflowProgress progress) throws Exception {
        List<ValidationError> errors = new ArrayList<ValidationError>(0);

        IepData activeIep = (IepData) progress.getWorkflow().getOwner();
        SisStudent student = activeIep.getStudent();
        String referdCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.SPED_REFERRED_CODE);

        student.setSpedStatusCode(referdCode);
        getBroker().saveBeanForced(student);
        /*
         * Set the status of the active IEP back to pending.
         */
        activeIep.setStatusCodeEnum(StatusCode.DRAFT);
        m_behavior.rollbackEligible(progress, getOrganization(), getBroker());
        return errors;
    }

    /**
     * Validate form.
     *
     * @param formInstance FormInstance
     * @param formStorage X2BaseBean
     * @param userData UserDataContainer
     * @return List
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#validateForm(com.follett.fsc.core
     *      .k12.beans.FormInstance, com.follett.fsc.core.k12.beans.X2BaseBean,
     *      com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    public List validateForm(FormInstance formInstance, X2BaseBean formStorage, UserDataContainer userData) {
        List<ValidationError> returnErrors = super.validateForm(formInstance, formStorage, userData);

        String formOid = formInstance.getFormDefinition().getOid();

        Map<String, String> methodMap = getFormOidMethodNameMap();

        if (methodMap.containsKey(formOid)) {

            try {
                Method method = this.getClass().getDeclaredMethod(methodMap.get(formOid),
                        new Class[] {FormInstance.class, X2BaseBean.class, UserDataContainer.class});
                if (method != null) {
                    List erorrs = (List) method.invoke(this, new Object[] {formInstance, formStorage, userData});
                    returnErrors.addAll(erorrs);
                }

            } catch (NoSuchMethodException e) {
                // TODO Nothing to do
            } catch (SecurityException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

        }

        return returnErrors;
    }

    /**
     * may by it is old decision and not it is not needed.
     *
     * @return Map
     */
    private Map<String, String> getFormOidMethodNameMap() {
        if (m_formOidMethodNameMap.isEmpty()) {
            //
        }
        return m_formOidMethodNameMap;
    }


    /**
     * prepare form instance. List of filling instance depend on number of phase
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
    private List<IepMeeting> setInitialTypeToMeeting(Workflow workflow) {
        List<IepMeeting> iepMeetings = new ArrayList<IepMeeting>();
        DataDictionary ddx = m_ilWorkflowHelper.getDictionaryByExtendedDictionaryId(EXTENDED_DICTIOANRY_ID_SPED_IL_IEP);
        List<String> meetingCustomTypes = new ArrayList<String>(Arrays.asList(MeetingTypes.INITIAL_DOMAIN.toString(),
                MeetingTypes.INITIAl_ELIGIBILITY.toString(),
                MeetingTypes.INITIAL_IEP.toString()));
        int index = 0;
        for (String meetingTypes : meetingCustomTypes) {
            index++;
            String align = ALIGN_H_MEET + (index > 1 ? "" + index : "");
            IepMeeting iepMeeting = m_ilWorkflowHelper.setMeetingTypeIntoMeeting(workflow, ddx, align,
                    IepMeeting.TypeCode.INITIAL, meetingTypes);
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
