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

/*
 * Required imports for BIP Report generation
 */
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.procedures.SessionAwareProcedure;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.ga.SRSpedLevel;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehavior;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehaviorFactory;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.NotSerializableException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
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
public class SpedReferralProcedureBip extends WorkflowProcedure implements SessionAwareProcedure {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private SpedWorkflowBehavior m_behavior = null;
    private UserDataContainer m_userData;

    /**
     * Aliases
     */
    private static final String ALIAS_IEP_BABIES_CANT_WAIT = "babies-no-wait";
    private static final String ALIAS_IEP_INITIAL_EVALUATION = "initEvalDate";
    private static final String ALIAS_IEP_INITIAL_MEETING_DATE = "initMtgDate";
    private static final String ALIAS_IEP_MOST_RECENT_ELIGIBILITY = "iep-most-recent-elig";
    private static final String ALIAS_IEP_PLACEMENT_DATE = "initPlaceDate";
    private static final Object FORM_DEFINITION_ID_REFERRAL = "SPED-REF";
    private static final String ALIAS_SPED_REFERRAL_DATE = "parent-referral-date";

    /**
     * Constructs a new SpedReferralProcedure.
     *
     * @param definition WorkflowDefinition
     * @param district Organization
     * @param user User
     * @param broker X2Broker
     * @param locale Locale
     */
    public SpedReferralProcedureBip(WorkflowDefinition definition, Organization district,
            User user, X2Broker broker, Locale locale) {
        super(definition, district, user, broker, locale);
        m_behavior = SpedWorkflowBehaviorFactory.getWorkflowBehavior(getOrganization());
    }

    /**
     * Updates "Babies Can't Wait" field in Iep with date specified in workflow progress.
     *
     * @param progress WorkflowProgress
     */
    public void executeBabiesCantWait(WorkflowProgress progress) {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        setFieldDate(broker, iep, ALIAS_IEP_BABIES_CANT_WAIT, progress);
        broker.saveBeanForced(iep);
    }

    /**
     * Performs tasks associated with finding a student eligible using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeEligible(WorkflowProgress progress) {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        iep.setInitialEligibilityDate(progress.getDate());
        broker.saveBeanForced(iep);

        this.executeSetEligibility(progress);
    }

    /**
     * Updates "Initial Evaluation" field in Iep with date specified in workflow progress.
     *
     * @param progress WorkflowProgress
     */
    public void executeEligibleDevelopIep(WorkflowProgress progress) {

        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        setFieldDate(broker, iep, ALIAS_IEP_INITIAL_MEETING_DATE, progress);
        iep.setInitialEligibilityDate(progress.getDate());
        broker.saveBeanForced(iep);

        this.executeSetEligibility(progress);
    }

    /**
     * Sets start date of IEP. Calculates + sets end date of IEP.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeFinalizeIep(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner();

        // get + set start date
        PlainDate startDate = progress.getDate();
        iep.setStartDate(startDate);

        // set meeting date to start date
        iep.setMeetingDate(startDate);

        // calculate + set end date
        Calendar endDate = Calendar.getInstance();
        endDate.setTime(startDate);
        endDate.add(Calendar.YEAR, 1);
        endDate.add(Calendar.DATE, -1);
        iep.setEndDate(new PlainDate(endDate.getTime()));
        // Set Next IEP Review Date
        iep.setNextReviewDate(new PlainDate(endDate.getTime()));
        // set iep status to "Pending Approval"
        iep.setStatusCodeEnum(IepData.StatusCode.PENDING_APPROVAL);

        broker.saveBeanForced(iep);
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
        if (isBipIncludedInIep(progress)) {
            runBehaviorInterventionReport();
        }
    }

    /**
     * Changes a student's status to ineligible using default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeIneligible(WorkflowProgress progress) throws Exception {
        // save event code 12's date
        X2Broker broker = getBroker();
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(SpedUtils.getIepDictionary(getOrganization(), broker),
                        broker.getPersistenceKey());

        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);
        // save event code 12's date
        iep.setFieldValueByAlias(SRSpedLevel.ALIAS_PARENT_REFUSE_INIT_PLACEMENT, progress.getDate().toString(),
                dictionary);
        broker.saveBeanForced(iep);

        m_behavior.executeIneligible(progress, getOrganization(), broker);
    }

    /**
     * Updates "Initial Eligibility" field in Iep with date specified in workflow progress.
     *
     * @param progress WorkflowProgress
     */
    public void executeInitialEligibility(WorkflowProgress progress) {
        X2Broker broker = getBroker();
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(SpedUtils.getIepDictionary(getOrganization(), broker),
                        broker.getPersistenceKey());
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);
        iep.setInitialEligibilityDate(progress.getDate());

        // save event code 11's date
        iep.setFieldValueByAlias(SRSpedLevel.ALIAS_NOT_ELIGIBLE, progress.getDate().toString(), dictionary);
        broker.saveBeanForced(iep);

        m_behavior.executeIneligible(progress, getOrganization(), broker);
    }

    /**
     * Updates "Initial Evaluation" field in Iep with date specified in workflow progress.
     *
     * @param progress WorkflowProgress
     */
    public void executeInitialEvaluation(WorkflowProgress progress) {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);
        setFieldDate(broker, iep, ALIAS_IEP_INITIAL_EVALUATION, progress);
        broker.saveBeanForced(iep);
    }

    /**
     * Updates "Initial Meeting Date" field in Iep with date specified in workflow progress.
     * Calls executeNextReview()
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeInitialMeetingDate(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);
        setFieldDate(broker, iep, ALIAS_IEP_INITIAL_MEETING_DATE, progress);
        broker.saveBeanForced(iep);
        this.executeHoldMeeting(progress);
    }

    /**
     * Sets last evaluation date, updates "Initial Placement Meeting" field in Iep with date
     * specified
     * in workflow progress, executes iep approved.
     *
     * @param progress WorkflowProgress
     */
    public void executeInitialPlacementMeeting(WorkflowProgress progress) {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);
        setFieldDate(broker, iep, ALIAS_IEP_PLACEMENT_DATE, progress);
        m_behavior.executeIepApproved(progress, getOrganization(), getBroker());
    }

    /**
     * Calculate and set the next review date.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeNextReview(WorkflowProgress progress) throws Exception {
        SimpleDateFormat shortDateFormat = new SimpleDateFormat("MM/dd/yyyy");
        Calendar nextReviewDate = Calendar.getInstance();

        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);

        // get initial meeting date
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(SpedUtils.getIepDictionary(getOrganization(), broker),
                        broker.getPersistenceKey());
        String initialIepMeeting = (String) iep.getFieldValueByAlias("initMtgDate", dictionary);
        PlainDate initialIepMeetingDate = (PlainDate) shortDateFormat.parse(initialIepMeeting);

        // calculate next review date to be one day less than one year from the initial meeting.
        nextReviewDate.setTime(initialIepMeetingDate);
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
            ((IepData) owner).setMeetingTypeCodeEnum(IepMeeting.TypeCode.INITIAL);
            broker.saveBeanForced(owner);
        }
    }

    /**
     * Gets the IEP consent date.
     * Sets the IEP last evaluation date with the consent date.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeParentalConsent(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);
        iep.setConsentReceivedDate(progress.getDate());
        iep.setLastEvaluationDate(progress.getDate());
        broker.saveBeanForced(iep);
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
     * Sets next eligibility determination date to be that of initial eligibility date.
     * Sets next evaluation date to be one day less than one year from the initial eligibility date.
     *
     * @param progress WorkflowProgress
     */
    public void executeSetEligibility(WorkflowProgress progress) {
        Calendar nextEligibilityDate = Calendar.getInstance();

        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);
        PlainDate initialEligibilityDate = iep.getInitialEligibilityDate();

        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(SpedUtils.getIepDictionary(getOrganization(), broker),
                        broker.getPersistenceKey());
        DateAsStringConverter converter = (DateAsStringConverter) ConverterFactory
                .getConverterForClass(Converter.DATE_CONVERTER, getLocale(), true);
        iep.setFieldValueByAlias(ALIAS_IEP_MOST_RECENT_ELIGIBILITY, converter.getSystemString(initialEligibilityDate),
                dictionary);

        // calculate next eligibility date to be one day less than three years from the initial
        // meeting.
        nextEligibilityDate.setTime(initialEligibilityDate);
        nextEligibilityDate.add(Calendar.YEAR, 3);
        nextEligibilityDate.add(Calendar.DATE, -1);

        // set nextEligibility date.
        iep.setNextEvaluationDate(new PlainDate(nextEligibilityDate.getTime()));

        broker.saveBeanForced(iep);
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
        IepData iep = m_behavior.executeReferral((SisStudent) selectionBean, workflowDefinition, formInstances, date,
                getBroker());
        X2Broker broker = getBroker();

        // set meeting iep type to "initial"
        iep.setMeetingTypeCodeEnum(IepMeeting.TypeCode.INITIAL);

        FormInstance referralFormInstance = formInstances.get(FORM_DEFINITION_ID_REFERRAL);

        // Set the IEP referral date to the date found on the CT Sped Referral Form
        if (referralFormInstance != null) {
            GenericFormData genericFormData = (GenericFormData) broker.getBeanByOid(GenericFormData.class,
                    referralFormInstance.getStorageObjectOid());
            DataDictionary dictionary = DataDictionary
                    .getDistrictDictionary(genericFormData.getExtendedDataDictionary(), broker.getPersistenceKey());
            String referralDateString =
                    (String) genericFormData.getFieldValueByAlias(ALIAS_SPED_REFERRAL_DATE, dictionary);
            Converter converter = ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                    Locale.getDefault(),
                    true);
            PlainDate referralDate =
                    (PlainDate) ((DateAsStringConverter) converter).parseSystemString(referralDateString);
            if (referralDate != null) {
                iep.setReferralDate(referralDate);
            }
        }

        // save the bean
        broker.saveBeanForced(iep);

        return iep;
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
     * Rolls back "Babies Can't Wait" date field in Iep to null.
     *
     * @param progress WorkflowProgress
     */
    public void rollbackBabiesCantWait(WorkflowProgress progress) {
        rollbackFieldDate(ALIAS_IEP_BABIES_CANT_WAIT, progress);
    }

    /**
     * Rolls back changes made by the eligible method using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackEligible(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        iep.setInitialEligibilityDate(null);
        broker.saveBeanForced(iep);
    }

    /**
     * Rolls back "Initial Evaluation" date field in Iep to null.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackEligibleDevelopIep(WorkflowProgress progress) throws Exception {
        rollbackFieldDate(ALIAS_IEP_INITIAL_MEETING_DATE, progress);
        m_behavior.rollbackEligible(progress, getOrganization(), getBroker());
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

        // reset fields to null
        iep.setStartDate(null);
        iep.setMeetingDate(null);
        iep.setEndDate(null);

        // reset iep status to "Draft"
        iep.setStatusCodeEnum(IepData.StatusCode.DRAFT);
        broker.saveBeanForced(iep);
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
        X2Broker broker = getBroker();
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(SpedUtils.getIepDictionary(getOrganization(), broker),
                        broker.getPersistenceKey());

        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);
        // save event code 12's date
        iep.setFieldValueByAlias(SRSpedLevel.ALIAS_PARENT_REFUSE_INIT_PLACEMENT, null, dictionary);
        broker.saveBeanForced(iep);

        m_behavior.rollbackIneligible(progress, getOrganization(), broker);
    }

    /**
     * Rolls back "Initial Eligibility" date field in Iep to null.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackInitialEligibility(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(SpedUtils.getIepDictionary(getOrganization(), broker),
                        broker.getPersistenceKey());
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);
        iep.setInitialEligibilityDate(null);
        // code 11
        iep.setFieldValueByAlias(SRSpedLevel.ALIAS_NOT_ELIGIBLE, null, dictionary);
        broker.saveBeanForced(iep);

        m_behavior.rollbackIneligible(progress, getOrganization(), getBroker());
    }

    /**
     * Rolls back "Initial Evaluation" date field in Iep to null.
     *
     * @param progress WorkflowProgress
     */
    public void rollbackInitialEvaluation(WorkflowProgress progress) {
        rollbackFieldDate(ALIAS_IEP_INITIAL_EVALUATION, progress);
    }

    /**
     * Rolls back "Initial Meeting Date" date field in Iep to null.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackInitialMeetingDate(WorkflowProgress progress) throws Exception {
        rollbackFieldDate(ALIAS_IEP_INITIAL_MEETING_DATE, progress);
        this.rollbackHoldMeeting(progress);
    }

    /**
     * Rolls back changes made in executeInitialPlacementMeeting.
     *
     * @param progress WorkflowProgress
     */
    public void rollbackInitialPlacementMeeting(WorkflowProgress progress) {
        rollbackFieldDate(ALIAS_IEP_PLACEMENT_DATE, progress);
        m_behavior.executeIepApproved(progress, getOrganization(), getBroker());
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
        iep.setConsentReceivedDate(null);
        iep.setLastEvaluationDate(null);
        broker.saveBeanForced(iep);
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
     * Rolls back changes made by the set eligibility method by setting the
     * eligibility determination date and next evaluation date to null.
     *
     * @param progress WorkflowProgress
     */
    public void rollbackSetEligibility(WorkflowProgress progress) {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);

        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(SpedUtils.getIepDictionary(getOrganization(), broker),
                        broker.getPersistenceKey());
        iep.setFieldValueByAlias(ALIAS_IEP_MOST_RECENT_ELIGIBILITY, null, dictionary);
        iep.setNextEvaluationDate(null);
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
     * Sets field "dateField" to null.
     *
     * @param dateField String
     * @param progress WorkflowProgress
     */
    private void rollbackFieldDate(String dateField, WorkflowProgress progress) {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);

        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(SpedUtils.getIepDictionary(getOrganization(), broker),
                        broker.getPersistenceKey());
        iep.setFieldValueByAlias(dateField, null, dictionary);
        broker.saveBeanForced(iep);
    }

    /**
     * Sets field "dateField" with date value found in workflow progress.
     *
     * @param broker X2Broker
     * @param iep IepData
     * @param dateField String
     * @param progress WorkflowProgress
     */
    private void setFieldDate(X2Broker broker, IepData iep, String dateField, WorkflowProgress progress) {
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(SpedUtils.getIepDictionary(getOrganization(), broker),
                        broker.getPersistenceKey());
        DateAsStringConverter converter = (DateAsStringConverter) ConverterFactory
                .getConverterForClass(Converter.DATE_CONVERTER, getLocale(), true);
        iep.setFieldValueByAlias(dateField, converter.getSystemString(progress.getDate()), dictionary);
    }

    /**
     * Run the BIP Report in the background, copy results to Documents side-tab, and send
     * notification e-mail to IEP Team and Current Teachers
     */
    private void runBehaviorInterventionReport() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ID, "MEL3824348");

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        Report bipReport = (Report) getBroker().getBeanByQuery(query);
        if (bipReport != null) {
            try {
                ToolInput inputFile = new ToolInput(bipReport, null, m_userData, Locale.getDefault());
                inputFile.setParameterAsString("printAsDraft", "false");
                inputFile.setParameterAsString("includeAllAttendees", "false");
                inputFile.setParameterAsString("emailNotifDocCopy", "true");

                m_userData.setToolInput(inputFile);

                ToolJob job = ToolJob.createJob(bipReport, m_userData, m_userData.getUserTempFolder(), "outputFile.txt",
                        false, m_userData.getLocale());
                try {
                    job.run();


                    File outputFolder = null;
                    String jobResultsOid = null;
                    int jobStatus = job.getStatus();
                    String jobStatusResponse = "Job status:";
                    if (jobStatus == ToolJob.STATUS_RUNNING) {
                        jobStatusResponse += "RUNNING";
                    } else if (jobStatus == ToolJob.STATUS_ERROR) {
                        jobStatusResponse += "ERROR";
                        AppGlobals.getLog().log(Level.INFO, "Error detected");
                        Throwable error = job.getThrowable();
                        if (error != null) {
                            AppGlobals.getLog().log(Level.INFO, "Error message: " + error.getMessage());
                        } else {
                            AppGlobals.getLog().log(Level.INFO, "Error is null");
                        }
                    } else if (jobStatus == ToolJob.STATUS_NO_DATA) {
                        jobStatusResponse += "NO DATA";
                    } else if (jobStatus == ToolJob.STATUS_RUNNING) {
                        jobStatusResponse += "RUNNING";
                    } else if (jobStatus == ToolJob.STATUS_FORMATTING) {
                        jobStatusResponse += "FORMATTING";
                    } else if (jobStatus == ToolJob.STATUS_SUCCESS) {
                        jobStatusResponse += "SUCCESS";
                        jobResultsOid = job.getJobResultOid();
                        outputFolder = job.getTempFolder();
                    } else if (jobStatus == ToolJob.STATUS_ABORT) {
                        jobStatusResponse += "ABORT";
                    }
                    AppGlobals.getLog().log(Level.INFO, "jobStatusResponse:" + jobStatusResponse);

                    if (jobResultsOid != null) {
                        JobResult results = (JobResult) getBroker().getBeanByOid(JobResult.class, jobResultsOid);
                        if (results != null) {
                            AppGlobals.getLog().log(Level.INFO, "Job Comment:" + results.getComments());
                        } else {
                            AppGlobals.getLog().log(Level.INFO, "No results");
                        }
                    } else {
                        AppGlobals.getLog().log(Level.INFO, "No results oid");
                    }

                    if (outputFolder != null && outputFolder.exists() && outputFolder.isDirectory()) {
                        File[] files = outputFolder.listFiles();
                        for (File file : files) {
                            if (file.getPath().indexOf("outputFile.txt") >= 0) {
                                try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
                                    String line = "";
                                    while ((line = reader.readLine()) != null) {
                                        AppGlobals.getLog().log(Level.INFO, "Line: " + line);
                                    }
                                } catch (Exception e) {
                                    // No action
                                }
                            }
                        }
                    } else {
                        AppGlobals.getLog().log(Level.INFO, "Output Folder does not exist");
                    }

                } finally {
                    job.getResultHandler().close();
                    m_userData.setToolInput(null);
                }
            } catch (X2BaseException xbe) {
                AppGlobals.getLog().warning("Unable to initiate Behavior Intervention Report: " + xbe.getMessage());
            } catch (NotSerializableException nse) {
                AppGlobals.getLog().warning("Unable to initiate Behavior Intervention Report: " + nse.getMessage());
            } catch (IOException ioe) {
                AppGlobals.getLog().warning("Unable to initiate Behavior Intervention Report: " + ioe.getMessage());
            }
        }
    }

    private boolean isBipIncludedInIep(WorkflowProgress progress) {
        boolean bipIsIncluded = false;

        IepData iepForm = (IepData) progress.getWorkflow().getOwner();
        if (iepForm != null) {
            DataDictionary iepFormDictionary = DataDictionary.getDistrictDictionary(iepForm.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey());
            String bipDeveloped =
                    (String) iepForm.getFieldValueByAlias("iep-behavioral-intervention", iepFormDictionary);

            if (!StringUtils.isEmpty(bipDeveloped) && bipDeveloped.equalsIgnoreCase("Yes")) {
                bipIsIncluded = true;
            }
        }

        return bipIsIncluded;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.SessionAwareProcedure#setUserData(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    public void setUserData(UserDataContainer userData) {
        m_userData = userData;
    }
}
