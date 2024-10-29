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
package com.x2dev.procedures.sys.sped.ct;

import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehavior;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehaviorFactory;
import com.x2dev.utils.DateUtils;
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
import java.util.Calendar;
import java.util.Locale;
import java.util.Map;

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
    private static final long serialVersionUID = 1L;
    private SpedWorkflowBehavior m_behavior = null;
    private final String FORM_DEFINITION_ID_REFERRAL = "SPED-REF";
    private final String ALIAS_SPED_REFERRAL_DATE = "ed621-date-referred";

    /**
     * Aliases
     */
    private static final String ALIAS_IEP_ELIGIBLE = "iep-eligible";

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

        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);

        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(SpedUtils.getIepDictionary(getOrganization(), broker),
                        broker.getPersistenceKey());
        iep.setFieldValueByAlias(ALIAS_IEP_ELIGIBLE, "Yes", dictionary);
        broker.saveBeanForced(iep);
    }

    /**
     * Method executed when a meeting is held on a referral or IEP renewal workflow. The
     * meeting date on the IEP is set if it has not been set already. It is set to the date of the
     * IEP meeting instance in the attached form, if one exists.
     *
     * @param progress WorkflowProgress
     * @return List of ValiationError objects
     */
    public void executeHoldMeeting(WorkflowProgress progress) {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        PlainDate meetingDate = null;

        if (iep.getMeetingDate() == null) {
            FormInstance formInstance = progress.getFormInstance(broker);
            if (formInstance != null) {
                IepMeeting meeting = (IepMeeting) formInstance.getStorageObject(broker);
                if (meeting != null) {
                    meetingDate = meeting.getDate();
                }
            }
        }

        if (meetingDate != null) {
            iep.setMeetingDate(meetingDate);

            // calculate + set review dates
            Calendar calendar = Calendar.getInstance();
            calendar.setTime(meetingDate);
            calendar.add(Calendar.DAY_OF_YEAR, -1);
            calendar.add(Calendar.YEAR, 1);

            iep.setLastReviewDate(meetingDate);
            PlainDate nextWeekdayReviewDate = DateUtils.getPriorWeekDay(new PlainDate(calendar.getTimeInMillis()));
            iep.setNextReviewDate(nextWeekdayReviewDate);

            // calculate + set evaluation dates
            calendar.add(Calendar.YEAR, 2);

            iep.setLastEvaluationDate(meetingDate);
            PlainDate nextWeekdayEvaluationDate = DateUtils.getPriorWeekDay(new PlainDate(calendar.getTimeInMillis()));
            iep.setNextEvaluationDate(nextWeekdayEvaluationDate);

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
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeIneligible(WorkflowProgress progress) throws Exception {
        m_behavior.executeIneligible(progress, getOrganization(), getBroker());

        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);

        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(SpedUtils.getIepDictionary(getOrganization(), broker),
                        broker.getPersistenceKey());
        iep.setFieldValueByAlias(ALIAS_IEP_ELIGIBLE, "No", dictionary);
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
        IepData draftIep = m_behavior.executeReferral((SisStudent) selectionBean, workflowDefinition, formInstances,
                date, getBroker());


        FormInstance referralFormInstance = formInstances.get(FORM_DEFINITION_ID_REFERRAL);

        // Set the IEP referral date to the date found on the CT Sped Referral Form
        if (referralFormInstance != null) {
            SimpleDateFormat dataFormatter = new SimpleDateFormat("yyyy-MM-dd");
            X2Broker broker = getBroker();

            GenericFormData genericFormData = (GenericFormData) broker.getBeanByOid(GenericFormData.class,
                    referralFormInstance.getStorageObjectOid());
            DataDictionary dictionary = DataDictionary
                    .getDistrictDictionary(genericFormData.getExtendedDataDictionary(), broker.getPersistenceKey());
            String referralDateString =
                    (String) genericFormData.getFieldValueByAlias(ALIAS_SPED_REFERRAL_DATE, dictionary);
            PlainDate referralDate = null;

            try {
                referralDate = new PlainDate(dataFormatter.parse(referralDateString));
            } catch (ParseException e) {
                // do nothing. not a date
            }

            draftIep.setReferralDate(referralDate);

            // broker.saveBeanForced(draftIep);
        }

        return draftIep;
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
     * Rolls back changes made by the eligible method using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackEligible(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackEligible(progress, getOrganization(), getBroker());

        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);

        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(SpedUtils.getIepDictionary(getOrganization(), broker),
                        broker.getPersistenceKey());
        iep.setFieldValueByAlias(ALIAS_IEP_ELIGIBLE, null, dictionary);
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

        // note extra effects of executeHoldMeeting don't need to be rolled back
        // as default behaviour is to set next evaluation date back to last evaluation date.
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

        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);

        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(SpedUtils.getIepDictionary(getOrganization(), broker),
                        broker.getPersistenceKey());
        iep.setFieldValueByAlias(ALIAS_IEP_ELIGIBLE, null, dictionary);
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
     * Rolls back changes made by the submit for approval method using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     */
    public void rollbackSubmitForApproval(WorkflowProgress progress) {
        addValidationErrors(m_behavior.rollbackSubmitForApproval(progress, getBroker()));
    }

}
