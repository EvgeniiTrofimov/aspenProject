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

import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.MessageProperties;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.WriteEmailManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.business.sped.NewJerseySpedWorkflowBehavior;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehaviorFactory;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Defines NJ specific special behavior that occurs on the special education initial referral
 * workflow for New Jersey.
 * <p>
 * By default, all behavior is defined in the business layer. Custom workflow behavior can be
 * defined directly in this procedure. When adding behavior beyond what is defined in the business
 * layer, provide a detailed description of the changes made.
 *
 * @author X2 Development Corporation
 */
public class IEPWorkflowInitialRefProcedureData extends WorkflowProcedure {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private static final String ALIAS_INITIAL_EVAL_CONSENT = "iep-init-par-consent-date";
    private static final String ALIAS_INITIAL_MEETING_DATE = "iep-init-meeting-date";
    private static final String ALIAS_IMPLEMENT_CONSENT_DATE = "iep-sped-consent-initial-iep";
    private static final String REFERRED_WAIVED_SERVICES = "Referred Waived Serv";
    private static final String REFERRED_NOT_IDENTIFIED = "Referred Not Ident";
    private static final String INELIGIBLE = "Ineligible";
    private static final String REFERRED = "Referred";

    private static final String REFERRAL_FORM_ID = "SPED-REF";
    private static final String ALIAS_PARENT_REFERRAL_DATE = "parent-referral-date";
    private static final String ALIAS_CASE_MANAGER = "Case Manager";

    private NewJerseySpedWorkflowBehavior m_behavior = null;

    /**
     * Constructs a new SpedReferralProcedure.
     *
     * @param definition WorkflowDefinition
     * @param district Organization
     * @param user User
     * @param broker X2Broker
     * @param locale Locale
     */
    public IEPWorkflowInitialRefProcedureData(WorkflowDefinition definition, Organization district,
            User user, X2Broker broker, Locale locale) {
        super(definition, district, user, broker, locale);
        m_behavior = (NewJerseySpedWorkflowBehavior) SpedWorkflowBehaviorFactory.getWorkflowBehavior(getOrganization());
    }

    /**
     * Use the executeIneligible to discard the iep and set student sped status to
     * be marked as not identified.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeDiscardNotIdentified(WorkflowProgress progress) throws Exception {
        boolean discardIep = true;
        executeIneligible(progress, discardIep, REFERRED_NOT_IDENTIFIED);
    }

    /**
     * Rolls back changes made by executeDiscardNotIdentified().
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackDiscardNotIdentified(WorkflowProgress progress) throws Exception {
        rollbackIneligible(progress);
    }

    /**
     * Execute ineligible.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeIneligible(WorkflowProgress progress) throws Exception {
        executeIneligible(progress, true, INELIGIBLE);
    }

    /**
     * Set the student's evaluation date, if requested.
     * Set the student's iep status to be either "Discarded" or "Rejected" as requested.
     * Set the student's sped status to be the specified code.
     *
     * @param progress WorkflowProgress
     * @param discardIep boolean
     * @param studentSpedStatus String
     * @throws Exception exception
     */
    public void executeIneligible(WorkflowProgress progress,
                                  boolean discardIep,
                                  String studentSpedStatus)
            throws Exception {
        // Get the iep bean
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner();

        // Set the iep status to be either discarded or rejected
        if (discardIep) {
            iep.setStatusCodeEnum(IepData.StatusCode.DISCARDED);
        } else {
            iep.setStatusCodeEnum(IepData.StatusCode.REJECTED);
        }
        // Save the iep bean
        broker.saveBeanForced(iep);

        // Set the student sped status to the requested value, then save bean
        SisStudent student = iep.getStudent();
        student.setSpedStatusCode(studentSpedStatus);

        broker.saveBeanForced(student);
    }

    /**
     * Rolls back changes made by the ineligible method.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackIneligible(WorkflowProgress progress) throws Exception {
        // Set the IEP status to DRAFT
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner();

        iep.setStatusCodeEnum(IepData.StatusCode.DRAFT);

        broker.saveBeanForced(iep);

        // Set the student sped status to Referred
        SisStudent student = iep.getStudent();

        student.setSpedStatusCode(REFERRED);

        broker.saveBeanForced(student);
    }

    /**
     * Use executeIneligible to discard iep and set student sped status to
     * be waived.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeDiscardWaived(WorkflowProgress progress) throws Exception {
        boolean discardIep = true;
        executeIneligible(progress, discardIep, REFERRED_WAIVED_SERVICES);
    }

    /**
     * Rolls back changes made by executeDiscardWaived().
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackDiscardWaived(WorkflowProgress progress) throws Exception {
        rollbackIneligible(progress);
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
     * Performs tasks associated with a successful IEP approval, setting the Initial Meeting Date,
     * Initial IEP Consent Date, and Meeting Date.
     *
     * @param progress WorkflowProgress
     */
    public void executeIepApproved(WorkflowProgress progress) {
        DataDictionary dictionary = getDataDictionary();

        IepData iep = (IepData) progress.getWorkflow().getOwner();
        iep.setFieldValueByAlias(ALIAS_IMPLEMENT_CONSENT_DATE, progress.getDate().toString(), dictionary);

        IepMeeting meeting = getMeetingFromProgress(progress);
        if (meeting != null) {
            iep.setFieldValueByAlias(ALIAS_INITIAL_MEETING_DATE, meeting.getDate().toString(), dictionary);
        }

        iep.setSignedDate(progress.getDate());

        getBroker().saveBeanForced(iep);
    }

    /**
     * Rolls back changes made when an IEP is approved.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackIepApproved(WorkflowProgress progress) throws Exception {
        DataDictionary dictionary = getDataDictionary();

        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner();

        iep.setFieldValueByAlias(ALIAS_IMPLEMENT_CONSENT_DATE, null, dictionary);
        iep.setFieldValueByAlias(ALIAS_INITIAL_MEETING_DATE, null, dictionary);
        iep.setSignedDate(null);

        broker.saveBeanForced(iep);
    }

    /**
     * Performs tasks associated with an IEP rejection.
     *
     * @param progress WorkflowProgress
     */
    public void executeIepRejected(WorkflowProgress progress) {
        m_behavior.executeIepRejected(progress, getBroker());
    }

    /**
     * Rolls back changes made when an IEP is rejected.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackIepRejected(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();

        IepData iep = (IepData) progress.getWorkflow().getOwner();
        iep.setStatusCodeEnum(StatusCode.DRAFT);

        broker.saveBeanForced(iep);
    }

    /**
     * Implements the draft IEP using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeImplementIep(WorkflowProgress progress) throws Exception {
        IepData iepOrg = (IepData) progress.getWorkflow().getOwner();
        PlainDate OrigNextEvaluationDate = iepOrg.getNextEvaluationDate();

        List<ValidationError> errors =
                m_behavior.executeImplementIep(progress, getOrganization(), getLocale(), getBroker());
        if (errors.size() > 0) {
            addValidationErrors(errors);
        } else {
            // Update the Case Manager Name on the Student when the IEP gets saved.
            IepData iep = (IepData) progress.getWorkflow().getOwner();

            iep.setNextEvaluationDate(OrigNextEvaluationDate);
            String staffName = iep.getStaff().getNameView();
            Student student = iep.getStudent();
            student.setFieldValueByAlias(ALIAS_CASE_MANAGER, staffName);

            getBroker().saveBeanForced(student);
        }
    }

    /**
     * Implements the draft IEP (with email notification) using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeImplementNotifyIep(WorkflowProgress progress) throws Exception {
        IepData iepOrg = (IepData) progress.getWorkflow().getOwner();
        PlainDate OrigNextEvaluationDate = iepOrg.getNextEvaluationDate();

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

            List<String> recp = buildRecipientsList((IepData) progress.getWorkflow().getOwner(), iep.getStudent());
            String message = getOrganization().getName() + " is sending this electronic notification to inform you:\n"
                    + "\n" + "The IEP belonging to " + student.getPerson().getFirstName() + " "
                    + student.getPerson().getLastName()
                    + " is now active. You are required to be familiar with the IEP and to assist the student in meeting the goals, including the implementation of any modifications specified in the student's IEP.\n"
                    + "\n" + "Please take a few minutes to review the active IEP Plan at your earliest convenience.\n"
                    + "\n" + "Thank you.";

            sendemail(recp, message);
            iep.setNextEvaluationDate(OrigNextEvaluationDate);
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
     * Rolls back changes made by the implement IEP method using the default sped workflow behavior.
     * During rollback, no notification is sent to the user.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackImplementNotifyIep(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackImplementIep(progress, getOrganization(), getLocale(), getBroker());

        IepData iep = (IepData) progress.getWorkflow().getOwner();

        iep.setStatusCodeEnum(StatusCode.DRAFT);

        getBroker().saveBeanForced(iep);
    }

    /**
     * Sets the IEP field for initial evaluation consent to the progress date.
     *
     * @param progress WorkflowProgress
     */
    public void executeReceiveConsent(WorkflowProgress progress) {
        m_behavior.executeReceiveConsent(progress, getOrganization(), getLocale(), getBroker());

        DataDictionary dictionary = getDataDictionary();

        IepData iep = (IepData) progress.getWorkflow().getOwner();

        iep.setFieldValueByAlias(ALIAS_INITIAL_EVAL_CONSENT, progress.getDate().toString(), dictionary);

        getBroker().saveBeanForced(iep);
    }

    /**
     * Rolls back changes made by executeInitialEvalConsent().
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackReceiveConsent(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackReceiveConsent(progress, getOrganization(), getBroker());

        DataDictionary dictionary = getDataDictionary();

        IepData iep = (IepData) progress.getWorkflow().getOwner();

        iep.setFieldValueByAlias(ALIAS_INITIAL_EVAL_CONSENT, null, dictionary);

        getBroker().saveBeanForced(iep);
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
     * Execute schedule eval meeting.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeScheduleEvalMeeting(WorkflowProgress progress) throws Exception {
        // no behavior necessary
    }

    /**
     * Rollback schedule eval meeting.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackScheduleEvalMeeting(WorkflowProgress progress) throws Exception {
        // no behavior necessary
    }

    /**
     * Execute schedule det meeting.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeScheduleDetMeeting(WorkflowProgress progress) throws Exception {
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
     * Execute schedule iep meeting.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeScheduleIepMeeting(WorkflowProgress progress) throws Exception {
        // no behavior necessary
    }

    /**
     * Rollback schedule iep meeting.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackScheduleIepMeeting(WorkflowProgress progress) throws Exception {
        // no behavior necessary
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

        FormInstance referralForm = formInstances.get(REFERRAL_FORM_ID);
        String referralDateString = (String) referralForm.getFormValueByAlias(ALIAS_PARENT_REFERRAL_DATE, getBroker());

        if (!StringUtils.isEmpty(referralDateString)) {
            SystemStringConverter converter = (SystemStringConverter) ConverterFactory
                    .getConverterForClass(Converter.DATE_CONVERTER, Locale.getDefault(), true);
            PlainDate referralDate = (PlainDate) converter.parseSystemString(referralDateString);
            iep.setReferralDate(referralDate);

            getBroker().saveBeanForced(iep);
        }
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
     * Helper method to retrieve the data dictionary.
     *
     * @return DataDictionary
     */
    private DataDictionary getDataDictionary() {
        ExtendedDataDictionary extendedDataDictionary = SpedUtils.getIepDictionary(getOrganization(), getBroker());
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());
        return dictionary;
    }

    /**
     * Returns the IepMeeting object from the passed workflow progress. If the progress
     * isn't attached to a meeting form, null is returned.
     *
     * @param progress WorkflowProgress
     * @return Iep meeting
     */
    private IepMeeting getMeetingFromProgress(WorkflowProgress progress) {
        IepMeeting meeting = null;

        FormInstance formInstance = progress.getFormInstance(getBroker());
        if (formInstance != null) {
            meeting = (IepMeeting) formInstance.getStorageObject(getBroker());
        }

        return meeting;
    }
    /*
     * Effect: Sends e-mail to people within recipient list
     */

    /**
     * Sendemail.
     *
     * @param staffemaillist List<String>
     * @param message String
     */
    private void sendemail(List<String> staffemaillist, String message) {
        WriteEmailManager manager =
                new WriteEmailManager(getOrganization(), getCurrentOwnerOid(), getCurrentOwnerType(), getUser());

        if (manager.connect()) {
            try {
                MessageProperties msg = new MessageProperties(null, null, staffemaillist, null, null, "IEP Alert",
                        message, "text/plain", null, null, null);
                manager.sendMail(msg);

            } finally {
                manager.disconnect();
            }
        }
    }

    /**
     * Collects a list of recipients to receive an e-mail notification of an IEP or ED Plan
     * acceptance, including all current teachers and IEP/Ed Plan team members (which presumably
     * include the student's primary and possibly secondary contacts).
     *
     * @param iep IepData
     * @param student SisStudent
     * @return List<String>
     */
    private List<String> buildRecipientsList(IepData iep, SisStudent student) {
        List<String> recipientsList = new ArrayList<String>();

        // Add teacher emails
        Collection<StudentSchedule> schedules = student.getStudentSchedules(getBroker());

        Collection<ScheduleTeacher> allStaff = null;

        for (StudentSchedule schedule : schedules) {
            if (schedule != null) {
                Schedule currentSchedule = schedule.getSchedule();
                if (currentSchedule != null
                        && schedule.getSection() != null
                        && currentSchedule.getDistrictContextOid().equals(getOrganization().getCurrentContextOid())) {
                    allStaff = schedule.getSection().getTeacherSections();

                    for (ScheduleTeacher allStaffPersons : allStaff) {
                        Person sPerson = allStaffPersons.getStaff().getPerson();
                        if (sPerson != null
                                && !StringUtils.isEmpty(sPerson.getEmail01())
                                && !recipientsList.contains(sPerson.getEmail01())) {
                            recipientsList.add(sPerson.getEmail01());
                        }
                    }
                }
            }
        }

        return recipientsList;
    }
}
