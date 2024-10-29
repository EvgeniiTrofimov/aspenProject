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
package com.x2dev.procedures.sys.sped.bc;

import static com.follett.fsc.core.k12.business.BusinessRules.IMPLEMENT_IEP_DRAFT_REQUIRED;
import static com.follett.fsc.core.k12.business.ValidationConstants.BUSINESS_RULE_VIOLATION;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.Organization;
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
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisStudent.SpedTypeCode;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehavior;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehaviorFactory;
import com.x2dev.utils.types.PlainDate;
import java.util.Calendar;
import java.util.LinkedList;
import java.util.List;
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
public class SpedAmendmentProcedure extends WorkflowProcedure {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /*
     * SSP Constants
     */
    private static final String SPED_TYPE_ALIAS = "sped-type";
    private static final String SPED_TYPE_SSP = "SSP";

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
     * Changes student's sped status to <i>Active</i> and creates a designation alert.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeEligibleIep(WorkflowProgress progress) throws Exception {
        List<ValidationError> errors = new LinkedList<ValidationError>();

        boolean rollback = false;
        getBroker().beginTransaction();
        try {
            IepData draftIep = (IepData) progress.getWorkflow().getOwner();
            if (draftIep == null) {
                rollback = true;
                errors.add(new ValidationError(BUSINESS_RULE_VIOLATION, Integer.valueOf(IMPLEMENT_IEP_DRAFT_REQUIRED)));
            } else {
                SisStudent student = draftIep.getStudent();

                /*
                 * Change the student's special education status to active
                 */
                String studentActiveCode = PreferenceManager.getPreferenceValue(getOrganization(),
                        SisPreferenceConstants.SPED_ACTIVE_CODE);
                if (!studentActiveCode.equals(student.getSpedStatusCode())) {
                    student.setSpedStatusCode(studentActiveCode);
                    getBroker().saveBeanForced(student);
                }
            }
        } finally {
            if (rollback) {
                addValidationErrors(errors);
                getBroker().rollbackTransaction();
            } else {
                getBroker().commitTransaction();
            }
        }
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
     * Method executed when an IEP is implemented on a referral workflow.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeImplementIep(WorkflowProgress progress) throws Exception {
        addValidationErrors(implementPlan(progress, SpedTypeCode.IEP));
    }

    /**
     * Method executed when a SSP is implemented on a referral workflow.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeImplementSsp(WorkflowProgress progress) throws Exception {
        addValidationErrors(implementPlan(progress, SpedTypeCode.SSP));
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
     * Sets Student's sped type code to an SSP.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeReferSSP(WorkflowProgress progress) throws Exception {
        List<ValidationError> errors = new LinkedList<ValidationError>();
        IepData draftIep = (IepData) progress.getWorkflow().getOwner();

        if (draftIep == null) {
            errors.add(new ValidationError(BUSINESS_RULE_VIOLATION, Integer.valueOf(IMPLEMENT_IEP_DRAFT_REQUIRED)));
        } else {
            /*
             * Convert the IEP to a SSP by setting STD_SPED_TYPE field on student to SSP
             */
            SisStudent student = draftIep.getStudent();
            student.setSpedTypeCode(SisStudent.SpedTypeCode.SSP.ordinal());

            getBroker().saveBeanForced(student);

            SisOrganization district = student.getSchool().getOrganization1();
            ExtendedDataDictionary iepDictionary = SpedUtils.getIepDictionary(district, getBroker());
            DataDictionary dictionary =
                    DataDictionary.getDistrictDictionary(iepDictionary, getBroker().getPersistenceKey());
            draftIep.setFieldValueByAlias(SPED_TYPE_ALIAS, SPED_TYPE_SSP, dictionary);
            getBroker().saveBeanForced(draftIep);
        }

        addValidationErrors(errors);
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
     */
    @Override
    public X2BaseBean initializeOwner(X2BaseBean selectionBean,
                                      Map<String, FormInstance> formInstances,
                                      PlainDate date,
                                      WorkflowDefinition workflowDefinition) {
        LinkedList<ValidationError> errors = new LinkedList<ValidationError>();

        IepData owner = m_behavior.executeCreateAmendmentDraft((SisStudent) selectionBean, new PlainDate(getTimeZone()),
                errors, getBroker());

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
     * Rolls back changes made by the submit for approval method using the default sped workflow
     * behavior.
     *
     * @param progress WorkflowProgress
     */
    public void rollbackSubmitForApproval(WorkflowProgress progress) {
        addValidationErrors(m_behavior.rollbackSubmitForApproval(progress, getBroker()));
    }

    /**
     * Rolls back changes made by the implementIep method.
     *
     * @param progress WorkflowProgress
     */
    public void rollbackImplementIep(WorkflowProgress progress) {
        IepData activeIep = (IepData) progress.getWorkflow().getOwner();
        SisStudent student = activeIep.getStudent();

        /*
         * Set the status of the active IEP back to pending.
         */
        activeIep.setStatusCodeEnum(StatusCode.DRAFT);

        /*
         * Retrieve the previous IEP and change its status back to active; note that for the initial
         * referral workflow, a previous IEP will not exist.
         */
        IepData previousIep = student.getPreviousIep(getBroker());
        if (previousIep != null) {
            previousIep.setStatusCodeEnum(StatusCode.ACTIVE);
            getBroker().saveBeanForced(previousIep);
        }

        getBroker().saveBeanForced(activeIep);
    }

    /**
     * Rolls back changes made by the implementSsp method.
     *
     * @param progress WorkflowProgress
     */
    public void rollbackImplementSsp(WorkflowProgress progress) {
        /*
         * Same behavior as of rolling back implementing IEP.
         */
        rollbackImplementIep(progress);
    }

    /**
     * Rolls back changes made by the writeIep method.
     *
     * @param progress WorkflowProgress
     */
    public void rollbackWriteIep(WorkflowProgress progress) {
        /*
         * Just to rollback implementing IEP as it is the last step of the workflow.
         */
        rollbackImplementIep(progress);
    }

    /**
     * Rolls back changes made by the writeIep method.
     *
     * @param progress WorkflowProgress
     */
    public void rollbackWriteSsp(WorkflowProgress progress) {
        /*
         * Just to rollback implementing SSP as it is the last step of the workflow.
         */
        rollbackImplementSsp(progress);
    }

    /**
     * Implements the current plan (IEP or SSP), and sets the student's SPED type accordingly. This
     * type setting is
     * necessary for students transitioning between an IEP/SSP.
     *
     * @param progress WorkflowProgress
     * @param spedType SpedTypeCode
     * @return List<ValidationError>
     */
    private List<ValidationError> implementPlan(WorkflowProgress progress, SpedTypeCode spedType) {
        List<ValidationError> errors = new LinkedList<ValidationError>();
        IepData draftIep = (IepData) progress.getWorkflow().getOwner();
        if (draftIep == null) {
            errors.add(new ValidationError(BUSINESS_RULE_VIOLATION, Integer.valueOf(IMPLEMENT_IEP_DRAFT_REQUIRED)));
        } else {
            SisStudent student = draftIep.getStudent();
            student.setSpedTypeCode(spedType.ordinal());
            if (student.isSpedTypeCodeDirty()) {
                getBroker().saveBeanForced(student);
            }

            Calendar calendar = null;
            if (getLocale() != null) {
                calendar = Calendar.getInstance(getLocale());
            } else {
                calendar = Calendar.getInstance();
            }

            /*
             * Set the end date on the active IEP, and change its status to previous; note that for
             * the
             * initial referral workflow, an active IEP will not exist.
             */
            IepData activeIep = student.getActiveIep(getBroker());
            if (activeIep != null) {
                activeIep.setStatusCodeEnum(StatusCode.PREVIOUS);
                getBroker().saveBeanForced(activeIep);
            }

            if (isDateSettingEnabled(getOrganization())) {
                /*
                 * Set the start and end dates on the active IEP, and change its status to active. A
                 * draft
                 * IEP will exist for both workflows supported by this method.
                 */
                if (draftIep.getStartDate() == null) {
                    draftIep.setStartDate(progress.getDate());
                }

                calendar.setTime(draftIep.getStartDate());
                calendar.add(Calendar.YEAR, 1);

                if (draftIep.getEndDate() == null) {
                    draftIep.setEndDate(new PlainDate(calendar.getTime()));
                }
            }

            draftIep.setStatusCodeEnum(StatusCode.ACTIVE);
            getBroker().saveBeanForced(draftIep);
        }

        return errors;
    }

    /**
     * Returns true if dates should be set on the IEP automatically.
     *
     * @param district Organization
     * @return boolean
     */
    private boolean isDateSettingEnabled(Organization district) {
        return !Boolean.parseBoolean(PreferenceManager.getPreferenceValue(district,
                SisPreferenceConstants.SPED_DISABLE_WORKFLOW_DATE_DEFAULTS));
    }
}
