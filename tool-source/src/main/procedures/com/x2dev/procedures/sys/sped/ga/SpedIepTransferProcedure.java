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
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehavior;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehaviorFactory;
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
public class SpedIepTransferProcedure extends WorkflowProcedure {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private SpedWorkflowBehavior m_behavior = null;

    /**
     * Constructs a new SpedIepTransferProcedure.
     *
     * @param definition WorkflowDefinition
     * @param district Organization
     * @param user User
     * @param broker X2Broker
     * @param locale Locale
     */
    public SpedIepTransferProcedure(WorkflowDefinition definition, Organization district,
            User user, X2Broker broker, Locale locale) {
        super(definition, district, user, broker, locale);
        m_behavior = SpedWorkflowBehaviorFactory.getWorkflowBehavior(getOrganization());
    }

    /**
     * Sets start date and meeting date of IEP to be the "Completed On" date from the form.
     * Also set the "Next IEP Review Date" to be the end date.
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

        // set the "Next IEP Review Date" to be the End Date
        PlainDate endDate = iep.getEndDate();
        iep.setNextReviewDate(endDate);

        // set status code to 'pending approval'
        iep.setStatusCodeEnum(IepData.StatusCode.PENDING_APPROVAL);

        broker.saveBeanForced(iep);
    }

    /**
     * Implements the draft IEP using the default sped workflow behavior,
     * then if there are no validation errors, sets endDate to null, so that it is empty.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeImplementIep(WorkflowProgress progress) throws Exception {
        // TODO test again.

        List<ValidationError> errors =
                m_behavior.executeImplementIep(progress, getOrganization(), getLocale(), getBroker());
        if (errors.size() > 0) {
            addValidationErrors(errors);
        }

        else {
            X2Broker broker = getBroker();
            IepData iep = (IepData) progress.getWorkflow().getOwner();
            broker.saveBeanForced(iep);
        }
    }

    /**
     * Implements the draft IEP (with email notification) using the default sped workflow behavior,
     * then if there are no validation errors, sets endDate to null, so that it is empty.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeImplementNotifyIep(WorkflowProgress progress) throws Exception {
        List<ValidationError> errors =
                m_behavior.executeImplementIep(progress, getOrganization(), getLocale(), getBroker());
        if (errors.size() > 0) {
            addValidationErrors(errors);
        }

        else {
            X2Broker broker = getBroker();
            IepData iep = (IepData) progress.getWorkflow().getOwner();
            broker.saveBeanForced(iep);
        }
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
            ((IepData) owner).setMeetingTypeCodeEnum(IepMeeting.TypeCode.OTHER);
            broker.saveBeanForced(owner);
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
        SisStudent student = (SisStudent) selectionBean;
        X2Broker broker = getBroker();

        SisOrganization district = student.getSchool().getOrganization1();
        ExtendedDataDictionary iepDictionary = SpedUtils.getIepDictionary(district, broker);
        FormInstance transferForm = formInstances.get(SpedDefaultWorkflowBehavior.TRANSFER_FORM_DEFINITION_ID);

        IepData draftIep = new IepData(broker.getPersistenceKey());
        draftIep.setExtendedDataDictionaryOid(iepDictionary.getOid());
        draftIep.setStaffOid(
                (String) transferForm.getFormValueByAlias(SpedDefaultWorkflowBehavior.CASE_MANAGER_ALIAS, broker));
        draftIep.setStatusCodeEnum(StatusCode.DRAFT);
        draftIep.setStudentOid(student.getOid());

        // Set the meeting type code to "Other"
        draftIep.setMeetingTypeCode(IepMeeting.TypeCode.OTHER.ordinal());

        // Set the end date from the Transfer IEP (sped.transfer) form
        SystemStringConverter converter = (SystemStringConverter) ConverterFactory
                .getConverterForClass(Converter.DATE_CONVERTER, getLocale(), true);
        PlainDate iepEndDate = (PlainDate) converter.parseSystemString((String) transferForm
                .getFormValueByAlias(SpedDefaultWorkflowBehavior.TRANSFER_IEP_END_DATE_ALIAS, broker));
        draftIep.setEndDate(iepEndDate);

        broker.saveBeanForced(draftIep);

        String activeCode = PreferenceManager.getPreferenceValue(district, SisPreferenceConstants.SPED_ACTIVE_CODE);

        student.setSpedStatusCode(activeCode);
        broker.saveBeanForced(student);

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
     * Rolls back "Start Date" + "End Date" fields of IEP to null.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackFinalizeIep(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        iep.setStartDate(null);
        iep.setEndDate(null);
        broker.saveBeanForced(iep);
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
}
