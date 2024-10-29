/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2014 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped.bc;

import static com.follett.fsc.core.k12.business.BusinessRules.IMPLEMENT_IEP_DRAFT_REQUIRED;
import static com.follett.fsc.core.k12.business.ValidationConstants.BUSINESS_RULE_VIOLATION;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.FormDefinition;
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
import com.x2dev.sis.model.beans.IepMeeting.TypeCode;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.MeetingAttendanceManager;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehavior;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehaviorFactory;
import com.x2dev.utils.types.PlainDate;
import java.util.Calendar;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Defines special behavior that occurs on the Ministry Identification workflow for BC.
 * <p>
 * By default, all behavior is defined in the business layer. Custom workflow behavior can be
 * defined directly in this procedure. When adding behavior beyond what is defined in the business
 * layer, provide a detailed description of the changes made.
 *
 * @author X2 Development Corporation
 */
public class SpedReferralProcedure extends WorkflowProcedure {
    private static final long serialVersionUID = 1L;

    private SpedWorkflowBehavior m_behavior = null;

    private static final String CASE_MANAGER_ALIAS = "staff-oid";
    private static final String REFERRAL_FORM_DEFINITION_ID = "FORM-A";
    private static final String SPED_TYPE_ALIAS = "sped-type";
    private static final String SPED_TYPE_IEP = "IEP";
    private static final String SPED_TYPE_SSP = "SSP";

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
                executeImplementIep(progress);
            }
        }
    }

    /**
     * Changes a student's status to exited using default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see
     *      com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior#executeExit(WorkflowProgress,
     *      Organization, X2Broker)
     */
    public void executeExit(WorkflowProgress progress) throws Exception {
        IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
        String exitCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.SPED_EXITED_CODE);

        m_behavior.executeExit(iep.getStudent(), progress.getDate(), exitCode, getBroker());
    }

    /**
     * Method executed when an IEP is implemented on a referral workflow.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeImplementIep(WorkflowProgress progress) throws Exception {
        List<ValidationError> errors = new LinkedList<ValidationError>();
        IepData draftIep = (IepData) progress.getWorkflow().getOwner();
        if (draftIep == null) {
            errors.add(new ValidationError(BUSINESS_RULE_VIOLATION, Integer.valueOf(IMPLEMENT_IEP_DRAFT_REQUIRED)));
        } else {
            SisStudent student = draftIep.getStudent();
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

        addValidationErrors(errors);
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
             * Convert the IEP to a SSP by setting STD_SPED_TYPE field on student to SSP and set the
             * student's
             * special education status to active
             */
            SisStudent student = draftIep.getStudent();
            student.setSpedTypeCode(SisStudent.SpedTypeCode.SSP.ordinal());
            student.setSpedStatusCode(
                    PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.SPED_ACTIVE_CODE));
            getBroker().saveBeanForced(student);

            SisOrganization district = student.getSchool().getOrganization1();
            ExtendedDataDictionary iepDictionary = SpedUtils.getIepDictionary(district, getBroker());
            DataDictionary dictionary =
                    DataDictionary.getDistrictDictionary(iepDictionary, getBroker().getPersistenceKey());
            draftIep.setFieldValueByAlias(SPED_TYPE_ALIAS, SPED_TYPE_SSP, dictionary);
            getBroker().saveBeanForced(draftIep);
        }

        addValidationErrors(errors);
        executeImplementIep(progress);
    }

    /**
     * Rolls back changes made by the eligible IEP operation. Student's status is changed
     * back to <i>Referred</i>.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackEligibleIep(WorkflowProgress progress) throws Exception {
        IepData draftIep = (IepData) progress.getWorkflow().getOwner();
        if (draftIep != null) {
            SisStudent student = draftIep.getStudent();
            String referredCode = PreferenceManager.getPreferenceValue(getOrganization(),
                    SisPreferenceConstants.SPED_REFERRED_CODE);
            if (!referredCode.equals(student.getSpedStatusCode())) {
                student.setSpedStatusCode(referredCode);
                getBroker().saveBeanForced(student);
            }
        }
    }

    /**
     * Rolls back changes made by the exit method using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see
     *      com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior#rollbackExit(WorkflowProgress,
     *      Organization, X2Broker)
     */
    public void rollbackExit(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackExit(progress, getOrganization(), getBroker());
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
     * Rolls back changes made by the refer SSP operation. Student's sped type is changed
     * back to IEP.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackReferSSP(WorkflowProgress progress) throws Exception {
        IepData draftIep = (IepData) progress.getWorkflow().getOwner();
        if (draftIep != null) {
            /*
             * Convert the SSP back to a IEP by setting STD_SPED_TYPE field on student to IEP
             */
            SisStudent student = draftIep.getStudent();
            student.setSpedTypeCode(SisStudent.SpedTypeCode.IEP.ordinal());

            getBroker().saveBeanForced(student);

            SisOrganization district = student.getSchool().getOrganization1();
            ExtendedDataDictionary iepDictionary = SpedUtils.getIepDictionary(district, getBroker());
            DataDictionary dictionary =
                    DataDictionary.getDistrictDictionary(iepDictionary, getBroker().getPersistenceKey());
            draftIep.setFieldValueByAlias(SPED_TYPE_ALIAS, SPED_TYPE_IEP, dictionary);
            getBroker().saveBeanForced(draftIep);
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

        SisOrganization district = student.getSchool().getOrganization1();

        ExtendedDataDictionary iepDictionary = SpedUtils.getIepDictionary(district, getBroker());

        FormInstance referralForm = formInstances.get(REFERRAL_FORM_DEFINITION_ID);

        IepData draftIep = new IepData(getBroker().getPersistenceKey());
        draftIep.setExtendedDataDictionaryOid(iepDictionary.getOid());

        // The referral form would be null if no values were entered on it
        if (referralForm != null) {
            draftIep.setStaffOid((String) referralForm.getFormValueByAlias(CASE_MANAGER_ALIAS, getBroker()));
        }

        draftIep.setStatusCodeEnum(StatusCode.DRAFT);
        draftIep.setStudentOid(student.getOid());

        TypeCode meetingType = MeetingAttendanceManager.getMeetingType(workflowDefinition,
                PreferenceManager.getPreferenceSet(district));
        draftIep.setMeetingTypeCodeEnum(meetingType);

        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(iepDictionary, getBroker().getPersistenceKey());
        draftIep.setFieldValueByAlias(SPED_TYPE_ALIAS, SPED_TYPE_IEP, dictionary);

        getBroker().saveBeanForced(draftIep);

        FormDefinition iepFormDefinition = getFormDefinition(m_behavior.getIepFormDefinitionId(), getBroker());

        FormInstance iepForm = X2BaseBean.newInstance(FormInstance.class, getBroker().getPersistenceKey());
        iepForm.setFormDefinitionOid(iepFormDefinition.getOid());
        iepForm.setCreatedTime(System.currentTimeMillis());
        iepForm.setOwnerObjectOid(draftIep.getOid());
        iepForm.setStorageObjectOid(draftIep.getOid());

        getBroker().saveBeanForced(iepForm);

        String referredCode = PreferenceManager.getPreferenceValue(district,
                SisPreferenceConstants.SPED_REFERRED_CODE);

        student.setSpedStatusCode(referredCode);
        student.setSpedTypeCode(SisStudent.SpedTypeCode.IEP.ordinal());

        if (isDateSettingEnabled(district)) {
            draftIep.setReferralDate(date);
        }

        getBroker().saveBeanForced(student);
        getBroker().saveBeanForced(draftIep);

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
     * Returns the form definition with the passed ID.
     *
     * @param id String
     * @param broker X2Broker
     * @return FormDefinition
     */
    private FormDefinition getFormDefinition(String id, X2Broker broker) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(FormDefinition.COL_ID, id);

        QueryByCriteria query = new QueryByCriteria(FormDefinition.class, criteria);

        return (FormDefinition) broker.getBeanByQuery(query);
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
