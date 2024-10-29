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

import static com.follett.fsc.core.k12.business.BusinessRules.IEP_RENEWAL_ELIGIBILITY;
import static com.follett.fsc.core.k12.business.BusinessRules.IMPLEMENT_IEP_DRAFT_REQUIRED;
import static com.follett.fsc.core.k12.business.ValidationConstants.BUSINESS_RULE_VIOLATION;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.BeanCopier;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.beans.IepGoal;
import com.x2dev.sis.model.beans.IepMeeting.TypeCode;
import com.x2dev.sis.model.beans.IepPerformanceLevel;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisStudent.SpedTypeCode;
import com.x2dev.sis.model.business.sped.MeetingAttendanceManager;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehavior;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehaviorFactory;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Defines special behavior that occurs on the Ministry Identification Review and IEP Review
 * workflows for BC.
 * <p>
 * By default, all behavior is defined in the business layer. Custom workflow behavior can be
 * defined directly in this procedure. When adding behavior beyond what is defined in the business
 * layer, provide a detailed description of the changes made.
 *
 * @author X2 Development Corporation
 */
public class SpedReviewProcedure extends WorkflowProcedure {
    /*
     * Form aliases
     */
    private static final String ALIAS_REVIEWED_BY = "reviewed-by";
    private static final String ALIAS_REVIEWED_DATE = "reviewed-date";
    private static final String ALIAS_SPED_TYPE = "sped-type";

    /*
     * Workflow phase OIDs
     */
    private static final String FINALIZE_IEP_PHASE_OID = "wphBCIEPFinIEP";
    private static final String FINALIZE_SSP_PHASE_OID = "wphBCIEPFinSSP";

    /*
     * Other constants
     */
    private static final String SPED_TYPE_IEP = "IEP";
    private static final String SPED_TYPE_SSP = "SSP";

    private static final long serialVersionUID = 1L;

    private SpedWorkflowBehavior m_behavior = null;

    /**
     * Constructs a new SpedReviewProcedure.
     *
     * @param definition WorkflowDefinition
     * @param district Organization
     * @param user User
     * @param broker X2Broker
     * @param locale Locale
     */
    public SpedReviewProcedure(WorkflowDefinition definition, Organization district,
            User user, X2Broker broker, Locale locale) {
        super(definition, district, user, broker, locale);
        m_behavior = SpedWorkflowBehaviorFactory.getWorkflowBehavior(getOrganization());
    }

    /**
     * Resolves the conditional branch on outcomes of <i>Amend IEP or Student Support Plan</i> and
     * <i>Continue Ministry Identification IEP or Student Support Plan</i> phases. Condition is to
     * skip the next phase and go to <i>Finalize IEP</i>, or <i>Finalize SSP</i> phase
     * depending on students' sped type.
     *
     * @param progress WorkflowProgress
     * @return WorkflowPhase
     */
    public WorkflowPhase branchFinalize(WorkflowProgress progress) {
        WorkflowPhase phase = null;

        IepData draftIep = (IepData) progress.getWorkflow().getOwner();
        if (draftIep != null) {
            SisStudent student = draftIep.getStudent();

            int spedTypeCode = student.getSpedTypeCode();
            if (spedTypeCode == SisStudent.SpedTypeCode.IEP.ordinal()) {
                phase = (WorkflowPhase) getBroker().getBeanByOid(WorkflowPhase.class, FINALIZE_IEP_PHASE_OID);
            } else if (spedTypeCode == SisStudent.SpedTypeCode.SSP.ordinal()) {
                phase = (WorkflowPhase) getBroker().getBeanByOid(WorkflowPhase.class, FINALIZE_SSP_PHASE_OID);
            }
        }

        /*
         * HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK
         * 
         * TODO: Remove with resolution to defect D-11787
         * 
         * Need to create a workflow progress record for the next standard phase and set the
         * appropriate outcome on
         * that to ensure the checklist is being properly displayed
         */
        WorkflowPhaseOutcome outcome = progress.getWorkflowPhaseOutcome();
        if (phase != null && outcome != null) {
            WorkflowPhase standardNextPhase = outcome.getNextWorkflowPhase();

            // Check for existing progress record
            Criteria criteria = new Criteria();
            criteria.addEqualTo(WorkflowProgress.COL_WORKFLOW_OID, progress.getWorkflowOid());
            criteria.addEqualTo(WorkflowProgress.COL_WORKFLOW_PHASE_OID, standardNextPhase.getOid());

            QueryByCriteria query = new QueryByCriteria(WorkflowProgress.class, criteria);
            WorkflowProgress nextProgress = (WorkflowProgress) getBroker().getBeanByQuery(query);

            /*
             * Need to create new progress record if it does not exist. BC's workflow design ensures
             * there will only
             * be one phase per workflow.
             */
            if (nextProgress == null) {
                String nextPhaseOutcomeOid = null;
                for (WorkflowPhaseOutcome nextPhaseOutcome : standardNextPhase.getWorkflowPhaseOutcomes(getBroker())) {
                    if (phase.getOid().equals(nextPhaseOutcome.getNextWorkflowPhaseOid())) {
                        nextPhaseOutcomeOid = nextPhaseOutcome.getOid();
                    }
                }

                nextProgress = X2BaseBean.newInstance(WorkflowProgress.class, getBroker().getPersistenceKey());
                nextProgress.setDate(getPlainDate());
                nextProgress.setTimestamp(System.currentTimeMillis());
                nextProgress.setUserOid(progress.getUserOid());
                nextProgress.setWorkflowOid(progress.getWorkflowOid());
                nextProgress.setWorkflowPhaseOid(standardNextPhase.getOid());
                nextProgress.setWorkflowPhaseOutcomeOid(nextPhaseOutcomeOid);

                getBroker().saveBeanForced(nextProgress);
            }
        }

        return phase;
    }

    /**
     * Changes sped type code on student to SSP.
     *
     * @param progress WorkflowProgress
     */
    public void executeChangeSpedType(WorkflowProgress progress) {
        IepData draftIep = (IepData) progress.getWorkflow().getOwner();
        if (draftIep != null) {
            /*
             * Convert the IEP to a SSP by setting STD_SPED_TYPE field on student to SSP
             */
            SisStudent student = draftIep.getStudent();
            student.setSpedTypeCode(SisStudent.SpedTypeCode.SSP.ordinal());

            if (student.isDirty()) {
                getBroker().saveBeanForced(student);
            }
        }
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
     * Execute method for "finalize" method ID.
     *
     * @param progress WorkflowProgress
     */
    public void executeFinalize(WorkflowProgress progress) {
        /*
         * This method is present to prevent an exception logged onto system logs
         * for java.lang.NoSuchMethodException
         */
    }

    /**
     * Method executed when an IEP is implemented.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeImplementIep(WorkflowProgress progress) throws Exception {
        addValidationErrors(implementPlan(progress, SpedTypeCode.IEP));
    }

    /**
     * Method executed when a SSP is implemented ministry identification, or review workflow. If an
     * <i>active</i> SSP exists for the student, its status is set to <i>previous</i>. The status of
     * the <i>draft</i> SSP is then set to <i>active</i>.
     * <p>
     * Using a separate method as a hook to add additional functionality in future.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeImplementSsp(WorkflowProgress progress) throws Exception {
        addValidationErrors(implementPlan(progress, SpedTypeCode.SSP));
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
            draftIep.setFieldValueByAlias(ALIAS_SPED_TYPE, SPED_TYPE_SSP, dictionary);
            getBroker().saveBeanForced(draftIep);
        }

        addValidationErrors(errors);
    }

    /**
     * Initialize form.
     *
     * @param formInstance FormInstance
     * @param formStorage X2BaseBean
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.FormInitializationProcedure#initializeForm(com.
     *      follett.fsc.core.k12.beans.FormInstance, com.follett.fsc.core.k12.beans.X2BaseBean,
     *      com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    public void initializeForm(FormInstance formInstance, X2BaseBean formStorage, UserDataContainer userData)
            throws X2BaseException {
        super.initializeForm(formInstance, formStorage, userData);

        if (formStorage instanceof GenericFormData) {
            GenericFormData formData = (GenericFormData) formStorage;
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(
                    formInstance.getFormDefinition().getExtendedDataDictionary(), getBroker().getPersistenceKey());

            String formDefinitionId = formInstance.getFormDefinition().getId();
            if ("FORM-F".equals(formDefinitionId) || "FORM-G".equals(formDefinitionId)) {
                GenericFormChildData childFormData =
                        X2BaseBean.newInstance(GenericFormChildData.class, getBroker().getPersistenceKey());

                String userNameView = userData.getUser().getNameView();
                if (userNameView != null && userNameView.length() > 50) {
                    userNameView = userNameView.substring(0, 50);
                }
                childFormData.setFieldValueByAlias(ALIAS_REVIEWED_BY, userNameView, dictionary);
                childFormData.setFieldValueByAlias(ALIAS_REVIEWED_DATE, new PlainDate(getTimeZone()).toString(),
                        dictionary);

                formData.addToGenericFormDataChildren(childFormData);
                getBroker().saveBeanForced(formData);
            }
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

        IepData owner = executeCopy((SisStudent) selectionBean, workflowDefinition, getLocale(), errors, getBroker());

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
     * Rolls back changes made by change sped type operation. Student's sped type
     * is changed back to IEP.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackChangeSpedType(WorkflowProgress progress) throws Exception {
        IepData draftIep = (IepData) progress.getWorkflow().getOwner();

        if (draftIep != null) {
            /*
             * Convert the SSP back to IEP on STD_SPED_TYPE
             */
            SisStudent student = draftIep.getStudent();
            student.setSpedTypeCode(SisStudent.SpedTypeCode.IEP.ordinal());

            if (student.isDirty()) {
                getBroker().saveBeanForced(student);
            }
        }
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
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackExit(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackExit(progress, getOrganization(), getBroker());
    }

    /**
     * Rolls back changes made by the implement IEP method using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
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
     * Rolls back changes made by implement SSP operation.
     * <p>
     * Using a separate method to add additional functionality in future.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackImplementSSP(WorkflowProgress progress) throws Exception {
        rollbackImplementIep(progress);
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
            draftIep.setFieldValueByAlias(ALIAS_SPED_TYPE, SPED_TYPE_IEP, dictionary);
            getBroker().saveBeanForced(draftIep);
        }
    }

    /**
     * Method executed when an IEP renewal workflow is initiated. This method creates a new draft
     * IEP. If specified by the user on the renewal form, the draft IEP is created by copying the
     * current active IEP. Otherwise a blank draft IEP is created.
     *
     * @param student SisStudent
     * @param workflowDefinition WorkflowDefinition
     * @param locale Locale
     * @param errors List<ValidationError>
     * @param broker X2Broker
     * @return IepData
     */
    private IepData executeCopy(SisStudent student,
                                WorkflowDefinition workflowDefinition,
                                Locale locale,
                                List<ValidationError> errors,
                                X2Broker broker) {
        SisOrganization organization = student.getSchool().getOrganization1();

        IepData activeIep = student.getActiveIep(broker);
        IepData draftIep = null;

        if (activeIep == null || student.getDraftIep(broker) != null) {
            errors.add(new ValidationError(BUSINESS_RULE_VIOLATION, Integer.valueOf(IEP_RENEWAL_ELIGIBILITY)));
        } else {
            Collection<String> relationshipIds = getIepRelationshipIdsToCopy(organization, broker);

            BeanCopier beanCopier = new BeanCopier(broker, true);
            draftIep = (IepData) beanCopier.copy(activeIep, relationshipIds);

            TypeCode meetingType = MeetingAttendanceManager.getMeetingType(workflowDefinition,
                    PreferenceManager.getPreferenceSet(organization));
            draftIep.setMeetingTypeCodeEnum(meetingType);
            draftIep.setStatusCodeEnum(StatusCode.DRAFT);
            draftIep.setAmendedIndicator(false);
            draftIep.setSignedDate(null);

            DataDictionary dictionary =
                    DataDictionary.getDistrictDictionary(SpedUtils.getIepDictionary(organization, getBroker()),
                            getBroker().getPersistenceKey());
            draftIep.setFieldValueByAlias(ALIAS_SPED_TYPE, SPED_TYPE_IEP, dictionary);

            broker.saveBeanForced(draftIep);

            FormDefinition iepFormDefinition = getFormDefinition(m_behavior.getIepFormDefinitionId(), broker);

            FormInstance iepForm = X2BaseBean.newInstance(FormInstance.class, broker.getPersistenceKey());
            iepForm.setFormDefinitionOid(iepFormDefinition.getOid());
            iepForm.setCreatedTime(System.currentTimeMillis());
            iepForm.setOwnerObjectOid(draftIep.getOid());
            iepForm.setStorageObjectOid(draftIep.getOid());

            broker.saveBeanForced(iepForm);
        }

        return draftIep;
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
     * Returns a collection of IEP relationship IDs to be copied over to another IEP.
     *
     * @param organization Organization
     * @param broker X2Broker
     * @return Collection&ltString&gt
     */
    private Collection<String> getIepRelationshipIdsToCopy(Organization organization, X2Broker broker) {
        ExtendedDataDictionary iepExtendedDictionary = SpedUtils.getIepDictionary(organization, broker);
        DataDictionary iepDictionary =
                DataDictionary.getDistrictDictionary(iepExtendedDictionary, broker.getPersistenceKey());

        Collection<String> relationshipIds = Arrays.asList(new String[] {

                iepDictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_ACCOMMODATIONS)
                        .getId(),
                iepDictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_DISABILITY)
                        .getId(),
                iepDictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_GOALS).getId(),

                iepDictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_GOALS).getId()
                        + "." +
                        iepDictionary.findDataDictionaryRelationship(IepGoal.class.getName(),
                                IepGoal.REL_IEP_GOAL_OBJECTIVES).getId(),

                iepDictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_PERFORMANCE_LEVEL)
                        .getId(),

                iepDictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_PERFORMANCE_LEVEL)
                        .getId() + "." +
                        iepDictionary.findDataDictionaryRelationship(IepPerformanceLevel.class.getName(),
                                IepPerformanceLevel.REL_IEP_PERFORMANCE_SOURCES).getId(),

                iepDictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_SERVICES).getId(),
                iepDictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_OTHER_SERVICES)
                        .getId(),
                iepDictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_TEAM_MEMBERS).getId()

        });

        return relationshipIds;
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

            student.setSpedTypeCode(spedType.ordinal());
            if (student.isSpedTypeCodeDirty()) {
                getBroker().saveBeanForced(student);
            }
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
