/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped.on;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepMeeting.TypeCode;
import com.x2dev.sis.model.beans.IepPlacement;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.MeetingAttendanceManager;
import com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehavior;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * @author Follett Software Company
 * @copyright 2019
 */
public class OnSpedWorkflowProcedure extends WorkflowProcedure {
    private static final String ALIAS_IPRC_MEMBER = "itm-iprc-member-indicator";
    private static final String FORM_TEAM_DEFINITION_FORM_ID = "ON-SPED-TEAM";
    private static final String FORM_TEAM_DEFINITION_IPRC_INSTANCE = "A";

    class OntarioSpedWorkflowBehavior extends SpedDefaultWorkflowBehavior implements SpedWorkflowBehavior {
        private static final String ALIAS_GFD_SPED_REF_STAFF_OID = "gfd-staff-oid";
        private static final String FORM_DEFINITION_IEP = "ON-SPED-IEP";
        private static final String FORM_DEFINITION_IEPA = "ON-SPED-IEPA";
        private static final String FORM_DEFINITION_MEETING = "ON-SPED-MTG";
        private static final String FORM_DEFINITION_REFERRAL = "ON-SPED-REF";

        private static final String ALIAS_IEP_COMPLETION_DATE = "iep-completion-date";
        private static final String ALIAS_IPRC_PLACEMENT = "iep-iprc-placement-decision";
        private static final String ALIAS_IPRC_PREV_PLACEMENT = "iep-iprc-prv-placement";
        private static final String ALIAS_IPRC_PREV_IDENTIFICATION = "iep-iprc-prv-identification";
        private static final String ALIAS_IMPLEMENTATION_SCHOOL = "iep-implementation-school";

        /**
         * Amending an IEP.
         *
         * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior#executeCreateAmendmentDraft(com.x2dev.sis.model.beans.SisStudent,
         *      com.x2dev.utils.types.PlainDate, java.util.List,
         *      com.follett.fsc.core.k12.business.X2Broker)
         */
        @Override
        public IepData executeCreateAmendmentDraft(SisStudent student,
                                                   PlainDate date,
                                                   List<ValidationError> errors,
                                                   X2Broker broker) {
            IepData amendmentIep = super.executeCreateAmendmentDraft(student, date, errors, broker);
            IepData activeIep = student.getActiveIep(broker);

            copyPreviousPlacement(activeIep, amendmentIep, broker, false);
            setImplementationSchool(amendmentIep, student);
            copyStudentDates(amendmentIep, student);
            if (amendmentIep.isDirty()) {
                broker.saveBeanForced(amendmentIep);
            }
            return amendmentIep;
        }

        /**
         * Implement the IEP at the end of the referral workflow.
         *
         * @param progress WorkflowProgress
         * @param district Organization
         * @param locale Locale
         * @param broker X2Broker
         * @return List of ValiationError objects
         *
         * @throws Exception exception
         *
         * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior#executeImplementIep(com.follett.fsc.core.k12.beans.WorkflowProgress,
         *      com.follett.fsc.core.k12.beans.Organization, java.util.Locale,
         *      com.follett.fsc.core.k12.business.X2Broker)
         */
        @Override
        public List<ValidationError> executeImplementIep(WorkflowProgress progress,
                                                         Organization district,
                                                         Locale locale,
                                                         X2Broker broker)
                throws Exception {
            // Capture the current active IEP.
            String activeIepOid = null;
            IepData draftIep = (IepData) progress.getWorkflow().getOwner();
            SisStudent student = draftIep.getStudent();
            IepData activeIep = student.getActiveIep(broker);
            if (activeIep != null) {
                activeIepOid = activeIep.getOid();
            }
            draftIep.setLastEvaluationDate(student.getSpedLastEvaluationDate());            

            List<ValidationError> errors = super.executeImplementIep(progress, district, locale, broker);

            if (errors.isEmpty()) {
                // Set the previously active IEP end date to the start of the new implemented IEP.
                activeIep = getBroker().getBeanByOid(IepData.class, activeIepOid);
                if (activeIep != null) {
                    // Set end date
                    PlainDate endDate = draftIep.getStartDate();
                    PlainDate oldEndDate = activeIep.getEndDate();
                    if (oldEndDate == null || oldEndDate.after(endDate)) {
                        activeIep.setEndDate(endDate);
                        if (activeIep.isDirty()) {
                            getBroker().saveBeanForced(activeIep);
                        }
                    }
                }

                // Fill in the completion date from the workflow progress.
                draftIep = (IepData) progress.getWorkflow().getOwner();
                DataDictionary dictionary = DataDictionary.getDistrictDictionary(draftIep.getExtendedDataDictionary(),
                        broker.getPersistenceKey());
                DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_IEP_COMPLETION_DATE);
                if (field != null) {
                    DateAsStringConverter converter = (DateAsStringConverter) ConverterFactory
                            .getConverterForClass(Converter.DATE_CONVERTER, locale, true);
                    String completionDate = (String) draftIep.getFieldValueByBeanPath(field.getJavaName());
                    if (StringUtils.isEmpty(completionDate)) {
                        String newDate = converter.getSystemString(progress.getDate());
                        draftIep.setFieldValueByBeanPath(field.getJavaName(), newDate);
                    }
                }
                setImplementationSchool(draftIep, draftIep.getStudent());
                if (draftIep.isDirty()) {
                    broker.saveBeanForced(draftIep);
                }
            }

            return errors;
        }

        /**
         * When implementing an amended IEP, set the IEP end date on the old IEP to the start date
         * of the new IEP. This ends exceptionalities and programs on the OLD IEP.
         *  
         * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior#executeImplementAmendedIep(com.follett.fsc.core.k12.beans.WorkflowProgress, com.follett.fsc.core.k12.beans.Organization, com.follett.fsc.core.k12.business.X2Broker)
         */
        @Override
        public List<ValidationError> executeImplementAmendedIep(WorkflowProgress progress,
                                                                Organization district,
                                                                X2Broker broker) {
            // Get the previous IEP for modification after.
            String activeIepOid = null;
            IepData draftIep = (IepData) progress.getWorkflow().getOwner();
            SisStudent student = draftIep.getStudent();
            IepData activeIep = student.getActiveIep(broker);
            if (activeIep != null) {
                activeIepOid = activeIep.getOid();
            }
            draftIep.setLastEvaluationDate(student.getSpedLastEvaluationDate());            

            List<ValidationError> errors = super.executeImplementAmendedIep(progress, district, broker);
            
            if (errors.isEmpty() && activeIepOid  != null) {
                draftIep = (IepData) progress.getWorkflow().getOwner();
                activeIep = getBroker().getBeanByOid(IepData.class, activeIepOid);
                if (activeIep != null) {
                    // Set end date
                    PlainDate endDate = draftIep.getStartDate();
                    PlainDate oldEndDate = activeIep.getEndDate();
                    if (oldEndDate == null || oldEndDate.after(endDate)) {
                        activeIep.setEndDate(endDate);
                        if (activeIep.isDirty()) {
                            getBroker().saveBeanForced(activeIep);
                        }
                    }
                }
                
                setImplementationSchool(draftIep, draftIep.getStudent());
                if (draftIep.isDirty()) {
                    broker.saveBeanForced(draftIep);
                }
            }
            
            return errors;
        }

        /**
         * Method executed when an IEP referral workflow is initiated. This method creates a new
         * draft
         * IEP.
         *
         * @param student SisStudent
         * @param workflowDefinition WorkflowDefinition
         * @param formInstances Map<String,FormInstance>
         * @param date PlainDate
         * @param broker X2Broker
         * @return IepData
         */
        @Override
        public IepData executeReferral(SisStudent student,
                                       WorkflowDefinition workflowDefinition,
                                       Map<String, FormInstance> formInstances,
                                       PlainDate date,
                                       X2Broker broker) {
            SisOrganization district = student.getSchool().getOrganization1();

            ExtendedDataDictionary iepDictionary = SpedUtils.getIepDictionary(district, broker);

            FormInstance referralForm = formInstances.get(getReferralFormDefinitionId());

            /* New instance, assign ddx OID and default values. */
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(iepDictionary, broker.getPersistenceKey());
            IepData draftIep = X2BaseBean.newInstance(IepData.class, dictionary);

            // The referral form would be null if no values were entered on it
            if (referralForm != null) {
                draftIep.setStaffOid((String) referralForm.getFormValueByAlias(ALIAS_GFD_SPED_REF_STAFF_OID, broker));
            }

            draftIep.setStatusCodeEnum(StatusCode.DRAFT);
            draftIep.setStudentOid(student.getOid());

            TypeCode meetingType = MeetingAttendanceManager.getMeetingType(workflowDefinition,
                    PreferenceManager.getPreferenceSet(district));
            draftIep.setMeetingTypeCodeEnum(meetingType);
            setImplementationSchool(draftIep, draftIep.getStudent());

            broker.saveBeanForced(draftIep);

            FormDefinition iepFormDefinition = getFormDefinition(getIepFormDefinitionId(), broker);

            FormInstance iepForm = X2BaseBean.newInstance(FormInstance.class, broker.getPersistenceKey());
            iepForm.setFormDefinitionOid(iepFormDefinition.getOid());
            iepForm.setCreatedTime(System.currentTimeMillis());
            iepForm.setOwnerObjectOid(draftIep.getOid());
            iepForm.setStorageObjectOid(draftIep.getOid());

            broker.saveBeanForced(iepForm);

            String referredCode = PreferenceManager.getPreferenceValue(district,
                    SisPreferenceConstants.SPED_REFERRED_CODE);

            student.setSpedStatusCode(referredCode);
            student.setSpedTypeCode(SisStudent.SpedTypeCode.IEP.ordinal());

            if (isDateSettingEnabled(district)) {
                draftIep.setReferralDate(date);
            }

            broker.saveBeanForced(student);
            broker.saveBeanForced(draftIep);

            return draftIep;
        }

        /**
         * Renewing an IEP.
         *
         * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior#executeRenewal(com.x2dev.sis.model.beans.SisStudent,
         *      com.follett.fsc.core.k12.beans.WorkflowDefinition, java.util.Map, java.util.Locale,
         *      java.util.List, com.follett.fsc.core.k12.business.X2Broker)
         */
        @Override
        public IepData executeRenewal(SisStudent student,
                                      WorkflowDefinition workflowDefinition,
                                      Map<String, FormInstance> formInstances,
                                      Locale locale,
                                      List<ValidationError> errors,
                                      X2Broker broker) {
            IepData draftIep = super.executeRenewal(student, workflowDefinition, formInstances, locale, errors, broker);
            
            draftIep.setMeetingTypeCode(IepMeeting.TypeCode.REVIEW.ordinal());
            setImplementationSchool(draftIep, draftIep.getStudent());
            copyStudentDates(draftIep, student);
            if (draftIep.isDirty()) {
                broker.saveBeanForced(draftIep);
            }

            if (errors == null || errors.isEmpty()) {
                IepData activeIep = student.getActiveIep(broker);
                copyPreviousPlacement(activeIep, draftIep, broker, true);
            }

            return draftIep;
        }

        @Override
        public IepData executeTransfer(SisStudent student,
                                       Map<String, FormInstance> formInstances,
                                       Locale locale,
                                       X2Broker broker) {
            IepData draftIep = super.executeTransfer(student, formInstances, locale, broker);

            setImplementationSchool(draftIep, draftIep.getStudent());
            if (draftIep.isDirty()) {
                broker.saveBeanForced(draftIep);
            }

            return draftIep;
        }

        /**
         * @see com.x2dev.sis.model.business.sped.SpedWorkflowBehavior#getAmendmentFormDefinitionId()
         */
        @Override
        public String getAmendmentFormDefinitionId() {
            return FORM_DEFINITION_IEPA;
        }

        /**
         * @see com.x2dev.sis.model.business.sped.SpedWorkflowBehavior#getIepFormDefinitionId()
         */
        @Override
        public String getIepFormDefinitionId() {
            return FORM_DEFINITION_IEP;
        }

        /**
         * @see com.x2dev.sis.model.business.sped.SpedWorkflowBehavior#getIepMeetingFormDefinitionId()
         */
        @Override
        public String getIepMeetingFormDefinitionId() {
            return FORM_DEFINITION_MEETING;
        }

        public String getReferralFormDefinitionId() {
            return FORM_DEFINITION_REFERRAL;
        }

        /**
         * Copy previous IPRC Identification and Placement to new draft IPRC.
         * Previous IEP Primary Disability description is copied to Previous-Identification field.
         * Previous IEP IPRC Placement field is copied to Previous-Placement field.
         *
         * @param activeIep
         * @param draftIep
         * @param broker
         * @param placements
         */
        private void copyPreviousPlacement(IepData activeIep, IepData draftIep, X2Broker broker, boolean placements) {
            if (activeIep != null) {
                // For IPRC, copy previous Placement into the "Previous" fields.
                SisOrganization district = activeIep.getStudent().getSchool().getOrganization1();
                ExtendedDataDictionary iepDictionary = SpedUtils.getIepDictionary(district, broker);
                DataDictionary dictionary =
                        DataDictionary.getDistrictDictionary(iepDictionary, broker.getPersistenceKey());

                String placement = (String) activeIep.getFieldValueByAlias(ALIAS_IPRC_PLACEMENT, dictionary);
                draftIep.setFieldValueByAlias(ALIAS_IPRC_PREV_PLACEMENT, placement, dictionary);

                // For IPRC, copy previous Identification (Disabilities) into the "Previous" fields.
                String identification = null;
                Collection<IepDisability> disabilities = activeIep.getIepDisability(broker);
                for (IepDisability idb : disabilities) {
                    identification = idb.getDisabilityCode();
                    if (idb.getPrimaryIndicator()) {
                        break;
                    }
                }
                if (!StringUtils.isEmpty(identification)) {
                    DataDictionaryField field = dictionary.findDataDictionaryField(IepDisability.class.getName(),
                            IepDisability.COL_DISABILITY_CODE);
                    if (field.hasReferenceTable()) {
                        X2Criteria criteria = new X2Criteria();
                        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
                        criteria.addEqualTo(ReferenceCode.COL_CODE, identification);
                        ReferenceCode rcd =
                                getBroker().getBeanByQuery(new QueryByCriteria(ReferenceCode.class, criteria));
                        if (rcd != null && !StringUtils.isEmpty(rcd.getDescription())) {
                            identification = rcd.getDescription();
                        }
                    }
                }

                // Copy IEP_PLACEMENT records, as those are not copied by the default behavior copy
                // method.
                if (placements) {
                    for (IepPlacement oldPlacement : activeIep.getPlacements(broker)) {
                        IepPlacement newPlacement = (IepPlacement) oldPlacement.copyBean();
                        newPlacement.setIepDataOid(draftIep.getOid());
                        broker.saveBean(newPlacement);
                    }
                }

                draftIep.setFieldValueByAlias(ALIAS_IPRC_PREV_IDENTIFICATION, identification, dictionary);
                broker.saveBeanForced(draftIep);
            }

        }

        /**
         * Copy Sped dates from the Student into the new draft IEP
         * if they are empty.
         * This will make the dates be present when the IEP is made Active later,
         * and copied back into the Student.
         *  
         * @param amendmentIep
         * @param student
         */
        private void copyStudentDates(IepData draftIep, SisStudent student) {
            if (draftIep.getReferralDate() == null) {
                draftIep.setReferralDate(student.getSpedReferralDate());
            }
            if (draftIep.getInitialEligibilityDate() == null) {
                draftIep.setInitialEligibilityDate(student.getSpedInitialEligibilityDate());
            }
            if (draftIep.getLastEligibilityDate() == null) {
                draftIep.setLastEligibilityDate(student.getSpedLastEligibilityDate());
            }
            if (draftIep.getLastEvaluationDate() == null) {
                draftIep.setLastEvaluationDate(student.getSpedLastEvaluationDate());
            }
            if (draftIep.getLastReviewDate() == null) {
                draftIep.setLastReviewDate(student.getSpedLastReviewDate());
            }
        }

        /**
         * Returns the form definition with the passed ID.
         *
         * @param id
         * @param broker
         *
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

        /**
         * @param amendmentIep
         * @param student
         */
        private void setImplementationSchool(IepData iep, SisStudent student) {
            String schoolOid = student.getSchoolOid();
            if (!StringUtils.isEmpty(schoolOid)) {
                ExtendedDictionaryAttributes iepDictionary = iep.getExtendedDataDictionary();
                DataDictionary dictionary =
                        DataDictionary.getDistrictDictionary(iepDictionary, iep.getPersistenceKey());
                iep.setFieldValueByAlias(ALIAS_IMPLEMENTATION_SCHOOL, schoolOid, dictionary);
            }
        }
    }

    protected SpedWorkflowBehavior m_behavior = null;

    /**
     * @param definition
     * @param organization
     * @param user
     * @param broker
     * @param locale
     */
    public OnSpedWorkflowProcedure(WorkflowDefinition definition, Organization organization, User user, X2Broker broker,
            Locale locale) {
        super(definition, organization, user, broker, locale);
        m_behavior = new OntarioSpedWorkflowBehavior();
    }

    /**
     * Student is Ineligible, change status IEP status to Discarded and Student to not eligible.
     *
     * @param progress
     *
     * @throws Exception
     */
    public void executeIneligible(WorkflowProgress progress) throws Exception {
        m_behavior.executeIneligible(progress, getOrganization(), getBroker());
    }

    /**
     * Student is eligible, change status IEP status to pending approval.
     *
     * @param progress
     *
     * @throws Exception
     */
    public void executeSubmitForApproval(WorkflowProgress progress) throws Exception {
        m_behavior.executeSubmitForApproval(progress, getBroker());
    }

    /**
     * Iep is rejected, change status IEP status to rejected.
     *
     * @param progress
     *
     * @throws Exception
     */
    public void executeIepRejected(WorkflowProgress progress) throws Exception {
        m_behavior.executeIepRejected(progress, getBroker());
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
     * Iep is accepted and implemented, set status to Active.
     *
     * @param progress
     *
     * @throws Exception
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
     * Rollback Student is Ineligible, change status IEP status to Discarded and Student to not
     * eligible.
     *
     * @param progress
     *
     * @throws Exception
     */
    public void rollbackIneligible(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackIneligible(progress, getOrganization(), getBroker());
    }

    /**
     * Rollback Student is eligible, change status IEP status to pending approval.
     *
     * @param progress
     *
     * @throws Exception
     */
    public void rollbackSubmitForApproval(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackSubmitForApproval(progress, getBroker());
    }

    /**
     * Rollback Iep is rejected, change status IEP status to rejected.
     *
     * @param progress
     *
     * @throws Exception
     */
    public void rollbackIepRejected(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackIepRejected(progress, getBroker());
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
     * Rollback Iep is accepted and implemented, set status to Active.
     *
     * @param progress
     *
     * @throws Exception
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
     * Validate form steps.
     *
     * @see com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#validateForm(com.follett.fsc.core.k12.beans.FormInstance,
     *      com.follett.fsc.core.k12.beans.X2BaseBean,
     *      com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    public List validateForm(FormInstance formInstance, X2BaseBean formStorage, UserDataContainer userData) {
        List<ValidationError> errors = new ArrayList<ValidationError>();
        return errors;
    }
}
