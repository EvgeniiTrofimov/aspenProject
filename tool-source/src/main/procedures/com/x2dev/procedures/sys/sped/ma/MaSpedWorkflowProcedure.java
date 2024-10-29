/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped.ma;

import static com.follett.fsc.core.k12.business.BusinessRules.IEP_AMENDMENT_ELIGIBILITY;
import static com.follett.fsc.core.k12.business.BusinessRules.IEP_RENEWAL_ELIGIBILITY;
import static com.follett.fsc.core.k12.business.ValidationConstants.BUSINESS_RULE_VIOLATION;
import static com.follett.fsc.core.k12.business.ValidationConstants.CUSTOM_ERROR;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.BeanCopier;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.ReferenceCodeRetriever;
import com.follett.fsc.core.k12.business.ValidationConstants;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.business.workflow.WorkflowManager;
import com.follett.fsc.core.k12.tools.procedures.SessionAwareProcedure;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebUtils;
import com.follett.fsc.core.k12.web.forms.FormDetail;
import com.follett.fsc.core.k12.web.workflow.SessionAccessManager;
import com.x2dev.procedures.sys.sped.ma.SpedMAWorkflowCommonProcedure.ExtendedSpedNotificationManager;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.beans.IepMeeting.TypeCode;
import com.x2dev.sis.model.business.sped.MassachusettsSpedWorkflowBehavior;
import com.x2dev.sis.model.business.sped.MeetingAttendanceManager;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.sis.web.workflow.SisOutcomeDetail;
import com.x2dev.utils.LoggerUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class MaSpedWorkflowProcedure.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class MaSpedWorkflowProcedure extends WorkflowProcedure implements SessionAwareProcedure {

    /**
     * The Class ExtendedMAWorkflowBehaviour.
     */
    class ExtendedMAWorkflowBehaviour extends MassachusettsSpedWorkflowBehavior {
        private static final String ALIAS_IEP_OID = "iep-oid";
        private static final String ALIAS_IEP_MAILED_TO_PARENT = "mailed-to-parent";
        private static final String ALIAS_IEP_PARENT_APPROVAL = "parent-approval";
        private static final String ALIAS_IEP_PARENT_REJECTED_PORTIONS = "parent-rejected-portions";
        private static final String ALIAS_IEP_PARENT_MEETING_REQUEST = "parent-meeting-request";
        private static final String ALIAS_IEP_PARENT_COMMENT = "parent-comment";
        private static final String ALIAS_PARTIAL_STATUS = "partial-status";
        private static final String ALIAS_REISSUE_TYPE = "reissue-type";
        private static final String ALIAS_REJECT_IN_PART_IEP = "reject-in-part-iep";
        private static final String ALIAS_TRANSITION_PLANNING_DATE = "trplan-date-completed";
        private static final String FORM_ID_TPF = "TPF";
        private static final String MA_RENEWAL_FORM_DEFINITION_ID = "SPED-REN-MA";
        public static final String TPF_FORM_INSTANCE_FORM_DEFINITION_ID = "fmdMaTrPlanNew";
        public static final String REISSUE_WORKFLOW_DEFINITION_ID = "SYS-SPED-REISSUE";

        private static final String REISSUE_TYPE_AMENDMENT = "Amendment";

        private static final String STATUS_PARTIAL = "Partial";

        private static final String WORKFLOW_ID_AMENDMENT = "SYS-SPED-AMEND";

        /**
         * Execute acc rej partial.
         *
         * @param progress WorkflowProgress
         * @param organization Organization
         * @param broker X2Broker
         * @throws Exception exception
         */
        public void executeAccRejPartial(WorkflowProgress progress,
                                         Organization organization,
                                         X2Broker broker)
                throws Exception {
            IepData iep = (IepData) progress.getWorkflow().getOwner(broker);
            ExtendedDataDictionary iepExtendedDictionary = SpedUtils.getIepDictionary(getOrganization(), broker);
            DataDictionary iepDictionary =
                    DataDictionary.getDistrictDictionary(iepExtendedDictionary, getBroker().getPersistenceKey());
            iep.setFieldValueByAlias(ALIAS_PARTIAL_STATUS, STATUS_PARTIAL, iepDictionary);
        }

        /**
         * Execute create amendment draft.
         *
         * @param student SisStudent
         * @param date PlainDate
         * @param errors List<ValidationError>
         * @param broker X2Broker
         * @return IepData
         * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior#executeCreateAmendmentDraft(com.x2dev.sis.model.beans.SisStudent,
         *      com.x2dev.utils.types.PlainDate, java.util.List,
         *      com.follett.fsc.core.k12.business.X2Broker)
         */
        @Override
        public IepData executeCreateAmendmentDraft(SisStudent student,
                                                   PlainDate date,
                                                   List<ValidationError> errors,
                                                   X2Broker broker) {
            IepData amendmentIep = null;
            IepData activeIep = student.getActiveIep(broker);
            SisOrganization district = student.getSchool().getOrganization1();

            if (activeIep == null || student.getAmendmentDraftIep(broker) != null) {
                errors.add(new ValidationError(BUSINESS_RULE_VIOLATION, Integer.valueOf(IEP_AMENDMENT_ELIGIBILITY)));
            } else {
                ExtendedDataDictionary iepExtendedDictionary = SpedUtils.getIepDictionary(district, broker);
                DataDictionary dictionary =
                        DataDictionary.getDistrictDictionary(iepExtendedDictionary, broker.getPersistenceKey());

                BeanCopier beanCopier = new BeanCopier(broker, true);
                amendmentIep = (IepData) beanCopier.copy(activeIep,
                        getAmendmentRelationships(dictionary),
                        getAmendmentIepValuesToSet(dictionary, broker));

                // fix the IepGoal OIDs in the copied IepGoalProgress beans
                // fixIepGoalProgressReferences(amendmentIep, activeIep, broker);

                IepAmendment amendment = new IepAmendment(broker.getPersistenceKey());
                amendment.setDate(date);
                amendment.setIepDataOid(amendmentIep.getOid());
                amendment.setStudentOid(amendmentIep.getStudentOid());
                broker.saveBeanForced(amendment);

                amendmentIep.setAmendedIndicator(true);
                amendmentIep.setStatusCodeEnum(StatusCode.AMENDMENT_DRAFT);
                amendmentIep.setIepAmendmentOid(amendment.getOid());

                // reset fields
                amendmentIep.setFieldValueByAlias(ALIAS_IEP_MAILED_TO_PARENT, null, dictionary);
                amendmentIep.setFieldValueByAlias(ALIAS_IEP_PARENT_APPROVAL, null, dictionary);
                amendmentIep.setFieldValueByAlias(ALIAS_IEP_PARENT_REJECTED_PORTIONS, null, dictionary);
                amendmentIep.setFieldValueByAlias(ALIAS_IEP_PARENT_MEETING_REQUEST, null, dictionary);
                amendmentIep.setFieldValueByAlias(ALIAS_IEP_PARENT_COMMENT, null, dictionary);

                broker.saveBeanForced(amendmentIep);

                // Create IEP form instance
                createIepForm(broker, amendmentIep);

                // Create IEP Amendment form instance
                FormDefinition amendmentFormDefinition = getFormDefinition(getAmendmentFormDefinitionId(), broker);
                if (amendmentFormDefinition != null) {
                    FormInstance amendmentForm = X2BaseBean.newInstance(FormInstance.class, broker.getPersistenceKey());
                    amendmentForm.setFormDefinitionOid(amendmentFormDefinition.getOid());
                    amendmentForm.setCreatedTime(System.currentTimeMillis());
                    amendmentForm.setOwnerObjectOid(amendmentIep.getOid());
                    amendmentForm.setStorageObjectOid(amendment.getOid());
                    broker.saveBeanForced(amendmentForm);
                }
                createAmendmentForms(activeIep, amendmentIep, broker);
            }

            return amendmentIep;
        }

        /**
         * Method executed when a student is found eligible. The student eligibility date field is
         * set.
         * For initial referral workflows, the initial eligibility date field is set.
         *
         * @param progress WorkflowProgress
         * @param district Organization
         * @param broker X2Broker
         * @return List of ValiationError objects
         */
        @Override
        public List<ValidationError> executeEligible(WorkflowProgress progress,
                                                     Organization district,
                                                     X2Broker broker) {
            List<ValidationError> errors = new ArrayList<ValidationError>(0);

            if (isDateSettingEnabledMA(district)) {
                IepData iep = (IepData) progress.getWorkflow().getOwner();
                PlainDate evalDate = progress.getDate();
                PlainDate dates[] = adjustIepDates(progress, iep, broker);
                if (dates != null) {
                    evalDate = dates[0];
                }

                iep.setLastEligibilityDate(evalDate);

                if (!isRenewalWorkflowMA(progress.getWorkflow().getWorkflowDefinition(), district)) {
                    iep.setInitialEligibilityDate(evalDate);
                }
                broker.saveBeanForced(iep);
            }

            return errors;
        }

        /**
         * Custom workflow method for Lexington.
         *
         * @param progress WorkflowProgress
         * @return List
         */
        public List<ValidationError> executeIepReissueRejected(WorkflowProgress progress) {
            List<ValidationError> errors = null;

            IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
            if (REISSUE_TYPE_AMENDMENT.equals(getReissueType(iep, getBroker()))) {
                errors = m_behavior.executeIepAmendmentRejected(progress, getBroker());
            } else {
                errors = m_behavior.executeIepRejected(progress, getBroker());
            }
            return errors;
        }

        /**
         * Custom workflow method for Lexington.
         *
         * @param progress WorkflowProgress
         * @return List
         */
        public List<ValidationError> executeIepRejectedReissue(WorkflowProgress progress) {
            List<ValidationError> errors = m_behavior.executeIepReissueRejected(progress);

            IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
            errors.addAll(createReissueDraft(progress, iep, getBroker()));
            return errors;
        }

        /**
         * Execute implement amended iep.
         *
         * @param progress WorkflowProgress
         * @param district Organization
         * @param broker X2Broker
         * @return List
         * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior#executeImplementAmendedIep(com.follett.fsc.core.k12.beans.WorkflowProgress,
         *      com.follett.fsc.core.k12.beans.Organization,
         *      com.follett.fsc.core.k12.business.X2Broker)
         */
        @Override
        public List<ValidationError> executeImplementAmendedIep(WorkflowProgress progress,
                                                                Organization district,
                                                                X2Broker broker) {
            List<ValidationError> errors = super.executeImplementAmendedIep(progress, district, broker);
            IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
            WorkflowDefinition workflowDef = progress.getWorkflow().getWorkflowDefinition();
            errors.addAll(validateDiscontinuedRefcodes(iep));
            if (errors.isEmpty()) {
                if (STATUS_PARTIAL.equals(getPartialStatus(iep, broker))) {
                    errors.addAll(createReissueDraft(progress, iep, broker));
                }
            }
            return errors;
        }

        /**
         * Method executed when a meeting is held on a referral or IEP renewal workflow. The
         * meeting date on the IEP is set if it has not been set already. It is set to the date of
         * the
         * IEP meeting instance in the attached form, if one exists.
         *
         * @param progress WorkflowProgress
         * @param district Organization
         * @param broker X2Broker
         * @return List of ValiationError objects
         */
        @Override
        public List<ValidationError> executeHoldMeeting(WorkflowProgress progress,
                                                        Organization district,
                                                        X2Broker broker) {
            List<ValidationError> errors = new ArrayList<ValidationError>(0);

            if (isDateSettingEnabledMA(district)) {
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

                    Calendar calendar = Calendar.getInstance();
                    calendar.setTime(meetingDate);
                    calendar.add(Calendar.YEAR, 1);
                    calendar.add(Calendar.DAY_OF_YEAR, -1);

                    iep.setLastReviewDate(meetingDate);
                    iep.setNextReviewDate(new PlainDate(calendar.getTimeInMillis()));

                    boolean isEvalWorkflow = isEvalWorkflowMA(progress.getWorkflow().getWorkflowDefinition(), district);
                    if (isEvalWorkflow) {
                        calendar = Calendar.getInstance();
                        calendar.setTime(meetingDate);
                        calendar.add(Calendar.YEAR, 3);
                        calendar.add(Calendar.DAY_OF_YEAR, -1);

                        iep.setLastEvaluationDate(meetingDate);
                        iep.setNextEvaluationDate(new PlainDate(calendar.getTimeInMillis()));
                    }

                    broker.saveBeanForced(iep);
                }
            }

            return errors;
        }

        /**
         * Execute implement iep.
         *
         * @param progress WorkflowProgress
         * @param district Organization
         * @param locale Locale
         * @param broker X2Broker
         * @return List
         * @throws Exception exception
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
            List<ValidationError> errors = super.executeImplementIep(progress, district, locale, broker);
            IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
            WorkflowDefinition workflowDef = progress.getWorkflow().getWorkflowDefinition();
            if (isRenewalWorkflowMA(workflowDef, district)) {
                errors.addAll(validateDiscontinuedRefcodes(iep));
            }
            if (errors.isEmpty()) {
                if (STATUS_PARTIAL.equals(getPartialStatus(iep, broker))) {
                    errors.addAll(createReissueDraft(progress, iep, broker));
                }
            }
            return errors;
        }

        /**
         * Execute implement notify iep.
         *
         * @param progress WorkflowProgress
         * @param district Organization
         * @param locale Locale
         * @param broker X2Broker
         * @return List
         * @throws Exception exception
         * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior#executeImplementNotifyIep(com.follett.fsc.core.k12.beans.WorkflowProgress,
         *      com.follett.fsc.core.k12.beans.Organization, java.util.Locale,
         *      com.follett.fsc.core.k12.business.X2Broker)
         */
        @Override
        public List<ValidationError> executeImplementNotifyIep(WorkflowProgress progress,
                                                               Organization district,
                                                               Locale locale,
                                                               X2Broker broker)
                throws Exception {
            List<ValidationError> errors =
                    m_behavior.executeImplementIep(progress, getOrganization(), getLocale(), getBroker());

            IepData implementedIep = (IepData) progress.getWorkflow().getOwner();
            ExtendedSpedNotificationManager notificationManager =
                    m_riWorkflowHelper.new ExtendedSpedNotificationManager(implementedIep);
            notificationManager.sendEmailNotification();
            return errors;
        }

        /**
         * Execute implement notify reissue iep.
         *
         * @param progress WorkflowProgress
         * @param district Organization
         * @param locale Locale
         * @param broker X2Broker
         * @return List
         * @throws Exception exception
         */
        public List<ValidationError> executeImplementNotifyReissueIep(WorkflowProgress progress,
                                                                      Organization district,
                                                                      Locale locale,
                                                                      X2Broker broker)
                throws Exception {
            List<ValidationError> errors = null;
            IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
            if (REISSUE_TYPE_AMENDMENT.equals(getReissueType(iep, getBroker()))) {
                errors = m_behavior.executeImplementNotifyAmendedIep(progress, district, broker);
            } else {
                errors = m_behavior.executeImplementNotifyIep(progress, district, locale, broker);
            }
            return errors;
        }

        /**
         * Execute implement reissue iep.
         *
         * @param progress WorkflowProgress
         * @param district Organization
         * @param locale Locale
         * @param broker X2Broker
         * @return List
         * @throws Exception exception
         */
        public List<ValidationError> executeImplementReissueIep(WorkflowProgress progress,
                                                                Organization district,
                                                                Locale locale,
                                                                X2Broker broker)
                throws Exception {
            List<ValidationError> errors = null;
            IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
            if (REISSUE_TYPE_AMENDMENT.equals(getReissueType(iep, getBroker()))) {
                errors = m_behavior.executeImplementAmendedIep(progress, district, broker);
            } else {
                errors = m_behavior.executeImplementIep(progress, district, locale, broker);
            }
            return errors;
        }

        /**
         * Execute renewal.
         *
         * @param student SisStudent
         * @param workflowDefinition WorkflowDefinition
         * @param formInstances Map<String,FormInstance>
         * @param locale Locale
         * @param errors List<ValidationError>
         * @param broker X2Broker
         * @return IepData
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
            SisOrganization district = student.getSchool().getOrganization1();

            FormInstance renewalForm = formInstances.get(MA_RENEWAL_FORM_DEFINITION_ID);
            String selectedIepOid = (String) renewalForm.getFormValueByAlias(ALIAS_IEP_OID, broker);
            IepData selectedIep = (IepData) broker.getBeanByOid(IepData.class, selectedIepOid);
            IepData draftIep = student.getActiveIep(broker); // active iep only returned on error
                                                             // condition

            if (student.getActiveIep(broker) == null || student.getDraftIep(broker) != null) {
                errors.add(new ValidationError(BUSINESS_RULE_VIOLATION, Integer.valueOf(IEP_RENEWAL_ELIGIBILITY)));
            } else {
                ExtendedDataDictionary iepExtendedDictionary = SpedUtils.getIepDictionary(district, broker);
                DataDictionary iepDictionary =
                        DataDictionary.getDistrictDictionary(iepExtendedDictionary, broker.getPersistenceKey());
                if (selectedIep != null) {
                    BeanCopier beanCopier = new BeanCopier(broker, true);
                    draftIep = (IepData) beanCopier.copy(selectedIep,
                            getRenewalRelationships(iepDictionary),
                            getRenewalIepValuesToSet(renewalForm, iepDictionary, broker));
                } else {
                    draftIep = X2BaseBean.newInstance(IepData.class, iepDictionary);
                    draftIep.setStaffOid((String) renewalForm.getFormValueByAlias(CASE_MANAGER_ALIAS, broker));
                    draftIep.setStatusCodeEnum(StatusCode.DRAFT);
                    draftIep.setStudentOid(student.getOid());
                }

                TypeCode meetingType = MeetingAttendanceManager.getMeetingType(workflowDefinition,
                        PreferenceManager.getPreferenceSet(district));
                draftIep.setMeetingTypeCodeEnum(meetingType);
                draftIep.setAmendedIndicator(false);
                draftIep.setSignedDate(null);

                // reset fields
                draftIep.setFieldValueByAlias(ALIAS_IEP_MAILED_TO_PARENT, null, iepDictionary);
                draftIep.setFieldValueByAlias(ALIAS_IEP_PARENT_APPROVAL, null, iepDictionary);
                draftIep.setFieldValueByAlias(ALIAS_IEP_PARENT_REJECTED_PORTIONS, null, iepDictionary);
                draftIep.setFieldValueByAlias(ALIAS_IEP_PARENT_MEETING_REQUEST, null, iepDictionary);
                draftIep.setFieldValueByAlias(ALIAS_IEP_PARENT_COMMENT, null, iepDictionary);


                broker.saveBeanForced(draftIep);

                // Create IEP form instance
                createIepForm(broker, draftIep);

                DataDictionaryField fieldTransitionPlanningDate =
                        iepDictionary.findDataDictionaryFieldByAlias(ALIAS_TRANSITION_PLANNING_DATE);
                if (fieldTransitionPlanningDate != null) {
                    String transitionPlanningDate =
                            (String) draftIep.getFieldValueByBeanPath(fieldTransitionPlanningDate.getJavaName());
                    FormDefinition tpfFormDefinition = getFormDefinition(FORM_ID_TPF, broker);
                    if (!StringUtils.isEmpty(transitionPlanningDate) &&
                            tpfFormDefinition != null &&
                            !StringUtils.isEmpty(tpfFormDefinition.getOid())) {
                        FormInstance tpfForm = X2BaseBean.newInstance(FormInstance.class, broker.getPersistenceKey());
                        tpfForm.setFormDefinitionOid(tpfFormDefinition.getOid());
                        tpfForm.setCreatedTime(System.currentTimeMillis());
                        tpfForm.setOwnerObjectOid(draftIep.getOid());
                        tpfForm.setStorageObjectOid(draftIep.getOid());
                        tpfForm.setOwnerView(draftIep.getFirstIdentifyingValue());

                        broker.saveBeanForced(tpfForm);
                    }
                }
            }

            return draftIep;
        }

        /**
         * submit IEP for approval.
         * Implement the default behavior, and also calculate end, review and reevaluation dates if
         * they are empty.
         *
         * @param progress
         * @param district
         * @param locale
         * @param broker
         * @return
         */
        public List<ValidationError> executeSubmitForApproval(WorkflowProgress progress,
                                                              Organization district,
                                                              Locale locale,
                                                              X2Broker broker) {
            List<ValidationError> errors = super.executeSubmitForApproval(progress, getBroker());

            if (isDateSettingEnabledMA(district)) {
                IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
                PlainDate dates[] = adjustIepDates(progress, iep, broker);
                if (dates != null) {
                    if (iep.getStartDate() == null) {
                        iep.setStartDate(dates[0]);
                    }
                    if (iep.getEndDate() == null) {
                        iep.setEndDate(dates[1]);
                    }
                    if (iep.getNextReviewDate() == null) {
                        iep.setNextReviewDate(dates[1]);
                    }
                    if (isEvalWorkflowMA(progress.getWorkflow().getWorkflowDefinition(), district)) {
                        if (iep.getNextEvaluationDate() == null) {
                            iep.setNextEvaluationDate(dates[2]);
                        }
                    }

                    if (iep.isDirty()) {
                        broker.saveBeanForced(iep);
                    }
                }
            }

            return errors;
        }

        /**
         * Execute transfer.
         *
         * @param student SisStudent
         * @param formInstances Map<String,FormInstance>
         * @param locale Locale
         * @param broker X2Broker
         * @return IepData
         * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior#executeTransfer(com.x2dev.sis.model.beans.SisStudent,
         *      java.util.Map, java.util.Locale, com.follett.fsc.core.k12.business.X2Broker)
         */
        @Override
        public IepData executeTransfer(SisStudent student,
                                       Map<String, FormInstance> formInstances,
                                       Locale locale,
                                       X2Broker broker) {
            SisOrganization district = student.getSchool().getOrganization1();

            ExtendedDataDictionary iepDictionary = SpedUtils.getIepDictionary(district, broker);

            FormInstance transferForm = formInstances.get(TRANSFER_FORM_DEFINITION_ID);

            DataDictionary dictionary = DataDictionary.getDistrictDictionary(iepDictionary, broker.getPersistenceKey());
            IepData draftIep = X2BaseBean.newInstance(IepData.class, dictionary);
            draftIep.setStaffOid((String) transferForm.getFormValueByAlias(CASE_MANAGER_ALIAS, broker));
            draftIep.setStatusCodeEnum(StatusCode.DRAFT);
            draftIep.setStudentOid(student.getOid());

            SystemStringConverter converter =
                    (SystemStringConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, locale,
                            true);

            draftIep.setMeetingTypeCode(IepMeeting.TypeCode.INITIAL.ordinal());
            String meetingTypeCode = (String) transferForm.getFormValueByAlias(TRANSFER_IEP_MEETING_TYPE_ALIAS, broker);
            if (!StringUtils.isEmpty(meetingTypeCode)) {
                try {
                    draftIep.setMeetingTypeCode(Integer.parseInt(meetingTypeCode));
                } catch (NumberFormatException e) {
                    // ignore format error
                }
            }


            PlainDate iepStartDate = (PlainDate) converter
                    .parseSystemString(
                            (String) transferForm.getFormValueByAlias(TRANSFER_IEP_START_DATE_ALIAS, broker));
            draftIep.setStartDate(iepStartDate);

            PlainDate iepEndDate = (PlainDate) converter
                    .parseSystemString((String) transferForm.getFormValueByAlias(TRANSFER_IEP_END_DATE_ALIAS, broker));
            draftIep.setEndDate(iepEndDate);

            String meetingDate = (String) transferForm.getFormValueByAlias(TRANSFER_IEP_MEETING_DATE_ALIAS, broker);
            if (!StringUtils.isEmpty(meetingDate)) {
                PlainDate iepMeetingDate = (PlainDate) converter.parseSystemString(meetingDate);
                draftIep.setMeetingDate(iepMeetingDate);
            }

            broker.saveBeanForced(draftIep);

            // Create IEP form instance
            createIepForm(broker, draftIep);

            String activeCode = PreferenceManager.getPreferenceValue(district,
                    SisPreferenceConstants.SPED_ACTIVE_CODE);

            student.setSpedStatusCode(activeCode);
            broker.saveBeanForced(student);

            return draftIep;
        }

        /**
         * Rollback acc rej partial.
         *
         * @param progress WorkflowProgress
         * @throws Exception exception
         */
        public void rollbackAccRejPartial(WorkflowProgress progress) throws Exception {
            IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
            ExtendedDataDictionary iepExtendedDictionary = SpedUtils.getIepDictionary(getOrganization(), getBroker());
            DataDictionary iepDictionary =
                    DataDictionary.getDistrictDictionary(iepExtendedDictionary, getBroker().getPersistenceKey());
            iep.setFieldValueByAlias(ALIAS_PARTIAL_STATUS, "", iepDictionary);
        }

        /**
         * Rollback iep reissue rejected.
         *
         * @param progress WorkflowProgress
         * @throws Exception exception
         */
        public void rollbackIepReissueRejected(WorkflowProgress progress) throws Exception {
            IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
            if (REISSUE_TYPE_AMENDMENT.equals(getReissueType(iep, getBroker()))) {
                m_behavior.rollbackIepAmendmentRejected(progress, getBroker());
            } else {
                m_behavior.rollbackIepRejected(progress, getBroker());
            }
        }

        /**
         * Rollback acc rej partial.
         *
         * @param progress WorkflowProgress
         * @throws Exception exception
         */
        public void rollbackIepRejectedReissue(WorkflowProgress progress) throws Exception {
            rollbackIepReissueRejected(progress);
            IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
            deleteReissueDraft(iep);
        }

        /**
         * Rollback implement amended iep.
         *
         * @param progress WorkflowProgress
         * @param district Organization
         * @param broker X2Broker
         * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior#rollbackImplementAmendedIep(com.follett.fsc.core.k12.beans.WorkflowProgress,
         *      com.follett.fsc.core.k12.beans.Organization,
         *      com.follett.fsc.core.k12.business.X2Broker)
         */
        @Override
        public void rollbackImplementAmendedIep(WorkflowProgress progress, Organization district, X2Broker broker) {
            super.rollbackImplementAmendedIep(progress, district, broker);
            IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
            if (STATUS_PARTIAL.equals(getPartialStatus(iep, broker))) {
                deleteReissueDraft(iep);
            }
        }

        /**
         * Rollback implement iep.
         *
         * @param progress WorkflowProgress
         * @param district Organization
         * @param locale Locale
         * @param broker X2Broker
         * @throws Exception exception
         * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior#rollbackImplementIep(com.follett.fsc.core.k12.beans.WorkflowProgress,
         *      com.follett.fsc.core.k12.beans.Organization, java.util.Locale,
         *      com.follett.fsc.core.k12.business.X2Broker)
         */
        @Override
        public void rollbackImplementIep(WorkflowProgress progress,
                                         Organization district,
                                         Locale locale,
                                         X2Broker broker)
                throws Exception {
            super.rollbackImplementIep(progress, district, locale, broker);
            IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
            if (STATUS_PARTIAL.equals(getPartialStatus(iep, broker))) {
                deleteReissueDraft(iep);
            }
        }

        /**
         * Rollback implement reissue iep.
         *
         * @param progress WorkflowProgress
         * @param district Organization
         * @param locale Locale
         * @param broker X2Broker
         * @throws Exception exception
         */
        public void rollbackImplementReissueIep(WorkflowProgress progress,
                                                Organization district,
                                                Locale locale,
                                                X2Broker broker)
                throws Exception {
            IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
            if (REISSUE_TYPE_AMENDMENT.equals(getReissueType(iep, getBroker()))) {
                m_behavior.rollbackImplementAmendedIep(progress, district, broker);
            } else {
                m_behavior.rollbackImplementIep(progress, district, locale, broker);
            }
        }

        /**
         * Returns true if dates should be set on the IEP automatically.
         *
         * @param district Organization
         * @return boolean
         */
        protected boolean isDateSettingEnabledMA(Organization district) {
            return !Boolean.parseBoolean(PreferenceManager.getPreferenceValue(district,
                    SisPreferenceConstants.SPED_DISABLE_WORKFLOW_DATE_DEFAULTS));
        }

        /**
         * Returns true if the passed workflow is an IEP evaluation workflow. Evaluation occurs
         * during
         * the referral and re-evaluation workflows.
         *
         * @param workflowDefinition WorkflowDefinition
         * @param district Organization
         * @return boolean
         */
        protected boolean isEvalWorkflowMA(WorkflowDefinition workflowDefinition, Organization district) {
            String referralId =
                    PreferenceManager.getPreferenceValue(district, SisPreferenceConstants.SPED_WORKFLOW_REFERRAL);
            boolean isEval = referralId.equals(workflowDefinition.getId());

            if (!isEval) {
                String reevalId =
                        PreferenceManager.getPreferenceValue(district, SisPreferenceConstants.SPED_WORKFLOW_REEVAL);
                isEval = reevalId.equals(workflowDefinition.getId());
            }

            return isEval;
        }

        /**
         * Returns true if the passed workflow is an IEP renewal workflow. Re-evaluation and renewal
         * workflows both fall under this category.
         *
         * @param workflowDefinition WorkflowDefinition
         * @param district Organization
         * @return boolean
         */
        protected boolean isRenewalWorkflowMA(WorkflowDefinition workflowDefinition, Organization district) {
            String renewalId =
                    PreferenceManager.getPreferenceValue(district, SisPreferenceConstants.SPED_WORKFLOW_RENEWAL);
            boolean isRenewal = renewalId.equals(workflowDefinition.getId());

            if (!isRenewal) {
                String reevalId =
                        PreferenceManager.getPreferenceValue(district, SisPreferenceConstants.SPED_WORKFLOW_REEVAL);
                isRenewal = reevalId.equals(workflowDefinition.getId());
            }

            return isRenewal;
        }

        /**
         * Generate new iep end date and next review and re-evaluation dates based on
         * the last meeting date in the workflow.
         * result[0] Last meeting date (N3) in the workflow.
         * result[1] Next review is last meeting date plus one year minus one day.
         * result[2] Next re-evaluation is the last meeting date plus three years minus one day.
         *
         * @param progress
         * @param iep
         * @param broker
         *
         * @return PlainDate result[3]
         */
        private PlainDate[] adjustIepDates(WorkflowProgress progress,
                                           IepData iep,
                                           X2Broker broker) {
            PlainDate results[] = null;
            PlainDate meetingDate = null;

            // Find the latest meeting date
            Workflow workflow = progress.getWorkflow();
            Collection<WorkflowProgress> progresses = workflow.getWorkflowProgress();
            for (WorkflowProgress wfprogress : progresses) {
                Collection<WorkflowProgressForm> forms = wfprogress.getWorkflowProgressForms();
                for (WorkflowProgressForm form : forms) {
                    FormInstance instance = form.getFormInstance();
                    if (instance != null) {
                        FormDefinition definition = instance.getFormDefinition();
                        // check for a meeting form.
                        if (getIepMeetingFormDefinitionId().equals(definition.getId())) {
                            X2BaseBean bean = instance.getStorageObject();
                            if (bean instanceof IepMeeting) {
                                IepMeeting meeting = (IepMeeting) bean;
                                if (meetingDate == null ||
                                        (meeting.getDate() != null && meeting.getDate().after(meetingDate))) {
                                    meetingDate = meeting.getDate();
                                }
                            }
                        }
                    }
                }
            }
            // If we cannot find a meeting date, use the IPE Start date.
            if (meetingDate == null) {
                meetingDate = iep.getStartDate();
            }
            // Calculate review and re-eval dates.
            if (meetingDate != null) {
                Calendar calendar = Calendar.getInstance();
                calendar.setTime(meetingDate);
                calendar.add(Calendar.YEAR, 1);
                calendar.add(Calendar.DAY_OF_YEAR, -1);
                PlainDate nextReviewDate = new PlainDate(calendar.getTimeInMillis());
                calendar = Calendar.getInstance();
                calendar.setTime(meetingDate);
                calendar.add(Calendar.YEAR, 3);
                calendar.add(Calendar.DAY_OF_YEAR, -1);
                PlainDate nextReevalDate = new PlainDate(calendar.getTimeInMillis());
                results = new PlainDate[3];
                results[0] = meetingDate;
                results[1] = nextReviewDate;
                results[2] = nextReevalDate;
            }
            return results;
        }

        /**
         * Creates the amendment forms.
         *
         * @param activeIep IepData
         * @param newIep IepData
         * @param broker X2Broker
         */
        private void createAmendmentForms(IepData activeIep, IepData newIep, X2Broker broker) {
            // Create IEP Meeting form instances
            FormDefinition meetingFormDefinition = getFormDefinition(getIepMeetingFormDefinitionId(), broker);
            if (meetingFormDefinition != null) {
                Collection<IepMeeting> meetings = newIep.getIepMeeting(broker);
                for (IepMeeting meeting : meetings) {
                    // Get form for old version of this meeting
                    FormInstance oldForm = null;

                    Criteria criteria = new Criteria();
                    criteria.addEqualTo(IepMeeting.COL_DATE, meeting.getDate());
                    criteria.addEqualTo(IepMeeting.COL_TYPE_CODE, Integer.valueOf(meeting.getTypeCode()));
                    criteria.addEqualTo(IepMeeting.COL_IEP_DATA_OID, activeIep.getOid());
                    QueryByCriteria oldMeetingQuery = new QueryByCriteria(IepMeeting.class, criteria);
                    IepMeeting oldMeeting = broker.getBeanByQuery(oldMeetingQuery);

                    if (oldMeeting != null) {
                        criteria = new Criteria();
                        criteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, activeIep.getOid());
                        criteria.addEqualTo(FormInstance.COL_STORAGE_OBJECT_OID, oldMeeting.getOid());
                        QueryByCriteria oldFormQuery = new QueryByCriteria(FormInstance.class, criteria);
                        oldForm = broker.getBeanByQuery(oldFormQuery);
                    }

                    FormInstance meetingForm =
                            X2BaseBean.newInstance(FormInstance.class, broker.getPersistenceKey());
                    meetingForm.setFormDefinitionOid(meetingFormDefinition.getOid());
                    meetingForm.setCreatedTime(
                            oldForm == null ? System.currentTimeMillis() : oldForm.getCreatedTime());
                    meetingForm.setOwnerObjectOid(newIep.getOid());
                    meetingForm.setStorageObjectOid(meeting.getOid());
                    broker.saveBeanForced(meetingForm);
                }
            }

            // Create IEP Placement form instances
            Criteria criteria = new Criteria();
            criteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, activeIep.getOid());
            criteria.addLike(FormInstance.COL_STORAGE_OBJECT_OID, IepPlacement.OBJECT_PREFIX + "%");
            QueryByCriteria query = new QueryByCriteria(FormInstance.class, criteria);
            Collection<FormInstance> relatedForms = broker.getCollectionByQuery(query);

            for (FormInstance form : relatedForms) {
                FormInstance placementForm = X2BaseBean.newInstance(FormInstance.class, broker.getPersistenceKey());
                placementForm.setFormDefinitionOid(form.getFormDefinitionOid());
                placementForm.setCreatedTime(form.getCreatedTime());
                placementForm.setOwnerObjectOid(newIep.getOid());

                // Query for the new copied version of the old placement record
                Criteria placementCriteria = new Criteria();
                IepPlacement oldPlacement = (IepPlacement) form.getStorageObject();
                placementCriteria.addEqualTo(IepPlacement.COL_IEP_PLACEMENT_PROGRAM_OID,
                        oldPlacement.getIepPlacementProgramOid());
                placementCriteria.addEqualTo(IepPlacement.COL_IEP_DATA_OID, newIep.getOid());
                placementCriteria.addEqualTo(IepPlacement.COL_START_DATE, oldPlacement.getStartDate());
                QueryByCriteria newPlacementQuery = new QueryByCriteria(IepPlacement.class, placementCriteria);

                IepPlacement placement = (IepPlacement) broker.getBeanByQuery(newPlacementQuery);
                if (placement != null) {
                    placementForm.setStorageObjectOid(placement.getOid());
                    broker.saveBeanForced(placementForm);
                }
            }

        }

        /**
         * Creates the iep form.
         *
         * @param broker X2Broker
         * @param iep IepData
         */
        private void createIepForm(X2Broker broker, IepData iep) {
            FormDefinition iepFormDefinition = getFormDefinition(getIepFormDefinitionId(), broker);

            FormInstance iepForm = X2BaseBean.newInstance(FormInstance.class, broker.getPersistenceKey());
            iepForm.setFormDefinitionOid(iepFormDefinition.getOid());
            iepForm.setCreatedTime(System.currentTimeMillis());
            iepForm.setOwnerObjectOid(iep.getOid());
            iepForm.setStorageObjectOid(iep.getOid());

            broker.saveBeanForced(iepForm);
        }

        /**
         * Creates the reissue draft.
         *
         * @param iep IepData
         * @param broker X2Broker
         * @return List
         */
        private List<ValidationError> createReissueDraft(WorkflowProgress progress, IepData iep, X2Broker broker) {
            List<ValidationError> errors = new LinkedList();
            ExtendedDataDictionary iepExtendedDictionary = SpedUtils.getIepDictionary(getOrganization(), broker);
            DataDictionary iepDictionary =
                    DataDictionary.getDistrictDictionary(iepExtendedDictionary, getBroker().getPersistenceKey());

            BeanCopier beanCopier = new BeanCopier(broker, true);
            HashMap<ModelProperty, Object> valuesToSet = getAmendmentIepValuesToSet(iepDictionary, broker);
            valuesToSet.put(new ModelProperty(IepData.class, IepData.COL_STATUS_CODE, broker.getPersistenceKey()),
                    Integer.valueOf(StatusCode.DRAFT.ordinal()));
            IepData draftIep = (IepData) beanCopier.copy(iep,
                    getAmendmentRelationships(iepDictionary),
                    valuesToSet);
            String reissueType = getReissueType(iep, getBroker());
            if (StringUtils.isEmpty(reissueType)
                    && WORKFLOW_ID_AMENDMENT.equals(progress.getWorkflow().getWorkflowDefinition().getId())) {
                draftIep.setFieldValueByAlias(ALIAS_REISSUE_TYPE, REISSUE_TYPE_AMENDMENT, iepDictionary);
            } else {
                draftIep.setFieldValueByAlias(ALIAS_REISSUE_TYPE, reissueType, iepDictionary);
            }
            draftIep.setFieldValueByAlias(ALIAS_PARTIAL_STATUS, "", iepDictionary);
            getBroker().saveBean(draftIep);

            /*
             * Set a temporary status of "Partial" on a user-defined field
             */
            iep.setFieldValueByAlias(ALIAS_REJECT_IN_PART_IEP, draftIep.getOid(), iepDictionary);
            getBroker().saveBean(iep);

            /*
             * Create an "IEP Form" for the draft IEP
             */
            createIepForm(broker, draftIep);

            /*
             * Create amendment forms
             */
            createAmendmentForms(iep, draftIep, broker);

            /*
             * Initiate a new "Rejection Draft" workflow
             */
            WorkflowDefinition rejectionDraft = getReissueDraftWorkflowDefinition();
            if (rejectionDraft != null) {
                WorkflowManager manager = new WorkflowManager(getBroker());
                SessionAccessManager accessManager = new SessionAccessManager(m_userData, getBroker());
                WorkflowPhaseOutcome outcome = rejectionDraft.getFirstWorkflowPhase().getStandardPhaseOutcome();

                try {
                    manager.initiateWorkflow(draftIep.getOid(), outcome, null, m_userData, new PlainDate(getTimeZone()),
                            false, getLocale(), accessManager);
                    if (!manager.getValidationErrors().isEmpty()) {
                        errors.addAll(manager.getValidationErrors());
                    }
                } catch (X2BaseException ex) {
                    // These would already be logged onto system logs
                }
            }
            return errors;
        }

        /**
         * Delete reissue draft.
         *
         * @param iep IepData
         */
        private void deleteReissueDraft(IepData iep) {
            ExtendedDataDictionary iepExtendedDictionary = SpedUtils.getIepDictionary(getOrganization(), getBroker());
            DataDictionary iepDictionary =
                    DataDictionary.getDistrictDictionary(iepExtendedDictionary, getBroker().getPersistenceKey());
            /*
             * Delete "Rejection Draft" iep
             */
            IepData rejectInPartIep = getBroker().getBeanByOid(IepData.class,
                    (String) iep.getFieldValueByAliasExtended(ALIAS_REJECT_IN_PART_IEP, iepDictionary));
            if (rejectInPartIep != null) {
                WorkflowDefinition rejectionDraft = getReissueDraftWorkflowDefinition();
                if (rejectionDraft != null) {
                    Criteria criteria = new Criteria();
                    criteria.addEqualTo(Workflow.COL_WORKFLOW_DEFINITION_OID, rejectionDraft.getOid());
                    criteria.addEqualTo(Workflow.COL_OWNER_OID, rejectInPartIep.getOid());

                    QueryByCriteria query = new QueryByCriteria(Workflow.class, criteria);

                    getBroker().deleteByQuery(query);
                }
                getBroker().deleteBean(rejectInPartIep);
            }

            /*
             * Blank out "Partial" field
             */
            iep.setFieldValueByAlias(ALIAS_REJECT_IN_PART_IEP, "", iepDictionary);
            getBroker().saveBean(iep);
        }

        /**
         * Gets the amendment relationships.
         *
         * @param dictionary DataDictionary
         * @return Collection
         */
        private Collection<String> getAmendmentRelationships(DataDictionary dictionary) {
            return Arrays.asList(new String[] {

                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_ACCOMMODATIONS)
                            .getId(),
                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_AMENDMENTS)
                            .getId(),
                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_DISABILITY)
                            .getId(),
                    dictionary
                            .findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_GOAL_PROGRESS)
                            .getId(),
                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_GOALS)
                            .getId(),

                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_GOALS)
                            .getId() + "." +
                            dictionary.findDataDictionaryRelationship(IepGoal.class.getName(),
                                    IepGoal.REL_IEP_GOAL_OBJECTIVES).getId(),

                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_GOALS)
                            .getId() + "." +
                            dictionary.findDataDictionaryRelationship(IepGoal.class.getName(),
                                    IepGoal.REL_IEP_GOAL_PROGRESS).getId(),

                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_MEETING)
                            .getId(),

                    dictionary
                            .findDataDictionaryRelationship(IepData.class.getName(),
                                    IepData.REL_IEP_PERFORMANCE_LEVEL)
                            .getId(),

                    dictionary
                            .findDataDictionaryRelationship(IepData.class.getName(),
                                    IepData.REL_IEP_PERFORMANCE_LEVEL)
                            .getId() + "." +
                            dictionary.findDataDictionaryRelationship(IepPerformanceLevel.class.getName(),
                                    IepPerformanceLevel.REL_IEP_PERFORMANCE_SOURCES).getId(),

                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_SERVICES)
                            .getId(),
                    dictionary
                            .findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_OTHER_SERVICES)
                            .getId(),

                    dictionary
                            .findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_SERVICES)
                            .getId() + "." +
                            dictionary.findDataDictionaryRelationship(IepService.class.getName(),
                                    IepService.REL_IEP_SERVICE_GOAL_ALIGNMENTS).getId(),
                    dictionary
                            .findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_GOALS)
                            .getId() + "." +
                            dictionary.findDataDictionaryRelationship(IepGoal.class.getName(),
                                    IepGoal.REL_IEP_SERVICE_GOAL_ALIGNMENTS).getId(),

                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_TEAM_MEMBERS)
                            .getId(),
                    dictionary
                            .findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_MEETING_ATTENDANCE)
                            .getId(),

                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_MEETING)
                            .getId()
                            + "." +
                            dictionary.findDataDictionaryRelationship(IepMeeting.class.getName(),
                                    IepMeeting.REL_MEETING_ATTENDANCE).getId(),

                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_TEAM_MEMBERS)
                            .getId()
                            + "." +
                            dictionary.findDataDictionaryRelationship(IepTeamMember.class.getName(),
                                    IepTeamMember.REL_MEETING_ATTENDANCE).getId(),

                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_AMENDMENTS)
                            .getId(),
                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_PLACEMENTS)
                            .getId(),

                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_AMENDMENTS)
                            .getId() + "." +
                            dictionary.findDataDictionaryRelationship(IepAmendment.class.getName(),
                                    IepAmendment.REL_IEP_AMENDMENT_DETAILS).getId(),

            });
        }

        /**
         * Gets the form definition.
         *
         * @param id String
         * @param broker X2Broker
         * @return Form definition
         */
        private FormDefinition getFormDefinition(String id, X2Broker broker) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(FormDefinition.COL_ID, id);

            QueryByCriteria query = new QueryByCriteria(FormDefinition.class, criteria);

            return (FormDefinition) broker.getBeanByQuery(query);
        }

        /**
         * Gets the partial status.
         *
         * @param iep IepData
         * @param broker X2Broker
         * @return Object
         */
        private Object getPartialStatus(IepData iep, X2Broker broker) {
            ExtendedDataDictionary iepExtendedDictionary = SpedUtils.getIepDictionary(getOrganization(), broker);
            DataDictionary iepDictionary =
                    DataDictionary.getDistrictDictionary(iepExtendedDictionary, getBroker().getPersistenceKey());
            String partialStatus = (String) iep.getFieldValueByAlias(ALIAS_PARTIAL_STATUS, iepDictionary);
            return partialStatus;
        }

        /**
         * Gets the rejection draft workflow definition.
         *
         * @return Workflow definition
         */
        private WorkflowDefinition getReissueDraftWorkflowDefinition() {
            WorkflowDefinition rejectionDraft = null;

            Criteria criteria = new Criteria();
            criteria.addEqualTo(WorkflowDefinition.COL_ID, REISSUE_WORKFLOW_DEFINITION_ID);

            QueryByCriteria query = new QueryByCriteria(WorkflowDefinition.class, criteria);

            rejectionDraft = (WorkflowDefinition) getBroker().getBeanByQuery(query);

            return rejectionDraft;
        }

        /**
         * Gets the reissue type.
         *
         * @param iep IepData
         * @param broker X2Broker
         * @return Object
         */
        private String getReissueType(IepData iep, X2Broker broker) {
            ExtendedDataDictionary iepExtendedDictionary = SpedUtils.getIepDictionary(getOrganization(), broker);
            DataDictionary iepDictionary =
                    DataDictionary.getDistrictDictionary(iepExtendedDictionary, getBroker().getPersistenceKey());
            String reissueType = (String) iep.getFieldValueByAlias(ALIAS_REISSUE_TYPE, iepDictionary);
            return reissueType;
        }

        /**
         * Gets the renewal relationships.
         *
         * @param dictionary DataDictionary
         * @return Collection
         */
        private Collection<String> getRenewalRelationships(DataDictionary dictionary) {
            return Arrays.asList(new String[] {

                    dictionary
                            .findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_ACCOMMODATIONS)
                            .getId(),
                    dictionary
                            .findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_DISABILITY)
                            .getId(),
                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_GOALS)
                            .getId(),

                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_GOALS)
                            .getId() + "." +
                            dictionary.findDataDictionaryRelationship(IepGoal.class.getName(),
                                    IepGoal.REL_IEP_GOAL_OBJECTIVES).getId(),

                    dictionary.findDataDictionaryRelationship(IepData.class.getName(),
                            IepData.REL_IEP_PERFORMANCE_LEVEL).getId(),

                    dictionary.findDataDictionaryRelationship(IepData.class.getName(),
                            IepData.REL_IEP_PERFORMANCE_LEVEL).getId() + "." +
                            dictionary.findDataDictionaryRelationship(IepPerformanceLevel.class.getName(),
                                    IepPerformanceLevel.REL_IEP_PERFORMANCE_SOURCES).getId(),

                    dictionary
                            .findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_SERVICES)
                            .getId() + "." +
                            dictionary.findDataDictionaryRelationship(IepService.class.getName(),
                                    IepService.REL_IEP_SERVICE_GOAL_ALIGNMENTS).getId(),
                    dictionary
                            .findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_GOALS)
                            .getId() + "." +
                            dictionary.findDataDictionaryRelationship(IepGoal.class.getName(),
                                    IepGoal.REL_IEP_SERVICE_GOAL_ALIGNMENTS).getId(),

                    dictionary
                            .findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_SERVICES)
                            .getId(),

                    dictionary
                            .findDataDictionaryRelationship(IepData.class.getName(),
                                    IepData.REL_IEP_OTHER_SERVICES)
                            .getId(),
                    dictionary
                            .findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_TEAM_MEMBERS)
                            .getId()
            });
        }

    }



    public static final String ALIAS_COMPLETE_FORMS = "ora-ma-sped-mult-forms";
    public static final String ALIAS_REQ_FIELDS = "ora-ma-sped-form-req-fields";
    public static final String ALIAS_SKIP_FORMS = "ora-ma-sped-skip-forms";

    private static final String MA_PL135_FORM_DEFINITION_ID = "PL1 (3-5)";
    private static final String MA_PL1621_FORM_DEFINITION_ID = "PL1 (6-21)";
    private static final String MA_PARENT_PLAEMENT_FORM_DEFINITION_ID = "IEP-PARENT-PLACEMENT";

    public static final String DEFAULT_COMPLETE_FORMS = BooleanAsStringConverter.TRUE;
    public static final String DEFAULT_REQ_FIELDS = "PL2 (3-5){a:educational-environment-ec}" + "\n"
            + "PL2 (6-21){a:educational-environment}" + "\n" + "PL3{a:level-of-need}";
    public static final String DEFAULT_SKIP_FORMS = "28M/10, SLD1-4;SLD-OBS";

    public static final List<String> ALIAS_NAMES =
            Arrays.asList(ALIAS_COMPLETE_FORMS, ALIAS_REQ_FIELDS, ALIAS_SKIP_FORMS);
    public static final List<String> ALIAS_DEFAULTS =
            Arrays.asList(DEFAULT_COMPLETE_FORMS, DEFAULT_REQ_FIELDS, DEFAULT_SKIP_FORMS);

    public static final String DDX_ORA_ID = "ORA-MA-SPED-CFIG";

    /**
     * Aliases
     */
    private static final String ALIAS_CONSENT_RECEIVED = "asm-consent-date";

    private static final String LOG_INVALID_METHOD = "BIZ-00040";

    /**
     * Gets the sped config bean. If the bean cannot be found, create a default bean.
     *
     * @param broker X2Broker
     * @return Organization attributes
     */
    public static OrganizationAttributes getSpedConfig(X2Broker broker) {
        OrganizationAttributes bean = null;

        ExtendedDataDictionary ddx = getSpedConfigDictionary(broker);
        if (ddx != null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(OrganizationAttributes.COL_EXTENDED_DATA_DICTIONARY_OID, ddx.getOid());
            List<OrganizationAttributes> attributes = (List<OrganizationAttributes>) broker
                    .getCollectionByQuery(new BeanQuery(OrganizationAttributes.class, criteria));
            if (attributes.isEmpty()) {
                bean = X2BaseBean.newInstance(OrganizationAttributes.class, broker.getPersistenceKey());
                bean.setOrganizationOid(OrganizationManager.ROOT_ORGANIZATION);
                bean.setExtendedDataDictionaryOid(ddx.getOid());

                DataDictionary dictionary = DataDictionary.getDistrictDictionary(ddx, broker.getPersistenceKey());
                Iterator<String> aliasNames = ALIAS_NAMES.iterator();
                Iterator<String> aliasDefaults = ALIAS_DEFAULTS.iterator();
                while (aliasNames.hasNext()) {
                    String aliasName = aliasNames.next();
                    String defaultValue = aliasDefaults.next();
                    DataDictionaryField dictionaryField = dictionary.findDataDictionaryFieldByAlias(aliasName);
                    if (dictionaryField != null) {
                        bean.setFieldValueByBeanPath(dictionaryField.getJavaName(), defaultValue);
                    }
                }
                broker.saveBeanForced(bean);
            } else if (attributes.size() == 1) {
                bean = attributes.iterator().next();
            } else if (attributes.size() > 1) {
                throw new IllegalStateException("There must be only one row in the Organization Attributes for the "
                        + DDX_ORA_ID + " extended dictionary.");
            }
        }

        if (bean == null) {
            throw new IllegalStateException("Required ExtendedDataDictionary not found - " + DDX_ORA_ID);
        }

        return bean;
    }

    /**
     * Gets the sped config dictionary.
     *
     * @param broker X2Broker
     * @return Extended data dictionary
     */
    public static ExtendedDataDictionary getSpedConfigDictionary(X2Broker broker) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, DDX_ORA_ID);
        return (ExtendedDataDictionary) broker.getBeanByQuery(new BeanQuery(ExtendedDataDictionary.class, criteria));
    }

    /**
     * Fields
     */
    protected ExtendedMAWorkflowBehaviour m_behavior = null;
    protected SpedMAWorkflowCommonProcedure m_riWorkflowHelper = null;

    private boolean m_completeMultipleForms = true;
    private Map<String, FormDefinition> m_skipFormEditOids;
    private UserDataContainer m_userData;
    private Map<String, Collection<String>> m_validatedForms = new HashMap();

    /**
     * Instantiates a new ma sped workflow procedure.
     *
     * @param definition WorkflowDefinition
     * @param organization Organization
     * @param user User
     * @param broker X2Broker
     * @param locale Locale
     */
    public MaSpedWorkflowProcedure(WorkflowDefinition definition, Organization organization, User user, X2Broker broker,
            Locale locale) {
        super(definition, organization, user, broker, locale);
        m_behavior = new ExtendedMAWorkflowBehaviour();

        OrganizationAttributes ora = getSpedConfig(getBroker());
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(ora.getExtendedDataDictionary(), broker.getPersistenceKey());
        loadSkipForms(ora, dictionary);
        loadCompleteMultipleForms(ora, dictionary);
        loadValidatedForms(ora, dictionary);
        m_riWorkflowHelper = new SpedMAWorkflowCommonProcedure(this, definition, organization, user, broker, locale);
    }

    /**
     * Complete phase.
     *
     * @param progress WorkflowProgress
     * @see com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#completePhase(com.follett.fsc.core.k12.beans.WorkflowProgress)
     */
    @Override
    public void completePhase(WorkflowProgress progress) {
        List<ValidationError> errors = new LinkedList();

        if (m_completeMultipleForms) {
            GenericDetail detail = m_userData.getCurrentDetail();
            if (detail instanceof SisOutcomeDetail) {
                SisOutcomeDetail wfDetail = (SisOutcomeDetail) detail;
                if (wfDetail.getProgress() != null
                        && progress.getWorkflowOid().equals(wfDetail.getProgress().getWorkflowOid())) {
                    Set<String> initializedFormDefinitionOids = new HashSet();
                    List<FormDefinition> forms = wfDetail.getFormDefinitions();

                    String currentFormOid = wfDetail.getCurrentFormDefinitionOid();
                    for (FormDefinition form : forms) {
                        wfDetail.setCurrentFormDefinitionOid(form.getOid());
                        FormDetail formDetail = wfDetail.getCurrentFormDetail();
                        if (formDetail.getFormInstance() != null && formDetail.getFormInstance().getOid() != null) {
                            initializedFormDefinitionOids.add(form.getOid());
                        }
                    }
                    wfDetail.setCurrentFormDefinitionOid(currentFormOid);

                    Set<FormDefinition> missingForms = new HashSet();
                    for (WorkflowPhaseOutcomeForm wpf : progress.getWorkflowPhaseOutcome()
                            .getWorkflowPhaseOutcomeForms()) {
                        if (!initializedFormDefinitionOids.contains(wpf.getFormDefinition().getOid())
                                && !m_skipFormEditOids.keySet().contains(wpf.getFormDefinition().getOid())) {
                            missingForms.add(wpf.getFormDefinition());
                        }
                    }
                    if (!missingForms.isEmpty()) {
                        StringBuilder errorMessage = new StringBuilder();
                        errorMessage.append("The form(s) {");
                        Iterator<FormDefinition> iterator = missingForms.iterator();
                        while (iterator.hasNext()) {
                            FormDefinition fmd = iterator.next();
                            errorMessage.append(fmd.getName());
                            if (iterator.hasNext()) {
                                errorMessage.append(", ");
                            }
                        }
                        errorMessage.append("} must be populated before completing this phase.");
                        errors.add(new ValidationError(CUSTOM_ERROR, null, errorMessage.toString()));
                    }
                }
            }
        }

        if (errors.isEmpty()) {
            Object response = executeMethod(EXECUTE_METHOD_PREFIX, progress);
            if (response instanceof List) {
                addValidationErrors((List) response);
            }
        } else {
            addValidationErrors(errors);
        }
    }

    /**
     * Resolve conditional branch.
     *
     * @param progress WorkflowProgress
     * @return WorkflowPhase
     * @see com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#resolveConditionalBranch(com.follett.fsc.core.k12.beans.WorkflowProgress)
     */
    @Override
    public WorkflowPhase resolveConditionalBranch(WorkflowProgress progress) {
        return (WorkflowPhase) executeMethod(BRANCH_METHOD_PREFIX, progress);
    }

    /**
     * Rollback phase.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#rollbackPhase(com.follett.fsc.core.k12.beans.WorkflowProgress)
     */
    @Override
    public void rollbackPhase(WorkflowProgress progress) throws Exception {
        executeMethod(ROLLBACK_METHOD_PREFIX, progress);
    }

    /**
     * Execute acc rej partial.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeAccRejPartial(WorkflowProgress progress) throws Exception {
        m_behavior.executeAccRejPartial(progress, getOrganization(), getBroker());
    }

    /**
     * Sets the IEP field for initial evaluation consent to the progress date.
     *
     * @param progress WorkflowProgress
     */
    public void executeAssessmentConsentReceived(WorkflowProgress progress) {
        m_behavior.executeReceiveConsent(progress, getOrganization(), getLocale(), getBroker());
        ExtendedDataDictionary extendedDataDictionary = SpedUtils.getIepDictionary(getOrganization(), getBroker());
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        iep.setFieldValueByAlias(ALIAS_CONSENT_RECEIVED, progress.getDate().toString(), dictionary);
        getBroker().saveBeanForced(iep);
    }

    /**
     * Sets the IEP field for initial evaluation consent to the progress date.
     * Notify staff by email.
     *
     * @param progress WorkflowProgress
     *
     * @throws Exception
     */
    public void executeAssessmentConsentReceivedNotify(WorkflowProgress progress) throws Exception {
        this.executeAssessmentConsentReceived(progress);

        IepData implementedIep = (IepData) progress.getWorkflow().getOwner();
        ExtendedSpedNotificationManager notificationManager =
                m_riWorkflowHelper.new ExtendedSpedNotificationManager(implementedIep);
        notificationManager.sendEmailNotification();
    }

    /**
     * Check if the student is Age 5, but in grade K.
     * If so, they should have a PL2 6-21 form (rather than or in addition to PL2 3-5 forms).
     *
     * @param progress
     * @return List<ValidationError>
     */
    public List<ValidationError> executePlacements(WorkflowProgress progress) {
        ExtendedDataDictionary extendedDataDictionary = SpedUtils.getIepDictionary(getOrganization(), getBroker());
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());
        IepData iep = (IepData) progress.getWorkflow().getOwner();

        List<ValidationError> errors = checkPlacementAge(iep, dictionary);

        // Make parent approval form instances for each/all PL1 forms storage.
        if (errors == null || errors.size() == 0) {
            setupPlacementApprovalForms(iep, dictionary);
        }

        return errors;
    }

    /**
     * Sets the IEP to discarded and reset student special ed status.
     *
     * @param progress WorkflowProgress
     */
    public void executeNotReceived(WorkflowProgress progress) {
        // m_behavior.executeIneligible(progress, getOrganization(), getBroker());
        X2BaseBean owner = progress.getWorkflow().getOwner();
        if (owner instanceof IepData) {
            // update student Last IEP Evaluation date based on IEP last evaluation date.
            // there is no rollback feature for this update
            IepData iep = (IepData) owner;
            iep.setStatusCodeEnum(IepData.StatusCode.DISCARDED);
            getBroker().saveBeanForced(iep);

            SisStudent student = iep.getStudent();
            PlainDate date = iep.getLastEvaluationDate();
            if (student != null) {
                IepData activeIep = student.getActiveIep(getBroker());
                WorkflowDefinition wDef = progress.getWorkflow().getWorkflowDefinition();
                if (activeIep != null || isReevalWorkflow(wDef, getOrganization())) {
                    String activeCode = PreferenceManager.getPreferenceValue(
                            getOrganization(), SisPreferenceConstants.SPED_ACTIVE_CODE);
                    student.setSpedStatusCode(activeCode);
                } else {
                    student.setSpedStatusCode(null);
                }
                if (date != null) {
                    student.setSpedLastEvaluationDate(date);
                }
                getBroker().saveBeanForced(student);
            }
        }
    }

    /**
     * Rollback acc rej partial.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackAccRejPartial(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackAccRejPartial(progress);
    }

    /**
     * Rolls back changes made by executeAssessmentConsentReceived().
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackAssessmentConsentReceived(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackReceiveConsent(progress, getOrganization(), getBroker());
        ExtendedDataDictionary extendedDataDictionary = SpedUtils.getIepDictionary(getOrganization(), getBroker());
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());
        IepData iep = (IepData) progress.getWorkflow().getOwner();
        iep.setFieldValueByAlias(ALIAS_CONSENT_RECEIVED, null, dictionary);
        getBroker().saveBeanForced(iep);
    }

    /**
     * Rolls back changes made by executeAssessmentConsentReceivedNotify().
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackAssessmentConsentReceivedNotify(WorkflowProgress progress) throws Exception {
        this.rollbackAssessmentConsentReceived(progress);
    }

    /**
     * Sets the IEP to discarded and reset student special ed status.
     *
     * @param progress WorkflowProgress
     * @throws Exception
     */
    public void rollbackNotReceived(WorkflowProgress progress) throws Exception {
        X2BaseBean owner = progress.getWorkflow().getOwner();
        if (owner instanceof IepData) {
            // Only change the current IEP back to DRAFT.
            // Do not need to change anything on the student or other IEPs.
            IepData iep = (IepData) owner;
            iep.setStatusCodeEnum(IepData.StatusCode.DRAFT);
            getBroker().saveBeanForced(iep);
        }
    }

    /**
     * Sets the user data.
     *
     * @param userData void
     * @see com.follett.fsc.core.k12.tools.procedures.SessionAwareProcedure#setUserData(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    public void setUserData(UserDataContainer userData) {
        m_userData = userData;
    }

    /**
     * Validate form.
     *
     * @param formInstance FormInstance
     * @param formStorage X2BaseBean
     * @param userData UserDataContainer
     * @return List
     * @see com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#validateForm(com.follett.fsc.core.k12.beans.FormInstance,
     *      com.follett.fsc.core.k12.beans.X2BaseBean,
     *      com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    public List validateForm(FormInstance formInstance, X2BaseBean formStorage, UserDataContainer userData) {
        List<ValidationError> validationErrors = new ArrayList();
        if (m_validatedForms.keySet().contains(formInstance.getFormDefinition().getId())) {
            DataDictionary dictionary =
                    DataDictionary.getDistrictDictionary(formInstance.getFormDefinition().getExtendedDataDictionary(),
                            userData.getPersistenceKey());
            for (String fieldName : m_validatedForms.get(formInstance.getFormDefinition().getId())) {
                DataDictionaryField field = getDictionaryField(dictionary, fieldName);
                if (field == null) {
                    StringBuilder errorMsg = new StringBuilder();
                    errorMsg.append("The required field ");
                    errorMsg.append(fieldName);
                    errorMsg.append(" on form ");
                    errorMsg.append(formInstance.getFormDefinition().getName());
                    errorMsg.append(" cannot be found in the dictionary");
                    ValidationError error =
                            new ValidationError(ValidationConstants.CUSTOM_ERROR, "", errorMsg.toString());
                    validationErrors.add(error);
                } else {
                    if (field.getDataTable().getClassName().equals(formStorage.getClass().getName())) {
                        Object value = null;
                        try {
                            value = WebUtils.getProperty(formStorage, field.getJavaName());
                        } catch (X2BaseException e) {
                            // Do nothing
                        }
                        if (value == null || StringUtils.isEmpty(value.toString())) {
                            StringBuilder errorMsg = new StringBuilder();
                            errorMsg.append("The value in the ");
                            errorMsg.append(field.getUserLongName());
                            errorMsg.append(" on form ");
                            errorMsg.append(formInstance.getFormDefinition().getName());
                            errorMsg.append(" must be populated");
                            ValidationError error =
                                    new ValidationError(ValidationConstants.CUSTOM_ERROR, "", errorMsg.toString());
                            validationErrors.add(error);
                        }
                    }
                }
            }
        }
        return validationErrors;
    }

    /**
     * Scan the IEP and clear fields that have invalid/discontinued reference code values.
     *
     * @param iep
     */
    protected List<ValidationError> validateDiscontinuedRefcodes(IepData iep) {
        List<ValidationError> errors = new ArrayList<ValidationError>();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(iep.getExtendedDataDictionary(),
                getBroker().getPersistenceKey());
        ReferenceCodeRetriever codeRetriever = ReferenceCodeRetriever.getInstance(getBroker().getPersistenceKey());
        // Clean the IEP Data.
        {
            DataDictionaryTable table = dictionary.findDataDictionaryTableByClass(IepData.class.getName());
            errors.addAll(validateDiscontinuedRefcodesTable(iep, dictionary, table, codeRetriever));
        }
        // Clean the IEP Goals and Objectives.
        {
            Collection<IepGoal> goals = iep.getIepGoals(getBroker());
            DataDictionaryTable table = dictionary.findDataDictionaryTableByClass(IepGoal.class.getName());
            for (IepGoal goal : goals) {
                errors.addAll(validateDiscontinuedRefcodesTable(goal, dictionary, table, codeRetriever));
                DataDictionaryTable otable =
                        dictionary.findDataDictionaryTableByClass(IepGoalObjective.class.getName());
                Collection<IepGoalObjective> objectives = goal.getIepGoalObjectives(getBroker());
                for (IepGoalObjective objective : objectives) {
                    errors.addAll(validateDiscontinuedRefcodesTable(objective, dictionary, otable, codeRetriever));
                }
            }
        }
        // Clean the IEP Placements.
        {
            Collection<IepPlacement> placements = iep.getPlacements(getBroker());
            DataDictionaryTable table = dictionary.findDataDictionaryTableByClass(IepPlacement.class.getName());
            for (IepPlacement placement : placements) {
                errors.addAll(validateDiscontinuedRefcodesTable(placement, dictionary, table, codeRetriever));
            }
        }
        // Clean the IEP Services.
        {
            Collection<IepService> services = iep.getIepServices(getBroker());
            DataDictionaryTable table = dictionary.findDataDictionaryTableByClass(IepService.class.getName());
            for (IepService service : services) {
                errors.addAll(validateDiscontinuedRefcodesTable(service, dictionary, table, codeRetriever));
            }
        }
        // Clean the IEP Amendments.
        {
            Collection<IepAmendment> amendments = iep.getIepAmendments(getBroker());
            DataDictionaryTable table = dictionary.findDataDictionaryTableByClass(IepAmendment.class.getName());
            for (IepAmendment amendment : amendments) {
                errors.addAll(validateDiscontinuedRefcodesTable(amendment, dictionary, table, codeRetriever));
            }
        }
        // Clean the IEP Accommodations.
        {
            Collection<IepAccommodation> accommodations = iep.getAccommodations(getBroker());
            DataDictionaryTable table = dictionary.findDataDictionaryTableByClass(IepAccommodation.class.getName());
            for (IepAccommodation accommodation : accommodations) {
                errors.addAll(validateDiscontinuedRefcodesTable(accommodation, dictionary, table, codeRetriever));
            }
        }
        // Clean the IEP Team Members.
        {
            Collection<IepTeamMember> members = iep.getTeamMembers(getBroker());
            DataDictionaryTable table = dictionary.findDataDictionaryTableByClass(IepTeamMember.class.getName());
            for (IepTeamMember member : members) {
                errors.addAll(validateDiscontinuedRefcodesTable(member, dictionary, table, codeRetriever));
            }
        }
        return errors;
    }

    /**
     * Check all reference table fields for valid reference code.
     * If a field value is invalid, clear the field.
     * Return true if any fields were modified.
     *
     * @param bean
     * @param dictionary
     * @param table
     *
     * @return boolean
     */
    protected List<ValidationError> validateDiscontinuedRefcodesTable(X2BaseBean bean,
                                                                      DataDictionary dictionary,
                                                                      DataDictionaryTable table,
                                                                      ReferenceCodeRetriever codeRetriever) {
        List<ValidationError> errors = new ArrayList<ValidationError>();
        List<DataDictionaryField> fields = dictionary.getFieldsForContext(table.getClassName());
        boolean dirty = false;
        int flags = ReferenceCodeRetriever.EXCLUDE_DISABLED_CODES | ReferenceCodeRetriever.EXCLUDE_HIDDEN_CODES;
        for (DataDictionaryField field : fields) {
            ReferenceTable refTbl = field.getReferenceTable();
            if (refTbl != null && field.getValidReferenceOnlyIndicator()) {
                String fieldValue = (String) bean.getFieldValueByBeanPath(field.getJavaName());
                if (!StringUtils.isEmpty(fieldValue)) {
                    List<ReferenceCode> refCodes = codeRetriever.getCodes(refTbl, flags, m_userData, getBroker());
                    boolean found = false;
                    if (field.getJavaName().startsWith("fieldD")) {
                        List<String> fieldValues = StringUtils.convertDelimitedStringToList(fieldValue, ',', true);
                        Iterator<String> iterator = fieldValues.iterator();
                        boolean rowDirty = false;
                        while (iterator.hasNext()) {
                            String rowValue = iterator.next();
                            boolean rowFound = false;
                            for (ReferenceCode code : refCodes) {
                                if (code.getCode().equals(fieldValue)) {
                                    rowFound = true;
                                    break;
                                }
                            }
                            if (!found) {
                                rowDirty = true;
                            }
                        }
                        if (rowDirty) {
                            String message = table.getUserName() + " " + field.getUserLongName()
                                    + " has an invalid value: " + fieldValue;
                            errors.add(new ValidationError(CUSTOM_ERROR, null, message));
                        }
                    } else {
                        for (ReferenceCode code : refCodes) {
                            if (code.getCode().equals(fieldValue)) {
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            String message = table.getUserName() + " " + field.getUserLongName()
                                    + " has an invalid value: " + fieldValue;
                            errors.add(new ValidationError(CUSTOM_ERROR, null, message));
                        }
                    }
                }
            }
        }
        return errors;
    }

    /**
     * Returns true if the passed workflow is an IEP Re-evaluation workflow.
     *
     * @param workflowDefinition WorkflowDefinition
     * @param district Organization
     * @return boolean
     */
    private boolean isReevalWorkflow(WorkflowDefinition workflowDefinition, Organization district) {
        String reevalId = PreferenceManager.getPreferenceValue(district, SisPreferenceConstants.SPED_WORKFLOW_REEVAL);
        return reevalId.equals(workflowDefinition.getId());
    }

    /**
     * Executes a workflow procedure method. The method to execute is determined by the method
     * prefix (either EXECUTE or ROLLBACK) and the method ID defined on the workflow phase outcome.
     *
     * @param methodPrefix String
     * @param progress WorkflowProgress
     * @return Object
     */
    private Object executeMethod(String methodPrefix, WorkflowProgress progress) {
        Object returnValue = null;

        if (progress.isOutcomeSelected() &&
                !StringUtils.isEmpty(progress.getWorkflowPhase().getWorkflowDefinition().getProcedureId()) &&
                !StringUtils.isEmpty(progress.getWorkflowPhaseOutcome().getMethodId())) {
            String methodId = progress.getWorkflowPhaseOutcome().getMethodId();

            removeErrors();

            String methodName = methodPrefix +
                    methodId.substring(0, 1).toUpperCase() +
                    methodId.substring(1);

            Exception e = null;
            try {
                Method method = getClass().getMethod(methodName, new Class[] {WorkflowProgress.class});
                returnValue = method.invoke(this, new Object[] {progress});
            } catch (NoSuchMethodException nsme) {
                e = nsme;
            } catch (IllegalAccessException iae) {
                e = iae;
            } catch (InvocationTargetException ite) {
                throw new X2RuntimeException(ite.getCause());
            } finally {
                if (e != null) {
                    LoggerUtils.addLogRecord(AppGlobals.getLog(), Level.SEVERE, LOG_INVALID_METHOD,
                            new Object[] {methodName}, e);
                }
            }
        }

        return returnValue;
    }

    /**
     * Gets the dictionary field.
     *
     * @param dictionary DataDictionary
     * @param fieldName String
     * @return Data dictionary field
     */
    private DataDictionaryField getDictionaryField(DataDictionary dictionary, String fieldName) {
        DataDictionaryField field = null;
        if (fieldName.startsWith(ALIAS_PREFIX)) {
            String alias = fieldName.substring(ALIAS_PREFIX.length());
            field = dictionary.findDataDictionaryFieldByAlias(alias);
        } else {
            field = dictionary.findDataDictionaryField(fieldName);
        }
        return field;
    }

    /**
     * Load complete multiple forms.
     *
     * @param ora OrganizationAttributes
     * @param dictionary DataDictionary
     */
    private void loadCompleteMultipleForms(OrganizationAttributes ora, DataDictionary dictionary) {
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_COMPLETE_FORMS);
        if (field != null) {
            String value = (String) ora.getFieldValueByBeanPath(field.getJavaName());
            if (BooleanAsStringConverter.FALSE.equals(value)) {
                m_completeMultipleForms = false;
            }
        }
    }

    /**
     * Load skip forms.
     *
     * @param ora OrganizationAttributes
     * @param dictionary DataDictionary
     */
    private void loadSkipForms(OrganizationAttributes ora, DataDictionary dictionary) {
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SKIP_FORMS);
        if (field != null) {
            String values = (String) ora.getFieldValueByBeanPath(field.getJavaName());
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(FormDefinition.COL_ID, Arrays.asList(values.split("\\s*;\\s*")));
            m_skipFormEditOids =
                    getBroker().getMapByQuery(new BeanQuery(FormDefinition.class, criteria), X2BaseBean.COL_OID, 10);
        } else {
            m_skipFormEditOids = new HashMap();
        }
    }

    /**
     * Load validated forms.
     *
     * @param ora OrganizationAttributes
     * @param dictionary DataDictionary
     */
    private void loadValidatedForms(OrganizationAttributes ora, DataDictionary dictionary) {
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_REQ_FIELDS);
        if (field != null) {
            Pattern pattern = Pattern
                    .compile(
                            "([a-zA-Z0-9-\\/()',][a-zA-Z0-9-\\/()', ]+[a-zA-Z0-9-\\/()',])\\s*\\{([a-zA-Z0-9-:\\s,]+)\\}");
            String value = (String) ora.getFieldValueByBeanPath(field.getJavaName());
            Matcher matcher = pattern.matcher(value);
            while (matcher.find()) {
                String formId = matcher.group(1);
                String fields = matcher.group(2);
                m_validatedForms.put(formId, Arrays.asList(fields.split("\\s*,\\s*")));
            }
        }
    }

    /**
     * Determine if the student is age 5 and grade level K (KF, KP), then they must have a
     * PL2 placement form.
     *
     * @param iep
     * @param dictionary
     * @return List<ValidationError>
     */
    private List<ValidationError> checkPlacementAge(IepData iep, DataDictionary dictionary) {
        List<ValidationError> errors = null;
        errors = new ArrayList<ValidationError>();
        DataDictionaryField gradeCodefield =
                dictionary.findDataDictionaryField(Student.class.getName(), Student.COL_GRADE_LEVEL);
        Student student = iep.getStudent();
        String gradeCode = student.getGradeLevel();
        String gradeStateCode = dictionary.findStateReferenceCode(gradeCodefield.getReferenceTableOid(), gradeCode);

        PlainDate dob = student.getPerson().getDob();
        Calendar cal = Calendar.getInstance();
        cal.setTimeInMillis(System.currentTimeMillis() - dob.getTime());
        int age = cal.get(Calendar.YEAR) - 1970;
        if (age == 5 && gradeStateCode != null && gradeStateCode.startsWith("K")) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, iep.getOid());
            criteria.addEqualTo(FormInstance.COL_FORM_DEFINITION_OID, "fmdMaPL2621a");
            BeanQuery query = new BeanQuery(FormInstance.class, criteria);
            FormInstance fmi = getBroker().getBeanByQuery(query);
            if (fmi == null) {
                errors.add(new ValidationError(CUSTOM_ERROR, null,
                        "This student requires an additional placement PL2 6-21 form be completed. They are in Grade K and 5 years old."));
            }
        }
        return errors;
    }

    /**
     * For each PL1 form (3-5 and 6-21) find or add the Parent Approval form associated with the
     * same placement.
     *
     * @param iep
     * @param dictionary
     */
    private void setupPlacementApprovalForms(IepData iep, DataDictionary dictionary) {
        // Get all PL1 placement form instances.

        X2Criteria definitionCriteria = new X2Criteria();
        definitionCriteria.addEqualTo(FormDefinition.COL_ID, MA_PARENT_PLAEMENT_FORM_DEFINITION_ID);
        BeanQuery definitionQuery = new BeanQuery(FormDefinition.class, definitionCriteria);
        FormDefinition parentFormDef = getBroker().getBeanByQuery(definitionQuery);
        if (parentFormDef != null) {
            ArrayList<String> formDefs = new ArrayList<String>(2);
            formDefs.add(MA_PL135_FORM_DEFINITION_ID);
            formDefs.add(MA_PL1621_FORM_DEFINITION_ID);
            X2Criteria placementCriteria = new X2Criteria();
            placementCriteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, iep.getOid());
            placementCriteria.addIn(FormInstance.REL_FORM_DEFINITION + "." + FormDefinition.COL_ID, formDefs);
            BeanQuery placementQuery = new BeanQuery(FormInstance.class, placementCriteria);

            X2Criteria parentCriteria = new X2Criteria();
            parentCriteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, iep.getOid());
            parentCriteria.addEqualTo(FormInstance.REL_FORM_DEFINITION + "." + FormDefinition.COL_ID,
                    MA_PARENT_PLAEMENT_FORM_DEFINITION_ID);
            BeanQuery parentQuery = new BeanQuery(FormInstance.class, parentCriteria);

            Collection<FormInstance> placementInstances = getBroker().getCollectionByQuery(placementQuery);
            Collection<FormInstance> parentInstances = getBroker().getCollectionByQuery(parentQuery);
            for (FormInstance placement : placementInstances) {
                boolean parentFound = false;
                for (FormInstance parent : parentInstances) {
                    if (parent.getStorageObjectOid().equals(placement.getStorageObjectOid())) {
                        parentFound = true;
                    }
                }
                if (!parentFound) {
                    FormInstance newParent = X2BaseBean.newInstance(FormInstance.class, dictionary);
                    newParent.setFormDefinitionOid(parentFormDef.getOid());
                    newParent.setOwnerObjectOid(placement.getOwnerObjectOid());
                    newParent.setStorageObjectOid(placement.getStorageObjectOid());
                    newParent.setOwnerView(placement.getOwnerView());
                    newParent.setCreatedTime(System.currentTimeMillis());
                    getBroker().saveBeanForced(newParent);
                }
            }
        }
    }
}


