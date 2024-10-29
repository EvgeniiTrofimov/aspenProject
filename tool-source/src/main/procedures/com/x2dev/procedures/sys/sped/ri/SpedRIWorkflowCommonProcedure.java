/* #PROCEDURE-ID [SYS-SPED-RI-COMMON-W] */
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
package com.x2dev.procedures.sys.sped.ri;

import static com.follett.fsc.core.k12.business.BusinessRules.IMPLEMENT_IEP_DRAFT_REQUIRED;
import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import static com.follett.fsc.core.k12.business.ValidationConstants.BUSINESS_RULE_VIOLATION;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.BeanCopier;
import com.follett.fsc.core.k12.business.EmailManager;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.TextBankUtils;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.WriteEmailManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.business.SpedNotificationManager;
import com.x2dev.sis.model.business.sped.RhodeIslandAliases;
import com.x2dev.sis.model.business.sped.RhodeIslandSpedWorkflowBehavior;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SpedRIWorkflowCommonProcedure.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class SpedRIWorkflowCommonProcedure {

    @SuppressWarnings("unused")
    private WorkflowProcedure m_workflowProcedure = null;
    private X2Broker m_broker = null;
    @SuppressWarnings("unused")
    private Locale m_locale = null;
    @SuppressWarnings("unused")
    private User m_user = null;
    @SuppressWarnings("unused")
    private WorkflowDefinition m_workflowDefinition = null;
    private Organization m_district = null;

    /**
     * The Class ExtendedSpedNotificationManager.
     *
     * @author Follett Software Company
     * @copyright 2017
     */
    class ExtendedSpedNotificationManager extends SpedNotificationManager {


        /**
         * Resource key for the e-mail subject label.
         */
        private static final String MAIL_SUBJECT_LABEL = "message.email.iep.subjectHeader";

        private WriteEmailManager m_emailManager = null;
        private IepData m_iep = null;


        /**
         * Instantiates a new extended sped notification manager.
         *
         * @param iep IepData
         */
        public ExtendedSpedNotificationManager(IepData iep) {
            super(m_broker, m_district, iep);

            m_iep = iep;

            m_emailManager = new WriteEmailManager(m_district);
        }


        /**
         * @see com.x2dev.sis.model.business.SpedNotificationManager#sendEmailNotification()
         */
        @Override
        public boolean sendEmailNotification() throws Exception {
            SisStudent student = m_iep.getStudent();

            String subject = LocalizationCache.getMessages(m_broker.getPersistenceKey()).getMessage(MAIL_SUBJECT_LABEL,
                    student.getNameView());
            String message = PreferenceManager.getPreferenceValue(m_district,
                    SisPreferenceConstants.SPED_DEFAULT_IEP_EMAIL_BODY_TEXT);
            message = TextBankUtils.specifyNames(message, student.getPerson().getFirstName() + " "
                    + student.getPerson().getLastName());
            message = TextBankUtils.specifyDistrict(message, m_district.getName());

            List<String> recipientsList = buildRecipientsList2(m_iep, student);

            return m_emailManager.sendMassEmail(recipientsList, subject, message, null);

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
        private List<String> buildRecipientsList2(IepData iep, SisStudent student) {
            List<String> recipientsList = new ArrayList<String>();

            // add teacher emails
            X2Criteria staffCriteria = new X2Criteria();
            staffCriteria.addEqualTo(StudentSchedule.COL_STUDENT_OID, student.getOid());
            staffCriteria.addEqualToField(StudentSchedule.COL_SCHEDULE_OID,
                    StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER +
                            SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER
                            + SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID);
            staffCriteria.addNotEmpty(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    Section.REL_PRIMARY_STAFF + PATH_DELIMITER + SisStaff.REL_PERSON + PATH_DELIMITER +
                    SisPerson.COL_EMAIL01, m_broker.getPersistenceKey());

            SubQuery staffSubQuery = new SubQuery(StudentSchedule.class, StudentSchedule.REL_SECTION +
                    PATH_DELIMITER + Section.REL_PRIMARY_STAFF + PATH_DELIMITER + SisStaff.REL_PERSON +
                    PATH_DELIMITER + SisPerson.COL_EMAIL01, staffCriteria);

            recipientsList.addAll(m_broker.getSubQueryCollectionByQuery(staffSubQuery));

            // add IEP team member emails
            X2Criteria teamMemberCriteria = new X2Criteria();
            teamMemberCriteria.addEqualTo(IepTeamMember.COL_IEP_DATA_OID, iep.getOid());
            DataDictionary ddx = getDictionaryByExtendedDictionaryId(DDX_ID_SPED_RI_IEP);
            DataDictionaryField field = ddx.findDataDictionaryFieldByAlias(ALIAS_ITM_EXCLUDE_EMAIL);

            if (field != null) {
                teamMemberCriteria.addNotEqualTo(field.getJavaName(), BooleanAsStringConverter.TRUE);
            }
            teamMemberCriteria.addNotEmpty(IepTeamMember.REL_PERSON + PATH_DELIMITER +
                    SisPerson.COL_EMAIL01, m_broker.getPersistenceKey());

            SubQuery teamMemberSubQuery = new SubQuery(IepTeamMember.class, IepTeamMember.REL_PERSON +
                    PATH_DELIMITER + SisPerson.COL_EMAIL01, teamMemberCriteria);

            recipientsList.addAll(m_broker.getSubQueryCollectionByQuery(teamMemberSubQuery));

            /*
             * Exclude invalid emails.
             */
            if (!CollectionUtils.isEmpty(recipientsList)) {
                List<String> invalidEmails = new ArrayList<String>();
                for (String email : recipientsList) {
                    if (!EmailManager.validateEmailAddress(email)) {
                        invalidEmails.add(email);
                    }
                }

                recipientsList.removeAll(invalidEmails);
            }

            // add guidance counselor
            // TODO: add guidance counselor once it is a system field on Student table

            return recipientsList;
        }

    }

    /**
     * Inner class to customize RI Special Education Renewal workflow with custom behaviour.
     * In particular it extends Renew and Amendment worflows to make a copy for My Career
     * Development Plan
     *
     * @author X2 Development Corporation
     */
    class ExtendedRISpedWorkflowBehavior extends RhodeIslandSpedWorkflowBehavior {

        /*
         * A preference controlling the delayed activation feature.
         */
        protected static final String PREFERENCE_KEY = "sys.sped.ri.delayActivation";

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
            IepData amendmentDraftIEP = super.executeCreateAmendmentDraft(student, date, errors, broker);
            if (errors.isEmpty()) {
                copyForm(student, amendmentDraftIEP, FMD_ID_SPED_RI_CRR_PLAN, broker);

                // If student is K or Pre-K, copy COSF.
                Integer numericGradeLevel = getStudentGradeLevelNumeric(student, broker);
                if (numericGradeLevel != null && numericGradeLevel.intValue() <= 0) {
                    copyForm(student, amendmentDraftIEP, FMD_ID_SPED_RI_COSF, broker);
                }
            }
            return amendmentDraftIEP;

        }

        /**
         * Records the meetings OID on the IEP, so that its details can be displayed directly on the
         * IEP
         * print out (report).
         *
         * @param progress WorkflowProgress
         * @param district Organization
         * @param broker X2Broker
         * @return List
         */
        @Override
        public List<ValidationError> executeHoldMeeting(WorkflowProgress progress,
                                                        Organization district,
                                                        X2Broker broker) {
            List<ValidationError> errors = new ArrayList<ValidationError>(0);
            if (isDateSettingEnabled(district)) {
                IepData iep = (IepData) progress.getWorkflow().getOwner();
                PlainDate meetingDate = null;

                if (iep.getMeetingDate() == null) {
                    Optional<WorkflowProgressForm> progressForm = progress.getWorkflowProgressForms(broker).stream()
                            .filter(form -> IepMeeting.class
                                    .isAssignableFrom(form.getFormInstance().getStorageObject(broker).getClass()))
                            .findAny();
                    if (progressForm.isPresent()) {
                        FormInstance formInstance = progressForm.get().getFormInstance();
                        if (formInstance != null) {
                            IepMeeting meeting = (IepMeeting) formInstance.getStorageObject(broker);
                            if (meeting != null) {
                                meetingDate = meeting.getDate();
                                iep.setFieldValueByAlias(RhodeIslandAliases.RELATED_MEETING_OID, meeting.getOid());
                            }
                        }
                    }
                }

                if (meetingDate != null) {
                    iep.setMeetingDate(meetingDate);

                    Calendar calendar = Calendar.getInstance();
                    calendar.setTime(meetingDate);
                    calendar.add(Calendar.YEAR, 1);

                    iep.setLastReviewDate(meetingDate);
                    iep.setNextReviewDate(DateUtils.getWeekday(new PlainDate(calendar.getTimeInMillis())));

                    boolean isEvalWorkflow = isEvalWorkflow(progress.getWorkflow().getWorkflowDefinition(), district);
                    if (isEvalWorkflow) {
                        calendar.add(Calendar.YEAR, 2);

                        iep.setLastEvaluationDate(meetingDate);
                        iep.setNextEvaluationDate(DateUtils.getWeekday(new PlainDate(calendar.getTimeInMillis())));
                    }

                    broker.saveBeanForced(iep);
                }
            }

            return errors;
        }

        /**
         * Method executed when an IEP is implemented on a referral or IEP renewal workflow. If an
         * <i>active</i> IEP exists for the student, its status is set to <i>previous</i>. The
         * status of
         * the <i>draft</i> IEP is then set to <i>active</i>.
         * <p>
         * The following dates are set by this method:
         * <ul>
         * <li>The end date of the previous IEP is set to the day before the phase completion date
         * <li>The start date of the new IEP is set to the phase completion date
         * <li>The end date of the new IEP is set to the phase completion date plus one year
         * <li>The student's next review date is set to the phase completion date plus one year
         * <li>The student's next evaluation date is set to the phase completion date plus three
         * years
         * </ul>
         * <p>
         * The student's special education status is changed to <i>active</i> if necessary.
         *
         * @param progress WorkflowProgress
         * @param district Organization
         * @param locale Locale
         * @param broker X2Broker
         * @return List of ValiationError objects
         * @throws Exception exception
         */
        @Override
        public List<ValidationError> executeImplementIep(WorkflowProgress progress,
                                                         Organization district,
                                                         Locale locale,
                                                         X2Broker broker)
                throws Exception {
            List<ValidationError> errors = new LinkedList<ValidationError>();
            IepData draftIep = (IepData) progress.getWorkflow().getOwner();

            if (draftIep == null) {
                errors.add(new ValidationError(BUSINESS_RULE_VIOLATION, Integer.valueOf(IMPLEMENT_IEP_DRAFT_REQUIRED)));
            } else {
                SisStudent student = draftIep.getStudent();
                Calendar calendar = null;
                if (locale != null) {
                    calendar = Calendar.getInstance(locale);
                } else {
                    calendar = Calendar.getInstance();
                }

                String currentValue = PreferenceManager.getPreferenceValue(district, PREFERENCE_KEY);
                boolean useDelayedActivation = false;
                if (!StringUtils.isEmpty(currentValue)) {
                    useDelayedActivation = Boolean.parseBoolean(currentValue);
                }

                PlainDate date = progress.getDate();
                PlainDate startDate = draftIep.getStartDate();
                boolean postDated = false;
                if (date != null && startDate != null) {
                    postDated = date.before(startDate);
                }

                /*
                 * Set the end date on the active IEP, and change its status to previous;
                 * Note that for the initial referral workflow, an active IEP will not exist.
                 */
                if (!postDated || !useDelayedActivation) {
                    IepData activeIep = student.getActiveIep(broker);
                    if (activeIep != null) {
                        activeIep.setStatusCodeEnum(StatusCode.PREVIOUS);
                        broker.saveBeanForced(activeIep);
                    }
                }

                if (isDateSettingEnabled(district)) {
                    /*
                     * Set the start and end dates on the active IEP, and change its status to
                     * active. A
                     * draft
                     * IEP will exist for both workflows supported by this method.
                     */
                    if (draftIep.getStartDate() == null) {
                        draftIep.setStartDate(date);
                    }

                    calendar.setTime(draftIep.getStartDate());
                    calendar.add(Calendar.YEAR, 1);

                    if (draftIep.getEndDate() == null) {
                        draftIep.setEndDate(new PlainDate(calendar.getTime()));
                    }

                }

                if (postDated && useDelayedActivation) {
                    draftIep.setStatusCodeEnum(StatusCode.PENDING_APPROVAL);
                } else {
                    draftIep.setStatusCodeEnum(StatusCode.ACTIVE);
                }
                broker.saveBeanForced(draftIep);

                /*
                 * Change the student's special education status to active
                 */
                String studentActiveCode = PreferenceManager.getPreferenceValue(district,
                        SisPreferenceConstants.SPED_ACTIVE_CODE);
                if (!studentActiveCode.equals(student.getSpedStatusCode()) &&
                        (!postDated || !useDelayedActivation)) {
                    student.setSpedStatusCode(studentActiveCode);
                }

                broker.saveBeanForced(student);
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
            IepData renewalIEP =
                    super.executeRenewal(student, workflowDefinition, formInstances, locale, errors, broker);
            if (errors.isEmpty()) {
                copyForm(student, renewalIEP, FMD_ID_SPED_RI_CRR_PLAN, broker);

                // If student is K or Pre-K, copy COSF.
                Integer numericGradeLevel = getStudentGradeLevelNumeric(student, broker);
                if (numericGradeLevel != null && numericGradeLevel.intValue() <= 0) {
                    copyForm(student, renewalIEP, FMD_ID_SPED_RI_COSF, broker);
                }
            }
            return renewalIEP;
        }

        /**
         * Method executed when an IEP transfer workflow is initiated. This method creates a new
         * draft IEP, but sets the student's special ed status to active.
         *
         * NOTE: THIS OVERRIDE WAS CREATED ONLY TO FACILITATE DEPLOYMENT INSTEAD OF CORE
         * MODIFICATION
         * It could be removed once core changes are made.
         *
         * @param student SisStudent
         * @param formInstances Map<String,FormInstance>
         * @param locale Locale
         * @param broker X2Broker
         * @return IepData
         * @see
         *      com.follett.fsc.core.k12.business.sped.SpedWorkflowBehavior#executeTransfer(com.follett.fsc.
         *      core.k12.beans.CoreStudent, com.follett.fsc.core.k12.beans.FormInstance,
         *      java.util.Locale,
         *      com.follett.fsc.core.k12.business.X2Broker)
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

            String activeCode = PreferenceManager.getPreferenceValue(district,
                    SisPreferenceConstants.SPED_ACTIVE_CODE);

            student.setSpedStatusCode(activeCode);
            broker.saveBeanForced(student);

            return draftIep;
        }

        /**
         * Copy all forms for a Form Definition.
         *
         * @param student SisStudent
         * @param newIep IepData
         * @param formId String
         * @param broker X2Broker
         */
        private void copyForm(SisStudent student, IepData newIep, String formId, X2Broker broker) {

            BeanCopier beanCopier = new BeanCopier(broker, true);
            IepData activeIep = student.getActiveIep(broker);

            SisOrganization district = student.getSchool().getOrganization1();
            ExtendedDataDictionary iepExtendedDictionary = SpedUtils.getIepDictionary(district, broker);
            DataDictionary iepDictionary =
                    DataDictionary.getDistrictDictionary(iepExtendedDictionary, broker.getPersistenceKey());

            Collection<String> relationshipIds = Arrays.asList(new String[] {
                    iepDictionary
                            .findDataDictionaryRelationship(GenericFormData.class.getName(),
                                    GenericFormData.REL_GENERIC_FORM_DATA_CHILDREN)
                            .getId(),
            });

            // getting related form instances
            X2Criteria instanceCriteria = new X2Criteria();
            instanceCriteria.addEqualTo(FormInstance.REL_FORM_DEFINITION + PATH_DELIMITER +
                    FormDefinition.COL_ID, formId);
            instanceCriteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, activeIep.getOid());
            QueryByCriteria instanceQuery = new QueryByCriteria(FormInstance.class, instanceCriteria);
            Map<String, FormInstance> instanceMap =
                    broker.getMapByQuery(instanceQuery, FormInstance.COL_STORAGE_OBJECT_OID, 8);

            // getting related generic form data (gfd)
            SubQuery storageSubQuery =
                    new SubQuery(FormInstance.class, FormInstance.COL_STORAGE_OBJECT_OID, instanceCriteria);
            X2Criteria genericFormDataCriteria = new X2Criteria();
            genericFormDataCriteria.addIn(X2BaseBean.COL_OID, storageSubQuery);
            QueryByCriteria genericFormDataQuery =
                    new QueryByCriteria(GenericFormData.class, genericFormDataCriteria);
            Map<String, GenericFormData> genericFormDataMap =
                    broker.getMapByQuery(genericFormDataQuery, X2BaseBean.COL_OID, 10);

            for (GenericFormData genericFormData : genericFormDataMap.values()) {
                GenericFormData newGenericFormData =
                        (GenericFormData) beanCopier.copy(genericFormData, relationshipIds);

                if (instanceMap.containsKey(genericFormData.getOid())) {
                    FormInstance formInstance = instanceMap.get(genericFormData.getOid());
                    FormInstance newFormInstance =
                            (FormInstance) beanCopier.copy(formInstance, null,
                                    getFormInstanceDefaults(newGenericFormData.getOid(), newIep.getOid(), broker));
                    broker.saveBeanForced(newFormInstance);
                }

                broker.saveBeanForced(newGenericFormData);
            }

        }

        /**
         * Gets the form instance defaults.
         *
         * @param genericFormDataOid String
         * @param iepOid String
         * @param broker X2Broker
         * @return Hash map
         */
        private HashMap<ModelProperty, Object> getFormInstanceDefaults(String genericFormDataOid,
                                                                       String iepOid,
                                                                       X2Broker broker) {
            HashMap<ModelProperty, Object> valuesToSet = new HashMap<ModelProperty, Object>();

            valuesToSet.put(new ModelProperty(FormInstance.class, FormInstance.COL_OWNER_OBJECT_OID,
                    broker.getPersistenceKey()), iepOid);
            valuesToSet.put(new ModelProperty(FormInstance.class, FormInstance.COL_STORAGE_OBJECT_OID,
                    broker.getPersistenceKey()), genericFormDataOid);

            return valuesToSet;
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
         * Returns true if the passed workflow is an IEP evaluation workflow. Evaluation occurs
         * during
         * the referral and re-evaluation workflows.
         *
         * @param workflowDefinition WorkflowDefinition
         * @param district Organization
         * @return boolean
         */
        private boolean isEvalWorkflow(WorkflowDefinition workflowDefinition, Organization district) {
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
         * Returns the numeric grade level for the students grade level.
         *
         * @param student
         * @param broker
         * @return
         */
        private Integer getStudentGradeLevelNumeric(SisStudent student, X2Broker broker) {
            String gradeLevel = student.getGradeLevel();
            TreeMap<Integer, List<String>> gradeMap = StudentManager.buildGradeLevelMap(broker);
            for (Integer numericGrade : gradeMap.keySet()) {
                List<String> gradesAtLevel = gradeMap.get(numericGrade);
                if (gradesAtLevel.contains(gradeLevel)) {
                    return numericGrade;
                }
            }
            return null;
        }
    }

    private static final String ALIAS_ITM_EXCLUDE_EMAIL = "itm-exclude-email";

    private static final String DDX_ID_SPED_RI_IEP = "SPED-RI-IEP";
    private static final String FMD_ID_SPED_RI_COSF = "SPED-RI-COSF";
    private static final String FMD_ID_SPED_RI_CRR_PLAN = "SPED-RI-CRR-PLAN";


    /**
     * Instantiates a new sped RI workflow common procedure.
     *
     * @param procedure WorkflowProcedure
     * @param definition WorkflowDefinition
     * @param district Organization
     * @param user User
     * @param broker X2Broker
     * @param locale Locale
     */
    public SpedRIWorkflowCommonProcedure(WorkflowProcedure procedure, WorkflowDefinition definition,
            Organization district,
            User user, X2Broker broker, Locale locale) {

        m_workflowProcedure = procedure;
        m_workflowDefinition = definition;
        m_broker = broker;
        m_user = user;
        m_district = district;
        m_locale = locale;
    }

    /**
     * Gets the broker.
     *
     * @return X 2 broker
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getBroker()
     */
    protected X2Broker getBroker() {
        return m_broker;
    }



    /**
     * use the same method on <code>SpedIlWorkflowCommonProcedure</code>.
     *
     * @param extendedDataDictionaryID String
     * @return Data dictionary
     */
    public DataDictionary getDictionaryByExtendedDictionaryId(String extendedDataDictionaryID) {
        DataDictionary returnValue = null;
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, extendedDataDictionaryID);
        QueryByCriteria byCriteria = new QueryByCriteria(ExtendedDataDictionary.class, criteria);
        ExtendedDataDictionary extendedDataDictionary = (ExtendedDataDictionary) getBroker().getBeanByQuery(byCriteria);
        returnValue = DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());
        return returnValue;
    }

}
