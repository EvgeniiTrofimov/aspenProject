/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.Workflow;
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.beans.WorkflowPhase;
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.BusinessRules;
import com.follett.fsc.core.k12.business.EmailManager;
import com.follett.fsc.core.k12.business.MessageProperties;
import com.follett.fsc.core.k12.business.ValidationConstants;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.WriteEmailManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.procedures.SessionAwareProcedure;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.PdActivityCourse;
import com.x2dev.sis.model.beans.PdActivitySection;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffPdActivity;
import com.x2dev.sis.model.beans.StaffPdActivityApproval;
import com.x2dev.sis.model.beans.StaffPdActivityRequest;
import com.x2dev.sis.model.beans.StaffPdPlan;
import com.x2dev.sis.model.business.pd.PdScheduleManager;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.util.Locale;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * Procedure for the PD Activity Request workflow. This procedure defines special behavior that
 * occurs along with workflow, including:
 * <ul>
 * <li>Adding approval records
 * <li>Adding activity records when a staff member is enrolled in an activity
 * <li>Handling the conditional approval branch for immediate vs. delayed activity enrollments
 * <li>Maintaining the request status field
 * <li>Sending e-mail notifications to the requesting staff member when appropriate
 * </ul>
 *
 * @author X2 Development Corporation
 */
public class PdActivityRequestProcedure extends WorkflowProcedure implements SessionAwareProcedure {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private UserDataContainer userData;

    /**
     * Constructs a new PdActivityRequestProcedure.
     *
     * @param definition WorkflowDefinition
     * @param organization Organization
     * @param user User
     * @param broker X2Broker
     * @param locale Locale
     */
    public PdActivityRequestProcedure(WorkflowDefinition definition, Organization organization, User user,
            X2Broker broker, Locale locale) {
        super(definition, organization, user, broker, locale);
    }

    /**
     * Resolves the conditional branch on the approval phase of the workflow. The condition is on
     * the value of the section enrollment type - immediate or delayed. If immediate, the workflow
     * ends (null is returned). If delayed, the workflow moves on to the enrollment verification
     * phase.
     *
     * @param progress WorkflowProgress
     * @return WorkflowPhase
     */
    public WorkflowPhase branchApproved(WorkflowProgress progress) {
        WorkflowPhase nextPhase = null;

        StaffPdActivityRequest request = getRequest(progress.getWorkflow());
        PdActivitySection section = request.getPdSection();

        switch (section.getEnrollmentTypeEnum()) {
            case IMMEDIATE:
                // nextPhase remains null;
                break;

            case DELAYED:
                nextPhase = progress.getWorkflowPhaseOutcome().getNextWorkflowPhase();
                //$FALL-THROUGH$
            default:
        }

        return nextPhase;
    }

    /**
     * Sets required values on the request form.
     *
     * @param selectionBean X2BaseBean
     * @param formInstances Map<String,FormInstance>
     * @param date PlainDate
     * @param workflowDefinition WorkflowDefinition
     * @return X2BaseBean
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#initializeOwner(com.follett.fsc.
     *      core.k12.beans.X2BaseBean, java.util.Map, com.x2dev.utils.types.PlainDate,
     *      com.follett.fsc.core.k12.beans.WorkflowDefinition)
     */
    @Override
    public X2BaseBean initializeOwner(X2BaseBean selectionBean,
                                      Map<String, FormInstance> formInstances,
                                      PlainDate date,
                                      WorkflowDefinition workflowDefinition) {
        StaffPdPlan plan = (StaffPdPlan) super.initializeOwner(selectionBean, formInstances, date, workflowDefinition);

        if (!formInstances.isEmpty()) {
            FormInstance requestForm = formInstances.values().iterator().next();

            StaffPdActivityRequest request = (StaffPdActivityRequest) requestForm.getStorageObject(getBroker());

            request.setPdPlanOid(plan.getOid());
            request.setStaffOid(plan.getStaff().getOid());
            request.setDate(new PlainDate(getTimeZone()));
            request.setTime(new PlainTime(getTimeZone()));
            request.setStatus(StaffPdActivityRequest.Status.PENDING_APPROVAL.ordinal());

            getBroker().saveBeanForced(request);
        }

        return plan;
    }

    /**
     * Method executed when a request is approved. The following special behavior occurs here:
     * <ol>
     * <li>The approval record is updated with the approved status and the OID of the approver
     * <li>If the section enrollment type is immediate, an activity is created if there is room in
     * the section and the request status is set to APPROVED; if there is no room in the
     * section, a validation error is added
     * <li>If the section enrollment type is delayed, the request status is set to
     * PENDING_ENROLLMENT
     * <li>An e-mail is sent to the requesting staff member notifying them of the immediate or
     * delayed enrollment
     * </ol>
     *
     * @param progress WorkflowProgress
     */
    public void executeApproved(WorkflowProgress progress) {
        FormInstance approvalForm = progress.getFormInstance(getBroker());
        StaffPdActivityApproval approval = (StaffPdActivityApproval) approvalForm.getStorageObject(getBroker());
        StaffPdActivityRequest request = getRequest(progress.getWorkflow());

        approval.setStatus(StaffPdActivityApproval.Status.APPROVED.ordinal());
        approval.setStaffPdActivityRequestOid(request.getOid());
        approval.setStaffOid(request.getStaffOid());
        approval.setDate(new PlainDate(getTimeZone()));

        if (getUser().getPerson() != null) {
            Staff approver = getUser().getPerson().getStaff(getBroker());
            if (approver != null) {
                approval.setApproverOid(approver.getOid());
            }
        }

        boolean rollback = false;
        getBroker().beginTransaction();
        try {
            switch (request.getPdSection().getEnrollmentTypeEnum()) {
                case IMMEDIATE:
                    StaffPdActivity activity = createActivity(request, false);
                    if (activity != null) {
                        request.setStatus(StaffPdActivityRequest.Status.APPROVED.ordinal());
                        approval.setStaffPdActivityOid(activity.getOid());
                        sendApprovalMessage(request);
                    } else {
                        rollback = true;
                    }
                    break;

                case DELAYED:
                default:
                    request.setStatus(StaffPdActivityRequest.Status.PENDING_ENROLLMENT.ordinal());
                    sendPendingEnrollmentMessage(request);
            }

            getBroker().saveBeanForced(approval);
            getBroker().saveBeanForced(request);
        } finally {
            if (rollback) {
                getBroker().rollbackTransaction();
                addValidationError(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                        Integer.valueOf(BusinessRules.PD_SECTION_ENROLLMENT_MAX)));
            } else {
                getBroker().commitTransaction();
            }
        }
    }

    /**
     * Method executed when a request is denied. The following special behavior occurs here:
     * <ol>
     * <li>The approval record is updated with the denied status and the OID of the approver
     * <li>The request status is set to DENIED
     * <li>An e-mail is sent to the requesting staff member notifying them of the denial
     * </ol>
     *
     * @param progress WorkflowProgress
     */
    public void executeDenied(WorkflowProgress progress) {
        FormInstance approvalForm = progress.getFormInstance(getBroker());
        StaffPdActivityApproval approval = (StaffPdActivityApproval) approvalForm.getStorageObject(getBroker());
        StaffPdActivityRequest request = getRequest(progress.getWorkflow());

        approval.setStatus(StaffPdActivityApproval.Status.NOT_APPROVED.ordinal());
        approval.setStaffPdActivityRequestOid(request.getOid());
        approval.setStaffOid(request.getStaffOid());
        approval.setDate(new PlainDate(getTimeZone()));

        Staff approver = getUser().getPerson().getStaff(getBroker());
        if (approver != null) {
            approval.setApproverOid(approver.getOid());
        }

        getBroker().saveBeanForced(approval);

        request.setStatus(StaffPdActivityRequest.Status.DENIED.ordinal());
        getBroker().saveBeanForced(approval);

        sendDeniedMessage(request);
    }

    /**
     * Method executed when a staff member is enrolled in a section on the enrollment verification
     * phase. The following special behavior occurs here:
     * <ol>
     * <li>The request status is set to APPROVED
     * <li>An activity is created regardless of the available seats in the class (it is assumed that
     * the user knowingly chose to overbook the section)
     * <li>An e-mail is sent to the requesting staff member notifying them of the enrollment
     * </ol>
     *
     * @param progress WorkflowProgress
     */
    public void executeEnrolled(WorkflowProgress progress) {
        StaffPdActivityRequest request = getRequest(progress.getWorkflow());
        request.setStatus(StaffPdActivityRequest.Status.APPROVED.ordinal());
        getBroker().saveBeanForced(request);

        StaffPdActivity activity = createActivity(request, true);

        for (StaffPdActivityApproval approval : request.getStaffPdActivityApprovals(getBroker())) {
            approval.setStaffPdActivityOid(activity.getOid());
            getBroker().saveBeanForced(approval);
        }

        sendApprovalMessage(request);
    }

    /**
     * Method executed when a staff member is wait-listed for enrollment in a section on the
     * enrollment verification phase. The following special behavior occurs here:
     * <ol>
     * <li>The request status is set to WAIT_LIST
     * <li>An e-mail is sent to the requesting staff member notifying them of their waiting list
     * status
     * </ol>
     *
     * @param progress WorkflowProgress
     */
    public void executeWaitList(WorkflowProgress progress) {
        StaffPdActivityRequest request = getRequest(progress.getWorkflow());
        request.setStatus(StaffPdActivityRequest.Status.WAIT_LIST.ordinal());
        getBroker().saveBeanForced(request);

        sendWaitingListMessage(request);
    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.SessionAwareProcedure#setUserData(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    public void setUserData(UserDataContainer userData) {
        this.userData = userData;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getCurrentOwnerOid()
     */
    @Override
    protected String getCurrentOwnerOid() {
        return userData != null ? userData.getCurrentOwnerOid() : super.getCurrentOwnerOid();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getCurrentOwnerType()
     */
    @Override
    protected int getCurrentOwnerType() {
        return userData != null ? userData.getCurrentOwnerType() : super.getCurrentOwnerType();
    }

    /**
     * Sends the immediate approval e-mail notification.
     *
     * @param request StaffPdActivityRequest
     */
    protected void sendApprovalMessage(StaffPdActivityRequest request) {
        MessageResources messages = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale());

        String subject = messages.getMessage(getLocale(), "message.pdActivityRequest.subject");

        PdActivitySection section = request.getPdSection();
        PdActivityCourse course = section.getPdCourse();

        String staffName = request.getStaff().getNameView();
        String sectionId = course.getId() + "-" + section.getSectionNumber();
        String title = course.getTitle();
        String meetingDates = section.getMeetingTimeView();
        String location = section.getLocationView();

        Object[] params = new Object[] {staffName, sectionId, title, meetingDates, location};

        String body = messages.getMessage(getLocale(), "message.pdActivityRequest.immediateApproval", params);

        sendEmail(request.getStaff(), subject, body);
    }

    /**
     * Sends the denied enrollment e-mail notification.
     *
     * @param request StaffPdActivityRequest
     */
    protected void sendDeniedEnrollmentMessage(StaffPdActivityRequest request) {
        MessageResources messages = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale());

        String subject = messages.getMessage(getLocale(), "message.pdActivityRequest.subject");

        PdActivitySection section = request.getPdSection();
        PdActivityCourse course = section.getPdCourse();

        String staffName = request.getStaff().getNameView();
        String sectionId = course.getId() + "-" + section.getSectionNumber();
        String title = course.getTitle();
        String meetingDates = section.getMeetingTimeView();
        String location = section.getLocationView();

        Object[] params = new Object[] {staffName, sectionId, title, meetingDates, location};

        String body = messages.getMessage(getLocale(), "message.pdActivityRequest.deniedEnrollment", params);

        sendEmail(request.getStaff(), subject, body);
    }

    /**
     * Sends the request denial e-mail notification.
     *
     * @param request StaffPdActivityRequest
     */
    protected void sendDeniedMessage(StaffPdActivityRequest request) {
        MessageResources messages = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale());

        String subject = messages.getMessage(getLocale(), "message.pdActivityRequest.subject");

        PdActivitySection section = request.getPdSection();
        PdActivityCourse course = section.getPdCourse();

        String staffName = request.getStaff().getNameView();
        String sectionId = course.getId() + "-" + section.getSectionNumber();
        String title = course.getTitle();
        String meetingDates = section.getMeetingTimeView();
        String location = section.getLocationView();

        Object[] params = new Object[] {staffName, sectionId, title, meetingDates, location};

        String body = messages.getMessage(getLocale(), "message.pdActivityRequest.denied", params);

        sendEmail(request.getStaff(), subject, body);
    }

    /**
     * Sends the delayed approval e-mail notification.
     *
     * @param request StaffPdActivityRequest
     */
    protected void sendPendingEnrollmentMessage(StaffPdActivityRequest request) {
        MessageResources messages = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale());

        String subject = messages.getMessage(getLocale(), "message.pdActivityRequest.subject");

        PdActivitySection section = request.getPdSection();
        PdActivityCourse course = section.getPdCourse();

        Converter dateConverter = ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, getLocale());

        String staffName = request.getStaff().getNameView();
        String sectionId = course.getId() + "-" + section.getSectionNumber();
        String title = course.getTitle();
        String meetingDates = section.getMeetingTimeView();
        String location = section.getLocationView();
        String enrollmentEndDate = dateConverter.javaToString(section.getEnrollmentCloseDate());

        Object[] params = new Object[] {staffName, sectionId, title, meetingDates, location, enrollmentEndDate};

        String body = messages.getMessage(getLocale(), "message.pdActivityRequest.delayedApproval", params);

        sendEmail(request.getStaff(), subject, body);
    }

    /**
     * Sends the waiting list e-mail notification.
     *
     * @param request StaffPdActivityRequest
     */
    protected void sendWaitingListMessage(StaffPdActivityRequest request) {
        MessageResources messages = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale());

        String subject = messages.getMessage(getLocale(), "message.pdActivityRequest.subject");

        PdActivitySection section = request.getPdSection();
        PdActivityCourse course = section.getPdCourse();

        String staffName = request.getStaff().getNameView();
        String sectionId = course.getId() + "-" + section.getSectionNumber();
        String title = course.getTitle();
        String meetingDates = section.getMeetingTimeView();
        String location = section.getLocationView();

        Object[] params = new Object[] {staffName, sectionId, title, meetingDates, location};

        String body = messages.getMessage(getLocale(), "message.pdActivityRequest.waitingList", params);

        sendEmail(request.getStaff(), subject, body);
    }

    /**
     * Creates an activity corresponding to the passed request.
     *
     * @param request StaffPdActivityRequest
     * @param allowOverMax if true, the activity will be created even if there are no available
     *        seats in the section; if false, a validation error will be added
     *        if there are no available seats in the section
     * @return activity
     */
    private StaffPdActivity createActivity(StaffPdActivityRequest request, boolean allowOverMax) {
        StaffPdActivity activity = new StaffPdActivity(getBroker().getPersistenceKey());

        PdActivitySection section = request.getPdSection();
        PdActivityCourse course = section.getPdCourse();

        activity.setStaffOid(request.getStaffOid());
        activity.setPdPlanOid(request.getPdPlanOid());
        activity.setPdCourseOid(course.getOid());
        activity.setPdSectionOid(request.getPdSectionOid());

        activity.setTitle(course.getTitle());
        activity.setDescription(course.getDescription());
        activity.setStartDate(section.getStartDate());
        activity.setContentArea(course.getContentArea());
        activity.setProvider(course.getProvider());
        activity.setCost(course.getCost());
        activity.setFundingSource(course.getFundingSource());

        getBroker().saveBeanForced(activity);

        int newEnrollmentTotal = PdScheduleManager.getEnrollmentTotal(section.getOid(), getBroker());
        if (!allowOverMax && newEnrollmentTotal > section.getEnrollmentMax()) {
            activity = null;
            addValidationError(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                    Integer.valueOf(BusinessRules.PD_SECTION_ENROLLMENT_MAX)));
        }

        return activity;
    }

    /**
     * Returns the request object corresponding to the passed workflow instance.
     *
     * @param workflow Workflow
     * @return StaffPdActivityRequest
     */
    private StaffPdActivityRequest getRequest(Workflow workflow) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(WorkflowProgress.COL_WORKFLOW_PHASE_OID,
                workflow.getWorkflowDefinition().getFirstWorkflowPhaseOid());
        criteria.addEqualTo(WorkflowProgress.COL_WORKFLOW_OID, workflow.getOid());

        QueryByCriteria query = new QueryByCriteria(WorkflowProgress.class, criteria);

        WorkflowProgress progress = (WorkflowProgress) getBroker().getBeanByQuery(query);

        FormInstance formInstance = progress.getFormInstance(getBroker());

        return (StaffPdActivityRequest) formInstance.getStorageObject(getBroker());
    }

    /**
     * Sends an e-mail to the passed staff member with the passed subject and message body. The
     * e-mail is sent to the address in the Person email01 field. If this value has not been set,
     * or does not contain a valid e-mail address, the message is not sent.
     *
     * @param staff SisStaff
     * @param messageSubject String
     * @param messageBody String
     */
    private void sendEmail(SisStaff staff, String messageSubject, String messageBody) {
        String emailAddress = staff.getPerson().getEmail01();
        if (emailAddress != null && EmailManager.validateEmailAddress(emailAddress)) {
            WriteEmailManager emailManager =
                    new WriteEmailManager(getOrganization(), getCurrentOwnerOid(), getCurrentOwnerType(), getUser());
            if (emailManager.connect()) {
                try {
                    MessageProperties message = new MessageProperties(emailAddress,
                            null,
                            emailManager.getUsername(),
                            messageSubject,
                            messageBody);
                    emailManager.sendMail(message);
                } finally {
                    emailManager.disconnect();
                }
            }
        }
    }
}
