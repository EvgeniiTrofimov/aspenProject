/* #PROCEDURE-ID [SYS-SPED-AMEND] */
/*
 *
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2016 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.procedures.sys.sped.il;

import static com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure.EXTENDED_DICTIOANRY_ID_SPED_IL_IEP;
import static com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure.NUMBER_OF_PHASE_FIRST;
import com.follett.fsc.core.framework.persistence.X2Criteria;
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
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure.MeetingTypes;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepMeeting.TypeCode;
import com.x2dev.sis.model.beans.IepMeetingAttendance;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.MeetingAttendanceManager;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehavior;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehaviorFactory;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

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
    private static final String ALIGN_NOTES3 = "Notes3";
    private static final String ALIGN_H_MEET3 = "HMeet3";
    private static final String ALIAS_MTG_TYPE = "mtg-type";
    private static final String EMPTY = "";
    private static final long serialVersionUID = 1L;
    private SpedWorkflowBehavior m_behavior = null;
    private SpedIlWorkflowCommonProcedure m_ilWorkflowHelper = null;
    private Map<String, DataDictionary> m_iepDictionaryMap = new HashMap<String, DataDictionary>();

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
        m_ilWorkflowHelper = new SpedIlWorkflowCommonProcedure(this, definition, district, user, broker, locale);
    }

    /**
     * Create new Form Instance when Re-Evaluation Workflow is created and fill it by data from Old
     * FormInstance.
     *
     * @param owner IepData
     * @param student SisStudent
     */
    public void addFormInstances(IepData owner, SisStudent student) {
        m_ilWorkflowHelper.addInd13FormInstances(owner, student);
    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#completePhase(com.follett.fsc.core.k12.beans.WorkflowProgress)
     */
    @Override
    public void completePhase(WorkflowProgress progress) {
        IepData iepData = (IepData) progress.getWorkflow().getOwner();
        prepareFormInstance(iepData, progress.getWorkflowPhase().getSequenceNumber());
        super.completePhase(progress);
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
        m_ilWorkflowHelper.setMeeitngType(progress, MeetingTypes.AMENDMENT.toString());
    }


    /**
     * Execute schedule amendment meeting.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void executeScheduleAmendmentMeeting(WorkflowProgress progress) throws Exception {
        m_ilWorkflowHelper.setMeeitngType(progress, MeetingTypes.AMENDMENT.toString());
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
     * Change IEP meeting type on workflow converting.
     *
     * @param workflow Workflow
     * @param broker X2Broker
     */
    @Override
    // * this is feature for b-5-5 and biggest version
    public void executeOnConvert(Workflow workflow, X2Broker broker) {
        X2BaseBean owner = workflow.getOwner();
        if (owner instanceof IepData) {
            IepData iepData = (IepData) owner;
            m_ilWorkflowHelper.setTypeToIep(iepData, IepMeeting.TypeCode.AMENDMENT);
            List<IepMeeting> meetings = setAmendmentTypeToMeeting(workflow);
            m_ilWorkflowHelper.deleteOtherMeeting(iepData, meetings);
            m_ilWorkflowHelper.alignDifFormDefinition(workflow, new ArrayList(Arrays.asList(ALIGN_NOTES3)));
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

        IepData owner = m_behavior.executeCreateAmendmentDraft((SisStudent) selectionBean, new PlainDate(getTimeZone()),
                errors, getBroker());
        TypeCode meetingType = MeetingAttendanceManager.getMeetingType(workflowDefinition,
                PreferenceManager.getPreferenceSet(getOrganization()));

        owner.setMeetingTypeCodeEnum(meetingType);
        getBroker().saveBeanForced(owner);
        fixIepMeetingAttendanceReferences(owner, errors);

        addFormInstances(owner, (SisStudent) selectionBean);

        addValidationErrors(errors);

        return owner;
    }


    /**
     * @see com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#initializeForm(com.follett.fsc.core.k12.beans.FormInstance,
     *      com.follett.fsc.core.k12.beans.X2BaseBean,
     *      com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    public void initializeForm(FormInstance formInstance, X2BaseBean formStorage, UserDataContainer userData)
            throws X2BaseException {
        super.initializeForm(formInstance, formStorage, userData);
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
        m_ilWorkflowHelper.rollbackMeeitngType(progress, null);
    }

    /**
     * Rollback schedule amendment meeting.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     */
    public void rollbackScheduleAmendmentMeeting(WorkflowProgress progress) throws Exception {
        m_ilWorkflowHelper.rollbackMeeitngType(progress, MeetingTypes.AMENDMENT.toString());
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
     * Determine pair meeting.
     *
     * @param newIep IepData
     * @param wrongMeeting IepMeeting
     * @return IepMeeting
     */
    private IepMeeting determinePairMeeting(IepData newIep, IepMeeting wrongMeeting) {
        IepMeeting pairMeeting = null;
        String wrongKey = getBeanKey(wrongMeeting);
        for (IepMeeting iepMeeting : newIep.getIepMeeting()) {
            String currentKey = getBeanKey(iepMeeting);
            if (wrongKey.equals(currentKey)) {
                pairMeeting = iepMeeting;
                break;
            }
        }
        return pairMeeting;
    }

    /**
     * Determine pair team member.
     *
     * @param newIep IepData
     * @param wrongTeamMember IepTeamMember
     * @return IepTeamMember
     */
    private IepTeamMember determinePairTeamMember(IepData newIep, IepTeamMember wrongTeamMember) {
        IepTeamMember iepTeamMember = null;
        String wrongKey = getBeanKey(wrongTeamMember);
        for (IepTeamMember teamMember : newIep.getTeamMembers()) {
            String currentKey = getBeanKey(teamMember);
            if (currentKey.equals(wrongKey)) {
                iepTeamMember = teamMember;
                break;
            }
        }
        return iepTeamMember;
    }

    /**
     * Fix iep meeting attendance references.
     *
     * @param newIep IepData
     * @param errors LinkedList<ValidationError>
     */
    private void fixIepMeetingAttendanceReferences(IepData newIep, LinkedList<ValidationError> errors) {
        Criteria criteria = new X2Criteria();
        criteria.addEqualTo(IepMeetingAttendance.COL_IEP_DATA_OID, newIep.getOid());
        Collection<IepMeetingAttendance> attendances =
                getBroker().getCollectionByQuery(new QueryByCriteria(IepMeetingAttendance.class, criteria));
        Iterator<IepMeetingAttendance> iterator = attendances.iterator();
        while (iterator.hasNext()) {
            IepMeetingAttendance attendance = iterator.next();
            IepMeeting attMeeting = attendance.getIepMeeting();
            if (attMeeting != null) {
                IepData meetingIepData = attMeeting.getIepData();
                String iepDataOid = meetingIepData.getOid();
                if (!iepDataOid.equals(newIep.getOid())) {
                    IepTeamMember wrongTeamMember = attendance.getTeamMember();
                    IepMeeting pairMeeting = determinePairMeeting(newIep, attMeeting);
                    IepTeamMember pairMember = determinePairTeamMember(newIep, wrongTeamMember);
                    if (pairMeeting != null && pairMember != null) {
                        attendance.setIepMeetingOid(pairMeeting.getOid());
                        attendance.setTeamMemberOid(pairMember.getOid());
                        errors.addAll(getBroker().saveBean(attendance));
                    }
                }
            } else {
                iterator.remove();
                getBroker().deleteBean(attendance);
            }
        }

    }


    /**
     * Gets the bean key.
     *
     * @param iepMeeting IepMeeting
     * @return String
     */
    private String getBeanKey(IepMeeting iepMeeting) {
        String returnValue = null;
        if (iepMeeting != null) {
            DataDictionary iepDictionary = getIepDataDictionary(iepMeeting.getIepData());
            returnValue = (String) iepMeeting.getFieldValueByAlias(ALIAS_MTG_TYPE, iepDictionary);
        }
        return returnValue == null ? EMPTY : returnValue;
    }

    /**
     * Gets the bean key.
     *
     * @param teamMember IepTeamMember
     * @return String
     */
    private String getBeanKey(IepTeamMember teamMember) {
        String returnValue = null;
        if (teamMember != null) {
            String person = teamMember.getPersonOid();
            person = person == null ? EMPTY : person;
            String role = teamMember.getMemberRoleCode();
            role = role == null ? EMPTY : role;
            returnValue = person + role;
        }
        return returnValue == null ? EMPTY : returnValue;

    }

    /**
     * Gets the iep data dictionary.
     *
     * @param iepData IepData
     * @return Data dictionary
     */
    private DataDictionary getIepDataDictionary(IepData iepData) {
        DataDictionary dictionary = null;

        SisOrganization district = iepData.getStudent().getSchool().getOrganization1();
        String key = district.getOid();
        dictionary = m_iepDictionaryMap.get(key);
        if (dictionary == null) {
            ExtendedDataDictionary iepExtendedDictionary = SpedUtils.getIepDictionary(district, getBroker());
            dictionary = DataDictionary.getDistrictDictionary(iepExtendedDictionary, getBroker().getPersistenceKey());
            m_iepDictionaryMap.put(key, dictionary);
        }
        return dictionary;
    }

    /**
     * Prepare form instance.
     *
     * @param iepData IepData
     * @param numberOfPhase int
     */
    private void prepareFormInstance(IepData iepData, int numberOfPhase) {
        if (numberOfPhase == NUMBER_OF_PHASE_FIRST) {
            m_ilWorkflowHelper.fill3457BC(iepData);
            m_ilWorkflowHelper.fill3457C(iepData);
            m_ilWorkflowHelper.fillSpAppr(iepData);
            m_ilWorkflowHelper.fill3454I(iepData);
            m_ilWorkflowHelper.fillIndicator(iepData);
            m_ilWorkflowHelper.fillSumPref(iepData);
            m_ilWorkflowHelper.fill3454V(iepData);
        }
    }

    /**
     * start using from b-5-5 version
     * set AnnualReview meeting type to meeting which belong to forminstance where outcome has
     * HMeet3 align.
     *
     * @param workflow Workflow
     * @return List
     */
    private List<IepMeeting> setAmendmentTypeToMeeting(Workflow workflow) {
        List<IepMeeting> iepMeetings = new ArrayList<IepMeeting>();
        DataDictionary ddx = m_ilWorkflowHelper.getDictionaryByExtendedDictionaryId(EXTENDED_DICTIOANRY_ID_SPED_IL_IEP);
        IepMeeting iepMeeting = m_ilWorkflowHelper.setMeetingTypeIntoMeeting(workflow, ddx, ALIGN_H_MEET3,
                IepMeeting.TypeCode.AMENDMENT, MeetingTypes.AMENDMENT.toString());
        if (iepMeeting != null) {
            iepMeetings.add(iepMeeting);
            if (iepMeeting.isDirty()) {
                getBroker().saveBeanForced(iepMeeting);
            }
        }

        return iepMeetings;
    }



}
