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
import static com.x2dev.sis.model.business.conduct.ConductAliases.*;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.adjusters.CaseInsensitiveAdjuster;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.workflow.WorkflowManager;
import com.follett.fsc.core.k12.tools.procedures.SessionAwareProcedure;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.workflow.SessionAccessManager;
import com.follett.fsc.core.k12.web.workflow.WorkflowUtils;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductActionDate;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisWorkflow;
import com.x2dev.sis.model.business.ConductManager;
import com.x2dev.sis.model.business.conduct.ConductAttendanceHelper;
import com.x2dev.sis.web.conduct.ConductHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.converters.DecimalAsStringConverter;
import com.x2dev.utils.converters.TimeAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure for the conduct referral, portal conduct referral, and conduct investigation workflows.
 * This class is responsible for creating <code>ConductIncident</code> and
 * <code>ConductAction</code> records based on the referral and action forms collected in the
 * workflow.
 *
 * @author X2 Development Corporation
 */
public class ConductReferralProcedure extends WorkflowProcedure implements SessionAwareProcedure {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private DateAsStringConverter m_dateConverter;
    private DecimalAsStringConverter m_decimalConverter;
    private TimeAsStringConverter m_timeConverter;
    private UserDataContainer m_userData;

    /**
     * Constructs a new ConductReferralProcedure.
     *
     * @param definition WorkflowDefinition
     * @param district Organization
     * @param user User
     * @param broker X2Broker
     * @param locale Locale
     */
    public ConductReferralProcedure(WorkflowDefinition definition, Organization district, User user,
            X2Broker broker, Locale locale) {
        super(definition, district, user, broker, locale);
    }

    /**
     * Called at the completion of the portal conduct referral workflow, this method will create
     * a new conduct referral workflow and complete the first phase by using values retrieved
     * in the last phase of the portal conduct referral workflow.
     *
     * @param progress WorkflowProgress
     */
    public void executeInitiateNextWorkflow(WorkflowProgress progress) {
        FormInstance reviewForm = WorkflowUtils.getFormInstance(progress.getWorkflow(), REVIEW_FORM_ID, getBroker());
        GenericFormData reviewFormData = (GenericFormData) reviewForm.getStorageObject();

        DataDictionary reviewDictionary =
                DataDictionary.getDistrictDictionary(reviewForm.getFormDefinition().getExtendedDataDictionary(),
                        getBroker().getPersistenceKey());

        String selectionOid = reviewFormData.getFieldValueByAlias(ALIAS_OFFENDER_OID, reviewDictionary).toString();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(X2BaseBean.COL_OID, WORKFLOW_DEFINITION_OID_CND_REF);
        WorkflowDefinition cndRefDef = (WorkflowDefinition) getBroker().getBeanByQuery(
                new QueryByCriteria(WorkflowDefinition.class, criteria));
        WorkflowPhaseOutcome cndRefFirstOutcome = cndRefDef.getFirstWorkflowPhase().getStandardPhaseOutcome();

        FormInstance cndRefFormInstance = createAndSaveCndRefFormInstance(reviewFormData, progress, reviewDictionary);
        Map<String, FormInstance> cndRefFormInstances = new HashMap<String, FormInstance>(2);
        cndRefFormInstances.put(REFERRAL_FORM_ID, cndRefFormInstance);

        WorkflowManager workflowManager = new WorkflowManager(getBroker());
        try {
            Workflow workflow = workflowManager.initiateWorkflow(selectionOid,
                    cndRefFirstOutcome, cndRefFormInstances, m_userData, getPlainDate(),
                    progress.getWorkflow().getFirstWorkflowProgress(getBroker()).getAnonymousIndicator(),
                    getLocale(), new SessionAccessManager(m_userData, getBroker()));

            // set the workflow oid alias to link in case of rollback
            reviewFormData.setFieldValueByAlias(ALIAS_NEXT_WORKFLOW_OID, workflow.getOid(),
                    reviewDictionary);
            getBroker().saveBeanForced(reviewFormData);
        } catch (X2BaseException e) {
            AppGlobals.getLog().log(Level.SEVERE, e.getMessage(), e);
        }
    }

    /**
     * Called at the completion of the conduct referral workflow, this method will create a new
     * conduct investigation workflow and completes the first phase.
     *
     * @param progress WorkflowProgress
     */
    public void executeInitiateInvestigation(WorkflowProgress progress) {
        FormInstance cndRefForm = WorkflowUtils.getFormInstance(progress.getWorkflow(), REFERRAL_FORM_ID, getBroker());
        GenericFormData cndRefFormData = (GenericFormData) cndRefForm.getStorageObject();
        DataDictionary referralDictionary =
                DataDictionary.getDistrictDictionary(cndRefForm.getFormDefinition().getExtendedDataDictionary(),
                        getBroker().getPersistenceKey());

        String selectionOid = progress.getWorkflow().getOwnerOid();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(X2BaseBean.COL_OID, WORKFLOW_DEFINITION_OID_CND_INV);
        WorkflowDefinition cndInvDef = (WorkflowDefinition) getBroker().getBeanByQuery(
                new QueryByCriteria(WorkflowDefinition.class, criteria));
        WorkflowPhaseOutcome cndInvFirstOutcome = cndInvDef.getFirstWorkflowPhase().getStandardPhaseOutcome();

        GenericFormData cndInvFormData = (GenericFormData) cndRefFormData.copyBean();
        getBroker().saveBeanForced(cndInvFormData);

        FormInstance cndInvFormInstance = null;
        cndInvFormInstance = X2BaseBean.newInstance(FormInstance.class, getBroker().getPersistenceKey());
        cndInvFormInstance.setFormDefinitionOid(cndRefForm.getFormDefinitionOid());
        cndInvFormInstance.setOwnerObjectOid(progress.getUserOid());
        cndInvFormInstance.setStorageObjectOid(cndInvFormData.getOid());
        cndInvFormInstance.setCreatedTime(System.currentTimeMillis());
        getBroker().saveBeanForced(cndInvFormInstance);

        Map<String, FormInstance> cndInvFormInstances = new HashMap<String, FormInstance>(2);
        cndInvFormInstances.put(REFERRAL_FORM_ID, cndInvFormInstance);

        WorkflowManager workflowManager = new WorkflowManager(getBroker());
        try {
            Workflow workflow = workflowManager.initiateWorkflow(selectionOid, cndInvFirstOutcome,
                    cndInvFormInstances, m_userData, getPlainDate(), false, getLocale(),
                    new SessionAccessManager(m_userData, getBroker()));

            cndRefFormData.setFieldValueByAlias(ALIAS_NEXT_WORKFLOW_OID, workflow.getOid(), referralDictionary);
            getBroker().saveBeanForced(cndRefFormData);
        } catch (X2BaseException e) {
            AppGlobals.getLog().log(Level.SEVERE, e.getMessage(), e);
        }
    }

    /**
     * Creates and posts actions to the already created incident.
     *
     * @param progress WorkflowProgress
     */
    public void executePostActions(WorkflowProgress progress) {
        createAndSaveActions(((SisWorkflow) progress.getWorkflow()).getConductIncident(), progress);
    }

    /**
     * Creates an incident and related actions executed when the conduct referral is posted.
     *
     * @param progress WorkflowProgress
     */
    public void executePostConduct(WorkflowProgress progress) {
        ConductIncident incident = ((SisWorkflow) progress.getWorkflow()).getConductIncident();

        // Only make a new incident if the incident has not yet been created.
        if (incident == null) {
            // Required converters
            m_dateConverter = (DateAsStringConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER,
                    getLocale(), true);
            m_timeConverter = (TimeAsStringConverter) ConverterFactory.getConverterForClass(Converter.TIME_CONVERTER,
                    getLocale(), true);
            m_decimalConverter = (DecimalAsStringConverter) ConverterFactory
                    .getConverterForClass(Converter.BIG_DECIMAL_CONVERTER, getLocale(), true);

            incident = createAndSaveIncident(progress);
            createAndSaveActions(incident, progress);
        } else {
            setInvestigationStatus(incident, progress.getWorkflowPhaseOutcomeOid());
        }
    }

    /**
     * Sets the investigation status for the related conduct incident (if it exists) based upon the
     * outcome chosen.
     *
     * @param progress WorkflowProgress
     */
    public void executeSetInvestigationStatus(WorkflowProgress progress) {
        ConductIncident incident = ((SisWorkflow) progress.getWorkflow()).getConductIncident();
        if (incident != null) {
            setInvestigationStatus(incident, progress.getWorkflowPhaseOutcomeOid());
            if (incident.isDirty()) {
                getBroker().saveBeanForced(incident);
            }
        }
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
        if (REVIEW_FORM_ID.equals(formInstance.getFormDefinition().getId())) {
            initializeCndRevForm(formInstance, formStorage, userData);
        } else if (SAFETY_FORM_ID.equals(formInstance.getFormDefinition().getId())) {
            initializeCndSftForm(formInstance, formStorage, userData);
        } else if (INVESTIGATION_FORM_ID.equals(formInstance.getFormDefinition().getId())) {
            initializeCndBlyForm(formInstance, formStorage, userData);
        } else if (REFERRAL_FORM_ID.equals(formInstance.getFormDefinition().getId())
                && formInstance.getOwnerObjectOid() != null) {
            initializeCndRevForm(formInstance, formStorage, userData);
        }
    }

    /**
     * Checks if is dynamic access.
     *
     * @param progress WorkflowProgress
     * @return true, if is dynamic access
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#isDynamicAccess(com.follett.fsc.
     *      core.k12.beans.WorkflowProgress)
     */
    @Override
    public boolean isDynamicAccess(WorkflowProgress progress) {
        /*
         * Makes sure when looped back to phase one that only the initiator has access
         */
        return WORKFLOW_PHASE_OID_PTL_REF.equals(progress.getWorkflowPhaseOid()) &&
                !progress.getWorkflow().getWorkflowProgress().isEmpty();
    }

    /**
     * Resolve dynamic access.
     *
     * @param progress WorkflowProgress
     * @return Collection
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#resolveDynamicAccess(com.follett.
     *      fsc.core.k12.beans.WorkflowProgress)
     */
    @Override
    public Collection<WorkflowParticipant> resolveDynamicAccess(WorkflowProgress progress) {
        Collection<WorkflowParticipant> participants = new ArrayList<WorkflowParticipant>(1);

        WorkflowParticipant participant = null;
        participant = X2BaseBean.newInstance(WorkflowParticipant.class, getBroker().getPersistenceKey());
        participant.setUserOid(progress.getWorkflow().getUserOid());
        participant.setWorkflowPhaseOid(progress.getWorkflowPhaseOid());
        participant.setTypeEnum(WorkflowParticipant.Type.SINGLE_USER);

        participants.add(participant);

        return participants;
    }

    /**
     * Sets the investigation status for the related conduct incident to "Initiated". Does not
     * delete the incident record if investigation status is not N/A.
     *
     * @param progress WorkflowProgress
     */
    public void rollbackPostConduct(WorkflowProgress progress) {
        ConductIncident incident = ((SisWorkflow) progress.getWorkflow()).getConductIncident();
        if (incident != null) {
            if (ConductIncident.investigationStatus.INVESTIGATION_STATUS_NA.ordinal() == incident.getInvestigation()) {
                getBroker().deleteBean(incident);
            } else {
                incident.setInvestigation(ConductIncident.investigationStatus.INVESTIGATION_STATUS_INITIATED.ordinal());
                getBroker().saveBeanForced(incident);
            }
        }
    }

    /**
     * Sets the investigation status for the related conduct incident (if it exists) to "Initiated".
     *
     * @param progress WorkflowProgress
     */
    public void rollbackSetInvestigationStatus(WorkflowProgress progress) {
        ConductIncident incident = ((SisWorkflow) progress.getWorkflow()).getConductIncident();
        if (incident != null) {
            incident.setInvestigation(ConductIncident.investigationStatus.INVESTIGATION_STATUS_INITIATED.ordinal());
            getBroker().saveBeanForced(incident);
        }
    }

    /**
     * Deletes the workflow created in <code>executeInitiateInvestigation</code>.
     *
     * @param progress WorkflowProgress
     * @param formId String
     */
    public void rollbackWorkflow(WorkflowProgress progress, String formId) {
        FormInstance cndRefForm = WorkflowUtils.getFormInstance(progress.getWorkflow(), formId, getBroker());
        GenericFormData cndRefFormData = (GenericFormData) cndRefForm.getStorageObject();
        DataDictionary referralDictionary =
                DataDictionary.getDistrictDictionary(cndRefForm.getFormDefinition().getExtendedDataDictionary(),
                        getBroker().getPersistenceKey());

        String nextWorkflowOid =
                cndRefFormData.getFieldValueByAlias(ALIAS_NEXT_WORKFLOW_OID, referralDictionary).toString();

        getBroker().deleteBeanByOid(Workflow.class, nextWorkflowOid);
    }

    /**
     * Sets the user data.
     *
     * @param userData void
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.SessionAwareProcedure#setUserData(com.follett.fsc.
     *      core.k12.web.UserDataContainer)
     */
    @Override
    public void setUserData(UserDataContainer userData) {
        m_userData = userData;
    }

    /**
     * Creates action records for the passed incident and saves them.
     *
     * @param incident ConductIncident
     * @param progress WorkflowProgress
     */
    private void createAndSaveActions(ConductIncident incident, WorkflowProgress progress) {
        X2Broker broker = getBroker();
        FormInstance actionForm = progress.getFormInstanceById(ACTION_FORM_ID, broker);

        // Actions
        if (actionForm != null) {
            GenericFormData actionData = (GenericFormData) actionForm.getStorageObject(broker);
            DataDictionary actionDictionary = DataDictionary.getDistrictDictionary(
                    actionForm.getFormDefinition().getExtendedDataDictionary(), broker.getPersistenceKey());

            // Required converters
            m_dateConverter = (DateAsStringConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER,
                    getLocale(), true);
            m_timeConverter = (TimeAsStringConverter) ConverterFactory.getConverterForClass(Converter.TIME_CONVERTER,
                    getLocale(), true);
            m_decimalConverter = (DecimalAsStringConverter) ConverterFactory
                    .getConverterForClass(Converter.BIG_DECIMAL_CONVERTER, getLocale(), true);

            for (GenericFormChildData child : actionData.getGenericFormDataChildren(broker)) {
                String actionCode = (String) child.getFieldValueByAlias(ALIAS_ACTION_CODE, actionDictionary);
                String actionStartDate = (String) child.getFieldValueByAlias(ALIAS_ACTION_START_DATE, actionDictionary);
                String actionEndDate = (String) child.getFieldValueByAlias(ALIAS_ACTION_END_DATE, actionDictionary);
                String penaltyTime = (String) child.getFieldValueByAlias(ALIAS_ACTION_PT, actionDictionary);
                String actionDescription =
                        (String) child.getFieldValueByAlias(ALIAS_ACTION_DESCRIPTION, actionDictionary);
                String actionDates = (String) child.getFieldValueByAlias(ALIAS_ACTION_DATES, actionDictionary);
                int actionType = ConductManager.getActionType(actionCode, broker.getPersistenceKey());

                ConductAction action = new ConductAction(broker.getPersistenceKey());
                action.setSchoolOid(incident.getStudent().getSchoolOid());
                action.setStudentOid(incident.getStudentOid());
                action.setIncidentOid(incident.getOid());
                action.setActionCode(actionCode);
                action.setActionStartDate((PlainDate) m_dateConverter.parseSystemString(actionStartDate));
                action.setActionEndDate((PlainDate) m_dateConverter.parseSystemString(actionEndDate));
                action.setActionPenaltyTime((BigDecimal) m_decimalConverter.parseSystemString(penaltyTime));
                action.setDescription(actionDescription);

                broker.saveBeanForced(action);

                if (actionType > 0) {
                    ConductAttendanceHelper conductAttHelper = new ConductAttendanceHelper(broker, action);
                    SimpleDateFormat format = new SimpleDateFormat(ConductHelper.DATE_KEY_FORMAT);
                    String[] selectedDays = actionDates.split(",");
                    for (String day : selectedDays) {
                        if (!StringUtils.isEmpty(day)) {
                            try {
                                PlainDate selectedDate = new PlainDate(format.parse(day));

                                ConductActionDate actionDate = new ConductActionDate(broker.getPersistenceKey());
                                actionDate.setActionOid(action.getOid());
                                actionDate.setStudentOid(action.getStudentOid());
                                actionDate.setDate(selectedDate);
                                actionDate.setSchedDate(selectedDate);
                                actionDate.setType(actionType);

                                broker.saveBeanForced(actionDate);

                                List<ValidationError> errors = conductAttHelper.createRelatedAttendances(actionDate,
                                        m_userData);
                                addValidationErrors(errors);

                            } catch (ParseException e) {
                                AppGlobals.getLog().log(Level.SEVERE, e.getMessage(), e);
                            }
                        }
                    }
                }
            }
        }
    }


    /**
     * Creates a GenericFormData object and sets its values for the conduct referral form based
     * on values taken from the storage object from the portal review form.
     *
     * @param reviewFormData GenericFormData
     * @param progress WorkflowProgress
     * @param reviewDictionary DataDictionary
     * @return FormInstance
     */
    private FormInstance createAndSaveCndRefFormInstance(GenericFormData reviewFormData,
                                                         WorkflowProgress progress,
                                                         DataDictionary reviewDictionary)

    {
        Object code = reviewFormData.getFieldValueByAlias(ALIAS_INCIDENT_CODE, reviewDictionary);
        Object id = reviewFormData.getFieldValueByAlias(ALIAS_INCIDENT_ID, reviewDictionary);
        Object date = reviewFormData.getFieldValueByAlias(ALIAS_INCIDENT_DATE, reviewDictionary);
        Object time = reviewFormData.getFieldValueByAlias(ALIAS_INCIDENT_TIME, reviewDictionary);
        Object location = reviewFormData.getFieldValueByAlias(ALIAS_INCIDENT_LOCATION, reviewDictionary);
        Object description = reviewFormData.getFieldValueByAlias(ALIAS_INCIDENT_DESCRIPTION, reviewDictionary);
        Object victimOid = reviewFormData.getFieldValueByAlias(ALIAS_VICTIM_OID, reviewDictionary);

        FormDefinition referralFormDef = FormDefinition.getById(REFERRAL_FORM_ID, getBroker());
        DataDictionary referralDictionary = DataDictionary.getDistrictDictionary(
                referralFormDef.getExtendedDataDictionary(), getBroker().getPersistenceKey());

        GenericFormData formStorageObject = null;
        formStorageObject = X2BaseBean.newInstance(GenericFormData.class, getBroker().getPersistenceKey());
        formStorageObject.setFieldValueByAlias(ALIAS_INCIDENT_CODE, code, referralDictionary);
        formStorageObject.setFieldValueByAlias(ALIAS_INCIDENT_ID, id, referralDictionary);
        formStorageObject.setFieldValueByAlias(ALIAS_INCIDENT_DATE, date, referralDictionary);
        formStorageObject.setFieldValueByAlias(ALIAS_INCIDENT_TIME, time, referralDictionary);
        formStorageObject.setFieldValueByAlias(ALIAS_INCIDENT_LOCATION, location, referralDictionary);
        formStorageObject.setFieldValueByAlias(ALIAS_INCIDENT_DESCRIPTION, description, referralDictionary);
        formStorageObject.setFieldValueByAlias(ALIAS_VICTIM_OID, victimOid, referralDictionary);
        getBroker().saveBeanForced(formStorageObject);

        FormInstance formInstance = null;
        formInstance = X2BaseBean.newInstance(FormInstance.class, getBroker().getPersistenceKey());
        formInstance.setFormDefinitionOid(referralFormDef.getOid());
        formInstance.setOwnerObjectOid(progress.getUserOid());
        formInstance.setStorageObjectOid(formStorageObject.getOid());
        formInstance.setCreatedTime(System.currentTimeMillis());

        getBroker().saveBeanForced(formInstance);

        return formInstance;
    }


    /**
     * Instantiates the ConductIncident record and returns it.
     *
     * @param progress WorkflowProgress
     * @return ConductIncident
     */
    private ConductIncident createAndSaveIncident(WorkflowProgress progress) {
        FormInstance referralForm =
                WorkflowUtils.getFormInstance(progress.getWorkflow(), REFERRAL_FORM_ID, getBroker());
        GenericFormData referralData = (GenericFormData) referralForm.getStorageObject(getBroker());
        DataDictionary referralDictionary = DataDictionary.getDistrictDictionary(
                referralForm.getFormDefinition().getExtendedDataDictionary(), getBroker().getPersistenceKey());

        // Incident fields
        String ownerOid = null;
        String referralStaffOid = null;
        String userOid = progress.getWorkflow().getUserOid();
        User user = (User) getBroker().getBeanByOid(User.class, userOid);

        if (user != null) {
            Person person = user.getPerson();
            if (person != null) {
                Staff staff = person.getStaff();
                if (staff != null) {
                    referralStaffOid = staff.getOid();
                }
            }
        }

        if (OUTCOME_OID_POST.equals(
                BeanManager.getTrimmedId(progress.getWorkflowPhaseOutcomeOid(), getBroker().getPersistenceKey()))) {
            FormInstance actionForm = progress.getFormInstanceById(ACTION_FORM_ID, getBroker());
            if (actionForm != null) {
                GenericFormData actionData = (GenericFormData) actionForm.getStorageObject(getBroker());
                DataDictionary actionDictionary = DataDictionary.getDistrictDictionary(
                        actionForm.getFormDefinition().getExtendedDataDictionary(), getBroker().getPersistenceKey());

                ownerOid = (String) actionData.getFieldValueByAlias(ALIAS_OWNER_STAFF_OID, actionDictionary);
            }
        }

        String incidentCode = (String) referralData.getFieldValueByAlias(ALIAS_INCIDENT_CODE, referralDictionary);
        String incidentId = (String) referralData.getFieldValueByAlias(ALIAS_INCIDENT_ID, referralDictionary);
        String incidentDate = (String) referralData.getFieldValueByAlias(ALIAS_INCIDENT_DATE, referralDictionary);
        String incidentTime = (String) referralData.getFieldValueByAlias(ALIAS_INCIDENT_TIME, referralDictionary);
        String incidentLocation =
                (String) referralData.getFieldValueByAlias(ALIAS_INCIDENT_LOCATION, referralDictionary);
        String incidentDescription =
                (String) referralData.getFieldValueByAlias(ALIAS_INCIDENT_DESCRIPTION, referralDictionary);
        String incidentVictimOid = (String) referralData.getFieldValueByAlias(ALIAS_VICTIM_OID, referralDictionary);

        /*
         * If this is a re-post then we will clear the existing incident and the associated actions.
         */
        if (!StringUtils.isEmpty(incidentId)) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ConductIncident.COL_STUDENT_OID, progress.getWorkflow().getOwnerOid());
            criteria.addEqualTo(ConductIncident.COL_INCIDENT_ID, incidentId);

            QueryByCriteria query = new QueryByCriteria(ConductIncident.class, criteria);

            Collection<ConductIncident> incidents = getBroker().getCollectionByQuery(query);
            for (ConductIncident incident : incidents) {
                getBroker().deleteBean(incident);
            }

            incidents.clear();
            incidents = null;
        }

        ConductIncident incident = new ConductIncident(getBroker().getPersistenceKey());

        incident.setStudentOid(progress.getWorkflow().getOwnerOid());
        incident.setReferralStaffOid(referralStaffOid);
        incident.setOwnerOid(ownerOid);
        incident.setIncidentCode(incidentCode);
        incident.setIncidentId(incidentId);
        incident.setIncidentDate(
                incidentDate == null ? null : (PlainDate) m_dateConverter.parseSystemString(incidentDate));
        incident.setIncidentTime(
                incidentTime == null ? null : (PlainTime) m_timeConverter.parseSystemString(incidentTime));
        incident.setIncidentLocation(incidentLocation);
        incident.setDescription(incidentDescription);
        incident.setSchoolOid(((Student) progress.getWorkflow().getOwner()).getSchoolOid());
        incident.setVictimOid(incidentVictimOid);
        incident.setWorkflowOid(progress.getWorkflowOid());

        setInvestigationStatus(incident, progress.getWorkflowPhaseOutcomeOid());

        getBroker().saveBeanForced(incident);

        return incident;
    }

    /**
     * Returns the OID of the student whose name matches the referred name in the following formats:
     * [(FirstName LastName), (LastName, FirstName)]. If more than one student matches, then neither
     * is returned
     *
     * @param referredStudent String
     * @return String
     */
    private String getStudentOid(String referredStudent) {
        String studentOid = null;

        try {
            if (referredStudent != null) {
                Criteria criteria = StudentManager.buildStudentSearchCriteria(referredStudent);
                SubQuery query = new SubQuery(Student.class, X2BaseBean.COL_OID, criteria);

                CaseInsensitiveAdjuster caseAdjuster = new CaseInsensitiveAdjuster(getBroker().getPersistenceKey());
                query.addQueryAdjuster(caseAdjuster);

                if (getBroker().getCount(query) == 1) {
                    studentOid = getBroker().getSubQueryValueByQuery(query).toString();
                }
            }
        } catch (Exception e) {
            AppGlobals.getLog().log(Level.SEVERE, e.getMessage(), e);
        }

        return studentOid;
    }

    /**
     * Initializes the Investigation form.
     *
     * @param formInstance FormInstance
     * @param formStorage X2BaseBean
     * @param userData UserDataContainer
     */
    private void initializeCndBlyForm(FormInstance formInstance,
                                      X2BaseBean formStorage,
                                      UserDataContainer userData) {
        FormInstance referralForm = WorkflowUtils.getFormInstance(
                userData.getCurrentWorkflow(), REFERRAL_FORM_ID, getBroker());
        GenericFormData referralData = (GenericFormData) referralForm.getStorageObject(getBroker());
        DataDictionary referralDictionary = DataDictionary.getDistrictDictionary(
                referralForm.getFormDefinition().getExtendedDataDictionary(), getBroker().getPersistenceKey());

        Object offenderOid = referralData.getFieldValueByAlias(ALIAS_OFFENDER_OID, referralDictionary);
        Object victimOid = referralData.getFieldValueByAlias(ALIAS_VICTIM_OID, referralDictionary);

        DataDictionary invDictionary = DataDictionary.getDistrictDictionary(
                formInstance.getFormDefinition().getExtendedDataDictionary(), getBroker().getPersistenceKey());

        if (offenderOid != null) {
            Student offender = (Student) getBroker().getBeanByOid(Student.class, offenderOid.toString());
            formStorage.setFieldValueByAlias(ALIAS_VICTIM_NAME, StringUtils.truncate(offender.getNameView(),
                    invDictionary.findDataDictionaryFieldByAlias(ALIAS_VICTIM_NAME).getDatabaseLength()),
                    invDictionary);

        }

        if (victimOid != null) {
            Student victim = (Student) getBroker().getBeanByOid(Student.class, victimOid.toString());
            formStorage.setFieldValueByAlias(ALIAS_VICTIM_NAME, StringUtils.truncate(victim.getNameView(),
                    invDictionary.findDataDictionaryFieldByAlias(ALIAS_VICTIM_NAME).getDatabaseLength()),
                    invDictionary);
        }

        getBroker().saveBeanForced(formStorage);
    }

    /**
     * Initializes the Review referral form.
     *
     * @param formInstance FormInstance
     * @param formStorage X2BaseBean
     * @param userData UserDataContainer
     */
    private void initializeCndRevForm(FormInstance formInstance, X2BaseBean formStorage, UserDataContainer userData) {
        FormInstance referralForm = WorkflowUtils.getFormInstance(
                userData.getCurrentWorkflow(), PORTAL_REFERRAL_FORM_ID, getBroker());
        if (referralForm == null) {
            referralForm = WorkflowUtils.getFormInstance(
                    userData.getCurrentWorkflow(), REFERRAL_FORM_ID, getBroker());
        }

        if (referralForm != null) {
            GenericFormData referralData = (GenericFormData) referralForm.getStorageObject(getBroker());
            DataDictionary referralDictionary = DataDictionary.getDistrictDictionary(
                    referralForm.getFormDefinition().getExtendedDataDictionary(), getBroker().getPersistenceKey());

            Object id = referralData.getFieldValueByAlias(ALIAS_INCIDENT_ID, referralDictionary);
            Object date = referralData.getFieldValueByAlias(ALIAS_INCIDENT_DATE, referralDictionary);
            Object time = referralData.getFieldValueByAlias(ALIAS_INCIDENT_TIME, referralDictionary);
            Object location = referralData.getFieldValueByAlias(ALIAS_INCIDENT_LOCATION, referralDictionary);
            Object description = referralData.getFieldValueByAlias(ALIAS_INCIDENT_DESCRIPTION, referralDictionary);
            Object offender = referralData.getFieldValueByAlias(ALIAS_OFFENDER_NAME, referralDictionary);
            Object victim = referralData.getFieldValueByAlias(ALIAS_VICTIM_NAME, referralDictionary);
            Object offenderOid = referralData.getFieldValueByAlias(ALIAS_VICTIM_OID, referralDictionary);
            Object victimOid = null;
            if (userData.getCurrentWorkflow() != null) {
                victimOid = userData.getCurrentWorkflow().getOwnerOid();
            }
            DataDictionary reviewDictionary = DataDictionary.getDistrictDictionary(
                    formInstance.getFormDefinition().getExtendedDataDictionary(), getBroker().getPersistenceKey());

            formStorage.setFieldValueByAlias(ALIAS_INCIDENT_ID, id, reviewDictionary);
            formStorage.setFieldValueByAlias(ALIAS_INCIDENT_DATE, date, reviewDictionary);
            formStorage.setFieldValueByAlias(ALIAS_INCIDENT_TIME, time, reviewDictionary);
            formStorage.setFieldValueByAlias(ALIAS_INCIDENT_LOCATION, location, reviewDictionary);
            formStorage.setFieldValueByAlias(ALIAS_INCIDENT_DESCRIPTION, description, reviewDictionary);
            if (victim != null) {
                formStorage.setFieldValueByAlias(ALIAS_VICTIM_NAME, victim, reviewDictionary);
                formStorage.setFieldValueByAlias(ALIAS_VICTIM_OID, getStudentOid(victim.toString()), reviewDictionary);
            } else {
                formStorage.setFieldValueByAlias(ALIAS_VICTIM_OID, victimOid, reviewDictionary);
            }
            if (offender != null) {
                formStorage.setFieldValueByAlias(ALIAS_OFFENDER_NAME, offender, reviewDictionary);
                formStorage.setFieldValueByAlias(ALIAS_OFFENDER_OID, getStudentOid(offender.toString()),
                        reviewDictionary);
            } else {
                formStorage.setFieldValueByAlias(ALIAS_OFFENDER_OID, offenderOid, reviewDictionary);
                if (userData.getCurrentWorkflow() != null) {
                    userData.getCurrentWorkflow().setOwnerOid((String) offenderOid);
                }
                getBroker().saveBeanForced(userData.getCurrentWorkflow());
            }

            getBroker().saveBeanForced(formStorage);
        }
    }

    /**
     * Initializes the Safety form.
     *
     * @param formInstance FormInstance
     * @param formStorage X2BaseBean
     * @param userData UserDataContainer
     */
    private void initializeCndSftForm(FormInstance formInstance, X2BaseBean formStorage, UserDataContainer userData) {
        FormInstance referralForm = WorkflowUtils.getFormInstance(
                userData.getCurrentWorkflow(), REFERRAL_FORM_ID, getBroker());
        GenericFormData referralData = (GenericFormData) referralForm.getStorageObject(getBroker());
        DataDictionary referralDictionary = DataDictionary.getDistrictDictionary(
                referralForm.getFormDefinition().getExtendedDataDictionary(), getBroker().getPersistenceKey());

        Object victimOid = referralData.getFieldValueByAlias(ALIAS_VICTIM_OID, referralDictionary);

        if (victimOid != null) {
            DataDictionary safetyDictionary = DataDictionary.getDistrictDictionary(
                    formInstance.getFormDefinition().getExtendedDataDictionary(), getBroker().getPersistenceKey());

            Student victim = (Student) getBroker().getBeanByOid(Student.class, victimOid.toString());
            formStorage.setFieldValueByAlias(ALIAS_VICTIM_NAME, StringUtils.truncate(victim.getNameView(),
                    safetyDictionary.findDataDictionaryFieldByAlias(ALIAS_VICTIM_NAME).getDatabaseLength()),
                    safetyDictionary);

            getBroker().saveBeanForced(formStorage);
        }
    }

    /**
     * Sets the investigation status for the passed incident based upon the passed outcome oid.
     *
     * @param incident ConductIncident
     * @param outcomeOid String
     */
    private void setInvestigationStatus(ConductIncident incident, String outcomeOid) {
        if (OUTCOME_OID_CONFIRMED.equals(outcomeOid) ||
                OUTCOME_OID_CONFIRMED_LAW.equals(outcomeOid)) {
            incident.setInvestigation(ConductIncident.investigationStatus.INVESTIGATION_STATUS_CONFIRMED.ordinal());
        } else if (OUTCOME_OID_NON_BULLYING.equals(outcomeOid) ||
                OUTCOME_OID_NO_WRONGDOING.equals(outcomeOid)) {
            incident.setInvestigation(ConductIncident.investigationStatus.INVESTIGATION_STATUS_NULLIFIED.ordinal());
        } else {
            incident.setInvestigation(ConductIncident.investigationStatus.INVESTIGATION_STATUS_NA.ordinal());
        }
    }
}
