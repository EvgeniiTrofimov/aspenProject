/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2012 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.x2dev.sis.model.business.StudentWaitListManager.ALIAS_ACTION_PARENT;
import static com.x2dev.sis.model.business.StudentWaitListManager.ALIAS_REMOVED_OIDS;
import static com.x2dev.sis.model.business.StudentWaitListManager.ALIAS_REQUESTED_OIDS;
import static com.x2dev.sis.model.business.StudentWaitListManager.ALIAS_WAITLIST_OID;
import static com.x2dev.sis.model.business.StudentWaitListManager.EMBEDDED_LIST_ID;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.UpdateQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.MessageProperties;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.WriteEmailManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.admin.ReferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.tools.procedures.FormAwareProcedure;
import com.follett.fsc.core.k12.tools.procedures.SessionAwareProcedure;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.AuthenticatedActionForm;
import com.follett.fsc.core.k12.web.ChildDetailSet;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebUtils;
import com.follett.fsc.core.k12.web.forms.FormDetail;
import com.follett.fsc.core.k12.web.struts.UserEvent;
import com.follett.fsc.core.k12.web.template.Template;
import com.follett.fsc.core.k12.web.workflow.OutcomeDetail;
import com.follett.fsc.core.k12.web.workflow.OutcomeDetailForm;
import com.x2dev.sis.model.beans.StudentCase;
import com.x2dev.sis.model.beans.StudentCaseChild;
import com.x2dev.sis.model.beans.StudentWaitList;
import com.x2dev.sis.model.business.StudentWaitListManager;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;
import java.util.logging.Level;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure for the "Student Waiting List" workflow.
 */
public class StudentCaseWaitlistWorkflow extends WorkflowProcedure
        implements DynamicFormProcedure, SessionAwareProcedure, FormAwareProcedure {
    /**
     * Alias for the StudentWaitList context field.
     */
    public static final String ALIAS_CONTEXT = "swl-remove-context";

    /**
     * Alias for the StudentWaitList grade level field.
     */
    public static final String ALIAS_GRADE_LEVEL = "swl-remove-grade";

    /**
     * Alias for the StudentWaitList OID field.
     */
    public static final String ALIAS_OID = "swl-remove-oid";

    /**
     * Alias for the StudentWaitList priority field.
     */
    public static final String ALIAS_PRIORITY = "swl-remove-priority";

    /**
     * Alias for the StudentWaitList program field.
     */
    public static final String ALIAS_PROGRAM = "swl-remove-program";

    /**
     * Alias for the StudentWaitList program rank field.
     */
    public static final String ALIAS_PROGRAM_PRIORITY = "swl-remove-program-priority";

    /**
     * Alias for the StudentWaitList program rank field.
     */
    public static final String ALIAS_PROGRAM_RANK = "swl-remove-program-rank";

    /**
     * Alias for the StudentWaitList rank field.
     */
    public static final String ALIAS_RANK = "swl-remove-rank";

    /**
     * Alias for the StudentWaitList school field.
     */
    public static final String ALIAS_SCHOOL = "swl-remove-school";

    /**
     * ID for the first form of the WaitList Removal workflow.
     */
    public static final String FORM_1 = "SWL-REM-001";

    /**
     * ID for the second form of the WaitList Removal workflow.
     */
    public static final String FORM_2 = "SWL-REM-002";

    private AuthenticatedActionForm m_form;
    private UserDataContainer m_userData;

    /**
     * Constructs a new Student Registration Workflow object.
     *
     * @param definition WorkflowDefinition
     * @param organization Organization
     * @param user User
     * @param broker X2Broker
     * @param locale Locale
     */
    public StudentCaseWaitlistWorkflow(WorkflowDefinition definition,
            Organization organization,
            User user,
            X2Broker broker,
            Locale locale) {
        super(definition, organization, user, broker, locale);
    }

    /**
     * After save template.
     *
     * @param detail GenericDetail
     * @param userData UserDataContainer
     * @param broker ModelBroker
     * @return List
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#afterSaveTemplate(
     *      com.follett.fsc.core.k12.web.GenericDetail,
     *      com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.business.ModelBroker)
     */
    @Override
    public List<ValidationError> afterSaveTemplate(GenericDetail detail,
                                                   UserDataContainer userData,
                                                   ModelBroker broker) {
        return null;
    }

    /**
     * Executes updating the workflow owner OID. The action that initiates the workflow does not
     * specify an owner. This method sets the owner on the workflow and form instance beans. Forms
     * for subsequent phases will have the owner set properly.
     *
     * @param progress WorkflowProgress
     */
    public void executeUpdateOwner(WorkflowProgress progress) {
        FormInstance formInstance = progress.getFormInstanceById(FORM_1, getBroker());

        if (formInstance != null) {
            Workflow workflow = progress.getWorkflow();

            StudentCase studentCase = (StudentCase) formInstance.getStorageObject(getBroker());

            /*
             * If we're starting the workflow on the second phase, the owner will already be set.
             * Starting the workflow from the first phase will not properly set the owner.
             */
            workflow.setOwnerOid(studentCase.getOid());
            formInstance.setOwnerObjectOid(studentCase.getOid());

            if (workflow.isDirty()) {
                getBroker().saveBeanForced(workflow);
            }

            if (formInstance.isDirty()) {
                getBroker().saveBeanForced(formInstance);
            }
        }
    }

    /**
     * com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure.initializeOwner(X2BaseBean
     * selectionBean, Map<String, FormInstance> formInstances, PlainDate date, WorkflowDefinition
     * workflowDefinition)
     *
     * @param selectionBean X2BaseBean
     * @param formInstances Map<String,FormInstance>
     * @param date PlainDate
     * @param workflowDefinition WorkflowDefinition
     * @return X2BaseBean
     */
    @Override
    public X2BaseBean initializeOwner(X2BaseBean selectionBean,
                                      Map<String, FormInstance> formInstances,
                                      PlainDate date,
                                      WorkflowDefinition workflowDefinition) {
        X2BaseBean owner = selectionBean;

        if (formInstances != null) {
            FormInstance formInstance = formInstances.get(FORM_1);
            if (formInstance != null) {
                owner = formInstance.getStorageObject(getBroker());

                if (owner instanceof StudentCase) {
                    String oids = ((OutcomeDetailForm) m_form).getSelectedChildren(EMBEDDED_LIST_ID);

                    DataDictionary dictionary =
                            DataDictionary.getDistrictDictionary(
                                    formInstance.getFormDefinition().getExtendedDataDictionary(),
                                    m_userData.getPersistenceKey());

                    ((StudentCase) owner).setFieldValueByAlias(ALIAS_REQUESTED_OIDS, oids, dictionary);

                    getBroker().saveBeanForced(owner, dictionary);
                }
            } else {
                formInstance = formInstances.get(FORM_2);

                if (formInstance != null) {
                    if (owner instanceof StudentCase) {
                        String oids = ((OutcomeDetailForm) m_form).getSelectedChildren(ALIAS_REMOVED_OIDS);

                        DataDictionary dictionary =
                                DataDictionary.getDistrictDictionary(
                                        formInstance.getFormDefinition().getExtendedDataDictionary(),
                                        m_userData.getPersistenceKey());

                        ((StudentCase) owner).setFieldValueByAlias(ALIAS_REMOVED_OIDS, oids, dictionary);
                        getBroker().saveBeanForced(owner, dictionary);
                    }
                }
            }
        }

        return owner;
    }

    /**
     * Initialize template.
     *
     * @param template Template
     * @param applicationContext ApplicationContext
     * @param dictionary DataDictionary
     * @param privilegeSet PrivilegeSet
     * @param locale Locale
     * @throws X2BaseException exception
     * @see void
     *      com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure.initializeTemplate(
     *      Template
     *      template, ApplicationContext applicationContext, DataDictionary dictionary, PrivilegeSet
     *      privilegeSet, Locale locale) throws X2BaseException
     */
    @Override
    public void initializeTemplate(Template template,
                                   ApplicationContext applicationContext,
                                   DataDictionary dictionary,
                                   PrivilegeSet privilegeSet,
                                   Locale locale)
            throws X2BaseException {
        // Nothing needed
    }

    /**
     * Sets the invalidate date to today for wait lists corresponding to the passed student case
     * child OIDs.
     *
     * @param scdChildOids String
     * @param formDetail FormDetail
     * @param timeZone TimeZone
     * @param broker X2Broker
     * @return int
     */
    public int invalidateWaitlists(String scdChildOids, FormDetail formDetail, TimeZone timeZone, X2Broker broker) {
        int results = 0;

        // Get the SWL OIDs for the selected case children
        DataDictionary dictionary = formDetail.getDataDictionary();
        DataDictionaryField dataField = dictionary.findDataDictionaryFieldByAlias(ALIAS_WAITLIST_OID);
        if (dataField != null) {
            Criteria criteria = new X2Criteria();
            criteria.addIn(X2BaseBean.COL_OID, StringUtils.convertDelimitedStringToList(scdChildOids, ','));
            QueryByCriteria query = new QueryByCriteria(StudentCaseChild.class, criteria, true);
            Collection<String> swlOids =
                    CollectionUtils.getPropertyCollection(broker.getCollectionByQuery(query), dataField.getJavaName());

            if (!swlOids.isEmpty()) {
                ModelProperty property = new ModelProperty(StudentWaitList.class.getName(),
                        StudentWaitList.COL_ACTION_CODE, broker.getPersistenceKey());

                Collection<String> codes =
                        ReferenceManager.getCodesByLogicalAlias(property.getField().getReferenceTable(),
                                ALIAS_ACTION_PARENT,
                                broker,
                                formDetail.getLocale());

                HashMap<String, Object> fields = new HashMap<String, Object>(2);
                if (!codes.isEmpty()) {
                    fields.put(StudentWaitList.COL_ACTION_CODE, codes.iterator().next());
                }

                fields.put(StudentWaitList.COL_ACTION_DATE, new PlainDate(timeZone));

                Criteria updateCriteria = new X2Criteria();
                updateCriteria.addIn(X2BaseBean.COL_OID, swlOids);
                UpdateQuery updateQuery = new UpdateQuery(StudentWaitList.class,
                        updateCriteria,
                        fields);
                results = broker.executeUpdateQuery(updateQuery);
            }
        }

        return results;
    }

    /**
     * Modify form.
     *
     * @param detail GenericDetail
     * @param key String
     * @param value String
     * @param userData UserDataContainer
     * @param template Template
     * @param errors List
     * @return Map
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure.modifyForm(GenericDetail
     *      detail, String key, String value, UserDataContainer userData, Template template, List
     *      errors)
     *      throws X2BaseException
     */
    @Override
    public Map<String, Object> modifyForm(GenericDetail detail,
                                          String key,
                                          String value,
                                          UserDataContainer userData,
                                          Template template,
                                          List errors)
            throws X2BaseException {
        Map<String, Object> properties = new HashMap<String, Object>();

        OutcomeDetail outcomeDetail = (OutcomeDetail) detail;
        FormDetail formDetail = outcomeDetail.getCurrentFormDetail();

        // On form 1, import the waiting lists into student case children
        if (FORM_1.equals(formDetail.getFormInstance().getFormDefinition().getId())) {
            // Make sure the correct embedded list exists
            ChildDetailSet set = formDetail.getChildDetailSet(EMBEDDED_LIST_ID);
            if (set != null) {
                // Only populate once the student and context year are selected
                String studentOid = (String) formDetail.getValueByBeanPath(StudentCase.COL_STUDENT_OID);
                String contextOid = (String) formDetail.getValueByAlias(ALIAS_CONTEXT);

                if (!StringUtils.isEmpty(studentOid) && !StringUtils.isEmpty(contextOid)) {
                    X2Criteria criteria =
                            StudentWaitListManager.getActiveWaitListCriteria(studentOid,
                                    contextOid,
                                    getBroker().getPersistenceKey());

                    QueryByCriteria query = new QueryByCriteria(StudentWaitList.class, criteria);
                    query.addOrderByAscending(StudentWaitList.COL_RANK);

                    Collection<StudentWaitList> waitlists = getBroker().getCollectionByQuery(query);

                    /*
                     * Go through existing details:
                     * - remove children from the results list that already are in the set
                     * - delete the children that no longer match the criteria
                     */
                    ArrayList<String> selectedOids = StringUtils.convertDelimitedStringToList(value, ',', true);

                    for (String existingKey : new HashSet<String>(set.getChildDetailsKeys())) {
                        GenericDetail existingDetail = set.getChildDetail(existingKey);
                        String existingSwlOid = (String) existingDetail.getValueByAlias(ALIAS_OID);

                        // Try to remove the detail from waiting lists, if not found delete child
                        // detail
                        if (CollectionUtils.remove(waitlists, X2BaseBean.COL_OID, existingSwlOid) == null) {
                            set.markForDelete(existingDetail.getId());

                            selectedOids.remove(existingDetail.getId());

                            properties.put(DynamicFormProcedure.PROPERTY_DYNAMIC_FORM_REFRESH,
                                    Integer.valueOf(UserEvent.EVENT_REFRESH));
                        }
                    }

                    // Update selected children list
                    Map<String, String> values = new HashMap<String, String>(1);
                    values.put("selectedChildren(" + EMBEDDED_LIST_ID + ")",
                            StringUtils.convertCollectionToDelimitedString(selectedOids, ','));
                    properties.put(DynamicFormProcedure.PROPERTY_DYNAMIC_FORM_PROPERTIES, values);

                    // For remaining results, add new child details since they are not there yet
                    for (StudentWaitList swl : waitlists) {
                        SchoolCapacity sca = swl.getSchoolCapacity();

                        ModelBroker broker = new ModelBroker(userData.getPrivilegeSet());
                        String childId = set.addNewChild(userData, broker);

                        GenericDetail newDetail = set.getChildDetail(childId);

                        newDetail.setValueByAlias(ALIAS_GRADE_LEVEL, sca.getGradeLevel());
                        newDetail.setValueByAlias(ALIAS_OID, swl.getOid());
                        newDetail.setValueByAlias(ALIAS_PRIORITY, swl.getPriority());
                        newDetail.setValueByAlias(ALIAS_PROGRAM, sca.getProgramCode());
                        newDetail.setValueByAlias(ALIAS_PROGRAM_PRIORITY, swl.getProgramPriority());
                        newDetail.setValueByAlias(ALIAS_PROGRAM_RANK, Integer.valueOf(swl.getProgramRank()).toString());
                        newDetail.setValueByAlias(ALIAS_RANK, Integer.valueOf(swl.getRank()).toString());
                        newDetail.setValueByAlias(ALIAS_SCHOOL, sca.getSchool().getName());

                        set.markForSave(childId);

                        properties.put(DynamicFormProcedure.PROPERTY_DYNAMIC_FORM_REFRESH,
                                Integer.valueOf(UserEvent.EVENT_REFRESH));
                    }
                } else if (!set.getChildDetails().isEmpty()) {
                    set.markAllForDelete();
                    ((OutcomeDetailForm) m_form).setSelectedChildren(EMBEDDED_LIST_ID, "");

                    Map<String, String> values = new HashMap<String, String>(1);
                    values.put("selectedChildren(" + EMBEDDED_LIST_ID + ")", "");
                    properties.put(DynamicFormProcedure.PROPERTY_DYNAMIC_FORM_PROPERTIES, values);

                    properties.put(DynamicFormProcedure.PROPERTY_DYNAMIC_FORM_REFRESH,
                            Integer.valueOf(UserEvent.EVENT_REFRESH));
                }
            }
        }

        return properties;
    }

    /**
     * Sets the invalidate date to today for wait lists corresponding to the passed student case
     * child OIDs.
     *
     * @param stc StudentCase
     * @param scdChildOids String
     * @param formDetail FormDetail
     * @param timeZone TimeZone
     * @param broker X2Broker
     */
    public void sendNotification(StudentCase stc,
                                 String scdChildOids,
                                 FormDetail formDetail,
                                 TimeZone timeZone,
                                 X2Broker broker) {
        Person recipient = stc.getUser().getPerson();
        if (!StringUtils.isEmpty(recipient.getEmail01())) {
            DataDictionary dictionary = formDetail.getDataDictionary();
            DataDictionaryField dataField = dictionary.findDataDictionaryFieldByAlias(ALIAS_WAITLIST_OID);
            if (dataField != null) {
                Person student = stc.getStudent().getPerson();

                // Email message
                String subject = "Confirmation of waiting list removal";
                String message = recipient.getFirstName() + " " + recipient.getLastName()
                        + ",<br><br>Your request for waiting list removal has been processed. " +
                        student.getFirstName() + " has been removed from the following waiting lists:<br><br>";

                // swlRank, ctxSchoolYear, sklSchoolName, scaProgramCode, scaGradeLevel
                DataDictionaryField rank =
                        dictionary.findDataDictionaryField(StudentWaitList.class.getName(), StudentWaitList.COL_RANK);
                DataDictionaryField schoolYear = dictionary.findDataDictionaryField(
                        DistrictSchoolYearContext.class.getName(), DistrictSchoolYearContext.COL_SCHOOL_YEAR);
                DataDictionaryField schoolName =
                        dictionary.findDataDictionaryField(School.class.getName(), School.COL_NAME);
                DataDictionaryField programCode = dictionary.findDataDictionaryField(SchoolCapacity.class.getName(),
                        SchoolCapacity.COL_PROGRAM_CODE);
                DataDictionaryField gradeLevel = dictionary.findDataDictionaryField(SchoolCapacity.class.getName(),
                        SchoolCapacity.COL_GRADE_LEVEL);

                message += "<table border='1' cellspacing='2' cellpadding='6'><tr><th>" + rank.getUserShortName() +
                        "</th><th>" + schoolYear.getUserShortName() +
                        "</th><th>" + schoolName.getUserShortName() +
                        "</th><th>" + programCode.getUserShortName() +
                        "</th><th>" + gradeLevel.getUserShortName() +
                        "</th></tr>";

                // Waiting lists invalidated
                Criteria subCriteria = new X2Criteria();
                subCriteria.addIn(X2BaseBean.COL_OID, StringUtils.convertDelimitedStringToList(scdChildOids, ','));
                SubQuery subQuery = new SubQuery(StudentCaseChild.class, dataField.getJavaName(), subCriteria);

                Criteria criteria = new Criteria();
                criteria.addIn(X2BaseBean.COL_OID, subQuery);
                QueryByCriteria query = new QueryByCriteria(StudentWaitList.class, criteria);
                QueryIterator iterator = broker.getIteratorByQuery(query);
                try {
                    while (iterator.hasNext()) {
                        StudentWaitList swl = (StudentWaitList) iterator.next();
                        SchoolCapacity sca = swl.getSchoolCapacity();

                        // swlRank, ctxSchoolYear, sklSchoolName, scaProgramCode, scaGradeLevel
                        message += "<tr><td>" + swl.getRank() +
                                "</td><td>" + sca.getDistrictContext().getSchoolYear() +
                                "</td><td>" + sca.getSchool().getName() +
                                "</td><td>" + sca.getProgramCode() +
                                "</td><td>" + sca.getGradeLevel() +
                                "</td></tr>";
                    }
                } finally {
                    iterator.close();
                }

                message += "</table><br><br>Thank you,<br>Aspen System Administrator";

                // Send email
                WriteEmailManager manager = new WriteEmailManager(getOrganization(), m_userData.getCurrentOwnerOid(),
                        m_userData.getCurrentOwnerType(), m_userData.getUser());
                if (manager.connect()) {
                    try {
                        ArrayList<String> to = new ArrayList<String>(1);
                        to.add(recipient.getEmail01());

                        MessageProperties msgProperties = new MessageProperties(to,
                                null,
                                null,
                                null,
                                subject,
                                message,
                                "text/html",
                                WebUtils.convertHtmlToPlainText(message),
                                "text/plain",
                                null);

                        manager.sendMail(msgProperties);
                    } finally {
                        manager.disconnect();
                    }
                } else {
                    String msg = LocalizationCache.getMessages(broker.getPersistenceKey())
                            .getMessage("message.email.sendMailError");
                    AppGlobals.getLog().log(Level.SEVERE, msg);
                }
            }
        }
    }

    /**
     * Sets the form.
     *
     * @param form void
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.FormAwareProcedure.setForm(
     *      AuthenticatedActionForm
     *      form)
     */
    @Override
    public void setForm(AuthenticatedActionForm form) {
        m_form = form;
    }

    /**
     * Sets the user data.
     *
     * @param userData void
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.SessionAwareProcedure.setUserData(
     *      UserDataContainer
     *      userData)
     */
    @Override
    public void setUserData(UserDataContainer userData) {
        m_userData = userData;
    }

    /**
     * Validate template.
     *
     * @param form GenericDetailForm
     * @param detail GenericDetail
     * @param userData UserDataContainer
     * @param broker ModelBroker
     * @return List
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#validateTemplate(com.follett.
     *      fsc.core.k12.web.GenericDetailForm, com.follett.fsc.core.k12.web.GenericDetail,
     *      com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.business.ModelBroker)
     */
    @Override
    public List<ValidationError> validateTemplate(GenericDetailForm form,
                                                  GenericDetail detail,
                                                  UserDataContainer userData,
                                                  ModelBroker broker) {
        return new ArrayList<ValidationError>();
    }
}
