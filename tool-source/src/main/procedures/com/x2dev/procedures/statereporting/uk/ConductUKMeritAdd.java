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
package com.x2dev.procedures.statereporting.uk;

import static com.x2dev.sis.model.business.conduct.ConductAliases.*;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.workflow.WorkflowManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.tools.procedures.SessionAwareProcedure;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.workflow.SessionAccessManager;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.TimeAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This procedure allows teachers to create a Conduct Incident or a
 * Conduct Referral Workflow Instance for students
 * depending on the Incident Code's RequireReferral field.
 *
 * This Procedure is expected to be run this from a Staff View
 *
 * @author X2 Development Corporation
 */
public class ConductUKMeritAdd extends ProcedureJavaSource implements SessionAwareProcedure {
    private static final String PARAM_STUDENT_OIDS = "student-oids";
    private static final String PARAM_VICTIM_OID = "victim-oid";
    private static final String PARAM_REFERRAL_STAFF_OID = "referral-staff-oid";
    private static final String PARAM_INCIDENT_DATE = "incident-date";
    private static final String PARAM_INCIDENT_TIME = "incident-time";
    private static final String PARAM_INCIDENT_ID = "incident-id";
    private static final String PARAM_INCIDENT_CODE = "incident-code";
    private static final String PARAM_INCIDENT_LOCATION = "incident-location";
    private static final String PARAM_INCIDENT_DESCRIPTION = "incident-description";
    private static final String PARAM_TEACHER_NOTES = "teacher-notes";

    private static final String EXTENED_DATA_DICTIONARY_CONDUCT_REF = "ddxConductRef";

    private UserDataContainer m_userData;

    private School m_school;
    private ArrayList m_studentOids = new ArrayList();
    private String m_victimOid;
    private String m_referralStaffOid;
    private PlainDate m_incidentDate;
    private PlainTime m_incidentTime;
    private String m_incidentId;
    private String m_incidentCode;
    private String m_incidentLocation;
    private String m_incidentDescription;
    private String m_teacherNotes;
    private TimeAsStringConverter m_timeConverter;
    private Map<String, Student> m_students;

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
     * Save the data for each student as either a Conduct Incident or a
     * Conduct Referral Workflow Instance with the first phase completed,
     * depending on the Incident Code's RequireReferral field.
     */
    protected void createIncidents() {
        if (m_studentOids.size() > 0) {
            ReferenceCode referenceCode = getConductIncidentRefCode(m_incidentCode, m_school.getOid());

            if (referenceCode != null) {
                Boolean requireReferral =
                        Boolean.valueOf("1".equals(referenceCode.getFieldValueByAlias(ALIAS_REF_CODE_REQUIRE_REFERRAL)));
                if (!requireReferral.booleanValue()) {
                    logMessage("Saved a new Conduct Incident for: ");

                    // Save as an Student Conduct Incident for each student.
                    Iterator it = m_studentOids.iterator();
                    while (it.hasNext()) {
                        String studentOid = (String) it.next();
                        Student student = m_students.get(studentOid);
                        String studentName = student.getNameView();

                        ConductIncident incident = new ConductIncident(getBroker().getPersistenceKey());

                        incident.setStudentOid(studentOid);
                        incident.setSchoolOid(m_school.getOid());
                        if (!StringUtils.isEmpty(m_victimOid)) {
                            incident.setVictimOid(m_victimOid);
                        }
                        incident.setReferralStaffOid(m_referralStaffOid);
                        incident.setOwnerOid(m_referralStaffOid);
                        incident.setIncidentDate(m_incidentDate);
                        incident.setIncidentTime(m_incidentTime);
                        incident.setIncidentId(m_incidentId);
                        incident.setIncidentCode(m_incidentCode);
                        incident.setIncidentLocation(m_incidentLocation);
                        incident.setDescription(m_incidentDescription);
                        if (!StringUtils.isEmpty(m_teacherNotes)) {
                            incident.setFieldValueByAlias(ALIAS_INCIDENT_TEACHER_NOTES, m_teacherNotes);
                        }
                        getBroker().saveBeanForced(incident);

                        logMessage("    " + studentName);
                    }
                } else {
                    logMessage("Created a Conduct Referral Workflow Instance for: ");

                    WorkflowManager workflowManager = new WorkflowManager(getBroker());

                    // Get Conduct Referral WorkflowDefinition
                    Criteria workflowDefinitionCriteria = new Criteria();
                    workflowDefinitionCriteria.addEqualTo(X2BaseBean.COL_OID, WORKFLOW_DEFINITION_OID_CND_REF);
                    QueryByCriteria workflowDefinitionQuery =
                            new QueryByCriteria(WorkflowDefinition.class, workflowDefinitionCriteria);
                    WorkflowDefinition referralWorkFlowDef =
                            (WorkflowDefinition) getBroker().getBeanByQuery(workflowDefinitionQuery);
                    WorkflowPhaseOutcome referralWorkFlowFirstPhaseOutcome =
                            referralWorkFlowDef.getFirstWorkflowPhase().getStandardPhaseOutcome();

                    // Get FormDefiniton OID - REFERRAL_FORM_ID
                    FormDefinition referralFormDef = FormDefinition.getById(REFERRAL_FORM_ID, getBroker());
                    DataDictionary referralDictionary = DataDictionary.getDistrictDictionary(
                            referralFormDef.getExtendedDataDictionary(), getBroker().getPersistenceKey());

                    Student victim = null;
                    String victimName = null;
                    if (!StringUtils.isEmpty(m_victimOid)) {
                        victim = (Student) getBroker().getBeanByOid(Student.class, m_victimOid);
                        victimName = victim.getNameView();
                    }

                    // Save as Conduct Referral Workflow Instance with the first phase completed for
                    // each student.
                    Iterator it = m_studentOids.iterator();
                    while (it.hasNext()) {
                        String studentOid = (String) it.next();
                        Student student = m_students.get(studentOid);
                        String studentName = student.getNameView();

                        // 1. Create GenericFormData from parameters
                        GenericFormData referralData =
                                X2BaseBean.newInstance(GenericFormData.class, getBroker().getPersistenceKey());
                        referralData.setOrganization1Oid(
                                OrganizationManager.getRootOrganization(getOrganization()).getOid());
                        referralData.setExtendedDataDictionaryOid(EXTENED_DATA_DICTIONARY_CONDUCT_REF);
                        referralData.setFieldValueByAlias(ALIAS_REFERRAL_STAFF_OID, m_referralStaffOid,
                                referralDictionary);
                        referralData.setFieldValueByAlias(ALIAS_OFFENDER_OID, studentOid, referralDictionary);
                        referralData.setFieldValueByAlias(ALIAS_OFFENDER_NAME, studentName, referralDictionary);
                        if (!StringUtils.isEmpty(m_victimOid)) {
                            referralData.setFieldValueByAlias(ALIAS_VICTIM_OID, m_victimOid, referralDictionary);
                            referralData.setFieldValueByAlias(ALIAS_VICTIM_NAME, victimName, referralDictionary);
                        }
                        referralData.setFieldValueByAlias(ALIAS_INCIDENT_ID, m_incidentId, referralDictionary);
                        referralData.setFieldValueByAlias(ALIAS_INCIDENT_CODE, m_incidentCode, referralDictionary);
                        referralData.setFieldValueByAlias(ALIAS_INCIDENT_DATE, m_incidentDate.toString(),
                                referralDictionary);
                        referralData.setFieldValueByAlias(ALIAS_INCIDENT_TIME, m_incidentTime.toString(),
                                referralDictionary);
                        referralData.setFieldValueByAlias(ALIAS_INCIDENT_LOCATION, m_incidentLocation,
                                referralDictionary);
                        referralData.setFieldValueByAlias(ALIAS_INCIDENT_DESCRIPTION, m_incidentDescription,
                                referralDictionary);
                        if (!StringUtils.isEmpty(m_teacherNotes)) {
                            referralData.setFieldValueByAlias(ALIAS_INCIDENT_TEACHER_NOTES, m_teacherNotes,
                                    referralDictionary);
                        }
                        getBroker().saveBeanForced(referralData);

                        // 2. Create FormInstance from FormDefinition(REFERRAL_FORM_ID)
                        FormInstance formInstance =
                                X2BaseBean.newInstance(FormInstance.class, getBroker().getPersistenceKey());
                        formInstance.setFormDefinitionOid(referralFormDef.getOid());
                        formInstance.setOwnerObjectOid(studentOid);
                        formInstance.setOwnerView(studentName);
                        formInstance.setStorageObjectOid(referralData.getOid());
                        formInstance.setCreatedTime(System.currentTimeMillis());
                        getBroker().saveBeanForced(formInstance);

                        // 3. Create WorkFlow from WorkFlowDefinition
                        // 4. Create WorkflowProgress from WorkFlowDefinition, it's first
                        // WorkFlowPhase and WorkFlowPhaseOutcome
                        // 5. Create WorkflowProgressForm from WorkFlowDefinition and FormDefinition
                        Map<String, FormInstance> referralFormInstances = new HashMap<String, FormInstance>(2);
                        referralFormInstances.put(REFERRAL_FORM_ID, formInstance);

                        try {
                            Workflow workflow =
                                    workflowManager.initiateWorkflow(studentOid, referralWorkFlowFirstPhaseOutcome,
                                            referralFormInstances, m_userData, getPlainDate(), false, getLocale(),
                                            new SessionAccessManager(m_userData, getBroker()));

                            referralData.setFieldValueByAlias(ALIAS_NEXT_WORKFLOW_OID, workflow.getOid(),
                                    referralDictionary);
                            getBroker().saveBeanForced(referralData);

                            if (!workflowManager.getValidationErrors().isEmpty()) {
                                AppGlobals.getLog().log(Level.SEVERE,
                                        "Procedure: ConductUKMeritAdd failed. Validations errors occuring during creation of a Workflow. ");
                                logMessage(
                                        "Error: ConductUKMeritAdd failed. Validations errors occuring during creation of a Workflow. ");
                            }
                        } catch (X2BaseException e) {
                            AppGlobals.getLog().log(Level.SEVERE, e.getMessage(), e);
                        }

                        logMessage("    " + studentName);
                    }

                }

            }

        } else {
            logMessage("Error: No Students selected.");
        }
    }

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.x2dev.sis.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        logMessage("Start Add Merit/Demerit Procedure.");

        m_school = getSchool();
        if (m_school == null) {
            logMessage("Error: This procedure needs to be run within the context of Staff user.");
            return;
        }
        logMessage("Running for School: " + m_school.getName());

        setUp();

        loadParameters();

        loadStudents();

        createIncidents();

        logMessage("Finish Add Merit/Demerit Procedure.");
    }

    /**
     * Load parameters.
     */
    protected void loadParameters() {
        String studentOidList = (String) getParameter(PARAM_STUDENT_OIDS);
        m_studentOids = StringUtils.convertDelimitedStringToList(studentOidList, ',');
        m_victimOid = (String) getParameter(PARAM_VICTIM_OID);
        m_referralStaffOid = (String) getParameter(PARAM_REFERRAL_STAFF_OID);
        m_incidentDate = (PlainDate) getParameter(PARAM_INCIDENT_DATE);
        m_incidentTime = (PlainTime) m_timeConverter.parseSystemString((String) getParameter(PARAM_INCIDENT_TIME));
        m_incidentId = (String) getParameter(PARAM_INCIDENT_ID);
        m_incidentCode = (String) getParameter(PARAM_INCIDENT_CODE);
        m_incidentLocation = (String) getParameter(PARAM_INCIDENT_LOCATION);
        m_incidentDescription = (String) getParameter(PARAM_INCIDENT_DESCRIPTION);
        m_teacherNotes = (String) getParameter(PARAM_TEACHER_NOTES);
    }

    /**
     * Load Students.
     */
    protected void loadStudents() {
        // TODO Try to use a subquery to work about the sqlserver limitation of 500 items for a SQL
        // IN statement.
        if (m_studentOids.size() > 0) {
            Criteria studentCriteria = new Criteria();
            studentCriteria.addIn(X2BaseBean.COL_OID, m_studentOids);

            QueryByCriteria studentQuery = new QueryByCriteria(Student.class, studentCriteria);

            m_students = getBroker().getMapByQuery(studentQuery, X2BaseBean.COL_OID, m_studentOids.size());
        } else {
            m_students = new HashMap<String, Student>();
        }

    }

    /**
     * Print parameters
     * For Testing.
     */
    protected void printParameters() {
        System.out.println("Received: ");
        System.out.println(PARAM_REFERRAL_STAFF_OID + " : " + m_referralStaffOid);
        System.out.println(PARAM_STUDENT_OIDS + " : " + m_studentOids);
        System.out.println("studentOids: " + m_studentOids.size());
        System.out.println(PARAM_VICTIM_OID + " : " + m_victimOid);
        System.out.println(PARAM_INCIDENT_ID + " : " + m_incidentId);
        System.out.println(PARAM_INCIDENT_CODE + " : " + m_incidentCode);
        System.out.println(PARAM_INCIDENT_DATE + " : " + m_incidentDate);
        System.out.println(PARAM_INCIDENT_TIME + " : " + m_incidentTime);
        System.out.println(PARAM_INCIDENT_LOCATION + " : " + m_incidentLocation);
        System.out.println(PARAM_INCIDENT_DESCRIPTION + " : " + m_incidentDescription);
        System.out.println(PARAM_TEACHER_NOTES + " : " + m_teacherNotes);
    }

    /**
     * Setup converters.
     */
    protected void setUp() {
        m_timeConverter = (TimeAsStringConverter) ConverterFactory.getConverterForClass(Converter.TIME_CONVERTER,
                getLocale(), true);
    }

    /**
     * Get the Conduct Incident Reference Code.
     *
     * @param code String
     * @param schoolOid String
     * @return ReferenceCode
     */
    private ReferenceCode getConductIncidentRefCode(String code, String schoolOid) {
        Criteria refCodeCriteria = new Criteria();
        refCodeCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, REF_CODE_CONDUCT_INCIDENT_TYPE);
        refCodeCriteria.addEqualTo(ReferenceCode.COL_CODE, code);

        // If incident codes are assigned at the school level.
        // TODO add a subquery for selecting the user's schoolOid or if the field is null in db.
        // Create a separate criteria and or to main criteria.
        if (!StringUtils.isEmpty(schoolOid)) {
            // refCodeCriteria.addEqualTo(ReferenceCode.COL_SCHOOL_OID, schoolOid);
        }
        QueryByCriteria refCodeQuery = new QueryByCriteria(ReferenceCode.class, refCodeCriteria);

        ReferenceCode referenceCode = (ReferenceCode) getBroker().getBeanByQuery(refCodeQuery);

        return referenceCode;
    }

}
