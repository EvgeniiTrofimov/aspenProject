/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2020 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped.on;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationMessageResources;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.IepMeeting.TypeCode;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramDetail;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Ontario Special Education Program converter.
 * This utility converts Student Program Participation records
 * containing special education data into IEPData records.
 *
 * 1. The student is Active in Special Education.
 * 2. The student has a special ed StudentProgramParticipation record.
 * 3. The student does not have an active IEPData record.
 *
 *
 * @author Follett Software Company
 * @copyright 2020
 */
public class ConvertProgramsToIep extends ProcedureJavaSource {

    private static final String PROGRAM_CODE = "SPECED";

    private static final String ALIAS_PGM_EXCEPTIONALITY = "pgm-speced-exceptionality";
    private static final String ALIAS_PGM_PRIOR_EXCEPTIONALITY = "pgm-speced-pri-exceptionality";
    private static final String ALIAS_PGM_REPORT = "pgm-speced-report-ind";

    private static final String ALIAS_PGD_PLACEMENT = "pgd-speced-placement-type";
    private static final String ALIAS_PGD_PROGRAM_NAME = "pgd-speced-program-name";
    private static final String ALIAS_PGD_PROGRAM_LOCATION = "pgd-speced-program-location";
    private static final String ALIAS_PGD_START_DATE = "pgd-speced-start-date";
    private static final String ALIAS_PGD_TYPE = "pgd-speced-type";

    private static final String ALIAS_IEP_PLACEMENT = "iep-iprc-placement-decision";
    private static final String ALIAS_IEP_REVIEW_DATE = "iep-iprc-last-review-date";
    private static final String ALIAS_IEP_REASON = "iep-reason";
    private static final String ALIAS_IEP_PROGRAM = "iep-program";
    private static final String ALIAS_IEP_PROGRAM_LOCATION = "iep-program-location";

    private static final String ALIAS_GFD_STAFF = "gfd-staff-oid";
    private static final String ALIAS_REFERRAL_TYPE = "gfd-referral-type";
    
    private static final String LABEL_STUDENTS_SELECTED = "label.procedure.convertPrograms.selected";
    private static final String LABEL_PROGRAMS_CONVERTED = "label.procedure.convertPrograms.programs";
    private static final String LABEL_HAD_IEP = "label.procedure.convertPrograms.iep";
    private static final String LABEL_NO_PROGRAM = "label.procedure.convertPrograms.no.program";
    
    private static final String WORKFLOW_ID = "SYS-SPED-REFER";
    private static final String WORKFLOW_PHASE_ID1 = "Referral";
    private static final String WORKFLOW_PHASE_ID2 = "DetermineIPRC";
    private static final String WORKFLOW_OUTCOME_ID = "Complete";
    private static final String WORKFLOW_IEP_FORM_ID = "ON-SPED-IEP";
    private static final String WORKFLOW_REFERRAL_FORM_ID = "ON-SPED-REF";

    private int m_records;
    private int m_hasIep;
    private int m_copied;
    private List<String> m_noProgram = new ArrayList<String>();
    private BeanQuery m_query;
    private DataDictionary m_iepDictionary;
    private DataDictionary m_programDictionary;
    private DataDictionary m_formDictionary;
    private DateAsStringConverter m_converter;
    private PlainDate m_today = new PlainDate();

    private WorkflowDefinition m_wfd;
    private WorkflowPhase m_wph1;
    private WorkflowPhase m_wph2;
    private WorkflowPhaseOutcome m_wpo;
    private FormDefinition m_fmdIep;
    private FormDefinition m_fmdReferral;
    private LocalizationMessageResources m_resources;

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {

        m_resources = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale(), true);
        loadResources();
        convertProgramToIep();
    }

    protected void convertProgramToIep() {
        Collection<SisStudent> students = getStudents();
        for (SisStudent student : students) {
            m_records++;
            Collection<IepData> ieps = student.getIepData(getBroker());
            if (ieps != null && ieps.size() > 0) {
                m_hasIep++;
                continue;
            }
            List<StudentProgramParticipation> programs = getPrograms(student.getOid());
            if (programs.size() == 0) {
                m_noProgram.add(student.getNameView());
                continue;
            }
            IepData iep = copyProgram(programs, student);
            if (iep != null) {
                createWorkflow(iep);
                m_copied++;
            } else {
                m_noProgram.add(student.getNameView());
            }
        }

        StringBuilder builder = new StringBuilder();
        builder.append(getMessage(LABEL_STUDENTS_SELECTED, "Students selected: ", null))
        .append(Integer.toString(m_records))
        .append("\n")
        .append(getMessage(LABEL_PROGRAMS_CONVERTED, "Student Programs converted to IEP: ", null))
        .append(Integer.toString(m_copied))
        .append("\n")
        .append(getMessage(LABEL_HAD_IEP, "Student already has an IEP: ", null))
        .append(Integer.toString(m_hasIep))
        .append("\n")
        .append(getMessage(LABEL_NO_PROGRAM, "Students who do not have a Program record:", null));
        for (String name : m_noProgram) {
            builder.append("\n")
            .append(name);
        }
        logMessage(builder.toString());
    }

    /**
     * Capture the current list criteria for "current selection" option.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        m_query = userData.getCurrentList().getQuery(true);
        if (m_query.getBaseClass() != Student.class && m_query.getBaseClass() != SisStudent.class) {
            m_query = null;
        }
    }

    /**
     * Create a new IepData, copy information from the program into the IEP.
     *
     * @param program
     * @param student
     *
     * @return IepData
     */
    private IepData copyProgram(List<StudentProgramParticipation> programs, SisStudent student) {
        if (m_programDictionary == null) {
            m_programDictionary =
                    DataDictionary.getDistrictDictionary(programs.iterator().next().getExtendedDataDictionary(),
                            getBroker().getPersistenceKey());
        }

        Person person = getUser().getPerson();
        Staff staff = null;
        if (person != null) {
            staff = person.getStaff();
        }

        IepData iep = null;
        List<String> exceptionalities = new ArrayList<String>();
        List<String> exceptionalitiesPrimary = new ArrayList<String>();
        StudentProgramParticipation primaryProgram = null;
        for (StudentProgramParticipation program : programs) {
            PlainDate endDate = program.getEndDate();
            if (endDate == null || endDate.after(m_today)) {
                if (primaryProgram == null) {
                    primaryProgram = program;
                }
                // Get primary data from the program record.
                String exceptionality =
                        (String) program.getFieldValueByAlias(ALIAS_PGM_EXCEPTIONALITY, m_programDictionary);
                String primaryExceptionality =
                        (String) program.getFieldValueByAlias(ALIAS_PGM_PRIOR_EXCEPTIONALITY, m_programDictionary);
                if (!StringUtils.isEmpty(exceptionality)) {
                    if (BooleanAsStringConverter.TRUE.equals(primaryExceptionality)) {
                        primaryProgram = program;
                    }
                    exceptionalities.add(exceptionality);
                    exceptionalitiesPrimary.add(primaryExceptionality);
                }
            }
        }
        // get date and reason from the student.
        PlainDate spedLastEval = student.getSpedLastEvaluationDate();
        String reason = (exceptionalities.size() > 0 && spedLastEval != null) ? "IPRC" : "Other";

        if (primaryProgram != null) {
            // Find the latest Placement from the program details table.
            String spedPlacement = null;
            String spedProgram = null;
            // String spedProgramLocation = null;
            String spedPlacementDate = null;
            String spedProgramDate = null;
            Collection<StudentProgramDetail> details = primaryProgram.getProgramDetails(getBroker());
            if (details.size() > 0) {
                for (StudentProgramDetail detail : details) {
                    String detailType = (String) detail.getFieldValueByAlias(ALIAS_PGD_TYPE, m_programDictionary);
                    if ("Placement".equals(detailType)) {
                        String detailPlacementDate =
                                (String) detail.getFieldValueByAlias(ALIAS_PGD_START_DATE, m_programDictionary);
                        if (spedPlacementDate == null ||
                                (detailPlacementDate != null && spedPlacementDate.compareTo(detailPlacementDate) < 0)) {
                            spedPlacement =
                                    (String) detail.getFieldValueByAlias(ALIAS_PGD_PLACEMENT, m_programDictionary);
                            spedPlacementDate = detailPlacementDate;
                        }
                    } else if ("Program".equals(detailType)) {
                        String detailProgramDate =
                                (String) detail.getFieldValueByAlias(ALIAS_PGD_START_DATE, m_programDictionary);
                        if (spedProgramDate == null ||
                                (detailProgramDate != null && spedProgramDate.compareTo(detailProgramDate) < 0)) {
                            spedProgram =
                                    (String) detail.getFieldValueByAlias(ALIAS_PGD_PROGRAM_NAME, m_programDictionary);
                            // spedProgramLocation = (String)
                            // detail.getFieldValueByAlias(ALIAS_PGD_PROGRAM_LOCATION,
                            // m_programDictionary);
                            spedProgramDate = detailProgramDate;
                        }
                    }
                }
            }

            iep = X2BaseBean.newInstance(IepData.class, m_iepDictionary);
            iep.setStudentOid(student.getOid());
            iep.setStatusCodeEnum(StatusCode.DRAFT);
            iep.setMeetingTypeCodeEnum(TypeCode.INITIAL);
            iep.setStartDate(primaryProgram.getStartDate());
            iep.setAmendedIndicator(false);
            iep.setStaffOid(staff != null ? staff.getOid() : null);
            iep.setFieldValueByAlias(ALIAS_IEP_PLACEMENT, spedPlacement, m_iepDictionary);
            iep.setFieldValueByAlias(ALIAS_IEP_REASON, reason, m_iepDictionary);
            iep.setFieldValueByAlias(ALIAS_IEP_PROGRAM, spedProgram, m_iepDictionary);
            // iep.setFieldValueByAlias(ALIAS_IEP_PROGRAM_LOCATION, spedProgramLocation,
            // m_iepDictionary);

            if (spedLastEval != null) {
                iep.setFieldValueByAlias(ALIAS_IEP_REVIEW_DATE, m_converter.getSystemString(spedLastEval),
                        m_iepDictionary);
            }
            getBroker().saveBeanForced(iep);

            for (int i = 0; i < exceptionalities.size(); i++) {
                String exceptionality = exceptionalities.get(i);
                String exceptionalityPrimary = exceptionalitiesPrimary.get(i);

                IepDisability disability = X2BaseBean.newInstance(IepDisability.class, m_iepDictionary);
                disability.setStudentOid(student.getOid());
                disability.setIepDataOid(iep.getOid());
                disability.setDisabilityCode(exceptionality);
                disability.setPrimaryIndicator(BooleanAsStringConverter.TRUE.equals(exceptionalityPrimary));
                getBroker().saveBeanForced(disability);
            }
        }

        return iep;
    }

    /**
     * Create a workflow first phase for an Initial Referral workflow.
     *
     * @param iep
     */
    private void createWorkflow(IepData iep) {
        PlainDate today = new PlainDate();

        Workflow workflow = X2BaseBean.newInstance(Workflow.class, m_iepDictionary);
        workflow.setWorkflowDefinitionOid(m_wfd.getOid());
        workflow.setOwnerOid(iep.getOid());
        workflow.setDateInitiated(today);
        workflow.setUserOid(getUser().getOid());
        workflow.setOwnerView(iep.getStudent().getNameView());
        getBroker().saveBeanForced(workflow);

        WorkflowProgress progress1 = X2BaseBean.newInstance(WorkflowProgress.class, m_iepDictionary);
        progress1.setWorkflowOid(workflow.getOid());
        progress1.setWorkflowPhaseOid(m_wph1.getOid());
        progress1.setWorkflowPhaseOutcomeOid(m_wpo.getOid());
        progress1.setDate(today);
        progress1.setUserOid(getUser().getOid());
        getBroker().saveBeanForced(progress1);

        WorkflowProgress progress2 = X2BaseBean.newInstance(WorkflowProgress.class, m_iepDictionary);
        progress2.setWorkflowOid(workflow.getOid());
        progress2.setWorkflowPhaseOid(m_wph2.getOid());
        getBroker().saveBeanForced(progress2);

        GenericFormData gfd = X2BaseBean.newInstance(GenericFormData.class, m_formDictionary);
        gfd.setOrganization1Oid(OrganizationManager.ROOT_ORGANIZATION);
        gfd.setFieldValueByAlias(ALIAS_GFD_STAFF, iep.getStaffOid());
        gfd.setFieldValueByAlias(ALIAS_REFERRAL_TYPE, "Other");
        getBroker().saveBeanForced(gfd);

        FormInstance instance1 = X2BaseBean.newInstance(FormInstance.class, m_iepDictionary);
        instance1.setFormDefinitionOid(m_fmdIep.getOid());
        instance1.setOwnerObjectOid(iep.getOid());
        instance1.setOwnerView(iep.getStudent().getNameView());
        instance1.setCreatedTime(System.currentTimeMillis());
        instance1.setStorageObjectOid(iep.getOid());
        getBroker().saveBeanForced(instance1);

        FormInstance instance2 = X2BaseBean.newInstance(FormInstance.class, m_iepDictionary);
        instance2.setFormDefinitionOid(m_fmdReferral.getOid());
        instance2.setOwnerObjectOid(iep.getOid());
        instance2.setOwnerView(iep.getStudent().getNameView());
        instance2.setCreatedTime(System.currentTimeMillis());
        instance2.setStorageObjectOid(gfd.getOid());
        getBroker().saveBeanForced(instance2);

    }

    /**
     * Returns the most recent Special Ed Student Program Participation record for the student.
     *
     * @param studentOid
     *
     * @return StudentProgramParticipation
     */
    private List<StudentProgramParticipation> getPrograms(String studentOid) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentProgramParticipation.COL_STUDENT_OID, studentOid);
        criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, PROGRAM_CODE);
        BeanQuery query = new BeanQuery(StudentProgramParticipation.class, criteria);
        query.addOrderBy(StudentProgramParticipation.COL_START_DATE, false);
        return (List<StudentProgramParticipation>) getBroker().getCollectionByQuery(query);
    }

    /**
     * lookup and load iep and workflow resources for creating new iep.
     */
    private void loadResources() {
        ExtendedDataDictionary iepExtendedDictionary = SpedUtils.getIepDictionary(getOrganization(), getBroker());
        m_iepDictionary = DataDictionary.getDistrictDictionary(iepExtendedDictionary, getBroker().getPersistenceKey());
        m_converter = (DateAsStringConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER,
                getLocale(), true);

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(WorkflowDefinition.COL_ID, WORKFLOW_ID);
        BeanQuery query = new BeanQuery(WorkflowDefinition.class, criteria);
        m_wfd = getBroker().getBeanByQuery(query);

        criteria = new X2Criteria();
        criteria.addEqualTo(WorkflowPhase.COL_WORKFLOW_DEFINITION_OID, m_wfd.getOid());
        criteria.addEqualTo(WorkflowPhase.COL_ID, WORKFLOW_PHASE_ID1);
        query = new BeanQuery(WorkflowPhase.class, criteria);
        m_wph1 = getBroker().getBeanByQuery(query);

        criteria = new X2Criteria();
        criteria.addEqualTo(WorkflowPhase.COL_WORKFLOW_DEFINITION_OID, m_wfd.getOid());
        criteria.addEqualTo(WorkflowPhase.COL_ID, WORKFLOW_PHASE_ID2);
        query = new BeanQuery(WorkflowPhase.class, criteria);
        m_wph2 = getBroker().getBeanByQuery(query);

        criteria = new X2Criteria();
        criteria.addEqualTo(WorkflowPhaseOutcome.COL_WORKFLOW_PHASE_OID, m_wph1.getOid());
        criteria.addEqualTo(WorkflowPhaseOutcome.COL_ID, WORKFLOW_OUTCOME_ID);
        query = new BeanQuery(WorkflowPhaseOutcome.class, criteria);
        m_wpo = getBroker().getBeanByQuery(query);

        criteria = new X2Criteria();
        criteria.addEqualTo(FormDefinition.COL_ID, WORKFLOW_REFERRAL_FORM_ID);
        query = new BeanQuery(FormDefinition.class, criteria);
        m_fmdReferral = getBroker().getBeanByQuery(query);

        criteria = new X2Criteria();
        criteria.addEqualTo(FormDefinition.COL_ID, WORKFLOW_IEP_FORM_ID);
        query = new BeanQuery(FormDefinition.class, criteria);
        m_fmdIep = getBroker().getBeanByQuery(query);

        m_formDictionary = DataDictionary.getDistrictDictionary(m_fmdReferral.getExtendedDataDictionary(),
                getBroker().getPersistenceKey());

    }

    /**
     * @return
     */
    private Collection<SisStudent> getStudents() {
        if (m_query == null) {
            String studentActiveCode = PreferenceManager.getPreferenceValue(getOrganization(),
                    SisPreferenceConstants.SPED_ACTIVE_CODE);
            String studentReferredCode = PreferenceManager.getPreferenceValue(getOrganization(),
                    SisPreferenceConstants.SPED_REFERRED_CODE);

            List<String> spedCodes = new ArrayList<String>();
            spedCodes.add(studentActiveCode);
            spedCodes.add(studentReferredCode);

            X2Criteria criteria = new X2Criteria();
            criteria.addIn(SisStudent.COL_SPED_STATUS_CODE, spedCodes);

            m_query = new BeanQuery(SisStudent.class, criteria);
        }
        return getBroker().getCollectionByQuery(m_query);
    }

    /**
     * Lookup a message resource, or apply the default text.
     * format parameters into the mesage text if parameters are provided.
     *
     * @param key
     * @param defaultMessage
     * @param parameters
     *
     * @return String
     */
    private String getMessage(String key, String defaultMessage, Object[] parameters) {
        String message = m_resources.getMessage(getLocale(), key);
        if (message == null || message.startsWith("??")) {
            message = defaultMessage;
        }

        String formattedMessage = message;
        if (parameters != null && parameters.length > 0) {
            try {
                formattedMessage = MessageFormat.format(formattedMessage, parameters);
            } catch (IllegalArgumentException iae) {
                StringBuilder parameterBuffer = new StringBuilder(parameters.length * 16);

                for (int i = 0; i < parameters.length; i++) {
                    parameterBuffer.append("; ");
                    parameterBuffer.append(parameters[i].toString());
                }

                formattedMessage += parameterBuffer.toString();
            }
        }

        return formattedMessage;
    }
}
