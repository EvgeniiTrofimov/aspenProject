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

package com.x2dev.reports.sys.sped.dodea;

import static com.follett.fsc.core.k12.business.BusinessRules.IEP_AMENDMENT_ELIGIBILITY;
import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import static com.follett.fsc.core.k12.business.ValidationConstants.BUSINESS_RULE_VIOLATION;
import static com.follett.fsc.core.k12.business.ValidationConstants.CUSTOM_ERROR;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.BeanCopier;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.ChildDetailSet;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.forms.FormDetail;
import com.follett.fsc.core.k12.web.presentation.BlobFieldFormatter;
import com.follett.fsc.core.k12.web.presentation.FieldFormatter;
import com.follett.fsc.core.k12.web.presentation.FieldFormatterFactory;
import com.follett.fsc.core.k12.web.struts.UserEvent;
import com.follett.fsc.core.k12.web.template.Template;
import com.follett.fsc.core.k12.web.workflow.OutcomeDetail;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehavior;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehaviorFactory;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.math.MathContext;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.logging.Level;
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
public class SpedAmendmentProcedureAlt extends WorkflowProcedure implements DynamicFormProcedure {
    /*
     * Form ID constants
     */
    private static final String MEETING_FORM_ID = "MTG";

    private SpedWorkflowBehavior m_behavior = null;

    /**
     * Constructs a new SpedAmendmentProcedure.
     *
     * @param definition
     * @param district
     * @param user
     * @param broker
     * @param locale
     */
    public SpedAmendmentProcedureAlt(WorkflowDefinition definition, Organization district,
            User user, X2Broker broker, Locale locale) {
        super(definition, district, user, broker, locale);
        AppGlobals.getLog().severe("***X2 SPED: Modification procedure");
        m_behavior = SpedWorkflowBehaviorFactory.getWorkflowBehavior(getOrganization());
    }

    /*
     * Disability Constants should mathc the Disability Codes reference table codes
     */
    private static final String AUTISM = "Autism SD";
    private static final String DEAF_BLIND = "Deaf-Blindness";
    private static final String DEAF = "Deafness";
    private static final String DEVELOP = "Developmental Delay";
    private static final String EMOTIONAL = "Emotion Disturbance";
    private static final String HEARING = "Hearing Impairment";
    private static final String INTELLECTUAL = "Intellectual";
    private static final String MULTIPLE = "Multiple";
    private static final String ORTHO = "Ortho Impairment";
    private static final String OTHER = "Oth Hlth Impairment";
    private static final String SPEECH_ARTICULATION = "SL-Articulation";
    private static final String SPEECH_DYSFLUENCY = "SL-Dysfluency";
    private static final String SPEECH_LANGPHON = "SL-Language";
    private static final String SPEECH_VOICE = "SL-Voice";
    private static final String SPECIFIC = "Spec Lrn Disability";
    private static final String TBI = "Traumatic Brain Inj";
    private static final String VISION = "Visual Impairment";
    private static final String SPEECH_LANGUAGE = "Speech/Lang Impairments"; // added 061719 v1
                                                                             // updated 8/24/20

    /*
     * IEP hearing/vision aliases
     */
    private static final String IEP_HEARING_DATE_ALIAS = "csc-mtgmins-hear-date";
    private static final String IEP_HEARING_PASS_ALIAS = "csc-mtgmins-hear-result";
    private static final String IEP_VISION_DATE_ALIAS = "csc-mtgmins-vis-date";
    private static final String IEP_VISION_PASS_ALIAS = "csc-mtgmins-vis-result";

    private static final String REF_DISABILITY_CODES = "rtbSpedDisab  "; // OID for Disability Code
                                                                         // Reference Table
    private static final String DDX_OID = "ddxMaIep";
    private static final int iepAssessType = 0; // This is the default type for IEP for
                                                // IepAccommodation beans

    // OID for the Confirm Eligibility Form
    // private static final String ELIG_CONFIRM_OID = "FMD000001He0Pd";
    private static final String FORM_ID_ELIG_CONFIRM = "Confirm Elig";


    private static final String ELIGIBLE_DISABILITY = "Eligible";
    private static final int iepType = 0;
    private static final String ELIG_MEETING_PURPOSE = "Eligibility', 'ACCEPT IEP/ELIGIBILITY"; // this
                                                                                                // is
                                                                                                // the
                                                                                                // trigger
                                                                                                // to
                                                                                                // populate
                                                                                                // the
                                                                                                // Eligibility
                                                                                                // Meeting
                                                                                                // date
                                                                                                // field
    private static final String SPED_SETTING_CODES =
            "PSCD classroom','Self-Contained Classroom', 'Therapy Room', 'Resource Room";
    private static final String IEP_SERVICE_TOTAL_ALIAS = "iep-lre-service-min";
    private static final String IEP_SETTING_TOTAL_ALIAS = "iep-lre-setting-min";
    private static final String PERMISSION_DATE = "csc-rfa-permission-date";

    private static final String IEP_SERVICE_PCT_ALIAS = "iep-lre-service-per";
    private static final String IEP_SETTING_SPEDPCT_ALIAS = "iep-lre-sped-set-per";
    private static final String IEP_SETTING_GENEDPCT_ALIAS = "iep-lre-gened-set-per";

    // OID for the Assessment Request Form
    // private static final String REQUEST_FORM_OID = "FMD0000004p00S";
    private static final String FORM_ID_REQUEST = "Assess Request";

    // OID for the Confirm IEP Form
    // private static final String CONFIRM_IEP_OID = "FMD000001Hi06T";
    private static final String FORM_ID_CONFIRM_IEP = "Confirm IEP";

    // OID for the Individual Transition Plan
    // private static final String FINAL_TRANSITION_FORM_OID = "FMD0000003u0cf";
    private static final String FORM_ID_FINAL_TRANSITION = "Trans. Plan/Final Yr";

    // OID for the Age of Majority Form
    // private static final String MAJORITY_FORM_OID = "FMD0000003u09N";
    private static final String FORM_ID_MAJORITY = "Trnsfr of Rights";

    // OID for the Validate IEP Form
    // private static final String VALID_IEP_OID = "FMD000002Aw04L";
    private static final String FORM_ID_VALID_IEP = "IEP_VALID";

    private String formInstanceOwnerOid = "";

    /**
     * Finds the disabilities marked as eligible on the IepData table and creates records on the
     * Student Disability Table
     *
     * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior
     */
    public void transferDisability(IepData iep, String disability) {
        Boolean cExisting = checkExistingDisability(iep, disability);
        if (!(cExisting)) {
            IepDisability disabilityP = X2BaseBean.newInstance(IepDisability.class, getBroker().getPersistenceKey());
            disabilityP.setDisabilityCode(disability);
            disabilityP.setPlanType(iepType);
            disabilityP.setIepDataOid(iep.getOid());
            disabilityP.setStudentOid(iep.getStudent().getOid());
            getBroker().saveBeanForced(disabilityP);
        }
    }


    /**
     * Finds the date of the last meeting tied to the current IEP with the Purpose of Eligibility or
     * Eligibility/IEP
     *
     * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior
     */
    public String updateEligMeetingDate(IepData iep) {
        Date meetingDate = null;
        String meetingDateString = "";
        String query = new String(
                "SELECT  " +
                        "IMG_DATE " +
                        "FROM dbo.IEP_MEETING as IP " +
                        "WHERE IP.IMG_IEP_OID = '" + iep.getOid() + "'" +
                        " AND IP.IMG_FIELDB_002 IN ('" + ELIG_MEETING_PURPOSE + "')");

        try {
            Connection connection = getBroker().borrowConnection();
            Statement statement = connection.createStatement();


            try {
                ResultSet resultSet = statement.executeQuery(query);


                while (resultSet.next()) {
                    Date date = resultSet.getDate("IMG_DATE");
                    meetingDate = date;
                }

                resultSet.close();
            } catch (Exception e) {
            }

            statement.close();
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            getBroker().returnConnection();
        }

        if (meetingDate == null) {
            meetingDateString = "";
        } else {
            meetingDateString = meetingDate.toString();
        }
        return meetingDateString;
    }


    /**
     * Finds the suspected disabilities marked as eligible on the IepData table and creates records
     * on the Student Disability Table
     *
     * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior
     */
    public void writeDisabilities(String iepOid) {
        IepData iep = (IepData) getBroker().getBeanByOid(IepData.class, iepOid);
        String suspectedDisabilities = iep.getFieldC027() + iep.getFieldC028() + iep.getFieldC029();
        if (iep.getFieldB034() != null && iep.getFieldB034().contains(ELIGIBLE_DISABILITY)
                && (suspectedDisabilities.contains(VISION))) {
            transferDisability(iep, VISION);
        }
        if (iep.getFieldB036() != null && iep.getFieldB036().contains(ELIGIBLE_DISABILITY)
                && suspectedDisabilities.contains(AUTISM)) {
            transferDisability(iep, AUTISM);
        }
        if (iep.getFieldB038() != null && iep.getFieldB038().contains(ELIGIBLE_DISABILITY)
                && suspectedDisabilities.contains(DEAF)) {
            transferDisability(iep, DEAF);
        }
        if (iep.getFieldB040() != null && iep.getFieldB040().contains(ELIGIBLE_DISABILITY)
                && suspectedDisabilities.contains(DEAF_BLIND)) {
            transferDisability(iep, DEAF_BLIND);
        }
        if (iep.getFieldB042() != null && iep.getFieldB042().contains(ELIGIBLE_DISABILITY)
                && suspectedDisabilities.contains(DEVELOP)) {
            transferDisability(iep, DEVELOP);
        }
        if (iep.getFieldB044() != null && iep.getFieldB044().contains(ELIGIBLE_DISABILITY)
                && suspectedDisabilities.contains(EMOTIONAL)) {
            transferDisability(iep, EMOTIONAL);
        }
        if (iep.getFieldB046() != null && iep.getFieldB046().contains(ELIGIBLE_DISABILITY)
                && suspectedDisabilities.contains(INTELLECTUAL)) {
            transferDisability(iep, INTELLECTUAL);
        }
        if (iep.getFieldB048() != null && iep.getFieldB048().contains(ELIGIBLE_DISABILITY)
                && suspectedDisabilities.contains(HEARING)) {
            transferDisability(iep, HEARING);
        }
        if (iep.getFieldB050() != null && iep.getFieldB050().contains(ELIGIBLE_DISABILITY)
                && suspectedDisabilities.contains(ORTHO)) {
            transferDisability(iep, ORTHO);
        }
        if (iep.getFieldB052() != null && iep.getFieldB052().contains(ELIGIBLE_DISABILITY)
                && suspectedDisabilities.contains(OTHER)) {
            transferDisability(iep, OTHER);
        }
        if (iep.getFieldB054() != null && iep.getFieldB054().contains(ELIGIBLE_DISABILITY)
                && suspectedDisabilities.contains(MULTIPLE)) {
            transferDisability(iep, MULTIPLE);
        }
        if (iep.getFieldB056() != null && iep.getFieldB056().contains(ELIGIBLE_DISABILITY)
                && suspectedDisabilities.contains(SPECIFIC)) {
            transferDisability(iep, SPECIFIC);
        }
        if (iep.getFieldB060() != null && iep.getFieldB060().contains(ELIGIBLE_DISABILITY)
                && suspectedDisabilities.contains(TBI)) {
            transferDisability(iep, TBI);
        }
        // deleted old disability conditionals(4) 061719 v1
        // added 061719 v1
        if (iep.getFieldB058() != null && iep.getFieldB058().contains(ELIGIBLE_DISABILITY)
                && (iep.getFieldB065().contains("Yes") || iep.getFieldB063().contains("Yes")
                        || iep.getFieldB062().contains("Yes") || iep.getFieldB064().contains("Yes"))
                && suspectedDisabilities.contains("SL-")) {
            transferDisability(iep, SPEECH_LANGUAGE);
        }

        Collection<IepDisability> disabilities = iep.getIepDisability();
        if (disabilities != null) {
            int size = disabilities.size();
            if (size == 1) {
                for (IepDisability disability : disabilities) {
                    disability.setPrimaryIndicator(true);
                    getBroker().saveBeanForced(disability);
                }
            }
        }

        iep.setFieldA078(updateEligMeetingDate(iep));

    }

    /**
     * Verify the start and end dates are set on the IEP for the Convene IEP Development meeting
     * phase.
     *
     * @param progress
     * @throws Exception
     */
    public void executeConveneDevMeeting(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);
        if (iep.getStartDate() == null || iep.getEndDate() == null) {
            String errorMessage = "Please ensure the start and end dates of the IEP are recorded on the Details tab.";
            List<ValidationError> errors = new ArrayList<ValidationError>();
            errors.add(new ValidationError(CUSTOM_ERROR, null, errorMessage));
            addValidationErrors(errors);
        }
    }

    /**
     * Verify the case manager is set on the IEP for the Send Parent Permission for Evaluation
     * phase.
     *
     * @param progress
     * @throws Exception
     */
    public void executeSendParentPermission(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();
        boolean isError = false;
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);
        FormInstance form = progress.getFormInstanceById("Parent Perm", getBroker());
        GenericFormData formData = (GenericFormData) form.getStorageObject(broker);
        String staffOid = formData.getFieldO001();
        if (StringUtils.isEmpty(staffOid)) {
            String errorMessage = "Please ensure Case Manager field is recorded.";
            List<ValidationError> errors = new ArrayList<ValidationError>();
            errors.add(new ValidationError(CUSTOM_ERROR, null, errorMessage));
            addValidationErrors(errors);
            isError = true;
        }
        if (!isError && (iep.getStaffOid() == null || !iep.getStaffOid().equals(staffOid))) {
            iep.setStaffOid(staffOid);
            broker.saveBeanForced(iep);
        }
    }

    /**
     * Reads the three disabilities from the IEP Data Table(after the confirmation phase)
     *
     * Creates Required Assessment Records for each disability (only a single record but notated if
     * for additional disabilities)
     *
     * Updated to reflect the possible selection of Multiple in one of the first three disabilities
     */
    public void executeReqAssess(WorkflowProgress progress) throws Exception {
        X2Broker broker = getBroker();
        IepData iep = (IepData) progress.getWorkflow().getOwner(broker);
        copyDisabilities(iep); // this copies the suspected disabilities to the Eligibility
                               // Disabilities Fields
        List<String> disabilities = new ArrayList();
        if (MULTIPLE.contains(iep.getFieldC027())) // checks first disability field for Multiple
        {
            if (iep.getFieldC033() != null) // first multiple disability field
            {
                disabilities.add("MU" + iep.getFieldC033());
            }

            if (iep.getFieldC034() != null) // second multiple disability field
            {
                disabilities.add("MU" + iep.getFieldC034());
            }

            if (iep.getFieldC035() != null) // third multiple disability field
            {
                disabilities.add("MU" + iep.getFieldC035());
            }
        } else {
            disabilities.add(iep.getFieldC027());
        }

        if (!(StringUtils.isEmpty(iep.getFieldC028()))) {
            if (MULTIPLE.contains(iep.getFieldC028())) {
                if (iep.getFieldC033() != null) // first multiple disability field
                {
                    disabilities.add("MU" + iep.getFieldC033());
                }

                if (iep.getFieldC034() != null) // second multiple disability field
                {
                    disabilities.add("MU" + iep.getFieldC034());
                }

                if (iep.getFieldC035() != null) // third multiple disability field
                {
                    disabilities.add("MU" + iep.getFieldC035());
                }
            } else {
                disabilities.add(iep.getFieldC028());
                // AppGlobals.getLog().severe("Adding 28 " + iep.getFieldC028());
            }

        }

        if (!(StringUtils.isEmpty(iep.getFieldC029()))) {
            if (MULTIPLE.contains(iep.getFieldC029())) {
                if (iep.getFieldC033() != null) // first multiple disability field
                {
                    disabilities.add("MU" + iep.getFieldC033());
                }

                if (iep.getFieldC034() != null) // second multiple disability field
                {
                    disabilities.add("MU" + iep.getFieldC034());
                }

                if (iep.getFieldC035() != null) // third multiple disability field
                {
                    disabilities.add("MU" + iep.getFieldC035());
                }
            } else {
                disabilities.add(iep.getFieldC029());
            }

        }

        for (String dis : disabilities) {
            Boolean multiple = false;
            if (dis.startsWith("MU")) {
                dis = dis.substring(2);
                multiple = true;
            }
            String assessments = getDisabilityAssessmentString(dis);
            if (assessments != null && assessments.length() > 0) {
                List<String> required = Arrays.asList(assessments.split("\\s*,\\s*"));
                for (String req : required) {
                    String assessmentOid = getCheckExisting(iep, req);
                    if (assessmentOid == null) {
                        IepAccommodation assessmentRecord =
                                X2BaseBean.newInstance(IepAccommodation.class, getBroker().getPersistenceKey());
                        assessmentRecord.setName(req);
                        assessmentRecord.setExtendedDataDictionaryOid(DDX_OID);
                        if (multiple) {
                            assessmentRecord.setType(MULTIPLE);
                        } else {
                            assessmentRecord.setType(dis);
                        }
                        assessmentRecord.setPlanType(iepAssessType);
                        assessmentRecord.setIepDataOid(iep.getOid());
                        assessmentRecord.setStudentOid(iep.getStudent().getOid());
                        assessmentRecord.setFieldA007("1");
                        getBroker().saveBeanForced(assessmentRecord);
                    } else {
                        IepAccommodation existingAssessment =
                                (IepAccommodation) getBroker().getBeanByOid(IepAccommodation.class, assessmentOid);
                        if (multiple) {
                            dis = MULTIPLE;
                        }
                        if (existingAssessment.getFieldB002() == null) {
                            if (!(dis.contains(existingAssessment.getType()))) {
                                existingAssessment.setFieldB002(dis);
                                existingAssessment.setFieldA007("1");
                            }
                        } else {
                            if (existingAssessment.getFieldB003() == null) {
                                if (!(dis.contains(existingAssessment.getFieldB002()))
                                        && (!(dis.contains(existingAssessment.getType())))) {
                                    existingAssessment.setFieldB003(dis);
                                    existingAssessment.setFieldA007("1");
                                }
                            } else {
                                if (existingAssessment.getFieldB004() == null) {
                                    if (!(dis.contains(existingAssessment.getFieldB003()))
                                            && !(dis.contains(existingAssessment.getFieldB002()))
                                            && (!(dis.contains(existingAssessment.getType())))) {
                                        existingAssessment.setFieldB004(dis);
                                        existingAssessment.setFieldA007("1");
                                    }
                                } else {
                                    if (existingAssessment.getFieldB005() == null) {
                                        if (!(dis.contains(existingAssessment.getFieldB004()))
                                                && !(dis.contains(existingAssessment.getFieldB003()))
                                                && !(dis.contains(existingAssessment.getFieldB002()))
                                                && (!(dis.contains(existingAssessment.getType())))) {
                                            existingAssessment.setFieldB005(dis);
                                            existingAssessment.setFieldA007("1");
                                        }
                                    } else {
                                        if (existingAssessment.getFieldB006() == null) {
                                            if (!(dis.contains(existingAssessment.getFieldB005()))
                                                    && !(dis.contains(existingAssessment.getFieldB004()))
                                                    && !(dis.contains(existingAssessment.getFieldB003()))
                                                    && !(dis.contains(existingAssessment.getFieldB002()))
                                                    && (!(dis.contains(existingAssessment.getType())))) {
                                                existingAssessment.setFieldB006(dis);
                                                existingAssessment.setFieldA007("1");
                                            }
                                        }
                                    } // else B005 close

                                } // else B004 close

                            } // else B003 close
                        } // else B002 close
                        getBroker().saveBeanForced(existingAssessment);

                    } // assess Oid isnull close
                } // for Req close
            } // assessments.length > 0
        } // for dis close
        cleanupAssessments(iep); // this procedure removes any assessments not required by the
                                 // curernt Suspected List of disabilities

    } // Procedure close


    /**
     * Deletes any required assessment records not tied to current values in the Suspected
     * Disability Fields
     *
     * for use on the Eligibility Table
     *
     */
    public void cleanupAssessments(IepData iep) {
        String susD = iep.getFieldC027() + iep.getFieldC028() + iep.getFieldC029(); // only include
                                                                                    // the three
                                                                                    // suspected
        String allD = AUTISM + DEAF_BLIND + DEAF + DEVELOP + EMOTIONAL + HEARING + INTELLECTUAL + MULTIPLE + ORTHO
                + OTHER + SPEECH_ARTICULATION + SPEECH_DYSFLUENCY + SPEECH_LANGPHON + SPEECH_VOICE + SPECIFIC + TBI
                + VISION;
        Collection<IepAccommodation> assessments = iep.getAccommodations();
        if (assessments != null) {
            for (IepAccommodation assessment : assessments) {
                if (assessment.getFieldA007() != null && assessment.getFieldA007().equals("1")) // if
                                                                                                // not
                                                                                                // a
                                                                                                // required
                                                                                                // assessment
                                                                                                // we
                                                                                                // leave
                                                                                                // it
                                                                                                // alone
                {
                    // check to see if one of the 6 type fields have a disability code in it if not
                    // we ignore the reocrd
                    if (allD.contains(assessment.getType()) || allD.contains(assessment.getFieldB002())
                            || allD.contains(assessment.getFieldB003()) || allD.contains(assessment.getFieldB004())
                            || susD.contains(assessment.getFieldB005()) || allD.contains(assessment.getFieldB006())) {
                        // check to see if at least one of the 6 fields has one of the suspected
                        // disabilities if not we delete the record otherwise we confirm a suspected
                        // diability in each of the 6 fields
                        if (susD.contains(assessment.getType())
                                || (assessment.getFieldB002() != null ? susD.contains(assessment.getFieldB002())
                                        : false)
                                || (assessment.getFieldB003() != null ? susD.contains(assessment.getFieldB003())
                                        : false)
                                || (assessment.getFieldB004() != null ? susD.contains(assessment.getFieldB004())
                                        : false)
                                || (assessment.getFieldB005() != null ? susD.contains(assessment.getFieldB005())
                                        : false)
                                || (assessment.getFieldB006() != null ? susD.contains(assessment.getFieldB006())
                                        : false)) {
                            if (!(susD.contains(assessment.getType()))) // if the field value does
                                                                        // not match a suspected
                                                                        // diability then we set the
                                                                        // field to blank
                            {
                                assessment.setType("");
                            }
                            if (assessment.getFieldB002() != null && !(susD.contains(assessment.getFieldB002()))) // if
                                                                                                                  // the
                                                                                                                  // field
                                                                                                                  // value
                                                                                                                  // does
                                                                                                                  // not
                                                                                                                  // match
                                                                                                                  // a
                                                                                                                  // suspected
                                                                                                                  // diability
                                                                                                                  // then
                                                                                                                  // we
                                                                                                                  // set
                                                                                                                  // the
                                                                                                                  // field
                                                                                                                  // to
                                                                                                                  // blank
                            {
                                assessment.setFieldB002("");
                            }
                            if (assessment.getFieldB003() != null && !(susD.contains(assessment.getFieldB003()))) // if
                                                                                                                  // the
                                                                                                                  // field
                                                                                                                  // value
                                                                                                                  // does
                                                                                                                  // not
                                                                                                                  // match
                                                                                                                  // a
                                                                                                                  // suspected
                                                                                                                  // diability
                                                                                                                  // then
                                                                                                                  // we
                                                                                                                  // set
                                                                                                                  // the
                                                                                                                  // field
                                                                                                                  // to
                                                                                                                  // blank
                            {
                                assessment.setFieldB003("");
                            }
                            if (assessment.getFieldB004() != null && !(susD.contains(assessment.getFieldB004()))) // if
                                                                                                                  // the
                                                                                                                  // field
                                                                                                                  // value
                                                                                                                  // does
                                                                                                                  // not
                                                                                                                  // match
                                                                                                                  // a
                                                                                                                  // suspected
                                                                                                                  // diability
                                                                                                                  // then
                                                                                                                  // we
                                                                                                                  // set
                                                                                                                  // the
                                                                                                                  // field
                                                                                                                  // to
                                                                                                                  // blank
                            {
                                assessment.setFieldB004("");
                            }
                            if (assessment.getFieldB005() != null && !(susD.contains(assessment.getFieldB005()))) // if
                                                                                                                  // the
                                                                                                                  // field
                                                                                                                  // value
                                                                                                                  // does
                                                                                                                  // not
                                                                                                                  // match
                                                                                                                  // a
                                                                                                                  // suspected
                                                                                                                  // diability
                                                                                                                  // then
                                                                                                                  // we
                                                                                                                  // set
                                                                                                                  // the
                                                                                                                  // field
                                                                                                                  // to
                                                                                                                  // blank
                            {
                                assessment.setFieldB005("");
                            }
                            if (assessment.getFieldB006() != null && !(susD.contains(assessment.getFieldB006()))) // if
                                                                                                                  // the
                                                                                                                  // field
                                                                                                                  // value
                                                                                                                  // does
                                                                                                                  // not
                                                                                                                  // match
                                                                                                                  // a
                                                                                                                  // suspected
                                                                                                                  // diability
                                                                                                                  // then
                                                                                                                  // we
                                                                                                                  // set
                                                                                                                  // the
                                                                                                                  // field
                                                                                                                  // to
                                                                                                                  // blank
                            {
                                assessment.setFieldB006("");
                            }
                            getBroker().saveBeanForced(assessment);
                        } else // Assessment record type fields has at least one valid disability
                               // but no suspected disabilities
                        {
                            AppGlobals.getLog().severe("Ready for Deletion of " + assessment.getType());
                            deleteAssessment(assessment);
                        }
                    }

                }
            }
        }

    }


    /**
     * Deletes an IEPAccommodation Record based on a passed in OID
     *
     *
     *
     */
    public void deleteAssessment(IepAccommodation assessment) {
        String deleteStatement = " DELETE  " +
                "   FROM STUDENT_ACCOMMODATION " +
                "  WHERE STUDENT_ACCOMMODATION.IAC_OID = '" + assessment.getOid() + "' ";


        try {
            Connection connection = getBroker().borrowConnection();
            Statement statement = connection.createStatement();

            try {
                ResultSet resultSet = statement.executeQuery(deleteStatement);
                AppGlobals.getLog().severe("Assessment Record Deleted ");

            } catch (Exception e) {
            }

            statement.close();
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            getBroker().returnConnection();
        }
    }


    /**
     * Finds the assessment string of the given Disbaility Code
     *
     */
    public String getDisabilityAssessmentString(String dis) {

        String assessString = "";
        String query = new String(
                "SELECT  " +
                        "RCD_FIELDD_001 " +
                        "FROM dbo.REF_CODE as RCD " +
                        "WHERE RCD_RTB_OID = 'RTB0000004M0UL'" +
                        " AND RCD_CODE = '" + dis + "'");

        try {
            Connection connection = getBroker().borrowConnection();
            Statement statement = connection.createStatement();


            try {
                ResultSet resultSet = statement.executeQuery(query);


                while (resultSet.next()) {
                    String name = resultSet.getString("RCD_FIELDD_001");
                    assessString = name;
                }

                resultSet.close();
            } catch (Exception e) {
            }

            statement.close();
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            getBroker().returnConnection();
        }

        return assessString;
    }



    /**
     * Returns the OID of the Assessment record if it already exists in the list of procedures for
     * this IEP
     *
     */
    public String getCheckExisting(IepData iep1, String dis) {

        String checkString = null;
        String query = new String(
                "SELECT  " +
                        "IAC_OID " +
                        "FROM dbo.STUDENT_ACCOMMODATION as SA " +
                        "WHERE SA.IAC_IEP_OID = '" + iep1.getOid() + "'" +
                        " AND SA.IAC_NAME = '" + dis + "'");

        try {
            Connection connection = getBroker().borrowConnection();
            Statement statement = connection.createStatement();


            try {
                ResultSet resultSet = statement.executeQuery(query);


                while (resultSet.next()) {
                    String name = resultSet.getString("IAC_OID");
                    checkString = name;
                }

                resultSet.close();
            } catch (Exception e) {
            }

            statement.close();
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            getBroker().returnConnection();
        }

        return checkString;
    }

    /**
     * Returns the Date 45 days after the completion date of the Phase
     *
     */
    public Date getCompletionDate(IepData iep1, WorkflowProgress progress) {

        Date completionDate = null;
        if (progress.getDate() != null) {
            String calendar =
                    "(SELECT CAS_OID FROM CALENDAR_SCHOOL WHERE CALENDAR_SCHOOL.CAS_CALENDAR_ID = 'Standard' AND CALENDAR_SCHOOL.CAS_SKL_OID = '"
                            + iep1.getStudent().getSchool().getOid() + "')";
    
            String query = new String(
                    "SELECT t.CSD_DATE  " +
                            "FROM " +
                            "(SELECT CSD_DATE, ROW_NUMBER() OVER(ORDER BY CSD_DATE) AS rownum " +
                            "FROM CALENDAR_SCHOOL_DATE cd" +
                            " WHERE cd.CSD_DATE > '" + progress.getDate() + "'" +
                            " AND cd.CSD_CAS_OID IN " + calendar +
                            " AND cd.CSD_IN_SESSION_IND = '1') AS t" +
                            " WHERE t.rownum = 45" +
                            " Order by t.CSD_DATE ");
            try {
                Connection connection = getBroker().borrowConnection();
                Statement statement = connection.createStatement();
    
                try {
                    ResultSet resultSet = statement.executeQuery(query);
    
                    while (resultSet.next()) {
                        completionDate = resultSet.getDate("CSD_DATE");
                    }
    
                    resultSet.close();
                } catch (Exception e) {
                }
    
                statement.close();
            } catch (SQLException sqle) {
                AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
            } finally {
                getBroker().returnConnection();
            }
        }
        return completionDate;
    }

    /**
     * Returns a number of Service Minutes per week for this IEP, service start date must equal IEP
     * start date
     *
     */
    public int calculateServiceMinutes(IepData iep1) {

        int totalMinutes = 0;
        if (iep1.getStartDate() != null) {
            String query = new String(
                    "SELECT  " +
                            "ISV_DURATION, ISV_CYCLE " +
                            "FROM dbo.IEP_SERVICE as S " +
                            "WHERE S.ISV_IEP_OID = '" + iep1.getOid() + "'" +
                            " AND S.ISV_START_DATE <= '" + iep1.getStartDate() + "'");
    
            try {
                Connection connection = getBroker().borrowConnection();
                Statement statement = connection.createStatement();
    
    
                try {
                    ResultSet resultSet = statement.executeQuery(query);
    
    
                    while (resultSet.next()) {
                        int duration = resultSet.getInt("ISV_DURATION");
                        String cycle = resultSet.getString("ISV_CYCLE");
                        switch (cycle) {
                            case "Monthly":
                                duration = duration / 4;
                                break;
                            case "10 days":
                                duration = duration / 2;
                                break;
                            case "Daily":
                                duration = duration * 5;
                                break;
                            case "Quarterly":
                                duration = duration / 9;
                                break;
                            case "Yearly":
                                duration = duration / 36;
                                break;
                            default:
                                duration = duration;
                        }
                        totalMinutes = totalMinutes + duration;
    
    
                    }
    
                    resultSet.close();
                } catch (Exception e) {
                }
    
                statement.close();
            } catch (SQLException sqle) {
                AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
            } finally {
                getBroker().returnConnection();
            }
        }
        return totalMinutes;
    }

    /**
     * Returns a number of SPED Setting Service Minutes per week for this IEP, service start date
     * must equal IEP start date
     *
     */
    public int calculateSettingMinutes(IepData iep1) {

        int settingMinutes = 0;
        if (iep1.getStartDate() != null) {
            String query = new String(
                    "SELECT  " +
                            "ISV_DURATION, ISV_CYCLE " +
                            "FROM dbo.IEP_SERVICE as S " +
                            "WHERE S.ISV_IEP_OID = '" + iep1.getOid() + "'" +
                            " AND S.ISV_SETTING_CODE IN ('" + SPED_SETTING_CODES + "')" +
                            " AND S.ISV_START_DATE <= '" + iep1.getStartDate() + "'");
    
            try {
                Connection connection = getBroker().borrowConnection();
                Statement statement = connection.createStatement();
    
    
                try {
                    ResultSet resultSet = statement.executeQuery(query);
    
    
                    while (resultSet.next()) {
                        int duration = resultSet.getInt("ISV_DURATION");
                        String cycle = resultSet.getString("ISV_CYCLE");
                        switch (cycle) {
                            case "Monthly":
                                duration = duration / 4;
                                break;
                            case "10 days":
                                duration = duration / 2;
                                break;
                            case "Daily":
                                duration = duration * 5;
                                break;
                            case "Quarterly":
                                duration = duration / 9;
                                break;
                            case "Yearly":
                                duration = duration / 36;
                                break;
                            default:
                                duration = duration;
                        }
                        settingMinutes = settingMinutes + duration;
    
                    }
    
                    resultSet.close();
                } catch (Exception e) {
                }
    
                statement.close();
            } catch (SQLException sqle) {
                AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
            } finally {
                getBroker().returnConnection();
            }
        }
        return settingMinutes;
    }

    /**
     * Returns a
     *
     */
    public Boolean checkExistingDisability(IepData iep1, String dis) {
        Boolean checkString = false;
        String query = new String(
                "SELECT  " +
                        "IDB_OID " +
                        "FROM dbo.STUDENT_DISABILITY as SD " +
                        "WHERE SD.IDB_IEP_OID = '" + iep1.getOid() + "'" +
                        " AND SD.IDB_DISABILITY_CODE = '" + dis + "'");

        try {
            Connection connection = getBroker().borrowConnection();
            Statement statement = connection.createStatement();


            try {
                ResultSet resultSet = statement.executeQuery(query);


                while (resultSet.next()) {
                    String name = resultSet.getString("IDB_OID");
                    checkString = true;
                }

                resultSet.close();
            } catch (Exception e) {
            }

            statement.close();
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            getBroker().returnConnection();
        }

        return checkString;
    }


    /**
     * Copies the 6 disability fields from the IEP Data Table to 6 fields on the IEP Data Table
     *
     * for use on the Eligibility Table
     *
     */
    public void copyDisabilities(IepData iepD) {
        // set all to blank first - this clears existing values if this procedure is run a second
        // time
        iepD.setFieldC030("");
        iepD.setFieldC031("");
        iepD.setFieldC032("");
        iepD.setFieldC036("");
        iepD.setFieldC037("");
        iepD.setFieldC038("");

        iepD.setFieldC030(iepD.getFieldC027());
        iepD.setFieldC031(iepD.getFieldC028());
        iepD.setFieldC032(iepD.getFieldC029());
        iepD.setFieldC036(iepD.getFieldC033());
        iepD.setFieldC037(iepD.getFieldC034());
        iepD.setFieldC038(iepD.getFieldC035());
        getBroker().saveBeanForced(iepD);
    }

    /**
     * Calculates the Total Service Minutes per week and Total Number of SPED Setting Minutes
     *
     * and updates all of the dates on the IEP (IEP Data and Student Table)
     *
     */
    public void confirmIep(String iepOid, DataDictionary dictionary) throws X2BaseException {
        X2Broker broker = getBroker();
        DecimalFormat df = new DecimalFormat("#0.0");
        IepData iepConfirm = (IepData) getBroker().getBeanByOid(IepData.class, iepOid);
        String serviceMin = "" + calculateServiceMinutes(iepConfirm);
        String settingMin = "" + calculateSettingMinutes(iepConfirm);
        iepConfirm.setFieldValueByAlias(IEP_SERVICE_TOTAL_ALIAS, serviceMin, dictionary);
        iepConfirm.setFieldValueByAlias(IEP_SETTING_TOTAL_ALIAS, settingMin, dictionary);
        String servicePct;
        String settingSpedPct;
        String settingGenEdPct;
        MathContext mcD = new MathContext(4);
        MathContext mc = new MathContext(2);
        BigDecimal hundred = new BigDecimal(100);
        BigDecimal week = new BigDecimal(1950);

        if ("CF,PK3,PK".contains(iepConfirm.getStudent().getGradeLevel())
                && (!("K".contains(iepConfirm.getStudent().getGradeLevel())))) {
            servicePct = "";
            settingSpedPct = "";
            settingGenEdPct = "";
        } else {

            BigDecimal sP = new BigDecimal(serviceMin);
            sP = sP.divide(week, mcD);
            sP = sP.multiply(hundred, mc);
            servicePct = "" + df.format(sP);
            BigDecimal setP = new BigDecimal(settingMin);
            setP = setP.divide(week, mcD);
            setP = setP.multiply(hundred, mc);
            settingSpedPct = "" + df.format(setP);
            BigDecimal spedGenSetPct = hundred.subtract(setP);
            settingGenEdPct = "" + df.format(spedGenSetPct);
            iepConfirm.setFieldValueByAlias(IEP_SERVICE_PCT_ALIAS, servicePct, dictionary);
            iepConfirm.setFieldValueByAlias(IEP_SETTING_SPEDPCT_ALIAS, settingSpedPct, dictionary);
            iepConfirm.setFieldValueByAlias(IEP_SETTING_GENEDPCT_ALIAS, settingGenEdPct, dictionary);
        }

        setIepDates(iepConfirm);
        getBroker().saveBeanForced(iepConfirm);
    }

    // updated 2/3/2019 to correct the corrected phases being pulled (Use Like rather than In)
    public PlainDate getIepMeetingDate(IepData iep) {
        PlainDate meetingDate = iep.getStartDate() != null ? iep.getStartDate() : new PlainDate();

        String query = new String(
                " SELECT IMG_DATE FROM dbo.IEP_MEETING " +
                        " Inner Join FORM_INSTANCE ON IMG_OID = FMI_OBJ_OID_STORAGE " +
                        " WHERE FMI_OID IN " +
                        "(SELECT WFF_FMI_OID FROM WORKFLOW_PROGRESS_FORM " +
                        " WHERE WFF_WFP_OID IN " +
                        " (SELECT WFP_OID FROM dbo.WORKFLOW_PROGRESS " +
                        " Inner Join WORKFLOW ON WFP_WFL_OID = WFL_OID " +
                        " WHERE WFL_OWNER_OID = '" + iep.getOid() + "'" +
                        " AND WFP_WPH_OID IN " +
                        " (SELECT WPH_OID FROM WORKFLOW_PHASE WHERE WPH_NAME " +
                        "  LIKE 'Convene IEP Development Meeting%'" +
                        " OR WPH_NAME LIKE 'Convene Annual Review/IEP Development Meeting%'" +
                        " OR WPH_NAME LIKE 'Convene Annual Review Meeting%')))");


        try {
            Connection connection = getBroker().borrowConnection();
            Statement statement = connection.createStatement();

            try {
                ResultSet resultSet = statement.executeQuery(query);
                Date resultDate;

                while (resultSet.next()) {
                    resultDate = resultSet.getDate("IMG_DATE");
                    meetingDate = new PlainDate(resultDate);
                }

                resultSet.close();
            } catch (Exception e) {
            }

            statement.close();
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            getBroker().returnConnection();
        }

        return meetingDate;
    }

    /**
     * Calculates and updates the fields on the Request for Assessment
     *
     * designed to run on the Parent Permission Phase Accept outcome
     *
     */
    public void executeParentPermission(WorkflowProgress progress) throws Exception {

        String compDate = "";
        X2Broker broker = getBroker();
        IepData iepAssess = (IepData) progress.getWorkflow().getOwner(broker);
        FormInstance requestAssess = X2BaseBean.newInstance(FormInstance.class, getBroker().getPersistenceKey());
        requestAssess.setFormDefinitionOid(getFormDefinitionOid(FORM_ID_REQUEST));
        requestAssess.setOwnerObjectOid(iepAssess.getOid());
        requestAssess.setCreatedTime(System.currentTimeMillis());

        GenericFormData requestData = X2BaseBean.newInstance(GenericFormData.class, getBroker().getPersistenceKey());
        getBroker().saveBeanForced(requestData);

        requestAssess.setStorageObjectOid(requestData.getOid());
        requestAssess.setOwnerView(iepAssess.getStudent().getNameView());

        requestData.setExtendedDataDictionaryOid(DDX_OID);
        requestData.setFieldA022("1"); // Permission Field

        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
        String permissionDate = formatter.format(progress.getDate());
        Date completionDate = getCompletionDate(iepAssess, progress);
        if (completionDate != null) {
            compDate = formatter.format(completionDate);
        }

        requestData.setFieldA023(permissionDate); // Permission Date Field (From Permission Phase
                                                  // -Note : It must be called from that phase)
        requestData.setFieldA025(compDate); // Completion Date - Based on the school calendar and
                                            // build calendar of the student attached to the IEP
        requestData.setFieldC001(
                iepAssess.getStaff().getPerson().getFirstName() + " " + iepAssess.getStaff().getPerson().getLastName());
        requestData.setFieldB022(iepAssess.getStaff().getPerson().getPhone02()); // Duty Phone from
                                                                                 // the Person Table


        getBroker().saveBeanForced(requestAssess);
        getBroker().saveBeanForced(requestData);

    }

    /**
     *
     * Updates all of the dates on the IEP (IEP Data and Student Table)
     *
     */
    public void setIepDates(IepData iepConfirm) throws X2BaseException {
        SisStudent student = (SisStudent) getBroker().getBeanByOid(SisStudent.class, iepConfirm.getStudentOid());
        Date dateMeeting = new Date();
        PlainDate nextReview;
        Calendar c = Calendar.getInstance();
        c.setTime(getIepMeetingDate(iepConfirm)); // 1 Year from IEP Development Meeting Date
        c.add(Calendar.YEAR, 1);
        nextReview = new PlainDate(c.getTime());
        PlainDate nextEvaluation;
        Calendar cE = Calendar.getInstance();
        SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd");

        try {
            dateMeeting = format.parse(iepConfirm.getFieldA078());
        }

        catch (Exception e) {
        } finally {
        }


        cE.setTime(dateMeeting); // 3 years from the IEP Eligibility Meeting Date
        cE.add(Calendar.YEAR, 3);
        nextEvaluation = new PlainDate(cE.getTime());
        PlainDate lastReview = new PlainDate(iepConfirm.getStartDate());
        PlainDate lastEvaluation = new PlainDate(dateMeeting);
        switch (iepConfirm.getFieldB002()) {
            case "Initial IEP":
                iepConfirm.setNextReviewDate(nextReview);
                iepConfirm.setNextEvaluationDate(nextEvaluation);
                student.setSpedNextReviewDate(nextReview);
                student.setSpedNextEvaluationDate(nextEvaluation);
                student.setSpedLastReviewDate(lastReview);
                student.setSpedLastEvaluationDate(lastEvaluation);
                break;
            case "Modification":
                break;
            case "Triennial Review":
                iepConfirm.setNextReviewDate(nextReview);
                iepConfirm.setNextEvaluationDate(nextEvaluation);
                student.setSpedNextReviewDate(nextReview);
                student.setSpedNextEvaluationDate(nextEvaluation);
                student.setSpedLastReviewDate(lastReview);
                student.setSpedLastEvaluationDate(lastEvaluation);
                break;
            case "Annual Review":
                iepConfirm.setNextReviewDate(nextReview);
                student.setSpedNextReviewDate(nextReview);
                student.setSpedLastReviewDate(lastReview);
                break;
            case "Transfer From EDIS":
                iepConfirm.setNextReviewDate(nextReview);
                iepConfirm.setNextEvaluationDate(nextEvaluation);
                student.setSpedNextReviewDate(nextReview);
                student.setSpedNextEvaluationDate(nextEvaluation);
                student.setSpedLastReviewDate(lastReview);
                student.setSpedLastEvaluationDate(lastEvaluation);
                break;

        }

        getBroker().saveBeanForced(iepConfirm);
        getBroker().saveBeanForced(student);
    }


    /**
     * returns a boolean value based on student grade level(11 or 12), iep end date(if after current
     * school year and 11th grade) and presence of Final Transition Plan
     *
     */
    public boolean getFinalTrans(IepData iep) {
        boolean finalTrans = false;
        Date d = getOrganization().getCurrentContext().getEndDate();
        if (("11".contains(iep.getStudent().getGradeLevel()) && (iep.getEndDate().compareTo(d) > 0))
                || "12".contains(iep.getStudent().getGradeLevel())) {
            if (!(findForm(iep, getFormDefinitionOid(FORM_ID_FINAL_TRANSITION)))) {
                finalTrans = true;
            }
        }
        return finalTrans;
    }

    /**
     * returns a boolean value based on if all services have a linked goal(s)
     *
     */
    public boolean getLinkedGoalstoServices(IepData iep) {
        boolean linked = false;
        for (IepGoal goal : iep.getIepGoals()) {
            for (IepServiceGoalAlignment alignment : goal.getIepServiceGoalAlignments(getBroker())) {
                if (!alignment.getIepService().getServiceMode().contains("Secret Agent")
                        && !StringUtils.isEmpty(alignment.getIepService().getProviderCode())) {
                    linked = true;
                }
            }
        }
        return linked;
    }

    /*
     * Returns the form instance for the passed owner and definition ID. Note this method does not
     * distinguish if multiple instances of a form are supported for 1 owner.
     *
     * @param owner
     *
     * @param formId
     *
     * return boolean
     */
    private boolean findForm(IepData owner, String formOid) {
        boolean checkForm = false;
        Criteria criteria = new Criteria();
        criteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, owner.getOid());
        criteria.addEqualTo(FormInstance.REL_FORM_DEFINITION + "." + FormDefinition.COL_OID, formOid);
        QueryByCriteria query = new QueryByCriteria(FormInstance.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                FormInstance form = (FormInstance) iterator.next();
                checkForm = true;
            }

        } finally {
            iterator.close();
        }

        return checkForm;
    }

    /**
     * checks to see if any errors (contains ***) are in the passed Array
     *
     */
    public boolean checkErrors(List<String> list) {
        boolean check = false;
        for (String element : list) {
            if (element.contains("***")) {
                check = true;
            }
        }
        return check;
    }

    /**
     * Returns a boolean based on the presence of any disability attached to the passed IEP
     *
     */
    public Boolean checkAnyExistingDisability(IepData iep1) {
        Boolean checkString = false;
        String query = new String(
                "SELECT  " +
                        "IDB_OID " +
                        "FROM dbo.STUDENT_DISABILITY as SD " +
                        "WHERE SD.IDB_IEP_OID = '" + iep1.getOid() + "'");

        try {
            Connection connection = getBroker().borrowConnection();
            Statement statement = connection.createStatement();


            try {
                ResultSet resultSet = statement.executeQuery(query);


                while (resultSet.next()) {
                    String name = resultSet.getString("IDB_OID");
                    checkString = true;
                }

                resultSet.close();
            } catch (Exception e) {
            }

            statement.close();
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            getBroker().returnConnection();
        }

        return checkString;
    }

    /**
     * Returns a boolean based on the presence of a primary disability attached to the passed IEP
     *
     */
    public Boolean checkAnyExistingPrimaryDisability(IepData iep1) {
        Boolean checkString = false;
        for (IepDisability disability : iep1.getIepDisability()) {
            if (disability.getPrimaryIndicator()) {
                checkString = true;
            }
        }

        return checkString;
    }

    /**
     * returns a Boolean based on whether the student will turn 16 prior to the end of the IEP
     *
     */
    public boolean getYear16(IepData iep) {
        boolean recent = false;
        Calendar cal = new GregorianCalendar();
        cal.setTime(iep.getStudent().getPerson().getDob());
        cal.add(Calendar.YEAR, +16);
        Date bday16 = cal.getTime();
        PlainDate bday16Plain = new PlainDate(bday16);
        PlainDate endDate = iep.getEndDate();
        if (endDate == null) {
            endDate = new PlainDate();
        }
        if (endDate.after(bday16Plain)) {
            recent = true;
        }
        return recent;
    }

    /**
     * returns a Boolean based on whether the student will turn 17 prior to the end of the IEP
     *
     */
    public boolean getYear17(IepData iep) {
        boolean majority = true;
        boolean recent = false;
        Calendar cal = new GregorianCalendar();
        cal.setTime(iep.getStudent().getPerson().getDob());
        cal.add(Calendar.YEAR, +17);
        Date bday17 = cal.getTime();
        PlainDate bday17Plain = new PlainDate(bday17);
        PlainDate endDate = iep.getEndDate();
        if (endDate == null) {
            endDate = new PlainDate();
        }
        if (endDate.after(bday17Plain)) {
            recent = true;
        }
        {
            if (recent && !(findForm(iep, getFormDefinitionOid(FORM_ID_MAJORITY)))) {
                majority = false;
            }
        }
        return majority;
    }


    /**
     * Runs a series of methods to determine status of validation tasks
     *
     * Saves the results to a validation field
     */
    public void executeValidation(WorkflowProgress progress) {
        IepData iep = null;
        if (progress != null) {
            iep = (IepData) progress.getWorkflow().getOwner(getBroker());
        } else {
            iep = (IepData) getBroker().getBeanByOid(IepData.class, formInstanceOwnerOid);
        }

        // gather strings for each check and any error messages
        // IEP start date is blank

        String startDateCheck = validateStartDate(iep);
        String startDate = (startDateCheck.contains("***") ? startDateCheck : "");

        // IEP end date is blank
        String endDate = (iep.getEndDate() != null ? "" : "*** The End Date is blank");

        // Service Start dates are on or after IEP Start Date
        String servicesDatesValidate = "";
        // if (!(iep.getFieldB002().contains("Modification")))
        // {
        // String servicesDatesValidateCheck = validateServiceDates(iep);
        // servicesDatesValidate = (servicesDatesValidateCheck.contains("***")?
        // servicesDatesValidateCheck : "");
        // }

        // Check student will be or is 16 by the end date of the IEP and then if the Vocational Info
        // has been filled in
        String vocationalCheck =
                "*** There is no Start and/or End Date on the IEP so the need for Vocational/Transitional Data is unclear";
        if (iep.getStartDate() != null) {
            if (getYear16(iep)) {
                vocationalCheck = checkVocational(iep);
            } else {
                vocationalCheck = "";
            }
        }
        String vocational = (vocationalCheck.contains("***") ? vocationalCheck : "");

        // Has the Eligible button been selected
        String eligibleButton = (iep.getFieldB004() != null ? "" : "*** Eligible radio button not selected.");

        // Are all 4 Assessment fields filled in
        String assessmentAcc = ((iep.getFieldC018() == null || iep.getFieldC020() == null || iep.getFieldC022() == null
                || iep.getFieldC043() == null)
                        ? "*** All System-wide Assessments Fields should be completed even if Student not in this grade band or not applicable"
                        : "");

        // if the Alternate Assessment is selected the verify the Justification statement is filled
        // in
        String aaJustification =
                (iep.getFieldC020() != null && iep.getFieldC020().contains("Alternate") && iep.getFieldD032().isEmpty())
                        ? "*** Student is projected to take the Alternate Assessment under the Summative - the AA justification statement field is blank. "
                        : "";

        // Level of Service field has a value
        String levelOfService = ("K,01,02,03,04,05,06,07,08,09,10,11,12".contains(iep.getStudent().getGradeLevel())
                && iep.getFieldB068() == null
                        ? "*** Level of Service (K12) not selected under the Confirm IEP phase. One must be selected."
                        : "");

        // Setting Field has a value based on Grade Level
        String setting = (("K,01,02,03,04,05,06,07,08,09,10,11,12".contains(iep.getStudent().getGradeLevel())
                && iep.getFieldB075() != null)
                || ("PK3,PK,SS,CF".contains(iep.getStudent().getGradeLevel()) && iep.getFieldB069() != null) ? ""
                        : "*** Level of Setting (K12 or PK) not selected under the Confirm IEP phase. One must be selected.");

        // IEP has a disability associated
        String disabilityPresent = checkAnyExistingDisability(iep) ? ""
                : "*** Disability is missing. Please run Reset Disability Procedure under Options Tab or contact your local Aspen SPED Helpdesk";

        // IEP has one disability assigned primary
        String disabilityPrimaryPresent = checkAnyExistingPrimaryDisability(iep) ? ""
                : "*** No Primary Disability selected. Please identify the Primary Disability on the eligibility determination phase";

        // Student is in Grade 12 Next year and no Final Transition Plan
        String finalTransitionPlan = "";
        if (iep.getEndDate() != null) {
            finalTransitionPlan = getFinalTrans(iep)
                    ? "*** Student is in grade 12 or will be in grade 12 next SY during this IEP. Please generate Final Year Individual Transition Plan."
                    : "";
        }

        // ESY Question is blank
        String esyQuestion = iep.getFieldB007() != null ? "" : "*** No ESY choice was selected.";

        // Age of Majority Form is present
        String majorityPresent = (getYear17(iep) ? ""
                : "*** Student will reach 17 before the end of the IEP and the Age of Majority Form has not been completed");

        // Next review Date is 1 year after IEP Development Meeting date
        PlainDate nextReview;
        Calendar c = Calendar.getInstance();
        c.setTime(getIepMeetingDate(iep)); // 1 Year from IEP Development Meeting Date
        c.add(Calendar.YEAR, 1);
        nextReview = new PlainDate(c.getTime());
        String reviewDateCheck = "";
        if ("Annual Review, Initial Referral".contains(iep.getFieldB002())) {
            reviewDateCheck = nextReview.compareTo(iep.getNextReviewDate()) == 0 ? ""
                    : "*** Next IEP Review Date is not one year from Meeting Date";
        }
        // Physical Education Question is blank
        String peQuestion = iep.getFieldA083() != null ? "" : "*** Physical Education field is blank.";

        // Physical Education Question is No but No Type
        String peTypeQuestion =
                iep.getFieldA083() != null && iep.getFieldA083().contains("No") && iep.getFieldC009() == null
                        ? "*** Physical Education field has NO selected but PE Type is blank"
                        : "";

        // Transportation Question is blank
        String transQuestion = iep.getFieldA084() != null ? "" : "*** Transportation field is blank.";

        // Transportation Question is Yes but No Modifications
        String transModQuestion =
                iep.getFieldA084() != null && iep.getFieldA084().contains("Yes") && iep.getFieldD031().length() < 1
                        ? "*** Transportation has YES selected but no modification is listed. If no modification is required please select No Modification Needed."
                        : "";

        // Check Services all have linked goals
        String servicesGoal =
                getLinkedGoalstoServices(iep) ? "" : " *** Goal(s) are not linked to services under Services Tab.";

        // Check all prepopulated goals were updated
        String emptyGoal = getEmptyGoals(iep)
                ? "*** At least one Goal that was prepopulated by the PLAAFP process was not updated."
                : "";

        // Service Duration are greater than zero
        String servicesDurationValidateCheck = validateServiceDuration(iep);
        String servicesDurationValidate =
                (servicesDurationValidateCheck.contains("***") ? servicesDurationValidateCheck : "");

        // order(alpha) the strings for entry then add a line break
        List<String> validateStrings = Arrays.asList(emptyGoal, servicesDurationValidate, majorityPresent, esyQuestion,
                startDate, endDate, servicesDatesValidate, vocational, eligibleButton, assessmentAcc, aaJustification,
                levelOfService, setting, disabilityPresent, disabilityPrimaryPresent, finalTransitionPlan,
                reviewDateCheck, peQuestion, peTypeQuestion, transQuestion, transModQuestion, servicesGoal);
        String validate = "Nice job.  All validations are in place. CONGRATULATIONS you may proceed with the workflow.";
        if (checkErrors(validateStrings)) {
            Collections.sort(validateStrings, String.CASE_INSENSITIVE_ORDER);
            validate = validateStrings.toString();
            validate = validate.replaceAll(",", "\n"); // add line return after each string
            validate = validate.replace("[", ""); // remove the right bracket
            validate = validate.replace("]", ""); // remove the left bracket
            validate = validate.trim();
        }
        if (iep.getStudent().getGradeLevel().contains("CF")) {
            validate = validate + "\n"
                    + " Once IEP is active, please notify the registrar for appropriate grade placement.";
        }
        iep.setFieldD035(validate);
        AppGlobals.getLog().severe("***X2 SPED: Renewal procedure  " + validate);
        getBroker().saveBeanForced(iep);

    }

    /**
     * Validates IEP Service Duration is greater than zero
     *
     * Returns the appropriate string
     */
    public String validateServiceDuration(IepData iep) {
        String result = "All Service Duration(s) are greater than 0";

        for (IepService service : iep.getIepServices()) {
            if (!(service.getServiceMode().contains("Secret Agent")) && service.getDuration() < 1) {
                result = "*** Service Duration(s) is blank  zero.  Please update.";
            }
        }
        return result;

    }

    /**
     * returns a boolean value based on if all prepopulated goals have not been completed
     *
     */
    public boolean getEmptyGoals(IepData iep) {
        boolean linked = false;
        for (IepGoal goal : iep.getIepGoals()) {
            if (StringUtils.isEmpty(goal.getGoal())) {
                linked = true;
            }

        }
        return linked;
    }

    /**
     * Validates IEP Start Date is on or after IEP Meeting Date or Todays Date
     *
     * Returns the appropriate string
     */
    public String validateStartDate(IepData iep) {
        String result = "The IEP Start Date is on or after the IEP Meeting Date";
        if (iep.getStartDate() != null) {
            if (iep.getStartDate().compareTo(getIepMeetingDate(iep)) < 0) {
                result = "*** The IEP Start Date is NOT at least on or after the IEP Meeting Date";
            }
        } else {
            result = "*** The IEP Start Date is blank";
        }
        return result;

    }

    /**
     * Validates IEP Voctaional/Transitional data is filled out currently using the 2 fields -
     * Education and Employment
     *
     * Returns the appropriate string
     */
    public String checkVocational(IepData iep) {
        String result = "The IEP Vocational Section is completed";
        if (!(iep.getFieldC004() != null || iep.getFieldC002() != null)) {
            result = "*** The student will be 16 prior to the end of the IEP and must have the Vocational/Transition Tab filled out along with measurable Transition goal(s).";
        }
        return result;

    }

    /**
     * Validates IEP Service Start Dates are equal to or greater than the IEP Start Date
     *
     * Returns the appropriate string
     */
    public String validateServiceDates(IepData iep) {
        String result = "All Service Start Date(s) are equal to or after the IEP Start Date";

        if (iep.getStartDate() != null) {
            for (IepService service : iep.getIepServices()) {
                if (!(service.getServiceMode().contains("Secret Agent"))
                        && service.getStartDate().compareTo(iep.getStartDate()) < 0) {
                    if (!(result.contains("***"))) {
                        result = "*** Service Start Date(s) are not equal to or after the IEP Start Date";
                    }
                }
            }
        }
        return result;

    }

    /**
     * Updates all existing Goals with the PLAAFP for Performance Level
     * Creates a shell Goal record if none exists for the Area of Need
     *
     */
    public void executeSetPlaafp(WorkflowProgress progress) {
        IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
        if (iep.getStatusCode() == 0 || iep.getStatusCode() == 5) // if IEP is in draft mode
        {
            resetPlaafp(iep, true);
        }

    }


    /**
     * Updates all existing Goals with the PLAAFP for Performance Level
     * Creates a shell Goal record if none exists for the Area of Need
     * blank indicates whether or not to create a shell Goal record
     */

    public void resetPlaafp(IepData iep, Boolean blank) {
        Boolean exist = false;
        X2Broker broker = getBroker();
        for (IepPerformanceLevel level : iep.getIepPerformanceLevel()) {
            exist = false;
            for (IepGoal goal : iep.getIepGoals()) {
                switch (goal.getFocus()) { // case = goal.focus and then compares to Level
                    case "Aug./Adapt./Assit. Tech.":
                        if (level.getFieldC001() != null
                                && level.getFieldC001().contains("Adaptive-Alternative-Assistive Techniques")) {
                            goal.setFieldD003(level.getFieldD002());
                            exist = true;
                        }
                        break;

                    case "Cognitive Skills":
                    case "Communication Skills":
                    case "Language Arts":
                    case "Functional Life Skills":
                    case "Language Development":
                    case "Mathematics":
                    case "Motor Skills":
                    case "Reading":
                        if (level.getFieldC001() != null && level.getFieldC001().contains(goal.getFocus())) {
                            goal.setFieldD003(level.getFieldD002());
                            exist = true;
                        }
                        break;

                    case "Fine Motor":
                        if (level.getFieldC001() != null && level.getFieldC001().contains("Fine Motor Skills")) {
                            goal.setFieldD003(level.getFieldD002());
                            exist = true;
                        }
                        break;

                    case "Learning Stgy/Stdy Skills":
                        if (level.getFieldC001() != null
                                && level.getFieldC001().contains("Learning Strategies -Study Skills")) {
                            goal.setFieldD003(level.getFieldD002());
                            exist = true;
                        }
                        break;

                    case "Orientation/Mobility":
                        if (level.getFieldC001() != null && level.getFieldC001().contains("Orientation - Mobility")) {
                            goal.setFieldD003(level.getFieldD002());
                            exist = true;
                        }
                        break;

                    case "Pre-academics":
                    case "PreK Cognitive":
                    case "PreK Language":
                    case "PreK Literacy":
                    case "PreK Math":
                    case "PreK Physical":
                    case "PreK Social/Emotional":
                        if (level.getFieldC001() != null && level.getFieldC001().contains("Pre-academics")) {
                            goal.setFieldD003(level.getFieldD002());
                            exist = true;
                        }
                        break;

                    case "Prevoc./Voc. Skills":
                        if (level.getFieldC001() != null
                                && level.getFieldC001().contains("Prevocational - Vocational Skills")) {
                            goal.setFieldD003(level.getFieldD002());
                            exist = true;
                        }
                        break;

                    case "Social/Interpers. Skills":
                        if (level.getFieldC001() != null
                                && level.getFieldC001().contains("Social - Interpersonal Skills")) {
                            goal.setFieldD003(level.getFieldD002());
                            exist = true;
                        }
                        break;

                    case "Transition Svcs/Support":
                        if (level.getFieldC001() != null
                                && level.getFieldC001().contains("Transition Services - Support")) {
                            goal.setFieldD003(level.getFieldD002());
                            exist = true;
                        }
                        break;


                }
                if (goal.isDirty()) {
                    getBroker().saveBeanForced(goal);
                }

            }
            AppGlobals.getLog().severe("After Goals " + level.getFieldC001() + "  " + exist);
            if (level.getFieldC001() == null) {
                exist = true;
            }
            if (!exist && blank) {
                String focus = "";
                switch (level.getFieldC001()) {
                    case "Adaptive-Alternative-Assistive Techniques":
                        focus = "Aug./Adapt./Assit. Tech.";
                        break;

                    case "Cognitive Skills":
                    case "Communication Skills":
                    case "Language Arts":
                    case "Functional Life Skills":
                    case "Language Development":
                    case "Mathematics":
                    case "Motor Skills":
                    case "Reading":
                    case "Pre-academics":
                        focus = level.getFieldC001();
                        break;

                    case "Fine Motor Skills":
                        focus = "Fine Motor";
                        break;

                    case "Learning Strategies -Study Skills":
                        focus = "Learning Stgy/Stdy Skills";
                        break;

                    case "Orientation - Mobility":
                        focus = "Orientation/Mobility";
                        break;

                    case "Prevocational - Vocational Skills":
                        focus = "Prevoc./Voc. Skills";
                        break;

                    case "Social - Interpersonal Skills":
                        focus = "Social/Interpers. Skills";
                        break;

                    case "Transition Services - Support":
                        focus = "Transition Svcs/Support";
                        break;
                }
                IepGoal shell = X2BaseBean.newInstance(IepGoal.class, broker.getPersistenceKey());
                shell.setFieldD003(level.getFieldD002());
                shell.setFieldD001("No Special Factors");
                shell.setFocus(focus);
                shell.setIepDataOid(iep.getOid());
                shell.setStudentOid(iep.getStudent().getOid());
                getBroker().saveBeanForced(shell);

            }

        }
    }



    /**
     * Copy's goals, goal objectives and Services from Active IEP
     * Blanks Out . Start Date/End Date, Duration, and Cycle on Services
     *
     */
    public void setServices(IepData iep) {
        for (IepService service : iep.getIepServices()) {
            service.setDuration(0);
            service.setCycle(null);
            service.setEndDate(null);
            service.setStartDate(null);
            getBroker().saveBeanForced(service);

        }


    }

    /**
     * Identifies the existing performance levels and creates new records as needed.
     *
     */
    public void setPerformanceLevels(IepData iep) {
        String existingAreas = "None";

        for (IepPerformanceLevel level : iep.getIepPerformanceLevel()) {
            existingAreas = existingAreas + level.getFieldC001();
        }
        for (IepGoal goal : iep.getIepGoals()) {
            String area = "None";
            if (goal.getFocus() != null) {
                area = getArea(goal.getFocus());
            }
            if (!(existingAreas.contains(area)) && !("None".contains(area))) {
                addPerformanceLevel(goal, area);
            }
        }
    }

    // returns the String of the Area Affected on the Performance Level record based on the Goal
    // Focus
    public String getArea(String focus)

    {
        String area = "";
        switch (focus) { // case = goal.focus and then compares to Level
            case "Aug./Adapt./Assit. Tech.":
                area = "Adaptive-Alternative-Assistive Techniques";
                break;

            case "Cognitive Skills":
            case "Communication Skills":
            case "Language Arts":
            case "Functional Life Skills":
            case "Language Development":
            case "Mathematics":
            case "Motor Skills":
            case "Reading":
                area = focus;
                break;

            case "Fine Motor":
                area = "Fine Motor Skills";
                break;

            case "Learning Stgy/Stdy Skills":
                area = "Learning Strategies -Study Skills";
                break;

            case "Orientation/Mobility":
                area = "Orientation - Mobility";
                break;

            case "Pre-academics":
            case "PreK Cognitive":
            case "PreK Language":
            case "PreK Literacy":
            case "PreK Math":
            case "PreK Physical":
            case "PreK Social/Emotional":
                area = "Pre-academics";
                break;

            case "Prevoc./Voc. Skills":
                area = "Prevocational - Vocational Skills";
                break;

            case "Social/Interpers. Skills":
                area = "Social - Interpersonal Skills";
                break;

            case "Transition Svcs/Support":
                area = "Transition Services - Support";
                break;


        }
        return area;
    }

    /**
     * Either adds a new performance level record or appends a current one
     *
     */
    public void addPerformanceLevel(IepGoal goal, String area) {
        boolean existing = false;
        for (IepPerformanceLevel level : goal.getIepData().getIepPerformanceLevel()) {
            if (level.getFieldC001() != null) {
                if (level.getFieldC001().contains(area))

                {
                    existing = true;
                    if (level.getFieldD002() != null && goal.getBaseline() != null
                            && !(level.getFieldD002().contains(goal.getBaseline()))) {
                        String plaafp = level.getFieldD002() + " " + goal.getBaseline();
                        level.setFieldD002(plaafp);
                        getBroker().saveBeanForced(level);
                    }

                }
            }
        }
        if (!existing) {
            IepPerformanceLevel newPl =
                    X2BaseBean.newInstance(IepPerformanceLevel.class, getBroker().getPersistenceKey());
            newPl.setFieldD002(goal.getBaseline());
            newPl.setFieldC001(area);
            newPl.setIepDataOid(goal.getIepData().getOid());
            newPl.setStudentOid(goal.getStudent().getOid());
            getBroker().saveBeanForced(newPl);
        }

    }

    /**
     * Updates Disabilities for changes Summer 2019
     *
     */
    public void updateDisabilities(IepData iep) {
        int speechCounter = 0;
        boolean speechPrimary = false;
        for (IepDisability disability : iep.getIepDisability()) {
            if (disability.getDisabilityCode().contains("Emotion")) {
                disability.setDisabilityCode(EMOTIONAL);
            }

            if (disability.getDisabilityCode().contains("SL")) {
                disability.setDisabilityCode(SPEECH_LANGUAGE);
                speechCounter++;
                if (disability.getPrimaryIndicator()) {
                    speechPrimary = true;
                }
            }
            getBroker().saveBeanForced(disability);

        }

        if (speechCounter > 1) {
            speechCounter = 0;
            for (IepDisability disability : iep.getIepDisability()) {
                if (disability.getDisabilityCode().contains("Speech") && speechCounter == 0) {
                    speechCounter++;
                    disability.setPrimaryIndicator(true);
                    getBroker().saveBeanForced(disability);
                } else {
                    if (disability.getDisabilityCode().contains("Speech")) {
                        deleteDisability(disability);
                    }
                    speechCounter++;
                }
            }
        }
    }

    /**
     * Deletes an IEP Disability Record based on a passed in OID
     *
     */
    public void deleteDisability(IepDisability disability) {
        String deleteStatement = " DELETE  " +
                "   FROM STUDENT_DISABILITY " +
                "  WHERE STUDENT_DISABILITY.IDB_OID = '" + disability.getOid() + "' ";


        try {
            Connection connection = getBroker().borrowConnection();
            Statement statement = connection.createStatement();

            try {
                ResultSet resultSet = statement.executeQuery(deleteStatement);
                AppGlobals.getLog().severe("Disability Record Deleted ");

            } catch (Exception e) {
            }

            statement.close();
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            getBroker().returnConnection();
        }
    }



    /**
     * @see com.x2dev.sis.tools.procedures.WorkflowProcedure#initializeForm(com.x2dev.sis.model.beans.FormInstance,
     *      com.x2dev.sis.model.beans.X2BaseBean, com.x2dev.sis.web.UserDataContainer)
     */
    @Override
    public void initializeForm(FormInstance formInstance, X2BaseBean formStorage, UserDataContainer userData)
            throws X2BaseException {
        WorkflowProgress progress = null;
        formInstanceOwnerOid = formInstance.getOwnerObjectOid();

        /*
         * If this is the the Confirm Eligibility Form then copy the Disabilities to the Disability
         * Table
         */
        if (formInstance.getFormDefinition().getId().equals(FORM_ID_ELIG_CONFIRM)) {
            writeDisabilities(formInstance.getOwnerObjectOid());
        }

        if (formInstance.getFormDefinition().getId().equals(FORM_ID_VALID_IEP)) {
            executeValidation(progress);
        }

        if (formInstance.getFormDefinition().getId().equals(FORM_ID_CONFIRM_IEP)) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(
                    formInstance.getFormDefinition().getExtendedDataDictionary(), getBroker().getPersistenceKey());
            confirmIep(formInstance.getOwnerObjectOid(), dictionary);
        }
    }


    /**
     * Holds an amendment meeting using the default sped workflow behavior.
     *
     * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior
     *
     * @throws Exception
     */
    public void executeHoldAmendmentMeeting(WorkflowProgress progress) throws Exception {
        m_behavior.executeHoldAmendmentMeeting(progress, getBroker());
    }



    /**
     * Performs tasks associated with a successful IEP appeal using the default sped workflow
     * behavior.
     *
     * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeIepAmendmentAppeal(WorkflowProgress progress) {
        m_behavior.executeIepAmendmentAppeal(progress, getBroker());
    }

    /**
     * Performs tasks associated with an IEP rejection using the default sped workflow behavior.
     *
     * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeIepAmendmentRejected(WorkflowProgress progress) {
        m_behavior.executeIepAmendmentRejected(progress, getBroker());
    }

    /**
     * Performs tasks associated with a successful IEP approval using the default sped workflow
     * behavior.
     *
     * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeIepApproved(WorkflowProgress progress) {
        m_behavior.executeIepApproved(progress, getOrganization(), getBroker());
    }

    /**
     * Implements the draft amendment IEP using the default sped workflow behavior.
     *
     * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior
     *
     * @throws Exception
     */
    public void executeImplementAmendedIep(WorkflowProgress progress) throws Exception {
        IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(iep.getExtendedDataDictionary(), getBroker().getPersistenceKey());
        setGlSchool(iep);
        resetPlaafp(iep, false);
        executeValidation(progress);
        getBroker().saveBeanForced(iep);
        if (iep.getStatusCode() != 8) {
            addValidationErrors(m_behavior.executeImplementAmendedIep(progress, getOrganization(), getBroker()));
        }
    }

    // method which sets the Grade Level and School Name/Address Field on the IEP to the Student's
    // Current Values

    public void setGlSchool(IepData iep) {
        String schoolPhone = "";
        String schoolNameAddress = "";
        iep.setFieldA085(iep.getStudent().getGradeLevel());
        if (iep.getStudent().getSchool().getAddress() != null) {
            schoolPhone = iep.getStudent().getSchool().getAddress().getPhone01() == null ? ""
                    : " -  " + iep.getStudent().getSchool().getAddress().getPhone01();
            schoolNameAddress = iep.getStudent().getSchool().getName() + "\n"
                    + iep.getStudent().getSchool().getAddress().getAddressLine03() + schoolPhone;
        } else {
            schoolNameAddress = iep.getStudent().getSchool().getName();
        }
        iep.setFieldD033(schoolNameAddress);
        getBroker().saveBeanForced(iep);
    }

    /**
     * Returns the String that matches the local code of the reference table
     *
     */
    public String getDenyString(int dis) {

        String denyString = null;
        String query = new String(
                "SELECT  " +
                        "RCD_DESCRIPTION " +
                        "FROM dbo.REF_CODE as RCD " +
                        "WHERE RCD_RTB_OID = 'RTB000001TN7dO'" + // need to update when deployed to
                                                                 // active
                        " AND RCD_CODE_LOCAL = '" + dis + "'");

        try {
            Connection connection = getBroker().borrowConnection();
            Statement statement = connection.createStatement();


            try {
                ResultSet resultSet = statement.executeQuery(query);


                while (resultSet.next()) {
                    String name = resultSet.getString("RCD_DESCRIPTION");
                    denyString = name;
                }

                resultSet.close();
            } catch (Exception e) {
            }

            statement.close();
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            getBroker().returnConnection();
        }

        return denyString;
    }

    /**
     * Changes a student's status to exited using default sped workflow behavior and sets the
     * Discard Type to Deny
     */
    public void executeDiscardDeny(WorkflowProgress progress) throws Exception {
        int local = 0;
        String discardType = getDenyString(local);
        IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
        executeExit(progress);
        iep.setFieldB084(discardType);
        getBroker().saveBeanForced(iep);
    }

    /**
     * Changes a student's status to exited using default sped workflow behavior and sets the
     * Discard Type to Exit
     */
    public void executeDiscardExit(WorkflowProgress progress) throws Exception {
        int local = 1;
        String discardType = getDenyString(local);
        IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
        executeExit(progress);
        iep.setFieldB084(discardType);
        getBroker().saveBeanForced(iep);
        iep.getStudent().setFieldB019("Exit-Mastered");
        getBroker().saveBeanForced(iep.getStudent());
    }

    /**
     * Changes a student's status to ineligible using default sped workflow behavior and sets the
     * Discard Type to Ineligible
     */
    public void executeDiscardIneligible(WorkflowProgress progress) throws Exception {
        int local = 2;
        String discardType = getDenyString(local);
        executeIneligible(progress);
        IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
        iep.setFieldB084(discardType);
        executeExit(progress);
        getBroker().saveBeanForced(iep);
    }


    /**
     * Changes a student's status to exited using default sped workflow behavior and sets the
     * Discard Type to Refuse
     */
    public void executeDiscardRefuse(WorkflowProgress progress) throws Exception {
        int local = 3;
        String discardType = getDenyString(local);
        IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
        executeExit(progress);
        iep.setFieldB084(discardType);
        getBroker().saveBeanForced(iep);
    }



    /**
     * Changes a student's status to exited using default sped workflow behavior and sets the
     * Discard Type to Revoke
     */
    public void executeDiscardRevoke(WorkflowProgress progress) throws Exception {
        int local = 4;
        String discardType = getDenyString(local);
        IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
        executeExit(progress);
        iep.setFieldB084(discardType);
        getBroker().saveBeanForced(iep);
    }

    /**
     * Changes a student's status to exited using default sped workflow behavior and sets the
     * Discard Type to Reject
     */
    public void executeDiscardReject(WorkflowProgress progress) throws Exception {
        int local = 6;
        String discardType = getDenyString(local);
        m_behavior.executeIepAmendmentRejected(progress, getBroker());
        IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
        iep.setFieldB084(discardType);
        getBroker().saveBeanForced(iep);
    }

    /**
     * Rolls back changes made by the submit for approval method using the default sped workflow
     * behavior.
     *
     * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeSubmitForApproval(WorkflowProgress progress) {
        m_behavior.executeSubmitForApproval(progress, getBroker());
    }

    /**
     * @see com.x2dev.sis.tools.procedures.WorkflowProcedure#initializeOwner(com.x2dev.sis.model.beans.X2BaseBean,
     *      java.util.Map, PlainDate, com.x2dev.sis.model.beans.WorkflowDefinition)
     */
    @Override
    public X2BaseBean initializeOwner(X2BaseBean selectionBean,
                                      Map<String, FormInstance> formInstances,
                                      PlainDate date,
                                      WorkflowDefinition workflowDefinition) {
        LinkedList<ValidationError> errors = new LinkedList<ValidationError>();
        SisStudent student = (SisStudent) selectionBean;
        X2Broker broker = getBroker();
        IepData amendmentIep = null;
        IepData activeIep = student.getActiveIep(broker);
        SisOrganization district = (student.getSchool().getOrganization1());

        if (activeIep == null || student.getAmendmentDraftIep(broker) != null
                || checkIepsForDrafts((SisStudent) selectionBean)) {
            errors.add(new ValidationError(BUSINESS_RULE_VIOLATION, new Integer(IEP_AMENDMENT_ELIGIBILITY)));
        } else {
            SisExtendedDataDictionary iepExtendedDictionary = (SpedUtils.getIepDictionary(district, broker));
            DataDictionary dictionary =
                    DataDictionary.getDistrictDictionary(iepExtendedDictionary, broker.getPersistenceKey());

            Collection<String> relationshipIds = Arrays.asList(new String[] {

                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_ACCOMMODATIONS)
                            .getId(),
                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_AMENDMENTS)
                            .getId(),
                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_DISABILITY)
                            .getId(),
                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_GOALS).getId(),
                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_GOALS).getId()
                            + PATH_DELIMITER +
                            dictionary.findDataDictionaryRelationship(IepGoal.class.getName(),
                                    IepGoal.REL_IEP_GOAL_OBJECTIVES).getId(),
                    // dictionary.findDataDictionaryRelationship(IepData.class.getName(),
                    // IepData.REL_IEP_GOALS).getId() + PATH_DELIMITER +
                    // dictionary.findDataDictionaryRelationship(IepGoal.class.getName(),
                    // IepGoal.REL_IEP_GOAL_OBJECTIVES).getId() + PATH_DELIMITER +
                    // dictionary.findDataDictionaryRelationship(IepGoal.class.getName(),
                    // IepGoal.REL_IEP_SERVICE_GOAL_ALIGNMENTS).getId(),
                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_MEETING).getId(),
                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_SERVICES)
                            .getId(),
                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_OTHER_SERVICES)
                            .getId(),
                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_TEAM_MEMBERS)
                            .getId(),
                    dictionary
                            .findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_PERFORMANCE_LEVEL)
                            .getId(),
                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_MEETING_ATTENDANCE)
                            .getId(),
                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_AMENDMENTS)
                            .getId(),
                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_PLACEMENTS).getId(),
                    dictionary.findDataDictionaryRelationship(IepData.class.getName(), IepData.REL_IEP_AMENDMENTS)
                            .getId() + PATH_DELIMITER +
                            dictionary.findDataDictionaryRelationship(IepAmendment.class.getName(),
                                    IepAmendment.REL_IEP_AMENDMENT_DETAILS).getId()

            });

            BeanCopier beanCopier = new BeanCopier(broker, true);
            amendmentIep =
                    (IepData) beanCopier.copy(activeIep, relationshipIds, getRenewalIepValuesToSet(dictionary, broker));

            IepAmendment amendment = X2BaseBean.newInstance(IepAmendment.class, broker.getPersistenceKey());
            amendment.setDate(date);
            amendment.setIepDataOid(amendmentIep.getOid());
            amendment.setStudentOid(amendmentIep.getStudentOid());
            broker.saveBeanForced(amendment);

            amendmentIep.setAmendedIndicator(true);
            amendmentIep.setStatusCodeEnum(StatusCode.AMENDMENT_DRAFT);
            amendmentIep.setIepAmendmentOid(amendment.getOid());
            amendmentIep.setMeetingTypeCodeEnum(IepMeeting.TypeCode.AMENDMENT);
            amendmentIep.setFieldB002("Modification");
            // added DEC 2018 Update
            amendmentIep.setStartDate(null);
            amendmentIep.setSignedDate(null);
            amendmentIep.setMeetingDate(null);
            // added to set performance level PLAAFS record from Active IEP
            setPerformanceLevels(amendmentIep);
            setGlSchool(amendmentIep);

            String majorityOid = getFormDefinitionOid(FORM_ID_MAJORITY);
            if (findForm(activeIep, majorityOid)) {
                copyForm(activeIep, amendmentIep, majorityOid);
            }

            String transitionOid = getFormDefinitionOid(FORM_ID_FINAL_TRANSITION);
            if (findForm(activeIep, transitionOid)) {
                copyForm(activeIep, amendmentIep, transitionOid);
            }

            broker.saveBeanForced(amendmentIep);


            // Create IEP form instance
            FormDefinition iepFormDefinition = getFormDefinition(m_behavior.getIepFormDefinitionId(), broker);

            FormInstance iepForm = X2BaseBean.newInstance(FormInstance.class, broker.getPersistenceKey());
            iepForm.setFormDefinitionOid(iepFormDefinition.getOid());
            iepForm.setCreatedTime(System.currentTimeMillis());
            iepForm.setOwnerObjectOid(amendmentIep.getOid());
            iepForm.setStorageObjectOid(amendmentIep.getOid());
            broker.saveBeanForced(iepForm);

            // Create IEP Amendment form instance
            FormDefinition amendmentFormDefinition =
                    getFormDefinition(m_behavior.getAmendmentFormDefinitionId(), broker);

            FormInstance amendmentForm = X2BaseBean.newInstance(FormInstance.class, broker.getPersistenceKey());
            amendmentForm.setFormDefinitionOid(amendmentFormDefinition.getOid());
            amendmentForm.setCreatedTime(System.currentTimeMillis());
            amendmentForm.setOwnerObjectOid(amendmentIep.getOid());
            amendmentForm.setStorageObjectOid(amendment.getOid());
            broker.saveBeanForced(amendmentForm);

            // Create IEP Meeting form instances
            FormDefinition meetingFormDefinition = getFormDefinition(MEETING_FORM_ID, broker);

            Collection<IepMeeting> meetings = amendmentIep.getIepMeeting();
            for (IepMeeting meeting : meetings) {
                FormInstance meetingForm = X2BaseBean.newInstance(FormInstance.class, broker.getPersistenceKey());
                meetingForm.setFormDefinitionOid(meetingFormDefinition.getOid());
                meetingForm.setCreatedTime(System.currentTimeMillis());
                meetingForm.setOwnerObjectOid(amendmentIep.getOid());
                meetingForm.setStorageObjectOid(meeting.getOid());
                broker.saveBeanForced(meetingForm);
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
                placementForm.setCreatedTime(System.currentTimeMillis());
                placementForm.setOwnerObjectOid(amendmentIep.getOid());

                // Query for the new copied version of the old placement record
                Criteria placementCriteria = new Criteria();
                IepPlacement oldPlacement = (IepPlacement) form.getStorageObject();
                placementCriteria.addEqualTo(IepPlacement.COL_IEP_PLACEMENT_PROGRAM_OID,
                        oldPlacement.getIepPlacementProgramOid());
                placementCriteria.addEqualTo(IepPlacement.COL_IEP_DATA_OID, amendmentIep.getOid());
                placementCriteria.addEqualTo(IepPlacement.COL_START_DATE, oldPlacement.getStartDate());
                QueryByCriteria newPlacementQuery = new QueryByCriteria(IepPlacement.class, placementCriteria);

                IepPlacement placement = (IepPlacement) broker.getBeanByQuery(newPlacementQuery);
                if (placement != null) {
                    placementForm.setStorageObjectOid(placement.getOid());
                    broker.saveBeanForced(placementForm);
                }
            }
        }

        addValidationErrors(errors);
        updateDisabilities(amendmentIep);
        return amendmentIep;
    }

    // Check a set of forms tied to a single form definition to copy from one IEP to another if the
    // form uses the Generic Form Data

    private void copyForm(IepData owner, IepData newOwner, String formOid) {
        X2Broker broker = getBroker();
        BeanCopier beanCopier = new BeanCopier(broker, true);
        Criteria criteria = new Criteria();
        criteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, owner.getOid());
        criteria.addEqualTo(FormInstance.REL_FORM_DEFINITION + "." + FormDefinition.COL_OID, formOid);
        QueryByCriteria query = new QueryByCriteria(FormInstance.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                Collection<String> relationships = new ArrayList<String>();
                FormInstance form = (FormInstance) iterator.next();
                FormInstance newForm = X2BaseBean.newInstance(FormInstance.class, broker.getPersistenceKey());
                newForm = (FormInstance) beanCopier.copy(form, relationships);
                newForm.setOwnerObjectOid(newOwner.getOid());
                broker.saveBeanForced(newForm);
                Criteria storageCriteria = new Criteria();
                storageCriteria.addEqualTo(GenericFormData.COL_OID, form.getStorageObjectOid());
                QueryByCriteria storageQuery = new QueryByCriteria(GenericFormData.class, storageCriteria);
                QueryIterator storageIterator = getBroker().getIteratorByQuery(storageQuery);
                try {
                    while (storageIterator.hasNext()) {
                        GenericFormData oldData = (GenericFormData) storageIterator.next();
                        GenericFormData newData =
                                X2BaseBean.newInstance(GenericFormData.class, broker.getPersistenceKey());
                        newData = (GenericFormData) beanCopier.copy(oldData, relationships);
                        newForm.setStorageObjectOid(newData.getOid());
                        broker.saveBeanForced(newForm);
                    }
                } finally {
                    storageIterator.close();
                }

            }

        } finally {
            iterator.close();
        }

    }

    /**
     * Returns a map of values to set on a Amnend IEP copy.
     *
     * @param iepFormDictionary
     * @param broker
     *
     * @return HashMap<ModelProperty, Object>
     */
    protected HashMap<ModelProperty, Object> getRenewalIepValuesToSet(DataDictionary iepFormDictionary,
                                                                      X2Broker broker) {
        HashMap<ModelProperty, Object> valuesToSet = new HashMap<ModelProperty, Object>();
        // Suspected Disabilities Clear
        valuesToSet.put(new ModelProperty(IepData.class, IepData.COL_FIELD_C027, broker.getPersistenceKey()), null); // Suspected
                                                                                                                     // Disabilities
                                                                                                                     // 1
        valuesToSet.put(new ModelProperty(IepData.class, IepData.COL_FIELD_C028, broker.getPersistenceKey()), null); // Suspected
                                                                                                                     // Disabilities
                                                                                                                     // 2
        valuesToSet.put(new ModelProperty(IepData.class, IepData.COL_FIELD_C029, broker.getPersistenceKey()), null); // Suspected
                                                                                                                     // Disabilities
                                                                                                                     // 3
        valuesToSet.put(new ModelProperty(IepData.class, IepData.COL_FIELD_C033, broker.getPersistenceKey()), null); // Suspected
                                                                                                                     // Disabilities
                                                                                                                     // 4
        valuesToSet.put(new ModelProperty(IepData.class, IepData.COL_FIELD_C034, broker.getPersistenceKey()), null); // Suspected
                                                                                                                     // Disabilities
                                                                                                                     // 5
        valuesToSet.put(new ModelProperty(IepData.class, IepData.COL_FIELD_C035, broker.getPersistenceKey()), null); // Suspected
                                                                                                                     // Disabilities
                                                                                                                     // 6

        // LRE Minutes and Setting Fields
        valuesToSet.put(new ModelProperty(IepData.class, IepData.COL_FIELD_B068, broker.getPersistenceKey()), null);
        valuesToSet.put(new ModelProperty(IepData.class, IepData.COL_FIELD_B075, broker.getPersistenceKey()), null);
        valuesToSet.put(new ModelProperty(IepData.class, IepData.COL_FIELD_B071, broker.getPersistenceKey()), null);
        valuesToSet.put(new ModelProperty(IepData.class, IepData.COL_FIELD_B069, broker.getPersistenceKey()), null);
        valuesToSet.put(new ModelProperty(IepData.class, IepData.COL_FIELD_B070, broker.getPersistenceKey()), null);

        valuesToSet.put(new ModelProperty(IepData.class, IepData.COL_FIELD_D035, broker.getPersistenceKey()), null); // IEP
                                                                                                                     // Validation

        valuesToSet.put(new ModelProperty(IepData.class, IepData.COL_FIELD_C030, broker.getPersistenceKey()), null); // Eligibility
                                                                                                                     // Disabilities
                                                                                                                     // 1
        valuesToSet.put(new ModelProperty(IepData.class, IepData.COL_FIELD_C031, broker.getPersistenceKey()), null); // Eligibility
                                                                                                                     // Disabilities
                                                                                                                     // 2
        valuesToSet.put(new ModelProperty(IepData.class, IepData.COL_FIELD_C032, broker.getPersistenceKey()), null); // Eligibility
                                                                                                                     // Disabilities
                                                                                                                     // 3
        valuesToSet.put(new ModelProperty(IepData.class, IepData.COL_FIELD_C036, broker.getPersistenceKey()), null); // Eligibility
                                                                                                                     // Disabilities
                                                                                                                     // 4
        valuesToSet.put(new ModelProperty(IepData.class, IepData.COL_FIELD_C037, broker.getPersistenceKey()), null); // Eligibility
                                                                                                                     // Disabilities
                                                                                                                     // 5
        valuesToSet.put(new ModelProperty(IepData.class, IepData.COL_FIELD_C038, broker.getPersistenceKey()), null); // Eligibility
                                                                                                                     // Disabilities
                                                                                                                     // 6


        return valuesToSet;
    }


    /**
     * Check the student to see if they have an existing draft IEP.
     *
     * @param student
     * @return
     */
    private boolean checkIepsForDrafts(SisStudent student) {
        boolean hasDraft = false;

        ArrayList<String> draftStatusCodes = new ArrayList<String>();
        draftStatusCodes.add("0"); // Draft IEP's
        draftStatusCodes.add("5"); // Amendment Draft IEP's

        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepData.COL_STUDENT_OID, student.getOid());
        criteria.addIn(IepData.COL_STATUS_CODE, draftStatusCodes);

        QueryByCriteria query = new QueryByCriteria(IepData.class, criteria);
        Collection<IepData> ieps = getBroker().getCollectionByQuery(query);

        if (!CollectionUtils.isEmpty(ieps)) {
            hasDraft = true;
        }

        return hasDraft;
    }

    /**
     * Returns the form definition with the passed ID.
     *
     * TODO: this is duplicated from SpedDefaultWorkflowBehavior.java to avoid the need for a code
     * change for DoDEA SPED. This should be removed once integration with the codebase has
     * occurred.
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
     * @see com.x2dev.sis.tools.procedures.WorkflowProcedure#ownerExistsBeforeInitiation()
     */
    @Override
    public boolean ownerExistsBeforeInitiation() {
        return false;
    }

    /**
     * Rolls back changes made by the hold amendment meeting method using the default sped workflow
     * behavior.
     *
     * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior
     *
     * @throws Exception
     */
    public void rollbackHoldAmendmentMeeting(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackHoldAmendmentMeeting(progress, getBroker());
    }

    /**
     * Rolls back changes made when an IEP is appealed using the default sped workflow behavior.
     *
     * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackIepAmendmentAppeal(WorkflowProgress progress) {
        m_behavior.rollbackIepAmendmentAppeal(progress, getBroker());
    }

    /**
     * Rolls back changes made when an IEP is rejected using the default sped workflow behavior.
     *
     * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior
     *
     * @throws Exception
     */
    public void rollbackIepAmendmentRejected(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackIepAmendmentRejected(progress, getBroker());
    }

    /**
     * Rolls back changes made when an IEP is approved using the default sped workflow behavior.
     *
     * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior
     *
     * @throws Exception
     */
    public void rollbackIepApproved(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackIepApproved(progress, getOrganization(), getBroker());
    }

    /**
     * Rolls back implementation of a draft amendment IEP using the default sped workflow behavior.
     *
     * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior
     *
     * @throws Exception
     */
    public void rollbackImplementAmendedIep(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackImplementAmendedIep(progress, getOrganization(), getBroker());
        m_behavior.rollbackImplementIep(progress, getOrganization(), getLocale(), getBroker());
    }

    /**
     * Marks student's draft IEP's DISCARDED and exits student from SPED.
     * behavior.
     *
     * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeExit(WorkflowProgress progress) {
        m_behavior.executeExit(progress, getOrganization(), getBroker());
        IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
        iep.getStudent().setSpedStatusCode("Exited");

    }


    /**
     * Changes a student's status to ineligible using default sped workflow behavior.
     *
     * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior
     *
     * @throws Exception
     */
    public void executeIneligible(WorkflowProgress progress) throws Exception {
        m_behavior.executeIneligible(progress, getOrganization(), getBroker());
        IepData iep = (IepData) progress.getWorkflow().getOwner(getBroker());
        int discard = 7;
        iep.setStatusCode(discard);
        iep.getStudent().setSpedStatusCode("Exited");

        String inEligible = "Ineligible";
        iep.setFieldB004(inEligible);

        getBroker().saveBeanForced(iep);

    }

    /**
     * Rolls back changes made by the submit for approval method using the default sped workflow
     * behavior.
     *
     * @param progress
     */
    public void rollbackSubmitForApproval(WorkflowProgress progress) {
        addValidationErrors(m_behavior.rollbackSubmitForApproval(progress, getBroker()));
    }

    /**
     * Implements the draft amendment IEP (with email notification) using the default sped workflow
     * behavior.
     *
     * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior
     *
     * @throws Exception
     */
    public void executeImplementNotifyAmendedIep(WorkflowProgress progress) throws Exception {
        addValidationErrors(m_behavior.executeImplementNotifyAmendedIep(progress, getOrganization(), getBroker()));
    }

    /**
     * Rolls back implementation of a draft amendment IEP using the default sped workflow behavior.
     * During rollback, no notification is sent to the user.
     *
     * @see com.x2dev.sis.model.business.sped.SpedDefaultWorkflowBehavior
     *
     * @throws Exception
     */
    public void rollbackImplementNotifyAmendedIep(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackImplementAmendedIep(progress, getOrganization(), getBroker());
    }

    /**
     * Look up a Form Definition OID by ID.
     *
     * @param formId
     *
     * @return String
     */
    private String getFormDefinitionOid(String formId) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(FormDefinition.COL_ID, formId);
        QueryByCriteria query = new QueryByCriteria(FormDefinition.class, criteria);
        FormDefinition form = getBroker().getBeanByQuery(query);
        if (form != null) {
            return form.getOid();
        }
        return null;
    }

    // ***************************************************************************************
    // Template procedure methods.
    // ***************************************************************************************

    private static final String KEY_LOAD_PLAAFP = "load-plaafp";
    private static final String KEY_GOAL_FOCUS = "set-goal-focus";
    private static final String KEY_CASE_STAFF = "case-staff";
    private static final String FIELD_PLAAFP = "iglFieldD003";
    private static final String FIELD_PL_DESCR = "iplFieldD002";
    private static final String FIELD_STAFF_NAME = "stfNameView";
    private static final String FORM_ID_REFERRAL = "SPED-REF";
    private static final String ALIAS_STAFF_OID = "staff-oid";

    /**
     * After save template.
     *
     * @param detail GenericDetail
     * @param userData UserDataContainer
     * @param broker ModelBroker
     * @return List
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#afterSaveTemplate(com.follett.fsc.core.k12.web.GenericDetail,
     *      com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.business.ModelBroker)
     */
    @Override
    public List<ValidationError> afterSaveTemplate(GenericDetail detail,
                                                   UserDataContainer userData,
                                                   ModelBroker broker) {
        // No action.
        return null;
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
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#initializeTemplate(com.follett.fsc.core.k12.web.template.Template,
     *      com.follett.fsc.core.k12.web.ApplicationContext,
     *      com.follett.fsc.core.k12.business.dictionary.DataDictionary,
     *      com.follett.fsc.core.k12.business.PrivilegeSet, java.util.Locale)
     */
    @Override
    public void initializeTemplate(Template template,
                                   ApplicationContext applicationContext,
                                   DataDictionary dictionary,
                                   PrivilegeSet privilegeSet,
                                   Locale locale)
            throws X2BaseException {
        // No action.
    }

    /**
     * Modify form.
     *
     * Goal template, on load, populate the IepPerformanceLevels embedded list.
     * New ADD details will be empty and these need the performace level to be loaded.
     * Existing details can be ignored, they will already have the performace levels loaded.
     *
     * Goal Focus, when the focus is set, look up a performance level that matches the focus,
     * copy the performance level plaafp description to the goal plaafp description.
     *
     * @param detail GenericDetail
     * @param key String
     * @param value String
     * @param userData UserDataContainer
     * @param template Template
     * @param errorsList List
     * @return Map
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#modifyForm(com.follett.fsc.core.k12.web.GenericDetail,
     *      java.lang.String, java.lang.String, com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.web.template.Template, java.util.List)
     */
    @Override
    public Map<String, Object> modifyForm(GenericDetail detail,
                                          String key,
                                          String value,
                                          UserDataContainer userData,
                                          Template template,
                                          List errorsList)
            throws X2BaseException {
        Map<String, Object> returnMap = new HashMap<String, Object>();

        if (KEY_LOAD_PLAAFP.equals(key) && value != null) {
            boolean refresh = loadPerfLevels(value, detail, userData);
            if (refresh) {
                returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, Integer.valueOf(UserEvent.EVENT_REFRESH));
            }
        } else if (KEY_GOAL_FOCUS.equals(key) && value != null) {
            Map<String, Object> prop = setGoalPlaaf(value, detail, userData);
            if (prop != null) {
                returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, prop);
            }
        } else if (KEY_CASE_STAFF.equals(key)) {
            Map<String, Object> prop = lookupCaseStaff(detail, userData);
            if (prop != null) {
                returnMap.put(PROPERTY_DYNAMIC_FORM_PROPERTIES, prop);
                returnMap.put(PROPERTY_DYNAMIC_FORM_REFRESH, Integer.valueOf(UserEvent.EVENT_REFRESH));
            }
        }
        return returnMap;
    }

    /**
     * Validate template.
     *
     * @param form GenericDetailForm
     * @param detail GenericDetail
     * @param userData UserDataContainer
     * @param broker ModelBroker
     * @return List
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#validateTemplate(com.follett.fsc.core.k12.web.GenericDetailForm,
     *      com.follett.fsc.core.k12.web.GenericDetail,
     *      com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.business.ModelBroker)
     */
    @Override
    public List<ValidationError> validateTemplate(GenericDetailForm form,
                                                  GenericDetail detail,
                                                  UserDataContainer userData,
                                                  ModelBroker broker) {
        List<ValidationError> errors = new ArrayList<ValidationError>();
        return errors;
    }

    /**
     * @param value
     * @param detail
     * @param userData
     */
    private boolean loadPerfLevels(String value, GenericDetail detail, UserDataContainer userData) {
        boolean refresh = false;
        if (value != null && detail.getOid() != null) {
            String ids[] = value.split(":");
            if (ids.length == 2) {
                ChildDetailSet goalSet = detail.getChildDetailSet(ids[0]);
                GenericDetail goalDetail = goalSet.getChildDetail(ids[1]);
                ChildDetailSet plaafpSet = goalDetail.getChildDetailSet("goalplaafp");
                if (plaafpSet != null && plaafpSet.getChildDetails().size() == 0) {
                    // Plaafp set exists and is empty.
                    ModelBroker broker = new ModelBroker(userData);
                    X2Criteria criteria = new X2Criteria();
                    criteria.addEqualTo(IepPerformanceLevel.COL_IEP_DATA_OID, detail.getOid());
                    BeanQuery query = new BeanQuery(IepPerformanceLevel.class, criteria);
                    Collection<IepPerformanceLevel> levels = broker.getCollectionByQuery(query);
                    if (levels.size() > 0) {
                        for (IepPerformanceLevel level : levels) {
                            try {
                                String newChildId = plaafpSet.addNewChild(level, userData, broker);
                                plaafpSet.markForSave(newChildId);
                                refresh = true;
                            } catch (X2BaseException e) {
                                AppGlobals.getLog().log(Level.WARNING, "IEP Add Goal PLAAFP load", e);
                            }
                        }
                    }
                }
            }
        }
        return refresh;
    }

    /**
     * Lookup any previously existing case staff OID from the IEP or the Referral form.
     *
     * @param detail
     * @param userData
     *
     * @return prop
     */
    private Map<String, Object> lookupCaseStaff(GenericDetail detail, UserDataContainer userData) {
        ModelBroker broker = new ModelBroker(userData);
        Map<String, Object> prop = null;

        DataDictionaryField field = detail.getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_STAFF_OID);
        String staffOidField = field.getDataFieldConfig().getDataFieldOid().trim();
        String existingStaffOid = (String) detail.getValue(staffOidField);
        if (StringUtils.isEmpty(existingStaffOid)) {
            IepData iep = null;
            if (detail instanceof FormDetail) {
                FormDetail fDetail = (FormDetail) detail;
                iep = (IepData) fDetail.getFormInstance().getOwner(broker);
            } else if (detail instanceof OutcomeDetail) {
                OutcomeDetail oDetail = (OutcomeDetail) detail;
                iep = (IepData) oDetail.getCurrentFormDetail().getFormInstance().getOwner(broker);
            }
            if (iep != null) {
                String staffOid = iep.getStaffOid();
                if (StringUtils.isEmpty(staffOid)) {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, iep.getOid());
                    criteria.addEqualTo(FormInstance.REL_FORM_DEFINITION + "." +
                            FormDefinition.COL_ID, FORM_ID_REFERRAL);
                    BeanQuery query = new BeanQuery(FormInstance.class, criteria);
                    FormInstance instance = broker.getBeanByQuery(query);
                    if (instance != null) {
                        X2BaseBean storage = instance.getStorageObject(broker);
                        ExtendedDataDictionary ddx = instance.getFormDefinition().getExtendedDataDictionary();
                        DataDictionary formDictionary =
                                DataDictionary.getDistrictDictionary(ddx, userData.getPersistenceKey());
                        if (storage != null) {
                            staffOid = (String) storage.getFieldValueByAlias(ALIAS_STAFF_OID, formDictionary);
                        }
                    }
                }

                if (!StringUtils.isEmpty(staffOid)) {
                    SisStaff staff = broker.getBeanByOid(SisStaff.class, staffOid);
                    if (staff != null) {
                        detail.setValue(staffOidField, staffOid);
                        detail.setValue(FIELD_STAFF_NAME, staff.getNameView());
                        prop = new HashMap<String, Object>();
                        prop.put(staffOidField, staffOid);
                    }
                }
            }
        }

        return prop;
    }

    /**
     * Use the newly entered goal focus to find a matching IepPerformanceLevel.
     * When found, copy the IepPerformanceLevel description to the Goal PLAAFP description.
     *
     * @param ids
     * @param detail
     * @return Map
     */
    private Map<String, Object> setGoalPlaaf(String focus, GenericDetail detail, UserDataContainer userData) {
        ModelBroker broker = new ModelBroker(userData);
        Map<String, Object> prop = new HashMap<String, Object>();
        String iepOid = detail.getOid();
        Collection<IepPerformanceLevel> levels = null;
        if (!StringUtils.isEmpty(iepOid)) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(IepPerformanceLevel.COL_IEP_DATA_OID, iepOid);
            BeanQuery query = new BeanQuery(IepPerformanceLevel.class, criteria);
            levels = broker.getCollectionByQuery(query);
        }

        if (levels != null) {
            for (IepPerformanceLevel level : levels) {
                switch (focus) { // case = goal.focus and then compares to Level
                    case "Aug./Adapt./Assit. Tech.":
                        if (level.getFieldC001() != null
                                && level.getFieldC001().contains("Adaptive-Alternative-Assistive Techniques")) {
                            String value = level.getFieldD002();
                            String presentationText = getPresentationText(value, detail, userData);
                            prop.put(FIELD_PLAAFP, presentationText);
                        }
                        break;

                    case "Cognitive Skills":
                    case "Communication Skills":
                    case "Language Arts":
                    case "Functional Life Skills":
                    case "Language Development":
                    case "Mathematics":
                    case "Motor Skills":
                    case "Reading":
                        if (level.getFieldC001() != null && level.getFieldC001().contains(focus)) {
                            String value = level.getFieldD002();
                            String presentationText = getPresentationText(value, detail, userData);
                            prop.put(FIELD_PLAAFP, presentationText);
                        }
                        break;

                    case "Fine Motor":
                        if (level.getFieldC001() != null && level.getFieldC001().contains("Fine Motor Skills")) {
                            String value = level.getFieldD002();
                            String presentationText = getPresentationText(value, detail, userData);
                            prop.put(FIELD_PLAAFP, presentationText);
                        }
                        break;

                    case "Learning Stgy/Stdy Skills":
                        if (level.getFieldC001() != null
                                && level.getFieldC001().contains("Learning Strategies -Study Skills")) {
                            String value = level.getFieldD002();
                            String presentationText = getPresentationText(value, detail, userData);
                            prop.put(FIELD_PLAAFP, presentationText);
                        }
                        break;

                    case "Orientation/Mobility":
                        if (level.getFieldC001() != null && level.getFieldC001().contains("Orientation - Mobility")) {
                            String value = level.getFieldD002();
                            String presentationText = getPresentationText(value, detail, userData);
                            prop.put(FIELD_PLAAFP, presentationText);
                        }
                        break;

                    case "Pre-academics":
                    case "PreK Cognitive":
                    case "PreK Language":
                    case "PreK Literacy":
                    case "PreK Math":
                    case "PreK Physical":
                    case "PreK Social/Emotional":
                        if (level.getFieldC001() != null && level.getFieldC001().contains("Pre-academics")) {
                            String value = level.getFieldD002();
                            String presentationText = getPresentationText(value, detail, userData);
                            prop.put(FIELD_PLAAFP, presentationText);
                        }
                        break;

                    case "Prevoc./Voc. Skills":
                        if (level.getFieldC001() != null
                                && level.getFieldC001().contains("Prevocational - Vocational Skills")) {
                            String value = level.getFieldD002();
                            String presentationText = getPresentationText(value, detail, userData);
                            prop.put(FIELD_PLAAFP, presentationText);
                        }
                        break;

                    case "Social/Interpers. Skills":
                        if (level.getFieldC001() != null
                                && level.getFieldC001().contains("Social - Interpersonal Skills")) {
                            String value = level.getFieldD002();
                            String presentationText = getPresentationText(value, detail, userData);
                            prop.put(FIELD_PLAAFP, presentationText);
                        }
                        break;

                    case "Transition Svcs/Support":
                        if (level.getFieldC001() != null
                                && level.getFieldC001().contains("Transition Services - Support")) {
                            String value = level.getFieldD002();
                            String presentationText = getPresentationText(value, detail, userData);
                            prop.put(FIELD_PLAAFP, presentationText);
                        }
                        break;
                }
            }
        }
        if (prop.size() == 0) {
            String value = "";
            String presentationText = getPresentationText(value, detail, userData);
            prop.put(FIELD_PLAAFP, presentationText);
        }
        return prop;
    }

    /**
     * The value of level.fieldD002 needs to be returned to the UI for display,
     * but needs to be the complete display string, not just the field value.
     * We need the field formatter to generate the complete display value.
     *
     * Then return two values separated by 'x2&'.
     * The first part is the display value (with <textarea> html),
     * The second part is the raw field value for the hidden input.
     *
     * @param level
     * @param detail
     * @param userData
     * @return String
     */
    private String getPresentationText(String value, GenericDetail detail, UserDataContainer userData) {
        DataDictionaryField field = detail.getDataDictionary().findDataDictionaryField(FIELD_PL_DESCR);
        ModelProperty property = new ModelProperty(field, detail.getDataDictionary());
        FieldFormatter formatter = FieldFormatterFactory.createFieldFormatter(property, userData.getPrivilegeSet());
        if (formatter instanceof BlobFieldFormatter) {
            ((BlobFieldFormatter) formatter).setRows("3");
        }
        String formatValue = formatter.formatDisplay(value, userData.getLocale());
        return formatValue + "x2&" + value;
    }
}
