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
package com.x2dev.reports.sys.sped.nj;

import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepOtherService;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class ConsentToInviteAgency.
 */
public class ConsentToInviteAgency extends BaseFormReportJavaSource {
    /**
     * Instances
     */
    private ReportDataGrid m_consentForm = null;
    private IepData m_currentIep = null;
    private Date m_responseDate = new Date();
    private String m_studentGender = null;

    /**
     * Aliases
     */
    private static final String ALIAS_INTERAGENCY_NAME = "iep-agency";
    private static final String ALIAS_INTERAGENCY_DESCRIPTION = "iep-agency-description";
    private static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    private static final String ALIAS_SCHOOL_FAX = "DOE SCHOOL FAX";

    /**
     * Fields
     */
    private static final String FIELD_ADDRESS_LINE1 = "addressLine1";
    private static final String FIELD_ADDRESS_LINE2 = "addressLine2";
    private static final String FIELD_AGENCY_DESCRIPTION = "agencyDescription";
    private static final String FIELD_AGENCY_NAME = "agencyName";
    private static final String FIELD_CONSENTEE_NAME = "consenteeName";

    /**
     * Params
     */
    private static final String PARAM_CHAIR_PERSON = "CHAIR_PERSON";
    private static final String PARAM_CONSENT_TYPE = "CONSENT_TYPE";
    private static final String PARAM_CONSENT_LINE1 = "CONSENT_LINE1";
    private static final String PARAM_CONSENT_LINE2 = "CONSENT_LINE2";
    private static final String PARAM_FORM_DATE = "FORM_DATE";
    private static final String PARAM_NOTICE_DETAILS = "NOTICE_DETAILS";
    private static final String PARAM_SERVICE_NAME_INTERAGENCY = "InterAgency";
    // Report Header Parameters
    private static final String PARAM_SCHOOL_NAME = "SCHOOL_NAME";
    private static final String PARAM_SCHOOL_ADDRESS_LINE1 = "SKL_ADDRESS_LINE1";
    private static final String PARAM_SCHOOL_ADDRESS_LINE2 = "SKL_ADDRESS_LINE2";
    private static final String PARAM_SCHOOL_PHONE_NO = "SKL_PHONE_NO";
    private static final String PARAM_SCHOOL_FAX_NO = "SKL_FAX_NO";
    private static final String PARAM_SKL_ADMIN1 = "SKL_ADMIN1";
    private static final String PARAM_SKL_ADMIN2 = "SKL_ADMIN2";
    private static final String PARAM_SKL_ADMIN3 = "SKL_ADMIN3";

    /**
     * Other Constants
     */
    private static final String FEMALE = "F";
    private static final String FORM_ID_PARENT = "INVITE-AGENCY-PARENT";
    private static final String FORM_ID_STUDENT = "INVITE-AGENCY-STD";
    private static final String EMPTY_STRING = "";
    private static final String FAX = "FAX";
    private static final String GENDER_CODE_REF_TABLE = "rtbGenderCode";
    private static final String OPTION_CONSENT_GIVEN =
            "I give permission to invite a representative from the agency/agencies listed above to my ";
    private static final String OPTION_CONSENT_NOT_GIVEN =
            "I do not give permission to invite a representative from the agency/agencies listed above to my ";
    private static final String MALE = "M";
    private static final String NOTICE_DETAILS_PART1 = "We are beginning to plan for your ";
    private static final String NOTICE_DETAILS_PART2 = /* (son’s/daughter’s/empty) */" next IEP meeting.  " +
            "During the meeting, we will be discussing ";
    private static final String NOTICE_DETAILS_PART3 =
            /* (his/her/your) */" transition from school to adult life. To assist in planning for your ";
    private static final String NOTICE_DETAILS_PART4 =
            /* (son’s/daughter’s/empty) */" future after high school, we would " +
                    "like to invite a representative from an agency or agencies " +
                    "that would be likely to provide or pay for transition " +
                    "services. A brief list or description of the services offered " +
                    "by the agency or agencies is listed below.  Before a " +
                    "representative may be invited your written consent is needed. " +
                    "Please complete the form below and return it to school no later than ";
    private static final String NOTICE_DETAILS_PART5 = /*
                                                        * (date) QUESTION: Is there a typical number
                                                        * of days you ask for this to be returned …
                                                        * calculated date?
                                                        */
            " so that we may invite the necessary person(s) to the meeting. " +
                    "An invitation to the meeting will be sent to you at as soon as " +
                    "we schedule the meeting.";
    private static final String REFCODE_PARENT = "Parent/Guardian";
    private static final String STRING_DAUGHTERS = "daughter's";
    private static final String STRING_EMPTY = "";
    private static final String STRING_HER = "her";
    private static final String STRING_HIS = "his";
    private static final String STRING_HIS_HER = "his/her";
    private static final String STRING_IEP_MEETING = " IEP meeting.";
    private static final String STRING_PARENT = "Parent";
    private static final String STRING_SONS = "son's";
    private static final String STRING_SONS_DAUGHTERS = "son's/daughter's";
    private static final String STRING_STUDENT = "Student";
    private static final String STRING_TABS = "\t\t";
    private static final String STRING_YOUR = "your";

    private final SimpleDateFormat DATE_FORMATTER = new SimpleDateFormat("MM/dd/yyyy");

    /**
     * This method builds the entire report depending on whether it is the parent version or the
     * child version.
     *
     * @return Object
     * @throws Exception exception
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportType reportType = null;
        Person studentPerson = null;
        m_consentForm = new ReportDataGrid();
        FormInstance formInstance = getFormInstance();

        Calendar calendar = Calendar.getInstance();
        Date formDate = new Date();
        if (formInstance != null) {
            formDate = new Date(formInstance.getCreatedTime());
        }
        calendar.setTime(formDate);
        calendar.add(Calendar.DATE, 5);
        m_responseDate = calendar.getTime();
        addParameter(PARAM_FORM_DATE, DATE_FORMATTER.format(formDate));

        SisStudent student = m_currentIep.getStudent();
        if (student != null) {
            studentPerson = student.getPerson();
        }
        getChairPersonDetails();
        loadReportHeader();

        String formId = getFormDefinition().getId();
        if (FORM_ID_PARENT.equals(formId)) {
            reportType = ReportType.PARENT;
            addParameter(PARAM_CONSENT_TYPE, STRING_PARENT);
            if (studentPerson != null) {
                buildParentForm(studentPerson);
            }
        } else if (FORM_ID_STUDENT.equals(formId)) {
            reportType = ReportType.STUDENT;
            addParameter(PARAM_CONSENT_TYPE, STRING_STUDENT);
            if (studentPerson != null) {
                buildStudentForm(studentPerson);
            }
        }
        addParameter(PARAM_NOTICE_DETAILS, bldNoticeBasedOnRptType(reportType));
        addParameter(PARAM_CONSENT_LINE1, bldConsLnBasedOnRptTypeAndCons(reportType, true));
        addParameter(PARAM_CONSENT_LINE2, bldConsLnBasedOnRptTypeAndCons(reportType, false));

        if (m_consentForm.rowCount() == 0) {
            m_consentForm.append();
        }
        m_consentForm.beforeTop();
        return m_consentForm;
    }

    /**
     * This method builds the parent version of the form.
     *
     * @param studentPerson Person
     */
    public void buildParentForm(Person studentPerson) {
        Collection<IepTeamMember> teamMembers = m_currentIep.getTeamMembers();
        if (teamMembers == null || teamMembers.isEmpty()) {
            m_studentGender = lookupGenderCode(studentPerson.getGenderCode());
            initReportFields();
        }

        for (IepTeamMember teamMember : teamMembers) {
            m_studentGender = lookupGenderCode(studentPerson.getGenderCode());
            String title = teamMember.getMemberRoleCode();
            if (title != null && REFCODE_PARENT.equalsIgnoreCase(title)) {
                loadInterAgencyDetails(teamMember.getPerson());
            }
        }
    }

    /**
     * This method builds the student version of the form.
     *
     * @param studentPerson Person
     */
    public void buildStudentForm(Person studentPerson) {
        loadInterAgencyDetails(studentPerson);
    }

    /**
     * This method loads the interagency details of the student.
     *
     * @param studentPerson Person
     */
    private void loadInterAgencyDetails(Person studentPerson) {
        Collection<IepOtherService> otherInterAgencySvcs = m_currentIep.getIepOtherServices(getBroker());
        String agencyName = null;
        String agencyDescription = null;
        if (otherInterAgencySvcs == null || otherInterAgencySvcs.isEmpty()) {
            setAllReportFieldsExceptAgency(studentPerson);
        }
        for (IepOtherService otherInterAgencySvc : otherInterAgencySvcs) {
            String serviceType = otherInterAgencySvc.getServiceType();
            if (PARAM_SERVICE_NAME_INTERAGENCY.equals(serviceType)) {
                agencyName = (String) otherInterAgencySvc.getFieldValueByAlias(ALIAS_INTERAGENCY_NAME, getDictionary());
                agencyDescription = (String) otherInterAgencySvc.getFieldValueByAlias(ALIAS_INTERAGENCY_DESCRIPTION,
                        getDictionary());
                setAllReportFieldsExceptAgency(studentPerson);
                m_consentForm.set(FIELD_AGENCY_NAME, agencyName != null ? agencyName : STRING_TABS);
                m_consentForm.set(FIELD_AGENCY_DESCRIPTION,
                        agencyDescription != null ? agencyDescription : STRING_TABS);
            }
        }
    }

    /**
     * This method sets most of the report fields.
     *
     * @param person void
     */
    public void setAllReportFieldsExceptAgency(Person person) {
        Address parentMailingAddress = person.getMailingAddress();
        m_consentForm.append();
        m_consentForm.set(FIELD_ADDRESS_LINE1, STRING_TABS);
        m_consentForm.set(FIELD_ADDRESS_LINE2, STRING_TABS);
        if (parentMailingAddress != null) {
            m_consentForm.set(FIELD_ADDRESS_LINE1, parentMailingAddress.getAddressLine01());
            m_consentForm.set(FIELD_ADDRESS_LINE2, parentMailingAddress.getAddressLine03());
        }

        m_consentForm.set(FIELD_CONSENTEE_NAME, person.getFirstName() + " " + person.getLastName());
    }

    /**
     * This method sets the chair person's details for the signature line of the SPED form. If the
     * team members does not
     * include a chair person, then the case manager signs the SPED form.
     *
     * @return void
     */
    private void getChairPersonDetails() {
        addParameter(PARAM_CHAIR_PERSON, "");
        SisPerson chairPerson = null;
        Collection<IepTeamMember> teamMbrs = m_currentIep.getTeamMembers();
        for (IepTeamMember teamMbr : teamMbrs) {
            if (teamMbr.getChairpersonIndicator()) {
                chairPerson = teamMbr.getPerson();
                break;
            }
        }

        if (chairPerson == null) {
            SisStaff caseManager = m_currentIep.getStaff();
            if (caseManager != null) {
                chairPerson = caseManager.getPerson();
            }
        }

        if (chairPerson != null) {
            addParameter(PARAM_CHAIR_PERSON, chairPerson.getFirstName() + " " + chairPerson.getLastName());
        }
    }

    /**
     * This method initializes the report fields.
     */
    private void initReportFields() {
        m_consentForm.append();
        m_consentForm.set(FIELD_ADDRESS_LINE1, STRING_TABS);
        m_consentForm.set(FIELD_ADDRESS_LINE2, STRING_TABS);
        m_consentForm.set(FIELD_CONSENTEE_NAME, STRING_TABS);
        m_consentForm.set(FIELD_AGENCY_NAME, STRING_TABS);
        m_consentForm.set(FIELD_AGENCY_DESCRIPTION, STRING_TABS);
    }

    /**
     * This method builds the notice part of the form based on the report type.
     *
     * @param reportType ReportType
     * @return String
     */
    private String bldNoticeBasedOnRptType(ReportType reportType) {
        String hisHers = (reportType == ReportType.PARENT) ? (MALE.equals(m_studentGender) ? STRING_HIS
                : (FEMALE.equals(m_studentGender) ? STRING_HER
                        : STRING_HIS_HER))
                : STRING_YOUR;
        String sonDaughter = (reportType == ReportType.PARENT) ? (MALE.equals(m_studentGender) ? STRING_SONS
                : (FEMALE.equals(m_studentGender) ? STRING_DAUGHTERS
                        : STRING_SONS_DAUGHTERS))
                : STRING_EMPTY;
        return (NOTICE_DETAILS_PART1 + sonDaughter + NOTICE_DETAILS_PART2 + hisHers + NOTICE_DETAILS_PART3 + sonDaughter
                + NOTICE_DETAILS_PART4 + DATE_FORMATTER.format(m_responseDate) + NOTICE_DETAILS_PART5);
    }

    /**
     * This method builds the consent part of the form based on the report type and consent type.
     *
     * @param reportType ReportType
     * @param consentGiven boolean
     * @return String
     */
    private String bldConsLnBasedOnRptTypeAndCons(ReportType reportType, boolean consentGiven) {
        String sonDaughter = (reportType == ReportType.PARENT) ? (MALE.equals(m_studentGender) ? STRING_SONS
                : (FEMALE.equals(m_studentGender) ? STRING_DAUGHTERS
                        : STRING_SONS_DAUGHTERS))
                : STRING_EMPTY;
        String consent = consentGiven ? OPTION_CONSENT_GIVEN : OPTION_CONSENT_NOT_GIVEN;
        consent = consent + sonDaughter + STRING_IEP_MEETING;
        return consent;
    }

    /**
     * This method looks up the gender code from the reference table and returns the gender type.
     *
     * @param genderCode String
     * @return String
     */
    private String lookupGenderCode(String genderCode) {
        String genderStateCode = null;
        Criteria refCodeCriteria = new Criteria();
        refCodeCriteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + "." + X2BaseBean.COL_OID, GENDER_CODE_REF_TABLE);
        QueryByCriteria refCodeQuery = new QueryByCriteria(ReferenceCode.class, refCodeCriteria);
        refCodeQuery.addOrderByAscending(ReferenceCode.COL_SEQUENCE_NUMBER);
        Map<String, ReferenceCode> genderCodeReferenceTbl =
                getBroker().getMapByQuery(refCodeQuery, ReferenceCode.COL_CODE, 8);
        if (genderCodeReferenceTbl != null) {
            ReferenceCode genderRefCode = genderCodeReferenceTbl.get(genderCode);
            if (genderRefCode != null) {
                genderStateCode = genderRefCode.getStateCode();
            }
        }
        return genderStateCode;
    }

    /**
     * This sets the report type.
     * 
     * @author Follett Software Company
     *
     */
    private enum ReportType {
        PARENT, STUDENT;
    }

    /**
     * The following methods are called, in order, during the life-cycle of a ReportJavaSource
     * instance:
     * <ol>
     * <li><code>saveState(UserDataContainer)</code>
     * <li><code>initialize()</code>
     * <li><code>gatherData()</code>
     * <li><code>releaseResources()</code>
     * </ol>
     *
     * This method is provided as a convenient way for subclasses to initialize member variables
     * before the <code>run()</code> method is called. The default implementation does nothing.
     */
    @Override
    protected void initialize() {
        if (getFormInstance() != null || getFormDefinition() != null) {
            super.initialize();
        }
    }

    /**
     * This method is provided as a way for subclasses to save session state information. The
     * default implementation does nothing.
     *
     * @param userData UserDataContainer
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);
        m_currentIep = userData.getCurrentRecord(IepData.class);
    }

    /**
     * Load the title data of the report.
     */

    private void loadReportHeader() {
        SisStudent student = m_currentIep.getStudent();
        String admin1 = "";

        addParameter(PARAM_SCHOOL_ADDRESS_LINE1, EMPTY_STRING);
        addParameter(PARAM_SCHOOL_ADDRESS_LINE2, EMPTY_STRING);
        addParameter(PARAM_SCHOOL_PHONE_NO, EMPTY_STRING);
        addParameter(PARAM_SKL_ADMIN1, EMPTY_STRING);
        addParameter(PARAM_SKL_ADMIN2, EMPTY_STRING);
        addParameter(PARAM_SKL_ADMIN3, EMPTY_STRING);
        SisSchool school = student.getSchool();
        String schoolName = school.getName();
        addParameter(PARAM_SCHOOL_NAME, schoolName);
        String fax = (String) school.getFieldValueByAlias(ALIAS_SCHOOL_FAX);
        addParameter(PARAM_SCHOOL_FAX_NO, !StringUtils.isEmpty(fax) ? FAX + fax : EMPTY_STRING);
        SisAddress schoolAddress = school.getAddress();
        if (schoolAddress != null) {
            if (!StringUtils.isEmpty(schoolAddress.getPhone01()) ||
                    !StringUtils.isEmpty(schoolAddress.getPhone02())) {
                addParameter(PARAM_SCHOOL_PHONE_NO, (StringUtils.isEmpty(schoolAddress.getPhone01())
                        ? schoolAddress.getPhone02() : schoolAddress.getPhone01()));
            }
            addParameter(PARAM_SCHOOL_ADDRESS_LINE1, schoolAddress.getAddressLine01());
            addParameter(PARAM_SCHOOL_ADDRESS_LINE2, schoolAddress.getAddressLine03());
        }

        if (null != school.getAdministrator1()) {
            SisPerson adminPerson1 = school.getAdministrator1().getPerson();
            admin1 = adminPerson1.getFirstName() + " " + adminPerson1.getLastName();
            addParameter(PARAM_SKL_ADMIN1, admin1);
            String superintendent = (String) school.getAdministrator1().getFieldValueByAlias(ALIAS_DEPT_SUPERVISOR);
            if (null != superintendent) {
                String[] admin2 = superintendent.split(",");
                addParameter(PARAM_SKL_ADMIN2, admin2[1] + " " + admin2[0]);
            }
        }
    }
}
