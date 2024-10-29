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
package com.x2dev.reports.sys.sped.nj.hunterdon;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.web.AppGlobals;
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
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class AgencySubData.
 */
public class AgencySubData extends BaseFormReportJavaSource {
    /**
     * This sets the report type.
     * 
     * @author Follett Software Company
     */
    private enum ReportType {
        PARENT, STUDENT;
    }

    /**
     * Aliases
     */
    private static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    private static final String ALIAS_INTERAGENCY_DESCRIPTION = "iep-agency-description";
    private static final String ALIAS_INTERAGENCY_NAME = "iep-agency";
    private static final String ALIAS_SCHOOL_FAX = "'DOE SCHOOL FAX";
    private static final String ALIAS_STAFF_ROLE = "DOE STAFF ROLE";

    /**
     * Other Constants
     */
    private static final String EMPTY_STRING = "";
    private static final String EXTN_WORK_PHONE = "(908) 284-";
    private static final String FAX = "FAX";

    /**
     * Fields
     */
    private static final String FIELD_ADDRESS_LINE1 = "addressLine1";
    private static final String FIELD_ADDRESS_LINE2 = "addressLine2";
    private static final String FIELD_AGENCY_DESCRIPTION = "agencyDescription";
    private static final String FIELD_AGENCY_NAME = "agencyName";
    private static final String FIELD_CONSENTEE_NAME = "consenteeName";

    private static final String NOTICE_DETAILS_PART1 = "We are beginning to plan for";
    private static final String NOTICE_DETAILS_PART2 = /* (son’s/daughter’s/empty) */" next IEP meeting.  " +
            "During the meeting, we will be discussing";
    private static final String NOTICE_DETAILS_PART3 =
            /* (his/her/your) */" transition from school to adult life. To assist in planning for";
    private static final String NOTICE_DETAILS_PART4 =
            /* (son’s/daughter’s/empty) */" future after high school, we would " +
                    "like to invite a representative from an agency or agencies " +
                    "that would be likely to provide or pay for transition " +
                    "services. A brief list or description of the services offered " +
                    "by the agency or agencies is listed below.  Before a " +
                    "representative may be invited, your written consent is needed. " +
                    "Please complete the form below and return it to school no later than ";
    private static final String NOTICE_DETAILS_PART5 = /*
                                                        * (date) QUESTION: Is there a typical number
                                                        * of days you ask for this to be returned
                                                        * … calculated date?
                                                        */
            " so that we may invite the necessary person(s) to the meeting. " +
                    "An invitation to the meeting will be sent to you as soon as " +
                    "we schedule the meeting.";
    /**
     * Params
     */
    private static final String PARAM_CHAIR_PERSON = "CHAIR_PERSON";
    private static final String PARAM_CHAIR_PERSON_PHONE = "CHAIR_PERSON_PHONE";
    private static final String PARAM_CHAIR_PERSON_TITLE = "CHAIR_PERSON_TITLE";
    private static final String PARAM_CONSENT_LINE1 = "CONSENT_LINE1";
    private static final String PARAM_CONSENT_LINE2 = "CONSENT_LINE2";
    private static final String PARAM_CONSENT_TYPE = "CONSENT_TYPE";
    private static final String PARAM_FOOTER_NAMEVIEW = "FOOTER_NAMEVIEW";
    private static final String PARAM_FORM_DATE = "FORM_DATE";
    private static final String PARAM_NOTICE_DETAILS = "NOTICE_DETAILS";
    private static final String PARAM_SCHOOL_ADDRESS_LINE1 = "SKL_ADDRESS_LINE1";
    private static final String PARAM_SCHOOL_ADDRESS_LINE2 = "SKL_ADDRESS_LINE2";
    private static final String PARAM_SCHOOL_FAX_NO = "SKL_FAX_NO";
    private static final String PARAM_SCHOOL_NAME = "SCHOOL_NAME";
    private static final String PARAM_SCHOOL_PHONE_NO = "SKL_PHONE_NO";
    private static final String PARAM_SKL_ADMIN1 = "SKL_ADMIN1";
    private static final String PARAM_SKL_ADMIN2 = "SKL_ADMIN2";
    private static final String PARAM_SKL_ADMIN3 = "SKL_ADMIN3";
    private static final String PARAM_SNAME = "SNAME";
    private static final String PARAM_SSS_STAFF = "sssStaff";
    private static final String PARAM_THE = "THE";

    private static final String REFCODE_PARENT = "Parent/Guardian";
    private static final String STRING_PARENT = "Parent";
    private static final String STRING_TABS = "\t\t";

    /**
     * Instances
     */
    private Map<String, String> m_agencyInfo = new HashMap<String, String>();
    private ReportDataGrid m_consentForm = null;
    private IepData m_currentIep = null;
    private String m_responseDate = null;
    private String m_sssStaffName;

    /**
     * This method builds the parent version of the form.
     *
     * @param studentPerson Person
     */
    public void buildParentForm(Person studentPerson) {
        Collection<IepTeamMember> teamMembers = m_currentIep.getTeamMembers();
        if (teamMembers == null || teamMembers.isEmpty()) {
            // m_studentGender = lookupGenderCode(studentPerson.getGenderCode());
            initReportFields();
        }

        for (IepTeamMember teamMember : teamMembers) {
            // m_studentGender = lookupGenderCode(studentPerson.getGenderCode());
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
     * This method sets most of the report fields.
     *
     * @param person void
     */
    public void setAllReportFieldsExceptAgency(Person person) {
        Address parentMailingAddress = person.getResolvedMailingAddress();
        m_consentForm.append();
        m_consentForm.set(FIELD_ADDRESS_LINE1, STRING_TABS);
        m_consentForm.set(FIELD_ADDRESS_LINE2, STRING_TABS);
        if (parentMailingAddress != null) {
            m_consentForm.set(FIELD_ADDRESS_LINE1, parentMailingAddress.getAddressLine01());
            m_consentForm.set(FIELD_ADDRESS_LINE2, parentMailingAddress.getAddressLine03());
        }

        IepData iep = (IepData) getFormOwner();

        ExtendedDataDictionary extDictionary =
                (ExtendedDataDictionary) getBroker().getBeanByOid(ExtendedDataDictionary.class, "ddxNjIep");
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extDictionary, getBroker().getPersistenceKey());

        String letterRecp = (String) iep.getFieldValueByAliasExtended("ipp-iac-recipient", dictionary);

        SisStudent sisStudent = iep.getStudent();

        String studentsName = sisStudent.getNameView();

        addParameter(PARAM_FOOTER_NAMEVIEW, studentsName);

        if (studentsName != null) {
            String delims = ",";
            String[] tokens = studentsName.split(delims);

            String fName = tokens[1];
            String lName = tokens[0];

            studentsName = fName + " " + lName;
        }



        if (letterRecp.equals("Student")) {
            m_consentForm.set(FIELD_CONSENTEE_NAME, person.getFirstName() + " " + person.getLastName());
            addParameter(PARAM_SNAME, "");
            addParameter(PARAM_THE, "");
        } else {
            m_consentForm.set(FIELD_CONSENTEE_NAME, "Parent/Guardian");
            addParameter(PARAM_SNAME, " of " + studentsName);
            addParameter(PARAM_THE, "To the ");
        }

    }

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

        IepData iep = (IepData) getFormOwner();

        ExtendedDataDictionary extDictionary =
                (ExtendedDataDictionary) getBroker().getBeanByOid(ExtendedDataDictionary.class, "ddxNjIep");
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extDictionary, getBroker().getPersistenceKey());

        iep.getFieldValueByAliasExtended("ipp-iac-recipient", dictionary);

        String formDate = (String) iep.getFieldValueByAlias("iep-ald-sentdate", dictionary);

        String sFormatDate = "";

        if (formDate != null) {
            String delims = "-";
            String[] tokens = formDate.split(delims);

            String pYear = tokens[0];
            String pMonth = tokens[1];
            String pDay = tokens[2];

            sFormatDate = pMonth + "/" + pDay + "/" + pYear;
        }

        formDate = sFormatDate;

        SimpleDateFormat formatter = new SimpleDateFormat("MM/dd/yyyy");
        SimpleDateFormat formatted = new SimpleDateFormat("MMMMM dd, yyyy");

        String dFormDate = formatted.format(formatter.parse(formDate));
        String formReturnDate = (String) iep.getFieldValueByAlias("iep-ald-returndate", dictionary);

        String sFormatReturnDate = "";

        if (formReturnDate != null) {
            String delims = "-";
            String[] tokens = formReturnDate.split(delims);

            String pYear = tokens[0];
            String pMonth = tokens[1];
            String pDay = tokens[2];

            sFormatReturnDate = pMonth + "/" + pDay + "/" + pYear;
        }

        formReturnDate = sFormatReturnDate;

        m_responseDate = formReturnDate;
        addParameter(PARAM_FORM_DATE, dFormDate);

        SisStudent student = m_currentIep.getStudent();
        if (student != null) {
            studentPerson = student.getPerson();
        }
        getChairPersonDetails();
        loadReportHeader();

        reportType = ReportType.PARENT;
        addParameter(PARAM_CONSENT_TYPE, STRING_PARENT);
        if (studentPerson != null) {
            buildParentForm(studentPerson);
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
     * This method builds the consent part of the form based on the report type and consent type.
     *
     * @param reportType ReportType
     * @param consentGiven boolean
     * @return String
     */
    private String bldConsLnBasedOnRptTypeAndCons(ReportType reportType, boolean consentGiven) {
        IepData iep = (IepData) getFormOwner();

        ExtendedDataDictionary extDictionary =
                (ExtendedDataDictionary) getBroker().getBeanByOid(ExtendedDataDictionary.class, "ddxNjIep");
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extDictionary, getBroker().getPersistenceKey());

        String letterRecp = (String) iep.getFieldValueByAliasExtended("ipp-iac-recipient", dictionary);

        SisStudent sisStudent = iep.getStudent();

        String studentsName = sisStudent.getNameView();
        String studentFirstNameFormatted = "";
        String consent = "";

        if (studentsName != null) {
            String delims = ",";
            String[] tokens = studentsName.split(delims);

            String fName = tokens[1];
            String lName = tokens[0];

            studentsName = fName + " " + lName;
            if (fName.endsWith("s")) {
                studentFirstNameFormatted = fName + "'";
            } else {
                studentFirstNameFormatted = fName + "'s";
            }

        }

        if (letterRecp.equals("Parent")) {
            if (consentGiven == true) {
                consent = "I give permission to invite a representative from the agency/agencies listed above to"
                        + studentFirstNameFormatted + " IEP meeting.";
            } else if (consentGiven == false) {
                consent = "I do not give permission to invite a representative from the agency/agencies listed above to"
                        + studentFirstNameFormatted + " IEP meeting.";
            }
        }

        else if (letterRecp.equals("Student")) {
            if (consentGiven == true) {
                consent =
                        "I give permission to invite a representative from the agency/agencies listed above to my IEP meeting.";
            } else if (consentGiven == false) {
                consent =
                        "I do not give permission to invite a representative from the agency/agencies listed above to my IEP meeting.";
            }
        }

        return consent;
    }

    /**
     * This method builds the notice part of the form based on the report type.
     *
     * @param reportType ReportType
     * @return String
     */
    private String bldNoticeBasedOnRptType(ReportType reportType) {
        IepData iep = (IepData) getFormOwner();

        ExtendedDataDictionary extDictionary =
                (ExtendedDataDictionary) getBroker().getBeanByOid(ExtendedDataDictionary.class, "ddxNjIep");
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extDictionary, getBroker().getPersistenceKey());

        String letterRecp = (String) iep.getFieldValueByAliasExtended("ipp-iac-recipient", dictionary);

        SisStudent sisStudent = iep.getStudent();

        String studentsName = sisStudent.getNameView();
        String studentFirstNameFormatted = "";

        if (studentsName != null) {
            String delims = ",";
            String[] tokens = studentsName.split(delims);

            String fName = tokens[1];
            String lName = tokens[0];

            studentsName = fName + " " + lName;
            if (fName.endsWith("s")) {
                studentFirstNameFormatted = fName + "'";
            } else {
                studentFirstNameFormatted = fName + "'s";
            }

        }

        String Statement = "";
        if (letterRecp.equals("Parent")) {
            Statement = NOTICE_DETAILS_PART1 + studentFirstNameFormatted + NOTICE_DETAILS_PART2
                    + studentFirstNameFormatted + NOTICE_DETAILS_PART3 + studentFirstNameFormatted
                    + NOTICE_DETAILS_PART4 + m_responseDate + NOTICE_DETAILS_PART5;
        } else {
            Statement = NOTICE_DETAILS_PART1 + " your" + NOTICE_DETAILS_PART2 + " your" + NOTICE_DETAILS_PART3 + " your"
                    + NOTICE_DETAILS_PART4 + m_responseDate + NOTICE_DETAILS_PART5;
        }

        return (Statement);
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
        addParameter(PARAM_CHAIR_PERSON_TITLE, "Case Manager");
        addParameter(PARAM_CHAIR_PERSON_PHONE, "");
        SisPerson chairPerson = null;
        Collection<IepTeamMember> teamMbrs = m_currentIep.getTeamMembers();
        for (IepTeamMember teamMbr : teamMbrs) {
            if (teamMbr.getChairpersonIndicator()) {
                chairPerson = teamMbr.getPerson();
                break;
            }
        }
        String workPhone = "";
        SisStaff caseManager = m_currentIep.getStaff();
        if (caseManager != null) {
            chairPerson = caseManager.getPerson();
            workPhone = (String) caseManager.getFieldValueByAlias("DOE STAFF WORK PHONE");
        }

        if (chairPerson != null) {
            addParameter(PARAM_CHAIR_PERSON, chairPerson.getFirstName() + " " + chairPerson.getLastName());
            workPhone = EXTN_WORK_PHONE + workPhone;
            addParameter(PARAM_CHAIR_PERSON_PHONE, workPhone);
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
     * Initialize Supervisor name.
     */
    private void initSssName() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_STAFF_ROLE);
        if (field != null) {
            String beanPath = field.getJavaName();
            ReferenceTable refTable = field.getReferenceTable();
            Collection<ReferenceCode> codes = refTable.getReferenceCodes();

            String sssCode = null;

            for (ReferenceCode code : codes) {
                String stateCode = code.getStateCode();
                if (!StringUtils.isEmpty(stateCode) &&
                        stateCode.equals("SSS")) {
                    sssCode = code.getCode();
                }
            }

            if (!StringUtils.isEmpty(sssCode)) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(beanPath, sssCode);
                QueryByCriteria query = new QueryByCriteria(SisStaff.class, criteria);
                SisStaff sssStaff = (SisStaff) getBroker().getBeanByQuery(query);
                m_sssStaffName = sssStaff.getPerson().getFirstName() + " " + sssStaff.getPerson().getLastName();
            }
        }
    }

    /**
     * This method loads the interagency details of the student.
     *
     * @param studentPerson Person
     */
    private void loadInterAgencyDetails(Person studentPerson) {
        Criteria agencyCriteria = new Criteria();
        agencyCriteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE, "rtbNjAgency");

        QueryByCriteria cGetAgencyQuery = new QueryByCriteria(ReferenceCode.class, agencyCriteria);

        QueryIterator attniterator = getBroker().getIteratorByQuery(
                cGetAgencyQuery);

        try {
            while (attniterator.hasNext()) {
                ReferenceCode refCode = (ReferenceCode) attniterator.next();
                m_agencyInfo.put(refCode.getCode(), refCode.getDescription());
            }
        }

        finally {
            attniterator.close();
        }

        Collection<IepOtherService> otherInterAgencySvcs = m_currentIep.getIepOtherServices(getBroker());
        String agencyName = null;
        String agencyDescription = null;
        if (otherInterAgencySvcs == null || otherInterAgencySvcs.isEmpty()) {
            setAllReportFieldsExceptAgency(studentPerson);
        }
        for (IepOtherService otherInterAgencySvc : otherInterAgencySvcs) {
            agencyName =
                    (String) otherInterAgencySvc.getFieldValueByAliasExtended(ALIAS_INTERAGENCY_NAME, getDictionary());
            agencyDescription = (String) otherInterAgencySvc.getFieldValueByAliasExtended(ALIAS_INTERAGENCY_DESCRIPTION,
                    getDictionary());
            setAllReportFieldsExceptAgency(studentPerson);
            m_consentForm.set(FIELD_AGENCY_NAME, agencyName != null ? m_agencyInfo.get(agencyName) : "");
            m_consentForm.set(FIELD_AGENCY_DESCRIPTION, agencyDescription != null ? agencyDescription : "");

            AppGlobals.getLog().severe("agencyName " + agencyName);
            AppGlobals.getLog().severe("mAgencyInfo.get(agencyName) " + m_agencyInfo.get(agencyName));
        }
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
        initSssName();
        addParameter(PARAM_SSS_STAFF, m_sssStaffName);
    }
}
