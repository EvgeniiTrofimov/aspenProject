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
import com.follett.fsc.core.k12.beans.Contact;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
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
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class ConsentToInviteAgencyData.
 */
public class ConsentToInviteAgencyData extends BaseFormReportJavaSource {
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
     * Aliases
     */
    private static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    private static final String ALIAS_IEP_AGENCY_SHOW = "iep-agency-show";
    private static final String ALIAS_IEP_ALD_RETURNDATE = "iep-ald-returndate";
    private static final String ALIAS_IEP_ALD_SENTDATE = "iep-ald-sentdate";
    private static final String ALIAS_INTERAGENCY_NAME = "iep-agency";
    private static final String ALIAS_IPP_IAC_RECIPIENT = "ipp-iac-recipient";
    private static final String ALIAS_STAFF_ROLE = "DOE STAFF ROLE";
    private static final String ALIAS_STAFF_WORK_PHONE = "DOE STAFF WORK PHONE";

    /**
     * Other Constants
     */
    private static final String EMPTY_STRING = "";
    private static final String EXTENDED_DATA_DICITIONARY = "ddxNjIep";
    private static final String EXTN_WORK_PHONE = "(908) 284-";
    /**
     * Fields
     */
    private static final String FIELD_ADDRESS_LINE1 = "addressLine1";
    private static final String FIELD_ADDRESS_LINE2 = "addressLine2";

    private static final String NOTICE_DETAILS_PART1 = "We are beginning to plan for ";
    private static final String NOTICE_DETAILS_PART2 = " next IEP meeting.  " +
            "During the meeting, we will be discussing ";
    private static final String NOTICE_DETAILS_PART3 =
            " transition from school to adult life. To assist in planning for ";
    private static final String NOTICE_DETAILS_PART4 = " future after high school, we would " +
            "like to invite a representative from an agency or agencies " +
            "that would be likely to provide or pay for transition " +
            "services. A brief list or description of the services offered " +
            "by the agency or agencies is listed below.  Before a " +
            "representative may be invited, your written consent is needed. " +
            "Please complete the form below and return it to school no later than ";
    private static final String NOTICE_DETAILS_PART5 =
            " so that we may invite the necessary person(s) to the meeting. " +
                    "An invitation to the meeting will be sent to you as soon as " +
                    "we schedule the meeting.";
    /**
     * Params
     */
    private static final String PARAM_AGENCY_NAME = "agencyName";
    private static final String PARAM_AGENCY_PHONE = "agencyphone";
    private static final String PARAM_AGENCY_WEBSITE = "agencywebsite";
    private static final String PARAM_CHAIR_PERSON = "CHAIR_PERSON";
    private static final String PARAM_CHAIR_PERSON_PHONE = "CHAIR_PERSON_PHONE";
    private static final String PARAM_CHAIR_PERSON_TITLE = "CHAIR_PERSON_TITLE";
    private static final String PARAM_CONSENT_LINE1 = "CONSENT_LINE1";
    private static final String PARAM_CONSENT_LINE2 = "CONSENT_LINE2";
    private static final String PARAM_CONSENT_TYPE = "CONSENT_TYPE";
    private static final String PARAM_CONSENTEE_NAME = "consenteeName";
    private static final String PARAM_FORM_DATE = "FORM_DATE";
    private static final String PARAM_NOTICE_DETAILS = "NOTICE_DETAILS";
    private static final String PARAM_SCHOOL_ADDRESS_LINE1 = "SKL_ADDRESS_LINE1";
    private static final String PARAM_SCHOOL_ADDRESS_LINE2 = "SKL_ADDRESS_LINE2";
    private static final String PARAM_SCHOOL_NAME = "SCHOOL_NAME";
    private static final String PARAM_SCHOOL_PHONE_NO = "SKL_PHONE_NO";
    private static final String PARAM_SERVICE_NAME_INTERAGENCY = "InterAgency";
    private static final String PARAM_SKL_ADMIN1 = "SKL_ADMIN1";
    private static final String PARAM_SKL_ADMIN2 = "SKL_ADMIN2";
    private static final String PARAM_SKL_ADMIN3 = "SKL_ADMIN3";
    private static final String PARAM_SNAME = "SNAME";
    private static final String PARAM_SSS_STAFF = "sssStaff";
    private static final String PARAM_THE = "THE";

    private static final String REF_TABLE_INTERAGENCY_CODES = "RTB0000002h15m";

    private static final String STRING_APOSTROPHE = "'";
    private static final String STRING_CASE_MANAGER = "Case Manager";
    private static final String STRING_COMMA = ",";
    private static final String STRING_CONSENT_SUFFIX = " IEP meeting.";
    private static final String STRING_FORWARDSLASH = "/";
    private static final String STRING_HYPHEN = "-";
    private static final String STRING_NEW_LINE = "\n";
    private static final String STRING_OF = " of ";
    private static final String STRING_PAR_CONSENT_GIVEN =
            "I give permission to invite a representative from the agency/agencies listed above to ";
    private static final String STRING_PAR_CONSENT_NOT_GIVEN =
            "I do not give permission to invite a representative from the agency/agencies listed above to ";
    private static final String STRING_PARENT = "Parent";
    private static final String STRING_PARENT_GUARDIAN = "Parent/Guardian";
    private static final String STRING_PHONE = "Phone";
    private static final String STRING_POSSESSIVE_SUFFIX = "'s";
    private static final String STRING_S = "s";
    private static final String STRING_SPACE = " ";
    private static final String STRING_STD_CONSENT_GIVEN =
            "I give permission to invite a representative from the agency/agencies listed above to my IEP meeting.";
    private static final String STRING_STD_CONSENT_NOT_GIVEN =
            "I do not give permission to invite a representative from the agency/agencies listed above to my IEP meeting.";
    private static final String STRING_STUDENT = "Student";
    private static final String STRING_TO_THE = "To the ";
    private static final String STRING_TRUE = "true";
    private static final String STRING_WEB = "Web";
    private static final String STRING_YOUR = "your";


    private Map<String, String> m_agencyInfo = new HashMap<String, String>();
    private ReportDataGrid m_consentForm = null;
    /**
     * Member variables
     */
    private IepData m_currentIep = null;
    private String m_responseDate = null;
    private String m_sssStaffName;

    /**
     * This method builds the parent version of the form.
     *
     * @param student SisStudent
     */
    public void buildParentForm(SisStudent student) {
        SisPerson studentPerson = student.getPerson();
        addParameter(PARAM_CONSENTEE_NAME, STRING_PARENT_GUARDIAN);
        addParameter(PARAM_SNAME, STRING_OF + getFullName(studentPerson));
        addParameter(PARAM_THE, STRING_TO_THE);

        Address studentMailingAddress = studentPerson.getMailingAddress();
        if (studentMailingAddress == null) {
            studentMailingAddress = studentPerson.getPhysicalAddress();
        }
        String studentMailingAddressView = EMPTY_STRING;
        if (studentMailingAddress.getAddressLine01() != null) {
            studentMailingAddressView = studentMailingAddress.getAddressLine01().trim();
        }

        Collection<StudentContact> studentContacts = student.getContacts();

        Set<String> prevMailingAddresses = new HashSet<String>();
        String contactMailingAddressView = EMPTY_STRING;


        // One single copy to the Student's mailing address
        // For this report the recipients name will not be used.
        addNotificationRecipient(m_consentForm, studentMailingAddress);

        prevMailingAddresses.add(studentMailingAddressView);

        // Additional single copies to any other contacts that don't LiveWith the student
        for (StudentContact studentContact : studentContacts) {
            if (studentContact.getGradeMailingIndicator()) {
                Contact contact = studentContact.getContact();
                SisPerson contactPerson = (SisPerson) contact.getPerson();

                SisAddress contactMailingAddress = contactPerson.getMailingAddress();
                contactMailingAddressView = EMPTY_STRING;

                // Don't print a letter if the contact doesn't have an address.
                if (contactMailingAddress != null) {
                    if (contactMailingAddress.getAddressLine01() != null) {
                        contactMailingAddressView = contactMailingAddress.getAddressLine01().trim();
                    }

                    if (!StringUtils.isEmpty(contactMailingAddressView)) {
                        // Print only one copy of the report for each address of all contacts having
                        // the GradeMailing flag set.
                        // Only send a copy of the report to contact that don't Live With the
                        // student.
                        if (!prevMailingAddresses.contains(contactMailingAddressView)
                                && !(studentContact.getLivesWithIndicator())) {

                            addNotificationRecipient(m_consentForm, contactMailingAddress);

                            prevMailingAddresses.add(contactMailingAddressView);
                        }
                    }
                }
            }
        }
    }

    /**
     * This method builds the student version of the form.
     *
     * @param studentPerson SisPerson
     */
    public void buildStudentForm(SisPerson studentPerson) {
        m_consentForm.append();
        Address studentMailingAddress = studentPerson.getResolvedMailingAddress();
        m_consentForm.set(FIELD_ADDRESS_LINE1, EMPTY_STRING);
        m_consentForm.set(FIELD_ADDRESS_LINE2, EMPTY_STRING);

        if (studentMailingAddress != null) {
            m_consentForm.set(FIELD_ADDRESS_LINE1, studentMailingAddress.getAddressLine01());
            m_consentForm.set(FIELD_ADDRESS_LINE2, studentMailingAddress.getAddressLine03());
        }

        String studentsName = getFullName(studentPerson);

        addParameter(PARAM_SNAME, EMPTY_STRING);
        addParameter(PARAM_THE, EMPTY_STRING);
        addParameter(PARAM_CONSENTEE_NAME, studentsName);

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
        SisPerson studentPerson = null;
        m_consentForm = new ReportDataGrid();

        IepData iep = (IepData) getFormOwner();

        ExtendedDataDictionary extDictionary = (ExtendedDataDictionary) getBroker()
                .getBeanByOid(ExtendedDataDictionary.class, EXTENDED_DATA_DICITIONARY);
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extDictionary, getBroker().getPersistenceKey());

        String letterRecp = (String) iep.getFieldValueByAliasExtended(ALIAS_IPP_IAC_RECIPIENT, dictionary);

        String formDate = (String) iep.getFieldValueByAlias(ALIAS_IEP_ALD_SENTDATE, dictionary);

        String sFormatDate = EMPTY_STRING;

        if (formDate != null) {
            String delims = STRING_HYPHEN;
            String[] tokens = formDate.split(delims);

            String pYear = tokens[0];
            String pMonth = tokens[1];
            String pDay = tokens[2];

            sFormatDate = pMonth + STRING_FORWARDSLASH + pDay + STRING_FORWARDSLASH + pYear;
        }

        formDate = sFormatDate;

        SimpleDateFormat formatter = new SimpleDateFormat("MM/dd/yyyy");
        SimpleDateFormat formatted = new SimpleDateFormat("MMMMM dd, yyyy");

        String dFormDate = formatted.format(formatter.parse(formDate));
        String formReturnDate = (String) iep.getFieldValueByAlias(ALIAS_IEP_ALD_RETURNDATE, dictionary);

        String sFormatReturnDate = EMPTY_STRING;

        if (formReturnDate != null) {
            String delims = STRING_HYPHEN;
            String[] tokens = formReturnDate.split(delims);

            String pYear = tokens[0];
            String pMonth = tokens[1];
            String pDay = tokens[2];

            sFormatReturnDate = pMonth + STRING_FORWARDSLASH + pDay + STRING_FORWARDSLASH + pYear;
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
        loadInterAgencyDetails();

        if (letterRecp != null) {
            if (letterRecp.equals(STRING_PARENT)) {
                reportType = ReportType.PARENT;
                addParameter(PARAM_CONSENT_TYPE, STRING_PARENT);
                if (studentPerson != null) {
                    buildParentForm(student);
                }
            }

            else if (letterRecp.equals(STRING_STUDENT)) {
                reportType = ReportType.STUDENT;
                addParameter(PARAM_CONSENT_TYPE, STRING_STUDENT);
                if (studentPerson != null) {
                    buildStudentForm(studentPerson);
                }
            }
        } else {
            reportType = ReportType.PARENT;
            addParameter(PARAM_CONSENT_TYPE, STRING_PARENT);
            if (studentPerson != null) {
                buildParentForm(student);
            }
        }

        addNoticeParameter(reportType);
        addConsentParameters(reportType);

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
     * This method builds the consent part of the form based on the report type.
     *
     * @param reportType ReportType
     */
    private void addConsentParameters(ReportType reportType) {
        IepData iep = (IepData) getFormOwner();
        SisStudent student = iep.getStudent();
        String firstNameFormatted = getFormattedFirstName(student);

        if (ReportType.PARENT.equals(reportType)) {
            addParameter(PARAM_CONSENT_LINE1, STRING_PAR_CONSENT_GIVEN + firstNameFormatted + STRING_CONSENT_SUFFIX);
            addParameter(PARAM_CONSENT_LINE2,
                    STRING_PAR_CONSENT_NOT_GIVEN + firstNameFormatted + STRING_CONSENT_SUFFIX);
        }

        else if (ReportType.STUDENT.equals(reportType)) {
            addParameter(PARAM_CONSENT_LINE1, STRING_STD_CONSENT_GIVEN);
            addParameter(PARAM_CONSENT_LINE2, STRING_STD_CONSENT_NOT_GIVEN);
        }
    }

    /**
     * This method builds the notice part of the form based on the report type.
     *
     * @param reportType ReportType
     */
    private void addNoticeParameter(ReportType reportType) {

        IepData iep = (IepData) getFormOwner();
        SisStudent student = iep.getStudent();
        String firstNameFormatted = getFormattedFirstName(student);

        if (ReportType.PARENT.equals(reportType)) {
            addParameter(PARAM_NOTICE_DETAILS, NOTICE_DETAILS_PART1 + firstNameFormatted + NOTICE_DETAILS_PART2
                    + firstNameFormatted + NOTICE_DETAILS_PART3 + firstNameFormatted + NOTICE_DETAILS_PART4
                    + m_responseDate + NOTICE_DETAILS_PART5);
        } else if (ReportType.STUDENT.equals(reportType)) {
            addParameter(PARAM_NOTICE_DETAILS, NOTICE_DETAILS_PART1 + STRING_YOUR + NOTICE_DETAILS_PART2
                    + STRING_YOUR + NOTICE_DETAILS_PART3 + STRING_YOUR + NOTICE_DETAILS_PART4
                    + m_responseDate + NOTICE_DETAILS_PART5);
        }
    }

    /**
     * Add meeting invitation recipient data to grid for display on report.
     *
     * @param grid ReportDataGrid
     * @param studentMailingAddress Address
     */
    private void addNotificationRecipient(ReportDataGrid grid, Address studentMailingAddress) {
        grid.append();

        if (studentMailingAddress == null) {
            grid.set(FIELD_ADDRESS_LINE1, EMPTY_STRING);
            grid.set(FIELD_ADDRESS_LINE2, EMPTY_STRING);
        }

        if (studentMailingAddress != null) {
            grid.set(FIELD_ADDRESS_LINE1, studentMailingAddress.getAddressLine01());
            grid.set(FIELD_ADDRESS_LINE2, studentMailingAddress.getAddressLine03());
        }
    }

    /**
     * This method sets the chair person's details for the signature line of the SPED form. If the
     * team members does not
     * include a chair person, then the case manager signs the SPED form.
     *
     * @return void
     */
    private void getChairPersonDetails() {
        addParameter(PARAM_CHAIR_PERSON, EMPTY_STRING);
        addParameter(PARAM_CHAIR_PERSON_TITLE, STRING_CASE_MANAGER);
        addParameter(PARAM_CHAIR_PERSON_PHONE, EMPTY_STRING);
        SisPerson chairPerson = null;
        Collection<IepTeamMember> teamMbrs = m_currentIep.getTeamMembers();
        for (IepTeamMember teamMbr : teamMbrs) {
            if (teamMbr.getChairpersonIndicator()) {
                chairPerson = teamMbr.getPerson();
                break;
            }
        }
        String workPhone = EMPTY_STRING;
        SisStaff caseManager = m_currentIep.getStaff();
        if (caseManager != null) {
            chairPerson = caseManager.getPerson();
            workPhone = (String) caseManager.getFieldValueByAlias(ALIAS_STAFF_WORK_PHONE);
        }

        if (chairPerson != null) {
            addParameter(PARAM_CHAIR_PERSON, getFullName(chairPerson));
            workPhone = EXTN_WORK_PHONE + workPhone;
            addParameter(PARAM_CHAIR_PERSON_PHONE, workPhone);
        }
    }

    /**
     * Gets the formatted first name.
     *
     * @param student SisStudent
     * @return String
     */
    private String getFormattedFirstName(SisStudent student) {
        String studentFirstNameFormatted = EMPTY_STRING;
        if (student != null && student.getPerson() != null) {
            String firstName = student.getPerson().getFirstName();
            if (firstName.endsWith(STRING_S)) {
                studentFirstNameFormatted = firstName + STRING_APOSTROPHE;
            } else {
                studentFirstNameFormatted = firstName + STRING_POSSESSIVE_SUFFIX;
            }
        }
        return studentFirstNameFormatted;
    }

    /**
     * Get full name from a person bean.
     *
     * @param person SisPerson
     * @return fullName
     */
    private String getFullName(SisPerson person) {
        String fullName = EMPTY_STRING;

        fullName = fullName + person.getFirstName();
        if (!StringUtils.isEmpty(fullName)) {
            fullName = fullName + STRING_SPACE;
        }
        fullName = fullName + person.getLastName();

        return fullName;
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
     */
    private void loadInterAgencyDetails() {
        Criteria agencyCriteria = new Criteria();
        agencyCriteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE, REF_TABLE_INTERAGENCY_CODES);

        QueryByCriteria cGetAgencyQuery = new QueryByCriteria(ReferenceCode.class, agencyCriteria);

        Collection<ReferenceCode> agencyCodes = getBroker().getCollectionByQuery(cGetAgencyQuery);
        for (ReferenceCode refCode : agencyCodes) {

            m_agencyInfo.put(refCode.getCode(), refCode.getDescription());
            m_agencyInfo.put(refCode.getCode() + STRING_PHONE, refCode.getLocalizedDescription(1));
            m_agencyInfo.put(refCode.getCode() + STRING_WEB, refCode.getLocalizedDescription(2));
        }

        Collection<IepOtherService> otherInterAgencySvcs = m_currentIep.getIepOtherServices(getBroker());
        String agencyName = null;

        StringBuilder agencyNameString = new StringBuilder();
        StringBuilder agencyPhoneString = new StringBuilder();
        StringBuilder agencyWebString = new StringBuilder();

        for (IepOtherService otherInterAgencySvc : otherInterAgencySvcs) {
            String serviceType = otherInterAgencySvc.getServiceType();
            if (PARAM_SERVICE_NAME_INTERAGENCY.equals(serviceType)) {
                DataDictionary dictionary = getDictionary();
                String showAgencyOnReport = String
                        .valueOf(otherInterAgencySvc.getFieldValueByAliasExtended(ALIAS_IEP_AGENCY_SHOW, dictionary));
                if (showAgencyOnReport.equals(STRING_TRUE)) {
                    agencyName = (String) otherInterAgencySvc.getFieldValueByAliasExtended(ALIAS_INTERAGENCY_NAME,
                            dictionary);

                    agencyNameString.append(agencyName != null ? m_agencyInfo.get(agencyName) : EMPTY_STRING);
                    agencyNameString.append(STRING_NEW_LINE);
                    agencyPhoneString
                            .append(agencyName != null ? m_agencyInfo.get(agencyName + STRING_PHONE) : EMPTY_STRING);
                    agencyPhoneString.append(STRING_NEW_LINE);
                    agencyWebString
                            .append(agencyName != null ? m_agencyInfo.get(agencyName + STRING_WEB) : EMPTY_STRING);
                    agencyWebString.append(STRING_NEW_LINE);
                }
            }
        }

        addParameter(PARAM_AGENCY_NAME, agencyNameString.toString());
        addParameter(PARAM_AGENCY_PHONE, agencyPhoneString.toString());
        addParameter(PARAM_AGENCY_WEBSITE, agencyWebString.toString());

    }

    /**
     * Load the title data of the report.
     */

    private void loadReportHeader() {
        SisStudent student = m_currentIep.getStudent();
        String admin1 = EMPTY_STRING;

        addParameter(PARAM_SCHOOL_ADDRESS_LINE1, EMPTY_STRING);
        addParameter(PARAM_SCHOOL_ADDRESS_LINE2, EMPTY_STRING);
        addParameter(PARAM_SCHOOL_PHONE_NO, EMPTY_STRING);
        addParameter(PARAM_SKL_ADMIN1, EMPTY_STRING);
        addParameter(PARAM_SKL_ADMIN2, EMPTY_STRING);
        addParameter(PARAM_SKL_ADMIN3, EMPTY_STRING);
        SisSchool school = student.getSchool();
        String schoolName = school.getName();
        addParameter(PARAM_SCHOOL_NAME, schoolName);
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
            admin1 = getFullName(adminPerson1);
            addParameter(PARAM_SKL_ADMIN1, admin1);
            String superintendent = (String) school.getAdministrator1().getFieldValueByAlias(ALIAS_DEPT_SUPERVISOR);
            if (null != superintendent) {
                String[] admin2 = superintendent.split(STRING_COMMA);
                addParameter(PARAM_SKL_ADMIN2, admin2[1] + STRING_SPACE + admin2[0]);
            }
        }

        initSssName();
        addParameter(PARAM_SSS_STAFF, m_sssStaffName);
    }
}
