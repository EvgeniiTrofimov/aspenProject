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
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * New Jersey SPED form for Invitation to Disciplinary Action Meeting.
 *
 * @author Follett Software Company
 */
public class InvitationToDisciplinaryActionMeetingData extends BaseFormReportJavaSource {
    /**
     * Other Constants
     */
    private static final String AGE_CATEGORY_O18 = "AgeCategoryO18";
    private static final String AGE_CATEGORY_U18 = "AgeCategoryU18";
    /**
     * Aliases
     */
    private static final String ALIAS_CASE_MANAGER = "Case Manager";
    private static final String ALIAS_COUNSELOR = "counselor";
    private static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    private static final String ALIAS_IMDA_CONDUCT = "imda-conduct";
    private static final String ALIAS_IMDA_CONTACT_BY = "imda-contactby";
    private static final String ALIAS_IMDA_DATE = "imda-date";
    private static final String ALIAS_IMDA_DETERMINE = "imda-determine";
    private static final String ALIAS_IMDA_LOCATION = "imda-location";
    private static final String ALIAS_IMDA_NAME = "imda-name";
    private static final String ALIAS_IMDA_OTHER = "imda-other";
    private static final String ALIAS_IMDA_OTHER_TB = "imda-othertb";
    private static final String ALIAS_IMDA_PLAN = "imda-plan";
    private static final String ALIAS_IMDA_REVIEW = "imda-review";
    private static final String ALIAS_IMDA_ROLE = "imda-role";
    private static final String ALIAS_IMDA_TIME = "imda-time";
    private static final String ALIAS_IMDA_TM1 = "imda-tm1";
    private static final String ALIAS_IMDA_TM2 = "imda-tm2";
    private static final String ALIAS_IMDA_TM3 = "imda-tm3";
    private static final String ALIAS_IMDA_TM4 = "imda-tm4";
    private static final String ALIAS_IMDA_TM5 = "imda-tm5";
    private static final String ALIAS_IMDA_TM6 = "imda-tm6";
    private static final String ALIAS_IMDA_TM7 = "imda-tm7";
    private static final String ALIAS_IMDA_TM8 = "imda-tm8";
    private static final String ALIAS_SCHOOL_FAX = "DOE SCHOOL FAX";
    private static final String ALIAS_STAFF_ROLE = "DOE STAFF ROLE";
    private static final String ALIAS_VICE_PRINCIPAL = "Vice Principal";
    private static final String ALIAS_WORK_PHONE = "DOE STAFF WORK PHONE";

    private static final String BOOLEAN_DATABASE_TRUE = "1";
    // Codes
    private static final String CODE_RELATIONSHIP_CASE_MANAGER = "Case Manager";
    private static final String CODE_RELATIONSHIP_CUSTODIAN = "Custodian";
    private static final String CODE_RELATIONSHIP_FATHER = "Father";
    private static final String CODE_RELATIONSHIP_GUARDIAN = "Guardian";
    private static final String CODE_RELATIONSHIP_GUIDANCE_COUNSELOR = "Guidance Counselor";
    private static final String CODE_RELATIONSHIP_MOTHER = "Mother";
    private static final String CODE_RELATIONSHIP_STUDENT = "Student";
    private static final String CODE_RELATIONSHIP_VICE_PRINCIPAL = "Vice Principal";
    /**
     * Report Constants
     */
    private static final String EXTN_WORK_PHONE = "(908) 284-";
    private static final String FAX = "FAX";
    /**
     * Report Fields
     */
    private static final String FIELD_ADDRESS_LINE1 = "addressLine1";
    private static final String FIELD_ADDRESS_LINE2 = "addressLine2";
    private static final String FIELD_FULL_NAME = "fullName";
    /**
     * Report Parameters
     */
    private static final String PARAM_AGE_CATEGORY = "AGE_CATEGORY";
    private static final String PARAM_CHAIR_PERSON = "CHAIR_PERSON";
    private static final String PARAM_CHAIR_PERSON_PHONE = "CHAIR_PERSON_PHONE";
    private static final String PARAM_CHAIR_PERSON_TITLE = "CHAIR_PERSON_TITLE";
    private static final String PARAM_FORM_DATE = "FORM_DATE";
    private static final String PARAM_INVITED_TEAM_MBRS = "INVITED_TEAM_MBRS";
    private static final String PARAM_MEETING_DATE = "MEETING_DATE";
    private static final String PARAM_MEETING_LOCATION = "MEETING_LOCATION";
    private static final String PARAM_MEETING_TIME = "MEETING_TIME";
    private static final String PARAM_OTHER_DESC = "OTHER_DESC";
    private static final String PARAM_OTHER_TEXT = "otherText";
    private static final String PARAM_PURPOSE1 = "purpose1";
    private static final String PARAM_PURPOSE2 = "purpose2";
    private static final String PARAM_PURPOSE3 = "purpose3";
    private static final String PARAM_PURPOSE4 = "purpose4";
    private static final String PARAM_PURPOSE5 = "purpose5";
    private static final String PARAM_RESPONSE_DATE = "RESPONSE_DATE";
    private static final String PARAM_SCHOOL_ADDRESS_LINE1 = "SKL_ADDRESS_LINE1";
    private static final String PARAM_SCHOOL_ADDRESS_LINE2 = "SKL_ADDRESS_LINE2";
    private static final String PARAM_SCHOOL_FAX_NO = "SKL_FAX_NO";
    private static final String PARAM_SCHOOL_NAME = "SCHOOL_NAME";
    private static final String PARAM_SCHOOL_PHONE_NO = "SKL_PHONE_NO";
    private static final String PARAM_SKL_ADMIN1 = "SKL_ADMIN1";
    private static final String PARAM_SKL_ADMIN2 = "SKL_ADMIN2";
    private static final String PARAM_SKL_ADMIN3 = "SKL_ADMIN3";
    private static final String PARAM_SSS_STAFF = "sssStaff";
    private static final String PARAM_STUDENT_NAME = "STUDENT_NAME";
    private static final String PARAM_TITLE_CASE_MANAGER = "Case Manager";

    private static final String STRING_CLOSE_PAREN = ")";
    private static final String STRING_COMMA = ",";
    private static final String STRING_EMPTY = "";
    private static final String STRING_HYPHEN = "-";
    private static final String STRING_NEW_LINE = "\n";
    private static final String STRING_OPEN_PAREN = "(";
    private static final String STRING_SLASH = "/";
    private static final String STRING_SPACE = " ";
    private static final String STRING_X = "X";

    private final SimpleDateFormat DATE_FORMATTER_LONG = new SimpleDateFormat("MMMMM dd, yyyy");
    private final SimpleDateFormat DATE_FORMATTER_SHORT = new SimpleDateFormat("MM/dd/yyyy");

    /**
     * Instances
     */
    private IepData m_currentIep = null;
    private ReportDataGrid m_grid = new ReportDataGrid();
    private String m_sssStaffName;
    private StringBuilder m_teamNameString = new StringBuilder();

    /**
     * This method builds the entire report.
     *
     * @return Object
     * @throws Exception exception
     */
    @Override
    protected Object gatherData() throws Exception {
        SisStudent student = m_currentIep.getStudent();

        loadReportHeader();

        loadMainDataFromViewForm();

        loadChairPersonDetails();

        loadTeamMemberFromStudentContacts(student);

        loadTeamMemberFromViewForm();

        loadStudentAgeData(student);

        if (m_grid.rowCount() == 0) {
            m_grid.append();
        }

        m_grid.beforeTop();

        return m_grid;
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
     * This method sets the chair person's details for the signature line of the SPED form. If the
     * team members does not
     * include a chair person, then the case manager signs the SPED form.
     */
    private void loadChairPersonDetails() {
        addParameter(PARAM_CHAIR_PERSON, STRING_EMPTY);
        addParameter(PARAM_CHAIR_PERSON_PHONE, STRING_EMPTY);
        addParameter(PARAM_CHAIR_PERSON_TITLE, STRING_EMPTY);

        SisPerson chairPerson = null;
        Collection<IepTeamMember> teamMbrs = m_currentIep.getTeamMembers();
        for (IepTeamMember teamMbr : teamMbrs) {
            if (teamMbr.getChairpersonIndicator()) {
                chairPerson = teamMbr.getPerson();
                break;
            }
        }

        String workPhone = STRING_EMPTY;
        if (chairPerson == null) {
            SisStaff caseManager = m_currentIep.getStaff();
            if (caseManager != null) {
                chairPerson = caseManager.getPerson();
            }
        }

        workPhone = (String) m_currentIep.getStaff().getFieldValueByAlias(ALIAS_WORK_PHONE);

        if (chairPerson != null) {
            addParameter(PARAM_CHAIR_PERSON, chairPerson.getFirstName() + STRING_SPACE + chairPerson.getLastName());
            addParameter(PARAM_CHAIR_PERSON_TITLE, PARAM_TITLE_CASE_MANAGER);

            workPhone = EXTN_WORK_PHONE + workPhone;
            addParameter(PARAM_CHAIR_PERSON_PHONE, workPhone);
        }
    }

    /**
     * Load Main Data Form View.
     */
    private void loadMainDataFromViewForm() {
        String contactBy = (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias(ALIAS_IMDA_CONTACT_BY,
                getDictionary()));
        String date =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias(ALIAS_IMDA_DATE, getDictionary()));
        String time =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias(ALIAS_IMDA_TIME, getDictionary()));
        String location = (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias(ALIAS_IMDA_LOCATION,
                getDictionary()));

        String conduct = (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias(ALIAS_IMDA_CONDUCT,
                getDictionary()));
        String plan =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias(ALIAS_IMDA_PLAN, getDictionary()));
        String review = (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias(ALIAS_IMDA_REVIEW,
                getDictionary()));
        String determine = (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias(ALIAS_IMDA_DETERMINE,
                getDictionary()));
        String other =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias(ALIAS_IMDA_OTHER, getDictionary()));
        String otherTB = (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias(ALIAS_IMDA_OTHER_TB,
                getDictionary()));

        addParameter(PARAM_PURPOSE1, STRING_EMPTY);
        addParameter(PARAM_PURPOSE2, STRING_EMPTY);
        addParameter(PARAM_PURPOSE3, STRING_EMPTY);
        addParameter(PARAM_PURPOSE4, STRING_EMPTY);
        addParameter(PARAM_PURPOSE5, STRING_EMPTY);
        addParameter(PARAM_OTHER_TEXT, STRING_EMPTY);

        if (conduct.equals(BOOLEAN_DATABASE_TRUE)) {
            addParameter(PARAM_PURPOSE1, STRING_X);
        }
        if (plan.equals(BOOLEAN_DATABASE_TRUE)) {
            addParameter(PARAM_PURPOSE2, STRING_X);
        }
        if (review.equals(BOOLEAN_DATABASE_TRUE)) {
            addParameter(PARAM_PURPOSE3, STRING_X);
        }
        if (determine.equals(BOOLEAN_DATABASE_TRUE)) {
            addParameter(PARAM_PURPOSE4, STRING_X);
        }
        if (other.equals(BOOLEAN_DATABASE_TRUE)) {
            addParameter(PARAM_PURPOSE5, STRING_X);
            addParameter(PARAM_OTHER_TEXT, otherTB);
        }

        addParameter(PARAM_MEETING_DATE, STRING_EMPTY);
        addParameter(PARAM_MEETING_TIME, STRING_EMPTY);
        addParameter(PARAM_MEETING_LOCATION, STRING_EMPTY);

        String contactByDate = STRING_EMPTY;

        if (contactBy != null) {
            String[] tokens = contactBy.split(STRING_HYPHEN);

            String personYear = tokens[0];
            String personMonth = tokens[1];
            String personDay = tokens[2];

            contactByDate = personMonth + STRING_SLASH + personDay + STRING_SLASH + personYear;
        }
        contactBy = contactByDate;

        String dateContactByDate = null;
        try {
            dateContactByDate = DATE_FORMATTER_LONG.format(DATE_FORMATTER_SHORT.parse(contactBy));
            addParameter(PARAM_RESPONSE_DATE, dateContactByDate);
        } catch (ParseException e) {
            addParameter(PARAM_RESPONSE_DATE, null);
        }

        String strFormatDate = STRING_EMPTY;

        if (date != null) {
            String[] tokens = date.split(STRING_HYPHEN);

            String personYear = tokens[0];
            String personMonth = tokens[1];
            String personDay = tokens[2];

            strFormatDate = personMonth + STRING_SLASH + personDay + STRING_SLASH + personYear;
        }

        addParameter(PARAM_MEETING_DATE, strFormatDate);
        addParameter(PARAM_MEETING_TIME, time);
        addParameter(PARAM_MEETING_LOCATION, location);

        FormInstance formInstance = getFormInstance();
        addParameter(PARAM_FORM_DATE, DATE_FORMATTER_LONG.format(new Date()));
        if (formInstance != null) {
            addParameter(PARAM_FORM_DATE, DATE_FORMATTER_LONG.format(new Date(formInstance.getCreatedTime())));
        }
    }

    /**
     * Load Report Header.
     */
    private void loadReportHeader() {
        SisStudent student = m_currentIep.getStudent();
        String admin1 = STRING_EMPTY;

        addParameter(PARAM_SCHOOL_ADDRESS_LINE1, STRING_EMPTY);
        addParameter(PARAM_SCHOOL_ADDRESS_LINE2, STRING_EMPTY);
        addParameter(PARAM_SCHOOL_PHONE_NO, STRING_EMPTY);
        addParameter(PARAM_SKL_ADMIN1, STRING_EMPTY);
        addParameter(PARAM_SKL_ADMIN2, STRING_EMPTY);
        addParameter(PARAM_SKL_ADMIN3, STRING_EMPTY);

        SisSchool school = student.getSchool();
        String schoolName = school.getName();

        addParameter(PARAM_SCHOOL_NAME, schoolName);

        String fax = (String) school.getFieldValueByAlias(ALIAS_SCHOOL_FAX);

        addParameter(PARAM_SCHOOL_FAX_NO, !StringUtils.isEmpty(fax) ? FAX + fax : STRING_EMPTY);

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
            admin1 = adminPerson1.getFirstName() + STRING_SPACE + adminPerson1.getLastName();

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

    /**
     * Load Student Age related Data.
     *
     * @param student SisStudent
     */
    private void loadStudentAgeData(SisStudent student) {
        int age = 15; // Some default age value which is less than 18 to print the form for parents.

        addParameter(PARAM_AGE_CATEGORY, (age < 18) ? AGE_CATEGORY_U18 : AGE_CATEGORY_O18);
        addParameter(PARAM_STUDENT_NAME,
                student.getPerson().getFirstName() + STRING_SPACE + student.getPerson().getLastName());
        addParameter(PARAM_OTHER_DESC, STRING_EMPTY);


        // Print one copy to the student mailing address.
        Address studentMailingAddress = student.getPerson().getMailingAddress();
        if (studentMailingAddress == null) {
            studentMailingAddress = student.getPerson().getPhysicalAddress();
        }
        String studentMailingAddressView = STRING_EMPTY;
        if (studentMailingAddress.getAddressLine01() != null) {
            studentMailingAddressView = studentMailingAddress.getAddressLine01().trim();
        }

        Collection<StudentContact> studentContacts = student.getContacts();

        Set<String> prevMailingAddresses = new HashSet<String>();
        String contactMailingAddressView = STRING_EMPTY;

        setReportFields(student.getPerson());

        prevMailingAddresses.add(studentMailingAddressView);


        // Additional single copies to any other contacts that don't LiveWith the student
        for (StudentContact studentContact : studentContacts) {
            if (studentContact.getGradeMailingIndicator()) {
                Contact contact = studentContact.getContact();
                Person contactPerson = contact.getPerson();

                Address contactMailingAddress = contactPerson.getMailingAddress();
                contactMailingAddressView = STRING_EMPTY;

                // Don't print a letter if the contact doesn't have an address.
                if (contactMailingAddress != null) {
                    if (contactMailingAddress.getAddressLine01() != null) {
                        contactMailingAddressView = contactMailingAddress.getAddressLine01().trim();
                    }

                    if (!StringUtils.isEmpty(contactMailingAddressView)) {
                        // Print only one copy of the report for each address of all contacts having
                        // the GradeMailing flag set.
                        // Only send a copy of the report to a contact that doesn't Live With the
                        // student.
                        if (!prevMailingAddresses.contains(contactMailingAddressView)
                                && !(studentContact.getLivesWithIndicator())) {
                            setReportFields(contactPerson);

                            prevMailingAddresses.add(contactMailingAddressView);
                        }
                    }
                }
            }
        }

    }

    /**
     * Load Team Members from the Student's Contacts.
     *
     * @param student SisStudent
     */
    private void loadTeamMemberFromStudentContacts(SisStudent student) {
        // Compile list of participants from the student's contacts
        String studentName = student.getPerson().getFirstName() + STRING_SPACE + student.getPerson().getLastName();

        String counselorName = (String) student.getFieldValueByAlias(ALIAS_COUNSELOR);
        String caseManagerName = (String) student.getFieldValueByAlias(ALIAS_CASE_MANAGER);
        String vicePrincipalName = (String) student.getFieldValueByAlias(ALIAS_VICE_PRINCIPAL);

        if (!StringUtils.isEmpty(counselorName) && counselorName.contains(STRING_COMMA)) {
            String[] splitCounselorName = counselorName.split(STRING_COMMA);
            String counselorNameLast = splitCounselorName[0];
            String counselorNameFirst = splitCounselorName[1];
            counselorName = counselorNameFirst.trim() + STRING_SPACE + counselorNameLast.trim();
        }

        if (!StringUtils.isEmpty(caseManagerName) && caseManagerName.contains(STRING_COMMA)) {
            String[] splitCaseManagerName = caseManagerName.split(STRING_COMMA);
            String caseManagerNameLast = splitCaseManagerName[0];
            String caseManagerNameFirst = splitCaseManagerName[1];
            caseManagerName = caseManagerNameFirst.trim() + STRING_SPACE + caseManagerNameLast.trim();
        }

        if (!StringUtils.isEmpty(vicePrincipalName) && vicePrincipalName.contains(STRING_COMMA)) {
            String[] splitVicePrincipalName = vicePrincipalName.split(STRING_COMMA);
            String vicePrincipalNameLast = splitVicePrincipalName[0];
            String vicePrincipalNameFirst = splitVicePrincipalName[1];
            vicePrincipalName = vicePrincipalNameFirst.trim() + STRING_SPACE + vicePrincipalNameLast.trim();
        }

        String momName = null;
        String dadName = null;
        String custodianName = null;
        String guardianName = null;

        Collection<StudentContact> contacts = student.getContacts(getBroker());

        for (StudentContact contact : contacts) {
            String relationshipCode = contact.getRelationshipCode();

            // There is an assumption that there will be one one of each type. If there is more than
            // two the last one if used.
            // No logic for the other type such as Emergency Contact.
            if (relationshipCode != null) {
                switch (relationshipCode) {
                    case CODE_RELATIONSHIP_MOTHER:
                        momName = contact.getPerson().getFirstName() + STRING_SPACE + contact.getPerson().getLastName();
                        break;
                    case CODE_RELATIONSHIP_FATHER:
                        dadName = contact.getPerson().getFirstName() + STRING_SPACE + contact.getPerson().getLastName();
                        break;
                    case CODE_RELATIONSHIP_CUSTODIAN:
                        custodianName =
                                contact.getPerson().getFirstName() + STRING_SPACE + contact.getPerson().getLastName();
                        break;
                    case CODE_RELATIONSHIP_GUARDIAN:
                        guardianName =
                                contact.getPerson().getFirstName() + STRING_SPACE + contact.getPerson().getLastName();
                        break;
                }
            }
        }


        String tm1 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias(ALIAS_IMDA_TM1, getDictionary()));
        String tm2 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias(ALIAS_IMDA_TM2, getDictionary()));
        String tm3 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias(ALIAS_IMDA_TM3, getDictionary()));
        String tm4 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias(ALIAS_IMDA_TM4, getDictionary()));
        String tm5 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias(ALIAS_IMDA_TM5, getDictionary()));
        String tm6 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias(ALIAS_IMDA_TM6, getDictionary()));
        String tm7 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias(ALIAS_IMDA_TM7, getDictionary()));
        String tm8 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias(ALIAS_IMDA_TM8, getDictionary()));

        if (tm1.equals(BOOLEAN_DATABASE_TRUE) && studentName != null) {
            m_teamNameString.append(studentName != null
                    ? studentName + STRING_SPACE + STRING_OPEN_PAREN + CODE_RELATIONSHIP_STUDENT + STRING_CLOSE_PAREN
                    : STRING_OPEN_PAREN + CODE_RELATIONSHIP_STUDENT + STRING_CLOSE_PAREN);
            m_teamNameString.append(STRING_NEW_LINE);
        }
        if (tm2.equals(BOOLEAN_DATABASE_TRUE) && momName != null) {
            m_teamNameString.append(momName != null
                    ? momName + STRING_SPACE + STRING_OPEN_PAREN + CODE_RELATIONSHIP_MOTHER + STRING_CLOSE_PAREN
                    : STRING_OPEN_PAREN + CODE_RELATIONSHIP_MOTHER + STRING_CLOSE_PAREN);
            m_teamNameString.append(STRING_NEW_LINE);
        }
        if (tm3.equals(BOOLEAN_DATABASE_TRUE) && dadName != null) {
            m_teamNameString.append(dadName != null
                    ? dadName + STRING_SPACE + STRING_OPEN_PAREN + CODE_RELATIONSHIP_FATHER + STRING_CLOSE_PAREN
                    : STRING_OPEN_PAREN + CODE_RELATIONSHIP_FATHER + STRING_CLOSE_PAREN);
            m_teamNameString.append(STRING_NEW_LINE);
        }
        if (tm7.equals(BOOLEAN_DATABASE_TRUE) && custodianName != null) {
            m_teamNameString.append(custodianName != null
                    ? custodianName + STRING_SPACE + STRING_OPEN_PAREN + CODE_RELATIONSHIP_CUSTODIAN
                            + STRING_CLOSE_PAREN
                    : STRING_OPEN_PAREN + CODE_RELATIONSHIP_CUSTODIAN + STRING_CLOSE_PAREN);
            m_teamNameString.append(STRING_NEW_LINE);
        }
        if (tm8.equals(BOOLEAN_DATABASE_TRUE) && guardianName != null) {
            m_teamNameString.append(guardianName != null
                    ? guardianName + STRING_SPACE + STRING_OPEN_PAREN + CODE_RELATIONSHIP_GUARDIAN + STRING_CLOSE_PAREN
                    : STRING_OPEN_PAREN + CODE_RELATIONSHIP_GUARDIAN + STRING_CLOSE_PAREN);
            m_teamNameString.append(STRING_NEW_LINE);
        }
        if (tm4.equals(BOOLEAN_DATABASE_TRUE) && caseManagerName != null) {
            m_teamNameString.append(caseManagerName != null
                    ? caseManagerName + STRING_SPACE + STRING_OPEN_PAREN + CODE_RELATIONSHIP_CASE_MANAGER
                            + STRING_CLOSE_PAREN
                    : STRING_OPEN_PAREN + CODE_RELATIONSHIP_CASE_MANAGER + STRING_CLOSE_PAREN);
            m_teamNameString.append(STRING_NEW_LINE);
        }
        if (tm5.equals(BOOLEAN_DATABASE_TRUE) && counselorName != null) {
            m_teamNameString.append(counselorName != null
                    ? counselorName + STRING_SPACE + STRING_OPEN_PAREN + CODE_RELATIONSHIP_GUIDANCE_COUNSELOR
                            + STRING_CLOSE_PAREN
                    : STRING_OPEN_PAREN + CODE_RELATIONSHIP_GUIDANCE_COUNSELOR + STRING_CLOSE_PAREN);
            m_teamNameString.append(STRING_NEW_LINE);
        }
        if (tm6.equals(BOOLEAN_DATABASE_TRUE) && vicePrincipalName != null) {
            m_teamNameString.append(vicePrincipalName != null
                    ? vicePrincipalName + STRING_SPACE + STRING_OPEN_PAREN + CODE_RELATIONSHIP_VICE_PRINCIPAL
                            + STRING_CLOSE_PAREN
                    : STRING_OPEN_PAREN + CODE_RELATIONSHIP_VICE_PRINCIPAL + STRING_CLOSE_PAREN);
            m_teamNameString.append(STRING_NEW_LINE);
        }

    }

    /**
     * Load Team Members from the From's View.
     */
    private void loadTeamMemberFromViewForm() {
        // Append the list of participants from the form's entered members
        String gfdOID = (((GenericFormData) getFormStorage()).getOid());
        Criteria gfdcCriteria = new Criteria();
        gfdcCriteria.addEqualTo(GenericFormChildData.COL_GENERIC_FORM_DATA_OID, gfdOID);

        QueryByCriteria gfdcQuery = new QueryByCriteria(GenericFormChildData.class, gfdcCriteria);

        String gfdcName = STRING_EMPTY;
        String gfdcRole = STRING_EMPTY;

        Collection<GenericFormChildData> geneticFormDataChildren = getBroker().getCollectionByQuery(gfdcQuery);

        for (GenericFormChildData geneticFormDataChild : geneticFormDataChildren) {
            Object imdaName = geneticFormDataChild.getFieldValueByAlias(ALIAS_IMDA_NAME, getDictionary());

            if (imdaName != null) {
                gfdcName = (String) imdaName;

                if (!StringUtils.isEmpty(gfdcName)) {
                    m_teamNameString.append(gfdcName);

                    Object imdaRole = geneticFormDataChild.getFieldValueByAlias(ALIAS_IMDA_ROLE, getDictionary());

                    if (imdaRole != null) {
                        gfdcRole = (String) imdaRole;

                        if (!StringUtils.isEmpty(gfdcRole)) {
                            m_teamNameString.append(STRING_SPACE + STRING_OPEN_PAREN + gfdcRole + STRING_CLOSE_PAREN);
                        }
                    }

                    m_teamNameString.append(STRING_NEW_LINE);
                }
            }
        }

        addParameter(PARAM_INVITED_TEAM_MBRS, m_teamNameString.toString());
    }

    /**
     * This method sets all the report fields related to a Person.
     *
     * @param person void
     */
    private void setReportFields(Person person) {
        m_grid.append();
        m_grid.set(FIELD_FULL_NAME, person.getFirstName() + STRING_SPACE + person.getLastName());
        m_grid.set(FIELD_ADDRESS_LINE1, STRING_EMPTY);
        m_grid.set(FIELD_ADDRESS_LINE2, STRING_EMPTY);

        Address mailingAddress = person.getResolvedMailingAddress();
        if (mailingAddress != null) {
            m_grid.set(FIELD_ADDRESS_LINE1, mailingAddress.getAddressLine01());
            m_grid.set(FIELD_ADDRESS_LINE2, mailingAddress.getAddressLine03());
        }
    }
}
