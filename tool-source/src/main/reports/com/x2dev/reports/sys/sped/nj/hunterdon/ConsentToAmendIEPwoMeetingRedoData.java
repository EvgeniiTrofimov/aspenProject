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
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;
import net.sf.jasperreports3.engine.data.JRBeanCollectionDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class ConsentToAmendIEPwoMeetingRedoData.
 */
public class ConsentToAmendIEPwoMeetingRedoData extends BaseFormReportJavaSource {
    /**
     * Aliases
     */
    private static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    private static final String ALIAS_OTHER_OPTIONS_CONSIDERED = "sped-other-options-considered";
    private static final String ALIAS_PROPOSED_CHANGES = "sped-proposed-changes";
    private static final String ALIAS_SCHOOL_FAX = "DOE SCHOOL FAX";
    private static final String ALIAS_SPED_RECIPIENT = "sped-Recipient";
    private static final String ALIAS_STAFF_ROLE = "DOE STAFF ROLE";
    private static final String ALIAS_USED_PROC_FACTORS = "sped-used-procedure-factors";
    private static final String ALIAS_WORK_PHONE = "DOE STAFF WORK PHONE";
    /**
     * Other Constants
     */
    private static final String COL_PARAMETER_MAP = "parameters";

    /**
     * Report Constants
     */
    private static final String EXTN_WORK_PHONE = "(908) 284-";
    private static final String FAX = "FAX";
    /**
     * Report Fields
     */
    private static final String FIELD_BLANK_TYPE = "blankType";
    private static final String FIELD_PROC_SAFE_DATA = "procSafeData";
    private static final String FIELD_RECIPIENT_NAME = "recipientName";
    private static final String FIELD_STUDENT_ADDRESS_01 = "studentAddress01";
    private static final String FIELD_STUDENT_CITY = "studentCity";
    private static final String FIELD_STUDENT_EIGHTEENTH = "studentEighteenth";
    private static final String FIELD_STUDENT_NAME = "studentName";
    private static final String FIELD_STUDENT_STATE = "studentState";
    private static final String FIELD_STUDENT_ZIP = "studentZip";

    private static final String FORM_DEF_ID_REDETERMINE = "SPED-NJ-REDETERMINE";

    /**
     * Codes
     */
    private static final String LETTER_RECIPIENT = "Parent";
    private static final String MEMBER_ROLE_STUDENT = "Student";

    private static final String NOTICE_DETAILS_PART1 = "According to N.J.A.C. 6A:14-3.7(d), an IEP may be amended " +
            "without a meeting, if you provide written consent to the change(s) " +
            "being proposed.  Your consent is requested to make changes to the " +
            "IEP of ";
    private static final String NOTICE_DETAILS_PART2 = ".  If you do not respond to this request by ";
    private static final String NOTICE_DETAILS_PART3 =
            ", the proposal expires and the change(s) cannot be made to the " +
                    "student’s IEP at this time.   If you agree with the proposed " +
                    "change(s), the student’s IEP will be amended and you will " +
                    "receive a copy of the amended IEP.  A copy of the short " +
                    "procedural safeguards statement is attached.";

    private static final int NUMBER_15 = 15;
    private static final int NUMBER_18 = 18;
    /**
     * Report Parameters
     */
    private static final String PARAM_CASE_MANAGER = "caseManager";
    private static final String PARAM_CASE_MANAGER_NAME = "caseManagerName";
    private static final String PARAM_CASE_MANAGER_PHONE = "caseManagerPhone";
    private static final String PARAM_CASE_MANAGER_ROLE = "caseManagerRole";
    private static final String PARAM_CASE_MANAGER_WORKPHONE = "workPhone";
    private static final String PARAM_CHAIR_PERSON = "CHAIR_PERSON";
    private static final String PARAM_CHAIR_PERSON_ROLE = "chairPersonRole";
    private static final String PARAM_CHAIR_PERSON_SCHOOL_ADDRESS_LINE1 = "chairPersonAddressLine1";
    private static final String PARAM_CHAIR_PERSON_SCHOOL_ADDRESS_LINE2 = "chairPersonAddressLine2";
    private static final String PARAM_CHAIR_PERSON_SCHOOL_NAME = "chairPersonSchoolName";
    private static final String PARAM_CHAIR_PERSON_WORKPHONE = "chairPersonWorkPhone";
    private static final String PARAM_DATE_CONVERTER = "dateAsStringConverter";
    private static final String PARAM_FORM_DATE = "FORM_DATE";
    private static final String PARAM_IEP_DATA = "iepData";
    private static final String PARAM_IS_REDETERMINE_ELIG = "isRedetermineEligibility";
    private static final String PARAM_LETTER_RECIPIENT = "letterRecp";
    private static final String PARAM_MEETING = "meeting";
    private static final String PARAM_MEETING_DICTIONARY = "meetingDictionary";
    private static final String PARAM_NOTICE_DETAILS = "noticeDetails";
    private static final String PARAM_OTHER_OPTIONS_CONSIDERED = "otherOptionsConsidered";
    private static final String PARAM_PROC_SAFE_REPORT = "procSafeReport";
    private static final String PARAM_PROC_SAFE_SUBREPORT_ID = "procSafeSubreportId";
    private static final String PARAM_PROPOSED_CHANGES = "proposedChanges";
    private static final String PARAM_SCHOOL_ADDRESS_LINE1 = "SKL_ADDRESS_LINE1";
    private static final String PARAM_SCHOOL_ADDRESS_LINE2 = "SKL_ADDRESS_LINE2";
    private static final String PARAM_SCHOOL_FAX_NO = "SKL_FAX_NO";
    private static final String PARAM_SCHOOL_NAME = "SCHOOL_NAME";
    private static final String PARAM_SCHOOL_PHONE_NO = "SKL_PHONE_NO";
    private static final String PARAM_SKL_ADMIN1 = "SKL_ADMIN1";
    private static final String PARAM_SKL_ADMIN2 = "SKL_ADMIN2";
    private static final String PARAM_SKL_ADMIN3 = "SKL_ADMIN3";
    private static final String PARAM_SSS_STAFF = "sssStaff";
    private static final String PARAM_STUDENT_FIRST_NAME = "studentFirstName";
    private static final String PARAM_USED_PROC_FACTORS = "usedProcedureFactors";

    private static final String STRING_COMMA = ",";
    private static final String STRING_EMPTY = "";
    private static final String STRING_SPACE = " ";

    private final SimpleDateFormat DATE_FORMATTER_SHORT = new SimpleDateFormat("MM/dd/yyyy");
    private final SimpleDateFormat DATE_FORMATTER_LONG = new SimpleDateFormat("MMMMM dd, yyyy");

    private IepData m_currentIep;
    private GenericFormData m_meeting;
    private DataDictionary m_meetingDictionary;
    private Date m_responseDate = new Date();
    private String m_sssStaffName;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        String letterRecp = LETTER_RECIPIENT;
        GenericFormData genericFormData = (GenericFormData) getFormStorage();
        if (genericFormData != null) {
            if ((String) genericFormData.getFieldValueByAlias(ALIAS_SPED_RECIPIENT, getDictionary()) != null) {
                letterRecp = (String) genericFormData.getFieldValueByAlias(ALIAS_SPED_RECIPIENT, getDictionary());
            }
            addParameter(PARAM_LETTER_RECIPIENT, letterRecp);

            String proposedChanges =
                    (String) genericFormData.getFieldValueByAlias(ALIAS_PROPOSED_CHANGES, getDictionary());
            String otherOptionsConsidered =
                    (String) genericFormData.getFieldValueByAlias(ALIAS_OTHER_OPTIONS_CONSIDERED, getDictionary());
            String usedProcedureFactors =
                    (String) genericFormData.getFieldValueByAlias(ALIAS_USED_PROC_FACTORS, getDictionary());

            addParameter(PARAM_PROPOSED_CHANGES, (proposedChanges != null) ? proposedChanges : STRING_EMPTY);
            addParameter(PARAM_OTHER_OPTIONS_CONSIDERED,
                    (otherOptionsConsidered != null) ? otherOptionsConsidered : STRING_EMPTY);
            addParameter(PARAM_USED_PROC_FACTORS, (usedProcedureFactors != null) ? usedProcedureFactors : STRING_EMPTY);
        }

        getChairPersonDetails();
        FormInstance formInstance = getFormInstance();
        Calendar calendar = Calendar.getInstance();
        Date formDate = new Date();
        if (formInstance != null) {
            formDate = new Date(formInstance.getCreatedTime());
        }

        calendar.setTime(formDate);
        calendar.add(Calendar.DATE, NUMBER_15);
        m_responseDate = calendar.getTime();
        addParameter(PARAM_FORM_DATE, DATE_FORMATTER_LONG.format(formDate));

        ReportDataGrid grid = new ReportDataGrid();
        HashMap<String, Object> parameterMap = new HashMap<String, Object>();

        loadReportHeader();

        SisStudent student = m_currentIep.getStudent();
        Person studentPerson = student.getPerson();

        // This allows blank form to be produced if requested
        Collection<X2BaseBean> emptyReportGrid = new ArrayList<X2BaseBean>();
        emptyReportGrid.add(getOrganization());

        if (getParameter(PARAM_BLANK).equals(Boolean.FALSE)) {
            addParameter(PARAM_IEP_DATA, m_currentIep);
            addParameter(PARAM_MEETING, m_meeting);
            addParameter(PARAM_MEETING_DICTIONARY, m_meetingDictionary);

            if (m_currentIep.getStaff() != null &&
                    m_currentIep.getStaff().getPerson() != null) {
                SisStaff staff = m_currentIep.getStaff();

                if (staff != null) {
                    String caseManagerRole = staff.getSpedRole();
                    addParameter(PARAM_CASE_MANAGER_ROLE, caseManagerRole);

                    SisPerson staffPerson = staff.getPerson();

                    if (staffPerson != null) {
                        String caseManager = getFullName(staffPerson);
                        String caseManagerName = staffPerson.getFirstName() + STRING_SPACE + staffPerson.getLastName();
                        addParameter(PARAM_CASE_MANAGER, caseManager);
                        addParameter(PARAM_CASE_MANAGER_NAME, caseManagerName);
                    }
                }
            }

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

            if (letterRecp.equals(MEMBER_ROLE_STUDENT)) {
                grid.append();
                grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(emptyReportGrid));

                addMeetingData(grid);

                addNotificationRecipient(grid, studentPerson, studentMailingAddress);
            } else // MEMBER_ROLE_PARENT
            {
                // One single copy to the Student's mailing address
                grid.append();
                grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(emptyReportGrid));

                addMeetingData(grid);

                // For this report the recipients name will not be used.
                addNotificationRecipient(grid, studentPerson, studentMailingAddress);

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
                                // Print only one copy of the report for each address of all
                                // contacts having the GradeMailing flag set.
                                // Only send a copy of the report to contact that don't Live With
                                // the student.
                                if (!prevMailingAddresses.contains(contactMailingAddressView)
                                        && !(studentContact.getLivesWithIndicator())) {
                                    grid.append();
                                    grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(emptyReportGrid));

                                    addMeetingData(grid);

                                    addNotificationRecipient(grid, contactPerson, contactMailingAddress);

                                    prevMailingAddresses.add(contactMailingAddressView);
                                }
                            }
                        }
                    }
                }
            }
        }

        String subReportID = (String) getParameter(PARAM_PROC_SAFE_SUBREPORT_ID);
        Report evalSubreport = ReportUtils.getReport(subReportID, getBroker());
        addParameter(PARAM_PROC_SAFE_REPORT, new ByteArrayInputStream(evalSubreport.getCompiledFormat()));

        String redetermineFormId = getFormDefinition().getId();
        addParameter(PARAM_IS_REDETERMINE_ELIG,
                FORM_DEF_ID_REDETERMINE.equals(redetermineFormId) ? Boolean.TRUE : Boolean.FALSE);

        // if there are no recipients, use an empty grid to print a blank report
        if (grid.rowCount() == 0) {
            grid.append();
            grid.set(FIELD_BLANK_TYPE, Boolean.FALSE);
            grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(emptyReportGrid));
            grid.append();
            grid.set(FIELD_BLANK_TYPE, Boolean.TRUE);
            grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(emptyReportGrid));
        }

        IepData iepData = (IepData) getFormOwner();

        grid.set(PARAM_CASE_MANAGER_WORKPHONE, STRING_EMPTY);
        Staff staff = iepData.getStaff();
        if (staff != null) {
            grid.set(PARAM_CASE_MANAGER, staff);
            String caseManagerWorkPhone = (String) staff.getFieldValueByAlias(ALIAS_WORK_PHONE);
            if (caseManagerWorkPhone != null) {
                grid.set(PARAM_CASE_MANAGER_WORKPHONE, EXTN_WORK_PHONE + caseManagerWorkPhone);
            }
            grid.set(COL_PARAMETER_MAP, parameterMap);
            if (caseManagerWorkPhone != null) {
                addParameter(PARAM_CASE_MANAGER_PHONE, EXTN_WORK_PHONE + caseManagerWorkPhone);
            }
        }

        grid.beforeTop();

        String studentName = null;
        String studentFirstName = null;
        if (studentPerson != null) {
            studentName = getFullName(studentPerson);
            studentFirstName = studentPerson.getFirstName();

            addParameter(PARAM_STUDENT_FIRST_NAME, studentFirstName);
            addParameter(PARAM_NOTICE_DETAILS, NOTICE_DETAILS_PART1 + studentName + NOTICE_DETAILS_PART2
                    + DATE_FORMATTER_SHORT.format(m_responseDate) + NOTICE_DETAILS_PART3);
        }

        return grid;
    }

    /**
     * Initialize report, m_currentIep (IepData), m_meeting (GenericFormData), and
     * m_meetingDictionary.
     */
    @Override
    protected void initialize() {
        super.initialize();
        m_currentIep = (IepData) getFormOwner();
        m_meeting = (GenericFormData) getFormStorage();

        ExtendedDictionaryAttributes extendDictionary = m_meeting.getExtendedDataDictionary();
        m_meetingDictionary = DataDictionary.getDistrictDictionary(extendDictionary, getBroker().getPersistenceKey());

        addParameter(PARAM_DATE_CONVERTER,
                ConverterFactory.getConverterForClass(PlainDate.class.getName(), Locale.getDefault(), true));
    }

    /**
     * Add meeting data to grid for display on report.
     *
     * @param grid ReportDataGrid
     */
    private void addMeetingData(ReportDataGrid grid) {
        if (m_currentIep.getStudent() != null &&
                m_currentIep.getStudent().getPerson() != null) {
            String studentName = getFullName(m_currentIep.getStudent().getPerson());
            grid.set(FIELD_STUDENT_NAME, studentName);
        }

        PlainDate eighteenthBirthDate = null;
        if (m_currentIep.getStudent() != null) {
            Person person = m_currentIep.getStudent().getPerson();
            PlainDate birthDate = person.getDob();
            if (birthDate != null) {
                eighteenthBirthDate = DateUtils.add(birthDate, Calendar.YEAR, NUMBER_18);
            }
        }

        grid.set(FIELD_STUDENT_EIGHTEENTH, eighteenthBirthDate);
    }

    /**
     * Add meeting invitation recipient data to grid for display on report.
     *
     * @param grid ReportDataGrid
     * @param person Person
     * @param address Address
     */
    private void addNotificationRecipient(ReportDataGrid grid, Person person, Address address) {
        if (person != null) {
            String recipientName = getFullName(person);
            grid.set(FIELD_RECIPIENT_NAME, recipientName);

            grid.set(FIELD_STUDENT_ADDRESS_01, null);
            grid.set(FIELD_STUDENT_CITY, null);
            grid.set(FIELD_STUDENT_STATE, null);
            grid.set(FIELD_STUDENT_ZIP, null);

            if (address != null) {
                grid.set(FIELD_STUDENT_ADDRESS_01, address.getAddressLine01());
                grid.set(FIELD_STUDENT_CITY, address.getCity());
                grid.set(FIELD_STUDENT_STATE, address.getState());
                grid.set(FIELD_STUDENT_ZIP, address.getPostalCode());
            }
        }
    }

    /**
     * This method sets the chair person's details/address for the signature line of the SPED form.
     * For this form,
     * the case manager is the chair person.
     *
     * @return void
     */
    private void getChairPersonDetails() {
        addParameter(PARAM_CHAIR_PERSON, STRING_EMPTY);
        addParameter(PARAM_CHAIR_PERSON_SCHOOL_NAME, STRING_EMPTY);
        addParameter(PARAM_CHAIR_PERSON_SCHOOL_ADDRESS_LINE1, STRING_EMPTY);
        addParameter(PARAM_CHAIR_PERSON_SCHOOL_ADDRESS_LINE2, STRING_EMPTY);
        SisPerson chairPerson = null;

        SisStaff caseManager = m_currentIep.getStaff();

        if (caseManager != null) {
            SisSchool school = caseManager.getSchool();

            if (school != null) {
                addParameter(PARAM_CHAIR_PERSON_SCHOOL_NAME, school.getName());

                Address schoolAddress = school.getAddress();
                if (schoolAddress != null) {
                    addParameter(PARAM_CHAIR_PERSON_SCHOOL_ADDRESS_LINE1, schoolAddress.getAddressLine01());
                    addParameter(PARAM_CHAIR_PERSON_SCHOOL_ADDRESS_LINE2, schoolAddress.getAddressLine03());
                }
            }

            chairPerson = caseManager.getPerson();

            if (chairPerson != null) {
                addParameter(PARAM_CHAIR_PERSON, StringUtils.coalesce(chairPerson.getFirstName() + STRING_SPACE +
                        chairPerson.getLastName(), STRING_EMPTY));
                addParameter(PARAM_CHAIR_PERSON_ROLE, StringUtils.coalesce(caseManager.getSpedRole(), STRING_EMPTY));

                if (chairPerson.getStaff() != null) {
                    addParameter(PARAM_CHAIR_PERSON_WORKPHONE,
                            StringUtils.coalesce(EXTN_WORK_PHONE + chairPerson.getStaff()
                                    .getFieldValueByAlias(ALIAS_WORK_PHONE), STRING_EMPTY));
                }
            }

        }
    }

    /**
     * Get full name from a person bean.
     *
     * @param person Person
     * @return fullName
     */
    private String getFullName(Person person) {
        String fullName = STRING_EMPTY;

        fullName = person.getFirstName();

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
}
