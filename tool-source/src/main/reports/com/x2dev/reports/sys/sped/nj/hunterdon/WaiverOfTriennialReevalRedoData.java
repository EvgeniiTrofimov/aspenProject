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
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
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
import java.util.Collection;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;
import net.sf.jasperreports3.engine.data.JRBeanCollectionDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class WaiverOfTriennialReevalRedoData.
 */
public class WaiverOfTriennialReevalRedoData extends BaseFormReportJavaSource {
    /**
     * Aliases
     */
    private static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    private static final String ALIAS_RI_OP1 = "ri-op1";
    private static final String ALIAS_RI_OP2 = "ri-op2";
    private static final String ALIAS_RI_OP3 = "ri-op3";
    private static final String ALIAS_STAFF_ROLE = "DOE STAFF ROLE";
    private static final String ALIAS_TRIENNIAL_AGAIN = "triennial-again";
    private static final String ALIAS_TRIENNIAL_DUE_DATE = "triennial-due-date";
    private static final String ALIAS_TRIENNIAL_LETTER_DATE = "triennial-letter-date";
    private static final String ALIAS_TRIENNIAL_LETTER_RETURNDATE = "triennial-letter-returndate";
    private static final String ALIAS_TRIENNIAL_OTHER_OPTIONS = "triennial-other-options";
    private static final String ALIAS_TRIENNIAL_REASONS_WAIVING = "triennial-reasons-waiving";
    private static final String ALIAS_TRIENNIAL_RECIPIENT = "triennial-Recipient";
    private static final String ALIAS_WORK_PHONE = "DOE STAFF WORK PHONE";
    /**
     * Report Constants
     */
    private static final String CONSTANT_POSSESSIVE_SUFFIX = "'s";
    /**
     * Report Fields
     */
    private static final String FIELD_BLANK_TYPE = "blankType";
    private static final String FIELD_PROC_SAFE_DATA = "procSafeData";
    private static final String FIELD_RECIPIENT_ADDRESS_01 = "recipientAddress01";
    private static final String FIELD_RECIPIENT_CITY = "recipientCity";
    private static final String FIELD_RECIPIENT_NAME = "recipientName";
    private static final String FIELD_RECIPIENT_STATE = "recipientState";
    private static final String FIELD_RECIPIENT_ZIP = "recipientZip";
    private static final String FIELD_STUDENT_NAME = "studentName";
    /**
     * Codes
     */
    private static final String LETTER_RECIPIENT = "Parent";
    private static final String MEMBER_ROLE_STUDENT = "Student";
    /**
     * Report Parameters
     */
    private static final String PARAM_CASE_MANAGER = "caseManager";
    private static final String PARAM_CASE_MANAGER_NAME = "caseManagerName";
    private static final String PARAM_CASE_MANAGER_PHONE = "caseManagerPhone";
    private static final String PARAM_CASE_MANAGER_ROLE = "caseManagerRole";
    private static final String PARAM_CASE_MANAGER_WORKPHONE = "workPhone";
    private static final String PARAM_CONSENT_LINE1 = "CONSENT_LINE1";
    private static final String PARAM_CONSENT_LINE2 = "CONSENT_LINE2";
    private static final String PARAM_DATE_CONVERTER = "dateAsStringConverter";
    private static final String PARAM_FORM_DATE = "FORM_DATE";
    private static final String PARAM_IEP_DATA = "iepData";
    private static final String PARAM_IS_REDETERMINE_ELIG = "isRedetermineEligibility";
    private static final String PARAM_LETTER_RECP = "letterRecp";
    private static final String PARAM_MEETING = "meeting";
    private static final String PARAM_MEETING_DICTIONARY = "meetingDictionary";
    private static final String PARAM_NOTICE_DETAILS = "NOTICE_DETAILS";
    private static final String PARAM_OTHER = "other";
    private static final String PARAM_PROC_SAFE_REPORT = "procSafeReport";
    private static final String PARAM_PROC_SAFE_SUBREPORT_ID = "procSafeSubreportId";
    private static final String PARAM_REASONS = "reasons";
    private static final String PARAM_RETURNLINE = "returnline";
    private static final String PARAM_RIOP1 = "riop1";
    private static final String PARAM_RIOP2 = "riop2";
    private static final String PARAM_RIOP3 = "riop3";
    private static final String PARAM_SCHOOL_ADDRESS_LINE1 = "SKL_ADDRESS_LINE1";
    private static final String PARAM_SCHOOL_ADDRESS_LINE2 = "SKL_ADDRESS_LINE2";
    private static final String PARAM_SCHOOL_NAME = "SCHOOL_NAME";
    private static final String PARAM_SCHOOL_PHONE_NO = "SKL_PHONE_NO";
    private static final String PARAM_SELECTION = "selection";
    private static final String PARAM_SKL_ADMIN1 = "SKL_ADMIN1";
    private static final String PARAM_SKL_ADMIN2 = "SKL_ADMIN2";
    private static final String PARAM_SKL_ADMIN3 = "SKL_ADMIN3";
    private static final String PARAM_SSS_STAFF = "sssStaff";

    private static final String PHONE_PREFIX = "(908) 284-";
    private static final String REDETERMINE_ELIG_FORM_ID = "SPED-NJ-REDETERMINE";

    /**
     * Other Constants
     */
    private static final String STRING_1 = "1";
    private static final String STRING_COMMA = ",";
    private static final String STRING_EMPTY = "";
    private static final String STRING_HYPHEN = "-";
    private static final String STRING_SPACE = " ";
    private static final String STRING_X = "X";

    private final SimpleDateFormat DATE_FORMATTER_SHORT = new SimpleDateFormat("MM/dd/yyyy");
    private final SimpleDateFormat DATE_FORMATTER_LONG = new SimpleDateFormat("MMMMM dd, yyyy");

    /**
     * Member Variables
     */
    private IepData m_currentIep;
    private GenericFormData m_meeting;
    private DataDictionary m_meetingDictionary;
    private String m_sssStaffName;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        String letterRecp = LETTER_RECIPIENT;
        DataDictionary dictionary = getDictionary();
        GenericFormData genericFormData = (GenericFormData) getFormStorage();
        if ((String) (genericFormData.getFieldValueByAlias(ALIAS_TRIENNIAL_RECIPIENT, dictionary)) != null) {
            letterRecp = (String) (genericFormData.getFieldValueByAlias(ALIAS_TRIENNIAL_RECIPIENT, dictionary));
        }

        addParameter(PARAM_LETTER_RECP, letterRecp);

        ReportDataGrid grid = new ReportDataGrid();

        SisStudent student = m_currentIep.getStudent();
        SisPerson studentPerson = student.getPerson();

        SisStaff caseManagerStaff = m_currentIep.getStaff();


        loadReportHeader();

        // this allows blank form to be produced if requested
        Collection emptyReportCollection = new ArrayList<X2BaseBean>();
        emptyReportCollection.add(getOrganization());

        if (Boolean.FALSE.equals(getParameter(PARAM_BLANK))) {
            addParameter(PARAM_IEP_DATA, m_currentIep);
            addParameter(PARAM_MEETING, m_meeting);
            addParameter(PARAM_MEETING_DICTIONARY, m_meetingDictionary);

            if (caseManagerStaff != null && caseManagerStaff.getPerson() != null) {
                String caseManagerName = caseManagerStaff.getPerson().getFirstName() + STRING_SPACE
                        + caseManagerStaff.getPerson().getLastName();
                addParameter(PARAM_CASE_MANAGER_NAME, caseManagerName);

                String caseManagerRole = caseManagerStaff.getSpedRole();
                addParameter(PARAM_CASE_MANAGER_ROLE, caseManagerRole);
            }

            SisAddress studentMailingAddress = studentPerson.getMailingAddress();
            if (studentMailingAddress == null) {
                studentMailingAddress = studentPerson.getPhysicalAddress();
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
                grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(emptyReportCollection));

                addMeetingData(grid);

                addNotificationRecipient(grid, studentPerson, studentMailingAddress);
            } else // MEMBER_ROLE_PARENT
            {
                // One single copy to the Student's mailing address
                grid.append();
                grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(emptyReportCollection));

                addMeetingData(grid);

                // For this report the recipients name will not be used.
                addNotificationRecipient(grid, studentPerson, studentMailingAddress);

                prevMailingAddresses.add(studentMailingAddressView);

                // Additional single copies to any other contacts that don't LiveWith the student
                for (StudentContact studentContact : studentContacts) {
                    if (studentContact.getGradeMailingIndicator()) {
                        Contact contact = studentContact.getContact();
                        SisPerson contactPerson = (SisPerson) contact.getPerson();

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
                                    grid.set(FIELD_PROC_SAFE_DATA,
                                            new JRBeanCollectionDataSource(emptyReportCollection));

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

        Report evalSubreport = ReportUtils.getReport((String) getParameter(PARAM_PROC_SAFE_SUBREPORT_ID), getBroker());
        addParameter(PARAM_PROC_SAFE_REPORT, new ByteArrayInputStream(evalSubreport.getCompiledFormat()));

        addParameter(PARAM_IS_REDETERMINE_ELIG,
                getFormDefinition().getId().equals(REDETERMINE_ELIG_FORM_ID) ? Boolean.TRUE : Boolean.FALSE);

        String formDateStr = (String) (genericFormData.getFieldValueByAlias(ALIAS_TRIENNIAL_LETTER_DATE, dictionary));
        String formatterFormDateStr = STRING_EMPTY;
        PlainDate formDate = convertDate(formDateStr);
        if (formDate != null) {
            formatterFormDateStr = DATE_FORMATTER_LONG.format(formDate);
        }
        addParameter(PARAM_FORM_DATE, formatterFormDateStr);

        // if there are no recipients, use an empty grid to print a blank report
        if (grid.rowCount() == 0) {
            grid.append();
            grid.set(FIELD_BLANK_TYPE, Boolean.FALSE);
            grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(emptyReportCollection));
            grid.append();
            grid.set(FIELD_BLANK_TYPE, Boolean.TRUE);
            grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(emptyReportCollection));
        }

        String caseManagerWorkPhone = (String) caseManagerStaff.getFieldValueByAlias(ALIAS_WORK_PHONE);
        addParameter(PARAM_CASE_MANAGER_PHONE, PHONE_PREFIX + caseManagerWorkPhone);

        grid.set(PARAM_CASE_MANAGER, caseManagerStaff);
        grid.set(PARAM_CASE_MANAGER_WORKPHONE, PHONE_PREFIX + caseManagerWorkPhone);

        String studentFirstName = studentPerson.getFirstName();
        String selection = (String) (genericFormData.getFieldValueByAlias(ALIAS_TRIENNIAL_AGAIN, dictionary));

        if (STRING_1.equals(selection)) {
            addParameter(PARAM_SELECTION, "If you consent to waive this triennial reevaluation, " + studentFirstName
                    + " continues to be eligible "
                    + "for special education and related services and " + studentFirstName + CONSTANT_POSSESSIVE_SUFFIX
                    + " next triennial will be due three years from "
                    + "the date you provide consent to the district.");
        } else {
            addParameter(PARAM_SELECTION, "If you consent to waive this triennial reevaluation, " + studentFirstName
                    + " continues to be eligible "
                    + "for special education and related services but no other triennial reevaluation is anticipated, as "
                    + studentFirstName
                    + " will be graduating/exiting the school system before another three years have passed.");
        }


        String riop1 = (String) (genericFormData.getFieldValueByAlias(ALIAS_RI_OP1, dictionary));
        String riop2 = (String) (genericFormData.getFieldValueByAlias(ALIAS_RI_OP2, dictionary));
        String riop3 = (String) (genericFormData.getFieldValueByAlias(ALIAS_RI_OP3, dictionary));

        if (STRING_1.equals(riop1)) {
            addParameter(PARAM_RIOP1, STRING_X);
        } else {
            addParameter(PARAM_RIOP1, STRING_EMPTY);
        }

        if (STRING_1.equals(riop2)) {
            addParameter(PARAM_RIOP2, STRING_X);
        } else {
            addParameter(PARAM_RIOP2, STRING_EMPTY);
        }

        if (STRING_1.equals(riop3)) {
            addParameter(PARAM_RIOP3, STRING_X);
        } else {
            addParameter(PARAM_RIOP3, STRING_EMPTY);
        }

        String dueDateStr = (String) (genericFormData.getFieldValueByAlias(ALIAS_TRIENNIAL_DUE_DATE, dictionary));
        String formattedDueDateStr = STRING_EMPTY;
        PlainDate formDueDate = convertDate(dueDateStr);
        if (formDueDate != null) {
            formattedDueDateStr = DATE_FORMATTER_SHORT.format(formDueDate);
        }

        String Statement = STRING_EMPTY;
        if (studentFirstName != null) {
            Statement = "A triennial (three year) reevaluation of " + studentFirstName +
                    " which includes a reevaluation and re-determination of eligibility would need to be completed by "
                    + formattedDueDateStr + ". " +
                    "However, in accordance with N.J.A.C. 6A:14-3.8(e), the district is proposing to waive the triennial reevaluation"
                    +
                    " of " + studentFirstName
                    + " at this time and is requesting your consent to do so.  The district reviewed the following relevant information:";
        }
        addParameter(PARAM_NOTICE_DETAILS, Statement);

        String returnDateStr =
                (String) (genericFormData.getFieldValueByAlias(ALIAS_TRIENNIAL_LETTER_RETURNDATE, dictionary));
        String formatterReturnDate = STRING_EMPTY;
        PlainDate returnDate = convertDate(returnDateStr);
        if (returnDate != null) {
            formatterReturnDate = DATE_FORMATTER_SHORT.format(returnDate);
        }

        String consentMessage1 =
                "I consent to waive the current triennial reevaluation of " + studentFirstName + " that is due on ";
        String consentMessage2 = "I do not consent to waive the current triennial reevaluation of " + studentFirstName
                + " that is due on ";

        if (!StringUtils.isEmpty(formattedDueDateStr)) {
            addParameter(PARAM_CONSENT_LINE1, consentMessage1 + formattedDueDateStr);
            addParameter(PARAM_CONSENT_LINE2, consentMessage2 + formattedDueDateStr);
            addParameter(PARAM_RETURNLINE, "Please return this signed form by " + formatterReturnDate + " to:");
        } else {
            addParameter(PARAM_CONSENT_LINE1, consentMessage1);
            addParameter(PARAM_CONSENT_LINE2, consentMessage2);
            addParameter(PARAM_RETURNLINE, "Please return this signed form to:");
        }

        String reasonsWaiving =
                (String) (genericFormData.getFieldValueByAlias(ALIAS_TRIENNIAL_REASONS_WAIVING, dictionary));
        addParameter(PARAM_REASONS, reasonsWaiving);

        String otherOptions =
                (String) (genericFormData.getFieldValueByAlias(ALIAS_TRIENNIAL_OTHER_OPTIONS, dictionary));
        addParameter(PARAM_OTHER, otherOptions);

        grid.beforeTop();

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
        SisStudent student = m_currentIep.getStudent();

        if (student != null && student.getPerson() != null) {
            String studentName = getFullName(student.getPerson());

            grid.set(FIELD_STUDENT_NAME, studentName);
        }
    }

    /**
     * Add meeting invitation recipient data to grid for display on report.
     *
     * @param grid ReportDataGrid
     * @param person SisPerson
     * @param address Address
     */
    private void addNotificationRecipient(ReportDataGrid grid, SisPerson person, Address address) {
        if (person != null) {
            String recipientName = getFullName(person);
            grid.set(FIELD_RECIPIENT_NAME, recipientName);

            grid.set(FIELD_RECIPIENT_ADDRESS_01, null);
            grid.set(FIELD_RECIPIENT_CITY, null);
            grid.set(FIELD_RECIPIENT_STATE, null);
            grid.set(FIELD_RECIPIENT_ZIP, null);

            if (address != null) {
                grid.set(FIELD_RECIPIENT_ADDRESS_01, address.getAddressLine01());
                grid.set(FIELD_RECIPIENT_CITY, address.getCity());
                grid.set(FIELD_RECIPIENT_STATE, address.getState());
                grid.set(FIELD_RECIPIENT_ZIP, address.getPostalCode());
            }
        }
    }

    /**
     * Get a PlainDate object from a date string with a format of yyyy-mm-dd.
     *
     * @param dateStr String
     * @return PlainDate
     */
    private PlainDate convertDate(String dateStr) {
        PlainDate plainDate = null;

        if (!StringUtils.isEmpty(dateStr)) {
            if (dateStr.contains(STRING_HYPHEN)) {
                plainDate = DateUtils.getDate(dateStr);
            }
        }

        return plainDate;
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

        if (school.getAdministrator1() != null) {
            SisPerson adminPerson1 = school.getAdministrator1().getPerson();
            admin1 = adminPerson1.getFirstName() + STRING_SPACE + adminPerson1.getLastName();
            addParameter(PARAM_SKL_ADMIN1, admin1);

            String superintendent = (String) school.getAdministrator1().getFieldValueByAlias(ALIAS_DEPT_SUPERVISOR);
            if (superintendent != null && superintendent.contains(STRING_COMMA)) {
                String[] admin2 = superintendent.split(STRING_COMMA);
                addParameter(PARAM_SKL_ADMIN2, admin2[1] + STRING_SPACE + admin2[0]);
            }
        }
        initSssName();
        addParameter(PARAM_SSS_STAFF, m_sssStaffName);
    }
}
