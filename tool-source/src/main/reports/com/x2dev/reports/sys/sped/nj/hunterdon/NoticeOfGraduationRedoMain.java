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
import java.sql.Date;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;
import net.sf.jasperreports3.engine.data.JRBeanCollectionDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class NoticeOfGraduationRedoMain.
 */
public class NoticeOfGraduationRedoMain extends BaseFormReportJavaSource {
    // Aliases
    private static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    private static final String ALIAS_STAFF_ROLE = "DOE STAFF ROLE";
    private static final String ALIAS_WORK_PHONE = "DOE STAFF WORK PHONE";

    // Report Constants
    private static final String EXTN_WORK_PHONE = "(908) 284-";

    // Report fields
    private static final String FIELD_BLANK_TYPE = "blankType";
    private static final String FIELD_PROC_SAFE_DATA = "procSafeData";
    private static final String FIELD_RECIPIENT_ADDRESS_01 = "recipientAddress01";
    private static final String FIELD_RECIPIENT_CITY = "recipientCity";
    private static final String FIELD_RECIPIENT_NAME = "recipientName";
    private static final String FIELD_RECIPIENT_STATE = "recipientState";
    private static final String FIELD_RECIPIENT_ZIP = "recipientZip";
    private static final String FIELD_STUDENT_EIGHTEENTH = "studentEighteenth";
    private static final String FIELD_STUDENT_NAME = "studentName";

    private static final String PARAM_CASE_MANAGER = "caseManager";
    private static final String PARAM_CASE_MANAGER_NAME = "caseManagerName";
    private static final String PARAM_CASE_MANAGER_PHONE = "caseManagerPhone";
    private static final String PARAM_CASE_MANAGER_ROLE = "caseManagerRole";
    private static final String PARAM_CASE_MANAGER_WORKPHONE = "workPhone";
    private static final String PARAM_DATE_CONVERTER = "dateAsStringConverter";
    private static final String PARAM_DETAILS = "details";
    private static final String PARAM_IEP_DATA = "iepData";
    private static final String PARAM_LETTER_DATE = "letterDate";
    private static final String PARAM_MEETING = "meeting";
    private static final String PARAM_MEETING_DATE = "meetingDate";
    private static final String PARAM_MEETING_DICTIONARY = "meetingDictionary";
    private static final String PARAM_PROC_SAFE_REPORT = "procSafeReport";
    private static final String PARAM_PROC_SAFE_SUBREPORT_ID = "procSafeSubreportId";
    private static final String PARAM_SCHOOL_ADDRESS_LINE1 = "SKL_ADDRESS_LINE1";
    private static final String PARAM_SCHOOL_ADDRESS_LINE2 = "SKL_ADDRESS_LINE2";
    private static final String PARAM_SCHOOL_NAME = "SCHOOL_NAME";
    private static final String PARAM_SCHOOL_PHONE_NO = "SKL_PHONE_NO";
    private static final String PARAM_SCHOOL_YEAR = "SchoolYear";
    private static final String PARAM_SKL_ADMIN1 = "SKL_ADMIN1";
    private static final String PARAM_SKL_ADMIN2 = "SKL_ADMIN2";
    private static final String PARAM_SKL_ADMIN3 = "SKL_ADMIN3";
    private static final String PARAM_SSS_STAFF = "sssStaff";
    private static final String PARAM_STUDENT = "student";
    private static final String PARAM_STUDENT_NAME = "studentsName";
    private static final String PARAM_STUDENT_PERSON = "StudentPerson";

    // Constants
    private static final String STRING_COMMA = ",";
    private static final String STRING_EMPTY = "";
    private static final String STRING_SPACE = " ";

    private final SimpleDateFormat DATE_FORMATTER_SHORT = new SimpleDateFormat("MM/dd/yyyy");
    private final SimpleDateFormat DATE_FORMATTER_LONG = new SimpleDateFormat("MMMMM dd, yyyy");

    // Member variables
    private IepData m_currentIep;
    private GenericFormData m_meeting;
    private DataDictionary m_meetingDictionary;
    private String m_sssStaffName;


    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        IepData iepData = (IepData) getFormOwner();
        SisStudent student = iepData.getStudent();
        Person studentPerson = student.getPerson();
        boolean isStudentEighteen = studentPerson.getAge() >= 18;

        addParameter(PARAM_STUDENT_PERSON, student);
        addParameter(PARAM_SCHOOL_YEAR, getCurrentContext().getContextId());

        SisStaff caseManagerStaff = m_currentIep.getStaff();

        ReportDataGrid grid = new ReportDataGrid();

        loadReportHeader();

        Address studentMailingAddress = student.getPerson().getMailingAddress();
        if (studentMailingAddress == null) {
            studentMailingAddress = student.getPerson().getPhysicalAddress();
        }

        // this allows blank form to be produced if requested
        Collection<X2BaseBean> emptyReportGrid = new ArrayList<X2BaseBean>();
        emptyReportGrid.add(getOrganization());

        if (getParameter(PARAM_BLANK).equals(Boolean.FALSE)) {
            addParameter(PARAM_IEP_DATA, m_currentIep);
            addParameter(PARAM_MEETING, m_meeting);
            addParameter(PARAM_MEETING_DICTIONARY, m_meetingDictionary);

            if (caseManagerStaff != null && caseManagerStaff.getPerson() != null) {
                String caseManagerFullName = getFullName(caseManagerStaff.getPerson());
                String caseManagerName = caseManagerStaff.getPerson().getFirstName() + STRING_SPACE
                        + caseManagerStaff.getPerson().getLastName();
                String caseManagerRole = caseManagerStaff.getSpedRole();

                addParameter(PARAM_CASE_MANAGER, caseManagerFullName);
                addParameter(PARAM_CASE_MANAGER_NAME, caseManagerName);
                addParameter(PARAM_CASE_MANAGER_ROLE, caseManagerRole);
            }

            String studentMailingAddressView = STRING_EMPTY;
            if (studentMailingAddress.getAddressLine01() != null) {
                studentMailingAddressView = studentMailingAddress.getAddressLine01().trim();
            }

            Collection<StudentContact> studentContacts = student.getContacts();

            Set<String> prevMailingAddresses = new HashSet<String>();
            String contactMailingAddressView = STRING_EMPTY;


            // If the student 18 or greater then just send a copy to the individual
            if (isStudentEighteen) {
                grid.append();
                grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(emptyReportGrid));

                addMeetingData(grid);

                // For this report the recipients name will not be used.
                addNotificationRecipient(grid, studentPerson, studentMailingAddress);
            } else {
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

        GenericFormData formData = (GenericFormData) getFormStorage();
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(getFormDefinition().getExtendedDataDictionary(),
                        getBroker().getPersistenceKey());

        grid.set(PARAM_STUDENT, student);

        grid.set(PARAM_DETAILS, formData.getFieldValueByAlias(PARAM_DETAILS, dictionary));
        addParameter(PARAM_DETAILS, formData.getFieldValueByAlias(PARAM_DETAILS, dictionary));

        String letterDateAsString = (String) formData.getFieldValueByAlias(PARAM_LETTER_DATE, dictionary);
        if (!StringUtils.isEmpty(letterDateAsString)) {
            grid.set(PARAM_LETTER_DATE, DATE_FORMATTER_LONG.format(Date.valueOf(letterDateAsString)));

            addParameter(PARAM_LETTER_DATE, DATE_FORMATTER_LONG.format(Date.valueOf(letterDateAsString)));
        } else {
            grid.set(PARAM_LETTER_DATE, null);

            addParameter(PARAM_LETTER_DATE, STRING_EMPTY);
        }

        String meetingDateAsString = (String) formData.getFieldValueByAlias(PARAM_MEETING_DATE, dictionary);
        if (!StringUtils.isEmpty(meetingDateAsString)) {
            grid.set(PARAM_MEETING_DATE, DATE_FORMATTER_SHORT.format(Date.valueOf(meetingDateAsString)));

            addParameter(PARAM_MEETING_DATE, DATE_FORMATTER_SHORT.format(Date.valueOf(meetingDateAsString)));
        } else {
            grid.set(PARAM_MEETING_DATE, null);

            addParameter(PARAM_MEETING_DATE, STRING_EMPTY);
        }

        String caseManagerWorkPhone = (String) caseManagerStaff.getFieldValueByAlias(ALIAS_WORK_PHONE);
        addParameter(PARAM_CASE_MANAGER_PHONE, EXTN_WORK_PHONE + caseManagerWorkPhone);

        grid.set(PARAM_CASE_MANAGER, caseManagerStaff);
        grid.set(PARAM_CASE_MANAGER_WORKPHONE, EXTN_WORK_PHONE + caseManagerWorkPhone);

        Report evalSubreport = ReportUtils.getReport((String) getParameter(PARAM_PROC_SAFE_SUBREPORT_ID), getBroker());
        addParameter(PARAM_PROC_SAFE_REPORT, new ByteArrayInputStream(evalSubreport.getCompiledFormat()));

        // if there are no recipients, use an empty grid to print a blank report
        if (grid.rowCount() == 0) {
            grid.append();
            grid.set(FIELD_BLANK_TYPE, Boolean.FALSE);
            grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(emptyReportGrid));
            grid.append();
            grid.set(FIELD_BLANK_TYPE, Boolean.TRUE);
            grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(emptyReportGrid));
        }

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

        if (student != null &&
                student.getPerson() != null) {
            String studentName = getFullName(student.getPerson());
            grid.set(FIELD_STUDENT_NAME, studentName);

            addParameter(PARAM_STUDENT_NAME, studentName);
        }

        PlainDate eighteenthBirthDate = new PlainDate();
        if (student != null) {
            Person person = student.getPerson();
            PlainDate birthDate = person.getDob();
            eighteenthBirthDate = DateUtils.add(birthDate, Calendar.YEAR, 18);
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
            if (superintendent != null) {
                String[] admin2 = superintendent.split(STRING_COMMA);
                addParameter(PARAM_SKL_ADMIN2, admin2[1] + STRING_SPACE + admin2[0]);
            }
        }
        initSssName();
        addParameter(PARAM_SSS_STAFF, m_sssStaffName);
    }

}
