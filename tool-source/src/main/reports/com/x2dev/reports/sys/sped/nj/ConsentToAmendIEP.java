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
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepTeamMember;
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
import java.util.Locale;
import net.sf.jasperreports3.engine.data.JRBeanCollectionDataSource;

/**
 * The Class ConsentToAmendIEP.
 */
public class ConsentToAmendIEP extends BaseFormReportJavaSource {
    private Date m_responseDate = new Date();

    // Aliases
    private static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    private static final String ALIAS_SCHOOL_FAX = "DOE SCHOOL FAX";
    private static final String ALIAS_WORK_PHONE_EXTENTION = "DOE STAFF WORK PHONE";

    /// IEP_DATA (Extended)
    private static final String ALIAS_OTHER_OPTIONS_CONSIDERED = "sped-other-options-considered";
    private static final String ALIAS_PROPOSED_CHANGES = "sped-proposed-changes";
    private static final String ALIAS_RECIPIENT = "sped-Recipient";
    private static final String ALIAS_USED_PROC_FACTORS = "sped-used-procedure-factors";

    // Report fields
    private static final String REPORT_FIELD_BLANK_TYPE = "blankType";
    private static final String REPORT_FIELD_CASE_MANAGER = "caseManager";
    private static final String REPORT_FIELD_CASE_MANAGER_WORK_PHONE = "workPhone";
    private static final String REPORT_FIELD_PROC_SAFE_DATA = "procSafeData";
    private static final String REPORT_FIELD_RECIPIENT_NAME = "recipientName";
    private static final String REPORT_FIELD_RECIPIENT_ADDRESS_01 = "recipientAddress01";
    private static final String REPORT_FIELD_RECIPIENT_CITY = "recipientCity";
    private static final String REPORT_FIELD_RECIPIENT_STATE = "recipientState";
    private static final String REPORT_FIELD_RECIPIENT_ZIP = "recipientZip";
    private static final String REPORT_FIELD_STUDENT_NAME = "studentName";
    private static final String REPORT_FIELD_STUDENT_EIGHTEENTH = "studentEighteenth";
    private static final String REPORT_FIELD_TEAM_MEMBER = "teamMember";

    // Report parameters
    private static final String REPORT_PARAM_CASE_MANAGER = "caseManager";
    private static final String REPORT_PARAM_CASE_MANAGER_NAME = "caseManagerName";
    private static final String REPORT_PARAM_CASE_MANAGER_PHONE = "caseManagerPhone";
    private static final String REPORT_PARAM_CASE_MANAGER_ROLE = "caseManagerRole";
    private static final String REPORT_PARAM_CHAIR_PERSON = "CHAIR_PERSON";
    private static final String REPORT_PARAM_CHAIR_PERSON_ROLE = "chairPersonRole";
    private static final String REPORT_PARAM_CHAIR_PERSON_SCHOOL_ADDRESS_LINE1 = "chairPersonAddressLine1";
    private static final String REPORT_PARAM_CHAIR_PERSON_SCHOOL_ADDRESS_LINE2 = "chairPersonAddressLine2";
    private static final String REPORT_PARAM_CHAIR_PERSON_SCHOOL_NAME = "chairPersonSchoolName";
    private static final String REPORT_PARAM_CHAIR_PERSON_WORKPHONE = "chairPersonWorkPhone";
    private static final String REPORT_PARAM_DATE_STRING_CONVERTER = "dateAsStringConverter";
    private static final String REPORT_PARAM_FORM_DATE = "FORM_DATE";
    private static final String REPORT_PARAM_IEP_DATA = "iepData";
    private static final String REPORT_PARAM_IS_REDETERMINE_ELIG = "isRedetermineEligibility";
    private static final String REPORT_PARAM_LETTER_RECIPIENT = "letterRecp";
    private static final String REPORT_PARAM_MEETING = "meeting";
    private static final String REPORT_PARAM_MEETING_DICTIONARY = "meetingDictionary";
    private static final String REPORT_PARAM_OTHER_OPTIONS_CONSIDERED = "otherOptionsConsidered";
    private static final String REPORT_PARAM_PROC_SAFE_REPORT = "procSafeReport";
    private static final String REPORT_PARAM_PROPOSED_CHANGES = "proposedChanges";
    private static final String REPORT_PARAM_USED_PROC_FACTORS = "usedProcedureFactors";
    private static final String REPORT_PARAM_SCHOOL_NAME = "SCHOOL_NAME";
    private static final String REPORT_PARAM_SCHOOL_ADDRESS_LINE1 = "SKL_ADDRESS_LINE1";
    private static final String REPORT_PARAM_SCHOOL_ADDRESS_LINE2 = "SKL_ADDRESS_LINE2";
    private static final String REPORT_PARAM_SCHOOL_PHONE_NUMBER = "SKL_PHONE_NO";
    private static final String REPORT_PARAM_SCHOOL_FAX_NUMBER = "SKL_FAX_NO";
    private static final String REPORT_PARAM_SKL_ADMIN1 = "SKL_ADMIN1";
    private static final String REPORT_PARAM_SKL_ADMIN2 = "SKL_ADMIN2";
    private static final String REPORT_PARAM_SKL_ADMIN3 = "SKL_ADMIN3";
    private static final String REPORT_PARAM_STUDENT_FIRST_NAME = "studentFirstName";

    private static final String PARAM_FORM_REDETERMINE_ID = "SPED-NJ-REDETERMINE";
    private static final String PARAM_PROC_SAFE_SUBREPORT_ID = "procSafeSubreportId";

    private static final String REPORT_PARAM_NOTICE_DETAILS = "noticeDetails";
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

    private static final String REPORT_ERROR_NO_GUARDIAN = "*** No Guardian set for this Student's IEP ***";

    // Report Header Parameters
    private static final String PARAM_LETTER_RECIPIENT_PARENT = "Parent";
    private static final String PARAM_LETTER_RECIPIENT_STUDENT = "Student";

    // Constants
    private static final String FAX = "FAX";
    private static final String REPORT_PARAMETER_MAP = "parameters";
    private static final String PHONE_PREFIX = "(908) 284-";

    private static final String STRING_EMPTY = "";
    private static final String STRING_SPACE = " ";
    private static final String STRING_COMMA = ",";

    private static final String MEMBER_ROLE_PARENT_GUARDIAN = "Parent/Guardian";

    private final SimpleDateFormat DATE_FORMATTER = new SimpleDateFormat("MM/dd/yyyy");
    private final SimpleDateFormat DATE_FORMATTER_LONG = new SimpleDateFormat("MMMMM dd, yyyy");

    // Member variables
    private IepData m_currentIep;
    private GenericFormData m_meeting;
    private DataDictionary m_MeetingDictionary;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        SisStudent student = m_currentIep.getStudent();

        String letterRecp = PARAM_LETTER_RECIPIENT_PARENT;
        if ((String) (((GenericFormData) getFormStorage()).getFieldValueByAlias(ALIAS_RECIPIENT,
                getDictionary())) != null) {
            letterRecp = (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias(ALIAS_RECIPIENT,
                    getDictionary()));
        }

        addParameter(REPORT_PARAM_LETTER_RECIPIENT, letterRecp);

        addParameter(REPORT_PARAM_PROPOSED_CHANGES, STRING_EMPTY);
        String proposedChanges = (String) (((GenericFormData) getFormStorage())
                .getFieldValueByAlias(ALIAS_PROPOSED_CHANGES, getDictionary()));
        if (proposedChanges != null) {
            addParameter(REPORT_PARAM_PROPOSED_CHANGES, proposedChanges);
        }

        addParameter(REPORT_PARAM_OTHER_OPTIONS_CONSIDERED, STRING_EMPTY);
        String otherOptionsConsidered = (String) (((GenericFormData) getFormStorage())
                .getFieldValueByAlias(ALIAS_OTHER_OPTIONS_CONSIDERED, getDictionary()));
        if (otherOptionsConsidered != null) {
            addParameter(REPORT_PARAM_OTHER_OPTIONS_CONSIDERED, otherOptionsConsidered);
        }

        addParameter(REPORT_PARAM_USED_PROC_FACTORS, STRING_EMPTY);
        String usedProcedureFactors = (String) (((GenericFormData) getFormStorage())
                .getFieldValueByAlias(ALIAS_USED_PROC_FACTORS, getDictionary()));
        if (usedProcedureFactors != null) {
            addParameter(REPORT_PARAM_USED_PROC_FACTORS, usedProcedureFactors);
        }

        setReportChairPersonDetails();

        FormInstance formInstance = getFormInstance();
        Calendar calendar = Calendar.getInstance();
        Date formDate = new Date();
        if (formInstance != null) {
            formDate = new Date(formInstance.getCreatedTime());
        }

        calendar.setTime(formDate);
        calendar.add(Calendar.DATE, 15);
        m_responseDate = calendar.getTime();
        addParameter(REPORT_PARAM_FORM_DATE, DATE_FORMATTER_LONG.format(formDate));

        String prevParentAddress = STRING_EMPTY;
        String ParentAddress = STRING_EMPTY;

        ReportDataGrid grid = new ReportDataGrid();
        HashMap<String, Object> parameterMap = new HashMap<String, Object>();

        setReportHeader(student);

        // this allows blank form to be produced if requested
        Collection dummy = new ArrayList<X2BaseBean>();
        dummy.add(getOrganization());

        if (getParameter(PARAM_BLANK).equals(Boolean.FALSE)) {
            addParameter(REPORT_PARAM_IEP_DATA, m_currentIep);
            addParameter(REPORT_PARAM_MEETING, m_meeting);
            addParameter(REPORT_PARAM_MEETING_DICTIONARY, m_MeetingDictionary);

            SisStaff staff = m_currentIep.getStaff();

            addParameter(REPORT_PARAM_CASE_MANAGER, STRING_EMPTY);
            addParameter(REPORT_PARAM_CASE_MANAGER_NAME, STRING_EMPTY);
            addParameter(REPORT_PARAM_CASE_MANAGER_ROLE, STRING_EMPTY);

            if (staff != null) {
                SisPerson person = staff.getPerson();

                if (person != null) {
                    String caseManager = getFullName(person);
                    String caseManagerName = person.getFirstName() + STRING_SPACE + person.getLastName();
                    String caseManagerRole = staff.getSpedRole();

                    addParameter(REPORT_PARAM_CASE_MANAGER, caseManager);
                    addParameter(REPORT_PARAM_CASE_MANAGER_NAME, caseManagerName);
                    addParameter(REPORT_PARAM_CASE_MANAGER_ROLE, caseManagerRole);
                }
            }

            Collection<IepTeamMember> teamMembers = m_currentIep.getTeamMembers();

            // print a notification for each parent
            // the report is completed in the detail band
            boolean hasGuardian = false;
            for (IepTeamMember teamMember : teamMembers) {
                if (teamMember.getMemberRoleCode().equals(MEMBER_ROLE_PARENT_GUARDIAN)) {
                    teamMember.getPerson().getContact().getStudentContacts();

                    Collection<StudentContact> studentContacts =
                            teamMember.getPerson().getContact().getStudentContacts();

                    for (StudentContact studentContact : studentContacts) {
                        if (letterRecp.equals(PARAM_LETTER_RECIPIENT_STUDENT)) {
                            if (studentContact.getLivesWithIndicator()) {
                                grid.append();
                                grid.set(REPORT_FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(dummy));

                                addReportMeetingData(grid);
                                addReportNotificationRecipient(grid, teamMember);
                                hasGuardian = true;
                                prevParentAddress = studentContact.getContact().getAddressView();
                            }
                        } else {
                            if (studentContact.getStudentOid().equals(m_currentIep.getStudentOid())) {
                                if (studentContact.getGradeMailingIndicator()) {
                                    prevParentAddress = prevParentAddress.trim();
                                    ParentAddress = studentContact.getContact().getAddressView();
                                    ParentAddress = ParentAddress.trim();

                                    if (ParentAddress.equals(prevParentAddress)) {
                                        // Not printing Parent Letter because a previous one was
                                        // printed with a matching address
                                    } else {
                                        grid.append();
                                        grid.set(REPORT_FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(dummy));

                                        addReportMeetingData(grid);
                                        addReportNotificationRecipient(grid, teamMember);
                                        hasGuardian = true;
                                        prevParentAddress = studentContact.getContact().getAddressView();
                                    }
                                }
                            }
                        }
                    }
                }
            }

            if (!hasGuardian) {
                grid.append();
                grid.set(REPORT_FIELD_RECIPIENT_NAME, REPORT_ERROR_NO_GUARDIAN);
                grid.set(REPORT_FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(dummy));

                addReportMeetingData(grid);
            }
        }

        String safeSubreportId = (String) getParameter(PARAM_PROC_SAFE_SUBREPORT_ID);
        if (safeSubreportId != null) {
            Report evalSubreport = ReportUtils.getReport(safeSubreportId, getBroker());

            addParameter(REPORT_PARAM_PROC_SAFE_REPORT, new ByteArrayInputStream(evalSubreport.getCompiledFormat()));

            addParameter(REPORT_PARAM_IS_REDETERMINE_ELIG,
                    getFormDefinition().getId().equals(PARAM_FORM_REDETERMINE_ID) ? Boolean.TRUE : Boolean.FALSE);
        }

        // if there are no recipients, use an empty grid to print a blank report
        if (grid.rowCount() == 0) {
            grid.append();
            grid.set(REPORT_FIELD_BLANK_TYPE, Boolean.FALSE);
            grid.set(REPORT_FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(dummy));

            grid.append();
            grid.set(REPORT_FIELD_BLANK_TYPE, Boolean.TRUE);
            grid.set(REPORT_FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(dummy));
        }
        IepData iepData = (IepData) getFormOwner();

        if (iepData != null) {
            addParameter(REPORT_PARAM_CASE_MANAGER_PHONE, STRING_EMPTY);

            SisStaff staff = iepData.getStaff();
            if (staff != null) {
                String caseManagerPhone = (String) staff.getFieldValueByAlias(ALIAS_WORK_PHONE_EXTENTION);
                if (caseManagerPhone != null) {
                    addParameter(REPORT_PARAM_CASE_MANAGER_PHONE, PHONE_PREFIX + caseManagerPhone);

                    grid.set(REPORT_FIELD_CASE_MANAGER, staff);
                    grid.set(REPORT_FIELD_CASE_MANAGER_WORK_PHONE, PHONE_PREFIX + caseManagerPhone);
                }
            }
        }
        grid.set(REPORT_PARAMETER_MAP, parameterMap);
        grid.beforeTop();

        String studentName = getFullName(student.getPerson());
        String studentFirstName = student.getPerson().getFirstName();

        addParameter(REPORT_PARAM_STUDENT_FIRST_NAME, studentFirstName);
        addParameter(REPORT_PARAM_NOTICE_DETAILS, NOTICE_DETAILS_PART1 + studentName + NOTICE_DETAILS_PART2
                + DATE_FORMATTER.format(m_responseDate) + NOTICE_DETAILS_PART3);

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
        m_MeetingDictionary = DataDictionary.getDistrictDictionary(extendDictionary, getBroker().getPersistenceKey());

        addParameter(REPORT_PARAM_DATE_STRING_CONVERTER,
                ConverterFactory.getConverterForClass(PlainDate.class.getName(), Locale.getDefault(), true));
    }

    /**
     * Add meeting data to grid for display on report.
     *
     * @param grid ReportDataGrid
     */
    private void addReportMeetingData(ReportDataGrid grid) {
        grid.set(REPORT_FIELD_STUDENT_NAME, STRING_EMPTY);
        grid.set(REPORT_FIELD_STUDENT_EIGHTEENTH, STRING_EMPTY);

        SisStudent student = m_currentIep.getStudent();
        if (student != null) {
            SisPerson person = student.getPerson();
            if (person != null) {
                String studentName = getFullName(person);
                grid.set(REPORT_FIELD_STUDENT_NAME, studentName);

                PlainDate eighteenthBirthDate = new PlainDate();
                PlainDate birthDate = person.getDob();
                eighteenthBirthDate = DateUtils.add(birthDate, Calendar.YEAR, 18);

                grid.set(REPORT_FIELD_STUDENT_EIGHTEENTH, eighteenthBirthDate);
            }
        }
    }

    /**
     * Add meeting invitation recipient data to grid for display on report.
     *
     * @param grid ReportDataGrid
     * @param teamMember IepTeamMember
     */
    private void addReportNotificationRecipient(ReportDataGrid grid, IepTeamMember teamMember) {
        grid.set(REPORT_FIELD_RECIPIENT_NAME, STRING_EMPTY);
        grid.set(REPORT_FIELD_RECIPIENT_ADDRESS_01, STRING_EMPTY);
        grid.set(REPORT_FIELD_RECIPIENT_CITY, STRING_EMPTY);
        grid.set(REPORT_FIELD_RECIPIENT_STATE, STRING_EMPTY);
        grid.set(REPORT_FIELD_RECIPIENT_ZIP, STRING_EMPTY);

        if (teamMember != null) {
            grid.set(REPORT_FIELD_TEAM_MEMBER, teamMember);

            SisPerson person = teamMember.getPerson();
            if (person != null) {
                SisAddress address = person.getPhysicalAddress();

                String recipientName = getFullName(person);

                grid.set(REPORT_FIELD_RECIPIENT_NAME, recipientName);

                if (address != null) {
                    if (address.getAddressLine01() != null) {
                        grid.set(REPORT_FIELD_RECIPIENT_ADDRESS_01, address.getAddressLine01());
                    }
                    if (address.getCity() != null) {
                        grid.set(REPORT_FIELD_RECIPIENT_CITY, address.getCity());
                    }
                    if (address.getState() != null) {
                        grid.set(REPORT_FIELD_RECIPIENT_STATE, address.getState());
                    }
                    if (address.getPostalCode() != null) {
                        grid.set(REPORT_FIELD_RECIPIENT_ZIP, address.getPostalCode());
                    }
                }
            }
        }
    }

    /**
     * Get full name from a person bean.
     *
     * @param person SisPerson
     * @return fullName
     */
    private String getFullName(SisPerson person) {
        String fullName = person.getFirstName();

        if (!StringUtils.isEmpty(fullName)) {
            fullName = fullName + STRING_SPACE;
        }

        fullName = fullName + person.getLastName();

        return fullName;
    }

    /**
     * This method sets the chair person's details/address for the signature line of the SPED form.
     * For this form,
     * the case manager is the chair person.
     */
    private void setReportChairPersonDetails() {
        addParameter(REPORT_PARAM_CHAIR_PERSON, STRING_EMPTY);
        addParameter(REPORT_PARAM_CHAIR_PERSON_SCHOOL_NAME, STRING_EMPTY);
        addParameter(REPORT_PARAM_CHAIR_PERSON_SCHOOL_ADDRESS_LINE1, STRING_EMPTY);
        addParameter(REPORT_PARAM_CHAIR_PERSON_SCHOOL_ADDRESS_LINE2, STRING_EMPTY);
        addParameter(REPORT_PARAM_CHAIR_PERSON, STRING_EMPTY);
        addParameter(REPORT_PARAM_CHAIR_PERSON_ROLE, STRING_EMPTY);
        addParameter(REPORT_PARAM_CHAIR_PERSON_WORKPHONE, STRING_EMPTY);

        SisPerson chairPerson = null;

        SisStaff caseManager = m_currentIep.getStaff();
        if (caseManager != null) {
            SisSchool school = caseManager.getSchool();
            if (school != null) {
                addParameter(REPORT_PARAM_CHAIR_PERSON_SCHOOL_NAME, school.getName());

                Address schoolAddress = school.getAddress();
                if (schoolAddress != null) {
                    if (schoolAddress.getAddressLine01() != null) {
                        addParameter(REPORT_PARAM_CHAIR_PERSON_SCHOOL_ADDRESS_LINE1, schoolAddress.getAddressLine01());
                    }
                    if (schoolAddress.getAddressLine03() != null) {
                        addParameter(REPORT_PARAM_CHAIR_PERSON_SCHOOL_ADDRESS_LINE2, schoolAddress.getAddressLine03());
                    }
                }
            }

            chairPerson = caseManager.getPerson();
            if (chairPerson != null) {
                String chairPersonName = chairPerson.getLastName();
                addParameter(REPORT_PARAM_CHAIR_PERSON, StringUtils
                        .coalesce(chairPerson.getFirstName() + STRING_SPACE + chairPersonName, STRING_EMPTY));
                addParameter(REPORT_PARAM_CHAIR_PERSON_ROLE,
                        StringUtils.coalesce(caseManager.getSpedRole(), STRING_EMPTY));

                String chairPersonWorkPhone =
                        (String) chairPerson.getStaff().getFieldValueByAlias(ALIAS_WORK_PHONE_EXTENTION);
                addParameter(REPORT_PARAM_CHAIR_PERSON_WORKPHONE,
                        StringUtils.coalesce(PHONE_PREFIX + chairPersonWorkPhone, STRING_EMPTY));
            }
        }
    }

    /**
     * Load the Student title data of the report.
     *
     * @param student void
     */
    private void setReportHeader(SisStudent student) {
        addParameter(REPORT_PARAM_SCHOOL_NAME, STRING_EMPTY);
        addParameter(REPORT_PARAM_SCHOOL_ADDRESS_LINE1, STRING_EMPTY);
        addParameter(REPORT_PARAM_SCHOOL_ADDRESS_LINE2, STRING_EMPTY);
        addParameter(REPORT_PARAM_SCHOOL_PHONE_NUMBER, STRING_EMPTY);
        addParameter(REPORT_PARAM_SKL_ADMIN1, STRING_EMPTY);
        addParameter(REPORT_PARAM_SKL_ADMIN2, STRING_EMPTY);
        addParameter(REPORT_PARAM_SKL_ADMIN3, STRING_EMPTY);
        addParameter(REPORT_PARAM_SCHOOL_FAX_NUMBER, STRING_EMPTY);

        SisSchool school = student.getSchool();
        if (school != null) {
            String schoolName = school.getName();
            addParameter(REPORT_PARAM_SCHOOL_NAME, schoolName);

            String faxNumber = (String) school.getFieldValueByAlias(ALIAS_SCHOOL_FAX);
            if (!StringUtils.isEmpty(faxNumber)) {
                addParameter(REPORT_PARAM_SCHOOL_FAX_NUMBER, FAX + STRING_SPACE + faxNumber);
            }

            SisAddress schoolAddress = school.getAddress();
            if (schoolAddress != null) {
                if (!StringUtils.isEmpty(schoolAddress.getPhone01()) ||
                        !StringUtils.isEmpty(schoolAddress.getPhone02())) {
                    addParameter(REPORT_PARAM_SCHOOL_PHONE_NUMBER, (StringUtils.isEmpty(schoolAddress.getPhone01())
                            ? schoolAddress.getPhone02() : schoolAddress.getPhone01()));
                }

                if (schoolAddress.getAddressLine01() != null) {
                    addParameter(REPORT_PARAM_SCHOOL_ADDRESS_LINE1, schoolAddress.getAddressLine01());
                }
                if (schoolAddress.getAddressLine03() != null) {
                    addParameter(REPORT_PARAM_SCHOOL_ADDRESS_LINE2, schoolAddress.getAddressLine03());
                }
            }

            SisStaff administator = school.getAdministrator1();
            if (administator != null) {
                SisPerson adminPerson = administator.getPerson();
                if (adminPerson != null) {
                    String adminFullName = adminPerson.getFirstName() + STRING_SPACE + adminPerson.getLastName();
                    addParameter(REPORT_PARAM_SKL_ADMIN1, adminFullName);
                }

                String superintendentName = (String) administator.getFieldValueByAlias(ALIAS_DEPT_SUPERVISOR);
                if (!StringUtils.isEmpty(superintendentName)) {
                    String[] superintendentNames = superintendentName.split(STRING_COMMA);
                    if (superintendentNames.length > 1) {
                        String superintendentFullName = superintendentNames[1] + STRING_SPACE + superintendentNames[0];
                        addParameter(REPORT_PARAM_SKL_ADMIN2, superintendentFullName);
                    }
                }
            }

        }

    }
}
