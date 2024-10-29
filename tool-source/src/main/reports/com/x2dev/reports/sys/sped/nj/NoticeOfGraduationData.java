/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2014 Follett Software Company
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
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import java.sql.Date;
import java.text.SimpleDateFormat;
import java.util.Collection;

/**
 * The Class NoticeOfGraduationData.
 */
public class NoticeOfGraduationData extends BaseFormReportJavaSource {
    private static final String FAX = "FAX";
    private final SimpleDateFormat DATE_FORMATTER = new SimpleDateFormat("MM/dd/yyyy");
    private final SimpleDateFormat DATE_FORMATTER_LONG = new SimpleDateFormat("MMMMM dd, yyyy");

    /**
     * Aliases
     */
    private static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    private static final String ALIAS_SCHOOL_FAX = "DOE SCHOOL FAX";
    private static final String ALIAS_WORK_PHONE = "DOE STAFF WORK PHONE";

    /**
     * The variables below are used for setting the parameters of the printed report.
     */
    private static final String PARAM_ADDRESS1 = "address1";
    private static final String PARAM_ADDRESS2 = "address2";
    private static final String PARAM_ADULT_STUDENT = "adultStudent";
    private static final String PARAM_CASE_MANAGER = "caseManager";
    private static final String PARAM_CASE_MANAGER_WORKPHONE = "workPhone";
    private static final String PARAM_DETAILS = "details";
    private static final String PARAM_GENDER = "gender";
    private static final String PARAM_LETTER_DATE = "letterDate";
    private static final String PARAM_MEETING_DATE = "meetingDate";
    private static final String PARAM_PARENT = "parent";
    private static final String PARAM_SCHOOL_ADDRESS_LINE1 = "SKL_ADDRESS_LINE1";
    private static final String PARAM_SCHOOL_ADDRESS_LINE2 = "SKL_ADDRESS_LINE2";
    private static final String PARAM_SCHOOL_FAX_NO = "SKL_FAX_NO";
    private static final String PARAM_SCHOOL_NAME = "SKL_NAME";
    private static final String PARAM_SCHOOL_PHONE_NO = "SKL_PHONE_NO";
    private static final String PARAM_SKL_ADMIN1 = "SKL_ADMIN1";
    private static final String PARAM_SKL_ADMIN2 = "SKL_ADMIN2";
    private static final String PARAM_STUDENT = "student";

    /**
     * Input definition contact information
     */
    private static final String PARAM_CHILD_STUDY_TEAM = "schoolChildStudyTeam";
    private static final String PARAM_COUNTY_SUPERVISOR_NAME = "countySupervisor";
    private static final String PARAM_COUNTY_SUPERVISOR_NUMBER = "countySupervisorPhoneNumber";
    private static final String PARAM_CST_NUMBER = "schoolCSTNumber";
    private static final String PARAM_SCHOOL_CITY = "schoolCity";
    private static final String PARAM_SCHOOL_DISABILITY_CONTACT_NAME = "schoolDisabilityContactName";
    private static final String PARAM_SCHOOL_DISABILITY_CONTACT_NUM = "schoolDisabilityContactNumber";
    private static final String PARAM_SCHOOL_DISABILITY_RESOURCE = "schoolDisabilityResource";
    private static final String PARAM_SCHOOL_NUMBER = "schoolPhoneNumber";
    private static final String PARAM_SCHOOL_STREET = "schoolStreet";
    private static final String PARAM_SPECIAL_SERVICES = "schoolSpecialServicesDepartment";

    // Member variables
    private IepData m_currentIep;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected ReportDataGrid gatherData() throws Exception {
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(getFormDefinition().getExtendedDataDictionary(),
                        getBroker().getPersistenceKey());

        GenericFormData formData = (GenericFormData) getFormStorage();
        IepData iepData = (IepData) getFormOwner();
        SisStudent student = iepData.getStudent();
        SisSchool school = student.getSchool();
        ReportDataGrid grid = new ReportDataGrid();

        Address studentAddress = student.getPerson().getResolvedMailingAddress();
        String addressOne = (studentAddress != null ? studentAddress.getAddressLine01() : "");
        String addressTwo = (studentAddress != null ? studentAddress.getAddressLine03() : "");

        String genderCode = student.getPerson().getGenderCode();

        m_currentIep = (IepData) getFormOwner();
        Collection<IepTeamMember> teamMembers = m_currentIep.getTeamMembers();
        String prevParentAddress = "";
        String ParentAddress = "";
        // print a notification for each parent
        // the report is completed in the detail band

        for (IepTeamMember teamMember : teamMembers) {
            if (teamMember.getMemberRoleCode().equals("Parent/Guardian")) {
                teamMember.getPerson().getContact().getStudentContacts();

                Collection<StudentContact> studentContacts = teamMember.getPerson().getContact().getStudentContacts();
                for (StudentContact sContact : studentContacts) {
                    if (sContact.getStudentOid().equals(m_currentIep.getStudentOid())) {
                        if (sContact.getGradeMailingIndicator() == true) {
                            prevParentAddress = prevParentAddress.trim();
                            ParentAddress = sContact.getContact().getAddressView();
                            ParentAddress = ParentAddress.trim();

                            if (!ParentAddress.equals(prevParentAddress)) {
                                Person parent = student.getContact1().getContact().getPerson();

                                addParameters(school);

                                grid.append();
                                grid.set(PARAM_ADDRESS1, addressOne);
                                grid.set(PARAM_ADDRESS2, addressTwo);
                                grid.set(PARAM_ADULT_STUDENT, student);
                                grid.set(PARAM_GENDER, genderCode);
                                String letterDateAsString =
                                        (String) formData.getFieldValueByAlias(PARAM_LETTER_DATE, dictionary);

                                if (!StringUtils.isEmpty(letterDateAsString)) {
                                    grid.set(PARAM_LETTER_DATE,
                                            DATE_FORMATTER_LONG.format(Date.valueOf(letterDateAsString)));
                                } else {
                                    grid.set(PARAM_LETTER_DATE, null);
                                }

                                String meetingDateAsString =
                                        (String) formData.getFieldValueByAlias(PARAM_MEETING_DATE, dictionary);
                                if (!StringUtils.isEmpty(meetingDateAsString)) {
                                    grid.set(PARAM_MEETING_DATE,
                                            DATE_FORMATTER.format(Date.valueOf(meetingDateAsString)));
                                } else {
                                    grid.set(PARAM_MEETING_DATE, null);
                                }

                                grid.set(PARAM_PARENT, parent);
                                grid.set(PARAM_DETAILS, formData.getFieldValueByAlias(PARAM_DETAILS, dictionary));
                                grid.set(PARAM_CASE_MANAGER, iepData.getStaff());
                                grid.set(PARAM_CASE_MANAGER_WORKPHONE,
                                        iepData.getStaff() != null
                                                ? "(908) 284-"
                                                        + iepData.getStaff().getFieldValueByAlias(ALIAS_WORK_PHONE)
                                                : "");
                                grid.set(PARAM_STUDENT, student);

                                prevParentAddress = sContact.getContact().getAddressView();
                            }
                        }
                    }

                }
            }
        }

        grid.beforeTop();

        return grid;
    }

    /**
     * Adds all the parameters to the report.
     *
     * @param school SisSchool
     */
    private void addParameters(SisSchool school) {
        addParameter(PARAM_SCHOOL_PHONE_NO, "");
        addParameter(PARAM_SCHOOL_ADDRESS_LINE1, "");
        addParameter(PARAM_SCHOOL_ADDRESS_LINE2, "");
        addParameter(PARAM_SCHOOL_NAME, school.getName());
        String fax = (String) school.getFieldValueByAlias(ALIAS_SCHOOL_FAX);
        addParameter(PARAM_SCHOOL_FAX_NO, !StringUtils.isEmpty(fax) ? FAX + fax : "");

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

        String admin1 = "";
        addParameter(PARAM_SKL_ADMIN1, "");
        addParameter(PARAM_SKL_ADMIN2, "");

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

        addParameter(PARAM_SPECIAL_SERVICES, getParameter(PARAM_SPECIAL_SERVICES));
        addParameter(PARAM_SCHOOL_STREET, getParameter(PARAM_SCHOOL_STREET));
        addParameter(PARAM_SCHOOL_CITY, getParameter(PARAM_SCHOOL_CITY));
        addParameter(PARAM_SCHOOL_NUMBER, getParameter(PARAM_SCHOOL_NUMBER));
        addParameter(PARAM_CHILD_STUDY_TEAM, getParameter(PARAM_CHILD_STUDY_TEAM));
        addParameter(PARAM_CST_NUMBER, getParameter(PARAM_CST_NUMBER));
        addParameter(PARAM_SCHOOL_DISABILITY_RESOURCE, getParameter(PARAM_SCHOOL_DISABILITY_RESOURCE));
        addParameter(PARAM_SCHOOL_DISABILITY_CONTACT_NAME, getParameter(PARAM_SCHOOL_DISABILITY_CONTACT_NAME));
        addParameter(PARAM_SCHOOL_DISABILITY_CONTACT_NUM, getParameter(PARAM_SCHOOL_DISABILITY_CONTACT_NUM));
        addParameter(PARAM_COUNTY_SUPERVISOR_NAME, getParameter(PARAM_COUNTY_SUPERVISOR_NAME));
        addParameter(PARAM_COUNTY_SUPERVISOR_NUMBER, getParameter(PARAM_COUNTY_SUPERVISOR_NUMBER));
    }
}
