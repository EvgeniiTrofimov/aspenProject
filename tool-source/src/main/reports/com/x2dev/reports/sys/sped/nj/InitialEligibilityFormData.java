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

import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Locale;
import net.sf.jasperreports3.engine.data.JRBeanCollectionDataSource;

/**
 * The Class InitialEligibilityFormData.
 */
public class InitialEligibilityFormData extends BaseFormReportJavaSource {
    private static final String EMPTY_STRING = "";
    private static final String FAX = "FAX";
    private static final String COL_PARAMETER_MAP = "parameters";
    // Aliases
    private static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    private static final String ALIAS_SCHOOL_FAX = "DOE SCHOOL FAX";

    // Report fields
    private static final String FIELD_BLANK_TYPE = "blankType";
    private static final String FIELD_PROC_SAFE_DATA = "procSafeData";
    private static final String FIELD_RECIPIENT_NAME = "recipientName";
    private static final String FIELD_RECIPIENT_ADDRESS_01 = "recipientAddress01";
    private static final String FIELD_RECIPIENT_CITY = "recipientCity";
    private static final String FIELD_RECIPIENT_STATE = "recipientState";
    private static final String FIELD_RECIPIENT_ZIP = "recipientZip";
    private static final String FIELD_STUDENT_NAME = "studentName";
    private static final String FIELD_STUDENT_EIGHTEENTH = "studentEighteenth";
    private static final String FIELD_TEAM_MEMBER = "teamMember";

    // Report parameters
    private static final String PARAM_CASE_MANAGER = "caseManager";
    private static final String PARAM_IEP_DATA = "iepData";
    private static final String PARAM_IS_REDETERMINE_ELIG = "isRedetermineEligibility";
    private static final String PARAM_MEETING = "meeting";
    private static final String PARAM_MEETING_DICTIONARY = "meetingDictionary";
    private static final String PARAM_PROC_SAFE_REPORT = "procSafeReport";
    private static final String PARAM_PROC_SAFE_SUBREPORT_ID = "procSafeSubreportId";
    private static final String PARAM_EVAL_REQ = "evalreq";
    private static final String PARAM_EVAL_ASSESSMENTS = "evalassessment";
    private static final String PARAM_EVAL_OTHEREVAL = "evalothereval";
    private static final String PARAM_EVAL_STDRECORDS = "evalstdrec";
    private static final String PARAM_ELIG_DISCOR = "eligdiscor";
    private static final String PARAM_ELIG_DISADV = "eligdisadv";
    private static final String PARAM_ELIG_NOSPED = "elignosped";
    private static final String ALIAS_WORK_PHONE = "DOE STAFF WORK PHONE";
    private static final String PARAM_CASE_MANAGER_WORKPHONE = "workPhone";

    // Report Header Parameters
    private static final String PARAM_SCHOOL_NAME = "SCHOOL_NAME";
    private static final String PARAM_SCHOOL_ADDRESS_LINE1 = "SKL_ADDRESS_LINE1";
    private static final String PARAM_SCHOOL_ADDRESS_LINE2 = "SKL_ADDRESS_LINE2";
    private static final String PARAM_SCHOOL_PHONE_NO = "SKL_PHONE_NO";
    private static final String PARAM_SCHOOL_FAX_NO = "SKL_FAX_NO";
    private static final String PARAM_SKL_ADMIN1 = "SKL_ADMIN1";
    private static final String PARAM_SKL_ADMIN2 = "SKL_ADMIN2";
    private static final String PARAM_SKL_ADMIN3 = "SKL_ADMIN3";

    // Member variables
    private IepData m_currentIep;
    private GenericFormData m_meeting;
    private DataDictionary m_MeetingDictionary;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        String letterRecp = "Parent";
        if ((String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("elig-Recipient",
                getDictionary())) != null) {
            letterRecp = (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("elig-Recipient",
                    getDictionary()));
        }

        addParameter("letterRecp", letterRecp);

        String evalReq =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("elig-eligible", getDictionary()));
        addParameter(PARAM_EVAL_REQ, evalReq);

        String eligAssessments = (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("elig-assessments",
                getDictionary()));
        String eligOtherEval = (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("elig-other-eval",
                getDictionary()));
        String eligSTDRecords = (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("elig-std-records",
                getDictionary()));

        addParameter(PARAM_EVAL_ASSESSMENTS, eligAssessments);
        addParameter(PARAM_EVAL_OTHEREVAL, eligOtherEval);
        addParameter(PARAM_EVAL_STDRECORDS, eligSTDRecords);


        String eligdiscor = (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("elig-disability-cors",
                getDictionary()));
        String eligdisadv = (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("elig-disability-adver",
                getDictionary()));
        String elignosped =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("elig-no-sped", getDictionary()));

        addParameter(PARAM_ELIG_DISCOR, eligdiscor);
        addParameter(PARAM_ELIG_DISADV, eligdisadv);
        addParameter(PARAM_ELIG_NOSPED, elignosped);

        String prevParentAddress = "";
        String ParentAddress = "";

        ReportDataGrid grid = new ReportDataGrid();
        HashMap<String, Object> parameterMap = new HashMap<String, Object>();

        loadReportHeader();
        // this allows blank form to be produced if requested
        Collection dummy = new ArrayList<X2BaseBean>();
        dummy.add(getOrganization());
        if (getParameter(PARAM_BLANK).equals(Boolean.FALSE)) {
            addParameter(PARAM_IEP_DATA, m_currentIep);
            addParameter(PARAM_MEETING, m_meeting);
            addParameter(PARAM_MEETING_DICTIONARY, m_MeetingDictionary);

            if (m_currentIep.getStaff() != null &&
                    m_currentIep.getStaff().getPerson() != null) {
                String caseManager = getFullName(m_currentIep.getStaff().getPerson());
                String caseManagerName = m_currentIep.getStaff().getPerson().getFirstName() + " "
                        + m_currentIep.getStaff().getPerson().getLastName();
                String caseManagerRole = m_currentIep.getStaff().getSpedRole();
                addParameter(PARAM_CASE_MANAGER, caseManager);
                addParameter("caseManagerName", caseManagerName);
                addParameter("caseManagerRole", caseManagerRole);
            }

            Collection<IepTeamMember> teamMembers = m_currentIep.getTeamMembers();

            // print a notification for each parent
            // the report is completed in the detail band
            boolean hasGuardian = false;
            for (IepTeamMember teamMember : teamMembers) {
                if (teamMember.getMemberRoleCode().equals("Parent/Guardian")) {
                    teamMember.getPerson().getContact().getStudentContacts();

                    Collection<StudentContact> studentContacts =
                            teamMember.getPerson().getContact().getStudentContacts();

                    for (StudentContact sContact : studentContacts) {
                        if (letterRecp.equals("Student")) {
                            if (sContact.getLivesWithIndicator() == true) {
                                grid.append();
                                grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(dummy));
                                addMeetingData(grid);
                                addNotificationRecipient(grid, teamMember);
                                hasGuardian = true;
                                prevParentAddress = sContact.getContact().getAddressView();
                            }
                        } else {
                            if (sContact.getStudentOid().equals(m_currentIep.getStudentOid())) {
                                if (sContact.getGradeMailingIndicator() == true) {
                                    prevParentAddress = prevParentAddress.trim();
                                    ParentAddress = sContact.getContact().getAddressView();
                                    ParentAddress = ParentAddress.trim();

                                    if (ParentAddress.equals(prevParentAddress)) {
                                        AppGlobals.getLog().severe(
                                                "Not printing Parent Letter because a previous one was printed with matching address ");
                                    } else {
                                        grid.append();
                                        grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(dummy));
                                        addMeetingData(grid);
                                        addNotificationRecipient(grid, teamMember);
                                        hasGuardian = true;
                                        prevParentAddress = sContact.getContact().getAddressView();
                                    }
                                }
                            }
                        }
                    }
                }
            }

            if (!hasGuardian) {
                grid.append();
                grid.set(FIELD_RECIPIENT_NAME, "*** No Guardian set for this Student's IEP ***");
                grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(dummy));
                addMeetingData(grid);
            }
        }
        Report evalSubreport = ReportUtils.getReport((String) getParameter(PARAM_PROC_SAFE_SUBREPORT_ID), getBroker());
        addParameter(PARAM_PROC_SAFE_REPORT, new ByteArrayInputStream(evalSubreport.getCompiledFormat()));

        addParameter(PARAM_IS_REDETERMINE_ELIG,
                getFormDefinition().getId().equals("SPED-NJ-REDETERMINE") ? Boolean.TRUE : Boolean.FALSE);

        // if there are no recipients, use an empty grid to print a blank report
        if (grid.rowCount() == 0) {
            grid.append();
            grid.set(FIELD_BLANK_TYPE, Boolean.FALSE);
            grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(dummy));
            grid.append();
            grid.set(FIELD_BLANK_TYPE, Boolean.TRUE);
            grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(dummy));
        }
        IepData iepData = (IepData) getFormOwner();
        addParameter("caseManagerPhone", "(908) 284-" + iepData.getStaff().getFieldValueByAlias(ALIAS_WORK_PHONE));
        grid.set(PARAM_CASE_MANAGER, iepData.getStaff());
        grid.set(PARAM_CASE_MANAGER_WORKPHONE, iepData.getStaff() != null
                ? "(908) 284-" + iepData.getStaff().getFieldValueByAlias(ALIAS_WORK_PHONE) : "");
        grid.set(COL_PARAMETER_MAP, parameterMap);
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
        m_MeetingDictionary = DataDictionary.getDistrictDictionary(extendDictionary, getBroker().getPersistenceKey());
        addParameter("dateAsStringConverter",
                ConverterFactory.getConverterForClass(PlainDate.class.getName(), Locale.getDefault(), true));
    }

    /**
     * Add meeting invitation recipient data to grid for display on report.
     *
     * @param grid ReportDataGrid
     * @param teamMember IepTeamMember
     */
    private void addNotificationRecipient(ReportDataGrid grid, IepTeamMember teamMember) {
        grid.set(FIELD_TEAM_MEMBER, teamMember);
        if (teamMember.getPerson() != null) {
            SisPerson person = teamMember.getPerson();
            SisAddress address = person.getPhysicalAddress();

            String recipientName = getFullName(person);
            grid.set(FIELD_RECIPIENT_NAME, recipientName);

            if (address == null) {
                // address = person.getMailingAddress();
                grid.set(FIELD_RECIPIENT_ADDRESS_01, "");
                grid.set(FIELD_RECIPIENT_CITY, "");
                grid.set(FIELD_RECIPIENT_STATE, "");
                grid.set(FIELD_RECIPIENT_ZIP, "");
            }

            if (address != null) {
                grid.set(FIELD_RECIPIENT_ADDRESS_01, address.getAddressLine01());
                grid.set(FIELD_RECIPIENT_CITY, address.getCity());
                grid.set(FIELD_RECIPIENT_STATE, address.getState());
                grid.set(FIELD_RECIPIENT_ZIP, address.getPostalCode());
            }
        }
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

        PlainDate eighteenthBirthDate = new PlainDate();
        if (m_currentIep.getStudent() != null) {
            Person person = m_currentIep.getStudent().getPerson();
            PlainDate birthDate = person.getDob();
            eighteenthBirthDate = DateUtils.add(birthDate, Calendar.YEAR, 18);
        }
        grid.set(FIELD_STUDENT_EIGHTEENTH, eighteenthBirthDate);
    }

    /**
     * Get full name from a person bean.
     *
     * @param person SisPerson
     * @return fullName
     */
    private String getFullName(SisPerson person) {
        String fullName = "";

        fullName = fullName + person.getFirstName();
        if (!StringUtils.isEmpty(fullName)) {
            fullName = fullName + " ";
        }
        fullName = fullName + person.getLastName();

        return fullName;
    }

    /**
     * Load report header.
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
