/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.nj;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.Contact;
import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepMeetingAttendance;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the NJ IEP meeting invitation form. This class prepares a ReportDataGrid that
 * contains a row for each page of the report. One page is printed per team member with the
 * "print invitation" field set to true.l
 * <p>
 * Special handling exists for printing blank invitations. In this case, a single copy of the letter
 * is included. The report is designed to work directly off a current "IepMeetingInvitation" object
 * on a detail page in addition to from a form-enabled area (forms manager, workflow, etc.).
 *
 * @author X2 Development Corporation
 */
public class MeetingInviteMainData extends BaseFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Input Parameters
     */
    private static final String PARAM_DIS_CONTACT_NUM = "schoolDisabilityContactNumber";
    private static final String PARAM_SCHOOL_CONTACT_NAME = "schoolDisabilityContactName";
    private static final String PARAM_SCHOOL_CSTEAM = "schoolChildStudyTeam";
    private static final String PARAM_SCHOOL_CSTNUM = "schoolCSTNumber";
    private static final String PARAM_SCHOOL_DISRES = "schoolDisabilityResource";
    private static final String PARAM_SCHOOL_NAME2 = "schoolName";
    private static final String PARAM_SCHOOL_PHONE = "schoolPhoneNumber";
    private static final String PARAM_SCHOOL_SD = "schoolSpecialServicesDepartment";
    private static final String PARAM_SCHOOL_STREET = "schoolStreet";
    private static final String PARAM_SSS_STAFF = "sssStaff";
    private static final String PARAM_SUPERVISOR = "countySupervisor";
    private static final String PARAM_SUPERVISOR_NUM = "countySupervisorPhoneNumber";

    // Constants for the letter (N3I) portion of the report
    private static final String TO_PARAMETER = "to";
    private static final String FAX = "FAX";
    private static final String EXTN_WORK_PHONE = "(908) 284-";

    // Constants for the waiver (N3W) portion of the report
    private static final int MAX_REPORT_COL_LIMIT = 7;

    // ReportDataGrid column constants for the main report
    private static final String COL_SUBREPORT_DATA_SOURCE = "datasource";
    private static final String COL_SUBREPORT_FORMAT = "format";
    private static final String COL_PAGE_NUMBER = "pageNumber";
    private static final String COL_PAGE_IDENTIFIER = "pageIdentifier";
    private static final String COL_PARAMETER_MAP = "parameters";

    // ReportDataGrid column constants for the main report
    private static final String PAGE_1_FORMAT_ID_STUDENT = "SYS-SPED-NJ-TRMTG-P1";
    private static final String PAGE_1_FORMAT_ID = "SYS-SPED-NJ-MTG-P1";
    private static final String PAGE_1_PAGE_IDENTIFIER = "P1";
    private static final String MEETING_FORM_DEF_ID = "MTG";

    /**
     * Report Parameters
     */
    private static final String PARAM_ADDRESS_LINE1 = "ADDRESS_LINE1";
    private static final String PARAM_ADDRESS_LINE2 = "ADDRESS_LINE2";
    private static final String PARAM_AGE_CATEGORY = "AGE_CATEGORY";
    private static final String PARAM_BUSINESS_PHONE_NO = "BUSINESS_PHONE_NO";
    private static final String PARAM_CHAIR_PERSON = "CHAIR_PERSON";
    private static final String PARAM_FORM_DATE = "FORM_DATE";
    private static final String PARAM_INVITATION_DATE = "invitationdate";
    private static final String PARAM_INVITED_TEAM_MBRS1 = "INVITED_TEAM_MBRS1";
    private static final String PARAM_INVITED_TEAM_MBRS2 = "INVITED_TEAM_MBRS2";
    private static final String PARAM_INVITEES_COL1 = "invitees1";
    private static final String PARAM_INVITEES_COL2 = "invitees2";
    private static final String PARAM_MEET_DATE = "MEET_DATE";
    private static final String PARAM_MEETING_DATE = "MEETING_DATE";
    private static final String PARAM_MEETING_LOCATION = "MEETING_LOCATION";
    private static final String PARAM_MEETING_TIME = "MEETING_TIME";
    private static final String PARAM_MEMBER_ROLE = "MEMBER_ROLE";
    private static final String PARAM_OTHER_CONTACT_PERSON = "OTHER_CONTACT_PERSON";
    private static final String PARAM_PERSON_NAME = "PERSON_NAME";
    private static final String PARAM_RESPONSE_DATE = "RESPONSE_DATE";
    private static final String PARAM_S_NAME = "SNAME";
    private static final String PARAM_SCHOOL_ADDRESS_LINE1 = "SKL_ADDRESS_LINE1";
    private static final String PARAM_SCHOOL_ADDRESS_LINE2 = "SKL_ADDRESS_LINE2";
    private static final String PARAM_SCHOOL_FAX_NO = "SKL_FAX_NO";
    private static final String PARAM_SCHOOL_NAME = "SCHOOL_NAME";
    private static final String PARAM_SCHOOL_PHONE_NO = "SKL_PHONE_NO";
    private static final String PARAM_SKL_ADMIN1 = "SKL_ADMIN1";
    private static final String PARAM_SKL_ADMIN2 = "SKL_ADMIN2";
    private static final String PARAM_SKL_ADMIN3 = "SKL_ADMIN3";
    private static final String PARAM_STUDENT_FIRST_NAME = "STUDENT_FIRST_NAME";
    private static final String PARAM_STUDENT_NAME = "STUDENT_NAME";

    /**
     * Aliases
     */
    private static final String ALIAS_OTHER_CONTACT_PERSON = "sped-other-contact-person";
    private static final String ALIAS_MTG_INVITATION_DATE = "meeting-invitation-date";
    private static final String ALIAS_STAFF_WORK_PHONE = "DOE STAFF WORK PHONE";
    private static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    private static final String ALIAS_SCHOOL_FAX = "DOE SCHOOL FAX";
    private static final String ALIAS_STAFF_ROLE = "DOE STAFF ROLE";

    /**
     * Other Constants
     */
    private static final String AGE_CATEGORY_U18 = "AgeCategoryU18";
    private static final String AGE_CATEGORY_O18 = "AgeCategoryO18";
    private static final String INVITEE_DELIMITER = " - ";
    private static final String STRING_COMMA = ",";
    private static final String STRING_EMPTY = "";
    private static final String STRING_SPACE = " ";
    private static final String STRING_NEW_LINE = "\n";
    private static final String TITLE_CASE_MANAGER = "Case Manager";

    private final SimpleDateFormat DATE_FORMATTER = new SimpleDateFormat("MM/dd/yyyy");
    private final SimpleDateFormat DATE_FORMATTER_LONG = new SimpleDateFormat("MMMMM dd, yyyy");
    private final SimpleDateFormat TIME_FORMATTER = new SimpleDateFormat("h:mm a");

    /**
     * Codes
     */
    private static final String MEMBER_ROLE_PARENT = "Parent/Guardian";
    private static final String MEMBER_ROLE_STUDENT = "Student";

    /**
     * Variables
     */
    private int m_currentPageNumber = 0;
    private Map<?, ?> m_subReports = null;
    private Map<?, ?> m_subReportsStudent = null;
    private IepMeeting m_currentMeeting = null;
    private IepData m_currentIep = null;
    private IepMeeting m_meeting = null;
    private PlainDate m_responseDate = new PlainDate();
    private String m_sssStaffName;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        addParameter(PARAM_SCHOOL_NAME2, getParameter(PARAM_SCHOOL_NAME2));
        addParameter(PARAM_SCHOOL_SD, getParameter(PARAM_SCHOOL_SD));
        addParameter(PARAM_SCHOOL_STREET, getParameter(PARAM_SCHOOL_STREET));
        addParameter(PARAM_SCHOOL_PHONE, getParameter(PARAM_SCHOOL_PHONE));
        addParameter(PARAM_SCHOOL_CSTEAM, getParameter(PARAM_SCHOOL_CSTEAM));
        addParameter(PARAM_SCHOOL_CSTNUM, getParameter(PARAM_SCHOOL_CSTNUM));
        addParameter(PARAM_SCHOOL_DISRES, getParameter(PARAM_SCHOOL_DISRES));
        addParameter(PARAM_SCHOOL_CONTACT_NAME, getParameter(PARAM_SCHOOL_CONTACT_NAME));
        addParameter(PARAM_DIS_CONTACT_NUM, getParameter(PARAM_DIS_CONTACT_NUM));
        addParameter(PARAM_SUPERVISOR, getParameter(PARAM_SUPERVISOR));
        addParameter(PARAM_SUPERVISOR_NUM, getParameter(PARAM_SUPERVISOR_NUM));

        IepData iep = (IepData) getFormOwner();
        SisStudent sisStudent = iep.getStudent();
        String studentsName = STRING_EMPTY;
        studentsName = sisStudent.getNameView();

        if (studentsName != null) {
            String[] tokens = studentsName.split(STRING_COMMA);
            String lastName = tokens[0];
            String firstName = tokens[1];
            studentsName = firstName + STRING_SPACE + lastName;
        }
        addParameter(PARAM_S_NAME, studentsName);

        ReportDataGrid grid = new ReportDataGrid();

        loadSubReports();

        if (m_currentMeeting != null) {
            setFormStorage(m_currentMeeting);
            setFormOwner(m_currentMeeting.getIepData());
        }

        m_meeting = (IepMeeting) getFormStorage();

        IepMeeting meeting = (IepMeeting) getFormStorage();

        if (isBlank()) {
            preparePage1(grid, new IepMeetingAttendance(getBroker().getPersistenceKey()), STRING_EMPTY, STRING_EMPTY);
        } else {
            StringBuilder inviteeStringColumn1 = new StringBuilder();
            StringBuilder inviteeStringColumn2 = new StringBuilder();
            int inviteeCount = 0;

            /*
             * Build list of invitees and collection of attendance objects for each page of the
             * invitation
             */
            List<IepMeetingAttendance> attendanceList = new LinkedList<IepMeetingAttendance>();

            Criteria attendanceCriteria = new Criteria();
            attendanceCriteria.addEqualTo(IepMeetingAttendance.COL_IEP_MEETING_OID, meeting.getOid());
            attendanceCriteria.addEqualTo(IepMeetingAttendance.COL_EXCUSED_INDICATOR, Boolean.valueOf(false));

            QueryByCriteria attendanceQuery = new QueryByCriteria(IepMeetingAttendance.class, attendanceCriteria);
            attendanceQuery.addOrderByAscending(
                    IepMeetingAttendance.REL_TEAM_MEMBER + ModelProperty.PATH_DELIMITER + IepTeamMember.COL_NAME_VIEW);

            @SuppressWarnings("unchecked")
            Collection<IepMeetingAttendance> attendanceMembers = getBroker().getCollectionByQuery(attendanceQuery);

            StringBuilder inviteeString = inviteeStringColumn1;
            String parentAddress = STRING_EMPTY;
            Set<String> previousAddresses = new HashSet<String>();

            for (IepMeetingAttendance attendanceBean : attendanceMembers) {
                IepTeamMember teamMember = attendanceBean.getTeamMember();
                String memberName = teamMember.getNameView();

                if (MEMBER_ROLE_PARENT.equals(teamMember.getMemberRoleCode())) {
                    Person person = teamMember.getPerson();

                    if (person != null) {
                        Contact contact = person.getContact();

                        if (contact != null) {
                            Collection<StudentContact> studentContacts = contact.getStudentContacts();

                            for (StudentContact studentContact : studentContacts) {
                                if (studentContact.getStudentOid().equals(iep.getStudentOid())) {
                                    parentAddress = studentContact.getContact().getAddressView();
                                    if (parentAddress != null) {
                                        parentAddress = parentAddress.trim();
                                    }


                                    if (!attendanceBean.getHideNameIndicator() && !StringUtils.isEmpty(memberName)) {
                                        inviteeString.append(teamMember.getNameView());
                                        inviteeString.append(INVITEE_DELIMITER);
                                    }

                                    inviteeString.append(teamMember.getMemberRoleCode());
                                    inviteeString.append(STRING_NEW_LINE);

                                    if (!previousAddresses.contains(parentAddress)) {
                                        attendanceList.add(attendanceBean);
                                        previousAddresses.add(parentAddress);
                                    }
                                }
                            }
                        }
                    }
                } else {
                    if (inviteeCount++ > MAX_REPORT_COL_LIMIT) {
                        inviteeString = inviteeStringColumn2;
                    }

                    if (!attendanceBean.getHideNameIndicator() && !StringUtils.isEmpty(memberName)) {
                        inviteeString.append(teamMember.getNameView());
                        inviteeString.append(INVITEE_DELIMITER);
                    }

                    inviteeString.append(teamMember.getMemberRoleCode());
                    inviteeString.append(STRING_NEW_LINE);

                    attendanceList.add(attendanceBean);
                }
            }

            for (IepMeetingAttendance attendanceBean : attendanceList) {
                IepTeamMember teamMember = attendanceBean.getTeamMember();

                if (MEMBER_ROLE_STUDENT.equals(teamMember.getMemberRoleCode())) {
                    preparePage1Student(grid, attendanceBean, inviteeStringColumn1.toString(),
                            inviteeStringColumn2.toString());
                } else {
                    preparePage1(grid, attendanceBean, inviteeStringColumn1.toString(),
                            inviteeStringColumn2.toString());
                }
            }
        }

        grid.beforeTop();

        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);

        m_currentIep = userData.getCurrentRecord(IepData.class);

        /*
         * If the form def is null, then the invitation was run from the reports menu on the meeting
         * list.
         * In this case the superclass can't determine the form definition so we hard-set it here.
         */

        if (getFormDefinition() == null) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(FormDefinition.COL_ID, MEETING_FORM_DEF_ID);

            BeanQuery beanQuery = new BeanQuery(FormDefinition.class, criteria);

            setFormDefinition((FormDefinition) getBroker().getBeanByQuery(beanQuery));

            m_currentMeeting = userData.getCurrentRecord(IepMeeting.class);
        }
    }

    /**
     * This method sets the chair person's details for the signature line of the SPED form. If the
     * team members does not
     * include a chair person, then the case manager signs the SPED form.
     *
     * @param parameterMap HashMap<String,Object>
     * @param dictionary DataDictionary
     * @return void
     */
    private void getChairPersonDetails(HashMap<String, Object> parameterMap, DataDictionary dictionary) {
        addParameter(PARAM_CHAIR_PERSON, STRING_EMPTY);

        SisPerson chairPerson = null;
        SisStaff caseManager = m_currentIep.getStaff();
        String caseManagerWorkPhone = null;
        String title = null;
        Collection<IepTeamMember> teamMembers = m_currentIep.getTeamMembers();

        String businessPhoneNo = STRING_EMPTY;

        for (IepTeamMember teamMember : teamMembers) {
            if (teamMember.getChairpersonIndicator()) {
                chairPerson = teamMember.getPerson();
                title = teamMember.getMemberRoleCode();
                caseManagerWorkPhone = (String) caseManager.getFieldValueByAlias(ALIAS_STAFF_WORK_PHONE, dictionary);
                businessPhoneNo = EXTN_WORK_PHONE + caseManagerWorkPhone;
                break;
            }
        }

        if (chairPerson == null) {
            if (caseManager != null) {
                chairPerson = caseManager.getPerson();
                title = TITLE_CASE_MANAGER;
                caseManagerWorkPhone = (String) caseManager.getFieldValueByAlias(ALIAS_STAFF_WORK_PHONE, dictionary);
                businessPhoneNo = EXTN_WORK_PHONE + caseManagerWorkPhone;
            }
        }

        if (chairPerson != null) {
            parameterMap.put(PARAM_CHAIR_PERSON, chairPerson.getFirstName() + STRING_SPACE + chairPerson.getLastName());
            parameterMap.put(PARAM_MEMBER_ROLE, title);
            parameterMap.put(PARAM_BUSINESS_PHONE_NO, businessPhoneNo);
        }
    }

    /**
     * Returns the format of subreport for the given page constant.
     *
     * @param pageId String
     * @return byte[]
     */
    private byte[] getSubreportFormat(String pageId) {
        Report report = (Report) m_subReports.get(pageId);

        return report.getCompiledFormat();
    }

    /**
     * Returns the format of subreport for the given page constant.
     *
     * @param pageId String
     * @return byte[]
     */
    private byte[] getSubreportFormatStudent(String pageId) {
        Report report = (Report) m_subReportsStudent.get(pageId);

        return report.getCompiledFormat();
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
     * Prepares the first page.
     *
     * @param grid ReportDataGrid
     * @param attendanceBean IepMeetingAttendance
     * @param inviteesList1 String
     * @param inviteesList2 String
     */
    @SuppressWarnings("unchecked")
    private void preparePage1(ReportDataGrid grid,
                              IepMeetingAttendance attendanceBean,
                              String inviteesList1,
                              String inviteesList2) {
        if (attendanceBean.getPrintInvitationIndicator()) {
            JRDataSource dataSource =
                    new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
            m_currentIep = attendanceBean.getIepData();

            ExtendedDictionaryAttributes extendDictionary = m_currentIep.getExtendedDataDictionary();
            DataDictionary dictionary =
                    DataDictionary.getDistrictDictionary(extendDictionary, getBroker().getPersistenceKey());
            String invitationDateStr = (String) m_meeting.getFieldValueByAlias(ALIAS_MTG_INVITATION_DATE, dictionary);
            PlainDate invitationDate = DateUtils.getDate(invitationDateStr);

            SisStaff staff = m_currentIep.getStaff();
            String extnWorkPhone = (String) staff.getFieldValueByAlias(ALIAS_STAFF_WORK_PHONE, dictionary);
            String businessPhoneNo =
                    (!StringUtils.isEmpty(extnWorkPhone)) ? EXTN_WORK_PHONE + extnWorkPhone : STRING_EMPTY;

            HashMap<String, Object> parameterMap = new HashMap<String, Object>();
            parameterMap.put(PARAM_FORM_DATE, DATE_FORMATTER_LONG.format(invitationDate));
            parameterMap.put(TO_PARAMETER, attendanceBean.getTeamMember());
            parameterMap.putAll(getParameters());
            parameterMap.put(PARAM_INVITEES_COL1, inviteesList1);
            parameterMap.put(PARAM_INVITEES_COL2, inviteesList2);
            parameterMap.put(PARAM_BUSINESS_PHONE_NO, businessPhoneNo);

            loadReportHeader(parameterMap);

            loadInviteeAddressDetails(attendanceBean, parameterMap);

            grid.append();
            grid.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
            grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_1_FORMAT_ID));
            grid.set(COL_PARAMETER_MAP, parameterMap);
            grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
            grid.set(COL_PAGE_IDENTIFIER, PAGE_1_PAGE_IDENTIFIER);
        }
    }

    /**
     * Prepares the first page for Student.
     *
     * @param grid ReportDataGrid
     * @param attendanceBean IepMeetingAttendance
     * @param inviteesList1 String
     * @param inviteesList2 String
     */
    private void preparePage1Student(ReportDataGrid grid,
                                     IepMeetingAttendance attendanceBean,
                                     String inviteesList1,
                                     String inviteesList2) {
        HashMap<String, Object> parameterMap = new HashMap<String, Object>();

        if (attendanceBean.getPrintInvitationIndicator()) {
            ExtendedDictionaryAttributes extendDictionary = m_currentIep.getExtendedDataDictionary();
            DataDictionary dictionary =
                    DataDictionary.getDistrictDictionary(extendDictionary, getBroker().getPersistenceKey());
            getChairPersonDetails(parameterMap, dictionary);

            SisStudent student = m_currentIep.getStudent();
            Person person = attendanceBean.getTeamMember().getPerson();
            String schoolName = student.getSchool().getName();
            PlainDate dateOfBirth = student.getPerson().getDob();
            PlainDate meetingDate = m_meeting.getDate();

            int age = 15; // Some default age value which is less than 18 to print the form for
                          // parents.
            parameterMap.put(PARAM_MEETING_DATE, STRING_EMPTY);
            parameterMap.put(PARAM_MEETING_TIME, STRING_EMPTY);
            parameterMap.put(PARAM_RESPONSE_DATE, STRING_EMPTY);

            if (meetingDate != null) {
                age = Person.getAgeAsOfDate(dateOfBirth, meetingDate);
                Calendar calendar = Calendar.getInstance();
                calendar.setTime(meetingDate);
                calendar.add(Calendar.DATE, -2);
                m_responseDate = DateUtils.getPriorWeekDay(new PlainDate(calendar.getTime()));

                parameterMap.put(PARAM_MEETING_DATE, DATE_FORMATTER.format(meetingDate));
                parameterMap.put(PARAM_MEETING_TIME, TIME_FORMATTER.format(m_meeting.getTime()));
                parameterMap.put(PARAM_RESPONSE_DATE, DATE_FORMATTER.format(m_responseDate));
            }

            String invitationDateStr = (String) m_meeting.getFieldValueByAlias(ALIAS_MTG_INVITATION_DATE, dictionary);
            PlainDate invitationDate = DateUtils.getDate(invitationDateStr);

            parameterMap.put(PARAM_AGE_CATEGORY, (age < 18) ? AGE_CATEGORY_U18 : AGE_CATEGORY_O18);
            parameterMap.put(PARAM_FORM_DATE, DATE_FORMATTER_LONG.format(invitationDate));
            addParameter(PARAM_MEET_DATE, DATE_FORMATTER_LONG.format(invitationDate));
            addParameter(PARAM_INVITATION_DATE, DATE_FORMATTER_LONG.format(invitationDate));

            parameterMap.put(PARAM_STUDENT_NAME,
                    student.getPerson().getFirstName() + STRING_SPACE + student.getPerson().getLastName());
            parameterMap.put(PARAM_STUDENT_FIRST_NAME, student.getPerson().getFirstName());
            parameterMap.put(PARAM_SCHOOL_NAME, schoolName);
            parameterMap.put(PARAM_MEETING_LOCATION, m_meeting.getLocation());
            parameterMap.put(PARAM_OTHER_CONTACT_PERSON,
                    m_meeting.getFieldValueByAlias(ALIAS_OTHER_CONTACT_PERSON, dictionary));
            parameterMap.put(PARAM_INVITED_TEAM_MBRS1, inviteesList1);
            parameterMap.put(PARAM_INVITED_TEAM_MBRS2, inviteesList2);

            loadReportHeader(parameterMap);

            loadInviteeAddressDetailsStudent(attendanceBean, parameterMap, student, person);

            grid.append();

            JRDataSource dataSource =
                    new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
            grid.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
            grid.set(COL_SUBREPORT_FORMAT, getSubreportFormatStudent(PAGE_1_FORMAT_ID_STUDENT));
            grid.set(COL_PARAMETER_MAP, parameterMap);
            grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
            grid.set(COL_PAGE_IDENTIFIER, PAGE_1_PAGE_IDENTIFIER);
        }
    }

    /**
     * Load the address details of Parent Invitee.
     *
     * @param attendanceBean IepMeetingAttendance
     * @param parameterMap HashMap<String,Object>
     * @param student SisStudent
     * @param person Person
     */
    private void loadInviteeAddressDetailsStudent(IepMeetingAttendance attendanceBean,
                                                  HashMap<String, Object> parameterMap,
                                                  SisStudent student,
                                                  Person person) {
        parameterMap.put(PARAM_PERSON_NAME, person.getFirstName() + STRING_SPACE + person.getLastName());
        parameterMap.put(PARAM_ADDRESS_LINE1, STRING_EMPTY);
        parameterMap.put(PARAM_ADDRESS_LINE2, STRING_EMPTY);

        IepTeamMember teamMember = attendanceBean.getTeamMember();
        String title = teamMember.getMemberRoleCode();

        if (title != null && MEMBER_ROLE_STUDENT.equalsIgnoreCase(title)
                || MEMBER_ROLE_PARENT.equalsIgnoreCase(title)) {
            Address mailingAddress = person.getMailingAddress();
            if (mailingAddress == null) {
                mailingAddress = person.getPhysicalAddress();
            }

            if (mailingAddress != null) {
                parameterMap.put(PARAM_ADDRESS_LINE1, mailingAddress.getAddressLine01());
                parameterMap.put(PARAM_ADDRESS_LINE2, mailingAddress.getAddressLine03());
            }
        } else {
            SisSchool school = student.getSchool();
            SisAddress schoolAddress = school.getAddress();

            if (schoolAddress != null) {
                parameterMap.put(PARAM_ADDRESS_LINE1, schoolAddress.getAddressLine01());
                parameterMap.put(PARAM_ADDRESS_LINE2, schoolAddress.getAddressLine03());
            }
        }
    }

    /**
     * Load Invitee Address Details.
     *
     * @param attendanceBean IepMeetingAttendance
     * @param parameterMap HashMap<String,Object>
     */
    private void loadInviteeAddressDetails(IepMeetingAttendance attendanceBean, HashMap<String, Object> parameterMap) {
        SisStudent student = m_currentIep.getStudent();

        parameterMap.put(PARAM_ADDRESS_LINE1, STRING_EMPTY);
        parameterMap.put(PARAM_ADDRESS_LINE2, STRING_EMPTY);

        IepTeamMember teamMember = attendanceBean.getTeamMember();
        String title = teamMember.getMemberRoleCode();

        if (title != null && MEMBER_ROLE_STUDENT.equalsIgnoreCase(title)
                || MEMBER_ROLE_PARENT.equalsIgnoreCase(title)) {
            Address mailingAddress = student.getPerson().getMailingAddress();
            if (mailingAddress == null) {
                mailingAddress = student.getPerson().getPhysicalAddress();
            }

            if (mailingAddress != null) {
                parameterMap.put(PARAM_ADDRESS_LINE1, mailingAddress.getAddressLine01());
                parameterMap.put(PARAM_ADDRESS_LINE2, mailingAddress.getAddressLine03());
            }
        } else {
            SisSchool school = student.getSchool();
            SisAddress schoolAddress = school.getAddress();

            if (schoolAddress != null) {
                parameterMap.put(PARAM_ADDRESS_LINE1, schoolAddress.getAddressLine01());
                parameterMap.put(PARAM_ADDRESS_LINE2, schoolAddress.getAddressLine03());
            }
        }
    }

    /**
     * Load Report Header.
     *
     * @param parameterMap HashMap<String,Object>
     */
    private void loadReportHeader(HashMap<String, Object> parameterMap) {
        SisStudent student = m_currentIep.getStudent();
        String admin1 = STRING_EMPTY;

        parameterMap.put(PARAM_SCHOOL_ADDRESS_LINE1, STRING_EMPTY);
        parameterMap.put(PARAM_SCHOOL_ADDRESS_LINE2, STRING_EMPTY);
        parameterMap.put(PARAM_SCHOOL_PHONE_NO, STRING_EMPTY);
        parameterMap.put(PARAM_SKL_ADMIN1, STRING_EMPTY);
        parameterMap.put(PARAM_SKL_ADMIN2, STRING_EMPTY);
        parameterMap.put(PARAM_SKL_ADMIN3, STRING_EMPTY);

        SisSchool school = student.getSchool();
        String fax = (String) school.getFieldValueByAlias(ALIAS_SCHOOL_FAX);
        parameterMap.put(PARAM_SCHOOL_FAX_NO, !StringUtils.isEmpty(fax) ? FAX + fax : STRING_EMPTY);

        SisAddress schoolAddress = school.getAddress();
        if (schoolAddress != null) {
            if (!StringUtils.isEmpty(schoolAddress.getPhone01()) || !StringUtils.isEmpty(schoolAddress.getPhone02())) {
                parameterMap.put(PARAM_SCHOOL_PHONE_NO, (StringUtils.isEmpty(schoolAddress.getPhone01())
                        ? schoolAddress.getPhone02() : schoolAddress.getPhone01()));
            }

            parameterMap.put(PARAM_SCHOOL_ADDRESS_LINE1, schoolAddress.getAddressLine01());
            parameterMap.put(PARAM_SCHOOL_ADDRESS_LINE2, schoolAddress.getAddressLine03());
        }

        if (school != null) {
            if (school.getAdministrator1() != null) {
                SisPerson adminPerson = school.getAdministrator1().getPerson();
                admin1 = adminPerson.getFirstName() + STRING_SPACE + adminPerson.getLastName();
                parameterMap.put(PARAM_SKL_ADMIN1, admin1);

                String superintendent = (String) school.getAdministrator1().getFieldValueByAlias(ALIAS_DEPT_SUPERVISOR);
                if (superintendent != null) {
                    String[] admin2 = superintendent.split(STRING_COMMA);
                    parameterMap.put(PARAM_SKL_ADMIN2, admin2[1] + STRING_SPACE + admin2[0]);
                }
            }
        }

        initSssName();
        parameterMap.put(PARAM_SSS_STAFF, m_sssStaffName);
    }

    /**
     * Loads each subreport into a map for fast retrieval.
     */
    private void loadSubReports() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, PAGE_1_FORMAT_ID);

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, 8);

        Criteria criteriaStudent = new Criteria();
        criteriaStudent.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteriaStudent.addEqualTo(Report.COL_ID, PAGE_1_FORMAT_ID_STUDENT);

        QueryByCriteria queryStudent = new QueryByCriteria(Report.class, criteriaStudent);

        m_subReportsStudent = getBroker().getMapByQuery(queryStudent, Report.COL_ID, 8);
    }

}
