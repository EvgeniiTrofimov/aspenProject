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
 * New Jersey SPED form for Invitation to an Identification Meeting.
 * 
 * @author Follett Software Company
 *
 */
public class InvitationToIdentificationMeetingMainData extends BaseFormReportJavaSource {
    private static final String AGE_CATEGORY_O18 = "AgeCategoryO18";
    private static final String AGE_CATEGORY_U18 = "AgeCategoryU18";
    private static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    private static final String ALIAS_MTG_INVITATION_DATE = "meeting-invitation-date";

    /**
     * Aliases
     */
    private static final String ALIAS_OTHER_CONTACT_PERSON = "sped-other-contact-person";
    private static final String ALIAS_SCHOOL_FAX = "DOE SCHOOL FAX";
    private static final String ALIAS_STAFF_ROLE = "DOE STAFF ROLE";
    private static final String ALIAS_STAFF_WORK_PHONE = "DOE STAFF WORK PHONE";

    private static final String COL_PAGE_IDENTIFIER = "pageIdentifier";
    private static final String COL_PAGE_NUMBER = "pageNumber";
    private static final String COL_PARAMETER_MAP = "parameters";
    private static final String COL_SUBREPORT_DATA_SOURCE = "datasource";
    private static final String COL_SUBREPORT_FORMAT = "format";

    private static final String EMPTY_STRING = "";

    private static final String EXTN_WORK_PHONE = "(908) 284-";

    private static final String FAX = "FAX";

    private static final String INVITEE_DELIMITER = " - ";

    private static final int MAX_REPORT_COL_LIMIT = 7;

    private static final String MEETING_FORM_DEF_ID = "INVITE-IDENT-MTG";

    private static final String MEMBER_ROLE_PARENT = "Parent/Guardian";

    private static final String PAGE_1_FORMAT_ID = "SYS-SPED-NJ-IDENT-P1";

    private static final String PARAM_ADDRESS_LINE1 = "ADDRESS_LINE1";
    private static final String PARAM_ADDRESS_LINE2 = "ADDRESS_LINE2";
    private static final String PARAM_AGE_CATEGORY = "AGE_CATEGORY";
    private static final String PARAM_BUSINESS_PHONE_NO = "BUSINESS_PHONE_NO";
    private static final String PARAM_CHAIR_PERSON = "CHAIR_PERSON";
    private static final String PARAM_FORM_DATE = "FORM_DATE";
    private static final String PARAM_INVITED_TEAM_MBRS = "INVITED_TEAM_MBRS";
    private static final String PARAM_INVITED_TEAM_MBRS2 = "INVITED_TEAM_MBRS2";
    private static final String PARAM_MEET_DATE = "MEET_DATE";
    private static final String PARAM_MEETING_DATE = "MEETING_DATE";
    private static final String PARAM_MEETING_LOCATION = "MEETING_LOCATION";
    private static final String PARAM_MEETING_TIME = "MEETING_TIME";
    private static final String PARAM_MEMBER_ROLE = "MEMBER_ROLE";
    private static final String PARAM_OTHER_CONTACT_PERSON = "OTHER_CONTACT_PERSON";
    private static final String PARAM_PERSON_NAME = "PERSON_NAME";
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
    private static final String PARAM_STUDENT_FIRST_NAME = "STUDENT_FIRST_NAME";
    private static final String PARAM_TO = "to";

    private static final String REFCODE_PARENT = "Parent/Guardian";
    private static final String REFCODE_STUDENT = "Student";

    private static final String STRING_COLON = ":";
    private static final String STRING_COMMA = ",";
    private static final String STRING_NEW_LINE = "\n";
    private static final String STRING_SPACE = " ";

    private static final String TITLE_CASE_MANAGER = "Case Manager";

    private final SimpleDateFormat TIME_FORMATTER = new SimpleDateFormat("h:mm a");
    private final SimpleDateFormat DATE_FORMATTER_LONG = new SimpleDateFormat("MMMMM dd, yyyy");
    private final SimpleDateFormat DATE_FORMATTER = new SimpleDateFormat("MM/dd/yyyy");

    /**
     * Instances
     */
    private IepData m_currentIep = null;
    private IepMeeting m_currentMeeting = null;
    private int m_currentPageNumber = 0;
    private IepMeeting m_meeting = null;
    private PlainDate m_responseDate = new PlainDate();
    private String m_sssStaffName;
    private Map m_subReports = null;

    /**
     * This method builds the entire report.
     *
     * @return Object
     * @throws Exception exception
     */
    @Override
    protected Object gatherData() throws Exception {
        IepData iep = (IepData) getFormOwner();
        SisStudent sisStudent = iep.getStudent();
        String studentsName = EMPTY_STRING;
        studentsName = sisStudent.getNameView();

        if (studentsName != null) {
            String delims = STRING_COMMA;
            String[] tokens = studentsName.split(delims);
            String fName = tokens[1];
            String lName = tokens[0];
            studentsName = fName + STRING_SPACE + lName;
        }

        addParameter(PARAM_STUDENT_NAME, studentsName);

        ReportDataGrid grid = new ReportDataGrid();
        loadSubReports();
        if (m_currentMeeting != null) {
            setFormStorage(m_currentMeeting);
            setFormOwner(m_currentMeeting.getIepData());
        }
        m_meeting = (IepMeeting) getFormStorage();

        if (isBlank()) {
            preparePage1(grid, new IepMeetingAttendance(getBroker().getPersistenceKey()), EMPTY_STRING, EMPTY_STRING);
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
            attendanceCriteria.addEqualTo(IepMeetingAttendance.COL_IEP_MEETING_OID, m_meeting.getOid());
            attendanceCriteria.addEqualTo(IepMeetingAttendance.COL_EXCUSED_INDICATOR, Boolean.valueOf(false));

            QueryByCriteria attendanceQuery = new QueryByCriteria(IepMeetingAttendance.class, attendanceCriteria);
            attendanceQuery.addOrderByAscending(
                    IepMeetingAttendance.REL_TEAM_MEMBER + ModelProperty.PATH_DELIMITER + IepTeamMember.COL_NAME_VIEW);

            Collection<IepMeetingAttendance> attendanceMembers = getBroker().getCollectionByQuery(attendanceQuery);
            StringBuilder inviteeString = inviteeStringColumn1;
            String parentAddress = EMPTY_STRING;
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
                                        inviteeString.append(memberName);
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
                        inviteeString.append(memberName);
                        inviteeString.append(INVITEE_DELIMITER);
                    }

                    inviteeString.append(teamMember.getMemberRoleCode());
                    inviteeString.append(STRING_NEW_LINE);

                    attendanceList.add(attendanceBean);
                }
            }
            for (IepMeetingAttendance attendanceBean : attendanceList) {
                preparePage1(grid, attendanceBean, inviteeStringColumn1.toString(), inviteeStringColumn2.toString());
            }
        }
        if (grid.rowCount() == 0) {
            grid.append();
        }

        grid.beforeTop();

        return grid;
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
        String businessPhoneNo = EMPTY_STRING;
        addParameter(PARAM_CHAIR_PERSON, EMPTY_STRING);
        SisPerson chairPerson = null;
        SisStaff caseManager = m_currentIep.getStaff();
        String title = null;
        Collection<IepTeamMember> teamMbrs = m_currentIep.getTeamMembers();
        for (IepTeamMember teamMbr : teamMbrs) {
            if (teamMbr.getChairpersonIndicator()) {
                chairPerson = teamMbr.getPerson();
                title = teamMbr.getMemberRoleCode();
                businessPhoneNo =
                        EXTN_WORK_PHONE + (String) caseManager.getFieldValueByAlias(ALIAS_STAFF_WORK_PHONE, dictionary);
                break;
            }
        }

        if (chairPerson == null) {

            if (caseManager != null) {
                chairPerson = caseManager.getPerson();
                title = TITLE_CASE_MANAGER;
                businessPhoneNo =
                        EXTN_WORK_PHONE + (String) caseManager.getFieldValueByAlias(ALIAS_STAFF_WORK_PHONE, dictionary);
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
     * Load the address details of Parent Invitee.
     *
     * @param attendanceBean IepMeetingAttendance
     * @param parameterMap HashMap<String,Object>
     * @param student SisStudent
     * @param person Person
     */
    private void loadInviteeAddressDetails(IepMeetingAttendance attendanceBean,
                                           HashMap<String, Object> parameterMap,
                                           SisStudent student,
                                           Person person) {
        String personName = EMPTY_STRING;
        if (person != null) {
            personName = person.getFirstName() + STRING_SPACE + person.getLastName();
        }
        parameterMap.put(PARAM_PERSON_NAME, personName);
        parameterMap.put(PARAM_ADDRESS_LINE1, EMPTY_STRING);
        parameterMap.put(PARAM_ADDRESS_LINE2, EMPTY_STRING);

        String title = attendanceBean.getTeamMember().getMemberRoleCode();

        if (title != null && REFCODE_STUDENT.equalsIgnoreCase(title) || REFCODE_PARENT.equalsIgnoreCase(title)) {
            Address mailingAddress = person.getMailingAddress();

            if (null == mailingAddress) {
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
     * Load the title data of the report.
     *
     * @param parameterMap HashMap<String,Object>
     */

    private void loadReportHeader(HashMap<String, Object> parameterMap) {
        SisStudent student = m_currentIep.getStudent();
        String admin1 = EMPTY_STRING;

        parameterMap.put(PARAM_SCHOOL_ADDRESS_LINE1, EMPTY_STRING);
        parameterMap.put(PARAM_SCHOOL_ADDRESS_LINE2, EMPTY_STRING);
        parameterMap.put(PARAM_SCHOOL_PHONE_NO, EMPTY_STRING);
        parameterMap.put(PARAM_SKL_ADMIN1, EMPTY_STRING);
        parameterMap.put(PARAM_SKL_ADMIN2, EMPTY_STRING);
        parameterMap.put(PARAM_SKL_ADMIN3, EMPTY_STRING);
        SisSchool school = student.getSchool();
        String fax = (String) school.getFieldValueByAlias(ALIAS_SCHOOL_FAX);
        parameterMap.put(PARAM_SCHOOL_FAX_NO, !StringUtils.isEmpty(fax) ? FAX + STRING_COLON + fax : EMPTY_STRING);
        SisAddress schoolAddress = school.getAddress();
        if (schoolAddress != null) {
            if (!StringUtils.isEmpty(schoolAddress.getPhone01()) ||
                    !StringUtils.isEmpty(schoolAddress.getPhone02())) {
                parameterMap.put(PARAM_SCHOOL_PHONE_NO, (StringUtils.isEmpty(schoolAddress.getPhone01())
                        ? schoolAddress.getPhone02() : schoolAddress.getPhone01()));
            }
            parameterMap.put(PARAM_SCHOOL_ADDRESS_LINE1, schoolAddress.getAddressLine01());
            parameterMap.put(PARAM_SCHOOL_ADDRESS_LINE2, schoolAddress.getAddressLine03());
        }

        if (null != school.getAdministrator1()) {
            SisPerson adminPerson1 = school.getAdministrator1().getPerson();
            admin1 = adminPerson1.getFirstName() + STRING_SPACE + adminPerson1.getLastName();
            parameterMap.put(PARAM_SKL_ADMIN1, admin1);
            String superintendent = (String) school.getAdministrator1().getFieldValueByAlias(ALIAS_DEPT_SUPERVISOR);
            if (null != superintendent) {
                String[] admin2 = superintendent.split(STRING_COMMA);
                parameterMap.put(PARAM_SKL_ADMIN2, admin2[1] + STRING_SPACE + admin2[0]);
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
    }


    /**
     * Prepares the first page.
     *
     * @param grid ReportDataGrid
     * @param attendanceBean IepMeetingAttendance
     * @param invitees1 String
     * @param invitees2 String
     */
    private void preparePage1(ReportDataGrid grid,
                              IepMeetingAttendance attendanceBean,
                              String invitees1,
                              String invitees2) {
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
            parameterMap.put(PARAM_MEETING_DATE, EMPTY_STRING);
            parameterMap.put(PARAM_MEETING_TIME, EMPTY_STRING);
            parameterMap.put(PARAM_RESPONSE_DATE, EMPTY_STRING);
            if (meetingDate != null) {
                age = Person.getAgeAsOfDate(dateOfBirth, meetingDate);
                Calendar calendar = Calendar.getInstance();
                calendar.setTime(meetingDate);
                calendar.add(Calendar.DATE, -2);
                m_responseDate = DateUtils
                        .getDate((String) m_meeting.getFieldValueByAlias("iep-sped-meeting-contactdate", dictionary));

                parameterMap.put(PARAM_MEETING_DATE, DATE_FORMATTER.format(meetingDate));
                parameterMap.put(PARAM_MEETING_TIME, TIME_FORMATTER.format(m_meeting.getTime()));
                parameterMap.put(PARAM_RESPONSE_DATE, DATE_FORMATTER_LONG.format(m_responseDate));
            }
            PlainDate invitationDate =
                    DateUtils.getDate((String) m_meeting.getFieldValueByAlias(ALIAS_MTG_INVITATION_DATE, dictionary));
            parameterMap.put(PARAM_TO, attendanceBean.getTeamMember());
            parameterMap.put(PARAM_AGE_CATEGORY, (age < 18) ? AGE_CATEGORY_U18 : AGE_CATEGORY_O18);
            addParameter(PARAM_MEET_DATE, DATE_FORMATTER_LONG.format(invitationDate));
            parameterMap.put(PARAM_FORM_DATE, DATE_FORMATTER_LONG.format(invitationDate));
            parameterMap.put(PARAM_STUDENT_NAME,
                    student.getPerson().getFirstName() + " " + student.getPerson().getLastName());
            parameterMap.put(PARAM_STUDENT_FIRST_NAME, student.getPerson().getFirstName());
            parameterMap.put(PARAM_SCHOOL_NAME, schoolName);
            parameterMap.put(PARAM_MEETING_LOCATION, m_meeting.getLocation());
            parameterMap.put(PARAM_OTHER_CONTACT_PERSON,
                    m_meeting.getFieldValueByAlias(ALIAS_OTHER_CONTACT_PERSON, dictionary));
            parameterMap.put(PARAM_INVITED_TEAM_MBRS, invitees1);
            parameterMap.put(PARAM_INVITED_TEAM_MBRS2, invitees2);

            loadReportHeader(parameterMap);
            loadInviteeAddressDetails(attendanceBean, parameterMap, student, person);
            grid.append();
            JRDataSource dataSource =
                    new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
            grid.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
            grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_1_FORMAT_ID));
            grid.set(COL_PARAMETER_MAP, parameterMap);
            grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
            grid.set(COL_PAGE_IDENTIFIER, "P1");
        }
    }
}
