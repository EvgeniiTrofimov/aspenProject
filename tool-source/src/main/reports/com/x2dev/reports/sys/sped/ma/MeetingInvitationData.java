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
package com.x2dev.reports.sys.sped.ma;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Contact;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.procedures.sys.sped.ma.MaSpedAttribHelper;
import com.x2dev.procedures.sys.sped.ma.MaSpedAttribHelper.MaSpedCollectionDataSource;
import com.x2dev.procedures.sys.sped.ma.MaSpedAttribHelper.MaSpedDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepMeetingAttendance;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the Massachusetts IEP meeting invitation form. This class prepares a
 * ReportDataGrid that contains a row for each section of the report. Each row contains a format and
 * a java source for the corresponding section. The following are the sections prepared:
 * <p>
 * <table border="1">
 * <tr>
 * <th>Section</th>
 * <th>Java source description</th>
 * </tr>
 * <tr>
 * <td>Invitation letter</td>
 * <td>SimpleBeanDataSource on the meeting bean</td>
 * </tr>
 * <tr>
 * <td>Attendance sheet</td>
 * <td>QueryIteratorDataSource on attendance beans; this is to populate the attendance sheet
 * grid</td>
 * </tr>
 * <tr>
 * <td>Waiver form</td>
 * <td>SimpleBeanDataSource on the meeting bean</td>
 * </tr>
 * </table>
 * <p>
 * A letter and attendance sheet is printed for each team member who is both invited and designated
 * to receive an invitation. A waiver is printed for each team member who is either not invited or
 * excused.
 * <p>
 * Special handling exists for printing blank invitations. In this case, a single copy of each
 * format is included. The attendance sheet is configured to print several blank lines on the
 * team members table.
 *
 * @author X2 Development Corporation
 */
public class MeetingInvitationData extends BaseFormReportJavaSource {

    /**
     *
     */
    @SuppressWarnings("unused")
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    private static final long serialVersionUID = 1L;

    public static final String ALIAS_ADDTL_ATTENDEE_NAME = "ima-additionalName";

    // Constants for the letter (N3I) portion of the report
    public static final String CONTACT_LINE = "contact";
    public static final String ADDRESS_LINE_1 = "addressLine1";
    public static final String ADDRESS_LINE_2 = "addressLine2";
    public static final String PARENT_LANGUAGE = "parentLanguage";
    public static final String TRANSLATION_SUPPORT = "translate";

    // Constants for the waiver (N3W) portion of the report
    public static final String TEAM_MEMBER_PARAMETER = "teamMember";
    public static final String WAIVER_TYPE_PARAMETER = "waiverType";

    public static final String WAIVER_TYPE_EXCUSED = "excused";
    public static final String WAIVER_TYPE_NOT_INVITED = "notInvited";

    // ReportDataGrid column constants for the main report
    private static final String COL_SUBREPORT_DATA_SOURCE = "datasource";
    private static final String COL_SUBREPORT_FORMAT = "format";
    private static final String COL_PAGE_NUMBER = "pageNumber";
    private static final String COL_PAGE_IDENTIFIER = "pageIdentifier";
    private static final String COL_PARAMETER_MAP = "parameters";

    private static final String INCLUDE_MEETING_NOTES = "includeMtgNotes";
    private static final String PARAM_SCHOOL = "school";
    private static final String PARAM_GRADE = "grade";
    private static final String PARAM_YOG = "yog";
    private static final String PAGE_1_FORMAT_ID = "SYS-SPED-MA-N3I";
    private static final String PAGE_2_FORMAT_ID = "SYS-SPED-MA-N3A";
    private static final String PAGE_3_FORMAT_ID = "SYS-SPED-MA-N3N";
    private static final String PRINT_BLANK_MEETING_WAIVER_ONLY = "printBlankWaiver";
    private static final String PRINT_MEETING_NOTES_ONLY = "printMtgNotesOnly";
    private static final String WAIVER_FORMAT_ID = "SYS-SPED-MA-N3W";

    private MaSpedAttribHelper m_attribHelper;
    private int m_currentPageNumber = 0;
    private Map m_subReports = null;

    private List<IepTeamMember> m_parentTeamMemberList;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        m_attribHelper = new MaSpedAttribHelper(getBroker(), true);
        ReportDataGrid grid = new ReportDataGrid();
        loadSubReports();

        IepMeeting meeting = (IepMeeting) getFormStorage();
        m_parentTeamMemberList = getTeamMembers(meeting, 2);
        if ((Boolean) getParameter(PRINT_MEETING_NOTES_ONLY)) {
            preparePage3(grid);
        } else if ((Boolean) getParameter(PRINT_BLANK_MEETING_WAIVER_ONLY)) {
            prepareWaiver(grid, new IepTeamMember(null), new HashMap(0));
        } else {
            if (isBlank() || getFormOwner() == null) {
                if (m_subReports.containsKey(getJob().getTool().getId())) {
                    if (getJob().getTool().getId().equals(PAGE_1_FORMAT_ID)) {
                        preparePage1(grid, new IepMeetingAttendance(getBroker().getPersistenceKey()), "");
                    } else if (getJob().getTool().getId().equals(PAGE_2_FORMAT_ID)) {
                        preparePage2(grid);
                    } else if (getJob().getTool().getId().equals(PAGE_3_FORMAT_ID)
                            && (Boolean) getParameter(INCLUDE_MEETING_NOTES)) {
                        preparePage3(grid);
                    } else if (getJob().getTool().getId().equals(WAIVER_FORMAT_ID)) {
                        prepareWaiver(grid, new IepTeamMember(getBroker().getPersistenceKey()), new HashMap(0));
                    }
                } else {
                    preparePage1(grid, new IepMeetingAttendance(getBroker().getPersistenceKey()), "");
                    preparePage2(grid);
                    if ((Boolean) getParameter(INCLUDE_MEETING_NOTES)) {
                        preparePage3(grid);
                    }
                    prepareWaiver(grid, new IepTeamMember(getBroker().getPersistenceKey()), new HashMap(0));
                }
            } else {
                /*
                 * Add invitations
                 */
                Criteria attendanceCriteria = new Criteria();
                attendanceCriteria.addEqualTo(IepMeetingAttendance.COL_IEP_MEETING_OID, meeting.getOid());
                attendanceCriteria.addEqualTo(IepMeetingAttendance.COL_EXCUSED_INDICATOR, Boolean.valueOf(false));

                QueryByCriteria attendanceQuery = new QueryByCriteria(IepMeetingAttendance.class, attendanceCriteria);
                attendanceQuery
                        .addOrderByAscending(IepMeetingAttendance.REL_TEAM_MEMBER + "." + IepTeamMember.COL_NAME_VIEW);

                Map<String, IepMeetingAttendance> meetingAttendanceMap = getBroker()
                        .getMapByQuery(attendanceQuery, IepMeetingAttendance.COL_TEAM_MEMBER_OID, 10);

                boolean hasParentsSameAddress = false;
                boolean oneParentSelected = false;
                if (m_parentTeamMemberList.size() == 2) {
                    hasParentsSameAddress =
                            hasParentsSameAddress(m_parentTeamMemberList.get(0).getPerson(),
                                    m_parentTeamMemberList.get(1).getPerson());
                }

                for (IepMeetingAttendance attendanceBean : meetingAttendanceMap.values()) {
                    /*
                     * We check the print invitation indicator here instead of filtering it out of
                     * the query above since the query is also used below to generate waivers
                     */
                    if (attendanceBean.getPrintInvitationIndicator()) {
                        if (isParentTeamMember(attendanceBean.getTeamMemberOid()) && hasParentsSameAddress
                                && !oneParentSelected) {
                            preparePage1(grid, attendanceBean, getParentNames());
                            preparePage2(grid);
                            if ((Boolean) getParameter(INCLUDE_MEETING_NOTES)) {
                                preparePage3(grid);
                            }
                            oneParentSelected = true;
                        }
                        if (!(isParentTeamMember(attendanceBean.getTeamMemberOid()) && oneParentSelected)) {
                            preparePage1(grid, attendanceBean, attendanceBean.getTeamMember().getNameView());
                            preparePage2(grid);
                            if ((Boolean) getParameter(INCLUDE_MEETING_NOTES)) {
                                preparePage3(grid);
                            }
                        }
                    }
                }


                /*
                 * Add waivers
                 */

                Criteria excusalCriteria = new Criteria();
                excusalCriteria.addEqualTo(IepMeetingAttendance.COL_IEP_MEETING_OID, meeting.getOid());
                excusalCriteria.addEqualTo(IepMeetingAttendance.COL_EXCUSED_INDICATOR, Boolean.valueOf(true));

                QueryByCriteria excusalQuery = new QueryByCriteria(IepMeetingAttendance.class, excusalCriteria);

                Map<String, IepMeetingAttendance> excusalMap =
                        getBroker().getMapByQuery(excusalQuery, IepMeetingAttendance.COL_TEAM_MEMBER_OID, 16);

                attendanceCriteria = new X2Criteria();
                attendanceCriteria.addEqualTo(IepMeetingAttendance.COL_IEP_MEETING_OID, meeting.getOid());
                attendanceCriteria.addEqualTo(IepMeetingAttendance.COL_EXCUSED_INDICATOR, Boolean.FALSE);
                attendanceCriteria.addNotNull(IepMeetingAttendance.COL_TEAM_MEMBER_OID);

                Criteria waiverCriteria = new Criteria();
                waiverCriteria.addEqualTo(IepTeamMember.COL_IEP_DATA_OID, meeting.getIepDataOid());
                waiverCriteria.addNotIn(X2BaseBean.COL_OID, new SubQuery(IepMeetingAttendance.class,
                        IepMeetingAttendance.COL_TEAM_MEMBER_OID, attendanceCriteria));

                QueryByCriteria waiverQuery = new QueryByCriteria(IepTeamMember.class, waiverCriteria);
                waiverQuery.addOrderByAscending(IepTeamMember.COL_NAME_VIEW);

                QueryIterator waivedTeamMembers = getBroker().getIteratorByQuery(waiverQuery);
                try {
                    while (waivedTeamMembers.hasNext()) {
                        IepTeamMember teamMember = (IepTeamMember) waivedTeamMembers.next();
                        prepareWaiver(grid, teamMember, excusalMap);
                    }
                } finally {
                    waivedTeamMembers.close();
                }
            }
        }

        grid.beforeTop();

        return grid;
    }

    /**
     * Gets the city and zip.
     *
     * @param contact StudentContact
     * @return String
     */
    private String getCityAndZip(SisAddress address) {
        String cityAndZip = "";
        if (address != null) {
            cityAndZip = (StringUtils.isEmpty(address.getCity()) ? "" : address.getCity() + ", ")
                    + StringUtils.unNullify(address.getState()) + " "
                    + StringUtils.unNullify(address.getPostalCode());
        }
        return cityAndZip;
    }

    private String getParentNames() {
        String parentNames = "";

        for (IepTeamMember parentTeamMember : m_parentTeamMemberList) {
            parentNames += parentTeamMember.getNameView() + " and ";
        }
        parentNames = parentNames.substring(0, (parentNames.length() - 4));

        return parentNames;
    }

    /**
     * Return team members which also are student contacts
     * Team members sorted by STUDENT_CONTACT.COL_FORM_PRIORITY and limited by maxContacts param
     *
     * @param iep
     * @param maxMembers
     * @return
     */
    private List<IepTeamMember> getTeamMembers(IepMeeting meeting, int maxMembers) {
        List<IepTeamMember> teamMemberList = new ArrayList<IepTeamMember>();
        /*
         * Load the student contact team members with form priority ordered by form priority
         */
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(
                IepTeamMember.REL_MEETING_ATTENDANCE + PATH_DELIMITER + IepMeetingAttendance.COL_IEP_MEETING_OID,
                meeting.getOid());

        criteria.addNotEmpty(IepTeamMember.COL_FORM_PRIORITY, getBroker().getPersistenceKey());
        criteria.addEqualTo(IepTeamMember.REL_PERSON + PATH_DELIMITER + SisPerson.REL_CONTACT + PATH_DELIMITER
                + Contact.REL_STUDENT_CONTACTS + PATH_DELIMITER + StudentContact.COL_STUDENT_OID,
                meeting.getStudentOid());

        QueryByCriteria query = new QueryByCriteria(IepTeamMember.class, criteria);
        query.addOrderByAscending(IepTeamMember.COL_FORM_PRIORITY);
        teamMemberList.addAll(getBroker().getCollectionByQuery(query));

        if (maxMembers == 0 || teamMemberList.size() < maxMembers) {
            /*
             * Load additional team members without form priority but with
             * STUDENT_CONTACTS.EMERGENCY_PRIORITY ordered by
             * emergency priority
             */
            criteria = new X2Criteria();

            criteria.addEqualTo(
                    IepTeamMember.REL_MEETING_ATTENDANCE + PATH_DELIMITER + IepMeetingAttendance.COL_IEP_MEETING_OID,
                    meeting.getOid());
            criteria.addEmpty(IepTeamMember.COL_FORM_PRIORITY, getBroker().getPersistenceKey());
            criteria.addEqualTo(IepTeamMember.REL_PERSON + PATH_DELIMITER + SisPerson.REL_CONTACT + PATH_DELIMITER
                    + Contact.REL_STUDENT_CONTACTS + PATH_DELIMITER + StudentContact.COL_STUDENT_OID,
                    meeting.getStudentOid());
            query = new QueryByCriteria(IepTeamMember.class, criteria);

            query.addOrderByAscending(IepTeamMember.REL_PERSON + PATH_DELIMITER + SisPerson.REL_CONTACT
                    + PATH_DELIMITER
                    + Contact.REL_STUDENT_CONTACTS + PATH_DELIMITER + StudentContact.COL_EMERGENCY_PRIORITY);
            teamMemberList.addAll(getBroker().getCollectionByQuery(query));
        }

        if (maxMembers != 0 && teamMemberList.size() > maxMembers) {
            teamMemberList = teamMemberList.subList(0, maxMembers);
        }

        return teamMemberList;
    }

    /**
     * Gets the school by date.
     *
     * @param student SisStudent
     * @param date PlainDate
     * @param defaultIsCurrent boolean
     * @return Sis school
     */
    private SisSchool getSchoolByDate(SisStudent student, PlainDate date, boolean defaultIsCurrent) {
        SisSchool returnSchool = null;
        EnrollmentManager manager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        List<StudentEnrollment> enrollments = (manager.getOrderedEnrollment(student, null, date, null, false));
        StudentEnrollment enrollmentOnDate = null;
        for (StudentEnrollment enrollment : enrollments) {
            // if near enrollment is withdrawal and enrollment date before "date" param it mean that
            // student is not attend enrollment school on "date" param
            if (enrollment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)
                    && !date.equals(enrollment.getEnrollmentDate())) {
                break;
            }
            enrollmentOnDate = enrollment;
            break;
        }

        // situation when student han't enrollment before "date" param. Check that next enrollment
        // is W. It can be if E record is missed.
        if (enrollmentOnDate == null) {
            enrollments = (manager.getOrderedEnrollment(student, date, null, null, true));
            for (StudentEnrollment enrollment : enrollments) {
                if (enrollment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)
                        && !date.equals(enrollment.getEnrollmentDate())) {
                    enrollmentOnDate = enrollment;
                }
                break;
            }
        }

        if (enrollmentOnDate == null && defaultIsCurrent) {
            returnSchool = student.getSchool();
        } else {
            returnSchool = enrollmentOnDate.getSchool();
        }

        return returnSchool;
    }

    /**
     * Gets the attendee name.
     *
     * @param attendance IepMeetingAttendance
     * @return String
     */
    private String getAttendeeName(IepMeetingAttendance attendance) {
        String addtlAttName = null;
        IepTeamMember teamMember = attendance.getTeamMember();
        if (teamMember != null) {
            addtlAttName = teamMember.getNameView();
        } else {
            addtlAttName = (String) attendance.getFieldValueByAlias(ALIAS_ADDTL_ATTENDEE_NAME, getDictionary());
        }
        return addtlAttName == null ? "" : addtlAttName;
    }

    /***
     * Gets the street.
     *
     * @param contact StudentContact
     * @return String
     */
    private String getStreet(SisAddress address) {
        String streetLine = "";
        if (address != null) {
            streetLine = StringUtils.unNullify(address.getAddressLine01())
                    + (StringUtils.isEmpty(address.getAddressLine02()) ? "" : ", " + address.getAddressLine02());
        }

        return streetLine;
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

    private boolean hasParentsSameAddress(SisPerson person1, SisPerson person2) {
        boolean hasParentsSameAddress = false;

        SisAddress personAddress1 = person1.getPhysicalAddress();
        if (!mailingAddressIsIdentical(person1)) {
            personAddress1 = person1.getMailingAddress();
        }

        SisAddress personAddress2 = person2.getPhysicalAddress();
        if (!mailingAddressIsIdentical(person2)) {
            personAddress2 = person2.getMailingAddress();
        }

        if (personAddress1 != null && personAddress2 != null) {
            hasParentsSameAddress = isSameAddress(personAddress1, personAddress2);
        }
        return hasParentsSameAddress;

    }

    private boolean isSameAddress(SisAddress address1, SisAddress address2) {

        String addressHash1 =
                StringUtils.unNullify(address1.getAddressLine01())
                        + StringUtils.unNullify(address1.getAddressLine02())
                        + StringUtils.unNullify(address1.getAddressLine03())
                        + StringUtils.unNullify(address1.getCity())
                        + StringUtils.unNullify(address1.getState())
                        + StringUtils.unNullify(address1.getPostalCode());
        String addressHash2 =
                StringUtils.unNullify(address2.getAddressLine01())
                        + StringUtils.unNullify(address2.getAddressLine02())
                        + StringUtils.unNullify(address2.getAddressLine03())
                        + StringUtils.unNullify(address2.getCity())
                        + StringUtils.unNullify(address2.getState())
                        + StringUtils.unNullify(address2.getPostalCode());

        return addressHash1.equalsIgnoreCase(addressHash2);
    }

    private boolean isParentTeamMember(String teamMemberOid) {
        boolean isParentTeamMember = false;
        for (IepTeamMember parentTeamMember : m_parentTeamMemberList) {
            if (parentTeamMember.getOid().equalsIgnoreCase(teamMemberOid)) {
                isParentTeamMember = true;
                break;
            }
        }

        return isParentTeamMember;
    }

    /**
     * Loads each subreport into a map for fast retrieval.
     */
    private void loadSubReports() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addIn(Report.COL_ID, Arrays.asList(new String[] {PAGE_1_FORMAT_ID,
                PAGE_2_FORMAT_ID,
                PAGE_3_FORMAT_ID,
                WAIVER_FORMAT_ID}));

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, 8);
    }

    /**
     * Check if the mailing address for the person is either identical to the physical address or
     * empty. In either of
     * these cases, return true.
     *
     * @param person SisPerson
     * @return true, if successful
     */
    private boolean mailingAddressIsIdentical(SisPerson person) {
        String physicalAddressOid = person.getPhysicalAddressOid();
        String mailingAddressOid = person.getMailingAddressOid();
        boolean mailingAddressIsIdentical =
                physicalAddressOid == mailingAddressOid || StringUtils.isEmpty(mailingAddressOid);
        return mailingAddressIsIdentical;
    }

    /**
     * Prepares the first page (N3).
     *
     * @param grid ReportDataGrid
     * @param attendanceBean IepMeetingAttendance
     */
    private void preparePage1(ReportDataGrid grid, IepMeetingAttendance attendanceBean, String contact) {
        MaSpedDataSource dataSource =
                m_attribHelper.getMaSpedDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());

        HashMap<String, Object> parameterMap = new HashMap<String, Object>();
        IepTeamMember teamMember = attendanceBean.getTeamMember();
        Set<String> contactPersons = new HashSet();
        if (attendanceBean.getStudent() != null && !StringUtils.isEmpty(attendanceBean.getStudent().getPersonOid())) {
            contactPersons.add(attendanceBean.getStudent().getPersonOid());
        }

        if (attendanceBean.getStudent() != null && attendanceBean.getStudent().getContacts() != null) {
            for (StudentContact ctj : attendanceBean.getStudent().getContacts()) {
                if (ctj.getPerson() != null) {
                    contactPersons.add(ctj.getPerson().getOid());
                }
            }

        }

        if (teamMember != null) {
            String language = null;
            if (teamMember.getPerson() != null && teamMember.getPerson().getContact() != null) {
                if (teamMember.getPerson().getContact().getContactTypeCode()
                        .equals(Contact.CONTACT_TYPE_INDEPENDENT)) {
                    contactPersons.add(teamMember.getPerson().getOid());
                }
                language = (String) teamMember.getPerson().getContact().getFieldValueByAlias("contact-language");
            }

            SisPerson person = teamMember.getPerson();
            if (person != null) {
                SisAddress personAddress = person.getPhysicalAddress();
                if (!mailingAddressIsIdentical(person)) {
                    personAddress = person.getMailingAddress();
                }
                if (contactPersons.contains(person.getOid())) {
                    parameterMap.put(ADDRESS_LINE_1, getStreet(personAddress));
                    parameterMap.put(ADDRESS_LINE_2, getCityAndZip(personAddress));
                }
            }
            parameterMap.put(CONTACT_LINE, contact);
            parameterMap.put(PARENT_LANGUAGE, StringUtils.isEmpty(language) ? null : language);
        }

        IepMeeting meeting = (IepMeeting) getFormStorage();
        IepData iepData = (IepData) getFormOwner();

        if (meeting != null && meeting.getStudent() != null) {
            parameterMap.put(PARAM_SCHOOL, getSchoolByDate(meeting.getStudent(), meeting.getDate(), true));
        } else if (getFormOwner() != null) {
            PlainDate date = iepData.getMeetingDate();
            if (date == null) {
                date = new PlainDate(new Date());
            }
            parameterMap.put(PARAM_SCHOOL, getSchoolByDate(iepData.getStudent(), date, true));
        }

        String gradeLevelAtStart = iepData.getStudent().getGradeLevel();
        int yogAtStart = getYog(iepData.getStudent(), meeting.getDate());
        int schoolYearAtStart = getSchoolYear(meeting.getDate());

        // get grade level on creation time based on meeting date, if not form creation date,
        // on most recent entry enrollment record
        TreeMap<Integer, List<String>> gradeLevels = StudentManager.buildGradeLevelMap(getBroker());
        int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());
        List<String> grades =
                StudentManager.getMatchingGradeLevels(maxGradeLevel, yogAtStart, schoolYearAtStart, gradeLevels);
        if (grades != null && !grades.isEmpty()) {
            gradeLevelAtStart = grades.get(0);
        }

        parameterMap.put(PARAM_YOG, Integer.toString(yogAtStart));
        parameterMap.put(PARAM_GRADE, gradeLevelAtStart);
        
        parameterMap.putAll(getParameters());

        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_1_FORMAT_ID));
        grid.set(COL_PARAMETER_MAP, parameterMap);
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "N3");
    }

    /**
     * Prepares datasource for 'Meeting Attendance/Notes' pages
     *
     * @param meeting
     * @return
     */
    private MaSpedCollectionDataSource getAttendanceDataSource(IepMeeting meeting) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepMeetingAttendance.COL_IEP_MEETING_OID, meeting.getOid());
        criteria.addEqualTo(IepMeetingAttendance.COL_EXCUSED_INDICATOR, Boolean.valueOf(false));

        QueryByCriteria query = new QueryByCriteria(IepMeetingAttendance.class, criteria);

        // query.addOrderByAscending(IepMeetingAttendance.REL_TEAM_MEMBER + "." +
        // IepTeamMember.COL_NAME_VIEW);

        List<IepMeetingAttendance> attendance;

        if (isBlank()) {
            attendance = new ArrayList<IepMeetingAttendance>(5);

            IepData iep = (IepData) getFormOwner();

            IepMeetingAttendance blankAttendance = new IepMeetingAttendance(getBroker().getPersistenceKey());
            if (iep != null) {
                blankAttendance.setStudentOid(iep.getStudentOid());
                blankAttendance.setIepDataOid(iep.getOid());
            }

            for (int i = 0; i < 10; i++) {
                attendance.add(blankAttendance);
            }
        } else {
            attendance = new ArrayList(getBroker().getCollectionByQuery(query));
            Collections.sort(attendance, new Comparator<IepMeetingAttendance>() {

                @Override
                public int compare(IepMeetingAttendance o1, IepMeetingAttendance o2) {
                    String attendeeName1 = getAttendeeName(o1);
                    String attendeeName2 = getAttendeeName(o2);
                    int result = attendeeName1.compareTo(attendeeName2);
                    if (StringUtils.isEmpty(attendeeName2)) {
                        result = -1;
                    }
                    return result;
                }

            });
        }

        return m_attribHelper.getMaSpedCollectionDataSource(getFormOwner(), attendance, getDictionary(), getLocale());
    }



    /**
     * Prepares the second page (N3 A).
     *
     * @param grid ReportDataGrid
     */
    private void preparePage2(ReportDataGrid grid) {
        IepMeeting meeting = (IepMeeting) getFormStorage();
        MaSpedCollectionDataSource dataSource = getAttendanceDataSource(meeting);

        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_2_FORMAT_ID));
        grid.set(COL_PARAMETER_MAP, getParameters());
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "N3 A");
    }

    /**
     * Prepares the third page (N3 N).
     *
     * @param grid ReportDataGrid
     */
    private void preparePage3(ReportDataGrid grid) {
        IepMeeting meeting = (IepMeeting) getFormStorage();
        addParameter("mtgNotes", meeting.getFieldD002());
        MaSpedCollectionDataSource dataSource = getAttendanceDataSource(meeting);

        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_3_FORMAT_ID));
        grid.set(COL_PARAMETER_MAP, getParameters());
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "N3 N");
    }


    /**
     * Prepares a waiver page.
     *
     * @param grid ReportDataGrid
     * @param teamMember IepTeamMember
     * @param excusalMap Map<String,IepMeetingAttendance>
     */
    private void prepareWaiver(ReportDataGrid grid,
                               IepTeamMember teamMember,
                               Map<String, IepMeetingAttendance> excusalMap) {
        HashMap<String, Object> parameterMap = new HashMap<String, Object>();
        parameterMap.put(TEAM_MEMBER_PARAMETER, teamMember);
        parameterMap.put(WAIVER_TYPE_PARAMETER,
                excusalMap.containsKey(teamMember.getOid()) ? WAIVER_TYPE_EXCUSED : WAIVER_TYPE_NOT_INVITED);
        parameterMap.putAll(getParameters());

        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE,
                m_attribHelper.getMaSpedDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale()));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(WAIVER_FORMAT_ID));
        grid.set(COL_PARAMETER_MAP, parameterMap);
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "N3W");
    }

    /**
     * Gets the yog for the student on a particular date
     *
     * @param student the student
     * @param startDate the start date
     * @return the yog
     */
    private int getYog(SisStudent student, PlainDate startDate) {
        int yog = student.getYog();
        if (startDate != null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());
            criteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
            criteria.addGreaterOrEqualThan(StudentEnrollment.COL_YOG, Integer.valueOf(0));
            BeanQuery query = new BeanQuery(StudentEnrollment.class, criteria);
            query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
            query.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);
            try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
                if (iterator.hasNext()) {
                    StudentEnrollment enr = (StudentEnrollment) iterator.next();
                    if (enr.getYog() > 0) {
                        yog = enr.getYog();
                    }
                }
            }
        }
        return yog;
    }

    /**
     * Gets the school year for a particular date. If no school year matches, return the most recent
     * school year before the date.
     *
     * @param startDate the start date
     * @return the school year
     */
    private int getSchoolYear(PlainDate startDate) {
        DistrictSchoolYearContext ctx = null;
        if (startDate != null) {
            // get matching CTX
            X2Criteria criteria = new X2Criteria();
            criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, startDate);
            criteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, startDate);
            BeanQuery query = new BeanQuery(DistrictSchoolYearContext.class, criteria);
            try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
                if (iterator.hasNext()) {
                    ctx = (DistrictSchoolYearContext) iterator.next();
                }
            }

            // get latest CTX before date
            if (ctx == null) {
                criteria = new X2Criteria();
                criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, startDate);
                query = new BeanQuery(DistrictSchoolYearContext.class, criteria);
                query.addOrderByDescending(DistrictSchoolYearContext.COL_END_DATE);
                try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
                    if (iterator.hasNext()) {
                        ctx = (DistrictSchoolYearContext) iterator.next();
                    }
                }
            }
        } else {
            ctx = getCurrentContext();
        }
        return ctx.getSchoolYear();
    }
}
