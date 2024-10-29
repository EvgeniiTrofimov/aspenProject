/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.ga;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.Workflow;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.*;
import net.sf.jasperreports3.engine.JRDataSource;
import net.sf.jasperreports3.engine.data.JRBeanCollectionDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the Georgia IEP form.
 *
 * This class prepares parameters for an IEP's many sections:
 *
 * <table border="1" cellpadding="3" cellspacing="0">
 * <tr>
 * <th>Parameter</th>
 * <th>Value</th>
 * </tr>
 * <tr>
 * <td><code>$P{goalFormat}</code></td>
 * <td>Subreport for Goals</td>
 * </tr>
 * <tr>
 * <td><code>$P{objectiveFormat}</code></td>
 * <td>Subreport for Objectives</td>
 * </tr>
 * <tr>
 * <td><code>$P{testingFormat}</code></td>
 * <td>Subreport for Testing Accommodations</td>
 * </tr>
 * <tr>
 * <td><code>$P{servicesFormat}</code></td>
 * <td>Subreport for Services</td>
 * </tr>
 * <tr>
 * <td><code>$P{esyFormat}</code></td>
 * <td>Subreport for ESY Services</td>
 * </tr>
 * <tr>
 * <td><code>$P{eligibilityCategories}</code></td>
 * <td>Comma-delimited string of disabilities</td>
 * </tr>
 * <tr>
 * <td><code>$P{parent}</code></td>
 * <td>IEP's student's highest priority contact</td>
 * </tr>
 * <tr>
 * <td><code>$P{notice}</code></td>
 * <td>IEP Meeting same date as IEP, otherwise most recent</td>
 * </tr>
 * <tr>
 * <td><code>$P{goals}</code></td>
 * <td>IEP Goals</td>
 * </tr>
 * <tr>
 * <td><code>$P{objectives}</code></td>
 * <td>IEP Objectives</td>
 * </tr>
 * <tr>
 * <td><code>$P{testing}</code></td>
 * <td>Student Accommodations (category = Testing)</td>
 * </tr>
 * <tr>
 * <td><code>$P{insideServices}</code></td>
 * <td>IEP Services (mode = Inside)</td>
 * </tr>
 * <tr>
 * <td><code>$P{outsideServices}</code></td>
 * <td>IEP Services (mode = Outside)</td>
 * </tr>
 * <tr>
 * <td><code>$P{esyServices}</code></td>
 * <td>IEP Services (mode = Extended)</td>
 * </tr>
 * <tr>
 * <td><code>$P{instructional_accommodations}</code></td>
 * <td>Comma-delimited string of Student Accommodations (category = Instructional)</td>
 * </tr>
 * <tr>
 * <td><code>$P{classroom_testing_accommodations}</code></td>
 * <td>Comma-delimited string of Student Accommodations (category = Classroom Testing)</td>
 * </tr>
 * <tr>
 * <td><code>$P{supplemental_aids_and_services}</code></td>
 * <td>Comma-delimited string of Student Accommodations (category = Supplemental)</td>
 * </tr>
 * <tr>
 * <td><code>$P{supports_for_school_personnel}</code></td>
 * <td>Comma-delimited string of Student Accommodations (category = Supports)</td>
 * </tr>
 * </table>
 *
 * @author X2 Development Corporation
 */
public class IepFormData extends BaseFormReportJavaSource {

    private static final String FORMAT_ID_ESY = "SYS-SPED-GA-ESY";
    private static final String FORMAT_ID_GOALS = "SYS-SPED-GA-GOALS";
    private static final String FORMAT_ID_OBJECTIVES = "SYS-SPED-GA-OBJECTIV";
    private static final String FORMAT_ID_SVCS = "SYS-SPED-GA-SVC";
    private static final String FORMAT_ID_TESTING = "SYS-SPED-GA-TESTING";
    private static final String FORMAT_ID_REQUIRED = "SYS-SPED-GA-IEP-SUB1";
    private static final String FORMAT_ID_ADDITIONAL = "SYS-SPED-GA-IEP-SUB2";

    private static final String FORMAT_REPORT_ESY_PARAM = "esyFormat";
    private static final String FORMAT_REPORT_GOALS_PARAM = "goalFormat";
    private static final String FORMAT_REPORT_OBJS_PARAM = "objectiveFormat";
    private static final String FORMAT_REPORT_REQUIRED_PARAM = "requiredFormat";
    private static final String FORMAT_REPORT_ADDITIONAL_PARAM = "additionalFormat";
    private static final String FORMAT_REPORT_SVCS_PARAM = "servicesFormat";
    private static final String FORMAT_REPORT_TESTING_PARAM = "testingFormat";

    private static final String DATE_FORMAT_PARAM = "reportDateFormat";
    private static final String INCLUDE_ALL_ATTENDEES_PARAM = "includeAllAttendees";
    private static final String INPUT_DATE_FORMAT_PARAM = "inputDateFormat";
    private static final String ORGANIZATION_PARAM = "organization";

    private static final String IEP_ACCOM_CLASSROOM_PARAM = "classroom_testing_accommodations";
    private static final String IEP_ACCOM_INSTRUCT_PARAM = "instructional_accommodations";
    private static final String IEP_ACCOM_SUPPLEMENTAL_PARAM = "supplemental_aids_and_services";
    private static final String IEP_ACCOM_SUPPORTS_PARAM = "supports_for_school_personnel";
    private static final String IEP_STUDENT_AGE_PARAM = "studentAge";
    private static final String IEP_ELIGIBILITY_CATEGORIES_PARAM = "eligibilityCategories";
    private static final String IEP_ESY_SVCS_PARAM = "esyServices";
    private static final String IEP_GOALS_PARAM = "goals";
    private static final String IEP_REQUIRED_MEMBERS = "iepRequiredMembers";
    private static final String IEP_ADDITIONAL_MEMBERS = "iepAdditionalMembers";
    private static final String IEP_IN_SVCS_PARAM = "insideServices";
    private static final String IEP_NOTICE_PARAM = "notice";
    private static final String IEP_OBJS_PARAM = "objectives";
    private static final String IEP_OUT_SVCS_PARAM = "outsideServices";
    private static final String IEP_PARENT_PARAM = "parent";
    private static final String IEP_SCHOOL_NAME_PARAM = "schoolName";
    private static final String IEP_STUDENT_GRADE_LEVEL_PARAM = "gradeLevel";
    private static final String IEP_TESTING_PARAM = "testing";
    private static final String IEP_AMENDMENT_DATE_PARAM = "amendmentDate";
    private static final String IEP_TRANSFER_IEP_INDICATOR = "transferIepIndicator";

    private static final String ID_WORKFLOW_DEFINITION_TRANSFER = "SYS-SPED-TRANS";

    /*
     * Aliases used for IEP Meeting Table
     */
    private static final String ALIAS_REQUIRED_MEMBER = "itm-required-member";

    /*
     * Subreport constants
     */
    private static final String SUBREPORT_NAME_ROLE_FIELD = "nameRole";

    /*
     * Member Variables
     */
    private List<String> m_classroomAccomomdations = new ArrayList<String>(4);
    private IepData m_currentIep = null;
    private Collection<IepService> m_extendedServices = new LinkedList<IepService>();
    private ReportDataGrid m_goalData = new ReportDataGrid();
    private ReportDataGrid m_requiredMembersData = new ReportDataGrid();
    private ReportDataGrid m_AdditionalMembersData = new ReportDataGrid();
    private IepData m_iep;
    private Collection<IepService> m_insideServices = new LinkedList<IepService>();
    private List<String> m_instructionalAccommodations = new ArrayList<String>(4);
    private IepMeeting m_meeting = null;
    private ReportDataGrid m_objectivesData = new ReportDataGrid();
    private Collection<IepService> m_outsideServices = new LinkedList<IepService>();
    private Map<String, ReferenceCode> m_refCodeMap = null;
    private Set<String> m_servicesConsidered = new HashSet<String>();
    private Map<String, Report> m_subReports = null;
    private List<String> m_supplementalAccommodations = new ArrayList<String>(4);
    private List<String> m_supportsAccommodations = new ArrayList<String>(4);
    private ReportDataGrid m_testingAccommodations = new ReportDataGrid();

    /*
     * Minimum number of rows for the team members
     */
    private final int TEAM_MEMBERS_NUMBER = 9;
    /**
     * Count of Subreports
     */
    private final int SUBREPORTS_COUNT = 7;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        loadSubReports();
        addParameter(DATE_FORMAT_PARAM, new SimpleDateFormat("MM/dd/yy"));
        addParameter(INPUT_DATE_FORMAT_PARAM, new SimpleDateFormat("yyyy-MM-dd"));
        addParameter(ORGANIZATION_PARAM, getOrganization());
        addParameter(IEP_AMENDMENT_DATE_PARAM, getAmendmentDate());

        // load testing accommodation reference codes
        List<String> refTableOids =
                Arrays.asList(new String[] {"rtbGaIepPresen", "rtbGaIepRespon", "rtbGaIepSchedu", "rtbGaIepSettin"});
        m_refCodeMap = new HashMap<String, ReferenceCode>();
        Criteria refCodeCriteria = new Criteria();
        refCodeCriteria.addIn(ReferenceCode.REL_REFERENCE_TABLE + "." + X2BaseBean.COL_OID, refTableOids);
        QueryByCriteria refCodesQuery = new QueryByCriteria(ReferenceCode.class, refCodeCriteria);
        m_refCodeMap = getBroker().getMapByQuery(refCodesQuery, ReferenceCode.COL_CODE, 64);

        // get all the student disabilities and combine them into a delimited string
        Collection<String> disabilities = new LinkedList<String>();
        for (IepDisability disability : m_currentIep.getIepDisability(getBroker())) {
            disabilities.add(disability.getDisabilityCode());
        }
        addParameter(IEP_ELIGIBILITY_CATEGORIES_PARAM,
                StringUtils.convertCollectionToDelimitedString(disabilities, ", "));

        // get the highest priority contact (the lower the number, the higher the priority) of the
        // iep's student
        // considers the iep's team members who have contact priorities against the student's
        // contacts
        int teamMemberPriority = Integer.MAX_VALUE;
        Person teamMember = null;
        for (IepTeamMember member : m_currentIep.getTeamMembers()) {
            if (member.getFormPriority() != null && Integer.parseInt(member.getFormPriority()) < teamMemberPriority) {
                teamMemberPriority = Integer.parseInt(member.getFormPriority());
                teamMember = member.getPerson();
            }
        }
        if (teamMember != null) {
            addParameter(IEP_PARENT_PARAM, teamMember);
        }

        // get the student's age as of date of iep start date or referral date
        int age = -1;
        PlainDate iepStartDate = null;
        iepStartDate = m_currentIep.getStartDate();
        if (iepStartDate == null) {
            iepStartDate = m_currentIep.getReferralDate();
        }
        if (iepStartDate != null) {
            age = m_currentIep.getStudent().getPerson().getAgeAsOfDate(iepStartDate);
        }
        if (age != -1) {
            addParameter(IEP_STUDENT_AGE_PARAM, Integer.toString(age));
        }

        // get student's grade level and current school name
        String schoolName = null;
        String gradeLevel = null;
        if (m_currentIep.getStudent() != null) {
            SisStudent student = m_currentIep.getStudent();
            gradeLevel = student.getGradeLevel();
            if (student.getSchool() != null) {
                schoolName = student.getSchool().getName();
            }
        }

        // Get exit date and, if one exists, use it to update the school name
        // and grade level to reflect values found on enrollment at exit date.
        PlainDate iepExitDate = m_currentIep.getExitDate();
        if (iepExitDate != null) {
            // get enrollment as of iepEndDate
            StudentEnrollment enrollment = getStudentEnrollment(iepExitDate);

            if (enrollment != null) {
                // use enrollment to get school name as of iepEndDate
                if (enrollment.getSchool() != null) {
                    schoolName = enrollment.getSchool().getName();
                }

                // use enrollment to get student's grade level as of iepEndDate
                gradeLevel = getGradeLevel(enrollment, iepExitDate);
            }
        }

        // add school name and grade level to report
        addParameter(IEP_SCHOOL_NAME_PARAM, schoolName);
        addParameter(IEP_STUDENT_GRADE_LEVEL_PARAM, gradeLevel);

        // get the iep meeting that matches the iep's date, if not get the most recent one. member
        // attendances depend on that meeting
        m_meeting = getMeeting();
        addParameter(IEP_NOTICE_PARAM, m_meeting);

        // determine if this an IEP of type Transfer, by first retrieving the workflow
        X2Criteria workflowCriteria = new X2Criteria();

        workflowCriteria.addEqualTo(Workflow.COL_OWNER_OID, m_currentIep.getOid());
        QueryByCriteria workflowQuery = new QueryByCriteria(Workflow.class, workflowCriteria);
        Workflow workflow = (Workflow) getBroker().getBeanByQuery(workflowQuery);

        // if there is a workflow associated with the iep, determine if the workflow definition id
        // matches the transfer workflow id. If so set the report indicator to true.
        if (workflow != null) {
            if (ID_WORKFLOW_DEFINITION_TRANSFER.equals(workflow.getWorkflowDefinition().getId())) {
                addParameter(IEP_TRANSFER_IEP_INDICATOR, Boolean.TRUE);
            }
        }

        // load required and additional members to be used in a SubReport
        loadMemberAttendance();
        addParameter(IEP_REQUIRED_MEMBERS, m_requiredMembersData);
        addParameter(IEP_ADDITIONAL_MEMBERS, m_AdditionalMembersData);
        m_requiredMembersData.beforeTop();
        m_AdditionalMembersData.beforeTop();
        addParameter(FORMAT_REPORT_REQUIRED_PARAM, getSubreportFormat(FORMAT_ID_REQUIRED));
        addParameter(FORMAT_REPORT_ADDITIONAL_PARAM, getSubreportFormat(FORMAT_ID_ADDITIONAL));

        // load goals and objectives to be used in a subreport
        loadGoalsAndObjectives();
        addParameter(IEP_GOALS_PARAM, m_goalData);
        addParameter(IEP_OBJS_PARAM, m_objectivesData);
        m_goalData.beforeTop();
        m_objectivesData.beforeTop();
        addParameter(FORMAT_REPORT_GOALS_PARAM, getSubreportFormat(FORMAT_ID_GOALS));
        addParameter(FORMAT_REPORT_OBJS_PARAM, getSubreportFormat(FORMAT_ID_OBJECTIVES));

        // load all accommodations and combine together into strings. testing accommodations will be
        // in a subreport
        loadAccommodations();
        addParameter(IEP_ACCOM_INSTRUCT_PARAM,
                StringUtils.convertCollectionToDelimitedString(m_instructionalAccommodations, "\n\n"));
        addParameter(IEP_ACCOM_CLASSROOM_PARAM,
                StringUtils.convertCollectionToDelimitedString(m_classroomAccomomdations, "\n\n"));
        addParameter(IEP_ACCOM_SUPPLEMENTAL_PARAM,
                StringUtils.convertCollectionToDelimitedString(m_supplementalAccommodations, "\n\n"));
        addParameter(IEP_ACCOM_SUPPORTS_PARAM,
                StringUtils.convertCollectionToDelimitedString(m_supportsAccommodations, "\n\n"));
        addParameter(IEP_TESTING_PARAM, m_testingAccommodations);
        addParameter(FORMAT_REPORT_TESTING_PARAM, getSubreportFormat(FORMAT_ID_TESTING));

        loadServices();
        loadServiceTypes();
        addParameter(IEP_IN_SVCS_PARAM, new JRBeanCollectionDataSource(m_insideServices));
        addParameter(IEP_OUT_SVCS_PARAM, new JRBeanCollectionDataSource(m_outsideServices));
        addParameter(IEP_ESY_SVCS_PARAM, new JRBeanCollectionDataSource(m_extendedServices));
        addParameter(FORMAT_REPORT_SVCS_PARAM, getSubreportFormat(FORMAT_ID_SVCS));
        addParameter(FORMAT_REPORT_ESY_PARAM, getSubreportFormat(FORMAT_ID_ESY));

        SimpleFormDataSource dataSource =
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
        return dataSource;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.reports.BaseFormReportJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        if (getFormInstance() != null || getFormDefinition() != null) {
            super.initialize();
        } else if (m_currentIep != null) {
            setFormOwner(m_currentIep);
            setFormStorage(m_currentIep);
            setDictionary(DataDictionary.getDistrictDictionary(m_currentIep.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey()));

            addFormParameters();
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.x2dev.sis.tools.reports.BaseFormReportJavaSource#saveState(com.x2dev.sis.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);

        m_currentIep = userData.getCurrentRecord(IepData.class);
    }

    /**
     * Add a testing accommodation. Replace the reference codes to use their description.
     *
     * @param accommodation the accommodation to add
     */
    private void addTestingAccommodation(IepAccommodation accommodation) {
        String subtest = (String) accommodation.getFieldValueByAlias("iac-subtest", getDictionary());
        String setting = (String) accommodation.getFieldValueByAlias("iac-setting", getDictionary());
        String timingScheduling = (String) accommodation.getFieldValueByAlias("iac-timing-scheduling", getDictionary());
        String presentation = (String) accommodation.getFieldValueByAlias("iac-presentation", getDictionary());
        String response = (String) accommodation.getFieldValueByAlias("iac-response", getDictionary());

        if (subtest == null) {
            subtest = "";
        }
        if (setting == null) {
            setting = "";
        }
        if (timingScheduling == null) {
            timingScheduling = "";
        }
        if (presentation == null) {
            presentation = "";
        }
        if (response == null) {
            response = "";
        }

        // replace the commas with new lines and dashes
        setting = setting.replaceAll(",\\s?", " \n - ");
        timingScheduling = timingScheduling.replaceAll(",\\s?", "\n - ");
        presentation = presentation.replaceAll(",\\s?", "\n - ");
        response = response.replaceAll(",\\s?", "\n - ");

        // replace the reference codes with their descriptions
        Iterator<Map.Entry<String, ReferenceCode>> it = m_refCodeMap.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry<String, ReferenceCode> pair = it.next();
            setting = StringUtils.replaceAll(setting, pair.getKey(), pair.getValue().getDescription());
            timingScheduling =
                    StringUtils.replaceAll(timingScheduling, pair.getKey(), pair.getValue().getDescription());
            presentation = StringUtils.replaceAll(presentation, pair.getKey(), pair.getValue().getDescription());
            response = StringUtils.replaceAll(response, pair.getKey(), pair.getValue().getDescription());
        }

        m_testingAccommodations.append();
        m_testingAccommodations.set("test", accommodation.getFieldValueByAlias("iac-test", getDictionary()));
        m_testingAccommodations.set("subtest", subtest.replaceAll("&", "&amp;"));
        m_testingAccommodations.set("setting", setting.replaceAll("&", "&amp;"));
        m_testingAccommodations.set("timing-scheduling", timingScheduling.replaceAll("&", "&amp;"));
        m_testingAccommodations.set("presentation", presentation.replaceAll("&", "&amp;"));
        m_testingAccommodations.set("response", response.replaceAll("&", "&amp;"));
        m_testingAccommodations.set("type",
                accommodation.getFieldValueByAlias("iac-std-vs-conditional", getDictionary()));
    }

    /**
     * Returns the amendment date, as a string, if there is one.
     *
     * @return String
     */
    private String getAmendmentDate() {
        String dateAsString = null;

        if (m_currentIep.getIepAmendment() != null) {
            PlainDate amendmentDate = m_currentIep.getIepAmendment().getDate();
            SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
            dateAsString = dateFormat.format(amendmentDate);
        }

        return dateAsString;
    }

    /**
     * Returns the grade level based on the students enrollment at a given IEP start date.
     * If there is no associated district context or grade level, the current grade level
     * is returned.
     *
     * @param enrollment StudentEnrollment
     * @param date PlainDate
     * @return gradeLevel
     */
    private String getGradeLevel(StudentEnrollment enrollment, PlainDate date) {
        TreeMap<Integer, List<String>> gradeLevels = StudentManager.buildGradeLevelMap(getBroker());
        String gradeLevel = null;

        // student's YOG at this particular time
        int yog = enrollment.getYog();

        // get the school year context from the iep start date
        X2Criteria schoolYearCriteria = new X2Criteria();
        schoolYearCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, date);
        schoolYearCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, date);
        QueryByCriteria schoolYearQuery = new QueryByCriteria(DistrictSchoolYearContext.class, schoolYearCriteria);
        DistrictSchoolYearContext context = (DistrictSchoolYearContext) getBroker().getBeanByQuery(schoolYearQuery);
        DistrictSchoolYearContext currentContext = getCurrentContext();
        int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());

        // if there is a context and it is not the current context get the associated grade level
        if (context != null && currentContext != context) {
            int schoolYear = context.getSchoolYear();
            List<String> grades = StudentManager.getMatchingGradeLevels(maxGradeLevel, yog, schoolYear, gradeLevels);
            gradeLevel = grades.get(0);
        }

        // if there is no associated grade level use the current grade level
        if (StringUtils.isEmpty(gradeLevel)) {
            gradeLevel = m_currentIep.getStudent().getGradeLevel();
        }
        return gradeLevel;
    }

    /**
     * Returns the current IEP. If a blank form is being printed, a new (unsaved) IEP is created and
     * returned.
     *
     * @return IepData
     */
    private IepData getIep() {
        if (m_iep == null) {
            if (isBlank()) {
                IepData ownerIep = (IepData) getFormOwner();

                m_iep = new IepData(getBroker().getPersistenceKey());
                m_iep.setStudentOid(ownerIep.getStudentOid());
                m_iep.setStaffOid(ownerIep.getStaffOid());
            } else {
                m_iep = (IepData) getFormStorage();
            }
        }

        return m_iep;
    }

    /**
     * Return the IEP Meeting that has the same data as the IEP.
     * If not found, return the most recent IEP meeting.
     * Otherwise, return a blank IEP meeting.
     *
     * @return the IEP Meeting bean
     */
    private IepMeeting getMeeting() {
        IepMeeting meeting = null;

        if (isBlank() || m_currentIep.getIepMeeting().size() == 0) {
            meeting = new IepMeeting(getBroker().getPersistenceKey());
        } else {
            // obtain the most recent IEP meeting
            Criteria sameDateCriteria = new Criteria();
            sameDateCriteria.addEqualTo(IepMeeting.COL_IEP_DATA_OID, m_currentIep.getOid());
            sameDateCriteria.addEqualTo(IepMeeting.COL_DATE, m_currentIep.getMeetingDate());
            QueryByCriteria sameDateQuery = new QueryByCriteria(IepMeeting.class, sameDateCriteria);
            IepMeeting sameDateMeeting = (IepMeeting) getBroker().getBeanByQuery(sameDateQuery);

            if (sameDateMeeting != null) {
                meeting = sameDateMeeting;
            } else {
                // obtain an IEP meeting (if it exists) whose date matches the IEP's meeting date
                Criteria mostRecentMeetingCriteria = new Criteria();
                mostRecentMeetingCriteria.addEqualTo(IepMeeting.COL_IEP_DATA_OID, m_currentIep.getOid());
                QueryByCriteria mostRecentMeetingQuery =
                        new QueryByCriteria(IepMeeting.class, mostRecentMeetingCriteria);
                mostRecentMeetingQuery.addOrderByDescending(IepMeeting.COL_DATE);
                mostRecentMeetingQuery.addOrderByDescending(IepMeeting.COL_TIME);
                IepMeeting recentMeeting = (IepMeeting) getBroker().getBeanByQuery(mostRecentMeetingQuery);

                meeting = recentMeeting;
            }
        }

        return meeting;
    }

    /**
     * Return the most recent StudentEnrollment of type "Entry" on or before
     * the cutOffDate. Querying type "Entry" ensures the student has the most
     * recent enrollment with school and student information.
     *
     * @param date PlainDate
     * @return most recent StudentEnrollment on or before cutOffDate
     */
    private StudentEnrollment getStudentEnrollment(PlainDate date) {
        StudentEnrollment enrollment = null;

        String studentOid = m_currentIep.getStudentOid();

        if (!StringUtils.isEmpty(studentOid)) {
            X2Criteria studentEnrollmentCriteria = new X2Criteria();
            studentEnrollmentCriteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, studentOid);
            studentEnrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, date);
            studentEnrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);
            QueryByCriteria studentEnrollmentQuery =
                    new QueryByCriteria(StudentEnrollment.class, studentEnrollmentCriteria);
            studentEnrollmentQuery.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, false);
            studentEnrollmentQuery.addOrderBy(StudentEnrollment.COL_TIMESTAMP, false);
            QueryIterator iterator = getBroker().getIteratorByQuery(studentEnrollmentQuery);

            if (iterator.hasNext()) {
                enrollment = (StudentEnrollment) iterator.next();
            }
        }

        return enrollment;
    }

    /**
     * Returns the format of subreport for the given page constant.
     *
     * @param pageId String
     * @return byte[]
     */
    private InputStream getSubreportFormat(String pageId) {
        Report report = m_subReports.get(pageId);
        return new ByteArrayInputStream(report.getCompiledFormat());
    }

    /**
     * Loads IepAccommodations data and concatenate the content area and description into each
     * accommodation's list.
     */
    private void loadAccommodations() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepAccommodation.COL_IEP_DATA_OID, getIep().getOid());

        QueryByCriteria accommodationsQuery = new QueryByCriteria(IepAccommodation.class, criteria);
        accommodationsQuery.addOrderByDescending(IepAccommodation.COL_IMPLEMENTATION_DATE);
        Collection<IepAccommodation> accommodations = getBroker().getCollectionByQuery(accommodationsQuery);

        for (IepAccommodation accommodation : accommodations) {
            String category = accommodation.getCategory();
            String accommodationText = accommodation.getContentArea() + ": " + accommodation.getDescription();
            if (category.equals("Instructional")) {
                m_instructionalAccommodations.add(String.format("(%d) %s",
                        Integer.valueOf(m_instructionalAccommodations.size() + 1), accommodationText));
            } else if (category.equals("Classroom Testing")) {
                m_classroomAccomomdations.add(
                        String.format("(%d) %s", Integer.valueOf(m_classroomAccomomdations.size() + 1), accommodationText));
            } else if (category.equals("Supplemental")) {
                m_supplementalAccommodations.add(String.format("(%d) %s",
                        Integer.valueOf(m_supplementalAccommodations.size() + 1), accommodationText));
            } else if (category.equals("Supports")) {
                m_supportsAccommodations.add(
                        String.format("(%d) %s", Integer.valueOf(m_supportsAccommodations.size() + 1), accommodationText));
            } else if (category.equals("Testing")) {
                addTestingAccommodation(accommodation);
            }
        }

        if (m_testingAccommodations.rowCount() < 4) {
            for (int i = m_testingAccommodations.rowCount(); i < 4; i++) {
                m_testingAccommodations.append();
            }
        }

        /*
         * Bring the report data grid for testing accommodations to the top
         */
        m_testingAccommodations.beforeTop();

    }

    /**
     * Loads goal and objective data.
     */
    private void loadGoalsAndObjectives() {
        if (isBlank()) {
            for (int i = 0; i < 4; i++) {
                m_goalData.append();
                m_objectivesData.append();
            }
        } else {
            Criteria goalsCriteria = new Criteria();
            goalsCriteria.addEqualTo(IepGoal.COL_IEP_DATA_OID, m_currentIep.getOid());
            QueryByCriteria goalsQuery = new QueryByCriteria(IepGoal.class, goalsCriteria);
            goalsQuery.addOrderByAscending(IepGoal.COL_ID); // Goal number
            goalsQuery.addOrderByAscending(IepGoal.COL_FOCUS);

            SubQuery goalsSubQuery = new SubQuery(IepGoal.class, X2BaseBean.COL_OID, goalsCriteria);
            Criteria objectivesCriteria = new Criteria();
            objectivesCriteria.addIn(IepGoalObjective.COL_IEP_GOAL_OID, goalsSubQuery);
            QueryByCriteria objectivesQuery = new QueryByCriteria(IepGoalObjective.class, objectivesCriteria);
            objectivesQuery.addOrderByAscending(IepGoalObjective.COL_IEP_GOAL_OID);
            objectivesQuery.addOrderByAscending(IepGoalObjective.COL_SEQUENCE_NUMBER);

            Collection<IepGoalObjective> objectives = getBroker().getCollectionByQuery(objectivesQuery);

            Collection<IepGoal> goals = getBroker().getCollectionByQuery(goalsQuery);
            String currentGoalId = null;
            int goalCount = 0;

            for (IepGoalObjective objective : objectives) {
                if (!objective.getIepGoalOid().equals(currentGoalId)) {
                    if (goalCount < 8 && currentGoalId != null) {
                        for (int i = goalCount; i < 8; i++) {
                            m_objectivesData.append();
                            m_objectivesData.set("iglId", currentGoalId);
                        }
                    }
                    goalCount = 0;
                    currentGoalId = objective.getIepGoalOid();
                }
                m_objectivesData.append();
                m_objectivesData.set("iglId", objective.getIepGoalOid());
                m_objectivesData.set("goal", objective.getIepGoal().getGoal());
                m_objectivesData.set("goalId", objective.getIepGoal().getId());
                m_objectivesData.set("focus", objective.getIepGoal().getFocus());
                m_objectivesData.set("objective", objective.getObjective());
                m_objectivesData.set("criteria-for-mastery",
                        objective.getFieldValueByAlias("igo-criteria-for-mastery", getDictionary()));
                m_objectivesData.set("method-of-eval",
                        objective.getFieldValueByAlias("igo-method-of-eval", getDictionary()));
                goalCount++;
            }
            if (goalCount < 8 && currentGoalId != null) {
                for (int i = goalCount; i < 8; i++) {
                    m_objectivesData.append();
                    m_objectivesData.set("iglId", currentGoalId);
                }
            }

            for (IepGoal goal : goals) {
                m_goalData.append();
                m_goalData.set("id", goal.getId());
                m_goalData.set("focus", goal.getFocus());
                m_goalData.set("goal", goal.getGoal());
                m_goalData.set("goal-criteria", goal.getFieldValueByAlias("iep-goal-criteria", getDictionary()));
                m_goalData.set("goal-eval-method", goal.getFieldValueByAlias("iep-goal-eval-method", getDictionary()));
                m_goalData.set("goal-supports", goal.getFieldValueByAlias("iep-goal-supports", getDictionary()));
                m_goalData.set("progress-report-date-1",
                        goal.getFieldValueByAlias("iep-progress-report-date-1", getDictionary()));
                m_goalData.set("progress-report-date-2",
                        goal.getFieldValueByAlias("iep-progress-report-date-2", getDictionary()));
                m_goalData.set("progress-report-date-3",
                        goal.getFieldValueByAlias("iep-progress-report-date-3", getDictionary()));
                m_goalData.set("progress-report-date-4",
                        goal.getFieldValueByAlias("iep-progress-report-date-4", getDictionary()));
            }

            if (m_goalData.rowCount() < 4) {
                for (int i = m_goalData.rowCount(); i < 4; i++) {
                    m_goalData.append();
                }
            }

            if (m_objectivesData.rowCount() == 0) {
                for (int i = m_objectivesData.rowCount(); i < 4; i++) {
                    m_objectivesData.append();
                }
            }
        }
    }

    /**
     * Go through the IEP meeting attendances (from getMeeting) and set a parameter for each.
     *
     * On the IEP form, there is a section where Parent, LEA Rep, Special Ed Teacher, Regular Ed
     * Teacher, Agency Rep, and Student
     * that gets their own special spot. There will be a parameter specifically made for them.
     *
     * Since there are two Parent fields on the form, there is a "Parent 1" and "Parent 2"
     * parameter.
     * Any subsequent members whose role is Parent will be considered as an "Other".
     *
     * Other roles be in the "Other #" parameter, the # being the number they were found in the
     * order from the loop
     */
    private void loadMemberAttendance() {
        Collection<String> requiredMembers = new ArrayList<String>();
        Collection<String> additionalMembers = new ArrayList<String>();

        Collection<IepMeetingAttendance> attendances = m_meeting.getMeetingAttendance();

        Boolean includeAllAttendees = (Boolean) getParameter(INCLUDE_ALL_ATTENDEES_PARAM);
        for (IepMeetingAttendance attendant : attendances) {
            if ((attendant.getPresentIndicator() || includeAllAttendees.equals(Boolean.TRUE))
                    && attendant.getTeamMember() != null) {
                IepTeamMember teamMember = attendant.getTeamMember();
                if (teamMember != null) {
                    String memberRole = teamMember.getMemberRoleCode();
                    String memberName = teamMember.getNameView();
                    String nameRole = memberName + " / " + memberRole;

                    // Check if the attendee is a requiredMember or not
                    String isRequiredMember =
                            (String) teamMember.getFieldValueByAlias(ALIAS_REQUIRED_MEMBER, getDictionary());
                    if (isRequiredMember != null && isRequiredMember.equals("1")) {
                        requiredMembers.add(nameRole);
                    } else {
                        additionalMembers.add(nameRole);
                    }
                }
            }
        }

        // Make both required members and additional members the same height/size if possible
        // (minimum 9 rows)
        int minimumSize = TEAM_MEMBERS_NUMBER;
        if (requiredMembers.size() > minimumSize) {
            minimumSize = requiredMembers.size();
        }
        if (additionalMembers.size() > minimumSize) {
            minimumSize = additionalMembers.size();
        }

        for (int i = requiredMembers.size(); i < minimumSize; i++) {
            requiredMembers.add("");
        }

        for (int i = additionalMembers.size(); i < minimumSize; i++) {
            additionalMembers.add("");
        }

        for (String nameRole : requiredMembers) {
            m_requiredMembersData.append();
            m_requiredMembersData.set(SUBREPORT_NAME_ROLE_FIELD, nameRole);
        }

        for (String nameRole : additionalMembers) {
            m_AdditionalMembersData.append();
            m_AdditionalMembersData.set(SUBREPORT_NAME_ROLE_FIELD, nameRole);
        }
    }

    /**
     * Loads service data into parameter map for fast retrieval. The map loaded is keyed on
     * serviceMode
     * and each value contains a collection of sorted IepService objects.
     */
    private void loadServices() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepService.COL_IEP_DATA_OID, getIep().getOid());

        QueryByCriteria servicesQuery = new QueryByCriteria(IepService.class, criteria);
        servicesQuery.addOrderByAscending(IepService.COL_SERVICE_MODE);
        servicesQuery.addOrderByAscending(IepService.COL_GOAL_VIEW);
        servicesQuery.addOrderByAscending(IepService.COL_SERVICE_CODE);

        Collection<IepService> services = getBroker().getCollectionByQuery(servicesQuery);

        for (IepService service : services) {
            String serviceMode = service.getServiceMode();
            String serviceType = service.getServiceType();
            if (serviceMode.equals("General Education")) {
                m_insideServices.add(service);
                m_servicesConsidered.add(serviceType);
            } else if (serviceMode.equals("Special Education")) {
                m_outsideServices.add(service);
                m_servicesConsidered.add(serviceType);
            } else if (serviceMode.equals("Extended")) {
                m_extendedServices.add(service);
            }
        }

        // fill in the rest with blank services
        IepService blankService = new IepService(getBroker().getPersistenceKey());
        if (m_insideServices.size() < 9) {
            for (int i = m_insideServices.size(); i < 9; i++) {
                m_insideServices.add(blankService);
            }
        }

        if (m_outsideServices.size() < 9) {
            for (int i = m_outsideServices.size(); i < 9; i++) {
                m_outsideServices.add(blankService);
            }
        }

        if (m_extendedServices.size() < 4) {
            for (int i = m_extendedServices.size(); i < 4; i++) {
                m_extendedServices.add(blankService);
            }
        }
    }

    /**
     * Loads all the Reference Codes (RTB_OID = "rtbServiceType") to be displayed whether they were
     * considered or not.
     *
     * If a service was considered, an underlined checkmark ("4" in ZapfDingbats) will appear.
     * Otherwise, it will be an underlined space ("   " in ZapfDingabats).
     */
    private void loadServiceTypes() {
        DataDictionaryField field =
                getDictionary().findDataDictionaryField(IepService.class.getName(), IepService.COL_SERVICE_TYPE);
        Criteria serviceCriteria = new Criteria();
        serviceCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        QueryByCriteria serviceQuery = new QueryByCriteria(ReferenceCode.class, serviceCriteria);
        serviceQuery.addOrderByAscending(ReferenceCode.COL_SEQUENCE_NUMBER);
        Collection<ReferenceCode> serviceTypeRefCodes = getBroker().getCollectionByQuery(serviceQuery);

        String optionsConsidered =
                (String) m_currentIep.getFieldValueByAlias("iep-options-considered", getDictionary());
        if (optionsConsidered == null) {
            optionsConsidered = "";
        }
        m_servicesConsidered.addAll(Arrays.asList(optionsConsidered.split("\\s*,\\s*")));

        StringBuilder serviceTypeText = new StringBuilder();
        int numServiceTypes = serviceTypeRefCodes.size();
        int howManyTypesInAColumn = (int) Math.ceil(numServiceTypes / 4.0);
        int count = 0;
        int columnCount = 1;
        for (ReferenceCode serviceTypeRefCode : serviceTypeRefCodes) {
            String serviceType = serviceTypeRefCode.getCode();
            if (m_servicesConsidered.contains(serviceType)) {
                serviceTypeText.append("<style isUnderline=\"true\" pdfFontName=\"Helvetica\">X</style> ");
            } else {
                serviceTypeText.append("<style isUnderline=\"true\" pdfFontName=\"Helvetica\">   </style> ");
            }
            serviceTypeText.append(serviceType);
            serviceTypeText.append('\n');
            count++;
            if (count % howManyTypesInAColumn == 0) {
                addParameter("serviceTypes " + columnCount, serviceTypeText.toString());
                columnCount++;
                serviceTypeText = new StringBuilder();
            }
        }
        addParameter("serviceTypes 4", serviceTypeText.toString());
    }

    /**
     * Loads each IEP subreport into a map for fast retrieval.
     */
    private void loadSubReports() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addIn(Report.COL_ID, Arrays.asList(new String[] {FORMAT_ID_GOALS,
                FORMAT_ID_OBJECTIVES,
                FORMAT_ID_TESTING,
                FORMAT_ID_SVCS,
                FORMAT_ID_ESY,
                FORMAT_ID_REQUIRED,
                FORMAT_ID_ADDITIONAL}));

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, SUBREPORTS_COUNT);
    }

}
