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
package com.x2dev.reports.sys.sped.ma;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Contact;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.sys.sped.ma.MaSpedAttribHelper;
import com.x2dev.procedures.sys.sped.ma.MaSpedAttribHelper.MaSpedDataSource;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the Massachusetts IEP form. This class prepares a ReportDataGrid that contains
 * a row for each section of the IEP. Each row contains a format and a java source for the
 * corresponding section. The following are the sections prepared:
 * <p>
 * <table border="1">
 * <tr>
 * <th>Section</th>
 * <th>Java source description</th>
 * </tr>
 * <tr>
 * <td>IEP 1 - Vision</td>
 * <td>SimpleFormDataSource</td>
 * </tr>
 * <tr>
 * <td>IEP 2 - PLEP A</td>
 * <td>SimpleFormDataSource</td>
 * </tr>
 * <tr>
 * <td>IEP 3 - PLEP B</td>
 * <td>SimpleFormDataSource</td>
 * </tr>
 * <tr>
 * <td>IEP 4 - Goals</td>
 * <td>ReportDataGrid containing IepGoal records - one row per goal field to allow floating text
 * </td>
 * </tr>
 * <tr>
 * <td>IEP 5 - Services</td>
 * <td>ReportDataGrid containing IepService records (with blanks inserted to property render the
 * service grid)</td>
 * </tr>
 * <tr>
 * <td>IEP 6 - Schedule</td>
 * <td>SimpleFormDataSource</td>
 * </tr>
 * <tr>
 * <td>IEP 7 - Assessment</td>
 * <td>SimpleFormDataSource</td>
 * </tr>
 * <tr>
 * <td>IEP 8 - Response</td>
 * <td>SimpleFormDataSource</td>
 * </tr>
 * </table>
 * <p>
 * A response page only parameter is provided. If selected, only IEP 8 is prepared.
 *
 * @author X2 Development Corporation
 */
public class IepFormData extends MaBeanReport {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    /*
     * -------- Input parameter constants -------
     */

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private static final String RESPONSE_PAGE_ONLY_PARAM = "responseOnly";

    /*
     * -------- Constants for the main report -------
     */
    private static final String COL_IEP = "iep";
    private static final String COL_SUBREPORT_DATA_SOURCE = "datasource";
    private static final String COL_SUBREPORT_FORMAT = "format";
    private static final String COL_PAGE_IDENTIFIER = "pageIdentifier";

    private static final String NODE_ID_IEP_IEP_LIST_DETAIL = "iep.iep.list.detail";
    private static final String NODE_ID_STD_STD_LIST_IEP_DETAIL = "student.std.list.iep.detail";
    private static final int IEP_INITIAL_CAPACITY = 50;

    // Format IDs
    private static final String OVERFLOW_FORMAT_ID = "SYS-SPED-MA-IEP_OVER";
    private static final String PAGE_1_FORMAT_ID = "SYS-SPED-MA-IEP1";
    private static final String PAGE_2_FORMAT_ID = "SYS-SPED-MA-IEP2";
    private static final String PAGE_3_FORMAT_ID = "SYS-SPED-MA-IEP3";
    private static final String PAGE_4_FORMAT_ID = "SYS-SPED-MA-IEP4";
    private static final String PAGE_5_FORMAT_ID = "SYS-SPED-MA-IEP5";
    private static final String PAGE_5_SUB1_FORMAT_ID = "SYS-SPED-MA-IEP5-S1";
    private static final String PAGE_5_SUB2_FORMAT_ID = "SYS-SPED-MA-IEP5-S2";
    private static final String PAGE_6_FORMAT_ID = "SYS-SPED-MA-IEP6";
    private static final String PAGE_7_FORMAT_ID = "SYS-SPED-MA-IEP7";
    private static final String PAGE_8_FORMAT_ID = "SYS-SPED-MA-IEP8";

    /*
     * -------- Constants for the goals subreport -------
     */
    private static final String COL_GOAL = "goal";
    private static final String COL_GOAL_FIELD_ID = "fieldId";
    private static final String COL_GOAL_TEXT = "goalText";

    private static final String FIELD_ID_BASELINE = "baseline";
    private static final String FIELD_ID_BENCHMARK = "benchmark";
    private static final String FIELD_ID_GOAL = "goal";

    /*
     * -------- Constant parameters to see if page is printable or no -------
     */
    private static final String VISION = "vision";
    private static final String PLEP_A = "plaepA";
    private static final String PLEP_B = "plaepB";
    private static final String GOALS = "goals";
    private static final String SERVICES = "services";
    private static final String SCHEDULE = "schedule";
    private static final String ASSESSMENT = "assessment";
    private static final String RESPONSE = "response";

    /*
     * -------- Constants for the services subreport -------
     */
    private static final String COL_SERVICE = "service";

    private static final int MAX_SECTION_A_SERVICES = 6;
    private static final int MAX_SECTION_B_SERVICES = 8;
    private static final int MAX_SECTION_C_SERVICES = 8;

    private static final String SERVICES_SECTION_A_MODE = "Consultation";
    private static final String SERVICES_SECTION_B_MODE = "SpecialEd - General";
    private static final String SERVICES_SECTION_C_MODE = "SpecialEd - Other";

    private static final String IEP_PARAM = "iep";
    private static final String STD_GRADE_LEVEL = "gradeLevel";
    private static final String STD_DISABILITY = "disability";
    private static final String STD_SECONDARY_DISABILITY = "secondaryDisability";

    private static final String RTB_SIS_IMAGES = "SIS Image";
    private static final String ALIAS_RCD_IMAGE_BASE64 = "all-rcd-ImageBase64";
    private static final String CODE_SIGNATURE_LEA = "LEA Admin";
    private static final String CODE_SIGNATURE_SEA = "SPED Admin";
    private static final String PARAM_SIGNATURE_LEA = "ImgLEAAdmin";
    private static final String PARAM_SIGNATURE_SEA = "ImgSPEDAdmin";

    private Map m_goalData = null;
    private Map m_servicesData = null;
    private MaSpedAttribHelper m_attribHelper;

    private Map m_subReports = null;
    private boolean m_isDetailsView = false;

    /**
     * Gets the form storage.
     *
     * @return X 2 base bean
     * @see com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource#getFormStorage()
     */
    @Override
    /**
     * if report print from details view - storage should be existing IEP data object. It need for
     * show real data, not blank.
     *
     * @see com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource#getFormStorage()
     */
    public X2BaseBean getFormStorage() {
        X2BaseBean storage = null;
        if (m_isDetailsView) {
            storage = getFormOwner();
        } else {
            storage = super.getFormStorage();
        }
        return storage;
    }

    /**
     * Returns true if the form being printed is blank.
     * form storage has specific initialize. original method used m_formStorage member variable
     * ignore specific initialize.
     * there is why method was override and used getFormStorage() instead m_formStorage member
     * variable
     *
     * @return boolean
     */
    @Override
    public boolean isBlank() {
        return getFormStorage().getOid() == null;
    }

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();
        m_attribHelper = new MaSpedAttribHelper(getBroker(), true);

        IepData iep = getIep();
        if (iep.getOid() != null) {
            loadGoals();
            loadServices();
        }
        loadSubReports();

        boolean responseOnly = ((Boolean) getParameter(RESPONSE_PAGE_ONLY_PARAM)).booleanValue();

        if (!responseOnly) {
            if ((Boolean) getParameter(VISION)) {
                preparePage1(grid, iep);
            }

            if ((Boolean) getParameter(PLEP_A)) {
                preparePage2(grid, iep);
            }

            if ((Boolean) getParameter(PLEP_B)) {
                preparePage3(grid, iep);
            }

            if ((Boolean) getParameter(GOALS)) {
                preparePage4(grid, iep);
            }

            if ((Boolean) getParameter(SERVICES)) {
                preparePage5(grid, iep);
            }

            if ((Boolean) getParameter(SCHEDULE)) {
                preparePage6(grid, iep);
            }

            if ((Boolean) getParameter(ASSESSMENT)) {
                preparePage7(grid, iep);
            }
        }

        if ((Boolean) getParameter(RESPONSE)) {
            preparePage8(grid, iep);
        }

        grid.beforeTop();

        addParameter(IEP_PARAM, iep);
        addParameter(PARAM_SIGNATURE_LEA,
                getBase64ImageString(CODE_SIGNATURE_LEA, iep.getStudent().getSchoolOid(), getBroker()));
        addParameter(PARAM_SIGNATURE_SEA,
                getBase64ImageString(CODE_SIGNATURE_SEA, iep.getStudent().getSchoolOid(), getBroker()));

        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource#saveState(com.follett.fsc.
     *      core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);
        List<String> detailsNodes =
                new ArrayList<String>(Arrays.asList(NODE_ID_IEP_IEP_LIST_DETAIL, NODE_ID_STD_STD_LIST_IEP_DETAIL));
        if (detailsNodes.contains(userData.getCurrentNode().getId())) {
            m_isDetailsView = true;
        }
    }

    /**
     * create and add subreport into dataGrid<br>
     * subreport create row for each IepService in <code>services</code><br>
     * if <code>services</code> is empty - nothing to add.<br>
     * <code>services</code> shouldn't be null
     *
     * @param dataGrid ReportDataGrid
     * @param services List<IepService>
     */
    private void addSubReportIn5Page(ReportDataGrid dataGrid, List<IepService> services) {
        if (services.size() > 0) {
            ReportDataGrid subCGrid = new ReportDataGrid();
            subCGrid.setColumnValues(COL_SERVICE, services);
            subCGrid.beforeTop();

            dataGrid.append();
            dataGrid.set(COL_SUBREPORT_DATA_SOURCE, subCGrid);
            Boolean printSettingCoulmn = (Boolean) getParameter("printSettingColumn");

            String subreportFormat = printSettingCoulmn == null || printSettingCoulmn.booleanValue()
                    ? PAGE_5_SUB1_FORMAT_ID
                    : PAGE_5_SUB2_FORMAT_ID;
            dataGrid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(subreportFormat));

        }
    }

    /**
     * Helper method to <code>preparePage5(ReportDataGrid, IepData)</code> that returns a list of
     * services of a given mode for a single page adjusted to the correct size.
     *
     * @param sectionList List<IepService>
     * @param maxServicesCount int
     * @param sectionMode String
     * @return ArrayList
     */
    private List<IepService> adjustServiceListSize(List<IepService> sectionList,
                                                   int maxServicesCount,
                                                   String sectionMode) {
        if (sectionList == null) {
            sectionList = new ArrayList(maxServicesCount);
        }
        if (sectionList.size() < maxServicesCount) {
            while (sectionList.size() < maxServicesCount) {
                IepService blankService = new IepService(getBroker().getPersistenceKey());
                blankService.setServiceMode(sectionMode);
                sectionList.add(blankService);
            }
        }

        return sectionList;
    }

    /**
     * Returns a SimpleBeanDataSource for the passed IEP that supports an overflow page.
     *
     * @param iep IepData
     * @param grid ReportDataGrid
     * @return Simple form data source
     */
    private MaSpedDataSource getDataSource(IepData iep, ReportDataGrid grid) {
        Map overflowFields = new HashMap<String, Object>();
        overflowFields.put(COL_IEP, iep);

        return m_attribHelper.getMaSpedDataSource(iep,
                iep,
                grid,
                getSubreportFormat(OVERFLOW_FORMAT_ID),
                overflowFields,
                getDictionary(),
                getLocale());
    }

    /**
     * Returns the current IEP. If a blank form is being printed, a new (unsaved) IEP is created and
     * returned. (IEP created automatic in BaseFormReportJavaSource in initialize method and set
     * like storage)
     *
     * @return IepData
     */
    private IepData getIep() {
        return (IepData) getFormStorage();
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
     * Loads goal data into map for fast retrieval. The map loaded is keyed on IEP OID and each
     * value contains a collection of sorted IepGoal objects.
     */
    private void loadGoals() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepGoal.COL_IEP_DATA_OID, getIep().getOid());

        QueryByCriteria goalsQuery = new QueryByCriteria(IepGoal.class, criteria);

        goalsQuery.addOrderByAscending(IepGoal.COL_ID); // Goal number
        goalsQuery.addOrderByAscending(IepGoal.COL_FOCUS);

        m_goalData =
                getBroker().getGroupedCollectionByQuery(goalsQuery, IepGoal.COL_IEP_DATA_OID, IEP_INITIAL_CAPACITY);
    }

    /**
     * Loads service data into map for fast retrieval. The map loaded is keyed on IEP OID and each
     * value contains a collection of sorted IepService objects.
     */
    private void loadServices() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepService.COL_IEP_DATA_OID, getIep().getOid());

        QueryByCriteria servicesQuery = new QueryByCriteria(IepService.class, criteria);

        servicesQuery.addOrderByAscending(IepService.COL_SERVICE_MODE);
        servicesQuery.addOrderByAscending(IepService.COL_GOAL_VIEW); // Focus on goal number
        servicesQuery.addOrderByAscending(IepService.COL_SERVICE_CODE);

        m_servicesData = getBroker().getGroupedCollectionByQuery(servicesQuery, IepService.COL_IEP_DATA_OID,
                IEP_INITIAL_CAPACITY);
    }

    /**
     * Loads each IEP subreport into a map for fast retrieval.
     */
    private void loadSubReports() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addIn(Report.COL_ID, Arrays.asList(new String[] {PAGE_1_FORMAT_ID,
                PAGE_2_FORMAT_ID,
                PAGE_3_FORMAT_ID,
                PAGE_4_FORMAT_ID,
                PAGE_5_FORMAT_ID,
                PAGE_5_SUB1_FORMAT_ID,
                PAGE_5_SUB2_FORMAT_ID,
                PAGE_6_FORMAT_ID,
                PAGE_7_FORMAT_ID,
                PAGE_8_FORMAT_ID,
                OVERFLOW_FORMAT_ID}));

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, 8);
    }

    /**
     * Report parameter containing the first priority contact
     */
    public static final String PARAM_CONTACT_0 = "contact0";

    /**
     * Report parameter containing the second priority contact
     */
    public static final String PARAM_CONTACT_1 = "contact1";


    /**
     * Report parameter containing accommodation strings for PLEPA, PLEPB and Assessments.
     */
    public static final String PARAM_ACCOMMODATIONS_PLEPA = "accommodations-plepa";
    public static final String PARAM_ACCOMMODATIONS_PLEPB = "accommodations-plepb";
    public static final String PARAM_ACCOMMODATIONS_ASSESSMENT = "accommodations-assessment";

    /**
     * Report parameter containing a the most recent IEP meeting
     */
    public static final String PARAM_MEETING = "meeting";

    /**
     * Report parameter containing a the first Placement of IEPData
     */
    public static final String FIRST_PLACEMENT = "firstPlacement";

    /**
     * Report parameter containing a the second Placement of IEPData
     */
    public static final String SECOND_PLACEMENT = "secondPlacement";

    /**
     * Report parameter containing a the third Placement of IEPData
     */
    public static final String THIRD_PLACEMENT = "thirdPlacement";

    /**
     * Gets the data source ADM.
     *
     * @return JR data source
     */
    protected JRDataSource getDataSourceADM() {
        IepData iep = null;

        if (!isBlank()) {
            iep = (IepData) getFormOwner();
            List<IepMeeting> meetings = (List<IepMeeting>) iep.getIepMeeting();
            if (meetings != null && meetings.size() > 0) {
                addParameter(PARAM_MEETING, meetings.get(meetings.size() - 1));
            }

            // get age as of iep start date.
            PlainDate startDate = new PlainDate();
            if (iep.getStartDate() != null) {
                startDate = iep.getStartDate();
                int ageOfStudent = iep.getStudent().getPerson().getAgeAsOfDate(startDate);
                addParameter("studentAge", String.valueOf(ageOfStudent));
            }
            // get grade level on creation time based on iep start date, if not form creation date,
            // on most recent entry enrollment record
            TreeMap<Integer, List<String>> gradeLevels = StudentManager.buildGradeLevelMap(getBroker());
            String gradeLevel = null;
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, iep.getStudentOid());
            BeanQuery query = new BeanQuery(StudentEnrollment.class, criteria);
            query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
            query.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);
            Collection<StudentEnrollment> enrollments = getBroker().getCollectionByQuery(query);

            int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());

            for (StudentEnrollment e : enrollments) {
                if (startDate != null && e.getEnrollmentDate().before(startDate)) {

                    // student's YOG at this particular time
                    int yog = e.getYog();

                    // get the school year from basedDate
                    X2Criteria schoolYearCriteria = new X2Criteria();
                    schoolYearCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, startDate);
                    schoolYearCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, startDate);
                    QueryByCriteria schoolYearQuery =
                            new QueryByCriteria(DistrictSchoolYearContext.class, schoolYearCriteria);
                    DistrictSchoolYearContext ctx =
                            (DistrictSchoolYearContext) getBroker().getBeanByQuery(schoolYearQuery);
                    String currentContextOid = getCurrentContext().getContextId();
                    if (!StringUtils.isEmpty(currentContextOid)
                            && currentContextOid.equalsIgnoreCase(ctx.getContextId())) {
                        gradeLevel = iep.getStudent().getGradeLevel();
                    } else {
                        int schoolYear = ctx.getSchoolYear();
                        List<String> grades =
                                StudentManager.getMatchingGradeLevels(maxGradeLevel, yog, schoolYear, gradeLevels);
                        gradeLevel = grades.get(0);
                    }
                    break;
                }
            }
            if (StringUtils.isEmpty(gradeLevel)) {
                gradeLevel = iep.getStudent().getGradeLevel();
            }
            addParameter("studentGradeLevel", gradeLevel);


            List<StudentContact> contacts = getStudentContacts(iep, 2);

            if (!contacts.isEmpty()) {
                addParameter(PARAM_CONTACT_0, contacts.get(0));
            }

            if (contacts.size() >= 2) {
                addParameter(PARAM_CONTACT_1, contacts.get(1));
            }

        } else {
            iep = new IepData(getBroker().getPersistenceKey());
            if (getFormOwner() == null) {
                // When printing from District > Tools, we do not want the district name to print,
                // but we need it on all other blank forms.
                // This overrides the organization that would normally be pulled from
                // ToolJavaSource.prepareParameters()
                addParameter("organization", null);
            }
        }

        return m_attribHelper.getMaSpedDataSource(iep, getFormOwner(), getDictionary(), getLocale());
    }


    /**
     * Return student contacts which added like iep team members.
     * method created instead SpedUtils.getStudentContacts, because SpedUtils working with all
     * contacts
     * Contact sorted by COL_FORM_PRIORITY and limited by maxContacts param
     *
     * @param iep IepData
     * @param maxContacts int
     * @return List
     */
    private List<StudentContact> getStudentContacts(IepData iep, int maxContacts) {
        List<StudentContact> contacts = new ArrayList<StudentContact>();
        /*
         * Load the student contact team members with form priority ordered by form priority
         */
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentContact.COL_STUDENT_OID, iep.getStudentOid());
        criteria.addEqualTo(StudentContact.REL_CONTACT + PATH_DELIMITER +
                Contact.REL_PERSON + PATH_DELIMITER +
                SisPerson.REL_TEAM_MEMBERS + PATH_DELIMITER +
                IepTeamMember.COL_IEP_DATA_OID, iep.getOid());
        criteria.addNotEmpty(StudentContact.REL_CONTACT + PATH_DELIMITER +
                Contact.REL_PERSON + PATH_DELIMITER +
                SisPerson.REL_TEAM_MEMBERS + PATH_DELIMITER +
                IepTeamMember.COL_FORM_PRIORITY, getBroker().getPersistenceKey());

        QueryByCriteria query = new QueryByCriteria(StudentContact.class, criteria);
        query.addOrderByAscending(StudentContact.REL_CONTACT + PATH_DELIMITER +
                Contact.REL_PERSON + PATH_DELIMITER +
                SisPerson.REL_TEAM_MEMBERS + PATH_DELIMITER +
                IepTeamMember.COL_FORM_PRIORITY);
        contacts.addAll(getBroker().getCollectionByQuery(query));

        if (contacts.isEmpty()) {
            /*
             * Load additional student contact team members without form priority ordered by
             * emergency priority
             */
            criteria = new X2Criteria();
            criteria.addEqualTo(StudentContact.COL_STUDENT_OID, iep.getStudentOid());
            criteria.addEqualTo(StudentContact.REL_CONTACT + PATH_DELIMITER +
                    Contact.REL_PERSON + PATH_DELIMITER +
                    SisPerson.REL_TEAM_MEMBERS + PATH_DELIMITER +
                    IepTeamMember.COL_IEP_DATA_OID, iep.getOid());
            criteria.addEmpty(StudentContact.REL_CONTACT + PATH_DELIMITER +
                    Contact.REL_PERSON + PATH_DELIMITER +
                    SisPerson.REL_TEAM_MEMBERS + PATH_DELIMITER +
                    IepTeamMember.COL_FORM_PRIORITY, getBroker().getPersistenceKey());
            query = new QueryByCriteria(StudentContact.class, criteria);

            query.addOrderByAscending(StudentContact.COL_EMERGENCY_PRIORITY);
            contacts.addAll(getBroker().getCollectionByQuery(query));
        }

        if (maxContacts != 0 && contacts.size() > maxContacts) {
            contacts = contacts.subList(0, maxContacts);
        }

        return contacts;
    }

    // ///////////////////////////////////////////////

    /**
     * Prepares the first IEP page (IEP 1).
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void preparePage1(ReportDataGrid grid, IepData iep) {
        grid.append();
        grid.set(COL_IEP, iep);
        grid.set(COL_SUBREPORT_DATA_SOURCE, getDataSource(iep, grid));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_1_FORMAT_ID));
        grid.set(COL_PAGE_IDENTIFIER, "IEP 1");
        setGradeLevel(iep);
        setDisability(iep);
    }

    /**
     * Sets the grade level.
     *
     * @param iep void
     */
    private void setGradeLevel(IepData iep) {
        String gradeLevel = null;

        if (iep.getOid() != null) {
            // get age on as of iep start date.
            PlainDate startDate = new PlainDate();
            if (iep.getStartDate() != null) {
                startDate = iep.getStartDate();
            }

            // get grade level on creation time based on iep start date, if not form creation date,
            // on most recent entry enrollment record
            TreeMap<Integer, List<String>> gradeLevels = StudentManager.buildGradeLevelMap(getBroker());

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, iep.getStudentOid());
            BeanQuery query = new BeanQuery(StudentEnrollment.class, criteria);
            query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
            query.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);
            Collection<StudentEnrollment> enrollments = getBroker().getCollectionByQuery(query);
            int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());

            for (StudentEnrollment e : enrollments) {
                if (startDate != null && e.getEnrollmentDate().before(startDate)) {

                    // student's YOG at this particular time
                    int yog = e.getYog();

                    // get the school year from basedDate
                    X2Criteria schoolYearCriteria = new X2Criteria();
                    schoolYearCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, startDate);
                    schoolYearCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, startDate);
                    QueryByCriteria schoolYearQuery =
                            new QueryByCriteria(DistrictSchoolYearContext.class, schoolYearCriteria);
                    DistrictSchoolYearContext ctx =
                            (DistrictSchoolYearContext) getBroker().getBeanByQuery(schoolYearQuery);
                    String currentContextOid = getCurrentContext().getContextId();
                    if (!StringUtils.isEmpty(currentContextOid) && ctx != null
                            && currentContextOid.equalsIgnoreCase(ctx.getContextId())) {
                        gradeLevel = iep.getStudent().getGradeLevel();
                    } else if (ctx != null) {
                        int schoolYear = ctx.getSchoolYear();
                        List<String> grades =
                                StudentManager.getMatchingGradeLevels(maxGradeLevel, yog, schoolYear, gradeLevels);
                        gradeLevel = grades.get(0);
                    }
                    break;
                }
            }
            if (StringUtils.isEmpty(gradeLevel)) {
                gradeLevel = iep.getStudent().getGradeLevel();
            }
        }
        addParameter(STD_GRADE_LEVEL, gradeLevel);
    }

    /**
     * Sets the primary disability.
     *
     * @param iep void
     */
    private void setDisability(IepData iep) {
        String primaryDisabiity = "";
        StringBuilder secondaryDisability = new StringBuilder();
        Collection<IepDisability> disabilities = iep.getIepDisability(getBroker());
        for (IepDisability disability : disabilities) {
            if (disability.getPrimaryIndicator()) {
                primaryDisabiity = disability.getDisabilityCode();
            } else {
                if (secondaryDisability.length() > 0) {
                    secondaryDisability.append(", ");
                }
                secondaryDisability.append(disability.getDisabilityCode());
            }
        }
        addParameter(STD_DISABILITY, primaryDisabiity);
        addParameter(STD_SECONDARY_DISABILITY, secondaryDisability.toString());
    }

    /**
     * Prepares the second IEP page (IEP 2).
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void preparePage2(ReportDataGrid grid, IepData iep) {
        grid.append();
        grid.set(COL_IEP, iep);
        grid.set(COL_SUBREPORT_DATA_SOURCE, getDataSource(iep, grid));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_2_FORMAT_ID));
        grid.set(COL_PAGE_IDENTIFIER, "IEP 2");
        addParameter(PARAM_ACCOMMODATIONS_PLEPA, getAccommodationView(iep, "PLEP A"));
    }

    /**
     * Prepares the third IEP page (IEP 3).
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void preparePage3(ReportDataGrid grid, IepData iep) {
        grid.append();
        grid.set(COL_IEP, iep);
        grid.set(COL_SUBREPORT_DATA_SOURCE, getDataSource(iep, grid));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_3_FORMAT_ID));
        grid.set(COL_PAGE_IDENTIFIER, "IEP 3");
        addParameter(PARAM_ACCOMMODATIONS_PLEPB, getAccommodationView(iep, "PLEP B"));
    }

    /**
     * Prepares the fourth IEP page (IEP 4).
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void preparePage4(ReportDataGrid grid, IepData iep) {
        /*
         * Each IEP 4 page can contain 2 goals each. If there are more than two goals, we must
         * insert additional pages for IEP 4.
         */
        Collection goals = null;
        if (isBlank()) {
            IepGoal blankGoal = new IepGoal(getBroker().getPersistenceKey());
            blankGoal.setStudentOid(iep.getStudentOid());

            goals = new ArrayList<IepGoal>(3);
            goals.add(blankGoal);
            goals.add(blankGoal);
            goals.add(blankGoal);
        } else {
            goals = (Collection) m_goalData.get(iep.getOid());
        }

        ReportDataGrid currentPage = null;

        if (goals != null) {
            Iterator iterator = goals.iterator();
            while (iterator.hasNext()) {
                IepGoal goal = (IepGoal) iterator.next();
                currentPage = new ReportDataGrid(2, 1);
                preparePage4_addGoal(currentPage, goal);
                currentPage.beforeTop();
                grid.append();
                grid.set(COL_IEP, iep);
                grid.set(COL_SUBREPORT_DATA_SOURCE, currentPage);
                grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_4_FORMAT_ID));
                grid.set(COL_PAGE_IDENTIFIER, "IEP 4");
            }
        }

        // For "empty" reports, print one empty page.
        if (currentPage == null && iep.getOid() == null) {
            currentPage = new ReportDataGrid(2, 1);
            preparePage4_addGoal(currentPage, new IepGoal(getBroker().getPersistenceKey()));
            currentPage.beforeTop();
            grid.set(COL_IEP, iep);
            grid.set(COL_SUBREPORT_DATA_SOURCE, currentPage);
            grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_4_FORMAT_ID));
            grid.set(COL_PAGE_IDENTIFIER, "IEP 4");
        }
    }

    /**
     * Adds the passed goal to the subreport data grid used on the IEP's goal page.
     *
     * @param currentPage ReportDataGrid
     * @param goal IepGoal
     */
    private void preparePage4_addGoal(ReportDataGrid currentPage, IepGoal goal) {
        currentPage.append();
        currentPage.set(COL_GOAL, goal);
        currentPage.set(COL_GOAL_TEXT, goal.getBaseline());
        currentPage.set(COL_GOAL_FIELD_ID, FIELD_ID_BASELINE);

        currentPage.append();
        currentPage.set(COL_GOAL, goal);
        currentPage.set(COL_GOAL_TEXT, goal.getGoal());
        currentPage.set(COL_GOAL_FIELD_ID, FIELD_ID_GOAL);

        // Determine of we need to remove trailing zeros from Objective Sequence Numbers
        // 10, 20, 30 --> 1, 2, 3
        // if ALL sequence numbers are multiples of 10 (not including zero).
        Collection<IepGoalObjective> objectives = goal.getIepGoalObjectives(getBroker());
        boolean removeLastCharacter = true;
        for (IepGoalObjective objective : objectives) {
            int intNumber = objective.getSequenceNumber();
            if ((intNumber % 10) != 0 && intNumber != 0) {
                removeLastCharacter = false;
                break;
            }
        }
        StringBuilder objectivesView = new StringBuilder();
        for (IepGoalObjective objective : objectives) {
            if (objectivesView.length() > 0) {
                objectivesView.append("\n");
            }
            if (removeLastCharacter) {
                objectivesView.append(Integer.toString(objective.getSequenceNumber() / 10));
            } else {
                objectivesView.append(objective.getSequenceNumber());
            }
            objectivesView.append(".   ");
            objectivesView.append(objective.getObjective());
        }

        currentPage.append();
        currentPage.set(COL_GOAL, goal);
        currentPage.set(COL_GOAL_TEXT, objectivesView.toString());
        currentPage.set(COL_GOAL_FIELD_ID, FIELD_ID_BENCHMARK);
    }

    /**
     * Prepares the fifth IEP page (IEP 5).
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void preparePage5(ReportDataGrid grid, IepData iep) {
        /*
         * Each IEP 5 page can contain 5 section "A" services, 6 section "B" services, and 7 section
         * "C" services. If there are more of any of these types, we must insert additional
         * pages for IEP 5.
         */

        List<IepService> current_A_list = new ArrayList<IepService>(MAX_SECTION_A_SERVICES);
        List<IepService> current_B_list = new ArrayList<IepService>(MAX_SECTION_B_SERVICES);
        List<IepService> current_C_list = new ArrayList<IepService>(MAX_SECTION_C_SERVICES);
        // if blank need empty lines for services
        if (iep.getOid() == null) {
            adjustServiceListSize(current_A_list, MAX_SECTION_A_SERVICES, SERVICES_SECTION_A_MODE);
            adjustServiceListSize(current_B_list, MAX_SECTION_B_SERVICES, SERVICES_SECTION_B_MODE);
            adjustServiceListSize(current_C_list, MAX_SECTION_C_SERVICES, SERVICES_SECTION_C_MODE);
        } else {
            Collection<IepService> services = (Collection) m_servicesData.get(iep.getOid());


            if (services != null) {
                Iterator iterator = services.iterator();
                while (iterator.hasNext()) {
                    IepService service = (IepService) iterator.next();

                    if (SERVICES_SECTION_A_MODE.equals(service.getServiceMode())) {
                        current_A_list.add(service);
                    } else if (SERVICES_SECTION_B_MODE.equals(service.getServiceMode())) {
                        current_B_list.add(service);
                    } else if (SERVICES_SECTION_C_MODE.equals(service.getServiceMode())) {
                        current_C_list.add(service);
                    }
                }
            }
        }

        // if some service type doesn't exist we should print one row for each type
        adjustServiceListSize(current_A_list, 1, SERVICES_SECTION_A_MODE);
        adjustServiceListSize(current_B_list, 1, SERVICES_SECTION_B_MODE);
        adjustServiceListSize(current_C_list, 1, SERVICES_SECTION_C_MODE);

        ReportDataGrid grid5page = new ReportDataGrid();
        addSubReportIn5Page(grid5page, current_A_list);
        addSubReportIn5Page(grid5page, current_B_list);
        addSubReportIn5Page(grid5page, current_C_list);
        grid5page.beforeTop();

        grid.append();
        grid.set(COL_IEP, iep);
        grid.set(COL_SUBREPORT_DATA_SOURCE, grid5page);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_5_FORMAT_ID));
        grid.set(COL_PAGE_IDENTIFIER, "IEP 5");

    }


    /**
     * Prepares the sixth IEP page (IEP 6).
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void preparePage6(ReportDataGrid grid, IepData iep) {
        grid.append();
        grid.set(COL_IEP, iep);
        grid.set(COL_SUBREPORT_DATA_SOURCE, getDataSource(iep, grid));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_6_FORMAT_ID));
        grid.set(COL_PAGE_IDENTIFIER, "IEP 6");
    }

    /**
     * Prepares the seventh IEP page (IEP 7).
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void preparePage7(ReportDataGrid grid, IepData iep) {
        grid.append();
        grid.set(COL_IEP, iep);
        grid.set(COL_SUBREPORT_DATA_SOURCE, getDataSource(iep, grid));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_7_FORMAT_ID));
        grid.set(COL_PAGE_IDENTIFIER, "IEP 7");
        addParameter(PARAM_ACCOMMODATIONS_ASSESSMENT, getAccommodationView(iep, "Testing"));
    }

    /**
     * Prepares the eighth IEP page (IEP 8).
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void preparePage8(ReportDataGrid grid, IepData iep) {
        grid.append();
        grid.set(COL_IEP, iep);
        grid.set(COL_SUBREPORT_DATA_SOURCE, getDataSource(iep, grid));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_8_FORMAT_ID));
        grid.set(COL_PAGE_IDENTIFIER, "IEP 8");
    }

    /**
     * Gets the base 64 image string.
     *
     * @param imageCode String
     * @param broker X2Broker
     * @return String
     */
    private String getBase64ImageString(String imageCode, String schoolOid, X2Broker broker) {
        String base64Image = "";
        List<String> ownerOids = new ArrayList<String>();
        ownerOids.add(OrganizationManager.ROOT_ORGANIZATION);
        if (!StringUtils.isEmpty(schoolOid)) {
            ownerOids.add(schoolOid);
        }
        X2Criteria imageCriteria = new X2Criteria();
        imageCriteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + ModelProperty.PATH_DELIMITER +
                ReferenceTable.COL_USER_NAME, RTB_SIS_IMAGES);
        imageCriteria.addEqualTo(ReferenceCode.COL_CODE, imageCode);
        imageCriteria.addIn(ReferenceCode.COL_OWNER_OID, ownerOids);
        BeanQuery imageQuery = new BeanQuery(ReferenceCode.class, imageCriteria);
        imageQuery.addOrderBy(ReferenceCode.COL_OWNER_TYPE, false);
        ReferenceCode rcdBean = broker.getBeanByQuery(imageQuery);
        if (rcdBean != null) {
            ExtendedDataDictionary ddx = rcdBean.getExtendedDataDictionary();
            if (ddx != null) {
                DataDictionary rtbDictionary =
                        DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());
                base64Image = (String) rcdBean.getFieldValueByAlias(ALIAS_RCD_IMAGE_BASE64, rtbDictionary);
                if (StringUtils.isEmpty(base64Image)) {
                    base64Image = "";
                }
            }
        }
        return base64Image;
    }

    /**
     * Construct a string of accommodations based on an accommodation type.
     *
     * @param iep
     * @param type
     *
     * @return String
     */
    public String getAccommodationView(IepData iep, String type) {
        StringBuilder view = new StringBuilder(1024);

        DataDictionary dictionary = getDictionary();
        DataDictionaryField accomField = dictionary.findDataDictionaryField(IepAccommodation.class.getName(), IepAccommodation.COL_NAME);
        ReferenceTable accomRefTable = accomField.getReferenceTable();
        Collection<ReferenceCode> codes = accomRefTable.getReferenceCodes(getBroker());
        
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepAccommodation.COL_IEP_DATA_OID, iep.getOid());
        criteria.addEqualTo(IepAccommodation.COL_TYPE, type);

        QueryByCriteria query = new QueryByCriteria(IepAccommodation.class, criteria);
        query.addOrderByAscending(IepAccommodation.COL_CONTENT_AREA);
        query.addOrderByAscending(IepAccommodation.COL_NAME);

        Collection<IepAccommodation> iepAccommodations = getBroker().getCollectionByQuery(query);

        String lastContentArea = "";
        TreeSet<String> accomsD = new TreeSet<String>();
        TreeSet<String> accomsSC = new TreeSet<String>();
        for (IepAccommodation accommodation : iepAccommodations) {
            if (type.equals(accommodation.getType())) {
                String contentArea = accommodation.getContentArea() == null ? "" : accommodation.getContentArea();
                if (!contentArea.equals(lastContentArea)) {
                    if (accomsSC != null) {
                        for (String accom : accomsSC) {
                            view.append("* ").append(accom).append("\n");
                        }
                    }
                    if (accomsD != null) {
                        for (String accom : accomsD) {
                            view.append("* ").append(accom).append("\n");
                        }
                    }
                    view.append("\n");
                    view.append(contentArea.toUpperCase());
                    view.append("\n");
                    accomsSC.clear();
                    accomsD.clear();
                }

                ReferenceCode nameCode = null;
                String accomName = accommodation.getName();
                StringBuilder accomLabel = null;
                boolean sc = false;
                for (ReferenceCode code : codes) {
                    if (code.getCode().equals(accomName)) {
                        accomLabel = new StringBuilder();
                        if (!StringUtils.isEmpty(code.getStateCode())) {
                            accomLabel.append(code.getStateCode()).append("  ");
                            sc = true;
                        }
                        if (!StringUtils.isEmpty(code.getDescription())) {
                            accomLabel.append(code.getDescription());
                        } else {
                            accomLabel.append(code.getCode());
                        }
                        break;
                    }
                }
                if (accomLabel == null) {
                    accomLabel = new StringBuilder();
                }
                if (accomLabel.length() == 0) {
                    accomLabel.append(accomName);
                }
                if (sc) {
                    accomsSC.add(accomLabel.toString());
                } else {
                    accomsD.add(accomLabel.toString());
                }

                lastContentArea = contentArea;
            }
        }
        if (accomsSC != null) {
            for (String accom : accomsSC) {
                view.append("* ").append(accom).append("\n");
            }
        }
        if (accomsD != null) {
            for (String accom : accomsD) {
                view.append("* ").append(accom).append("\n");
            }
        }


        return view.toString();
    }
}
