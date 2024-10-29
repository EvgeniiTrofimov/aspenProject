/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2016 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepGoal;
import com.x2dev.sis.model.beans.IepGoalObjective;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.business.EnrollmentSnapshot;
import com.x2dev.sis.model.business.GradeLevelHistory;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the BC IEP report.
 *
 * @author X2 Development Corporation
 */
public class IepFormData extends BaseFormReportJavaSource {
    private static final long serialVersionUID = 1L;

    /*
     * Report parameters
     */
    private static final String DESIGNATION_PARAM = "designation";
    private static final String IEP_PARAM = "iep";
    private static final String IEP_CONTACT_PARAM = "iepContact";
    private static final String IMPLEMENTATION_DATE_PARAM = "implementDate";
    private static final String REVIEW_DATE_PARAM = "reviewDate";
    private static final String SCHOOL_YEAR_CONTEXT_PARAM = "iepContextId";
    private static final String START_SCHOOL_PARAM = "startSchool";

    /*
     * -------- Constants for the main report -------
     */
    private static final String COL_IEP = "iep";
    private static final String COL_PAGE_IDENTIFIER = "pageIdentifier";
    private static final String COL_PAGE_NUMBER = "pageNumber";
    private static final String COL_SUBREPORT_DATA_SOURCE = "datasource";
    private static final String COL_SUBREPORT_FORMAT = "format";


    /*
     * Format IDs
     */
    private static final String OVERFLOW_FORMAT_ID = "SYS-SPED-BC-OVERFLOW";
    private static final String PAGE_1_FORMAT_ID = "SYS-SPED-BC-IEP1";
    private static final String PAGE_1_SUB_FORMAT_ID = "SYS-SPED-BC-IEP1-SUB";
    private static final String PAGE_2_FORMAT_ID = "SYS-SPED-BC-IEP2";
    private static final String PAGE_2_SUB_FORMAT_ID = "SYS-SPED-BC-IEP2-SUB";
    private static final String PAGE_4_FORMAT_ID = "SYS-SPED-BC-IEP4";
    private static final String PAGE_5_FORMAT_ID = "SYS-SPED-BC-IEP5";

    /*
     * -------- Constants for the accommodations subreport -------
     */
    private static final String ACCOMMODATION_TYPE_ADAPTATION = "Adaptation";
    private static final String ACCOMMODATION_TYPE_ADJUDICATION = "Adjudication";
    private static final String COL_ACCOMMODATION = "accommodation";
    private static final String COL_ACCOMMODATION_TYPE = "type";
    private static final String ADAPTATIONS_DATA_PARAM = "adaptationsData";
    private static final String ADAPTATIONS_FORMAT_PARAM = "adaptationsFormat";
    private static final String ADJUDICATIONS_DATA_PARAM = "adjudicationsData";
    private static final String ADJUDICATIONS_FORMAT_PARAM = "adjudicationsFormat";

    /*
     * -------- Constants for the goals subreport -------
     */
    private static final String COL_GOAL = "goal";
    private static final String COL_OBJECTIVE = "objective";

    /*
     * -------- Constants for the team subreport -------
     */
    private static final String COL_TEAM_MEMBER_NAME = "name";
    private static final String COL_TEAM_MEMBER_TITLE = "title";
    private static final String COL_TEAM_SUBREPORT_DATA = "teamData";
    private static final String COL_TEAM_SUBREPORT_FORMAT = "teamFormat";

    /*
     * ------- Constants for the reviews subreport -------
     */
    private static final String COL_REVIEW_BY = "reviewBy";
    private static final String COL_REVIEW_COMMENT = "reviewComment";
    private static final String COL_REVIEW_DATE = "reviewDate";
    private static final String COL_REVIEW_TYPE = "reviewType";
    private static final String FORM_F_ID = "FORM-F";
    private static final String FORM_G_ID = "FORM-G";
    private static final String REVIEW_BY_ALIAS = "reviewed-by";
    private static final String REVIEW_COMMENT_ALIAS = "review-comments";
    private static final String REVIEW_DATE_ALIAS = "reviewed-date";

    /*
     * Aliases
     */
    private static final String ALIAS_INCLUDE_CONTACT_ON_IEP = "include-contact-iep";
    private static final String ALIAS_STUDENT_SUPPORT_TEAM = "itm-sst";
    private static final String BC_DESIGNATION_ALIAS = "std-sped-category";

    /*
     * Other constants
     */
    private static final int IEP_INITIAL_CAPACITY = 50;
    private static final String STD_GRADE_LEVEL = "gradeLevel";
    private static final String IMPLEMENTATION_WORKFLOW_PHASE_OID = "wphBCAmImpIep";
    private static final String IEP_REVIEW_IMPLEMENT_PHASE_OID = "wphBCIEPFinIEP";
    private static final String ENROLL_IMPLEMENT_PHASE_OID = "wphX2MinIdent ";
    private static final String REVIEW_MESSAGE_PREFIX = "June ";

    private IepData m_currentIep = null;
    private int m_currentPageNumber = 0;
    private Map m_goalData = null;
    private Map m_subReports = null;

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

        loadGoals();
        loadSubReports();

        IepData iep = getIep();

        preparePage1(grid, iep);
        preparePage2(grid, iep);
        preparePage4(grid, iep);
        preparePage5Reviews(grid, iep);

        grid.beforeTop();

        addParameter(IEP_PARAM, iep);
        setGradeLevel(iep);

        return grid;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource#initialize()
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
     * @see
     *      com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource#saveState(com.follett.fsc.
     *      core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);

        m_currentIep = userData.getCurrentRecord(IepData.class);
    }

    /**
     * Returns a SimpleBeanDataSource for the passed IEP that supports an overflow page.
     *
     * @param iep IepData
     * @param grid ReportDataGrid
     * @return Simple form data source
     */
    private SimpleFormDataSource getDataSource(IepData iep, ReportDataGrid grid) {
        Map overflowFields = new HashMap<String, Object>();
        overflowFields.put(COL_IEP, iep);

        return new SimpleFormDataSource(iep,
                iep,
                grid,
                getSubreportFormat(OVERFLOW_FORMAT_ID),
                overflowFields,
                getDictionary(),
                getLocale());
    }

    /**
     * Returns the current IEP. If a blank form is being printed, a new (unsaved) IEP is created and
     * returned.
     *
     * @return IepData
     */
    private IepData getIep() {
        IepData iep = null;

        if (isBlank()) {
            IepData ownerIep = (IepData) getFormOwner();

            iep = new IepData(getBroker().getPersistenceKey());
            iep.setStudentOid(ownerIep.getStudentOid());
            iep.setStaffOid(ownerIep.getStaffOid());
        } else {
            iep = (IepData) getFormStorage();
        }

        return iep;
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
     * Loads each IEP subreport into a map for fast retrieval.
     */
    private void loadSubReports() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getRootOrganization().getOid());
        criteria.addIn(Report.COL_ID, Arrays.asList(new String[] {PAGE_1_FORMAT_ID,
                PAGE_2_FORMAT_ID,
                PAGE_4_FORMAT_ID,
                PAGE_5_FORMAT_ID,
                OVERFLOW_FORMAT_ID}));

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, 5);
    }

    /**
     * Prepares data for accommodation subreport.
     *
     * @param iep IepData
     */
    private void prepareAccommodationSubreport(IepData iep) {
        ReportDataGrid adaptationsGrid = new ReportDataGrid(6, 2);
        ReportDataGrid adjudicationsGrid = new ReportDataGrid(6, 2);

        Collection<IepAccommodation> adaptations = new ArrayList<IepAccommodation>();
        Collection<IepAccommodation> adjudications = new ArrayList<IepAccommodation>();

        Collection<IepAccommodation> accommodations = iep.getAccommodations(getBroker());
        if (!CollectionUtils.isEmpty(accommodations)) {
            for (IepAccommodation accommodation : accommodations) {
                String type = accommodation.getType();
                if (ACCOMMODATION_TYPE_ADAPTATION.equals(type)) {
                    adaptations.add(accommodation);
                } else if (ACCOMMODATION_TYPE_ADJUDICATION.equals(type)) {
                    adjudications.add(accommodation);
                }
            }
        }

        if (isBlank() || adaptations.isEmpty()) {
            IepAccommodation accommodation = new IepAccommodation(getBroker().getPersistenceKey());
            for (int i = 0; i <= 3; i++) {
                adaptations.add(accommodation);
            }
        }

        if (isBlank() || adjudications.isEmpty()) {
            IepAccommodation accommodation = new IepAccommodation(getBroker().getPersistenceKey());
            for (int i = 0; i <= 3; i++) {
                adjudications.add(accommodation);
            }
        }

        /*
         * Prepare adaptations grid
         */
        for (IepAccommodation adaptation : adaptations) {
            adaptationsGrid.append();
            adaptationsGrid.set(COL_ACCOMMODATION, adaptation);
            adaptationsGrid.set(COL_ACCOMMODATION_TYPE, ACCOMMODATION_TYPE_ADAPTATION);
        }

        /*
         * Prepare adjudications grid
         */
        for (IepAccommodation adjudication : adjudications) {
            adjudicationsGrid.append();
            adjudicationsGrid.set(COL_ACCOMMODATION, adjudication);
            adjudicationsGrid.set(COL_ACCOMMODATION_TYPE, ACCOMMODATION_TYPE_ADJUDICATION);
        }

        adaptationsGrid.beforeTop();
        adjudicationsGrid.beforeTop();

        Report accommodationSubreportFormat = ReportUtils.getReport(PAGE_2_SUB_FORMAT_ID, getBroker());

        addParameter(ADAPTATIONS_DATA_PARAM, adaptationsGrid);
        addParameter(ADAPTATIONS_FORMAT_PARAM,
                new ByteArrayInputStream(accommodationSubreportFormat.getCompiledFormat()));

        addParameter(ADJUDICATIONS_DATA_PARAM, adjudicationsGrid);

        // Cannot use the same input stream more than once within the same page, so, create create
        // another one.
        addParameter(ADJUDICATIONS_FORMAT_PARAM,
                new ByteArrayInputStream(accommodationSubreportFormat.getCompiledFormat()));
    }

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
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "IEP 1");

        setIepSchoolYear(iep);
        setMinistryDesignation(iep);
        setPrimaryContact(iep);
        prepareTeamSubreport(iep);
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
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "IEP 2");

        prepareAccommodationSubreport(iep);
    }

    /**
     * Prepares the fourth IEP page (IEP 4).
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void preparePage4(ReportDataGrid grid, IepData iep) {
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

                if (currentPage != null) {
                    currentPage.beforeTop();
                }

                currentPage = new ReportDataGrid(2, 1);

                grid.append();
                grid.set(COL_IEP, iep);
                grid.set(COL_SUBREPORT_DATA_SOURCE, currentPage);
                grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_4_FORMAT_ID));
                grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
                grid.set(COL_PAGE_IDENTIFIER, "IEP 4");

                preparePage4_addGoal(currentPage, goal);
            }
        }

        if (currentPage == null) {
            currentPage = new ReportDataGrid(2, 1);
        }

        if (currentPage.rowCount() == 0) {
            preparePage4_addGoal(currentPage, new IepGoal(getBroker().getPersistenceKey()));
        }

        currentPage.beforeTop();
    }

    /**
     * Adds the passed goal to the subreport data grid used on the IEP's goal page.
     *
     * @param currentPage ReportDataGrid
     * @param goal IepGoal
     */
    private void preparePage4_addGoal(ReportDataGrid currentPage, IepGoal goal) {
        if (!isBlank()) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(IepGoalObjective.COL_IEP_GOAL_OID, goal.getOid());

            QueryByCriteria query = new QueryByCriteria(IepGoalObjective.class, criteria);
            query.addOrderByAscending(IepGoalObjective.COL_SEQUENCE_NUMBER);

            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    IepGoalObjective objective = (IepGoalObjective) iterator.next();

                    currentPage.append();
                    currentPage.set(COL_GOAL, goal);
                    currentPage.set(COL_OBJECTIVE, objective);
                }
            } finally {
                iterator.close();
            }
        }
    }

    /**
     * Loads the review data entered into Forms F and G for the IEP.
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void preparePage5Reviews(ReportDataGrid grid, IepData iep) {
        ReportDataGrid reviewData = new ReportDataGrid();

        // Get Forms F/G of IEP
        Criteria criteria = new Criteria();
        criteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, iep.getOid());
        criteria.addIn(FormInstance.REL_FORM_DEFINITION + PATH_DELIMITER + FormDefinition.COL_ID,
                Arrays.asList(FORM_F_ID, FORM_G_ID));

        QueryByCriteria query = new QueryByCriteria(FormInstance.class, criteria);
        query.addOrderByAscending(FormInstance.REL_FORM_DEFINITION + PATH_DELIMITER + FormDefinition.COL_ID);

        // Iterate over form instances to get storage objects and data
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            DateAsStringConverter dateConverter = (DateAsStringConverter) ConverterFactory
                    .getConverterForClass(Converter.DATE_CONVERTER, getLocale(), true);
            while (iterator.hasNext()) {
                FormInstance formInstance = (FormInstance) iterator.next();
                GenericFormData formData = (GenericFormData) formInstance.getStorageObject(getBroker());

                DataDictionary dictionary = DataDictionary.getDistrictDictionary(formData.getExtendedDataDictionary(),
                        getBroker().getPersistenceKey());

                for (GenericFormChildData childData : formData.getGenericFormDataChildren(getBroker())) {
                    reviewData.append();
                    reviewData.set(COL_REVIEW_TYPE, formInstance.getFormDefinition().getName());
                    reviewData.set(COL_REVIEW_BY, childData.getFieldValueByAlias(REVIEW_BY_ALIAS, dictionary));
                    reviewData.set(COL_REVIEW_COMMENT,
                            childData.getFieldValueByAlias(REVIEW_COMMENT_ALIAS, dictionary));

                    String dateValue = (String) childData.getFieldValueByAlias(REVIEW_DATE_ALIAS, dictionary);
                    if (!StringUtils.isEmpty(dateValue)) {
                        reviewData.set(COL_REVIEW_DATE, dateConverter.parseSystemString(dateValue));
                    }
                }
            }
        } finally {
            iterator.close();
        }

        reviewData.beforeTop();

        grid.append();
        grid.set(COL_IEP, iep);
        grid.set(COL_SUBREPORT_DATA_SOURCE, reviewData);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_5_FORMAT_ID));
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "IEP 5");
    }

    /**
     * Prepares data for team subreport.
     *
     * @param iep IepData
     */
    private void prepareTeamSubreport(IepData iep) {
        ReportDataGrid teamGrid = new ReportDataGrid(6, 2);

        Collection<IepTeamMember> members = null;
        if (isBlank()) {
            members = new ArrayList<IepTeamMember>(3);

            IepTeamMember member = X2BaseBean.newInstance(IepTeamMember.class, getBroker().getPersistenceKey());
            members.add(member);
            members.add(member);
            members.add(member);
        } else {
            members = iep.getTeamMembers(getBroker());

            /*
             * If there are no team members, add a blank row.
             */
            if (CollectionUtils.isEmpty(members)) {
                members = new ArrayList<IepTeamMember>(1);

                IepTeamMember member = X2BaseBean.newInstance(IepTeamMember.class, getBroker().getPersistenceKey());
                members.add(member);
            }
        }

        for (IepTeamMember member : members) {
            String aliasSstAsStr = (String) member.getFieldValueByAlias(ALIAS_STUDENT_SUPPORT_TEAM);

            if (aliasSstAsStr == null || BooleanAsStringConverter.TRUE.equals(aliasSstAsStr)) {
                teamGrid.append();
                teamGrid.set(COL_TEAM_MEMBER_NAME, member.getNameView());
                teamGrid.set(COL_TEAM_MEMBER_TITLE, member.getMemberRoleCode());
            }
        }

        teamGrid.beforeTop();
        addParameter(COL_TEAM_SUBREPORT_DATA, teamGrid);

        Report teamSubreportFormat = ReportUtils.getReport(PAGE_1_SUB_FORMAT_ID, getBroker());
        if (teamSubreportFormat != null) {
            addParameter(COL_TEAM_SUBREPORT_FORMAT, new ByteArrayInputStream(teamSubreportFormat.getCompiledFormat()));
        }
    }

    /**
     * Looks up the student's grade level at the time of the IEP start date.
     *
     * @param iep void
     */
    private void setGradeLevel(IepData iep) {
        String gradeLevel = null;

        if (iep.getStartDate() != null) {
            GradeLevelHistory history = new GradeLevelHistory(iep.getStudentOid(), 20, getOrganization(), getBroker());
            gradeLevel = history.getGradeLevel(iep.getStudentOid(), iep.getStartDate());
        }

        addParameter(STD_GRADE_LEVEL, StringUtils.coalesce(gradeLevel,
                iep.getStudent().getGradeLevel(getCurrentContext().getOid(), getBroker())));
    }

    /**
     * Adds school year as a parameter to report based on IEP's referral date.
     *
     * @param iep void
     */
    private void setIepSchoolYear(IepData iep) {
        // Request is to always have the current school year at the top of the report
        String iepContextId = getCurrentContext().getContextId();
        addParameter(SCHOOL_YEAR_CONTEXT_PARAM, iepContextId);

        // The school as of the IEP start date should show on the IEP as opposed to the current
        // school.
        EnrollmentSnapshot snapshot = new EnrollmentSnapshot(iep.getStudent(), iep.getStartDate(), getBroker());
        SisSchool schoolAsOfIepDate = snapshot.getSchool();
        if (schoolAsOfIepDate != null) {
            addParameter(START_SCHOOL_PARAM, schoolAsOfIepDate);
        } else {
            addParameter(START_SCHOOL_PARAM, iep.getStudent().getSchool());
        }

        /*
         * Requirements from BC
         * Active IEPs should display implementation date and Review Date.
         * Historical IEPs( previous and discarded) should display the implementation date. Display
         * review date as blank.
         * Draft IEP- Implementation date and Review Date should be blank
         */
        if (iep.getStatusCode() != IepData.StatusCode.DRAFT.ordinal() &&
                iep.getStatusCode() != IepData.StatusCode.AMENDMENT_DRAFT.ordinal()) {
            // Do not populate the implementation date or the review date for draft IEPs.

            // Look up the workflow phase that implements the IEP to grab the implementation date.
            Collection<String> phaseOids = Arrays.asList(ENROLL_IMPLEMENT_PHASE_OID, IMPLEMENTATION_WORKFLOW_PHASE_OID,
                    IEP_REVIEW_IMPLEMENT_PHASE_OID);

            Criteria phaseCriteria = new Criteria();
            phaseCriteria.addEqualTo(
                    WorkflowProgress.REL_WORKFLOW + ModelProperty.PATH_DELIMITER + Workflow.COL_OWNER_OID,
                    iep.getOid());
            phaseCriteria.addIn(WorkflowProgress.COL_WORKFLOW_PHASE_OID, phaseOids);
            QueryByCriteria phaseQuery = new QueryByCriteria(WorkflowProgress.class, phaseCriteria);
            WorkflowProgress progressBean = (WorkflowProgress) getBroker().getBeanByQuery(phaseQuery);

            if (progressBean != null && progressBean.getDate() != null) {
                addParameter(IMPLEMENTATION_DATE_PARAM, progressBean.getDate());
            } else {
                addParameter(IMPLEMENTATION_DATE_PARAM, iep.getStartDate());
            }

            if (iep.getStatusCode() == IepData.StatusCode.ACTIVE.ordinal()) {
                // Only display the review date for Active IEPs
                // Set the review date to June of the school year this IEP was started in.
                Calendar cal = Calendar.getInstance();
                cal.setTime((PlainDate) getParameter(IMPLEMENTATION_DATE_PARAM));

                int year = cal.get(Calendar.YEAR);
                int month = cal.get(Calendar.MONTH);

                if (month > Calendar.JULY) {
                    // If the month is August -> December than next June is the proper year.
                    year++;
                }

                // Expectations are for the format to be June <Year> rather than a date string
                String reviewDate = REVIEW_MESSAGE_PREFIX + String.valueOf(year);
                addParameter(REVIEW_DATE_PARAM, reviewDate);
            }
        }
    }

    /**
     * Adds primary disability as the ministry designation parameter on the report.
     *
     * @param iep void
     * @return String
     */
    private void setMinistryDesignation(IepData iep) {
        String designation = "";

        if (!isBlank()) {
            // Use the primary designation for the student
            designation = (String) iep.getStudent().getFieldValueByAlias(BC_DESIGNATION_ALIAS);
        }

        addParameter(DESIGNATION_PARAM, designation);
    }

    /**
     * Adds students' primary contact (contact with lowest emergency priority) as a parameter to
     * report.
     *
     * @param iep void
     */
    private void setPrimaryContact(IepData iep) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentContact.COL_STUDENT_OID, iep.getStudentOid());

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField includeContactField =
                dictionary.findDataDictionaryFieldByAlias(ALIAS_INCLUDE_CONTACT_ON_IEP);
        if (includeContactField != null) {
            criteria.addEqualTo(includeContactField.getJavaName(), BooleanAsStringConverter.TRUE);
        } else {
            addNoMatchCriteria(criteria);
        }

        QueryByCriteria query = new QueryByCriteria(StudentContact.class, criteria);
        query.addOrderByAscending(StudentContact.COL_EMERGENCY_PRIORITY);

        StudentContact iepContact = (StudentContact) getBroker().getBeanByQuery(query);
        if (iepContact != null) {
            addParameter(IEP_CONTACT_PARAM, iepContact);
        }
    }
}
