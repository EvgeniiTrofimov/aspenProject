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
package com.x2dev.reports.sys.sped.nh;

import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepGoal;
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.sis.model.business.sped.NewHampshireAliases;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the New Hampshire IEP form. This class prepares a ReportDataGrid that contains
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
 * <td>IEP 2 - Student Needs</td>
 * <td>SimpleFormDataSource</td>
 * </tr>
 * <tr>
 * <td>IEP 3 - Goals</td>
 * <td>ReportDataGrid containing IepGoal records</td>
 * </tr>
 * <tr>
 * <td>IEP 4 - Accommodations 1</td>
 * <td>ReportDataGrid containing Accommodations/Modifications</td>
 * </tr>
 * <tr>
 * <td>IEP 5 - Accommodations 2</td>
 * <td>SimpleFormDataSource</td>
 * </tr>
 * <tr>
 * <td>IEP 6 - Special Factors</td>
 * <td>SimpleFormDataSource</td>
 * </tr>
 * <tr>
 * <td>IEP 7 - Services</td>
 * <td>ReportDataGrid containing IepService records</td>
 * </tr>
 * </table>
 *
 * @author X2 Development Corporation
 */
public class IepFormData_Nh extends BaseFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * ReportDataGrid Column ID: goal (IepGoal.class)
     */
    private static final String COL_GOAL = "goal";

    /**
     * ReportDataGrid Column ID: IEP (IepData.class)
     */
    private static final String COL_IEP = "iep";

    /**
     * ReportDataGrid Column ID: page identifier (String)
     */
    private static final String COL_PAGE_IDENTIFIER = "pageIdentifier";

    /**
     * ReportDataGrid Column ID: page number (String)
     */
    private static final String COL_PAGE_NUMBER = "pageNumber";

    /**
     * ReportDataGrid Column ID: service (IepService.class)
     */
    private static final String COL_SERVICE = "service";

    /**
     * ReportDataGrid Column ID: datasource
     */
    private static final String COL_SUBREPORT_DATA_SOURCE = "datasource";

    /**
     * ReportDataGrid Column ID: format
     */
    private static final String COL_SUBREPORT_FORMAT = "format";

    /**
     * ReportDataGrid Column ID: modification-datasource
     * This is used in tandem with the main datasource column to permit double sub-reporting
     */
    private static final String COL_SUBREPORT_DATA_SOURCE_MOD = "datasourceMod";

    /**
     * ReportDataGrid Column ID: modification-format
     * This is used in tandem with the main format column to permit double sub-reporting
     */
    private static final String COL_SUBREPORT_FORMAT_MOD = "formatMod";

    /**
     * Number of days in the school week, used when determining the % hours per school
     * week spent receiving SPED services
     */
    private static final float DAYS_OF_WEEK = 5f;

    /**
     * Initial capacity of IEP goals section
     */
    private static final int IEP_INITIAL_CAPACITY = 50;

    /**
     * Parameter: IEP
     */
    private static final String IEP_PARAM = "iep";

    /**
     * Max number of services to store on one page
     */
    private static final int MAX_SECTION_SERVICES = 15;

    /**
     * Format ID: Overflow page
     */
    private static final String OVERFLOW_FORMAT_ID = "SYS-SPED-NH-OVERFLOW";

    /**
     * Format ID: IEP 1 (Vision statement)
     */
    private static final String PAGE_1_FORMAT_ID = "SYS-SPED-NH-IEP1";

    /**
     * Format ID: IEP 2 (Areas of weakness)
     */
    private static final String PAGE_2_FORMAT_ID = "SYS-SPED-NH-IEP2";

    /**
     * Format ID: IEP 3 (Goals)
     */
    private static final String PAGE_3_FORMAT_ID = "SYS-SPED-NH-IEP3";

    /**
     * Format ID: Subreport: IEP 3 (Goal Objectives)
     */
    private static final String PAGE_3_OBJ_FORMAT_ID = "SYS-SPED-NH-IEP3-OBJ";

    /**
     * Format ID: IEP 4 (Accommodations/Modifications)
     */
    private static final String PAGE_4_FORMAT_ID = "SYS-SPED-NH-IEP4";

    /**
     * Format ID: Subreport: IEP 4 (Accommodations)
     */
    private static final String PAGE_4_ACC_FORMAT_ID = "SYS-SPED-NH-IEP4-ACC";

    /**
     * Format ID: Subreport: IEP 4 (Modifications)
     */
    private static final String PAGE_4_MOD_FORMAT_ID = "SYS-SPED-NH-IEP4-MOD";

    /**
     * Format ID: IEP 5 (Assessment Accommodations)
     */
    private static final String PAGE_5_FORMAT_ID = "SYS-SPED-NH-IEP5";

    /**
     * Format ID: IEP 6 (Special Factors)
     */
    private static final String PAGE_6_FORMAT_ID = "SYS-SPED-NH-IEP6";

    /**
     * Format ID: IEP 7 (Services)
     */
    private static final String PAGE_7_FORMAT_ID = "SYS-SPED-NH-IEP7";

    /**
     * Parameter: Percentage of school week receiving SPED services
     */
    private static final String PARAM_SERVICE_PERCENT = "servicePercentage";

    /**
     * Parameter: Total # of hours spent receiving SPED services
     */
    private static final String PARAM_SERVICE_TOTAL = "serviceTotal";

    private IepData m_currentIep = null;
    private int m_currentPageNumber = 0;
    private Map m_goalData = null;
    private Map m_servicesData = null;
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
        loadServices();
        loadSubReports();

        IepData iep = getIep();

        preparePage1(grid, iep);
        preparePage2(grid, iep);
        preparePage3(grid, iep);
        preparePage4(grid, iep);
        preparePage5(grid, iep);
        preparePage6(grid, iep);
        preparePage7(grid, iep);

        grid.beforeTop();

        addParameter(IEP_PARAM, iep);

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
     * @return SimpleFormDataSource
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
     * Loads goal data into a map for fast retrieval. The map loaded is keyed on IEP OID and each
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
        servicesQuery.addOrderByAscending(IepService.COL_GOAL_VIEW);
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
                PAGE_3_OBJ_FORMAT_ID,
                PAGE_4_FORMAT_ID,
                PAGE_4_ACC_FORMAT_ID,
                PAGE_4_MOD_FORMAT_ID,
                PAGE_5_FORMAT_ID,
                PAGE_6_FORMAT_ID,
                PAGE_7_FORMAT_ID,
                OVERFLOW_FORMAT_ID}));

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, 10);
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
    }

    /**
     * Prepares the third IEP page (IEP 3).
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void preparePage3(ReportDataGrid grid, IepData iep) {
        /*
         * Each IEP 3 page can contain 1 goal. If there are multiple goals, we must
         * insert additional pages for IEP 3.
         */
        Collection goals = null;
        if (isBlank()) {
            IepGoal blankGoal = new IepGoal(getBroker().getPersistenceKey());
            blankGoal.setStudentOid(iep.getStudentOid());

            goals = new ArrayList<IepGoal>(1);
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

                grid.append();
                grid.set(COL_IEP, iep);
                grid.set(COL_SUBREPORT_DATA_SOURCE, currentPage);
                grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_3_FORMAT_ID));
                grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
                grid.set(COL_PAGE_IDENTIFIER, "IEP 3");

                preparePage3_addGoal(currentPage, goal);
                currentPage.beforeTop();
            }
        } else {
            currentPage = new ReportDataGrid(2, 1);
            currentPage.beforeTop();
        }
    }

    /**
     * Adds the passed goal to the subreport data grid used on the IEP's goal page.
     *
     * @param currentPage ReportDataGrid
     * @param goal IepGoal
     */
    private void preparePage3_addGoal(ReportDataGrid currentPage, IepGoal goal) {
        currentPage.append();
        currentPage.set(COL_GOAL, goal);
        currentPage.set(COL_SUBREPORT_DATA_SOURCE,
                new BeanCollectionDataSource(goal.getIepGoalObjectives(getBroker()), getDictionary(), getLocale()));
        currentPage.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_3_OBJ_FORMAT_ID));
    }

    /**
     * Prepares the fourth IEP page (IEP 4).
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void preparePage4(ReportDataGrid grid, IepData iep) {
        // Retrieve accommodations for this student/IEP pair
        Criteria accCriteria = new Criteria();
        accCriteria.addEqualTo(IepAccommodation.COL_IEP_DATA_OID, getIep().getOid());
        accCriteria.addEqualTo(IepAccommodation.COL_TYPE, NewHampshireAliases.TYPE_ACC);
        QueryByCriteria accQuery = new QueryByCriteria(IepAccommodation.class, accCriteria);
        accQuery.addOrderByAscending(IepAccommodation.COL_CATEGORY);

        // Retrieve modifications for this student/IEP pair
        Criteria modCriteria = new Criteria();
        modCriteria.addEqualTo(IepAccommodation.COL_IEP_DATA_OID, getIep().getOid());
        modCriteria.addEqualTo(IepAccommodation.COL_TYPE, NewHampshireAliases.TYPE_MOD);
        QueryByCriteria modQuery = new QueryByCriteria(IepAccommodation.class, modCriteria);
        modQuery.addOrderByAscending(IepAccommodation.COL_CATEGORY);

        ReportDataGrid currentPage = new ReportDataGrid(1, 4);

        currentPage.append();
        currentPage.set(COL_SUBREPORT_DATA_SOURCE,
                new BeanCollectionDataSource(getBroker().getCollectionByQuery(accQuery), getDictionary(), getLocale()));
        currentPage.set(COL_SUBREPORT_DATA_SOURCE_MOD,
                new BeanCollectionDataSource(getBroker().getCollectionByQuery(modQuery), getDictionary(), getLocale()));
        currentPage.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_4_ACC_FORMAT_ID));
        currentPage.set(COL_SUBREPORT_FORMAT_MOD, getSubreportFormat(PAGE_4_MOD_FORMAT_ID));
        currentPage.beforeTop();

        grid.append();
        grid.set(COL_IEP, iep);
        grid.set(COL_SUBREPORT_DATA_SOURCE, currentPage);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_4_FORMAT_ID));
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "IEP 4");
    }

    /**
     * Prepares the fifth IEP page (IEP 5).
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void preparePage5(ReportDataGrid grid, IepData iep) {
        grid.append();
        grid.set(COL_IEP, iep);
        grid.set(COL_SUBREPORT_DATA_SOURCE, getDataSource(iep, grid));
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_5_FORMAT_ID));
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
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
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "IEP 6");
    }

    /**
     * Prepares the seventh IEP page (IEP 7 - Services).
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void preparePage7(ReportDataGrid grid, IepData iep) {
        /*
         * Each IEP 7 page can contain 15 services. If there are more, we must insert additional
         * pages for IEP 7.
         */

        ReportDataGrid currentPage = new ReportDataGrid(MAX_SECTION_SERVICES, 1);

        Collection services = (Collection) m_servicesData.get(iep.getOid());

        // Determine the value of the calculated fields on the IEP Services page
        calculateServicePercent(services);

        // If there are no services, create a service list with one blank entry- so that the form
        // prints the service table
        if (services == null) {
            services = new ArrayList(1);
            IepService blankService = new IepService(getBroker().getPersistenceKey());
            services.add(blankService);
        }

        currentPage.setColumnValues(COL_SERVICE, services);

        currentPage.beforeTop();

        grid.append();
        grid.set(COL_IEP, iep);
        grid.set(COL_SUBREPORT_DATA_SOURCE, currentPage);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_7_FORMAT_ID));
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "IEP 7");
    }

    /**
     * Determines the total time spent receiving SPED services, as well as the percentage
     * of time spent receiving SPED services. If the total school hours is null/non-parsable
     * as a double, the PARAM_SERVICE_PERCENT parameter is not set.
     *
     * @param services Collection<IepService>
     */
    private void calculateServicePercent(Collection<IepService> services) {
        if (services != null) {
            // Resolve the reference tables associated with two of the service fields (that we need
            // to filter on)
            DataDictionary dictionary = getDictionary();
            ReferenceTable personnelTable = dictionary
                    .findDataDictionaryFieldByAlias(NewHampshireAliases.SERVICE_PERSONNEL).getReferenceTable();
            ReferenceTable serviceClassTable =
                    dictionary.findDataDictionaryFieldByAlias(NewHampshireAliases.SERVICE_CLASS).getReferenceTable();

            // Define filters by retrieving the reference codes with the defined sys_codes
            String filterA1 =
                    dictionary.findUserReferenceCode(personnelTable.getOid(), NewHampshireAliases.SYS_CODE_A1);
            String filterA2 =
                    dictionary.findUserReferenceCode(personnelTable.getOid(), NewHampshireAliases.SYS_CODE_A2);
            String filterB1 =
                    dictionary.findUserReferenceCode(serviceClassTable.getOid(), NewHampshireAliases.SYS_CODE_B1);

            try {
                double totalServiceHours = 0;
                double totalSchoolHours = 0;
                for (IepService current : services) {
                    String servicePersonnel = (String) current
                            .getFieldValueByAlias(NewHampshireAliases.SERVICE_PERSONNEL, getDictionary());
                    String serviceType =
                            (String) current.getFieldValueByAlias(NewHampshireAliases.SERVICE_CLASS, getDictionary());

                    /*
                     * The code below filters out services (omits them from the calculations) where:
                     * A) The personnel are consultants/paraprofessionals
                     * B) The service name is transportation
                     * Since they are not relevant when doing state reporting (for funding etc)
                     */
                    if ((servicePersonnel == null
                            || (!servicePersonnel.equals(filterA1) && !servicePersonnel.equals(filterA2))) &&
                            (serviceType == null || !serviceType.equals(filterB1))) {
                        double serviceTime = 0;
                        if (current.getDaysPerCycle() != 0) {
                            serviceTime = (current.getFrequency().doubleValue() * current.getDuration() / 60f)
                                    * DAYS_OF_WEEK / (current.getDaysPerCycle());
                        }
                        totalServiceHours += serviceTime;
                    }
                }
                addParameter(PARAM_SERVICE_TOTAL, Double.valueOf(totalServiceHours));

                String rawHourInput =
                        (String) getFormOwner().getFieldValueByAlias(NewHampshireAliases.SCHOOL_HOURS, getDictionary());
                if (rawHourInput != null) {
                    totalSchoolHours = Double.parseDouble(rawHourInput);
                }

                if (totalServiceHours != 0) {
                    addParameter(PARAM_SERVICE_PERCENT, Double.valueOf(totalServiceHours / totalSchoolHours));
                }
            } catch (NumberFormatException e) {
                // Unable to parse user input - do nothing
            }
        }
    }
}
