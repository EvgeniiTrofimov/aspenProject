/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2022 Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */

package com.x2dev.reports.sys.sped.dodea;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepGoalProgress;
import com.x2dev.sis.model.beans.SisDistrictSchoolYearContext;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * Report that lists the disabilities on current active IEPs.
 *
 * @author Follett School Solutions
 */
public class CumulativeProgressReport extends ReportJavaSourceNet {
    // Input parameters
    @SuppressWarnings("hiding")
    public static final String CONTEXT_OID_PARAM = "contextOid";
    public static final String POSTED_PARAM = "includeUnposted";
    private SisDistrictSchoolYearContext m_context;
    private SisStudent m_currentStudent;
    private IepData m_currentIep;

    private IepGoalProgress m_progressReport;

    private static final String ACCOMMODATION_SYSTEM_SUBREPORT_ID = "FSS-DOD-SPEDPR-009-SA";
    private static final String ACCOMMODATION_GENERAL_SUBREPORT_ID = "FSS-DOD-SPEDPR-009-GA";
    private static final String ACCOMMODATION_MODE_SYSTEM = "System";
    private static final String ACCOMMODATION_MODE_GENERAL = "General";
    private static final String COL_GENERAL_DESCRIPTION = "Description";
    private static final String COL_CATEGORY = "Category";
    private static final String COL_MAIN_AREA = "Main Area";
    private static final String COL_CONTENT_AREA = "Content Area";
    private static final String ACCOMMODATIONS_REFERENCE_TABLE_OID = "RTB0000004o1Fd";
    String contentArea = "";
    Map<String, String> codeMap = new HashMap<String, String>();
    Map<String, String> categoryMap = new HashMap<String, String>(128);
    Map<String, String> mainAreaMap = new HashMap<String, String>(128);

    /**
     * Name for the "accomodations system data source " report parameter. The value is a
     * JRDataSource object.
     *
     */
    public static final String SYSTEM_ACCOMMODATION_DATA_SOURCE_PARAM = "accommodationsSystemData";

    /**
     * Name for the "accomodations general data source " report parameter. The value is a
     * JRDataSource object.
     *
     */
    public static final String GENERAL_ACCOMMODATION_DATA_SOURCE_PARAM = "accommodationsGeneralData";

    /**
     * Name for the "ACCOMMODATION_SYSTEM format" report parameter. The value is a
     * java.io.InputStream object.
     */
    public static final String SYSTEM_ACCOMMODATION_FORMAT_PARAM = "accommodationsSystemFormat";

    /**
     * Name for the "ACCOMMODATION_GENERAL format" report parameter. The value is a
     * java.io.InputStream object.
     */
    public static final String GENERAL_ACCOMMODATION_FORMAT_PARAM = "accommodationsGeneralFormat";

    /**
     * Sys Accommodation Refeference Table OID.
     */
    public static final String SYS_ACC_REFTABLE = "accommodationsGeneralFormat";

    /**
     * Oid of a specific staff member to filter by. Chosen on input screen.
     */
    public static final String STAFF_OID = "staffOid";

    /**
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        String contextOid = (String) getParameter(CONTEXT_OID_PARAM);
        String staffOid = (String) getParameter(STAFF_OID);
        loadAccommodationMaps();
        Collection<String> iepQuery = new ArrayList<String>();
        m_context =
                (SisDistrictSchoolYearContext) getBroker().getBeanByOid(SisDistrictSchoolYearContext.class, contextOid);
        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepGoalProgress.COL_DISTRICT_CONTEXT_OID, m_context.getOid());

        if (!StringUtils.isEmpty(staffOid)) {
            criteria.addEqualTo(IepGoalProgress.COL_STAFF_OID, staffOid);
        }

        if (m_currentIep != null) {
            /*
             * Running for one studentm from IEP Tab
             */
            criteria.addEqualTo(IepGoalProgress.COL_STUDENT_OID, m_currentIep.getStudentOid());
        }
        if (m_currentStudent != null) {
            /*
             * Running for one student from Student Tab
             */
            criteria.addEqualTo(IepGoalProgress.COL_STUDENT_OID, m_currentStudent.getOid());

        } else {
            // Current selection
            SubQuery subQuery;
            if (m_currentIep != null) {
                Criteria iepCriteria = new Criteria();
                iepCriteria.addEqualTo(X2BaseBean.COL_OID, m_currentIep.getOid());
                subQuery = new SubQuery(IepData.class, X2BaseBean.COL_OID, iepCriteria);
            } else {
                subQuery = new SubQuery(IepData.class, X2BaseBean.COL_OID, getCurrentCriteria());
            }
            Criteria studentCriteria = new Criteria();
            studentCriteria.addIn(X2BaseBean.COL_OID, subQuery);
            SubQuery studentQuery = new SubQuery(IepData.class, IepData.COL_STUDENT_OID, studentCriteria);
            criteria.addIn(IepGoalProgress.COL_STUDENT_OID, studentQuery);

        }



        QueryByCriteria query = new QueryByCriteria(IepGoalProgress.class, criteria);
        query.addOrderByAscending(IepGoalProgress.REL_STUDENT + "." + Student.COL_NAME_VIEW);
        query.addOrderByAscending(IepGoalProgress.COL_DATE);
        query.addOrderByAscending(IepGoalProgress.REL_IEP_DATA + "." + X2BaseBean.COL_OID);


        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                IepGoalProgress goalProgress = (IepGoalProgress) iterator.next();
                iepQuery.add(goalProgress.getIepDataOid());
                // AppGlobals.getLog().severe("Iterating through the Progress Records");

            }
        } finally {
            iterator.close();
        }

        if (iepQuery.size() == 0) {
            iepQuery.add("==no-match==");
        }
        Criteria acc_criteria = new Criteria();
        acc_criteria.addIn(IepAccommodation.COL_IEP_DATA_OID, iepQuery);

        loadAccommodationData(acc_criteria);


        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query),
                DataDictionary.getDistrictDictionary(getExtendedDictionary(), getBroker().getPersistenceKey()),
                true,
                getLocale());
    }

    /**
     * Queries for all accommodation data across each IEP. Data is loaded based on IEP OID.
     */

    protected void loadAccommodationData(Criteria m_accommodationCriteria) {
        Map<String, ReportDataGrid> accommodationGeneralGridLookup = new HashMap<String, ReportDataGrid>();
        Map<String, ReportDataGrid> accommodationSystemGridLookup = new HashMap<String, ReportDataGrid>();
        QueryByCriteria query = new QueryByCriteria(IepAccommodation.class, m_accommodationCriteria);
        query.addOrderByAscending(IepAccommodation.COL_IEP_DATA_OID);
        query.addOrderByAscending(IepAccommodation.COL_TYPE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                IepAccommodation accommodation = (IepAccommodation) iterator.next();

                if (accommodation.getType() != null) {

                    if (ACCOMMODATION_MODE_GENERAL.contains(accommodation.getType())) {
                        String iepOid = accommodation.getIepDataOid();

                        ReportDataGrid asmGeneralGrid = accommodationGeneralGridLookup.get(iepOid);
                        if (asmGeneralGrid == null) {
                            asmGeneralGrid = new ReportDataGrid();
                            accommodationGeneralGridLookup.put(iepOid, asmGeneralGrid);
                        }


                        contentArea = loadContentAreas(accommodation);
                        asmGeneralGrid.append();
                        asmGeneralGrid.set(COL_GENERAL_DESCRIPTION, accommodation.getFieldD001());
                        asmGeneralGrid.set(COL_CONTENT_AREA, contentArea);



                    } else if (ACCOMMODATION_MODE_SYSTEM.contains(accommodation.getType())) {
                        String iepSystemOid = accommodation.getIepDataOid();

                        ReportDataGrid asmSystemGrid = accommodationSystemGridLookup.get(iepSystemOid);
                        if (asmSystemGrid == null) {
                            asmSystemGrid = new ReportDataGrid();
                            accommodationSystemGridLookup.put(iepSystemOid, asmSystemGrid);
                        }

                        contentArea = loadContentAreas(accommodation);
                        asmSystemGrid.append();
                        asmSystemGrid.set(COL_GENERAL_DESCRIPTION, codeMap.get(accommodation.getFieldB001()));
                        asmSystemGrid.set(COL_CONTENT_AREA, contentArea);
                        asmSystemGrid.set(COL_MAIN_AREA, categoryMap.get(accommodation.getFieldB001()));
                        asmSystemGrid.set(COL_CATEGORY, mainAreaMap.get(accommodation.getFieldB001()));


                    }

                }

            }

        } finally {
            iterator.close();
        }

        /*
         * For both of the accommodation grids, they need to also call beforeTop() in the iReport
         * format. Although this is a bit hackish, it's better than re-engineering the entire
         * report. Adding a note in the javasource in the event it is removed in the future
         * unintentionally.
         */

        for (String iepOid : accommodationSystemGridLookup.keySet()) {
            ReportDataGrid asmSystemGrid = accommodationSystemGridLookup.get(iepOid);
            asmSystemGrid.beforeTop();
        }

        for (String iepOid : accommodationGeneralGridLookup.keySet()) {
            ReportDataGrid asmGeneralGrid = accommodationGeneralGridLookup.get(iepOid);
            asmGeneralGrid.beforeTop();
        }

        addParameter(SYSTEM_ACCOMMODATION_DATA_SOURCE_PARAM, accommodationSystemGridLookup);
        addParameter(GENERAL_ACCOMMODATION_DATA_SOURCE_PARAM, accommodationGeneralGridLookup);

        Report report = ReportUtils.getReport(ACCOMMODATION_SYSTEM_SUBREPORT_ID, getBroker());
        addParameter(SYSTEM_ACCOMMODATION_FORMAT_PARAM, new ByteArrayInputStream(report.getCompiledFormat()));

        report = ReportUtils.getReport(ACCOMMODATION_GENERAL_SUBREPORT_ID, getBroker());
        addParameter(GENERAL_ACCOMMODATION_FORMAT_PARAM, new ByteArrayInputStream(report.getCompiledFormat()));


    }


    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        /*
         * If we're in the context of a single student, print the report for just that student
         */
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
        m_progressReport = userData.getCurrentRecord(IepGoalProgress.class);
        m_currentIep = userData.getCurrentRecord(IepData.class);
        if (m_progressReport != null) {
            m_currentIep = m_progressReport.getIepData();
        }


    }


    /**
     * Loads the accommodations (alias "iep-accomodations") Main Area and Categories into maps that
     * can be used for the report.
     */
    private void loadAccommodationMaps() {
        /*
         * Build the criteria
         */

        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, ACCOMMODATIONS_REFERENCE_TABLE_OID);

        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        query.addOrderByAscending(X2BaseBean.COL_OID);

        QueryIterator codes = getBroker().getIteratorByQuery(query);

        /*
         * Iterate over the selected values building a map of the categories and map of main area
         */
        try {
            while (codes.hasNext()) {

                ReferenceCode code = (ReferenceCode) codes.next();
                codeMap.put(code.getCode(), code.getDescription());
                mainAreaMap.put(code.getCode(), code.getStateCode());
                categoryMap.put(code.getCode(), code.getCategory());

            }

        } finally {
            codes.close();
        }

    }


    /**
     * Loads the content Area information used on the report format.
     */
    private String loadContentAreas(IepAccommodation accommodation) {
        String contentAreas = "";
        if (accommodation.getFieldA003() != null) {
            if ("Yes".contains(accommodation.getFieldA003())) {
                contentAreas = "M";
            }
        }

        if (accommodation.getFieldA004() != null) {
            if ("Yes".contains(accommodation.getFieldA004()) && contentAreas.length() > 0) {
                contentAreas = contentAreas + ", LA";
            } else {
                if ("Yes".contains(accommodation.getFieldA004())) {
                    contentAreas = contentAreas + "LA";
                }
            }
        }

        if (accommodation.getFieldA005() != null) {
            if ("Yes".contains(accommodation.getFieldA005()) && contentAreas.length() > 0) {
                contentAreas = contentAreas + ", R";
            } else {
                if ("Yes".contains(accommodation.getFieldA005())) {
                    contentAreas = contentAreas + "R";
                }
            }
        }

        if (accommodation.getFieldA002() != null) {
            if ("Yes".contains(accommodation.getFieldA002()) && contentAreas.length() > 0) {
                contentAreas = contentAreas + ", S";
            } else {
                if ("Yes".contains(accommodation.getFieldA002())) {
                    contentAreas = contentAreas + "S";
                }
            }
        }

        if (accommodation.getFieldA001() != null) {
            if ("Yes".contains(accommodation.getFieldA001()) && contentAreas.length() > 0) {
                contentAreas = contentAreas + ", SS";
            } else {
                if ("Yes".contains(accommodation.getFieldA001())) {
                    contentAreas = contentAreas + "SS";
                }
            }
        }
        return contentAreas;
    }
}
