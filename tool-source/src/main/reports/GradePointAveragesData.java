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

import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.tools.ToolRunException;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.GradePointAverageDefinition;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentGradePoint;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.tools.procedures.GradePointAverageProcedure;
import com.x2dev.utils.X2BaseException;

import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the Grade Point Average report. This report optionally invokes a selected GPA
 * procedure and generates a report of the results from the Student Grade Point table.
 *
 * @author X2 Development Corporation
 */
public class GradePointAveragesData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private static final int YEAR_COLUMNS = 4;

    // Input parameters
    private static final String CONTEXT_OID_PARAM = "contextOid";
    private static final String PARAM_ACTIVE_ONLY = "activeOnly";
    private static final String PARAM_CURRENT_YEAR_ONLY = "currentYearOnly";
    private static final String PARAM_GPA_DEFINITION_OID = "gpaDefinitionOid";
    private static final String PARAM_RANK_BY = "rankBy";
    private static final String PARAM_RECALCULATE = "recalculate";
    private static final String PARAM_SORT = "sort";
    private static final String PARAM_YOG = "yog";
    private static final String PARAM_UPDATE_STUDENT = "updateStudent";

    // private static final int ORDER_BY_GPA = 1;
    private static final int ORDER_BY_STUDENT = 2;

    // private static final int RANK_BY_GPA = 1;
    private static final int RANK_BY_POINTS = 2;

    // Grid column constants
    private static final String COL_STUDENT = "student";
    private static final String COL_GPA = "gpa";
    private static final String COL_POINTS = "points_"; // prefix; followed by a context OID
    private static final String COL_RANK = "rank";
    private static final String COL_TOTAL_WEIGHT = "totalWeight";
    private static final String COL_TOTAL_POINTS = "totalPoints";
    private static final String COL_WEIGHT = "weight_"; // prefix; followed by a context OID

    // Report parameters
    private static final String PARAM_CONTEXT_LOOKUP = "yearLookup";
    private static final String PARAM_GPA_DEFINITION = "gpaDefinition";
    private static final String PARAM_GRID = "grid";

    private DistrictSchoolYearContext m_context;
    private GradePointAverageDefinition m_gpaDefinition;
    private String m_gpaDefinitionOid;
    private Map m_pointLookupByOid;
    private Map m_pointLookupByStudent;
    private Integer m_rankBy;
    private Map m_rankMap;
    private Integer m_sort;
    private Integer m_yog;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Boolean activeOnly = (Boolean) getParameter(PARAM_ACTIVE_ONLY);
        Boolean currentYearOnly = (Boolean) getParameter(PARAM_CURRENT_YEAR_ONLY);
        Boolean recalculate = (Boolean) getParameter(PARAM_RECALCULATE);
        Boolean updateStudent = (Boolean) getParameter(PARAM_UPDATE_STUDENT);

        m_gpaDefinitionOid = (String) getParameter(PARAM_GPA_DEFINITION_OID);
        m_rankBy = (Integer) getParameter(PARAM_RANK_BY);
        m_sort = (Integer) getParameter(PARAM_SORT);
        m_yog = (Integer) getParameter(PARAM_YOG);

        String contextOid = (String) getParameter(CONTEXT_OID_PARAM);
        m_context = (DistrictSchoolYearContext) getBroker().getBeanByOid(
                DistrictSchoolYearContext.class, contextOid);

        if (recalculate.booleanValue()) {
            try {
                GradePointAverageProcedure.runGpa(m_gpaDefinitionOid, getOrganization(), (SisSchool) getSchool(),
                        m_context,
                        getParameters(), getJob().getTempFolder(), getBroker());
            } catch (ToolRunException pre) {
                AppGlobals.getLog().log(Level.WARNING, pre.getMessage(), pre);
            }
        }

        m_gpaDefinition = (GradePointAverageDefinition) getBroker().getBeanByOid(
                GradePointAverageDefinition.class, m_gpaDefinitionOid);

        addParameter(PARAM_GPA_DEFINITION, m_gpaDefinition);

        /*
         * Calculate class rank using the GradesManager. We must use a ModelBroker here since we
         * may be updating the student file.
         */
        ModelBroker broker = new ModelBroker(getPrivilegeSet());
        GradesManager gradesManager = new GradesManager(broker);

        boolean rankByPoints = m_rankBy.intValue() == RANK_BY_POINTS;
        try {
            m_rankMap = gradesManager.calculateRank(m_gpaDefinition,
                    rankByPoints,
                    getSchool().getOid(),
                    m_yog,
                    currentYearOnly.booleanValue(),
                    updateStudent.booleanValue(),
                    updateStudent.booleanValue(),
                    null,
                    activeOnly.booleanValue(),
                    getOrganization());
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        }

        /*
         * Populate the ReportDataGrid, which is the main structure for this report
         */
        ReportDataGrid grid = new ReportDataGrid(5000, 15);
        populateGrid(grid, activeOnly.booleanValue());

        /*
         * Load the context map, which is used by the format to determine which school years to
         * display in the 4 available columns
         */
        addParameter(PARAM_CONTEXT_LOOKUP, getContextMap());

        /*
         * Pass a reference to the grid as a report parameter since we use dynamic fields based on
         * the context map
         */
        addParameter(PARAM_GRID, grid);

        return grid;
    }
    
    /**
     * Release resources.
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#releaseResources()
     */
    @Override
    protected void releaseResources() {
        super.releaseResources();

        if (m_pointLookupByOid != null) {
            m_pointLookupByOid.clear();
        }
        m_pointLookupByOid = null;

        if (m_pointLookupByStudent != null) {
            m_pointLookupByStudent.clear();
        }
        m_pointLookupByStudent = null;

        if (m_rankMap != null) {
            m_rankMap.clear();
        }
        m_rankMap = null;
    }
    
    /**
     * 
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        /*
         * getJob().getTempFolder() is called in gatherData() method. 
         * This method is returning the users temporary folder for their login on the application server /temp/x2_xxxxx. 
         * However this folder does not exist on the report server. 
         * So runOnApplicationServer() has to be called here so that this report will be run at application server instead of report server
         */
        runOnApplicationServer();
        super.saveState(userData);
    }
    
    /**
     * Returns the context map, which is used by the format to determine the year column headers.
     *
     * @return HashMap of DistrictSchoolYearContext objects, keyed on numeric strings "0" - "3",
     *         corresponding to the 4 columns on the report format
     */
    private HashMap getContextMap() {
        HashMap contextMap = new HashMap(4);

        Criteria criteria = new Criteria();
        criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                Integer.valueOf(m_context.getSchoolYear()));

        QueryByCriteria contextQuery = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);
        contextQuery.addOrderByDescending(DistrictSchoolYearContext.COL_SCHOOL_YEAR);

        int yearIndex = 0;
        QueryIterator contexts = getBroker().getIteratorByQuery(contextQuery);
        try {
            while (contexts.hasNext() && yearIndex <= YEAR_COLUMNS) {
                DistrictSchoolYearContext context = (DistrictSchoolYearContext) contexts.next();
                contextMap.put(Integer.toString(yearIndex++), context);
            }
        } finally {
            contexts.close();
        }

        return contextMap;
    }

    /**
     * Loads a lookup map of keyed on OID containing the associated StudentGradePoint bean. This map
     * is loaded for performance reasons to avoid querying for a StudentGradePoint bean in each
     * iteration of the loop in populateGrid.
     */
    private void loadPointLookupByOid() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentGradePoint.COL_GRADE_POINT_AVERAGE_DEFINITION_OID, m_gpaDefinitionOid);
        if (m_yog != null) {
            criteria.addEqualTo(StudentGradePoint.REL_STUDENT + "." + SisStudent.COL_YOG, m_yog);
        }

        QueryByCriteria query =
                new QueryByCriteria(StudentGradePoint.class, criteria);

        m_pointLookupByOid = getBroker().getMapByQuery(query, X2BaseBean.COL_OID, 5000);
    }

    /**
     * Loads a lookup map keyed on student OID containing the list of StudentGradePoint beans in
     * school year order for the GPA being run.
     */
    private void loadPointLookupByStudent() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentGradePoint.COL_GRADE_POINT_AVERAGE_DEFINITION_OID,
                m_gpaDefinitionOid);
        if (m_yog != null) {
            criteria.addEqualTo(StudentGradePoint.REL_STUDENT + "." + SisStudent.COL_YOG, m_yog);
        }

        QueryByCriteria query =
                new QueryByCriteria(StudentGradePoint.class, criteria);
        query.addOrderByAscending(StudentGradePoint.REL_DISTRICT_CONTEXT + "." +
                DistrictSchoolYearContext.COL_SCHOOL_YEAR);

        m_pointLookupByStudent = getBroker().getGroupedCollectionByQuery(
                query, StudentGradePoint.COL_STUDENT_OID, 5000);
    }

    /**
     * Populates the "flattened" GPA point grid in either student or rank order.
     *
     *
     * @param grid ReportDataGrid
     * @param activeOnly boolean
     */
    private void populateGrid(ReportDataGrid grid, boolean activeOnly) {
        loadPointLookupByStudent();
        loadPointLookupByOid();

        Connection connection = getBroker().borrowConnection();
        Statement statement = null;
        ResultSet resultSet = null;
        try {
            boolean orderByStudent = m_sort.intValue() == ORDER_BY_STUDENT;
            boolean rankByPoints = m_rankBy.intValue() == RANK_BY_POINTS;

            String rankQuerySQL =
                    GradesManager.getRankQuerySQL(m_gpaDefinitionOid, orderByStudent, rankByPoints,
                            getSchool().getOid(), m_yog, null, m_gpaDefinition.getRankAllIndicator(), activeOnly,
                            getBroker());

            statement = connection.createStatement(ResultSet.TYPE_FORWARD_ONLY,
                    ResultSet.CONCUR_READ_ONLY);
            resultSet = statement.executeQuery(rankQuerySQL);
            while (resultSet.next()) {
                String oid = resultSet.getString("GPT_OID");

                StudentGradePoint latestPoint = (StudentGradePoint) m_pointLookupByOid.get(oid);

                grid.append();
                grid.set(COL_STUDENT, latestPoint.getStudent());
                grid.set(COL_GPA, latestPoint.getCumulativeGpa());
                grid.set(COL_TOTAL_POINTS, latestPoint.getCumulativeGradePoints());

                BigDecimal totalWeight = latestPoint.getCumulativeCourseWeight();

                grid.set(COL_TOTAL_WEIGHT, totalWeight);
                grid.set(COL_RANK, m_rankMap.get(latestPoint.getStudentOid()));

                List allPoints = (List) m_pointLookupByStudent.get(latestPoint.getStudentOid());
                Iterator allPointsIterator = allPoints.iterator();
                while (allPointsIterator.hasNext()) {
                    StudentGradePoint gradePoint = (StudentGradePoint) allPointsIterator.next();

                    BigDecimal weight = gradePoint.getCourseWeight();

                    grid.set(COL_WEIGHT + gradePoint.getDistrictContextOid(), weight);
                    grid.set(COL_POINTS + gradePoint.getDistrictContextOid(),
                            gradePoint.getGradePoints());
                }
            }
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            try {
                resultSet.close();
            } catch (Exception e) {
                // do nothing
            }

            try {
                statement.close();
            } catch (Exception e) {
                // do nothing
            }

            getBroker().returnConnection();
        }

        grid.beforeTop();
    }
}
