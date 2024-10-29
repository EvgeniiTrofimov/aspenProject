/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2004 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.DatabaseOptimizer;
import com.follett.fsc.core.framework.persistence.DatabaseOptimizerFactory;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.nav.FilterException;
import com.follett.fsc.core.k12.web.tools.QuickChartAction;
import com.x2dev.sis.model.beans.GraduationCourseRequirement;
import com.x2dev.sis.model.beans.GraduationProgram;
import com.x2dev.sis.model.beans.GraduationRequirement;
import com.x2dev.sis.model.beans.GraduationRequirementHistory;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.GraduationManager;
import com.x2dev.sis.tools.reports.GraduationReportData;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;
import org.jdom.JDOMException;

/**
 * Prepares the data for the "Graduation Progress" report.
 *
 * @author X2 Development Corporation
 */
public class GraduationProgressChartData extends GraduationReportData {
    private static final long serialVersionUID = 1L;

    /**
     * Name for the parameter for context.
     */
    private static final String BREAK_BY_PARAM = "breakByParam";

    /**
     * Name for the parameter for context.
     */
    private static final String CONTEXT_OID_PARAM = "contextOid";

    /**
     * Name for the parameter for creating history records.
     */
    private static final String CREATE_HISTORY_PARAM = "createHistory";

    /**
     * Name for the enumerated "filter" report parameter. The value is an Integer.
     */
    public static final String EXCLUDE_FRESHMAN_PARAM = "excludeFreshman";

    /**
     * Name for the enumerated "filter" report parameter. The value is an Integer.
     */
    public static final String FILTER_BY_PARAM = "filterBy";

    /**
     * Name for the enumerated "program studies oid" report parameter. The value is an String.
     */
    public static final String PROGRAM_STUDIES_BY_PARAM = "programStudiesOid";

    /**
     * Name for the "program studies" report parameter. The value is an ProgramStudies object.
     */
    public static final String PROGRAM_STUDIES_PARAM = "programStudies";

    /**
     * Name for the "requirement" report parameter. The value is an ProgramStudies object.
     */
    public static final String REQUIREMENT_PARAM = "requirement";

    /**
     * Name for the specific requirement report parameter. This value is a String.
     */
    public static final String REQUIREMENT_BY_PARAM = "requirementOid";

    /**
     * Name for the enumerated "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "selection" report parameter. The value is a String.
     */
    public static final String SCHOOL_QUERY_BY_PARAM = "schoolQueryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String SCHOOL_QUERY_STRING_PARAM = "schoolQueryString";


    protected static final String GRAPH_SUBREPORT_ID = "SYS-GRQ-002-SUB1";
    protected static final String SUMMARY_SUBREPORT_ID = "SYS-GRQ-002-SUB2";

    protected static final String BREAK_CHART = "breakData";
    protected static final String CATEGORY = "category";
    protected static final String CATEGORY_CONSTANT = "'CATEGORY_CONSTANT'";
    protected static final String CHART_DATA = "chartData";
    protected static final String COLUMN_CATEGORY = "categoryField";
    protected static final String COLUMN_COUNT = "countField";
    protected static final String COLUMN_SERIES = "seriesField";
    protected static final String DECIMAL_FORMAT = "decimalFormat";
    protected static final String FRESHMAN_GRADE_LEVEL = "09";
    protected static final String GRAPH_FORMAT = "graphFormat";
    protected static final String SERIES = "series";
    protected static final String SERIES_CONSTANT = "'SERIES_CONSTANT'";
    protected static final String SUMMARY_DATA = "chartData2";
    protected static final String SUMMARY_FORMAT = "summaryFormat";
    protected static final String TOTAL_COUNT = "totalCount";

    private X2Criteria m_additionalCriteria;
    private String m_breakColumn;
    private boolean m_createHistory;
    private String m_contextOid;
    private Map<String, BigDecimal> m_countMap;
    private GraduationManager m_graduationManager;
    private boolean m_excludeFreshman;
    private DatabaseOptimizer m_optimizer = null;
    private String m_programStudiesOid;
    private String m_requirementOid;
    private ReportDataGrid m_summaryData = null;


    /**
     * Builds the query which uses the criteria from the user's current list and
     * adds the necessary group by and sorting.
     *
     * @param additionalCriteria X2Criteria
     * @return ColumnQuery
     */
    protected ColumnQuery buildQuery(X2Criteria additionalCriteria) {
        String categoryOrderSql = "SKL_SCHOOL_ID";
        String seriesOrderSql =
                "IF (GRH_STATUS < 25.000, '0', IF (GRH_STATUS < 75.000 AND GRH_STATUS > 25.000, '25', '75'))";
        String categoryGroupSql =
                "IF (GRH_STATUS < 25.000, '0-25', IF (GRH_STATUS < 75.000 AND GRH_STATUS > 25.000, '25-75', '75-100'))";
        String seriesGroupSql = seriesOrderSql;

        LinkedHashSet<String> orderColumns = new LinkedHashSet<String>();
        orderColumns.add(categoryOrderSql);
        orderColumns.add(seriesOrderSql);

        LinkedHashSet<String> columns = new LinkedHashSet<String>();
        columns.add("COUNT(*)");
        columns.add(categoryGroupSql);
        columns.add(seriesGroupSql);
        columns.add(categoryOrderSql);
        columns.add(seriesOrderSql);
        X2Criteria gradCriteria = new X2Criteria();
        if (m_excludeFreshman) {
            gradCriteria.addGreaterThan(GraduationRequirementHistory.REL_STUDENT + "." + SisStudent.COL_GRADE_LEVEL,
                    FRESHMAN_GRADE_LEVEL);
        }
        gradCriteria.addAndCriteria(additionalCriteria);

        BeanQuery currentQuery = createQueryByCriteria(GraduationRequirementHistory.class, gradCriteria);
        ColumnQuery query = new ColumnQuery(currentQuery, columns.toArray(new String[columns.size()]));

        query.addGroupBy(categoryOrderSql);
        query.addGroupBy(categoryGroupSql);
        query.addGroupBy(seriesGroupSql);

        query.addOrderByAscending(categoryOrderSql);
        query.addOrderByAscending(seriesOrderSql);

        return query;
    }


    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        m_optimizer = DatabaseOptimizerFactory.getDatabaseOptimizer(getBroker().getPersistenceKey());

        Report graphReport = ReportUtils.getReport(GRAPH_SUBREPORT_ID, getBroker());
        Report summaryReport = ReportUtils.getReport(SUMMARY_SUBREPORT_ID, getBroker());
        m_breakColumn = (String) getParameter(BREAK_BY_PARAM);


        Criteria schoolCriteria = new X2Criteria();
        String[] columns = new String[] {m_breakColumn, "COUNT(*)"};
        ReportQueryByCriteria schoolQuery = new ReportQueryByCriteria(School.class, columns, schoolCriteria);
        schoolQuery.addGroupBy(m_breakColumn);
        schoolQuery.addOrderBy(m_breakColumn, true);

        ReportDataGrid grid = new ReportDataGrid();

        QueryIterator iterator = null;
        try {
            iterator = getBroker().getReportQueryIteratorByQuery(schoolQuery);
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                X2Criteria criteria = new X2Criteria();
                criteria.addAndCriteria(m_additionalCriteria);
                criteria.addEqualTo(GraduationRequirementHistory.REL_SCHOOL + "." + m_breakColumn, row[0]);
                int count = ((Long) row[1]).intValue();

                for (int i = 0; i < count; i += 5) {
                    grid.append();
                    grid.set(SUMMARY_DATA, gatherSummaryData(criteria, (String) row[0])); // Summary
                                                                                          // data
                                                                                          // first,
                                                                                          // usually
                                                                                          // chartData
                                                                                          // will be
                                                                                          // a
                                                                                          // subset
                                                                                          // of the
                                                                                          // summary
                                                                                          // data
                    grid.set(CHART_DATA, gatherChartData());
                    grid.set(GRAPH_FORMAT, new ByteArrayInputStream(graphReport.getCompiledFormat()));
                    grid.set(SUMMARY_FORMAT, new ByteArrayInputStream(summaryReport.getCompiledFormat()));
                    grid.set(BREAK_CHART, row[0]);

                    if (m_summaryData.getRows().isEmpty()) {
                        grid.deleteRow();
                    }
                }
            }
        } catch (Exception e) {
            String message = (e.getMessage() == null) ? "" : e.getMessage();
            AppGlobals.getLog().log(Level.WARNING, message, e);
        } finally {
            if (iterator != null) {
                iterator.close();
            }
        }
        addParameter("countMap", m_countMap);
        addParameter(DECIMAL_FORMAT, new DecimalFormat("##0.00%"));
        addParameter(CATEGORY, "School Id");
        addParameter(SERIES, "Percentage Complete Range");
        addParameter(QuickChartAction.FORMAT_TITLE, "Graduation Progress Breakdown");
        addParameter(QuickChartAction.GROUP_OPTION_DATA_SET, Boolean.valueOf(true));
        addParameter(QuickChartAction.GROUP_OPTION_PAGE_BREAK, Boolean.valueOf(true));
        grid.beforeTop();

        return grid;

    }

    /**
     * Gather chart data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.QuickChartData#gatherChartData()
     */
    protected Object gatherChartData() throws Exception {
        // Use the summary grid, but consolidate rows less than the OTHER_GROUP_PERCENTAGE into a
        // single row
        ReportDataGrid grid = new ReportDataGrid();

        m_summaryData.beforeTop();
        while (m_summaryData.next()) {
            grid.append(m_summaryData.getCurrentRow());
        }

        m_summaryData.beforeTop();
        grid.beforeTop();

        return grid;
    }

    /**
     * Gather summary data.
     *
     * @param criteria X2Criteria
     * @param key String
     * @return Object
     * @see com.follett.fsc.core.k12.tools.reports.QuickChartData#gatherSummaryData()
     */
    protected Object gatherSummaryData(X2Criteria criteria, String key) {
        m_summaryData = new ReportDataGrid();

        QueryIterator iterator = null;
        try {
            BigDecimal totalCount = new BigDecimal(0);

            iterator = getBroker().getReportQueryIteratorByQuery(buildQuery(criteria));
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                BigDecimal count = new BigDecimal(m_optimizer.getCount(row[0]));
                String category = (String) row[3];
                String series = (String) row[1];

                m_summaryData.append();
                m_summaryData.set(COLUMN_COUNT, count);
                m_summaryData.set(COLUMN_CATEGORY, category);
                m_summaryData.set(COLUMN_SERIES, series);

                totalCount = totalCount.add(count);
            }
            m_countMap.put(key, totalCount);
        } catch (Exception e) {
            String message = (e.getMessage() == null) ? "" : e.getMessage();
            AppGlobals.getLog().log(Level.WARNING, message, e);
        } finally {
            if (iterator != null) {
                iterator.close();
            }
        }

        m_summaryData.beforeTop();

        return m_summaryData;
    }



    /**
     * (non-Javadoc).
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_graduationManager = new GraduationManager(getBroker());
        m_programStudiesOid = (String) getParameter(PROGRAM_STUDIES_BY_PARAM);
        m_requirementOid = (String) getParameter(REQUIREMENT_BY_PARAM);
        m_contextOid = (String) getParameter(CONTEXT_OID_PARAM);
        m_countMap = new HashMap<String, BigDecimal>();
        m_excludeFreshman = ((Boolean) getParameter(EXCLUDE_FRESHMAN_PARAM)).booleanValue();
        m_additionalCriteria = new X2Criteria();
        m_createHistory = ((Boolean) getParameter(CREATE_HISTORY_PARAM)).booleanValue();

        GraduationProgram program =
                (GraduationProgram) getBroker().getBeanByOid(GraduationProgram.class, m_programStudiesOid);
        addParameter(PROGRAM_STUDIES_PARAM, program);

        GraduationRequirement requirement =
                (GraduationRequirement) getBroker().getBeanByOid(GraduationRequirement.class, m_requirementOid);
        addParameter(REQUIREMENT_PARAM, requirement);

        X2Criteria criteria = new X2Criteria();

        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        if (queryBy.equals(SELECTION_SPECIAL_CASE_PREFIX + CURRENT_KEY)) {
            criteria = getCurrentCriteria();
            if (getSchool() != null) {
                criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
            } else {
                criteria.addAndCriteria(getOrganizationCriteria(SisStudent.class));
            }
        } else {
            addUserCriteria(criteria, queryBy, (String) getParameter(QUERY_STRING_PARAM), null, null);

            X2Criteria studentCriteria = getStudentCritera(program.getOid());
            criteria.addAndCriteria(studentCriteria);
        }

        String schoolQueryBy = (String) getParameter(SCHOOL_QUERY_BY_PARAM);
        String schoolQueryString = (String) getParameter(SCHOOL_QUERY_STRING_PARAM);

        if (!"##all".equals(schoolQueryBy)) {
            m_additionalCriteria.addEqualTo(GraduationRequirementHistory.REL_SCHOOL + "." + schoolQueryBy,
                    schoolQueryString);
        }

        m_additionalCriteria.addEqualTo(GraduationRequirementHistory.COL_CONTEXT_OID, m_contextOid);
        m_additionalCriteria.addEqualTo(GraduationRequirementHistory.COL_PROGRAM_OID, m_programStudiesOid);
        if (!StringUtils.isEmpty(m_requirementOid)) {
            m_additionalCriteria.addEqualTo(GraduationRequirementHistory.COL_REQUIREMENT_OID, m_requirementOid);
        }

        if (getSchool() != null) {
            m_additionalCriteria.addEqualTo(GraduationRequirementHistory.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            m_additionalCriteria.addAndCriteria(getOrganizationCriteria(GraduationRequirementHistory.class));
        }

        SubQuery subquery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, criteria);
        m_additionalCriteria.addIn(GraduationRequirementHistory.COL_STUDENT_OID, subquery);

        QueryByCriteria query = new QueryByCriteria(GraduationRequirementHistory.class, m_additionalCriteria);
        int count = getBroker().getCount(query);

        if (count == 0 || m_createHistory) {
            try {
                createHistoryData(true, criteria);
            } catch (Exception e) {
                throw new X2BaseException(e);
            }
        }
    }


    /**
     * Creates the history data.
     *
     * @param needHistory boolean
     * @param additionalCriteria X2Criteria
     * @throws FilterException exception
     * @throws JDOMException exception
     * @throws IOException Signals that an I/O exception has occurred.
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    protected void createHistoryData(boolean needHistory, X2Criteria additionalCriteria)
            throws FilterException, JDOMException, IOException {
        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, additionalCriteria);
        studentQuery.addOrderByAscending(SisStudent.COL_SCHOOL_OID);

        SubQuery subquery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, additionalCriteria);

        /*
         * Clear current year history always if creating history.
         */
        if (m_createHistory) {
            clearGraduationHistory(subquery, m_programStudiesOid);
        }

        /*
         * Get a map of the courses with partial credit course requirements.
         */
        X2Criteria partialCourseReqCriteria = new X2Criteria();
        partialCourseReqCriteria.addEqualTo(
                GraduationCourseRequirement.REL_REQUIREMENT + "." + GraduationRequirement.COL_PROGRAM_STUDIES_OID,
                m_programStudiesOid);
        partialCourseReqCriteria.addNotEqualTo(GraduationCourseRequirement.COL_PARTIAL_CREDIT, Double.valueOf("0.0"));

        QueryByCriteria partialQuery = new QueryByCriteria(GraduationCourseRequirement.class, partialCourseReqCriteria);

        Map<String, List<GraduationCourseRequirement>> partialCourseRequirments =
                getBroker().getGroupedCollectionByQuery(partialQuery, GraduationCourseRequirement.COL_COURSE_OID, 100);

        QueryIterator iterator = getBroker().getIteratorByQuery(studentQuery);

        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();

                HashMap<String, List<SchoolCourse>> coursesGainedCredit = new HashMap<String, List<SchoolCourse>>();
                HashMap<String, List<SchoolCourse>> coursesTaken = new HashMap<String, List<SchoolCourse>>();
                HashMap<String, List<SchoolCourse>> coursesTaking = new HashMap<String, List<SchoolCourse>>();
                HashMap<String, Double> creditsGained = new HashMap<String, Double>();
                HashMap<String, Double> rawCreditsGained = new HashMap<String, Double>();
                HashMap<String, Double> creditsWaived = new HashMap<String, Double>();
                HashMap<String, Double> creditsRequired = new HashMap<String, Double>();
                HashMap<String, Double> creditsByCourse = new HashMap<String, Double>();
                HashMap<String, Double> creditsInProgress = new HashMap<String, Double>();
                HashMap<String, String> gradeLevelByCourse = new HashMap<String, String>();
                Map<String, Map<String, Object>> otherRequirementValues = new HashMap<String, Map<String, Object>>();
                List<String> satisfiedOtherRequirementOids = new ArrayList<String>();

                m_graduationManager.determineGraduationStatus(student,
                        getUserData(),
                        m_programStudiesOid,
                        coursesGainedCredit,
                        coursesTaken,
                        coursesTaking,
                        new HashMap<String, List<SchoolCourse>>(),
                        new HashMap<String, List<String>>(),
                        creditsGained,
                        rawCreditsGained,
                        creditsWaived,
                        creditsRequired,
                        creditsByCourse,
                        creditsInProgress,
                        new HashMap<String, Double>(),
                        gradeLevelByCourse,
                        false,
                        partialCourseRequirments,
                        new HashMap<String, Map<String, String>>(),
                        otherRequirementValues,
                        satisfiedOtherRequirementOids);

                String totalCompleted = "";
                double totalWaived = 0;
                double status = 0;
                double totalInProgress = 0;
                double required = 0;

                if (StringUtils.isEmpty(m_requirementOid)) {
                    totalCompleted = String.valueOf(
                            m_graduationManager.getTotalCreditsGained(m_programStudiesOid, null, rawCreditsGained));
                    totalWaived = m_graduationManager.getTotalWaiver(m_programStudiesOid, null, creditsWaived);
                    status = m_graduationManager.getProgramSatisfiedStatus(m_programStudiesOid, creditsGained,
                            rawCreditsGained,
                            creditsWaived, creditsRequired, satisfiedOtherRequirementOids, null);
                    totalInProgress = m_graduationManager.getTotalCreditsInProgress(creditsInProgress);
                    required = m_graduationManager.getTotalRequired(m_programStudiesOid, null, creditsRequired);
                } else {
                    status = m_graduationManager.getRequirementSatisfiedStatus(m_requirementOid, creditsGained,
                            rawCreditsGained, creditsWaived, creditsRequired, satisfiedOtherRequirementOids);
                    if (otherRequirementValues.get(m_requirementOid) != null
                            && !otherRequirementValues.get(m_requirementOid).isEmpty()) {
                        Map<String, Object> otherValues = otherRequirementValues.get(m_requirementOid);

                        for (String key : otherValues.keySet()) {
                            if (otherValues.get(key) instanceof String) {
                                String[] values = ((String) otherValues.get(key)).split(":");
                                totalCompleted = totalCompleted.concat(values[1] + " ");
                            }
                        }
                    } else {
                        totalCompleted = String.valueOf(
                                m_graduationManager.getTotalCreditsGained(null, m_requirementOid, rawCreditsGained));
                    }
                    totalWaived = m_graduationManager.getTotalWaiver(null, m_requirementOid, creditsWaived);
                    totalInProgress = 0;
                    if (coursesTaking.get(m_requirementOid) != null) {
                        for (SchoolCourse course : coursesTaking.get(m_requirementOid)) {
                            totalInProgress += course.getCredit().doubleValue();
                        }
                    }
                    required = m_graduationManager.getTotalRequired(null, m_requirementOid, creditsRequired);
                }

                if (m_createHistory || needHistory) {
                    /*
                     * Creates the history
                     */
                    createGraduationHistory(m_programStudiesOid, m_requirementOid, student, coursesTaking,
                            creditsGained, rawCreditsGained,
                            creditsWaived, creditsRequired, creditsInProgress, otherRequirementValues,
                            satisfiedOtherRequirementOids, totalCompleted,
                            totalWaived, status, totalInProgress, required, m_graduationManager);
                }
            }
        } finally {
            if (iterator != null) {
                iterator.close();
            }
        }
    }
}
