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

import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.GradeTerm;
import com.x2dev.sis.model.beans.GradebookColumnDefinition;
import com.x2dev.sis.model.beans.GradebookColumnType;
import com.x2dev.sis.model.beans.GradebookScore;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.gradebook.AverageCalculator;
import com.x2dev.sis.model.business.gradebook.AverageCalculatorFactory;
import com.x2dev.sis.model.business.gradebook.GradebookManager;
import com.x2dev.sis.model.business.gradebook.GradebookScoreManager;
import com.x2dev.sis.tools.reports.GradebookReportDataSourceNet;
import com.x2dev.utils.StringUtils;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Gradebook Average Breakdown" report. This data source is a near copy of
 * GradebookAssignmentHistoryData, the only difference is related to the setFormat() method.
 *
 * @author X2 Development Corporation
 */
public class GradebookAverageBreakdownData extends GradebookReportDataSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private static final String STUDENT_OID_PARAM = "studentOid";
    private static final String SECTION_PARAM = "section";
    private static final String TERM_OID_PARAM = "termOid";

    private static final String AVERAGE_CALCULATOR_MAP_PARAM = "calculators";

    private static final String NUMBER_FORMAT_PARAM = "numberFormat";
    private static final String TERMS_PARAM = "terms";

    private static final String COL_STUDENT = "student";

    private static final String COL_ASSIGNMENT = "assignment";
    private static final String COL_CATEGORY = "category";
    private static final String COL_SCORE = "score";
    private static final String COL_TERM = "term";

    private static final String COL_CATEGORY_ASSIGNMENT_COUNT = "assignments";
    private static final String COL_CATEGORY_ASSIGNMENT_POINT_TOTAL = "assignmentPoints";
    private static final String COL_CATEGORY_ASSIGNMENT_WEIGHT_TOTAL = "assignmentWeight";

    private static final String COL_TERM_CATEGORY_COUNT = "categories";
    private static final String COL_TERM_CATEGORY_WEIGHT_TOTAL = "categoryWeight";

    private static final String COL_TOTAL_POINTS = "totalPoints";

    private static final String COL_NUMERIC_SCORE = "numericScore";

    private static final String REPORT_ID_PREFIX = "SYS-GBK-002-";
    private static final String FORMAT_SUFFIX_WEIGHT_ALL = "WA";
    private static final String FORMAT_SUFFIX_WEIGHT_CATEGORIES = "WC";
    private static final String FORMAT_SUFFIX_TOTAL_POINTS = "TP";

    private Collection m_assignments = null;
    private Map m_categoryTotalsMap = null;
    private String m_selectedTermOid = null;
    private Collection m_students = null;
    private Map m_studentScoresMap = null;
    private Map m_termTotalsMap = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        /*
         * Load the score data for individual assignments and totals. Note that the order of these
         * method calls is important - most depend on loadStudents().
         */
        loadStudents();
        loadScores();
        loadCategoryTotals();
        loadTermTotals();
        loadAssignments();

        m_selectedTermOid = (String) getParameter(TERM_OID_PARAM);
        HashSet terms = new HashSet();

        ReportDataGrid grid = new ReportDataGrid(100 * m_students.size(), 16);

        Iterator students = m_students.iterator();
        while (students.hasNext()) {
            SisStudent student = (SisStudent) students.next();
            addStudentToGrid(grid, student, terms);
        }

        grid.beforeTop();

        addParameter(SECTION_PARAM, getSection());
        addParameter(NUMBER_FORMAT_PARAM, getNumberFormat());
        addParameter(TERMS_PARAM, Integer.valueOf(terms.size()));
        addCalculators();

        setFormat();

        return grid;
    }

    /**
     * Creates and adds the average calculators as a report parameter.
     */
    private void addCalculators() {
        HashMap calculators = new HashMap(50);

        AverageCalculatorFactory calculatorFactory = new AverageCalculatorFactory(getSection(),
                getDecimals(),
                getGradesManager(),
                getAverageScale(),
                getCalculationMode(),
                isCalculateFromStandards(),
                getStandards(),
                getBroker(),
                true,
                null,
                null);

        AverageCalculator overallCalculator = calculatorFactory.getOverallAverageCalculator(m_students);
        calculators.put("overall", overallCalculator);

        Collection terms = getGradesManager().getGradeTerms(getSection());
        Collection categories = getSection().getGradebookColumnTypes(getBroker());

        Iterator termIterator = terms.iterator();
        while (termIterator.hasNext()) {
            GradeTerm term = (GradeTerm) termIterator.next();

            AverageCalculator termCalculator = calculatorFactory.getTermAverageCalculator(term, m_students);
            calculators.put(term.getOid(), termCalculator);

            Iterator categoryIterator = categories.iterator();
            while (categoryIterator.hasNext()) {
                GradebookColumnType category = (GradebookColumnType) categoryIterator.next();

                AverageCalculator categoryCalculator =
                        calculatorFactory.getCategoryAverageCalculator(category, term, m_students);
                calculators.put(category.getOid() + term.getOid(), categoryCalculator);
            }
        }

        addParameter(AVERAGE_CALCULATOR_MAP_PARAM, calculators);
    }

    /**
     * Adds the given student all his/her scores and totals to the grid. Scores are only added for
     * the selected term (or all terms if the user didn't select a term).
     *
     * @param grid ReportDataGrid
     * @param student SisStudent
     * @param terms this set will be populated with any term for which the student has a score
     */
    private void addStudentToGrid(ReportDataGrid grid, SisStudent student, Set terms) {
        Map scores = (Map) m_studentScoresMap.get(student.getOid());

        Iterator assignments = m_assignments.iterator();
        while (assignments.hasNext()) {
            GradebookColumnDefinition assignment = (GradebookColumnDefinition) assignments.next();
            GradeTerm term = assignment.getGradeTerm();
            if (StringUtils.isEmpty(m_selectedTermOid) || term.getOid().equals(m_selectedTermOid)) {
                GradebookColumnType category = assignment.getColumnType();
                CategoryTotals categoryTotals =
                        (CategoryTotals) m_categoryTotalsMap.get(student.getOid() + term.getOid() + category.getOid());
                TermTotals termTotals = (TermTotals) m_termTotalsMap.get(student.getOid() + term.getOid());

                grid.append();
                grid.set(COL_STUDENT, student);
                grid.set(COL_CATEGORY, category);
                grid.set(COL_ASSIGNMENT, assignment);
                grid.set(COL_TERM, term);
                grid.set(COL_TOTAL_POINTS, assignment.getTotalPoints());

                if (categoryTotals != null) {
                    grid.set(COL_CATEGORY_ASSIGNMENT_COUNT, Integer.valueOf(categoryTotals.assignmentCount));
                    grid.set(COL_CATEGORY_ASSIGNMENT_POINT_TOTAL, Double.valueOf(categoryTotals.totalPoints));
                    grid.set(COL_CATEGORY_ASSIGNMENT_WEIGHT_TOTAL, Double.valueOf(categoryTotals.totalWeight));
                }

                if (termTotals != null) {
                    grid.set(COL_TERM_CATEGORY_COUNT, Integer.valueOf(termTotals.categoryCount));
                    grid.set(COL_TERM_CATEGORY_WEIGHT_TOTAL, Double.valueOf(termTotals.totalWeight));
                }

                if (scores != null) {
                    GradebookScore score = (GradebookScore) scores.get(assignment.getOid());
                    if (score != null) {
                        grid.set(COL_SCORE, score);

                        BigDecimal numericValue = getGradesManager().getNumericValue(score);
                        if (numericValue != null) {
                            grid.set(COL_NUMERIC_SCORE, Double.valueOf(numericValue.doubleValue()));
                        }

                        terms.add(term);
                    }
                }
            }
        }
    }

    /**
     * Closes the passed statement and result set.
     *
     * @param statement PreparedStatement
     * @param results ResultSet
     */
    private void closeSafely(PreparedStatement statement, ResultSet results) {
        try {
            results.close();
        } catch (Exception e) {
            //
        }

        try {
            statement.close();
        } catch (Exception e) {
            //
        }
    }

    /**
     * Loads the collection of assignments for the current section/teacher combination.
     */
    private void loadAssignments() {
        Criteria assignmentCriteria = new Criteria();
        assignmentCriteria.addEqualTo(GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID, getSection().getOid());
        assignmentCriteria.addEqualTo(GradebookColumnDefinition.COL_SYSTEM_ONLY_INDICATOR, Boolean.FALSE);

        QueryByCriteria assignmentQuery = new QueryByCriteria(GradebookColumnDefinition.class, assignmentCriteria);
        assignmentQuery
                .addOrderByAscending(GradebookColumnDefinition.REL_GRADE_TERM + "." + GradeTerm.COL_GRADE_TERM_NUM);
        assignmentQuery.addOrderByAscending(
                GradebookColumnDefinition.REL_COLUMN_TYPE + "." + GradebookColumnType.COL_COLUMN_TYPE);
        assignmentQuery.addOrderByAscending(GradebookColumnDefinition.COL_DATE_DUE);
        assignmentQuery.addOrderByAscending(GradebookColumnDefinition.COL_SEQUENCE_NUMBER);

        m_assignments = getBroker().getCollectionByQuery(assignmentQuery);
    }

    /**
     * Loads the category totals map. The result is a Map of CategoryTotals objects keyed on student
     * OID + term OID + category OID.
     */
    private void loadCategoryTotals() {
        m_categoryTotalsMap = new HashMap(32 * m_students.size());

        String sql = "SELECT GSC_STD_OID, " +
                " GCD_GTM_OID, " +
                " GCD_GCT_OID, " +
                " COUNT(*) AS assignment_count, " +
                " SUM(GCD_COLUMN_WEIGHT) AS total_weight, " +
                " SUM(GCD_TOTAL_POINTS) AS total_points " +
                "FROM GRADEBOOK_SCORE " +
                "INNER JOIN GRADEBOOK_COLUMN_DEFINITION ON GSC_GCD_OID = GCD_OID " +
                "INNER JOIN SCHEDULE_MASTER ON GCD_MST_OID = MST_OID " +
                "WHERE GCD_STF_OID = ? " +
                "AND GCD_MST_OID = ? " +
                "AND GCD_SYSTEM_ONLY_IND <> '1' " +
                "GROUP BY GSC_STD_OID, GCD_GTM_OID, GCD_GCT_OID";

        Connection connection = getBroker().borrowConnection();
        PreparedStatement statement = null;
        ResultSet results = null;
        try {
            statement = connection.prepareStatement(sql);
            statement.setString(1, getStaff().getOid());
            statement.setString(2, getSection().getOid());

            results = statement.executeQuery();
            while (results.next()) {
                String studentOid = results.getString("GSC_STD_OID");
                String termOid = results.getString("GCD_GTM_OID");
                String categoryOid = results.getString("GCD_GCT_OID");

                CategoryTotals categoryTotals = new CategoryTotals();

                categoryTotals.assignmentCount = results.getInt("assignment_count");
                categoryTotals.totalWeight = results.getDouble("total_weight");
                categoryTotals.totalPoints = results.getDouble("total_points");

                m_categoryTotalsMap.put(studentOid + termOid + categoryOid, categoryTotals);
            }
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            getBroker().returnConnection();
            closeSafely(statement, results);
        }
    }

    /**
     * Loads the scores for the selected students. The result is a nested Map of student OIDs to
     * column definition OIDs to GradebookScore beans.
     */
    private void loadScores() {
        Criteria scoreCriteria = new Criteria();
        scoreCriteria.addEqualTo(GradebookScore.REL_COLUMN_DEFINITION + "." +
                GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID, getSection().getOid());
        scoreCriteria.addNotEqualTo(GradebookScore.COL_SCORE,
                GradebookScoreManager.EXCLUDE_STUDENT_FROM_ASSIGNMENT_SPECIAL_CODE);

        QueryByCriteria scoreQuery = new QueryByCriteria(GradebookScore.class, scoreCriteria);

        m_studentScoresMap = getBroker().getNestedMapByQuery(scoreQuery, GradebookScore.COL_STUDENT_OID,
                GradebookScore.COL_COLUMN_DEFINITION_OID, m_students.size(), 128);
    }

    /**
     * Loads the selected students to include in this report.
     */
    private void loadStudents() {
        String studentOid = (String) getParameter(STUDENT_OID_PARAM);
        if (StringUtils.isEmpty(studentOid)) {
            Criteria rosterCriteria = GradebookManager.getStudentCriteria(GradebookManager.STUDENT_FILTER_ENROLLED,
                    getSection(), getBroker());

            QueryByCriteria rosterQuery = new QueryByCriteria(SisStudent.class, rosterCriteria);
            rosterQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
            m_students = getBroker().getCollectionByQuery(rosterQuery);
        } else {
            SisStudent student = (SisStudent) getBroker().getBeanByOid(SisStudent.class, studentOid);
            m_students = new ArrayList(1);
            m_students.add(student);
        }
    }

    /**
     * Loads the term totals map. The result is a Map of TermTotals objects keyed on student OID +
     * term OID.
     */
    private void loadTermTotals() {
        m_termTotalsMap = new HashMap(4 * m_students.size());

        String sql = "SELECT GSC_STD_OID, " +
                " GCD_GTM_OID, " +
                " COUNT(*) AS category_count, " +
                " SUM(weight) AS total_weight " +
                "FROM (SELECT GSC_STD_OID, " +
                " GCD_GTM_OID, " +
                " GCD_GCT_OID, " +
                " MAX(GCT_COLUMN_TYPE_WEIGHT) as weight " +
                "FROM GRADEBOOK_COLUMN_DEFINITION " +
                "INNER JOIN GRADEBOOK_COLUMN_TYPE ON GCD_GCT_OID = GCT_OID " +
                "INNER JOIN GRADEBOOK_SCORE ON GSC_GCD_OID = GCD_OID " +
                "WHERE GCD_STF_OID = ? " +
                "AND GCD_MST_OID = ? " +
                "GROUP BY GSC_STD_OID, GCD_GTM_OID, GCD_GCT_OID) term_category " +
                "GROUP by GSC_STD_OID, GCD_GTM_OID";

        Connection connection = getBroker().borrowConnection();
        PreparedStatement statement = null;
        ResultSet results = null;
        try {
            statement = connection.prepareStatement(sql);
            statement.setString(1, getStaff().getOid());
            statement.setString(2, getSection().getOid());

            results = statement.executeQuery();
            while (results.next()) {
                String studentOid = results.getString("GSC_STD_OID");
                String termOid = results.getString("GCD_GTM_OID");

                TermTotals termTotals = new TermTotals();

                termTotals.categoryCount = results.getInt("category_count");
                termTotals.totalWeight = results.getDouble("total_weight");

                m_termTotalsMap.put(studentOid + termOid, termTotals);
            }
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            getBroker().returnConnection();
            closeSafely(statement, results);
        }
    }

    /**
     * Sets the correct report format based on the calculation mode preference.
     */
    private void setFormat() {
        String formatSuffix;

        switch (getCalculationMode()) {
            case AverageCalculator.CALCULATION_MODE_TOTAL_POINTS:
                formatSuffix = FORMAT_SUFFIX_TOTAL_POINTS;
                break;

            case AverageCalculator.CALCULATION_MODE_WEIGHT_ALL:
                formatSuffix = FORMAT_SUFFIX_WEIGHT_ALL;
                break;

            case AverageCalculator.CALCULATION_MODE_WEIGHT_CATEGORIES:
            default:
                formatSuffix = FORMAT_SUFFIX_WEIGHT_CATEGORIES;

        }

        setFormatId(REPORT_ID_PREFIX + formatSuffix);
    }

    /**
     * Data structure for holding total information for a category.
     *
     * @author X2 Development Corporation
     */
    private class CategoryTotals {
        public int assignmentCount = 0;
        public double totalWeight = 0.0;
        public double totalPoints = 0.0;

        /**
         * Constructs a CategoryTotals.
         */
        CategoryTotals() {
            // Increase the visibility of the constructor to improve performance.
        }
    }

    /**
     * Data structure for holding total information for a term.
     *
     * @author X2 Development Corporation
     */
    private class TermTotals {
        public int categoryCount = 0;
        public double totalWeight = 0.0;

        /**
         * Constructs a TermTotals.
         */
        TermTotals() {
            // Increase the visibility of the constructor to improve performance.
        }
    }
}
