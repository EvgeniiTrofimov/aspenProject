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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.GradeTerm;
import com.x2dev.sis.model.beans.GradebookColumnDefinition;
import com.x2dev.sis.model.beans.GradebookColumnType;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.TranscriptColumnCollectionType;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.model.business.gradebook.AverageCalculator;
import com.x2dev.sis.model.business.gradebook.AverageCalculatorFactory;
import com.x2dev.sis.model.business.gradebook.CategoryAverageCalculator;
import com.x2dev.sis.model.business.gradebook.GradebookManager;
import com.x2dev.sis.model.business.gradebook.OverallAverageCalculator;
import com.x2dev.sis.model.business.gradebook.ReportStatisticsCalculator;
import com.x2dev.sis.model.business.gradebook.StatisticsCalculator.Statistic;
import com.x2dev.sis.model.business.gradebook.TermAverageCalculator;
import com.x2dev.sis.tools.reports.GradebookReportDataSourceNet;
import com.x2dev.sis.web.gradebook.GradeInputUtils;
import com.x2dev.utils.StringUtils;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Gradebook Class Statistics" report.
 *
 * @author X2 Development Corporation
 */
public class GradebookClassStatisticsData extends GradebookReportDataSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private static final String COL_ASSIGNMENT = "assignment";
    private static final String COL_AVERAGE_LABEL = "averageLabel";
    private static final String COL_CATEGORY = "category";
    private static final String COL_GROUP = "group";
    private static final String COL_HIGH = "high";
    private static final String COL_INVALID = "invalid";
    private static final String COL_LOW = "low";
    private static final String COL_MEAN = "mean";
    private static final String COL_MEDIAN = "median";
    private static final String COL_MISSING = "missing";
    private static final String COL_SECTION = "section";
    private static final String COL_STANDARD_DEVIATION = "standardDeviation";
    private static final String COL_TERM = "term";

    /*
     * TODO: support term and category input parameters. This procedure currently supports them;
     * a good way to select them on the report input is all that's left to do.
     */
    // private static final String INPUT_PARAM_CATEGORY = "category";
    private static final String INPUT_PARAM_INCLUDE = "include";
    // private static final String INPUT_PARAM_TERM = "term";

    private static final String GROUP_ASSIGNMENTS = "assignments";
    private static final String GROUP_AVERAGES = "averages";

    private static final int INCLUDE_GRADEBOOK_COLUMNS = 0;
    private static final int INCLUDE_POST_COLUMNS = 1;

    private static final String OPTION_ALL = "all";

    private AverageCalculatorFactory m_calculatorFactory;
    private String m_categorySelection;
    private int m_include;
    private Collection m_students;
    private String m_termSelection;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(50, 15);
        GradeTerm lastGradeTerm = null;
        GradebookColumnType lastCategory = null;

        Iterator assignments = getAssignments().iterator();
        while (assignments.hasNext()) {
            GradebookColumnDefinition assignment = (GradebookColumnDefinition) assignments.next();

            /*
             * Add averages if we are not viewing post columns
             */
            if (m_include == INCLUDE_GRADEBOOK_COLUMNS) {
                if (lastCategory != null && !lastCategory.equals(assignment.getColumnType())) {
                    addAverageRow(grid, lastGradeTerm, lastCategory);
                    setCategoryStatistics(lastGradeTerm, lastCategory, grid);
                }

                if (lastGradeTerm != null && !lastGradeTerm.equals(assignment.getEffectiveGradeTerm(getBroker()))) {
                    addAverageRow(grid, lastGradeTerm, lastCategory);
                    setTermStatistics(lastGradeTerm, grid);
                }
            }

            grid.append();
            grid.set(COL_ASSIGNMENT, assignment);
            grid.set(COL_TERM, assignment.getEffectiveGradeTerm(getBroker()));
            grid.set(COL_CATEGORY, assignment.getColumnType());
            grid.set(COL_GROUP, GROUP_ASSIGNMENTS);
            grid.set(COL_SECTION, getSection());

            ReportStatisticsCalculator calculator =
                    new ReportStatisticsCalculator(getDecimals(), assignment, m_students, getBroker());
            setStatistics(calculator, grid);

            lastCategory = assignment.getColumnType();
            lastGradeTerm = assignment.getEffectiveGradeTerm(getBroker());
        }

        // Add average statistics for the final set of assignments, and the overall average
        if (m_include == INCLUDE_GRADEBOOK_COLUMNS) {
            if (lastCategory != null) {
                addAverageRow(grid, lastGradeTerm, lastCategory);
                setCategoryStatistics(lastGradeTerm, lastCategory, grid);
            }
            if (lastGradeTerm != null) {
                addAverageRow(grid, lastGradeTerm, lastCategory);
                setTermStatistics(lastGradeTerm, grid);
            }

            addAverageRow(grid, lastGradeTerm, lastCategory);
            setOverallAverageStatistics(grid);
        }

        grid.beforeTop();

        return grid;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.reports.GradebookReportDataSource#initialize()
     */
    @Override
    @SuppressWarnings("deprecation")
    protected void initialize() {
        super.initialize();

        m_include = Integer.parseInt((String) getParameter(INPUT_PARAM_INCLUDE));
        // m_categorySelection = (String) getReportParameter(INPUT_PARAM_CATEGORY);
        m_students = getStudents();
        // m_termSelection = (String) getReportParameter(INPUT_PARAM_TERM);

        m_calculatorFactory = new AverageCalculatorFactory(getSection(),
                getDecimals(),
                getGradesManager(),
                getAverageScale(),
                getCalculationMode(),
                isCalculateFromStandards(),
                getStandards(),
                getBroker(),
                true);

        m_categorySelection = "all";
        m_termSelection = "all";
    }

    /**
     * Adds a row to the grid for average statistics.
     *
     * @param grid ReportDataGrid
     * @param lastGradeTerm GradeTerm
     * @param lastCategory GradebookColumnType
     */
    private void addAverageRow(ReportDataGrid grid, GradeTerm lastGradeTerm, GradebookColumnType lastCategory) {
        grid.append();
        grid.set(COL_TERM, lastGradeTerm);
        grid.set(COL_CATEGORY, lastCategory);
        grid.set(COL_GROUP, GROUP_AVERAGES);
        grid.set(COL_SECTION, getSection());
    }

    /**
     * Returns the assignments to calculate statistics for.
     *
     * @return Collection of GradebookColumnDefinition objects
     */
    private List getAssignments() {
        X2Criteria assignmentCriteria = new X2Criteria();
        assignmentCriteria.addEqualTo(GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID, getSection().getOid());
        assignmentCriteria.addEqualTo(GradebookColumnDefinition.COL_SYSTEM_ONLY_INDICATOR,
                Boolean.valueOf(m_include == INCLUDE_POST_COLUMNS));
        assignmentCriteria.addEqualTo(GradebookColumnDefinition.COL_NOT_GRADED_INDICATOR, Boolean.FALSE);

        QueryByCriteria assignmentQuery = new QueryByCriteria(GradebookColumnDefinition.class, assignmentCriteria);
        QueryIterator assignments = getBroker().getIteratorByQuery(assignmentQuery);

        List includedAssignments = new LinkedList();

        try {
            while (assignments.hasNext()) {
                GradebookColumnDefinition assignment = (GradebookColumnDefinition) assignments.next();

                if (assignment.isPostColumn()) {
                    /*
                     * A post column is included if
                     *
                     * 1) It is a "grade" post column (not a comment)
                     * 2) It is collected from the teacher
                     * 3) It matches the term selection
                     *
                     * The if statement is broken up to avoid a giant condition
                     */
                    if (assignment.getColumnTypeCode() == TranscriptColumnDefinition.COLUMN_TYPE_FINAL ||
                            assignment.getColumnTypeCode() == TranscriptColumnDefinition.COLUMN_TYPE_GRADE ||
                            assignment.getColumnTypeCode() == TranscriptColumnDefinition.COLUMN_TYPE_OTHER_AVERAGE ||
                            assignment.getColumnTypeCode() == TranscriptColumnDefinition.COLUMN_TYPE_TERM_AVERAGE) {
                        if (assignment.getTranscriptColumnDefinition() != null &&
                                assignment.getTranscriptColumnDefinition()
                                        .getCollectionType() != TranscriptColumnCollectionType.NONE.value()) {
                            if (m_termSelection.equals(OPTION_ALL)
                                    || m_termSelection.equals(assignment.getEffectiveGradeTermOid(getBroker()))) {
                                includedAssignments.add(assignment);
                            }
                        }
                    }
                } else {
                    /*
                     * A gradebook column is included if
                     *
                     * 1) It matches the term selection
                     * 2) It matches the category selection
                     *
                     * The if statement is broken up to avoid a giant condition
                     */
                    if (m_termSelection.equals(OPTION_ALL) || m_termSelection.equals(assignment.getGradeTermOid())) {
                        if (m_categorySelection.equals(OPTION_ALL)
                                || m_categorySelection.equals(assignment.getColumnTypeOid())) {
                            includedAssignments.add(assignment);
                        }
                    }
                }
            }
        } finally {
            assignments.close();
        }

        sortAssignments(includedAssignments);
        return includedAssignments;
    }

    /**
     * Returns the students to include in the statistics calculations.
     *
     * @return Collection of Student objects
     */
    private Collection getStudents() {
        Criteria studentCriteria = GradebookManager.getStudentCriteria(GradebookManager.STUDENT_FILTER_ENROLLED,
                getSection(), getBroker());
        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);

        return getBroker().getCollectionByQuery(studentQuery);
    }

    /**
     * Set statistics for the passed average on the grid. Note: this method does NOT append a new
     * row.
     *
     * @param averageKey String
     * @param grid ReportDataGrid
     */
    private void setAverageStatistics(String averageKey, ReportDataGrid grid) {
        AverageCalculator averageCalculator = m_calculatorFactory.getAverageCalculator(averageKey, m_students);
        ReportStatisticsCalculator statisticsCalculator =
                new ReportStatisticsCalculator(getDecimals(), getSection(), averageCalculator, getBroker());

        String averageLabel = GradeInputUtils.getAverageLabel(averageKey, false, false, getBroker(), getLocale());

        grid.set(COL_AVERAGE_LABEL, averageLabel);

        setStatistics(statisticsCalculator, grid);
    }

    /**
     * Sets category statistics on the grid. Note: this method does NOT append a new row.
     *
     * @param term GradeTerm
     * @param category GradebookColumnType
     * @param grid ReportDataGrid
     */
    private void setCategoryStatistics(GradeTerm term, GradebookColumnType category, ReportDataGrid grid) {
        String averageKey = CategoryAverageCalculator.getIdentifier(category.getOid(), term.getOid());
        setAverageStatistics(averageKey, grid);
    }

    /**
     * Sets category statistics on the grid. Note: this method does NOT append a new row.
     *
     * @param grid void
     */
    private void setOverallAverageStatistics(ReportDataGrid grid) {
        String averageKey = OverallAverageCalculator.getIdentifier(getSection().getOid());
        setAverageStatistics(averageKey, grid);
    }

    /**
     * Sets statistics from the passed calculator on the grid. Note: this method does NOT append a
     * new row.
     *
     * @param calculator ReportStatisticsCalculator
     * @param grid ReportDataGrid
     */
    private void setStatistics(ReportStatisticsCalculator calculator, ReportDataGrid grid) {
        grid.set(COL_HIGH, calculator.getStatisticView(Statistic.HIGH));
        grid.set(COL_INVALID, Integer.valueOf(calculator.getInvalid()));
        grid.set(COL_LOW, calculator.getStatisticView(Statistic.LOW));
        grid.set(COL_MEAN, calculator.getStatisticView(Statistic.MEAN));
        grid.set(COL_MEDIAN, calculator.getStatisticView(Statistic.MEDIAN));
        grid.set(COL_MISSING, Integer.valueOf(calculator.getMissing()));
        grid.set(COL_STANDARD_DEVIATION, calculator.getStatisticView(Statistic.STD));
    }

    /**
     * Sets category statistics on the grid.
     *
     * @param term GradeTerm
     * @param grid ReportDataGrid
     */
    private void setTermStatistics(GradeTerm term, ReportDataGrid grid) {
        String averageKey = TermAverageCalculator.getIdentifier(term.getOid());
        setAverageStatistics(averageKey, grid);
    }

    /**
     * Sorts the passed collection of GradebookColumnDefinition objects by:
     * <ol>
     * <li>Effective grade term
     * <li>Category code
     * <li>Due date (if possible)
     * <li>Column code
     * </ol>
     * .
     *
     * @param assignments List
     */
    private void sortAssignments(List assignments) {
        final X2Broker broker = getBroker();

        Comparator comparator = new Comparator() {
            @Override
            public int compare(Object object1, Object object2) {
                GradebookColumnDefinition assignment1 = (GradebookColumnDefinition) object1;
                GradebookColumnDefinition assignment2 = (GradebookColumnDefinition) object2;

                String sortString1 = getSortString(assignment1);
                String sortString2 = getSortString(assignment2);

                return sortString1.compareTo(sortString2);
            }

            private String getSortString(GradebookColumnDefinition assignment) {
                StringBuilder sortString = new StringBuilder(50);

                String termNumber = "99999";
                GradeTerm effectiveGradeTerm = assignment.getEffectiveGradeTerm(broker);
                if (effectiveGradeTerm != null) {
                    termNumber = Integer.toString(effectiveGradeTerm.getGradeTermNum());
                    termNumber = StringUtils.padLeft(termNumber, 5, '0');
                }

                String category = "";
                if (assignment.getColumnType() != null) {
                    category = assignment.getColumnType().getColumnType();
                }

                sortString.append(termNumber);
                sortString.append(category);

                if (assignment.getDateDue() != null) {
                    sortString.append(assignment.getDateDue().getTime());
                }

                sortString.append(assignment.getColumnCode());

                return sortString.toString();
            }
        };

        Collections.sort(assignments, comparator);
    }
}
