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
import com.x2dev.sis.model.beans.GradebookColumnDefinition;
import com.x2dev.sis.model.beans.GradebookScore;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.gradebook.GradebookManager;
import com.x2dev.sis.model.business.gradebook.GradebookScoreManager;
import com.x2dev.sis.model.business.gradebook.ReportStatisticsCalculator;
import com.x2dev.sis.tools.reports.GradebookReportDataSourceNet;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Gradebook Assignment Summary" report.
 *
 * @author X2 Development Corporation
 */
public class GradebookAssignmentSummaryData extends GradebookReportDataSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private static final String COL_RANK = "rank";
    private static final String COL_SCORE = "score";
    private static final String COL_STUDENT = "student";
    private static final String COL_STUDENT_DISPLAY = "studentDisplay";
    private static final String COL_SORT = "sort";

    private static final String INPUT_PARAM_ASSIGNMENT_OID = "assignmentOid";
    private static final String INPUT_PARAM_STUDENT_DISPLAY = "display";
    private static final String INPUT_PARAM_SORT = "sort";

    private static final String PARAM_ASSIGNMENT = "assignment";
    private static final String PARAM_SECTION = "section";
    private static final String PARAM_STATISTICS_CALCULATOR = "statistics";

    private GradebookColumnDefinition m_assignment;
    private Collection m_students;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(50, 15);

        Integer studentDisplay = (Integer) getParameter(INPUT_PARAM_STUDENT_DISPLAY);
        Integer sort = (Integer) getParameter(INPUT_PARAM_SORT);

        Map scoreMap = getScoreMap();
        Map rankMap = getRankMap(scoreMap);

        Iterator studentIterator = m_students.iterator();
        while (studentIterator.hasNext()) {
            SisStudent student = (SisStudent) studentIterator.next();
            GradebookScore score = (GradebookScore) scoreMap.get(student.getOid());

            grid.append();
            grid.set(COL_STUDENT, student);

            if (score != null) {
                grid.set(COL_SCORE, score.getScore());
                grid.set(COL_RANK, rankMap.get(student.getOid()));
            }

            switch (studentDisplay.intValue()) {
                case 0: // name
                    grid.set(COL_STUDENT_DISPLAY, student.getNameView());
                    break;

                case 1: // ID
                default:
                    grid.set(COL_STUDENT_DISPLAY, student.getLocalId());
            }

            switch (sort.intValue()) {
                case 0: // name
                    grid.set(COL_SORT, student.getNameView());
                    break;

                case 1: // ID
                    grid.set(COL_SORT, student.getLocalId());
                    break;

                case 2: // Rank
                default:
                    grid.set(COL_SORT, rankMap.get(student.getOid()));
                    break;
            }
        }

        grid.beforeTop();
        grid.sort(COL_SORT, false);

        addParameter(PARAM_ASSIGNMENT, m_assignment);

        ReportStatisticsCalculator statistics =
                new ReportStatisticsCalculator(getDecimals(), m_assignment, m_students, getBroker());
        addParameter(PARAM_STATISTICS_CALCULATOR, statistics);

        addParameter(PARAM_SECTION, getSection());

        return grid;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.reports.GradebookReportDataSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();

        String assignmentOid = (String) getParameter(INPUT_PARAM_ASSIGNMENT_OID);
        m_assignment =
                (GradebookColumnDefinition) getBroker().getBeanByOid(GradebookColumnDefinition.class, assignmentOid);

        Criteria studentCriteria = GradebookManager.getStudentCriteria(GradebookManager.STUDENT_FILTER_ENROLLED,
                getSection(), getBroker());
        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);

        m_students = getBroker().getCollectionByQuery(studentQuery);
    }

    /**
     * Returns a Map containing each student's rank for the assignment.
     *
     * @param scoreMap Map of GradebookScore objects keyed on student OID
     *
     * @return Map of Integer objects
     */
    private Map getRankMap(Map scoreMap) {
        HashMap rankMap = new HashMap(scoreMap.size());
        HashMap convertedScores = new HashMap();
        ArrayList uniqueScores = new ArrayList(scoreMap.size());

        GradebookManager gbManager = new GradebookManager(getBroker());

        Iterator scores = scoreMap.values().iterator();
        while (scores.hasNext()) {
            GradebookScore score = (GradebookScore) scores.next();

            if (m_students.contains(score.getStudent())) {
                Double convertedScore =
                        gbManager.convertScore(score.getScore(), m_assignment.getGradeScale(), getGradesManager(),
                                getSection().getSchoolCourse());

                if (convertedScore != null) {
                    if (!uniqueScores.contains(convertedScore)) {
                        uniqueScores.add(convertedScore);
                    }
                    convertedScores.put(score.getStudentOid(), convertedScore);
                }
            }
        }

        Collections.sort(uniqueScores);

        Iterator studentOids = convertedScores.keySet().iterator();
        while (studentOids.hasNext()) {
            String studentOid = (String) studentOids.next();
            Double score = (Double) convertedScores.get(studentOid);

            int position = uniqueScores.indexOf(score);
            if (position != -1) {
                rankMap.put(studentOid, Integer.valueOf(uniqueScores.size() - position));
            }
        }

        return rankMap;
    }

    /**
     * Returns a Map of scores for the assignment to report.
     *
     * @return Map of GradebookScore objects keyed on student OID
     */
    private Map getScoreMap() {
        Criteria scoreCriteria = new Criteria();
        scoreCriteria.addEqualTo(GradebookScore.COL_COLUMN_DEFINITION_OID, m_assignment.getOid());
        scoreCriteria.addNotEqualTo(GradebookScore.COL_SCORE,
                GradebookScoreManager.EXCLUDE_STUDENT_FROM_ASSIGNMENT_SPECIAL_CODE);

        QueryByCriteria scoreQuery = new QueryByCriteria(GradebookScore.class, scoreCriteria);

        return getBroker().getMapByQuery(scoreQuery, GradebookScore.COL_STUDENT_OID, 100);
    }
}
