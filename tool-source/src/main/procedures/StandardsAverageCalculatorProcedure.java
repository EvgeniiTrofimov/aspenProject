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


import com.follett.fsc.core.k12.business.X2Broker;
import com.x2dev.sis.model.beans.GradeAverageCalculation.GradeAverageCalculationType;
import com.x2dev.sis.model.beans.GradeScale;
import com.x2dev.sis.model.beans.GradeTerm;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.ReportingStandardAssignment;
import com.x2dev.sis.model.beans.ReportingStandardScore;
import com.x2dev.sis.model.beans.SectionReportingStandard;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.model.business.gradebook.AverageCalculatorDataHelper;
import com.x2dev.sis.model.business.gradebook.RubricRatingScaleCache;
import com.x2dev.sis.model.business.gradebook.StandardCalculatorDataHelper;
import com.x2dev.sis.model.business.gradebook.StandardsAverageCalculator;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;

/**
 * Default implementation of StandardsAverageCalculatorProcedure to be used to override specific
 * calculations done by StandardsAverageCalculator.
 *
 * @author Follett Software Company
 */
public class StandardsAverageCalculatorProcedure extends StandardsAverageCalculator {

    /**
     * The grade average calculation type.
     */
    public static final GradeAverageCalculationType CALCULATION_TYPE =
            GradeAverageCalculationType.STANDARD_OVERALL_AVERAGE;

    /**
     * Creates a new StandardsAverageCalculatorProcedure.
     *
     * @param term GradeTerm
     * @param decimals int
     * @param gradesManager GradesManager
     * @param students Collection
     * @param averageScale GradeScale
     * @param calculationMode int
     * @param standardAlignmentMap Map<String,Collection<ReportingStandardAssignment>>
     * @param broker X2Broker
     * @param includeHiddenAssignments boolean
     * @param calcHelper AverageCalculatorDataHelper
     * @param standardsCalcHelper StandardCalculatorDataHelper
     * @param section MasterSchedule
     * @param reportingStandard SectionReportingStandard
     * @param rubricCache RubricRatingScaleCache
     */
    public StandardsAverageCalculatorProcedure(GradeTerm term,
            int decimals,
            GradesManager gradesManager,
            Collection students,
            GradeScale averageScale,
            int calculationMode,
            Map<String, Collection<ReportingStandardAssignment>> standardAlignmentMap,
            X2Broker broker,
            boolean includeHiddenAssignments,
            AverageCalculatorDataHelper calcHelper,
            StandardCalculatorDataHelper standardsCalcHelper,
            MasterSchedule section,
            SectionReportingStandard reportingStandard,
            RubricRatingScaleCache rubricCache) {
        super(term,
                decimals,
                gradesManager,
                students,
                averageScale,
                calculationMode,
                standardAlignmentMap,
                broker,
                includeHiddenAssignments,
                calcHelper,
                standardsCalcHelper,
                section,
                reportingStandard,
                rubricCache);
    }

    /**
     * Calculate.
     *
     * @param studentOid String
     * @see
     *      com.x2dev.sis.model.business.gradebook.StandardsTrendCalculator#calculate(java.lang.String)
     */
    @Override
    protected void calculate(String studentOid) {
        LinkedList<Double> scores = new LinkedList<Double>();

        /*
         * Collect scores from component columns, which represent the score for a given assignment
         * in the current
         * standard. Assumes all scores are on a shared rating scale and require no conversion.
         * IMPORTANT: column collection should be ordered by assignment due date, ascending.
         */
        Iterator componentColumns = m_componentColumnSet.iterator();
        while (componentColumns.hasNext()) {
            String columnOid = (String) componentColumns.next();
            Map<String, ReportingStandardScore> scoresForColumnByStudent = m_scoresByStudent.get(columnOid);

            if (scoresForColumnByStudent != null &&
                    scoresForColumnByStudent.containsKey(studentOid)) {
                ReportingStandardScore rawScore = scoresForColumnByStudent.get(studentOid);

                Double score = Double.valueOf(findScoreForValue(rawScore.getScore()));
                scores.add(score);
            }
        }

        double trend = Double.NaN;
        if (scores.size() > 0) {
            trend = determineCalculatedScore(scores);
        }

        if (!Double.isNaN(trend)) {
            trend = enforceValidRange(trend);
            setAverage(studentOid, trend);
        } else {
            removeAverage(studentOid);
        }
    }
}
