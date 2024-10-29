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
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.ReportingStandardAssignment;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.model.business.gradebook.AverageCalculator;
import com.x2dev.sis.model.business.gradebook.AverageCalculatorFactory;
import com.x2dev.sis.model.business.gradebook.OverallRunningAverageCalculator;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

/**
 * Default implementation of OverallRunningAverageCalculatorProcedure to be used to
 * override specific calculations done by OverallRunningAverageCalculator.
 *
 * @author Follett Software Company
 */
public class OverallRunningAverageCalculatorProcedure extends OverallRunningAverageCalculator {
    /**
     * The grade average calculation type.
     */

    public static final GradeAverageCalculationType CALCULATION_TYPE = GradeAverageCalculationType.CUMULATIVE_AVERAGE;

    /**
     * Creates a new OverallRunningAverageCalculatorProcedure.
     *
     * @param section MasterSchedule
     * @param factory AverageCalculatorFactory
     * @param decimals int
     * @param gradesManager GradesManager
     * @param students Collection
     * @param averageScale GradeScale
     * @param calculationMode int
     * @param standardAlignmentMap Map<String,Collection<ReportingStandardAssignment>>
     * @param broker X2Broker
     * @param includeHiddenAssignments boolean
     */
    public OverallRunningAverageCalculatorProcedure(MasterSchedule section,
            AverageCalculatorFactory factory,
            int decimals,
            GradesManager gradesManager,
            Collection students,
            GradeScale averageScale,
            int calculationMode,
            Map<String, Collection<ReportingStandardAssignment>> standardAlignmentMap,
            X2Broker broker,
            boolean includeHiddenAssignments) {
        super(section,
                factory,
                decimals,
                gradesManager,
                students,
                averageScale,
                calculationMode,
                standardAlignmentMap,
                broker,
                includeHiddenAssignments);
    }

    /**
     * Calculate by total points.
     *
     * @param studentOid String
     * @see
     *      com.x2dev.sis.model.business.gradebook.AverageCalculator#calculateByTotalPoints(java.lang.
     *      String)
     */
    @Override
    protected void calculateByTotalPoints(String studentOid) {
        double pointsPossible = 0;
        double totalPoints = 0;
        double average = 0;

        boolean scoreFound = false;

        // Add total and possible points for component averages
        Iterator componentAverages = m_componentAverages.keySet().iterator();
        while (componentAverages.hasNext()) {
            AverageCalculator componentAverage = (AverageCalculator) componentAverages.next();
            componentAverage.recalculateAverages(studentOid);

            Double averagePossible = componentAverage.getPointsPossible(studentOid);
            Double averageTotal = componentAverage.getTotalPoints(studentOid);

            if (averagePossible != null) {
                pointsPossible += averagePossible.doubleValue();
            }

            if (averageTotal != null) {
                scoreFound = true;
                totalPoints += averageTotal.doubleValue();
            }
        }

        // Add total and possible points for component columns
        Iterator componentColumns = m_componentColumns.keySet().iterator();
        while (componentColumns.hasNext()) {
            String columnOid = (String) componentColumns.next();

            if (!isHidden(columnOid, studentOid)) {
                Double scoreValue = getScoreValue(studentOid, columnOid);

                if (scoreValue != null) {
                    Double points = getColumnWeight(columnOid);

                    if (points != null) {
                        if (!isExtraCredit(columnOid)) {
                            pointsPossible += points.doubleValue();
                        }

                        scoreFound = true;
                        totalPoints += scoreValue.doubleValue();
                    }
                }
            }
        }

        if (scoreFound && (pointsPossible > 0 || getCalculationMode() == CALCULATION_MODE_TOTAL_POINTS)) {
            if (pointsPossible > 0) {
                average = totalPoints / pointsPossible;
                average = GradesManager.scale(average, 0, 1, getMinimumPoints(), getMaximumPoints(), null, null);
            }

            setAverage(studentOid, average);

            m_pointsPossible.put(studentOid, Double.valueOf(pointsPossible));
            m_pointTotals.put(studentOid, Double.valueOf(totalPoints));
        } else {
            removeAverage(studentOid);

            m_pointsPossible.remove(studentOid);
            m_pointTotals.remove(studentOid);
        }
    }

    /**
     * Calculate by weight.
     *
     * @param studentOid String
     * @see
     *      com.x2dev.sis.model.business.gradebook.AverageCalculator#calculateByWeight(java.lang.String)
     */
    @Override
    protected void calculateByWeight(String studentOid) {
        double totalWeight = 0;
        double average = 0;

        int components = m_componentAverages.size() + m_componentColumns.size();
        ArrayList scores = new ArrayList(components);
        ArrayList weights = new ArrayList(components);

        // determine weight total, individual weights and scores
        Iterator componentAverages = m_componentAverages.keySet().iterator();
        while (componentAverages.hasNext()) {
            AverageCalculator componentAverage = (AverageCalculator) componentAverages.next();
            componentAverage.recalculateAverages(studentOid);
            Double averageValue = componentAverage.getAverageNumericUnrounded(studentOid);

            if (averageValue != null) {
                Double weight = getAverageWeight(componentAverage);

                totalWeight += weight.doubleValue();

                scores.add(averageValue);
                weights.add(weight);
            }
        }

        Iterator componentColumns = m_componentColumns.keySet().iterator();
        while (componentColumns.hasNext()) {
            String columnOid = (String) componentColumns.next();
            Double scoreValue = getScoreValue(studentOid, columnOid);

            if (!isHidden(columnOid, studentOid)) {
                if (scoreValue != null) {
                    Double weight = getColumnWeight(columnOid);

                    if (!isExtraCredit(columnOid)) {
                        totalWeight += weight.doubleValue();
                    }

                    scores.add(scoreValue);
                    weights.add(weight);
                }
            }
        }

        // calculate
        for (int i = 0; i < scores.size(); i++) {
            Double score = (Double) scores.get(i);
            Double weight = (Double) weights.get(i);

            average += (score.doubleValue() * (weight.doubleValue() / totalWeight));
        }

        if (totalWeight > 0) {
            setAverage(studentOid, average);
        } else {
            removeAverage(studentOid);
        }
    }
}
