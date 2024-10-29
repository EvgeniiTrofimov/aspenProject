/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2017 Follett Software Company.
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
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.model.business.gradebook.AverageCalculatorDataHelper;
import com.x2dev.sis.model.business.gradebook.AverageCalculatorFactory;
import com.x2dev.sis.model.business.gradebook.RubricRatingScaleCache;
import com.x2dev.sis.model.business.gradebook.StandardsOverallAverageCalculator;
import java.util.Collection;
import java.util.Map;

/**
 * Default implementation of OverallStandardsAverageCalculatorProcedure to be used to override
 * specific calculations/options done by AverageCalculator.
 *
 * @author Follett Software Company
 */
public class OverallStandardsAverageCalculatorProcedure extends StandardsOverallAverageCalculator {

    /**
     * The grade average calculation type.
     */
    public static final GradeAverageCalculationType CALCULATION_TYPE =
            GradeAverageCalculationType.OVERALL_STANDARDS_AVERAGE;

    /**
     * Creates a new OverallStandardsAverageCalculatorProcedure.
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
     * @param section MasterSchedule
     * @param rubricCache RubricRatingScaleCache
     * @param factory AverageCalculatorFactory
     */
    public OverallStandardsAverageCalculatorProcedure(GradeTerm term,
            int decimals,
            GradesManager gradesManager,
            Collection students,
            GradeScale averageScale,
            int calculationMode,
            Map<String, Collection<ReportingStandardAssignment>> standardAlignmentMap,
            X2Broker broker,
            boolean includeHiddenAssignments,
            AverageCalculatorDataHelper calcHelper,
            MasterSchedule section,
            RubricRatingScaleCache rubricCache,
            AverageCalculatorFactory factory) {
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
                section,
                rubricCache,
                factory);
    }


    /**
     * @see com.x2dev.sis.model.business.gradebook.StandardsOverallAverageCalculator#getCalculationColumnsType()
     */
    @Override
    protected CALCULATION_COLUMNS getCalculationColumnsType() {
        // Choices are either Trend or Average column for the individual standards.
        // CALCULATION_COLUMNS.TREND || CALCULATION_COLUMNS.AVERAGE
        return CALCULATION_COLUMNS.TREND;
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
        // By default calculate by weight where each standard receives equal weighting.
        calculateByWeight(studentOid);
    }
}
