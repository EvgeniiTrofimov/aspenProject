/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.business.GradeAverageCalculationCache;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.GradeAverageCalculation;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Creates Any default GradeAverageCacluations that are missing
 * Debug mode will show what it will create but will also
 * refresh the GradeAverageCalculationCache on its own
 *
 * USEAGE - Run once on any node with DEBUG = False to
 * create the GradeAverageCalculations. Run on all other
 * nodes with DEBUG = True, to refresh the GradeAverageCalculationCache
 * on those nodes.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class CreateDefaultGradeAverageCalculations extends ProcedureJavaSource {
    boolean debug = true;


    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        X2Broker broker = getBroker();
        QueryByCriteria query = new QueryByCriteria(School.class, new X2Criteria(), true);
        Collection<School> schools = broker.getCollectionByQuery(query);

        for (School school : schools) {

            logMessage("Starting Procedure for School: " + school.getName());

            // Removes the school from the cache so that the new records can be saved.
            GradeAverageCalculationCache.removeSchool(broker.getPersistenceKey().getDeploymentId(), school.getOid());

            // Use my own version of this method from GradeAverageCalculation class
            // So I can set the gradesAverageCalculation count to 2 instead of 0
            createDefaultGradeAverageCalculations(school.getOid(), broker);

            // Re-initialize the GradeAverageCalculationCache to avoid rolling restarts
            initializeGradeAverageCalculationCache(broker);
        }

    }

    /**
     * Creates the four standard GradeAverageCalculation entries for a given school.
     *
     * @param schoolOid String
     * @param broker X2Broker
     */
    public void createDefaultGradeAverageCalculations(String schoolOid, X2Broker broker) {
        Set<Integer> gradeAverageCalcList = new HashSet<Integer>();

        X2Criteria procedureExistenceCriteria = new X2Criteria();
        procedureExistenceCriteria.addEqualTo(GradeAverageCalculation.COL_SCHOOL_OID, schoolOid);
        QueryByCriteria query = new QueryByCriteria(GradeAverageCalculation.class, procedureExistenceCriteria);
        Collection<GradeAverageCalculation> gradeAverageCalculations = broker.getCollectionByQuery(query);

        // Create list of IDs that already exist at the school
        for (GradeAverageCalculation gradeAverageCalcs : gradeAverageCalculations) {
            Integer id = Integer.valueOf(gradeAverageCalcs.getId());
            gradeAverageCalcList.add(id);

        }

        int gradeAverageCalculationCount = broker.getCount(query);
        logMessage("Number of gradeAveragesCalcs = " + gradeAverageCalculationCount);

        X2Criteria defaultCalculationCriteria = new X2Criteria();
        defaultCalculationCriteria.addEmpty(GradeAverageCalculation.COL_SCHOOL_OID, broker.getPersistenceKey());

        QueryByCriteria defaultCalculationQuery =
                new QueryByCriteria(GradeAverageCalculation.class, defaultCalculationCriteria);

        Collection<GradeAverageCalculation> defaultGradeAverageCalculations =
                broker.getCollectionByQuery(defaultCalculationQuery);

        int count = 0;
        for (GradeAverageCalculation gradeAverageCalculation : defaultGradeAverageCalculations) {

            // Do not re-copy any GradeAverageCalculations that already exist
            Integer id = Integer.valueOf(gradeAverageCalculation.getId());
            if (!gradeAverageCalcList.contains(id)) {

                logMessage("gradeAverageCalculation being written = " + gradeAverageCalculation.getName());
                count++;

                if (!debug) {
                    GradeAverageCalculation schoolGradeAverageCalculation =
                            (GradeAverageCalculation) gradeAverageCalculation.copyBean();
                    schoolGradeAverageCalculation.setSchoolOid(schoolOid);
                    broker.saveBean(schoolGradeAverageCalculation);
                }
            }

        }
        logMessage("Number of Gade Average Calcs Created @ " + schoolOid + ": " + count);
    }

    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        // TODO Auto-generated method stub
        runOnApplicationServer();
    }

    /**
     * Initializes the gradeAverageCalculationCache for each PersistenceKey.
     *
     * @param broker X2Broker
     *
     * @param primaryPersistenceKeys Collection<PersistenceKey>
     */
    public void initializeGradeAverageCalculationCache(X2Broker broker) {
        GradeAverageCalculationCache.initialize(Arrays.asList(broker.getPersistenceKey()));
    }
}

