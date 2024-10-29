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
package com.x2dev.reports.bc;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.tools.procedures.SessionAwareProcedure;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.Transcript;
import java.util.List;
import org.apache.ojb.broker.query.Criteria;

/**
 * Procedure to create a new transcript record and/or a new student schedule record for any student
 * transcript records
 * that have a null final grade.
 *
 * @author X2 Development Corporation
 */
public class ContinuedCourseTranscriptCleanupProcedure extends ProcedureJavaSource implements SessionAwareProcedure {
    private static final String PARAM_LIST_ONLY = "listOnly";

    // Used for logging totals
    private int m_deletedTranscripts = 0;

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.SessionAwareProcedure#setUserData(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    public void setUserData(UserDataContainer userData) {
        // Do Nothing here
    }

    /**
     * @see com.x2dev.sis.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        boolean listOnly = ((Boolean) getParameter(PARAM_LIST_ONLY)).booleanValue();

        if (this.isSchoolContext()) {
            DistrictSchoolYearContext lastYear = getLastYearContext();
            if (lastYear != null) {
                try (QueryIterator iterator =
                        getBroker().getIteratorByQuery(getCopiedContinuedCourseTranscriptListForLastYear(lastYear))) {
                    while (iterator.hasNext()) {
                        Transcript trn = (Transcript) iterator.next();
                        deleteTranscript(trn, listOnly);
                    }
                }
            } else {
                logMessage("There is no last year school context, procedure cannot be run");
            }
        } else {

            logMessage("This procedure can only be run in the school view. No action was taken.");
        }

        logMessage("\n");
        logMessage("---------------------------------");
        if (listOnly) {
            logMessage("PREVIEW: Total transcripts marked to be deleted: " + m_deletedTranscripts);
        } else {
            logMessage("Total transcripts deleted: " + m_deletedTranscripts);
        }
    }

    /**
     * Deletes the student transcript that was flagged as a copied transcript
     *
     * @param trn
     * @param listOnly
     */
    private void deleteTranscript(Transcript trn, boolean listOnly) {
        String description = trn.getStudent().getNameView() + " for course description: " + trn.getCourseDescription()
                + " and course number: "
                + trn.getCourseNumber();

        if (!listOnly) {
            List errors = getBroker().deleteBean(trn);
            if (errors != null && errors.isEmpty()) {
                logMessage(
                        "Deleted student transcript for [" + description + "] because it was copied to the new year");
            } else {
                for (Object error : errors) {
                    logMessage("ERROR: unable to delete student transcript for [" + trn.getOid() + "] because "
                            + error.toString());
                }
            }
        } else {
            logMessage("PREVIEW: Student transcript for [" + description
                    + "] was copied to the new year and is MARKED to delete.");
        }
        logMessage("");
        m_deletedTranscripts++;
    }

    /**
     * This returns a query to get the transcripts for the given districtschoolyearcontext. There
     * must not be a final
     * grade, the school has to match, the school year will be last year and the copiedIndicator
     * must be true
     *
     * @param lastYear
     *
     * @return BeanQuery query
     */
    private BeanQuery getCopiedContinuedCourseTranscriptListForLastYear(DistrictSchoolYearContext lastYear) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEmpty(Transcript.COL_FINAL_GRADE, getBroker().getPersistenceKey());
        criteria.addEqualTo(Transcript.COL_SCHOOL_OID, this.getSchool().getOid());
        criteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, lastYear.getOid());
        criteria.addEqualTo(Transcript.COL_COPY_CONTINUE_IND, Boolean.TRUE);
        return new BeanQuery(Transcript.class, criteria);
    }

    /**
     * Gets last year's districtschoolyearcontext. This is used to get last year's transcript
     * records.
     *
     * @return DistrictSchoolYearContext
     */
    private DistrictSchoolYearContext getLastYearContext() {
        Criteria yearCriteria = new Criteria();
        Integer schoolYear = Integer.valueOf(getCurrentContext().getSchoolYear() - 1);
        yearCriteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR, schoolYear);
        BeanQuery yearQuery = new BeanQuery(DistrictSchoolYearContext.class, yearCriteria);
        return (DistrictSchoolYearContext) getBroker().getBeanByQuery(yearQuery);
    }
}
