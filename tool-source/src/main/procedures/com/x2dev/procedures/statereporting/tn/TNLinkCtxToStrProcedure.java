/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.StudentTransportation;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure to link CTX to historical STR records.
 *
 * @author X2 Development Corporation
 */
public class TNLinkCtxToStrProcedure extends ProcedureJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 

    private Collection<DistrictSchoolYearContext> m_ctxList;
    private Collection<StudentTransportation> m_strList;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.x2dev.sis.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        loadSchoolYears();
        loadTransportations();
        if (m_strList != null && !m_strList.isEmpty()) {
            for (StudentTransportation str : m_strList) {
                DistrictSchoolYearContext ctx = null;
                PlainDate startDate = str.getStartDate();
                PlainDate endDate = str.getEndDate();
                if (endDate != null) {
                    if (!startDate.after(endDate)) {
                        ctx = getCtxByDate(endDate);
                    } else {
                        logMessage("ERROR! Transportation for STUDENT with LASID = " + str.getStudent().getLocalId()
                                + " has start date after end date.");
                    }
                } else {
                    ctx = getCtxByDate(startDate);
                }
                if (ctx != null) {
                    if (str.getDistrictContext() == null) {
                        str.setDistrictContextOid(ctx.getOid());
                        getBroker().saveBeanForced(str);
                        logMessage("For Transportation for STUDENT with LASID = " + str.getStudent().getLocalId()
                                + " was set " + ctx.getContextName());
                    } else {
                        logMessage("Nothing to Update!!! STUDENT with LASID = " + str.getStudent().getLocalId()
                                + " have transportation with: " + str.getDistrictContext().getContextName());
                    }
                } else {
                    logMessage("School year for dates START DATE = " + startDate + ", END DATE = " + endDate
                            + " was not found!!!");
                }
            }
        } else {
            logMessage("No Student Transportations with empty School Year were found! Nothing to update!");
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
    }

    /**
     * Gets the ctx by date.
     *
     * @param date PlainDate
     * @return District school year context
     */
    private DistrictSchoolYearContext getCtxByDate(PlainDate date) {
        DistrictSchoolYearContext ctxToReturn = null;
        for (DistrictSchoolYearContext ctx : m_ctxList) {
            if (!date.before(ctx.getStartDate()) && !date.after(ctx.getEndDate())) {
                ctxToReturn = ctx;
                break;
            }
        }
        return ctxToReturn;
    }

    /**
     * Load all the district contexts.
     */
    private void loadSchoolYears() {
        QueryByCriteria distrctContextQuery = new QueryByCriteria(DistrictSchoolYearContext.class, new X2Criteria());
        distrctContextQuery.addOrderByDescending(DistrictSchoolYearContext.COL_CONTEXT_ID);
        m_ctxList = getBroker().getCollectionByQuery(distrctContextQuery);
    }

    /**
     * Load all the district contexts.
     */
    private void loadTransportations() {
        X2Criteria strCriteria = new X2Criteria();
        strCriteria.addNotNull(StudentTransportation.COL_START_DATE);
        QueryByCriteria strQuery = new QueryByCriteria(StudentTransportation.class, strCriteria);
        m_strList = getBroker().getCollectionByQuery(strQuery);
    }
}
