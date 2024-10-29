/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.sis.model.beans.StaffPosition;
import com.x2dev.utils.X2BaseException;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * @author Follett Software Company
 * @copyright 2021
 */
public class OnsisExportStaffPositions extends StateReportData {
    public static final String PARAM_CONTEXT_OID = "districtContextOid";

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return true;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        String contextOid = (String) getParameter(PARAM_CONTEXT_OID);
        DistrictSchoolYearContext ctx = getBroker().getBeanByOid(DistrictSchoolYearContext.class, contextOid);


        X2Criteria endDateCriteria = new X2Criteria();
        endDateCriteria.addIsNull(StaffPosition.COL_END_DATE);

        X2Criteria endDateCriteria2 = new X2Criteria();
        endDateCriteria2.addGreaterOrEqualThan(StaffPosition.COL_END_DATE, ctx.getStartDate());

        endDateCriteria.addOrCriteria(endDateCriteria2);

        X2Criteria startDateCriteria = new X2Criteria();
        startDateCriteria.addIsNull(StaffPosition.COL_START_DATE);

        X2Criteria startDateCriteria2 = new X2Criteria();
        startDateCriteria2.addLessOrEqualThan(StaffPosition.COL_START_DATE, ctx.getEndDate());

        startDateCriteria.addOrCriteria(startDateCriteria2);

        X2Criteria criteria = new X2Criteria();
        applyInputCriteria(criteria, true, null);
        criteria.addAndCriteria(startDateCriteria);
        criteria.addAndCriteria(endDateCriteria);


        QueryByCriteria query = new QueryByCriteria(StaffPosition.class, criteria);
        applyInputSort(query, null);
        setQuery(query);
    }

}
