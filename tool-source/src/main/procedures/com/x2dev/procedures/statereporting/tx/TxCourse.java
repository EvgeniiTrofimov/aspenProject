/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2015 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.tx;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.utils.X2BaseException;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This is a class for Tx Course Complex Type export (Education Organization Interchange).
 *
 * @author X2 Development Corporation
 */
public class TxCourse extends TxCoreReportData {

    private static final String PARAM_SCHOOL_YEAR_CONTEXT = "schoolYearContext";

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.x2dev.sis.model.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        String contextOid = (String) getParameter(PARAM_SCHOOL_YEAR_CONTEXT);
        // If no errors so far, continue with query.
        if (getSetupErrors().size() == 0) {
            X2Criteria crsCriteria = new X2Criteria();
            crsCriteria.addEqualTo(Course.COL_DISTRICT_CONTEXT_OID, contextOid);
            crsCriteria.addEqualTo(Course.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);
            QueryByCriteria crsQuery = new QueryByCriteria(Course.class, crsCriteria);

            setQuery(crsQuery);
        }
    }


}
