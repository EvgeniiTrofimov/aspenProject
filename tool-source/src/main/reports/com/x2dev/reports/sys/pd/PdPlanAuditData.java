/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.pd;

import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.SqlDataSource;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for the "PD Plan Audit" report, which displays record counts of PD plan data for
 * conversion verification purporses.
 *
 * @author X2 Development Corporation
 */
public class PdPlanAuditData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "active only" report parameter. The value is a Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Name for the enumerated "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
        int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
        String activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STAFF_ACTIVE_CODE);

        String sql =

                "SELECT STF_OID, STF_NAME_VIEW, STF_ID_LOCAL, SKL_SCHOOL_NAME, STF_DEPARTMENT_CODE, PLAN_COUNT, ACTIVITY_COUNT, REVIEW_COUNT "
                        +
                        "FROM STAFF " +
                        "LEFT OUTER JOIN SCHOOL ON STF_SKL_OID = SKL_OID " +
                        "LEFT OUTER JOIN " +
                        "(SELECT SDA_STF_OID, count(*) as ACTIVITY_COUNT " +
                        "FROM STAFF_PD_ACTIVITY " +
                        "WHERE SDA_STF_OID is not NULL " +
                        "GROUP BY SDA_STF_OID) x_activities ON STF_OID = SDA_STF_OID " +
                        "LEFT OUTER JOIN " +
                        "(SELECT SDR_STF_OID, count(*) as REVIEW_COUNT " +
                        "FROM STAFF_PD_PLAN_REVIEW " +
                        "WHERE SDR_STF_OID is not NULL " +
                        "GROUP BY SDR_STF_OID) x_reviews ON STF_OID = SDR_STF_OID " +
                        "LEFT OUTER JOIN " +
                        "(SELECT SDP_STF_OID, count(*) as PLAN_COUNT " +
                        "FROM STAFF_PD_PLAN " +
                        "WHERE SDP_STF_OID is not NULL " +
                        "GROUP BY SDP_STF_OID) x_plans ON STF_OID = SDP_STF_OID " +
                        "WHERE STF_ORG_OID_1 = '" + getOrganization().getOid() + "' " +
                        (activeOnly ? "AND STF_STATUS = '" + activeCode + "' " : "") +
                        "ORDER BY " +
                        (sort == 0 ? " STF_NAME_VIEW " : " STF_ID_LOCAL, STF_NAME_VIEW ");

        return new SqlDataSource(sql, new Object[0], getBroker());
    }
}
