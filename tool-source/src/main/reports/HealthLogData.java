/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.HealthLog;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Health Log" report.
 * <p>
 * This report collects student health logs from the current school.
 *
 * @author X2 Development Corporation
 */
public class HealthLogData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "date" report input parameter. This value is a PlainDate.
     */
    public static final String DATE_PARAM = "date";

    /**
     * Name for the "sort by" report input parameter. This value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        /*
         * Build the criteria based on user input
         */
        Criteria criteria = new Criteria();
        criteria.addEqualTo(HealthLog.COL_DATE, getParameter(DATE_PARAM));

        if (isSchoolContext()) {
            criteria.addEqualTo(HealthLog.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addAndCriteria(getOrganizationCriteria(HealthLog.class));
        }

        QueryByCriteria query = new QueryByCriteria(HealthLog.class, criteria);
        applyUserSort(query, (String) getParameter(SORT_PARAM));

        /*
         * Execute the query and return the results
         */
        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
    }
}
