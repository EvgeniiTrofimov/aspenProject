/* ++++ Upgraded by PhoenixUpgradeProcedure ++++ on Mon Feb 20 18:12:41 CST 2012 *//*
                                                                                    * ==============
                                                                                    * ==============
                                                                                    * ==============
                                                                                    * ==============
                                                                                    * ============
                                                                                    *
                                                                                    * X2 Development
                                                                                    * Corporation
                                                                                    *
                                                                                    * Copyright (c)
                                                                                    * 2002-2004 X2
                                                                                    * Development
                                                                                    * Corporation.
                                                                                    * All rights
                                                                                    * reserved.
                                                                                    *
                                                                                    * Redistribution
                                                                                    * and use in
                                                                                    * source and
                                                                                    * binary forms,
                                                                                    * with or
                                                                                    * without
                                                                                    * modification,
                                                                                    * is not
                                                                                    * permitted
                                                                                    * without a
                                                                                    * written
                                                                                    * agreement
                                                                                    * from X2
                                                                                    * Development
                                                                                    * Corporation.
                                                                                    *
                                                                                    * ==============
                                                                                    * ==============
                                                                                    * ==============
                                                                                    * ==============
                                                                                    * ============
                                                                                    */
package com.x2dev.reports.sys.sped.ma;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepPlacement;
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.sis.model.beans.IepServiceLog;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class IepServiceLogMedicaidExportData.
 */
public class IepServiceLogMedicaidExportData extends ReportJavaSourceNet {
    private static final long serialVersionUID = 1L;
    /**
     * An optional parameter specifying a specific case manager to filter the report on
     */
    public static final String CASE_MANAGER_PARAM = "caseManager";

    /**
     * Name for the "end date" input parameter. This value is a PlainDate object.
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * A parameter specifying if to exclude consultation services from the report
     */
    public static final String EXCLUDE_CONSULT_PARAM = "excludeConsult";

    /**
     * An optional parameter specifying a specific placement program to filter the report on
     */
    public static final String PLACEMENT_PARAM = "placement";

    /**
     * A parameter passed to iReport specifying whether or not an individual provider was selected
     */
    public static final String PROVIDER_SPECIFIED = "providerSpecified";

    /**
     * School OID parameter specifying which school the services should be listed for
     */
    public static final String SCHOOL_OID_PARAM = "schoolOid";

    /**
     * Start service provider parameter specifying the name of the service provider
     */
    public static final String SERVICE_PROVIDER_PARAM = "serviceProvider";

    /**
     * Name for the "start date" input parameter. This value is a PlainDate object.
     */
    public static final String START_DATE_PARAM = "startDate";

    private PlainDate m_endDate = null;
    private PlainDate m_startDate = null;

    // Database Field constant (TODO: make this a system preference)
    private static String CONSULTATION_FIELD = "Consultation";

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {

        m_endDate = (PlainDate) getParameter(END_DATE_PARAM);
        m_startDate = (PlainDate) getParameter(START_DATE_PARAM);

        Criteria criteria = new Criteria();

        criteria.addLessOrEqualThan(IepServiceLog.COL_DATE, m_endDate);
        criteria.addGreaterOrEqualThan(IepServiceLog.COL_DATE, m_startDate);

        String schoolOid = (String) getParameter(SCHOOL_OID_PARAM);

        if (!StringUtils.isEmpty(schoolOid)) {
            criteria.addEqualTo(IepServiceLog.REL_IEP_SERVICE + PATH_DELIMITER + IepService.REL_STUDENT + PATH_DELIMITER
                    + SisStudent.COL_SCHOOL_OID, schoolOid);
        }

        if (!StringUtils.isEmpty((String) getParameter(CASE_MANAGER_PARAM))) {
            criteria.addEqualTo(IepServiceLog.REL_IEP_SERVICE + PATH_DELIMITER + IepService.REL_IEP_DATA
                    + PATH_DELIMITER + IepData.COL_STAFF_OID, getParameter(CASE_MANAGER_PARAM));
        }

        if (!StringUtils.isEmpty((String) getParameter(PLACEMENT_PARAM))) {
            criteria.addEqualTo(
                    IepServiceLog.REL_IEP_SERVICE + PATH_DELIMITER + IepService.REL_IEP_DATA + PATH_DELIMITER
                            + IepData.REL_PLACEMENTS + PATH_DELIMITER + IepPlacement.COL_IEP_PLACEMENT_PROGRAM_OID,
                    getParameter(PLACEMENT_PARAM));
        }

        if (!StringUtils.isEmpty((String) getParameter(SERVICE_PROVIDER_PARAM))) {
            criteria.addEqualTo(IepServiceLog.REL_IEP_SERVICE + PATH_DELIMITER + IepService.COL_STAFF_OID,
                    getParameter(SERVICE_PROVIDER_PARAM));
            addParameter(PROVIDER_SPECIFIED, Boolean.valueOf(true));
        }

        if (Boolean.valueOf((String) getParameter(EXCLUDE_CONSULT_PARAM)).booleanValue()) {
            criteria.addNotEqualTo(IepServiceLog.REL_IEP_SERVICE + PATH_DELIMITER + IepService.COL_SERVICE_MODE,
                    CONSULTATION_FIELD);
        }

        QueryByCriteria query = new QueryByCriteria(IepServiceLog.class, criteria);

        /* Execute the query */
        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query));
    }
}
