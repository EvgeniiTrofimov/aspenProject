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
package com.x2dev.reports.sys.sped;

import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Report that lists the students receiving one or more accommodations on current active IEPs.
 * The report is based on the accommodations present in the student's active IEP.
 *
 * @author X2 Development Corporation
 */
public class AccommodationDirectoryData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Input parameter containing the accommodation name to include in the report. If a value is
     * not provided for this parameter, all accommodations are included.
     */
    public static final String PARAM_ACCOMMODATION_NAME = "name";

    /**
     * Input parameter containing the OID of the case manager (Staff) to include in the report. If a
     * value is not provided for this parameter, all accommodations are included.
     */
    public static final String PARAM_CASE_MANAGER_OID = "caseManagerOid";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        String name = (String) getParameter(PARAM_ACCOMMODATION_NAME);
        String caseManagerOid = (String) getParameter(PARAM_CASE_MANAGER_OID);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepAccommodation.REL_IEP_DATA + "." + IepData.COL_STATUS_CODE,
                Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()));

        if (!StringUtils.isEmpty(name)) {
            criteria.addEqualTo(IepAccommodation.COL_NAME, name);
        }

        if (!StringUtils.isEmpty(caseManagerOid)) {
            criteria.addEqualTo(IepAccommodation.REL_IEP_DATA + "." + IepData.COL_STAFF_OID, caseManagerOid);
        }

        criteria.addAndCriteria(getOrganizationCriteria(IepAccommodation.class));

        QueryByCriteria query = new QueryByCriteria(IepAccommodation.class, criteria);

        query.addOrderByAscending(IepAccommodation.COL_NAME);
        query.addOrderByAscending(IepAccommodation.REL_STUDENT + "." + SisStudent.COL_NAME_VIEW);

        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query),
                DataDictionary.getDistrictDictionary(getExtendedDictionary(), getBroker().getPersistenceKey()),
                true,
                getLocale());
    }
}
