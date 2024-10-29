/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2011 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.ri;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.Locale;

/**
 * Data source for the service census audit report. This report should be run from a location that
 * has a current Student on the session.
 * <p>
 * This report displays all services that were included in the last run of the Services Census
 * extract or validation report, as well as those that were eliminated programatically by the
 * census due to overlaps or other date issues.
 *
 * @author mmastrangelo
 */
public class ServiceCensusAuditData extends ReportJavaSourceNet {
    protected static final String SERVICE_IN_LAST_STATE_REPORT_ALIAS = "service-in-last-state-report";
    protected static final String SERVICE_ADJUSTMENT_REASON_ALIAS = "service-adjustment-reason";

    private String m_studentOid;

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

        Student student = userData.getCurrentRecord(Student.class);
        m_studentOid = student.getOid();
    }

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(getExtendedDictionary(), getBroker().getPersistenceKey());
        DataDictionaryField inReportField =
                dictionary.findDataDictionaryFieldByAlias(SERVICE_IN_LAST_STATE_REPORT_ALIAS);
        DataDictionaryField adjustmentReasonField =
                dictionary.findDataDictionaryFieldByAlias(SERVICE_ADJUSTMENT_REASON_ALIAS);

        X2Criteria serviceCriteria = new X2Criteria();
        serviceCriteria.addNotEmpty(adjustmentReasonField.getJavaName(), getBroker().getPersistenceKey());
        serviceCriteria.addOrEqualTo(inReportField.getJavaName(), BooleanAsStringConverter.TRUE);
        serviceCriteria.setEmbraced(true);

        X2Criteria reportCriteria = new X2Criteria();
        reportCriteria.addEqualTo(IepService.REL_IEP_DATA + "." + IepData.COL_STUDENT_OID, m_studentOid);
        reportCriteria.addAndCriteria(serviceCriteria);

        BeanQuery query = new BeanQuery(IepService.class, reportCriteria);
        query.addOrderByDescending(IepService.REL_IEP_DATA + "." + IepData.COL_START_DATE);
        query.addOrderByAscending(IepService.REL_IEP_DATA + "." + IepData.COL_STATUS_CODE);
        query.addOrderByAscending(IepService.COL_SERVICE_MODE);
        query.addOrderByAscending(IepService.COL_SERVICE_TYPE);
        query.addOrderByAscending(IepService.COL_START_DATE);

        return new QueryIteratorDataSource(getBroker().getIteratorByQuery(query), dictionary, true,
                Locale.getDefault());
    }
}
