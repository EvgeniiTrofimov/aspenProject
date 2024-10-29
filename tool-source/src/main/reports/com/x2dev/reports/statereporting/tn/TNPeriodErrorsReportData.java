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
package com.x2dev.reports.statereporting.tn;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class TNPeriodErrorsReportData.
 */
public class TNPeriodErrorsReportData extends ReportJavaSourceNet {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 
    /**
     * Constants
     */
    private static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL STATE ID";
    private static final String ERRROR_DATE_NOT_SETUP = "Not setup";
    private static final String REF_TBL_OID = "rtbAttLockCycl";

    /**
     * Data grid columns.
     */
    private static final String COLUMN_SCHOOL_NAME = "schoolName";
    private static final String COLUMN_SCHOOL_YEAR = "schoolYear";
    private static final String COLUMN_PERIOD = "period";
    private static final String COLUMN_DATE_BEGIN = "beginDate";
    private static final String COLUMN_DATE_END = "endDate";

    /**
     * Member fields
     */
    private X2Broker m_broker;
    private DistrictSchoolYearContext m_context = null;
    private Map<String, ReferenceCode> m_cycleCodeMap;
    private TNReportingPeriodHelper m_periodHelper = null;
    private Collection<SisSchool> m_schools;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        Map<String, Map<String, Map<String, PlainDate>>> sclDatesMap = m_periodHelper.getSclDatesMap();

        ReportDataGrid grid = new ReportDataGrid();

        if (sclDatesMap != null) {
            for (SisSchool school : m_schools) {
                Map<String, Map<String, PlainDate>> periodsMap = sclDatesMap.get(school.getOid());

                if (periodsMap != null) {
                    PlainDate dateBegin = null;
                    PlainDate dateEnd = null;

                    String dateBeginStr = null;
                    String dateEndStr = null;

                    for (String periodCode : m_cycleCodeMap.keySet()) {
                        grid.append();
                        grid.set(COLUMN_SCHOOL_NAME, school.getName());
                        grid.set(COLUMN_SCHOOL_YEAR, getCurrentContext().getContextName());
                        grid.set(COLUMN_PERIOD, periodCode);

                        Map<String, PlainDate> datesMap = periodsMap.get(periodCode);

                        if (datesMap != null) {
                            dateBegin = datesMap.get(TNReportingPeriodHelper.KEY_DATE_BEGIN);
                            dateEnd = datesMap.get(TNReportingPeriodHelper.KEY_DATE_END);
                        }

                        dateBeginStr = dateBegin != null ? dateBegin.toString() : ERRROR_DATE_NOT_SETUP;
                        dateEndStr = dateEnd != null ? dateEnd.toString() : ERRROR_DATE_NOT_SETUP;

                        grid.set(COLUMN_DATE_BEGIN, dateBeginStr);
                        grid.set(COLUMN_DATE_END, dateEndStr);
                    }
                } else {
                    for (String periodCode : m_cycleCodeMap.keySet()) {
                        grid.append();
                        grid.set(COLUMN_SCHOOL_NAME, school.getName());
                        grid.set(COLUMN_SCHOOL_YEAR, getCurrentContext().getContextName());
                        grid.set(COLUMN_PERIOD, periodCode);
                        grid.set(COLUMN_DATE_BEGIN, ERRROR_DATE_NOT_SETUP);
                        grid.set(COLUMN_DATE_END, ERRROR_DATE_NOT_SETUP);
                    }
                }
            }
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_broker = getBroker();
        m_context = getCurrentContext();
        m_periodHelper = new TNReportingPeriodHelper(getOrganization(), m_context, null, m_broker);
        initializeCyclesCodeMap();
        initializeSchools();

    }

    /**
     * Initialize map of cycles program codes.
     */
    private void initializeCyclesCodeMap() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, REF_TBL_OID);
        criteria.addNotEqualTo(ReferenceCode.COL_CODE, "00");
        criteria.addNotEqualTo(ReferenceCode.COL_CODE, "10");
        criteria.addNotEqualTo(ReferenceCode.COL_CODE, "99");
        QueryByCriteria cyclesQuery = new QueryByCriteria(ReferenceCode.class, criteria);
        m_cycleCodeMap = m_broker.getMapByQuery(cyclesQuery, ReferenceCode.COL_CODE, 64);
    }

    /**
     * Initialize map of schools.
     */
    private void initializeSchools() {
        X2Criteria schoolCriteria = new X2Criteria();

        schoolCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
        schoolCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
        DataDictionaryField aliasSklStateIDField =
                dictionary.findDataDictionaryFieldByAlias(ALIAS_SKL_STATE_ID);
        schoolCriteria.addNotEmpty(aliasSklStateIDField.getJavaName(), getBroker().getPersistenceKey());

        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
        schoolQuery.addOrderByAscending(SisSchool.COL_NAME);
        m_schools = getBroker().getCollectionByQuery(schoolQuery);
    }

}
