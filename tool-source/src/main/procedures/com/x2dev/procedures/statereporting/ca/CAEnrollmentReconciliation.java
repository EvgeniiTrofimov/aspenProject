/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.ca;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export Data Module for CA Staff Assignment.
 *
 * @author X2 Development Corporation
 */

public class CAEnrollmentReconciliation extends StateReportData {
    /**
     * Entity class for CA Enrollment Reconciliation report.
     *
     */
    public static class CAEnrollmentReconciliationEntity extends StateReportEntity {
        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public CAEnrollmentReconciliationEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
        }
    }

    private static final String PARAM_END_DATE = "endDate";
    private static final String PARAM_START_DATE = "startDate";

    private String m_endCycle;
    private PlainDate m_endDate;
    private ADADataHelper m_dataHelper;
    private String m_startCycle;
    private PlainDate m_startDate;

    /**
     * Define query .
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_dataHelper = new ADADataHelper(getBroker(), getOrganization());

        m_startCycle = (String) getParameter(ADADataHelper.INPUT_PARAM_START_CYCLE);
        m_endCycle = (String) getParameter(ADADataHelper.INPUT_PARAM_END_CYCLE);

        m_startDate = (PlainDate) getParameter(PARAM_START_DATE);
        m_endDate = (PlainDate) getParameter(PARAM_END_DATE);

        if (m_startDate != null && m_endDate != null) {
            // TODO logic for start and end date
        } else {
            m_dataHelper.setCycle(null, m_startCycle, m_endCycle, null, false);
            PlainDate[] dates = m_dataHelper.getDays();
            m_startDate = dates[0];
            m_endDate = dates[dates.length - 1];
        }

        X2Criteria criteria = new X2Criteria();
        criteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_startDate);
        criteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_endDate);

        if (isSchoolContext()) {
            criteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addAndCriteria((X2Criteria) getParameter("organizationCriteria"));
        }

        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);

        this.applyInputSort(query, null);
        this.setQuery(query);

        // Set Custom Entity
        this.setEntityClass(CAEnrollmentReconciliationEntity.class);

    }
}
