/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.pa;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisDistrictSchoolYearContext;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class for district organization export.
 */
public class PADistrictSnapshot extends StateReportData {
    /**
     * Entity class for district organization export.
     *
     */
    public static class PADistrictSnapshotEntity extends StateReportEntity {

        /**
         * Instantiates a new PA district snapshot entity.
         */
        public PADistrictSnapshotEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisDistrictSchoolYearContext ctx = (SisDistrictSchoolYearContext) getBean();
            return ctx.getOrganization1().getName() + " " + ctx.getContextId();
        }
    }

    private final String PARAM_YEAR_CONTEXT = "reportYearContext";

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        if (getSetupErrors().size() != 0) {
            return;
        }

        String ctxOid = (String) getParameter(PARAM_YEAR_CONTEXT);

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(X2BaseBean.COL_OID, ctxOid);

        QueryByCriteria query = new QueryByCriteria(SisDistrictSchoolYearContext.class, criteria, true);

        setQuery(query);
        setEntityClass(PADistrictSnapshotEntity.class);
    }
}
