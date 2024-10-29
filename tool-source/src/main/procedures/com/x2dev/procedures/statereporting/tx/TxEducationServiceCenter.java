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
package com.x2dev.procedures.statereporting.tx;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Organization;
import com.x2dev.utils.X2BaseException;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This is a class for TXEducationServiceCenter Complex Type export (Education Organization
 * Interchange).
 *
 * @author X2 Development Corporation
 */
public class TxEducationServiceCenter extends TxCoreReportData {
    /*
     * Aliases
     */
    private static final String ALIAS_SERVICE_CENTER_CATEGORY = "all-org-Category";

    /*
     * Internal variables
     */
    private String m_fieldScCategory;

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

        initializeFields();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            X2Criteria orgCriteria = new X2Criteria();
            orgCriteria.addIn(m_fieldScCategory,
                    codesFromEdFiEnumerationByAlias(ALIAS_SERVICE_CENTER_CATEGORY, "Education Service Center"));
            QueryByCriteria orgQuery = new QueryByCriteria(Organization.class, orgCriteria);

            setQuery(orgQuery);
        }
    }

    /**
     * Initialize fields that are used in the class.
     */
    private void initializeFields() {
        m_fieldScCategory = translateAliasToJavaName(ALIAS_SERVICE_CENTER_CATEGORY, true);
    }
}
