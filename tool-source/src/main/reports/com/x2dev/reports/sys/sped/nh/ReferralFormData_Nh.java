/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.nh;

import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.business.sped.NewHampshireAliases;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for the New Hampshire New SPED Student Data Entry Form.
 * Returns the owner object (IepData bean), and queries for the name and phone of the staff member
 * who entered the form (passed as parameters).
 *
 * @author X2 Development Corporation
 */

public class ReferralFormData_Nh extends BaseFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Parameter: Name of completed-by staff member (String)
     */
    public static final String PARAM_STAFF_NAME = "staffName";

    /**
     * Parameter: Completed-by staff member's phone number (String)
     */
    public static final String PARAM_STAFF_PHONE = "staffPhone";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        // Retrieve the user-inputed staff OID
        String staffOid = (String) getFormStorage().getFieldValueByAlias(NewHampshireAliases.COMPLETED_STAFF_OID,
                getDictionary());

        // Retrieve the staff bean corresponding to the specified staff OID
        SisStaff completedByUser = (SisStaff) getBroker().getBeanByOid(SisStaff.class, staffOid);

        // If the bean was successfully retrieved, store the staff's name and phone values as
        // parameters
        if (completedByUser != null) {
            addParameter(PARAM_STAFF_NAME, completedByUser.getNameView());
            addParameter(PARAM_STAFF_PHONE, completedByUser.getSchool().getAddress().getPhone01());
        }

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }


}
