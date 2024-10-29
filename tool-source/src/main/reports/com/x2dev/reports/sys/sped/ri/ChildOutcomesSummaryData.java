/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2011 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.ri;

import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;

/**
 * Java source for the RI COS form.
 *
 * @author X2 Development Corporation
 */
public class ChildOutcomesSummaryData extends BaseFormReportJavaSource {

    private static final String PARAM_SECTION_A = "sectionA";
    private static final String PARAM_SECTION_B = "sectionB";
    private static final String PARAM_SECTION_C = "sectionC";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        IepData iep = (IepData) getFormOwner();
        GenericFormData gfd = (GenericFormData) getFormStorage();
        if (iep != null && gfd != null) {
            addSectionVisibilityParameters();
        }

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * Set sections A,B.C visibility flags
     */
    private void addSectionVisibilityParameters() {
        for (String section : new String[] {PARAM_SECTION_A, PARAM_SECTION_B, PARAM_SECTION_C}) {
            addParameter(section, getParameter(section));
        }
    }
}
