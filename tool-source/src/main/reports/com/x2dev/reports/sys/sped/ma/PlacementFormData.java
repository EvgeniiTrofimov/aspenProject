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
package com.x2dev.reports.sys.sped.ma;

import com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource;

/**
 * Java source for the placement form. This class supports multiple pages. Page 1 is the Team
 * Placement Form (PL1); page 2 is the Administrative Placement Form (PL2).
 *
 * @author X2 Development Corporation
 */
public class PlacementFormData extends MultiPageFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private static final String PL1_FORMAT_ID = "SYS-SPED-MA-PL1";
    private static final String PL2_FORMAT_ID = "SYS-SPED-MA-PL2";

    /**
     * Gets the format ids.
     *
     * @return String[]
     * @see com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#getFormatIds()
     */
    @Override
    protected String[] getFormatIds() {
        return new String[] {PL1_FORMAT_ID, PL2_FORMAT_ID};
    }
}
