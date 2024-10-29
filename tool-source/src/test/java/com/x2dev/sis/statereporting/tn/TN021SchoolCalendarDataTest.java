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
package com.x2dev.sis.statereporting.tn;

import org.junit.Test;

/**
 * The Class TN021SchoolCalendarDataTest.
 */
// @FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TN021SchoolCalendarDataTest extends TNSTestBaseClass {

    /**
     * Run test.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest() throws Exception {
        setTNParameters();
        setStateReportFileName("TNSchoolCalendarData.java");
        setInputDefinitionFileName("TNInstructionalProgramReportPeriodInput.xml");
        setToolInputParameter("saveResults", "true");
        setToolInputParameter("contextOid", "ctx00000009999");
        setExportFormatFileName("export-format-EXPDATA-TN-CASP.xml");
        setStateReportProcedureId("EXPDATA-TN-CASP");
        setImportExportDefinitionId("EXP-TN-CASP");
        setInputJsonBeansFileName("json-input-data-TN-CASP.json");
        setExpectedResultFileName("TN-CASP-expected.csv");

        run();
    }
}
