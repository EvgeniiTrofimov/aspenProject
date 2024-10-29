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
 * The Class TN200DayCalendarTest.
 */
// @FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TN200DayCalendarTest extends TNSTestBaseClass {

    /**
     * Run test.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest() throws Exception {
        setTNParameters();
        setStateReportFileName("TN200DayCalendarData.java");
        setInputDefinitionFileName("TN200DayCalendarInput.xml");
        setToolInputParameter("saveResults", "true");
        setToolInputParameter("contextOid", "ctx00000009999");
        setExportFormatFileName("export-format-EXPDATA-TN-CTX.xml");
        setExportJavaSourceFileName("TNStateReportExport.java");
        setStateReportProcedureId("EXPDATA-TN-CTX");
        setImportExportDefinitionId("EXP-TN-CTX");
        setInputJsonBeansFileName("json-input-data-TN-CTX.json");
        setExpectedResultFileName("TN-CTX-expected.csv");

        run();
    }
}
