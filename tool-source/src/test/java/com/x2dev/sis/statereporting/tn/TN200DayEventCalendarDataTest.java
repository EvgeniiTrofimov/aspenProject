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
 * The Class TN200DayEventCalendarDataTest.
 */
// @FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TN200DayEventCalendarDataTest extends TNSTestBaseClass {

    /**
     * Run test.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest() throws Exception {
        setTNParameters();
        setStateReportFileName("TN200DayEventCalendarData.java");
        setInputDefinitionFileName("TN200DayEventCalendarInput.xml");
        setToolInputParameter("saveResults", "true");
        setToolInputParameter("contextOid", "ctx00000009999");
        setExportFormatFileName("export-format-EXPDATA-TN-CAL.xml");
        setStateReportProcedureId("EXPDATA-TN-CAL");
        setImportExportDefinitionId("EXP-TN-CAL");
        setInputJsonBeansFileName("json-input-data-TN-CAL.json");
        setExpectedResultFileName("TN-CAL-expected.csv");

        run();
    }
}
