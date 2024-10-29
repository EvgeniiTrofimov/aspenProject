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
 * The Class TN022SchoolCalendarDataTest.
 */
// @FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TN022SchoolCalendarDataTest extends TNSTestBaseClass {

    /**
     * Run test.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest() throws Exception {
        setTNParameters();
        setStateReportFileName("TNSchoolCalendarData.java");
        setInputDefinitionFileName("TNSchoolDaysInput.xml");
        setToolInputParameter("saveResults", "true");
        setToolInputParameter("contextOid", "ctx00000009999");
        setExportFormatFileName("export-format-EXPDATA-TN-CSD.xml");
        setStateReportProcedureId("EXPDATA-TN-CSD");
        setImportExportDefinitionId("EXP-TN-CSD");
        setInputJsonBeansFileName("json-input-data-TN-CSD.json");
        setExpectedResultFileName("TN-CSD-expected.csv");
        run();
    }
}
