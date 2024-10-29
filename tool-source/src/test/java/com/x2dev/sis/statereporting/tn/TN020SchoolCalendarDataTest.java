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
 * The Class TN020SchoolCalendarDataTest.
 */
// @FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TN020SchoolCalendarDataTest extends TNSTestBaseClass {

    /**
     * Run test.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest() throws Exception {
        setTNParameters();
        setStateReportFileName("TNSchoolCalendarData.java");
        setInputDefinitionFileName("TNInstructionalProgramInput.xml");
        setToolInputParameter("saveResults", "true");
        setToolInputParameter("contextOid", "ctx00000009999");
        setExportFormatFileName("export-format-EXPDATA-TN-CAS.xml");
        setStateReportProcedureId("EXPDATA-TN-CAS");
        setImportExportDefinitionId("EXP-TN-CAS");
        setInputJsonBeansFileName("json-input-data-TN-CAS.json");
        setExpectedResultFileName("TN-CAS-expected.csv");
        run();
    }
}
