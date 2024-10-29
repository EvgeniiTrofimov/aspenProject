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
 * The Class TNStudentAttendanceDataTest.
 */
// @FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TNStudentAttendanceDataTest extends TNSTestBaseClass {

    /**
     * Run test.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest() throws Exception {
        setTNParameters();
        setStateReportFileName("TNStudentAttendanceData.java");
        setInputDefinitionFileName("TNStudentAttendanceExportInput.xml");
        setToolInputParameter("saveResults", "true");
        setToolInputParameter("contextOid", "ctx00000009999");
        setExportFormatFileName("export-format-EXPDATA-TN-ATT.xml");
        setStateReportProcedureId("EXPDATA-TN-ATT");
        setImportExportDefinitionId("EXP-TN-ATT");
        setInputJsonBeansFileName("json-input-data-TN-ATT.json");
        setExpectedResultFileName("TN-ATT-expected.csv");
        run();
    }
}
