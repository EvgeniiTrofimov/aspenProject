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
 * The Class TNStudentEnrollmentTest.
 */
// @FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TNStudentEnrollmentTest extends TNSTestBaseClass {

    /**
     * Run test.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest() throws Exception {
        setTNParameters();
        setStateReportFileName("TNStudentEnrollmentData.java");
        setInputDefinitionFileName("TNStudentEnrollmentExportInput.xml");
        setExportFormatFileName("export-format-EXPDATA-TN-ENR.xml");
        setStateReportProcedureId("EXPDATA-TN-ENR");
        setImportExportDefinitionId("EXP-TN-ENR");
        setInputJsonBeansFileName("json-input-data-TN-ENR.json");
        setExpectedResultFileName("TN-ENR-expected.csv");
        setToolInputParameter("saveResults", "true");
        setToolInputParameter("contextOid", "ctx00000009999");
        run();
    }
}
