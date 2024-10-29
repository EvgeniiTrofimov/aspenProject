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
 * The Class TNStudentWithdrawTest.
 */
// @FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TNStudentWithdrawTest extends TNSTestBaseClass {

    /**
     * Run test.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest() throws Exception {
        setTNParameters();
        setStateReportFileName("TNStudentWithdrawData.java");
        setInputDefinitionFileName("TNStudentWithdrawExportInput.xml");
        setExportFormatFileName("export-format-EXPDATA-TN-ENRW.xml");
        setStateReportProcedureId("EXPDATA-TN-ENRW");
        setImportExportDefinitionId("EXP-TN-ENRW");
        setInputJsonBeansFileName("json-input-data-TN-ENRW.json");
        setExpectedResultFileName("TN-ENRW-expected.csv");
        setToolInputParameter("saveResults", "true");
        setToolInputParameter("contextOid", "ctx00000009999");

        run();
    }
}
