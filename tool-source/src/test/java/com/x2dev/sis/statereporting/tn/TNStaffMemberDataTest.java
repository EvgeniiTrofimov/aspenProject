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
 * The Class TNStaffMemberDataTest.
 */
// @FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TNStaffMemberDataTest extends TNSTestBaseClass {

    /**
     * Run test.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest() throws Exception {
        setTNParameters();
        setStateReportFileName("TNStaffMemberData.java");
        setInputDefinitionFileName("TNStaffMemberExportInput.xml");
        setToolInputParameter("saveResults", "true");
        setToolInputParameter("contextOid", "ctx00000009999");
        setExportFormatFileName("export-format-EXPDATA-TN-STF.xml");
        setStateReportProcedureId("EXPDATA-TN-STF");
        setImportExportDefinitionId("EXP-TN-STF");
        setInputJsonBeansFileName("json-input-data-TN-STF.json");
        setExpectedResultFileName("TN-STF-expected.csv");

        run();
    }
}
