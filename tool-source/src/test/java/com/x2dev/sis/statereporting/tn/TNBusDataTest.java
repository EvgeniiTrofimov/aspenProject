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
 * The Class TNBusDataTest.
 */
// @FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TNBusDataTest extends TNSTestBaseClass {

    /**
     * Run test.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest() throws Exception {
        setTNParameters();
        importBundledFilesByPattern("dictionary-REF-BUS.*");
        setStateReportFileName("TNBusData.java");
        setInputDefinitionFileName("TNBusInput.xml");
        setToolInputParameter("saveResults", "true");
        setToolInputParameter("contextOid", "ctx00000009999");
        setExportFormatFileName("export-format-EXPDATA-TN-BUS-V2.xml");
        setStateReportProcedureId("EXPDATA-TN-BUS");
        setImportExportDefinitionId("EXP-TN-BUS");
        setInputJsonBeansFileName("json-input-data-TN-BUS-V2.json");
        setExpectedResultFileName("TN-BUS-V2-expected.csv");

        run();
    }
}
