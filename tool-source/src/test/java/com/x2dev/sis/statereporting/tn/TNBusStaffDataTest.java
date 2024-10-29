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
 * The Class TNBusStaffDataTest.
 */
// @FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TNBusStaffDataTest extends TNSTestBaseClass {

    /**
     * Run test.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest() throws Exception {
        setTNParameters();
        setStateReportFileName("TNBusStaffData.java");
        setInputDefinitionFileName("TNBusStaffInput.xml");
        setToolInputParameter("saveResults", "true");
        setToolInputParameter("contextOid", "ctx00000009999");
        setExportFormatFileName("export-format-EXPDATA-TN-BST.xml");
        setStateReportProcedureId("EXPDATA-TN-BST");
        setImportExportDefinitionId("EXP-TN-BST");
        setInputJsonBeansFileName("json-input-data-TN-BST.json");
        setExpectedResultFileName("TN-BST-expected.csv");

        run();
    }
}
