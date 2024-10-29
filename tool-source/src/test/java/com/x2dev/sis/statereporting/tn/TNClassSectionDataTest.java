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
 * The Class TNClassSectionDataTest.
 */
// @FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TNClassSectionDataTest extends TNSTestBaseClass {

    /**
     * Run test.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest() throws Exception {
        setTNParameters();
        setStateReportFileName("TNClassSectionData.java");
        setInputDefinitionFileName("TNClassSectionExportInput.xml");
        setToolInputParameter("saveResults", "true");
        setToolInputParameter("contextOid", "ctx00000009999");
        /**
         * Can run for school context if needed. JSON configured properly for that.
         * Then you will need to remove/comment row with schoolId = 20 from result string.
         */
        // setToolInputParameter("schoolOid", "skl01Primary");

        setExportFormatFileName("export-format-EXPDATA-TN-MST.xml");
        setStateReportProcedureId("EXPDATA-TN-MST");
        setImportExportDefinitionId("EXP-TN-MST");
        setInputJsonBeansFileName("json-input-data-TN-MST.json");
        setExpectedResultFileName("TN-MST-expected.csv");

        run();
    }
}
