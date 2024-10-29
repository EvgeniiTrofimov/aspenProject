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
 * The Class TNClassSectionSchTest.
 */
// @FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TNClassSectionSchTest extends TNSTestBaseClass {

    /**
     * Run test.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest() throws Exception {
        setTNParameters();
        setStateReportFileName("TNClassSectionScheduleData.java");
        setInputDefinitionFileName("TNClassSectionScheduleExportInput.xml");
        setToolInputParameter("saveResults", "true");
        setToolInputParameter("contextOid", "ctx00000009999");
        setExportFormatFileName("export-format-EXPDATA-TN-MSTS.xml");
        setStateReportProcedureId("EXPDATA-TN-MSTS");
        setImportExportDefinitionId("EXP-TN-MSTS");
        setInputJsonBeansFileName("json-input-data-TN-MSTS.json");
        setExpectedResultFileName("TN-MSTS-expected.csv");

        run();
    }
}
