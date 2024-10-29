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
 * The Class TNStudentDisciplinaryActionTest.
 */
// @FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TNStudentDisciplinaryActionTest extends TNSTestBaseClass {

    /**
     * Run test.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest() throws Exception {
        setTNParameters();
        setStateReportFileName("TNStudentDisciplineData.java");
        setInputDefinitionFileName("TNStudentDisciplineExportInput.xml");
        setToolInputParameter("saveResults", "true");
        setExportFormatFileName("export-format-EXPDATA-TN-CND.xml");
        setStateReportProcedureId("EXPDATA-TN-CND");
        setImportExportDefinitionId("EXP-TN-CND");
        setInputJsonBeansFileName("json-input-data-TN-CND.json");
        setExpectedResultFileName("TN-CND-expected.csv");
        setToolInputParameter("contextOid", "ctx00000009999");

        run();
    }
}
