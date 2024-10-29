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
 * The Class TNStudentProgramDataTest.
 */
// @FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TNStudentProgramDataTest extends TNSTestBaseClass {

    /**
     * Run test 042.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest042() throws Exception {
        setTNParameters();
        setStateReportProcedureId("EXPDATA-TN-PGMI");
        setImportExportDefinitionId("EXP-TN-PGMI");
        setStateReportFileName("TNStudentProgramData.java");
        setInputDefinitionFileName("TNStudentIneligibilityFundingStatusInput.xml");
        setExportFormatFileName("export-format-EXPDATA-TN-PGMI.xml");
        setInputJsonBeansFileName("json-input-data-TN-PGMI.json");
        setExpectedResultFileName("TN-PGMI-expected.csv");
        setToolInputParameter("saveResults", "true");
        setToolInputParameter("contextOid", "ctx00000009999");

        run();
    }

    /**
     * Run test 044.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest044() throws Exception {
        setPackageName("com.x2dev.procedures.statereporting.tn");
        setStateReportProcedureId("EXPDATA-TN-PGMC");
        setImportExportDefinitionId("EXP-TN-PGMC");
        setStateReportFileName("TNStudentProgramData.java");
        setInputDefinitionFileName("TNStudentClassificationInput.xml");
        setExportFormatFileName("export-format-EXPDATA-TN-PGMC.xml");
        setExportJavaSourceFileName("TNStateReportExport.java");
        setInputJsonBeansFileName("json-input-data-TN-PGMC.json");
        setExpectedResultFileName("TN-PGMC-expected.csv");
        setAliasDefinitionSpreadsheet("TN State Reporting Alias Definition.csv");
        setToolInputParameter("saveResults", "true");
        setToolInputParameter("contextOid", "ctx00000009999");

        setToolInputParameter("suppressHeading", "true");
        setToolInputParameter("suppressTrailer", "true");

        run();
    }

    /**
     * Run test 052.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest052() throws Exception {
        setPackageName("com.x2dev.procedures.statereporting.tn");
        setStateReportProcedureId("EXPDATA-TN-PGMM");
        setImportExportDefinitionId("EXP-TN-PGMM");
        setStateReportFileName("TNStudentProgramData.java");
        setInputDefinitionFileName("TNStudentClubMembershipInput.xml");
        setExportFormatFileName("export-format-EXPDATA-TN-PGMM.xml");
        setExportJavaSourceFileName("TNStateReportExport.java");
        setInputJsonBeansFileName("json-input-data-TN-PGMM.json");
        setExpectedResultFileName("TN-PGMM-expected.csv");
        setAliasDefinitionSpreadsheet("TN State Reporting Alias Definition.csv");
        setToolInputParameter("saveResults", "true");
        setToolInputParameter("contextOid", "ctx00000009999");

        setToolInputParameter("suppressHeading", "true");
        setToolInputParameter("suppressTrailer", "true");

        run();
    }
}
