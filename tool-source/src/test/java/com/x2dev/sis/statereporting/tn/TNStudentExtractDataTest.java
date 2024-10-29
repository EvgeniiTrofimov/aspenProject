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

// @FixMethodOrder(MethodSorters.NAME_ASCENDING)

/**
 * The Class TNStudentExtractDataTest.
 */
/*
 * test failed because:
 * <field position="150" name="SUFFIX"> try get data form state code, but state code always empty.
 * Live site has this behavior too.
 */
public class TNStudentExtractDataTest extends TNSTestBaseClass {

    /**
     * Run test.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest() throws Exception {
        setTNParameters();
        setStateReportFileName("TNStudentExtractData.java");
        setInputDefinitionFileName("TNStudentExportInput.xml");
        setToolInputParameter("saveResults", "true");
        setToolInputParameter("contextOid", "ctx00000009998");
        setExportFormatFileName("export-format-EXPDATA-TN-STD.xml");
        setStateReportProcedureId("EXPDATA-TN-STD");
        setImportExportDefinitionId("EXP-TN-STD");
        setInputJsonBeansFileName("json-input-data-TN-STD.json");
        setExpectedResultFileName("TN-STD-expected.csv");

        run();
    }


    /**
     * Run test 2.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest2() throws Exception {
        setStateReportFileName("TNStudentExtractData.java");

        setInputDefinitionFileName("TNStudentExportInput.xml");
        setToolInputParameter("saveResults", "true");
        setToolInputParameter("contextOid", "ctx00000009999");

        setToolInputParameter("suppressHeading", "true");
        setToolInputParameter("suppressTrailer", "true");

        setExportFormatFileName("export-format-EXPDATA-TN-STD-V5.xml");
        setPackageName("com.x2dev.procedures.statereporting.tn");
        setStateReportProcedureId("EXPDATA-TN-STD");
        setImportExportDefinitionId("EXP-TN-STD");
        setExportJavaSourceFileName("TNStateReportExport.java");
        setInputJsonBeansFileName("json-input-data-TN-STDV5.json");
        setExpectedResultFileName("TN-STD-expectedV5.csv");
        setAliasDefinitionSpreadsheet("TN State Reporting Alias Definition.csv");

        run();
    }

}
