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
package com.x2dev.sis.statereporting.pa;

import org.junit.Test;

/**
 * The Class PAIncidentActionTest.
 */
// @FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class PAIncidentActionTest extends PATestBaseClass {

    /**
     * Run test.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest() throws Exception {
        setStateReportFileName("PAIncidentAction.java");
        setInputDefinitionFileName("PAIncidentActionInput.xml");
        setExportFormatFileName("export-format-EXPDATA-PA-ACT.xml");
        setPackageName("com.x2dev.procedures.statereporting.pa");
        setStateReportProcedureId("EXPDATA-PA-ACT");
        setImportExportDefinitionId("EXP-PA-ACT");
        setExportJavaSourceFileName("PimsStateReportExport.java");
        setInputJsonBeansFileName("json-input-data-PA-ACT.json");
        setExpectedResultFileName("ResultsFile_PA_ACT.tab");
        setCustomStateReportResultFileName("custom_2015.csv");
        setAliasDefinitionSpreadsheet("PA_Alias_Definition.xls");
        setToolInputParameters();

        run();
    }

    /**
     * Sets the tool input parameters.
     */
    protected void setToolInputParameters() {
        // setToolInputParameter("startDate", "12/29/2015");
        setToolInputParameter("startDate", "null"); // override default value
        setToolInputParameter("endDate", "null"); // override default value
        setToolInputParameter("queryBy1", "##all");
        setToolInputParameter("queryString1", "");
        setToolInputParameter("sortBy", "yog");
    }
}
