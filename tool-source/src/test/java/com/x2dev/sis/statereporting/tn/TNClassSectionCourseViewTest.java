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
 * The Class TNClassSectionCourseViewTest.
 */
// @FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TNClassSectionCourseViewTest extends TNSTestBaseClass {

    /**
     * Run test.
     *
     * @throws Exception exception
     */
    @Test
    public void runTest() throws Exception {
        setTNParameters();
        setToolInputParameter("contextOid", "ctx00000009999");
        setStateReportFileName("TNClassSectionData.java");
        setInputDefinitionFileName("TNClassSectionExportInput.xml");
        setExportFormatFileName("export-format-EXPDATA-TN-MST.xml");
        setStateReportProcedureId("EXPDATA-TN-MST");
        setImportExportDefinitionId("EXP-TN-MST");
        setInputJsonBeansFileName("json-input-data-TN-MST-CRS-VIEW.json");
        setExpectedResultFileName("TN-MST-CRS-VIEW-excepted.csv");

        run();
    }

}
