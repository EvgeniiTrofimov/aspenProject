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
package com.x2dev.sis.statereporting.fl;

import org.junit.Test;

/**
 * @author Follett Software Company
 * @copyright 2017
 */
public class FLStaffDemographicExportTest extends FLTestBaseClass {

    private final static String SPECIFIC_SETUP_JSON = "{'options':{'seed':'123'},"
            + "'beans':[{"
            + "'type':'districtschoolyearcontext',"
            + "'id':'" + TEST_CONTEXT_ID + "',"
            + "'fields':{"
            + "    'oid':'ctx00000009999',"
            + "    'schoolYear':'2017',"
            + "    'startDate':'2016-10-01',"
            + "    'endDate':'2017-06-30'"
            + "}"
            + "}]}";

    @Test
    public void runTest() throws Exception {
        setFLParameters();

        setSpecificSetupJson(SPECIFIC_SETUP_JSON);

        setStateReportFileName("FLStaffExtractData.java");
        setInputDefinitionFileName("FLStaffExtractExportInput.xml");
        setToolInputParameter("saveResults", "true");
        setToolInputParameter("schoolYearContext", "ctx00000009999");
        setToolInputParameter("surveyPeriod", getSurveyPeriodOid("1"));

        setExportFormatFileName("export-format-EXPDATA-FL-STF.xml");

        setStateReportProcedureId("EXPDATA-FL-STF");
        setImportExportDefinitionId("EXP-FL-STF");

        setExpectedResultFileName("FL-STF-expected.txt");

        setInputJsonBeansFileName("json-input-data-FL-STF.json");


        run();
    }
}
