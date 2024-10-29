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
import com.follett.fsc.core.k12.test.XMLComparator;

/**
 * @author Follett Software Company
 * @copyright 2017
 */
public class FLStudentDemographicExportTest extends FLTestBaseClass {

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

    /**
     * Run FLStudentExtractDataTest test.
     *
     * @throws Exception exception
     */
    @Test
    public void FLStudentExtractDataTest() throws Exception {
        setFLParameters();

        setSpecificSetupJson(SPECIFIC_SETUP_JSON);

        setStateReportFileName("FLStudentExtractData.java");

        setInputDefinitionFileName("FLStudentExtractExportInput.xml");
        setToolInputParameter("saveResults", "true");
        setToolInputParameter("schoolYearContext", "ctx00000009999");
        setToolInputParameter("surveyPeriod", getSurveyPeriodOid("1"));

        setExportFormatFileName("export-format-EXPDATA-FL-STD.xml");

        setStateReportProcedureId("EXPDATA-FL-STD");
        setImportExportDefinitionId("EXP-FL-STD");

        setExpectedResultFileName("FL-STD-expected.txt");

        setInputJsonBeansFileName("json-input-data-FL-STD.json");

        run();
    }

    /**
     * Run FLBulkRunTest test.
     *
     * @throws Exception exception
     */
    // @Test
    public void FLBulkRunTest() throws Exception {
        setComparator(new XMLComparator());

        setPackageName("com.x2dev.procedures.statereporting.fl");

        setStateReportFileName("FLExportConfiguration.java");

        setInputDefinitionFileName("FLBulkRunInput.xml");
        setToolInputParameter("surveyPeriod", "RCD00000000CC1");

        setExportFormatFileName("export-format-EXPDATA-FL-STD.xml");
        setExportJavaSourceFileName("FLBulkRunProcedure.java");
        // setExportJavaSourceFileName("FLStudentExtractData.java");

        addExtendedDictionaryFileName("dictionary-FL-UDA-VAL_ERROR.zip");

        setStateReportProcedureId("EXPDATA-FL-STD");
        // setImportExportDefinitionId("EXP-FL-STD");
        setImportExportDefinitionId("FL-BULK-RUN");

        setExpectedResultFileName("FL-STD-expected.txt");

        setInputJsonBeansFileName("json-input-data-FL-STD.json");
        setAliasDefinitionSpreadsheet("FL_Alias_Definition.csv");

        run();
    }
}
