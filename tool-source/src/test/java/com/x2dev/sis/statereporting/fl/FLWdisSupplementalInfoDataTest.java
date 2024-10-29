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
 * The Class FLWdisSupplementalInfoDataTest.
 */
public class FLWdisSupplementalInfoDataTest extends FLWdisTest {
    private final static String FILE_NAME_EXPECTED = "FL-W-SUP-expected.txt";
    private final static String FILE_NAME_EXPORT_FORMAT = "export-format-EXPDATA-FL-W-SUP.xml";
    private final static String FILE_NAME_INPUT_DEFINITION = "FLWdisSupplementalInfoInput.xml";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-W-SUP.json";
    private final static String FILE_NAME_PROCEDURE = "FLWdisSupplementalInfoData.java";

    private final static String ID_EXPORT = "EXP-FL-W-SUP";
    private final static String ID_PROCEDURE = "EXPDATA-FL-W-SUP";

    private final static String PARAM_SURVEY_PERIOD = "surveyPeriod";

    /**
     * Basic test.
     *
     * @throws Exception exception
     */
    @Test
    public void basicTest() throws Exception {
        initialize();

        importBundledFilesByPattern("reftable-FLWdisProgramsTotalHrs.xml");
        importBundledFilesByPattern("reftable-FLCapeIndustryCertification.xml");
        importBundledFilesByPattern("reftable-FLCapeIndustryCertificationId.xml");
        importBundledFilesByPattern("dictionary.*AGE.xml");
        importBundledFilesByPattern("dictionary.*CTE.xml");
        importBundledFilesByPattern("reftable-FLWdisYears.xml");

        setStateReportFileName(FILE_NAME_PROCEDURE);
        setInputDefinitionFileName(FILE_NAME_INPUT_DEFINITION);
        setExportFormatFileName(FILE_NAME_EXPORT_FORMAT);
        setStateReportProcedureId(ID_PROCEDURE);
        setImportExportDefinitionId(ID_EXPORT);
        setExpectedResultFileName(FILE_NAME_EXPECTED);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);

        setToolInputParameter(PARAM_SURVEY_PERIOD, getSurveyPeriodOid("F"));

        run();
    }
}
