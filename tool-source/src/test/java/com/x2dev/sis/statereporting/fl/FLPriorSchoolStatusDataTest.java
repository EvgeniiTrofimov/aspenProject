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
 * The Class FLPriorSchoolStatusDataTest.
 */
public class FLPriorSchoolStatusDataTest extends FLTestBaseClass {

    private final static String FILE_NAME_EXPECTED_PER_3 = "FL-ENR-expected_per_3.txt";
    private final static String FILE_NAME_EXPECTED_PER_5 = "FL-ENR-expected_per_5.txt";
    private final static String FILE_NAME_EXPORT_FORMAT = "export-format-EXPDATA-FL-ENR.xml";
    private final static String FILE_NAME_INPUT_DEFINITION = "FLPriorSchoolStatusExportInput.xml";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-ENR.json";
    private final static String FILE_NAME_PROCEDURE = "FLPriorSchoolStatusData.java";

    private final static String ID_EXPORT = "EXP-FL-ENR";
    private final static String ID_PROCEDURE = "EXPDATA-FL-ENR";


    private final static String PARAM_SURVEY_PERIOD = "surveyPeriod";

    /**
     * Basic test.
     *
     * @throws Exception exception
     */
    @Test
    public void basicTest() throws Exception {
        super.setFLParameters();

        importBundledFilesByPattern("reftable-FLScheduleTermCodes.xml");
        importBundledFilesByPattern("reftable-FLEnrollmentCodes.xml");
        importBundledFilesByPattern("reftable-FLCountryCodes.xml");
        importBundledFilesByPattern("reftable-FLDistrictCountyCodes.xml");
        importBundledFilesByPattern("reftable-FLStateTerritoryCodes.xml");

        setStateReportFileName(FILE_NAME_PROCEDURE);
        setInputDefinitionFileName(FILE_NAME_INPUT_DEFINITION);
        setExportFormatFileName(FILE_NAME_EXPORT_FORMAT);
        setStateReportProcedureId(ID_PROCEDURE);
        setImportExportDefinitionId(ID_EXPORT);
        setExpectedResultFileName(FILE_NAME_EXPECTED_PER_5);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);

        setToolInputParameter(PARAM_SURVEY_PERIOD, getSurveyPeriodOid("5"));

        run();
    }

    /**
     * Basic test.
     *
     * @throws Exception exception
     */
    @Test
    public void period3Test() throws Exception {
        super.setFLParameters();

        importBundledFilesByPattern("reftable-FLScheduleTermCodes.xml");
        importBundledFilesByPattern("reftable-FLEnrollmentCodes.xml");
        importBundledFilesByPattern("reftable-FLCountryCodes.xml");
        importBundledFilesByPattern("reftable-FLDistrictCountyCodes.xml");
        importBundledFilesByPattern("reftable-FLStateTerritoryCodes.xml");

        setStateReportFileName(FILE_NAME_PROCEDURE);
        setInputDefinitionFileName(FILE_NAME_INPUT_DEFINITION);
        setExportFormatFileName(FILE_NAME_EXPORT_FORMAT);
        setStateReportProcedureId(ID_PROCEDURE);
        setImportExportDefinitionId(ID_EXPORT);
        setExpectedResultFileName(FILE_NAME_EXPECTED_PER_3);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);

        setToolInputParameter(PARAM_SURVEY_PERIOD, getSurveyPeriodOid("3"));

        run();
    }
}
