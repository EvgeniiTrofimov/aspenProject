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
 * The Class FLIndicatorStatusDataTest.
 */
public class FLIndicatorStatusDataTest extends FLTestBaseClass {

    private final static String FILE_NAME_EXPECTED_PER_3 = "FL-FED-expected_per_3.txt";
    private final static String FILE_NAME_EXPECTED_PER_5 = "FL-FED-expected_per_5.txt";
    private final static String FILE_NAME_EXPORT_FORMAT = "export-format-EXPDATA-FL-FED.xml";
    private final static String FILE_NAME_INPUT_DEFINITION = "FLIndicatorStatusExportInput.xml";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-FED.json";
    private final static String FILE_NAME_PROCEDURE = "FLIndicatorStatusData.java";
    private final static String FILE_NAME_REF_TABLE_CND_INC = "reftable-FLConductIncidentType.xml";

    private final static String ID_EXPORT = "EXP-FL-FED";
    private final static String ID_PROCEDURE = "EXPDATA-FL-FED";

    private final static String PARAM_SURVEY_PERIOD = "surveyPeriod";

    /**
     * Basic test.
     *
     * @throws Exception exception
     */
    @Test
    public void basicTest() throws Exception {
        super.setFLParameters();

        cleanField("cndIncident");

        importBundledFilesByPattern("reftable-FLConductIncident.*");
        importBundledFilesByPattern("reftable-FLHomelessCode.xml");
        importBundledFilesByPattern("reftable-FLHomelessnessCause.xml");
        importBundledFilesByPattern("reftable-FLCapeIdentifier.xml");
        importBundledFilesByPattern("reftable-FLCapeDistrict.xml");
        importBundledFilesByPattern("reftable-FLMigrantStatus.xml");
        importBundledFilesByPattern("reftable-FLDropoutProgramCode.xml");

        setStateReportFileName(FILE_NAME_PROCEDURE);
        setInputDefinitionFileName(FILE_NAME_INPUT_DEFINITION);
        setExportFormatFileName(FILE_NAME_EXPORT_FORMAT);
        setStateReportProcedureId(ID_PROCEDURE);
        setImportExportDefinitionId(ID_EXPORT);
        setExpectedResultFileName(FILE_NAME_EXPECTED_PER_3);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);
        importBundledFilesByPattern(FILE_NAME_REF_TABLE_CND_INC);

        setToolInputParameter(PARAM_SURVEY_PERIOD, getSurveyPeriodOid("3"));

        run();
    }

    /**
     * Basic test.
     *
     * @throws Exception exception
     */
    @Test
    public void period5Test() throws Exception {
        super.setFLParameters();

        cleanField("cndIncident");

        importBundledFilesByPattern("reftable-FLConductIncident.*");
        importBundledFilesByPattern("reftable-FLHomelessCode.xml");
        importBundledFilesByPattern("reftable-FLHomelessnessCause.xml");
        importBundledFilesByPattern("reftable-FLCapeIdentifier.xml");
        importBundledFilesByPattern("reftable-FLCapeDistrict.xml");
        importBundledFilesByPattern("reftable-FLMigrantStatus.xml");
        importBundledFilesByPattern("reftable-FLDropoutProgramCode.xml");

        setStateReportFileName(FILE_NAME_PROCEDURE);
        setInputDefinitionFileName(FILE_NAME_INPUT_DEFINITION);
        setExportFormatFileName(FILE_NAME_EXPORT_FORMAT);
        setStateReportProcedureId(ID_PROCEDURE);
        setImportExportDefinitionId(ID_EXPORT);
        setExpectedResultFileName(FILE_NAME_EXPECTED_PER_5);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);
        importBundledFilesByPattern(FILE_NAME_REF_TABLE_CND_INC);

        setToolInputParameter(PARAM_SURVEY_PERIOD, getSurveyPeriodOid("5"));

        run();
    }
}
