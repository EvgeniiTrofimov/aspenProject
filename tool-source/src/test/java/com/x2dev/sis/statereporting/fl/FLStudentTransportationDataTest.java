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
 * The Class FLDropoutPreventionProgramDataTest.
 */
public class FLStudentTransportationDataTest extends FLTestBaseClass {

    private final static String FILE_NAME_EXPECTED = "FL-TRN-expected.txt";
    private final static String FILE_NAME_EXPORT_FORMAT = "export-format-EXPDATA-FL-TRN.xml";
    private final static String FILE_NAME_INPUT_DEFINITION = "FLStudentTransportationExportInput.xml";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-TRN.json";
    private final static String FILE_NAME_PROCEDURE = "FLStudentTransportationData.java";

    private final static String ID_EXPORT = "EXP-FL-TRN";
    private final static String ID_PROCEDURE = "EXPDATA-FL-TRN";

    private final static String PARAM_SURVEY_PERIOD = "surveyPeriod";

    /**
     * Basic test.
     *
     * @throws Exception exception
     */
    @Test
    public void basicTest() throws Exception {
        super.setFLParameters();

        importBundledFilesByPattern("reftable-FLTranMembershipCategory.xml");
        importBundledFilesByPattern("reftable-FLVehicleCategory.xml");

        setStateReportFileName(FILE_NAME_PROCEDURE);
        setInputDefinitionFileName(FILE_NAME_INPUT_DEFINITION);
        setExportFormatFileName(FILE_NAME_EXPORT_FORMAT);
        setStateReportProcedureId(ID_PROCEDURE);
        setImportExportDefinitionId(ID_EXPORT);
        setExpectedResultFileName(FILE_NAME_EXPECTED);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);

        setToolInputParameter(PARAM_SURVEY_PERIOD, getSurveyPeriodOid("4"));

        run();
    }
}