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
 * The Class FLStudentAdditionalFundingDataTest.
 */
public class FLStudentAdditionalFundingDataTest extends FLTestBaseClass {

    private final static String FILE_NAME_ASSESSMENT_DEFINITION_AICE = "assessment-definition-AICE.xml";
    private final static String FILE_NAME_ASSESSMENT_DEFINITION_AP = "assessment-definition-AP.xml";
    private final static String FILE_NAME_ASSESSMENT_DEFINITION_IB = "assessment-definition-IB.xml";
    private final static String FILE_NAME_EXPECTED = "FL-FTE-expected.txt";
    private final static String FILE_NAME_EXPORT_FORMAT = "export-format-EXPDATA-FL-FTE.xml";
    private final static String FILE_NAME_INPUT_DEFINITION = "FLStudentAdditFundExportInput.xml";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-FTE.json";
    private final static String FILE_NAME_PROCEDURE = "FLStudentAdditionalFundingData.java";

    private final static String ID_EXPORT = "EXP-FL-FTE";
    private final static String ID_PROCEDURE = "EXPDATA-FL-FTE";

    private final static String PARAM_SURVEY_PERIOD = "surveyPeriod";

    /**
     * Basic test.
     *
     * @throws Exception exception
     */
    @Test
    public void basicTest() throws Exception {
        super.setFLParameters();

        importBundledFilesByPattern("reftable-FLEarlyGraduate.xml");

        setStateReportFileName(FILE_NAME_PROCEDURE);
        setInputDefinitionFileName(FILE_NAME_INPUT_DEFINITION);
        setExportFormatFileName(FILE_NAME_EXPORT_FORMAT);
        setStateReportProcedureId(ID_PROCEDURE);
        setImportExportDefinitionId(ID_EXPORT);
        setExpectedResultFileName(FILE_NAME_EXPECTED);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);

        setToolInputParameter(PARAM_SURVEY_PERIOD, getSurveyPeriodOid("5"));

        importBundledFilesByPattern(FILE_NAME_ASSESSMENT_DEFINITION_AICE);
        importBundledFilesByPattern(FILE_NAME_ASSESSMENT_DEFINITION_AP);
        importBundledFilesByPattern(FILE_NAME_ASSESSMENT_DEFINITION_IB);

        run();
    }
}
