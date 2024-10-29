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
 * The Class FLExceptionalStudentDataTest.
 */
public class FLExceptionalStudentDataTest extends FLTestBaseClass {

    private final static String FILE_NAME_EXPECTED_PER_2 = "FL-EXCEPT-expected_per_2.txt";
    private final static String FILE_NAME_EXPORT_FORMAT = "export-format-EXPDATA-FL-EXCEPT.xml";
    private final static String FILE_NAME_INPUT_DEFINITION = "FLExceptionalStudentExportInput.xml";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-EXCEPT.json";
    private final static String FILE_NAME_PROCEDURE = "FLExceptionalStudentData.java";

    private final static String ID_EXPORT = "EXP-FL-EXCEPT";
    private final static String ID_PROCEDURE = "EXPDATA-FL-EXCEPT";

    private final static String PARAM_SURVEY_PERIOD = "surveyPeriod";

    // /**
    // * Basic test.
    // *
    // * @throws Exception exception
    // */
    // @Test
    // public void basicTest() throws Exception {
    // super.setFLParameters();
    //
    // setStateReportFileName(FILE_NAME_PROCEDURE);
    // setInputDefinitionFileName(FILE_NAME_INPUT_DEFINITION);
    // setExportFormatFileName(FILE_NAME_EXPORT_FORMAT);
    // setStateReportProcedureId(ID_PROCEDURE);
    // setImportExportDefinitionId(ID_EXPORT);
    // setExpectedResultFileName(FILE_NAME_EXPECTED_PER_5);
    // setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);
    //
    // setToolInputParameter(PARAM_SURVEY_PERIOD, PARAM_VALUE_SURVEY_PERIOD_5);
    //
    // run();
    // }

    /**
     * Basic test.
     *
     * @throws Exception exception
     */
    @Test
    public void period3Test() throws Exception {
        super.setFLParameters();
        importBundledFilesByPattern("reftable-FLAlternateAssessmentAdministered.xml");
        importBundledFilesByPattern("reftable-FLExceptionalStudentPlacementStatus.xml");
        importBundledFilesByPattern("reftable-FLExceptionalStudentReferralReason.xml");
        importBundledFilesByPattern("reftable-FLGiftedEligibility.xml");
        importBundledFilesByPattern("reftable-FLIDEADisabilities.xml");
        importBundledFilesByPattern("reftable-FLIDEAEducationalEnvironments.xml");
        importBundledFilesByPattern("reftable-FLExceptionalStudent60-DayExceptionExtension.xml");
        importBundledFilesByPattern("dictionary-FL-PGM-EXCEPT.xml");

        setStateReportFileName(FILE_NAME_PROCEDURE);
        setInputDefinitionFileName(FILE_NAME_INPUT_DEFINITION);
        setExportFormatFileName(FILE_NAME_EXPORT_FORMAT);
        setStateReportProcedureId(ID_PROCEDURE);
        setImportExportDefinitionId(ID_EXPORT);
        setExpectedResultFileName(FILE_NAME_EXPECTED_PER_2);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);

        setToolInputParameter(PARAM_SURVEY_PERIOD, getSurveyPeriodOid("2"));

        run();
    }
}
