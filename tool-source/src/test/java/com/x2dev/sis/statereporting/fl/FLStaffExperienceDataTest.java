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
 * The Class FLStaffExperienceDataTest.
 */
public class FLStaffExperienceDataTest extends FLTestBaseClass {

    private final static String FILE_NAME_EXPECTED = "FL-SXP-expected.txt";
    private final static String FILE_NAME_EXPORT_FORMAT = "export-format-EXPDATA-FL-SXP.xml";
    private final static String FILE_NAME_INPUT_DEFINITION = "FLStaffExperienceInput.xml";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-SXP.json";
    private final static String FILE_NAME_PROCEDURE = "FLStaffExperienceData.java";

    private final static String ID_EXPORT = "EXP-FL-SXP";
    private final static String ID_PROCEDURE = "EXPDATA-FL-SXP";

    private final static String PARAM_SURVEY_PERIOD = "surveyPeriod";

    /**
     * Basic test.
     *
     * @throws Exception exception
     */
    @Test
    public void basicTest() throws Exception {
        super.setFLParameters();

        setStateReportFileName(FILE_NAME_PROCEDURE);
        setInputDefinitionFileName(FILE_NAME_INPUT_DEFINITION);
        setExportFormatFileName(FILE_NAME_EXPORT_FORMAT);
        setStateReportProcedureId(ID_PROCEDURE);
        setImportExportDefinitionId(ID_EXPORT);
        setExpectedResultFileName(FILE_NAME_EXPECTED);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);

        setToolInputParameter(PARAM_SURVEY_PERIOD, getSurveyPeriodOid("3"));

        run();
    }
}
