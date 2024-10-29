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
 * The Class FLCTETeacherCourseDataTest.
 */
public class FLCTETeacherCourseDataTest extends FLTestBaseClass {

    private final static String FILE_NAME_EXPECTED = "FL-CTETC-expected.txt";
    private final static String FILE_NAME_EXPORT_FORMAT = "export-format-EXPDATA-FL-CTETC.xml";
    private final static String FILE_NAME_INPUT_DEFINITION = "FLCTETeacherCourseExportInput.xml";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-CTETC.json";
    private final static String FILE_NAME_PROCEDURE = "FLCTETeacherCourseData.java";

    private final static String ID_EXPORT = "EXP-FL-CTETC";
    private final static String ID_PROCEDURE = "EXPDATA-FL-CTETC";

    private final static String PARAM_SURVEY_PERIOD = "surveyPeriod";

    /**
     * Basic test.
     *
     * @throws Exception exception
     */
    @Test
    public void basicTest() throws Exception {
        super.setFLParameters();

        cleanField("trmTermCode");

        importBundledFilesByPattern("reftable-FLScheduleTermCodes.xml");
        importBundledFilesByPattern("reftable-FLCourseCodes.xml");

        setStateReportFileName(FILE_NAME_PROCEDURE);
        setInputDefinitionFileName(FILE_NAME_INPUT_DEFINITION);
        setExportFormatFileName(FILE_NAME_EXPORT_FORMAT);
        setStateReportProcedureId(ID_PROCEDURE);
        setImportExportDefinitionId(ID_EXPORT);
        setExpectedResultFileName(FILE_NAME_EXPECTED);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);

        setToolInputParameter(PARAM_SURVEY_PERIOD, getSurveyPeriodOid("5"));

        run();
    }
}
