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
 * The Class FLStaffPdDataTest.
 */
public class FLStaffPdDataTest extends FLTestBaseClass {

    private final static String FILE_NAME_EXPECTED = "FL-SPD-expected.txt";
    private final static String FILE_NAME_EXPORT_FORMAT = "export-format-EXPDATA-FL-SPD.xml";
    private final static String FILE_NAME_INPUT_DEFINITION = "FLStaffPdInput.xml";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-SPD.json";
    private final static String FILE_NAME_PROCEDURE = "FLStaffPdData.java";

    private final static String ID_EXPORT = "EXP-FL-SPD";
    private final static String ID_PROCEDURE = "EXPDATA-FL-SPD";

    private final static String PARAM_SURVEY_PERIOD = "surveyPeriod";

    private final static String SPECIFIC_SETUP_JSON = "{'options':{'seed':'123'},"
            + "'beans':[{"
            + "'type':'districtschoolyearcontext',"
            + "'id':'" + TEST_CONTEXT_ID + "',"
            + "'fields':{"
            + "    'oid':'ctx00000002017',"
            + "    'schoolYear':'2016',"
            + "    'startDate':'2016-10-01',"
            + "    'endDate':'2017-06-30'"
            + "}"
            + "}]}";

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

        setToolInputParameter(PARAM_SURVEY_PERIOD, getSurveyPeriodOid("5"));

        run();
    }

    /**
     * @see com.x2dev.sis.statereporting.fl.FLTestBaseClass#performStateSpecificSetup()
     */
    @Override
    protected void performStateSpecificSetup() {
        setSpecificSetupJson(SPECIFIC_SETUP_JSON);
        super.performStateSpecificSetup();
    }
}
