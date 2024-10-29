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
 * The Class FLWdisAgeStudentCourseDataTest.
 */
public class FLWdisAgeStudentCourseDataTest extends FLWdisTest {
    private final static String SPECIFIC_SETUP_JSON = "{'options':{'seed':'123'},"
            + "'beans':["
            + "{"
            + "'type':'Organization',"
            + "'id' : 'org001',"
            + "'fields':{"
            + "    'all-org-DistrictNumber':'26',"
            + "    'name':'Test Organization'"
            + "}"
            + "},"
            + "{"
            + "'type':'districtschoolyearcontext',"
            + "'id':'" + TEST_CONTEXT_ID + "',"
            + "'fields':{"
            + "    'schoolYear':'2017',"
            + "    'startDate':'2016-10-01',"
            + "    'endDate':'2017-06-30',"
            + "    'organization1Oid':{'id':'org001'}"
            + "}"
            + "}]}";

    private final static String FILE_NAME_EXPECTED = "FL-W-AGE-expected.txt";
    private final static String FILE_NAME_EXPORT_FORMAT = "export-format-EXPDATA-FL-W-AGE.xml";
    private final static String FILE_NAME_INPUT_DEFINITION = "FLWdisAgeStudentCourseExportInput.xml";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-W-AGE.json";
    private final static String FILE_NAME_PROCEDURE = "FLWdisAgeStudentCourseData.java";

    private final static String ID_EXPORT = "EXP-FL-W-AGE";
    private final static String ID_PROCEDURE = "EXPDATA-FL-W-AGE";

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
        importBundledFilesByPattern("dictionary.*CPC.*.xml");
        importBundledFilesByPattern("dictionary.*AGE.xml");
        importBundledFilesByPattern("reftable-FLCourseCodes.xml");
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

    /**
     * @see com.x2dev.sis.statereporting.fl.FLWdisTest#performStateSpecificSetup()
     */
    @Override
    protected void performStateSpecificSetup() {
        setSpecificSetupJson(SPECIFIC_SETUP_JSON);
        super.performStateSpecificSetup();
    }
}
