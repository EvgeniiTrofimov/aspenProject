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
 * The Class FLStudentCourseTranscriptInfDataTest.
 */
public class FLStudentCourseTranscriptInfDataTest extends FLTestBaseClass {

    private final static String ID_EXPORT = "EXP-FL-SCTI";
    private final static String ID_PROCEDURE = "EXPDATA-FL-SCTI";

    private final static String FILE_NAME_EXPECTED = "FL-SCTI-expected.txt";
    private final static String FILE_NAME_EXPORT_FORMAT = "export-format-EXPDATA-FL-SCTI.xml";
    private final static String FILE_NAME_INPUT_DEFINITION = "FLStudentCourseTranscriptInfExportInput.xml";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-SCTI.json";
    private final static String FILE_NAME_PROCEDURE = "FLStudentCourseTranscriptInfData.java";

    private final static String PARAM_SURVEY_PERIOD = "surveyPeriod";

    private static final String TEST_CONTEXT_ID_2016 = "ctx01";
    private static final String TEST_CONTEXT_ID_2014 = "ctx02";

    private final static String SPECIFIC_SETUP_JSON = "{'options':{'seed':'123'},"
            + "'beans':["
            + "{"
            + "'type':'Organization',"
            + "'fields':{"
            + "    'oid':'0001',"
            + "    'all-org-StateId':'26'"
            + "}"
            + "},"
            + "{"
            + "'type':'districtschoolyearcontext',"
            + "'id':'" + TEST_CONTEXT_ID_2016 + "',"
            + "'fields':{"
            + "    'oid':'ctx00000002017',"
            + "    'schoolYear':'2016',"
            + "    'startDate':'2016-10-01',"
            + "    'endDate':'2017-06-30',"
            + "    'organization1Oid':'0001'"
            + "}"
            + "},"
            + "{"
            + "'type':'districtschoolyearcontext',"
            + "'id':'" + TEST_CONTEXT_ID_2014 + "',"
            + "'fields':{"
            + "    'oid':'ctx00000002014',"
            + "    'schoolYear':'2014',"
            + "    'startDate':'2014-10-01',"
            + "    'endDate':'2015-06-30',"
            + "    'organization1Oid':'0001'"
            + "}"
            + "}"
            + "]}";

    /**
     * Basic test.
     *
     * @throws Exception exception
     */
    @Test
    public void basicTest() throws Exception {
        super.setFLParameters();

        cleanField("crsSchoolLevel");
        cleanField("trmTermCode");

        importBundledFilesByPattern("reftable-FLScheduleTermCodes.xml");
        importBundledFilesByPattern("reftable-FLSchoolLevelCodes.xml");
        importBundledFilesByPattern("reftable-FLCourseStateSubjectAreaRequirements.xml");
        importBundledFilesByPattern("reftable-FLCourseFlag.xml");
        importBundledFilesByPattern("reftable-FLCourseSubstitutedStateSubjectAreaRequirements.xml");
        importBundledFilesByPattern("reftable-FLOnlineCourse.xml");
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

    /**
     * @see com.x2dev.sis.statereporting.fl.FLTestBaseClass#performStateSpecificSetup()
     */
    @Override
    protected void performStateSpecificSetup() {
        setSpecificSetupJson(SPECIFIC_SETUP_JSON);
        super.performStateSpecificSetup();
    }
}
