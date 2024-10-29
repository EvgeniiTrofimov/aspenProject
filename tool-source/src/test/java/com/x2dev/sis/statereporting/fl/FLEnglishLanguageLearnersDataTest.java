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
 * The Class FLEnglishLanguageLearnersDataTest.
 */
public class FLEnglishLanguageLearnersDataTest extends FLTestBaseClass {

    private final static String FILE_NAME_EXPECTED_PER_5 = "FL-ELL-expected_per_5.txt";
    private final static String FILE_NAME_EXPORT_FORMAT = "export-format-EXPDATA-FL-ELL.xml";
    private final static String FILE_NAME_INPUT_DEFINITION = "FLEnglishLanguageLearnersExportInput.xml";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-ELL.json";
    private final static String FILE_NAME_PROCEDURE = "FLEnglishLanguageLearnersData.java";

    private final static String ID_EXPORT = "EXP-FL-ELL";
    private final static String ID_PROCEDURE = "EXPDATA-FL-ELL";

    private final static String PARAM_SURVEY_PERIOD = "surveyPeriod";

    /**
     * Basic test.
     *
     * @throws Exception exception
     */
    @Test
    public void basicTest() throws Exception {
        super.setFLParameters();

        importBundledFilesByPattern("dictionary-FL-PGM-ELL.xml");
        importBundledFilesByPattern("reftable-FLELLTierPlacement.xml");
        importBundledFilesByPattern("reftable-FLTestSubjectContentCode.xml");
        importBundledFilesByPattern("reftable-FLELLBasisOfEntry.xml");
        importBundledFilesByPattern("reftable-FLELLBasisOfExit.xml");
        importBundledFilesByPattern("reftable-FLELLProgramCode.xml");
        importBundledFilesByPattern("reftable-FLELLProgramParticipation.xml");
        importBundledFilesByPattern("reftable-FLELLTestScoreType.xml");

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
}
