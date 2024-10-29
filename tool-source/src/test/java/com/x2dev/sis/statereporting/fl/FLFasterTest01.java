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
 * The Class FLFasterTest01.
 */
public class FLFasterTest01 extends FLFasterRecordTest {
    private final static String FILE_NAME_EXPECTED = "FL-FST-01-expected.txt";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-FST-01.json";

    @Test
    public void creationProcedureTest() throws Exception {
        super.setFLParameters();

        importBundledFilesByPattern("reftable-FLMigrantStatus.xml");
        importBundledFilesByPattern("reftable-FLBirthCountryCodes.xml");
        importBundledFilesByPattern("reftable-FLBirthPlaceCodes.xml");
        importBundledFilesByPattern("reftable-FLMigrantContinuationofServices.xml");
        importBundledFilesByPattern("reftable-FLLocalSubjectAreaRequirements.xml");
        importBundledFilesByPattern("reftable-FLLanguageCode.xml");
        importBundledFilesByPattern("reftable-FLGradReqBasis.xml");

        importBundledFilesByPattern("dictionary-FL-PGM-MFS.xml");
        importBundledFilesByPattern("dictionary-FL-PGM-EXCEPT.xml");

        importBundledFilesByPattern("dictionary-HSC-HEARING.xml");
        importBundledFilesByPattern("dictionary-HSC-VISION.xml");

        importBundledFilesByPattern("export-format-EXPDATA-FL-FST-01.*");

        setExpectedResultFileName(FILE_NAME_EXPECTED);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);

        setToolInputParameter(INPUT_PARAM_RESTRICT_BY_RECORD_TYPE, RECORD_TYPE_01);

        run();
    }
}
