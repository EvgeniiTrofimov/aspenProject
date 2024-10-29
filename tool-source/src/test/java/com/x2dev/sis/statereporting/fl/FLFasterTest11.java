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
 * The Class FLFasterTest11.
 */
public class FLFasterTest11 extends FLFasterRecordTest {
    private final static String FILE_NAME_EXPECTED = "FL-FST-11-expected.txt";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-FST-11.json";

    @Test
    public void creationProcedureTest() throws Exception {
        super.setFLParameters();

        importBundledFilesByPattern("reftable-FLCountryCodes.xml");
        importBundledFilesByPattern("reftable-FLStateTerritoryCodes.xml");
        importBundledFilesByPattern("reftable-FLAmericanCountriesCodes.xml");
        importBundledFilesByPattern("reftable-FLCanMexSouthAmericanStateCodes.xml");

        importBundledFilesByPattern("dictionary-FL-PGM-MIGRANT.xml");

        importBundledFilesByPattern("export-format-EXPDATA-FL-FST-11.*");

        setExpectedResultFileName(FILE_NAME_EXPECTED);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);

        setToolInputParameter(INPUT_PARAM_RESTRICT_BY_RECORD_TYPE, RECORD_TYPE_11);

        run();
    }
}
