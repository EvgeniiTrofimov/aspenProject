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
 * The Class FLFasterTestG99ATV.
 */
public class FLFasterTestG99ATV extends FLFasterRecordTest {
    private final static String FILE_NAME_EXPECTED = "FL-FST-G99ATV-expected.txt";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-FST-G99ATV.json";

    @Test
    public void creationProcedureTest() throws Exception {
        super.setFLParameters();

        importBundledFilesByPattern("export-format-EXPDATA-FL-FST-99ATV.*");

        setExpectedResultFileName(FILE_NAME_EXPECTED);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);

        setToolInputParameter(INPUT_PARAM_RESTRICT_BY_RECORD_TYPE, RECORD_TYPE_99ATV);

        run();
    }
}