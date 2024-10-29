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
 * The Class FLFasterTest09.
 */
public class FLFasterTest09 extends FLFasterRecordTest {
    private final static String FILE_NAME_EXPECTED = "FL-FST-09-expected.txt";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-FST-09.json";

    @Test
    public void creationProcedureTest() throws Exception {
        super.setFLParameters();

        importBundledFilesByPattern("reftable-FLActionCodes.xml");
        importBundledFilesByPattern("reftable-FLConductIncidentType.xml");

        importBundledFilesByPattern("export-format-EXPDATA-FL-FST-09.*");

        setExpectedResultFileName(FILE_NAME_EXPECTED);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);

        setToolInputParameter(INPUT_PARAM_RESTRICT_BY_RECORD_TYPE, RECORD_TYPE_09);

        run();
    }
}