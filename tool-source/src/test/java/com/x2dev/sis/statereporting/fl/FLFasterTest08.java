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
 * The Class FLFasterTest08.
 */
public class FLFasterTest08 extends FLFasterRecordTest {
    private final static String FILE_NAME_EXPECTED = "FL-FST-08-expected.txt";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-FST-08.json";

    @Test
    public void creationProcedureTest() throws Exception {
        super.setFLParameters();

        importBundledFilesByPattern("assessment-definition-TEST.xml");

        importBundledFilesByPattern("reftable-FLTestCode.xml");
        importBundledFilesByPattern("reftable-FLTestScoreTypes.xml");
        importBundledFilesByPattern("reftable-FLTestSubjectContentCode.xml");

        importBundledFilesByPattern("export-format-EXPDATA-FL-FST-08.*");

        setExpectedResultFileName(FILE_NAME_EXPECTED);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);

        setToolInputParameter(INPUT_PARAM_RESTRICT_BY_RECORD_TYPE, RECORD_TYPE_08);

        run();
    }
}
