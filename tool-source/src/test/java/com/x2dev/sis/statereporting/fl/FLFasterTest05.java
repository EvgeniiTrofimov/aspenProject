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
 * The Class FLFasterTest05.
 */
public class FLFasterTest05 extends FLFasterRecordTest {
    private final static String FILE_NAME_EXPECTED = "FL-FST-05-expected.txt";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-FST-05.json";

    @Test
    public void creationProcedureTest() throws Exception {
        super.setFLParameters();

        importBundledFilesByPattern("reftable-FLCourseNumToAbbrevTitle.xml");
        importBundledFilesByPattern("reftable-FLWdisProgramsTotalHrs.xml");
        importBundledFilesByPattern("reftable-FLCteFullPgmCompleterCodes.xml");
        importBundledFilesByPattern("reftable-FLCapeIdentifier.xml");
        importBundledFilesByPattern("reftable-FLCapeIndustryCertification.xml");
        importBundledFilesByPattern("reftable-FLCapeIndustryCertificationId.xml");
        importBundledFilesByPattern("reftable-FLELLProgramCode.xml");
        importBundledFilesByPattern("reftable-FLELLBasisOfEntry.xml");
        importBundledFilesByPattern("reftable-FLELLBasisOfExit.xml");
        importBundledFilesByPattern("reftable-FLDropoutProgramCode.xml");
        importBundledFilesByPattern("reftable-FLDropoutPlacementReasons.xml");
        importBundledFilesByPattern("reftable-FLDropoutOutcomes.xml");

        importBundledFilesByPattern("dictionary-FL-PGM-CTE.xml");
        importBundledFilesByPattern("dictionary-FL-PGM-ELL.xml");
        importBundledFilesByPattern("dictionary-FL-PGM-DROP.xml");

        importBundledFilesByPattern("export-format-EXPDATA-FL-FST-05.*");

        setExpectedResultFileName(FILE_NAME_EXPECTED);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);

        setToolInputParameter(INPUT_PARAM_RESTRICT_BY_RECORD_TYPE, RECORD_TYPE_05);

        run();
    }
}
