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
 * The Class FLFasterTest04.
 */
public class FLFasterTest04 extends FLFasterRecordTest {
    private final static String FILE_NAME_EXPECTED = "FL-FST-04-expected.txt";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-FST-04.json";

    @Test
    public void creationProcedureTest() throws Exception {
        super.setFLParameters();

        importBundledFilesByPattern("reftable-FLCourseCodes.xml");
        importBundledFilesByPattern("reftable-FLCourseNumToAbbrevTitle.xml");
        importBundledFilesByPattern("reftable-FLCourseNumToSubAreaReq.xml");
        importBundledFilesByPattern("reftable-FLCourseFlag.xml");
        importBundledFilesByPattern("reftable-FLOnlineCourse.xml");
        importBundledFilesByPattern("reftable-FLWdisProgramsTotalHrs.xml");

        importBundledFilesByPattern("export-format-EXPDATA-FL-FST-04.*");

        setExpectedResultFileName(FILE_NAME_EXPECTED);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);

        setToolInputParameter(INPUT_PARAM_RESTRICT_BY_RECORD_TYPE, RECORD_TYPE_04);

        run();
    }
}
