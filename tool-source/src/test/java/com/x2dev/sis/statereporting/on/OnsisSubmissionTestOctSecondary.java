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
package com.x2dev.sis.statereporting.on;

import org.junit.Test;

/**
 * The Class FLCompensatoryProjectEvaluationData.
 */
public class OnsisSubmissionTestOctSecondary extends OnSISTestBaseClass {
    private final static String FILE_CSV_PATTERN = "EXSMS-OCT-SEC-.*";
    private final static String FILE_NAME_EXPECTED = "expected_ON_SIS_oct_secondary.xml";
    private final static String FILE_NAME_EXPORT_JAVASOURCE = "OnsisPublishAll.java";
    private final static String FILE_NAME_INPUT_DEFINITION = "OnsisPublishAllInput.xml";
    private final static String FILE_NAME_JSON_INPUT_DATA = "input_ON_SIS_oct_secondary.json";
    private final static String ID_IMPORT = "ONSIS-EXP-ALL";
    private final static String INPUT_PARAMETER_DO_VALIDATE = "doValidate";
    private final static String INPUT_PARAMETER_SCHOOL_OID = "schoolOids";
    private final static String INPUT_PARAMETER_SUBMISSION_TYPE = "submissionType";
    private final static String TEST_REPORT_DATE = "04/04/2007";
    private final static String TEST_SCHOOL_OID = "skl00000000001,skl00000000002";
    private final static String TEST_SUBMISSION_TYPE = "subType0000001";

    /**
     * Basic test.
     *
     * @throws Exception exception
     */
    @Test
    public void basicTest() throws Exception {
        super.setONParameters();
        setImportExportDefinitionId(ID_IMPORT);
        setExportJavaSourceFileName(FILE_NAME_EXPORT_JAVASOURCE);
        setInputDefinitionFileName(FILE_NAME_INPUT_DEFINITION);
        setToolInputParameter(INPUT_PARAMETER_SCHOOL_OID, TEST_SCHOOL_OID);
        setToolInputParameter(INPUT_PARAMETER_SUBMISSION_TYPE, TEST_SUBMISSION_TYPE);
        setToolInputParameter(INPUT_PARAMETER_DO_VALIDATE, Boolean.FALSE.toString());
        setToolInputParameter(INPUT_PARAM_REPORT_DATE, TEST_REPORT_DATE);
        setExpectedResultFileName(FILE_NAME_EXPECTED);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);
        run();
    }

    /**
     * @see com.x2dev.sis.statereporting.on.OnSISTestBaseClass#getCsvPattern()
     */
    @Override
    protected String getCsvPattern() {
        return FILE_CSV_PATTERN;
    }

    /**
     * @see com.x2dev.sis.statereporting.on.OnSISTestBaseClass#getResultFileName()
     */
    @Override
    protected String getResultFileName() {
        return "OnsisExport.xml";
    }
}
