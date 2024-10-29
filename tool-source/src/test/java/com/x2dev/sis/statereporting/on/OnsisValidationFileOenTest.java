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

import com.follett.fsc.core.k12.tools.Tool;
import org.junit.Test;

/**
 * The Class FLCompensatoryProjectEvaluationData.
 */
public class OnsisValidationFileOenTest extends OnSISTestBaseClass {
    private final static String FILE_NAME_EXPECTED = "expected_ON_SIS_oen_validation_file.xml";
    private final static String FILE_NAME_JSON_INPUT_DATA = "input_ON_SIS_oen_validation_file.json";
    private static final String FILE_NAME_OUTPUT = "OEN_validation_file.xml";

    private final static String ID_OEN = "ONSIS-VAL-OEN";
    private final static String ID_OEN_DATA = "ONSIS-VAL-OEN-DATA";
    private final static String ID_OEN_DATA_BM = "ONSIS-VAL-OEN-BM";
    private final static String INPUT_DEFINITION_OEN = "OnsisValidationFileOenInput.xml";
    private final static String INPUT_PARAMETER_SCHOOL_OID = "schoolOids";
    private final static String SOURCE_OEN = "OnsisValidationFileOen.java";
    private final static String SOURCE_OEN_DATA = "OnsisValidationFileOenData.java";

    private final static String TEST_REPORT_DATE = "04/01/2007";
    private final static String TEST_SCHOOL_OID = "skl00000000001,skl00000000002";

    /**
     * Basic test.
     *
     * @throws Exception exception
     */
    @Test
    public void basicTest() throws Exception {
        super.setONParameters();
        setExpectedResultFileName(FILE_NAME_EXPECTED);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);

        addProcedureToImport(SOURCE_OEN_DATA, ID_OEN_DATA);
        addProcedureToImport(SOURCE_OEN_DATA, ID_OEN_DATA_BM);
        addProcedureToImport(SOURCE_OEN, INPUT_DEFINITION_OEN, ID_OEN);

        setToolInputParameter(INPUT_PARAMETER_SCHOOL_OID, TEST_SCHOOL_OID);
        setToolInputParameter(INPUT_PARAM_REPORT_DATE, TEST_REPORT_DATE);

        run();
    }

    /**
     * @see com.x2dev.sis.statereporting.on.OnSISTestBaseClass#getResultFileName()
     */
    @Override
    protected String getResultFileName() {
        return FILE_NAME_OUTPUT;
    }

    /**
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#getToolToRun()
     */
    @Override
    protected Tool getToolToRun() {
        return getToolById(ID_OEN);
    }
}
