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

import com.follett.fsc.core.k12.test.CSVComparator;
import com.follett.fsc.core.k12.test.X2StateReportComparator;
import java.io.File;
import org.junit.Test;

/**
 * The Class FLCompensatoryProjectEvaluationData.
 */
public class OnsisValidationFileOenImportTest extends OnSISTestBaseClass {
    private final static String FILE_NAME_EXPECTED = "expected_ON_SIS_oen_validation_file_import.txt";
    private final static String FILE_NAME_EXPORT_JAVASOURCE = "OnsisValidationFileOenImport.java";
    private final static String FILE_NAME_INPUT_DEFINITION = "OnsisValidationFileOenImportInput.xml";
    private final static String FILE_NAME_JSON_INPUT_DATA = "input_ON_SIS_oen_validation_file.json";
    private final static String FILE_NAME_TO_IMPORT = "ValidationFileOen_test_output.xml";
    private final static String ID_IMPORT = "ONSIS-VAL-OEN-IMP";

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
        setExpectedResultFileName(FILE_NAME_EXPECTED);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);
        run();
    }

    /**
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#getComparator()
     */
    @Override
    protected X2StateReportComparator getComparator() {
        return new CSVComparator(null);
    }

    /**
     * @see com.x2dev.sis.statereporting.on.OnSISTestBaseClass#getFileToImport()
     */
    @Override
    protected File getFileToImport() {
        return getFilesByPattern(FILE_NAME_TO_IMPORT, getResourcesPath())[0];
    }

    /**
     * @see com.x2dev.sis.statereporting.on.OnSISTestBaseClass#getResultFileName()
     */
    @Override
    protected String getResultFileName() {
        return "OnsisExport.xml";
    }
}
