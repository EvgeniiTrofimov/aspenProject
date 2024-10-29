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
import com.x2dev.sis.model.beans.GradePointAverageDefinition;
import com.x2dev.utils.converters.BooleanAsStringConverter;

public class FLStudentEndOfYearStatusDataTest extends FLTestBaseClass {
    private final static String ALIAS_STATE_FIELD_INDICATOR = "all-gpd-StateIndicator";

    private static final String GRADE_POINT_DEFINITION_OID = "gpdHq00000wGPA";

    private final static String FILE_NAME_EXPECTED = "FL-SEYS-expected.txt";
    private final static String FILE_NAME_EXPORT_FORMAT = "export-format-EXPDATA-FL-SEYS.xml";
    private final static String FILE_NAME_INPUT_DEFINITION = "FLStudentEndOfYearStatusExportInput.xml";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-SEYS.json";
    private final static String FILE_NAME_PROCEDURE = "FLStudentEndOfYearStatusData.java";

    private final static String ID_EXPORT = "EXP-FL-SEYS";
    private final static String ID_PROCEDURE = "EXPDATA-FL-SEYS";

    private final static String PARAM_SURVEY_PERIOD = "surveyPeriod";

    /**
     * Basic test.
     *
     * @throws Exception exception
     */
    @Test
    public void basicTest() throws Exception {
        super.setFLParameters();

        importBundledFilesByPattern("reftable-FLDroppingOutReason.xml");
        importBundledFilesByPattern("reftable-FLDropout.*");

        setStateReportFileName(FILE_NAME_PROCEDURE);
        setInputDefinitionFileName(FILE_NAME_INPUT_DEFINITION);
        setExportFormatFileName(FILE_NAME_EXPORT_FORMAT);
        setStateReportProcedureId(ID_PROCEDURE);
        setImportExportDefinitionId(ID_EXPORT);
        setExpectedResultFileName(FILE_NAME_EXPECTED);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);

        setToolInputParameter(PARAM_SURVEY_PERIOD, getSurveyPeriodOid("5"));

        run();
    }

    /**
     * Perform state specific setup.
     *
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#performStateSpecificSetup()
     */
    @Override
    protected void performStateSpecificSetup() {
        try {
            super.performStateSpecificSetup();

            GradePointAverageDefinition gpd =
                    (GradePointAverageDefinition) getBroker().getBeanByOid(GradePointAverageDefinition.class,
                            GRADE_POINT_DEFINITION_OID);
            if (gpd == null) {
                throw new RuntimeException("GradePointAverageDefinition with oid " + GRADE_POINT_DEFINITION_OID +
                        " should be defined to run this test");
            }
            gpd.setFieldValueByAlias(ALIAS_STATE_FIELD_INDICATOR, BooleanAsStringConverter.TRUE);
            getBroker().saveBeanForced(gpd);
        } catch (Exception e) {
            revert();
            fail(e.getMessage());
        }
    }
}
