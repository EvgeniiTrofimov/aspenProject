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

import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.test.AspenIntegrationTestManager;
import org.junit.Test;

/**
 * The Class FLStudentCourseScheduleDataTest.
 */
public class FLStudentCourseScheduleDataTest extends FLTestBaseClass {
    private static final String ALIAS_FTE_MULTIPLIER_04_TO_12 = "fl-org-FteMultiplier04to12";
    private static final String ALIAS_FTE_MULTIPLIER_PK_TO_03 = "fl-org-FteMultiplierPKto03";

    private final static String FILE_NAME_EXPECTED = "FL-SSC-expected.txt";
    private final static String FILE_NAME_EXPORT_FORMAT = "export-format-EXPDATA-FL-SSC.xml";
    private final static String FILE_NAME_INPUT_DEFINITION = "FLStudentCourseScheduleExportInput.xml";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-SSC.json";
    private final static String FILE_NAME_PROCEDURE = "FLStudentCourseScheduleData.java";

    private final static String ID_EXPORT = "EXP-FL-SSC";
    private final static String ID_PROCEDURE = "EXPDATA-FL-SSC";

    private final static String PARAM_SURVEY_PERIOD = "surveyPeriod";

    private final static String SPECIFIC_SETUP_JSON = "{'options':{'seed':'123'},"
            + "'beans':["
            + "{"
            + "'type':'Organization',"
            + "'fields':{"
            + "    'oid':'0001',"
            + "    'all-org-StateId':'26'"
            + "}"
            + "},"
            + "{"
            + "'type':'districtschoolyearcontext',"
            + "'id':'" + TEST_CONTEXT_ID + "',"
            + "'fields':{"
            + "    'oid':'ctx00000002017',"
            + "    'schoolYear':'2016',"
            + "    'startDate':'2016-10-01',"
            + "    'endDate':'2017-06-30',"
            + "    'organization1Oid':'0001'"
            + "}"
            + "}"
            + "]}";

    private static final String TEST_MULTIPLIER_04_TO_12 = "0.00041667";
    private static final String TEST_MULTIPLIER_PK_TO_03 = "0.00041667";

    /**
     * Basic test.
     *
     * @throws Exception exception
     */
    @Test
    public void basicTest() throws Exception {
        super.setFLParameters();

        cleanField("trmTermCode");

        importBundledFilesByPattern("reftable-FLScheduleTermCodes.xml");
        importBundledFilesByPattern("reftable-FLCourseCodes.xml");

        setStateReportFileName(FILE_NAME_PROCEDURE);
        setInputDefinitionFileName(FILE_NAME_INPUT_DEFINITION);
        setExportFormatFileName(FILE_NAME_EXPORT_FORMAT);
        setStateReportProcedureId(ID_PROCEDURE);
        setImportExportDefinitionId(ID_EXPORT);
        setExpectedResultFileName(FILE_NAME_EXPECTED);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);

        setToolInputParameter(PARAM_SURVEY_PERIOD, getSurveyPeriodOid("3"));

        run();
    }

    /**
     * Perform state specific setup.
     *
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#performStateSpecificSetup()
     */
    @Override
    protected void performStateSpecificSetup() {
        setSpecificSetupJson(SPECIFIC_SETUP_JSON);
        super.performStateSpecificSetup();
        Organization rootOrg = OrganizationManager.getRootOrganization(getBroker());
        rootOrg.setFieldValueByAlias(ALIAS_FTE_MULTIPLIER_PK_TO_03, TEST_MULTIPLIER_PK_TO_03);
        rootOrg.setFieldValueByAlias(ALIAS_FTE_MULTIPLIER_04_TO_12, TEST_MULTIPLIER_04_TO_12);
        getBroker().saveBeanForced(rootOrg);
    }

    /**
     * @see com.x2dev.sis.statereporting.fl.FLTestBaseClass#revert()
     */
    @Override
    protected void revert() {
        Organization rootOrg = OrganizationManager.getRootOrganization(getBroker());
        rootOrg.setFieldValueByAlias(ALIAS_FTE_MULTIPLIER_PK_TO_03, null);
        rootOrg.setFieldValueByAlias(ALIAS_FTE_MULTIPLIER_04_TO_12, null);
        rootOrg.setCurrentContextOid(AspenIntegrationTestManager.ROOT_ORG_CONTEXT_OID);
        getBroker().saveBeanForced(rootOrg);

        super.revert();
    }
}
