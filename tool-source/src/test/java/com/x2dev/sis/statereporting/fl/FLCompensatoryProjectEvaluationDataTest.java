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

import org.apache.ojb.broker.query.QueryByCriteria;
import org.junit.Test;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;

/**
 * The Class FLCompensatoryProjectEvaluationData.
 */
public class FLCompensatoryProjectEvaluationDataTest extends FLTestBaseClass {
    private final static String ALIAS_SUBJECT_AREA = "pgm-subject-area";

    private final static String FILE_NAME_EXPECTED = "FL-CPE-expected.txt";
    private final static String FILE_NAME_EXPORT_FORMAT = "export-format-EXPDATA-FL-CPE.xml";
    private final static String FILE_NAME_EXTENDED_DICTIONARY = "dictionary-FL-PGM-MIGRANT.xml";
    private final static String FILE_NAME_INPUT_DEFINITION = "FLCompensatoryProjectEvaluationExportInput.xml";
    private final static String FILE_NAME_JSON_INPUT_DATA = "json-input-data-FL-CPE.json";
    private final static String FILE_NAME_PROCEDURE = "FLCompensatoryProjectEvaluationData.java";

    private final static String ID_EXPORT = "EXP-FL-CPE";
    private final static String ID_PROCEDURE = "EXPDATA-FL-CPE";

    private final static String PARAM_SURVEY_PERIOD = "surveyPeriod";

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
        importBundledFilesByPattern("reftable-FLImmigrantStudentServices.xml");
        importBundledFilesByPattern("reftable-FLMigrant.*");

        setStateReportFileName(FILE_NAME_PROCEDURE);
        setInputDefinitionFileName(FILE_NAME_INPUT_DEFINITION);
        setExportFormatFileName(FILE_NAME_EXPORT_FORMAT);
        setStateReportProcedureId(ID_PROCEDURE);
        setImportExportDefinitionId(ID_EXPORT);
        setExpectedResultFileName(FILE_NAME_EXPECTED);
        setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);

        setToolInputParameter(PARAM_SURVEY_PERIOD, getSurveyPeriodOid("5"));

        importBundledFilesByPattern(FILE_NAME_EXTENDED_DICTIONARY);

        run();
    }

    /**
     * Perform state specific setup.
     *
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#performStateSpecificSetup()
     */
    @Override
    protected void performStateSpecificSetup() {
        super.performStateSpecificSetup();

        X2Criteria fieldCriteria = new X2Criteria();
        fieldCriteria.addEqualTo(ExtendedDataField.COL_ALIAS, ALIAS_SUBJECT_AREA);
        QueryByCriteria fieldQuery = new QueryByCriteria(ExtendedDataField.class, fieldCriteria);
        ExtendedDataField subjectField = (ExtendedDataField) getBroker().getBeanByQuery(fieldQuery);
        if (subjectField != null) {
            subjectField.setDataFieldConfigOid(null);
            getBroker().saveBeanForced(subjectField);
        }
        DataDictionaryCache.clearDictionaries(getBroker().getPersistenceKey(), true);
    }
}
