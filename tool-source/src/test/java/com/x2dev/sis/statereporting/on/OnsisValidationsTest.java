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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.utils.RandomBeanGenerator;
import com.x2dev.procedures.statereporting.on.OnsisExtractHelper;
import com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.DictionaryExtractor;
import com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.SubmissionType;
import com.x2dev.procedures.statereporting.on.OnsisValidations.OnsisValidator;
import com.x2dev.sis.model.beans.UserDefinedTableA;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.Collection;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.junit.Assert;
import org.junit.Test;

public class OnsisValidationsTest extends OnSISTestBaseClass {
    private final String DDX_OID = "ddxOnSisError";
    private final String FILE_NAME_ONSIS_RESULT_TO_VALIDATE = "OnsisResultToValidate.xml";
    private final String INPUT_JSON = "input_validations_test.json";
    private final String SUBMISSION_TYPE_OID = "subType0000002";

    /**
     * Basic test.
     *
     * @throws Exception exception
     */
    @Test
    public void basicTest() throws Exception {
        OnsisExtractHelper onsisHelper = new OnsisExtractHelper(getBroker());

        setONParameters();
        beforeRun();

        onsisHelper.initializeMatchers(getFileToImport(), getTempFolder());
        File resultToValidate = getFilesByPattern(FILE_NAME_ONSIS_RESULT_TO_VALIDATE, getResourcesPath())[0];
        RandomBeanGenerator beanGen = null;
        try (FileReader fr = new FileReader(resultToValidate);
                BufferedReader bufferedReader = new BufferedReader(fr)) {
            super.setONParameters();
            beanGen =
                    new RandomBeanGenerator(new ModelBroker(getPrivilegeSet()), getFileFromResources(INPUT_JSON), this);
            beanGen.importBeans();

            StringBuilder result = new StringBuilder();
            String line = null;
            while ((line = bufferedReader.readLine()) != null) {
                result.append(line);
            }
            DictionaryExtractor dictExtractor = new DictionaryExtractor(getBroker());
            SubmissionType submissionType = SubmissionType.find(getBroker(), SUBMISSION_TYPE_OID, dictExtractor);

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(X2BaseBean.COL_OID, "*dst");
            List<Organization> organizations = (List<Organization>) getBroker()
                    .getCollectionByQuery(new QueryByCriteria(Organization.class, criteria));

            OnsisValidator.instance(organizations.get(0), getBroker(), submissionType, onsisHelper, true)
                    .validateResult(result.toString());
        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail();
        } finally {
            try {
                beanGen.deleteBeans();
            } catch (Exception e) {
                e.printStackTrace();
            }
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(UserDefinedTableA.COL_EXTENDED_DATA_DICTIONARY_OID, DDX_OID);
            Collection<UserDefinedTableA> errors =
                    getBroker().getCollectionByQuery(new QueryByCriteria(UserDefinedTableA.class, criteria));
            for (X2BaseBean error : errors) {
                getBroker().deleteBean(error);
            }
        }
    }
}
