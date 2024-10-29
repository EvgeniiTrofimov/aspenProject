/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
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
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.test.X2StateReportComparator;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.DictionaryExtractor;
import com.x2dev.procedures.statereporting.on.OnsisResultsHelper;
import com.x2dev.procedures.statereporting.on.OnsisResultsHelper.OnsisResult;
import com.x2dev.sis.model.beans.UserDefinedTableA;
import java.io.File;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.io.FileUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.junit.Assert;
import org.junit.Test;

/**
 * @author Follett Software Company
 * @copyright 2019
 */
public class OnsisErrorsImportTest extends OnSISTestBaseClass {

    private final static String DDX_ID_ERROR = "ON-SIS-ERROR";
    private final static String DDX_ID_RESULT = "ON-SIS-RESULT";
    private final static int EXPECTED_ERRORS_NUMBER = 2;
    private final static int EXPECTED_RESULT_NUMBER = 1;
    private final static String FILE_NAME_ALIAS_DEFINITION = "ON_Alias_Definition.csv";
    private final static String FILE_NAME_ERRORS_INPUT = "result_errors.xml";
    private final static String FILE_NAME_EXPECTED = "expected_errors_import.txt";
    private final static String FILE_NAME_JSON_INPUT_DATA = "input_ON_SIS_school.json";
    private final static String IMPORT_PROCEDURE_FILE_NAME = "OnsisErrorsImport.java";
    private final static String IMPORT_PROCEDURE_ID = "ONSIS-ERR-IMPORT";
    private final static String IMPORT_PROCEDURE_INPUT_FILE_NAME = "OnsisErrorsImportInput.xml";
    private final static String INPUT_PARAMETER_RESULT_OID = "resultOid";
    private final static String PACKAGE_NAME = "com.x2dev.procedures.statereporting.on";
    private final static String STAFF_OID = "staffOid111111";
    private final static String STUDENT_OID = "studentOid1111";

    private Map<String, Collection<X2BaseBean>> m_instancesByDdxId;

    /**
     * Test errors import.
     *
     * @throws Exception exception
     */
    @Test
    public void testErrorsImport() throws Exception {

        try {
            super.setONParameters();
            setImportExportDefinitionId(IMPORT_PROCEDURE_ID);
            setExportJavaSourceFileName(IMPORT_PROCEDURE_FILE_NAME);
            setInputDefinitionFileName(IMPORT_PROCEDURE_INPUT_FILE_NAME);
            setInputJsonBeansFileName(FILE_NAME_JSON_INPUT_DATA);
            setExpectedResultFileName(FILE_NAME_EXPECTED);
            DictionaryExtractor extractor = new DictionaryExtractor(getBroker());
            Organization organization = (Organization) getBroker()
                    .getCollectionByQuery(new QueryByCriteria(Organization.class)).iterator().next();
            OnsisResult result = OnsisResultsHelper.saveResult(organization, m_modelBroker, "",
                    Collections.EMPTY_MAP, extractor);

            assertNumberOfInstances(UserDefinedTableA.class, UserDefinedTableA.REL_EXTENDED_DATA_DICTIONARY,
                    EXPECTED_RESULT_NUMBER, DDX_ID_RESULT);

            setToolInputParameter(INPUT_PARAMETER_RESULT_OID, result.getBean().getOid());
            addProcedureToImport(IMPORT_PROCEDURE_FILE_NAME, IMPORT_PROCEDURE_INPUT_FILE_NAME, IMPORT_PROCEDURE_ID);

            run();

            List<X2BaseBean> errors = (List<X2BaseBean>) getInstancesByDdxId(UserDefinedTableA.class,
                    UserDefinedTableA.REL_EXTENDED_DATA_DICTIONARY, DDX_ID_ERROR);

            assertEquals(EXPECTED_ERRORS_NUMBER, errors.size());
            assertEquals(STAFF_OID, ((UserDefinedTableA) errors.get(0)).getUserDefinedTableB().getStaffOid());
            assertEquals(STUDENT_OID, ((UserDefinedTableA) errors.get(1)).getUserDefinedTableB().getStudentOid());

        } catch (Exception e) {
            throw e;
        } finally {
            cleanInstancesWithDdxId(UserDefinedTableA.class, UserDefinedTableA.REL_EXTENDED_DATA_DICTIONARY,
                    DDX_ID_RESULT);
            cleanInstancesWithDdxId(UserDefinedTableA.class, UserDefinedTableA.REL_EXTENDED_DATA_DICTIONARY,
                    DDX_ID_ERROR);
        }

    }

    @Override
    protected ToolInput createToolInput(Tool tool, UserDataContainer userData) throws Exception {
        ToolInput toolInput = super.createToolInput(tool, userData);
        toolInput.setFile(getFileFromResources(FILE_NAME_ERRORS_INPUT));
        return toolInput;
    }

    @Override
    protected X2StateReportComparator getComparator() {
        return new X2StateReportComparator() {
            /**
             * @see com.follett.fsc.core.k12.test.X2StateReportComparator#compare(java.io.File,
             *      java.io.File)
             */
            @Override
            public void compare(File expected, File result) throws Exception {
                if (!FileUtils.contentEquals(expected, result)) {
                    Assert.fail();
                }
            }
        };
    }

    private <T extends X2BaseBean> void assertNumberOfInstances(Class<T> clazz,
                                                                String relToDictionary,
                                                                int expectedNumber,
                                                                String ddxId) {
        Collection<X2BaseBean> createdResults = getInstancesByDdxId(clazz, relToDictionary, ddxId);

        assertEquals("Number of instances with extended dictionary id " + ddxId + " is wrong", expectedNumber,
                createdResults.size());
    }

    private <T extends X2BaseBean> void cleanInstancesWithDdxId(Class<T> clazz,
                                                                String relToDictionary,
                                                                String ddxId) {
        for (X2BaseBean bean : getInstancesByDdxId(clazz, relToDictionary, ddxId)) {
            getBroker().deleteBean(bean);
        }
    }

    private <T extends X2BaseBean> Collection<X2BaseBean> getInstancesByDdxId(Class<T> clazz,
                                                                              String relToDictionary,
                                                                              String ddxId) {
        if (m_instancesByDdxId == null) {
            m_instancesByDdxId = new HashMap<>();
        }

        Collection<X2BaseBean> instances = m_instancesByDdxId.get(ddxId);

        if (instances == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(relToDictionary + ModelProperty.PATH_DELIMITER
                    + ExtendedDataDictionary.COL_ID, ddxId);

            QueryByCriteria query = new QueryByCriteria(clazz, criteria);
            instances = getBroker().getCollectionByQuery(query);
            m_instancesByDdxId.put(ddxId, instances);
        }

        return instances;
    }

}
