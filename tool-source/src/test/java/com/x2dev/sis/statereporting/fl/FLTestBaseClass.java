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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.test.AspenIntegrationTestManager;
import com.follett.fsc.core.k12.test.CSVComparator;
import com.follett.fsc.core.k12.test.X2StateReportTest;
import com.follett.fsc.core.utils.RandomBeanGenerator;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * The Class FLStateReportTest.
 */
public class FLTestBaseClass extends X2StateReportTest {
    protected final static String ID_BASE_PROCEDURE = "FL-EXPDATA";
    protected final static String ID_ENTITY = "FL-EXPDATA-ENTITY";
    protected final static String ID_SCHEDULE_HELPER = "FL-SCHEDULE-HELP";
    protected final static String ID_STAFF_HELPER = "FL-STAFF-HELP";
    protected final static String ID_STUDENT_HELPER = "FL-STUDENT-HELP";
    protected final static String FILE_NAME_BASE_PROCEDURE = "FLStateReportData.java";
    protected final static String FILE_NAME_ENTITY = "FLStateReportEntity.java";
    protected final static String FILE_NAME_EXPORT = "FLStateReportExport.java";
    protected final static String FILE_NAME_SCHEDULE_HELPER = "FLScheduleHelper.java";
    protected final static String FILE_NAME_STAFF_HELPER = "FLStaffHelper.java";
    protected final static String FILE_NAME_STUDENT_HELPER = "FLStudentHelper.java";

    protected final static String TEST_CONTEXT_ID = "ctx01";

    private final static String FILE_NAME_ALIAS_DEFINITION = "FL_Alias_Definition.csv";

    private final static String PACKAGE_NAME = "com.x2dev.procedures.statereporting.fl";
    private final static String DEFAULT_SPECIFIC_SETUP_JSON = "{'options':{'seed':'123'},"
            + "'beans':[{"
            + "'type':'districtschoolyearcontext',"
            + "'id':'" + TEST_CONTEXT_ID + "',"
            + "'fields':{"
            + "    'oid':'ctx00000009999',"
            + "    'schoolYear':'2016',"
            + "    'startDate':'2016-10-01',"
            + "    'endDate':'2017-06-30'"
            + "}"
            + "}]}";
    private final static String REF_TABLE_OID_SURVEY_PERIODS = "rtbFlSurveyPer";

    private static boolean s_applicationInitFailure = false;
    private final static Set<String> s_cleanedFields = new HashSet<>();
    private static boolean s_initialized = false;

    protected ModelBroker m_modelBroker = null;

    private RandomBeanGenerator m_beanGenerator = null;
    private String m_specificSetupJson = null;
    private Map<String, String> m_surveyPeriodsMap = null;

    /**
     * Tear down.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#tearDown()
     */
    @Override
    public void tearDown() throws Exception {
        revert();
        super.tearDown();
    }

    /**
     * Clear existing ref tables that should be overwritten, because StateReportingSetupImport
     * doesn't overwrite ref table oid if it is already set
     *
     * @param fieldToClean
     */
    protected void cleanField(String fieldToClean) {
        if (s_cleanedFields.add(fieldToClean)) {
            X2Criteria fddCriteria = new X2Criteria();
            List<String> dataFieldOids = Arrays.asList(fieldToClean);
            fddCriteria.addIn(DataFieldConfig.COL_DATA_FIELD_OID, dataFieldOids);
            QueryByCriteria fddQuery = new QueryByCriteria(DataFieldConfig.class, fddCriteria);

            Collection<DataFieldConfig> fdds = m_modelBroker.getCollectionByQuery(fddQuery);
            for (DataFieldConfig fdd : fdds) {
                fdd.setReferenceTableOid(null);
                m_modelBroker.saveBeanForced(fdd);
            }

            DataDictionaryCache.clearDictionaries(m_modelBroker.getPersistenceKey(), true);

            resetAliasDefinitionState();
        }
    }

    /**
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#isInitFailed()
     */
    @Override
    protected boolean isInitFailed() {
        return s_applicationInitFailure;
    }

    /**
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#isInitialized()
     */
    @Override
    protected boolean isInitialized() {
        return s_initialized;
    }

    protected String getSurveyPeriodOid(String periodCode) {
        return m_surveyPeriodsMap.get(periodCode);
    }

    /**
     * Perform state specific setup.
     *
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#performStateSpecificSetup()
     */
    @Override
    protected void performStateSpecificSetup() {
        if (m_specificSetupJson == null) {
            m_specificSetupJson = DEFAULT_SPECIFIC_SETUP_JSON;
        }

        try {
            m_beanGenerator =
                    new RandomBeanGenerator((ModelBroker) getBroker(), m_specificSetupJson.replace("'", "\""), this);
        } catch (IOException e) {
            System.out.println(e);
            revert();
            fail(e.getMessage());
            return;
        }

        m_beanGenerator.importBeans();

        DistrictSchoolYearContext ctx = (DistrictSchoolYearContext) m_beanGenerator.getBeanById(TEST_CONTEXT_ID);
        if (ctx != null) {
            Organization rootOrg = OrganizationManager.getRootOrganization(getBroker());
            rootOrg.setCurrentContextOid(ctx.getOid());
            getBroker().saveBeanForced(rootOrg);
        }

        DataDictionaryCache.clearDictionaries(getBroker().getPersistenceKey(), true);
    }

    /**
     * Revert.
     */
    protected void revert() {
        Organization rootOrg = OrganizationManager.getRootOrganization(getBroker());
        rootOrg.setCurrentContextOid(AspenIntegrationTestManager.ROOT_ORG_CONTEXT_OID);
        getBroker().saveBeanForced(rootOrg);

        if (m_beanGenerator != null) {
            m_beanGenerator.deleteBeans();
        }
    }

    /**
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#setInitialized(boolean)
     */
    @Override
    protected void setInitialized(boolean isInitialized) {
        s_initialized = isInitialized;
    }

    /**
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#setInitFailed(boolean)
     */
    @Override
    protected void setInitFailed(boolean isInitFailed) {
        s_applicationInitFailure = isInitFailed;
    }

    /**
     * Sets the FL parameters.
     */
    protected void setFLParameters() {
        setPackageName(PACKAGE_NAME);
        initialize();
        setComparator(new CSVComparator(null));
        setAliasDefinitionSpreadsheet(FILE_NAME_ALIAS_DEFINITION);
        setExportJavaSourceFileName(FILE_NAME_EXPORT);
        addProcedureToImport(FILE_NAME_BASE_PROCEDURE, ID_BASE_PROCEDURE);
        addProcedureToImport(FILE_NAME_ENTITY, ID_ENTITY);
        addProcedureToImport(FILE_NAME_STAFF_HELPER, ID_STAFF_HELPER);
        addProcedureToImport(FILE_NAME_STUDENT_HELPER, ID_STUDENT_HELPER);
        addProcedureToImport(FILE_NAME_SCHEDULE_HELPER, ID_SCHEDULE_HELPER);
    }

    /**
     * Sets the specific setup json.
     *
     * @param specificSetupJson void
     */
    protected void setSpecificSetupJson(String specificSetupJson) {
        m_specificSetupJson = specificSetupJson;
    }

    /**
     * Initialize.
     */
    private void initialize() {
        m_modelBroker = new ModelBroker(getPrivilegeSet());

        importBundledFilesByPattern("reftable-FLSurveyPeriods.xml");

        m_surveyPeriodsMap = new HashMap<>();
        X2Criteria periodsCriteria = new X2Criteria();
        periodsCriteria.addEqualTo(
                ReferenceCode.REL_REFERENCE_TABLE + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                REF_TABLE_OID_SURVEY_PERIODS);
        String[] columns = {ReferenceCode.COL_CODE, X2BaseBean.COL_OID};
        ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, periodsCriteria);

        try (ReportQueryIterator<?> iterator = m_modelBroker.getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] rows = (Object[]) iterator.next();
                String code = (String) rows[0];
                String oid = (String) rows[1];
                m_surveyPeriodsMap.put(code, oid);
            }
        }
    }
}
