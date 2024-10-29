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
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.test.CSVComparator;
import com.follett.fsc.core.k12.test.X2StateReportTest;
import com.follett.fsc.core.utils.RandomBeanGenerator;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;
import org.junit.Ignore;

/**
 * The Class FLStateReportTest.
 */
@Ignore
public class FLWdisTest extends X2StateReportTest {
    protected final static String FILE_NAME_EXPORT = "FLStateReportExport.java";
    protected final static String TEST_CONTEXT_ID = "ctx01";

    private final static String FILE_NAME_ALIAS_DEFINITION = "FL_Alias_Definition.csv";

    private final static String PACKAGE_NAME = "com.x2dev.procedures.statereporting.fl";
    private final static String DEFAULT_SPECIFIC_SETUP_JSON = "{"
            + "'options':{'seed':'123'},"
            + "'beans':[{"
            + "'type':'districtschoolyearcontext',"
            + "'id':'" + TEST_CONTEXT_ID + "',"
            + "'fields':{"
            + "    'schoolYear':'2017',"
            + "    'startDate':'2016-10-01',"
            + "    'endDate':'2017-06-30'"
            + "}"
            + "}]}";
    private final static String REF_TABLE_OID_SURVEY_PERIODS = "rtbFlSurveyPer";

    private final static Set<String> m_cleanedFields = new HashSet<>();
    private final static Set<String> m_importedFiles = new HashSet<>();

    protected Organization m_rootOrganization = null;
    protected ModelBroker m_modelBroker = null;
    private DistrictSchoolYearContext m_defaultContext = null;
    private Map<String, Stack<X2BaseBean>> m_dependentAwaitingToPush = new HashMap<>();
    private Set<String> m_pushedTemporaryOids = new HashSet<>();
    private String m_specificSetupJson = null;
    private Map<String, String> m_surveyPeriodsMap = null;

    /**
     * @see com.follett.fsc.core.k12.test.X2BaseTest#pushTemporaryBean(com.follett.fsc.core.k12.business.X2Broker,
     *      com.follett.fsc.core.k12.beans.X2BaseBean)
     */
    @Override
    public void pushTemporaryBean(X2Broker broker, X2BaseBean bean) {
        if (bean instanceof SchoolScheduleContext) {
            SchoolScheduleContext schedContext = (SchoolScheduleContext) bean;
            String activeScheduleOid = schedContext.getActiveScheduleOid();
            if (!m_pushedTemporaryOids.contains(activeScheduleOid)) {
                Stack<X2BaseBean> awaitingToPush = m_dependentAwaitingToPush.get(activeScheduleOid);
                if (awaitingToPush == null) {
                    awaitingToPush = new Stack<>();
                    m_dependentAwaitingToPush.put(activeScheduleOid, awaitingToPush);
                }
                awaitingToPush.push(bean);
                return;
            }
        }
        m_pushedTemporaryOids.add(bean.getOid());
        super.pushTemporaryBean(broker, bean);
        Stack<X2BaseBean> awaitingToPush = m_dependentAwaitingToPush.get(bean.getOid());
        if (awaitingToPush != null) {
            while (!awaitingToPush.empty()) {
                pushTemporaryBean(broker, awaitingToPush.pop());
            }
        }
    }

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
        if (m_cleanedFields.add(fieldToClean)) {
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
        try {
            m_rootOrganization = OrganizationManager.getRootOrganization(getBroker());
            m_defaultContext = m_rootOrganization.getCurrentContext();
            if (m_specificSetupJson == null) {
                m_specificSetupJson = DEFAULT_SPECIFIC_SETUP_JSON;
            }
            RandomBeanGenerator beanGenerator =
                    new RandomBeanGenerator((ModelBroker) getBroker(), m_specificSetupJson.replace("'", "\""), this);
            beanGenerator.importBeans();

            DistrictSchoolYearContext ctx = (DistrictSchoolYearContext) beanGenerator.getBeanById(TEST_CONTEXT_ID);
            if (ctx != null) {
                m_rootOrganization.setCurrentContextOid(ctx.getOid());
                addToTemporaryBeans(ctx);
                getBroker().saveBeanForced(m_rootOrganization);
            }
        } catch (Exception e) {
            revert();
            fail(e.getMessage());
        }
        DataDictionaryCache.clearDictionaries(getBroker().getPersistenceKey(), true);
    }

    /**
     * Revert.
     */
    protected void revert() {
        if (m_defaultContext != null && m_rootOrganization != null) {
            m_rootOrganization.setCurrentContextOid("ctx00000002004");
            getBroker().saveBeanForced(m_rootOrganization);
        }
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
    protected void initialize() {
        m_modelBroker = new ModelBroker(getPrivilegeSet());

        setComparator(new CSVComparator(null));
        setPackageName(PACKAGE_NAME);
        setAliasDefinitionSpreadsheet(FILE_NAME_ALIAS_DEFINITION);
        setExportJavaSourceFileName(FILE_NAME_EXPORT);

        importBundledFilesByPattern("reftable-FLSurveyPeriods.xml");
        importBundledFilesByPattern("reftable-FLWdisFinancialAssistance.xml");

        m_surveyPeriodsMap = new HashMap<>();
        X2Criteria periodsCriteria = new X2Criteria();
        periodsCriteria.addEqualTo(
                ReferenceCode.REL_REFERENCE_TABLE + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                REF_TABLE_OID_SURVEY_PERIODS);
        String[] columns = {ReferenceCode.COL_CODE, X2BaseBean.COL_OID};
        ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, periodsCriteria);
        ReportQueryIterator iterator = m_modelBroker.getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] rows = (Object[]) iterator.next();
                String code = (String) rows[0];
                String oid = (String) rows[1];
                m_surveyPeriodsMap.put(code, oid);
            }
        } finally {
            iterator.close();
        }
    }
}
