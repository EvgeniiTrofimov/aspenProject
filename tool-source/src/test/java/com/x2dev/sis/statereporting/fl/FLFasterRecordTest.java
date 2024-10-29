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
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.test.CSVComparator;
import com.follett.fsc.core.k12.test.X2BaseTest;
import com.follett.fsc.core.k12.test.X2ToolTest;
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager;
import com.follett.fsc.core.utils.RandomBeanGenerator;
import com.follett.fsc.core.utils.conf.ConfigUtils;
import com.x2dev.utils.FolderDoesNotExistException;
import com.x2dev.utils.FolderUtils;
import com.x2dev.utils.UniqueFolderNotCreatedException;
import java.io.File;
import java.io.FilenameFilter;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.junit.Ignore;

/**
 * The Class FLStateReportTest.
 */
@Ignore
public class FLFasterRecordTest extends X2ToolTest {
    private final static String CODE_MESSAGE_TYPE_S01 = "S01";
    private final static String CODE_SCHOOL_ANCHOR = "1-83";
    private final static String CODE_SPEEDE_HERITAGE_ID = "710000013447700";

    protected final static String FILE_NAME_EXPORT = "FLStateReportExport.java";

    protected static final String INPUT_PARAM_ADDRESSED_SCHOOL = "addressedSchool";
    protected static final String INPUT_PARAM_IS_PRODUCTION = "isProduction";
    protected static final String INPUT_PARAM_MESSAGE_TYPE = "messageType";
    protected static final String INPUT_PARAM_RECORDS_TYPE = "recordsType";
    protected static final String INPUT_PARAM_REPORT_DATE = "reportDate";
    protected static final String INPUT_PARAM_RESTRICT_BY_RECORD_TYPE = "restrictByRecordType";
    protected static final String INPUT_PARAM_RUN_VALIDATIONS = "runValidations";
    protected static final String INPUT_PARAM_SENDING_SCHOOL = "schoolOid";
    protected static final String INPUT_PARAM_SHOW_TOTALS = "showTotals";
    protected static final String INPUT_PARAM_SPEEDE_INST_ID = "institutionId";
    protected static final String INPUT_PARAM_TRANSFER_TYPE = "transferType";

    private final static String REF_TABLE_NAME_MESSAGE_TYPES = "FL FASTER Request/Response Codes";
    private final static String REF_TABLE_NAME_SCHOOL_CODES = "FL FASTER Schools";
    private final static String REF_TABLE_NAME_SPEEDE_INST_ID_CODES = "FL FASTER SPEEDE/ExPRESS Institution ID Codes";

    protected static final String RECORD_TYPE_00 = "00";
    protected static final String RECORD_TYPE_01 = "01";
    protected static final String RECORD_TYPE_02 = "02";
    protected static final String RECORD_TYPE_03 = "03";
    protected static final String RECORD_TYPE_04 = "04";
    protected static final String RECORD_TYPE_05 = "05";
    protected static final String RECORD_TYPE_06 = "06";
    protected static final String RECORD_TYPE_07 = "07";
    protected static final String RECORD_TYPE_08 = "08";
    protected static final String RECORD_TYPE_09 = "09";
    protected static final String RECORD_TYPE_10 = "10";
    protected static final String RECORD_TYPE_11 = "11";
    protected static final String RECORD_TYPE_99 = "99";
    protected static final String RECORD_TYPE_99ATV = "99ATV";
    protected static final String RECORD_TYPE_99HS = "99HS";
    protected static final String RECORD_TYPE_99HC = "99HC";
    protected static final String RECORD_TYPE_99IMM = "99IMM";

    protected static final String RECORDS_TYPE_INTERDISTRICT = "I";
    protected static final String RECORDS_TYPE_SECONDARY = "S";

    protected final static String TEST_CONTEXT_ID = "ctx01";

    protected static final String TRANSFER_TYPE_RESPONSE = "Response";
    protected static final String TRANSFER_TYPE_REQUEST = "Request";

    private final static String FILE_NAME_ALIAS_DEFINITION = "FL_Alias_Definition.csv";

    private final static String PACKAGE_NAME = "com.x2dev.procedures.statereporting.fl";
    private final static String DEFAULT_SPECIFIC_SETUP_JSON = "{'options':{'seed':'123'},"
            + "'beans':[{"
            + "'type':'districtschoolyearcontext',"
            + "'id':'" + TEST_CONTEXT_ID + "',"
            + "'fields':{"
            + "    'oid':'ctx00000009999',"
            + "    'schoolYear':'2017',"
            + "    'startDate':'2016-09-01',"
            + "    'endDate':'2017-06-30'"
            + "}"
            + "}]}";

    private final static Set<String> m_cleanedFields = new HashSet<>();
    private final static Set<String> m_importedFiles = new HashSet<>();

    protected Organization m_rootOrganization = null;

    protected ModelBroker m_modelBroker = null;
    private DistrictSchoolYearContext m_defaultContext = null;
    private Map<String, Map<String, String>> m_refTableNameToCodesMap = new HashMap<>();
    private String m_specificSetupJson = null;
    private File m_tempFolder = null;

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
                getBroker().saveBeanForced(m_rootOrganization);
            }
        } catch (Exception e) {
            revert();
            fail(e.getMessage());
        }

        X2Criteria efdCriteria = new X2Criteria();
        efdCriteria.addLike(ExportFormatDefinition.COL_PROCEDURE_ID, "EXPDATA-FL-FST-%%%%%");
        QueryByCriteria efdQuery = new QueryByCriteria(ExportFormatDefinition.class,
                efdCriteria);
        Collection<ExportFormatDefinition> efds = getBroker().getCollectionByQuery(efdQuery);

        for (ExportFormatDefinition efd : efds) {
            ExportFormatManager.validateDefinition(efd, getBroker(), true);
        }
        DataDictionaryCache.clearDictionaries(m_modelBroker.getPersistenceKey(), true);
    }

    /**
     * Revert.
     */
    protected void revert() {
        if (m_defaultContext != null && m_rootOrganization != null) {
            m_rootOrganization.setCurrentContextOid(m_defaultContext.getOid());
            getBroker().saveBeanForced(m_rootOrganization);
        }
    }

    /**
     * Sets the FL parameters.
     */
    protected void setFLParameters() {
        initialize();
        setComparator(new CSVComparator(null));
        setPackageName(PACKAGE_NAME);
        setAliasDefinitionSpreadsheet(FILE_NAME_ALIAS_DEFINITION);

        importBundledFilesByPattern("reftable-FLFasterRequestResponseCodes.*");
        importBundledFilesByPattern("reftable-FLFasterSchool.xml");
        importBundledFilesByPattern("reftable-FLFasterSpeedeExpressCodes.xml");
        setRefTableParameter(INPUT_PARAM_MESSAGE_TYPE, REF_TABLE_NAME_MESSAGE_TYPES, CODE_MESSAGE_TYPE_S01);
        setRefTableParameter(INPUT_PARAM_ADDRESSED_SCHOOL, REF_TABLE_NAME_SCHOOL_CODES, CODE_SCHOOL_ANCHOR);
        setRefTableParameter(INPUT_PARAM_SPEEDE_INST_ID, REF_TABLE_NAME_SPEEDE_INST_ID_CODES, CODE_SPEEDE_HERITAGE_ID);

        importBundledFilesByPattern("dictionary-FL-FASTER-TNR.xml");
        importBundledFilesByPattern("dictionary-FL-FASTER-TNR-REC.xml");
        importBundledFilesByPattern("dictionary-FL-FASTER-TNR-STD.xml");

        importBundledFilesByPattern("reftable-FLScheduleTermCodes.xml");

        setToolInputParameter(INPUT_PARAM_RECORDS_TYPE, RECORDS_TYPE_INTERDISTRICT);
        setToolInputParameter(INPUT_PARAM_TRANSFER_TYPE, TRANSFER_TYPE_RESPONSE);
        setToolInputParameter(INPUT_PARAM_RUN_VALIDATIONS, Boolean.FALSE.toString());
        setToolInputParameter(INPUT_PARAM_SHOW_TOTALS, Boolean.FALSE.toString());
        setToolInputParameter(INPUT_PARAM_SENDING_SCHOOL, "skl00000000000");
        setToolInputParameter(INPUT_PARAM_IS_PRODUCTION, Boolean.TRUE.toString());
        setToolInputParameter(INPUT_PARAM_REPORT_DATE, "10/10/2016");

        importProcedure("FL-FASTER", "FLFasterCreateProcedure.java", "FLFasterCreateProcedureInput.xml");
        importProcedure("EXPDATA-FL-FST", "FLFasterRecordsData.java", null);
        importIED("EXP-FL-FST", "FLFasterStateReportExport.java", "FLFasterRecordsExportInput.xml");
        setToolToRunById("FL-FASTER");
    }

    /**
     * Sets the message type.
     *
     * @param messageType void
     */
    protected void setRefTableParameter(String inputParameter, String refTableName, String code) {
        Map<String, String> codeOidCodeMap = m_refTableNameToCodesMap.get(refTableName);
        if (codeOidCodeMap == null) {
            codeOidCodeMap = getRefCodesOidsByCodes(refTableName);
            m_refTableNameToCodesMap.put(refTableName, codeOidCodeMap);
        }

        setToolInputParameter(inputParameter, codeOidCodeMap.get(code));
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
     * Gets the files from package by pattern.
     *
     * @param pattern String
     * @return File[]
     */
    private File[] getFilesFromPackageByPattern(final String pattern) {
        File dir = new File(getProceduresFolder());
        File[] files = dir.listFiles(new FilenameFilter() {
            @Override
            public boolean accept(File fileDir, String name) {
                return name.matches(pattern);
            }
        });

        return files;
    }

    /**
     * Gets the ref codes oids by codes.
     *
     * @param refTableName String
     * @return Map
     */
    protected Map<String, String> getRefCodesOidsByCodes(String refTableName) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceTable.COL_USER_NAME, refTableName);
        QueryByCriteria queryByCriteria = new QueryByCriteria(ReferenceTable.class, criteria);
        ReferenceTable refTable = (ReferenceTable) getBroker().getBeanByQuery(queryByCriteria);
        HashMap<String, String> oidsByCodesMap = new HashMap<>();
        for (ReferenceCode refCode : refTable.getReferenceCodes()) {
            oidsByCodesMap.put(refCode.getCode(), refCode.getOid());
        }

        return oidsByCodesMap;
    }

    /**
     * Gets the procedures folder.
     *
     * @return String
     */
    private String getProceduresFolder() {
        String workspaceDir = X2BaseTest.getBootstrapDirectory().getParent();

        String p = Paths
                .get(workspaceDir, "com.x2dev.sis", "procedures", PACKAGE_NAME.replace(".", File.separator))
                .toString();

        if (!new File(p).exists()) {
            p = Paths
                    .get(workspaceDir, "com.x2dev.sis",
                            "procedures.com.x2dev.procedures.sys.shared".replace(".", File.separator))
                    .toString();
            if (!new File(p).exists()) {
                p = null;
            }
        }
        return p;
    }

    /**
     * Initialize.
     */
    private void initialize() {
        m_modelBroker = new ModelBroker(getPrivilegeSet());

        try {
            m_tempFolder = FolderUtils.createUniqueFolder(new File(ConfigUtils.getTempFolder()));
        } catch (FolderDoesNotExistException e) {
            e.printStackTrace();
        } catch (UniqueFolderNotCreatedException e) {
            e.printStackTrace();
        }
    }
}
