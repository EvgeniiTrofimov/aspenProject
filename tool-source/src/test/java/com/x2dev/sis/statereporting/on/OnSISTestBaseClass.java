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
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.test.X2BaseTest;
import com.follett.fsc.core.k12.test.X2StateReportComparator;
import com.follett.fsc.core.k12.test.X2StateReportTest;
import com.follett.fsc.core.k12.test.XMLComparator;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.utils.StreamUtils;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.zip.ZipException;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class OnSISTestBaseClass.
 */
public class OnSISTestBaseClass extends X2StateReportTest {
    enum PROCEDURES_TO_IMPORT {
        BASE("ONSIS-BASE", "OnsisStateReportData.java"),
        //
        CLS("ONSIS-CLS-ROOT", "OnsisSchoolClass.java"),
        //
        CND("ONSIS-CND-ROOT", "OnsisSchoolIncident.java"),
        //
        CND_STD("ONSIS-CND-STD-ROOT", "OnsisSchoolIncidentStd.java"),
        //
        CND_STD_INFR("ONSIS-CND-STD-INFR", "OnsisSchoolIncidentStdInfr.java"),
        //
        CND_STD_OUTC("ONSIS-CND-STD-OUTC", "OnsisSchoolIncidentStdOutc.java"),
        //
        CND_STD_SEP_NP("ONSIS-CND-STD-SEP-NP", "OnsisSchoolIncidentSepNp.java"),
        //
        CND_STD_SEP_RO("ONSIS-CND-STD-SEP-RO", "OnsisSchoolIncidentSepRo.java"),
        //
        COM("ONSIS-ENR-SAL-COM", "OnsisSalepComponent.java"),
        //
        CRS_CAL("ONSIS-CRS-CAL-ROOT", "OnsisCourseCalendar.java"),
        //
        CRS_CAL_CRS("ONSIS-CRS-CAL-CRS", "OnsisCourseCalendarCourse.java"),
        //
        AVG("ONSIS-AVG-ROOT", "OnsisAvgReportCardGrade.java"),
        //
        AVG_SST("ONSIS-AVG-SST", "OnsisAvgSubjectStrand.java"),
        //
        ENR("ONSIS-ENR-ROOT", "OnsisStudentSchoolEnrollment.java"),
        //
        ENR_DIP("ONSIS-ENR-DIP", "OnsisDiploma.java"),
        //
        ENR_EOS("ONSIS-ENR-EOS-ROOT", "OnsisOptionSheet.java"),
        //
        ENR_EOS_EOSC("ONSIS-ENR-EOS-EOSC", "OnsisOptionSheetCourse.java"),
        //
        ENR_NCADE("ONSIS-ENR-NCADE", "OnsisNonCreditAvDailyEnr.java"),
        //
        ENR_OCR("ONSIS-ENR-OCR-ROOT", "OnsisOtherCredit.java"),
        //
        ENR_OCR_ADE("ONSIS-ENR-OCR-ADE", "OnsisOtherCreditAde.java"),
        //
        ENR_PLAR("ONSIS-ENR-PLAR", "OnsisPlar.java"),
        //
        ENR_RPR("ONSIS-ENR-RPR", "OnsisRemedialProgram.java"),
        //
        ENR_SEYE("ONSIS-ENR-SEYE", "OnsisStuEarlyYrExp.java"),
        //
        ENR_SHSM_CRS("ONSIS-ENR-SHSM-CRS", "OnsisShsmCourse.java"),
        //
        ENR_SLP("ONSIS-ENR-SLP", "OnsisSecondLanguageProgram.java"),
        //
        ENR_SPCE("ONSIS-ENR-SPCE", "OnsisSpce.java"),
        //
        ENR_SPED("ONSIS-ENR-SPED", "OnsisSped.java"),
        //
        ENR_SSC("ONSIS-ENR-SSC", "OnsisStudentClassEnrolment.java"),
        //
        ENR_TRANS("ONSIS-ENR-TRANS", "OnsisTransitionDate.java"),
        //
        ILE("ONSIS-ILE-ROOT", "OnsisIle.java"),
        //
        ILE_CNT("ONSIS-ILE-CNT-ROOT", "OnsisIleLanguageProgramCount.java"),
        //
        ILE_CNT_TOD("ONSIS-ILE-CNT-TOD", "OnsisIleLanguageProgramCountTod.java"),
        //
        LIT_NUM("ONSIS-LIT-NUM-ROOT", "OnsisLiteracyNumeracy.java"),
        //
        LIT_NUM_GRD("ONSIS-LIT-NUM-GRD", "OnsisLiteracyNumeracyGrade.java"),
        //
        MTC("ONSIS-SEA-CASM", "OnsisEducatorClassAssignment.java"),
        //
        NON_ENR("ONSIS-NON-ENR", "OnsisStudentNonEnrolment.java"),
        //
        PLAR_RPT("ONSIS-PLAR-RPT", "OnsisPlarMatureReport.java"),
        //
        PLAR_RPT_CNT("ONSIS-PLAR-CNT", "OnsisPlarMatureEqCount.java"),
        //
        PLAR_RPT_("ONSIS-PLAR-CRS", "OnsisPlarMatureEqCourses.java"),
        //
        RPT_CRD("ONSIS-RPT-CRD-ROOT", "OnsisReportCard.java"),
        //
        RPT_CRD_SST("ONSIS-RPT-CRD-SST", "OnsisReportCardSubjectStrand.java"),
        //
        RPT_CRD_TERM("ONSIS-RPT-CRD-TERM", "OnsisReportCardTerm.java"),
        //
        RPT_CRD_SKILL("ONSIS-RPT-CRD-SKILL", "OnsisReportCardSkill.java"),
        //
        SAL("ONSIS-ENR-SAL-ROOT", "OnsisSalep.java"),
        //
        SEA("ONSIS-SEA-ROOT", "OnsisSchoolEducatorAssignment.java"),
        //
        SEA_ITM("ONSIS-SEA-ITM", "OnsisInstructionalTm.java"),
        //
        SEA_SBJ("ONSIS-SEA-SBJ-ROOT", "OnsisAssignedSubject.java"),
        //
        SEA_SBJ_GRADE("ONSIS-SEA-SBJ-GRD", "OnsisAssignedGrade.java"),
        //
        SEA_TLA("ONSIS-SEA-TLA", "OnsisTla.java"),
        //
        SEG("ONSIS-CLS-SEG", "OnsisClassSegment.java"),
        //
        SHSM("ONSIS-ENR-SHSM-ROOT", "OnsisShsmProgram.java"),
        //
        SHSM_CERT("ONSIS-ENR-SHSM-CERT", "OnsisShsmCertification.java"),
        //
        SKL("ONSIS-SKL", "OnsisSchool.java"),
        //
        STD("ONSIS-STD", "OnsisSchoolStudent.java"),
        //
        SUBMISSION_SKL("ONSIS-SUBMISSION-SKL", "OnsisSchoolSubmission.java"),
        //
        SEYE_ACT("ONSIS-SEYE-ACT", "OnsisStuEarlyYrCcActivity.java"),
        //
        SEYE_DET("ONSIS-SEYE-PRG-DET", "OnsisStuEarlyYrCcPrgDet.java"),
        //
        SEYE_HEAD("ONSIS-SEYE-PRG-HEAD", "OnsisStuEarlyYrCcPrgHead.java"),
        //
        SEYE_Q("ONSIS-SEYE-Q", "OnsisStuEarlyYrQuestion.java"),
        //
        SSC_ADE("ONSIS-SSC-ADE", "OnsisSscAde.java"),
        //
        SWT("ONSIS-SWT-ROOT", "OnsisSchoolSwt.java"),
        //
        SWT_PGM("ONSIS-SWT-PGM-ROOT", "OnsisSwtProgram.java"),
        //
        SWT_PGM_GENDER("ONSIS-SWT-PGM-GEN", "OnsisSwtProgramGender.java"),
        //
        UTIL_ELEM("ONSIS-UTIL-ELEM", "ExsmsElementsHelper.java"),
        //
        UTIL_EXTR("ONSIS-UTIL-EXTR", "OnsisExtractHelper.java"),
        //
        UTIL_FILT("ONSIS-UTIL-FILT", "FilterableFactory.java"),
        //
        UTIL_HIST("ONSIS-UTIL-HIST", "OnsisStudentHistoryHelper.java"),
        //
        UTIL_RES("ONSIS-UTIL-RES", "OnsisResultsHelper.java");

        private String m_fileName = null;
        private String m_procedureId = null;

        private PROCEDURES_TO_IMPORT(String procedureId, String fileName) {
            m_procedureId = procedureId;
            m_fileName = fileName;
        }

        public String getFileName() {
            return m_fileName;
        }

        public String getProcedureId() {
            return m_procedureId;
        }
    }

    private final static String FILE_CSV_PATTERN = "EXSMS-.*";
    private final static String FILE_NAME_ALIAS_DEFINITION = "ON_Alias_Definition.csv";
    private final static String FILE_NAME_DDX_ALL = "dictionary-.*xml";
    private final static String FILE_NAME_EXPORT_ASD_ALL = "assessment-definition.*xml";
    private final static String FILE_NAME_EXPORT_FORMATS_ALL = "export-format-.*xml";
    private final static String FILE_NAME_FORMS_ALL = "form-.*xml";

    private final static String FILE_NAME_REF_TABLE_PREFIX = "reftable-.*";

    private final static String PACKAGE_NAME = "com.x2dev.procedures.statereporting.on";

    protected static final String INPUT_PARAM_REPORT_DATE = "reportDate";

    private static boolean s_applicationInitFailure = false;
    private final static Set<String> s_cleanedFields = new HashSet<>();
    private final static Set<String> s_dynamicElements = new HashSet<>(Arrays.asList("TIME", "DATE", "FILE_ID"));
    private static boolean s_initialized = false;

    protected ModelBroker m_modelBroker = null;

    /**
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#beforeRun()
     */
    @Override
    protected void beforeRun() throws Exception {
        importBundledFilesByPattern(FILE_NAME_DDX_ALL);
        DataDictionaryCache.clearDictionaries(getBroker().getPersistenceKey(), true);
        importBundledFilesByPattern(FILE_NAME_REF_TABLE_PREFIX);
        importBundledFilesByPattern(FILE_NAME_EXPORT_ASD_ALL);
        importBundledFilesByPattern(FILE_NAME_EXPORT_FORMATS_ALL);
        importBundledFilesByPattern(FILE_NAME_FORMS_ALL);

        importProcedures();
        DataDictionaryCache.clearDictionaries(getBroker().getPersistenceKey(), true);
        DataDictionaryCache.clearExtendedDictionaries(getBroker().getPersistenceKey(), true);
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
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#createToolInput(com.follett.fsc.core.k12.tools.Tool,
     *      com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected ToolInput createToolInput(Tool tool, UserDataContainer userData) throws Exception {
        ToolInput toolInput = super.createToolInput(tool, userData);
        toolInput.setFile(getFileToImport());
        return toolInput;
    }

    /**
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#getComparator()
     */
    @Override
    protected X2StateReportComparator getComparator() {
        return new X2StateReportComparator() {
            /**
             * @see com.follett.fsc.core.k12.test.X2StateReportComparator#compare(java.io.File,
             *      java.io.File)
             */
            @Override
            public void compare(File expected, File result) throws Exception {
                String formattedResult = getFileAsString(result);
                System.out.println(formattedResult);
                new XMLComparator(s_dynamicElements).compare(
                        expected,
                        result);
            }
        };
    }

    protected String getCsvPattern() {
        return FILE_CSV_PATTERN;
    }

    protected String getResourcesPath() {
        String workspaceDir = X2BaseTest.getBootstrapDirectory().getParent();
        return Paths
                .get(workspaceDir, "com.x2dev.sis-state-tests", "resources", "com", "x2dev", "sis", "statereporting",
                        "on")
                .toString();
    }

    protected String getResultFileName() {
        return null;
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

    /**
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#setInitFailed(boolean)
     */
    @Override
    protected void setInitFailed(boolean isInitFailed) {
        s_applicationInitFailure = isInitFailed;
    }

    /**
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#setInitialized(boolean)
     */
    @Override
    protected void setInitialized(boolean isInitialized) {
        s_initialized = isInitialized;
    }

    /**
     * Sets the ON parameters.
     */
    protected void setONParameters() {
        setPackageName(PACKAGE_NAME);
        initialize();
        setAliasDefinitionSpreadsheet(FILE_NAME_ALIAS_DEFINITION);

        for (PROCEDURES_TO_IMPORT procedure : PROCEDURES_TO_IMPORT.values()) {
            addProcedureToImport(procedure.getFileName(), procedure.getProcedureId());
        }
    }

    @Override
    protected void validateRequiredParameters() throws Exception {
        // no need
    }

    private String getFileAsString(File file) throws ZipException, IOException {
        FileInputStream fis = new FileInputStream(file);
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        StreamUtils.copyStream(fis, bos);
        return new String(bos.toByteArray());
    }

    protected File getFileToImport() {
        return zipFiles(getFilesByPattern(getCsvPattern(), getResourcesPath()));
    }

    /**
     * Initialize.
     */
    private void initialize() {
        m_modelBroker = new ModelBroker(getPrivilegeSet());
    }

    /**
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#tearDown()
     */
    @Override
    public void tearDown() throws Exception {
        super.tearDown();

        QueryByCriteria formatFieldQuery = new QueryByCriteria(ExportFormatField.class);
        m_modelBroker.deleteByQuery(formatFieldQuery);

        QueryByCriteria formatQuery = new QueryByCriteria(ExportFormatDefinition.class);
        m_modelBroker.deleteByQuery(formatQuery);
    }
}
