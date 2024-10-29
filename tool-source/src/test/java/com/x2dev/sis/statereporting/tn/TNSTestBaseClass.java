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
package com.x2dev.sis.statereporting.tn;

import com.follett.fsc.core.k12.test.X2StateReportTest;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

/**
 * The Class FLStateReportTest.
 */
public class TNSTestBaseClass extends X2StateReportTest {

    private static Map<String, String> m_proceduresToImport = null;

    static {
        m_proceduresToImport = new HashMap<>();
        m_proceduresToImport.put("TN-HEAD-FOOT", "TNHeadingTrailing.java");
        m_proceduresToImport.put("TN-REPORT-DATA", "TNStateReportData.java");
        m_proceduresToImport.put("TN-ENROLL-HELP", "TNEnrollmentHelper.java");
        m_proceduresToImport.put("TN-REPORT-ENTITY", "TNStateReportEntity.java");
        m_proceduresToImport.put("TN-SECTION-HELP", "TNClassSectionHelper.java");
        m_proceduresToImport.put("EXPDATA-TN-ENR", "TNStudentEnrollmentData.java");
        m_proceduresToImport.put("TN-ENROLL-DATA", "TNEnrollReportData.java");
    }

    private static boolean s_applicationInitFailure = false;
    private static boolean s_initialized = false;

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

    protected void setTNParameters() {
        setPackageName("com.x2dev.procedures.statereporting.tn");
        setToolInputParameter("suppressHeading", "true");
        setToolInputParameter("suppressTrailer", "true");
        setExportJavaSourceFileName("TNStateReportExport.java");
        setAliasDefinitionSpreadsheet("TN State Reporting Alias Definition.csv");
        for (Entry<String, String> procedureToImport : m_proceduresToImport.entrySet()) {
            addProcedureToImport(procedureToImport.getValue(), procedureToImport.getKey());
        }
    }
}
