/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.sis.tools.procedures;

import com.follett.fsc.core.k12.beans.ToolSourceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.InvalidPreferenceException;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.test.PreferenceManagerTemporarySettings;
import com.follett.fsc.core.k12.test.X2BaseTest;
import com.follett.fsc.core.k12.test.X2DataTest;
import com.follett.fsc.core.k12.tools.ToolRunException;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisProcedure;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.model.business.health.immunizations.ImmunizationRuleTestMaster;
import com.x2dev.utils.ByteArrayCompiler;
import com.x2dev.utils.FolderUtils;

import java.io.InputStream;
import java.math.BigDecimal;
import java.nio.file.Paths;

import org.apache.commons.io.IOUtils;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;

/**
 * Unit tests for the credit update procedure framework and the standard credit update procedure.
 *
 * @author X2 Development Corporation
 */
public class CreditUpdateTest extends X2DataTest {
    private static final String CREDIT_PROCEDURE_ID = "TEST-CREDIT-UPDATE";

    private static final String SCHOOL_OID = "SKLCRDTST02001";

    private static final String TRANSCRIPT_OID_1 = "TRNCRDTST0veGW"; // Final grade: 95; Possible
                                                                     // credits: 2.5
    private static final String TRANSCRIPT_OID_2 = "TRNCRDTST0veHi"; // Final grade: C; Possible
                                                                     // credits: 5.0
    private static final String TRANSCRIPT_OID_3 = "TRNCRDTST0veRr"; // Final grade: F; Possible
                                                                     // credits: 2.5
    private static final String TRANSCRIPT_OID_4 = "TRNCRDTST0veUF"; // Final grade: 25; Possible
                                                                     // credits: 5.0

    private ModelBroker m_broker;
    private SisSchool m_school;
    @Rule
    public PreferenceManagerTemporarySettings preferenceManager = new PreferenceManagerTemporarySettings();

    /**
     * Tests instantiation of a credit update procedure based on the school-visible procedure ID
     * preference.
     *
     * @throws ToolRunException exception
     * @throws InvalidPreferenceException exception
     * @throws Exception exception
     */
    @Test
    public void testInitializeProcedure() throws ToolRunException, InvalidPreferenceException, Exception {
        // Set the credit update procedure to an invalid id; try to instantiate
        preferenceManager.setPreferenceValue(m_school.getParentOrganization(),
                m_broker,
                SisPreferenceConstants.GRADES_CREDIT_UPDATE_PROCEDURE,
                "invalid-id");

        CreditUpdateProcedure procedure = CreditUpdateProcedure.initializeProcedure(new GradesManager(m_broker),
                m_school,
                AppGlobals.getRootTemporaryFolder(),
                m_broker);
        assertNull(procedure);

        // Set the credit update procedure to the corred ID; try again
        preferenceManager.setPreferenceValue(m_school.getParentOrganization(),
                m_broker,
                SisPreferenceConstants.GRADES_CREDIT_UPDATE_PROCEDURE,
                CREDIT_PROCEDURE_ID);

        procedure = CreditUpdateProcedure.initializeProcedure(new GradesManager(m_broker),
                m_school,
                AppGlobals.getRootTemporaryFolder(),
                m_broker);
        assertNotNull(procedure);
    }

    /**
     * Tests the standard credit update procedure.
     *
     * @throws InvalidPreferenceException exception
     * @throws ToolRunException exception
     * @throws Exception exception
     */
    @Test
    public void testStandardCreditUpdate() throws InvalidPreferenceException, ToolRunException, Exception {
        preferenceManager.setPreferenceValue(m_school.getParentOrganization(),
                m_broker,
                SisPreferenceConstants.GRADES_CREDIT_UPDATE_PROCEDURE,
                CREDIT_PROCEDURE_ID);

        CreditUpdateProcedure procedure = CreditUpdateProcedure.initializeProcedure(new GradesManager(m_broker),
                m_school,
                AppGlobals.getRootTemporaryFolder(),
                m_broker);

        Transcript transcript1 = (Transcript) m_broker.getBeanByOid(Transcript.class, TRANSCRIPT_OID_1);
        procedure.updateCredit(transcript1);

        BigDecimal totalCredit = null;
        BigDecimal compare = null;
        transcript1 = (Transcript) m_broker.getBeanByOid(Transcript.class, TRANSCRIPT_OID_1);
        totalCredit = transcript1.getTotalCredit();
        compare = new BigDecimal(2.5).setScale(totalCredit.scale());
        assertEquals(compare, totalCredit);

        Transcript transcript2 = (Transcript) m_broker.getBeanByOid(Transcript.class, TRANSCRIPT_OID_2);
        procedure.updateCredit(transcript2);

        transcript2 = (Transcript) m_broker.getBeanByOid(Transcript.class, TRANSCRIPT_OID_2);
        totalCredit = transcript2.getTotalCredit();
        compare = new BigDecimal(5).setScale(totalCredit.scale());
        assertEquals(compare, totalCredit);

        Transcript transcript3 = (Transcript) m_broker.getBeanByOid(Transcript.class, TRANSCRIPT_OID_3);
        procedure.updateCredit(transcript3);

        transcript3 = (Transcript) m_broker.getBeanByOid(Transcript.class, TRANSCRIPT_OID_3);
        totalCredit = transcript3.getTotalCredit();
        compare = new BigDecimal(0).setScale(totalCredit.scale());
        assertEquals(compare, totalCredit);

        Transcript transcript4 = (Transcript) m_broker.getBeanByOid(Transcript.class, TRANSCRIPT_OID_4);
        procedure.updateCredit(transcript4);

        transcript4 = (Transcript) m_broker.getBeanByOid(Transcript.class, TRANSCRIPT_OID_4);
        totalCredit = transcript4.getTotalCredit();
        compare = new BigDecimal(0).setScale(totalCredit.scale());
        assertEquals(compare, totalCredit);
    }

    /**
     * Gets the data set file.
     *
     * @return String
     * @see com.follett.fsc.core.k12.test.X2DataTest#getDataSetFile()
     */
    @Override
    protected String getDataSetFile() {
        return "creditTestDataSet.xml";
    }

    /**
     * Sets the up.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.test.X2BaseTest#setUp()
     */
    @Override
    public void setUp() throws Exception {
        super.setUp();

        InputStream stream = CreditUpdateTest.class.getResourceAsStream("/StandardCreditUpdateProcedure.java");
        Assert.assertNotNull(stream);
        
        m_broker = new ModelBroker(getPrivilegeSet());

        m_school = (SisSchool) m_broker.getBeanByOid(SisSchool.class, SCHOOL_OID);

        byte[] javaSource = IOUtils.toByteArray(stream);
        byte[] compiledSource = ByteArrayCompiler.compile(javaSource, AppGlobals.getClasspath());

        SisProcedure procedure = X2BaseBean.newInstance(SisProcedure.class, getUser().getPersistenceKey());
        ToolSourceCode sourceCode = X2BaseBean.newInstance(ToolSourceCode.class, getUser().getPersistenceKey());
        procedure.setCategory("Grades");
        procedure.setId(CREDIT_PROCEDURE_ID);
        procedure.setOrganization1Oid("*ann");
        sourceCode.setSourceCode(new String(javaSource));
        sourceCode.setCompiledCode(compiledSource);
        procedure.setWeight(1);
        saveTemporaryBeanForced(m_broker, sourceCode);
        procedure.setSourceCodeOid(sourceCode.getOid());
        saveTemporaryBeanForced(m_broker, procedure);
    }
}
