/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.sis.model.business.health.immunizations;

import org.junit.Test;


/**
 * The Class ImmunizationRuleNYMMRTest.
 */
public class ImmunizationRuleNYMMRTest extends ImmunizationRuleTestMaster {

    public void setup(String today) {
        super.setup(today, "/com/x2dev/procedures/statereporting/ny/Immunizationrule-mmr.xml");
    }

    /**
     * Test dose pre K one.
     */
    @Test
    public void testDosePreKOne() {
        setup("2008-09-15");
        setStudentDOBandDosesString("2002-11-02, 2007-09-15"); // student got a dose 2 months before
        // their 6th bday
        evaluateStudentFromMStudentInformation(true); // Should pass, its within preK
    }

    /**
     * Test missing pre K dose one fails.
     */
    @Test
    public void testMissingPreKDoseOneFails() {
        // Today is in preK
        setup("2008-09-15");
        setStudentDOBandDosesString("2002-11-02");
        evaluateStudentFromMStudentInformation(false); // Should not pass, not enough doses
    }

    /**
     * Test missing K twelve no dose fails.
     */
    @Test
    public void testMissingKTwelveNoDoseFails() {
        // Today is in k-12
        setup("2010-09-15");
        setStudentDOBandDosesString("2002-11-02");
        evaluateStudentFromMStudentInformation(false); // Should not pass, not enough doses
    }

    /**
     * Test missing K twelve one dose fails.
     */
    @Test
    public void testMissingKTwelveOneDoseFails() {
        // Today is in k-12
        setup("2010-09-15");
        setStudentDOBandDosesString("2002-11-02, 2009-09-15");
        evaluateStudentFromMStudentInformation(false); // Should not pass, not enough doses
    }

    /**
     * Test missing K twelve two dose fails.
     */
    @Test
    public void testMissingKTwelveTwoDoseFails() {
        // Today is in k-12
        setup("2010-09-15");
        setStudentDOBandDosesString("2002-11-02, 2009-09-01, 2010-09-08");
        evaluateStudentFromMStudentInformation(true); // Should pass, two doses is fine, three is
        // preferred.
    }

    /**
     * Test K twelve three doses pass.
     */
    @Test
    public void testKTwelveThreeDosesPass() {
        // Today is in k-12
        setup("2010-09-15");
        setStudentDOBandDosesString("2002-11-02, 2008-09-15, 2009-09-15, 2010-06-05");
        evaluateStudentFromMStudentInformation(true); // Should pass, three acceptable doses
    }

    /**
     * Test student got dosesin prek and elem.
     */
    @Test
    public void testStudentGotDosesinPrekAndElem() {
        setup("2014-01-06");
        parseDoses("2004-01-06", new int[] {365, 365 * 7, 365}); // Setup one dose in prek (one year
        // old), two in k-12 (8 and 9 years
        // old)
        evaluateStudentFromMStudentInformation(true); // Should pass, three acceptable doses
    }
}
