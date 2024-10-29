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
 * The Class ImmunizationRuleNYPolioTest.
 */
public class ImmunizationRuleNYPolioTest extends ImmunizationRuleTestMaster {

    public void setup(String today) {
        super.setup(today, "/com/x2dev/procedures/statereporting/ny/Immunizationrule-polio.xml");
    }

    /**
     * PreK k-12
     * Polio (IPV or OPV) 3 doses* 3 doses
     *
     * Please note at this time that New York State requires 3 doses of diphtheria toxoid-containing
     * vaccine (New York City requires 4 doses for pre-kindergarten
     * and kindergarten only) and three doses of polio vaccine for entry into kindergarten and for
     * any student entering a school in New York State for the first time.
     * However, ACIP recommends 4 doses of diphtheria toxoid-containing vaccine by age 18 months and
     * 5 doses by age 4-6 years of age. Children 4-6 years of age
     * should receive 4 doses of polio vaccine unless the 3rd dose is given after 4 years of age.
     *
     */

    @Test
    public void testMissingPreKDoseZeroFails() {
        // Today is in preK
        setup("2008-09-15");
        setStudentDOBandDosesString("2002-11-02");
        evaluateStudentFromMStudentInformation(false); // Should not pass, not enough doses
    }

    /**
     * Test missing pre K dose one fails.
     */
    @Test
    public void testMissingPreKDoseOneFails() {
        // Today is in preK
        setup("2008-09-15");
        setStudentDOBandDosesString("2002-11-02, 2004-09-15");
        evaluateStudentFromMStudentInformation(false); // Should not pass, not enough doses
    }

    /**
     * Test missing pre K dose two fails.
     */
    @Test
    public void testMissingPreKDoseTwoFails() {
        // Today is in preK
        setup("2008-09-15");
        setStudentDOBandDosesString("2002-11-02, 2004-09-15, 2005-09-15");
        evaluateStudentFromMStudentInformation(false); // Should not pass, not enough doses
    }

    /**
     * Test missing pre K dose three passes.
     */
    @Test
    public void testMissingPreKDoseThreePasses() {
        // Today is in preK
        setup("2007-09-15");
        setStudentDOBandDosesString("2002-11-02, 2004-09-15, 2005-09-15, 2006-09-15");
        evaluateStudentFromMStudentInformation(true); // Should pass, enough doses for preK
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
        evaluateStudentFromMStudentInformation(false); // Should not pass, not enough doses
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
     * Test for ticket T 20125045.
     */
    @Test
    public void testForTicketT20125045() {
        setup("2013-10-23");
        setStudentDOBandDosesString("1996-05-15, 1996-07-19, 1996-09-20, 1998-03-31, 2002-11-10");
        evaluateStudentFromMStudentInformation(true); // Should pass, three acceptable doses
    }
}
