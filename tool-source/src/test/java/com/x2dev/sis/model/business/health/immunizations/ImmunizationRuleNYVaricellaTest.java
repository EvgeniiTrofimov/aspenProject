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
 * The Class ImmunizationRuleNYVaricellaTest.
 */
public class ImmunizationRuleNYVaricellaTest extends ImmunizationRuleTestMaster {

    public void setup(String today) {
        super.setup(today, "/com/x2dev/procedures/statereporting/ny/Immunizationrule-varicella.xml");
    }

    /**
     * Test dose one.
     */
    @Test
    public void testDoseOne() {
        setup(null);
        setStudentDOBandDosesString("2002-11-02, 2008-09-15"); // student got a dose 2 months before
        // their 6th bday
        evaluateStudentFromMStudentInformation(true); // Should pass, its within preK
        setStudentDOBandDosesString("2002-11-02, 2009-12-15"); // Student got a dose 1 month AFTER
        // their 7th bday
        evaluateStudentFromMStudentInformation(true); // Should pass, student has one dose in k-12
    }

    /**
     * Test missing pre K dose one fails.
     */
    @Test
    public void testMissingPreKDoseOneFails() {
        // Today is in preK
        setup("2008-09-15");
        setStudentDOBandDosesString("2002-11-02");
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test missing K twelve dose one fails.
     */
    @Test
    public void testMissingKTwelveDoseOneFails() {
        // Today is in k-12
        setup("2010-09-15");
        setStudentDOBandDosesString("2002-11-02");
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test fail comsewogue one dose oct twenty one.
     */
    @Test
    public void testFailComsewogueOneDoseOctTwentyOne() {
        setup("2013-10-21");
        setStudentDOBandDosesString("1997-07-22, 1998-09-02");
        evaluateStudentFromMStudentInformation(true); // Should pass
    }
}
