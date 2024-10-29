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
 * The Class ImmunizationRuleVAVaricellaTest.
 */
public class ImmunizationRuleVAVaricellaTest extends ImmunizationRuleTestMaster {

    public void setup(String today) {
        super.setup(today, "/com/x2dev/procedures/statereporting/va/Immunizationrule-varicella.xml");
    }

    /**
     * Test two dose pass.
     */
    @Test
    public void testTwoDosePass() {
        // DOB: 2002, dose 1: 2005, dose 2: 2007

        setup("2008-09-15");
        setStudentDOBandDosesString("2002-11-02, 2005-09-15, 2007-09-15");
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test no dose fails.
     */
    @Test
    public void testNoDoseFails() {
        setup("2008-09-15");
        setStudentDOBandDosesString("2002-11-02");
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test one dose fails.
     */
    @Test
    public void testOneDoseFails() {
        setup("2008-09-15");
        setStudentDOBandDosesString("2002-11-02, 2007-09-15");
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test three dose fails.
     */
    @Test
    public void testThreeDoseFails() {
        setup("2008-09-15");
        setStudentDOBandDosesString("2002-11-02, 2005-09-15, 2007-09-15, 2008-09-15");
        evaluateStudentFromMStudentInformation(true);
    }


    /**
     * Test outside first dose fails.
     */
    @Test
    public void testOutsideFirstDoseFails() {
        setup("2008-09-15");
        setStudentDOBandDosesString("2002-11-02, 2009-09-15, 2010-09-15"); // 1st dose late
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test outside second dose fails.
     */
    @Test
    public void testOutsideSecondDoseFails() {
        setup("2008-09-15");
        setStudentDOBandDosesString("2002-11-02, 2005-09-15, 2010-09-15"); // 2nd dose late
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test outside both dose fails.
     */
    @Test
    public void testOutsideBothDoseFails() {
        setup("2008-09-15");
        setStudentDOBandDosesString("2002-11-02, 2010-09-15, 2011-09-15"); // both dose late
        evaluateStudentFromMStudentInformation(false);
    }
}
