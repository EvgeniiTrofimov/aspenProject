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
 * The Class ImmunizationRuleVAMMRTest.
 */
public class ImmunizationRuleVAMMRTest extends ImmunizationRuleTestMaster {

    public void setup(String today) {
        super.setup(today, "/com/x2dev/procedures/statereporting/va/Immunizationrule-mmr.xml");
    }

    /**
     * Test doses K pass.
     */
    @Test
    public void testDosesKPass() {
        setup("2008-09-15");
        setStudentDOBandDosesString("2002-11-02, 2007-09-15, 2005-09-15, 2006-09-15"); // student
        // all the
        // dose 2
        // months
        // before
        // their 6th
        // bday
        evaluateStudentFromMStudentInformation(true); // Should pass, its within K
    }

    /**
     * Test no dose K fails.
     */
    @Test
    public void testNoDoseKFails() {
        setup("2008-09-15");

        setStudentDOBandDosesString("2002-11-02");
        evaluateStudentFromMStudentInformation(false); // Should not pass, not enough doses
    }

    /**
     * Test one dose K fails.
     */
    @Test
    public void testOneDoseKFails() {
        setup("2008-09-15");
        setStudentDOBandDosesString("2002-11-02, 2007-09-15");
        evaluateStudentFromMStudentInformation(false); // Should not pass, not enough doses
    }


    /**
     * Test two doses K fails.
     */
    @Test
    public void testTwoDosesKFails() {
        setup("2008-09-15");
        setStudentDOBandDosesString("2002-11-02, 2007-09-15, 2005-09-15");
        evaluateStudentFromMStudentInformation(true); // Should pass, its within K
    }

    /**
     * Test one doses grade 6 pass.
     */
    @Test
    public void testOneDosesGrade6Pass() {
        setup("2014-09-15");
        setStudentDOBandDosesString("2002-11-02, 2009-09-15");
        evaluateStudentFromMStudentInformation(true); // Should pass, its within K
    }

    /**
     * Test one doses grade 6 fail.
     */
    @Test
    public void testOneDosesGrade6Fail() {
        setup("2014-09-15");
        setStudentDOBandDosesString("2008-11-02, 2009-09-15");
        evaluateStudentFromMStudentInformation(false); // fail as age is 4
    }
}
