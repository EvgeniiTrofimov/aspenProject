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
 * The Class ImmunizationRuleNYTDAPTest.
 */
public class ImmunizationRuleNYTDAPTest extends ImmunizationRuleTestMaster {

    public void setup(String today) {
        super.setup(today, "/com/x2dev/procedures/statereporting/ny/Immunizationrule-tdap.xml");
    }

    /**
     * Test age 4 passes with no doses.
     */
    @Test
    public void testAge4PassesWithNoDoses() {
        setup("2013-10-21");
        setStudentDOBandDosesString("2009-08-20"); // student is 4 - no doses
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test age 6 passes with no doses.
     */
    @Test
    public void testAge6PassesWithNoDoses() {
        setup("2013-10-21");
        setStudentDOBandDosesString("2007-08-20"); // student is 6 - no doses
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test age 7 fails with no doses.
     */
    @Test
    public void testAge7FailsWithNoDoses() {
        setup("2013-10-21");
        setStudentDOBandDosesString("2006-08-20"); // student is 7 - no doses
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test age 9 fails with no doses.
     */
    @Test
    public void testAge9FailsWithNoDoses() {
        setup("2013-10-21");
        setStudentDOBandDosesString("2004-08-20"); // student is 9 - no doses
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test one dose at age 6 is age 6 pass.
     */
    @Test
    public void testOneDoseAtAge6IsAge6Pass() {
        setup("2013-10-21");
        setStudentDOBandDosesString("2007-08-20", "2012-09-20"); // student is 6 - one dose at age 6
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test one dose at age 6 is age 7 fail.
     */
    @Test
    public void testOneDoseAtAge6IsAge7Fail() {
        setup("2013-10-21");
        setStudentDOBandDosesString("2006-08-20", "2012-09-20"); // student is 7 - one dose at age 6
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test one dose at age 6 is age 9 fail.
     */
    @Test
    public void testOneDoseAtAge6IsAge9Fail() {
        setup("2013-10-21");
        setStudentDOBandDosesString("2004-08-20", "2010-09-20"); // student is 9 - one dose at age 6
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test one dose at age 7 is age 7 pass.
     */
    @Test
    public void testOneDoseAtAge7IsAge7Pass() {
        setup("2013-10-21");
        setStudentDOBandDosesString("2006-08-20", "2013-09-20"); // student is 7 - one dose at age 7
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test one dose at age 9 is age 9 pass.
     */
    @Test
    public void testOneDoseAtAge9IsAge9Pass() {
        setup("2013-10-21");
        setStudentDOBandDosesString("2004-08-20", "2013-09-20"); // student is 9 - one dose at age 9
        evaluateStudentFromMStudentInformation(true);
    }
}
