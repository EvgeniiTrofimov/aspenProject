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
 * The Class ImmunizationRuleVAHepBTest.
 */
public class ImmunizationRuleVAHepBTest extends ImmunizationRuleTestMaster {
    private static final String TODAY = "2014-09-15";

    public void setup(String today) {
        super.setup(today, "/com/x2dev/procedures/statereporting/va/Immunizationrule-hepb.xml");
    }

    /**
     * Test three dose pass.
     */
    @Test
    public void testThreeDosePass() {
        setup(TODAY);

        setStudentDOBandDosesString("2010-06-15, 2011-06-15, 2012-06-15, 2013-06-15");
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test no dose fails.
     */
    @Test
    public void testNoDoseFails() {
        setup(TODAY);
        setStudentDOBandDosesString("2010-06-15");
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test one dose fails.
     */
    @Test
    public void testOneDoseFails() {
        setup(TODAY);
        setStudentDOBandDosesString("2010-06-15, 2011-06-15");
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test two fails.
     */
    @Test
    public void testTwoFails() {
        setup(TODAY);
        setStudentDOBandDosesString("2010-06-15, 2011-06-15, 2012-06-15");
        evaluateStudentFromMStudentInformation(false);
    }

    // @Test
    // public void test2Dose11to15Pass()
    // {
    // setup("1999-06-15");
    // setStudentDOBandDosesString("1999-06-15, 2012-06-15, 2013-06-15");
    // evaluateStudentFromMStudentInformation(true);
    // }


}
