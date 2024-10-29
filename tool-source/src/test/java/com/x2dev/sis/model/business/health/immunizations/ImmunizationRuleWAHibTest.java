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
 * The Class ImmunizationRuleWAHibTest.
 */
public class ImmunizationRuleWAHibTest extends ImmunizationRuleTestMaster {

    public void setup(String today) {
        super.setup(today,"/com/x2dev/procedures/statereporting/wa/Immunizationrule-hib.xml");
    }

    /**
     * Test mukilteo twelve no doses compliance issue.
     */
    @Test
    public void testMukilteoTwelveNoDosesComplianceIssue() {
        setup("2013-10-23");
        setStudentDOBandDosesString("2001-03-17");
        evaluateStudentFromMStudentInformation(true);
        // Correct, students only need this in prek
    }

    /**
     * Test mukilteo T 20125072.
     */
    @Test
    public void testMukilteoT20125072() {
        // T20125072 - WA Immunizations: Student showing compliant when no doses of anything are
        // entered
        // 8/10/1999
        setup("2013-10-23");
        setStudentDOBandDosesString("1999-08-10");
        evaluateStudentFromMStudentInformation(true);
        // Correct, students only need this in prek
    }

    /**
     * Test student five years old no doses.
     */
    @Test
    public void testStudentFiveYearsOldNoDoses() {
        setup("2013-12-18");
        setStudentDOBandDosesString("2008-12-01");
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test ticket T 20126117 age 5 non compliant.
     */
    @Test
    public void testTicketT20126117Age5NonCompliant() {
        // 10/23/2008
        // 12/24/2008 4/8/2009 7/23/2009
        setup("2013-12-12");
        setStudentDOBandDosesString("2008-10-23, 2008-12-24, 2009-04-08, 2009-07-23");
        evaluateStudentFromMStudentInformation(true);
    }
}
