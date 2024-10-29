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
 * The Class ImmunizationRuleWAHepBTest.
 */
public class ImmunizationRuleWAHepBTest extends ImmunizationRuleTestMaster {

    public void setup(String today) {
        super.setup(today, "/com/x2dev/procedures/statereporting/wa/Immunizationrule-hepb.xml");
    }

    /**
     * Test minimum time between dose two and three.
     */
    @Test
    public void testMinimumTimeBetweenDoseTwoAndThree() {
        // T20124777 - WA Immunizations: Hep B: Student showing compliant when not meeting minimum
        // time between doses
        /*
         * dob 6/14/2007
         * Hep B
         * � dose1 09/05/2007-age: 2.7 months/11.9 weeks/83 days
         * � dose2 10/31/2007-age: 4.6 months/19.9 weeks/139 days
         * o time between dose1 and dose 2=1.8 months/8 weeks/56 days
         * � dose3 12/18/2007-age: 6.1 months/26.7 weeks/187 days
         * o time between dose 2 and dose 3=1.6 months/6.9 weeks/48 days
         * o time between dose 1 and dose 3=3.4 months/14.9 weeks/104 days
         */
        setup("2013-10-10");
        setStudentDOBandDosesString("2007-06-14, 2007-09-05, 2007-10-31, 2007-12-18, 2012-06-07");
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Student showing as compliant with no doses.
     */
    @Test
    public void studentShowingAsCompliantWithNoDoses() {
        // 9/25/2002
        // T20124826 - WA Immunizations: Student showing compliant for Hep B when no doses
        setup("2013-10-15");
        setStudentDOBandDosesString("2002-09-25");
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Ticket 20123996.
     */
    @Test
    public void ticket20123996() {
        // T20123996 - Immunization: Showing Hepatitis Compliant when student is not/Showing Non
        // compliant when student is
        // Bellevue district -Student Ulrich, Megan at Puesta School-all shots before 6 months of
        // age-student showing compliant, should not be.
        setup("2013-10-15");
        setStudentDOBandDosesString("2007-10-14, 2007-10-14, 2007-12-21, 2008-02-26, 2014-04-10");
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Ticket 20123996 part 2.
     */
    @Test
    public void ticket20123996Part2() {
        // T20123996 - Immunization: Showing Hepatitis Compliant when student is not/Showing Non
        // compliant when student is
        setup("2013-10-29");
        setStudentDOBandDosesString("2006-01-08, 2006-03-22, 2006-05-03, 2006-06-19");
        evaluateStudentFromMStudentInformation(false);
        // 1/8/2006
        // 3/22/2006 5/3/2006 6/19/2006
    }

    /**
     * Test ticket T 20125223 student has no doses false complaince.
     */
    @Test
    public void testTicketT20125223StudentHasNoDosesFalseComplaince() {
        setup("2013-11-06");
        setStudentDOBandDosesString("2001-08-29");
        evaluateStudentFromMStudentInformation(false);
    }
}
