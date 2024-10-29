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
 * The Class ImmunizationRuleNYDTAPTest.
 */
public class ImmunizationRuleNYDTAPTest extends ImmunizationRuleTestMaster {

    public void setup(String today) {
        super.setup(today, "/com/x2dev/procedures/statereporting/ny/Immunizationrule-dtap.xml");
    }

    /**
     * Test pre kand below.
     */
    @Test
    public void testPreKandBelow() {
        setup("2014-07-26");

        // Student with DOB 2009-07-01 is in PK grade based on test assumptions
        setStudentDOBandDosesString(
                "2009-07-01, 2009-09-01:DTaP-DTP, 2009-11-01:DTaP-DTP, 2010-01-01:DTaP-DTP, 2010-10-01:DTaP-DTP");
        evaluateStudentFromMStudentInformation(true);

        setStudentDOBandDosesString("2009-07-01, 2009-09-01:DTaP-DTP, 2009-11-01:DTaP-DTP, 2010-01-01:DTaP-DTP");
        evaluateStudentFromMStudentInformation(false); // not enough doses

        setStudentDOBandDosesString(
                "2009-07-01, 2009-09-01:DTaP-DTP, 2009-11-01:DTaP-DTP, 2010-01-01:DTaP-DTP, 2010-10-01:Tdap");
        evaluateStudentFromMStudentInformation(false); // incorrect series
    }

    /**
     * Test K.
     */
    @Test
    public void testK() {
        setup("2014-07-26");

        // Student with DOB 2008-07-01 is in K grade based on test assumptions
        setStudentDOBandDosesString(
                "2008-07-01, 2008-09-01:DTaP-DTP, 2008-11-01:DTaP-DTP, 2009-01-01:DTaP-DTP, 2009-10-01:DTaP-DTP, 2011-10-01:DTaP-DTP");
        evaluateStudentFromMStudentInformation(true); // 5 doses

        setStudentDOBandDosesString(
                "2008-07-01, 2008-09-01:DTaP-DTP, 2008-11-01:DTaP-DTP, 2009-01-01:DTaP-DTP, 2012-10-01:DTaP-DTP");
        evaluateStudentFromMStudentInformation(true); // 4 doses if 4th dose was received at age 4

        setStudentDOBandDosesString(
                "2008-07-01, 2008-09-01:DTaP-DTP, 2008-11-01:DTaP-DTP, 2009-01-01:DTaP-DTP, 2012-06-30:DTaP-DTP");
        evaluateStudentFromMStudentInformation(false); // 4th dose not received by age 4
    }

    /**
     * Test 1 to 5.
     */
    @Test
    public void test1to5() {
        setup("2014-07-26");

        // Student with DOB 2006-07-01 is in grade 2 based on test assumptions
        setStudentDOBandDosesString(
                "2006-07-01, 2006-09-01:DTaP-DTP, 2006-11-01:DTaP-DTP, 2007-01-01:DTaP-DTP, 2007-10-01:DTaP-DTP, 2009-10-01:DTaP-DTP");
        evaluateStudentFromMStudentInformation(true); // 5 doses

        setStudentDOBandDosesString(
                "2006-07-01, 2006-09-01:DT, 2006-11-01:DTaP-DTP, 2007-01-01:DTaP-DTP, 2007-10-01:DTaP-DTP, 2009-10-01:DTaP-DTP");
        evaluateStudentFromMStudentInformation(false); // 5 doses; DT not allowed for this DOB

        // Student with DOB 2004-12-30 is in 4 grade based on test assumptions
        setStudentDOBandDosesString(
                "2004-12-30, 2006-09-01:DT, 2006-11-01:DTaP-DTP, 2007-01-01:DTaP-DTP, 2007-10-01:DTaP-DTP, 2009-10-01:DTaP-DTP");
        evaluateStudentFromMStudentInformation(true); // 5 doses; DT is allowed for this DOB

        setStudentDOBandDosesString(
                "2006-07-01, 2006-09-01:DTaP-DTP, 2006-11-01:DTaP-DTP, 2007-01-01:DTaP-DTP, 2010-10-01:DTaP-DTP");
        evaluateStudentFromMStudentInformation(true); // 4 doses if 4th dose was received at age 4

        setStudentDOBandDosesString(
                "2006-07-01, 2006-09-01:DTaP-DTP, 2006-11-01:DTaP-DTP, 2007-01-01:DTaP-DTP, 2007-06-30:DTaP-DTP");
        evaluateStudentFromMStudentInformation(false); // 4th dose not received by age 4
    }

    /**
     * Test 6 to 12.
     */
    @Test
    public void test6to12() {
        setup("2014-07-26");

        // Student with DOB 2001-12-30 is in grade 7 based on test assumptions
        setStudentDOBandDosesString(
                "2001-12-30, 2001-09-01:DT, 2001-11-01:DTaP-DTP, 2002-01-01:DTaP-DTP, 2002-10-01:DTaP-DTP, 2004-10-01:DTaP-DTP");
        evaluateStudentFromMStudentInformation(true); // 5 doses; DT is allowed for this DOB

        setStudentDOBandDosesString(
                "2001-12-30, 2001-09-01:DT, 2001-11-01:DTaP-DTP, 2002-01-01:DTaP-DTP, 2005-12-30:DTaP-DTP");
        evaluateStudentFromMStudentInformation(true); // 4 doses if 4th dose was received at age 4

        setStudentDOBandDosesString(
                "2001-12-30, 2001-09-01:DT, 2001-11-01:DTaP-DTP, 2002-01-01:DTaP-DTP, 2005-12-29:DTaP-DTP");
        evaluateStudentFromMStudentInformation(false); // 4th dose not received by age 4

        // "Catch up" series
        setStudentDOBandDosesString("2001-12-30, 2009-01-01:Tdap, 2010-01-01:Td, 2011-02-01:Td");
        evaluateStudentFromMStudentInformation(true);

        setStudentDOBandDosesString("2001-12-30, 2009-01-01:Td, 2010-01-01:Td, 2011-02-01:Td");
        evaluateStudentFromMStudentInformation(false); // 1st dose should be Tdap

        setStudentDOBandDosesString("2001-12-30, 2009-01-01:Tdap, 2010-01-01:Tdap, 2011-02-01:Td");
        evaluateStudentFromMStudentInformation(false); // 2nd dose should be Td

        setStudentDOBandDosesString("2001-12-30, 2009-01-01:Tdap, 2010-01-01:Td, 2011-02-01:Tdap");
        evaluateStudentFromMStudentInformation(false); // 3rd dose should be Td

        setStudentDOBandDosesString("2001-12-30, 2008-12-29:Tdap, 2010-01-01:Td, 2011-02-01:Td");
        evaluateStudentFromMStudentInformation(false); // 1st dose must start at age 7

    }

    //
    // @Test
    // public void testMissingPreKDoseZeroFails()
    // {
    // //Today is in preK
    // setup("2008-09-15");
    // setStudentDOBandDosesString("2002-11-02");
    // evaluateStudentFromMStudentInformation(false); //Should not pass, not enough doses
    // }
    //
    // @Test
    // public void testMissingPreKDoseOneFails()
    // {
    // //Today is in preK
    // setup("2008-09-15");
    // setStudentDOBandDosesString("2002-11-02, 2004-09-15");
    // evaluateStudentFromMStudentInformation(false); //Should not pass, not enough doses
    // }
    //
    // @Test
    // public void testMissingPreKDoseTwoFails()
    // {
    // //Today is in preK
    // setup("2008-09-15");
    // setStudentDOBandDosesString("2002-11-02, 2004-09-15, 2005-09-15");
    // evaluateStudentFromMStudentInformation(false); //Should not pass, not enough doses
    // }
    //
    // @Test
    // public void testMissingPreKDoseThreePasses()
    // {
    // //Today is in preK
    // setup("2008-09-15");
    // setStudentDOBandDosesString("2002-11-02, 2004-09-15, 2005-09-15, 2006-09-15");
    // evaluateStudentFromMStudentInformation(true); //Should pass, enough doses for preK
    // }
    //
    // @Test
    // public void testMissingKTwelveNoDoseFails()
    // {
    // //Today is in k-12
    // setup("2010-09-15");
    // setStudentDOBandDosesString("2002-11-02");
    // evaluateStudentFromMStudentInformation(false); //Should not pass, not enough doses
    // }
    //
    // @Test
    // public void testMissingKTwelveOneDoseFails()
    // {
    // //Today is in k-12
    // setup("2010-09-15");
    // setStudentDOBandDosesString("2002-11-02, 2009-09-15");
    // evaluateStudentFromMStudentInformation(false); //Should not pass, not enough doses
    // }
    //
    // @Test
    // public void testMissingKTwelveTwoDoseFails()
    // {
    // //Today is in k-12
    // setup("2010-09-15");
    // setStudentDOBandDosesString("2002-11-02, 2009-09-01, 2010-09-08");
    // evaluateStudentFromMStudentInformation(false); //Should not pass, not enough doses
    // }
    //
    // @Test
    // public void testKTwelveThreeDosesPass()
    // {
    // //Today is in k-12
    // setup("2010-09-15");
    // setStudentDOBandDosesString("2002-11-02, 2008-09-15, 2009-09-15, 2010-06-05");
    // evaluateStudentFromMStudentInformation(true); //Should pass, three acceptable doses
    // }
}
