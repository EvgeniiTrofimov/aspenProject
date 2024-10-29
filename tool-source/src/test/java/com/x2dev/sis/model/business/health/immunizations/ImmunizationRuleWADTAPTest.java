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
 * The Class ImmunizationRuleWADTAPTest.
 */
public class ImmunizationRuleWADTAPTest extends ImmunizationRuleTestMaster {

    public void setup(String today) {
        super.setup(today,"/com/x2dev/procedures/statereporting/wa/Immunizationrule-dtap.xml");
    }

    /**
     * Test mukilteo pre K non compliance issue.
     */
    @Test
    public void testMukilteoPreKNonComplianceIssue() {
        // T20124829 - WA Immunizations: ALL preschoolers showing non-compliant that are
        // compliant-Polio and dtap
        setup("2013-10-17");
        setStudentDOBandDosesString("2010-01-25, 2010-03-26, 2010-05-28, 2010-07-30, 2011-08-12");
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test fails on zero doses.
     */
    @Test
    public void testFailsOnZeroDoses() {
        // T20125035 - WA Immunizations: Student showing Compliant for Tdap when they need booster
        // shot
        setup("2013-10-22");
        setStudentDOBandDosesString("2002-02-27");
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test student 4 doses ticket T 20125007.
     */
    @Test
    public void testStudent4DosesTicketT20125007() {
        // T20125007 - WA Immunizations: Student showing non-compliant for DTap student is
        // Bellevue, Lake Hills school, student Worku, Meron. DTap that SHOULD be �compliant�.
        // Dose #4 was given after age 4 (dob was 12/29/03)
        // 12/29/2003
        // 2/10/2004 3/1/2004 3/29/2004 9/29/2009
        setup("2013-10-22");
        setStudentDOBandDosesString("2003-12-29, 2004-02-10, 2004-03-01, 2004-03-29, 2009-09-29");
        evaluateStudentFromMStudentInformation(false);
        // Dose 1 and two are too close! Test should fail!
    }

    /**
     * Test student with four doses is ok T 20125042.
     */
    @Test
    public void testStudentWithFourDosesIsOkT20125042() {
        // T20125042 - WA Immunizations: Tdap: Cannot understand why student is non compliant
        // 1/8/2003
        // 4/17/2007 7/2/2007 4/17/2008 9/12/2008
        setup("2013-10-22");
        setStudentDOBandDosesString("2003-01-08, 2007-04-17, 2007-07-02, 2008-04-17, 2008-09-12");
        evaluateStudentFromMStudentInformation(false);
        // 6 months in between 3rd and 4th dose, not there
    }

    /**
     * Test student for fifth dose in sixth or higher fails without.
     */
    @Test
    public void testStudentForFifthDoseInSixthOrHigherFailsWithout() {
        setup("2013-10-30");
        parseIntIntoStudentAndDoses(7, new int[] {40, 70, 100, 1600});
        evaluateRule(false);
    }

    /**
     * Test student for fifth dose in sixth or higher passes with.
     */
    @Test
    public void testStudentForFifthDoseInSixthOrHigherPassesWith() {
        setup("2013-10-30");
        parseIntIntoStudentAndDoses(7, new int[] {40, 70, 180, 1310, 2100});
        evaluateRule(true);
    }

    /**
     * Test student for sixth dose in sixth or higher fails without.
     */
    @Test
    public void testStudentForSixthDoseInSixthOrHigherFailsWithout() {
        setup("2013-10-30");
        setStudentDOBandDosesString(
                "2000-02-27, 2000-05-09, 2000-07-11, 2000-09-12, 2001-05-29, 2003-03-30, 2004-03-30");// ,
        // 2013-10-14");
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test student for sixth dose in sixth or higher passes with.
     */
    @Test
    public void testStudentForSixthDoseInSixthOrHigherPassesWith() {
        setup("2013-10-30");
        parseIntIntoStudentAndDoses(7, new int[] {40, 70, 255, 365, 750, 2195});
        evaluateRule(true);
    }

    /**
     * Test student for 5 th grade dosent need booster.
     */
    @Test
    public void testStudentFor5thGradeDosentNeedBooster() {
        setup("2013-10-30");
        parseIntIntoStudentAndDoses(5, new int[] {40, 70, 255, 365, 750});
        evaluateRule(true);
    }

    /**
     * Test student for 5 th grade dosent need booster as old as possible.
     */
    @Test
    public void testStudentFor5thGradeDosentNeedBoosterAsOldAsPossible() {
        setup("2013-10-30");
        parseDoses("2002-09-01", new int[] {40, 70, 255, 365, 750});
        evaluateRule(true);
    }

    /**
     * Test basic elementary school student with compliance.
     */
    @Test
    public void testBasicElementarySchoolStudentWithCompliance() {
        // Real data
        // 7/30/2005
        // 10/19/2005 1/9/2006 10/10/2006 12/11/2006 12/14/2009
        setup("2013-10-30");
        setStudentDOBandDosesString("2005-07-30, 2005-10-19, 2006-01-09, 2006-10-10, 2006-12-11, 2009-12-14");
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test basic high school student with compliance.
     */
    @Test
    public void testBasicHighSchoolStudentWithCompliance() {
        // Real data
        // 8/21/1996
        // 10/22/1996 12/20/1996 2/21/1997 1/8/1998 3/15/2001 5/9/2007
        setup("2013-10-31");
        setStudentDOBandDosesString(
                "1996-08-21, 1996-10-22, 1996-12-20, 1997-02-21, 1998-01-08, 2001-03-15, 2007-05-09");
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test basic high school student with compliance 2.
     */
    @Test
    public void testBasicHighSchoolStudentWithCompliance2() {
        // Real data
        // 12/31/1997
        // 3/12/1998 4/30/1998 7/2/1998 1/5/1999 4/7/2003 4/7/2009
        setup("2013-10-31");
        setStudentDOBandDosesString(
                "1997-12-31, 1998-03-12, 1998-04-30, 1998-07-02, 1999-01-05, 2003-04-07, 2009-04-07");
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test basic high school student without compliance 3.
     */
    @Test
    public void testBasicHighSchoolStudentWithoutCompliance3() {
        // Real data
        // 2/23/1999
        // 4/4/1999 7/12/1999 1/23/2000 1/28/2002 9/10/2008
        setup("2013-10-31");
        setStudentDOBandDosesString("1999-02-23, 1999-04-04, 1999-07-12, 2000-01-23, 2002-01-28, 2008-09-10");
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test elem student.
     */
    @Test
    public void testElemStudent() {
        // Real Data
        // 8/4/2009
        // 10/8/2009 12/8/2009 2/4/2010 11/4/2010
        setup("2013-10-31");
        setStudentDOBandDosesString("2009-08-04, 2009-10-08, 2009-12-08, 2010-02-04, 2010-11-04");
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test elem student 2.
     */
    @Test
    public void testElemStudent2() {
        // Real Data
        // 6/29/2007
        // 9/6/2007 11/9/2007 1/9/2008 7/11/2008 11/20/2008
        setup("2013-10-31");
        setStudentDOBandDosesString("2007-06-29, 2007-09-06, 2007-09-11, 2008-01-09, 2008-07-11, 2008-11-20");
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test elem student 3.
     */
    @Test
    public void testElemStudent3() {
        // Real Data
        // 12/6/2006
        // 3/12/2007 9/5/2012 1/11/2013
        setup("2013-10-31");
        setStudentDOBandDosesString("2006-12-06, 2007-03-12, 2012-09-05, 2013-01-11");
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test elem student 4.
     */
    @Test
    public void testElemStudent4() {
        // Real Data
        // 7/6/2007
        // 10/10/2007 12/12/2007 3/4/2008 7/7/2010 8/28/2012
        setup("2013-10-31");
        setStudentDOBandDosesString("2007-07-06, 2007-10-10, 2007-12-12, 2008-03-04, 2010-07-07, 2012-08-28");
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test sixth grader.
     */
    @Test
    public void testSixthGrader() {
        setup("2013-10-31");
        parseIntIntoStudentAndDoses(6, new int[] {40, 70, 100, 400, 1000});
        evaluateRule(false);
    }

    /**
     * Pre school compliance issue.
     */
    @Test
    public void preSchoolComplianceIssue() {
        // Real Data
        // 8/15/2009
        // 10/20/2009 12/22/2009 3/3/2011 9/16/2011
        setup("2013-11-01");
        setStudentDOBandDosesString("2009-08-15, 2009-10-20, 2009-12-22, 2011-03-03, 2011-09-16");
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Pre school compliance issue 2.
     */
    @Test
    public void preSchoolComplianceIssue2() {
        // Real Data
        // 9/30/2008
        // 12/2/2008 2/10/2009 4/14/2009 3/16/2010
        setup("2013-11-01");
        setStudentDOBandDosesString("2008-09-30, 2008-12-02, 2009-03-10, 2009-04-14, 2010-03-16");
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test student with multiple booster shots.
     */
    @Test
    public void testStudentWithMultipleBoosterShots() {
        setup("2013-11-01");
        parseIntIntoStudentAndDoses(12, new int[] {40, 70, 255, 365, 750, 2195, 365});
        evaluateRule(true);
    }

    /**
     * Middle school compliance issue.
     */
    @Test
    public void middleSchoolComplianceIssue() {
        setup("2013-11-06");
        setStudentDOBandDosesString(
                "1999-10-05, 1999-12-06, 2000-02-07, 2000-04-06, 2001-01-06, 2003-11-11, 2005-02-11, 2011-10-25");
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Middle school complaince issue 2.
     */
    @Test
    public void middleSchoolComplainceIssue2() {
        setup("2013-11-06");
        // 10/10/1999
        // 1/19/2000 4/20/2000 10/4/2000 5/3/2001 8/13/2004 10/31/2007 10/24/2011
        setStudentDOBandDosesString(
                "1999-10-10, 2000-01-19, 2000-04-20, 2000-10-04, 2001-05-03, 2004-08-13, 2007-10-31, 2011-10-24");
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test ticket T 20125491.
     */
    @Test
    public void testTicketT20125491() {
        setup("2013-11-22");
        // 02/03/08
        // 02/04/08 03/05/08 07/06/08 05/08/09 02/13/2012
        setStudentDOBandDosesString("2008-02-03, 2008-02-04, 2008-03-05, 2008-07-06, 2009-05-08, 2012-02-13");
        evaluateStudentFromMStudentInformation(false);
        // Minimum age for the first dose is 38 days. (6 weeks plus 4 day grace period.)
    }

    /**
     * Test ticket T 20125490.
     */
    @Test
    public void testTicketT20125490() {
        setup("2013-11-22");
        // 12/13/2004
        // 8/26/2010 12/14/2010 01/09/2012
        setStudentDOBandDosesString("2004-12-13, 2010-08-26, 2010-12-14, 2012-01-09");
        evaluateStudentFromMStudentInformation(false);
        // Catch up schedule is not handled. These students need to be marked manually.
    }

    /**
     * Test ticket T 20125492.
     */
    @Test
    public void testTicketT20125492() {
        setup("2013-11-22");
        // 1/6/2000
        // 3/13/2000 5/8/2000 3/24/2005 9/6/2011
        setStudentDOBandDosesString("2000-01-06, 2000-03-13, 2000-05-08, 2005-03-24, 2011-09-06");
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test ticket T 20125912 fifth grade student compliance.
     */
    @Test
    public void testTicketT20125912FifthGradeStudentCompliance() {
        setup("2013-11-27");
        // 05/14/2002
        //
        // 1---9/7/2002 -3.8 months
        // 1to2 6.4 weeks
        // 2---10/22/2002- 5.3 months
        // 2to3 11.3 weeks
        // 3---1/9/2003 7.9 months
        // 3to4 52.9 weeks
        // 4---1/14/2004 1 yr 8 mos
        // 4to5 3 years 7 mos/177 weeks
        // 5---6/6/2007 5 yr .7 mos

        setStudentDOBandDosesString("2002-04-14, 2002-09-07, 2002-10-22, 2003-01-09, 2004-01-14, 2007-06-06");
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test ticket T 20125947 unknown error message.
     */
    @Test
    public void testTicketT20125947UnknownErrorMessage() {
        setup("2013-12-04");
        // dob 7/20/2005
        // Dose1: 9/25/2005 2.2 months, 9.6 weeks 67 day
        // --dose 1-2 .9 months/4 weeks/28 days
        // Dose2: 10/23/2005 3.1 months, 13.6 weeks 95 days
        // --dose 2-3 1.4 months/6 weeks/42 days
        // Dose3: 12/4/2005 4.5 months 19.6 weeks, 137 days
        // --dose 3-4 1 yr 9.6 months/21.6 months/93.7 weeks
        // Dose4: 9/21/2007 2 yrs 2.1 months 113.3 weeks 793 days
        // --dose 4-5 1 yr 8 months 20 months/86.9 weeks/608 days
        // Dose5: 5/21/2009 3 yrs 10 months 46 months 200.1 weeks 1401 days

        setStudentDOBandDosesString("2005-07-20, 2005-09-25, 2005-10-23, 2005-12-04, 2007-09-21, 2009-05-21");
        evaluateStudentFromMStudentInformation(false);
        // dose 3 did not occur 178 days after dose 2
    }

    /**
     * Test ticket T 20126114.
     */
    @Test
    public void testTicketT20126114() {
        setup("2013-12-17");
        setStudentDOBandDosesString("2001-07-28, 2001-11-29, 2002-01-29, 2003-03-27, 2005-10-05, 2012-12-04");
        evaluateStudentFromMStudentInformation(true);
    }
}
