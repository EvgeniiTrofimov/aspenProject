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
 * The Class ImmunizationRuleWAPolioTest.
 */
public class ImmunizationRuleWAPolioTest extends ImmunizationRuleTestMaster {

    public void setup(String today) {
        super.setup(today, "/com/x2dev/procedures/statereporting/wa/Immunizationrule-polio.xml");
    }

    /**
     * <pre>
     *       Dose #      Minimum Age     Minimum Interval Between Doses
     *		 Dose     1          6 weeks       4 weeks between Doses 1 & 2
     * 		 Dose     2         10 weeks       4 weeks between Doses 2 & 3
     *		 Dose     3         14 weeks       6 months between Dose 3 & 4
     *		 Dose     4          4 years       -
     * </pre>
     */

    // Dose #1 minimum age 6 weeks maximum age 3 months

    @Test
    public void testPolioMinAgeDoseOneTestOne() {
        setup("2013-10-17");
        setStudentDOBandDosesString("2013-09-12, 2013-10-01"); // Student is 5 weeks old, and one
        // dose
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test polio min age dose one test two.
     */
    @Test
    public void testPolioMinAgeDoseOneTestTwo() {
        setup("2013-10-17");
        setStudentDOBandDosesString("2013-09-12"); // student is 5 weeks old, and no dose
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test polio min age dose one test three.
     */
    @Test
    public void testPolioMinAgeDoseOneTestThree() {
        setup("2013-10-17");
        setStudentDOBandDosesString("2013-08-29, 2013-10-17"); // student is 7 weeks old, and one
        // dose greater than 6 weeks
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test polio min age dose one test four.
     */
    @Test
    public void testPolioMinAgeDoseOneTestFour() {
        setup("2013-10-17");
        setStudentDOBandDosesString("2013-08-29, 2013-12-15"); // student is 7 weeks old, and one
        // dose greater than 6 weeks and
        // greater than 3 months
        evaluateStudentFromMStudentInformation(false);
    }

    // Dose #2 minimum age 10 weeks maximum age 16 months

    /**
     * Test polio min age dose two test one.
     */
    @Test
    public void testPolioMinAgeDoseTwoTestOne() {
        setup("2011-05-15");
        setStudentDOBandDosesString("2011-01-01, 2011-02-15, 2011-03-15, 2011-05-01"); // Student is
        // 4.5 months
        // old, three
        // good doses
        // (upper end
        // of range
        // includes
        // dose
        // three)
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test polio min age dose two test two.
     */
    @Test
    public void testPolioMinAgeDoseTwoTestTwo() {
        setup("2011-05-15");
        setStudentDOBandDosesString("2011-01-01, 2011-02-15, 2011-03-01, 2011-06-01"); // student is
        // 11 weeks
        // old, one
        // bad dose,
        // one good
        // dose (Too
        // early),
        // may be
        // flagged as
        // skip early
        // and
        // excluded
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test polio min age dose two test three.
     */
    @Test
    public void testPolioMinAgeDoseTwoTestThree() {
        setup("2011-05-15");
        setStudentDOBandDosesString("2011-01-01, 2011-02-15"); // student is 2 months old, one good
        // dose
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test polio min age dose two test four.
     */
    @Test
    public void testPolioMinAgeDoseTwoTestFour() {
        setup("2013-01-01");
        setStudentDOBandDosesString("2011-01-01, 2011-02-15, 2013-01-01"); // student is 2 years
        // old, one bad dose, one
        // good dose (Too late)
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test polio min age dose three test one.
     */
    // Dose #3 minimum age 14 weeks maximum age 19 months
    @Test
    public void testPolioMinAgeDoseThreeTestOne() {
        setup("2011-05-15");
        setStudentDOBandDosesString("2011-01-01, 2011-02-15, 2011-03-15, 2011-05-01"); // Student is
        // 4.5 months
        // old, three
        // good doses
        // (upper end
        // of range
        // includes
        // dose
        // three)
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test polio min age dose three test two.
     */
    @Test
    public void testPolioMinAgeDoseThreeTestTwo() {
        setup("2011-05-15");
        setStudentDOBandDosesString("2011-01-01, 2011-02-15, 2011-03-01, 2011-06-01"); // student is
        // 11 weeks
        // old, one
        // bad dose,
        // one good
        // dose (Too
        // early),
        // may be
        // flagged as
        // skip early
        // and
        // excluded
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test polio min age dose three test three.
     */
    @Test
    public void testPolioMinAgeDoseThreeTestThree() {
        setup("2011-05-15");
        setStudentDOBandDosesString("2011-01-01, 2011-02-15"); // student is 2 months old, one good
        // dose
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test polio min age dose three test four.
     */
    @Test
    public void testPolioMinAgeDoseThreeTestFour() {
        setup("2013-01-01");
        setStudentDOBandDosesString("2011-01-01, 2011-02-15, 2013-01-01"); // student is 2 years
        // old, one bad dose, one
        // good dose (Too late)
        evaluateStudentFromMStudentInformation(false);
    }


    /**
     * Test eighteen exclusion pass no dose.
     */
    @Test
    public void testEighteenExclusionPassNoDose() {
        // Over 18 requires no doses
        setup(null);
        setStudentDOBandDosesString("1983-11-28");
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test eighteen exclusion pass one dose.
     */
    @Test
    public void testEighteenExclusionPassOneDose() {
        // Over 18 requires no doses
        setup(null);
        // Passes with doses that would cause errors elsewhere
        setStudentDOBandDosesString("1983-11-28, 1983-12-01");
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test preschool non compliance issue.
     */
    @Test
    public void testPreschoolNonComplianceIssue() {
        // T20124829 - WA Immunizations: ALL preschoolers showing non-compliant that are
        // compliant-Polio and dtap
        setup("2013-10-17");
        // DOB //2 months //4 months //6 months
        setStudentDOBandDosesString("2010-01-25, 2010-03-26, 2010-05-28, 2010-07-30"); // Student is
        // not 4
        evaluateStudentFromMStudentInformation(true);
    }
}
