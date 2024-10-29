/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, is not permitted
 * without express
 * written agreement from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.sis.model.business.health.immunizations;

import org.junit.Test;

/**
 * The Class ImmunizationRuleVAHpvTest.
 */
public class ImmunizationRuleVAHpvTest extends ImmunizationRuleTestMaster {
    private static final String TODAY = "2014-11-15";

    public void setup(String today) {
        super.setup(today, "/com/x2dev/procedures/statereporting/va/Immunizationrule-hpv.xml");
    }

    /**
     * Test 0 doses grade 7 male.
     */
    /*
     * test cases for male student
     * immunization rule only applies to females
     * all male students should pass regardless of number or date of doses
     */
    @Test
    public void test0DosesGrade7Male() {
        setup(TODAY);
        setStudentDOBandDosesString("2001-11-15:male"); // student is grade 7 - 0 doses
        evaluateStudentFromMStudentInformation(true);
    }


    /*
     * test cases for female student
     * immunization rules apply
     * 3 doses before 6th grade
     */

    /**
     * Test 0 doses is grade 7.
     */
    @Test
    public void test0DosesIsGrade7() {
        setup(TODAY);
        setStudentDOBandDosesString("2001-11-15:female"); // student is grade 7
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test 1 dose grade 5 is grade 7.
     */
    @Test
    public void test1DoseGrade5IsGrade7() {
        setup(TODAY);
        setStudentDOBandDosesString("2001-11-15:female", "2012-09-20"); // student is grade 7 - 1
        // dose in 5th grade
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test 2 doses grade 5 is grade 7.
     */
    @Test
    public void test2DosesGrade5IsGrade7() {
        setup(TODAY);
        setStudentDOBandDosesString("2001-11-15:female", "2012-09-20, 2012-09-21"); // student is
        // grade 7 - 2
        // doses in 5th
        // grade
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test 3 doses grade 5 is grade 7.
     */
    @Test
    public void test3DosesGrade5IsGrade7() {
        setup(TODAY);
        setStudentDOBandDosesString("2001-11-15:female", "2012-09-20, 2012-09-21, 2012-09-22"); // student
        // is
        // grade
        // 7
        // -
        // 3
        // doses
        // in
        // 5th
        // grade
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test 0 doses is grade 5.
     */
    @Test
    public void test0DosesIsGrade5() {
        setup(TODAY);
        setStudentDOBandDosesString("2003-11-15:female"); // student is grade 5 - 0 doses
        evaluateStudentFromMStudentInformation(true);
    }

    /**
     * Test 3 doses grade 7 is grade 8.
     */
    @Test
    public void test3DosesGrade7IsGrade8() {
        setup(TODAY);
        setStudentDOBandDosesString("2000-11-15:female", "2013-09-20, 2013-09-21, 2013-09-22"); // student
        // is
        // grade
        // 8
        // -
        // 3
        // doses
        // in
        // 7th
        // grade
        evaluateStudentFromMStudentInformation(false);
    }

    /**
     * Test 3 doses grade 6 is grade 7.
     */
    @Test
    public void test3DosesGrade6IsGrade7() {
        setup(TODAY);
        setStudentDOBandDosesString("2001-11-15:female", "2013-09-20, 2013-09-21, 2013-09-22"); // student
        // is
        // grade
        // 7
        // -
        // 3
        // doses
        // in
        // 6th
        // grade
        evaluateStudentFromMStudentInformation(false);
    }

}
