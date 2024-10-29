/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2011 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.sis.model.business.health.immunizations;

import org.junit.Test;

/**
 * Unit tests for the grade range attribute on immunization compliance rule sets.
 *
 * @author mmastrangelo
 */
public class ImmunizationRuleRulesetGradeRangeTest extends ImmunizationRuleTestMaster {

    public void setup(String today) {
        super.setup(today, "/com/x2dev/sis/model/business/health/immunizations/Immunizationrule-RulesetGradeRange.xml");
    }

    /**
     * Test inclusive grade range.
     */
    @Test
    public void testInclusiveGradeRange() {
        setup("2014-07-26");

        // Student with DOB 2007-07-01 is in 1st grade based on test assumptions
        setStudentDOBandDosesString("2007-07-01, 2006-07-15");
        evaluateStudentFromMStudentInformation(true); // Should pass; no rule covers grade 5

        // Student with DOB 2006-07-01 is in 2nd grade based on test assumptions
        setStudentDOBandDosesString("2006-07-01, 2006-07-15");
        evaluateStudentFromMStudentInformation(false); // Should fail, not enough doses

        setStudentDOBandDosesString("2006-07-01, 2006-07-15, 2007-07-15");
        evaluateStudentFromMStudentInformation(true); // Should pass; 2 doses required in grades 2-4
        // inclusive

        // Student with DOB 2005-07-01 is in 3rd grade based on test assumptions
        setStudentDOBandDosesString("2005-07-01, 2006-07-15");
        evaluateStudentFromMStudentInformation(false); // Should fail, not enough doses

        setStudentDOBandDosesString("2005-07-01, 2006-07-15, 2007-07-15");
        evaluateStudentFromMStudentInformation(true); // Should pass; 2 doses required in grades 2-4
        // inclusive

        // Student with DOB 2004-07-01 is in 4th grade based on test assumptions
        setStudentDOBandDosesString("2004-07-01, 2006-07-15");
        evaluateStudentFromMStudentInformation(false); // Should fail, not enough doses

        setStudentDOBandDosesString("2004-07-01, 2006-07-15, 2007-07-15");
        evaluateStudentFromMStudentInformation(true); // Should pass; 2 doses required in grades 2-4
        // inclusive

        // Student with DOB 2003-07-01 is in 5th grade based on test assumptions
        setStudentDOBandDosesString("2003-07-01, 2006-07-15");
        evaluateStudentFromMStudentInformation(true); // Should pass; no rule covers grade 5
    }

    /**
     * Test exclusive grade range.
     */
    @Test
    public void testExclusiveGradeRange() {
        setup("2014-07-26");

        // Student with DOB 2003-07-01 is in 5th grade based on test assumptions
        setStudentDOBandDosesString("2003-07-01, 2006-07-15, 2007-07-15, 2008-07-15");
        evaluateStudentFromMStudentInformation(true); // Should pass; no rule covers grade 5

        // Student with DOB 2002-07-01 is in 6th grade based on test assumptions
        setStudentDOBandDosesString("2002-07-01, 2006-07-15, 2007-07-15, 2008-07-15");
        evaluateStudentFromMStudentInformation(false); // Should fail; not enough doses

        setStudentDOBandDosesString("2002-07-01, 2006-07-15, 2007-07-15, 2008-07-15, 2009-07-15");
        evaluateStudentFromMStudentInformation(true); // Should pass; 4 doses required in grades 5-7
        // exclusive

        // Student with DOB 2001-07-01 is in 7th grade based on test assumptions
        setStudentDOBandDosesString("2001-07-01, 2006-07-15, 2007-07-15, 2008-07-15"); // Should
        // pass; no
        // rule
        // covers
        // grade 7
        evaluateStudentFromMStudentInformation(true); // Should pass; no rule covers grade 5
    }
}
