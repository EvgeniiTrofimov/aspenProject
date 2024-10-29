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
 * Unit tests for the gender attribute on immunization compliance rule sets.
 *
 * @author mmastrangelo
 */
public class ImmunizationRuleRulesetGenderTest extends ImmunizationRuleTestMaster {

    public void setup(String today) {
        super.setup(today, "/com/x2dev/sis/model/business/health/immunizations/Immunizationrule-RulesetGender.xml");
    }

    /**
     * Test gender.
     */
    @Test
    public void testGender() {
        setup("2014-07-26");

        setStudentDOBandDosesString("2004-12-31:M, 2006-07-15");
        evaluateStudentFromMStudentInformation(true);

        setStudentDOBandDosesString("2004-12-31:F, 2006-07-15");
        evaluateStudentFromMStudentInformation(false);

        setStudentDOBandDosesString("2004-12-31:F, 2006-07-15, 2006-08-11");
        evaluateStudentFromMStudentInformation(true);
    }
}
